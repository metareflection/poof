(defpackage :conflict-semantics
  (:use :cl)
  (:export #:conflict-detecting-generic-function
           #:delegate-to
           #:undelegate
           #:method-conflict-error
           #:defgeneric/conflict
           #:clear-resolution-cache))

(in-package :conflict-semantics)

;;; ============================================================
;;; Implementation compatibility layer
;;; ============================================================

(defun cs-class-direct-subclasses (class)
  #+sbcl (sb-mop:class-direct-subclasses class)
  #+ccl (ccl:class-direct-subclasses class)
  #-(or sbcl ccl) (error "class-direct-subclasses not implemented for this Lisp"))

(defun cs-class-direct-superclasses (class)
  #+sbcl (sb-mop:class-direct-superclasses class)
  #+ccl (ccl:class-direct-superclasses class)
  #-(or sbcl ccl) (error "class-direct-superclasses not implemented for this Lisp"))

(defun cs-generic-function-methods (gf)
  #+sbcl (sb-mop:generic-function-methods gf)
  #+ccl (ccl:generic-function-methods gf)
  #-(or sbcl ccl) (error "generic-function-methods not implemented for this Lisp"))

(defun cs-method-specializers (method)
  #+sbcl (sb-mop:method-specializers method)
  #+ccl (ccl:method-specializers method)
  #-(or sbcl ccl) (error "method-specializers not implemented for this Lisp"))

(defun cs-method-qualifiers (method)
  #+sbcl (sb-mop:method-qualifiers method)
  #+ccl (ccl:method-qualifiers method)
  #-(or sbcl ccl) (error "method-qualifiers not implemented for this Lisp"))

(defun cs-method-function (method)
  #+sbcl (sb-mop:method-function method)
  #+ccl (ccl:method-function method)
  #-(or sbcl ccl) (error "method-function not implemented for this Lisp"))

(defun cs-generic-function-name (gf)
  #+sbcl (sb-mop:generic-function-name gf)
  #+ccl (ccl:generic-function-name gf)
  #-(or sbcl ccl) (error "generic-function-name not implemented for this Lisp"))

(defun cs-class-finalized-p (class)
  #+sbcl (sb-mop:class-finalized-p class)
  #+ccl (ccl:class-finalized-p class)
  #-(or sbcl ccl) t)

(defun cs-finalize-inheritance (class)
  #+sbcl (sb-mop:finalize-inheritance class)
  #+ccl (ccl:finalize-inheritance class)
  #-(or sbcl ccl) nil)

(defun cs-class-precedence-list (class)
  #+sbcl (sb-mop:class-precedence-list class)
  #+ccl (ccl:class-precedence-list class)
  #-(or sbcl ccl) (error "class-precedence-list not implemented for this Lisp"))

;;; ============================================================
;;; Condition for conflicts
;;; ============================================================

(define-condition method-conflict-error (error)
  ((generic-function :initarg :generic-function :reader conflict-gf)
   (class :initarg :class :reader conflict-class)
   (qualifier :initarg :qualifier :reader conflict-qualifier :initform nil)
   (contributors :initarg :contributors :reader conflict-contributors))
  (:report
   (lambda (c s)
     (format s "Method conflict for ~S~@[ (~S methods)~] on class ~S:~%~
                The following superclasses contribute conflicting methods:~%~
                ~{  ~S~%~}"
             (cs-generic-function-name (conflict-gf c))
             (conflict-qualifier c)
             (class-name (conflict-class c))
             (mapcar (lambda (contrib)
                       (class-name (car contrib)))
                     (conflict-contributors c))))))

;;; ============================================================
;;; Custom GF class
;;; ============================================================

(defclass conflict-detecting-generic-function (standard-generic-function)
  ((delegation-table
    :initform (make-hash-table :test 'equal)
    :accessor gf-delegation-table
    :documentation "Maps (class-name qualifier) to delegated superclass name")
   (resolution-cache
    :initform (make-hash-table :test 'eq)
    :accessor gf-resolution-cache
    :documentation "Maps class to resolved effective-method-function")
   (cache-lock
    :initform #+sbcl (sb-thread:make-mutex)
              #+ccl (ccl:make-lock)
              #-(or sbcl ccl) nil
    :accessor gf-cache-lock
    :documentation "Lock for thread-safe cache access"))
  (:metaclass #+sbcl sb-mop:funcallable-standard-class
              #+ccl ccl:funcallable-standard-class
              #-(or sbcl ccl) standard-class))

(defmacro with-cache-lock ((gf) &body body)
  (let ((lock-var (gensym "LOCK")))
    `(let ((,lock-var (gf-cache-lock ,gf)))
       (declare (ignorable ,lock-var))
       #+sbcl (sb-thread:with-mutex (,lock-var) ,@body)
       #+ccl (ccl:with-lock-grabbed (,lock-var) ,@body)
       #-(or sbcl ccl) (progn ,@body))))

;;; ============================================================
;;; Cache invalidation
;;; ============================================================

(defun invalidate-cache-for-class (gf class)
  "Invalidate cached resolutions for CLASS and all its subclasses."
  (with-cache-lock (gf)
    (labels ((invalidate (c)
               (remhash c (gf-resolution-cache gf))
               (dolist (sub (cs-class-direct-subclasses c))
                 (invalidate sub))))
      (invalidate class))))

(defun invalidate-cache-for-gf (gf)
  "Invalidate all cached resolutions for GF."
  (with-cache-lock (gf)
    (clrhash (gf-resolution-cache gf))))

(defun clear-resolution-cache (gf)
  "Public interface to clear a GF's resolution cache."
  (invalidate-cache-for-gf gf))

;;; Hook into method addition/removal for cache invalidation
(defmethod add-method :after ((gf conflict-detecting-generic-function) method)
  (let ((spec (first (cs-method-specializers method))))
    (when (typep spec 'class)
      (invalidate-cache-for-class gf spec))))

(defmethod remove-method :after ((gf conflict-detecting-generic-function) method)
  (let ((spec (first (cs-method-specializers method))))
    (when (typep spec 'class)
      (invalidate-cache-for-class gf spec))))

;;; ============================================================
;;; Delegation declarations
;;; ============================================================

(defmacro delegate-to (class generic-function superclass &optional qualifier)
  "Declare that CLASS delegates GF (optionally for QUALIFIER) to SUPERCLASS."
  (let ((class-name (if (symbolp class) class `(class-name ,class))))
    `(progn
       (setf (gethash (list ',class-name ,qualifier)
                      (gf-delegation-table ,generic-function))
             ',superclass)
       (invalidate-cache-for-class ,generic-function (find-class ',class-name))
       ',class-name)))

(defmacro undelegate (class generic-function &optional qualifier)
  "Remove explicit delegation for CLASS in GF."
  (let ((class-name (if (symbolp class) class `(class-name ,class))))
    `(progn
       (remhash (list ',class-name ,qualifier)
                (gf-delegation-table ,generic-function))
       (invalidate-cache-for-class ,generic-function (find-class ',class-name))
       ',class-name)))

;;; ============================================================
;;; DAG analysis
;;; ============================================================

(defstruct (dag-node (:constructor make-dag-node (class)))
  class
  (primary-method nil)
  (before-methods '())
  (after-methods '())
  (around-methods '())
  (children '())
  (parents '()))

(defun dag-node-methods-for-qualifier (node qualifier)
  (ecase qualifier
    ((nil) (let ((m (dag-node-primary-method node)))
             (if m (list m) nil)))
    (:before (dag-node-before-methods node))
    (:after (dag-node-after-methods node))
    (:around (dag-node-around-methods node))))

(defun add-method-to-node (method node qualifier)
  (ecase qualifier
    ((nil) (setf (dag-node-primary-method node) method))
    (:before (push method (dag-node-before-methods node)))
    (:after (push method (dag-node-after-methods node)))
    (:around (push method (dag-node-around-methods node)))))

(defun method-qualifier-key (method)
  "Return the qualifier key for a method (nil for primary)."
  (let ((quals (cs-method-qualifiers method)))
    (cond ((null quals) nil)
          ((equal quals '(:before)) :before)
          ((equal quals '(:after)) :after)
          ((equal quals '(:around)) :around)
          (t (error "Unsupported method qualifiers: ~S" quals)))))

(defun build-method-dag (gf class)
  "Build DAG of classes with methods for GF, restricted to ancestors of CLASS.
   Returns (values node-table root-nodes class-node)."
  (unless (cs-class-finalized-p class)
    (cs-finalize-inheritance class))

  (let ((nodes (make-hash-table :test 'eq)))

    ;; First pass: collect all methods and their specializing classes
    (dolist (method (cs-generic-function-methods gf))
      (let ((spec (first (cs-method-specializers method))))
        (when (and (typep spec 'class)
                   (subtypep class spec))
          (let ((node (or (gethash spec nodes)
                          (setf (gethash spec nodes) (make-dag-node spec))))
                (qual (method-qualifier-key method)))
            (add-method-to-node method node qual)))))

    ;; Add ALL classes in the class precedence list to the DAG
    ;; This is necessary to detect conflicts through intermediate classes
    (dolist (c (cs-class-precedence-list class))
      (unless (gethash c nodes)
        (setf (gethash c nodes) (make-dag-node c))))

    ;; Build parent/child relationships
    (let ((node-classes (loop for k being the hash-keys of nodes collect k)))
      (dolist (c1 node-classes)
        (let ((node1 (gethash c1 nodes)))
          (dolist (c2 node-classes)
            (when (and (not (eq c1 c2))
                       (subtypep c1 c2))
              ;; c1 < c2 (c1 more specific)
              ;; Check no intermediate node
              (let ((dominated nil))
                (dolist (c3 node-classes)
                  (when (and (not (eq c3 c1))
                             (not (eq c3 c2))
                             (subtypep c1 c3)
                             (subtypep c3 c2))
                    (setq dominated t)
                    (return)))
                (unless dominated
                  (let ((node2 (gethash c2 nodes)))
                    (pushnew node2 (dag-node-parents node1))
                    (pushnew node1 (dag-node-children node2))))))))))

    ;; Find roots
    (let ((roots '()))
      (maphash (lambda (c node)
                 (declare (ignore c))
                 (when (null (dag-node-parents node))
                   (push node roots)))
               nodes)
      (values nodes roots (gethash class nodes)))))

;;; ============================================================
;;; Resolution structures
;;; ============================================================

(defstruct resolution
  method
  source-class
  conflict-p
  contributors)

(defstruct composite-resolution
  (primary nil :type (or resolution null))
  (before '() :type list)
  (after '() :type list)
  (around '() :type list))

;;; ============================================================
;;; Conflict resolution engine
;;; ============================================================

(defun resolve-for-qualifier (gf class node-table qualifier)
  "Resolve which method(s) CLASS should use for QUALIFIER."
  (let ((resolutions (make-hash-table :test 'eq))
        (delegation-table (gf-delegation-table gf)))

    (labels
        ((get-explicit-delegation (cls)
           (gethash (list (class-name cls) qualifier) delegation-table))

         (get-resolution (node)
           (or (gethash (dag-node-class node) resolutions)
               (compute-resolution node)))

         (own-methods (node)
           (remove nil (dag-node-methods-for-qualifier node qualifier)))

         (compute-resolution (node)
           (let* ((cls (dag-node-class node))
                  (own (own-methods node))
                  (parents (dag-node-parents node))
                  (explicit-del (get-explicit-delegation cls)))

             (setf (gethash cls resolutions)
                   (cond
                     ;; Explicit delegation
                     (explicit-del
                      (let* ((super (find-class explicit-del nil))
                             (super-node (and super (gethash super node-table))))
                        (if super-node
                            (let ((super-res (get-resolution super-node)))
                              (make-resolution
                               :method (resolution-method super-res)
                               :source-class (resolution-source-class super-res)
                               :conflict-p nil
                               :contributors nil))
                            (make-resolution
                             :method nil
                             :source-class nil
                             :conflict-p nil
                             :contributors nil))))

                     ;; Has own method(s)
                     (own
                      (make-resolution
                       :method (first own)
                       :source-class cls
                       :conflict-p nil
                       :contributors nil))

                     ;; No parents
                     ((null parents)
                      (make-resolution :method nil :source-class nil
                                       :conflict-p nil :contributors nil))

                     ;; Single parent
                     ((null (cdr parents))
                      (get-resolution (car parents)))

                     ;; Multiple parents - check conflict
                     (t
                      (let* ((parent-res (mapcar #'get-resolution parents))
                             (distinct (remove-duplicates
                                        (remove nil
                                                (mapcar #'resolution-method parent-res))
                                        :test #'eq)))
                        (cond
                          ((null distinct)
                           (make-resolution :method nil :source-class nil
                                            :conflict-p nil :contributors nil))

                          ((null (cdr distinct))
                           (find-if #'resolution-method parent-res))

                          (t
                           (make-resolution
                            :method nil
                            :source-class nil
                            :conflict-p t
                            :contributors
                            (loop for p in parents
                                  for r = (get-resolution p)
                                  when (resolution-method r)
                                    collect (cons (dag-node-class p)
                                                  (resolution-method r))))))))))

             (gethash cls resolutions))))

      (let ((class-node (gethash class node-table)))
        (if class-node
            (get-resolution class-node)
            (make-resolution :method nil :source-class nil
                             :conflict-p nil :contributors nil))))))

(defun collect-auxiliary-methods (gf class node-table qualifier)
  "Collect all auxiliary methods for QUALIFIER in inheritance order."
  (declare (ignore gf))
  (let ((methods '())
        (seen-methods (make-hash-table :test 'eq)))

    (unless (cs-class-finalized-p class)
      (cs-finalize-inheritance class))

    (dolist (c (cs-class-precedence-list class))
      (let ((node (gethash c node-table)))
        (when node
          (dolist (m (dag-node-methods-for-qualifier node qualifier))
            (when (and m (not (gethash m seen-methods)))
              (setf (gethash m seen-methods) t)
              (push m methods))))))

    (let ((ordered (nreverse methods)))
      (if (eq qualifier :after)
          (reverse ordered)
          ordered))))

(defun resolve-all-for-class (gf class)
  "Compute complete resolution for CLASS including primary and auxiliary methods."
  (multiple-value-bind (node-table roots class-node)
      (build-method-dag gf class)
    (declare (ignore roots class-node))

    (let ((primary-res (resolve-for-qualifier gf class node-table nil)))

      (when (resolution-conflict-p primary-res)
        (return-from resolve-all-for-class
          (make-composite-resolution :primary primary-res)))

      (let ((before (collect-auxiliary-methods gf class node-table :before))
            (after (collect-auxiliary-methods gf class node-table :after))
            (around (collect-auxiliary-methods gf class node-table :around)))

        (make-composite-resolution
         :primary primary-res
         :before before
         :after after
         :around around)))))

;;; ============================================================
;;; Effective method construction
;;; ============================================================

(defun invoke-method (method args)
  "Invoke METHOD with ARGS, handling implementation differences."
  (let ((fn (cs-method-function method)))
    #+sbcl (funcall fn args nil)
    #+ccl (apply fn args)
    #-(or sbcl ccl) (apply fn args)))

(defun invoke-method-function (fn args)
  "Invoke a method function with ARGS."
  #+sbcl (funcall fn args nil)
  #+ccl (apply fn args)
  #-(or sbcl ccl) (apply fn args))

(defun build-around-chain (around-methods before-fns primary-fn after-fns)
  "Build a chain of around methods with call-next-method support."
  (let ((around-fns (mapcar #'cs-method-function around-methods)))
    (labels
        ((make-inner-callable ()
           "Create the innermost callable (before + primary + after)."
           (lambda (args)
             (dolist (fn before-fns)
               (invoke-method-function fn args))
             (multiple-value-prog1
                 (invoke-method-function primary-fn args)
               (dolist (fn after-fns)
                 (invoke-method-function fn args)))))

         (wrap-around (inner around-fn)
           "Wrap INNER with an around method."
           (lambda (args)
             ;; SBCL method functions take (args next-methods)
             ;; where next-methods is a list and call-next-method
             ;; invokes the first one with the same args pattern
             #+sbcl
             (funcall around-fn
                      args
                      (list (sb-mop:make-method-lambda
                             ;; This is tricky - we need a callable that
                             ;; SBCL's call-next-method can invoke
                             ;; Actually, let's use a simpler approach
                             )))
             ;; Simpler approach: we construct the chain at build time
             (funcall around-fn args (list inner)))))

      ;; For simplicity, we'll construct the effective method differently:
      ;; Build a single lambda that manually chains the around methods
      (let ((inner-fn (make-inner-callable)))
        (lambda (&rest args)
          (let ((next-fn inner-fn))
            ;; Process around methods from innermost to outermost
            (dolist (around-fn (reverse around-fns))
              (let ((current-next next-fn))
                (setf next-fn
                      (lambda (args)
                        ;; Create a fake "next-methods" for the around method
                        #+sbcl
                        (funcall around-fn args
                                 (list (cons :pseudo-method
                                             (lambda (a n)
                                               (declare (ignore n))
                                               (funcall current-next a)))))
                        #+ccl
                        (let ((ccl::*next-methods*
                                (list (lambda (&rest a)
                                        (funcall current-next a)))))
                          (apply around-fn args))
                        #-(or sbcl ccl)
                        (funcall around-fn args)))))
            (funcall next-fn args)))))))

#+sbcl
(defun build-emf-sbcl (primary-method before-methods after-methods around-methods)
  "Build EMF for SBCL where method functions take (args next-methods)."
  (let* ((before-fns (mapcar #'cs-method-function before-methods))
         (after-fns (mapcar #'cs-method-function after-methods))
         (primary-fn (cs-method-function primary-method))
         (around-fns (mapcar #'cs-method-function around-methods))

         (inner-callable
           (lambda (args next-methods)
             (declare (ignore next-methods))
             (dolist (fn before-fns)
               (funcall fn args nil))
             (multiple-value-prog1
                 (funcall primary-fn args nil)
               (dolist (fn after-fns)
                 (funcall fn args nil))))))

    (if (null around-fns)
        (lambda (&rest args)
          (funcall inner-callable args nil))
        (lambda (&rest args)
          (funcall (first around-fns)
                   args
                   (append (cdr around-fns) (list inner-callable)))))))

#+ccl
(defun build-emf-ccl (primary-method before-methods after-methods around-methods)
  "Build EMF for CCL."
  ;; CCL method functions are regular functions, and call-next-method
  ;; uses ccl::*next-methods* (a list of methods, not functions)
  ;; and ccl::%call-next-method-with-args / ccl::%call-next-method
  ;;
  ;; Actually CCL is more complex - it stores method objects, not functions.
  ;; We may need to create wrapper "pseudo-methods" or use a different approach.
  ;;
  ;; For now, let's try the simple approach and see if CCL method functions
  ;; also take (args next-methods):
  (let* ((before-fns (mapcar #'cs-method-function before-methods))
         (after-fns (mapcar #'cs-method-function after-methods))
         (primary-fn (cs-method-function primary-method))
         (around-fns (mapcar #'cs-method-function around-methods))

         (inner-callable
           (lambda (args next-methods)
             (declare (ignore next-methods))
             (dolist (fn before-fns)
               (funcall fn args nil))
             (multiple-value-prog1
                 (funcall primary-fn args nil)
               (dolist (fn after-fns)
                 (funcall fn args nil))))))

    (if (null around-fns)
        (lambda (&rest args)
          (funcall inner-callable args nil))
        (lambda (&rest args)
          (funcall (first around-fns)
                   args
                   (append (cdr around-fns) (list inner-callable)))))))

(defun build-effective-method-function (gf class composite-res)
  "Build effective method with proper call-next-method support."
  (let ((primary-res (composite-resolution-primary composite-res)))

    (when (and primary-res (resolution-conflict-p primary-res))
      (return-from build-effective-method-function
        (lambda (&rest args)
          (declare (ignore args))
          (error 'method-conflict-error
                 :generic-function gf
                 :class class
                 :qualifier nil
                 :contributors (resolution-contributors primary-res)))))

    (let ((primary-method (and primary-res (resolution-method primary-res)))
          (before-methods (composite-resolution-before composite-res))
          (after-methods (composite-resolution-after composite-res))
          (around-methods (composite-resolution-around composite-res)))

      (unless primary-method
        (return-from build-effective-method-function
          (lambda (&rest args)
            (error "No applicable primary method for ~S on ~S with args ~S"
                   (cs-generic-function-name gf) class args))))

      #+sbcl (build-emf-sbcl primary-method before-methods after-methods around-methods)
      #+ccl (build-emf-ccl primary-method before-methods after-methods around-methods)
      #-(or sbcl ccl) (error "Unsupported implementation"))))


;;; Dynamic variable for call-next-method support
(defvar *next-method-function* nil
  "The next method function for call-next-method.")
(defvar *current-args* nil
  "Current arguments for call-next-method with no args.")

(defun call-with-next-method-binding (next-fn args thunk)
  "Call THUNK with *next-method-function* bound to NEXT-FN."
  (let ((*next-method-function* next-fn)
        (*current-args* args))
    (funcall thunk)))

(defun cs-call-next-method (&rest args)
  "Call the next method in the chain."
  (unless *next-method-function*
    (error "No next method"))
  (funcall *next-method-function*
           (if args args *current-args*)))

(defun build-method-chain (around-methods before-methods primary-method after-methods)
  "Build a callable chain for around methods."
  (let ((before-fns (mapcar #'cs-method-function before-methods))
        (after-fns (mapcar #'cs-method-function after-methods))
        (primary-fn (cs-method-function primary-method)))

    ;; The innermost function runs before/primary/after
    (let ((inner (lambda (args)
                   (dolist (fn before-fns)
                     (invoke-method-function fn args))
                   (multiple-value-prog1
                       (invoke-method-function primary-fn args)
                     (dolist (fn after-fns)
                       (invoke-method-function fn args))))))

      ;; Wrap with around methods, outermost first in the list
      ;; so we process in reverse to build inside-out
      (dolist (around-method (reverse around-methods))
        (let ((current-inner inner)
              (around-fn (cs-method-function around-method)))
          (setf inner
                (lambda (args)
                  (call-with-next-method-binding
                   current-inner args
                   (lambda ()
                     (invoke-method-function around-fn args)))))))

      inner)))

;;; The around method handling is complex due to call-next-method.
;;; Let's use a cleaner approach that doesn't try to fake the MOP machinery:

(defun build-effective-method-function-v2 (gf class composite-res)
  "Build the effective method function (simpler around handling)."
  (let ((primary-res (composite-resolution-primary composite-res)))

    (when (and primary-res (resolution-conflict-p primary-res))
      (return-from build-effective-method-function-v2
        (lambda (&rest args)
          (declare (ignore args))
          (error 'method-conflict-error
                 :generic-function gf
                 :class class
                 :qualifier nil
                 :contributors (resolution-contributors primary-res)))))

    (let ((primary-method (and primary-res (resolution-method primary-res)))
          (before-methods (composite-resolution-before composite-res))
          (after-methods (composite-resolution-after composite-res))
          (around-methods (composite-resolution-around composite-res)))

      (unless primary-method
        (return-from build-effective-method-function-v2
          (lambda (&rest args)
            (error "No applicable primary method for ~S on ~S with args ~S"
                   (cs-generic-function-name gf) class args))))

      ;; For around methods, we need to support call-next-method.
      ;; We'll use a dynamic variable approach.
      (if (null around-methods)
          ;; Simple case: no around methods
          (let ((primary-fn (cs-method-function primary-method))
                (before-fns (mapcar #'cs-method-function before-methods))
                (after-fns (mapcar #'cs-method-function after-methods)))
            (lambda (&rest args)
              (dolist (fn before-fns)
                (invoke-method-function fn args))
              (multiple-value-prog1
                  (invoke-method-function primary-fn args)
                (dolist (fn after-fns)
                  (invoke-method-function fn args)))))

          ;; Complex case: around methods present
          ;; We construct a chain where each around can call the next
          (let ((chain (build-method-chain
                        around-methods before-methods primary-method after-methods)))
            (lambda (&rest args)
              (funcall chain args)))))))

;;; ============================================================
;;; MOP integration
;;; ============================================================

(defun compute-and-cache-emf (gf class)
  "Compute EMF for CLASS and cache it."
  (let* ((composite-res (resolve-all-for-class gf class))
         (emf (build-effective-method-function gf class composite-res)))
    (with-cache-lock (gf)
      (setf (gethash class (gf-resolution-cache gf)) emf))
    emf))

(defun get-cached-emf (gf class)
  "Get or compute the effective method function for CLASS."
  (or (with-cache-lock (gf)
        (gethash class (gf-resolution-cache gf)))
      (compute-and-cache-emf gf class)))

#+sbcl
(defmethod sb-mop:compute-discriminating-function
    ((gf conflict-detecting-generic-function))
  (lambda (&rest args)
    (let* ((instance (first args))
           (class (class-of instance))
           (emf (get-cached-emf gf class)))
      (apply emf args))))

#+ccl
(defmethod ccl:compute-discriminating-function
    ((gf conflict-detecting-generic-function))
  (lambda (&rest args)
    (let* ((instance (first args))
           (class (class-of instance))
           (emf (get-cached-emf gf class)))
      (apply emf args))))

;;; ============================================================
;;; Class redefinition hooks
;;; ============================================================

(defvar *tracked-gfs*
  #+sbcl (make-hash-table :test 'eq :weakness :key)
  #+ccl (make-hash-table :test 'eq :weak :key)
  #-(or sbcl ccl) (make-hash-table :test 'eq)
  "Weak table of conflict-detecting GFs.")

(defmethod initialize-instance :after
    ((gf conflict-detecting-generic-function) &key)
  (setf (gethash gf *tracked-gfs*) t))

(defun invalidate-all-gfs-for-class (class)
  "Invalidate all conflict-detecting GF caches for CLASS."
  (maphash (lambda (gf val)
             (declare (ignore val))
             (invalidate-cache-for-class gf class))
           *tracked-gfs*))

#+sbcl
(defmethod sb-mop:finalize-inheritance :after ((class standard-class))
  (invalidate-all-gfs-for-class class))

#+ccl
(defmethod ccl:finalize-inheritance :after ((class standard-class))
  (invalidate-all-gfs-for-class class))

;;; ============================================================
;;; Convenience macro
;;; ============================================================

(defmacro defgeneric/conflict (name lambda-list &body options)
  "Define a generic function with conflict-detecting semantics."
  `(defgeneric ,name ,lambda-list
     (:generic-function-class conflict-detecting-generic-function)
     ,@options))

;;; ============================================================
;;; Debugging utilities
;;; ============================================================

(defun describe-resolution (gf class)
  "Print a description of how GF resolves for CLASS."
  (let* ((class-obj (if (symbolp class) (find-class class) class))
         (composite (resolve-all-for-class gf class-obj)))
    (format t "~&Resolution for ~S on ~S:~%"
            (cs-generic-function-name gf)
            (if (symbolp class) class (class-name class-obj)))

    (let ((primary (composite-resolution-primary composite)))
      (format t "  Primary: ~:[NONE~;~:*~S from ~S~]~@[ (CONFLICT: ~{~S~^, ~})~]~%"
              (and primary (resolution-method primary))
              (and primary (resolution-source-class primary)
                   (class-name (resolution-source-class primary)))
              (and primary
                   (resolution-conflict-p primary)
                   (mapcar (lambda (c) (class-name (car c)))
                           (resolution-contributors primary)))))

    (flet ((show-methods (label methods)
             (format t "  ~A (~D):~%" label (length methods))
             (dolist (m methods)
               (format t "    ~S~%"
                       (class-name (first (cs-method-specializers m)))))))
      (show-methods "Before" (composite-resolution-before composite))
      (show-methods "After" (composite-resolution-after composite))
      (show-methods "Around" (composite-resolution-around composite)))))

(defun list-delegations (gf)
  "List all delegations for GF."
  (format t "~&Delegations for ~S:~%" (cs-generic-function-name gf))
  (if (zerop (hash-table-count (gf-delegation-table gf)))
      (format t "  (none)~%")
      (maphash (lambda (key value)
                 (format t "  ~S~@[ (~S)~] -> ~S~%"
                         (first key) (second key) value))
               (gf-delegation-table gf))))

