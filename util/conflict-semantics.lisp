#|
The code below this comment was generated and cleaned up by Claude Code (something 4.5)

Test it with:
sbcl --load conflict-semantics.lisp --load conflict-semantics-test.lisp --eval '(conflict-semantics::run-tests)'
|#
;;;; conflict-semantics.lisp
;;;;
;;;; A CLOS extension that implements "conflict semantics" for multiple
;;;; inheritance, similar to C++/Eiffel style conflict detection rather
;;;; than CLOS's standard linearization-based resolution.
;;;;
;;;; When a class inherits from multiple superclasses that have different
;;;; methods for the same generic function, this signals an error rather
;;;; than silently picking one based on class precedence list order.
;;;;
;;;; Conflicts can be resolved by:
;;;; 1. Defining a method on the conflicting class itself
;;;; 2. Using DELEGATE-TO to explicitly choose a superclass's method

(defpackage :conflict-semantics
  (:use :cl)
  (:export #:conflict-detecting-generic-function
           #:defgeneric/conflict
           #:delegate-to
           #:undelegate
           #:method-conflict-error
           #:clear-resolution-cache
           #:describe-resolution
           #:list-delegations))

(in-package :conflict-semantics)

;;; ============================================================
;;; Implementation compatibility layer
;;;
;;; These wrappers provide a uniform interface to MOP functionality
;;; that differs between SBCL and CCL.
;;; ============================================================

(defun cs-class-direct-subclasses (class)
  #+sbcl (sb-mop:class-direct-subclasses class)
  #+ccl (ccl:class-direct-subclasses class)
  #-(or sbcl ccl) (error "class-direct-subclasses not implemented for this Lisp"))

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
;;; Error condition
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
;;; Custom generic function class
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
  "Execute BODY with the cache lock held for GF."
  (let ((lock-var (gensym "LOCK")))
    `(let ((,lock-var (gf-cache-lock ,gf)))
       (declare (ignorable ,lock-var))
       #+sbcl (sb-thread:with-mutex (,lock-var) ,@body)
       #+ccl (ccl:with-lock-grabbed (,lock-var) ,@body)
       #-(or sbcl ccl) (progn ,@body))))

;;; ============================================================
;;; Tracking generic functions for cache invalidation
;;; ============================================================

(defvar *tracked-gfs*
  #+sbcl (make-hash-table :test 'eq :weakness :key)
  #+ccl (make-hash-table :test 'eq :weak :key)
  #-(or sbcl ccl) (make-hash-table :test 'eq)
  "Weak table tracking all conflict-detecting generic functions.
   Used to invalidate caches when classes are redefined.")

(defmethod initialize-instance :after
    ((gf conflict-detecting-generic-function) &key)
  (setf (gethash gf *tracked-gfs*) t))

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

(defun invalidate-all-gfs-for-class (class)
  "Invalidate caches in all conflict-detecting GFs that might involve CLASS."
  (maphash (lambda (gf val)
             (declare (ignore val))
             (invalidate-cache-for-class gf class))
           *tracked-gfs*))

;;; Hook into method addition/removal for cache invalidation
(defmethod add-method :after ((gf conflict-detecting-generic-function) method)
  (let ((spec (first (cs-method-specializers method))))
    (when (typep spec 'class)
      (invalidate-cache-for-class gf spec))))

(defmethod remove-method :after ((gf conflict-detecting-generic-function) method)
  (let ((spec (first (cs-method-specializers method))))
    (when (typep spec 'class)
      (invalidate-cache-for-class gf spec))))

;;; Hook into class finalization for cache invalidation on class redefinition
#+sbcl
(defmethod sb-mop:finalize-inheritance :after ((class standard-class))
  (invalidate-all-gfs-for-class class))

#+ccl
(defmethod ccl:finalize-inheritance :after ((class standard-class))
  (invalidate-all-gfs-for-class class))

;;; ============================================================
;;; Delegation declarations
;;;
;;; These allow explicit resolution of conflicts by declaring that
;;; a class delegates a method to a specific superclass.
;;; ============================================================

(defmacro delegate-to (class generic-function superclass &optional qualifier)
  "Declare that CLASS delegates GF (optionally for QUALIFIER) to SUPERCLASS.
   This resolves method conflicts by explicitly choosing which inherited
   method to use."
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
;;; DAG node structure
;;;
;;; We build a DAG (directed acyclic graph) of classes relevant to
;;; method resolution. Each node tracks the methods defined directly
;;; on that class, organized by qualifier.
;;; ============================================================

(defstruct (dag-node (:constructor make-dag-node (class)))
  class
  (primary-method nil)
  (before-methods '())
  (after-methods '())
  (around-methods '())
  (children '())   ; more specific classes
  (parents '()))   ; less specific classes (ancestors)

(defun method-qualifier-key (method)
  "Return the qualifier key for a method (nil for primary)."
  (let ((quals (cs-method-qualifiers method)))
    (cond ((null quals) nil)
          ((equal quals '(:before)) :before)
          ((equal quals '(:after)) :after)
          ((equal quals '(:around)) :around)
          (t (error "Unsupported method qualifiers: ~S" quals)))))

(defun add-method-to-node (method node qualifier)
  "Add METHOD to NODE under the given QUALIFIER."
  (ecase qualifier
    ((nil) (setf (dag-node-primary-method node) method))
    (:before (push method (dag-node-before-methods node)))
    (:after (push method (dag-node-after-methods node)))
    (:around (push method (dag-node-around-methods node)))))

(defun dag-node-methods-for-qualifier (node qualifier)
  "Return list of methods on NODE for QUALIFIER."
  (ecase qualifier
    ((nil) (let ((m (dag-node-primary-method node)))
             (if m (list m) nil)))
    (:before (dag-node-before-methods node))
    (:after (dag-node-after-methods node))
    (:around (dag-node-around-methods node))))

;;; ============================================================
;;; DAG construction
;;;
;;; We build the DAG including ALL classes in the class precedence
;;; list, not just those with methods. This is necessary to detect
;;; conflicts through intermediate classes that don't define methods
;;; themselves but establish inheritance paths.
;;;
;;; Example: If A has a method, B and C inherit from A, and D inherits
;;; from both B and C, we need B and C in the DAG even if they don't
;;; have their own methods, so we can detect that D has only one
;;; inherited method (no conflict).
;;; ============================================================

(defun build-method-dag (gf class)
  "Build DAG of classes relevant to GF for CLASS.
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

    ;; Second pass: add ALL classes in the class precedence list.
    ;; This is necessary to detect conflicts through intermediate classes
    ;; that don't have methods but establish distinct inheritance paths.
    (dolist (c (cs-class-precedence-list class))
      (unless (gethash c nodes)
        (setf (gethash c nodes) (make-dag-node c))))

    ;; Third pass: build parent/child relationships.
    ;; A class C1 has C2 as a "parent" in our DAG if:
    ;;   - C1 is a proper subtype of C2
    ;;   - There is no intermediate C3 in our DAG where C1 < C3 < C2
    (let ((node-classes (loop for k being the hash-keys of nodes collect k)))
      (dolist (c1 node-classes)
        (let ((node1 (gethash c1 nodes)))
          (dolist (c2 node-classes)
            (when (and (not (eq c1 c2))
                       (subtypep c1 c2))
              ;; c1 < c2 (c1 more specific)
              ;; Check for intermediate node
              (let ((has-intermediate nil))
                (dolist (c3 node-classes)
                  (when (and (not (eq c3 c1))
                             (not (eq c3 c2))
                             (subtypep c1 c3)
                             (subtypep c3 c2))
                    (setq has-intermediate t)
                    (return)))
                (unless has-intermediate
                  (let ((node2 (gethash c2 nodes)))
                    (pushnew node2 (dag-node-parents node1))
                    (pushnew node1 (dag-node-children node2))))))))))

    ;; Find roots (nodes with no parents)
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
  method         ; the resolved method, or nil
  source-class   ; class that provided the method
  conflict-p     ; t if there's an unresolved conflict
  contributors)  ; list of (class . method) pairs for conflict error messages

(defstruct composite-resolution
  (primary nil :type (or resolution null))
  (before '() :type list)   ; list of methods, most-specific-first
  (after '() :type list)    ; list of methods, least-specific-first
  (around '() :type list))  ; list of methods, most-specific-first

;;; ============================================================
;;; Conflict resolution engine
;;;
;;; We walk the DAG from roots toward the target class, tracking
;;; which method each class "inherits" and detecting when a class
;;; would inherit conflicting methods from multiple parents.
;;; ============================================================

(defun resolve-for-qualifier (gf class node-table qualifier)
  "Resolve which method CLASS should use for QUALIFIER.
   Returns a RESOLUTION struct."
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
                     ;; Explicit delegation takes precedence
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

                     ;; Own method takes precedence (shadows inherited)
                     (own
                      (make-resolution
                       :method (first own)
                       :source-class cls
                       :conflict-p nil
                       :contributors nil))

                     ;; No parents = no method
                     ((null parents)
                      (make-resolution :method nil :source-class nil
                                       :conflict-p nil :contributors nil))

                     ;; Single parent = inherit its resolution
                     ((null (cdr parents))
                      (get-resolution (car parents)))

                     ;; Multiple parents = check for conflict
                     (t
                      (let* ((parent-res (mapcar #'get-resolution parents))
                             (distinct (remove-duplicates
                                        (remove nil
                                                (mapcar #'resolution-method parent-res))
                                        :test #'eq)))
                        (cond
                          ;; No parent has a method
                          ((null distinct)
                           (make-resolution :method nil :source-class nil
                                            :conflict-p nil :contributors nil))

                          ;; All parents agree (same method, e.g. diamond)
                          ((null (cdr distinct))
                           (find-if #'resolution-method parent-res))

                          ;; Conflict: multiple distinct inherited methods
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

(defun collect-auxiliary-methods (class node-table qualifier)
  "Collect all auxiliary methods for QUALIFIER in inheritance order.
   For :before and :around, returns most-specific-first.
   For :after, returns least-specific-first (so they run after the primary)."
  (let ((methods '())
        (seen-methods (make-hash-table :test 'eq)))

    (unless (cs-class-finalized-p class)
      (cs-finalize-inheritance class))

    ;; Walk the CPL collecting methods
    (dolist (c (cs-class-precedence-list class))
      (let ((node (gethash c node-table)))
        (when node
          (dolist (m (dag-node-methods-for-qualifier node qualifier))
            (when (and m (not (gethash m seen-methods)))
              (setf (gethash m seen-methods) t)
              (push m methods))))))

    ;; Methods are now in least-specific-first order (due to push)
    (let ((ordered (nreverse methods)))
      (if (eq qualifier :after)
          (reverse ordered)  ; after methods run least-specific-first
          ordered))))        ; before/around run most-specific-first

(defun resolve-all-for-class (gf class)
  "Compute complete resolution for CLASS including primary and auxiliary methods.
   Returns a COMPOSITE-RESOLUTION struct."
  (multiple-value-bind (node-table roots class-node)
      (build-method-dag gf class)
    (declare (ignore roots class-node))

    (let ((primary-res (resolve-for-qualifier gf class node-table nil)))

      ;; If primary has a conflict, return early
      (when (resolution-conflict-p primary-res)
        (return-from resolve-all-for-class
          (make-composite-resolution :primary primary-res)))

      ;; Collect auxiliary methods
      (let ((before (collect-auxiliary-methods class node-table :before))
            (after (collect-auxiliary-methods class node-table :after))
            (around (collect-auxiliary-methods class node-table :around)))

        (make-composite-resolution
         :primary primary-res
         :before before
         :after after
         :around around)))))

;;; ============================================================
;;; Effective method function construction
;;;
;;; We build a function that executes the resolved methods in the
;;; correct order: around methods wrap before/primary/after.
;;;
;;; SBCL method functions have signature (args next-methods) where
;;; call-next-method invokes (car next-methods) with (cdr next-methods).
;;; CCL may differ; the build-emf-ccl function handles that.
;;; ============================================================

(defun invoke-method-function (fn args)
  "Invoke a method function with ARGS."
  #+sbcl (funcall fn args nil)
  #+ccl (funcall fn args nil)
  #-(or sbcl ccl) (apply fn args))

#+sbcl
(defun build-emf-sbcl (primary-method before-methods after-methods around-methods)
  "Build effective method function for SBCL.
   SBCL method functions take (args next-methods) and call-next-method
   invokes (funcall (car next-methods) args (cdr next-methods))."
  (let* ((before-fns (mapcar #'cs-method-function before-methods))
         (after-fns (mapcar #'cs-method-function after-methods))
         (primary-fn (cs-method-function primary-method))
         (around-fns (mapcar #'cs-method-function around-methods))

         ;; The innermost callable runs before/primary/after
         ;; It has the same signature as method functions for call-next-method
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
        ;; No around methods - just call inner directly
        (lambda (&rest args)
          (funcall inner-callable args nil))
        ;; With around methods - build the next-methods chain
        (lambda (&rest args)
          (funcall (first around-fns)
                   args
                   (append (cdr around-fns) (list inner-callable)))))))

#+ccl
(defun build-emf-ccl (primary-method before-methods after-methods around-methods)
  "Build effective method function for CCL."
  ;; CCL's method function calling convention - adjust if needed
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
  "Build the effective method function from a composite resolution."
  (let ((primary-res (composite-resolution-primary composite-res)))

    ;; Handle conflict
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

      ;; Handle no applicable method
      (unless primary-method
        (return-from build-effective-method-function
          (lambda (&rest args)
            (error "No applicable primary method for ~S on ~S with args ~S"
                   (cs-generic-function-name gf) class args))))

      ;; Build implementation-specific EMF
      #+sbcl (build-emf-sbcl primary-method before-methods after-methods around-methods)
      #+ccl (build-emf-ccl primary-method before-methods after-methods around-methods)
      #-(or sbcl ccl) (error "Unsupported implementation"))))

;;; ============================================================
;;; MOP integration
;;;
;;; We override compute-discriminating-function to use our own
;;; method resolution instead of the standard CLOS linearization.
;;; ============================================================

(defun compute-and-cache-emf (gf class)
  "Compute effective method function for CLASS and cache it."
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
;;; Public interface
;;; ============================================================

(defmacro defgeneric/conflict (name lambda-list &body options)
  "Define a generic function with conflict-detecting semantics.
   This is like DEFGENERIC but uses conflict detection instead of
   standard CLOS linearization for method resolution."
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
      (cond
        ((null primary)
         (format t "  Primary: NONE~%"))
        ((resolution-conflict-p primary)
         (format t "  Primary: CONFLICT~%")
         (format t "    Contributors:~%")
         (dolist (c (resolution-contributors primary))
           (format t "      ~S -> ~S~%"
                   (class-name (car c))
                   (cdr c))))
        ((resolution-method primary)
         (format t "  Primary: ~S (from ~S)~%"
                 (resolution-method primary)
                 (class-name (resolution-source-class primary))))
        (t
         (format t "  Primary: NONE~%"))))

    (flet ((show-methods (label methods)
             (format t "  ~A (~D):~%" label (length methods))
             (dolist (m methods)
               (format t "    from ~S~%"
                       (class-name (first (cs-method-specializers m)))))))
      (show-methods "Before" (composite-resolution-before composite))
      (show-methods "After" (composite-resolution-after composite))
      (show-methods "Around" (composite-resolution-around composite)))

    (values)))

(defun list-delegations (gf)
  "List all delegations for GF."
  (format t "~&Delegations for ~S:~%" (cs-generic-function-name gf))
  (if (zerop (hash-table-count (gf-delegation-table gf)))
      (format t "  (none)~%")
      (maphash (lambda (key value)
                 (format t "  ~S~@[ (~S)~] -> ~S~%"
                         (first key) (second key) value))
               (gf-delegation-table gf)))
  (values))

;;; ============================================================
;;; End of conflict-semantics.lisp
;;; ============================================================
