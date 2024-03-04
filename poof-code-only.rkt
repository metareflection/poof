#lang racket
;; Code from poof.scrbl, manually extracted

(provide (all-defined-out))

(require (only-in racket/base [define def])
         syntax/parse/define
         srfi/1)

(define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
(define (mix c p) (lambda (f b) (c f (p f b))))

#; ;;; Note that we don't need to support vararg functions, supporting unary functions is enough:
(define (fix p b) (define f (p (lambda (x) (f x)) b)) f)


;; (deftype (Proto Self Super) (Fun Self Super -> Self st: (<: Self Super)))
;; fix : (Fun (Proto Self Super) Super -> Self st: (<: Self Super))
;; mix : (Fun (Proto Self Super) (Proto Super Sup2) -> (Proto Self Sup2))

;; $slot : (Fun k:Symbol V -> (Proto (Fun 'k -> V | A) A))
(define ($slot k v) ;; k v: constant key and value for this defined slot
  (λ (self super) ;; self super: usual prototype variables
    (λ (msg) ;; msg: message received by the instance, a.k.a. method name.
      (if (equal? msg k) v ;; if the message matches the key, return the value
        (super msg))))) ;; otherwise, recurse to the super instance

;; $slot-modify : (Fun k:Symbol (Fun V -> W)
;;   -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))
(define ($slot-modify k modify) ;; modify: function from super-value to value
  (λ (self super) (λ (msg)
    (if (equal? msg k) (modify (super msg))
      (super msg)))))

;; $slot-compute : (Fun k:Symbol (Fun A -> V) -> (Proto (Fun 'k -> V | A) A))
(define ($slot-compute k fun) ;; fun: function from self to value.
  (λ (self super)
    (λ (msg)
      (if (equal? msg k) (fun self)
        (super msg)))))

;; $slot-gen : (Fun k:Symbol (Fun A (Fun -> V) -> W)
;;   -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))
(define ($slot-gen k fun) ;; fun: from self and super-value thunk to value.
  (λ (self super)
    (λ (msg)
      (define (inherit) (super msg))
      (if (equal? msg k) (fun self inherit) (inherit)))))

;; bottom-record : (Fun Symbol -> _)
(define (bottom-record msg) (error "unbound slot" msg))

;; instantiate-prototype : (Fun (Proto Self Super) Super -> Self)
(define (instantiate-prototype prototype base-super)
  (define self (prototype (λ i (apply self i)) base-super))
  self)

;; compose-prototypes : (Fun (Proto Self Super) (Proto Super Super2)
;;   -> (Proto Self Super2) st: (<: Self Super Super2))
(define (compose-prototypes child parent)
  (λ (self super2) (child self (parent self super2))))

;; identity-prototype : (Proto Instance Instance)
(define (identity-prototype self super) super)

;; compose-prototype-list : (Fun (IndexedList I (λ (i)
;;   (Proto (A_ i) (A_ (1+ i))))) -> (Proto (A_ 0) (A_ (Card I))))
(define (compose-prototype-list prototype-list)
  (foldr compose-prototypes identity-prototype prototype-list))
;; instantiate-prototype-list : (Fun (IndexedList I (λ (i)
;;   (Proto (A_ i) (A_ (1+ i))))) (A_ (Card I)) -> (A_ 0))
(define (instantiate-prototype-list prototype-list base-super)
  (instantiate-prototype (compose-prototype-list prototype-list) base-super))

#; ;; Alternatively:
(define (compose-prototype-list l)
  (cond
   ((null? l) identity-prototype)
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (compose-prototypes (car l) (cadr l)))
   (else (compose-prototypes (car l) (compose-prototype-list (cdr l))))))

(define ($compose . proto-list) (compose-prototype-list proto-list))

;; bottom : (Fun I ... -> O ...)
(define (bottom . args) (error "bottom" args))

;; instance : (Fun (IndexedList I (λ (i) (Proto (A_ i) (A_ (1+ i)))))...
;;   -> (A_ 0)))
(define (instance . prototype-list)
  (instantiate-prototype-list prototype-list bottom))

;; $constant-prototype : (Fun A -> (Proto A _))
(define ($constant-prototype base-super) (λ (__self __super) base-super))

;; Or the same with a shorter name and a familiar definition as a combinator
;; $const : (Fun A -> (Proto A _))
(define ($const b) (λ _ b))

;; (deftype (Generator A) (Fun A -> A))
;; fix-generator : (Fun (Generator A) -> A)
(define (fix-generator g) (define f (g (λ i (apply f i)))) f)
;; proto->generator : (Fun (Proto A B) B -> (Generator A))
(define (proto->generator p b) (λ (f) (p f b)))
;; (== (fix-generator (proto->generator p b)) (fix p b))
;; apply-proto : (Fun (Proto A B) (Generator B) -> (Generator A))
(define (apply-proto p g) (λ (f) (p f (g f))))
;; (== (apply-proto p (proto->generator q b)) (proto->generator (mix p q) b))

(define ($number-order self super)
  (λ (msg) (case msg ((<) (λ (x y) (< x y)))
                     ((=) (λ (x y) (= x y)))
                     ((>) (λ (x y) (> x y)))
                     (else (super msg)))))
(define ($string-order self super)
  (λ (msg) (case msg ((<) (λ (x y) (string<? x y)))
                     ((=) (λ (x y) (string=? x y)))
                     ((>) (λ (x y) (string>? x y)))
                     (else (super msg)))))

(define ($compare<-order self super)
  (λ (msg) (case msg
             ((compare) (λ (x y) (cond (((self '<) x y) '<)
                                       (((self '>) x y) '>)
                                       (((self '=) x y) '=)
                                       (else (error "incomparable" x y)))))
             (else (super msg)))))
(define number-order (instance $number-order $compare<-order))
(define string-order (instance $string-order $compare<-order))

(define ($symbol-order self super)
  (λ (msg) (case msg
             ((< = > compare)
              (λ (x y) ((string-order msg) (symbol->string x) (symbol->string y))))
             (else (super msg)))))
(define symbol-order (instance $symbol-order))

(define ($binary-tree-map self super)
  (λ (msg)
    (define (node l kv r) ((self 'node) l kv r))
    (case msg
      ((empty) '())
      ((empty?) null?)
      ((node) (λ (l kv r) (list l (list kv) r)))
      ((singleton) (λ (k v) (node '() (cons k v) '())))
      ((acons)
       (λ (k v t)
         (if ((self 'empty?) t) ((self 'singleton) k v)
             (let* ((tl (car t)) (tkv (caadr t)) (tk (car tkv)) (tr (caddr t)))
               (case (((self 'Key) 'compare) k tk)
                 ((=) (node tl (cons k v) tr))
                 ((<) (node ((self 'acons) k v tl) tkv tr))
                 ((>) (node tl tkv ((self 'acons) k v tr))))))))
      ((ref)
       (λ (t k e)
         (if ((self 'empty?) t) (e)
             (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
               (case (((self 'Key) 'compare) k tk)
                 ((=) tv)
                 ((<) ((self 'ref) tl k e))
                 ((>) ((self 'ref) tr k e)))))))
      ((afoldr)
       (λ (acons empty t)
         (if ((self 'empty?) t) empty
           (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
             ((self 'afoldr)
              acons (acons tk tv ((self 'afoldr) acons empty tl)) tr)))))
      (else (super msg)))))

(define symbol-tree-map (instance ($slot 'Key symbol-order) $binary-tree-map))

(define ($avl-tree-rebalance self super)
  (λ (msg)
    (define (left t) (car t))
    (define (kv t) (caadr t))
    (define (height t) (if (null? t) 0 (cdadr t)))
    (define (right t) (caddr t))
    (define (balance t) (if (null? t) 0 (- (height (right t)) (height (left t)))))
    (define (mk l kv r)
      (let ((lh (height l)) (rh (height r)))
        (or (member (- rh lh) '(-1 0 1)) (error "tree unbalanced!"))
        (list l (cons kv (+ 1 (max lh rh))) r)))
    (define (node l ckv r)
      (case (- (height r) (height l))
        ((-1 0 1) (mk l ckv r))
        ((-2) (case (balance l)
                ((-1 0) (mk (left l) (kv l) (mk (right l) ckv r))) ;; LL rebalance
                ((1) (mk (mk (left l) (kv l) (left (right l))) ;; LR rebalance
                         (kv (right l)) (mk (right (right l)) ckv r)))))
        ((2) (case (balance r)
               ((-1) (mk (mk l ckv (left (left r))) ;; RL rebalance
                         (kv (left r)) (mk (right (left r)) (kv r) (right r))))
               ((0 1) (mk (mk l ckv (left r)) (kv r) (right r))))))) ;; RR rebalance
    (case msg ((node) node) (else (super msg)))))
(define Dict
  (instance $avl-tree-rebalance $binary-tree-map ($slot 'Key symbol-order)))

;; (deftype (δProto Self Super) (Fun (Delayed Self) (Delayed Super) -> Self))
;; δfix : (Fun (δProto Self Super) (Delayed Super) -> (Delayed Self))
(define (δfix p b) (define f (delay (p f b))) f)
;; δmix : (Fun (δProto Self Mid) (δProto Mid Super) -> (δProto Self Super))
(define (δmix c p) (λ (f b) (c f (delay (p f b)))))
;; δ$id : (δProto X X)
(define (δ$id f b) (λ (__self super) (force super)))

;; (deftype (MProto A) (Fun A A -> A))
;; fix : (Fun (MProto A) A -> A)
;; mix : (Fun (MProto A) (MProto A) -> (MProto A))

(define ($slot-gen/keys k fun)
  (λ (self super)
    (λ (msg) (cond ((equal? msg k) (fun self (λ () (super msg))))
                   ((equal? msg 'keys) (cons k (super 'keys)))
                   (else (super msg))))))

(define ($slot-gen/dict k fun)
  (λ (self super)
    (define (inherit) ((Dict 'ref) (super) k bottom))
    (λ () ((Dict 'acons) k (fun self inherit) (super)))))

(define (δ$slot-gen/dict k fun)
  (λ (self super)
    (delay (let ((inherit ((Dict 'ref) (force super) k (λ () (delay (bottom))))))
             ((Dict 'acons) k (delay (fun self inherit)) (force super))))))

(define (mix/pk child parent)
  (cons (mix (car child) (car parent))
        (append (cdr child) (cdr parent))))
(define (fix/pk proto base)
  (fix (mix ($slot 'keys (cdr proto)) (car proto)) base))

(define (make-object instance prototype) (cons instance prototype))
(define (object-instance object) (car object))
(define (object-prototype object) (cdr object))

(define (slot-ref object slot)
  (force ((Dict 'ref) (force (object-instance object)) slot bottom)))
(define ($slot/gen k fun)
  (λ (self super)
    (make-object ((Dict 'acons) k (fun self (delay (slot-ref super k)))
                  (object-instance (force super)))
                 (object-prototype (force super)))))
(define ($slot/value k v) ($slot/gen k (λ (__self __inherit) (delay v))))
(define ($slot/modify k modify)
  ($slot/gen k (λ (__ inherit) (delay (modify (force inherit))))))
(define ($slot/compute k fun) ($slot/gen k (λ (self __) (delay (fun self)))))

(define ($slot/values . kvs)
  (if (null? kvs) identity-prototype
    (compose-prototypes ($slot/value (car kvs) (cadr kvs))
                        (apply $slot/values (cddr kvs)))))


;; The (require srfi/1) below imports SRFI 1 list functions into Racket
;; YMMV if you use another Scheme implementation
(require srfi/1)

;; not-null? : Any -> Bool
(define (not-null? l) (not (null? l)))

;; remove-nulls : (List (List X)) -> (List (NonEmptyList X))
(define (remove-nulls l) (filter not-null? l))

;; remove-next : X (List (NonEmptyList X)) -> (List (NonEmptyList X))
(define (remove-next next tails)
  (remove-nulls (map (λ (l) (if (equal? (car l) next) (cdr l) l)) tails)))

;; c3-compute-precedence-list : A (A -> (List A)) (A -> (NonEmptyList A))
;;   -> (NonEmptyList A)
(define (c3-compute-precedence-list x get-supers get-precedence-list)
  (define supers (get-supers x)) ;; : (List A)
  (define super-precedence-lists (map get-precedence-list supers)) ;; : (List (NonEmptyList A))
  (define (c3-select-next tails) ;; : (NonEmptyList (NonEmptyList A)) -> A
    (define (candidate? c) (every (λ (tail) (not (member c (cdr tail)))) tails)) ;; : A -> Bool
    (let loop ((ts tails))
      (when (null? ts) (error "Inconsistent precedence graph"))
      (define c (caar ts))
      (if (candidate? c) c (loop (cdr ts)))))
  (let loop ((rhead (list x)) ;; : (NonEmptyList X)
             (tails (remove-nulls (append super-precedence-lists (list supers))))) ;; : (List (NonEmptyList X))
    (cond ((null? tails) (reverse rhead))
          ((null? (cdr tails)) (append-reverse rhead (car tails)))
          (else (let ((next (c3-select-next tails)))
                  (loop (cons next rhead) (remove-next next tails)))))))

(define (alist->Dict alist)
  (foldl (λ (kv a) ((Dict 'acons) (car kv) (cdr kv) a)) (Dict 'empty) alist))

(define (Dict->alist dict)
  ((Dict 'afoldr) (λ (k v a) (cons (cons k v) a)) '() dict))

(define (Dict-merge override-dict base-dict)
  ((Dict 'afoldr) (Dict 'acons) base-dict override-dict))

(define (Dict->Object dict) (make-object dict '()))
(define (object-prototype-function object)
  (define prototype (object-prototype object))
  (if (null? prototype)
    (λ (self super)
      (make-object (Dict-merge (object-instance object) (object-instance super))
                   (object-prototype super)))
    (slot-ref prototype 'function)))
(define (object-supers object)
  (define prototype (object-prototype object))
  (if (null? prototype) '() (slot-ref prototype 'supers)))
(define (object-precedence-list object)
  (define prototype (object-prototype object))
  (if (null? prototype) '() (slot-ref prototype 'precedence-list)))
(define (compute-precedence-list object)
  (c3-compute-precedence-list object object-supers object-precedence-list))
(define base-dict (Dict 'empty))
(define (instantiate supers function)
  (define proto
    (Dict->Object ((Dict 'acons) 'function (delay function)
                   ((Dict 'acons) 'supers (delay supers)
                    ((Dict 'acons) 'precedence-list (delay precedence-list)
                     (Dict 'empty))))))
  (define base (make-object base-dict proto))
  (define precedence-list (compute-precedence-list base))
  (define prototype-functions (map object-prototype-function precedence-list))
  (instantiate-prototype-list prototype-functions (delay base)))
(define (object function . supers) (instantiate supers function))

#;(define Top (object ($slot/value 'is? (λ (_) #t))))
#;(define Bottom (object ($slot/value 'is? (λ (_) #f))))

#;(define Representable (object ($slot/value '->sexp (λ (x) (list 'quote x))) Top))

(define Number (object ($slot/values 'is? number? '->sexp identity
                          '+ + '- - 'zero 0 'one 1)))

(define (list-of? t x) (or (null? x)
  (and (pair? x) ((slot-ref t 'is?) (car x)) (list-of? t (cdr x)))))
(define (ListOf t) (object ($slot/value 'is? (λ (x) (list-of? t x)))))

(define (fix! p b) (define f (hash-copy b)) (p f) f)
(define (mix! p q) (λ (f) (q f) (p f)))
(define ($slot-gen! k fun)
  (λ (self) (define inherit (hash-ref self k (delay (bottom))))
            (hash-set! self k (fun self inherit))))

;; II.1.2- Memoizing values, so slot access is not O(n) every time.
;; Usage: Put memoize-proto as first prototype.
;; NB1: Memoization uses side-effects internally, but does not expose them.
;; NB2: It's still O(n^2) overall rather than O(n); we can do better, later.
(define (memoize f)
  (let ((cache (make-hash)))
    (λ x (apply values (hash-ref! cache x (λ () (call-with-values (λ () (apply f x)) list)))))))

(define (make-counter)
  (let ((count 0))
    (λ () (let ((result count)) (set! count (+ count 1)) result))))

(define (memoize-proto self super) (memoize super))

(define (count-proto self super)
  (make-counter))

(define (pair-tree-for-each! x f)
  (let loop ((x x))
    (cond ((pair? x) (loop (car x)) (loop (cdr x)))
          ((null? x) (void))
          (else (f x)))))
(define (call-with-list-builder f)
  (define l '())
  (f (λ (x) (set! l (cons x l))))
  (reverse l))
(define (flatten-pair-tree x)
  (call-with-list-builder (λ (c) (pair-tree-for-each! x c))))

(define ($lens-gen setter getter wrapper method)
  (λ (cooked-self raw-super)
    (setter ((wrapper method) cooked-self (delay (getter raw-super)))
            raw-super)))

;; (deftype (ObjectWrapper Raw Cooked)
;;   (Forall (Object) (Fun (Raw Object) -> (Cooked Object))))
;; (deftype (CookedProto Cooked)
;;   (Forall (ObjectSuper ObjectSelf MethodSuper MethodSelf) ; s t a b
;;     (Fun (Cooked ObjectSelf) (Delayed MethodSuper) -> MethodSelf)))
;; (deftype (MethodSetter Raw)
;;   (Forall (ObjectSuper ObjectSelf MethodSelf) ; s t b
;;     (Fun MethodSelf (Raw ObjectSuper) -> (Raw ObjectSelf))))
;; (deftype (MethodGetter Raw)
;;   (Forall (Object Method) ; s a
;;     (Fun (Raw Object) -> Method)))
;; (deftype (MethodWrapper Cooked RawProto)
;;   (Fun (RawProto Cooked) -> (CookedProto Cooked)))

;; (deftype (DelayedProto A B) (Fun (Delayed A) (Delayed B) -> A))
;; delayed-fix : (Fun (DelayedProto A B) (Delayed B) -> (Delayed A))
