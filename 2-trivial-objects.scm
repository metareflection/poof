;;;; Chapter II. Trivial objects, the naive OO on top of poof

(displayln "II. Defining trivial object system")

;; II.1. Trivial "objects" as unary functions from keys to values
;;
;; Let's define our "objects" as functions that receive a "message",
;; being a symbol typically bound to variable msg,
;; and depending on which key k the message matches,
;; returns a field value of type (A_ k).
;;
;; Assuming some suitable dependent type notation, we could define
#;(deftype (Object A_) (Fun (A_ k) <- (: k Any)))

;; But first, as a temporary convention, let's use a prefix $
;; to indicate prototypes or functions that return prototypes,
;; and no prefix for an instances.

(displayln "II.1.1. Simple objects with constant field-value mapping")
;; To define an object with field k mapped to constant value v,
;; we could thus use this function:
(define ($field k v) ;; k v: constant key and value for this defined field
  (lambda (self super) ;; self super: usual variables for prototype fixed-point
    (lambda (msg) ;; msg: message received by the object
      (if (equal? msg k) v ;; if the message matches the key, return the value
        (super msg))))) ;; otherwise, recurse to the object's super object

;; Let's test simple objects.
(define foo0 (instance ($field 'foo 0)))
(check! (= (foo0 'foo) 0))

(define x1-y2 (instance ($field 'x 1) ($field 'y 2)))
(check! (= (x1-y2 'x) 1))
(check! (= (x1-y2 'y) 2))

(displayln "II.1.2. methods that refer to other fields")
;; An object prototype may define some fields (in this case, z, rho and theta)
;; by referring to other fields of the object (in this case, x and y):
(define ($polar-from-rect self super)
  (lambda (msg)
    (case msg
      ((z) (+ (self 'x) (* 0+i (self 'y))))
      ((rho) (magnitude (self 'z)))
      ((theta) (angle (self 'z)))
      (else (super msg)))))

;; NB: not that these prototypes commute, since there is no override:
(define my-object2
  (instance ($field 'x 1) $polar-from-rect ($field 'y 2)))
(check! (= (my-object2 'rho) (sqrt 5)))

(displayln "II.2. Using prototypes to incrementally define simple data structures")

;; Let's use prototypes to build some simple data structures.
;; First, write prototypes that offer an abstract over the ability
;; to compare entities of a same type, in this case,
;; either numbers or strings.

(define ($number-order self super)
  (lambda (msg)
    (case msg
      ((<) (lambda (x y) (< x y)))
      ((=) (lambda (x y) (= x y)))
      ((>) (lambda (x y) (> x y)))
      (else (super msg)))))

(define ($string-order self super)
  (lambda (msg)
    (case msg
      ((<) (lambda (x y) (string<? x y)))
      ((=) (lambda (x y) (string=? x y)))
      ((>) (lambda (x y) (string>? x y)))
      (else (super msg)))))

;; Now, add a "mixin" for an compare operator that summarizes in one call
;; the result of comparing two elements of the type being described.
;; A mixin is a prototype meant to extend other prototypes.
;; See how this mixin can be used to extend either of the prototypes above.
;; Also notice how, to refer to other slots in the final instance,
;; we call (self '<) and suches.
(define ($compare<-order self super)
  (lambda (msg)
    (case msg
      ((compare) (lambda (x y)
                   (cond (((self '<) x y) '<)
                         (((self '>) x y) '>)
                         (((self '=) x y) '=)
                         (else (error "incomparable" x y)))))
      (else (super msg)))))

;; Here are two concrete instances, for numbers, and for strings:
(define number-order (instance $number-order $compare<-order))
(define string-order (instance $string-order $compare<-order))

(check! ((number-order '<) 23 42))
(check! (eq? ((number-order 'compare) 8 4) '>))
(check! ((string-order '<) "Hello" "World"))
(check! (eq? ((string-order 'compare) "Foo" "FOO") '>))
(check! (eq? ((string-order 'compare) "42" "42") '=))

;; We can define a order on symbols by delegating strings!
(define ($symbol-order self super)
  (lambda (msg)
    (case msg
      ((< = > compare)
       (lambda (x y) ((string-order msg) (symbol->string x) (symbol->string y))))
      (else (super msg)))))

(define symbol-order (instance $symbol-order))
(check! ((symbol-order '<) 'aardvark 'aaron))
(check! ((symbol-order '=) 'zzz 'zzz))
(check! ((symbol-order '>) 'aa 'a))
(check! (eq? ((symbol-order 'compare) 'alice 'bob) '<))
(check! (eq? ((symbol-order 'compare) 'b 'c) '<))
(check! (eq? ((symbol-order 'compare) 'a 'c) '<))

;; What can we do with that? Well, for instance, a binary tree!

(define ($binary-tree-map self super)
  (lambda (msg)
    (define (node l kv r) ((self 'node) l kv r))
    (case msg
      ((empty) '())
      ((empty?) null?)
      ((node) (lambda (l kv r) (list l (list kv) r)))
      ((singleton) (lambda (k v) (node '() (cons k v) '())))
      ((acons)
       (lambda (k v t)
         (if ((self 'empty?) t) ((self 'singleton) k v)
             (let* ((tl (car t)) (tkv (caadr t)) (tk (car tkv)) (tr (caddr t)))
               (case (((self 'Key) 'compare) k tk)
                 ((=) (node tl (cons k v) tr))
                 ((<) (node ((self 'acons) k v tl) tkv tr))
                 ((>) (node tl tkv ((self 'acons) k v tr))))))))
      ((ref)
       (lambda (k t e)
         (if ((self 'empty?) t) (e)
             (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
               (case (((self 'Key) 'compare) k tk)
                 ((=) tv)
                 ((<) ((self 'ref) k tl e))
                 ((>) ((self 'ref) k tr e)))))))
      ((afoldl)
       (lambda (acons empty t)
         (if ((self 'empty?) t) empty
             (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
               ((self 'afoldl) acons (acons tk tv ((self 'afoldl) acons empty tl)) tr)))))
      (else (super msg)))))

(define ($key-is-symbol self super)
  (lambda (msg) (case msg ((Key) symbol-order) (else (super msg)))))

;; And with this scaffolding, here is a data structure we can use later
;; to differently represent objects!
(define symbol-tree-map (instance $key-is-symbol $binary-tree-map))
(check! (equal? (symbol-tree-map 'Key) symbol-order))

(define my-binary-dict
  (foldl (lambda (kv t) ((symbol-tree-map 'acons) (car kv) (cdr kv) t))
         (symbol-tree-map 'empty) '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))
;; Oh noes! This tree is heavily skewed left, height 5.
(check! (equal? my-binary-dict
                '(((((() ((a . "I")) ()) ((b . "II")) ()) ((c . "III")) ()) ((d . "IV")) ()) ((e . "V")) ())))
;; But it otherwise works
(check! (equal? ((symbol-tree-map 'ref) 'a my-binary-dict bottom) "I"))
(check! (equal? ((symbol-tree-map 'ref) 'b my-binary-dict bottom) "II"))
(check! (equal? ((symbol-tree-map 'ref) 'c my-binary-dict bottom) "III"))
(check! (equal? ((symbol-tree-map 'ref) 'd my-binary-dict bottom) "IV"))
(check! (equal? ((symbol-tree-map 'ref) 'e my-binary-dict bottom) "V"))
(check! (equal? ((symbol-tree-map 'ref) 'z my-binary-dict (lambda () -1)) -1))

;; Example incremental definition of data structures:
;; add automatic rebalancing of your binary trees by just overriding one method!
(define ($avl-tree-rebalance self super)
  (lambda (msg)
    (define (left t) (car t))
    (define (kv t) (caadr t))
    (define (height t) (if (null? t) 0 (cdadr t)))
    (define (right t) (caddr t))
    (define (balance t) (if (null? t) 0 (- (height (right t)) (height (left t)))))
    (define (mk l kv r)
      (define lh (height l))
      (define rh (height r))
      (check! (member (- rh lh) '(-1 0 1)))
      (list l (cons kv (1+ (max lh rh))) r))
    (define (node l ckv r)
      (case (- (height r) (height l))
        ((-1 0 1) (mk l ckv r))
        ((-2) (case (balance l)
                ((-1 0) (mk (left l) (kv l) (mk (right l) ckv r))) ;; LL rebalance
                ((1) (mk (mk (left l) (kv l) (left (right l))) ;; LR rebalance
                         (kv (right l))
                         (mk (right (right l)) ckv r)))))
        ((2) (case (balance r)
               ((-1) (mk (mk l ckv (left (left r))) ;; RL rebalance
                         (kv (left r))
                         (mk (right (left r)) (kv r) (right r))))
               ((0 1) (mk (mk l ckv (left r)) (kv r) (right r))))))) ;; RR rebalance
    (case msg
      ((node) node)
      (else (super msg)))))

;; So, let's repeat our dict example with AVL balanced binary trees...
(define symbol-avl-map (instance $avl-tree-rebalance $binary-tree-map $key-is-symbol))
(check! (equal? (symbol-avl-map 'Key) symbol-order))

(define my-avl-dict
  (foldl (lambda (kv t) ((symbol-avl-map 'acons) (car kv) (cdr kv) t))
         (symbol-avl-map 'empty) '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))
;; Yay! Now this tree is well balanced, height 3!
(check! (equal? my-avl-dict
                '(((() ((a . "I") . 1) ()) ((b . "II") . 2) (() ((c . "III") . 1) ()))
                  ((d . "IV") . 3) (() ((e . "V") . 1) ()))))
;; But it otherwise works
(check! (equal? ((symbol-avl-map 'ref) 'a my-avl-dict bottom) "I"))
(check! (equal? ((symbol-avl-map 'ref) 'b my-avl-dict bottom) "II"))
(check! (equal? ((symbol-avl-map 'ref) 'c my-avl-dict bottom) "III"))
(check! (equal? ((symbol-avl-map 'ref) 'd my-avl-dict bottom) "IV"))
(check! (equal? ((symbol-avl-map 'ref) 'e my-avl-dict bottom) "V"))
(check! (equal? ((symbol-avl-map 'ref) 'z my-avl-dict (lambda () -1)) -1))
