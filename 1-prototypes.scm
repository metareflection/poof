;;;; I. Function prototypes, in the most universal sense

(displayln "I. Defining function prototypes")

;;; --------------------------------------------------------------------------
;; I.1. (Prototype) Object Orientation in 109 characters:
(define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
(define (mix p q) (lambda (f b) (p f (q f b))))
;; The two functions above sum up the essence of OO. The rest in commentary.

;; Note for code minimalists: this isn't valid in Chez Scheme and many
;; other dialects, but in MIT Scheme or Gerbil Scheme, we could write:
;(define ((mix p q) f b) (p f (q f b)))
;; And then we'd have Object Orientation in 100 characters only.
;; Then again, in Gerbil Scheme, we could get down to only 86:
;(def (fix p b) (def f (p (λ i (apply f i)) b)) f) (def ((mix p q) f b) (p f (q f b)))
;;Or, compressing spaces, 74 (including newline, excluding semi-colon):
;(def(fix p b)(def f(p(λ i(apply f i))b))f)(def((mix p q)f b)(p f(q f b)))

;;; Now for the commentary.

;;; --------------------------------------------------------------------------
;; I.2. Types for prototypes
;;
;; A function prototype of A from B is a way to get a function of type A
;; starting from a base function of type B, where A is a subtype of B.
;;
;; In a typical use of a prototype object system, function of types A and B
;; will both map field names (symbols) to values of an appropriate types for
;; each field. A being a subtype of B means that A specifies more fields and/or
;; fields with stricter types than B. But the definitions in this file are
;; more general than that and work for any function types with a subtype
;; relation, with a trivial monomorphic case where we always have A=B,
;; and all functions involved have the same arbitrary function type A.
;;
;; In a suitable type system, it is an entity of a type as follows:
#;(deftype (Proto A B) (Fun A <- A B) st: (<: A B Function))
;; where
;;   (Fun O ... <- I ...)
;;     is the type of functions multiple yielding output values of types O ...
;;     from input values of types I ...
;; where (st: being a keyword short for "such that")
;;   st: Constraint1 Constraint2 ...
;; specifies constraints on the defined type, and
;; where
;;   (<: A B C ...)
;; is a subtyping constraint that A is a subtype of B, etc.
;;
;; Thus a function prototype for A from B computes an A given an A and a B.
;; Conceptually, the prototype is partial information about the computation of
;; a function of type A, than given given some (other?) function of type A and
;; a base function of type B, contributes to complete a computation of type A.

;; The purpose of a prototype of A from B is to be able to extract a function
;; of type A from the prototype and a base function of type B.
;; But since the prototype already requires a function of type A as argument,
;; how do we get the first argument of type A to provide to the prototype,
;; to begin with? Using a *fixed-point* function. The first argument will be
;; the fixed-point of the computation, passed as argument, to only be called
;; recursively with "simpler" arguments, or passed as function value to be
;; called later with "simpler" arguments. For whatever notion of "simpler"
;; allows the computation to make progress towards termination (or continued
;; operation) without forever recursing on the same computation.

;;; --------------------------------------------------------------------------
;; I.3. Instantiating a function prototype through a fixed-point.
;;
;; The fixed-point function fix, defined above, with have type as follows:
#;(: fix (Fun A <- (Proto A B) B))
;;
;; To understand that definition, let's first consider the simplest case for
;; functions: nullary functions taking no arguments, a.k.a thunks. Thus we
;; consider prototypes for functions of no arguments, yielding some values
;; of types OA ..., and B is also for thunks, yielding values of types OB ...,
;; where types OA ... are subtypes of OB ....
;;
;; We could define the fixed point operator as follows, where
;; f is the fixed point of type A, b is the base function of type B:
(define (fix/0--1 p b)
  (letrec ((f (lambda () ((p f b)))))
    f))
;; I find using define clearer than using letrec, so here is the equivalent
;; definition for fix/0 using define:
(define (fix/0--2 p b)
  (define (f) ((p f b)))
  f)
;; The problem with this function is that every time that the fixed-point f is
;; called it calls p again and redoes the entire computation from scratch.
;; There can be no sharing of information between calls to f:
;; no pre-computation, no cacheing, no memoization, no shared mutable state.

;; Thus, a slightly better fixed-point function only calls p once,
;; when initially computing the fixed-point, at which point
;; the prototype can setup all the sharing it wants:
(define (fix/0--3 p b)
  (letrec ((f (p (lambda () (f)) b)))
    f))
;; As you can see, the internal (lambda () (f)) is just the eta-expansion of f
;; itself, which is functionally equivalent to f itself. Wrapping f in this
;; closure is necessary to delay the evaluation in the fixed point;
;; otherwise p might be called with an unbound, default (void) or invalid value
;; from before f itself was intialized.

;; Our final variant of a fixed-point function for nullary function prototypes
;; is just the same as the above fix/0--3 using define instead of letrec:
(define (fix/0 p b)
  (define f (p (lambda () (f)) b))
  f)

;; Similarly, for unary and binary functions, we have:
(define (fix/1--1 p b) (letrec ((f (lambda (x) ((p f b) x)))) f)) ;; unary case, call p every time
(define (fix/1--2 p b) (define (f x) ((p f b) x)) f) ;; same as fix/1--1 above.
(define (fix/1--3 p b) (letrec ((f (p (lambda (x) (f x)) b))) f)) ;; unary case, call p only once
(define (fix/1 p b) (define f (p (lambda (x) (f x)) b)) f) ;; same as fix/1--3 above.

(define (fix/2--1 p b) (letrec ((f (lambda (x y) ((p f b) x y)))) f)) ;; binary case, call p every time
(define (fix/2--2 p b) (define (f x y) ((p f b) x y)) f) ;; same as fix/2--1 above.
(define (fix/2--3 p b) (letrec ((f (p (lambda (x y) (f x y)) b))) f)) ;; binary case, call p once
(define (fix/2 p b) (define f (p (lambda (x y) (f x y)) b)) f) ;; same as fix/2--3 above.

;; And for the general, n-ary, case, we have:
(define (fix--1 p b) (letrec ((f (lambda i (apply (p f b) i)))) f)) ;; general n-ary case, call p every time
(define (fix--2 p b) (define (f . i) (apply (p f b) i)) f) ;; same as fix--1 above.
(define (fix--3 p b) (letrec ((f (p (lambda i (apply f i)) b))) f)) ;; general n-ary case, call p once
(define (fix--4 p b) (define f (p (lambda i (apply f i)) b)) f) ;; same as fix--3 above and as fix on top

;;; --------------------------------------------------------------------------
;; I.4. Composing prototypes via prototype inheritance
;;
;; What makes function prototypes interesting is that they can be composed,
;; in a process called *prototype inheritance*, a.k.a. mixing, such that each
;; prototype can incrementally contribute part of the computation,
;; e.g. define or refine the values associated with some particular fields.
;; Mixing is done through the above-defined function mix of type as follows:
#;(: mix (Fun (Proto A C) <- (Proto A B) (Proto B C)))

;; The same function with more readable names could be defined as follows:
(define (compose-proto this parent)
  (lambda (self super) (this self (parent self super))))
;; Note the types of the variables and intermediate expressions:
; this : (Proto A B)
; parent : (Proto B C)
; self : A
; super : C
; (parent self super) : B
; (this self (parent self super)) : A
;; When writing long-form functions instead of vying for conciseness, we will
;; use the same naming conventions as in the function above:
;; - this for a prototype at hand, in leftmost position;
;; - parent for a prototype it is being mixed with, in latter position;
;; - self for the intended fixed point of the computation;
;; - super for the base (or so-far accumulated) value of the computation.

;; Mixing is associative, and has the following neutral element:
;; identity-proto : (Proto A A)
(define (identity-proto _self super) super)

;; Mixing prototypes doesn't in general commute: the computations of the
;; first (child, leftmost) prototype can override (ignore) or call and transform
;; computations of the second (parent, super, rightmost) prototype.
;;
;; As an example, consider these nullary prototypes for thunks yielding integers:
(define (b0) 0)
(define (p1+ _ b) (lambda () (+ 1 (b))))
(define (p2* _ b) (lambda () (* 2 (b))))
(check! (= ((fix (mix p1+ p2*) b0)) 1))
(check! (= ((fix (mix p2* p1+) b0)) 2))

;; We can also mix a list of prototypes, assuming each prototype in the list is
;; of a type suitable to be mixed with the previous or next one as applicable.
;; The operation can be typed with some indexed types, which could be expressed
;; with full dependent types, but also presumably with suitable staged types.
;; We will write this type as follows, using "obvious" conventions the precise
;; decoding of which is left as an exercise to the reader:
#;(: compose-proto*
     (Fun (Proto (A_ 0) (A_ (Card I)))
       <-  (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i)))))))
(define (compose-proto* l)
  (cond
   ((null? l) identity-proto)
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (compose-proto (car l) (cadr l)))
   (else (lambda (a b) ((car l) a ((compose-proto* (cdr l)) a b))))))

;; The above could also have been written as follows:
(define (compose-proto*--1 l) (foldr mix identity-proto l))


;;; --------------------------------------------------------------------------
;; I.5. A universal base function

;; We can define a bottom / zero element as a universal base function
;; that is member of all function types:
#;(: bottom (Fun O ... <- I ...))
(define (bottom . x) (apply error "bottom" x))

;; Then, we instantiate the combination of a bunch of prototypes in one go:
#;(: instance (Fun (A_ 0) <- (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i)))))...))
(define (instance . prototypes) (fix (compose-proto* prototypes) bottom))
