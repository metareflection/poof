;;;; Chapter I. Function prototypes, in the most universal sense

(displayln "I. Defining function prototypes")

;;; --------------------------------------------------------------------------
;; I.1. (Prototype) Object Orientation in 109 characters:
(define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
(define (mix p q) (lambda (f b) (p f (q f b))))
;; The two functions above sum up the essence of OO. The rest in commentary.

;;; Now for commentary... the rest of this repository.

;; I.1.1. Note for code minimalists.
;; This isn't valid in Chez Scheme and many other dialects, but in MIT Scheme
;; or in other dialects after it including Gerbil Scheme, we could write:
;(define ((mix p q) f b) (p f (q f b)))
;; And then we'd have Object Orientation in 100 characters only.
;; Then again, in Gerbil Scheme, we could get it down to only 86:
;(def (fix p b) (def f (p (λ i (apply f i)) b)) f) (def ((mix p q) f b) (p f (q f b)))
;;Or, compressing spaces, 74 (including newline, excluding semi-colon):
;(def(fix p b)(def f(p(λ i(apply f i))b))f)(def((mix p q)f b)(p f(q f b)))

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
;; In a suitable type system, a function prototype is an entity of a type as follows:
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
;; is the subtyping constraint that A is a subtype of B, etc.
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
;; A more thorough explanation of the fixed-point function is in Appendix A

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

;; If we don't care to specify a base function, we can define a bottom / zero
;; element as a universal base function that is member of all function types:
#;(: bottom (Fun O ... <- I ...))
(define (bottom . x) (apply error "bottom" x))

;; Then, we instantiate the combination of a bunch of prototypes in one go:
#;(: instance (Fun (A_ 0) <- (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i)))))...))
(define (instance . prototypes) (fix (compose-proto* prototypes) bottom))

;; If you do want to use a nicer-behaving function better-base instead of
;; the above bottom function, you can do it by putting at the end of the
;; prototype list you instantiate, as the rightmost element, in tail position,
;; the following prototype:
;;(define (use-better-base f b) better-base)
