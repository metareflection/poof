(displayln "II. General Prototypes")

(displayln "II.1. General Prototypes")




;; I.2- Short explanation of the above formulas
;;
;; 0. "Object Oriented Programming" is all about specifying programs
;;   in small increments of code that each specify part of an entity,
;;   while being able to refer to other parts of the entity
;;   that may use and not specify, their being specified in other increments.
;;
;;   An increment of code specification, we shall call a *prototype*.
;;   The entity obtained by composing together zero or more prototypes,
;;   we shall call an *instance*.
;;   When we compose prototypes, we say that the leftmost one "inherits"
;;   information from the rightmost one. The information in the leftmost
;;   prototype takes priority over that from the rightmost prototype;
;;   The leftmost prototype can override and/or use its inherit information.
;;
;;   In the most general case, we'll call *prototype* a function taking
;;   two parameters `self` and `super`, and returning an extended self.
;;   The `super` embodies information inherited from previous increments,
;;   that the current increment may either/both use and/or override.
;;   The `self` is a parameter that can be used in an "open recursion" scheme,
;;   so any prototype may refer to the end result of taking every prototype
;;   into account, even as more prototypes are added to the mix.
;;
;;   Using the notations from Appendix A, a prototype is a function of type:
#;(deftype (Proto Self Super) (Fun Self Super -> Self st: (<: Self Super)))
;;   Where `Self` is the type of `self` and `Super` the type of `super`,
;;   and the constraint is that `Self` is a subtype of `Super`.

;; 1. Instantiating a prototype is computing a fixed-point for this self,
;; as per function fix, which as the type:
#;(: fix (Fun (Proto Self Base) Base -> Self))
;; With longer variable names, we could redefine it as such:
(define (instantiate-prototype prototype base)
  (define self (prototype self base))
  self)
;; In other words, given a base value at the start of the recursion, find
;; the fixed-point self that recursively applies the partial information
;; in the prototype, thus "tying the knot" of the open recursion, such that
;; references to the self variable in the prototype shall indeed refer to
;; the final result of the instantiation process, the instance of the prototype.

;; 2. Combining prototypes by "inheritance" consists in chaining
;;   the computations via the super arguments while computing the same self,
;;   as per function mix.


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
#;(deftype (Proto A B) (Fun A B -> A) st: (<: A B Function))
;;
;; Thus a function prototype for A from B computes an A given an A and a B.
;; Conceptually, the prototype is partial information about the computation of
;; a function of type A, than given given some (other?) function of type A and
;; a base function of type B, contributes to complete a computation of type A.
;;
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



;; I.1.2. Note for code minimalists.
;; This isn't valid in Chez Scheme and many other dialects, but in MIT Scheme
;; or in other dialects after it including Gerbil Scheme, we could write:
;(define ((mix p q) f b) (p f (q f b)))
;; And then we'd have Object Orientation in 100 characters only.
;; Then again, in Gerbil Scheme, we could get it down to only 86:
;(def (fix p b) (def f (p (λ i (apply f i)) b)) f) (def ((mix p q) f b) (p f (q f b)))
;;Or, compressing spaces, 74 (including newline, excluding semi-colon):
;(def(fix p b)(def f(p(λ i(apply f i))b))f)(def((mix p q)f b)(p f(q f b)))

;;; --------------------------------------------------------------------------
;; I.3. Instantiating a function prototype through a fixed-point.
;;
;; The fixed-point function fix, defined above, with have type as follows:
#;(: fix (Fun (Proto A B) B -> A))
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
#;(: mix (Fun (Proto A B) (Proto B C) -> (Proto A C)))

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
     (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i)))))
       -> (Proto (A_ 0) (A_ (Card I)))))
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
#;(: bottom (Fun I ... -> O ...))
(define (bottom . x) (apply error "bottom" x))

;; Then, we instantiate the combination of a bunch of prototypes in one go:
#;(: instance (Fun (IndexedList I (lambda (i) (Proto (A_ i) (A_ (1+ i)))))... -> (A_ 0)))
(define (instance . prototypes) (fix (compose-proto* prototypes) bottom))

;; If you do want to use a nicer-behaving function better-base instead of
;; the above bottom function, you can do it by putting at the end of the
;; prototype list you instantiate, as the rightmost element, in tail position,
;; the following prototype:
;;(define (use-better-base f b) better-base)


;;;; Chapter III. Trivial objects, the naive OO on top of poof
(displayln "III. Prototypes for non-object functions")

;; Prototypes and prototype inheritance is not just for "objects",
;; i.e. functions taking a symbol as a message argument,
;; to return the value of a field named by that symbol.
;; Prototypes and prototype inheritance can be used for any function type.
;; Here, we will illustrate how to use prototypes to define real functions,
;; from reals to reals.
;; We'll use "rfp" to mean "real function prototype".

(displayln "III.1.1. A prototype for the identity function.")
(define (just x) (lambda _ x))

(define id-rfp (just (lambda (x) x)))

(define id (instance id-rfp))
(check! (= (id 42) 42))

(displayln "III.1.2. Prototypes to incrementally specify numeric functions.")

;; A prototype for an even function, that computes for positive values, and
;; returns the image of the opposite for negative values.
(define (even-rfp self super)
  (lambda (x) (if (< x 0) (self (- x)) (super x))))

;; A prototype mixin for squaring the parent value
(define (cube-rfp self super)
  (lambda (x) (let ((y (super x))) (* y y y))))

;; Assembling a function out of prototypes.
(define absx3 (instance even-rfp cube-rfp id-rfp))
(check! (= (absx3 3) 27))
(check! (= (absx3 -2) 8))
(check! (= (absx3 0) 0))
(check! (= (absx3 -1) 1))
