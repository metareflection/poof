(displayln "3. General Prototypes")

(displayln "3.1. General Prototypes")

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



;;;; Chapter 3. Trivial objects, the naive OO on top of poof
(displayln "3. Prototypes for non-object functions")

;; Prototypes and prototype inheritance is not just for "objects",
;; i.e. functions taking a symbol as a message argument,
;; to return the value of a field named by that symbol.
;; Prototypes and prototype inheritance can be used for any function type.
;; Here, we will illustrate how to use prototypes to define real functions,
;; from reals to reals.
;; We'll use "rfp" to mean "real function prototype".

(displayln "3.1.1. A prototype for the identity function.")
(define (just x) (lambda _ x))

(define id-rfp (just (lambda (x) x)))

(define id (instance id-rfp))
(check! (= (id 42) 42))

(displayln "3.1.2. Prototypes to incrementally specify numeric functions.")

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
