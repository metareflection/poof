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
