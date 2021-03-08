(displayln "3. Prototypes beyond Objects")
'("
Prototypes are not just for records as functions from symbol to value:
they can be used to incrementally specify any kind of functions!

Indeed, let's see how prototypes can be used to build numeric functions.
")

(displayln "3.1. Prototypes for Numeric Functions")

(displayln "3.1.1. Prototypes to incrementally specify numeric functions.")

;; A prototype for an even function, that computes for positive values, and
;; returns the image of the opposite for negative values.
(define ($even self super)
  (lambda (x) (if (< x 0) (self (- x)) (super x))))

;; A prototype mixin for squaring the parent value
(define ($cube self super)
  (lambda (x) (let ((y (super x))) (* y y y))))

;; Assembling a function out of prototypes.
(define absx3 (instance $even $cube ($const (lambda (x) x))))
(check! (= (absx3 3) 27))
(check! (= (absx3 -2) 8))
(check! (= (absx3 0) 0))
(check! (= (absx3 -1) 1))

(displayln "3.1.2. Number Thunks.")
'("
The simplest numeric functions: thunks (nullary functions) that yield a number.

They are just computations of no argument that yield a number.
")

(define (b0) 0)
(define (p1+ _ b) (lambda () (+ 1 (b))))
(define (p2* _ b) (lambda () (* 2 (b))))
(check! (= ((fix (mix p1+ p2*) b0)) 1))
(check! (= ((fix (mix p2* p1+) b0)) 2))

'("
... Lazy!
... call-by-push-value: computations vs values
... prototypes are partial computations, not partial values.
... instances are the complete computation.
")

