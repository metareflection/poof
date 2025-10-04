#lang racket
;; style: stateful Y, multiple arities with care, unit value for top

(define-syntax test
  (syntax-rules () ((_ x) (begin (display 'x) (display " ⇒ ") (display x) (newline)))))
(define (Y f) (letrec ((fp (lambda (x) (f fp x)))) fp))
(define ⊤ (void))

(define (instantiate s) (Y (λ (m i) ((s m i) ⊤))))
(define ((inherit c p) m i) (compose (c m i) (p m i)))
(define (compose f g) (lambda (x) (f (g x))))

(define ((coord-spec self i) super)
  (case i ((x) 2) ((y) 4) (else super)))
(define ((color-spec self i) super)
  (case i ((color) 'blue) (else super)))
(define point-p (instantiate
  (inherit coord-spec color-spec)))
(test (point-p 'x)) ;; 2
(test (point-p 'color)) ;; ⇒ blue

(define (((add-x-spec dx) self i) super)
  (case i ((x) (+ dx super))
          (else super)))
(define ((rho-spec self i) super)
  (case i ((rho)
           (sqrt (+ (sqr (self 'x))
                    (sqr (self 'y)))))
          (else super)))
(define point-r (instantiate
                   (inherit rho-spec (inherit (add-x-spec 1) coord-spec))))
(test (point-r 'x)) ;; ⇒ 3
(test (point-r 'rho)) ;; ⇒ 5
