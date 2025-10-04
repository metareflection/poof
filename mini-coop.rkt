#lang racket

(define (Y f) (letrec ((ff (lambda (i) (f ff i)))) ff))
(define ⊤ (lazy (error "bottom")))

(define (instantiate s) (Y (λ (m i) ((s m i) ⊤))))
(define ((inherit c p) m i) (compose (c m i) (p m i)))
(define (compose f g) (lambda (x) (f (g x))))

(define ((coord-spec self i) super)
  (case i ((x) 2) ((y) 4) (else super)))
(define ((color-spec self i) super)
  (case i ((color) 'blue) (else super)))

(define point-p (instantiate
  (inherit coord-spec color-spec)))

(define-syntax test (syntax-rules () ((_ x) (begin (display 'x) (display " ⇒ ") (display x) (newline)))))

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

(define point-q (instantiate
                   (inherit rho-spec (inherit (add-x-spec 1) coord-spec))))

(test (point-q 'x)) ;; ⇒ 3
(test (point-q 'rho)) ;; ⇒ 5
