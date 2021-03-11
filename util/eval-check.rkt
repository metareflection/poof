#lang racket/base

(provide eval:check eval:error)

(require rackunit
         syntax/parse/define
         syntax/srcloc
         (for-syntax racket/base))

(define-simple-macro (eval:check actual:expr expected:expr)
  #:with stx this-syntax
  (with-check-info*
   (list (make-check-name 'eval:check)
         (make-check-location (build-source-location-list (quote-syntax stx)))
         (make-check-expression 'stx))
   (λ () (let ([a actual]) (check-equal? a expected) a))))

(define-simple-macro (eval:error actual:expr)
  #:with stx this-syntax
  (with-check-info*
   (list (make-check-name 'eval:error)
         (make-check-location (build-source-location-list (quote-syntax stx)))
         (make-check-expression 'stx))
   (λ () (check-exn exn:fail? (λ () actual)))))
