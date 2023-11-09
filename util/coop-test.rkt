#lang racket ;; -*- Scheme -*-

(require
  rackunit
  "coop.ss")

(define (true . x) #t)
(define-syntax-rule (check-exception expr fun)
  (check-exn fun (lambda () expr)))
(define-syntax check
  (syntax-rules (=>)
    ((_ expr => res)
     (check-equal? expr res))))
(define-syntax-rule (import . _) (void))
(define-syntax-rule (export . a) (provide . a))

(include "coop-test.ss")

(require rackunit/text-ui)
(run-tests coop-test 'verbose)
