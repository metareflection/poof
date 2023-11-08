#lang racket ;; -*- Scheme -*-

(provide (all-defined-out)) ;; export everything for now... will refine later

(define-syntax define-identifier-macro
  (syntax-rules ()
    ((_ name symbol-expr list-expr)
     (define-syntax (name stx)
       (syntax-case stx ()
         ((_ . x) #'(list-expr . x))
         (_ #'symbol-expr))))
    ((_ name expr)
     (define-identifier-macro name expr expr))))

(include "coop.scm")
