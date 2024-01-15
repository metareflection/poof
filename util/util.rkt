#lang racket ;; -*- Scheme -*-

(provide (all-defined-out))

(require (only-in scribble/base elem)
         (only-in scribble/core make-paragraph make-style))

#;
(define-syntax-rule (when-not condition body ...)
  (when (not condition) body ...))

(define-syntax-rule (when/list condition body ...)
  (if condition (begin body ...) '()))

(define (pretitle content) (make-paragraph (make-style 'pretitle '()) content))
(define (tex . args) (apply elem #:style (make-style #f '(exact-chars)) args))
(define (noindent) (tex "\\noindent"))
