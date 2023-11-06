#lang racket ;; -*- Scheme -*-

(provide (all-defined-out))

#;
(define-syntax-rule (when-not condition body ...)
  (when (not condition) body ...))

(define-syntax-rule (when/list condition body ...)
  (if condition (begin body ...) '()))
