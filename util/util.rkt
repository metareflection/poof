#lang racket ;; -*- Scheme -*-

(provide (all-defined-out))

(require (only-in scribble/base elem linebreak)
         (only-in scribble/core make-paragraph make-style)
         (only-in scriblib/footnote note)
         (only-in scriblib/render-cond cond-element))

(require scribble/html-properties)
#;
(define-syntax-rule (when-not condition body ...)
  (when (not condition) body ...))

(define-syntax-rule (when/list condition body ...)
  (if condition (begin body ...) '()))

(define-syntax-rule (tex-only content)
  (cond-element (latex content) (else '())))
(define-syntax-rule (html-only content)
  (cond-element (html content) (else '())))

(define (pretitle content) (make-paragraph (make-style 'pretitle '()) content))
(define (tex . args) (tex-only (apply elem #:style (make-style #f '(exact-chars)) args)))
(define (noindent) (tex "\\noindent"))

;; TODO: find how to insert raw html
;;(define (Html . args) (html-only (apply elem #:style (make-style #f '(exact-chars)) args)))
(define (hhr) (html-only (elem #:style (make-style #f (list (make-alt-tag "hr"))))))

(define (xnote x . y)
  (list (apply note (append y (list (html-only (list (linebreak) (linebreak) (linebreak))))))
        x))
