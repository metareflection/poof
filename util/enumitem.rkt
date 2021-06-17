#lang racket/base

(provide enumparenalph)

(require racket/runtime-path
         (only-in scribble/core style)
         (only-in scribble/latex-properties tex-addition))

(define-runtime-path enumitem.tex "../tex/enumitem.tex")

(define enumparenalph
  (style "enumparenalph" `(,(tex-addition enumitem.tex))))
