#lang info

;; ---------------------------------------------------------
;; Package Info

(define collection "poof")
(define deps '("base" "scribble-lib" "rackunit-lib"
               "scribble-abbrevs" "scribble-minted"))

;; ---------------------------------------------------------
;; Collection Info

(define compile-omit-paths 'all)
(define compile-include-files
  '("main.rkt"
    "poof.scrbl"
    "util/examples-module.rkt"
    "util/eval-check.rkt"))
