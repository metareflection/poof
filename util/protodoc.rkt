#lang racket ;; -*- Scheme -*-

(provide (all-defined-out)) ;; export everything for now... will refine later

(require
  scribble/html
  "reveal.rkt"
  "util.rkt"
  "coop.rkt")

#; ;; Context -- To Be Removed if all we need is Doc
(define-type Ctx
  (Record
    doc: Doc)) ;; current document

#; ;; Document
(define-type Doc
  (Record
    title: String ;; title of this section (NB: not Xexpr)
    parent: (OrFalse Doc) ;; parent section
    path: (List Nat) ;; path from top document to current section
    slide: Xexpr ;; top slide (thunked)
    sections-r: (List Doc))) ;; subsections of this document

;; Empty / Top value of type doc
;; : Doc
(def top-doc
  (record
    title #f
    parent #f
    path '()
    slide #f
    sections-r '()))

(def ($top-doc self super) top-doc)

;; : Doc -> Doc
(def (doc-top doc)
  (def parent (doc'parent))
  (if parent (doc-top parent) doc))

;; : Doc -> (List Section)
(def (doc-sections doc)
  (if doc (reverse (doc'sections-r)) '()))

;; : Doc -> (List Section)
(def (doc-contents doc)
  (def slide (doc'slide))
  (def sections-contents (map doc-contents (doc-sections doc)))
  (if slide
    (cons (section '() slide) sections-contents)
    sections-contents))

;; : String -> $Proto Doc
(def $title ($kv 'title))

;; : Doc -> Slide -> $Proto Doc
(def $slide ($method 'slide))

(def (parent-title doc)
  (def parent (doc'parent))
  (and parent (parent'title)))

(def (parent-title-para doc)
  (def title (parent-title doc))
  (when/list title
    (p align: 'right valign: 'top (font size: 4 (b title)))))

(def (n-sections doc)
  (length (doc'sections-r)))

(def (plan-slide self super)
  (def plan-title (or (super'title) "Plan"))
  (def current-section (n-sections super))
  (def sections (doc-sections self))
  (def len (length sections))
  (def titles (map (getField 'title) sections))
  (def hilit-titles
    (for/list ((t titles) (i (in-naturals)))
      (if (= i current-section) (b t) t)))
  (list*
   (parent-title-para super)
   (h1 plan-title)
   (~)
   (if (<= len 4)
     (spacing* hilit-titles (~))
     hilit-titles)))

(def ($plan-slide self super)
  (@ rcons 'slide (@ plan-slide (self'parent) (super'parent)) super))

(def ($add-slide title xexpr self super)
  (def doc
    (record
     title title
     slide xexpr
     parent self
     path (cons (n-sections super) (super'path))
     sections-r '()))
  (@ rcons 'sections-r (cons doc (super'sections-r)) super))

(define (docfix . l) (@ fix (apply rcompose l) top-doc))

(def (reveal-doc doc)
  (reveal (doc'title) (doc-contents doc)))
