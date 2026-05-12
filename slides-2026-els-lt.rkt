#lang at-exp racket @; -*- Scheme -*-
#|
Lambda, the Ultimate Object, the Book

Slides for Lightning Talk at the European Lisp Symposium, 2026-05-11
  https://european-lisp-symposium.org/2026/

To compile it, use:
  racket slides-2026-els-lt.rkt > build/slides-2026-els-lt.html

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket


This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(require scribble/html
;;         "util/util.rkt"
         "util/coop.rkt"
         (rename-in "util/coop.rkt" (|@| $))
         "util/protodoc.rkt"
         "util/reveal.rkt")

(def (make-table lists)
  (table style: "border-bottom-width:0;"
   (map (lambda (l)
          (tr style: "border-bottom-width:0;"
              (map (lambda (x) (td style: "border-bottom-width:0;" x)) l)))
        lists)))

(define (pic file width alt . opts)
  (apply img
         src: (string-append "resources/pic/" file)
         alt: alt
         width: width
         valign: 'middle
         style: "
    vertical-align: middle;
    padding-left: 0em;
    padding-right: 0em;
    padding-top: 0em;
    padding-bottom: 0em;
    border: 0em;
"
         opts))

(def doc
  (docfix
    ($title "Lambda, the Ultimate Object")
    ($ $kv 'slide
      (list
       @div[class: 'logo
       @pic["cube.svg" "200%" "cube"]]
       @div[class: 'title
            style: "color: #55f; vertical-align: middle; text-align: center; font-size: 100%"
            @b{Lambda, the Ultimate Object}]
       @C[style: "font-size: 66%"]{
           @b{Object-Orientation Elucidated @(email "<fare@mukn.com>")}}
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @C{@small{@Url{https://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{European Lisp Symposium 2026-05-12}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
      ($slide "Lambda the Ultimate Object"
        @L{Object-Orientation Elucidated} ;; The Ultimate Book on Object-Orientation

        @L{So far: >10 chapters, >300 pages, @br >200 annotated bibliography entries.}

        @L{Claude proofreads (finds bugs!) — helps with exercises!}

        @L{Need help with last technical chapter (efficiency, MOP)})

      ($slide "1st Third: Informal"
        @L{1. Introduction: About the Book}

        @L{2. What Object-Orientation @em{is} — Informal Overview}

        @L{3. What Object-Orientation @em{is not}}

        @L{4. OO as Internal Extensible Modularity})

      ($slide "2nd Third: Proper Formal Reconstruction"
        @L{5. Minimal OO}

        @L{6. Rebuilding OO from its Minimal Core}

        @L{7. Inheritance:  Mixin, Single, Multiple, or Optimal}

        @L{8. Types for OO})

      ($slide "Last Third: Advanced + Closing"
        @L{9. Extending the Scope of OO}

        @L{10. Efficient Object Implementation}

        @L{11. Conclusion}

        @L{Annotated Bibliography})

      ($slide "Why a Book on OO ?"
        @L{30 years too late}

        @L{Yet still decades ahead}

        @L{Ideas unknown or under-known (even to Lispers!)}

        @L{Yes CLOS is far ahead overall... @br but not everywhere (FIX THAT!)})

      ($slide "Maybe new for Lispers"
        @L{Pure functional approach, laziness, lenses.}

        @L{Examples in pure Scheme with autocurrying}

        @L{Magic formula (with autocurrying): @br
                 @code{(def (fix m t) (Y (m t))} @br
                 @code{(def (mix p c t s) (c (p t s) s))}}

        @L{NB: Gerbil Scheme builtin objects are stateful @br
                        (but also pure prototype library)})

      ($slide "C4 Algorithm"
        @L{C3 is *objectively* better than CLOS linearization @br
                 (Dylan, Python, Perl...)}

        @L{You can mix "classes and structs" @br
                 (Ruby, Scala).}

        @L{Both together: C4.}

        @L{Implemented in Gerbil Scheme. @br
                 (I had Claude do it in C++ templates)}

        @L{Challenge: make it (optional) default in SBCL(?)})

     ($slide "Thank You!"
        @L{Slides: @(~ 8) @Https{github.com/metareflection/poof}}
        @L{Join me: @(~ 5) Gerbil Scheme @Url{https://cons.io}}
        @L{Blog: @Https{ngnghm.github.io} @~[3] X: @Https{x.com/ngnghm}}
        @L{Hire me: @(~ 5) @code{<fahree@"@"gmail.com>}})))

(reveal-doc doc)
