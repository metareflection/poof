#lang at-exp racket @; -*- Scheme -*-
#|
Prototypes: Object-Orientation, Functionally

Slides for presentation at NJPLS 2023 in Princeton, 2023-11-10

To compile it, use:
  racket slides-2023-njpls.rkt > slides-2023-njpls.html

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket


This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(require scribble/html
         "util/util.rkt"
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

(def doc
  (docfix
    $top-doc
    ($title "Prototypes: Object-Orientation, Functionally")
    ($ $kv 'slide
      (list
       @div[class: 'logo]{
       @img[src: "resources/pic/mukn-name.svg"
            alt: "Mutual Knowledge Systems"
            width: "50%"
            valign: 'middle
            style: "
    vertical-align: middle;
    background-color: white;
    padding-left: .5em;
    padding-right: .5em;
    padding-top: .5em;
    padding-bottom: .5em;
"]}
       @; TODO: hide the menu behind a button at the top when on mobile (?)
       @div[class: 'title
            style: "color: #55f; vertical-align: middle; text-align: center; font-size: 150%"
            @b{Prototypes: @(br) Object-Orientation, @(br) Functionally}]
       @(br clear: 'all)
       @p{@small{@(~)}}
       @L{@code{@(~ 18) fix = (λ (p t) (Y (λ (s) (p s t))))} @(br)
          @code{@(~ 18) mix = (λ (p q) (λ (s t) (p s (q s t))))}}
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>") @(br)
           Alex Knauth @(email "<alexander@knauth.org>") @(br)
           Nada Amin @(email "<namin@seas.harvard.edu>")}
       @C{@small{@Url{http://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{NJPLS 2023-11-10}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
    ($section "Introduction: In Lieu of a Presentation")
     $plan-slide
     ($slide "The Talk I Won’t be Giving Today"
        @Li{Repeat of my Scheme Workshop 2021 Presentation}
        @Li{In Depth Explanation of Prototype Objects}
        @Li{In Depth Much Anything}
        @Li{Computer Science})
     ($slide "The Talk I’ll Be Giving Instead"
        @Li{Computing Arts}
        @Li{Why you should care about Prototype OO}
        @Li{How (not) to be a Good Researcher}
        @Li{… Going Meta!})
    ($section
     ($title "Conclusion: OO is FP")
     $plan-slide
     ($slide @list{Paper: OO System@em{@u{s}} in λ-calculus} ;; Thesis
        @Li{@em{Simple} Constructive Semantics of OO in pure FP}
        @Li{Demystify fundamental concepts of OO}
        @Li{Prototypes before Classes, Purity before Mutation}
        @Li{@em{Why} it matters, @em{Why} it's that way})
     ($slide "Key Concepts"
        @Li{Incrementality & Modularity}
        @Li{Mixin Functions, compose beyond apply}
        @Li{Multiple inheritance: @(~ 3) modular dependencies}
        @Li{Conflation: @(~ 4) @code{Prototype = Mixin × Target}}
        @Li{@code{Class = Proto Type Top}})
     ($slide "Meta-Thesis"
        @Li{Humility, not fanaticism}
        @Li{Incommensurable paradigms? Go wider!}
        @Li{Simplicity matters}
        @Li{λ's for Semantics, macros for Syntax})
     ($slide "Thank You!"
        @Li{Theory: @(~ 8) @Url{https://github.com/metareflection/poof}}
        @Li{Practice: @(~ 5) @Url{https://github.com/fare/gerbil-poo}}
        @Li{OO in 30 loc — 80 loc with multiple inheritance}
        @Li{Hire me! @(~ 5) @code{<fare@"@"mukn.com>}}))))

(reveal-doc doc)
