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
          @code{@(~ 18) mix = (λ (p q) (λ (s u) (p s (q s u))))}}
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
    ($section "Introduction: In Lieu of a Presentation"
     $plan-slide
     ($slide "The Talk I Won’t be Giving Today"
        @Li{Repeat of Scheme Workshop 2021 Talk}
        @Li{In Depth Presentation of Prototypes}
        @Li{In Depth Much Anything}
        @Li{Computer Science})
     ($slide "The Talk I’ll Be Giving Instead"
        @Li{Computing Arts}
        @Li{Why care about Prototype OO}
        @Li{Insights learned along the way}
        @Li{How (not) to be a Good Researcher})
     ($slide "Prototype OO in a nutshell"
        @Li{OO w/o Classes… sometimes w/o Objects!}
        @Li{Incrementality & Modularity — @em{in} the language}
        @Li{Open Recursion, Ad Hoc Polymorphism}
        @Li{@code{(λ (self super) body ...)}}))
    ($section "Why Pure Functional Prototype OO ?"
     $plan-slide
     ($slide "Great Insight"
        @Li{OO semantics simplest & clearest}
        @Li{More fundamental than classes}
        @Li{Simplest OO types — and no simpler}
        @Li{Add OO to your language under 100 loc!})
     ($slide "Great System Configuration"
        @Li{GCL (2004), Jsonnet (2014), Nix (2015)…}
        @Li{Mixins provide or consume methods / slots}
        @Li{Large DAG where every node is an extension point}
        @Li{Global open recursion for extensibility})
     ($slide "Runtime Composable Type Descriptors"
        @Li{Type Descriptors + Codecs (GNU poke, QuickCheck)}
        @Li{Dynamic configurations, User-definable}
        @Li{Tries in 10x less code than Go, 2x less than OCaml}
        @Li{Lightweight, first addition to any dynamic language})
     ($slide "Class OO is but Prototype OO at type-level"
        @Li{@code{Class = Proto Type}}
        @Li{Squint and all OO is Prototypes}
        @Li{Use Prototypes & you can do all OO @em{and more}}
        @Li{… JS has prototypes!})
     ($slide "This very Presentation!"
        @Li{Pure functional, declarative}
        @Li{How can the toc include yet-unwritten section titles?}
        @Li{Prototype for slides w/ toc in ~125 loc}
        ;; @Li{FP + OO system in 350 loc including autocurry macros}
        @Li{… demo time!}))
    ($section "My Scheme Workshop 2021 Paper"
     $plan-slide
     ($slide "OO from Scratch"
        @Li{Prototypes: Incremental Specification}
        @Li{@code{self}: Complete Instance being specified}
        @Li{@code{super}: Partial Instance so far}
        @Li{Inheritance: Single, Mixin, Multiple})
     ($slide "Objects"
        @Li{Just did OO without Objects!}
        @Li{Objects: modular extension points}
        @Li{Conflation: @(~ 4) @code{Object = Prototype × Instance}}
        @Li{Confusion: conflation w/o distinction, vocabulary conflict})
     ($slide "Unexpected Insights"
        @Li{Computations vs Values... Laziness matters}
        @Li{Subclassing cannot be subtyping}
        @Li{Purity comes before Mutation}
        @Li{Global open loop, library curation}))
    ($section "Conclusion: Paradigm War and Peace"
     $plan-slide
     ($slide "Why did I care about Prototype OO?"
        @Li{Great Usenet Flamewars of OO vs FP}
        @Li{Talking Past Each Other… for Decades}
        @Li{The paper I wished I could have read younger}
        @Li{Lisp had it all the 1970s})
     ($slide "Why So Much Blindness?"
        @Li{Industry doesn’t care enough about Correctness}
        @Li{Academia doesn’t understand Programming In The Large}
        @Li{Both like to ignore the Human Factor}
        @Li{All get stuck in their paradigms})
     ($xslide "Inhabiting Constraints — But Which?"
        @C{Every task involves constraint,}
        @C{Solve the thing without complaint@";"}
        @C{There are magic links and chains}
        @C{Forged to loose our rigid brains.}
        @C{Structures, strictures, though they bind,}
        @C{Strangely liberate the mind.}
        @R{— James Falen, as quoted by dughof})
     ($slide "Meta-Thesis"
        @Li{Humility, not fanaticism}
        @Li{Incommensurable paradigms? Go wider!}
        @Li{Simplicity matters}
        @Li{λ’s for Semantics, macros for Syntax})
     ($slide "Thank You!"
        @Li{Theory: @(~ 8) @Url{https://github.com/metareflection/poof}}
        @Li{Practice: @(~ 5) Gerbil Scheme @Url{https://cons.io}}
        @Li{OO in 30 loc — 80 loc with multiple inheritance}
        @Li{Hire me! @(~ 5) @code{<fare@"@"mukn.com>}}))))

(reveal-doc doc)
