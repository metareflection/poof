#lang at-exp racket @; -*- Scheme -*-
#|
Multiple Inheritance: Wherefore, Why and How

Slides for presentation at LambdaConf, 2025-05-13

To compile it, use:
  racket slides-2025-lambdaconf.rkt > build/slides-2025-lambdaconf.html

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
    ($title "Multiple Inheritance: Wherefore, Why and How")
    ($ $kv 'slide
      (list
       @div[class: 'logo]{
       @img[src: "resources/pic/mukn-icon-whitebg.png"
            alt: "MuKn.com"
            width: "35%"
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
            @b{Multiple Inheritance: Wherefore, Why and How}]
       @(br clear: 'all)
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @C{@small{@Url{http://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{LambdaConf 2025-05-13}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
    ($section "Introduction: Inheritance"
     $plan-slide
     ($slide "The Talk I Won’t be Giving Today"
        @L{Redo of Scheme Workshop 2021}
        @L{Redo of LambdaConf 2024}
        @L{In Depth Presentation of OO}
        @L{In Depth Presentation of Inheritance})
     ($slide "The Talk I’ll Be Giving Instead"
        @L{Why care about OO}
        @L{Why care about (Multiple) Inheritance}
        @L{Some historical context}
        @L{The Best Inheritance Algorithm})
     ($slide "Technical Thesis"
        @L{OO is In-Language Incremental Modularity}
        @L{Multiple- Inheritance beats Single- and Mixin-}
        @L{Linearization and its Consistency Constraints Matter}
        @L{C4 is the best linearization algorithm})
     ($slide "Social Thesis"
        @Li{Programmers lack perspective on programming}
        @Li{

     ($slide @list{What OO @em{is not} about}
        @L{Classes}
        @L{“Encapsulation”, “Information Hiding”}
        @L{Inheritance vs Composition, OO vs FP}
        @L{Mutation everywhere}
        @L{Message Passing everywhere})
     ($slide @list{What OO @em{is} about}
        @L{Modularity: limited knowledge in} ;;  ⟶ Ad Hoc Polymorphism
        @L{Incrementality: limited knowledge out} ;;  ⟶ Open Recursion
        @L{... Intralinguistically}
        @L{A way to organize software development}
        @L{... to support @em{Division of Labor}})
     ($slide "Prototype OO"
        @Li{Proto: KRL'75 Ani'76 ThingLab'77 T'82 Self'85 JS'95} ;; after Simula 67, before Smalltalk 76
        @Li{Class: Simula'67 Smalltalk'76 Flavors'79 C++'79 Java'96} ;; Mesa'79 C_with_Classes'79 LOOPS'82 CLOS'84 C++'85 Eiffel'86
        @Li{OO w/o Classes… sometimes w/o Objects!}
        @Li{Not as well known, yet more fundamental}
        @Li{@code{(λ (self super) body ...)}})) ;; this inner in SIMULA
    ($section "Why Pure Functional Prototype OO ?"
     $plan-slide
     ($slide "Great for Semantic Insight"
        @Li{OO semantics simplest & clearest}
        @Li{More fundamental than Class OO}
        @Li{Simplest OO types — and no simpler}
        @Li{Laziness, statefulness, modularity}
        @Li{Add OO to your language under 100 loc!})
     ($slide "Great for System Configuration"
        @Li{GCL (2004), Jsonnet (2014), Nix (2015)…}
        @Li{From single computers to distributed systems}
        @Li{Mixins offer or use methods, incremental specification}
        @Li{Large graph where every node is an extension point}
        @Li{Global open recursion, not just local})
     ($slide "Great for Runtime Type Descriptors"
        @Li{GUI, QuickCheck, Codecs}
        @Li{Neatly decouples values from their interpretation}
        @Li{Dynamic configurations, User-definable, Composable}
        @Li{Tries in 10x less code than Go, 2x less than OCaml}
        @Li{Lightweight, my first code atop any dynamic language})
     ($slide "Class OO is but Prototype OO at type-level"
        @Li{@code{Class = Proto Type}}
        @Li{… JS has Prototypes!}
        @Li{Squint and all OO is Prototypes}
        @Li{… C++ templates @em{are} Pure Functional Prototype OO}
        @Li{Use Prototypes & you can do all OO @em{and more}})
     ($slide "This very Presentation!"
        @Li{Pure functional, declarative}
        @Li{Prototype for slides w/ toc in ~125 loc}
        @Li{FP + OO system in 350 loc including autocurry macros}
        @Li{How can the toc include yet-unwritten section titles?}
        @Li{… demo time?}))
    ($section "Prototype OO in a Nutshell"
     $plan-slide
     ($slide "OO from Scratch"
        @Li{Prototypes: Incremental Modular Specification}
        @L{@code{(λ (self} @";" Instance being specified (modularity) @br
           @(~ 15) @code{super)} @";" Partial Instance so far (incrementality) @br
           @(~ 15) @code{…)} @";" compute a more elaborate partial instance}
        @Li{Mixin Inheritance: compose prototypes! @br
            More expressive than single-inheritance, and simpler (with HOFs)})
     ($slide "OO in Two lines"
        @L{@code{fix = p t) (Y (λ (s) (p s t))))} @br
           @code{mix = (λ (p q) (λ (s u) (p s (q s u))))}}
        @L{@code{instantiate = λ mixin base ↦ @br
                 @(~ 18) Y (λ (self) (mixin self base))} @br
           @code{inherit = λ child parent ↦ λ self super ↦ @br
                 @(~ 18) child self (parent self super)}}
        @L{@code{child}, @code{parent}: mixins @br
            @code{self}: complete instance @(~ 10)
                 @code{super}: partial instance})
     ($slide "Objects"
        @Li{Just did OO without Objects! (This presentation)}
        @Li{Conflation: @(~ 4) @code{Object = Prototype × Instance}}
        @Li{Why? Modular extension points}
        @Li{Confusion: conflation w/o distinction, vocabulary conflict}
        @Li{Abstract vs Concrete Class = Prototype vs Instance})
     ($slide "Unexpected Insights"
        @Li{Computations vs Values... Laziness matters}
        @Li{Subclassing cannot be subtyping}
        @Li{Pure OO comes before Stateful OO}
        @Li{Multiple inheritance more modular than mixin}
        @Li{Global open loop, library curation — also for classes}))
    ($section "Conclusion: Paradigm War and Peace"
     $plan-slide
     ($slide "Why did I care about Prototype OO?"
        @Li{Great Usenet Flamewars of OO vs FP}
        @Li{Dysfunctional Programming vs @br
            @(~ 18) Objectionable Disorientation}
        @Li{Talking Past Each Other… for Decades}
        @Li{1970s Lisp had both (2000s Scala: with types)}
        @Li{The paper I wished I could have read younger})
     ($slide "Why So Much Blindness?"
        @Li{Industry doesn’t care enough about Correctness}
        @Li{Academia doesn’t understand Programming In The Large}
        @Li{Both like to ignore the Human Factor}
        @Li{All get stuck in their paradigms}
        @Li{Opposition in mindset, not in logic})
     ($slide "Two Mindsets"
        @Li{Compositionality for reducible problems}
        @Li{Incrementality for irreducible problems}
        @Li{Brighter than ambitious vs more ambitious than bright}
        @Li{History: OO invented to model complex SIMULAtions}
        @Li{History: FP to formalize computations down their simplest})
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
        @Li{Simplicity matters — so does Complexity}
        @Li{Paradigm: “Systems” vs “Programming Language”}
        @Li{Scheme: λ’s for Semantics, macros for Syntax})
     ($slide "Thank You!"
        @Li{Theory: @(~ 8) @Url{https://github.com/metareflection/poof}}
        @Li{Practice: @(~ 5) Gerbil Scheme @Url{https://cons.io}}
        @Li{X: @Url{https://x.com/ngnghm}  Blog: @Url{https://ngnghm.github.io}}
        @Li{Hire me — or be hired by me! @(~ 5) @code{<fare@"@"mukn.com>}}
        @Li{Plenty more research ideas, code and papers to write…}))))

(reveal-doc doc)
