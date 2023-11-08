#lang at-exp racket @; -*- Scheme -*-
#|
Prototypes: Object-Orientation, Functionally

Slides for presentation at LambdaConf 2024 in Estes Park, Colorado, 2024-05-06

To compile it, use:
  racket slides-2024-lambdaconf.rkt > slides-2024-lambdaconf.html

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket


This document is available under the bugroff license.
  http://www.oocities.org/soho/cafe/5947/bugroff.html
|#

(require scribble/html
         "util/util.rkt"
         "util/coop.rkt"
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
    ($kv 'slide
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
       @C{@small{LambdaConf 2024-05-06}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))

    ($section "Introduction: Wherefore OO?"
     $plan-slide
     ($slide @list{Paper: OO System@em{@u{s}} in λ-calculus} ;; Thesis
        @Li{@em{Simple} Constructive Semantics of OO in pure FP}
        @Li{Demystify fundamental concepts of OO}
        @Li{Prototypes before Classes, Purity before Mutation}
        @Li{@em{Why} it matters, @em{Why} it’s that way})
     ($slide "Language Wars: OO vs FP"
        @Li{Two sides. Gabriel: “Incommensurable paradigms”}
        @Li{FP in Academia, OO in Industry}
        @Li{Misunderstanding, Scorn}
        @Li{Videos, Books, University Courses... all wrong }
        @list{... And yet in practice: mutual adoption!})
     ($slide "OO for FP"
        @Li{FP is my tribe — probably yours}
        @Li{Vast evidence that OO matters}
        @Li{FP makes semantics simpler}
        @Li{Let’s understand what and why OO})
     ($slide "What OO isn’t about"
        @Li{Classes}
        @Li{“Encapsulation”, “Information Hiding”}
        @Li{Inheritance vs Composition vs ...}
        @Li{Mutation everywhere})
     ($slide @list{What OO @em{is} about}
        (make-table
         `(("Incrementality" ,nbsp "Modularity")
           (,nbsp            ,nbsp ,nbsp)
           ("Open Recursion" ,nbsp "Ad Hoc Polymorphism")))))
    ($section "Minimal OO: Mixin Functions"
     $plan-slide
     ($slide "Simplest OO Concept"
        @L{@code{Instance}: value to specify incrementally}
        @L{@code{Prototype}: modular increment of specification}
        @L{@code{Instantiate: Prototype → Instance}}
        @L{@code{Inherit: Prototype → Prototype → Prototype}})
     ($slide "Prototypes as Mixin Functions"
        @L{Inputs: whole spec (other aspects), previous spec @(br)
           Output: more complete spec}
        @L{@code{Mixin = Instance → Instance → Instance}}
        @L{@code{Mixin Self Super = Self → Super → Self @(br)
                 @(~ 3) | Self ⊂ Super}} ;|}}
        @L{@code{fix: Mixin Instance Top → Top → Instance} @(br)
           @code{mix: Mixin S T → Mixin T U → Mixin S U}})
     ($slide "The Simplest Solution"
        @L{@code{Mixin Self Super = Self → Super → Self @(br)
                 @(~ 3) | Self ⊂ Super}} ;|}}
        @L{@code{fix : Mixin Self Top → Top → Self} @(br)
           @code{fix = (λ (p t) (Y (λ (s) (p s t))))}}
        @L{@code{mix : Mixin S T → Mixin T U → Mixin S U} @(br)
           @code{mix = (λ (p q) (λ (s u) (p s (q s u))))}})
     ($slide "Mixins as “Prototypical” OO"
        @Li{Theory: Bracha & Cook, "Mixins" 1990 (w/ records)}
        @Li{Practice: Nix 2015 (Jsonnet 2014)}
        @Li{Mutable: Director ’76, ThingLab ’77, T ’82, Self ’86, JS ’95}
        @Li{Classes: Simula ’67, Smalltalk ’71, @em{Flavors} ’82}
        @Li{Typeful: Cool hack in Haskell Oliveira 2009 (w/o records)}))
    ($section "Is it OO? Mixins for Records"
     $plan-slide
     ($slide "Instances as Records"
        @L{@code{top = (λ (msg) (error "No such method" msg))}}
        @L{@code{rcons = (λ (k v r) (λ (m) @(br)
                 @(~ 30) (if (eq? k m) v (r m))))}}
        @L{@code{point = (rcons 'a 3 (rcons 'b 4 top))}}
        @L{@code{(point 'a)} ⟶ @code{3}})
     ($slide "Prototypes for Records"
        @L{@code{KV = (λ (k v) (λ (s t) (rcons k v t)))}}
        @L{@code{point = (fix (mix (KV 'a 3) (KV 'b 4)) top)}}
        @L{@code{(point 'a)} ⟶ @code{3}})
     ($slide "Non-Constant Prototypes"
        @L{@code{method = (λ (k f) (λ (s t) (λ (m) @(br)
                 @(~ 5) (if (eq? k m) (f s (λ () (t m))) (t m)))))}}
        @L{@code{(method 'x @(br)
                 @(~ 5) (λ (self next) (* (self 'fudge) (next))))}}
        @L{@code{(method 'p @(br)
                 @(~ 5) (λ (self _) (* (self 'x) (self 'y))))}}))
    ($section "Prototypes: OO without Objects"
     $plan-slide
     ($slide "Single Inheritance: Generators"
        @L{@code{Gen Self = Self → Self} @(~ 28)(@code{Top} is given)}
        @L{@code{fixGen : Gen Self → Self} @(~ 19)(= @code{Y} itself!)}
        @L{@code{mixGen = Mixin S T → Gen T → Gen S}}
        @L{You can @code{cons} (@code{apply}) but can’t @code{append} (@code{compose}) a mixin})
     ($slide "Multiple Inheritance: Dependency Graph"
        @img[alt: "c3 linearization example"
             src: "resources/pic/C3_linearization_example.svg.png"]
        @nbsp
        @L{Must @em{linearize} the DAG into a @em{precedence list}}
        @L{e.g. @code{(Z K1 K3 K2 C A B D E O)}})
     ($slide "Multiple Inheritance: Precedence Lists"
        @L{easy precedence lists: @code{(O) (A O) (B O) (C O)}}
        @L{bad precedence lists: @(br)
           @(~ 20) @code{(K1 C O A O B O)} @(br)
           @(~ 20) @code{(Z K1 K2 K3 A B C D E O)}}
        @L{C3: preserve coherence properties @(br)
           @(~ 20) @code{(K1 C A B O)} @(br)
           @(~ 20) @code{(Z K1 C K3 A K2 B D E O)}})
     ($slide "Multiple Inheritance: Modular Dependencies"
        @L{What if you change @code{K2} ?}
        @L{Manual curation: @br
           @(~ 20) - users must update when dependencies change @br
           @(~ 20) - users must track @em{transitive} dependencies}
        @L{Modularity: implementation computes precedence lists @br
           @(~ 20) - users needn’t update when dependencies change @br
           @(~ 20) - users only must track @em{direct} dependencies})
     ($slide "Prototypes beyond Mixin Functions"
        @L{@code{Proto = Mixin × (List Proto)}}
        @L{@code{Proto S T = Mixin ? ? × (List (Proto ? ?))}}
        @L{@code{Proto = Mixin × (List Proto) × Defaults}}
        @L{More: “No Such Message” handler, Constraints, Types, etc.})
     ($slide "How is it Object-Orientation?"
        @Li{So far, no classes}
        @Li{So far, no objects}
        @Li{Only prototypes and instances}
        @Li{Yet, we recognize (almost?) all of OO!}))
    ($section "Conflation: Object = Proto × Instance"
     $plan-slide
     ($slide "Modular Extensibility"
        @Li{Instance: Configuration as DAG of records}
        @Li{Prototype: Modular Incremental Specification}
        @Li{Which records in the DAG are extension points?}
        @Li{None | Exponential explosion | All of them})
     ($slide "Conflation: Prototype & Instance"
        @L{Ubiquitous Extensibility at every level}
        @L{Every “object” is both Prototype and Instance}
        @L{@code{Object = Proto × Instance} @br
           @(~ 5) (or @(~ 2) @code{Proto = … × Instance})}
        @L{Purity ⇒ Instance unique up to observation})
     ($slide "Not just for “records”"
        @Li{Prototypes for numerical functions, for integers}
        @Li{“records”, but with very different representations}
        @Li{Eager Y: only functions. Lazy Y: delay/lazy values.}
        @Li{Computations vs Values (CBPV 1999)})
     ($slide "Secret to Semantic Simplicity"
        @Li{Recognizing two distinct entities, Prototype and Instance}
        @Li{Conflation without Distinction = Confusion !}
        @Li{Lack of awareness ⇒ formalizations that miss the point}
        @Li{Jsonnet (2014) [“Components” in T (1982)]}))
    ($section "Class = Proto Type Top"
     $plan-slide
     ($slide "Where are the Classes?"
        @Li{Simula ’67, Smalltalk, C++, CLOS, Java, C#, Python…}
        @Li{So much in common… yet very different!}
        @Li{Is it even the same paradigm?}
        @Li{What relationship between Prototypes and Classes?})
     ($slide "Type descriptors!"
        @Li{Instance: Type descriptor (see ML module)}
        @Li{Prototype: add or override methods for Type}
        @Li{Abstract Class vs Concrete Class}
        @Li{“Object”: type descriptor? prototype? type element?})
     ($slide "From Type Descriptor to Type"
        @Li{Dependent types, Staging, Existential types?}
        @Li{Most PLs have limited type-level computations}
        @Li{Mixin vs single- vs multiple- inheritance matter}
        @Li{Singleton Class, but lack or dynamism})
     ($slide "Class vs Typeclass"
        @Li{Class vs Typeclass, vtable vs “dictionary”}
        @Li{Pre-vtable constructor vs burden of dictionary}
        @Li{Type-directed synthesis as meta-level computation}
        @Li{Automated translation, see LIL (2012)}))
    ($section "Pure Challenges"
     $plan-slide
     ($slide "Identity for DAG"
        @Li{Multiple Inheritance in Nix: no @code{===} for DAG}
        @Li{Unique tag: non-determinism? side-effect?}
        @Li{“Solutions”: State? Monads? Opaque tag? Explicit labels?}
        @Li{Labeling convention: moving computation to wetware})
     ($slide "Side-Effects"
        @Li{Multiple instances? Evolving DAG? Cloning vs Sharing?}
        @Li{Class OO: all prototypes are pure… at compile-time}
        @Li{Linearity: +Optimizations -Enforcement}
        @Li{Caching: +Sharing -Invalidation})
     ($slide "Purity vs non-local specification increments"
        @Li{Multimethods, Mutually recursive classes, Friend classes?}
        @Li{Haskell problem: orphan typeclass instances}
        @Li{Hack: side-effect global table, hope for no conflict}
        @Li{Nix solution: the Open Loop is Global})
     ($slide "Advanced OOP, purely?"
        @Li{Optics: generalize @code{method} to use @code{lens}}
        @Li{Advice, CLOS, AOP, before/after/around methods}
        @Li{Method combination: @code{list}, @code{and}, @code{+}, or user-specified}
        @Li{Any advanced topic in OOP?}))
    ($section "Conclusion: OO is FP"
     $plan-slide
     ($slide @list{Paper: OO System@em{@u{s}} in λ-calculus} ;; Thesis
        @Li{@em{Simple} Constructive Semantics of OO in pure FP}
        @Li{Demystify fundamental concepts of OO}
        @Li{Prototypes before Classes, Purity before Mutation}
        @Li{@em{Why} it matters, @em{Why} it’s that way})
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
        @Li{λ’s for Semantics, macros for Syntax})
     ($slide "Thank You!"
        @Li{Theory: @(~ 8) @Url{https://github.com/metareflection/poof}}
        @Li{Practice: @(~ 5) @Url{https://github.com/fare/gerbil-poo}}
        @Li{OO in 30 loc — 80 loc with multiple inheritance}
        @Li{Hire me! @(~ 5) @code{<fare@"@"mukn.com>}}))))

(reveal-doc doc)
