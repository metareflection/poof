#lang at-exp racket @; -*- Scheme -*-
#|
Compositional Object Oriented Prototypes

Slides for presentation at (fifteenth RacketCon), 2025-10-04

To compile it, use:
  racket slides-2025-racketcon.rkt > build/slides-2025-racketcon.html

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
    ($title "Compositional Object Prototypes")
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
            @b{Compositional Object-Oriented Prototypes}]
       @(br clear: 'all)
       @p{@small{@(~)}}
       @L[style: "font-size: 80%;"]{@code{(define (instantiate s) (Y (λ (m i) (s m i ⊥))))}}
       @L[style: "font-size: 80%;"]{@code{(define (inherit c p m i) (compose (c m i) (p m i)))}}
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @C{@small{@Url{http://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{(fifteenth RacketCon) 2025-10-04}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
    ($section "Introduction"
     $plan-slide
     ($slide "The Talk I Won’t be Giving Today"
        @L{Redo of talk at SW'21, LC'24, NJPLS'24 or LC'25}
        @L{Complete Theory of OO}
        @L{Presentation of coop.rkt} ;; the proof-of-concept library I wrote in Racket
        @L{Exploration of OO features})
     ($slide "The Talk I’ll Be Giving Instead"
        @L{The Essence of OO}
        @L{A minimal OO system}
        @L{Sketch of how other OO features fit on top}
        @L{Insights learned along the way})
     ($slide "Original Claims"
        @L{OO is about Internal Extensible Modularity @(br)
            OO is characterized by use of @em{Inheritance}}
        @L{Mixin Inheritance is simplest, but not most modular @(br)
            Best: combine Single and Multiple Inheritance}
        @L{You can have OO without classes, even without objects @(br)
            Key ignored notion: Conflation}))
    ($section "OO, Informally"
     $plan-slide
     ($slide @list{What OO @em{is not} about}
        ;; Concepts that intersect OO that people mistakenly confuse with OO.
        ;; No time to debunk in detail, but can easily find where they
        @Li{C++ (or even Java, etc.)}
        @Li{Classes}
        @Li{“Encapsulation”, “Information Hiding”}
        @Li{Opposite of FP, Inheritance vs Composition}
        @Li{Mutation everywhere}
        @Li{UML / Co-Algebras / Relational Modeling}
        @Li{Asynchronous Message Passing everywhere})
     ($slide @list{What OO @em{is} about}
        @L{Modularity: dev needs little knowledge in} ;;  ⟶ Ad Hoc Polymorphism
        @L{Extensibility: dev needs little knowledge out} ;;  ⟶ Open Recursion
        @L{... Intralinguistically} ;; First-class
        @L{}
        @L{Support for @em{Division of Labor}}) ;; across developers, across time
     ($slide @list{Internal vs External}
        @L{First class: internal, runtime (values, reflection)}
        @L{Second class: internal, compile-time (types, macros)}
        @L{}
        @L{Third class: external, software (tooling)}
        @L{Fourth class: external, wetware (design patterns)})
     ($slide @list{Essential Mechanism: Inheritance}
        @L{Single 1967/1976, Multiple 1975/1979, Mixin 1982/1990}
        @L{}
        @L{Uniquely enables modularity and extensibility together:}
        @L{Incrementally extend software @(br)
           with minimal knowledge what's inside or outside})
     ($slide @list{Not Essential: Classes, Objects}
        @L{Classes: SIMULA 1967 @(br)
           Smalltalk 1976, Flavors 1979, C++ 1985, Java 1996}
        @L{Prototypes: KRL 1975 @(br)
           Ani 1976, ThingLab 1977, SELF 1986, JS 1995}
        @L{“Specifications” (no classes, not even objects!) @(br)
           Yale T Scheme 1982 “components”}))
    ($section "Minimal OO, Formally"
     $plan-slide
     ($slide "Minimal (First-Class) Extensibility"
        @L{Entities of type @code{E}}
        @L{Extensions of type @code{E → E}}
        @L{Start with top @s{object} entity, e.g. @code{⊤ = #f}} ;; or (λ (_) ⊥), (delay ⊥)
        @L{Good: @em{Apply} extensions: @code{(extension entity)}}
        @L{Better: @em{Compose} extensions: @code{(compose e1 e2)}}
        @L{(Matters a lot for second-class extensions)})
     ($slide "Minimal (First-Class) Modularity: How"
        @L{Values: @code{V}, @code{W}, @code{X}... Identifiers: @code{I}. @(br)
           Set of bindings, a.k.a. record: @code{∏R = i:I → Rᵢ}}
        @L{Module context: @code{∏M = i:I → Mᵢ} @(br)
           Modular spec for @code{X}: @code{∏M → X} @(br)
           For each identifier, a modular spec: @(br)
           @code{∏(∏M→X) = i:I → ∏M → Xᵢ} @(br)
           Equivalent to @code{∏M → i:I → Xᵢ = ∏M → ∏X} @(br)
           … open modular spec for a set of bindings @code{∏X}})
     ($slide "Minimal (First-Class) Modularity: Y"
        @L{Open modular spec: @code{∏M → ∏X} @(br)
           @code{∏M} module context, record of identifiers being referenced @(br)
           @code{∏X} module interface, record of identifiers being defined.}
        @L{Close modular spec: @code{∏M → ∏M} @(br)
           every identifier referenced is defined}
        @L{Extract record @code{∏M} from spec @code{∏M → ∏M} ? @(br)
           Close open loops, tie knots… @em{fixed-point operator}, @code{Y}})
     ($slide "Digression: Scheme vs FP"
        @L{Issue 1: pure applicative Y sucks @(br)
            Solution: stateful Y, lazy Y, or second class Y}
        @L{Issue 2: unary functions are syntax-heavy @(br)
            Solution: cope, autocurry, or multiple arities with care}
        @L{@code{coop.rkt} @em{choices}: letrec + lazy fields, autocurry}) @; Obviously, YMMV
     ($slide "Minimal (First-Class) Modular Extensibility"
        @L{Extensions: @code{X → X} @(br)
           Modularity context: @code{∏M} @(br)
           Open modular extensible spec: @code{∏M → ∏(X → X)}}
        @L{Close modular extensible spec: @code{∏M → ∏(M → M)} @(br)
           To resolve each binding, apply to base object, or use @code{Y} @(br)
           Reduced to known modular spec @code{∏M → ∏M}})
     ($slide "Minimal “Object-Orientation”"
        @L{That's mixin inheritance, all the OO you need!}
        @L{@code{(define (instantiate spec) @(br)
                 @(~ 2) (Y (λ (self m) (spec self m ⊥)))) @(br)
                 (define (inherit child parent s m) @(br)
                 @(~ 2) (compose (child s m) (parent s m)))}}
        @L{@code{(deftype spec (Fun M → I → X → X)) @(br)
                 (define (my-spec self method super) @(br)
                   @(~ 2)...value)}})
     ($slide "Minimal Example"
        @L{@code{(define (coord-spec self i super) @(br)
                    @(~ 1) (case i ((x) 2) ((y) 4) (else super))) @(br)
                 (define (color-spec self i super) @(br)
                    @(~ 1) (case i ((color) 'blue) (else super)))}}
        @L{@code{(define point-p (instantiate @(br)
                    @(~ 2) (inherit coord-spec color-spec))) @(br)
                 (point-p 'y) ⇒ 4 @(br)
                 (point-p 'color) ⇒ blue}})
     ($slide "Minimal Example, non-trivial inheritance"
        @L{@code{(define (add-x-spec dx self i super) @(br)
                   @(~ 1) (case i ((x) (+ dx super)) @(br)
                   @(~ 9) (else super))) @(br)
                 @(br)
                 (define (rho-spec dx self i super) @(br)
                   @(~ 1) (case i ((rho) (sqrt (+ (sqr (self 'x) @(br)
                   @(~ 20)                        (sqr (self 'y)))))) @(br)
                   @(~ 9) (else super)))}})
     ($slide "Minimal Example, non-trivial inheritance (test)"
        @L{@code{(define point-q (instantiate @(br)
                    @(~ 2) (inherit rho-spec @(br)
                    @(~ 4) (inherit (add-x-spec 1) @(br)
                    @(~ 6) coord-spec)))) @(br)
                    @(br)
                 (point-q 'x) ⇒ 3 @(br)
                 (point-q 'rho) ⇒ 5}}))
    ($section "Wait, what?"
     $plan-slide
     ($slide "What did we just do?"
        @L{Reconstructed recognizable OO from first principles}
        @L{The first principles: @em{first-class, modularity, extensibility}}
        @L{OO literally in two short definitions, in any FP language}
        @L{No classes, no mutable objects, no objects!}
        @L{@(~)}
        @L{How is it even possible???})
     ($slide "Precedents" ;; (modulo trivial refactorings)
        @L{Yale T Scheme 1982, YASOS 1992 (applicative, stateful)}
        @L{Bracha & Cook 1990: theoretical “model” of inheritance}
        @L{GCL 2004: Runs all Google (lazy, dynamic scope)} ;; has objects
        @L{Jsonnet 2014: GCL cleanup (lexical scope, JS-y syntax)}
        @L{Nix extensions 2015: isomorphic to above, OO in 2 funs})
     ($slide "OO without objects (as in Yale T Scheme)"
        @L{“object” is any value, “instance” is just a regular record @(br)
           @em{No inheriting from an “instance”}} ;; yes extending it, but non-modularly
        @L{“component” is a specification @(br)
           @em{No computing methods from a “component”}} ;; not quite a record
        @L{No equivalent to object (or class) in other OO languages! @(br)
           ... contrast with GCL, Jsonnet, Nix, that have objects(!?)})
     ($slide "Conflation: hidden product, implicit cast"
        @L{Prototype = Spec × Target @(br)
           Target = lazily resolved value from spec}
        @L{Want to compute a method? Use the target @(br)
           Want to inherit? Use the spec}
        ;; Small, partial, incremental specifications are the whole point
        @L{Lazy is essential: most specifications are partial} ;; would fail resolution
        @L{Prototype OO: Ani 1976, ThingLab 1977, SELF 1986, @(br)
           JS 1995, GCL 2004, Jsonnet 2014, Nix 2015}))
    ($section "What about my favorite OO feature?"
     $plan-slide
     ($slide "What about Classes?"
        @L{Class = (Second-class) Prototype for Type (+ methods) @(br)
           Object, Instance = element of the target type}
        @L{static methods = methods of the target type @(br)
           object methods = static methods applied to the element}
        @L{abstract class = used only for its spec @(br)
           concrete class = used only for its target}
        @L{C++ templates: lazy functional Prototype OO at compile-time!})
     ($slide "What about Objects?"
        @L{T: "object" is any value, "instance" is target from spec @(br)
        @; i.e. Prototype is conflated Specification × Target
           Prototype OO: "object / instance" is Spec × Target @(br)
           Class OO: "object / instance" is Target type element}
        @L{"object" and "instance" are very ambiguous words @(br)
           "object" as a concept is not necessary for OO}
        @L{Fields are named first, understood much later.})
     ($slide "What about Types?"
        @L{Spec Inheritance: @em{before fix-point} @(br)
           Target Subtyping: @em{after fix-point} @(br)
           @(~ 15) @strong{DOES NOT COMMUTE!}}
        @L{Fortress 2011 Type checking modular multiple dispatch @(br)
           @(~ 4) with parametric polymorphism and multiple inheritance @(br)
           Scala 2012 Dependent Object Types}
        @L{Prototype OO? Dunno, maybe dependent types?})
     ($slide "What about Typeclasses or Modules?"
        @L{Haskell Typeclass, Rust Trait, ML modules... @(br)
           @(~ 4) modular but @em{not extensible} @(br)
           Contra Cook: extensibility matters!}
        @L{Interface Passing Style 2012: extensible typeclasses @(br)
           Isomorphism via macros: @(br)
           @(~ 8) class ≃ typeclass @(br)
           @(~ 8) (linear) pure ≃ stateful})
     ($slide "What about side-effects?"
        @L{Classes: pure lazy Prototype OO at compile-time}
        @L{Target types historically mutable, but don’t have to be}
        @L{Plenty of pure object libraries in Lisp, Java, Scala…}
        @L{Mutable inheritance: semantics is hard, as of all mutation @(br)
           CLOS: @code{update-instance-for-redefined-class} @(br)
           invalidate caches (atomically?)})
     ($slide "What about multiple dispatch?"
        @L{LOOPS 1986, CLOS 1991, Cecil 1992, Dylan 1992, Fortress 2006, Julia 2012}
        @L{“generic functions”, see CLOS for docs and experience @(br)
           see Fortress for proper types}
        @L{Pure FP vs orphan/friend/mutually-def'd (type)classes? @(br)
           Global fixed-point of entire namespace (nixpkgs…)})
     ($slide "What about single or multiple inheritance?"
        @L{Worth a section of its own (see next)}
        @L{Lowdown: @(br)
           @(~ 2) mixin inheritance is simplest @(br)
           @(~ 2) single inheritance is most efficient, least expressive @(br)
           @(~ 2) multiple inheritance is most modular @(br)
           @(~ 2) You can have the best of them together!}))
    ($section "Inheritance: Mixin, Single, Multiple"
     $plan-slide
     ($slide "Single Inheritance"
        @L{Hoare 1966 (sub)classes (handwaving) @(br) @; also NULL "billion dollar mistake"
           SIMULA 1967 “Prefix classes” (first implementation) @(br) @; also suffix, hence BETA "inner"
           Smalltalk 1976 “single inheritance” (compromise) @(br)
           C-with-classes 1979, Java 1996, C# 2000, COBOL 2002, JS 2015} ;; wildfire
        @L{Very efficient, simplest for 1960s before cheap lambdas @(br)
           Least expressive, least modular})
#|
(define (base-single self n) #f)
(define (inherit-single spec parent self n) (spec self (parent self) n))
(define (instantiate-single spec) (letrec ((self (λ (n) (spec self n)))) self))
(define (field k f self super n) (if (eq? n k) (f self (super n)) (super n)))
(define (my-spec self super n) ...new-value)
|#
     ($slide "Multiple Inheritance"
        @L{KRL 1975 “inheritance of properties” Ani 76, ThingLab 77 @(br)
           Method “conflict”: methods in parallel superclasses @(br)
           @; Sad, self-defeating worldview, less expressive, less extensible, less modular
           @(~ 2) Mesa 1979, SELF 1986, C++ 1989, ADA 2003 @(br) @; Julia 2012
           Method combinations, after linearizing inheritance DAG: @(br) @; Improved by Dylan's C3 1996
           @(~ 2) Flavors 1979, LOOPS 1986, CLOS 1991 @(br) @; Happy, Harmonious worldview
           Simple combination, can only call super method: @(br)
           @(~ 2) Ruby 1992, Python 1994, Scala 2004} @; Dull, boring worldview
        @L{Most expressive, most modular @(br)
           Most complex (~100 loc), somewhat inefficient in general})
     ($slide "Mixin Inheritance"
        @L{Yale T Scheme 1982: implemented, not conceptualized @(br)
           Bracha/Cook 1990: conceptualized, not implemented}
        @L{PLT Scheme 1998, Strongtalk 2002, GCL 2004, Newspeak 2006, Jsonnet 2014, Nix 2015}
        @L{Simplest in FP, more expressive & modular than Single @(br)
           As inefficient as Multiple Inheritance, less modular})
     ($slide "Mixin more Expressive than Single"
        @L{Second-class OOP: expressiveness issue. @(br)
           Single only @code{cons} a spec, not @code{append} spec @(br)
           Less sharing, more duplication, extra hoops @(br)
           … maintenance nightmare.}
        @L{First-class OOP: your own mixins on top (PLT 1998) @(br)
           Still extra hoops, extra concepts, extra complexity})
     ($slide "Multiple More Modular than Mixin"
        @L{Spec depends on others to avoid source duplication @(br)
           Can’t “just” pre-append: runtime duplication, bad, @(br)
           @(~ 2) exponential, non-commutative, non-idempotent}
        @L{Mixin inheritance: @em{manually} manage linearization @(br)
           A spec’s inheritance DAG becomes part of its interface @(br)
           Nightmare on transitive dependency change}
        @L{Multiple inheritance: just declare direct superspecs @(br)
           No maintenance nightmare, just bliss.})
     ($slide "Single and Multiple Together"
        @L{Two separate hierarchies (lame): CLOS (struct/class) @(br)
           Same hierarchy (yay): Ruby (class/module), @(br)
           @(~ 2) Scala (class/trait), Gerbil Scheme (struct/class)}
        @L{“struct” can inheritance from “class” and vice-versa @(br)
           struct only in “single inheritance” wrt other structs}
        @L{“tail property”: the property actually needed for efficiency @(br)
           @em{a struct’s class linearization is a suffix to its subclasses’s}}))
    ($section "Conclusion: Making sense of OO"
     $plan-slide
     ($slide "Original Claims (Redux)"
        @L{OO is about Internal Extensible Modularity @(br)
            OO is characterized by use of @em{Inheritance}}
        @L{Mixin Inheritance is simplest, but not most modular @(br)
            Best: combine Single and Multiple Inheritance}
        @L{You can have OO without classes, even without objects @(br)
            Key ignored notion: Conflation})
     ($slide "This very Presentation!"
        @L{@code{scribble} + @code{coop.rkt} spit HTML for @code{reveal.js}}
        @L{No objects, just specs and targets, mixin inheritance @(br)
           FP+OO system in 350 loc incl. autocurry, comments @(br)
           Prototypes for slides w/ toc in ~125 loc}
        @L{How can TOC appear purely before the end? @(br)
           Fixpoint magic! Lazy avoids exponential recomputation}
        @L{… Time for demo?})
     ($slide "Why did I care?"
        @L{Great Usenet Flamewars of 1990s: @(br)
           Dysfunctional Programming vs @(br)
            @(~ 18) Objectionable Disorientation @(br)
           Talking Past Each Other… for Decades}
        @L{1970s Lisp had both OO and FP @(br)
           2000s Fortress, Scala: with types}
        @L{The paper I wished I could have read younger})
     ($slide "Why So Much Blindness?"
        @L{Industry doesn’t care enough about Correctness @(br)
           Academia doesn’t grok Programming In The Large @(br)
           Both like to ignore the Human Factor}
        @L{All get stuck in their paradigms @(br)
           Opposition in mindset, not in reality})
     ($slide "Two Mindsets"
        @L{Compositionality for reducible problems @(br)
           Extensibility for irreducible problems}
        @L{Brighter than ambitious vs more ambitious than bright}
        @L{History: OO invented to model complex SIMULAtions @(br)
           History: FP to formalize computations in logic})
     ($slide "Meta-Thesis"
        @L{Humility, not fanaticism}
        @L{Incommensurable paradigms? Go wider!}
        @L{Researcher: old paradigm’s low-hanging fruits harvested}
        @L{Simplicity matters}
        @L{Racket, Scheme: λ’s for Semantics, macros for Syntax})
     ($slide "Thank You!"
        @L{Theory: @(~ 8) @Url{https://github.com/metareflection/poof}}
        @L{Practice: @(~ 5) Gerbil Scheme @Url{https://cons.io}}
        @L{X: @Url{https://x.com/ngnghm}  Blog: @Url{https://ngnghm.github.io}}
        @L{Hire me — or be hired by me! @(~ 5) @code{<fare@"@"mukn.com>}}
        @L{Plenty more research ideas, code and papers to write…}))))

(reveal-doc doc)
