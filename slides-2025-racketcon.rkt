#lang at-exp racket @; -*- Scheme -*-
#|
Compositional Object Oriented Prototypes

Slides for presentation at (fifteenth RacketCon), 2025-10-04

To compile it, use:
  racket slides-2025-racketcon.rkt > build/slides-2025-racketcon.html

To test interactively, try:
  racket -i -l scribble/reader -e "(use-at-readtable)" -l racket

Precompiled version:
http://fare.tunes.org/files/cs/poof/slides-2025-racketcon.html

Example code:
http://fare.tunes.org/files/cs/poof/mini-coop.rkt

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

(define c code)

(def doc
  (docfix
    ($title "Compositional Object-Oriented Prototypes")
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
            @b{Compositional @br Object-Oriented @br Prototypes}]
       @(br clear: 'all)
       @p{@small{@~}}
       @L[style: "font-size: 80%;"]{@c{(define (instantiate s) (Y (λ (m i) (s m i ⊤))))}}
       @L[style: "font-size: 80%;"]{@c{(define (inherit c p m i) (compose (c m i) (p m i)))}}
       @p{@small{@~}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @c{@small{@Url{http://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @c{@small{(fifteenth RacketCon) 2025-10-04}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@c{PgDn}: next} @td{@c{PgUp}: previous} @td{@c{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))
    ($section "Introduction: OO, Informally"
     $plan-slide
     ($slide "Claims"
        @L{OO is Internal Modular Extensibility @br
            We rebuild OO atop pure FP from these first principles}
        @L{Classes, mutation, objects(!) are not fundamental @br
            @; — can be done without or added on top @br
            Key ignored semantic notion: Conflation}
        @L{Mixin Inheritance: most fundamental, not most modular @br
            Best Inheritance: combine Single and Multiple})
     ($slide @list{What OO @em{is not} about}
        #| There are many things many people wrongly believe are necessary or sufficient for OO
           (because that matches what little OO they know).
           (Ignorance is not a good argument.)
           No time for a good debunking now — read the paper. |#
        @Li{C++ (or even Java, etc.)}
        @Li{Classes}
        @Li{“Encapsulation”, “Information Hiding”}
        @Li{Opposite of FP, Inheritance vs Composition}
        @Li{Mutable records everywhere}
        @Li{Asynchronous Message Passing everywhere}
        @Li{UML / Co-Algebras / Relational Modeling})
     ($slide @list{What OO @em{is} about}
        @L{Modularity: dev needs little knowledge @em{in}} ;; ⟶ Ad Hoc Polymorphism,
        @L{Extensibility: dev needs little knowledge @em{out}} ;; ⟶ Open Recursion
        @L{Language-internal: first-class or second-class} ;; First-class
        @~
        @L{Support for @em{Division of Labor}} ;; across developers, across time
        @~
        @L{Mechanism: Inheritance})
     ($slide @list{Internal vs External}
        @L{First class: internal, runtime (values, some via reflection)}
        @L{Second class: internal, compile-time (types, macros)}
        @~
        @L{Third class: external, software (tooling)}
        @L{Fourth class: external, wetware (design patterns)}))
    ($section "Minimal OO, Formally"
     $plan-slide
     ($slide "Minimal First-Class Semantics"
        @L{You only really understand it if you can program it}
        @L{Second-class is just first-class at compile-time @br
           Any semantics is necessarily first-class for @em{someone}}
        @L{A non-minimal model means you’re still confused})
     ($slide "Minimal First-Class Extensibility: What"
        @L{Values: @c{V} @br
           Extensions: @c{V → V}}
        @L{e.g. for type @c{V = Record} @br
           @c{(define point-p (record (x 2) (y 4))) @br
              (define (paint-blue p) @br
                @~ (extend-record p 'color 'blue))}})
     ($slide "Minimal First-Class Extensibility: How"
        @L{Good: @em{Apply} extensions @br @; extensions are magic constants
                 @c{@~ (ext value)}}
        @L{Better: @em{Compose} extensions @br @; now you have a rich algebra of extensions
                 @c{@~ (compose ext1 ext2)}}
        @L{... important if restricted extension language}) @; especially for second-class extensions
     ($slide "Minimal First-Class Extensibility: Top"
        @L{To get @c{V} from @c{V → V}, apply to top value @c{⊤}}
        @L{Typical choices: @br
           @(~ 2) @c{V = Record}, @c{⊤ = (record)} @br
           @(~ 2) @c{V = Number}, @c{⊤ = 0} @br
           @(~ 2) @c{V = Pointer}, @c{⊤ = null} @br @; NULL value, (void), #f, nil, etc.
           In pure partial lazy FP, @c{⊤ = (lazy ⊥)}} @; universal top value
        @L{Bad: use fixpoint combinator @c{Y : (V → V) → V}})
     ($slide "Minimal First-Class Modularity: Records"
        @L{Values: @c{V}, @c{W}, @c{X}... @br
           Identifiers: @c{I}, @c{J}. @br
           Set of bindings, a.k.a. record, a.k.a. indexed product: @br
           @(~ 2) @c{∏R = i:I → Rᵢ}}
        @L{Example: @br
           @c{R = {x: Number, y: Number, color: Symbol} @br
           (define point-q @br
           @~ (record (x 3) (y 4) (color 'blue)))}})
     ($slide "Minimal First-Class Modularity: Spec"
        @L{Module context: @c{∏M = i:I → Mᵢ} @br
           Modular specification for @c{X}: @c{∏M → X}}
        @L{Example: @br
           @c{M = { ls: Str → List(Str), @br
           @(~ 5) sort: List(Str) → List(Str) } @br
           (define (ls-sorted m) @br
           @~ (compose (m 'sort) (m 'ls)))}})
     ($slide "Minimal First-Class Modularity: What"
        @L{modular module specification: @br
           Given a common module context @c{∏M}, @br
           for each identifier @c{j}, a value of type @c{Xⱼ}: @br
           @c{∏M → ∏X = ∏M → j:J → Xⱼ} @br
           @c{∏M} module context, record of identifiers @em{referenced} @br
           @c{∏X} module body, record of identifiers @em{defined}.})
     ($slide "Minimal First-Class Modularity: Y"
        @L{Close modular spec: @c{∏M → ∏M} @br
           every identifier referenced is defined}
        @L{Extract target record @c{∏M} from spec @c{∏M → ∏M} ? @br
           link references, close open loops, tie knots, … @br
           @R{@em{fixpoint combinator} @c{Y} @(~ 4)}})
     ($slide "Digression: Scheme vs FP"
        @L{Issue 1: pure applicative Y sucks @br
            Solution: stateful Y, lazy Y, or second class Y}
        @L{Issue 2: unary functions are syntax-heavy @br
            Solution: cope, autocurry, or multiple arities with care}
        @L{@c{coop.rkt} @em{choices}: stateful Y (letrec), autocurry}) ;; Obviously, YMMV
     ($slide "Minimal First-Class Modular Extensibility"
        @L{Extensions: @c{X → X} @br
           Modularity context: @c{∏M} @br
           Open modular extensible spec: @c{∏M → ∏(X → X)}}
        @L{Close modular extensible spec: @c{∏M → ∏(M → M)} @br
           Resolve each binding — apply to top value @c{⊤} @br
           Target from reduced modular spec @c{∏M → ∏M} — use @c{Y}})
     ($slide "Minimal “Object-Orientation”"
        @L{@c{(define (instantiate spec) @br
                 @(~ 2) (Y (λ (m i) (spec m i ⊤)))) @br
              (define (inherit child parent m i) @br
                 @(~ 2) (compose (child m i) (parent m i)))}}
        @L{That's mixin inheritance, all the OO you need!}
        @L{@c{(deftype spec (Fun M → I → X → X)) @br
              (define (my-spec self method-id super) @br
                   @(~ 2)...value)}}) ;; id-spec super
     ($slide "Minimal Example"
        @L{@c{(define (coord-spec self i super) @br
                    @(~ 1) (case i ((x) 2) ((y) 4) (else super))) @br
                 (define (color-spec self i super) @br
                    @(~ 1) (case i ((color) 'blue) (else super)))}}
        @L{@c{(define point-p (instantiate @br
                    @(~ 2) (inherit coord-spec color-spec))) @br
                 (point-p 'x) ⇒ 2 @br
                 (point-p 'color) ⇒ blue}})
     ($slide "Minimal Example, non-trivial inheritance"
        @L{@c{(define (add-x-spec dx self i super) @br
                   @(~ 1) (case i ((x) (+ dx super)) @br
                   @(~ 9) (else super))) @br
                 @br
                 (define (rho-spec self i super) @br
                   @(~ 1) (case i ((rho) @br
                   @(~ 4) (sqrt (+ (sqr (self 'x)) @br
                   @(~ 13) (sqr (self 'y))))) @br
                   @(~ 9) (else super)))}})
     ($slide "Minimal Example, non-trivial inheritance (test)"
        @L{@c{(define point-r (instantiate @br
                    @(~ 2) (inherit (add-x-spec 1) @br
                    @(~ 4) (inherit coord-spec @br
                    @(~ 6) rho-spec)))) @br
                    @br
                 (point-r 'x) ⇒ 3 @br
                 (point-r 'rho) ⇒ 5}}))
    ($section "Wait, what?"
     $plan-slide
     ($slide "What did we just do?"
        @L{Reconstructed recognizable OO from first principles}
        @L{The first principles: @em{first-class, modularity, extensibility}}
        @~
        @L{OO literally in two short definitions, in any FP language}
        @~
        @L{No classes, no mutation, no objects!}
        @~
        @L{How is it even possible???})
     ($slide "Precedents" ;; (modulo trivial refactorings)
        @L{Yale T Scheme 1982, YASOS 1992 (applicative, stateful)}
        @L{Bracha & Cook 1990: theoretical “model” of inheritance}
        @L{GCL 2004: Runs all Google (lazy, dynamic scope)} ;; has objects
        @L{Jsonnet 2014: GCL cleanup (lexical scope, JS-y syntax)}
        @L{Nix extensions 2015: isomorphic to above, OO in 2 funs})
     ($slide "OO without objects"
        @L{“target” is any value @br
           @em{No inheriting from a “target”}} ;; yes extending it, but non-modularly
        @L{“specification” is not a record, but a function @br
           @em{No computing methods from a “specification”}} ;; not quite a record
        @L{No equivalent to object (or class) in other OO languages! @br
           ... contrast with GCL, Jsonnet, Nix, that have objects(!?)})
     ($slide "Conflation: hidden product, implicit cast"
        @L{Prototype = Spec × Target @br
           Target = lazily resolved fixpoint from spec}
        @L{Want to compute a method? Use the target @br
           Want to inherit? Use the spec}
        ;; Small, partial, incremental specifications are the whole point
        @L{Lazy is essential: most specifications are partial} ;; would fail resolution
        @L{Prototype OO: Ani 1976, ThingLab 1977, SELF 1986, @br
           @strong{JavaScript 1995}, GCL 2004, Jsonnet 2014, Nix 2015}))
    ($section "What about my favorite OO feature?"
     $plan-slide
     ($slide "What about Classes?"
        @L{Class = (Second-class) Prototype for Type (+ methods) @br
           Object, Instance = element of the target type}
        @L{static methods = methods of the target type @br
           object methods = static methods applied to the element}
        @L{abstract class = used only for its spec @br
           concrete class = used only for its target}
        @L{C++ templates: pure lazy FP Prototypes at compile-time})
     ($slide "What about Objects?"
        @L{T: "object" is any value, "instance" is target from spec @br
        @; i.e. Prototype is conflated Specification × Target
           Prototype OO: "object / instance" is Spec × Target @br
           Class OO: "object / instance" is Target type element}
        @L{"object" and "instance" are very ambiguous words @br
           "object" as a concept is not necessary for OO}
        @L{Fields are named first, understood much later.})
     ($slide "What about Types?"
        @L{Spec Subtyping: @em{before fixpoint} @br
           Target Subtyping: @em{after fixpoint} @br
           @C{@strong{SUBTYPING and FIXPOINT @em{DO NOT} COMMUTE!}}}
        @L{Fortress 2011 Type checking modular multiple dispatch @br
           @(~ 4) with parametric polymorphism and multiple inheritance @br
           Scala 2012 Dependent Object Types}
        @L{Prototype OO? Dunno, maybe dependent types?})
     ($slide "What about Typeclasses or Modules?"
        @L{Haskell Typeclass, Rust Trait, ML modules... @br
           @(~ 4) @strong{modular but @em{not extensible}} @br
           Contra Cook: extensibility matters!}
        @L{Interface Passing Style 2012: extensible typeclasses @br
           Isomorphism via macros: @br
           @(~ 8) class ≃ typeclass @br
           @(~ 8) (linear) pure ≃ stateful})
     ($slide "What about side-effects?"
        @L{Classes: pure lazy Prototype OO at compile-time}
        @L{Target types historically mutable, but don’t have to be}
        @L{Plenty of pure object libraries in Lisp, Java, Scala…}
        @L{Mutable inheritance: @strong{semantics is hard}, like all mutation @br
           CLOS @c{update-instance-for-redefined-class} @br
           invalidate caches (atomically?)})
     ($slide "Multiple dispatch? Method combination?"
        @L{both: LOOPS 1986, CLOS 1991, Dylan 1992}
        @L{multiple dispatch only: Cecil 1992, Fortress 2006, Julia 2012}
        @L{“generic functions”, see CLOS for docs and experience @br
           see Fortress for proper types}
        @L{Pure FP vs orphan/friend/mutually-def'd (type)classes? @br
           Global fixpoint of entire namespace (nixpkgs…)})
     ($slide "What about single or multiple inheritance?"
        @L{Worth a section of its own (see next)}
        @L{Lowdown: @br
           @(~ 2) mixin inheritance is simplest @br
           @(~ 2) single inheritance is most efficient, least expressive @br
           @(~ 2) multiple inheritance is most modular @br
           @(~ 2) You can have the best of them together!}))
    ($section "Inheritance: Mixin, Single, Multiple"
     $plan-slide
     ($slide "Single Inheritance"
        @L{Hoare 1966 (sub)classes (handwaving) @br @; also NULL "billion dollar mistake"
           SIMULA 1967 “Prefix classes” (first implementation) @br @; also suffix, hence BETA "inner"
           Smalltalk 1976 “single inheritance” (compromise) @br
           @(~ 2) C-with-classes 1979, Java 1996, C# 2000, @br
           @(~ 2) COBOL 2002, JavaScript 2015...} ;; wildfire
        @L{Very efficient, simplest for 1960s before cheap lambdas @br
           Least expressive, least modular})
#|
(define (base-single self n) #f)
(define (inherit-single spec parent self n) (spec self (parent self) n))
(define (instantiate-single spec) (letrec ((self (λ (n) (spec self n)))) self))
(define (field k f self super n) (if (eq? n k) (f self (super n)) (super n)))
(define (my-spec self super n) ...new-value)
|#
     ($slide "Multiple Inheritance"
        @L{KRL 1975 “inheritance of properties” Ani 76, ThingLab 77 @br
           Method @em{conflict} if in parallel superclasses: @br
           @; Sad, self-defeating worldview, less expressive, less extensible, less modular
           @(~ 2) Mesa 1979, SELF 1986, C++ 1989, ADA 2003 @br @; Julia 2012
           Method @em{combination} after linearizing inheritance DAG: @br @; Improved by Dylan's C3 1996
           @(~ 2) Flavors 1979, LOOPS 1986, CLOS 1991 @br @; Happy, Harmonious worldview
           Simple combination, can only call super method: @br
           @(~ 2) Ruby 1992, Python 1994, Scala 2004} @; Dull, boring worldview
        @L{Most expressive, most modular @br
           Most complex (~100 loc), somewhat inefficient in general})
     ($slide "Mixin Inheritance"
        @L{Yale T Scheme 1982: possible, but not conceptualized @br
           Bracha/Cook 1990: conceptualized, not implemented}
        @L{PLT Scheme 1998, Strongtalk 2002, GCL 2004, Newspeak 2006, Jsonnet 2014, Nix 2015}
        @L{Simplest in FP, more expressive & modular than Single @br
           As inefficient as Multiple Inheritance, less modular})
     ($slide "Mixin more Expressive than Single"
        @L{Second-class OOP: expressiveness issue. @br
           Single only @c{cons} a spec, not @c{append} spec @br
           Less sharing, more duplication, extra hoops @br
           … maintenance nightmare.}
        @L{First-class OOP: your own mixins on top (PLT 1998) @br
           Still extra hoops, extra concepts, extra complexity})
     ($slide "Multiple More Modular than Mixin"
        @L{Spec depends on others to avoid source duplication @br
           Can’t “just” pre-append: runtime duplication, bad, @br
           @(~ 2) exponential, non-commutative, non-idempotent}
        @L{Mixin inheritance: @em{manually} manage linearization @br
           A spec’s inheritance DAG becomes part of its interface @br
           Nightmare on transitive dependency change}
        @L{Multiple inheritance: just declare direct superspecs @br
           No maintenance nightmare, just bliss.})
     ($slide "Single and Multiple Together"
        @L{Two separate hierarchies (lame): CLOS (@strong{struct/class}) @br
           Same hierarchy (yay): Ruby (class/module), @br
           @(~ 2) Scala (class/trait), Gerbil Scheme (@strong{struct/class})}
        @L{“struct” can inherit from “class” and vice-versa @br
           struct only in “single inheritance” wrt other structs}
        @L{“tail property”: the property actually needed for efficiency @br
           @em{a struct’s class linearization is a suffix to its subclasses’s}}))
    ($section "Conclusion: Making sense of OO"
     $plan-slide
     ($slide "Claims (Redux)"
        @L{OO is Internal Modular Extensibility @br
            We rebuild OO atop pure FP from these first principles}
        @L{Classes, mutation, objects(!) are not fundamental @br
            @; — can be done without or added on top @br
            Key ignored semantic notion: Conflation}
        @L{Mixin Inheritance most fundamental Inheritance @br
            Best: combine Single and Multiple Inheritance})
     ($slide "This very Presentation!"
        @L{@c{scribble} + @c{coop.rkt} spit HTML for @c{reveal.js}}
        @L{No objects, just specs and targets, mixin inheritance @br
           FP+OO system in 350 loc incl. autocurry, comments @br
           Prototypes for slides w/ toc in ~125 loc}
        @L{How can TOC appear purely before the end? @br
           Fixpoint magic! Lazy avoids exponential recomputation}
        @L{… Time for demo?})
     ($slide "Why did I care?"
        @L{Great Usenet Flamewars of 1990s: @br
           Dysfunctional Programming vs @br
            @(~ 18) Objectionable Disorientation @br
           Talking Past Each Other… for Decades}
        @L{1970s Lisp had both OO and FP @br
           2000s Fortress, Scala: with types}
        @L{Opposition in Mind, not in Reality})
     ($slide "Why So Much Blindness?"
        @L{Industry doesn’t care enough about Correctness @br
           Academia doesn’t grok Programming In The Large}
        @L{Both ignore the Human Factor: @br
           interchangeable cogs, transient students}
        @L{All get stuck in their paradigms})
     ($slide "Two Mindsets"
        @L{Compositionality for reducible problems @br
           Extensibility for irreducible problems}
        @L{Brighter than ambitious vs more ambitious than bright}
        @L{History: OO invented to model complex SIMULAtions @br
           History: FP to formalize simple computations in logic})
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
        @L{Hire me — or be hired by me! @(~ 5) @c{<fare@"@"mukn.com>}}
        @L{Plenty more research ideas, code and papers to write…}))))

(reveal-doc doc)
