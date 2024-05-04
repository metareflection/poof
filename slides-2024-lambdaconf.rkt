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
         (rename-in "util/coop.rkt" (|@| $))
         "util/protodoc.rkt"
         "util/reveal.rkt")

(def (make-table lists)
  (table style: "border-bottom-width:0;"
   (map (lambda (l)
          (tr style: "border-bottom-width:0;"
              (map (lambda (x) (td style: "border-bottom-width:0;" x)) l)))
        lists)))

(def ⟶ (list (~ 3) "⟶" (~ 2)))
(def (bri) (list (br) (~ 6)))

(def doc
  (docfix
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
            @b{Prototypes: @br Object-Orientation, @br Functionally}]
       @(br clear: 'all)
       @p{@small{@(~)}}
       @L{@code{@(~ 18) build = λ b m ↦ Y (m b)} @br
          @code{@(~ 18) inherit = λ c p u s ↦ c (p u s) s}}
       @p{@small{@(~)}}
       @C[style: "font-size: 66%"]{
           François-René Rideau @(email "<fare@mukn.com>")}
       @C{@small{@Url{http://github.com/metareflection/poof}}}
       @div[style: "font-size: 50%;" (~)]
       @C{@small{LambdaConf 2024-05-06}}
       @div[style: "font-size: 50%;" (~)]
       @table[style: "text-align: left; padding-left: 0; margin-left: 0; width: 100%; font-size: 50%;"
         (tr @td{@code{PgDn}: next} @td{@code{PgUp}: previous} @td{@code{↑ ↓ ← → ESC ⏎}
             @td{Touchscreen: swipe down until you must swipe right}})]))

    ($section "Prelude: OO vs FP?"
     $plan-slide
     ($slide @list{Language Wars: OO @em{vs} FP}
        @Li{Talking Past Each Other… for Decades} ;; Usenet Flamewars. Misunderstanding, Scorn.
        @Li{FP in Ivory Towers, OO in Industrial Slums}
        @Li{Videos, Books, Courses… all wrong}
        @Li{“Incommensurable paradigms” (Gabriel)}) ;; Dick Gabriel
     ($slide @list{Reluctant Peace: OO @em{and} FP}
        @Li{Yet, in practice: mutual adoption!}
        @Li{Modern OO languages adopted (limited) FP}
        @Li{Modern FP languages adopted (limited) OO}
        @Li{… Lisp: both unlimited since ~1976})
     ($slide @list{OO @em{for} FP}
        @Li{FP is my tribe — probably yours}
        @Li{Vast evidence that OO matters}
        @Li{FP can make its semantics simpler}
        @Li{Expand our Paradigm})
     ($slide @list{Scheme Workshop 2021 Paper}
        @Li{Why OO matters — Incremental Modularity}
        @Li{What OO is — Constructive Semantics in FP} ;; for systems of increasing sophistication
        @Li{Demystify fundamental concepts of OO}
        @Li{Prototypes before Classes, Purity before Mutation}))
    ($section "Introduction: Wherefore OO?"
     $plan-slide
     ($slide @list{What OO @em{isn’t} about}
        @Li{Classes}
        @Li{“Encapsulation”, “Information Hiding”}
        @Li{Inheritance vs Composition}
        @Li{Mutation everywhere}
        @Li{Message Passing everywhere})
     ($slide @list{What OO @em{is} about}
        @Li{Modularity: limited knowledge in}
        @Li{Incrementality: limited knowledge out}
        @Li{... Intralinguistically}
        @Li{A way to organize software development}
        @Li{... to support @em{Division of Labor}})
     ($slide @list{How OO @em{does} it}
        @Li{Ad Hoc Polymorphism: Existentially Quantified Types}
        @Li{Open Recursion: Compose Operators @em{before} Fixed-Point}
        @Li{... as First-Class (or Second-Class) Entities}
        @Li{A way to @em{factor} software}
        @Li{... in impressionist strokes})
     ($xslide @list{Incremental Specification}
        ;; TODO: split in many steps, finally make it a graph?
       @img[src: "resources/pic/inheritance-diagram.svg"
            alt: "Inheritance diagram for Integer-Keyed AVL Tree"
            height: "50%"
            width: "50%"
            valign: 'middle
            style: "
    vertical-align: middle;
    padding-left: 0em;
    padding-right: 0em;
    padding-top: 0em;
    padding-bottom: 0em;
"]
        #;(
        @L{@code{Tree ⊂ Type}}
        @L{@code{Binary_Tree ⊂ Tree}}
        @L{@code{Balanced_Binary_Tree ⊂ Binary_Tree}}
        @L{@code{Red_Black_Tree ⊂ Balanced_Binary_Tree}}
        @L{@code{AVL_Tree ⊂ Balanced_Binary_Tree}}
        @L{@code{Integer_Keyed_Tree ⊂ Tree}}
        @L{@code{MyTree ⊂ Integer_Keyed_Tree ∩ AVL_Tree}})))

    ($section "Minimal OO: Mixin Functions"
     $plan-slide
     ($slide "Simplest OO Concept"
        @L{@code{Target} = structures and algorithms to specify}
        @L{@code{Spec} = modular increment of specification}
        ;; build assumes the specification is *complete*
        ;; inherit assumes the specifications are compatible.
        ;; note: spec composition is not commutative in general
        @L{@code{build :: Top → Spec → Target}}
        @L{@code{inherit :: Spec → Spec → Spec}})
     ($slide "Simplest Spec, modeled as a Function…"
        @L{Input 1: (Incrementality) partial target so far @br
           Input 2: (Modularity) reference to complete target @br
           Output: (Building) further elaborated partial(?) target}
        @L{}
        @L{@code{type Spec = Partial → Complete → Partial}})
     ($slide "Mixin Function: Simplest Spec (w/ subtyping)"
        @L{@code{type Mixin inherited used defined = @bri
                   inherited → used → defined}}
        @;@L{@code{build :: (partial → target) → top → (top → target → partial) → target} @br
        @;   @code{build = λ wrap base mixin ↦ Y (λ self ↦ wrap (mixin base self))}}
        @L{@code{build :: top → (top → target → partial) → target} @br
           @code{build = λ base mixin ↦ Y (mixin base)}}
        @L{@code{inherit :: (i2⋂d1 → u2 → d2) → (i1 → u1 → d1) @bri
                         → i1 → u1⋂u2 → d1⋂d2} @br
           @; @code{inherit = λ c p u s ↦ c (p u s) s} @br
           @code{inherit = λ child parent super self @bri
                         ↦ child (parent super self) self}})
     ($slide "Is it OO at all? Aren’t objects records?"
        @Li{OO is @em{usually} about key-value records}
        @Li{Record = Product of indexed (or dependent) types}
        @Li{method, attribute, field, var, slot, property, member…}
        @Li{You can (and often do) have records without OO!})
     ;; write a slide with {}, {k: v, ...r}, {a: 3, b:4}.a
     ;; put example early
     ($slide "Record (whether Target or not, untyped)"
        @L{@code{rtop = λ _ ↦ error "No such method"} @br
           @code{rcons = λ key val rest msg @bri
                   ↦ if msg == key then val else rest msg}}
        @L{}
        @L{@code{point = rcons 'a 3 (rcons 'b 4 rtop)}}
        @L{@code{point 'a} @⟶ @code{3}})
     ($slide "Specifications for Records"
        @L{@code{kv = λ key val super self ↦ rcons key val super}}
        @L{}
        @L{}
        @L{@code{point = build rtop (inherit (kv 'a 3) (kv 'b 4))}}
        @L{@code{point 'a} @⟶ @code{3}})
     ($slide "Non-Constant Specifications"
        @L{@code{method = λ key fun super self @bri
                        ↦ rcons key (fun (super key) self) super}}
        @L{@code{fudgeX = method 'x @br
                 @(~ 5) λ self next ↦ (self 'fudge) * next}}
        @L{@code{multXY = method 'p @br
                 @(~ 5) λ self _ ↦ (self 'x) * (self 'y)}})
     ($slide "Mixins as simplest functional model for OO"
        @Li{Theory: Bracha & Cook, “Mixins” 1990 (w/ records)}
        @Li{Practice: Jsonnet 2014, Nix 2015 (w/ records, untyped)}
        @Li{Types: Oliveira 2009 (w/o records)}
        @Li{Plenty of less simple, less pure precedents}))
    ($section "Inheritance"
     $plan-slide
     ($slide "Inheritance: The Essence of OO"
        @Li{Composing mixins @em{before} fixed-point}
        @Li{What distinguishes OO from non-OO}
        @Li{Limited by how well you support subtyping}
        @Li{Single- Mixin- or Multiple- Inheritance})
     ($slide "Mixin Inheritance: The First shall be Last"
        @Li{Simplest form of inheritance… @em{given FP}}
        @Li{Relies on higher-order functions, fixed-points, laziness}
        @Li{Requires more elaborate types than usual for FP @bri
           … or dynamic types, unpopular within FP}
        @Li{Historically discovered last ’90, deployed recently 2006})
     ($slide "Single Inheritance: Historical First"
        @L{“prefix class” in Simula ’67, Smalltalk ’71}
        @L{@code{type Gen self = self → self} @(~ 19)(@code{top} is given)}
        @L{@code{buildGen :: Gen self → self} @(~ 19)(= @code{Y} itself!)}
        @L{@code{inheritGen :: Mixin s t t → Gen t → Gen s}}
        @L{You can apply / cons mixins but not compose / append them})
     ($slide "Mixin Inheritance beats Single"
        @Li{Mixin Inheritance: write once, use many}
        @Li{More for less: More expressive, more incremental})
     ($slide "Problem: Mixin Dependencies"
        @img[alt: "c3 linearization example"
             src: "resources/pic/C3_linearization_example.svg.png"]
        @Li{Pre-composing mixins leads to @bri
            @code{[Z K1 C O A O B O K3 A O B O D O K2 D O E O]}}
        @Li{Post-composing mixins decreases modularity})
     ($slide "Keep Mixin Dependencies Modular?"
        @Li{What if you change @code{K3} ?}
        @Li{Manual curation: @br
           @(~ 12) - users must update when dependencies change @br
           @(~ 12) - users must track @em{transitive} dependencies}
        @Li{Modularity: implementation computes @em{precedence list} @br
           @(~ 12) - users needn’t update when dependencies change @br
           @(~ 12) - users need only track @em{direct} dependencies})
     ($slide "Not a Problem for Single Inheritance"
        @Li{Single Inheritance too inexpressive to have the issue}
        @Li{Only one direct super, no duplicate transitive super}
        @Li{“prefix” property: precedence list for super-spec is a suffix}
        @Li{Flavors precedence list reverse of Simula “prefix” class list})
     ($slide "Multiple Inheritance beats Mixin"
        @img[alt: "c3 linearization example"
             src: "resources/pic/C3_linearization_example.svg.png"]
        @Li{Automatically @em{linearize} the DAG into a @em{precedence list}}
        @Li{e.g. @code{[Z K1 K3 K2 C A B D E O]}})
     ($slide "Multiple Inheritance: Precedence Lists"
        @Li{easy precedence lists: @code{[O] [A O] [B O] [C O]}}
        @Li{bad precedence lists: @br
           @(~ 20) @code{[K1 C O A O B O]} @br
           @(~ 20) @code{[Z K1 K2 K3 A B C D E O]}}
        @Li{C3: preserve coherence properties @br
           @(~ 20) @code{[K1 C A B O]} @br
           @(~ 20) @code{[Z K1 C K3 A K2 B D E O]}})
     ($slide "Combining Multiple and Single Inheritance"
        @Li{Specs with “prefix” property enable optimizations}
        @Li{CLOS: “class” vs “struct”. Scala: “trait” vs “class”}
        @Li{C4: additional coherence property for “prefix” specs}
        @Li{Optimal combination — implemented in Gerbil Scheme}))
    ($section "OO with(out) Objects"
     $plan-slide
     ($slide "Specifications: OO without Objects"
        @Li{So far, no Class, no Object!}
        @Li{Only Spec (no record) and Target (no inheritance)}
        @Li{Sufficient to produce… these slides}
        @Li{Yet, we recognize (almost?) all of OO}
        @Li{Entire Field a Misnomer… “Inheritance” > “OO”})
     ($slide "Targets beyond Records"
        @Li{Specs for arbitrary “Records”,
            @bri … with very different representations}
        @Li{Specs for numerical functions, for integers, etc.}
        @Li{But Records allow intermediate aspects of any type
            @bri … without weird low-level encodings})
     ($slide "Modular Extensibility"
        @Li{Spec for a vast DAG of Records}
        @Li{Which records in the DAG are extension points?}
        @Li{None | Exponential explosion | All of them}
        @Li{Or maybe stage all extensions before computation?})
     ($slide "Conflation: Proto = Spec × Target"
        @Li{Ubiquitous Extensibility at every level}
        @Li{Every “Prototype” is both Spec and Target @bri
            @code{Proto = Spec × Target}}
        @Li{Purity ⇒ Target unique up to observation}
        @Li{@em{THAT} is what Prototype OO calls an “object”})
     ($slide "Prototype OO"
        @Li{An “object” (or “instance”) is a Prototype (for a Record)}
        @Li{Prototype is @em{first-class} entity conflating Spec and Target}
        @nbsp
        @Li{Mutable: Director ’76, ThingLab ’77, T ’82, Self ’86, JS ’95}
        @Li{Pure: Jsonnet 2014, Nix 2015})
     ($slide "Conflation: Specs beyond Mixins"
        @Li{multiple inheritance: @bri
            @code{Spec = Mixin ? ? ? × (List Spec)}}
        @Li{default values: @bri
            @code{Spec = Mixin ? ? ? × (List Spec) × Record ?}}
        @Li{More: “No Such Message” handler, Assertions, Types…})
     ($slide "Conflation: Targets beyond Records"
        @Li{Target can be Conflation of Record, Function, etc.}
        @Li{Callable objects (Smalltalk, T, CLOS, C++, Scala, Java, etc.)}
        @Li{Proxies for any “kind” value}
        @Li{“Pure OO”: No privileged builtins})
     ($slide "Secret to Semantic Simplicity"
        @Li{Recognizing two distinct entities, Spec and Target}
        @Li{Conflation without Distinction = Confusion @bri
            Lack of awareness ⇒ formalizations that miss the point}
        @Li{Conflation with Distinction = Simplicity @bri
            “reasonable” semantics, superior ergonomics}
        @Li{Jsonnet (2014) [“Components” in T (1982)]}))
    ($section "Class = Proto Type Top"
     $plan-slide
     ($slide "Where are the Classes?"
        @Li{Simula ’67, Smalltalk, C++, CLOS, Java, C#, Python…}
        @Li{So much in common with Prototype OO… yet so different!}
        @Li{Is Class OO even the same paradigm?}
        @Li{What relationship between Prototypes and Classes?})
     ($slide "Type descriptors!"
        @Li{Target = descriptor for Type and functions (SML module)}
        @Li{Spec = add or override methods for Type}
        @Li{Spec vs Target = Abstract Class vs Concrete Class})
     ($slide "Class OO"
        @Li{An “object” (or “instance”) is @bri
            an @em{element} of a Class (seen as its Target Type)}
        @Li{A class is a @em{second-class} Prototype for a Type}
        @nbsp
        @Li{Mutable: Simula ’67, Smalltalk ’71, Flavors ’82, C++ ’85}
        @Li{Pure: OCaml(?) ’96, OOHaskell 2005})
     ($slide "From Type Descriptor to Type"
        @Li{Staging, Existential types, Dependent types…}
        @Li{Most PLs have limited type-level computations}
        @Li{Mixin vs single- vs multiple- inheritance matter}
        @Li{Singleton Class a bit like Prototype, but lacking dynamism})
     ($slide "Class vs Typeclass"
        @Li{Class vs Typeclass, vtable vs “dictionary”}
        @Li{Pre-vtable constructor vs burden of dictionary}
        @Li{Type-directed synthesis as meta-level computation}
        @Li{Haskell Typeclasses: good modularity, bad incrementality}
        @Li{Automated translation, see LIL (2012)}))
    ($section "(Im)Pure Challenges"
     $plan-slide
     ($slide "Pure Functional Specs and Targets" ;; TODO: move this slide later, or reframe
        @Li{Eager Y: only function targets, no sharing, recomputations}
        @Li{Lazy Y: any (delayed) value, sharing, no recomputations}
        @Li{Target as Computation vs Value (CBPV 1999)}
        @Li{Fixed-Point as co-inductive vs inductive})
     ($slide "Pure? Identity for DAG"
        @Li{Multiple Inheritance in Nix: no @code{===} for DAG}
        @Li{Unique tag: non-determinism? side-effect?}
        @Li{“Solutions”: State? Monads? Opaque tag? Explicit labels?}
        @Li{Labeling convention: moving computation to wetware})
     ($slide "Issues with Side-Effects"
        @Li{Many targets for a same spec? Cloning vs Sharing? @bri
            Dynamically modify a super? a list of supers?}
        @Li{Class OO: all prototypes are pure… at compile-time}
        @Li{Linearity: more Optimizations, harder Enforcement}
        @Li{Caching: more Sharing, harder Invalidation})
     ($slide "Pure? Non-local specification increments"
        @Li{Multimethods, Mutually recursive classes, Friend classes?}
        @Li{Haskell problem: orphan typeclass targets}
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
        @Li{Incrementality & Modularity… intralinguistically}
        @Li{Mixin Functions, compose (append) beyond apply (cons)}
        @Li{Multiple inheritance: @(~ 3) modular dependencies}
        @Li{Conflation: @(~ 4) @code{Proto = Spec × Target}}
        @Li{@code{Class = Proto Type Top}})
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
        @Li{Practice: @(~ 5) @Url{https://github.com/fare/gerbil-poo}}
        @Li{OO in 30 loc — 80 loc with multiple inheritance}
        @Li{Hire me! @(~ 5) @code{<fare@"@"mukn.com>}}))))

(reveal-doc doc)
