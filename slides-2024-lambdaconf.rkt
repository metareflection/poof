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
(def (brii) (list (br) (~ 12)))
(def (briii) (list (br) (~ 18)))

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
          @code{@(~ 18) inherit = λ p c u s ↦ c (p u s) s}}
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
        @L{Talking Past Each Other… for Decades} ;; Usenet Flamewars. Misunderstanding, Scorn.
        @L{FP in Ivory Towers, OO in Industrial Slums}
        @L{Videos, Books, Courses… all wrong}
        @L{“Incommensurable paradigms” (Gabriel)}) ;; Dick Gabriel
     ($slide @list{Reluctant Peace: OO @em{and} FP}
        @L{Yet, in practice: mutual adoption!}
        @L{Modern OO languages adopted (limited) FP}
        @L{Modern FP languages adopted (limited) OO}
        @L{… Lisp: both unlimited since ~1976})
     ($slide @list{OO @em{for} FP}
        @L{FP is my tribe — probably yours}
        @L{Vast evidence that OO matters}
        @L{To elucidate its semantics — use FP}
        @L{Expand our Paradigm})
     ($slide @list{Scheme Workshop 2021 Paper}
        @L{Why OO matters — Incremental Modularity}
        @L{What OO is — Constructive Semantics in FP} ;; for systems of increasing sophistication
        @L{Demystify fundamental concepts of OO}
        @L{Prototypes before Classes, Purity before Mutation}))
    ($section "Introduction: Wherefore OO?"
     $plan-slide
     ($slide @list{What OO @em{isn’t} about}
        @L{Classes}
        @L{“Encapsulation”, “Information Hiding”}
        @L{Inheritance vs Composition}
        @L{Mutation everywhere}
        @L{Message Passing everywhere})
     ($slide @list{What OO @em{is} about}
        @L{Modularity: limited knowledge in}
        @L{Incrementality: limited knowledge out}
        @L{... Intralinguistically}
        @L{A way to organize software development}
        @L{... to support @em{Division of Labor}})
     ($slide @list{How OO @em{does} it}
        @L{Ad Hoc Polymorphism: Existentially Quantified Types}
        @L{Open Recursion: Compose Operators @em{before} Fixed-Point}
        @L{... as First-Class (or Second-Class) Entities}
        @L{A way to @em{factor} software}
        @L{... in impressionist strokes})
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
        @L{@code{Target} = data structures and algorithms to specify}
        @L{@code{Spec} = modular increment of specification}
        ;; build assumes the specification is *complete*
        ;; inherit assumes the specifications are compatible.
        ;; note: spec composition is not commutative in general
        @L{@code{build :: Base → Spec → Target}}
        @L{@code{inherit :: Spec → Spec → Spec}})
     ($slide "Mixins: Simplest Spec, modeled as function…"
        @L{Input 1: (Incrementality) partial target so far as @em{inherited} @br
           Input 2: (Modularity) complete target as @em{referenced} @br
           Output: (Building) further elaborated target as @em{defined}} @; partial(?)
        @L{@code{type Spec = Target → Target → Target}}
        @L{We call specs-as-functions “mixins” (or “mixins functions”)}
        @L{@code{mymixin = λ super self ↦ extend super …}})
     ($slide "Challenge: Typing Mixins"
        @L{@code{type Spec = Target → Target → Target}}
        @L{Problem: input 1 and output are @em{partial} targets}
        @L{Easy Solution / Workaround: use dynamic types}
        @L{@code{type Target = String → Any}})
     ($slide "Actual Solution: Subtyping"
        @L{@code{type Mixin inherited referenced defined = @bri
                      inherited → referenced → inherited⋂defined}}
        @;@L{The typesystem needs subtypes and/or type intersections}
        @L{More modular variant for reuse in many contexts:}
        @L{@code{type Mixin inherited referenced defined = @bri
                        ∀ super ⊂ inherited ⇒ @brii
                           super → referenced → super⋂defined}})
     ($slide "Minimal Kernel for Mixin Functions"
        @L{@code{type Mixin i r d = ∀ s ⊂ i ⇒ s → r → s⋂d}}
        @L{@code{build :: top → Mixin top target target → target @br
                 build = λ base mixin ↦ Y (mixin base)}}
        @L{@code{inherit :: Mixin i1 r1 d1 → Mixin i2⋂d1 r2 d2 → @bri
                         Mixin i1⋂i2 r1⋂r2 d1⋂d2 @br
                 inherit = λ parent child super self ↦ @bri
                         child (parent super self) self}})
     ($slide "Where are my methods?" ;; Is it OO at all? Aren’t objects records?
        @L{OO is @em{usually} about key-value records}
        @L{Record = Product of indexed types}
        @L{Dictionary mapping @code{String} to dependent type}
        @L{method, attribute, field, var, slot, property, member…}
        @L{You can (and often do) have records without OO!})
     ;; write a slide with {}, {k: v, ...r}, {a: 3, b:4}.a
     ;; put example early
     ($slide "Record (whether Target or not, untyped)"
        @L{@code{rtop = λ _ ↦ error "No such method"} @br
           @code{rcons = λ key val rest msg ↦ @bri
                   if msg == key @brii
                    then val @brii
                    else rest msg}}
        @L{@code{point = rcons "a" 3 (rcons "b" 4 rtop)}}
        @L{@code{point "a"} @⟶ @code{3}})
     ($slide "Incremental Specification of Records"
        @L{Mixin to define one key-value record binding:}
        @L{@code{kv = λ key val super self ↦ @bri
                   rcons key val super}} @; It's a mixin
        @L{@code{point = build rtop @bri
                       (inherit (kv "b" 4) (kv "a" 3))}}
        @L{@code{point "a"} @⟶ @code{3}})
     ($slide "Specifying a Non-Constant Value"
        @L{@code{method = λ key fun super self ↦ @bri
                        rcons key (fun (super key) self) super}}
        @L{@code{fudgeX = method "x" @bri
                   λ next self ↦ (self "fudge") * next @br
                 multXY = method "p" @bri
                   λ _ self ↦ (self "x") * (self "y")}})
     ($slide "Mixins as simplest functional model for OO"
        @L{Theory: Bracha & Cook, “Mixins” 1990 (w/ records)}
        @L{Practice: Jsonnet 2014, Nix 2015 (w/ records, untyped)}
        @L{Types: Oliveira 2009 (w/o records)}
        @L{Plenty of less simple, less pure precedents}))
    ($section "Inheritance"
     $plan-slide
     ($slide "Inheritance: The Essence of OO"
        @L{Composing mixins @em{before} fixed-point}
        @L{What distinguishes OO from non-OO}
        @L{Limited by how well you support subtyping}
        @L{3 flavors: Single- Mixin- or Multiple-})
     ($slide "Mixin Inheritance: The First shall be Last"
        @L{Simplest form of inheritance… @em{given FP}}
        @L{Relies on higher-order functions, fixed-points, laziness}
        @L{Requires more elaborate subtypes than usual for FP @bri
           … or dynamic types, unpopular within FP}
        @L{Historically discovered last ’90, deployed recently 2006})
     ($slide "Single Inheritance: Historical First"
        @L{@code{type Gen self = self → self @br
                 buildGen :: Gen self → self @br
                 buildGen = Y @br
                 inheritGen :: Gen s → Mixin s t t → Gen s⋂t @br
                 inheritGen = λ parent mixin self ↦ @bri
                   mixin (parent self) self}}
        @L{“prefix class” in Simula ’67, Smalltalk ’71})
     ($slide "Mixin Inheritance beats Single"
        @L{Single inheritance can apply mixins, not compose them}
        @L{Add one mixin at a time at the end of a prefix list}
        @L{Mixin Inheritance: write mixin once, use many}
        @L{More for less: More expressive, more incremental}
        @C{(… Matters because mixins are second-class, at type-level)})
     ($slide "Problem: Mixin Dependencies"
        @img[alt: "c3 linearization example"
             src: "resources/pic/C3_linearization_example.svg.png"]
        @L{Pre-composing mixins is wrong @br
            @code{[O] [O E] [O D] [O B] [O E O D O B K2]} @br
            @code{[O E O D O B K2 O D O A K3 O B O A O C K1 Z]}}
        @L{Post-composing mixins decreases modularity})
     ($slide "Keep Mixin Dependencies Modular?"
        @L{What if you change a node ?}
        @L{Manual curation: @br
           @(~ 12) - users must update when dependencies change @br
           @(~ 12) - users must track @em{transitive} dependencies}
        @L{Modularity: implementation computes @em{precedence list} @br
           @(~ 12) - users needn’t update when dependencies change @br
           @(~ 12) - users need only track @em{direct} dependencies})
     ($slide "Not a Problem for Single Inheritance"
        @L{Single Inheritance too inexpressive to have the issue}
        @L{Only one direct super, no duplicate transitive super}
        @L{prefix property: super’s precedence list is a prefix @br}
        @C{⚠️ precedence list often in reverse order, most specific first})
     ($slide "Multiple Inheritance beats Mixin"
        @img[alt: "c3 linearization example"
             src: "resources/pic/C3_linearization_example.svg.png"]
        @L{Automatically @em{linearize} the DAG into a @em{precedence list}}
        @L{e.g. @code{[O E D B A C K2 K3 K1 Z]}})
     ($slide "Multiple Inheritance: Precedence Lists"
        @L{easy precedence lists: @code{[O] [O A] [O B] [O C]}}
        @L{bad precedence lists: @br
           @(~ 20) @code{[O B O A O C K1]} @br
           @(~ 20) @code{[O A B C D E K1 K2 K3 Z]}}
        @L{C3: preserve coherence properties @br
           @(~ 20) @code{[O B A C K1]} @br
           @(~ 20) @code{[O E D B K2 A K3 C K1 Z]}})
     ($slide "Combining Multiple and Single Inheritance"
        @L{Specs with “prefix” property enable optimizations}
        @L{CLOS: “class” vs “struct” — Scala: “trait” vs “class”}
        @L{C4: additional coherence property for “prefix” specs}
        @L{Optimal combination — implemented in Gerbil Scheme}))
    ($section "OO with(out) Objects"
     $plan-slide
     ($slide "Specifications: OO without Objects"
        @L{So far, no Class, no Object!}
        @L{Only Spec (no record) and Target (no inheritance)}
        @L{Sufficient to produce… these slides}
        @L{Yet, we recognize (almost?) all of OO}
        @L{Entire Field a Misnomer… “Inheritance” > “OO”})
     ($slide "Targets beyond Records"
        @L{Specs for arbitrary “Records”,
            @bri … with very different representations}
        @L{Specs for numerical functions, for integers, etc.}
        @L{But Records allow intermediate aspects of any type
            @bri … without weird low-level encodings})
     ($slide "Modular Extensibility"
        @L{Spec for a vast DAG of Records}
        @L{Which records in the DAG are extension points?}
        @L{None | Exponential explosion | All of them}
        @L{Or maybe stage all extensions before computation?})
     ($slide "Conflation: Proto = Spec × Target"
        @L{Ubiquitous Extensibility at every level}
        @L{Every “Prototype” is both Spec and Target @bri
            @code{Proto = Spec × Target}}
        @L{Purity ⇒ Target unique up to observation}
        @L{@em{THAT} is what Prototype OO calls an “object”})
     ($slide "Prototype OO"
        @L{An “object” (or “instance”) is a Prototype (for a Record)}
        @L{Prototype is @em{first-class} entity conflating Spec and Target}
        @nbsp
        @L{Mutable: Director ’76, ThingLab ’77, T ’82, Self ’86, JS ’95}
        @L{Pure: Jsonnet 2014, Nix 2015})
     ($slide "Conflation: Specs beyond Mixins"
        @L{multiple inheritance: @bri
            @code{type Spec = Mixin × (List Spec)}}
        @L{default values: @bri
            @code{type Spec = Mixin × (List Spec) × Record}}
        @L{More: “No Such Message” handler, Assertions, Types…})
     ($slide "Conflation: Targets beyond Records"
        @L{Target can be Conflation of Record, Function, etc.}
        @L{Callable objects (Smalltalk, T, CLOS, C++, Scala, Java, etc.)}
        @L{Proxies for any “kind” value}
        @L{“Pure OO”: No privileged builtins})
     ($slide "Secret to Semantic Simplicity"
        @L{Recognizing two distinct entities, Spec and Target}
        @L{Conflation without Distinction = Confusion @bri
            Lack of awareness ⇒ formalizations that miss the point}
        @L{Conflation with Distinction = Simplicity @bri
            “reasonable” semantics, superior ergonomics}
        @L{Jsonnet (2014) [“Components” in T (1982)]}))
    ($section "Class = Proto Type"
     $plan-slide
     ($slide "Where are the Classes?"
        @L{Simula ’67, Smalltalk, C++, CLOS, Java, C#, Python…}
        @L{So much in common with Prototype OO… yet so different!}
        @L{Is Class OO even the same paradigm?}
        @L{What relationship between Prototypes and Classes?})
     ($slide "Type descriptors!"
        @L{Target = descriptor for Type and functions (SML module)}
        @L{Spec = add or override methods for Type}
        @C{… all at the type-level, at compile-time!}
        @L{Spec vs Target = Abstract Class vs Concrete Class})
     ($slide "Class OO"
        @L{An “object” (or “instance”) is @bri
            an @em{element} of a Class (seen as its Target Type)}
        @L{A class is a @em{second-class} Prototype for a Type}
        @nbsp
        @L{Mutable: Simula ’67, Smalltalk ’71, Flavors ’82, C++ ’85}
        @L{Pure: OCaml(?) ’96, OOHaskell 2005})
     ($slide "From Type Descriptor to Type"
        @L{Staging, Existential types, Dependent types…}
        @L{Most PLs have limited type-level computations}
        @L{Mixin- vs Single- vs Multiple- inheritance matters}
        @L{Singleton Class a bit like Prototype, but lacking dynamism})
     ($slide "Class vs Typeclass"
        @L{Class vs Typeclass, vtable vs “dictionary”}
        @L{Pre-vtable constructor vs burden of dictionary}
        @L{Type-directed synthesis as meta-level computation}
        @L{Haskell Typeclasses: good modularity, bad incrementality}
        @L{Automated translation, see LIL (2012)}))
    ($section "(Im)Pure Challenges"
     $plan-slide
     ($slide "Pure Functional Specs and Targets" ;; TODO: move this slide later, or reframe
        @L{Eager Y: only function targets, no sharing, recomputations}
        @L{Lazy Y: any (delayed) value, sharing, no recomputations}
        @L{Target as Computation vs Value (CBPV 1999)}
        @L{Fixed-Point as co-inductive vs inductive})
     ($slide "Pure? Identity for DAG"
        @L{Multiple Inheritance in Nix: no @code{===} for DAG}
        @L{Unique tag: non-determinism? side-effect?}
        @L{“Solutions”: State? Monads? Opaque tag? Explicit labels?}
        @L{Labeling convention: moving computation to wetware})
     ($slide "Issues with Side-Effects"
        @L{Many targets for a same spec? Cloning vs Sharing? @bri
            Dynamically modify a super? a list of supers?}
        @L{Class OO: all prototypes are pure… at compile-time}
        @L{Linearity: more Optimizations, harder Enforcement}
        @L{Caching: more Sharing, harder Invalidation})
     ($slide "Pure? Non-local specification increments"
        @L{Multimethods, Mutually recursive classes, Friend classes?}
        @L{Haskell problem: orphan typeclass targets}
        @L{Hack: side-effect global table, hope for no conflict}
        @L{Nix solution: the Open Loop is Global})
     ($slide "Advanced OOP, purely?"
        @L{Optics: generalize @code{method} to use @code{lens}}
        @L{Advice, CLOS, AOP, before/after/around methods}
        @L{Method combination: @code{list}, @code{and}, @code{+}, or user-specified}
        @L{Any advanced topic in OOP?}))
    ($section "Conclusion: OO is FP"
     $plan-slide
     ($slide @list{Scheme Workshop 2021 Paper}
        @L{Why OO matters — Incremental Modularity}
        @L{What OO is — Constructive Semantics in FP} ;; for systems of increasing sophistication
        @L{Demystify fundamental concepts of OO}
        @L{Prototypes before Classes, Purity before Mutation})
     ($slide "Key Concepts"
        @L{Incrementality & Modularity… intralinguistically}
        @L{Mixin Functions, composed not just applied}
        @L{Multiple inheritance: @(~ 3) modular dependencies}
        @L{Conflation: @(~ 4) @code{Proto target = Spec target × target}}
        @L{@code{Class = Proto Type}})
     ($slide "Why did I care about Prototype OO?"
        @L{Great Usenet Flamewars of OO vs FP}
        @L{Talking Past Each Other… for Decades}
        @L{Lisp had it all the 1970s}
        @L{The paper I wished I could have read younger})
     ($slide "Why So Much Blindness?"
        @L{Industry doesn’t care enough about Correctness}
        @L{Academia doesn’t understand Programming In The Large}
        @L{Both ignore the Human Factor}
        @; Industry wants interchangeable cogs, Academia fancies no brain limits
        @L{All get stuck in their paradigms})
     ($xslide "Inhabiting Constraints — But Which?"
        @C{Every task involves constraint,}
        @C{Solve the thing without complaint@";"}
        @C{There are magic links and chains}
        @C{Forged to loose our rigid brains.}
        @C{Structures, strictures, though they bind,}
        @C{Strangely liberate the mind.}
        @R{— James Falen, as quoted by dughof})
     ($slide "Meta-Thesis"
        @L{Humility, not fanaticism}
        @L{Incommensurable paradigms? Go wider!}
        @L{Simplicity matters}
        @L{λ’s for Semantics, macros for Syntax})
     ($slide "Thank You!"
        @L{Theory: @(~ 8) @Url{https://github.com/metareflection/poof}}
        @L{Practice: @(~ 5) @Url{https://github.com/fare/gerbil-poo}}
        @L{OO in 30 loc — 80 loc with multiple inheritance}
        @C{Hire me! @(~ 5) @code{<fare@"@"mukn.com>}}))))

(reveal-doc doc)
