#lang at-exp slideshow

;; TODO:
;; - have a plan and automatically inserts slides about where you are in the plan between every slide?
;; - maybe use reveal.rkt as a backend instead of slideshow?

(require slideshow/code
         slideshow/text
         (only-in pict/color white))

(define (P . x) (apply para #:align 'left x))

(slide
 #:title "Prototypes: Object-Orientation, Functionally"
 (t "François-René Rideau, Alex Knauth, and Nada Amin")
 (blank-line)
 (code (define (fix p b)                                          :
         (define f (p (lambda i (apply f i)) b))
         f))

 (code (define (mix c p)                                          :
         (lambda (f s)
           (c f (p f s)))))
 @comment{
   Hi, I am Faré Rideau.
   The paper my colleagues and I
   are presenting today argues that
   the two definitions above
   epitomize the essence of all
   Object Oriented Programming.
 })

(slide
 #:title "Prototypes: Object-Orientation, Functionally"
 (t "François-René Rideau, Alex Knauth, and Nada Amin")
 (blank-line)
 (code (define (instantiate proto base)                           :
         (define self (proto (lambda i (apply self i)) base))
         self))

 (code (define (inherit child parent)                             :
         (lambda (self super)
           (child self (parent self super)))))
 @comment{
   OK, these are the same definitions
   just with longer identifiers
   so it's easier to explain them.
   But I'll get back to that later.
 })

(slide
 #:title "What IS in the Paper"
  'next
  @P{Object Systems defined in the λ-calculus}
  (blank-line)
  @P{Fundamental concepts established}
  @P{Inheritance elucidated}
  @P{Prototypes before Classes}
  @P{Purity before Mutation}
  @P{Constructive Semantics}

  @comment{
    More formally, our paper makes and
    argues the following set of claims.

    In 23 pages of text plus 9 pages of
    appendices, we define, in Scheme, not
    just one Object System but a series of
    Object Systems of increasing
    sophistication, with key illustrating
    examples. The source code of our
    paper is actually runnable, and yields
    a library and its unit tests.

    Our definitions precisely characterize
    many entities that may or may not feel
    familiar to who uses or studies Object
    Orientation: Instances, prototypes,
    wrappers, generators, objects, classes,
    elements. Previous academic
    treatments of OO each implicitly
    confuse or conflate at least two of
    these core concepts, and omit others.

    Single inheritance has been well
    understood for decades. But wrapper
    composition was only described
    recently and never in its full
    generality. And there was no
    satisfactory formal treatment of
    multiple inheritance, how it relates
    to the previous, and why it matters.

    We formally relate Prototype OO and
    Class OO, explain why Prototype OO
    is more fundamental, yet why the
    "typical" approaches have wrongfully
    neglected it.

    Historically, OO has been associated
    to mutation: it was implemented using
    mutation, and colloquially used in
    imperative style. We show how a pure
    functional style confers much simpler
    semantics to OO. Then we discuss how
    to extend our model with mutation.

    Our definitions yield not only
    denotational semantics for OO, but
    also a practical implementation.
    Our construction is applicable on top
    of any language that contains the
    untyped lambda calculus.
  })

(slide
 #:title "What is NOT in the Paper"
  'next
  @P{Controversial opinions}
  (blank-line)
  @P{Guiding insights}

  @comment{
   But frankly, it would be pointless for
   this presentation to try to go over the
   argument in details or to even attempt
   a summary of it. If I could have made
   a shorter convincing argument, I would
   just have written a shorter paper.

   And so instead, I will treat you to all
   the controversial opinions that were
   not fit to print in an academic paper.
   To the insights that guided me in
   writing it.
  })

(slide
 #:title "What is Object-Orientation NOT about?"
 @P{Classes}
 (blank-line)
 @P{“Encapsulation”}
 (blank-line)
 @P{Inheritance instead of Composition}
 (blank-line)
 @P{Mutation everywhere}
 @comment{
   Let's start with what OO is NOT
   despite decades of successful
   marketing by big software company
   salescritters.

   OO is not about Classes.
   Don't get me wrong:
   Classes are an important concept.
   Yet there are OO languages wholly
   without Classes. Classes therefore
   cannot be the central concept of OO,
   only a secondary one. Just like
   there can be FP without Types.
   We can actually formalize the
   relationship between Classes and
   Types. But for who seeks to
   understand the fundamental principles
   of OO, classes are a huge distraction
   and an abysmally wrong starting point.

   OO is not about “encapsulation”.
   Some OO languages provide no such
   feature whatsoever, and those that do
   have completely different, unrelated
   designs. Scoping, visibility,
   namespaces, access control, functional
   abstraction, are important concepts,
   that exist completely independently
   from OO. In the end, “encapsulation”
   is empty slogan from crooks who sell
   snake oil to the gullible.
 }
 'next
 @comment{
   OO is not about using inheritance as
   an *alternative* to FP's composition.
   This *idiotic* claim was indeed made
   by many proponents and opponents of
   OO alike. But inheritance and
   composition being *different* concepts,
   they are by trivial consequence no
   substitute for one another.
   Each applies to situations where
   the other doesn't.

   OO is often associated to imperative
   programming: mutation is used to
   implement OO, and OO is used to
   implement mutable objects. These
   objects are initialized by mutating
   uninitialized fields, and are later
   used by further mutating these
   initialized fields. But this is all
   historical accidents and wrongful
   neglect by the academic community.
   OO like everything is best defined
   and understood in the context of
   pure FP.
 })

(slide
 #:title "What is Object-Orientation about?"
 @P{Incrementality}
 @P{Modularity}
 (blank-line)
 @P{Ad hoc Polymorphism}
 @P{Open Recursion}
 @comment{
   The core concept of OO is
   incrementality. OO is the ability to
   specify computations in terms of small
   increments, each responsible for one
   small aspect of the computation.
   Increments can be combined together
   with other such increments in some
   operation called ‘inheritance’. A
   complete specification can be
   ‘instantiated’ into an computation.

   Modularity. If I played buzzword bingo,
   I would say ‘Abstraction’, but that word
   has many precise & unprecise meanings
   none of them precisely right but many
   of them precisely wrong to characterize
   OO. Modularity is when specifications,
   whether incremental or not, can be
   split into separate entities, “modules”,
   that can be modified independently,
   such that frequent updates to one only
   necessitate infrequent updates to the
   others. Modularity is not specific to
   OO, and applies to FP and any other
   programming paradigm. But while
   Incrementality is what makes OO OO,
   Modularity will greatly influence
   which designs make good or bad OO.

   The above points describe OO from
   a extensional point of view, what
   it is supposed to do for its users.
   We could also describe OO from an
   intensional point of view, of what
   techniques it looks like inside.
 }
 'next
 @comment{

   Everyone in FP is familiar with
   parametric polymorphism. Yet, the
   same famous 1967 Strachey paper
   that introduced the term also
   introduced the dual notion of ad hoc
   polymorphism, wrongfully neglected by
   FPers. OO is about this dual form of
   polymorphism.

   Open Recursion. This means
   manipulating operators of which you
   intend to compute the fixed-point, but
   *later* — after having manipulated
   these operators, composed, applied,
   inspected, or modified them.
   Then comes the associated question
   of subtyping, and whether or not it
   commutes with the taking of this
   fixed-point. But that's a secondary
   concern really.
  })

(slide
 #:title "Fundamental Concepts"
 @P{Incrementality: Instances and Prototypes}
 @P{Inheritance: Wrappers and Generators}
 @P{Generality: Prototypes beyond records}
 @P{Multiple inheritance: modular dependencies}
 @P{Conflation: Object = instance + prototype}
 @P{Type Prototypes: Classes and Elements})

(slide
 #:title "Simplest Incrementality"
 'next
 @P{Instance: value to specify incrementally}
 @P{Prototype: increment of specification}
 @(blank-line)
 'next
 @P{Instantiate: prototype → instance}
 @P{Inherit: prototype prototype → prototype}
 @comment{
   Simplest OO design: take the latter
   equations as literal types, and find
   the simplest matching functions.
 })

(slide
 #:title "Simplest Instances: Records as Functions"
 @P{Record: Symbol → Value}
 )

(slide
 #:title "Simplest Prototypes: Wrappers"
 (code
   (code:comment "(deftype (Proto Self Super)                              :")
   (code:comment "  (Fun Self Super → Self st: (⊂ Self Super))))"))
 (code
   (code:comment ": (Proto Self Super) Super -> Self")
   (define (instantiate proto base)                           :
     (define self (proto (λ i (apply self i)) base))
     self))
 (code
   (code:comment ": (Proto Self Super) (Proto Super S2) -> (Proto Self S2)")
   (define (inherit child parent)                             :
     (lambda (self super)
       (child self (parent self super)))))
 @comment{
   Eta-expansion allows delayed binding:
    self == (lambda i (apply self i))

   Note how computer scientists are often
   confused about what is up and what is
   down in their metaphors. ‘Super’ means
   above, but the super-est of of all
   values is called the ‘base’ value,
   which means all the way below. WTF?
 })

(slide
  #:title "Wrappers at work"
  )

(slide
 #:title "Compare: Single Inheritance"
 @comment{
   Why would anyone ever implement or use
   single inheritance over the much
   simpler and much more powerful mixin
   composition?
   … because the Handicapper General
   prohibited closures, higher-order
   functions and automatic memory
   management, and instead imposed an
   inexpressive typesystem.
   Once you play by those rules, you
   might not have any IQ points left
   to go beyond single inheritance.
 })

#;
(slide
 #:title "Lazy Record Representation"
 (item "Records with Lazy Fields")
 (item "Prototypes for Lazy Computations")
 (small (para (code (delay f)) "instead of" (code (lambda i (apply f i)))
              "                              "
              "Used in δfix:"
              (code (define (δfix p b) (define f (delay (p f b))) f))))
 (comment "Prototypes are for Computations not Values."
          "Using laziness directly inside the fixpoint instantiation."))

#;
(slide
 (item "Distinctions")
 (item "Composition")
 (item "Multiple inheritance")
 (item "Classes")
 (item "Pure")
 (item "Mutation")
 (comment "Distictions: The conceptual distinction between"
          "instances, prototypes, wrappers and generators,"
          "objects, classes and class instances."
          "Composition: Composition of wrappers rather than their application to a generator"
          "as the algebraic structure of interest."
          "Multiple inheritance: Explanations of both why multiple inheritance is useful"
          "and how to formalize it."
          "Classes: How to derive class OOP from the more primitive prototype OOP."
          "Pure: A pure functional approach that provides not only denotational semantics"
          "atop the pure untyped lambda-calculus, but also a practical constructive implementation."
          "Mutation: A constructive model that does not rely on mutation,"
          "yet that can be extended to play well with it.")
 (comment "Instance is a record."
          "Wrapper is (Self Super -> Self) where (Self <: Super),"
          "Wrapper is one way of representing a prototype."
          "Generator of A is (A -> A),"
          "Single Inheritance is a much less expressive special case."
          "Prototype is a modular increment of computation"
          "Object is often a bundle/conflation of an instance and a prototype."
          "Class is a prototype for type descriptors."
          "Class Instance is vtable / method-dictionary"))

(slide
 #:title "Multiple Inheritance"
 @P{Modular dependencies})

(slide
 #:title "Classes"
 @P{Class OO = Prototype OO at type-level}
 @P{Abstract vs Concrete Class = Prototype vs Instance})

(slide
 #:title "From Pure to Stateful"
 @P{Pure: denotational semantics AND practical}
 @P{Is laziness pure? Is anything?}
 @comment{
   One man's purity is another man's effects, and vice versa.
 })

(slide
 #:title "Mutation Challenges"
 @P{Caching computations}
 @P{Mutate slot definitions? Super list?}
 @P{Slot values, precedence list}
 @comment{
 })

(slide
 #:title "Related Work"
 @P{(Stateful) Prototypes:
     1970s: Director, ThingLab
     1980s: T, SELF
     1990s: JavaScript}
 @P{Semantics:
     1980s Semantics (Reddy; Cook...)
     1990s Types (Cardelli; Pierce...)}
 @P{Haskell:
     2000s Mixins (Oliveira...)}
 @P{Pure Functional Prototypes:
     2014 Jsonnet, 2015 Nix}
 @comment{
 })

(slide
 #:title "Future Work"
  @P{Multiple dispatch (multimethods)}
  @P{Method Combinations}
  @P{Generalized Prototypes (with lenses)}
  @P{Usable static types}
  @P{Better caching control})

(slide
 #:title "Paper Claims (redux)"
 @P{Define Object Systems in the λ-calculus}
 (blank-line)
 @P{Establish fundamental concepts}
 @P{Elucidate Inheritance}
 @P{Prototypes before Classes}
 @P{Purity before Mutation}
 @P{Constructive Semantics})

(slide
 #:title "Meta Claims"
 @P{Humility, not fanaticism}
 @P{Incommensurable paradigms? Go wider!}
 @P{Simplicity matters}
 @P{λ's for Semantics, macros for Syntax}
 @comment{
   Foundations, not fundamentalism
 })

(slide
 #:title "Questions?"
 @P{Paper:
   @tt{https://github.com/metareflection/poof}}
 @P{Gerbil Scheme implementation:
   @tt{https://github.com/fare/gerbil-poo}}
 @P{Nix implementation:
   @tt{https://github.com/NixOS/nixpkgs/pull/116275}})

#;@P{
Back in the 1990s to 2000s, I saw many USENET flamewars about OOP vs FP. Neither side ever tried to listen to the other, address their concern, nor speak their language. I vainly yearned for an explanation of how the two were related, complementary, maybe even dual.

The haughty and the crass could revel in their own narrow minds as they blissfully talked past each other, employing incommensurable paradigms. https://dreamsongs.com/Files/Incommensurability.pdf But there had to be a richer paradigm nicely encompassing them both.

Thus, my 2012 paper “LIL: CLOS reaches higher-order, sheds identity, and has a transformative experience” http://github.com/fare/lil-ilc2012 formally addressed the isomorphisms between “OOP” classes and “FP” typeclasses, and between their stateful and pure (linear) variants.

Now in 2021, my paper “Prototypes: Object-Orientation, Functionally” https://github.com/metareflection/poof constructively explores the semantic foundations of OOP, how and why to build OOP on top of FP (pure lazy and dynamic), and why the “typical” approaches failed.

I have finally found my closure (pun intended) on reconciling OOP and FP. Pondering lessons from Jsonnet and Nix recently led me back to the first Lisp object systems of the 1970s, but with the modern resources to afford expressing computations in pure lazy functional style.

Way before two opposite paradigms solidified enough to rally flamewars, the Lisp co-inventors of what is now known as “prototype objects” already had the dual slogans:
“objects are just a poor man's closures” and
“closures are just a poor man's objects”.

I’ll add my own slogan:
“Classes are just prototypes at the meta-level (for type descriptors).”
And for those who feel repelled by the word ‘meta’
“If your computations don’t involve staging, you’re not even human.”

While I’m at it, let’s add for humility:
“Your program shouldn’t be thrown away just because my type system is too limited to analyze it.”
“Your type system shouldn’t be thrown away just because my program is too complex for it to analyze.”
}

#;
(slide
 #:title "TODO"
 @P{TODO}
 @P{TODO}
 @P{TODO}
 @P{TODO}
 @P{TODO}
 @comment{
   TODO
 })
