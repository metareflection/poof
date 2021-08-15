#lang at-exp slideshow

(require slideshow/code
         slideshow/text
         (only-in pict/color white))

(slide
 #:title "Prototypes: Object-Orientation, Functionally"
 (t "François-René Rideau, Alex Knauth, and Nada Amin")
 (blank-line)
 (blank-line)
 (small
  (code (define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
        (define (mix c p) (lambda (f b) (c f (p f b))))))
 @comment{
   Hi, I am Faré Rideau.
   The paper my colleagues and I
   are presenting argues that the
   two lines above epitomize
   the essence of all
   Object Oriented Programming.
 })

(slide
 #:title "What IS in the Paper"
  @item{Define Object Systems in the λ-calculus}
  (blank-line)
  @item{Establish fundamental concepts}
  @item{Elucidate Inheritance}
  @item{Prototypes before Classes}
  @item{Purity before Mutation}
  @item{Constructive Semantics}

  @comment{
    More formally, our paper makes and
    argues the following set of claims.

    In 23 pages of text plus 9 pages of
    appendices, we define, in Scheme, not
    just one Object System but a series of
    Object Systems of increasing
    sophistication. The source code of our
    paper is actually runnable, and yields
    a library and its unit tests.

    Our definitions precisely characterize
    many entities that may or may not feel
    familiar to who uses or studies Object
    Orientation: Instances, prototypes,
    wrappers, generators, objects, classes,
    class instances. Previous academic
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
  @item{Controversial opinions}
  (blank-line)
  @item{Guiding insights}

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
 @item{Classes}
 @item{“Encapsulation”}
 @item{Inheritance instead of Composition}
 @item{Mutation everywhere}
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
   Snake oil dealers often repeat this
   term with no formal meaning to vaguely
   refer to vastly different and unrelated
   designs. Scoping, namespaces, access
   control, functional abstraction, are
   important concepts, and completely
   orthogonal to OO. You have them just
   as well in FP and other paradigms.

   OO is not about using inheritance as
   an *alternative* to FP's composition.
   Yet this *idiotic* claim was made by
   many proponents and opponents of
   OO alike. Inheritance and composition
   are different concepts, hence
   not a substitute for one another.
   Each applies where the other doesn't.

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
 @item{Incrementality}
 @item{Modularity}
 (blank-line)
 @item{Ad hoc Polymorphism}
 @item{Open Recursion}
 @comment{
   The core concept of OO is
   incrementality. OO is the ability to
   specify computations in terms of small
   increments, each responsible for one
   small aspect of the computation.
   These increments are combined
   together with other such increments in
   some operation called “inheritance”,
   and a complete specification can be
   “instantiated” into an effective
   computation.

   Modularity. If I wanted to play
   buzzword bingo, I would say
   “Abstraction”. But that word has
   many precise and unprecise meanings
   many of them precisely wrong to
   characterize OO. Modularity is
   when specifications, whether
   incremental, can be split into
   separate entities, “modules”,
   that can be modified independently,
   such that frequent updates to one
   only necessitate infrequent updates
   to the others. Modularity is not
   specific to OO, and applies to FP
   and any other programming paradigm.
   But while Incrementality is what
   makes OO OO, Modularity will greatly
   influence which designs make
   good or bad OO.

   The above points describe OO from
   a extensional point of view, what
   it is supposed to do for its users.
   We could also describe OO from an
   intensional point of view, of what
   techniques it looks like inside.

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

#;
(slide
 #:title "Prototypes: Object-Orientation, Functionally"
 (item "Functions")
 (item "Prototype Composition")
 (item "Prototype Instantiation")
 (comment "Functions and Prototypes combine to make a simpler set of concepts,"
          "simpler yet more expressive."))

#;
(slide
 #:title "Function Value Representation"
 (item "Records as Functions")
 (item "Prototypes for Functions")
 (small (para "Function Eta-Expansion:" (code (lambda i (apply f i)))
              "                              "
              "Used in fix:"
              (code (define (fix p b) (define f (p (lambda i (apply f i)) b)) f))))
 (comment "Function Eta-Expansion allows computations to be delayed"
          "inside the fixpoint instantiation."))

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
          "yet that can be extended to play well with it."))

#;
(slide
 #:title "Distinctions"
 (item "Instance")
 (item "Wrapper")
 (item "Generator")
 (item "Prototype")
 (item "Object")
 (item "Class")
 (item "Class Instance")
 (comment "Instance is a record."
          "Wrapper is (Self Super -> Self) where (Self <: Super),"
          "Wrapper is one way of representing a prototype."
          "Generator of A is (A -> A),"
          "Single Inheritance is a much less expressive special case."
          "Prototype is a modular increment of computation"
          "Object is often a bundle/conflation of an instance and a prototype."
          "Class is a prototype for type descriptors."
          "Class Instance is vtable / method-dictionary"))

#;
(slide
 #:title "Composition"
 (item "Composition of wrappers rather than their application to a generator"
       "as the algebraic structure of interest."))

#;
(slide
 #:title "Multiple Inheritance"
 (item "Explanations of both why multiple inheritance is useful"
       "and how to formalize it."))

#;
(slide
 #:title "Classes"
 (item "How to derive class OOP from the more primitive prototype OOP."))

#;
(slide
 #:title "Pure"
 (item "A pure functional approach that provides not only denotational semantics"
       "atop the pure untyped lambda-calculus, but also a practical constructive implementation."))

#;
(slide
 #:title "Mutation"
 (item "A constructive model that does not rely on mutation,"
       "yet that can be extended to play well with it."))

(slide
 #:title "Future Work"
  @item{Multiple dispatch (multimethods)}
  @item{Method Combinations}
  @item{Generalized Prototypes (with lenses)}
  @item{Usable static types}
  @item{Better caching control})

(slide
 #:title "Paper Claims (redux)"
 @item{Define Object Systems in the λ-calculus}
 (blank-line)
 @item{Establish fundamental concepts}
 @item{Elucidate Inheritance}
 @item{Prototypes before Classes}
 @item{Purity before Mutation}
 @item{Constructive Semantics})

(slide
 #:title "Meta Claims"
 @item{Humility, not fanaticism}
 @item{Incommensurable paradigms? Go wider!}
 @item{Simplicity matters}
 @item{λ's for Semantics, macros for Syntax}
 @comment{
   Foundations, not fundamentalism
 })

(slide
 #:title "Questions?"
 @item{Paper: @tt{https://github.com/metareflection/poof}}
 (blank-line)
 @item{In production: @tt{https://github.com/fare/gerbil-poo}})

#;
(slide
 #:title "TITLE"
 @item{BLAH}
 @item{BLEH}
 @item{BLIH}
 @item{BLOH}
 @item{BLUH}
 @comment{
   Lorem ipsum
 })
