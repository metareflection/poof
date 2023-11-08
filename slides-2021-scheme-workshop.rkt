#lang at-exp slideshow

;; TODO:
;; - have a plan and automatically inserts slides about where you are in the plan between every slide?
;; - maybe use reveal.rkt as a backend instead of slideshow?

;; TODO: rename (object, prototype, instance) => (prototype, mixin, value) ?

;; Be clear early on that I claim prototype OO subsumes class OO.
;; Mixin inheritance is a special case of composition.
;; Mutation: talk about it, or don't
;; rename x1-y2 to (my-point msg)
;; define the terms: records, encapsulation, subclassing, subtyping
;; Say fewer things in a clearer way

(require slideshow/code
         slideshow/text
         syntax/parse/define
         (only-in pict/color white)
         (only-in unstable/gui/slideshow tabular)
         (only-in slideshow-text-style with-text-style))

(define (C . x) (apply para #:align 'center x))
(define (P . x) (apply para #:align 'left x))
(define (W . x) (parameterize ((current-para-width 1024)) (apply P x)))
(define-simple-macro (Code . x) (W (code . x)))

(slide
 #:title "Prototypes: Object-Orientation, Functionally"
 (t "François-René Rideau, Alex Knauth, and Nada Amin")
 (blank-line)
 (Code (define (fix p b)
         (define f (p (lambda i (apply f i)) b))
         f))
 (Code (define (mix c p)
         (lambda (f s)
           (c f (p f s)))))
 (blank-line)
 (blank-line)
 @tt{https://github.com/metareflection/poof}
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
 (Code (define (instantiate proto base)
         (define self (proto (lambda i (apply self i)) base))
         self))
 (Code
       (define (inherit child parent)
         (lambda (self super)
           (child self (parent self super)))))
 (blank-line)
 (blank-line)
 @tt{https://github.com/metareflection/poof}
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
    understood for decades. But few
    seem to understand wrapper
    composition, its relationship to
    single or multiple inheritance,
    and why it matters.

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
    In 30 lines of code, you get OOP;
    in 80, multiple inheritance.
  })

#;(slide
 #:title "What is NOT in the Paper"
  'next
  @P{Guiding insights}
  (blank-line)
  @P{Hot Takes} ;; Controversial opinions

  @comment{
   But frankly, it would be pointless for
   this presentation to try to go over the
   argument in details or to even attempt
   a summary of it. If I could have made
   a shorter convincing argument, I would
   just have written a shorter paper.

   And so instead, I will treat you to
   some of the key insights that guided
   me in writing the paper — and my
   controversial opinions that were not
   fit to print in an academic paper.
  })

(slide
#:title "What is Object-Orientation about?"
 @tabular[(list "Incrementality" "" "Open Recursion")
          (list "" "                             " "")
          (list "Modularity" "" "Ad hoc Polymorphism")]
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
   has many precise & unprecise
   meanings none of them precisely right
   but many
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

   Everyone in FP is familiar with
   parametric polymorphism. Yet, the
   same famous 1967 Strachey paper
   that introduced the term also
   introduced the dual notion of ad hoc
   polymorphism, wrongfully neglected by
   FPers. OO is about this dual form of
   polymorphism.
   Programming against an interface
   that can be implemented in many ways.
   Modularity!
  })

(slide
 #:title "What is Object-Orientation NOT about?"
 @P{Classes}
 (blank-line)
 @P{“Encapsulation”} ;; What does it even mean?
 (blank-line)
 @P{Inheritance being opposed to Composition}
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
   composition being *different*
   concepts, they are by trivial
   consequence no substitute for one
   another. Each applies to situations
   where the other doesn't.

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
 #:title "Fundamental Concepts"
 @P{Incrementality: Instances and Prototypes}
 @P{Inheritance: Wrappers and Generators}
 @P{Generality: Prototypes beyond records}
 @P{Multiple inheritance: modular dependencies}
 @P{Conflation: Object = Prototype × Instance}
 @P{Type Prototypes: Classes and Elements})

(slide
 #:title "Simplest Incrementality"
 'next
 @P{Instance: value to specify incrementally}
 @P{Prototype: increment of specification}
 @(blank-line)
 'next
 @P[@tt{instantiate: prototype → instance}]
 @P[@tt{inherit: prototype prototype → prototype}]
 @comment{
   Simplest OO design: take the latter
   equations as literal types, and find
   the simplest matching functions.
 })
 ;; TODO: mention "expression problem"?
 ;; Or is that breakdown only for Class OOP?


(slide
 #:title "Simplest Instances: Records as Functions"
 @P{Record: Symbol → Value}
 @(blank-line)
 @Code[(define (my-point msg)
         (case msg ((x) 1)
                   ((y) 2)
                   (else (error "invalid field"))))

       > (my-point 'y)
       2]
 @comment{
 })

(slide
 #:title "Simplest Prototypes: Wrappers"
 (Code
   (code:comment "(deftype (Proto Self Super)")
   (code:comment "  (Fun Self Super → Self st: (⊂ Self Super))))"))
 (Code
   (code:comment ": (Proto Self Super) Super -> Self")
   (define (instantiate proto base)
     (define self (proto (λ i (apply self i)) base))
     self))
 (Code
   (code:comment ": (Proto Self Super) (Proto Super S2) -> (Proto Self S2)")
   (define (inherit child parent)
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
 #:title "Simple Prototypes at work"
 @Code[
   (define (my-point msg) (case msg ((x) 1) ((y) 2) (else (⊥))))
 ]
 'next
 @Code[
   (define ($x3 self super)
     (λ (msg) (if (eq? msg 'x) 3 (super msg))))
   (define ($double-x self super)
     (λ (msg) (if (eq? msg 'x) (* 2 (super 'x)) (super msg))))
   (define ($z<-xy self super)
     (λ (msg) (case msg
              ((z) (+ (self 'x) (* 0+1i (self 'y))))
              (else (super msg)))))
 ]
 'next
 @Code[
   (define $your-point (inherit $z<-xy (inherit $double-x $x3)))
   (define your-point (instantiate $your-point my-point))
   > (your-point 'z)
   6+2i]
 @comment{
   TODO: show how it works
 })

;; TODO: skip in the 25' version
(slide
 #:title "Compare: Single Inheritance"
 (Code
  (code:comment "(deftype (Gen A) (Fun A -> A))")
  (code:comment "instantiate-generator : (Fun (Gen A) -> A)")
  (define (instantiate-generator g)
    (define f (g (λ i (apply f i)))) f)
  \ 
  (code:comment "proto->generator : (Fun (Proto A B) B -> (Gen A))")
  (define (proto->generator p b) (λ (f) (p f b)))
  (code:comment "(== (instantiate-generator (proto->generator p b))")
  (code:comment "    (instantiate p b))")
  \ 
  (code:comment "apply-proto : (Fun (Proto A B) (Gen B) -> (Gen A))")
  (define (apply-proto p g) (λ (f) (p f (g f))))
  (code:comment "(== (apply-proto p (proto->generator q b))")
  (code:comment "    (proto->generator (inherit p q) b))"))

 @comment{
   Composition of wrappers is a much more
   interesting algebraic structure than
   their application to a generator.
   Gilad Bracha has relently pushed for
   "mixins" (Newspeak, Racket, Scala).

   Why would anyone ever implement or
   use single inheritance over the much
   simpler and much more powerful mixin
   composition?
   … because the Handicapper General
   prohibited closures, higher-order
   functions and automatic memory
   management, and instead imposed an
   inexpressive typesystem.
   (and in the olden days, it did matter
   for memory and/or speed)

   And you can save a bit in slot and method
   access performance
   Once you play by those rules, you
   might not have any IQ points left
   to go beyond single inheritance.
 })

(slide
 #:title "Beyond Simple Records"
 @P{Prototypes for any type of instance...}
 (blank-line)
 @P{Prototypes build computations, not values (CBPV)}
 @P{Functions, thunks, delayed or lazy values}
 (blank-line)
 @P{Useful even without record subtyping}
 ;; TODO: include delayed fix and mix?
 @comment{
 })

;; TODO: skip in the 25' version
(slide
 #:title "Multiple Inheritance"
 @P{Make @tt{Wrapper} dependencies modular}
 (blank-line)
 @P{Users specify dependency DAG in local increments}
 @P{System computes and linearizes global DAG}
 (blank-line)
 @P[@tt{Prototype = Wrapper × List(Prototype) × …}]
 @comment{
 })

;; TODO: skip in the 25' version
(slide
 #:title "Conflation of Instance and Prototype"
 @P{We can do all OOP without “objects”,}
 @P{maintaining instance/prototype distinction, but…}
 (blank-line)
 @tt{Object = Prototype × Instance}
 (blank-line)
 @P{Conflation works better with purity}
 @P{Conflation without Distinction ⇒ Confusion}
 @comment{
   GCL 2004, Jsonnet 2014, Nix 2015.
 })

(slide
 #:title "Classes"
 @P{Class OO = Prototype OO at meta-level}
 @P{Instance = Type descriptor (fields, operations…)}
 @P{Class = Prototype for Type descriptor} ;; mention Typeclass
 (blank-line)
 @P{Abstract vs Concrete Class = Prototype vs Instance}
 @P{Subclass ≠ Subtype}
 (blank-line)
 @P{Classes: pure at meta-level (but multimethods…)}
 @P{‘object’, ‘instance’ meanings differ in Class vs Proto}
 @comment{
   Class OOP can be derived from
   prototype OOP:
   A class is a prototype for
   a type descriptor.
   A class instance at runtime is akin to
   a vtable in C++ or
   a method-dictionary in Haskell.
   Prototype OOP is therefore more
   primitive than Class OOP.

   TODO: discuss Typeclass as
   more general than Class.

   However note that class OOP calls
   "object" is NOT (necessarily) an object
   in prototype OOP.
   The class itself is a prototype object,
   at the meta-level.
   The "elements" of the class or just
   the elements of the type described
   by the type descriptor *instance*.
   The class as a prototype is not a
   type-descriptor, but some kind of
   wrapper function that returns a
   type-descriptor plus other meta-data.

   Inheritance of class membership is the
   idea that subtyping *should*, always,
   commute with fixed-points.
   A trivial logical falsehood.
   A terrible design constraint.
   A moronic industrial slogan.
   An academic pissing contest.
   All due to confusion between
   instance and prototype.
 })

;; TODO: skip in the 25' version
(slide
 #:title "Mutation"
 @P{Easy to extend pure model with mutation}
 (blank-line)
 @P{More efficient in linear case, less with sharing}
 @P{Simplified @tt{self/super} protocol}
 (blank-line)
 @P{Challenge: cache invalidation}
 @item{mutable slots vs derived slots}
 @item{mutable supers vs precedence list}
 @comment{
   Single self/super variable;
   its identity is the self;
   its storage is the super.

   There are two hard things in
   computer science:
   cache invalidation, naming things,
   and off-by-one errors.
 })

(slide
 #:title "Constructive Semantics"
 @P{Denotational Semantics × Practical Implementation}
 (blank-line)
 @P{30 loc prototype OOP in any λ language}
 @P{50 loc more for multiple inheritance}
 (blank-line)
 @P{No side-effect needed, but better with laziness}
 @P{Records need subtyping or dynamic types}
 @P{Classes also need staging or dependent types}
 @comment{
   Is laziness pure? Is anything?
   One man's purity is another man's
   effects, and vice versa.
 })

(slide
 #:title "Related Work"
 @P{(Stateful) Prototypes:
     1970s: Director, ThingLab;
     1980s: T, SELF;
     1990s: JavaScript}
 @P{Semantics:
     1980s Semantics (Reddy; Cook...);
     1990s Types (Cardelli; Pierce...)}
 @P{Composable Mixins:
     1990s StrongTalk… (Bracha);
     2000s Racket, Scala, Haskell…}
 @P{Pure Functional Prototypes: 2004 GCL (Google);
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
 (blank-line)
 @P{Simplicity matters}
 @P{λ's for Semantics, macros for Syntax}
 @comment{
   Foundations, not fundamentalism

   FP: accept dynamic types, or add richer
   types than just simple ML types
   OOP: accept closures, higher-order types,
   embrace logical consistency
 })

(with-text-style ([smaller #:size 24])
(slide
 #:title "Questions?"
 @P{Paper @smaller{(23 pages, 33 w/ appendices)}
   @tt{https://github.com/metareflection/poof}}
 @P{Gerbil Scheme implementation @smaller{(3 kloc w/ library)}
   @tt{https://github.com/fare/gerbil-poo}}
 @P{Nix implementation @smaller{(~80 loc w/ multiple inheritance)}
   @tt{https://github.com/NixOS/nixpkgs/pull/116275}}
 (blank-line)
 @C{We're hiring at MuKn!}
 @C{@tt{jobs@"@"mukn.io}}))

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
