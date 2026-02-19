#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 6)

@title[#:tag "ROOfiMC"]{Rebuilding OO from its Minimal Core}
@epigraph{Well begun is half done. @|#:- "Aristotle"|
}
Now that I have reconstructed a minimal OO system from first principles,
I can layer all the usual features from OO languages on top of that core.
In this chapter, I will rebuild the most common features from popular OO languages:
those so omnipresent that most developers think they are necessary for OO,
even though they are just affordances easily added on top of the above core.
More advanced and less popular features will follow in subsequent sections.

@section[#:tag "RPOO"]{Rebuilding Prototype OO}
@epigraph{
  If you have built castles in the air, your work need not be lost;
  that is where they should be.
  Now put the foundations under them.
    @|#:- "Henry David Thoreau"|
}

@subsection[#:tag "WdIjd"]{What did I just do?}
In the previous chapter, I reconstructed
a minimal yet recognizable model of OO from first principles,
the principles being modularity, extensibility, and first-class entities.
I will shortly extend this model to support more features
(at the cost of more lines of code).
Yet my “object system” so far has no classes, and indeed no objects at all!

I defined my system in two lines of code.
An equivalent OO system can be similarly defined
in any language that has higher-order functions,
even a pure functional language without mutation.
Indeed Nix defines its “extension” system
with essentially the same two-function kernel @~cite{nix2015}.
But there is indeed one extra thing Nix does that my model so far doesn’t,
wherein Nix has prototype objects and I don’t: conflation.

@subsection[#:tag "CCTHP"]{Conflation: Crouching Typecast, Hidden Product}

@Paragraph{Notional Pair}

Prototype object systems have a notion of “object”, also known as “prototype”,
that can be used both for computing methods, as with my model’s record @emph{targets},
and for composing with other objects through some form of inheritance,
as with my model’s @emph{specifications}.
How can these two very different notions be unified in a single entity?
Very simply: by bundling the two together as a pair.

The type for a prototype, @c{Proto = Spec × Target}, is thus notionally
the product of the type @c{Spec} for a specification
and the type @c{Target} for its target.
Giving more precise types for @c{Spec} and @c{Target} may involve
types for fixpoints, subtyping, existential types, etc.
See @secref{TfOO}.

However, the notions of specification and target and this notional product
all remain unmentioned in the documentation of any OO system that I am aware of.
Instead, the product remains implicit, hidden,
and the prototype is implicitly typecast to either of its factors
depending on context:
when calling a method on a prototype, the target is used;
when composing prototypes using inheritance, their respective specifications are used,
and then the resulting composed specification is wrapped into a pair consisting of
the specification and its target.

@Paragraph{Trivial Implementation}

My implementation below makes this product explicit,
where I use the prefix @c{pproto} to denote a prototype implemented as a pair.
I use the @c{cons} function of Scheme to create a pair, and
the functions @c{car} and @c{cdr} to extract its respective first and second components.
In a more practical implementation, a special kind of tagged pair would be used,
so the runtime would know to implicitly dereference the target in the common case,
without developers having to painfully maintain the knowledge and
explicitly tell the program when to dereference it (most of the time).
If more data is to be conflated with the specification and its record
(such as debugging information, static type annotations, etc.),
a record could be used for all these kinds of data and metadata, instead of a pair@xnote["."]{
  In Clojure, for instance, native data types all come with a @c{meta} accessor for
  an immutable record of arbitrary metadata.
  All data elements except the “main” one could be stored in that record.
}

The function @c{pproto←spec} is used to define a prototype from a specification,
and is used implicitly when composing prototypes using inheritance
with the @c{pproto-mix} function.
The function @c{spec←pproto} extracts the specification from a prototype,
so you may inherit from it.
The function @c{target←pproto} extracts the target from a prototype,
so you may call methods on it:

@Code{
(def (pproto←spec spec)
  (cons spec (delay (fix-record spec))))
(def spec←pproto car)
(def (target←pproto pproto)
  (force (cdr pproto)))
(def pproto-id (pproto←spec idModExt))
(def (pproto-mix parent child)
  (pproto←spec (mix (spec←pproto parent) (spec←pproto child))))
(define (pproto-mix* . l) (foldl (uncurry2 pproto-mix) pproto-id l))
}

@Paragraph{First Issue: Incomplete Specifications}

There is an important catch, however:
the whole point of specifications is that most specifications are not complete,
and cannot be instantiated without error or divergence.
In an abstract mathematical model where you can manipulate failing terms,
that’s not a problem.
But for a concrete implementation, that’s an issue.

To be able to uniformly conflate a specification and its target,
some device must be used to delay the evaluation of the target,
or delay potential errors and divergence until after the target is computed,
when a further attempt is made to use the target.
Otherwise, an invalid target, which is a necessarily common case,
would prevent access to the specification, defeating the entire point of conflation.

The simplest device to delay evaluation of the target is lazy evaluation:
@c{Proto = Spec × Lazy Target}.
In second-class Class OO, the target is a descriptor for type,
that itself can always be computed without error in finite time,
though the type may be empty, and trying to use it may result in static or dynamic errors.
Execution in a latter stage of computation (runtime vs compile-time)
can be seen as the ultimate form of delayed evaluation.

Note how in our representation of records as functions,
we already delay the potentially non-terminating or error-throwing behavior
until the time the function is invoked.
The function definition itself is guaranteed to terminate early on,
since the actual computations within are protected by λ-abstractions.
Thus, @c{delay} and @c{force} are not strictly necessary to prevent an invalid target from
causing an error when computing the product.
However, we use @c{delay} anyway for uniformity with the recursive conflation variant @c{qproto}
presented next, where @c{delay} is required for correctness.

@Paragraph{Second Issue: Recursion}

Now, there is a subtle issue with the above implementation:
when a target recursively refers to “itself” as per its specification,
it sees the target only, and not the conflation of the target and the specification.
This is not a problem with second-class OO, or otherwise with
a programming style involving strictly stratified stages
of evaluation wherein all the specifications are closed before any target is instantiated;
then at one stage you consider only specifications, at the other, you consider only targets.
And runtime recursion only happens during that other, latter stage.

But with a more dynamic style of programming where no such clear staging is guaranteed,
full first-class OO requires support for recursion in conflation:
programs and programmers must deal
with two different kinds of entities, conflated or unconflated,
depending on whether they are specified as second-class entities before recursion happens,
or produced during the recursion as first-class entities.
Tracking which kind you are dealing with at which time can be a big limitation,
that is extremely complex, time-consuming, and bug-prone to lift or work around@xnote["."]{
  @; TODO secref chapter 9 on metaobjects
  Metaobjects are a typical use case where you don’t (in general) have a clean program-wide staging
  of specification and targets: to determine the meta methods,
  you partially instantiate the “meta” part of the objects,
  based on which you can instantiate the rest.
}

As is often the case in software,
if you need to access some information (here, the specification)
that existed at some point but isn’t available anymore,
your best bet is to stop throwing away the useful information during the computation,
but keep it conflated with whichever entity should have remembered the information
(here, the target in a recursive reference).

@subsection[#:tag "RC"]{Recursive Conflation}

In a dynamic first-class OO language, the conflation of specification and target
into a single entity, the prototype, must be recursively seen by the target
when instantiating the specification.
This is achieved by having the instantiation function compose a “magic” wrapper specification
in front of the user-given specification before it takes a fixpoint.
Said magic wrapper will wrap any recursive reference to the target into
an implicit conflation pair of the specification and the target@xnote["."]{
  @citet{Abadi1996Primitive} struggle with variants of this problem,
  and fail to find a solution, because they @emph{want} to keep confusing target and specification
  even though at some level they can clearly see they are different things.
  If they had conceptualized the two as being entities that need to be distinguished semantically,
  then must be explicitly re-grouped together as a pair,
  they could have solved the problem and stayed on top of the λ-calculus.
  Instead, they abandon such attempts, and rebuild their own syntactic theory
  of a variant of the λ-calculus just for object,
  with hundreds of pages of greek symbols that still fail to properly modeling objects,
  in an insanely complex @emph{abstraction inversion} @~cite{Baker1992CritiqueDKL}.
  Their futile theory can neither enlighten OO practitioners,
  nor make OO interesting to mathematical syntax theoreticians.
}
Here is an implementation of that idea, wherein I prefix function names with @c{qproto}:

@Code{
(def (qproto-wrapper spec super _self)
  (cons spec super))
(def (qproto←spec spec)
  (delay (fix-record (mix spec (qproto-wrapper spec)))))
}
Note how the following functions are essentially unchanged compared to @c{pproto}:
@Code{
(def spec←qproto car)
(def (target←qproto qproto)
  (force (cdr qproto)))
(def (qproto-mix parent child)
  (qproto←spec (mix (spec←qproto parent) (spec←qproto child))))
(define (qproto-mix* . l) (foldl (uncurry2 qproto-mix) pproto-id l))
}
What changed from the previous @c{pproto} variant was that the
@c{(λ (x) (cons spec x))} extension was moved from outside the fixpoint to inside:
If @c{R} is the parametric type of the reference wrapper
(e.g. @c{R Integer} is the type of a reference to an integer),
and @c{M} is the parametric type of modular extension, also known as a @emph{recursion scheme},
then the type of @c{pproto} is @c{R (Y M)},
and that of @c{qproto} is @c{Y (R ∘ M)}, so in both cases I have
a reference to a recursive data structure that follows the recursion scheme,
but in the second case further recursive accesses also use the reference.
Note that @c{Y (R ∘ M) = R (Y (M ∘ R))} and @c{Y (M ∘ R)} is the type of
a raw record that follows the recursion scheme and uses references for recursion,
instead of the type of reference to such, i.e. I have in turn @c{Y (M ∘ R) = M (Y (R ∘ M))};
people interested in low-level memory access might want to privilege this latter @c{Y (M ∘ R)}
representation instead of @c{Y (R ∘ M)}, which indeed is a notable difference
between the OO models of C++ vs Java: C++ makes you deal with raw data structures,
Java with references to data structures.

Now, it is not unusual in software for access to records, recursive or not,
to be wrapped inside some kind of reference type:
pointer into memory, index into a table,
key into a database, cryptographic hash into a content-addressed store,
location into a file, string or identifier used in a hash-table or tree, etc.
In the case of recursive data structures implemented as data in contiguous regions of memory,
such level of indirection is inevitable, as there is no way to have a contiguous region of memory
of some size contain as a strict subset a contiguous region of memory of the same size,
as would be required for a data structure to directly
include an recursive element of the same type@xnote["."]{
  Exception: If after simplifying unit types away
  the only element of a structure is an element of the same structure.
  At which point it’s just an infinite loop to the same element,
  the type is isomorphic to the unit type, and can be represented as
  a trivial data structure of width zero.
  But if there is any other data element that isn’t a unit type,
  direct recursion would mean infinite copies of it, one for each recursive path,
  which can’t fit in finite memory.
}

This use of a reference wrapper can be seen as an instance of the so-called
“Fundamental theorem of software engineering” @~cite{WikiFTSE}:
@emph{We can solve any problem by introducing an extra level of indirection}.
But more meaningfully, it can also be seen as the embodiment of the fact that,
computationally, @principle{recursion is not free}.
While at some abstract level of pure logic or mathematics,
the inner object is of the same type as the outer one,
and accessing it is free or constant time,
at a more concrete level, recursion involves fetching data from another memory region.
In the best case of a simple sequence of data,
the sequence can be a contiguous array of memory and this fetching is constant time;
in general, this fetching goes through the memory caching hierarchy,
which logically requires a number of bits that grows logarithmically with the size of the working set,
and physically requires a latency which grows as the square root of that size@~cite{MythOfRAM2014}.

Importantly, isomorphic as it might be at some abstract level,
the reference type is not equal to the type being referenced,
and is not a subtype of it.
Thus, the necessary reference wrapper extension is crucially not a strict extension
with respect to the type being wrapped.
This proves that it is a bad idea to require all extensions to always be strict
(which would beg the question of which type to be strict for,
or lead to the trivial answer that of being strict for the top type,
for which all extensions are trivially strict).
The reference wrapper, pure isomorphism at one level,
yet effectful non-isomorphism at another
(requiring access to disk, database, network, credentials, user interface, etc.),
also illustrates that one man’s purity is another man’s side effect (to channel Alan Perlis).
For instance, with merkleization, a reference uniquely identifies some pure data structure
with a cryptographically secure hash that you can compute in a pure functional way;
but dereferencing the hash is only possible if you already know the data
based on which to compute and verify the hash, that you indexed into a database
that you need some side-effect to consult.
Many OO languages have an implicit builtin reference wrapper,
as opposed to, e.g., explicit pointers as in C++;
but the reference semantics doesn’t disappear for having been made implicit.

@subsection[#:tag "CfR"]{Conflation for Records}

If the target type can be anything, including an atomic value such as small integers,
then there’s nowhere in it to store the specification,
and the conflation of specification and target must necessarily involve
such a wrapping as I showed earlier.
But if the target type is guaranteed to be a (subtype of) Record,
it is possible to do better.

In the Nix extension system, a target is a record (called an attrset in Nix),
mapping string keys to arbitrary values,
and the modular extension instantiation function @c{fix'} stores
the specification under a “magic” string @c{"__unfix__"}.
The advantage is that casting a prototype (called “extension” in Nix)
to its target is a trivial zero-cost identity no-op;
the slight disadvantage is that the target must be a record,
but that record cannot use arbitrary keys,
and must avoid the magic string as key.
As a minor consequence, casting to a specification becomes slightly more expensive
(table lookup vs fixed-offset field access),
whereas casting to a target (the more common operation by far) becomes slightly cheaper
(free vs fixed-offset field access).
The semantics are otherwise essentially the same as for my implementation using pairs.

Here is a Scheme implementation of the same idea,
where the prefix @c{rproto} denotes a prototype implemented as a record,
and my magic key is @c{#f}, the boolean false value,
instead of some reserved symbol,
so it doesn’t impede the free use of arbitrary symbols as keys.
The function @c{rproto←spec} is used to define a prototype from a specification,
by prepending a special specification @c{rproto-wrapper} in front
that deals with remembering the provided specification;
this function is used implicitly when composing prototypes using inheritance
with the @c{rproto-mix} function.
The function @c{spec←rproto} extracts the specification from a prototype,
so you may inherit from it; this specifically doesn’t include the @c{rproto-wrapper},
which would notably interfere with mixing.
The function @c{target←rproto} extracts the target from a prototype,
so you may call methods on it—it is the identity function, and
you can often inline it away.
@Code{
(def (rproto-wrapper spec super self method-id)
  (if method-id (super method-id) spec))
(def (rproto←spec spec)
  (fix-record (mix spec (rproto-wrapper spec))))
(def rproto-id (rproto←spec idModExt))
(def (spec←rproto rproto)
  (rproto #f))
(def target←rproto identity)
(def (rproto-mix parent child)
  (rproto←spec (mix (spec←rproto parent) (spec←rproto child))))
(define (rproto-mix* . l) (foldl (uncurry2 rproto-mix) rproto-id l))
}
Once again, some special extension is used in front, that is not strict,
and is almost-but-not-quite an isomorphism, and specially memorizes the specification.

You may also define a prototype from a record by giving it a “spec”
that just returns the record as a constant:
@Code{
(def (rproto←record r)
  (rproto←spec (constant-spec r)))
}

@subsection[#:tag "CwUAoS"]{Conflation with User Application of Self}

One simple encoding of objects conflates specification and target
in a way that is subtly different from the previous one.
It is notable for being used by Yale T Scheme @~cite{Rees1982T Adams1988oopscheme},
and after it by YASOS @~cite{Dickey1992}, and also for being the essence of
the JavaScript object system @~cite{Eich1996JavaScript}:
a prototype is a record of methods encoded as functions that take
the record itself as parameter.

The subtle difference is that instead of taking a resolved module context as first argument,
functions take as first argument an unresolved record of functions
that themselves take the same unresolved record as first argument.
Callers, whether from outside the object or recursively from inside,
must constantly be aware of the calling convention and pass the record as argument
to all methods after extracting the methods from the record.
At no point in this encoding is there a handle on a fully resolved target,
only to the half-resolved specification@xnote["."]{
  The encoding can be considered as being based on the duplication combinator D
  rather than on the fixpoint combinator Y.
  D is sometimes known as the self-application operator U (see @secref{DSF})
  in the context of computing fixpoints, and can also be viewed as half of Y.
  That is why I will speak of Y-encoding and U-encoding of recursion schemes,
  and also why, when comparing the two, I like to write U-encoded specifications
  as using @c{half} rather than @c{self} as the name of the first variable:
  Essentially, you have @c{self = (half half)} and @c{super = (hyper half)}.
  Most implementations traditionally switch the order of arguments between
  that @c{half} and @c{method-id} but that is semantically an isomorphism.
}

In T and after it YASOS, an @c{operation} can abstract over the calling convention
providing a regular functional interface to the functionality,
as well as a locus to define an operation-specific default behavior,
and potentially more (see also generic functions in CLOS). @; TODO secref ch8

Meanwhile, inheritance is supported by an @c{operate-as} function
that extracts a method from an ancestor prototype,
then calls it to the “real” self as first argument:
single or mixin inheritance are simply about chaining calls to
the method for the next ancestor.
T or YASOS themselves do not offer builtin infrastructure to locate the next ancestor,
but it is trivial to roll your own for single inheritance
by having each object delegate to the known next ancestor (@secref{SI}).
Mixin inheritance can be achieved by abstracting over the super object
(i.e. function that takes it as argument and returns the half-resolved record),
though it requires a bit of infrastructure to dynamically skip
over objects that do not handle a message (@secref{MxI}).
A uniform field in which to store the super object (if any) could help in both cases above.
Multiple inheritance can be done as a layer above mixin inheritance,
or else by a change in protocol wherein methods take
a “method resolution continuation” argument in addition to the “self” argument (@secref{MI}).

In the end, this specialized object encoding is efficient,
which is why it has been adopted, in many variants,
by many implementations of many languages.
But it comes with significant complexity, both technical and conceptual.
It also sets objects apart from other kinds of records or values,
with their own distinct query protocol, when my fixpoint encoding
allows any value of any type to be the target of a specification,
after which it follows the same protocol as any other value of that type.
Also, this encoding giving you conflation for free is both a blessing and a curse.
A blessing because the object implicitly embodies both target and specification
without extra effort.
A curse because this costlessness historically contributed
to blurring the lines between specification and target,
and therefore to the ongoing confusion between them,
making their conceptual conflation harder to untangle.

Often efficiency is important at runtime, yet simplicity is even more important
at the conceptual layer, for programmers to understand the software they work with.
That is why I describe this most common implementation strategy last instead of first,
and leave its details as an exercise to the reader.

@subsection{Small-Scale Advantages of Conflation: Performance, Sharing}

First, note how, if a specification is pure functional,
i.e. without side-effects, whether state, non-determinism, I/O, or otherwise,
then indeed there is only one target, uniquely specified up to behavioral equality:
recomputing the target multiple times will lead to the same result in all contexts.
It thus makes sense to consider “the” target for a specification,
and to see it as but another aspect of it.
Caching the target value next to the specification
can then be seen simply as a performance enhancement.
Indeed, in case of recursive access to the target, this performance enhancement
can grow exponentially with the depth of the recursion,
by using a shared computation instead of repeated recomputations
(see the related discussion on the applicative Y combinator in
@secref{DSF}).

If on the other hand, the specification has side-effects
(which of course supposes the language has side-effects to begin with),
then multiple computations of the target value will lead to different results,
and caching a canonical target value next to the specification is
not just a performance enhancement, but a critical semantic feature enabling
the sharing of the state and side-effects of a prototype between all its users.
Meanwhile, if some users explicitly want to recompute the target,
so as to get a fresh state to be modified by its own set of side-effects,
they can always clone the prototype,
i.e. create a new prototype that uses the same specification.
Equivalently, they can create a prototype that inherits from it
using as extension the neutral element @c{(rproto←spec idModExt)}.

Now, plenty of earlier or contemporary Prototype OO languages,
from Director and ThingLab to Self and JavaScript and beyond,
support mutation yet also offer objects as conflation of two aspects:
one for inheritable and instantiatable specification,
another one for the instantiated target that holds mutable state
and that methods are called against.
However, the two aspects might not be as neatly separated
in these stateful languages as in pure functional languages,
because the mutation of the internals of the specification, in languages that allow it,
may interact with the target state and behavior in weird ways.
This mutation is not usually idiomatic in production code,
but may be heavily relied upon during interactive development,
or as part of implementing advanced infrastructure. @;{ TODO secref mutation }

Last but not least, if your choice of representation for specifications and targets
is such that instantiating a specification may itself issue side-effects such
as errors or non-termination or irreversible I/O—then it becomes essential
to wrap your target behind lazy evaluation, or, in Scheme, a @c{delay} form.
Thus you may still define prototypes from incomplete erroneous specifications,
and use them through inheritance to build larger prototypes, that, when complete,
will not have undesired side-effects.
Once again, @principle{Laziness proves essential to OO,
even and especially in presence of side-effects}.

@subsection{Large-Scale Advantage of Conflation: More Modularity}

Remarkably, conflation is more modular than the lack thereof:
thanks to it, programmers do not have to decide in advance
when each part of their configuration is to be extended or instantiated.
Indeed, if only using unconflated specifications and targets,
one would have to choose, for each definition of each identifier in each (sub)module,
whether to use a specification or its target.
And when choosing to maintain a specification for the purpose of extensibility,
the programmer may still want to explicitly bind an identifier to the target,
and another one to the specification,
for the sake of state sharing, and sanity, and not having to remember the hard way
the “shape” of the nested extensions to replace by the values when instantiating the target.
Thus, users would end up doing by hand in an ad hoc way what conflation
gives them systematically for free.

Furthermore, users cannot know which of their definitions they themselves, or other users,
might later want to extend.
A simple service will often be made part of a larger service set,
in multiple copies each slightly tweaked;
its configuration, complete and directly instantiable as it may be for its original author,
will actually be extended, copied, overridden, many times, in a larger configuration,
by the maintainers of the larger service set.

The modularity of conflation is already exploited at scale
for large software distributions on one machine or many,
using GCL, Jsonnet or Nix as (pure functional) Prototype OO languages:
@itemize[
@item{
  The Google Control Language GCL@~cite{gclviewer2008} (née BCL, Borg Control Language),
  has been used to specify all of Google’s distributed software deployments
  since about 2003 (but uses dynamic rather than static scoping,
  causing dread among Google developers).}
@item{
  Jsonnet@~cite{jsonnet}, inspired by GCL but cleaned up to use static scoping,
  has been particularly used to generate configurations for AWS or Kubernetes.}
@item{
  Nix@~cite{dolstra2008nixos} is used not just to configure
  entire software distributions for Linux or macOS,
  but also distributed services with NixOps or DisNix.}]
All three languages have proven the practicality of pure lazy functional prototype objects,
with mixin inheritance and conflation of specification and target,
as a paradigm to specify configuration and deployment of software on a world-wide scale,
each with hundreds of active developers, tens of thousands of active users,
and millions of end-users.
Despite its minimal semantics and relatively obscure existence,
this mixin prototype object model has vastly outsized outreach.
It is hard to measure how much of this success is due to the feature of Conflation,
yet this feature is arguably essential to the ergonomics of these languages.

@subsection{Implicit Recognition of Conflation by OO Practitioners}

The notion of a @emph{conflation of specification and target},
that I presented, is largely unknown by OO developers, and
seems never to have been made explicit in the literature until
I published @citet{poof2021}, that itself remained confidential.
And yet, the knowledge of this conflation is necessarily present, if implicit,
if not across the community of OO practioners,
at the very least among individual OO implementers—or else
OO wouldn’t be possible at all.
Sixty years of crucial information you don’t know you know!

Common practitioners of OO have long implicitly recognized
the conflated concepts of specification and target.
Back in 1979, Flavors @~cite{Cannon1979} introduces the concept of a @emph{mixin} as
a flavor meant to be inherited from, but not to be instantiated,
by opposition to an instantiatable flavor;
however, the nomenclature only stuck in the Lisp community
(and even there, flavors yielded to classes though the term mixin stayed).
In other communities, the decade-later terms of art are
@emph{abstract classes} and @emph{concrete classes}@~cite{Goldberg1983 Johnson1988}:
an abstract class is one that is only used for its specification—to inherit from it;
a concrete class is one that is only used for its target type—to use its methods
to create and process class instances.
Experienced practitioners recommend keeping the two kinds of classes separate, and
frown at inheriting from a concrete class,
or trying to instantiate an abstract class.

Theorists have also long implicitly recognized the conflated concepts
when developing sound typesystems for OO:
for instance, Fisher @~cite{Fisher1996} distinguishes
@c{pro} types for objects-as-prototypes and @c{obj} types for objects-as-records;
and Bruce @~cite{bruce1996typing} complains that
“the notions of type and class are often confounded in object-oriented programming languages” and
there again distinguishes subtyping for a class’s target type (which he calls “subtyping”)
and for its open specification (which he calls “matching”).
Yet though they offer correct models for typing OO,
both authors fail to distinguish specification and target
as syntactically and semantically separate entities in their languages,
leading to much extraneous complexity in their respective typesystems.

Implementers of stateful object systems at runtime may not have realized the conflation of entities,
because they are too focused on low-level mechanisms for “delegation” or “inheritance”.
By contrast, writers of compilers for languages with second-class Class OO
may not have realized the conflation because at their
level it’s all pure functional specification with no target until runtime.
One group of people though, must explicitly deal with the conflation of specification and target
embodied as a first-class value: implementers of pure functional prototype systems.
Nix @~cite{nix2015} explicitly remembers the specification by inserting
the @c{__unfix__} attribute into its target records;
and Jsonnet @~cite{jsonnet} must do something similar under the hood,
though it is hidden under much complexity in the implementation.
Yet the authors of neither system make note of it in their documentation
as a phenomenon worthy of remark. Though they implicitly rediscovered the concept
and made it flesh, they failed to realize how momentous the discovery was,
and shrugged it off as yet another one of those annoying implementation details
they had to face along the way.

Finally, the confusion between target and specification can be seen as a special case of
the confusion between object and implementation discussed in @citet{Chiba2000MetaHelix},
wherein you can see the specification as @emph{implementing} the target.
But though these authors saw a more general case in a wider theory with far reaching potential,
they do not seem to have noticed this common case application.
@; TODO handle that in ch9 and add secref ?

Thus, through all the confusion of class OO languages so far,
both practitioners and theorists have felt the need
to effectively distinguish specification and target,
yet no one seems to have been able to fully tease apart the concepts up until recently.

@exercise[#:difficulty "Easy"]{
  Reimplement the code from the previous chapter
  to use the @c{rproto} encoding above.
  Include both code I wrote, and code you wrote in exercises.
}
@exercise[#:difficulty "Easy"]{
  Locate and read the YASOS source code (it’s short).
  Play with it.
  Write a function that takes an object in YASOS encoding,
  and wraps it into an target record as per my minimal OO model.
}
@exercise[#:difficulty "Medium"]{
  Use Jsonnet, Nix, or some other Prototype OO language
  to read and write configurations of some kind.
  Then use prototype OO to import an existing useful configuration
  and extend it into a new one without modifying the original.
  For instance, use Nix extensions or customizations or functions of super and self
  of some kind to change the version of a library or compiler used by other packages.
  Can you identify a case where you enjoy such extension not just globally,
  but only for a small subset of packages?
}
@exercise[#:difficulty "Medium"]{
  Implement a variant of @c{qproto} where instead of a pair,
  your recursive proxy object is itself a record with fields @c{specification} and @c{target}.
  Then add more metadata fields such as for object @c{type}, provenance annotation, etc.
  Notice you can also add metadata fields (or one master metadata field)
  in the @c{rproto} representation.
}
@exercise[#:difficulty "Hard"]{
  Modify YASOS to correctly support mixin inheritance.
  What do the equivalent of @c{mix}, @c{fix} and @c{field-spec} look like?
  Instead of @c{record-ref}, what function @c{yasos-ref} do you call
  to consult an object method?
  Play with the representation, manually translate functions to use this encoding.
  Write functions that can wrap specifications both ways
  between your modified YASOS encoding and the @c{rproto} encoding.
  What tradeoffs can you see between the two encodings?
}

@exercise[#:difficulty "Hard"]{
  Assuming you did exercise @exercise-ref{5alist},
  write a variant of @c{rproto} that uses your representation of records
  as data structures rather than as functions from symbol to value.
}

@section[#:tag "RCOO"]{Rebuilding Class OO}
@epigraph{
  JavaScript classes, introduced in ECMAScript 2015,
  are primarily syntactical sugar over JavaScript’s existing prototype-based inheritance.
  The class syntax does not introduce a new object-oriented inheritance model to JavaScript.
    @|#:- "Mozilla Developer Network"|}

@subsection{A Class is a Prototype for a Type}

Having elucidated Prototype OO in the previous sections,
including its notion of Object, a.k.a. Prototype, as conflation of Specification and Target,
I can now fully elucidate Class OO including its notion of Class:
@principle{A Class is a Prototype for a Type}.
Or, to be pedantic, a class is a prototype, the target of which is a @emph{type descriptor},
i.e. a record describing a type together with methods associated with the type.

The target type of a class is usually, but not always,
a record type (indexed product, structure, struct, named tuple),
at which point the descriptor will also describe its fields;
it may also be an enumeration type (indexed sum, enum, variant, discriminated union, tagged union),
at which point the descriptor will also describe its alternatives;
and while this is less common in Class OO, a class’s target could really describe
any kind of type: function, number, array, associative array, etc.

The target of a class may be a runtime type descriptor,
that describes how to create, recognize, and process elements of the type
in the same evaluation environment that the type exists in;
or, as is often the case in second-class Class OO,
the target may be a compile-time type descriptor,
that describes how to generate code for entities of that type
to be executed in a further stage of evaluation;
or it may be both.

Whichever kind of type descriptors are used,
Class OO is but a special case of Prototype OO,
wherein a class is a prototype for a type,
i.e., the conflation of a modular extension for a type descriptor,
and the type descriptor that is the fixpoint of that specification.
Thus, when I claimed in @secref{OOiCO} that
the situation of classes in OO was similar to that of types in FP,
I meant it quite literally.

@subsection[#:tag "SFCTD"]{Simple First-Class Type Descriptors}

Since I do not wish to introduce a Theory of Compilation in this book
in addition to a Theory of OO, I will only illustrate how to build
@emph{first-class} Class OO, at runtime, on top of Prototype OO.
I will use a technique described by Lieberman @~cite{Lieberman1986},
and no doubt broadly similar to how many Class OO systems were implemented on top of JavaScript
before web browsers ubiquitously adopted ES6 classes@~cite{EcmaScript2015}. @;{TODO CITE ???}

A type descriptor (which in C++ would correspond to a @c{vtable})
will typically have methods as follows:
@itemize[
@item{A method @c{instance-methods} returning a record of instance methods
      (or, as a runtime optimization that requires more compile-time bookkeeping,
      encode those object methods directly as methods of the type descriptor).}
@item{For record types, a method @c{instance-fields} returning a record of field descriptors,
      each of them a record with fields @c{name} and @c{type} (and possibly more).}
@item{Additionally, self-describing records (the default)
      will have a special field @c{#t} (the Scheme true boolean) to hold their type descriptor
      (I could have used the string @c{"__type__"} if keys had to be strings,
      using a common convention of surrounding a system-reserved identifier with underscores;
      but in Scheme I can use a different kind of entity and leave strings entirely for users;
      my choice of @c{#t} also rhymes with my previous choice of
      @c{#f} (the Scheme false boolean) to hold the specification;
      plus @c{#t} has the same letter as the initial of “type”).}
@item{In dynamic languages, or static languages that accept
      subsets of canonical inferred static types as types,
      a method @c{element?} returning a predicate that is
      true if its argument is an element of the type.
      If the language supports error reporting,
      a method @c{validate} returning
      a unary function that returns its argument unchanged if it is an element
      and otherwise raises a suitable error.
      Either method can be derived from the other using sensible defaults.
      First-class class instances are definitely subsets of whichever underlying data type
      is used for their representation, and so their type descriptors
      will usefully have such a method.}
@item{Usually, some kind of method @c{make-instance} returning
      a constructor to create a new object instance
      with some extensible protocol from user-specified arguments.
      To support indexed sum types, either the @c{make-instance} method
      will take a discriminant as first argument,
      or a method @c{instance-constructors} could hold a record
      of several “constructor” functions for each case.
      Instead of constructors, or in addition to them,
      a basic @c{instance-prototype} (or @c{instance-prototypes}) method
      could be provided that implements the skeleton of a class instance.}
@item{For languages that support user management of dynamic object lifetime,
      a @c{destroy-instance} method could be part of the class, or among the @c{instance-methods};
      or if there can be several kinds of destructors,
      they could be held by a method @c{instance-destructors}.}]

A “class instance” (corresponding to an “object” in Class OO) is a self-describing record,
the type descriptor of which can be extracted with the function @c{type-of} below;
to call an “instance method” on it, you use the function “instance-call” with it as first argument,
and it calls the method from the instance’s type’s @c{instance-methods} record,
with the instance as first argument.
@Code{
(def (type-of instance)
  (instance #t))
(def (instance-call instance method-id)
  (type-of instance 'instance-methods method-id instance))
}
Note that, if I use the Nix approach of zero-cost casting to target when the target is a record,
then I can use the very same representation for type descriptors, whether they were generated
as the fixpoint target of a specification, or directly created as records without such a fixpoint.
This kind of representation is notably useful
for bootstrapping a Meta-Object Protocol@~cite{amop}.

As for “class methods” (also known as “static methods” in C++ or Java),
they can be regular methods of the type descriptor,
or there can be a method @c{class-methods} in the type descriptor containing a record of them.

@;{
TODO Discuss encodings and types?

Encoding as record of pre-methods

Reppy, Rieke "Classes in ObjectML via Modules" 1996 FOOL3

BruceCardelliPierce2006
}

@subsection{Parametric First-Class Type Descriptors}

There are two main strategies to represent parametric types:
“function of descriptors” vs “descriptor of functions”.

In the “function of descriptors” strategy, a parametric type is represented as
a function from type descriptor to type descriptor:
the function takes a type parameter as input, and
returns a type descriptor specialized for that parameters as output.
As it computes the specialized descriptor, it can apply various specializations and optimizations,
pre-selecting code paths and throwing away cases that do not apply,
allowing for slightly better specialized code,
at the expense of time and space generating the specialized descriptor.
This representation allows for uniform calling conventions
for all type descriptors satisfying the resulting monomorphic interface,
regardless of whether it was obtained by specializing a parametric type or not.
This strategy is notably used during the “monomorphization” phase
used within C++ compilers when expanding templates.
It is useful when trying to statically inline away all traces of type descriptors before runtime,
but in a dynamic setting requires creation of more descriptors,
or some memoization mechanism.

In the “descriptor of functions” strategy, a parametric type is represented as
a type descriptor the methods of which may take extra parameters in front,
one for the type descriptor of each type parameter.
Methods that return non-function values may become functions of one or more type parameters.
Thus, the type descriptor for a functor @c{F} may have a method @c{map}
that takes two type parameters @c{A} and @c{B} and transforms an element of @c{P A}
into an element of @c{P B}.
This strategy eliminates the need to heap-allocate a lot of specialized type descriptors;
but it requires more bookkeeping, to remember which method of which type descriptor
takes how many extra type descriptor parameters.

Both strategies are useful; which to prefer depends on the use case and its tradeoffs.
A given program or compiler may use both, and may very well have to:
even using the “descriptor-of-functions” strategy, you may still have to generate
specialized type descriptors, so that you may pass them to functions
that expect their parametric type descriptors to only take @c{N} type parameters,
and are unaware that these were specialized from more general parametric type descriptors
with @c{N + M} type parameters (where @c{M > 0}).
This unawareness may stem from any kind of dynamic typing, or
from a choice to avoid generating many copies of the code
for each value of @c{M} as the depth of parametric constructors varies.
And even using the “function-of-descriptors” strategy,
you may want to maintain collections of functions that each take
one or many descriptors as arguments,
especially when such collections contain a large number of functions,
only one or a few of which are to be used at a time per tuple of descriptor arguments,
and this tuple of descriptor arguments itself changes often.

Even fixpoints can be done differently in the two strategies:
the “function-of-descriptors” strategy leads to generating descriptors that embed the fixpoints
so that clients never need to know they were even fixpoints;
whereas the “descriptor-of-functions” strategy leads to descriptors the calling convention of which
requires clients to systematically pass the descriptor itself to the methods it contains
so as to close the fixpoint loop.

@subsection[#:tag "CSvTS"]{Class-style vs Typeclass-style}

Now, there are two common styles for using type descriptors: Class-style and Typeclass-style.

In Class-style, each type element
(known in this style as “class instance”, “instance”, or “object”)
carries its own type descriptor.
To this end, the type descriptor is conflated with the type element
in the same way that specifications are conflated with their targets
(see @secref{RC}, @secref{CfR}).
As mentioned before, this can be very cheap when the type elements are records
(see @secref{CfR}, @secref{SFCTD}): just add a special field with a “magic” key.

In Typeclass-style, by contrast, type descriptors and type elements are kept distinct and separate.
There is emphatically no conflation.
Type-descriptors are passed “out of band” as extra variables
(see second-class “dictionaries” in Haskell @~cite{typeclasses},
or first-class “interfaces” in “Interface-Passing Style” @~cite{LIL2012}).
This is efficient in another way, because you can pass around a few type descriptors
that do not change within a given algorithm, and not spend time wrapping and unwrapping conflations.

There are many tradeoffs that can make one style preferable to the other, or not:
@itemize[
  @item{
    When the precise type of the element is known in advance,
    a runtime type descriptor need not be used in either style,
    and the compiler can directly generate code for the known type.
    When a function works on data the precise type for which is @emph{not} known in advance,
    that’s when a runtime type descriptor in either style may help.}
  @item{
    Class-style helps most when each piece of data is always going to be used with the same type,
    and/or when functions are more “dynamic” and do not always work with the same type of data.
    Typeclass-style helps most when each piece of data can be used as element of many types,
    and/or when functions are more “static” and always work with the same type of data.
    The two styles can be complementary, as the balance of static and dynamic information
    about functions and data can vary across libraries and within a program.}
  @item{
    Typeclass-style works better when the data is or might not be records, but has homogeneous types,
    and it is only wasteful to re-extract the same types from each piece of data.
    Typeclass-style works well for processing a lot of uniform data.}
  @item{
    Class-style works better when data is made of records with heterogeneous types,
    and functions make control-flow decisions based on the kind of records the users feed them.
    Class-style works well for processing weakly-structured documents.}
  @item{
    Typeclass-style works better if you can build your type descriptor once per function
    and use it on a lot of data.
    Class-style works better if you can build your type descriptor once per piece of data
    and use it on a lot of functions.}
  @item{
    Typeclass-style works better when the same pieces of data are being reinterpreted
    as elements of varying types, often enough or at a fine enough grain,
    that it does not make sense to re-conflate the entire data set with every change of point of view.
    Class-style works better if type reinterpretations are few and localized.}
  @item{
    In particular, typeclass-style works better when type descriptors applied to objects
    can be extended after those objects are defined;
    class-style works better when new kinds of objects that match a type descriptor
    can be created after those type descriptors have been defined;
    in either style, you can use an extra parameter to describe at runtime the discrepancy
    between the semantics you could express at compile-time and the one you want at runtime.}
  @item{
    There can be many different type descriptors that match any given object,
    with different methods to work with them from a different point of view,
    parameterized by administrator-configured or user-specified parameters that vary at runtime.
    For instance, a given number may be used, in different contexts,
    with type descriptor that will cause it to be interacted with as a decimal number text,
    a hexadecimal number text, a position on a slide bar, or a block of varying color intensity;
    or to be serialized according to some encoding or some other.
    In a static language like Haskell, @c{newtype} enables compile-time selection between
    multiple different points of view on what is, underneath, a “same” low-level data representation;
    in a language with Prototype OO,
    “typeclass”-style type descriptors enable the same kind of behavior,
    sometimes to implement at runtime
    second-class typeclasses that are known constants at compile-time,
    but sometimes for first-class typeclasses that change at runtime based on user interaction,
    even while the object representation may stay the same.}
  @item{
    Constructors are quite special in “class-style”,
    since regular methods are called by extracting them from an object’s type descriptor,
    but there is not yet an object from which to extract a type descriptor
    when what you are doing is precisely constructing the first known object of that type
    (in the given scope at least).
    Constructors are so special, that some encodings of classes identify a class with
    “the” constructor function that generates an object of that type,
    complete with an appropriate type descriptor.
    This creates an asymmetry between constructors and other methods,
    that requires special treatment when transforming type descriptors.
    By contrast, in “typeclass-style”, constructors are just regular methods;
    there can be more than one, using different calling conventions,
    or creating elements from different subsets or subtypes of the type (disjoint or not).
    Many typeclass transformations, dualities, etc., become more uniform and simpler
    when using typeclass-style. Type descriptors in typeclass style, in which constructors
    are just normal methods, are therefore more natural and easy to use for parametric polymorphism
    than type descriptors in class style.}
  @item{
    Finally, typeclass-style can be extended more easily and more uniformly
    than traditional class-style to support APIs involving
    multiple related types being simultaneously defined in mutually recursive ways:
    data structures and their indexes, paths or zippers, containers and containees,
    bipartite graphs, grammars with multiple non-terminals, expressions and types, etc.
    Instead of distinguishing a main class under which others are nested,
    or having a hierarchy of “friend” classes indirectly recursing through a global context,
    typeclass-style treats all classes uniformly, yet can locally encapsulate
    an entire family of them, potentially infinite.
    There is, however, a way to retrieve most of these advantages of typeclass-style
    while remaining in class-style, though only few languages support it:
    using multi-methods (see below). @; TODO secref
}]

There is an isomorphism between class-style and typeclass-style type descriptors,
to the point that a simple transformation can wrap objects written in one style and their methods
so they follow the calling convention of the other style@xnote["."]{
  To go from Class-style to Typeclass-style, simply extract the type descriptor,
  class metaobject, vtable, etc., from an object, and make that your type descriptor.
  For “typeclasses” that are parameterized by many types, use a record that contains
  each of those descriptors for each of those types as fields.

  To go from Typeclass-style to Class-style, you can wrap every value or tuple of values
  into a record of the values and the typeclass they are supposed to be used with.
  When invoking typeclass functions, unwrap the values from those records to use them as arguments;
  and wrap the results back into records that conflate the values and an appropriate typeclass.
  This is the same trick that we used with recursive conflation.
  You just need to know which arguments and results of which method to wrap or unwrap.

  Note how the values associated with typeclasses can be “naked” primitive values
  that need not be records, just like the fields of class instances.
}
Simple metaprograms that enact this transformation have been written in Lisp @~cite{LIL2012}
in the simple case of functions in which the self-type only appears directly
as one of the (potentially multiple) arguments or (potentially multiple) return values of a method.
Such automation could be generalized to work with any kind of higher-order function types,
where occurrences of the self-type in arbitrary position
require suitable wrapping of arguments and return values,
in ways similar to how higher-order contracts
wrap arguments and return values with checks@~cite{Findler2002}.
Note how similarly, metaprograms have been written to transform pure objects into
mutable objects that store a current pure value, or mutable objects into
linear pure objects that become invalid if a use is attempted after mutation.

Thus, programming with either (A1) classes or (A2) typeclasses,
wherein objects are either (B1) mutable or (B2) pure,
is a matter of style, with some tradeoffs with respect to performance,
ease of reasoning, between the four combined styles.
You could add more style variants, such as data representation as
(C1) a graph of records, or (C2) tables of entities,
(D1) dynamically typed, or (D2) statically typed, etc.
In the end, programs in one style can be mechanically transformed
into programs in another style, and vice versa.
Some programs, given the kind of data they are expected to work on at runtime,
may be better compiled onto one of those styles, if not directly written in it.
But if programmers have to deal with conceptual issues
that are more natural in a different style,
they may be better off writing their programs in the style that works for them,
and pay the price of automatic or manual translation later.

Interestingly, all this section’s discussion was about styles for target programs.
Type descriptors can be used in any and all of those styles without any OO whatsoever@xnote["."]{
  Indeed, Haskell typeclasses are multi-type descriptors with modularity but without inheritance,
  and so their Rust equivalent called “traits”—
  not to be confused with Scala “traits”, that are single-type descriptors with multiple inheritance.
  Modules in SML or OCaml can also offer “typeclass-style” type descriptors
  without modular extensibility through inheritance.

  Non-OO class-style type descriptors are also possible.
  For instance, Gambit Scheme allows users to define new data structures,
  and to declare the equivalent of methods that specialize how values of those new types
  will be printed, or tested for equality;
  these methods are not part of any actual object system capable of inheritance,
  yet each “structure” record carries the equivalent of a type descriptor field
  in “class style” so that the system knows which method to use.
  It is possible to layer an actual OO class system on top of such non-OO “class-style” mechanism,
  and indeed the @(GerbilScheme) object system is built
  atop Gambit Scheme’s non-OO structure facility.
}
And OO can be used at runtime or compile-time to specify and generate
type descriptors in any and all of those styles, as well as non-type-descriptor targets.
OO is completely agnostic as to whether you write in any particular style or another.
OO is about specifications for programs and parts of programs.
Although it was historically developed in the context
of the style (A1, B1, C1, D1) of dynamic graph of mutable classes (Smalltalk, Lisp, JavaScript),
or the style (A1, B1, C1, D2) of static graph of mutable classes (Simula, C++, Java),
OO can well accommodate any style.
I wrote OO in both Typeclass-style (A2) and pure-style (B2) @~cite{LIL2012 GerbilPOO},
and I have no doubt you could write OO to manipulate tables (C2) instead of record graphs (C1).
There is no reason why you couldn’t use OO to specify programs in the (A2, B2, C2, D2)-style
of static tables of pure functional typeclasses: this combination would make for a nice
hypothetical language to statically specify pure transformations on tables of uniform data,
with applications to modeling neural networks, physics engines or 3d video pipelines.

@subsection{A Class is Second-Class in Most Class OO}

In most Class OO languages,
the semantics of Class OO are fully evaluated at compile-time,
in a “type-level” evaluation stage.
The class specifications, and the type descriptors they specify,
are second-class entities:
they are not available as first-class values subject to arbitrary programming at runtime.
Class OO then is a special case of Prototype OO,
but only in a restricted second-class language—a reality
that is quite obvious when doing template metaprogramming in C++.
@;{TODO SECREF appendix demonstrating lazy prototype OO in C++}

Some dynamic languages, such as Lisp, Smalltalk, Ruby or Python,
let you programmatically use, create, inspect or modify classes at runtime,
using some reflection mechanisms.
The regular definition and usage of classes involve dedicated syntax,
such that restricting yourself to the language fragment with that syntax
and never using reflection would be equivalent to classes being second-class;
but the reflection mechanisms ultimately make classes into first-class entities.
Indeed, the second-class semantics of classes are often implemented in terms of
those first-class mechanisms, that are thus just as powerful, or more so.

Static languages lack such full-powered runtime reflection mechanisms
(or they would arguably be dynamic languages indeed).
Some lack any runtime reflection at all;
but many offer read-only runtime reflection,
whereby users can inspect classes created at compile-time, yet not create new ones:
such capabilities can notably simplify I/O, property-based testing, debugging, etc.
A few static language implementations may even offer limited ability
to modify existing classes at runtime,
but often void the compiler’s warranty for those who use them.

@subsection{Type-Level Language Restrictions}

The language in which second-class classes are defined and composed
in static languages is almost never the same as the “base” language
in which the programmer specifies first-class computations (e.g. C++, Java, C#),
but instead a distinct @emph{type-level language},
deliberately restricted in expressiveness so as to enable static analysis and optimizations,
in which the types and the base-level functions operating on them
are being modularly and extensibly specified.

Programming language designers put restrictions on their type-level language
as they attempt to keep them both (1) sound, and also, inasmuch as possible
(2) terminating in finite and practically guaranteed short time.
These attempt sometimes succeed, but more often than not utterly fail,
because computational power and/or logical contradiction emerge
from unforeseen interactions as the languages grow in complexity over time
(see @secref{OOTP})@xnote["."]{
  Even the C preprocessor, with annoying rules added to “guarantee” termination in finite time,
  ends up allowing arbitrary metaprogramming in practice @~cite{Hirrolot2021}.
  Henry Baker tried to explain it in old posts on USENET that I never understood,
  stupidly believing the guarantees, even though I could myself prove that
  the “finite time” of termination could easily be made longer than the age of the universe.
  @; TODO find and cite hbaker usenet
}
The attempts do usually succeed, however, at making these type-level languages
require a completely different mindset from the “base language”,
and very roundabout design patterns, to do anything useful,
a task then reserved for experts.

Computationally powerful or not, the type-level language of a Class OO language
is almost always very different from the base language:
the type-level languages tend to be pure functional or logic programming languages
with pattern-matching and laziness but without any I/O support;
this is in stark contrast with the base languages, that themselves tend to be
eager stateful procedural languages with lots of I/O support
and often without pattern-matching or laziness
(or limited ones as afterthoughts).

@subsection{More Popular yet Less Fundamental}

Class OO was historically discovered (1967),
nine years before Prototype OO (1976),
and remains overall more popular in the literature and in practice:
most popular OO languages only offer Class OO; @;{TODO cite}
and even though the arguably most popular OO language, JavaScript,
may have started with Prototype OO only @~cite{Eich1996JavaScript},
people were constantly reimplementing classes on top—and twenty years later,
classes were added to the language itself@~cite{EcmaScript2015}.

And yet I will argue that Prototype OO is more fundamental than Class OO:
as I demonstrated above, Class OO can be easily expressed in terms of Prototype OO
and implemented on top of it,
such that inheritance among classes is indeed a special case of
inheritance among the underlying prototypes;
however the opposite is not possible,
since you cannot express Prototype OO’s first-class entities and their inheritance
in terms of Class OO’s second-class entities and their inheritance.

At best, Prototype OO can be implemented on top of those dynamic languages
that offer full-powered reflection so that prototypes can be classes;
but even then it is unclear how much these mechanisms help,
compared to directly implementing prototypes.
There could be code sharing between the two; yet trying to fit prototypes
on top of classes rather than the other way around is what Henry Baker dubbed
an @emph{abstraction inversion} @~cite{Baker1992CritiqueDKL},
i.e. putting the cart before the horse.


@exercise[#:difficulty "Easy"]{
  Implement a simple type descriptor for a @c{Point} class with fields @c{x} and @c{y},
  an @c{instance-methods} record containing a @c{distance-from-origin} method.
  What are the space and time tradeoffs of such a method
  compared to the @c{rho-spec} of previous chapter?
}
@exercise[#:difficulty "Easy"]{
  If you did exercise @exercise-ref{alist0},
  package the functions together in one prototype.
  You now have a typeclass-style type descriptor.
}
@exercise[#:difficulty "Medium"]{
  Wrap the typeclass-style type descriptor above into a class-style type descriptor.
  Play with it on some examples.
}
@exercise[#:difficulty "Medium"]{
  The usual idiotic puzzle in Class OO design is whether
  @c{Square} should be a subclass of @c{Rectangle}
  or the other way around,
  when a Square is defined by its width,
  and the Rectangle by its width and height,
  so it looks Rectangle is a subclass of Square,
  yet clearly a Square is a subtype of Rectangle.
  The two classes should both inherit from a common superclass @c{Shape}
  with a method @c{area} that the @c{Square} and @c{Rectangle} classes should appropriately override.
  Can you figure out and implement the correct solution without reading the footnote?@Note{
    The actual solution is of course that should distinguish a
    @c{RectangleInterface} that has @emph{getter methods} @c{width} and @c{height},
    from @c{RectangleImplementation} that has @emph{fields} @c{%width} and @c{%height},
    and similarly for the Square having only a single field @c{%side}
    with and a getter method @c{side},
    wherein @c{SquareImplementation} inherits from @c{SquareInterface}
    that inherits from @c{RectangleInterface}
    (that it could also implement, though that might happen in a separate helper class),
    whereas @c{RectangleImplementation} only inherits from @c{RectangleInterface}.
    In practice, it is nicer to have a short prefix and/or suffix for interfaces,
    and another for implementations, as your naming conventions, e.g.
    @c{IRectangle} or @c{<Rectangle>} for interface,
    and @c{$Rectangle} or @c{Rectangle%} for implementation.
    You might choose the bare word without prefix or suffix for either,
    but obviously not for both.
    Many apparent paradoxes in OO design having to do with covariance and contravariance
    can similarly be solved by separating the
    “positive” and “negative” sides of a class
    (what it provides to users vs what it requires from users).
}}
@exercise[#:difficulty "Medium"]{
  Unwrap the class-style type descriptors from the previous exercise
  into typeclass-style type descriptors, and play with them.
  What did using data in one style feel compared to the other?
}
@exercise[#:difficulty "Hard"]{
  If you did the exercise @exercise-ref{4polyinterpol},
  redo it with parametric typeclass-style type descriptors.
  Throw a dice for which available representation to use
  for records, for prototypes, for polymorphism.
}
@exercise[#:difficulty "Hard"]{
  Define a typeclass-style multi-type descriptor for
  a Graph with Vertex and Edge types, and
  implement Tarjan’s Strongly Connected Component algorithm
  as a client to that descriptor.
}

@section[#:tag "SOO"]{Stateful OO}

@subsection{Mutability of Fields as Orthogonal to OO}

I have shown how OO is best explained in terms of pure lazy functional programming,
and how mutable state is therefore wholly unnecessary for OO.
There have been plenty of pure functional object libraries since at least the 1990s,
even for languages that support mutable objects;
OO languages that do not support any mutation at all have also existed since at least the 1990s,
and practical such languages with wide adoption exist since at least the early 2000s.
@;{TODO cite}

Yet, historical OO languages (Lisp, Simula, Smalltalk),
just like historical OO-less languages of the same time (FORTRAN, ALGOL, Pascal),
were stateful, heavily relying on mutation of variables and record fields.
So are the more popular OO and OO-less languages of today, still,
though there are now plenty of less-popular “pure (functional)” options.
How then does mutation fit in my functional OO paradigm?
The very same way it does on top of Functional Programming in general, with or without OO:
by adding an implicit (or then again explicit) “store” argument to all (or select) functions,
that gets linearly (or “monadically”) modified and passed along the semantics of those functions
(the linearity, uniqueness or monadicity ensuring that there is one single shared state
at a time for all parts of the program).
Then, a mutable variable or record field is just
a constant pointer into a mutable cell in that store,
a “(mutable) reference”.

This approach perfectly models the mutability of object fields, as found in most OO languages.
It has the advantages of keeping this concern orthogonal to others,
so that indexed products, fixpoints, mutation, visibility rules and subtyping constraints
(separately before and after fixpointing), etc.,
can remain simple independent constructs each with simple reasoning rules,
logically separable from each other yet harmoniously combinable together.
By contrast, the “solution” found in popular languages like C++ or Java
is all too often to introduce a single mother-of-all syntactic and semantic construct
of immense complexity, the “class”, that frankly not a single person in the world fully understands,
and of which scientific papers only dare study
but simplified (yet still very complex) models.

Actually, when I remember that in most OO languages,
OO is only ever relevant but at compile-time,
I realize that of course mutation is orthogonal to OO,
even in these languages, nay, especially so in these languages:
since OO fragments are wholly evaluated at a time before there is any mutation whatsoever,
mutation cannot possibly be part of OO, even though it is otherwise part of these languages.
Indeed the compile-time programming model of these languages, if any, is pure lazy functional.
Thus, whether fields are mutable or immutable is of precious little concern
to the compiler fragment that processes OO:
it’s just a flag passed to the type checker and code generator after OO is processed away.

@subsection{Mutability of Inheritance as Code Upgrade}

Keeping mutability orthogonal to OO as above works great as long as the fields are mutable,
but the inheritance structure of specifications is immutable.
Happily, this covers every language with second-class classes (which is most OO languages),
but also all everyday uses of OO even in languages with first-class prototypes and classes.
Still, there are use cases in which changes to class or prototype hierarchies
is actively used by some dynamic OO systems such as Smalltalk or Lisp:
to support interactive development, or schema upgrade in long-lived persistent systems.
How then to model mutability of the inheritance structure itself,
when the specification and targets of prototypes and classes are being updated?

First, I must note that such events are relatively rare,
because they involve programmers not only typing, but thinking,
which happens millions of times slower than computers process data—even when the programmer is an AI.
Most evaluation of most programs, especially where performance matters,
happens between two such code upgrades,
in large spans of time during which the code is constant.
Therefore, the usual semantics that consider inheritance structures as constant
still apply for an overwhelming fraction of the time and an overwhelming fraction of objects,
even in presence of such mutability.
There are indeed critical times when these usual semantics are insufficient,
and the actual semantics at those times must be explained;
but the usual semantics wherein inheritance is constant are not rendered irrelevant
by the possibility of dynamic changes to object inheritance.

Second, updates to the inheritance structure of OO specifications
can be seen as a special case of code upgrade in a dynamic system.
Code upgrade, whether it involves changes to inheritance structure or not,
raises many issues such as the atomicity of groups of upgrades
with respect to the current thread and concurrent threads,
or what happens to calls to updated functions from previous function bodies in
frames of the current thread’s call stack,
how the code upgrades do or do not interfere with optimizations such as inlining or reordering
of function calls,
how processor caches are invalidated, page permissions are updated,
how coherency is achieved across multiple processors on a shared memory system, etc.
These issues are not specific to inheritance mutability, and
while they deserve a study of their own,
the present book is not the right place for such a discussion.
The only popular programming language that fully addresses all these code upgrade issues
in its defined semantics is Erlang @~cite{Armstrong2003};
however it does not have any OO support, and its approach
is not always transposable to other languages@xnote["."]{
  Erlang will notably kill processes that still use obsolete code
  from before the current version now being upgraded to the next one.
  This is possible because Erlang has only very restricted sharing of state between processes,
  so it can ensure PCLSRing @~cite{PCLSRing} without requiring user cooperation;
  this is useful because Erlang and its ecosystem have a deep-seated “let it fail” philosophy
  wherein processes randomly dying is expected as a fact of life,
  and much infrastructure is provided for restarting failed processes,
  that developers are expected to use.
}
Lacking such deep language support, user support is required to ensure upgrades only happen
when the system is “quiescent” (i.e. at rest,
so there are no issues with outdated code frames upstack or in concurrent threads)
for “Dynamic Software Updating” @~cite{DSU2001};
at that point, the compiler need only guarantee that calls to the upgradable entry points
will not have been inlined.
Happily, since code upgrade events happen at a much larger timescale than regular evaluation,
it is also generally quite acceptable for systems to wait until the right moment
that the system is indeed quiescent, after possibly telling its activities to temporarily shutdown,
before applying such code upgrades.

Third, I must note how languages such as Smalltalk and Common Lisp include a lot of support
for updating class definitions, including well-defined behavior with respect to how objects
are updated when their classes change:
see for instance the protocol around @c{update-instance-for-redefined-class}
in CLOS, the Common Lisp Object System @~cite{Bobrow1988CLOS}.
These facilities allow continuous concurrent processing of data elements
with some identity preserved as the code evolves, even as
the data associated to these identities evolves with the code.
Even then, these languages do not provide precise and portable semantics
for how such code upgrades interfere with code running in other threads,
or in frames up the stack from the upgrade, so once again,
users must ensure quiescence or may have to deal with spurious incoherence issues,
up to possible system corruption.

Lastly, as to providing a semantics for update in inheritance structure,
language designers and/or programmers will have to face the question of what to do
with previously computed targets when a specification is updated:
Should a target once computed be left forever unchanged,
now out-of-synch with the (possibly conflated) specification?
Should a target be wholly invalidated, losing any local state updates since it was instantiated?
Should a target have “direct” properties that override any computation involving inheritance,
while “indirect” properties are recomputed from scratch just in case the inheritance structure changed?
Should some “indirect” properties be cached, and if so how is this cache invalidated when
there are changes?
Should a protocol such as @c{update-instance-for-redefined-class} be invoked to update this state?
Should this protocol be invoked in an eager or lazy way
(i.e. for all objects right after code update, or on a need basis for each object)?
Should a class maintain at all times and at great cost a collection of all its instances,
just so this protocol can be eagerly updated once in a rare while?
Should some real-time system process such as the garbage collector
ensure timely updates across the entire heap
even in absence of such explicitly maintained collection?
Are children responsible for “deep” validity checks at every use,
or do parents make “deep and wide” invalidations at rare modifications,
or must parents and children somehow deal with incoherence?
There is no one-size-fits-all answer to these questions.

If anything, thinking in terms of objects with identity and mutable state
both forces software designers to face these issues,
inevitable in interactive or long-lived persistent systems,
and provides them with a framework to give coherent answers to these questions.
Languages that assume “purity” or absence of code upgrade, thereby deny these issues,
and leave their users helpless, forced to reinvent entire frameworks of identity and update
so they may then live in systems they build on top of these frameworks,
rather than directly in the language that denies the issues.

@exercise[#:difficulty "Easy"]{
  Implement a @c{Counter} prototype two ways:
}
@itemize[
  @item{Pure functional: @c{increment} returns a new counter with value increased by 1}
  @item{Mutable: @c{increment!} modifies the counter in place and returns @c{#f},
         or some other appropriate unit value (e.g. @c{(void)} in many Scheme implementations).
  }]
Show that both versions can be used to count from 0 to 10.
What is the key difference in how client code must be written for each version?

@exercise[#:difficulty "Easy"]{
  The chapter claims that mutation is orthogonal to OO
  because OO is fully evaluated at compile-time in most languages.
  Examine a simple class hierarchy in Java or C++.
  Identify which computations happen at compile-time (class structure, method resolution)
  versus runtime (field mutation, method execution).
  Does mutation ever affect the compile-time OO computations? Why or why not?
}

@exercise[#:difficulty "Medium"]{
  The chapter mentions that conflation enables state sharing
  between all users of a prototype.
  Demonstrate this by creating a mutable @c{SharedCounter} prototype,
  obtaining two "references" to it (by extracting the target twice),
  incrementing through one reference, and observing the change through the other.
  Then demonstrate cloning (creating a new prototype with the same specification)
  to obtain independent state. What is the semantic difference between
  "extracting the target" and "cloning the prototype"?
}

@exercise[#:difficulty "Medium"]{
  The chapter states that “laziness proves essential to OO,
  even and especially in presence of side-effects.”
  Demonstrate this by:}
  @itemize[
    @item{Creating a specification that performs I/O (e.g., prints a message)
          during instantiation.}
    @item{Showing that without laziness, composing specifications via @c{mix}
          causes unwanted duplicate I/O.}
    @item{Showing that with lazy instantiation, I/O only happens once,
          when the final target is forced.}]

@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{05to06}, compare your previous answers with mine.
  See what you got right or wrong about Prototype OO, Class OO, and Types for OO,
  and more importantly, what surprised you—which is what you actually learn from this book.
}

@exercise[#:difficulty "Hard, Recommended" #:tag "06to07"]{
  Now that you are familiar with how to use and implement OO,
  using what underneath is mixin inheritance,
  can you model other forms of inheritance in wide use
  that you are familiar with?
  Unless you have never used any OO before,
  making a model of single inheritance should be relatively easy.
  Making a model of multiple inheritance, on the other hand can be quite hard,
  especially since there are actually two very different kinds of multiple inheritance,
  the “flavorless” (as in Smalltalk, C++, Ada) and
  the “flavorful” (as in CLOS, Ruby, Python, Scala).
  Can you model whichever kinds of multiple inheritance you have used in the past, if any?
  Or invent your own, if you are not familiar with either?
  Save your answer to compare with the treatment in @secref{IMSMO}.
}

@exercise[#:difficulty "Hard"]{
  Read my article @citet{LIL2012}, and implement in your language of choice
  the automated wrapping between pure and stateful style, class and typeclass styles.
  For extra points, also implement higher-order wrappings in the style of @citet{Findler2002}.
  For research points, instead of wrapping just the interface, translate the code itself.
  For extra research points, automate the translation between styles at the compiler level,
  on demand, depending on the context.
}

@exercise[#:difficulty "Hard"]{
  C++ is a pure functional lazy dynamic language—at compile-time.
  With the help of AI if needed, implement lazy streams, a lazy stream of all the integers,
  and a stream of the all the factorial numbers, plus a test for the tenth one,
  @c{using} a recent version of the C++ template metaprogramming language.
  If you feel gung ho about C++ templates, implement the λ-calculus,
  and on top of it the @c{mix} and @c{fix} functions and the examples from @secref{MOO}.
}

@exercise[#:difficulty "Hard"]{
  Design and implement a simple protocol for updating instances
  when their class is redefined, inspired by CLOS’s
  @c{update-instance-for-redefined-class}.
  Your protocol should handle:}
  @itemize[
    @item{Adding a new field with a default value}
    @item{Removing a field (what happens to its data?)}
    @item{Renaming a field (preserving its value)}
    @item{Changing a field's type (with a user-provided conversion function)}]
  Demonstrate with a @c{Person} class that evolves through three versions:
  v1 has @c{name}; v2 adds @c{age}; v3 renames @c{name} to @c{full-name}
  and adds @c{email}.
  Bonus: Make it a research topic by generalizing the issue to that
  of schema upgrade for a persistent object store with many indexes
  that may require multiple phases to update.

@exercise[#:difficulty "Hard"]{
  The chapter discusses the question of what happens to computed targets
  when a specification is updated.
  Implement and compare three strategies:}
  @itemize[
    @item{@emph{Eager invalidation}: all targets are immediately recomputed
          when any specification changes.}
    @item{@emph{Lazy invalidation}: targets are marked "dirty" and only recomputed
          on next access.}
    @item{@emph{Versioned}: old targets remain valid for their specification version;
          new accesses get new targets.}]
  Discuss the tradeoffs in terms of consistency, performance, and complexity.
  Which strategy is most appropriate for (a) interactive development,
  (b) long-running servers, (c) real-time systems?

@exercise[#:difficulty "Hard"]{
  The chapter mentions that Erlang is the only popular language
  that fully addresses code upgrade semantics.
  Research Erlang's hot code loading mechanism and its interaction with processes.
  Then design (and optionally implement) a similar mechanism for a Scheme-based OO system:}
  @itemize[
    @item{How do you ensure "quiescence" before applying updates?}
    @item{How do you handle objects that are mid-operation during an upgrade?}
    @item{How do you handle objects referenced from the call stack?}]
  What simplifying assumptions does your design make compared to Erlang?

@exercise[#:difficulty "Research"]{
  Add support for class update to an existing object system that doesn’t support it yet,
  or implement a new object system that supports it.
  You may have to add a level of indirection to your representations, or a read barrier.
  For extra brownies, specify and implement a protocol to ensure objects are quiescent
  before they may be updated (see notably my thesis @~cite{FarePhD Rideau2018Climbing})
}
