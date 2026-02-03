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

@subsection[#:tag "WdIjd"]{What did I just do?}
In the previous chapter, I reconstructed
a minimal yet recognizable model of OO from first principles,
the principles being modularity, extensibility, and first-class entities.
I will shortly extend this model to support more features
(at the cost of more lines of code).
Yet my “object system” so far has no classes, and indeed no objects at all:
instead, like the object system of Yale T Scheme @~cite{Adams1988oopscheme},
on top of which its windowing system was built,
my system is made of target records and their specifications,
that can do almost everything that a Prototype object system does,
but without either records or specifications being objects or prototypes
(see @secref{PaC}).

I defined my system in two lines of code, that can be similarly defined
in any language that has higher-order functions,
even a pure functional language without mutation;
and indeed Nix defines its “extension” system similarly@~cite{nix2015}.
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
  (cons spec (fix-record spec)))
(def spec←pproto car)
(def (target←pproto pproto)
  (cdr pproto))
(def pproto-id (pproto←spec idModExt))
(def (pproto-mix child parent)
  (pproto←spec (mix (spec←pproto child) (spec←pproto parent))))
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
or delay errors and divergence until after the target is computed,
when a further attempt is made to use the target.
In other words, a way to consider the target as an unevaluated computation,
rather than as the value returned by that computation if it ever finishes without an error.

The simplest such device is lazy evaluation: @c{Proto = Spec × Lazy Target}.
In second-class Class OO, the target is a descriptor for type,
that itself can always be computed without error in finite time,
though the type may be empty, and trying to use it may result in static or dynamic errors.
Execution in a latter stage of computation (runtime vs compile-time)
can be seen as the ultimate form of delayed evaluation.

Note how in our representation of records as function so far,
all the potentially non-terminating or error-throwing behavior,
including but not limited to recursion, was protected by lambda-abstractions.
Thus, delay and force are not necessary in XXXXXXXXXXXXXX

@Paragraph{Second Issue: Recursion}

Now, there is a subtle issue with the above implementation:
when a target recursively refers to “itself” as per its specification,
it sees the target only, and not the conflation of the target and the specification.
This is not a problem with second-class OO, or otherwise with a statically staged style
of programming where all the specifications are closed before any target is instantiated;
then at one stage you consider only specifications, at the other, you consider only targets.

But with a more dynamic style of programming where no such clear staging is guaranteed,
it is insufficient@xnote["."]{
  @; TODO Have a later chapter just on metaobjects?
  Metaobjects are a typical use case where you don’t (in general) have a clean program-wide staging
  of specification and targets: to determine the meta methods,
  you partially instantiate the meta part of the objects, based on which you can instantiate the rest.
}

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
  If they had conceptualized the two as being entities that need to be distinguished semantically
  then explicitly grouped together as a pair, they could have solved the problem and stayed
  on top of the λ-calculus.
  Instead, they abandon such attempts, and rebuild their own syntactic theory
  of a variant of the λ-calculus just for object,
  an insanely complex @emph{abstraction inversion} @~cite{Baker1992CritiqueDKL}
  that won’t enlighten OO practitioners,
  nor make OO interesting to mathematical syntax theoreticians.
}
Here is an implementation of that idea, wherein I prefix function names with @c{qproto}:

@Code{
(def (qproto-wrapper spec _self super)
  (cons spec super))
(def (qproto←spec spec)
  (delay (fix-record (mix (qproto-wrapper spec) spec))))
}
Note how the following functions are essentially unchanged compared to @c{pproto}:
@Code{
(def spec←qproto car)
(def (target←qproto qproto)
  (force (cdr qproto)))
(def (qproto-mix child parent)
  (qproto←spec (mix (spec←qproto child) (spec←qproto parent))))
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
between the OO models of C++ vs Java: C++ makes you deal with data structures,
Java with references to data structures.

Now, it is not unusual in computer science for access to records, recursive or not,
to be wrapped inside some kind of reference type:
pointer into memory, index into a table,
key into a database, cryptographic hash into a content-addressed store,
location into a file, string or identifier used in a hash-table or tree, etc.
In the case of recursive data structures implemented as data in contiguous regions of memory,
such level of indirection is inevitable, as there is no way to have a contiguous region of memory
of some size contain as a strict subset a contiguous region of memory of the same size,
as would be required for a data structure to directly
include an recursive element of the same type@xnote["."]{
  Exception: If the only element of a structure is an element of the same structure.
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
the latency of which grows as the square root of the size of the working set@~cite{MythOfRAM2014}.

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

Another device might be for the targets to be records of lazy values,
The target
they could also be records of modular definitions
wherein the fixpoint is only computed when the user tries to call a method.
XXXXX

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
(def (rproto-wrapper spec self super method-id)
  (if method-id (super method-id) spec))
(def (rproto←spec spec)
  (fix-record (mix (rproto-wrapper spec) spec)))
(def rproto-id (rproto←spec idModExt))
(def (spec←rproto rproto)
  (rproto #f))
(def target←rproto identity)
(def (rproto-mix child parent)
  (rproto←spec (mix (spec←rproto child) (spec←rproto parent))))
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
seems never, ever, to have been made explicit in the literature until
I published it @~cite{poof2021}.
And yet, the knowledge of this conflation is necessarily present, if implicit,
if not across the OO community, at the very least among OO implementers—or else
OO wouldn’t be possible at all.
Sixty years of crucial information you don’t know you know!

Common practitioners of OO have long implicitly recognized
the conflated concepts of specification and target.
Back in 1979, Flavors @~cite{Cannon1979} introduces the concept of a @emph{mixin} as
a flavor meant to be inherited from, but not to be instantiated,
by opposition to an instantiatable flavor;
however, the nomenclature only stuck in the Lisp community
(and even there, flavors yielded to classes though the term mixin stayed).
In other communities, the terms of art are
@emph{abstract classes} and @emph{concrete classes}@~cite{johnson1988designing}:
an abstract class is one that is only used for its specification—to inherit from it;
a concrete class is one that is only used for its target type—to use its methods
to create and process class instances.
Experienced practitioners recommend keeping the two kinds of classes separate, and
frown at inheriting from a concrete class,
or trying to instantiate an abstract class.

Theorists have also long implicitly recognized the conflated concepts
when working to develop sound typesystems for OO:
for instance, Fisher @~cite{Fisher1996thesis} distinguishes
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
the @c{__unfix__} attribute into its target records,
and Jsonnet @~cite{jsonnet} must do something similar under the hood;
yet the authors of neither system make note of it in their documentation
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

@section[#:tag "RCOO"]{Rebuilding Class OO}

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
and no doubt broadly similar to how many Class OO systems were implemented on top of Javascript
before web browsers ubiquitously adopted ES6 classes@~cite{EcmaScript2015}. @;{CITE ???}

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

@subsection[#:tag "Class_style_vs_Typeclass_style"]{Class-style vs Typeclass-style}

Now, there are two common styles for using type descriptors: Class-style and Typeclass-style.

In Class-style, each type element
(known in this style as “class instance”, “instance”, or “object”)
carries its own type descriptor.
To this end, the type descriptor is conflated with the type element
in the same way that specifications were conflated with their targets
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
wrap arguments and return values with checks@~cite{findler2002contracts}.
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

Restrictions on the type-level language
often attempt to keep it from being “Turing-equivalent”.
This attempt sometimes succeeds (as in OCaml), but more often than not
utterly fails, as computational power emerges from unforeseen interactions
between language features, especially as features get added over time
(as in C++, Java, Haskell) @~cite{Grigore2016}@xnote["."]{
  Even the C preprocessor, with annoying rules added to “guarantee” termination,
  ends up allowing arbitrary metaprogramming in practice.
  @; TODO cite hbaker usenet
  @; TODO cite metalang99, https://github.com/Hirrolot/awesome-c-preprocessor
@TODO{cite 2016__Amin_Tate__Java_and_Scala_s_Type_Systems_are_Unsound}
@TODO{cite LangSec on unforeseen emergence of Turing-equivalence through “weird machines”?}
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

@section[#:tag "TfOO"]{Types for OO}

@subsection{Dynamic Typing}

One could just say that objects are of a monomorphic type @c{Record},
and that all accesses to methods are to be dynamically typed and checked at runtime,
with no static safety.
Many OO languages, like Smalltalk, Lisp, Ruby, Python, JavaScript, Jsonnet, Nix, etc.,
adopt this strategy.

Advantages of dynamic typing include the ability to express programs that even the best
static typesystems cannot support, especially when state-of-the-art typesystems are too
rigid, or not advanced enough in their OO idioms.
Programs that involve dependent types, staged computation,
metaprogramming, long-lived interactive systems,
dynamic code and data schema evolution at runtime, etc.,
can be and have been written in dynamically typed systems
that could not have been written in existing statically typed languages,
or would have required reverting to unitypes with extra verbosity.

On the other hand, system-supported static types can bring extra performance and safety,
help in refactoring, debugging programs,
some forms of type-directed metaprogramming, and more.
Even without system enforcement, thinking in terms of types can help understand
what programs do or don’t and how to write and use them.
I have already started to go deeper by describing records as indexed products.
Let’s see how to model OO with more precise types.

@subsection{Partial Record Knowledge as Subtyping}

In a language with static types,
programmers writing extensible modular definitions should be able to specify types
for the entities they provide (an extension to) and require (a complete version of),
without having to know anything about the types of the many other entities
they neither provide nor require:
indeed, these other entities may not have been written yet, and by other people,
yet will be linked together with his modules into a complete program.

Now, with only modularity, or only extensibility, what’s more second-class only,
you could contrive a way for the typechecker to always exactly know all the types required,
by prohibiting open recursion through the module context,
and generating magic projections behind the scenes (and a magic merge during linking).
But as I previously showed in @secref{IME},
once you combine modularity and extensibility, what’s more first-class,
then open recursion through the module context becomes the entire point,
and your typesystem must confront it.

Strict extensibility, which consists in monotonically contributing partial knowledge
about the computation being built,
once translated in the world of types, is subtyping.
In the common case of records,
a record type contains not just records
that exactly bind given identifiers to given types,
but also records with additional bound identifiers, and existing identifiers bound to subtypes.
Record types form a subtyping hierarchy; subtyping is a partial order among types;
and function types monotonically increase with their result types,
and decrease with their argument types.
Then, when modularly specifying a module extension, the modular type for the module context,
that only contains “negative” constraints about types for the identifiers being required,
will match the future actual module constraint, that may satisfy many more constraints;
meanwhile, the “positive” constraints about types for the identifiers being provided
may satisfy many more constraints than those actually required from other modules using it.

@subsection[#:tag "NNOOTT"]{The NNOOTT: Naive Non-recursive OO Type Theory}

The simplest and most obvious theory for typing OO,
that I will dub the Naive Non-recursive Object-Oriented Type Theory (NNOOTT),
consists in considering subclassing (a relation between specifications)
as the same as subtyping (a relation between targets).
Thus, in this theory, a subclass, that extends a class with new fields,
is (supposedly) a subtype of the parent “superclass” being extended.
@;{ XXX Eiffel, Java, C#, Smalltalk
  The theory is implicit in the names of the @c{is} operator in C#.
}

To demonstrate an advanced variant of the NNOOTT,
consider extending the Simply Typed Lambda-Calculus (STLC),
or some more elaborate but still well-understood variant of the λ-calculus,
with primitives for the language’s builtin constructs and standard libraries,
and (if not already available) a minimal set of features for OO:
indexed products for records, subtyping (@c{⊂} or @c{≤} or in ASCII @c{<:})
and type intersections (@c{∩}).
In this NNOOTT variant, a NNOOTT Modular Extension could have a type of the form
@Code{
type NModExt required inherited provided =
  required → inherited → (inherited ∩ provided)
}
A @c{NModExt} is a type with three parameters,
the type @c{required} of the information required by the modular extension from the module context,
the type @c{inherited} of the information inherited and to be extended,
and the type @c{provided} of the information provided to extend what is inherited.
Note that this type refines the @c{C → V → V} from @secref{MOI}:
@c{inherited} and @c{provided} each separately refine the value @c{V} being specified;
that value can be anything: it need not be a record at all, and if it is,
it can have any shape or type, and need not have the same as the module context.
Meanwhile, @c{required} refines the module context @c{C}, and is (almost) always some kind of record.
The two need not be the same at all, and usually are not for (open) modular extensions,
unless and until you’re ready to close the recursion, tie the loops and compute a fixpoint.

In Prototype OO, the value inherited holds the methods defined so far by the ancestors;
the value provided consists in new methods and specialization of existing methods;
the top value is an empty record.
In Class OO, that value inherited holds methods and fields defined so far by the ancestors;
the value provided consists in the new or specialized fields and methods;
the top value is a type descriptor for an empty record type.
In both cases, the context required may narrowly define a prototype or class,
but may also more broadly define an entire namespace.

The @c{fix} operator, first takes a top value as a seed,
then second takes a specification for a target with the target itself as module context
and starting with the top value as a seed, and returns the target fixpoint.
The @c{mix} operator chains two mixins, with the asymmetry that
information provided by the parent (parameter @c{p2} for the second argument)
can be used by the child (first argument), but not the other way around.
@Code{
fix : top → NModExt target top target → target
mix : NModExt r1 i1∩p2 p1 → NModExt r2 i2 p2 →
        NModExt r1∩r2 i1∩i2 p1∩p2
}

This model is simple and intuitive, and
has good didactic value to explain how inheritance works:
given two “mixin” specifications, you can chain them as child and parent;
the combined specification requires a context with all the information
required from either child or parent;
the inherited information must contain all information expected by the parent,
and all information expected by the child that isn’t provided by the parent;
the provided information contains all information provided by either child or parent.

However, this “Naive Non-recursive OO Type Theory”, as the name indicates,
is a bit naive indeed, and only works in simple non-recursive cases.
Yet the NNOOTT is important to understand,
both for the simple cases it is good enough to cover,
and for its failure modes that tripped so many good programmers
into wrongfully trying to equate inheritance and subtyping.

@subsection{Limits of the NNOOTT}

The NNOOTT works well in the non-recursive case, i.e.
when the types of fields do not depend on the type of the module context;
or, more precisely, when there are no circular “open” references between
types being provided by a modular extension,
and types it requires from the module context.
In his paper on objects as co-algebras,
Bart Jacobs characterizes the types for the arguments and results of his methods
as being “(constant) sets” @~cite{Jacobs1995ObjectsAC}@xnote[","]{
  Jacobs is particularly egregious in smuggling this all-important restriction
  to how his paper fails to address the general and interesting case of OO
  in a single word, furthermore, in parentheses, at the end of section 2,
  without any discussion whatsoever as to the momentous significance of that word.
  A discussion of that significance could in itself have turned this bad paper into a stellar one.
  Instead, the smuggling of an all-important hypothesis makes the paper misleading at best.
  His subsequent paper @~cite{Jacobs1996InheritanceAC}
  has the slightly more precise sentence I also quote,
  and its section 2.1 tries to paper over what it calls “anomalies of inheritance”
  (actually, the general case), by separating methods into a “core” part
  where fields are declared, that matter for typing inheritance,
  and for which his hypothesis applies, and “definitions” that must be reduced to the core part.
  The conference reviewing committees really dropped the ball on accepting those papers,
  though that section 2.1 was probably the result of at least one reviewer doing his job right.
  Did reviewers overall let themselves be impressed by formalism beyond their ability to judge,
  or were they complicit in the sleight of hand to grant their domain of research
  a fake mantle of formal mathematical legitimacy?
  Either way, the field is ripe with bad science,
  not to mention the outright snake oil of the OO industry in its heyday:
  The 1990s were a time when IBM would hire comedians to become “evangelists”
  for their Visual Age Smalltalk technology, soon recycled into Java evangelists.
  Jacobs is not the only one, and he may even have extenuating circumstances.
  He may have been ill-inspired by Goguen, whom he cites, who also abuses
  the terminology from OO to make his own valid but loosely-related
  application of Category Theory to software specification.
  He may also have been pressured to make his work “relevant” by publishing in OO conferences,
  under pains of losing funding, and
  he may have been happy to find his work welcome even though he didn’t try hard,
  trusting reviewers to send stronger feedback if his work hadn’t been fit.
  The reviewers, unfamiliar with the formalism,
  may have missed or underestimated the critical consequences of a single word;
  they may have hoped that further work would lift the limitation.
  In other times, researchers have been hard pressed to join the bandwagon of
  Java, Web2, Big Data, Mobile, Blockchain or AI, or whatever trendy topic of the year;
  and reviewers for the respective relevant conferences may have welcomed
  newcomers with unfamiliar points of view.
  Even Barbara Liskov, future Turing Award recipient, was invited to contribute to OO conferences,
  and quickly dismissed inheritance to focus on her own expertise,
  which involves modularity without extensibility—and stating
  her famous “Liskov Substitution Principle” as she did @~cite{Liskov1987};
  brilliant, though not OO. @; CITE
  Are either those who talk and publish what turns out not to be OO at all at OO conferences,
  or those who invite them to talk and publish, being deliberately misleading?
  Probably not, yet, the public can be fooled just the same as if dishonesty were meant:
  though the expert of the day can probably make the difference,
  the next generation attending or looking through the archives
  may well get confused as to what OO is or isn’t about as they learn from example.
  At the very least, papers like that make for untrustworthy identification and labeling
  of domains of knowledge and the concepts that matter.
  The larger point here being that one should be skeptical of papers,
  even by some of the greatest scientists
  (none of Jacobs’, Goguen’s nor Liskov’s expertises are in doubt),
  even published at some of the most reputable conferences in the field (e.g. OOPSLA, ECOOP),
  because science is casually corrupted by power and money,
  and only more cheaply so for the stakes being low.
  This particular case from thirty years ago is easily corrected in retrospect;
  its underlying lie was of little consequence then and is of no consequence today;
  but the system that produced dishonest science hasn’t been reformed,
  and I can but imagine what kind of lies it produces to this day in topics
  that compared to the semantics of OO are both less objectively arguable,
  and higher-stake economically and politically.
}
which he elaborates in another paper @~cite{Jacobs1996InheritanceAC}
as meaning «not depending on the “unknown” type X (of self).»
This makes his paper inapplicable to most OO, but interestingly,
precisely identifies the subset of OO for which inheritance coincides with subtyping,
or, to speak more precisely,
subtyping of modular extensions coincides with subtyping of their targets.

Indeed, in general, specifications may contain so called “binary methods”
that take another value of the same target type as argument,
such as in very common comparison functions (e.g. equality or order)
or algebraic operations (e.g. addition, multiplication, composition), etc.;
and beyond these, they can actually contain arbitrary higher-order functions
involving the target type in zero, one or many positions,
both “negative” (as an overall argument)
or “positive” (as an overall result), @; TODO cite Felleisen???
or as parameters to type-level functions, “templates”, etc.
These methods will break the precondition for subclassing being subtyping.

And such methods are not an “advanced” or “anomalous” case, but quintessential.
The very first example in the very first paper about actual classes @~cite{Simula1967},
involves recursive data types:
it is a class @c{linkage} that defines references @c{suc} and @c{pred} to the “same” type,
that classes can inherit from so that their elements shall be part of a doubly linked list.
This example, and any data structure defined using recursion,
will defeat the NNOOTT if examined closely.
Not only is such recursion a most frequent occurrence, I showed above in @secref{IME} that
while you can eschew support for fixpoints through the module context
when considering modularity or extensibility separately,
open recursion through module contexts becomes essential when considering them together.
In the general and common case in which a class or prototype specification
includes self-reference, subtyping and subclassing are very different,
a crucial distinction that was first elucidated in @citet{Cook1989Inheritance}.

Now, the NNOOTT can be “saved” by reserving static typing to non-self-referential methods,
whereas any self-reference must dynamically typed:
wherever a recursive self-reference to the whole would happen, e.g. in the type of a field,
programmers must instead declare the value as being of a dynamic “Any” type,
or some other “base” type or class,
so that there is no self-reference in the type, and the static typechecker is happy.
Thus, when defining a list of elements of type @c{A}, you could not write the usual recursive formula
@c{List(A) = 1 + A*List(A)} or the fixpoint @c{List(A) = Y (λ Self . 1 + A*Self)},
and would just write @c{List(A) = 1 + A*Any}.
Similarly, for trees with leaves of type @c{B}, you couldn’t write the recursive formula
@c{Tree(B) = B + List(Tree(B))}, and
would instead write just the non-recursive and dynamically typed
@c{Tree(B) = B + List(Any))}.

To compensate for the imprecision of the typesystem
when retrieving an element of the desired self-type,
some kind of explicit dereference, typecast (downcast), or coercion
is required from the programmer;
that operation may be either safe (with a dynamic runtime check), or
unsafe (program may silently misbehave at runtime if called with the wrong argument).
In some languages, self-reference already has to go through
pointer indirection (e.g. in C++), or
boxing (e.g. in Haskell, when using a @c{newtype Fix} generic constructor for fixpoints,
while the open modular definition goes into a “recursion scheme”);
thus the NNOOTT does not so much introduce an extra indirection step for recursion
as it makes an existing indirection step obvious—and
makes it dynamically rather than statically typed.
In other words, it makes us realize once again that @emph{recursion is not free}.

@subsection{Why NNOOTT?}

The NNOOTT was implicit in the original OO paper @~cite{Simula1967}
as well as in Hoare’s seminal paper that inspired it @~cite{Hoare1965Record}@xnote["."]{
  Hoare probably intended subtyping initially indeed for his families of record types;
  yet subclassing is what he and the Simula authors discovered instead.
  Such is scientific discovery:
  if you knew in advance what lied ahead, it would not be a discovery at all.
  Instead, you set out to discover something, but usually discover something else,
  that, if actually new, will be surprising.
  The greater the discovery, the greater the surprise.
  And you may not realize what you have discovered until analysis is complete much later.
  The very best discoveries will then seem obvious in retrospect,
  given the new understanding of the subject matter,
  and familiarity with it due to its immense success.
}
It then proceeded to dominate the type theory of OO
until debunked in the late 1980s @~cite{Cook1989Inheritance}.
Even after that debunking, it has remained prevalent in popular opinion,
and still very active also in academia and industry alike,
and continually reinvented even when not explicitly transmitted
@~cite{Cartwright2013Inheritance abdelgawad2014domain}.
And I readily admit it’s the first idea I too had
when I tried to put types on my modular extensions,
as you can see in @citet{poof2021}.

The reasons why, despite being inconsistent, the NNOOTT was and remains so popular,
not just among the ignorant masses, but even among luminaries in computer science,
is well worth examining.

@itemize[
@item{
  The NNOOTT directly follows from the confusion between specification and target
  when conflating them without distinguishing them (@secref{PaC}).
  The absurdity of the theory also follows from the categorical error of equating entities,
  the specification and its target, that
  not only are not equivalent, but are not even of the same type.
  But no one @emph{intended} for “a class” to embody two very distinct semantic entities;
  quite on the contrary, Hoare, as well as the initial designers of
  Simula, KRL, Smalltalk, Director, etc.,
  were trying to have a unified concept of “class” or “frame” or “actor”, etc.
  Consequently, the necessity of considering two distinct entities
  was only fully articulated in the 2020s(!).}
@item{
  In the 1960s and 1970s, when both OO and type theory were in their infancy,
  and none of the pioneers of one were familiar with the other,
  the NNOOTT was a good enough approximation that even top language theorists were fooled.
  Though the very first example in OO could have disproven the NNOOTT,
  still it requires careful examination and familiarity with both OO and Type Theory
  to identify the error, and pioneers had more urgent problems to solve.}
@item{
  The NNOOTT actually works quite well in the simple “non-recursive” case
  that I characterized above.
  In particular, the NNOOTT makes sense enough
  in the dynamically typed languages that (beside the isolated precursor Simula)
  first experimented with OO in the 1970s and 1980s,
  mostly Smalltalk, Lisp and their respective close relatives.
  In those languages, the “types” sometimes specified for record fields
  are suggestions in comments, dynamic checks at best,
  sometimes promises made by the user to the compiler,
  never static guarantees made by the language;
  the recursive case was always dynamically typed, as was any non “atomic” value.}
@item{
  Even in the 1980s and 1990s, theorists and practitioners being mostly disjoint populations,
  did not realize that they were not talking about precisely the same thing
  when talking about a “class”.
  Those trained to be careful not to make categorical errors
  might not have realized that others were doing it in ways that mattered.
  The few at the intersection may not have noticed
  the discrepancy, or understood its relevance, when scientific modeling
  must necessarily make many reasonable approximations all the time.
  Once again, more urgent issues were on their minds.}
@item{
  Though the NNOOTT is inconsistent in the general case of OO,
  as obvious from quite common examples involving recursion,
  it will logically satisfy ivory tower theorists or charismatic industry pundits
  who never get to experience cases more complex than textbook examples,
  and pay no price for dismissing recursive cases as “anomalies” @~cite{Jacobs1996InheritanceAC}
  when confronted with them.
  Neither kind owes their success to getting a consistent theory
  that precisely matches actual practice.}
@item{
  The false theory will also emotionally satisfy those practitioners and their managers
  who care more about feeling like they understand rather than actually understanding.
  This is especially true of the many who have trouble thinking about recursion,
  as is the case for a majority of novice programmers and vast majority of non-programmers.
  Even those who can successfully @emph{use} recursion,
  might not be able to @emph{conceptualize} it, much less criticize a theory of it.
  @;{ TODO locate study that measures the recursion-ables from the unable. }
}]

@subsection{Beyond the NNOOTT}

The key to dispelling the
“conflation of subtyping and inheritance” @~cite{Fisher1996thesis}
or the “notions of type and class [being] often confounded” @~cite{bruce1996typing}
is indeed first to have dispelled, as I just did previously,
the conflation of specification and target.
Thereafter, OO semantics becomes simple:
by recognizing target and specification as distinct,
one can take care to always treat them separately,
which is relatively simple,
at the low cost of unbundling them apart before processing,
and rebundling them together afterwards if needed.
Meanwhile, those who insist on treating them as a single entity with a common type
only set themselves for tremendous complexity and pain.

That is how I realized that what most people actually mean by “subtyping” in extant literature is
@emph{subtyping for the target type of a class} (or target prototype),
which is distinct from
@emph{subtyping for the specification type of a class} (or prototype),
variants of the latter of which Kim Bruce calls “matching” @~cite{SubtypingMatch1997}.
@; TODO cite further
But most people, being confused about the conflation of specification and target,
don’t conceptualize the distinction, and either
try to treat them as if it were the same thing,
leading to logical inconsistency hence unsafety and failure;
or they build extremely complex calculi to do the right thing despite the confusion.
By having a clear concept of the distinction,
I can simplify away all the complexity without introducing inconsistency.

One can use the usual rules of subtyping @~cite{cardelli1986understanding} @; TODO cite
and apply them separately to the types of specifications and their targets,
knowing that “subtyping and fixpointing do not commute”,
or to be more mathematically precise,
@emph{fixpointing does not distribute over subtyping},
or said otherwise, @principle{the fixpoint operator is not monotonic}:
If @c{F} and @c{G} are parametric types,
i.e. type-level functions from @c{Type} to @c{Type},
and @c{F ⊂ G} (where @c{⊂}, sometimes written @c{≤} or @c{<:},
is the standard notation for “is a subtype of”,
and for elements of @c{Type → Type} means @c{∀ t, F t ⊂ G t}),
it does not follow that @c{Y F ⊂ Y G} where @c{Y} is the fixpoint operator for types@xnote["."]{
  The widening rules for the types of specification
  and their fixpoint targets are different;
  in other words, forgetting a field in a target record, or its some of its precise type information,
  is not at all the same as forgetting that field or its precise type in its specification
  (which introduces incompatible behavior with respect to inheritance,
  since extra fields may be involved as intermediary step in the specification,
  that must be neither forgotten, nor overridden with fields of incompatible types).

  If the two entities are treated as a single one syntactically and semantically,
  as all OO languages so far have done, @; ALL??
  then their typesystem will have to encode in a weird way a pair of subtly different types
  for each such entity, and the complexity will have to be passed on to the user,
  with the object, and each of its field having two related but different declared types,
  and potentially different visibility settings.
  Doing this right involves a lot of complexity, both for the implementers and for the users,
  at every place that object types are involved, either specified by the user, or display to him.
  Then again, some languages may do it wrong by trying to have the specification fit the rules
  of the target (or vice versa), leading to inconsistent rules and consistently annoying errors.

  A typical way to record specification and target together is to annotate fields with visibility:
  @c{public} (visible in the target)
  yet possibly with a more specific type in the target
  than in the specification (to allow for further extensions that diverge from the current target);
  fields marked @c{protected} (visible only to extensions of the specification, not in the target);
  and fields marked @c{private} (not visible to extensions of the specification,
  even less so to the target; redundant with just defining a variable in a surrounding @c{let} scope).
  I retrieve these familiar notions from C++ and Java just by reasoning from first principles
  and thinking about distinct but related types for a specification and its target.

  Now, my opinion is that it is actually better to fully decouple the types
  of the target and the specification, even in an “implicit pair” conflating the two:
  Indeed, not only does that means that types are much simpler, that also mean that
  intermediate computations, special cases to bootstrap a class hierarchy,
  transformations done to a record after it was computed as a fixpoint, and
  records where target and specification are out of sync
  because of effects somewhere else in the system, etc.,
  can be safely represented and typed,
  without having to fight the typesystem or the runtime.
}

A more precise view of a modular extension is thus as
an entity parameterized by the varying type @c{self} of the module context
(that Bruce calls @c{MyType} @~cite{bruce1996typing SubtypingMatch1997}). @; TODO cite further
As compared to the previous parametric type @c{NModExt} that is parametrized by types @c{r i p},
this parametric type @c{ModExt} is itself parametrized by parametric types @c{r i p}
that each take the module context type @c{self} as parameter@xnote[":"]{
  The letters @c{r i p}, by contrast to the @c{s t a b} commonly used for generalized lenses,
  suggest the mnemonic slogan: “Generalized lenses can stab, but modular extensions can rip!”
}
@Code{
type ModExt required inherited provided =
  ∀ self, super : Type
    self ⊂ required self, super ⊂ inherited self ⇒
        self → super → (provided self) ∩ super
}

Notice how the type @c{self} of the module context
is @emph{recursively} constrained by @c{self ⊂ required self}),
whereas the type @c{super} of the value in focus being extended
is constrained by @c{super ⊂ inherited self},
and the returning a value is of type @c{provided self ∩ super},
and there is no direct recursion there
(but there can be indirectly if the focus is itself referenced via self somehow).
Those familiar with universal quantifiers may also notice how
the quantification of @c{self} and @c{super},
in absence of reflective capabilities, pretty much forces those functions
to be well-behaved with respect to gracefully passing through
any call to a method they do not define, override or otherwise handle.
Finally, notice how, as with the simpler NNOOTT variant above,
the types @c{self} and @c{referenced self} refer to the module context,
whereas the types @c{super} and @c{provided self} refer to some value in focus,
that isn’t at all the same as the module context
for open modular extensions in general.

My two OO primitives then have the following type:
@Code{
fix : ∀ required, inherited, provided : Type → Type, ∀ self, top : Type,
      self = inherited self ∩ provided self,
      self ⊂ required self,
      top ⊂ inherited self ⇒
        top → ModExt required inherited provided → self
mix : ModExt r1 i1∩d2 p1 → ModExt r2 i2 p2 → ModExt r1∩r2 i1∩i2 p1∩p2
}

In the @c{fix} function, I implicitly define a fixpoint @c{self}
via suitable recursive subtyping constraints.
I could instead make the last constraint a definition
@c{self = Y (inherited ∩ provided)}
and check the two subtyping constraints about @c{top} and @c{referenced}.
As for the type of @c{mix}, though it looks identical with @c{ModExt}
as the NNOOTT type previously defined with @c{NModExt},
there is an important but subtle difference:
with @c{ModExt}, the arguments being intersected
are not of kind @c{Type} as with @c{NModExt},
but @c{Type → Type}, where
given two parametric types @c{f} and @c{g},
the intersection @c{f∩g} is defined by @c{(f∩g)(x) = f(x)∩g(x)}.
Indeed, the intersection operation is defined polymorphically, and
in a mutually recursive way for types, functions over types, etc.

@subsection{Typing Advantages of OO as Modular Extensions}

By defining OO in terms of the λ-calculus, indeed in two definitions @c{mix} and @c{fix},
I can do away with the vast complexity of “object calculi” of the 1990s,
@; TODO cite Cardelli, Fisher, Bruce, Pierce, etc.
and use regular, familiar and well-understood concepts of functional programming such as
subtyping, bounded parametric types, fixpoints, existential types, etc.
No more @c{self} or @c{MyType} “pseudo-variables” with complex “matching” rules,
just regular variables @c{self} or @c{MyType} or however the user wants to name them,
that follow regular semantics, as part of regular λ-terms@xnote["."]{
  Syntactic sugar may of course be provided for optional use,
  that can automatically be macro-expanded away through local expansion only;
  but the ability to think directly in terms of the expanded term,
  with simple familiar universal logic constructs that are not ad hoc but from first principles,
  is most invaluable.
}
OO can be defined and studied without the need for ad hoc OO-specific magic,
making explanations readily accessible to the public.
Indeed, defining OO types in term of LaTeX deduction rules for ad hoc OO primitives
is just programming in an informal, bug-ridden metalanguage that few are familiar with,
with no tooling, no documentation, no tests,
no actual implementation,
and indeed no agreed upon syntax much less semantics@xnote["…"]{
  See Guy Steele’s keynote “It’s Time for a New Old Language”
  at the Principles and Practice of Parallel Programming 2017 conference
  about the “Computer Science Metanotation” found in scientific publications.
}
the very opposite of the formality the authors affect.

Not having OO-specific magic also means that when I add features to OO,
as I will demonstrate in the rest of this book, such as single or multiple inheritance,
method combinations, multiple dispatch, etc.,
I don’t have to update the language
to use increasingly more complex primitives for declaration and use of prototypes or classes.
By contrast, the “ad hoc logic” approach grows in complexity so fast
that authors soon may have to stop adding features
or find themselves incapable of reasoning about the result
because the rules for those “primitives” boggle the mind@xnote["."]{
  Authors of ad hoc OO logic primitives also soon find themselves
  incapable of fitting a complete specification within the limits of a conference paper,
  what’s more with intelligible explanations, what’s more in a way that anyone will read.
  The approach is too limited to deal even with the features 1979 OO @~cite{Cannon1979},
  much less those of more modern systems.
  Meanwhile, readers and users (if any) of systems described with ad hoc primitives
  have to completely retool their in-brain model at every small change of feature,
  or introduce misunderstandings and bugs,
  instead of being able to follow a solidly known logic that doesn’t vary with features.
}
Instead, I can let logic and typing rules be as simple as possible,
yet construct my object features to be as sophisticated as I want,
without a gap in reasoning ability, or inconsistency in the primitives.

My encoding of OO in terms of “modular extension”, functions of the form
@c{mySpec (self : Context, super : Focus) : Focus}, where in the general “open” case,
the value under @c{Focus} is different from the @c{Context}, is also very versatile
by comparison to other encodings, that are typically quite rigid, specialized for classes,
and unable to deal with OO features and extensions.
Beyond closed specifications for classes, or for more general prototypes,
my @c{ModExt} type can scale down to open specifications for individual methods,
or for submethods that partake in method combination;
it can scale up to open specifications for groups of mutually defined or nested classes or prototypes,
all the way to open or closed specifications for entire ecosystems.

More importantly, my general notion of “modular extension”
opens an entire universe of algebraically well-behaved composability
in the spectrum from method to ecosystem;
the way that submethods are grouped into methods, methods into prototypes,
prototypes into classes, classes into libraries, libraries into ecosystems, etc.,
can follow arbitrary organizational patterns largely orthogonal to OO,
that will be shaped the evolving needs of the programmers,
yet will at all times benefit from the modularity and extensibility of OO.

OO can be one simple feature orthogonal to many other features
(products and sums, scoping, etc.), thereby achieving @emph{reasonability}, @; TODO cite
i.e. one may easily reason about OO programs this way.
Instead, too many languages make “classes” into a be-all, end-all ball of mud of
more features than can fit in anyone’s head, interacting in sometimes unpredictable ways,
thereby making it practically impossible to reason about them,
as is the case in languages like C++, Java, C#, etc.

@subsection{Typing First-Class OO}

I am aiming at understanding OO as @emph{first-class} modular extensibility,
so I need to identify what kind of types are suitable for that.
The hard part is to type @emph{classes}, and more generally specifications
wherein the type of the target recursively refers to itself
through the open recursion on the module context.

Happily, my construction neatly factors the problem of OO
into two related but mostly independent parts:
first, understanding the target, and second, understanding its instantiation via fixpoint.

I already discussed in @secref{RCOO}
how a class is a prototype for a type descriptor:
the target is a record that describes one type and a set of associated functions.
The type is described as a table of field descriptors
(assuming it’s a record type;
or a list of variants for a tagged union, if such things are supported; etc.),
a table of static methods, a table of object methods,
possibly a table of constructors separate from static methods, etc.
A type descriptor enables client software to be coded against its interface,
i.e. being able to use the underlying data structures and algorithms
without having to know the details and internals.

A first-class type descriptor is a record whose type is existentially quantified:
@~cite{cardelli1986understanding mitchell1988abstract PT1993STTFOOP}
@; TODO cite harper1994modules remy1994mlart
as per the Curry–Howard correspondence, it is a witness of the proposition according to which
“there is a type @c{T} that has this interface”, where the interface may include field getters
(functions of type @c{T → F} for some field value @c{F}),
some field setters (functions of type @c{T → F → T} for a pure linear representation,
or @c{T → F @(⇝) 1} if I denote by @c{@(⇝)} a “function” with side-effects),
binary tests (functions of type @c{T → T → 2}),
binary operations (functions of type @c{T → T → T}),
constructors for @c{T} (functions that create a new value of type @c{T},
at least some of which without access to a previous value of type @c{T}),
but more generally any number of functions, including higher-order functions,
that may include the type @c{T} zero, one or arbitrarily many times
in any position “positive” or “negative”, etc.
So far, this is the kind of thing you could write with first-class ML modules, @; TODO cite
which embodies first-class modularity, but not modular extensibility.

Now, if the typesystem includes subtypes, extensible records, and
fixpoints involving open recursion,
e.g. based on recursively constrained types @~cite{isoop1995 iloop1995}, then
those first-class module values can be the targets of modular extensions.
@;{TODO @~cite{remy1994mlart} ?}
And there we have first-class OO capable of expressing classes.

Regarding subtyping, however, note that when modeling a class as a type descriptor,
not only is it absolutely not required that a subclass’s target should be
a subtype of its superclass’s target (which would be the NNOOTT above),
but it is not required either that a subclass’s specification should be
a subtype of its superclass’s specification.
Indeed, adding new variants to a sum type, which makes the extended type a supertype of the previous,
is just as important as adding fields to a product type (or specializing its fields),
which makes the extended type a subtype of the previous.
Typical uses include extending a language grammar (as in @citet{garrigue2000code}),
defining new error cases, specializing the API of some reified protocol, etc.
In most statically typed OO languages, that historically mandate the subclass specification type
to be a subtype of its superclass specification types, programmers work around this limitation
by defining many subclasses of each class, one for each of the actual cases of an implicit variant;
but this coping strategy requires defining a lot of subclasses,
makes it hard to track whether all cases have been processed;
essentially, the case analysis of the sum type is being dynamically rather than statically typed.

@Paragraph{Note on Types for Second-Class Class OO}
Types for second-class classes can be easily deduced
from types for first-class classes:
A second-class class is “just” a first-class class that happens
to be statically known as a compile-time constant, rather than a runtime variable.
The existential quantifications of first-class OO and their variable runtime witnesses
become unique constant compile-time witnesses,
whether global or, for nested classes, scoped.
This enables many simplifications and optimizations,
such as lambda-lifting (making all classes global objects, modulo class parameters),
and monomorphization (statically inlining each among the finite number of cases
of compile-time constant parameters to the class),
and inlining globally constant classes away.

However, you cannot at all deduce types for first-class classes from
types for second-class classes:
you cannot uninline constants, cannot unmake simplifying assumptions,
cannot generalize from a compile-time constant to a runtime variable.
The variable behavior essentially requires generating code
that you wouldn’t have to generate for a constant.
That is why typing first-class prototypes is more general and more useful
than only typing second-class classes;
and though it may be harder in a way, involving more elaborate logic,
it can also be simpler in other ways, involving more uniform concepts.

@subsection{First-Class OO Beyond Classes}

My approach to OO can vastly simplify types for it, because it explicitly decouples
concepts that previously people implicitly conflated:
not only specifications and their targets,
but also modularity and extensibility,
fixpoints and types.

By decoupling specifications and targets, I can type them separately, subtype them separately,
not have to deal with the extreme complexity of
the vain quest of trying to type and subtype them together.

By decoupling modularity and extensibility, I can type not just closed specifications,
but also open specifications, which makes everything so much simpler,
more composable and decomposable.
Individual class, object, method, sub-method specifications, etc.,
can be typed with some variant of the @c{C → V → V} pattern,
composed, assembled in products or co-products, etc.,
with no coupling making the unit of specification the same as the unit of fixpointing.

Finally, with typeclass-style (as in @secref{Class_style_vs_Typeclass_style}),
the unit of fixpointing need not be a type descriptor;
it could be a value without an existential type, or a descriptor for multiple existential types;
it could be a descriptor not just for a finite set of types,
but even an infinite family of types, etc.
In an extreme case, it can even be not just a type descriptor,
but the entire ecosystem — a pattern actively used
in Jsonnet or Nix (though without formal types).

To build such records, one may in turn make them the targets of modular extensions,
such that first-class OO can express much more than mere “classes”,
especially so than second-class classes of traditional Class OO.
First-class OO can directly express sets of cooperating values, types and algorithms
parameterized by other values, types and algorithms.

@;{TODO
@subsection{More Static Typing for OO}
XXX TODO integrate citations to the below and more

Pierce @~cite{PT1993STTFOOP Pierce2002TAPL} at times manages
to push the difficulty with fixpoints and recursion
into a small corner wherein they let the user manually tie a simple knot,
and their automation can take it from there;
it’s brilliant and even insightful, yet in the end they are dodging
the hard problem behind OO, rather than solving it—as in the classic joke:
“Tell me everything you need, and I’ll show you how to do without it.”

Oliveira @~cite{MonadsMixins}
shows that there is enough subtyping in Haskell typeclasses
for cool applications of Mixins, though he stays short of the type astronautics
needed to enable the structural subtyping of extensible records of colloquial OO.

Special mention for the magical Oleg @~cite{Kiselyov2005HaskellOOS}
who shows that, actually, there is enough structural subtyping hidden in the Haskell typesystem
to do precisely the kind of things people usually do with OO,
with lists of types at the type-level to handle extensible record types.
Though Oleg does not bother to implement multiple inheritance,
some have implemented Quicksort at the type-level in Haskell,
so topological sort with C3 is probably just some more type astronautics away.
As compared to most OO papers that discuss theory only, or push their own ad hoc system,
Oleg discusses pragmatics, and reuses someone else’s language, without having to change it,
all by programming at the type-level.
On the other hand, Oleg is well versed in the theory precisely because he does his practical work
decades after the theorists wrote their papers.

Wegner and Cardelli 1985 @~cite{cardelli1986understanding}. NNOOTT.

Cardelli 1988. NNOOTT.

OCaml 199x. @~cite{remy1994mlart}

Scala DOT 200x. <= doesn't do inheritance.
Ask nada.

Fortress 2011

Typescript
https://www.typescriptlang.org/docs/handbook/utility-types.html

Type-Safe Prototype-Based Component Evolution" (2002)
https://www.cs.cornell.edu/andru/cs711/2002fa/reading/zenger02typesafe.pdf

Ego
https://www.cs.cmu.edu/~aldrich/ego/

Bad(?): NOOP Robert Cartwright & Moez Abdelgawad
https://www.semanticscholar.org/reader/3855d0beac44b1623731bf581f80ec4d348eb4ba

https://counterexamples.org/subtyping-vs-inheritance.html

Andrew K. Wright & Robert Cartwright
"A practical soft type system for Scheme"
1997

TODO

Why do "unary methods" work in class OO, but not e.g. binary methods?
Because you moved construction / destruction out of the way,
so all you’re doing is consuming data,
in a way that (as far as types are concerned) is extensible.
(if considered not trivially returning unit, but effectful with linear resources to forcibly manage).
Also, when an object of same type is linearly returned,
there is one obvious place from which to copy the extended rest of the object;
when multiple objects are returned... that is still a possible interpretation
(and though that’s seldom the useful one, that’s enough for the type theorist).

Kim Bruce, @;CITE 1993 1994 1995
thereafter gave sounder types to OO,
saving subtyping where it could be saved because the NNOOTT applies, @; CITE PolyTOIL
but otherwise abandoning subtyping as a goal. @; CITE LOOM 1997
@; Kathleen Fisher @;CITE ...

Meanwhile, the relationship between a module context and a focused value being
modularly and extensibly specified within it is characterized by
a @emph{lens} @~cite{Foster2007CombinatorsFB},
generalizing a path of identifiers to some arbitrary way of accessing a subcomputation.

Classes as object-generator see Cook87, A self-ish model of inheritance
or Cook89a A Denotational Semantics of Inheritance, thesis

What makes Typing OO so complex is the confusion of specification and target.
People are trying to give types to an entity that is the fruit of a fixpoint,
and also retroactively undo the fixpoint to somehow type what was before.
Some superbright people manage to juggle the immense complexity of the endeavor
(by actually remembering the operator before fixpoint, of course), and
proudly show their superdupercomplex calculi as if they’ve solved the problem of semantics for OO.
The real solution is to reject complexity, just unbundled specification and target,
and it all becomes the trivial matter of lots simple regular algebraic operations
before a well-known general-purpose fixpoint.
}

@section[#:tag "SOO"]{Stateful OO}

@subsection{Mutability of Fields as Orthogonal to OO}

I showed how OO is best explained in terms of pure lazy functional programming,
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
but simplified (yet still very complex) models@xnote["."]{
  The concept of class is the “Katamari” of semantics:
  just like in the 2004 game “Katamari Damacy”,
  it is an initially tiny ball that indiscriminately clumps together with everything on its path,
  growing into a Big Ball of Mud @~cite{Foote1997BBoM},
  then a giant chaotic mish mash of miscellaneous protruding features,
  until it is so large that it collapses under its own weight to become a star—and,
  eventually, a black hole into which all reasonable meaning disappears never to reappear again.
  Languages like C++, Java, C#, Scala, have a concept of class so complex that it boggles the mind,
  and it keeps getting more complex with each release.
  “C++, like Perl, is a Swiss-Army chainsaw of a programming language.
  But all the blades are permanently stuck half-open while running full throttle.”
  @; Emacs: Kitchen Sink
  I much prefer the opposite approach: have a small basis of orthogonal primitives,
  that follow a few simple rules that can simultaneously fit in a brain,
  out of which to make large assemblies (but no larger than needed),
  from which the emerging meaning is always explainable.
  Certainly, there is a time when the emerging meaning of a large enough assembly
  has a complexity of its own that cannot be reduced,
  but this is @emph{intrinsic} complexity.
  Classes made into humongously complex language primitives
  introduce @emph{extrinsic} complexity, omnipresent unnecessary parasitic interactions—which
  utterly defeats the very purpose for which classes were invented:
  modularity, the ability to clearly think in terms
  of entities that minimally interact with each other.
  If a large utility function or macro must exist to define classes, each use of it
  should clearly reduce to an entity that could have been directly written without the utility,
  though perhaps in a less concise way, but still can be described in terms of simple primitives,
  with no more exhibited affordances than needed.
  Now, at a certain scale, the smaller entities disappear behind the abstraction, and
  those that were large become the building blocks;
  but if they were built with bad primitives, the protruding affordances will generate
  lots of unwanted interactions that break the abstraction,
  causing the semantic plumbing to leak everywhere,
  and offering attack vectors for active enemies to enter between the cracks.
}

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
which happens millions of times slower than computers process data.
Most evaluation of most programs, especially where performance matters,
happens in-between two such code upgrades,
in large spans of time during which the code is constant.
Therefore, the usual semantics that consider inheritance structures as constant
still apply for an overwhelming fraction of the time and an overwhelming fraction of objects,
even in presence of such mutability.
There are indeed critical times when these usual semantics are insufficient,
and the actual semantics must be explained;
but these usual semantics are not rendered irrelevant
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
the present paper is not the right place for such a discussion.
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
for how such code upgrade upgrades interfere with code running in other threads,
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
Languages that assume “purity” or lack of code upgrade, thereby deny these issues,
and leave their users helpless, forced to reinvent entire frameworks of mutation
so they may then live in systems they build on top of these frameworks,
rather than directly in the language that denies the issues.

