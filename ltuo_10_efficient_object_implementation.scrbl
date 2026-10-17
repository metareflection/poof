#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 10)

@title[#:tag "EOI"]{Efficient Object Implementation}
@epigraph{
  The psychological profile [of a programmer] is mostly
  the ability to shift levels of abstraction, from low level to high level.
  To see something in the small and to see something in the large.
  When you’re writing a program, you’re saying, ‘Add one to the counter,’
  but you know why you’re adding one to the counter.
  You can step back and see a picture of the way a process is moving.
  Computer scientists see things simultaneously at the low level and the high level.
    @|#:-"Donald Knuth"|
}
The efficient implementation of object systems, and more broadly,
the efficient implementation of programming languages, of which these systems are but an aspect,
is far too vast a topic for this chapter to cover in detail.
My goal will be to give you a quick overview of this topic,
point at low-hanging fruit, warn about the most common pitfalls,
and convey some underappreciated perspectives from which to think about it.
You won’t have all the solutions after reading this chapter,
but you will hopefully know where and how to look for them.

@section[#:tag "TMoI"]{The Meaning of Implementation}
@epigraph{
  Efficiency is doing things right;
  effectiveness is doing the right things.
    @|#:- "Peter Drucker"|
}
@subsection{Implementing the Right Thing}
First principle: @principle{Efficiently Implementing OO means Implementing OO}.
This means respecting the semantics explored in the previous chapters.
If you sacrifice faithfulness to the semantics for the sake of efficiency,
you haven’t efficiently implemented OO—you have incorrectly implemented it.
If you don’t know where to go, you most likely won’t get there, and
higher speed will only get you nowhere faster.

Now, many tinkerers, while trying to implement OO,
have invented various systems and techniques that did not quite implement OO.
If they had then said
“while trying to implement OO that I don’t understand, I instead stumbled on
that other thing that I don’t understand either but looks very cool,
let’s figure out what’s happening”,
then that would be great, actually:
some of the greatest scientific discoveries arose from curiosity at happy accidents—including OO.

But some tinkerers in effect claimed
“I don’t really understand OO, but this thing is easier for me to implement
so I’m going to pretend it’s the same thing, or good enough,
and those who disagree are losers”,
and that’s not respectful—not of OO, not of other people,
not of the thing they have discovered, not of themselves.

@subsection{Implementing It Right}
As for what it even means for something to be an implementation,
this is another vast topic, but one I happen to have studied in depth,
so I will direct you to my works @~cite{Rideau2018 FarePhD}.

An implementation is a correspondence between two computing systems,
one (relatively more) abstract, being implemented,
the other (relatively more) concrete, doing the implementing.
For the implementation to be valid, and moreover for it to be useful,
this correspondence must satisfy various properties that can be formalized logically.
Most importantly, some (not all) of the concrete states will be “observable”,
and will each have an abstract state that it “implements”.
But most concrete states will not have any abstract state they implement;
they will be “intermediate” concrete states between two observable hyper states,
or plain invalid concrete states that should never be reached by the implementation (if correct).

Thus, the two systems may have very different sets of states and interactions overall,
most of them not being in a correspondence with anything in the other system.
Yet some subset of the concrete system will be in faithful correspondence
with some subset of the abstract system in a way that preserves their interactions.
The challenge is then to create configurations of the concrete system
that faithfully embody a subset of the abstract system’s interactions
large enough to include all the interactions one wants to have with the abstract system.

And that is how I will be able to implement the pure functional lazy objects I want
with the stateful von Neumann-style machines I have.
(Note how in other contexts I might want the implementation the other way around:
for instance, to write formal proofs of correctness about executable code in type theory.)

@subsection{Layers of Implementation}

Here is some vocabulary I coined @~cite{FarePhD}
to precisely name the elements of an implementation.
When considering how one computing system may or may not implement another,
the more abstract system is typically pictured above, and I use the prefix “hyper” for it:
the hyper-system, its hyper-states between which there are hyper-transitions.
The more concrete system is typically pictured below, and I use the prefix “hypo” for it:
the hypo-system, its hypo-states between which there are hypo-transitions.

These terms are relative to a particular implementation:
the very same system can be considered as the hypo-system of one implementation,
and the hyper-system of another. Indeed, decomposing an implementation into
a tower of implementations is how you can manage the complexity of it all:
each implementation introduces or eliminates some concerns,
applies optimizations, etc., such that you can (formally or informally)
reason about the correctness properties of the overall implementation
by reasoning piecewise about the more manageable correctness properties
of the smaller individual implementations.

I will call “layer” an implementation between two computing systems,
seen as part of such a “tower” of implementations.
Contiguous layers compose into a larger implementation.
Each computing system can itself be viewed as a layer—an identity layer
in which every state corresponds to itself. The space of all implementations
is then a category—though you will want to consider not just implementations in general,
but more constrained categories of enriched implementations
that satisfy certain additional properties.

Why spend so much time precisely naming all these concepts?
For two related reasons:
(1) so you can reason about them formally and write proofs about programs and their implementations.
(2) so you don’t confuse them with sometimes conflated concepts that may be related yet different.

What other concepts? Well, later in this chapter,
together with @emph{layers of implementation} (prefixes @emph{hypo-/hyper-})
I’ll consider @emph{stages of computation} (prefixes @emph{pre-/post-}), @; TODO secref
and @emph{planes of control} (prefixes @emph{back-/fore-}), @; TODO secref
that are often conflated under the names or concepts of
“metaprogram”, “metaobject”, “metacircular evaluator”, “reflection”, “representation”, etc.
By having distinct names for these independent concepts,
we can better understand what they do and what they don’t.

@section[#:tag "RR"]{Representing Records}

@subsection{Records Matter}

@subsubsection{As Fundamental as Inheritance}
One way or another, @principle{Object-Orientation involves dealing with @emph{records}},
mappings from identifiers to values.
This is all the more remarkable since the rules of inheritance,
that characterize the semantics of OO as such,
say nothing whatsoever about records,
as I discussed in chapters @seclink["MOO"]{5} to @seclink["IMSMO"]{7}.

Yet, the entire point of OO is to enable modular extensible programming,
and records are the very unit of modularity, as seen in chapter @seclink["OOaIEM"]{4}:
products of (mostly) independent values labeled by meaningful names,
records verily embody the combined efforts of the independent developers
who contribute to some software project.
And inheritance is only the means to that end.
Applying inheritance to values other than records is definitely possible;
but unless those values encode something record-like by other means,
the opportunities for supporting modularity will be very limited.

Thus, a very important issue in implementing object systems is
how to efficiently represent records, from the dual points of view of
how they are produced and how they are consumed.
And the solution would be easy if the two could be neatly staged;
however the open recursion inherent in inheritance means that
the two can be interleaved in arbitrary ways.

@subsubsection{Semantics First}
Now, the original intent of records,
even before they were first formally treated by @citet{Hoare1965},
has always involved an efficient low-level representation as contiguous words of memory.
And most presentations of OO just introduce a low-level encoding
for how their language implements objects
(often using a variant of @seclink["CfUe"]{U-encoding}),
and declare that’s how things are.
Out of that, extremely complex low-level semantics emerge,
and you are left to untangle desired meaning from undesired noise.

My approach in this book is the radical opposite:
I am always putting the desired semantics first.
In previous chapters, I chose to represent records as functions,
which simplified many aspects of my conceptual presentation, including fixpoints and types.
In this chapter, I am intent on connecting this semantics
to a low-level representation as contiguous words of memory.
And to that end, I will start from the same high-level semantics, and
zoom down to practical implementations thereof,
the desired meaning being clear at all times.

@subsection[#:tag "RaF"]{Records as Functions}
@epigraph{
  To have a right to do a thing is not at all the same as to be right in doing it.
  @|#:-"G.K. Chesterton"|
}
@subsubsection{A Universal Strategy}
So far I have encoded records as opaque functions
taking some kind of identifier as input argument (symbols, in Scheme),
and returning an arbitrary value as output (@secref{MOO}).

This record representation strategy works, and
is portable to any language with higher-order functions.
By using it, I have demonstrated a general recipe to implement OO
directly on top of the λ-calculus only,
which can itself be easily implemented on top of
any language or virtual machine past, present or future.
Indeed I have successfully deployed this very technique “in production”
to build a lightweight pure functional HTML authoring tool for my slides;
and anyone could likewise use that technique in a pinch to quickly build
extensible configuration generators, or an extensible data structure library
in an otherwise constrained environment.

Another advantage of this strategy is that it fits nicely with the Y combinator:
the record itself was the fixpoint, its type was that of a regular function.
There was no need for complicated ad hoc wrapping and unwrapping with types
that are never 100% compatible from one language to the next,
nor for language-specific record primitives with ever subtly different types,
the explanations for which would have burdened an already concept-heavy exposition.

@subsubsection{Issues with Records as Arbitrary Functions}
However, there are various drawbacks to representing records as arbitrary functions:
@itemize[
  @item{Arbitrary functions are opaque, lacking introspection on which keys are valid@xnote["."]{
    On the plus side, the opacity of arbitrary functions can be seen as a feature
    in the context of parametric behavior (initial semantics), and/or
    of denying introspective capabilities in some security-sensitive contexts.
  }}
  @item{Everything being opaque functions can make code harder to debug,
        with less distinctive error messages.}
  @item{Records as opaque functions will perform poorly in both time and space.}]

Indeed, if @emph{arbitrary} functions are accepted, then there is no trusting what
is in functions that you extend, no knowing what keys they handle,
no filtering those keys to keep or process those you want and remove those you don’t.
Extending a record-as-arbitrary-function to handle a binding from key to value
necessarily shadows any previous binding without removing it, and thus leaks space;
and the old bindings, though shadowed, will necessarily still be checked,
when doing a linear search through previously extended records-as-arbitrary-functions;
and the shadowed values associated with those keys, though no longer reachable that way,
will still be considered reachable by the garbage collector.

In practice, this representation strategy is best suited only to short-lived programs,
such as a generator for HTML documents or configuration files, as previously discussed,
for which it can help you get the job done quickly anytime anywhere on a small budget,
with no dependencies on libraries of any kind.
Larger or longer-running systems will require a better solution.
Happily, for larger or longer-running systems,
one can afford the STEAM (Skill Time Energy Attention Money)
to invest in a better solution, such as the ones I discuss below.

@subsubsection{Not-so-Arbitrary Functions}
Now, functions do not have to be arbitrary.
They can follow some systematic pattern;
but then that pattern defines the @emph{actual} data structure,
whether the pattern is explicitly coded by the programmer,
or heroically inferred by the implementation, either statically or dynamically.
The function then is but a convenient interface (or not-so-convenient, depending on expectations)
to the underlying functionality,
and could expose more of that functionality.
Now, trying to combine arbitrary functions that may follow incompatible protocols
leads to the same miserable and decidedly not-so-arbitrary data structure as in the section above;
however, not-so-arbitrary functions implementing a well-identified protocol
can implement arbitrary data structures, including whichever best fits your needs—embodying
the principle that closures are a poor man’s objects @~cite{VanStraaten2003}.

For instance, a function could implement the same linear search for a key that I used,
but also maintain a list of bound keys, that is returned if a “magic” value is given as input
(the magic value being otherwise outside the set of keys that can be arbitrarily bound).
Or, assuming a totally ordered set of keys, the function could use
some balanced binary tree underneath instead of a linear search,
and there again provide introspection on the node structure when queried with magic values.
Your imagination is the limit, but notice a few things:
@itemize[
  @item{The existence of magic input values means that what you have indeed is not “a function”
    from arbitrary value to arbitrary value: instead it is a function from the sum of two types
    (regular keys and magic messages) to a sum of two types (regular values and magic answers),
    where the sides of the sum are preserved, which indeed encodes a conflation of two functions,
    each of which may be a “record” of sorts.}
  @item{The first function from regular keys to regular values behaves like
    our records-as-arbitrary-functions above, while the second conflated function,
    from “magic” messages to magic answers, provides some reflective interface
    with which you can probe the implementation underneath the first function.
    Reflection will be the recurring theme of this chapter.}]

At that point, the functional interface only adds some small constant space and time overhead
to whatever data structure is actually used underneath.
The benefits of the functional interface are then the uniformity of its abstraction,
and its direct compatibility with the Y combinator.
Using this technique, introspection of keys is possible (though not necessary if unwanted),
as is “extrospection” (second-class or external knowledge of keys from outside the runtime),
if the data structure is maintained extra-linguistically via a design pattern.
And through this introspection or extrospection,
the space and time leak of shadowed bindings can be avoided.

Now, to go further, I will strip the functional interface, and peer at what is hiding inside it.
Once I do, I may find that a function from key to value was not the best interface,
and not even the best functional interface.
And so I may not want to put that interface back afterwards.
But then, I won’t be able to use the Y combinator the same way I did;
I may be able to use the Y combinator still, through some indirection layer;
or I may be able to achieve the self-recursion intrinsic to objects
through a very different mechanism.
But first, let’s examine what data structure was implicit
in chaining records-as-arbitrary-functions.

@subsection[#:tag "RaFM"]{Records as Finite Maps}

@subsubsection{From Implicit to Explicit Alists}
The only primitive that I could afford given arbitrary functions as records
was the @c{extend-record} primitive that added one binding to an existing record,
ultimately from the @c{empty-record}.
I could use the @c{case} primitive of Scheme to add several bindings at a time,
but that was only a minor optimization that didn’t change much in the end:
a record is a chain of bindings that you consult with a linear search.
And thus the data structure implicit in that choice of constraint is the @emph{alist},
or association list: a singly linked list of pairs of a key and a value.

The alist is a “naïve” data structure, but not in a bad way:
it is quite a simple and obvious data structure to think about, and indeed, reason about;
while not blazing fast, it does not suffer from a hidden flaw that makes it terrible.
That is why the alist is often used as the basis on which to model, formalize, implement and
study the properties of records, “environments” of variable bindings, and finite maps in general.
Underlying the alist, the singly linked list
(called just “list” in Lisp, Scheme, and
the standard library of most functional programming languages)
is also popular, for the same reason, as a representation for
lists, finite sequences, sets, etc.

The alist works with any set of keys that you can compare for equality, and
has acceptable performance for short maps, even though it is not particularly efficient:
every access requires a linear search
in @c{O(n)} time where @c{n} is the length of the list@xnote["."]{
  I will keep all my “performance estimates” as general complexity classes,
  under the simplified model of a flat memory with @c{O(1)} access time.
  Once you delve into details of levels of memory from CPU registers to L1, L2, L3 caches,
  to local disks then remote servers and their disks,
  the cost of accessing memory depends on the size and locality of the working set.
  When the size @c{n} of the working set grows large enough,
  physical limits on information density and signal propagation speed
  introduce a factor @c{Ω(³√n)} on random-access latency
  for three-dimensional storage constrained by materials science,
  which further degrades to @c{Ω(√n)} when gravity kicks in @~cite{Ernerfeldt2014}.
}
Even then, @emph{there is a clear advantage to explicitly using the alist}
over implicitly letting its structure emerge from the use of @c{extend-record}:
by making the alist explicit, one can actually remove shadowed entries,
and eliminate the space and time leak inevitable in the previous representation.

@subsubsection{Finite Maps Beyond Alists}
Stepping back from the particulars of the alist and its performance profile,
one can then realize that the abstract interface it satisfies
is that of a @emph{finite map}.
And indeed, the essence of a first-class record is a finite map:
a map from a finite set of keys to a set of values,
though those keys may be a subset of an infinite type.
Finite maps are also known in various programming contexts as
maps, dictionaries, associative arrays, or associative containers@xnote["."]{
  When, as is the case here, finite maps bind values to @emph{identifiers},
  they are also sometimes known as (lexical) environments.
  And some Lisps have “first-class (lexical) environments” that can indeed nicely reify
  the result of resolving modular definitions.
  But the term “environment” has broader connotations in general,
  and can mean any kind of context for evaluation;
  so I will avoid it in this discussion.
}

Larger finite maps can be more efficiently implemented using balanced binary trees,
which allow access operations in @c{O(log n)} instead of @c{O(n)}.
Comparing identifiers as strings can be somewhat expensive, however, and
it is sometimes faster to compare their unique addresses,
or their hash (which can be cached),
or a unique number assigned to them (guaranteeing no hash collision),
after having @emph{interned} the strings as symbols, as in Lisp or Scheme@xnote[":"]{
  Applications that rely on cryptographically secure hash functions may already
  be “guaranteeing” no collision: more precisely, they make collisions vanishingly unlikely,
  and the entire system’s security is already predicated
  on such collisions not occurring in practice,
  making it pointless to further protect against them.
  On the other hand, such cryptographically secure hash functions are expensive enough to compute
  compared to the “insecure” alternative that they are probably not worth using
  just for the no collision “guarantee”, unless that guarantee is supposed to be truly global,
  across large computer networks.
}
Lisp “symbols” are essentially pre-registered strings, in a global table,
to pre-pay the @c{O(n)} cost of string comparison once (where @c{n} is the length of the string)
and make each subsequent comparison for equality a cheaper @c{O(1)} operation@xnote["."]{
  Since the late 1970s, Lisp, as subsequently codified by Common Lisp,
  actually has a two-level system, with a global table of @emph{packages},
  each named by a string, and containing a table of @emph{symbols}.
  Common Lisp also has a notion of @emph{uninterned symbols} that have a name,
  but are really meant to be compared by address.
  Scheme has a single-level system of symbols;
  some implementations have uninterned symbols, but not all;
  on the other hand, most implementations have a notion of @emph{identifier}
  that is richer than a symbol, embodying some provenance information with the symbol,
  so you can distinguish identifiers with a common name (a symbol) but different lexical identities,
  that serve as keys to different bindings.
  Note that even though the underlying programming language may fail to provide you with
  a small unique id number for each symbol, your object system can do the assigning itself,
  at least for symbols used as field names, at the cost of another interning table
  mapping symbols to such numbers.
}
Particularly popular among pure functional finite maps are tries
and their hash-optimized variants
Hash Array Mapped Tries (HAMTs) @~cite{Okasaki1998 Bagwell2001 Steindorfer2015},
and weight-balanced trees. @;{TODO cite https://github.com/dco-dev/ordered-collections/blob/021-specialized-ropes/doc/why-weight-balanced-trees.md}

@subsubsection{Mutable Records}
Mutable records, or records used linearly and implemented with in-place updates,
can be implemented by mutable finite maps.
A traditional implementation with a mutable hash table provides access in expected @c{O(1)}
(compared to @c{O(log n)} for balanced-tree finite maps),
albeit with a constant factor that is typically one or two orders of magnitude larger
than a direct field access in a statically typed language.
My above remarks about hashing, interning, and unique numbering apply
to mutable hash tables as well as HAMTs.

@subsection{What One Knows When}

@subsubsection{A Note on Power}
@epigraph{
                Every task involves constraint, @(linebreak)
              Solve the thing without complaint; @(linebreak)
                There are magic links and chains @(linebreak)
               Forged to loose our rigid brains. @(linebreak)
           Structures, strictures, though they bind, @(linebreak)
                 Strangely liberate the mind.
                 @|#:-"James Falen"| @; TODO get permission, or only include the last 2 verses
}
One might find it remarkable that,
despite granting users the power to hide arbitrarily sophisticated
record implementation algorithms behind the interface of a function,
the records-as-arbitrary-functions strategy led to a data structure
that is objectively @emph{worse} than the obvious naïve solution.

Actually, that power is the very reason why
the @c{extend-record} function for records-as-functions is so powerless:
The programmer can exercise the power of an arbitrary function,
but once and only once, for some base record.
Thereafter, the algorithms that modify that record cannot assume anything
about the internals of the record being extended,
and are thus maximally restricted in how they may extend such records;
all they can do is chain into the opaque record.
You win big, once, at the beginning;
afterwards you lose big on every subsequent operation,
which is where the system actually spends most of its time and space.

The more general lesson is that strictures on data structures enable better performance.
More precisely, @principle{strictures on what some operation consumes
create a space in which past operations are free to be more efficient in what they produce;
and strictures on what the operation produces
create a space in which future operations are free to be more efficient in what they consume}.
From this dual constraint emerge various tradeoffs between strictures and freedoms.

An operation doesn’t have to compute the outputs it knows won’t be used;
it can efficiently access inputs that it knows were correctly encoded at a convenient place;
wrappings and unwrappings can be cancelled away; sanity checks can be skipped.
Each constraint makes the constrained operation’s job slightly harder,
but its upstream and downstream operations’ jobs slightly easier.

Now, in general, the very same operations, implemented by the very same algorithms,
will be used in both the past and future of a data structure, in an often unpredictable way.
Therefore, a balancing act is required from the programmer, to choose the compromises
that will maximize the benefits and minimize the cost of the software over its expected lifespan.
Wisely chosen constraints can overall smooth out the flow of information;
foolishly chosen constraints can overall strangle it.

@subsubsection{Staging the Computation}

The flow of information in computations is not uniformly random through time,
but itself follows larger structures:
past and future are not wholly unpredictable, and there are often clear-cut
or at least statistically expected @emph{stages} in the computation,
during each of which the nature and frequency of operations used are different,
such that each stage would best benefit from its own specific choice of tradeoffs
that would not work well in other stages.
A translation between different representations
at the boundary between some or all of these stages
may then take advantage of this discrepancy.

Obvious stages of computation are compile-time and runtime,
in language implementations that involve a compiler.
But stages need not be so sharply and uniformly defined over the entire program.
Even within one function call there can be many stages of computation:
many algorithms involve setup and/or teardown stages whose purpose is
just such a change of representation,
from whatever general-purpose representation the inputs are in,
to something that simplifies the algorithm,
back to a general-purpose representation of the outputs after processing.
And these stages of computation within a procedure can often be the opportunities
for optimizations, whether manually added by a programmer, or
automatically detected by the implementation, or anything in-between.

When some clear event separates two stages of computation,
I will use the prefix @emph{pre-} to denote
the computation that happens before the event, and its attributes,
and the prefix @emph{post-} to denote those after the event.
This chapter is about efficient implementation;
therefore I will focus on those pre-computations that can be used
to efficiently implement post-computations,
in particular those that generate a hypo-computation of a post-computation
(in lay terms, an implementation of it)—as when running a compiler.
But mind that in the general case, the two axes hypo/hyper and pre/post are independent@xnote["."]{
  A pre-computation does not have to generate a hypo-computation.
  Indeed, it could generate data at the same level of abstraction, to be used as regular input.
  It could even be generating a computation at a higher rather than lower level of abstraction,
  a hyper-computation of the post-computation:
  a type or resource analysis, a permission check, a decompilation.
  It might be a useful computation that helps the post-computation,
  yet is itself definitely not a hypo-computation of it,
  such as the identifier interning discussed above or the perfect hashing discussed below.
  Or it might be doing something related to the post-computation,
  yet largely independent from its computational aspect:
  logging, bookkeeping, content-addressing, statistical analysis, sampling, access control, etc.
  Of course, if a pre-computation is totally unrelated to a post-computation,
  and the two do not interfere in any way, then no one will bother to consider them together.

  Conversely, a hypo- and a hyper-computation can be happening
  at the same stage, one being just a view of the other.
  Already in the examples given above,
  a hyper-computation could be computed by a pre-computation as well as a hypo-computation.
  They can also be computed by a post-computation:
  a hyper-computation can be generated after the fact as summary
  for resource accounting or security reporting;
  or a hypo-computation can be generated after the fact for debugging
  (e.g. time-travel debugging of a failed deterministic computation).

  Finally be mindful that just as with @emph{hypo-} and @emph{hyper-},
  the prefixes @emph{pre-} and @emph{post-} are not an absolute property of a computation,
  but an attribute of its role in a staging relation with another computation.
}

A common pattern will be that during a pre-stage S0,
some information about a post-stage S2 is @emph{static}, i.e. known in advance,
as opposed to @emph{dynamic}, i.e. not known in advance.
For instance, a variable @c{v} might hold a constant value @c{c} during S2.
Then, during S0, a pre-computation may prepare the code meant to run during S2
to take advantage of that information,
e.g. by replacing variable operations using @c{v}
by cheaper constant operations using @c{c},
and by replacing pure computations on known constants by their results
(a.k.a. constant folding).

Some optimizations may involve more than two stages.
Maybe at pre-stage S0 you already know that some variable won’t change during post-stage S2,
but the exact value won’t be determined until intermediate stage S1.
At stage S0, you may prepare for variables to be @emph{invariant} during stage S2,
which you statically know to be the case, and apply many optimizations,
even though the future constant value itself is still dynamic at this point:
you might be able to take shortcuts, eliminate dead code, remove unnecessary checks,
use preallocated buffers, etc., based on the partial knowledge of that future invariant.

Beware though that each stage also has a finite lifespan.
The optimized stage S2 may itself have an end,
after which the static information becomes invalid,
and code and data may not assume a shape optimized for this static information anymore.
The end can itself be statically known (e.g. the termination of a subroutine),
or dynamically determined, by checking a guard condition to confirm the assumptions made,
and falling back to an unoptimized code path when the guard fails.
Often, the stage S2 is ephemeral enough
that the cost of some further optimization or a guard would not be covered
by the expected gains in performance—at which point the optimization would be a bad trade,
and it is best to keep using a more general-purpose strategy@xnote["."]{
  @citet{Dybvig2006} explains how while developing Chez Scheme,
  his benchmark for whether to accept an optimization was
  whether applying it to the compiler improved the speed of the compiler enough
  to repay the cost of performing the optimization.
}

@subsection[#:tag "RaR"]{Records as Records}

@subsubsection{Class Disparities}

Now here comes a disconnect: I have established that
the semantics of first-class records is essentially that of a finite map,
with a performance profile much worse than what everyone expects of a record.
Indeed, the name “record” itself, as per @citet{Hoare1965},
suggests a low-level representation of data in terms of consecutive words of memory:
Accessing a field should be just a matter of reading memory at a fixed offset from
the start of the “record” as a contiguous array of memory.
Indeed, I used the title “Records as Records” for its cognitive dissonance,
but what I mean and most readers will understand (albeit not necessarily consciously so)
is actually “Records as Contiguous Memory Arrays”.
Why isn’t that the conclusion of the discussion so far?
I see what my neighbor has—fast record field access—and I want the same.

The key to this discrepancy is, of course,
the difference between first-class records and second-class records:
when compiling access to second-class records of known type or “shape”
through fields labeled by second-class identifiers,
a static analysis can assign a fixed offset to each of these accesses—at
a previous stage, typically compile-time.
But when implementing first-class records,
you must be prepared for newly defined records,
with newly defined sets of identifiers as keys,
accessed through fields labeled by dynamically computed first-class identifiers.

Thinking back about it, it wouldn’t make sense
to implement first-class records with first-class records.
At best, that would be a tautological identity, bringing nothing;
or it would be a level of gratuitous wrapping and indirection, bringing only overhead.
What it cannot bring is simplification or improvement.
Now, implementing first-class records with second-class records
together with a first-class descriptor,
then implementing the second-class records with
the memory arrays that lie underneath—@emph{that} does make sense.
Second-class records then are indeed a case of the staging discussed above, where
computations before and after the stage transition (in this case, compilation)
can use different implementation strategies, yielding various optimizations.

@subsubsection{From Each According to Its Shape}

The static knowledge that makes second-class records reasonably fast is
knowledge of their shape, i.e. of the mapping from identifier to offset,
together with the knowledge of the identifier.
Combined, they lead to the offset being static—a
special case of constant-folding where the operation is table lookup,
and the constants are the table (shape) and the key (identifier).

When using dynamic lookup, often the identifier is still static, but the shape is not;
sometimes both are dynamic.
Either way, a good heuristic is that recently encountered combinations will likely recur,
especially in repetitive loops for which performance matters;
you may thus cache recent lookups and usually recover the value you seek
with a cheap computation.
The cache can be global based on a hash of the (table, key) data as in @citet{Goldberg1983},
or it could be a simple per-lookup-site cache (or per-identifier cache)
with a single entry as in the inline caches of @citet{Deutsch1984}
or multiple entries as in the polymorphic inline caches of @citet{Hoelzle1991}.
If using a hash, combining the table address with a random-looking salt derived from the field,
multiplying the result by a suitable odd constant, then taking high bits as an index into the cache
can be very fast on modern hardware. @; ((shape_address XOR field_salt) * C) >> shift
The salt can be precomputed by hashing the field name, or using a bijective mixing function
from a unique number assigned to each interned identifier.

An alternative approach is to ensure efficient lookup
by consulting a table associated to the shape,
rather than to the field or combined shape+field.
This table can typically be found from the record
with one or two indirections.
There again, a very simple hash function could transform the field salt
to collision-free offsets into a hash-table containing the desired values...
or, in the case of shapes, maybe even directly into the desired offset values,
if you’re free to shuffle them and ready to pay for space overhead.
(Either way, if you do not have a static guarantee that the shape being looked up
contains a field with the given identifier, you will also have to include
a dynamic check that your offset indeed corresponds to what you’re looking for.)
This design is called “perfect hashing” @~cite{Fredman1984}. @;{ TODO cite
  Tarjan1979 "Storing a Sparse Table"?
  Driesen1995
  Ducournau2008
  Ducournau2011Hashing
  Ducournau2011Implementing }
Finding the perfect hash function is usually a matter of trying a lot of parameters
within a family of functions of a certain simple shape,
until satisfactory parameters are found;
a fallback to a more generic representation is used
if no good solution is found within the search budget@xnote["."]{
  Most record shapes are relatively small, in which case finding suitable parameters
  can be very fast. For larger shapes, finding the parameters can take time,
  but much less so if you agree to make the hash less perfect and
  trade more space in the descriptor.
  How many resources to expend into finding better hash functions
  depends on how much and how long you expect the shape tables to be used;
  if the shapes are stable across many compilation attempts, many machines, etc.,
  then search results can be cached, and randomized attempts at improving still-imperfect results
  can be shared over the network, so that performance increases with time—at the expense of
  compilation not being deterministic.
  The pre-stage of computation for some optimizations has then been distributed.
  Super-optimization tables can be viewed the same way. @; TODO cite

  Also note that which of field-first or shape-first lookup approach to pick
  can depend on the expected costs and benefits of each, determined while compiling,
  or from runtime measurements on the current machine, during benchmarking,
  or from measurements gathered in production. Representation choices
  do not have to be made rigidly in advance according to blind strategies.
}

When using caches, you may have to guard the use of cached values with a check that
the cache entry actually corresponds to the lookup keys you are using.
You can view cache lookup as a speculative guess that you’re in that post-stage
where some optimized fast path applies, and cache writeback as the pre-stage
for a similar post-stage in the future, while double-checking the entry
is the guard for the validity of that post-stage.
In this broad sense, all temporal dependencies in data or code representation
are cases of staging.

In the end, @principle{an efficient implementation for first-class records is essentially
a second-class record plus a first-class record descriptor}
through which the mapping of identifier to offset can be efficiently recovered in the dynamic cases.

@subsubsection{The General vs The Common}

The @emph{general} case of first-class records requires the full functionality of finite maps,
whose general operations are necessarily more expensive
than constant-offset access to second-class records.
But the @emph{common} case of first-class records is about the same as for second-class records:
most of the time, the shape of the record can be known or predicted in advance,
and so can the identifiers used to label the fields being accessed;
then a fixed offset can be computed in advance or cached, or
a likely shape can be speculatively assumed under a guard.

@principle{An implementation must always support the general case} of what it is implementing,
and be ready to fall back to less efficient but more general algorithms.
@principle{But an implementation can rely on the common case}.
And in the words of Butler Lampson: @principle{Handle normal and worst case separately}
@~cite{Lampson1983}.
Use a more efficient algorithm @emph{most of the time},
being careful to guards its use with checks that the preconditions
for the validity of the more efficient algorithm still hold.
Those checks can often be moved outside of performance-critical loops.
(Indeed in the case of second-class records themselves,
the checks can be moved wholly from runtime to compile-time.)

These common-case optimized representations target reads or writes of predictable fields
in records of known or at least stable shape.
An operation like extending a record with new fields, or removing fields from a record,
is actually made @emph{slower} by these representations,
because now you may need to construct or locate a new shape descriptor,
perhaps through a global weak registry,
and reshuffle the contents of the extended records to fit its new unrelated layout.
If your set of fields is very dynamic, you may want an alist (if short)
or a regular balanced tree (if pure) or a hash-table (if stateful).
On the other hand, if you’re only or mostly using second-class Class OO
(or some type system that ensures all record shapes are static),
then you never remove fields, and adding fields only happens
when you define new classes (or more generally write new code),
which is rare enough indeed.

With enough static information, optimization also can go beyond the common case:
if a record is a known constant, and the field with which to access it is also a known constant,
then the result of accessing the record is a known constant,
and the access can be constant-folded into directly returning that constant result.
In a lazy language, you can also fold away a record access returning not a constant result
so much as a known shared lazy computation (from an eager point of view: a constant pointer
to a lazy computation record with non-constant contents). That’s a very special case,
not the common case and even less so the general case; yet the case is common enough
that it is worth supporting in an optimizer.

@subsubsection{The Means of Record Production}
@; TODO remove this section unless and until I provide code?
@; TODO move this section to discussion of responsibilities in enforcing representation invariants?

Any host environment you choose for your implementation
will likely offer some kind of underlying low-level memory array or vector of nodes,
that can be used to implement a graph of records and other @emph{objects}—in
the 1950s sense of entities each occupying a contiguous chunk of memory.
However every such host language will offer subtly different primitives
that you then have to deal with:
offsets counted from 0 or 1 or some other constant due to reserved fields;
word-addressing or byte-addressing with various alignment constraints;
static or dynamic typing of the data being stored;
varying support for garbage collection;
issues regarding coherence or atomicity of access across threads;
etc.

In a practical implementation, you will seek the most performant primitive available to you.
In Scheme, which I use in this book,
some implementations offer suitable primitives, but nothing portable is defined.
For instance, Gambit Scheme exposes the internals of its “structure” machinery
with operators such as @c{##make-structure}, @c{##structure-ref} and @c{##structure-set!};
and Gerbil Scheme builds its object system on top of those.
But the closest you can get portably in Scheme is
to hijack vectors, the standard mechanism for one-dimensional arrays of arbitrary values,
as the underlying representation.
That is what I will do in the code accompanying this book.

I will not try to maintain “full abstraction” by
redefining user-level vectors and all their primitives so that
user-level vectors form a subset of system-level vectors
disjoint from those I use to represent objects.
I will leave that as an exercise to the reader.
Presumably this problem disappears in the common case where compilation
cleanly separates user-level objects and vectors as observed by the source language
from the underlying system-level representations
as seen by lower-level implementation primitives at a later stage.

Happily, managing the details of translating a high-level representation strategy
into a zoo of low-level primitives that change subtly with each host system
is a good task for modern AI to semi-automate.

@section{Representing Recursion}

@subsection{Where did the Fixpoint Go?}

@subsubsection{No Place for Fixpoints}
Just as essential to OO as records is the open recursion through fixpoints from inheritance.
Now, whichever specific data structure is used underneath to represent a finite map,
that data structure is, importantly, an “inert” @emph{value}:
looking up bindings can be done in a “pure” way involving no meaningful side effect,
and even adding, shadowing or removing bindings involves no side effects
except those specific to the data structure if mutable (and not even that if immutable).
In particular, there is no place in such a data structure for an arbitrary
@emph{computation} that a fixpoint may be part of.
The recursion through which modular extensions and OO in general are defined and resolved
must happen @emph{outside} of the data structure itself.

This is very unlike the case of records-as-arbitrary-functions that I was using previously:
calling the function could do more than consult a fixed list of bindings—it
could evaluate arbitrary computations (and, in Scheme, issue arbitrary side effects).
Indeed, I specifically relied on some arbitrary computation happening when looking up bindings
to make that computation itself the fixpoint of a modular definition.
My computations would recursively refer to the @c{self} variable,
consult it for other bindings, store it in data structures,
compute along chains of inherited @c{super} methods
that could themselves recurse through @c{self} some more.

But now that the bindings themselves are organized as an “inert” data structure,
consulted through well-defined algorithms with no space for arbitrary computations,
the fixpoint will have to happen @emph{before} or @emph{after} the data structure is consulted,
depending on whether one uses Y-encoding (@secref{MOO}) or U-encoding (@secref{CfUe}), respectively:
@itemize[
  @item{When using Y-encoding,
    the recursion knot is necessarily tied @emph{before}
    the data structure is built:
    binding a @c{self} variable as part of the fixpoint operation
    must happen before references to that self may be used
    in initializing fields of the data structure.
    Some of these references may be part of lazy computations that only fully unravel
    after the data structure is returned and used;
    but the crucial binding happens before.}
  @item{When using U-encoding, the whole point is that the recursion knot happens @emph{after}
    the data structure is built, when a caller invokes a method using the self-application
    operator @c{U} or an argument-swapping variant thereof—which corresponds to
    the outer “@c{U.}” to the left of @c{Y = U.(.U)}.
    The generator is composed to the right with @c{U}—which corresponds to
    the inner “@c{.U}” to the right—before returning the data structure,
    but that is deferred self-application, and does not involve any fixpoint yet.}]

@subsubsection{Fixpoints are for Computations, not Values}
Either way, as mentioned in @secref{DLCCF},
fixpoints, and thus resolution of modular definitions and of modular extensions,
are essentially operations on @emph{computations}, and not quite on @emph{values}.
The distinction between computations and values, and their duality,
is well explained by @citet{Levy1999}: @; TODO read and maybe cite Curien2000 Herbelin2007
computations are active processes that may return a value of corresponding type,
while values are just inert results.
Now, values are naturally embedded in the universe of computations, wherein to each value
corresponds a simple “pure” computation that just returns the value.
But there are many ways to embed computations in the universe of values,
none of them more “natural” than the others, though each programming language may make one
cheaper to express:
@itemize[
  @item{Computations as thunks—functions from unit to the desired value type,
    with some set of acceptable side effects.}
  @item{Computations as delayed values—about the same as thunks, but
    the delayed value must be “forced” instead of a thunk being invoked,
    at which point the underlying computation happens at most once,
    and subsequent attempts to force the delayed value yield the same value.}
  @item{Computations as lazy values—about the same as delayed values,
    but the lazy value is treated syntactically as a value,
    the “forcing” happens automatically if the value is inspected,
    and the lazy wrapper is idempotent, i.e. if a value is already lazy,
    you don’t have to force it twice (important since forcing is implicit).}
  @item{Computations as forks—as if a thunk were already scheduled to be evaluated in parallel,
    and its side effects may and will take place even if the computation isn’t explicitly invoked
    to wait for its results.}
  @item{Computations as futures—same as a fork, but invoking the computation is implicit
    when the result is needed, as with lazy vs delayed values.}]

Infinitely many such embeddings can be devised.
For instance, some variants will be thread-safe using some expensive mutual exclusion primitive;
whereas other variants require users to enforce mutual exclusion manually or will misbehave;
some variants may involve one copy of the computation in each “process”,
while others may try to ensure a unique (or centrally managed) copy
of the computation in a single process on a given machine or across a given network;
further network variants may guarantee a number of redundant copies instead of a unique copy.

In a given fixpoint, including but not limited to the case of computing the target of
a modular or modularly extensible specification,
one of those embeddings must be chosen,
with corresponding consequences for the semantics of the fixpoint computation.
Each object system or library in each programming language
may choose its own embedding, parameterize over it,
offer bridges to and from other embeddings, etc.
Whichever strategy is chosen,
I will call @emph{suspension} the embedding of a computation into a value,
and @emph{outcome} of the suspension the value that results
from evaluating the computation to its end (if it terminates).
I will speak of the suspended computation, but also, by extension, of the suspended value
that is the outcome of the computation.
If the computation yields a value of type @c{foo},
I will also call the computation a @emph{suspended foo}
(e.g. suspended record, suspended integer).
If the computation fails to terminate, trying to extract its outcome will also not terminate,
but will only produce the side effects of the computation.

@; TODO: will there be Scheme code in 1st Ed? Or only 2nd Ed?
@; In my Scheme code,
I will call @c{suspend} and @c{outcome}
the primitives to suspend a computation and extract the outcome of a suspension.
The resulting syntax is slightly verbose and awkward, but that is the whole point:
to make the transitions between values and computations explicit,
so you the reader can more clearly see where those transitions are needed.
My sample implementation of them will simply be the (non-thread-safe) @c{delay} and @c{force}
primitives of Scheme, but you can use whichever primitives make sense for you,
in whichever language you are using, in the context of the application you’re building.
In a lazy-by-default language such as Nix or Haskell, these two primitives
may become implicit and invisible, as the language already hides and handles this complexity for you;
at least, they will remain invisible until you want to think about performance and optimizations,
or need to transform the code into monadic style to enable some side effect
in the computations by which you define and build your objects.

@subsection[#:tag "SRoRoS"]{Suspended Records or Records of Suspensions}

When considering the low-level details of the implementation of objects,
one will want to explicitly separate the issues at hand:
first, one must choose a representation for records,
considered as values, not computations;
second, one must choose a representation for the recursive computation being specified,
distinguishing suspensions from their outcomes.
Then, the simple formulas with a Y combinator of @secref{MOO} break down,
to be replaced by more elaborate variants;
and each implementor of OO must face the tradeoffs associated with their representation choices.
Among those choices is whether to put the suspended fixpoints before or after the record.

Let’s assume a Y-encoding to start with.
In the suspended record representation, a regular record of the method values
is being computed as a fixpoint;
but the actual target value is the suspension of this fixpoint computation,
yielding the record as its outcome.
A suspended variant of Y is used, of type @c{Suspended(Record(k:K,Vₖ))},
where @c{Suspended(X)} is essentially equivalent to a thunk @c{1→X}.
And @c{Record(k:K,Vₖ)} is the dependent type of records having
for each key @c{k} a value of type @c{Vₖ}.
@; TODO @Code{ ... } see @(pommette)
By contrast, in the record-of-suspensions representation, the target is a record whose values
are suspensions that each yield the method value for the given key,
of type @c{Record(k:K,Suspended(Vₖ))}.
A suspended variant of Y can be used,
or an eager variant of Y specifically allowing forward reference for records (with language support).
Finally, you can have both the record and its fields be suspensions, in a belt-and-suspenders move;
this is actually what you have implicitly when you use Nix, Haskell,
or other languages that are lazy by default.

I briefly mentioned this choice between two isomorphic but different representations
when discussing U-encoding @secref{CfUe}:
An actual direct U-encoding would be a suspended record you invoke as
@c{(record-ref (half half) method-id)},
which would inefficiently recompute the entire record every time
(or @c{(record-ref (half hyper half) method-id)} to support mixin or multiple inheritance).
Instead, the ubiquitous isomorphic representation that everyone uses is actually
a record of suspensions you invoke as @c{(record-ref half method-id half)}
(or @c{(record-ref half method-id hyper half)} for mixin inheritance,
@c{(record-ref half method-id (precedence-list half) half)} for multiple inheritance).
Note that @c{half} itself, by requiring a call with @c{half} as argument (and maybe @c{hyper}),
is already a form of suspension. No further form of suspension is required,
though such forms can be useful as caching mechanisms.

There are tradeoffs with each representation.
Each suspension adds a little bit of overhead at each use,
but can save a significant though constant amount of work if actually unused.
In a very dynamic system where it is hard to predict those things,
and/or with feature-rich objects that provide a lot of convenience methods
of which a small fraction are used, suspensions are a boon.
In a spartan static system that was neatly designed to only define what is strictly needed
and use it all—suspensions are pure overhead.
That said, the spartan system might want to use OO only at compile-time,
and expand to some optimized low-level code, à la C++.

@subsection[#:tag "RtM"]{Recursion through Mutation}

@subsubsection[#:tag "aPStIR"]{A Popular Strategy to Implement Recursion}

Interestingly, mutation, the commonly available side effect whereby
programs may update record fields in place, provides a popular
way of constructing recursive definitions in two separate steps:
First, allocate a record and retain a stable reference to it@xnote["."]{
  In this paradigm, you can view a mutable memory cell as a single-field record,
  and after a “boxing” transformation, a mutable variable as a mutable memory cell.
  Any of those three could be a primitive to which the other ones are reduced:
  a mutable record could be reduced to an immutable tuple of memory cells,
  and a memory cell to a record-as-closure
  whose @c{get} and @c{set} methods access a mutable variable.
  Pick whichever your language or meta-language makes easier to reason with.
  I picked mutable records as primitives, because their API better reflects
  the performance expectations of today’s typical OO environments.
}
Then, use the reference to update the values of record fields in place
by calling some initializer function with that reference as argument.
A field is said to be @emph{initialized} after this update is complete,
@emph{uninitialized} until then.

In this mutable paradigm, references provide access to future recursively defined records
even before they are fully initialized, much like suspensions in the immutable paradigm.
With a uniform representation of fields, as in a typical dynamic language implementation,
each word-sized field may mark itself uninitialized with a special “unbound” marker
(with a uniform lazy representation, it would contain either a suspension or a computed value).
With field representations specialized to their types, as in a typical static language implementation,
an out-of-band bitmap could indicate which fields are still uninitialized.
In some implementations, the bitmap could even be garbage collected
after the object is fully initialized (@secref{LS})@xnote["."]{
  An implementation of a pure lazy language could also implement records
  with efficient packed values and out-of-band suspension information for narrow fields
  that gets garbage collected when the record is fully computed.
  By contrast, as of 2026, GHC 9.14 can unpack suitable strict fields into their enclosing records,
  whereas ordinary fields that remain lazy are represented by pointers, even after evaluation.
  An evaluated thunk can become a forwarding node pointing to its result,
  much like a copying collector’s “broken heart”—except that the destination
  is a computed result rather than a relocated copy.

  Thunk updating and indirection-shortcutting by the garbage collector
  can eliminate the suspension, but do not thereby pack its result into the enclosing record.
}
The same duality between computation and value, open future and closed past,
exists in both paradigms, though languages with mutation
tend to blur it syntactically (@secref{IRS}).

I discussed the semantic difficulties with modularly initializing recursive structures
in @secref{IRS}, with solutions ranging from unsafe uninitialized memory
to null values to options to catching uninitialized reads
to dynamic safety with suspensions to static safety with correctness proofs (manual or automated),
or dodges like using “builder” methods.
These solutions presented tradeoffs between
performance, abstraction, expressiveness, complexity, modularity, etc.
These difficulties, solutions and tradeoffs apply
whenever data structures are computed modularly,
including when some of these data structures are nested records,
including when some of these records are prototype targets,
including when some of these prototypes are classes,
including when some of the records are instances (elements) of classes.

@subsubsection{Mutation over Purity, Purity over Mutation}

Recursion-through-mutation provides a commonplace alternative to pure fixpoints
in languages with mutation.
But the relationship between mutation and purity goes both ways,
and each can usefully implement the other.

Mutation can be expressed in a pure functional way, through a state monad
that explicitly passes around a record as a “mutable store”,
the current mapping of identifiers (or locations) to values.
Recursion-through-mutation can then be a valid implementation strategy
in a pure @emph{applicative} programming language (or pure total language)
without any builtin primitive for lazy or delayed values:
mutable state, though awkwardly emulated,
enables the indirect expression of forward references and circular data structures,
without requiring primitive support for cyclic values.
For instance, in proof assistants like Rocq, Lean, Agda, Idris,
circular structures can be encoded as explicit stores indexed by keys.
You can also use this technique to reason inductively about programs
in a language with effects, using a pure functional and total subset of that language:
in Common Lisp using ACL2,
in F* using its pure fragment, etc.

Conversely, a pure semantics can be implemented by mutation:
A memoizing suspension may use mutation to replace a stored pure computation with its result,
changing the representation without changing the value denoted.
But mutation can also be used to represent a change of value denoted in a pure semantics,
when some single-threaded record is replaced by a new record while the old one becomes unreachable;
ACL2 and Clean can do it based on
(type-based for Clean) static analysis, @; TODO cite Boyer2002 Barendsen1993
while Lean 4 makes similar decisions based on dynamic reference counting; @; TODO cite Ullrich2019
Haskell’s @c{ST} monad permits mutation behind a pure interface,
the type of @c{runST} preventing access to the internal state from outside the computation,
without requiring individual mutable references within it to be unique. @; TODO cite Launchbury1994
Pure programs can thus often run with the performance of mutating ones.
F* combines these perspectives: stateful programs can be specified and verified
against a pure model of state passed through a monad,
then compiled to actual mutation—to OCaml,
or for its Low* subset, to C. @; TODO cite Swamy2016 Protzenko2017
ATS exposes primitive mutation and pointer operations,
whose safe use is established through linear types and proofs about memory;
the proofs are erased before execution. @; TODO cite Xi

Either way, as far as OO and other pure functional concepts go,
@principle{Mutation is an implementation technique, not a semantics}.
I will return to this point in a more general setting
when discussing side effects in towers of implementation. @; TODO secref

@section{Dimensions of Modular Computations}

The preceding representations illustrate several dimensions of a larger design space.
Object initialization is a particularly revealing example of modular computation:
its parts may refer to the very context whose contents they are collectively computing.
But the same questions arise whenever a modular computation makes some results available
while others remain to be computed.
Having discussed the semantics of initializing recursive structures in @secref{IRS},
I will now focus on how implementations can organize such computations.

I will distinguish seven dimensions:
@itemize[
  @item{@emph{Availability of partial results}:
    what may be observed or requested before the whole computation is complete.}
  @item{@emph{Granularity}:
    which parts are separately suspended, computed or coordinated.}
  @item{@emph{Dependency resolution}:
    how the prerequisites of each computation are identified and satisfied.}
  @item{@emph{Uniformity vs specialization}:
    how much representation and code is shared between different modular computations.}
  @item{@emph{Lifetime staging}:
    which invariants and representations change as computation progresses.}
  @item{@emph{Concurrent access}:
    which computations may simultaneously produce or consume partial results.}
  @item{@emph{Responsibility for invariants}:
    what the current implementation guarantees and what it requires of its clients.}]

These are not successive grades of sophistication,
but somewhat independent dimensions.
They still interact in ways that make
some combinations cheaper or more expensive than others:
For instance, a uniform data representation that supports suspensions
may make it cheap to adopt dynamic availability of partial results at a fine granularity.
The most appropriate combination depends on the precise semantics being implemented,
and the last dimension makes explicit the boundary between which part of the overall semantics
is to be provided by an implementation layer vs by the client layers above.

@subsection{Availability of Partial Results}

As discussed in @secref{IRS},
a component of a modular computation
can already be defined without its value having been computed yet,
or can remain uninitialized until an operation supplies that value.
A request for an unavailable value may start its suspended computation,
wait for a computation already underway,
return a distinguished default if that is what the protocol specifies,
signal an error, or silently misbehave.

What matters with respect to this dimension is the semantics of partial availability,
i.e. what clients may observe while partial computations are underway.
This is quite different from the matter of how data is represented underneath.
A promise may be represented by a standard-size mutable cell that contains
a promise descriptor until it contains the non-promise result;
or it may be represented by an appropriate entry in a packed data structure
(e.g. of one or a few bits or bytes), with a promise flag or descriptor out-of-band;
that would be the same semantics.
If the user must follow a lower-level protocol where cell assignment is explicit,
and must manually enforce the promise invariants—then
the underlying representation might be the very same, yet the semantics are different.

@subsection{Granularity}

The availability protocol specifies what happens when an unavailable result is requested.
A related but distinct question is: what are the units
of independently suspended or executed modular computations?
I already described a concrete instance of this dimension in @secref{SRoRoS}:
a suspended record versus a record of suspensions.
One computation may produce the record, or separate computations may produce its fields.

But there is a continuum between the two, and
the @emph{granularity} of the computation protocol is a second dimension of design.
At a finer grain, more fields of a structure, and subfields within substructures therein,
may each have their own suspended computation or initialization protocol,
that may be triggered and scheduled, or left unexecuted, independently from the others.
At a coarser grain, groups of fields are computed together;
many languages have a protocol in terms of separate “constructors” or initialization methods
for each “object” in the data graph;
more primitive languages let you register initialization hooks to be run
for each compilation unit at program startup or upon dynamic loading.

Finer-grained support can provide more modularity in developing the software in independent parts;
but it can also incur more management overhead (whether at runtime or compile-time).
Coarser granularity can be much more efficient by sharing or eliminating this overhead.
When the system only supports coarse-grained initialization and leaves
finer-grained dependencies between individual field values to be managed manually,
their management has been moved from first-class to fourth-class;
runtime automation was cut, but programmer-time manual work was increased.
Another way of looking at it: there is always a protocol that covers each and every entity,
but this protocol may have multiple tiers of entities with various levels of system support,
possibly corresponding to multiple levels of language
at which different kinds of developers interact.

For the “best of both worlds”,
an implementation may analyze finer-grained dependencies at an earlier stage, then
automatically group computations into coarser-grained units to reduce overhead—when
the grouping preserves the semantics being implemented.

@subsection{Dependency Resolution}

The third dimension is how dependencies between units of computation are resolved.
At one end, they are discovered dynamically as the computation proceeds,
for instance by forcing suspensions when their values are demanded,
as in a pure functional protocol where every individual field is lazily defined.
At the other end, they are analyzed statically at an earlier stage,
so that a fixed order can be established in advance,
with a guaranteed absence of circular demands.
The computations can then follow that order without the overhead of dynamic discovery.

Requiring a statically established order can cost modularity and extensibility in three ways.
First, the cruder the analysis, the more ordering information programmers may have to supply.
Having to supply more information, or worse, revise earlier modules, erodes extensibility.
Having to coordinate with other modules to figure out the information undermines modularity.
Second, even the most elaborate sound, fully automated analysis
cannot recognize all safe initialization patterns in a sufficiently expressive language:
it must either restrict its domain to the cases it can decide,
or approximate dependencies conservatively and reject programs over phantom loops;
either way, it rejects valid programs, and couples entities that should be independent.
Third, and most fundamentally for OO, an order fixed when a specification is compiled
cannot take into account computations that have not been defined yet;
it cannot topologically sort fields that will be defined or overridden
in future descendants of the specification.
Yet OO is all about partial specifications with incomplete initialization behavior,
later extended and eventually completed by further partial specifications.

The third problem can be avoided by compiling initialization anew for each class,
once its whole ancestry is known,
as when every class of second-class OO is monomorphized:
the compiler can then analyze all its slot-defining clauses
and schedule their computations without temporary null values or unbound markers,
provided it can establish a suitable @emph{static} order,
rather than only a @emph{dynamic} one.
Compilers already perform related dependency analyses outside OO:
Scheme implementations use them to optimize @c{letrec} and @c{letrec*}
while preserving their initialization semantics
@~cite{Waddell2005 Ghuloum2009}. @; TODO also cite Reynaud2021
Unhappily, I don’t know of any OO system that has adopted
this automatic derivation of initialization order.
Existing second-class OO systems tend to offer the worst of both worlds,
with neither the flexibility of dynamic dependency resolution,
nor the guarantees of a statically validated and still modular initialization order.

Semantics and implementation of dependency resolution
can also be chosen somewhat independently.
A language permitting dynamic dependency resolution may be implemented naively,
discovering all dependencies at runtime the hard way;
or an implementation may analyze what it can at compile-time, optimize accordingly,
and fall back to dynamic discovery for the rest,
reporting errors at runtime when the initialization protocol is violated.
A language requiring a statically validated order rejects programs
for which no such order has been established,
but its implementation may, for the sake of simplicity,
eschew the runtime optimizations enabled by the analysis.

When a protocol sacrifices too much modularity and extensibility for the sake of “efficiency”,
programmers do not stop needing the flexibility; they rebuild it themselves,
initializing fields with dummy values and later updating them to their actual values,
or moving the actual computation of initial values into “builder” objects (@secref{IRS}).
As with granularity, the part of the protocol lacking first-class or second-class support
didn’t disappear—it was relegated to fourth-class conventions.
@;{ TODO
Swift statically validates initialization, but only an order spelled out by hand.

Scala and Kotlin offer lazy fields as opt-in dynamic resolution.

Kotlin: with the usual member-property translation, each declaration
using a fresh by lazy expression has its own delegate storage.
Overriding the property with another by lazy does not reuse the inherited delegate.
Check the target backend and generated code before quantifying the per-instance cost.
Also distinguish laziness of the value from initialization of its delegate:
access from a superclass initializer can reach a derived property
before the derived delegate has been initialized.

Scala: an overriding lazy val cannot use super.myVal to obtain
the inherited lazy value. To compose initialization through super,
one workaround is to retain lazy val myVal = initMyVal
and override the method initMyVal, calling super.initMyVal as needed.
}

@subsection{Uniformity vs Specialization}

An implementation may use the same uniform representation and access code
for all computations and their suspensions;
or it may have many specialized representations, each with its own access code,
tailored to each data structure.
I already discussed cases of uniform and specialized representations
in @secref{PFCTD} and @secref{aPStIR};
the same tradeoff applies to all modular computations,
regardless of whether the representations arise from interpreters, ahead-of-time compilers, or JITs.

Specialization may save time and space thanks to more efficient representations,
fewer indirections, fewer checks, fewer wrappings and unwrappings, etc.
But specialization requires more work at some earlier stage,
and may generate more code to handle the many cases, increasing instruction-cache pressure.
A uniform implementation can therefore be preferable even when specialization is possible,
and only more so when structures change frequently or each specialized case is used only briefly,
such that specialization is not amortized over enough uses to save more than it costs.

A compromise is to use uniform representations most of the time,
and specialized representations for performance hotspots,
either statically or dynamically (with a JIT).
Runtime descriptors can also provide a uniform level of indirection
to specialized implementations, as with first-class records
implemented as second-class records plus a first-class descriptor (@secref{RaR}).
Either way, specialization is best kept below an abstraction boundary:
exposing specialized encodings across module interfaces decreases modularity,
by forcing clients to know which encodings other modules use.

What can be specialized depends on what is known when.
In second-class OO, classes are known at compile-time:
the compiler can @emph{monomorphize} code,
generating a specialized version for each class, at the cost of code size.
In first-class OO, some information may be known at compile-time,
especially under a Closed-World Assumption @~cite{Ducournau2009},
and the same specializations may be available.
However, under an Open-World Assumption,
and especially in an interactive environment with reflective redefinition
of variables, functions, prototypes, classes,
many current assumptions may later be invalidated.
Optimizations relying on such assumptions need guards
or dependency tracking and invalidation, @; TODO @~cite{Chambers1989}, Hoelzle1992 ?
with synchronization where concurrent changes are permitted.
Runtime observations provide another opportunity for specialization:
a call site or field access that has only seen one shape so far is @emph{monomorphic},
and can be speculatively specialized for that shape under a guard,
with polymorphic inline caches for a few shapes (@secref{RaR}),
and a fallback to general lookup when no guard matches.

Translating between second-class and first-class representations
has its cost both ways:
forcing first-class OO into a static straitjacket
brings pain in proportion to the impedance mismatch;
while translating second-class OO into first-class OO
by simply forgetting static information
forfeits the optimizations that this information enabled.

@subsection[#:tag "LS"]{Lifetime Staging}

A modular computation need not use the same representation throughout its lifetime.
Its execution may involve distinctive stages that an implementation may exploit
to pick efficient representations for each stage.

For instance, a mutable record may evolve quickly in the beginning,
with a dynamic set of keys, and be represented as an alist at first.
At some point, its set of keys may become fixed, and
the record be implemented as a second-class record
with a descriptor for its uniform field representation.
In a later stage, the values of its fields may become more stable, or
the information about the types of its fields may become more precise,
enabling the record to gain a packed representation.
Then, the record may itself become immutable,
and be treated as invariant for a stage,
and even folded as a constant when optimizing the next stage.
In a yet further stage, it may become mutable again, or gain new fields that are mutable,
returning to an alist or a hash-table at least for the new fields.
See typical representation choices for objects in @secref{OL}.

The stage transitions themselves can be based on a static analysis of the source code,
with multiple versions of “the same” variable
after transformation to static single assignment (SSA) form @; TODO cite Cytron1991
helping to identify uses that can have different representations; @; TODO cite
or they can be based on dynamic usage measurements and runtime detection of usage patterns.
In either case, what is “the same” entity in source code, and can be tracked
along its entire lifetime with the same identifier from the REPL, shell, debugger or GUI,
has different representations underneath across time.

The same low-level storage may be accompanied by a different descriptor,
or an internal discriminator (number, tag or bitmap) that changes
with each transition that is not static.
C++ implementations already change virtual-table pointers
during construction and destruction of each ancestor’s part of an object
whenever virtual dispatch is involved @~cite{Stroustrup1989 Itanium1999}. @;{ TODO
  Swift statically enforces a two-stage initialization:
  all stored properties must be initialized before @c{self} may be used
  or any method called, after which the object’s full interface becomes available.
  @; TODO cite{} ???
}
In many JavaScript engines, as earlier in Self @~cite{Chambers1989},
each object has a hidden and changing “shape” or “class” or “map”
that describes its internal representation and guides specialized accesses;
an object may change shape when fields are added or removed.
Any method usable both before and after a transition must support both representations,
use an interface that remains valid across the transition,
or itself have separate specialized implementations used before and after the transition.

If the transitions involve unpredictable growth, and
reserving the maximum possible size for every object is not an affordable strategy,
then new low-level storage may be required, which brings its own challenge:
how to preserve references across stages when the actual storage record has moved?
One simple solution is to introduce a level of indirection between
the stable identity of an entity and the underlying storage record for its data:
the identity might be a number in a table rather than a pointer in memory @~cite{Goldberg1983},
or a pointer to a “header” record that embodies the identity but points
to a “slot vector” to carry the data in its current shape @~cite{Kiczales1991}.
Another solution is to readily change low-level storage records and their address,
but leave a forwarding pointer or “broken heart” at the previous location,
with a “read barrier” ensuring that the latest location is used before actual work
@~cite{Weinreb1981}; @; TODO cite Cheney1970 Ungar1984 Baker1995Cheney Miranda2015
alternatively, existing references can be located and rewritten
before execution resumes, as in a stop-the-world copying collector @~cite{Abelson1996},
trading a small tax at every read for a big tax at every move.
Yet another solution is for the referenced entity to
maintain a set of backpointers to those locations that reference it,
so they are updated at each transition @~cite{Bawden1992}.
Special attention can be given to the common (but not general) case
where there is only one such reference, and the backpointer can be left implicit
from whichever path was used to access the record to begin with.
Rust-style affine types can be seen as static enforcement for this latter case.

Choosing what static or dynamic transitions in representation to support
is a design space of its own.
As per the theme of this chapter, once you understand the desired semantics,
finding efficient encodings for it is just an open puzzle for clever or cultured implementers.

@XXXX{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

@subsection{Concurrent Access}
@;{
A partially computed result may be confined to one thread,
shared only after an agreed completion condition holds,
or made referenceable by other threads before the computation completes,
with some mechanism to block access until it completes.

Separately, its components may be computed sequentially or concurrently.
Concurrent access and concurrent computation are therefore different guarantees:
several threads might demand the same suspension while only one evaluates it,
or different threads might evaluate independent components.

Sharing partial results requires a protocol for coordinating their production and use.
A consumer must not observe a result as ready before the result itself is available;
a change of representation must likewise preserve the meaning of concurrent accesses.
Synchronization can make an otherwise cheap state transition expensive,
and can affect the appropriate granularity of the computation.
The important question here is which interactions the implementation supports;
the detailed mechanisms belong with the later discussion of lower-level subsystems.
@; Existing examples: non-thread-safe delay/force vs a synchronized suspension;
@; thread-confined construction vs publication of completed objects.

In a concurrent setting, appropriate use of atomic delay primitives
can likewise prevent race conditions in the forcing of the suspension itself.

Those semantic/organizational choices determine the need for
atomics, synchronization and publication barriers.
Detailed mechanisms can wait for the later lower-level-interactions section.
}

@subsection{Responsibility for invariants}
@epigraph{
  A programming language is low level when its programs require attention to the irrelevant.
  @|#:-"Alan Perlis"|
}
@;{
Finally, each of these dimensions must be considered relative to
where we place the boundary between an implementation and its clients.
There is freedom in how a system is factored into implementation layers;
but once a boundary is chosen, the semantics promised there determine
which invariants the implementation must establish and which preconditions its clients must meet.
At one boundary, demanding a field may automatically arrange its computation.
At another, reading a cell may require the client to have initialized it already.
A further implementation layer can use the latter mechanism to provide the former guarantee.

Thus, exposing allocation and assignment does not by itself implement
our lazy modular semantics, but can provide tools with which to implement it.
The remaining responsibility need not fall on the application programmer:
the client may itself be a library, generated code or another implementation component@xnote["."]{
  Some languages deliberately stop at comparatively concrete primitives,
  leaving higher-level protocols to their clients, traditionally human programmers.
  Other systems supply those protocols while internally using similarly concrete primitives.
  A low-level interface can be appropriate for a compiler or optimizer
  even when its invariants would be unreasonable to maintain manually in application code.
  Human-written and AI-generated code can occupy either side of such a boundary.
}
@; Preserve the existing CLOS example for the later BOP discussion:
@; initialize-instance, shared-initialize, :initform, :initarg, :default-initargs.
@; Here it illustrates protocols supplied by the system versus invariants still owed by clients.
What must be explicit at a given boundary follows from this division of responsibility;
it is not an additional, independent dimension of the design space.

XXXX

An imperative initialization protocol, by contrast,
necessarily requires programmers to explicitly deal with more details,
as it separates object computation into allocation and initialization stages.
The imperative protocol is therefore comparatively @emph{low-level},
which might make sense in the relatively rare cases where you really care
about the performance of such initialization operations.
But for most programmers, in most cases, this performance does not matter enough
to justify having to care about all these irrelevant additional details.
And most high-level languages will offer escape hatches to deal with such details
in the rare cases where performance concerns justify the attention
(indeed, those who write and extend compilers eventually need to get down to that level).

More advanced protocols, such as
the @c{initialize-instance}, @c{shared-initialize} and related generic functions of CLOS
(that also offers a simple declarative protocol
based on the @c{:initform} or @c{:initarg} of each slot,
and @c{:default-initargs} of each class),
allow for much more sophisticated orders of slot initialization
that can involve the results of arbitrary intermediate computations,
indeed intermediate method calls, etc.;
yet the more sophisticated such methods get,
the more discipline they will require from programmers,
who are notoriously bad at maintaining such discipline.
And even such sophisticated protocols will have limitations
that require the occasional use of uninitialized or null values.
They will still not be as flexible as the simple pure functional protocol I defined
when used with laziness.

Once these choices are fixed, implementations can still differ in how much effort
they spend analyzing dependencies, specializing code or managing representations dynamically.
That effort has to earn its keep through the savings discussed earlier.
Being more clever underneath an abstraction boundary does not, by itself,
change the semantics promised above it or the obligations imposed on its clients.



Notionally comes first/last—define what the current level does vs layers above or below.

Where does the obligation live?

A higher implementation layer may guarantee safe access;
a lower one may expose a precondition to its client, allows more performance.

This is where some of the current rhetoric about low-level protocols belongs—not “imperative bad”,
but the lower the abstraction boundary, the more obligations may be exposed.

Decides what gets automated below, and automated or done manually above.


@; TODO MORE CODE
}

@;{
@subsection{From Computation to Structure}

So far in this chapter I’ve focused on the computational aspect
of how objects are brought into existence:
how to think about them algorithmically;
the importance of staging (second-class vs first-class)
in determining what algorithms are even possible;
how the duality of computation and value arises;
how to implement the recursion inherent to modular definitions.

Each of these topics by itself is vast:
the theory of algorithms, even restricted to implementing things as simple as finite maps;
the theory of performance in the abstract
(asymptotic or algorithmic complexity, models for large memory),
in the concrete (memory hierarchy and other practical limits to scaling),
and in-between (cache-oblivious algorithms);
values, hierarchies of computations, type and effect systems,
or concretely space and time overhead associated with calling conventions
and choices of encodings for suspensions and outcomes;
staged computations, moving termination issues to the meta-level,
bootstrap issues, how to support staging, and interaction loops involving it,
in toolchains and down to the operating system level.

The interaction between these topics is vaster still.
There is no room in this book to contain them, only to frame them
in a way that reveals relationships you might not otherwise notice in the extant literature.
I am inviting you, the reader, to consider the tradeoffs involved
when implementing objects—not in the narrow sense of minor optimizations
after having accepted the semantics and ABI of some language you are extending,
but in the broader sense of all the semantics you could be considering.
For the choice of semantics will affect not just the performance of the programs you write,
but what programs you might be writing instead under different semantic assumptions.
I am hoping to inspire some of you to explore heretofore unexplored territory
in the modular and extensible design and implementation of software.

But I’ll stop here for now with these lofty goals
(that, lofty as they may be, sometimes involve very low-level details).
Let us now focus instead on pragmatic and familiar details of
how to concretely represent objects at the end of these computations.
However computations are suspended,
once enough is known about their outcomes,
their residual structure can be represented directly
and optimized for the operations that will consume it.
}

@;{

@subsubsection{Modular and Unmodular Object Initialization}

TODO: Move the below to Recursion, @secref{IRS} @; Initializing Recursive Structures

An essential constraint about object representation, yet one easy to overlook at first,
is that the object must be referenceable and usable during its own initialization.
In the simple case of my Minimal OO (@secref{MOO}), one distinctive feature of OO
was indeed the ability of modular definitions, and the modular extensions they are made out of,
to reference the thing being defined it-@c{self},
a.k.a. the modular @c{context} being resolved,
a.k.a. or @c{final} value of the object being computed.
This ability to self-reference, or open recursion, is quintessential to OO,
and carries on to whatever variant of OO you use, with prototypes or classes, mutable or immutable.
And this ability creates all the many challenges in how objects are represented.
Inasmuch as you would like objects to be records of values (and not suspended computations),
in the general case, you can’t—yet in the common case, often you can,
and your performance will depend on being able to exploit this ability.


Now, Class OO allows some level of coping:
Class OO only needs OO to apply to the class targets that are type descriptors
(moreover only at compile-time, for second-class Class OO);
it doesn’t need to support modular extensibility or even just modularity to apply to
the “class instances” that are only the elements of the described types.
Class methods are defined using OO, instance fields are not.
Thus, many Class OO languages only offer trivial object initialization, at least by default:
All initial field values for the “object” (class instance)
must be provided either as arguments to a constructor,
or as simple default values or initialization expressions from a non-modular specification
(though possibly one overridable by extensions).
Every field not otherwise initialized is bound to some top value (zero, unit, null or “undefined”),
or (in some languages) to a magic “unbound” marker that triggers an early error
when dereferencing an unbound field (XXXXX).

In @emph{stateful} Class OO languages (which is most of them),
a slightly more sophisticated protocol is often offered
to further adjust these initial values before the object construction completes
and “clients” may see the object:
constructors or initializers will execute side effects to modify fields,
including calls to methods on the partially initialized object.
It is up to the programmer to ensure that everything goes right,
that methods called during initialization work properly
despite the object only being partially-initialized.
And if the constructor registers the object to some global table,
from which some methods are called before the object is fully and successfully initialized,
well, this as well as any other weird situation is the responsibility of the programmer.
Methods should be careful not to rely on invariants of fully initialized objects
if they are to be called before the object is fully initialized.
“Use at your own risk”.




An object needs a usable representation even while its recursive initialization is in progress;
layout is not only something chosen after computation “settles”.
Builder objects: “solves” initialization, but explicit staging,
much reduces modularity in initialization.

TT
Different invariants become available at different lifetime stages: construction, initialized/mature object, perhaps later frozen/sealed stages.
Therefore an implementation may change representation after initialization.
Code usable across the boundary must either support both representations, use an invariant indirection or be specialized separately.
General point: representations are stage-relative too.
The lifetime-stage point (construction → mature → sealed)

object lifetime as staging: during recursive construction the shape may already be invariant while contents are unresolved; after initialization contents acquire further invariants; sealing can expose still more. Put the suspension-placement alternatives here. This is also where your switch back to a stateful low-level OO implementation belongs: you have explained the semantic alternatives, and now deliberately choose mutation as an implementation technique.

How may I arrange things in memory?
Field ordering and offsets.
Classes, explicit or inferred
(Self maps, V8 (Chrome) hidden classes, SpiderMonkey (Firefox) shapes, object descriptors,
inline cache structures) - adding fields dynamically (hash-consing; issue with suffix classes).
Prefix layout for slots of suffix classes.
Property storage (in-object vs external).

@; TODO the layout is largely independent from how suspension is implemented.
@; susp{i,R_i} ? {i,(susp(R_i)} ? {i,susp_i(R_i)} ? susp{i,susp_i(R_i)} ?
}

@section[#:tag "OL"]{Object Layout}
@;{

TODO: New structure = derive all OO-specific optimizations from generic optimizations above
given the construction of OO in previous chapters:

* first-class pure functional OO (higher-level) vs
  some invariant responsibility to programmer (lower-level,
  maybe only for first-class stuff, not second-class stuff)

* specifications

* fixpoints

* records as targets

* suffix property

* prototypes as conflation of target and specification,

* classes as type descriptors,

* dynamic dispatch as using a special type descriptor field,

* instance methods as getting instance-methods from classes
(then passing self as first argument whether using U-encoding or Y-encoding), etc.

* computing effective methods? See ch9.
  With a bit of macrology / partial evaluation / code generation / open coding
  to optimize the chaining of applicable methods.

* static (or dynamic!) typed pointers as providing information
  onto which interface(s) to use a dynamically typed object with.

* repeated inheritance as compressed pair of pointer for container plus contained
@; TODO cite Myers1995 Itanium1999 Click2002 Stroustrup1989 Snyder1986

* ECS transparently possible with SoA instead of AoS.

* become / change-class, cost of indirection vs read-barrier vs global rewrite

At no point(?!) do we need to invent specific optimizations for OO,
we can just apply generic record optimization. OO (especially (mostly) 2nd class Class OO)
just provides context of structure, knowledge and expectations for optimizations.
As usual, we follow our "understand semantics, and optimizations follow" principle.

@; How many shapes for how many meanings? => object layout
}

@subsection{Suffix Specifications}
@;{
When using single inheritance (@secref{SI}) or optimal inheritance (@secref{OISMIT}),
second-class accesses to a static field in the suffix fragment of a prototype or class
(including the entire prototype if itself declared suffix, or if using single inheritance)
can be uniformly compiled into constant offset accesses
for all descendants of the prototype or class, making those accesses faster.

If fields are identified not just by name but also qualified
by the name of the specification that first declared them
(so field @c{x} introduced by specification @c{a} is @c{a#x} while
field @c{x} introduced by specification @c{b} is @c{b#x}),
then each field has a fixed offset, and a dynamic access need only check
that the runtime object’s suffix was a descendant of the one that introduced the field.
Since suffixes among each other are in a single inheritance relationship,
each suffix’s suffix ancestors are in a total order, and descendant testing
can be achieved in constant time in exchange for a slight increase in memory pressure:
keeping the suffix ancestors in a totally ordered vector,
and for any candidate descendant, see whether it has the speculative ancestor
in nth position from the tail of its suffix ancestors,
where n is the size of the ancestor’s own suffix ancestry.

If using optimal inheritance, then fields defined after the suffix are to be treated
as with multiple inheritance. However, when caching record shapes,
mind that these shapes must be distinguished not just by the set of identifiers they bind,
but also by the ordered subset of fields defined by their suffix as opposed to
specifications not included in the suffix.
}

@subsection[#:tag "RI"]{Repeated Inheritance}
@epigraph{
  @hyperlink["https://www.etsy.com/listing/682771837/signed-print-of-my-cartoon-those-who"]{
    Those who don’t study history are doomed to repeat it.
    Yet those who @emph{do} study history are doomed to stand by
    helplessly while everyone else repeats it.}
  @|#:- @elem{@hyperlink["https://tomtoro.com/"]{Tom Toro}, completing Santayana}|
  @; TODO check provenance, add best URL for picture
}
The “tree inheritance” from Snyder’s CommonObjects @~cite{Snyder1986} and
or the equivalent “repeated (non-virtual) inheritance” from Stroustrup’s C++ @~cite{Stroustrup1989}
are totally misguided ideas born from extreme confusion.
The simple and correct way to achieve the same effect with plain OO is object composition:
put some data in a field of type A, and proxy A’s API to that field,
at least within some scope (for “private” such inheritance),
with appropriate wrappers for those proxied method calls where needed.
Of course, use macros to automate the proxying—unless you’re a masochist
who loves typing boilerplate, wasting tokens and context on boilerplate,
and using blub languages without macros.
You may standardize those macros if you do it a lot.
If you want multiple As in your B, just declare as many A-typed fields as you like,
or an array, or a your favorite container full of As.
There is no need for this concept. Just regular fields.

Snyder (for “encapsulation” reasons) and Stroustrup (for “performance” reasons)
thought they could force a simpler tree data structure
as a substitution for the more complex inheritance DAG of multiple inheritance,
without understanding that multiple inheritance was solving a specific problem,
and that its semantics was therefore not arbitrary.
It’s as stupid as selling square pegs as solutions to fit a round holes
just because you know how to manufacture square pegs for cheaper.
Of course the real idiots in the story are the buyers, not the sellers.
Snyder happily didn’t have many buyers because Lispers aren’t idiots.
Unlike Stroustrup.

Now, there is the additional problem that in C++ you are supposed to be able to fit
a pointer to an “internal” A that also knows it is really an internal part of an enclosing
descendant of type B.
The only challenge is that C++ being a low-level language, a “pointer to an A”
must fit in the usual fixed pointer size, even though it is notionally
a pair of two pointers, one to the inner A, the other to the outer B
(alternatively, it could be a pointer to B plus an inward path to the inner A,
or a pointer to an A plus an outward path to the outer B).
Anyway, in any other language you could just use a “fat pointer”
made of two regular pointers, when for some twisted reason you truly wanted
to be able to point within an object.
But in C++ for historical and backwards compatibility reasons (“,
you have to squeeze that fat pointer into regular pointer-size
(or make all regular pointers fatter, which goes against the spirit of “performance”)@xnote["."]{
  Pointers to C++ member functions, that are not subject to these historical compatibility reasons,
  typically involve such fat pointers.
  There is so much horror and compounding complexity
  involved in the semantics and implementation of C++.
}
Once you understand that the semantics issues of repeated inheritance lie entirely
in this pointer compression, it becomes an artificial problem of low-level representation.

The C++ standard ABI @~cite{Itanium1999}
uses a particular representation of repeated classes:
a potentially repeated polymorphic class A
(those with virtual member functions or virtual base classes)
must include a separate descriptor field (“vtable”) of pointer-width
for each repetition of an ancestor A in the layout of a descendant B.
@citet{Stroustrup1989} repeatedly justifies representation choices in terms of
avoiding an indirection, an adjustment or a memory reference. @; TODO reread Stroustrup1989
That might have made good sense for 1989 machines when DRAM latency was 2-3 CPU cycles,
and pointers were very narrow (often still 16-bit, though already 32-bit for the rich).
But it doesn’t make sense on 2026 machines where DRAM latency is 150-600 CPU cycles
and pointers are wide with lots of spare bits
(64-bit for significantly smaller actual address spaces).
Today’s performance relies heavily on caching, and you can do a lot of work
while waiting for the next information to be loaded from DRAM,
or to avoid having to load it to begin with. @; TODO cite Wulf1995 on the Memory Wall
The separate vtables add up, then multiply by the number of objects in the system,
even if you never actually want to use the feature of internal pointer
(so much for the C++ slogan of “zero-cost abstraction”).
They will also clog your cache lines.
And so the short-sighted choices of Stroustrup and his successors
for how to implement this bad feature are now a bad trade.

A much better implementation of this C++ “feature” could be
to conscript a few unused bits off from the pointers
(that some hardware already mask for you, and that are also cheap to mask in software),
and use them as a count for which repetition of A within B you mean; and if you “only”
have 6 bits of “free” count, you can always fall back to the “many vtables” strategy
if you have more than 64 repetitions of a class@xnote["."]{
  As of 2026, the most popular CPU architecture, the Intel x64, has 64-bit addresses;
  but the hardware only manages only 48 or 57 bits of actual virtual address space.
  Recent CPU variants are even kind enough to make 15 or 6 of the high bits available
  for use at no extra cost with a little bit of Operating System support
  (reserving the sign bit for system space vs user space pointers).
  That’s a minimum of 6 free count bits.
  If you mask bits in software, you can have as many or as few counter bits as you want;
  and there are also 3 extra low bits for cheap.
  But 6 should be plenty enough in practice, and 15 more than enough.
  Instead of a full 64-bit descriptor field, you could also point to an 8-bit
  “extra count bits” field immediately following the main descriptor;
  and those “extra count bits” fields could in turn be shared by all repeated classes.
  But really, if your programs have more than 64 repeated polymorphic classes in C++,
  you should be seriously reconsidering your life choices.
}
Happily, such large number of repetitions don’t seem to happen in practice in large C++ codebases,
and you may as well have your compiler reject the code—even though
in theory the number of repetitions may increase exponentially
as the ancestry of your classes grows deeper.
And in closed-world compilation, or with a suitable linker or JIT,
you only need to pay the price of compressed pointers
for polymorphic classes that are actually repeated.

Now, whichever representation trick you use for repeated inheritance, it’s all fun and games,
but a huge pointless waste of one of the scarce resources of the computing arts:
the time and attention from authors of compilers.
Really, shame on Stroustrup.
All that for a dubious “feature” that is the non-solution to a non-problem
because the language authors never stopped to understand what OO was actually about.

The larger point of this section is not to laugh at how bad C++ may be,
but to realize that @principle{you should think in terms of the higher meaning of your programs},
and then not only will you be able to find more efficient implementations for them,
you will also be able to wholly avoid having to implement features
that wouldn’t exist had you better designed your programs
(including your programming languages).

@; But there are other implementation strategies @~cite{Ducournau2009}

@; TODO FKS hashing Fredman1984 Ducournau2012

@; budget: how long to search for better optimization vs how much to gain?

@; TODO cite Ducournau2011Coloring

@; TODO cite Ducournau2008 Ducournau2009 Ducournau2011Implementing

@subsection[#:tag "ED"]{Efficient Dispatch}
@;{
Object descriptor, vtables (works well with single inheritance / suffix classes), interfaces, etc.
Multiple dispatch.
Inline caching per call-site @~cite{Deutsch1984 Hoelzle1991}, caching per-function. @~cite{Bobrow1986}
Method combinations: precomputing effective methods.
Sealing and devirtualization.
Type feedback and speculation—JIT at the atomic block level, not call site level.
Fast paths (statically or dynamicall compiled) and fallback (dynamic: deoptimization and bailout).
Caching and invalidation for dynamic class hierarchies.
Usual optimization techniques after having expanded the OO semantics;
but also typical OO patterns for what is static|dynamic{JITable|random}.

Space/time/flexibility tradeoff. C++ vs CLOS.
Instead of making every object hundreds of bytes larger,
you could just make object pointers slightly larger—a pair of
}

@section[#:tag "MOP"]{BOP: Meta-Object Protocol, Unconflated}
@epigraph{
  Metaobject protocols also disprove the adage that adding
  more flexibility to a programming language reduces its performance.
  @|#:-"Kiczales, des Rivières, Bobrow"| @; Kiczales1991
}
@;{
@epigraph{We can solve any problem by introducing an extra level of indirection.
@|#:-`("Butler Lampson" , @~cite{WikiFTSE})|}
@epigraph{Almost all programming can be viewed as an exercise in caching.
@|#:-"Terje Mathisen, programming optimization guru"|}
@epigraph{There are two hard things in computer science:
  cache invalidation, naming things, and off-by-one errors.
@|#:-"Leon Bambrick"| @; http://martinfowler.com/bliki/TwoHardThings.html
}
BOP: MOP rebranded for control-plane analogy

What is a MOP?

Metaobjects (in CLOS MOP): classes, direct/effective slots,
individual methods descriptors, effective functions, generic functions,
specializers, method combinations (see Verna), etc.

Metaclasses: classes whose instances are classes. @~cite{Cointe1987 Kiczales1991}

Introspection vs intercession.

The MOP as a reflective tower.

Tying together all the bells and whistles in defining
bindings, representations, objects, classes, methods, combinations, etc.
I adapt and generalize the techniques from AMOP @~cite{Kiczales1991}
in a pure functional setting.

Make the compilation infrastructure itself Modularly Extensible?
Why yes of course!
Nano-passes, @; TODO cite Siek2023
Ziggurat. @; TODO cite Fisher2008

@subsection{Reflection: Introspection and Intercession}

Reflection:
(a) merely describing/reifying something,
(b) introspecting it,
(c) actually interceding/controlling how it is implemented.

Back-objects, fore-objects.
}

@section{Tower of Implementation}
@;{

Reflective towers (3-Lisp, Brown, etc.).

Reification and reflection. Names... sometimes upside down.

Infinite towers vs truncated towers. Turtles all the way down, not hooks all the way up.

Collapsing towers for efficiency. Namin.

Partial evaluation and Futamura projections (or move to generic computation/recursion section?)

Meta-Helix @citet{Chiba1996}.

Add serialization, persistence, to a meta-object, NOT to the object.

Different capabilities for objects and their meta-objects => more security.

Controlling Meta-Objects,
from Synchronous Message-Passing Proxies to Fully Abstract Asynchronous Containers.
I can only briefly survey this topic, maybe reusing the Collapsing Towers of Interpreters.
}

@subsection{Side Effects}
@;{
TODO Move to Implementation Towers?

I will revert to stateful OO, because I suspect
that’s what my public is interested in, will use, and has in their underlying language.
But you can do it all with pure functional data structures if you want (see exercises below).

One man’s purity is another man’s side effects.
You can have side effects in the implementation of a pure language,
or a pure language implementing a stateful one.
Indeed, the transition can happen many times in a same program:
(a) some programmer writes a program in a language that for convenience,
provides stateful side effects;
(b) the program is translated into some pure functional semantics in which
some security and robustness properties can be automatically specified and proven correct;
(c) the program is then implemented efficiently by translating linearity into side effects,
and using dynamic stateful caches to accelerate repetitive computations;
(d) the execution is interpreted as pure transitions of a low-level virtual machine
so a zero-knowledge proof of faithful execution can be produced;
(e) after however many layers of translation, everything happens in term of electronic circuits,
that you can see either as stateful changes in current levels or pure monadic transitions
of the machine state.

Purity or statefulness is a matter of the programmer’s point of view,
not intrinsic properties of the underlying computation.
What @emph{is} an intrinsic property of computation on the other hand is
that it can and will be decomposed many layers of implementations.
}

@section{Bootstrapping an Implementation}
@;{
TODO move into Recursion above?

The chicken-and-egg problem. Metacircular definition.
At some level, “just” recursion and fixpoints.
Tying the knot: how does Object have a class?
Bootstrapping CLOS (or Smalltalk, or your system).
Staged bootstrapping.
}

@section{Interactions with lower-level subsystems}
@;{
TODO: Merge with MOP, or keep below? Probably the latter.

Garbage collection.
Weak references for method caches.
Finalizers and instance cleanup.
GC-safe object layout (tagged pointers, object headers).
Generational GC and write barriers for mutable slots.
Or read barriers.
Safe points for GC.

Concurrency.
Thread-safe slot access. Thread-safe class upgrade.
Lock-free method dispatch.
Safe points for JIT invalidation.
Actor-model vs shared-memory implications.

Serialization and Persistence
Pickling objects with their classes. Interaction with Levels of implementation.
Schema evolution (adding/removing slots).
Object-relational mapping as a representation issue.
Persisting continuations.

FFI and Interoperability.
Representing foreign objects
Bridging OO systems (e.g., Python↔C++, JS↔WASM)
Proxy objects and wrappers

Debugging and Tooling.
Preserving debugging information.
Object inspectors and their MOP requirements.
Stack traces through method combinations.

Allocation Strategies.
Inline allocation / bump pointers.
Object pooling for common shapes.
Stack allocation of non-escaping objects (escape analysis).
Unboxing and scalar replacement; specialized arrays (unboxed floats, etc.).
Immediate values (e.g. fixnums) and their type descriptors for OO dispatch.

Security Considerations.
Encapsulation enforcement at runtime.
Capability-based security via object references.
Sandboxing and membrane patterns.
}

@section{Finale: Putting It All Together}
@;{
TODO: Just wrap up the chapter. Postpone most stuff to 2nd Ed.

How does it all connect?
A tour through an actual implementation: the Gerbil MOP.
What this book’s accompanying code does (and doesn’t) do.
}

@;{ No code yet :-( TODO: implement stuff for ch10
@exercise[#:difficulty "Easy"]{
  Read and make sense of the code I wrote for this chapter in @(pommette),
  that you may find e.g. at
  @url{https://github.com/metareflection/poof/blob/main/pommette/pommette.scm}
}}

@exercise[#:difficulty "Medium"]{
  Read about the Meta-Object Protocol in CLOS @~cite{Kiczales1991},
  particularly the protocols for class redefinition and instance update.
  Compare the CLOS approach to the simpler models discussed in this chapter.
  What additional flexibility does the MOP provide?
  What are the costs of that flexibility in terms of
  implementation complexity and reasoning difficulty?
}
@exercise[#:difficulty "Medium"]{
  Implement method caching for generic functions:
  the generic function maintains an LRU cache of the last 8 times it was called,
  on what (tuples of) specifications it was called,
  and what effective method resulted.

  Harder: use macros to instead (or additionally) implement
  a 4-deep LRU cache of effective methods @emph{per (dynamic) call site}
  of a generic function.
}
@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{09to10}, compare
  your attempt at explaining implementation strategies for OO with mine.
  What aspects did you anticipate? What surprised you?
  What did you do better or worse?
}
@exercise[#:difficulty "Hard"]{
  Implement pure functional variants of vectors and hashmaps,
  in the style of Clojure Seq’s and Associative’s,
  or Coalton’s Seq and HashMap,
  or their Haskell equivalent, etc.
  Then implement a pure functional variant of a MOP on top of that.
}
