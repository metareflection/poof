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
@section{Representing Records}
One way or another, object-oriented programming involves dealing with @emph{records},
mappings from identifier to value,
that embody the results of modular computations (see @secref{MFCM}).
In previous chapters, I chose to represent records as functions,
which simplified many aspects of my conceptual presentation, including fixpoints and types.
However, in a practical system, the intent of records has always been
an efficient low-level representation as contiguous words of memory @~cite{Hoare1965}.
Most presentations of OO just introduce a low-level encoding for how their language implements objects,
and declare that’s how things are;
out of that, extremely complex low-level semantics emerge,
and you are left to untangle desired meaning from undesired noise.
I am taking a radically opposite approach: I instead am going to bridge this conceptual gap
by starting from the high-level semantics, and zooming down to practical implementations thereof,
the desired meaning being clear at all times.

@subsection[#:tag "RaF"]{Records as Functions}
@epigraph{
  To have a right to do a thing is not at all the same as to be right in doing it.
  @|#:-"G.K. Chesterton"|
}
@Paragraph{A Universal Strategy}
So far I have encoded Records as opaque functions
with some kind of identifier as input argument (symbols, in Scheme),
and returning an arbitrary value as output (@secref{MOO}).

This record representation strategy works, and
is portable to any language with Higher-Order Functions.
By using it, I have demonstrated a general recipe to implement OO
directly on top of the λ-calculus only,
which can itself be easily implemented on top of
any language or virtual machine past, present or future.
Indeed I have successfully deployed this very technique “in production”
to build a lightweight pure functional HTML authoring tool for my slides;
and so could anyone use that technique in a pinch to quickly build
extensible configuration generators, or an extensible data structure library
in an otherwise constrained environment.

Another advantage of this strategy is that it fitted nicely with the Y combinator:
the record itself was the fixpoint, its type was that of a regular function.
There was no need for complicated ad hoc wrapping and unwrapping with types
that are never 100% compatible from one language to the next,
and introduce language-specific record primitives with ever subtly different types,
the explanations for which would have burdened an already concept-heavy exposé.

@Paragraph{Issues with Records as Arbitrary Functions}
However, there are various drawbacks to representing records as arbitrary functions:
@itemize[
  @item{arbitrary functions are opaque, lacking introspection on which keys are valid@xnote[";"]{
    On the plus side, the opacity of arbitrary functions can be seen as a feature
    in the context of parametric behavior (initial semantics), and/or
    of denying introspective capabilities in some security-sensitive contexts.
  }}
  @item{everything being functions can make code harder to debug,
        with less distinctive error messages}
  @item{records as opaque functions will perform poorly in time taken or space occupied.}]

Indeed, if @emph{arbitrary} functions are accepted, then there is no trusting what
is in functions that you extend, no knowing what keys they handle,
no filtering those keys to keep or process those you want and remove those you don’t.
Extending a record-as-arbitrary-function to handle a binding from key to value
necessarily shadows any previous binding without removing it, and thus leaks space;
and the old bindings, though shadowed, will necessarily still be checked,
when doing a linear search through previously extended records-as-arbitrary-functions;
and the shadowed values associated to those keys, though no longer reachable that way,
will still be considered reachable by the garbage collection.

In the end, this representation strategy is only applicable for short-lived programs,
such as a generator for HTML documents or configuration files, as previously discussed,
for which it can help you get the job done quickly anytime anywhere on a small budget,
with no dependencies on libraries of any kind.
Larger or longer-running systems will require a better solution;
happily, for larger or longer-running systems,
one can afford the STEAM (Skill Time Energy Attention Money)
to invest in a better solution, such as the ones I discuss below.

@Paragraph{Not-so-Arbitrary Functions}
Now, functions do not have to be arbitrary.
They can follow some systematic pattern;
but then that pattern defines the @emph{actual} data structure,
whether the pattern is explicitly coded by the programmer,
or heroically inferred by the implementation, either statically or dynamically.
The function then is but a convenient interface (or not-so-convenient, depending on expectations)
to the underlying functionality, one that offers some modular abstraction
over different underlying representations also using a function as an interface
(however, try to combine differently built functions,
and you will soon be back to arbitrary functions as above).

For instance, a function could implement the same linear search for a key that I used,
but also maintain a list of bound keys, that is returned if a “magic” value is given as input
(the magic value being otherwise outside the set of keys that can be arbitrarily bound).
Or, assuming a totally ordered set of keys, the function could use
some balanced binary tree underneath instead of a linear search,
and there again provide introspection on the node structure when queried with magic values.
Your imagination is the limit, but notice a few things:
@itemize[
  @item{The existence of magic input values mean that what you have indeed is not “a function”
    from arbitrary value to arbitrary value: instead it is a function from the sum of two types
    (regular keys and magic messages) to a sum of two types (regular values and magic answers);
    which is indeed a conflation of two functions, each of which may be a “record” of sorts.}
  @item{The first function from regular keys to regular values behaves like
    our records-as-arbitrary-functions above, while the second conflated function,
    from “magic” messages to magic answers, provides some reflective interface
    with which you can probe the implementation underneath the first function.
    Reflection will be the recurring theme of this chapter.}]

At that point, the functional interface only adds a small constant space and time overhead
on top of whatever underlying data structure implementation is actually used underneath,
and otherwise provides the costs and benefits of the uniform functional interface
and its direct compatibility with the Y combinator.
Using this technique, introspection of keys is possible (though not necessary if unwanted),
as is “extrospection” (second-class or external knowledge of keys from outside the runtime),
if the data structure is maintained extralinguistically via a design pattern.
And through this introspection of extrospection,
the space and time leak of shadowed bindings can be avoided.

Now, to go further, I must strip the functional interface, and peer at what is hiding inside it.
Once I do, I may find that a function from key to value was not the best interface,
and not even the best functional interface.
And so I may not want to put that interface back afterwards.
But then, I won’t be able to use the Y combinator the same way I did;
I may be able to use the Y combinator still, through some indirection layer;
or I may be able to achieve the self-recursion intrinsic to objects
through a very different mechanism.
But first, let’s examine what was the data structure implicit
in chaining records-as-arbitrary-functions.

@subsection[#:tag "RaFM"]{Records as Finite Maps}

@Paragraph{From Implicit to Explicit Alists}
The only primitive that we could afford given arbitrary functions as records,
was the @c{extend-record} primitive, that added one binding to an existing record,
ultimately from the @c{empty-record}.
I could use the @c{case} primitive of Scheme to add several bindings at a time,
but that was only a minor optimization that didn’t change much in the end:
records are a chain of bindings that you consult with a linear search.
And thus the data structure implicit in that choice of constraint is the @emph{alist},
or association list: a singly linked list of pairs of a key and a value.

Alists are a “naïve” data structure, but not in a bad way:
they are quite a simple and obvious approach to think about, and indeed, reason about;
yet while not blazing fast, they do not suffer from a hidden flaw that make them terrible.
That is why alists are often used as the basis on which to model, formalize, implement and
study the properties of records, “environments” of variable bindings, and finite maps in general.
The main underlying structure of a singly linked list
(called just “list” in Lisp, Scheme, and
the standard library of most functional programming languages)
is also popular for the same reason to represent lists, finite sequences, sets, etc.

An alist works with any set of keys that you can compare for equality, and
has acceptable performance for short maps, even though it is not particularly efficient:
every random access is a linear search in @c{O(l)} where @c{l} is the length of list@xnote["."]{
  I will keep all my “performance estimates” as general complexity classes,
  under the simplified model of a flat memory with @c{O(1)} random access time.
  Once you delve into details of layers of memory from CPU registers to L1, L2, L3 caches,
  to local disk then remote server and its disk, you have to factor in a @c{O(√n)} slowdown
  (where @c{n} is the size of the “working set” of data one works with),
  to deal with the physical limits of the memory hierarchy @~cite{MythOfRAM2014}.
}
Even then, @emph{there is a clear advantage to explicitly using alists}
over implicitly letting their structure emerge from the use of @c{extend-record}:
by making alists explicit, one can actually remove shadowed entries,
and eliminate the space and time leak inevitable in the previous representation.

@Paragraph{Finite Maps Beyond Alists}
Stepping back from the particulars of an alist and its performance profile,
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

Now, finite maps can be more efficiently implemented using balanced binary trees,
that allow random-access operations in @c{O(log n)} instead of @c{O(n)}.
Comparing identifiers as strings can be somewhat expensive, however, and
it is sometimes faster to compare their unique addresses,
or their hash (which can be cached),
or a unique number assigned to them (guaranteeing no hash collision)@xnote[","]{
  Applications that rely on cryptographically secure hash functions may already
  be “guaranteeing” no collision: more precisely, they make collisions vanishingly unlikely,
  and the entire system’s security is already predicated
  on such collisions not occurring in practice,
  making it pointless to further protect against them.
  On the other hand, such cryptographically secure hash functions are expensive enough to compute
  compared to the “insecure” alternative that they are probably not worth using
  just for the no collision “guarantee”.
}
after having @emph{interned} the strings as symbols, as in Lisp or Scheme:
Lisp “symbols” are essentially pre-registered strings, in a global table,
to pre-pay the @c{O(l)} cost of string comparison once (where @c{l} is the length of the string)
and make the many subsequent comparisons for equality a cheaper @c{O(1)}@xnote["."]{
  Since the late 1970s, Lisp, as subsequently codified by Common Lisp,
  actually has a two-layer system, with a global table of @emph{packages},
  each named by a string, and containing a table of @emph{symbols}.
  Common Lisp also has a notion of @emph{uninterned symbols} that have a name,
  but are really meant to be compared by address.
  Scheme has a single-layer system of symbols;
  some implementations have uninterned symbols, but not all;
  on the other hand, most implementations have a notion of @emph{identifier}
  that is richer than a symbol, embodying some provenance information with the symbol
  so you can distinguish identifiers with a common name (a symbol) but different lexical identity,
  being keys to different bindings.
  Note that even though the underlying programming language may fail to provide you with
  a small unique id number for each symbol, your object system can do the assigning itself,
  at least for symbols used as field names, at the cost of another interning table
  mapping symbols to such numbers.
}
Particularly popular among pure functional balanced tree algorithms are tries
and their variants optimized for hash maps @~cite{Okasaki1998 Bagwell2001 Steindorfer2015},
or weight-balanced trees. @;{TODO cite https://github.com/dco-dev/ordered-collections/blob/021-specialized-ropes/doc/why-weight-balanced-trees.md}

For mutable records, or records used with a linear discipline,
a traditional mutable hash table or HashMap provides @c{O(1)} random access,
albeit with a constant factor that is typically one or two orders of magnitude larger
than a direct field access in a statically typed language.
My above remarks about hashing, interning, and unique numbering apply to mutable hash tables
as well as immutable ones.

@Paragraph{A Note on Power}
@epigraph{
                Every task involves constraint, @(linebreak)
              Solve the thing without complaint; @(linebreak)
                There are magic links and chains @(linebreak)
               Forged to loose our rigid brains. @(linebreak)
           Structures, strictures, though they bind, @(linebreak)
                 Strangely liberate the mind.
                 @|#:-"James Falen"|
}
One might find it remarkable that,
despite granting users the power to hide arbitrarily sophisticated
record implementation algorithms behind the interface of a function,
the records-as-arbitrary-function strategy led to a data structure
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
and you lose big on all subsequent operations everytime afterwards,
which is where the system actually spends most of its time and space.

The more general lesson is that strictures on data structures enable better performance.
More precisely, strictures on past operations create a space in which future operations
are free to be more efficient, whereas strictures on future operations create a space in which
past operations are free to be more efficient.
From this dual constraint emerge various tradeoffs
between the two kinds of strictures and freedoms.

Now, in general, the very same operations implemented by the very same algorithms,
will be used in both the past and future of a data structure, in an often unpredictable way.
Then, a balancing act is required from the programmer, to choose the compromises
that will maximize the benefits and minimize the cost of the software over its expected lifespan.

Then again, sometimes past and future are not wholly unpredictable:
there are clear-cut or at least statistically expected @emph{stages} to the computation,
during each of which the nature and frequency of operations used are different,
such that each stage would best benefit from its own specific choice of tradeoffs
that would not work well in other stages.

@subsection[#:tag "RaR"]{Records as Records}

@Paragraph{Class Disparities}

Now here comes a disconnect: I have established that
the semantics of first-class records is essentially that of a finite map,
with a known performance profile that is much worse than the idea
that everyone has of a record.
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
a static analysis can assign a fixed offset to each of these accesses.
But when implementing first-class records,
you must be prepared for newly defined records,
with newly defined sets of identifiers as keys,
accessed through fields labeled by dynamically computed first-class identifiers.

Thinking back about it, it wouldn’t have made sense
to implement first-class records with first-class records.
At best, it is a tautological identity that brings nothing;
or it’s a level of gratuitous wrapping and indirection that only brings overhead.
What it cannot bring is simplification or improvement.
Whereas implementing first-class records with second-class records,
and, these themsevles implemented with the memory arrays that lie underneath—that does make sense,
if it can be achieved.
Second-class records then are indeed a case as discussed above, where staging
enables computations before and after the stage transition (in this case, compilation)
to use slightly different implementation strategies, yielding various optimizations.

@Paragraph{The General vs The Common}

The @emph{general} case of first-class records cannot be simplified beyond these finite maps,
that are much slower than second-class records.
But the @emph{common} case of first-class records is about the same as for second-class records:
most of the time, the shape of the record can be known or guessed in advance,
and so can the identifiers used to label the fields being accessed;
then, a fixed offset can be computed in advance and cached.

@principle{An implementation must always support the general case} of what it is implementing,
and be ready to fall back to less efficient but more general algorithms.
@principle{But an implementation can rely on the common case},
and use a more efficient algorithm @emph{most of the time},
as long as it guards it with checks that indeed the preconditions
for the validity of the more efficient algorithm still hold.
And those checks can often be moved outside of performance-critical loops.
(Indeed in the case of second-class records themselves, the checks can be moved
wholly outside of the runtime, into the compile-time.)

Thus, an implementation for first-class records can represent records
as a second-class record plus a first-class record descriptor
that among other things perhaps, contains a mapping from field identifier to field offset.
Accesses of a constant field can use a cache of record offsets for the field
depending on recently seen record descriptors, to skip most of the computation
after checking that indeed it matches expectations.
Alternatively, or in addition, the mapping from field identifier to offset
given a record shape can be optimized once using
“perfect hashing” @;{ TODO cite
  Fredman1984 "Storing a Sparse Table with O(1) Worst Case Access Time"?
  Tarjan1979 "Storing a Sparse Table"? }
which at a one-time cost may speed up each subsequent access.

In the case of specifications with suffix specifications (@secref{OISMIT}),
where some of the offsets must be inherited, while others may be allocated via perfect hashing,
an indirection can be used, mapping identifiers to offset in a translation table
that yields the offset in the record; thus, interestingly,
support for suffix specifications slightly improves the performance of second-class record access,
at the expense of slightly worsening the performance of first-class record access!

@Paragraph{Some Concrete Encoding Considerations}

In any case, any host language you choose for your implementation
will offer some kind of underlying low-level memory arrays,
or of vector of nodes in an object graph,
that can be leveraged to implement records.
But every such host language will offer subtly different primitives,
that you then have to deal with:
offsets counted from 0 or 1 or some other constant due to reserved fields;
word-addressing or byte-addressing with various alignment constraints;
static or dynamic typing of the data being stored;
varying support for garbage collection, coherence or atomicity of access across threads;
etc.

In a practical implementation, you will seek the most performant primitive available to you.
In Scheme, that I use in this book,
some implementations offer suitable primitives, but nothing portable is defined.
For instance, Gambit Scheme exposes the internals of its “structure” machinery
with operators such as @c{##make-structure}, @c{##structure-ref} and @c{##structure-set!};
and Gerbil Scheme builds its object system on top of those.
But the closest equivalent you can do portably in Scheme is
to hijack vectors, the standard mechanism for one-dimensional array of arbitrary values,
as the underlying representation.
That is what I will do in the code accompanying this book.

I will not try to maintain “full abstraction” by
redefining user-level vectors and all their primitives
to be a subset of system-level vectors disjoint from the one I use to represent objects.
I will leave that as an exercise to the reader.
Presumably this problem disappears in the common case of using actual low-level primitives
of an implementation language separated from the target language
by compilation happening in a distinct evaluation stage.

Happily, managing the details of translating a high-level representation strategy
into a zoo of low-level primitives that change subtly with each host system
is a good task for modern AI to semi-automate.

@section{Representing Recursion}

@subsection{Where did the Fixpoint Go?}

@Paragraph{No Place for Fixpoints}
Even more so than records, OO involves open recursion through fixpoints.
Now, whichever specific data structure is used underneath to represent a finite map,
importantly, a data structure is an “inert” @emph{value}:
looking up bindings can be done in a “pure” way involving no meaningful side-effect,
and even adding, shadowing or removing bindings, involves no side-effect
except those specific to the data structure if mutable (and not even that if immutable).
In particular, there is no place in such a data structure for an arbitrary
@emph{computation} that a fixpoint may be part of.
The recursion through which modular extensions and OO in general are defined and resolved
must happen @emph{outside} of the data structure itself.

This is very unlike the case of records-as-arbitrary-functions that I was using previously:
calling the function could do more than consult a fixed list of bindings,
it could evaluate arbitrary computations (and, in Scheme, issue arbitrary side-effects).
And I specifically relied on some arbitrary computation happening when looking up bindings
to make that computation itself the fixpoint of a modular definition:
my computations would recursively refer to the @c{self} variables,
consult it for other bindings, store it in data structures,
compute along chains of inherited @c{super} methods
that could themselves recurse through @c{self} some more.

But now that the bindings themselves are organized as an “inert” data structure,
consulted through well-defined algorithms with no space for arbitrary computations,
the fixpoint will have to happen @emph{before} or @emph{after} the data structure is consulted,
depending on whether one used Y-encoding (@secref{MOO}) or U-encoding (@secref{CfUe}), respectively:
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
    the outer @c{U.} to the left of @c{Y = U.(.U)}.
    The recursion schema is being composed to the right with @c{U}—which corresponds to
    the inner @c{.U} to the right—before returning the data structure,
    but that is deferred self-application, and does not involve any fixpoint yet.}]

@Paragraph{Fixpoints are for Computations, not Values}
Either way, as mentioned in @secref{DSF},
fixpoints, and thus resolution of modular definitions and of modular extensions,
are essentially operations on @emph{computations}, and not quite on @emph{values},
where the distinction between the two, and the duality of the two,
is well explained by @citet{Levy1999}:
computations are active processes that may return a value of corresponding type,
while values are just inert results.
Now, values are embedded in the universe of computations, wherein to each value
corresponds a simple “pure” computation that just returns the value.
But there are many ways to embed computations in the universe of values:
@itemize[
  @item{Computations as thunks—functions from unit to the desired value type,
    with some set of acceptable side-effects.}
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
    and its side-effects may and will take place even if the computation isn’t explicitly invoked
    to wait for its results.}
  @item{Computations as futures—same as a fork, but invoking the computation is implicit
    when the result is needed, as with lazy vs delayed values.}]

An infinite number of more such embeddings can be devised.
For instance, some variants will be thread-safe using some expensive mutual exclusion primitive;
whereas other variants require users to enforce mutual exclusion manually or will misbehave;
some variants may involve one copy of the computation in each “process”, while others
may try to ensure some unique (or centrally managed) copy
of the computation in a single process on a given machine or across a given network;
further network variants may guarantee a number of redundant copies instead of a unique copy.

In a given fixpoint, including but not limited to the case of computing the target of
a modular or modularly extensible specification,
one of those embeddings must be chosen, with corresponding consequences on
the semantics of the fixpoint computation.
Each object system or library in each programming language,
may choose its own embedding, parameterize over it,
offer bridges between different such embeddings, etc.
Whichever strategy is chosen,
I will call @emph{suspension} the embedding of a computation into a value,
and @emph{outcome} of the suspension the value that results
from evaluating the computation to its end (if it terminates).
I will speak of the suspended computation, but also, by extension, of the suspended value
that is the outcome of the computation.
If the computation yields a value of type foo,
I will also call the computation a @emph{suspended foo}
(e.g. suspended record, suspended integer).
If the computation fails to terminate, trying to extract its outcome will also not terminate,
but will only produce the side-effects of the computation.

In my Scheme code, I will call @c{suspend} and @c{outcome}
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
or need to transform the code into monadic style to enable some side-effect in your object definitions.

@subsection[#:tag "SRoRoS"]{Suspended Records or Records of Suspensions}

Once one explicitly separates the issues of
representing records on the one hand, distinguishing records from their access functions,
and representing recursion on the other hand, distinguishing suspensions from their outcomes,
the simple formulas with a Y combinator of @secref{MOO} break down
to become slightly more elaborate, and an implementor of OOP must face some choices.
One notable choice is whether to put the suspended fixpoints before or after the record.

Let’s assume a Y-encoding to start with.
In the suspended record representation, a regular record of the method values
is being computed as a fixpoint, but the actual target value is
the suspension of this fixpoint computation, yielding the record as its outcome;
a suspended variant of Y is used.
@; TODO @Code{ ... } see pommette.scm
In the record-of-suspensions representation, the target is a record whose values
are suspensions that each yield the method value for the given key;
a suspended variant of Y can be used,
or an eager variant of Y specially allowing forward-reference for records (with language support).
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
(or @c{(record-ref half method-id hyper half)} for mixin and multiple inheritance).
Note that @c{half} itself, by requiring a call with @c{half} as argument (and maybe @c{hyper}),
is already a form of suspension. No further form of suspension is required.

@; TODO MORE CODE

@subsection[#:tag "RtM"]{Recursion through Mutation}

@subsubsection{A Popular Strategy to Implement Recursion}

Interestingly, mutation, the commonly available side-effect whereby
programs may mutate variable bindings or memory cells, introduces a popular
way to construct recursive definitions in two separate steps:
First, allocate some cell to which you can hold a stable reference@xnote["."]{
  I’ll speak of memory cell, to make mutation more explicit;
  but your programming language might make it easier to think in terms
  of mutable variables (as in Scheme), or mutable record (as in C).
  Indeed, mutable variables would have been slightly easier to deal with in Scheme;
  but my purpose is to make mutation semantics explicit, not to make its syntax easy.
  The reference is then the address of the cell in memory or in the persistent store,
  possibly coded or wrapped to fit the implementation and conventions of the programming language.
}
Then, use the reference to backpatch the value of the cell
in a way that implements the recursion schema.
The cell is said to be @emph{initialized} after this mutation is complete,
@emph{uninitialized} until then.

In this mutable paradigm, the reference provides access to the future recursively-defined object
even before it is fully initialized (i.e. all cells involved initialized),
much like the delayed computation in the immutable paradigm.
Meanwhile, the cell value, to be obtained by dereferencing the reference,
provides access to the actual recursively-defined object,
but only past complete initialization.
The same duality between computation and value, open future and closed past, exists,
as in the immutable paradigm.
But it might not be as obvious to a novice programmer introduced to a language with mutation
without a clear conceptual distinction between a variable (or memory cell),
a reference to said variable or cell, and the current content or value of the variable or cell;
indeed the very ergonomics of languages with mutation depends on
an easy punning between these subtly different but contextually easily disambiguated notions.

@subsubsection{Initialization Order}

Recursion-through-mutation involves a tradeoff in terms of complexity:
references to mutable cells provide direct access to recursive data structures
that are both very performant for the machine and quite ergonomic for the programmer;
but using them @emph{correctly} hinges on the programmer following a strict discipline
with respect to making sure every cell is always suitably initialized before it is used.

Failure to follow a proper initialization order can lead to catastrophic results,
especially when trying to dereference an uninitialized cell:
In a safe language like CLOS, the system will immediately raise an “unbound slot” error,
wherein you can easily diagnose where the problem happens and find a fix,
and even restart the program with the fixed code when developing interactively.
In a less safe language like Java or JavaScript, a slot accessed before it is initialized
will contain a “null” or “undefined” value, ticking time-bomb
that will explode at the programmer’s face long after the invalid access,
when the program eventually tries to use the value and fails
with a “null pointer exception” or some other error,
potentially wasting hours of the programmer’s time
identifying the culprit then fixing the bug,
but at least avoiding bigger issues.
In unsafe languages like C or C++, a slot accessed before it is initialized
can result in “Undefined Behavior”, invalid memory patterns being used,
pointers to nowhere or to the wrong place, silent memory corruption,
unsuspected incorrect results, vulnerability to attacks by malicious actors,
your money being stolen, your computer compromised, your AI subverted, etc.
A more subtle and happily rarer problem happens when a cell is initialized more than once
with inconsistent values, sometimes subtly so
(e.g. apparently equivalent values that lack some expected pointer equality),
causing difficult to explain failures later in the program,
or worse, unsuspected incorrect results.
Then again, concurrent object initialization
can introduce another category of difficulty: race conditions.

To avoid such costly failure modes, languages or libraries introduce
various initialization protocols.
But these protocols often fall short for being either
too rudimentary or too complex—or, sometimes, both at once.

A rudimentary protocol, as available in most languages,
will be easy for compilers to enforce and programmers to follow:
each constructor will have to explicitly and verbosely
call parent constructors in repetitive boilerplate,
in some constrained order with some constrained arguments,
with little latitude in what these calls can do.
But such a protocol has little room for intermediate computations,
forward references, circularities, or divergence of orderings among descendents;
thus programmers, forced to make decisions about ancestor initialization
before they have the necessary information from descendents,
soon find they have to leave fields uninitialized,
initialized with dummy values (like the infamous null),
or worse, with incomplete temporary values,
only to later mutate them with the actual initialized value.
Rudimentary protocols thus solve the easy problems while leaving programmers on their own
to solve the hard problems the hard way through extra layers of conventions—such as
“factory” classes, static factory methods, or the “builder” pattern.

More advanced protocols, such as
the @c{initialize-instance}, @c{shared-initialize} and related methods of CLOS
(that also offers a simple yet not verbose protocol
based on the @c{:initform} or @c{:initarg} of each slot,
and @c{:default-initargs} of each class),
allow for much more sophisticated orders of slot initialization,
that can involve the results of arbitrary intermediate computations,
indeed intermediate methods calls, etc.;
yet the more sophisticated such methods get,
the more discipline they will require from programmers, who are notoriously bad at it.
And even such sophisticated protocols will have limitations
that require the occasional use of uninitialized or null values.
They will still not be as flexible as the simple pure functional protocol we defined
when used with laziness.

@subsubsection{Initialization through Mutation is Too Low-level}
@epigraph{
  A programming language is low level when its programs require attention to the irrelevant.
  @|#:-"Alan Perlis"|
}
In our previous pure functional suspended protocol,
fields are bound to suspended computations that yield their values,
and these computations can in turn access other fields as well as inherited values.
This protocol can dynamically infer which field computations
depend on which other fields.
That makes the protocol maximally modular, since it enables maximal cooperation
between specifications with minimal coordination as to any initialization order,
and supports arbitrary changes in initialization order due to overrides.
Fields are defined using regular inheritance and overrides,
without the need for a separate initialization protocol.
The protocol can even detect circular dependencies@xnote["."]{
  A lazy initialization protocol can still diverge into stack or heap overflow
  due to infinite regress in producing more entities to initialize.
  But such infinite regress cannot happen if programmers stick to common patterns
  of static sets of identifiers without complex recursion;
  and the same complex recursion patterns can cause infinite regress
  even if tediously used in imperative languages.
}

An imperative initialization protocol, by contrast,
necessarily requires programmers to specially deal with more details,
as it separates object computation into allocation and initialization phases.
The imperative protocol is therefore comparatively @emph{low-level},
which might make sense in the relatively rare cases that you really care
about the performance of such initialization operations.
But for most programmers, in most cases, this performance does not matter enough
to justify having to care for all these irrelevant additional details.
And most high-level languages will offer escape hatches to deal with such details
in the rare cases that performance concerns justify the attention
(indeed, those who write and extend compilers eventually need get down to that level).

A deep reason why a proper initialization order can be quite difficult to define,
much less enforce, in such low-level imperative languages intended for performance-junkies,
is that compilers must statically generate simple binary code for each piece of source code
to achieve the intended performance.
But such simple static code by construction cannot handle subtle dependencies
of initialization that haven’t been specified yet;
it cannot topologically sort cells that haven’t been defined or overridden yet.
Yet such deferred definitions and overrides occur naturally in arbitrary OO code,
made of partial specification the initialization behavior of which
is to be extended and completed by further partial specifications.

First-class OO, and direct translation of second-class OO to first-class OO,
necessarily requires dynamic semantics;
and attempts to project such dynamic semantics into a static straight jacket
necessarily bring pain in proportion to the impedance mismatch.
But second-class OO, when each class is monomorphized by the compiler,
and the language allows for dependency-based re-ordering of slot defining clauses,
might topologically sort those definitions to accommodate a wider range of definitions
that avoid initialization to temporary null values
(assuming there is a @emph{static} such order, and not just a @emph{dynamic} one)—though
I don’t know of any OO system that has chosen this semantics.
Existing second-class OO systems tend to offer the worst-of-both-worlds
in terms of initialization protocols.

@subsubsection{Mutation-over-Purity}

Recursion-through-mutation provides a colloquial alternative to pure fixpoints
in languages with mutation.
But it is also a useful strategy to implement recursive data structures in pure functional languages!

Mutation can be expressed in a pure functional way, through a state monad
that explicitly passes around a global record as a “mutable store”,
the current mapping of identifiers to values.
Recursion-through-mutation can then be a valid implementation strategy
in a pure @emph{applicative} programming language
without any builtin primitive for lazy or delayed values.
This is notably the case when restricting yourself to a pure total strict subset of a language
so that you can reason inductively about programs:
the theorem prover ACL2 is based on doing exactly that in Lisp;
and this is a common style in systems like Rocq, Lean, F*, ATS, Agda, Idris, etc.
Mutable state, though awkwardly emulated,
enables the expression of forward references and circular definitions,
which would not be directly expressible in a pure applicative setting.

@XXXX{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

@section[#:tag "OL"]{Object Layout}

How do we arrange things in memory?
Field ordering and offsets.
Classes, explicit or inferred
(Self maps, V8 (Chrome) hidden classes, SpiderMonkey (Firefox) shapes, object descriptors,
inline cache structures) - adding fields dynamically (hash-consing; issue with suffix classes).
Prefix layout for slots of suffix classes.
Property storage (in-object vs external).

@section[#:tag "ED"]{Efficient Dispatch}

Object descriptor, vtables (works well with single inheritance / suffix classes), interfaces, etc.
Multiple dispatch.
Inline caching per call-site @~cite{Hoelzle1991}, caching per-function.
Method combinations: precomputing effective methods.
Sealing and devirtualization.
Type feedback and speculation—JIT at the atomic block level, not call site level.
Fast paths (statically or dynamicall compiled) and fallback (dynamic: deoptimization and bailout).
Caching and invalidation for dynamic class hierarchies.
Usual optimization techniques after having expanded the OO semantics;
but also typical OO patterns for what is static|dynamic{JITable|random}.

@section[#:tag "MOP"]{Meta-Object Protocols}
@epigraph{
  Metaobject protocols also disprove the adage that adding
  more flexibility to a programming language reduces its performance.
  @|#:-"Kiczales, des Rivières, Bobrow"| @; AMOP
}
@epigraph{We can solve any problem by introducing an extra level of indirection.
@|#:-`("Butler Lampson" , @~cite{WikiFTSE})|}
@epigraph{Almost all programming can be viewed as an exercise in caching.
@|#:-"Terje Mathisen, programming optimization guru"|}
@epigraph{There are two hard things in computer science:
  cache invalidation, naming things, and off-by-one errors.
@|#:-"Leon Bambrick"| @; http://martinfowler.com/bliki/TwoHardThings.html
}
What is a MOP?
Metaobjects: classes, slots, methods, generic functions.
Metaclasses: classes whose instances are classes.
Introspection vs intercession.
The MOP as a reflective tower.

@subsection{Reflection: Introspection and Intercession}

@section{Tower of Implementation}
Reflective towers (3-Lisp, Brown, etc.).
Reification and reflection.
Infinite towers vs truncated towers. Turtles all the way down, not hooks all the way up.
Collapsing towers for efficiency.
Partial evaluation and Futamura projections.

@citet{Chiba2000MetaHelix}.
Add serialization, persistence, to a meta-object, NOT to the object.

Different capabilities for objects and their meta-objects => more security.

@subsection{Side Effects}

I will revert to stateful OO, because I suspect
that's what my public is interested in, will use, and has in their underlying language.
But you can do it all with pure functional data structures if you want (see exercises below).

One man’s purity is another man’s side-effects.
You can have side-effects in the implementation of a pure language,
or a pure language implementing a stateful one.
Indeed, the transition can happen many times in a same program:
(a) some programmer writes a program in a language that for convenience,
provides stateful side-effects;
(b) the program is translated into some pure functional semantics in which
some security and robustness properties can be automatically specified and proven correct;
(c) the program is then implemented efficiently by translating linearity into side-effects,
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

@section{Bootstrapping an Implementation}
The chicken-and-egg problem. Metacircular definition.
At some level, “just” recursion and fixpoints.
Tying the knot: how does Object have a class?
Bootstrapping CLOS (or Smalltalk, or your system).
Staged bootstrapping.

@section{Interactions with lower-level subsystems}
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

@section{Finale: Putting It All Together}
How does it all connect?
A tour through an actual implementation: the Gerbil MOP.
What this book's accompanying code does (and doesn’t) do.

@exercise[#:difficulty "Easy"]{
  Read and make sense of the code I wrote for this chapter,
  that you may find e.g. at
  @url{https://github.com/metareflection/poof/blob/main/util/pommette.scm}
}

@exercise[#:difficulty "Medium"]{
  Read about the Meta-Object Protocol in CLOS @~cite{amop},
  particularly the protocols for class redefinition and instance update.
  Compare the CLOS approach to the simpler models discussed in this chapter.
  What additional flexibility does the MOP provide?
  What are the costs of that flexibility in terms of
  implementation complexity and reasoning difficulty?
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
