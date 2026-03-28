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

@subsection[#:tag "RaF"]{Records as Functions}
@epigraph{
  To have a right to do a thing is not at all the same as to be right in doing it.
  @|#:-"G.K. Chesterton"|
}
@Paragraph{A Universal Strategy}
So far I have encoded Records as opaque functions
with some kind of identifier as input argument,
and returning a value as output (@secref{MOO}).

This record representation strategy works, and
is portable to any language with Higher-Order Functions.
By using it, I have demonstrated a general recipe to implement OO
directly on top of the λ-calculus only,
which can itself be easily implemented on top of
any language or virtual machine past, present or future.
Indeed I have successfully deployed this very technique
to build a lightweight HTML authoring tool for my slides;
and so could anyone use that technique in a pinch to quickly build
extensible configuration generators, or an extensible data structure library
in an otherwise constrained environment.

Another advantage of this strategy is that it fit nicely with the Y combinator:
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
They can follow some systematic pattern, at which point
that pattern defines the real data structure,
whether the pattern is explicitly coded by the programmer,
or heroically inferred by the implementation, either statically or dynamically.
The function then is but a convenient interface (or not-so-convenient, depending on expectations)
to the underlying functionality, one that offers some modular abstraction
over different underlying representations also using a function as an interface
(though try to combine differently built functions,
and you will soon be back to arbitrary functions as above).

For instance, a function could implement the same linear search for a key that I used,
but also maintain a list of bound keys, that is returned if a “magic” value is given as input
(the magic value being otherwise outside the set of keys that can be arbitrarily bound).
Or, assuming a totally ordered set of keys, the function could use
some balanced binary tree underneath instead of a linear search,
and there again provide introspection as to the node structure when queried with magic values.
Your imagination is the limit, but notice a few things:
@itemize[
  @item{The existence of magic input values mean that what you have indeed is not “a function”
    from arbitrary value to arbitrary value: instead it is a function from the sum of two types
    (regular keys and magic messages) to a sum of two types (regular values and magic answers);
    which is indeed a conflation of two functions, each of which may be a “record” of sorts.}
  @item{The first function from regular keys to regular values behaves like
    our records-as-arbitrary-functions above, while the second conflated function,
    from “magic” messages to magic answers, provides some reflective interface
    with which you can probe the implementation underneath the common interface.
    Reflection will be the recurring theme of this chapter.}]

At that point, the functional interface only adds a small constant space and time overhead
on top of whatever underlying data structure implementation is actually used underneath,
and otherwise provides the costs and benefits of the uniform functional interface
and its direct compatibility with the Y combinator.
Introspection of keys is possible (though not necessary if unwanted),
as is “extrospection” (second-class or external knowledge of keys from outside the runtime),
if the data structure is maintained extralinguistically via a design pattern.
The space and time leak of shadowed bindings can be avoided.

To go further, I must strip the functional interface, and peer at was what inside it.
Once I do, I may find that a function from key to value was not the best interface,
and not even the best functional interface.
And so I may not want to put that interface back afterwards.
But then, I won’t be able to use the Y combinator the same way I did;
I may be able to use the Y combinator still, through some indirection layer;
or I may be able to achieve the self-recursion intrinsic to objects
through a very different mechanism.
But first, let’s examine what was the data structure implicit
in the chaining records-as-arbitrary-functions.

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

Alists are a “naive” data structure, but not in a bad way:
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
Even then, there is a clear advantage to explicitly using alists
over implicitly letting their structure emerge from the use of extend-record:
by making alists explicit, one can actually remove shadowed entries,
and eliminate the space and time leak inevitable in the previous representation.

@Paragraph{Finite Maps Beyond Alists}
Stepping back from the particulars of an alist and its performance profile,
the abstract interface it satisfies is that of a @emph{finite map}.
And indeed, the essence of a first-class record is a finite map:
a map from a finite set of keys to a set of values,
though those keys may be a subset of an infinite type.
Finite maps are also known in various programming contexts as
maps, dictionaries, associative arrays, associative containers, or sometimes,
environments (since we are binding values to @emph{identifiers}).

Now, finite maps can be more efficient implemented using balanced binary trees,
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
  Scheme has one has a single layer system of symbols;
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
and their variants optimized for hash maps @~cite{Okasaki1998 Bagwell2001 Steindorfer2015}.

For mutable records, or records used with a linear discipline,
a traditional mutable hash table or HashMap provides @c{O(1)} random access.
Remarks about hashing, interning, and unique numbering apply to mutable hash tables
as well as immutable ones.

@XXXX{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

@Paragraph{No Side Effects for an Internal Fixpoint}

Now, whichever specific data structure is used underneath to represent a finite map,
importantly, these data structures are “inert” values.
Computing the value associated to a key can be done by an algorithm
that always terminates in finite time.

No side-effects... including those required for a fixpoint.

Computation of X vs value of X @~cite{Levy1999}.


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
that is objectively @emph{worse} than the obvious naive solution.

Actually, that power is the very reason why
the @c{extend-record} function is so powerless:
The power of an arbitrary function, the programmer can choose to exercise once and only once,
for some base record;
thereafter, the algorithms that modify that record cannot assume anything
about the internals of the record being extended,
and are thus restricted in how they may extending such records;
all they can do is chain into the opaque record.
You win big once in the beginning where it doesn’t matter,
and lose big on all subsequent operations everytime afterwards.

The more general lesson is that strictures on data structures enable better performance.
More precisely, strictures on past operations create a space in which future operations
are free to be more efficient, whereas strictures on future operations create a space in which
past operations are free to be more efficient—with various tradeoffs
between the two kinds of strictures and freedoms.

Now, in general, the very same operations implemented by the very same algorithms,
will be used in both the past and future of a data structure, in an often unpredictable way.
Then, a balancing act is required from the programmer, to choose the compromises
that will maximize the benefits and minimize the cost of the software over its expected lifespan.

Then again, sometimes past and future are not wholly unpredictable:
there are clear-cut or at least statistically expected @emph{stages} to the computation,
during each of which the nature and frequency of operations used are different,
such that each stage would best benefit from its own choice of tradeoffs
that would not work well in other stages.


Since the first-class data structures

Record implementations that explicitly restrict the structure of records being extended
unlock the option for the extension operators to look inside the records being extended
and take advantage of the structure.
You have less power for initial record creation, but
much more for all the subsequent operations that will matter for your runtime performance.


@subsection[#:tag "RaR"]{Records as Records}

Implementing first-class records with first-class records is a tautology
that doesn’t actually bring anything.

I see what my neighbor has, and I want the same.

First-class vs second-class records.


second-class records as memory vectors plus some second-class mapping of symbol to index.
Now we’re talking.

Records as memory vectors.

The name “record” itself, as per @citet{Hoare1965},
suggests a low-level representation of data in terms of consecutive words of memory.
And so, I will show how to better implement records.
The downside is that the techniques involved will necessarily be
less portable than records-as-function.
But any implementation language you use will have some kind
of underlying low-level arrays of bytes, or of vector of nodes in an object graph,
that can be leveraged to implement records.

For a static implementation... no much more to add.
For a dynamic implementation... how to map identifiers to offsets?

It is possible to do better... but to actually efficient, I’ll have to write non-portable code.
the code won’t be trivially portable.
Your LLMs will help you.

Records as records? Now we’re talking.

For each shape (vector of names, or in reverse, mapping of names to slot index),
compute a perfect hash so that lookup is not just O(1) but actually quite fast.

@subsection{Lazy Records}



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


@subsection{Reflection: Introspection and Intercession}

@subsection{Bootstrapping an Implementation}

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

@subsection{Tower of Implementation}

@citet{Chiba2000MetaHelix}.

Add serialization, persistence, to a meta-object, NOT to the object.

Different capabilities for objects and their meta-objects => more security.


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
