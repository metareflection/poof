#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 10)

@title[#:tag "EOI"]{Efficient Object Implementation}
@epigraph{
  Metaobject protocols also disprove the adage that adding
  more flexibility to a programming language reduces its performance.
  @|#:- "Kiczales, des Rivières, Bobrow"| @; AMOP
}

@section{Representing Records}

@subsection[#:tag "RaR"]{Records as Records}

@XXXX{XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX HERE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX}

So far we encoded Records as opaque functions
with some kind of identifier as input argument,
and returning a value as output.
This strategy works, and is portable to any language with Higher-Order Functions.
By demonstrating it, I have provided you with a general recipe to implement OO
that you can easily adapt to any language or virtual machine past, present or future,
However, (1) Records-as-Functions is rather inefficient in time, and, (2) it leaks space.
Therefore, while it can help you get the job done quickly anywhere on a small budget,
it is not meant as a permanent solution for long-running systems
where you have the STEAM (Skill Time Energy Attention Money) to do
a harder but better job for long term benefits.


Meanwhile, the name “record” itself, as per @citet{Hoare1965},
suggests a low-level representation in terms of consecutive words of memory.
And so, I will show how to better implement records.
The downside is that the techniques involved will be less portable,
and less readable without the kind of syntactic abstraction I cannot afford in this book.

A better strategy is possible.

It is possible to do better... but to actually efficient, I’ll have to write non-portable code.
the code won’t be trivially portable.
Your LLMs will help you.

Records as records? Now we’re talking.

@subsection{Lazy Records}



@section[#:tag "MOP"]{Meta-Object Protocols}
@epigraph{We can solve any problem by introducing an extra level of indirection.
@|#:-"Butler Lampson @~cite{WikiFTSE}"|}
@epigraph{Almost all programming can be viewed as an exercise in caching.
@|#:-"Terje Mathisen, programming optimization guru"|}
@epigraph{There are two hard things in computer science:
  cache invalidation, naming things, and off-by-one errors.
@|#:-"Leon Bambrick"|} @; http://martinfowler.com/bliki/TwoHardThings.html


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
