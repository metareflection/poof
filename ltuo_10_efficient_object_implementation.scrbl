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

So far we encoded Records as opaque functions
with some kind of identifier as input argument,
and returning a value as output.
This strategy works, and is portable to any language with Higher-Order Functions.
But, (1) it is rather inefficient in time, and, (2) it leaks space.

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




@section{Meta-Object Protocols}

@exercise[#:difficulty "Medium"]{
  Read about the Meta-Object Protocol in CLOS @~cite{amop},
  particularly the protocols for class redefinition and instance update.
  Compare the CLOS approach to the simpler models discussed in this chapter.
  What additional flexibility does the MOP provide?
  What are the costs of that flexibility in terms of
  implementation complexity and reasoning difficulty?
}
