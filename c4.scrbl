#lang scribble/acmart @manuscript @anonymous @review @nonacm
@; -*- Scheme -*-
@; Default already: @10pt @natbib @screen @acmsmall
@; @anonymous @authordraft @authorversion @timestamp @review @nonacm

@; @title{C4: The best of single and multiple inheritance}
@title{Lambda, the Ultimate Object}
@subtitle{A theory of OO, and a modest contribution on optimal inheritance}

@author[
  #:email (email "fare@mukn.com")
  #:affiliation (affiliation #:institution @institution{@emph{MUKN, Inc.}}
                             #:country "USA")
]{François-René Rideau}

@abstract{
We discuss how best to combine single inheritance and multiple inheritance
in an Object-Oriented (OO) language.
For that, we first recapitulate the essential concepts of OO
that enable us to identify what “best” might mean to begin with:
what is inheritance, what is its purpose, what are known variants of it,
what makes each variant desirable or not, what are the relevant challenges with
using, implementing or combining them.
In particular, we discuss dependencies between prototypes (or classes),
(prototype or class) linearization, why linearization is desirable
and what are known desired properties of linearization.
Our solution extends the well-known C3 algorithm for linearization
with a way of combining multiple and single inheritance
that was previously adopted by Scala 3.
We call the resulting algorithm C4.
Importantly, we discuss why our solution preserves what matters about single inheritance
in the wider context of multiple inheritance,
even though there are other aspects of single inheritance it doesn’t preserve.
We compare our solution to those of other languages, most notably Common Lisp and Scala.
Overall, our contribution is as much in presenting the concepts of OO properly as in
setting that one last concept just right.
}

@(require scriblib/bibtex
          (only-in scribble/manual racket racketblock code codeblock litchar itemize item)
          (only-in scribble/example examples make-base-eval)
          (only-in scriblib/footnote note)
          (only-in scribble-abbrevs appendix)
          (only-in scribble-math/dollar $)
@;          scribble/minted
          syntax/parse/define
          "util/examples-module.rkt"
          "util/enumitem.rkt"
          "util/util.rkt"
          (for-label racket))

@(define-simple-macro (r a ...) (racket a ...))
@(define (anonymize x . y) x)
@(define (omega) "ω")
@(define (GerbilScheme) @anonymize["our Scheme implementation"]{Gerbil Scheme})

@;; Suppress the page count for the Camera-ready version by uncommenting the below.
@;@tex{\thispagestyle{empty}\pagestyle{empty}}
@;; Instead, we could set the start page for the document with:
@;@pageStart{42}

@(define-bibtex-cite "poof.bib" ~cite citet generate-bibliography)
@(define (~nocite . x) (let ((_ (apply @~cite x))) (void)))

@(define-simple-macro (defsection name tag text) (define (name (x text)) (seclink tag x)))
@(defsection section1 "Prototypes_bottom_up" "section 1")

@pretitle{
@tex{
\acmYear{2025}
\copyrightyear{2025}
\acmConference[Scheme]{Scheme Workshop}{2025}{online}
\acmISBN{978-1-xxxx-xxxx-x/21/10}
\acmPrice{00.00}
\acmDOI{https://dx.doi.org/xx.xxxx/xxxxxxx.xxxxxxx}
\setcopyright{none}
}}

@section[#:tag "Intro"]{Introduction}

@subsection{Our Contribution: An Optimal Inheritance Mechanism — and a Theory Why}

@subsubsection{Context: Inheritance}
Object-Oriented (“OO”) language are characterized by their use of
@emph{inheritance} @~cite{inheritance1996},
a mechanism to modularly and incrementally combine partial specifications of programs.@note{
A notable dissident to this characterization is William Cook,
a respected academic who made key contributions to understanding the semantics of inheritance
@~cite{Cook1989 cook1989inheritance bracha1990mixin Cook1994}
yet later argued that inheritance was orthogonal to OO @; Also Cook1989?
and that OO is about “classes” of “objects” that can only be accessed through “interfaces”
@~cite{Cook2009 Cook2012}.

However, coding against an SML “module” would count as OO by Cook's criteria,
and indeed Cook explicitly calls the untyped λ-calculus “the first object-oriented language”,
while dismissing Smalltalk as not OO enough because its integers are not pure objects@~cite{Cook2009}.
Cook's definition, that embodies the modular aspect of OO while rejecting
its incremental or dynamic aspect, runs contrary to all practice,
and brings no insight whatsoever on what people commonly call OO,
the many languages that provide it,
the common idioms, libraries and patterns on top of those languages.
It brings no light on any of the OO languages cursed by Cook as not actually being OO,
no light on any of the Functional Programming (FP) languages blessed by Cook as actually being OO
to the surprise of their users, and no light on the difference between the two.

Cook's many works on OO over the years also systematically paper over important concepts
in OO, such as prototypes, multiple inheritance, method combination or multiple dispatch.
In the end, while Cook's PhD and subsequent academic career grew out of
brilliantly modeling the key mechanism of OO (inheritance)
from the foreign point of view of FP,
his wilful ignorance and deep misunderstanding of the OO tradition, indeed missing the point,
were such that they have become proverbial: immortalized in Gabriel's essay
“The Structure of a Programming Language Revolution” @~cite{gabriel2012}
as a prototypical failure to understand a phenomenon when viewed
through a scientific paradigm incommensurable with the one that produced it.

In the end, Cook is well worth mentioning precisely to illustrate the lack of
common vocabulary, common concepts, and common paradigms among those
who practice and study OO, even or especially
among notable academics with deep expertise in the field.
And yet, there are undeniably common practices, common phenomena, common concepts,
worth understanding, conceptualizing, defining and naming
in the rich (and often mutually conflicting) traditions
that grew around OO.
Elucidating the key concepts hehind the hype and confusion is the main purpose of this paper.
}
Now language designers have faced a dilemma since almost the very beginning,
between several variants of inheritance:
some languages choose single inheritance for its simplicity and performance
@~cite{Simula1967 kay1996early},
whereas others prefer multiple inheritance
for its greater expressiveness and modularity @~cite{Bobrow1976 Cannon1979}.
A few outliers use mixin inheritance, a variant between the two
that can also be seen as more fundamental and composable @~cite{bracha1990mixin}.
@; XXX Reverse Inheritance as trivially expressible in terms of mixin or multiple inheritance.

@subsubsection{Problem: Optimal Inheritance?}
Is there a form of inheritance that is objectively better than the others,
with respect to expressiveness, modularity, incrementality, and runtime performance?
Is one of the usual variants superior to the others in every way?
If not, is there a combination of them, or a superset of them, that is?
Some languages notably combine both single inheritance and multiple inheritance,
though with some constraints @~cite{cltl2 scalableComponentAbstractions2005};
would the best way to do inheritance subsume these combinations?
Importantly, multiple inheritance usually involves a computation called
the @emph{linearization} of the inheritance graph.
Is that linearization necessary?
What becomes of it in this optimal inheritance?

@subsubsection{Claim: A Theory of OO}
So that we may offer a solution to the problem of optimal inheritance,
we will first quickly recapitulate a theory of OO sufficient to state the problem
and the criteria that may make a solution better than others.
In a way, we will only be stating lore that has been known for decades.
Yet no element of that lore is uniformly known to all;
some may never have been told explicitly in academic literature;
and they have never been formulated together.
What more, our theory does not merely restate the obvious,
but shapes it into a coherent whole that is @emph{productive}:
capable of generating and justifying new knowledge,
and not merely explaining existing one.


@subsubsection{Claim: C4 is Optimal}
We claim that (a) indeed there is a best way to combine single and multiple inheritance,
that (b) indeed it involves linearization of the inheritance graph,
that (c) there are enough constraints on linearization for the optimal algorithm
to be well-defined up to some heuristic, and
that (d) even then there are good reasons to prefer a specific heuristic.
We call C4 the resulting algorithm, that we implemented, and that
is included in the next release of @(GerbilScheme)
as part of its builtin object system.@note{
Scheme @; XXX CITE lambda-the-ultimate r4rs r7rs-small
is a language with a rather minimalist definition.
There are dozens of implementations of the language
(with various degrees of compliance to the various standards),
and each implementation provides its own extensions on top of this common core,
to offer a usable programming environment.
The extensions are usually mutually incompatible,
though there are trends, similarities, variants of shared code between the many implementation,
and even standards for some of these extensions.
In particular though, there is no standard object system,
instead plenty of different systems that span the entire design space for OO—except for
generally lacking static types.
}

Once our theory of OO is (re)stated, the C4 algorithm and the justification of its benefits
should seem rather obvious, though still novel.
Yet without that (re)statement, neither the algorithm nor its benefits would be obvious.
C4 is thus a comparatively modest yet meaningful innovation on top of our theory,
that crowns the theory as productive.

@subsection{Plan of the Article}

In section 2, we dispel common misconceptions about OO,
to ensure that our theory isn’t met with misunderstanding due to misconceptions
or disagreements about what is being theorized.

In section 3, we provide a quick summary of the issues at stake
with Object Orientation, and the three variants of Inheritance in common use:
what are the important concepts, and why they matter.

In section 4, we examine the known consistency constraints that matter
for linearization algorithms in the context of multiple inheritance,
and state of the art in satisfying them, the C3 algorithm.

In section 5, we describe the state of the art in combining
multiple and single inheritance,
with the solutions respectively adopted by Common Lisp @~cite{cltl2}
and Scala @~cite{scalableComponentAbstractions2005}.

In section 6, we propose a linearization algorithm we call C4,
that satisfies all the constraints we discussed for a good linearization algorithm,
and for combining single and multiple inheritance.
We explain why the residual heuristic we also adopt from C3 is arguably the best one.

@;{In section 6, we examine how our algorithm behaves on a few examples
lifted from relevant literature.}

Finally, in section 7 we conclude by recapitulating our findings.

@subsection{A note on nomenclature}

As we restate well-known and lesser-known lore of Object Orientation,
we will endeavour to precisely define the terms we use in the rest of the article.
Defining terms is especially important since various authors
from the communities around each of many OO language
each use slightly different intersecting terminologies
that often use different words for the same concepts,
or, much worse, the same words for different concepts.
This tower of Babel can cause much confusion
when trying to communicate ideas across communities,
as people ascribe opposite presuppositions and connotations to the words
used by other people, and talk past each other
while incorrectly believing they understood what the other said.

Thus, when multiple nomenclatures conflict, we will give precedence to
the @emph{least ambiguous} word for a concept,
even if neither the most popular word for the concept, nor the oldest,
even if we sometimes make it up just for this article.
The words we choose will hopefully cause readers to pause and reflect,
rather than unwittingly misunderstand the sometimes subtle points we make
due to a treacherously familiar word.

In particular, we will conspicuously avoid using the unqualified word “class”,
because it connotes for each reader, depending on the traditions he has adopted,
a different set of assumptions, that are parasitic to the theory we are laying out.
We will also reject the word “class” to mean the most general kind
of entity subject to inheritance, since that entity is actually a @emph{prototype},
of which a class is but a quite limited special case.

@; TODO for submission, move to appendix?
@section{What Object-Orientation is @emph{not}}

Before we explain in detail what OO @emph{is},
let us cast aside a lot of things it @emph{isn’t}
that too many people (both proponents and opponents)
too often false identified with OO.
This is important, because attempts to explain a theory of OO often fail
due to the reader having wrong expectations about what OO is supposed to be.

@subsection{Whatever C++ is}

At one point the most popular OO language,
C++ indeed enables programming in OO style to a large degree;
yet it is not at all representative of how other OO languages work,
and colloquial C++ often goes against the principles of OO@note{
Alan Kay famously declared at OOPSLA '97, near peak C++ popularity:
“I made up the term ‘object-oriented’, and I can tell you I didn’t have C++ in mind.”}.
Therefore, if what you know of “Object Orientation” comes from C++,
please put it aside, at least while reading this article, and come with a fresh mind.

This is especially true with regard to multiple inheritance,
that will be an important topic of this paper.
C++ boasts support for multiple inheritance, and many people,
when thinking of multiple inheritance, think of what C++ offers.
Yet what C++ calls “multiple inheritance” is not at all the same as
what everyone else calls “multiple inheritance”:
while C++ supports single inheritance well, what it calls “multiple inheritance”
is actually a modified kind of mixin inheritance with automatic renaming (for non-@r[virtual] classes),
and a subset of multiple inheritance (for @r[virtual] classes and members).
Moreover, C++ crucially lacks the proper method resolution
that enables a lot of the modularity of multiple inheritance in other languages.

Now, you can use C++'s powerful template language to reconstitute actual multiple inheritance
on top of C++'s weird variant of inheritance@~cite{smaragdakis2000mixin};
but this technique is quite uncolloquial, syntactically heavy, slower than the colloquial ersatz,
and you have to manually compute and chain your class precedence list,
which cancels some of the modularity benefits of multiple inheritance
versus single and mixin inheritance.

Finally, and at the very least, consider that
unless you explicitly tag your classes and their members @r[virtual],
C++ will deliberately eschew the “dynamic dispatch” of OO
and use “static dispatch” instead for the sake of “going fast”.
In the end, C++ is many great and not-so-great things, but only few of those things are OO,
and even those that look a lot like OO are often different enough that
they do not reliably inform about OO in general.

@subsection{Classes Only}

Many claim that classes, as introduced by Simula 67@~cite{Simula1967}
(though implementing a concept previously named by Hoare@~cite{hoare1965record}),
are essential to OO, and only ever care to implement, use, formalize,
study, teach, propagandize, or criticize class-based OO (a.k.a. Class OO).
Books and book chapters about OO from summities about Programming Languages
seldom even mention any other kind of OO, much less study it.

Yet KRL@~cite{Winograd1975},
the second recognizably OO language and the first language
whose authors applied the words “inheritance” and “prototypes” to describe it,
has prototype-based OO (a.k.a. Prototype OO).
On the other hand, the modern concept of OO
can be traced back to Smalltalk adopting inheritance in 1976
and popularizing the word and concept of it among programming language designers;
and Smalltalk was class-based.
Yet contemporary with Smalltalk and immediately after it
were prototype-based languages Director @~cite{Kahn1976 Kahn1979} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}@note{
ThingLab was built on top of Smalltalk, and oscillated between
having or not having classes in addition to prototypes.}.
Plenty more Prototype OO or “class-less” OO languages followed
@~cite{Rees82t:a adams88oopscheme chambers1989efficient Lawall89SelfInScheme Salzman05prototypeswith jsonnet nix2015 poof2021}.
There are lot more Prototype OO languages than we could have time to review @~cite{WikiProto},
but prominent among them is JavaScript @~cite{eich1995javascript},
one of the most used programming language in the world @~cite{TopPL2022},
maybe the top one by users
(though it recently also adopted classes on top of prototypes @~cite{EcmaScript:15}).

What more, we will argue below that Prototype OO @~cite{Lieberman1986 Borning1986}
is more general than Class OO, that is but a special case of it.
And we will even argue that you can recognizably have OO with neither prototypes nor classes.

It is therefore just wrong to dismiss Prototype OO as not being part and parcel
of the OO tradition, historically, conceptually, and popularly.

Now of course, classes, while not @emph{essential} to OO,
are still @emph{important} in its tradition.
The situation is similar to that of types in Functional Programming (“FP”):
the historical preexistence and continued use of the untyped λ-calculus
and the wide adoption of dynamically typed functional languages like Scheme or Nix
are ample evidence that types are not essential to FP;
yet types are undoubtly an important topic that occupies much of the theory and practice of FP.
Actually, the analogy goes further since, as we’ll see,
classes are precisely an application of OO to types.

@subsection{Imperative Programming}

Many people assume that OO requires mutation,
wherein all attributes of all objects should be mutable, or at least be so by default,
and object initialization must happen by mutation.
Furthermore, they assume that OO requires the same applicative (eager) evaluation model
for procedure calls and variable references as in every common imperative language.
@; TODO{CITE? C++ Perl5 Python Java JavaScript Scala Ruby Go (see GitHub)}
Meanwhile, many have of late claimed that purity (the lack of side-effects including mutable state)
is essential to FP, making it incompatible with OO.
Some purists even argue that normal-order evaluation (call-by-name or call-by-need)
is also essential for “true” FP, making it (they say) even more incompatible with OO.

However, there are many good historical reasons,
having to do with speed and memory limitations at both runtime and compile-time,
why early OO and FP languages alike, from the 1960s to the 1980s,
as well as most languages until relatively recently,
were using state and side-effects everywhere, and an eager evaluation model, at least by default.
And with early 1980s slogans like “objects are a poor man’s closures” and
“closures are a poor man’s objects”@~cite{adams88oopscheme},
the problem back then was clearly not whether OO could be done purely with functions,
but whether it made practical sense to program purely without side-effects in general.
That question that would only be slowly answered positively, in theory in the early 1990s
and in practice in the mid 2000s to mid 2010s, as Haskell grew up to become a practical language.
@; darcs 2003, cabal 2005, bytestring 2005, “cabal hell” 2006, ghc6 2006, pandoc 2006, xmonad 2007,
@; “Real World Haskell” 2008. Stack 2015 “made non-trivial haskell programs & scripts repeatable”
@; There’s obviously a lot of subjectivity there—but I expect an S curve such that
@; whichever arbitrary threshhold criteria you choose the answer would be at about the same time.

Yet, there are (a) pure models of OO such as those of
Kamin, Reddy, Cook and Bracha@~cite{Kamin1988 ObjectsAsClosures Cook1989 bracha1990mixin},
(b) pure lazy dynamic OO languages such as Jsonnet or Nix@~cite{jsonnet dolstra2008nixos},
and pure lazy OO systems for Scheme@~cite{poof2021}
@; TODO maybe mention foreshadowing by Oleg Kiselyov ?
and (c) languages happily combining OO and FP such as Common Lisp or Scala
@;TODO cite ScalaZ, etc.
with plenty of libraries restricting themselves to pure functional objects only.
These provide ample evidence that OO does not at all require mutation,
but can be done in a pure setting, and is very compatible with FP, purity,
and even with laziness and normal-order evaluation.
@; Haskell typeclasses embody half of OO@~cite{typeclasses LIL2012},
@; and could be tweaked to embody all of it.
@; though its designers might not wholly embrace the OO tradition. @; TODO{CITE}
@; TODO: add inheritance to Haskell typeclasses or Rust traits, or just cite CL gf, Clojure protocols.
Actually, we will argue based on studying of the semantics of OO that
pure lazy functional programming is the natural setting for OO.

@subsection{Encapsulation}

Many OO pundits claim that an essential concept in OO
is “encapsulation” or “information hiding”@~cite{DeRemerKron1975},
though there is no consensus as to what this or these concepts mean,
and no clear definition. @; TODO{CITE}

Inasmuch as “encapsulation” informally denotes but part or all of modularity,
the ability to code against an interface,
with code on either side not caring which way the other side implements its part of the interface
(or not even being able to distinguish between multiple such implementations),
then yes, of course, this is an essential part of OO, as per our definition.
Some may also call this concept “data abstraction” or some other kind of “abstraction”.
@; XXX cite Liskov???

However, inasmuch as some people identify encapsulation as the presence
of specific visibility mechanisms such as found in C++ or Java
(with some attributes or methods being @r[public], @r[private] or something in–between),
we’ll easily dismiss such mechanisms as not actually essential to OO,
since many quintessential OO languages like Smalltalk or Common Lisp
lack any such specific mechanism,
whereas many non-OO languages possess mechanisms to achieve the same effect,
in the form of modules defining but not exporting identifiers
(e.g. not declaring them @r[extern] in C),
or simply lexical scoping@~cite{rees1995}.
@; TODO{cite Simula? JS?}

Now, these mechanisms themselves can be very useful,
worthy features to add to an OO language, to use and study, etc.
They are just not essential to OO and not specific to it,
though of course their adaptation to OO languages will follow
the specific shape of OO constructs not found in non-OO languages.
And misidentifying OO as being about these mechanisms rather
than about the modularity they do or do not support can only lead to
sacrificing the ends to the means.

@subsection{Inheritance as opposed to Composition}
Some argue that there is an essential conflict between OO and FP,
between Inheritance and Composition,
wherein OO is about model every possible domain in terms of inheritance,
especially so where it can be preferred compared to FP and composition.

But OO and FP are just distinct concepts neither of which subsumes the other,
that thus fit distinct sets of situations.
@;Each distinct concept has its set of situations that it fits,
@;distinct from that of any other concept (or else they are actually the same concept);
@;a concept that fits all situations has no content and is useless;
@;and two concepts like OO and FP neither of which subsumes the other,
@;cover sets of situations neither of which is a subset of the other.
It makes no sense to oppose them, especially not when we see that
OO can be expressed in a few lines of FP, whereas
most modern OO languages contain FP as a subset,
and Lisp has harmonously combined OO and FP together since both their emergences in the 1970s,
decades before anyone had the idea to invent a conflict between the two.

The argument is actually a distortion of a legitimate question of OO design, @; TODO cite
wherein one has to decide whether some aspect of a class (respectively prototype or pattern)
embodied as attributes or methods, should be included directly in the class
(a) by inheriting from another class defining the aspect
(the class @emph{is-a} subclass of it — inheritance of classes), or
(b) indirectly by the class having as an attribute an object of that other class
(the class @emph{has-a}n attribute that is it —
composition of classes seen as constructor functions).

The answer of course depends on expectations about how the class will be further specialized
within a static or dynamically evolving schema of data structures and algorithms.
If the schema is small, static, well-understood and won’t need to evolve,
it doesn’t really matter which technique is used to model it.
But as it grows, evolves and boggles the mind,
a more modular and incremental approach is more likely to enable adapting the software
to changing situations, at which point thoughtful uses of inheritance can help a lot.@note{
@emph{Is} a car a chassis (inheritance),
or does it @emph{have} a chassis while not @emph{being} it (composition)?
If you’re writing a program that is only interested in the length of objects,
you may model a @r{car} as a @r{lengthy} object with a @r{length} slot,
and a @r{chassis} too. Now if your program will only ever be interested
but in the length of objects, you may altogether skip any object modelling:
and only use numeric length values directly everywhere for all program variables.
Is a car a chassis? Yes, they are both their length, which is the same number,
and you may unify the three, or let your compiler’s optimizer unify the two variables
as they get initialized them from the same computation.
Now if you know your program will evolve to get interested in
the width of objects as well as their length,
you might have records with length and width rather than mere numbers,
and still unify a car and its chassis.
But if your program eventually becomes interested in the height, weight or price of objects,
you’ll soon enough see that the two entities may somehow share some attributes
yet be actually distinct: ultimately, both @r{car} and @r{chassis} @emph{are} @r{lengthy},
but a @r{car} @emph{has} a @r{chassis} and @emph{is not} a @r{chassis}.

There is also an old slogan of OO design, notably found in GoF @~cite{GoF1994},
that you should “favor object composition over class inheritance”.
GoF argues not to create an exponential number of subclasses
that specialize based on static information about what is or could be a runtime value,
because classes are compile-time and human-developer-time objects
that are less flexible and costlier in human effort than runtime entities.
These arguments of course do not apply for regular Prototype OO,
wherein umpteen combinations of prototypes (and classes as a particular case)
can be generated at runtime at no additional cost in human effort.
Still, the point can be made that if a programmer is confused about which of is-a or has-a
to use in a particular case, it’s a good heuristic to start with has-a,
which will quickly lead to an obvious showstopper issue if it doesn't work,
whereas picking is-a where has-a was the better choice can lead to a lot of complications
before it is realized that it won't work right. Yet it's better to understand the difference
between “is” and “has”, to understand the domain being modeled, and to use the correct one.
In any case, this slogan has nothing to do with OO vs FP,
it's about using OO effectively or not.
}

@subsection{Message Passing}
Alan Kay, who invented Smalltalk and coined the term “Object-Oriented Programming”
notably explained@~cite{Kay2020} that by that he originally meant
a metaphor of computation through independent (concurrent, isolated) processes
communicating by passing asynchronous messages.
This metaphor also guided the modifications originally
brought by Simula to Algol@~cite{Simula1966}.
It is also present in notable early object systems such as
Director @~cite{Kahn1976 Kahn1979} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}.

However, neither Simula, nor Smalltalk nor any claimed “OO” language
actually fits that metaphor, though Simula comes closer than its successors.
Instead, the only commonly used language ever to fit this metaphor
is Erlang@~cite{OOP2010};
yet Erlang is not part of the OO tradition, and its authors have instead
described its paradigm as “Concurrency-Oriented Programming”.
Meanwhile the theory of computation through message-passing processes
was studied with various “process calculi”, @; TODO cite pi calculus, join calculus, rho calculus, etc.
that are also foreign to the OO tradition,
and largely unembraced by the OO community.
Indeed Erlang lacks a form of dynamic dispatch to embody the “late binding”
that Alan Kay also once mentioned was essential for OO@note{
In Erlang, each process is a dynamic pure applicative functional language
enriched with the ability to send and receive messages to and from other processes.
Now, as we’ll see, you need fixed-points to express the semantics of OO;
but in a pure applicative context, you cannot directly express sharing the results of a computation,
so the pure fixed-point combinators lead to exponential recomputations as the fixed-point
involves deeper self-references. OO therefore cannot be supported directly within Erlang.
It could be achieved indirectly, by restricting it to compile-time (as in most static class OO),
or by using a global cache (an in-process table, or one spawning one process per lazy computation)
which would also require some reimplementation of garbage collection for lazy computation caches.
Neither solution would qualify as supporting OO any the more than
assembly language “supports” OO or any Turing-universal language “supports” any paradigm, though.
In the end, OO, which is Prototype OO, is essentially a pure lazy functional paradigm,
and in any other paradigm but indirectly support it.
}

Moreover, many OO languages generalize and extend their method dispatch mechanism
from “single dispatch” to “multiple dispatch”@~cite{
  bobrow86commonloops bobrow88clos CecilMultimethods allen2011type}.
Their “multimethods” are attached to tuples of prototypes or classes,
and there is no single prototype, class, or single independent entity of any kind
capable of either “receiving” or “sending” a message.
Instead, they are attached to a “generic function”
that handles the dispatch based on the types of its arguments@note{
  The “generic function” functionality from in the Common Lisp Object System (CLOS) @; TODO cite
  can be viewed as isomorphic to the “protocols” functionality of Clojure.
  They would in turn be isomorphic to the “typeclasses” of Haskell or the “traits” of Rust, @; TODO cite
  if only these latter two suppored inheritance.}.
While multimethods are obviously not essential to OO, they are a well-liked, age-old extension
in many OO languages (CLOS, CECIL, Dylan, Fortress, Clojure, Julia)
and extensions exist for C++, Java, JavaScript, TypeScript, C#, Python, Ruby, etc.
@; TODO cite stroustrup2007multimethods https://en.wikipedia.org/wiki/Multiple_dispatch
The “message passing” paradigm, having no place for multimethods,
thus falls short compared to other explanations of OO that accommodate them.
Now, the message passing paradigm can be extended with a notion of “group messaging”
where one object sends a “message” to a “group” of objects (rather than each member of the target group)
@; TODO cite ABCL group mesaging
or to a “chemical” paradigm where a “chemical reaction” may involve
multiple entities in and multiple entities out, with “message” entities
conveying the changes in intermediary steps. @; TODO cite CHAM
But even with these extensions to the paradigm,
you would still have to somehow shoe-horn incrementality in
for methods to fit, whether with single dispatch or multiple dispatch.

In conclusion, whatever historical role the paradigm of message-passing processes
may have had in inspiring the discovery of OO,
it remains a distinct enough paradigm,
with its own mostly disjoint tradition and very different concerns,
that describes a different set of languages and usage patterns.

@subsection{Modeling the World}

Some have claimed that OO is meant to be @emph{the} way to model the world,
often in association with the concurrent message passing model
we already established above was not quite OO,
or with some class-based OO framework they sell. @; cite UML

However, while OO can indeed be of great use in modeling a lot of problems,
especially where the modeling language needs modularity and incrementality,
it by no means is supposed to be a Theory of Everything that subsumes
Quantum Mechanics, Category Theory, Darwinism, Aristotelian Poetics, etc.
Even if we stick to software, there are plenty of paradigms other than OO
that can explain or generate software and that OO does not subsume:
functional programming, logic programming, machine learning,
relational databases, reactive programming, temporal logic,
concurrent programming, dataflow programming, homomorphic encryption, etc.
Inasmuch as OO languages can be used to implement any of these paradigms,
so can any Turing Tar-Pit. And inasmuch as any of these paradigms
can be usefully extended with OO, that does not make them a subset of it.
People seriously studying OO should not take at face value the claims of
Snake Oil and Silver Bullet salesmen, either about what their products can do,
or about whether these products indeed embody OO. Mostly, they do not.

Consider UML and similar modeling methodologies involving
drawing diagrams of relations between classes and claiming to be OO.
Beside the fact that classes are not essential to OO as seen previously,
UML and similar languages do not even meaningfully have classes:
there is no semantics to inheritance,
especially in presence of fields that recursively refer back to a class;
should the child class have a link to the parent class or to the child class?
In absence of recursion, UML is more akin to relational data modeling than to OO.
In presence of recursion, UML fails to distinguish between subclassing and subtyping,
a gross logical inconsistency, that might have been forgivable in the 1980s,
but by 1990 it was clear that subclassing and subtyping were fundamentally different things, @; CITE
long before the authors of UML proceeded to ignore basic logic.
More broadly, the precise semantics of OO, of inheritance,
of method resolution in computing properties of objects along their class hierarchies, etc.,
are generally absent from these methodologies.
The deeper problem with them lies precisely because,
unlike Functional Programming or Category Theory,
they are unable to deal with abstractions, functions, type parameters,
side-effects, higher-order reasoning, and more broadly with logical rigor or
anything that has the precision required to specify code that can actually run.
In fact, these handwaving methodologies seem specifically designed to make
those incapable or unwilling to wrestle with logic
believe their understand all there is to know about software.
Yet the nature and correctness of software lies precisely
in this gap they are unable or unwilling to explore.

For an actual theory of types for data and code,
and the relationships between those types and the elements in those types,
one should rather look into Type Theory, Category Theory, Linear Logic,
Computability Logic, Abstract Algebra, and all kind of theories
that take logic seriously.
OO can be used to incrementally describe the structures in those theories,
and the more advanced of these theories can be used to describe OO.
Then you'll see that using OO to describe types
necessarily involves operators with type parameters and fixed-points thereof,
both crucial notions that are glaringly absent from UML and its ilk
(or indeed from the simpler variants of Type Theory and such,
but they don’t claim to be theories of all software).

In the end, if you care about modeling the types in your software (and you often should),
write your software in a language with a strong static type system that is logically consistent,
or at least, one whose inconsistencies have been well mapped and can be avoided.
Where you must use a language that does not have such a type system,
you can still annotate your functions with types in comments,
or in metadata actually used for debugging or optimization.
Whether with Scala, Rust, Haskell or ML, or even just Java or C++,
this will do all that the modeling of any methodology can, and much more,
except in a precise and meaningful way that you can actually reason about.
And in the case of Scala or C++ at least, it can actually be OO, too.


@(generate-bibliography)
