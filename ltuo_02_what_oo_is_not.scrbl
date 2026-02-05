#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 2)

@title[#:tag "WOOin"]{What Object Orientation is @emph{not}}
@epigraph{
  It’s not ignorance that does so much damage;
  it’s knowing so darned much that ain’t so. @|#:- "Josh Billings"|
}
Before I explain in detail what OO @emph{is},
I shall cast aside a lot of things it @emph{isn’t}
that too many people (both proponents and opponents)
falsely identify with OO.
This is important, because attempts at building or explaining a theory of OO often fail
due to authors and readers having incompatible expectations about what OO is supposed to be.

If you find yourself shocked and in disagreement, that’s fine.
You don’t have to agree at this point.
Just consider that what I call OO and discuss at length in this book
may be something slightly different from what you currently call OO.
Then please allow me to narrow down what I mean, and make my argument.
Or don’t and close this book.
But I hope you’ll give my ideas a fair hearing.

@section{OO isn’t Whatever C++ is}
@epigraph{
  I made up the term ‘object-oriented’, and I can tell you I didn’t have C++ in mind.
  @|#:- @elem{Alan Kay, at OOPSLA ’97 (near peak C++ popularity)}|
}
The most popular OO language in the decades that OO was a popular trend (roughly 1980 to 2010),
C++ indeed supports some form of OOP.
But C++ is a rich language with many aspects completely independent of OO
(e.g. efficient bit-banging, RAII, template metaprogramming, pointer aliasing, a memory model),
whereas the OO aspects that it undoubtedly offers
are very different from how OO works in most other OO languages,
and colloquial C++ often goes against the principles of OO.
Therefore, C++ is in no way representative of OO in general, and
if what you know of “Object Orientation” comes from C++,
please put it aside, at least while reading this book, and come with a fresh mind.

This is especially true with regard to multiple inheritance,
that will be an important topic later in this book.
C++ boasts support for multiple inheritance, and many people,
when thinking of multiple inheritance, think of what C++ offers.
Yet, while C++ supports single inheritance well,
what it calls “multiple inheritance” @~cite{Stroustrup1989Multiple}
is not at all the same as what almost everyone else calls “multiple inheritance”@xnote[":"]{
  Interestingly, the design of C++ non-virtual classes is very similar
  to the solution from Snyder’s CommonObjects @~cite{Snyder1986Encapsulation},
  even though Stroustrup does not cite Snyder:
  redefine the problem to be whatever the desired “solution” does—a Tree instead of a DAG—and
  hope the users won’t notice the difference.
  On the other hand, Stroustrup does cite the Lisp Machine Manual @~cite{Weinreb1981Chinual3},
  and rejects Flavors because it is not
  “sufficiently simple, general and, efficient enough to warrant the complexity it would add to C++”,
  which is exceedingly ironic considering Flavors was 1.4kloc (in October 1980, when cited),
  and C++ ~100kloc (in 1989, when citing),
  with Flavors having much richer and more general OO functionality than C++.
}
It is actually a modified kind of mixin inheritance
with some kind of “duplication” of superclasses
(for non-@c{virtual} classes, with members renamed along the inheritance tree),
and a subset of multiple inheritance (for @c{virtual} classes and members,
with restriction from a “conflict” view of inheritance, see @secref{DMRMI}).
Notably, C++ lacks the proper method resolution that enables a lot of
the modularity of multiple inheritance in other languages.

Now, you can use C++’s powerful template language to reconstitute actual mixin inheritance
and its method resolution on top of C++’s weird variant of inheritance@~cite{Smaragdakis2000Mixin};
and you could no doubt further implement proper multiple inheritance on top of that@xnote["."]{
  One could achieve multiple inheritance as a design pattern on top of mixin inheritance,
  as I will describe later in this book,
  wherein developers would manually compute and specify
  each class’s superclass precedence list;
  but this cancels some of the modularity benefits of multiple inheritance
  versus single and mixin inheritance.
  Alternatively, someone could extend the above technique to also reimplement
  the entire superclass linearization apparatus
  within the C++ template metaprogramming language.
  Template metaprogramming is most definitely powerful enough for the task,
  though it will take a very motivated developer to do the hard work,
  and the result will still be a burden for any developer who wants to use it.
  Moreover, for all that cost, classes defined that way would only interoperate
  with other classes following the exact same pattern.
  Maybe the library implementing the pattern could eventually be included
  in some semi-standard library, until, if it gets any traction,
  the language itself is eventually amended to do the Right Thing™.
}
But this technique is quite uncolloquial, syntactically heavy, slower than the colloquial ersatz,
and programmers have to rigorously follow, enforce and maintain some complex design patterns.

Finally, and at the very least, consider that
unless you explicitly tag your classes and their members @c{virtual},
C++ will deliberately eschew the “dynamic dispatch” of OO
and use “static dispatch” instead for the sake of performance (at doing the wrong thing).
In the end, C++ is many great and not-so-great things, but only few of those things are OO,
and even most of those that look like OO are often different enough that
@principle{C++ does not reliably inform about OO in general}@xnote["."]{
  The situation is similar for Ada, that adopted multiple inheritance in 2003
  by seemingly copying the general design of C++.
  Now even when C++ got multiple inheritance wrong,
  ignorance was no valid excuse,
  since Lisp got it right ten years earlier@~cite{Cannon1979}
  and Stroustrup even cited it via @~cite{Weinreb1981Chinual3}.
  Ignorance is even less excusable in the case of Ada
  copying C++’s “multiple inheritance” yet 14 years later.
  By contrast, many languages got it right in the same time frame,
  including Python (1991), Ruby (1995), Scala (2004).
}

@section[#:tag "OOiCO"]{OO isn’t Classes Only}
@epigraph{
  The class/instance distinction is not needed if the alternative of using prototypes is adopted.
  @|#:- @citet{Lieberman1986}|
}
Many claim that classes, as first implemented by Simula 67@~cite{Simula1967}
(though implementing a concept previously named by Hoare@~cite{Hoare1965Record}),
are essential to OO, and only ever care to implement, use, formalize,
study, teach, promote, or criticize class-based OO (a.k.a. Class OO).
Books from luminaries in Programming Languages @~cite{Pierce2002TAPL plai eopl3},
in their chapter about OO, barely even mention any other kind of OO if at all,
much less study it.

Yet KRL@~cite{Winograd1975 Bobrow1976},
the second recognizable precursor to OO,
whose authors introduced the words “inheritance” and “prototypes”
with the same meaning as in OO in the context of their language
(though the words were initially used as descriptions rather than definitions),
has what I would now call prototype-based OO (a.k.a. Prototype OO).
The modern concept of OO can be traced back to Smalltalk adopting inheritance in 1976,
naming inheritance after KRL’s usage,
and popularizing the word and concept of it among programming language designers
(KRL, a layer on top of Lisp, is arguably not a @emph{programming} language,
though it integrates with one).
Certainly, Smalltalk was class-based.
Yet contemporary with Smalltalk or immediately after it
were prototype-based languages Director @~cite{Kahn1976 Kahn1979Ani Kahn1979Director} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}@xnote["."]{
  ThingLab was built on top of Smalltalk by members of the same team at PARC,
  and oscillated between having or not having classes in addition to prototypes.}
Plenty more Prototype OO or “class-less” OO languages followed
@~cite{Hewitt1979Security Rees1982T Adams1988OOPScheme Ungar1987 Chambers1989 Lawall89SelfInScheme Salzman2005PrototypesMultipleDispatch jsonnet nix2015 poof2021}.
There are a lot more Prototype OO languages than I could have time to review @~cite{WikiProto},
but prominent among them is JavaScript @~cite{Eich1996JavaScript},
one of the most used programming languages in the world @~cite{TopPL2022},
maybe the top one by users
(though it relatively recently also adopted classes on top of prototypes @~cite{EcmaScript2015}).

Moreover, I will argue that Prototype OO @~cite{Borning1986}
is more general than Class OO, that is but a special case of it @~cite{Lieberman1986}
(see @secref{CaPfT}, @secref{RCOO}).
And I will even argue that you can recognizably have OO
with neither prototypes nor classes, as in T @~cite{Adams1988oopscheme}
(see @secref{MFtPaC}, @secref{MOO}, @secref{ROOfiMC}).
Despite common misinformed opinions to the contrary,
@principle{Class-less OO is part and parcel of the OO tradition},
historically, conceptually, and popularly.

Now of course, classes, while not @emph{essential} to OO,
are still @emph{important} in its tradition.
The situation is similar to that of types in Functional Programming (a.k.a. FP):
the historical preexistence and continued relevance of the untyped λ-calculus
and the wide adoption of dynamically typed functional languages like Scheme or Nix
are ample evidence that types are not essential to FP;
yet types are undoubtedly an important topic that occupies much of the theory and practice of FP.
Actually, the analogy goes further since, as we’ll see,
classes are precisely an application of OO to types (see @secref{P&C}, @secref{RCOO}).

@section{OO isn’t Imperative Programming}
@epigraph{Objects are a poor man’s closures. @|#:- "Norman Adams"|}
@epigraph{Closures are a poor man’s objects. @|#:- "Christian Queinnec"|
}
Many people assume that OO requires mutation,
wherein all attributes of all objects should be mutable, or at least be so by default,
and object initialization must happen by mutation.
Furthermore, they assume that OO requires the same applicative (eager) evaluation model
for procedure calls and variable references as in every common imperative language.
@; TODO{CITE? C++ Perl5 Python Java JavaScript Scala Ruby Go (see GitHub)}
At the same time, many now claim that purity (the lack of side-effects including mutable state)
is essential to FP, making it incompatible with OO.
Some purists further argue that normal-order evaluation (call-by-name or call-by-need)
is also essential for “true” FP, making it (they say) even more incompatible with OO.

However, there are many good historical reasons,
related to speed and memory limitations at both runtime and compile-time,
why early OO and FP languages alike, from the 1960s to the 1980s,
as well as most languages until relatively recently,
were using mutable state everywhere, and an eager evaluation model, at least by default.
And with 1990s slogans among Lispers like
“objects are a poor man’s closures”@~cite{Dickey1992SWOB}, and
“closures are a poor man’s objects”@~cite{Queinnec1996LiSP},
the problem back then (and as early as at least Yale T Scheme @~cite{Rees1982T})
was clearly not whether OO could be done purely with functions—obviously it could—but
whether it made practical sense to program purely without side-effects in general.
That question would only be slowly answered positively,
in theory in the early 1990s @~cite{Moggi1991Monads}
and in practice in the mid 2000s to mid 2010s,
as Haskell grew up to become a practical language@xnote["."]{
  Some identify darcs (2003) as the first widely used real-world application written in Haskell.
  After it came innovations such as bytestring (2005), cabal (2005)
  (and the “cabal hell” it started causing around 2006 until later solved by Stack),
  ghc6 (2006), that made Haskell much more practical to use, and
  new notable applications appeared like pandoc (2006), or xmonad (2007).
  A turning point was perhaps the publication of “Real World Haskell” @~cite{OSullivan2008RWH}.
  Eventually, Stack (2015) made non-trivial Haskell programs and scripts repeatable.
  Now there’s obviously a lot of subjectivity in deciding
  when exactly Haskell became “practical”—but one should expect
  the transition to practicality to be an S curve, such that
  whichever reasonable yet somewhat arbitrary threshold criteria you choose,
  the answer would be at about the same time.
  In any case, making a practical language pure functional was just not an option before 2010 or so,
  and it is absurd to declare any programming language concept intrinsically stateful
  merely because all its practical implementations before 2010 were stateful.
  You could similarly make the absurd claim that logic programming, functional programming,
  or linear algebra are intrinsically stateful.
}

Yet, there are (a) pure models of OO such as those of
Kamin, Reddy, Cook and Bracha@~cite{Kamin1988 ObjectsAsClosures Cook1989 bracha1990mixin},
(b) pure lazy dynamic OO languages such as Jsonnet or Nix@~cite{jsonnet dolstra2008nixos nix2015},
and pure lazy OO systems for Scheme@~cite{poof2021},
(c) languages happily combining OO and FP such as Common Lisp or Scala,
with plenty of libraries restricting themselves
to pure functional objects only @~cite{LIL2012 Chiusano2014FPScala}, and
(d) last but not least, Oleg Kiselyov’s implementation of OO, even stateful OO if you want,
in the pure FP language Haskell(!) @~cite{Kiselyov2005HaskellOOS}.

These provide ample evidence that OO does not at all require mutation,
but can be done in a pure setting, and is very compatible with FP, purity,
and even with laziness and normal-order evaluation.
@; Haskell typeclasses embody half of OO@~cite{typeclasses LIL2012},
@; and could be tweaked to embody all of it.
@; though its designers might not wholly embrace the OO tradition. @; TODO{CITE}
@; TODO: add inheritance to Haskell typeclasses or Rust traits, or just cite CL gf, Clojure protocols.
Actually, I will argue based on studying of the semantics of OO that
@principle{Pure Lazy Functional Programming is the natural setting for OO}. @;{TODO secref}

@section{OO isn’t Encapsulation}
@epigraph{A half-truth is a whole lie. @|#:-"Yiddish proverb"|
}
Many OO pundits claim that an essential concept in OO
is “encapsulation” or “information hiding”@~cite{DeRemerKron1975}.
Some instead speak of “data abstraction” or some other kind of “abstraction”.
There is no consensus as to what this or these concepts mean, and no clear definition,
@; TODO{CITE} @; XXX cite Liskov??? Mary Shaw???
but overall, these words refer either (a) to part or all of what I call @emph{modularity}
(see @secref{MO}, @secref{M}),
or (b) to some specific set of visibility primitives in some OO languages.

Indeed, “encapsulation” usually denotes the ability to code against an interface,
with code on either side not caring which way the other side implements its part of the interface,
not even being able to distinguish between multiple such implementations,
even less to look inside at the state of the other module.
Viewed broadly, this is indeed what I call modularity,
which in my theory is indeed half of the essence of OO.
But the word modularity much better identifies the broader purpose,
beyond a mere technical property.
And even then, modularity only characterizes half of OO,
so that people who try to equate OO with that half only
crucially miss the other half—@emph{extensibility} (see @secref{EO}, @secref{E})—and
thus fail to properly identify OO.

Now, insofar as some people identify encapsulation narrowly as the presence
of specific visibility mechanisms such as found in C++ or Java
(with some attributes or methods being @c{public}, @c{private} or something in-between,
whose precise semantics the designers of different languages cannot agree on),
I’ll easily dismiss such mechanisms as not essential to OO,
since many quintessential OO languages like Smalltalk or Common Lisp
lack any such specific mechanism,
whereas many non-OO languages possess mechanisms to achieve the same effect,
in the form of modules defining but not exporting identifiers
(e.g. not declaring them @c{extern} in C),
or simply lexical scoping@~cite{Rees1995W7}.
@; TODO{cite Simula? JS?}

Certainly, these mechanisms can be very useful,
worthy features to add to an OO language.
They are just not essential to OO and not specific to it,
though of course their adaptation to OO languages will follow
the specific shape of OO constructs not found in non-OO languages.
Misidentifying OO as being about these mechanisms rather
than about the modularity they are meant to support can only lead to
sacrificing the ends to the means.

@section{OO isn’t opposite to FP}
@; I destroy my enemies when I make them my friends. — Lincoln
@; What about side by side with a friend? — Legolas in The Two Towers (movie)
@epigraph{¿Por qué no los dos? (Why not both?)
  @|#:- "Old El Paso"|
}
Some argue that there is an essential conflict between OO and FP,
between Inheritance and Composition,
wherein OO is about modeling every possible domain in terms of inheritance,
and FP is about modeling every possible domain in terms of composition,
and the two must somehow duel to death.

But OO and FP, inheritance and composition, are just pairs of distinct concepts.
Neither of which subsumes the other; each fits a distinct set of situations.
@;Each distinct concept has its set of situations that it fits,
@;distinct from that of any other concept (or else they are actually the same concept);
@;a concept that fits all situations has no content and is useless;
@;and two concepts like OO and FP neither of which subsumes the other,
@;cover sets of situations neither of which is a subset of the other.
It makes no sense to oppose them, especially not when I see that
OO can be implemented in a few lines of FP, whereas
most modern OO languages contain FP as a subset—and
Lisp has harmoniously combined OO and FP together ever since they both emerged in the 1970s,
@; TODO cite
decades before anyone had the idea to fantasize a conflict between the two.

The argument of Composition vs Inheritance is actually a distortion
of a legitimate question of OO design, @; TODO cite
wherein one has to decide whether some aspect of a class@xnote[""]{
  My counter-argument also works for prototypes or arbitrary OO specifications,
  but since the argument is usually given for classes, I will use classes in this section.
}
embodied as attributes or methods, should be included directly in the class
(a) by inheriting from another class defining the aspect
(the class @emph{is-a} subclass of the aspect class—inheritance of classes), or
(b) indirectly by the class having as an attribute an object of that other class
(the class @emph{has-a}n attribute of the aspect class—composition of classes
seen as constructor functions)@xnote["."]{
  There is also an old slogan of OO design,
  notably found in the famous “Gang of Four” (“GoF”) book @~cite{GoF1994},
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
  which will quickly lead to an obvious showstopper issue if it doesn’t work,
  whereas picking is-a where has-a was the better choice can lead to a lot of complications
  before it is realized that it won’t work right.
  Yet it is always preferable to understand the difference between “is” and “has”,
  and to use the correct one based on understanding of the domain being modeled,
  rather than on vague heuristics that substitute for lack of understanding.
  At any rate, this slogan, though oft-quoted out of context in online debates,
  actually has nothing to do with the OO vs FP debate—it is about using OO effectively.
}

The answer of course depends on expectations about how the class will be further specialized
within a static or dynamically evolving schema of data structures and algorithms.
If the schema is small, static, well-understood and won’t need to evolve,
it doesn’t really matter which technique is used to model it.
But as it grows, evolves and boggles the mind,
a more modular and extensible approach is more likely to enable adapting the software
to changing situations, at which point thoughtful uses of inheritance can help a lot@xnote["."]{
  @emph{Is} a car a chassis (inheritance),
  or does it @emph{have} a chassis while not @emph{being} it (composition)?
  If you’re writing a program that is only interested in the length of objects,
  you may model a @c{car} as a @c{lengthy} object with a @c{length} slot,
  and a @c{chassis} too. Now if your program will only ever be interested
  but in the length of objects, you may altogether skip any object modelling:
  and only use numeric length values directly everywhere for all program variables.
  Is a car a chassis? Yes, they are both their length, which is the same number,
  and you may unify the three, or let your compiler’s optimizer unify them
  as you initialize them from the same computation.
  Now if you know your program will evolve to become interested in
  the width of objects as well as their length,
  you might have records with length and width rather than mere numbers,
  and still unify a car and its chassis.
  But if your program eventually becomes interested in the height, weight or price of objects,
  and those of their components when they need be replaced,
  you’ll soon enough see that the two entities may somehow share some attributes
  yet be actually distinct: ultimately, both @c{car} and @c{chassis} @emph{are} @c{lengthy},
  but a @c{car} @emph{has} a @c{chassis} and @emph{is not} a @c{chassis}.
}

In the end, @principle{OO and FP are complementary, not opposite}.
If there is a real opposition, it is not between two perfectly compatible techniques,
but between two mindsets, between two tribes of programmers each locked
into their narrow paradigm@~cite{Gabriel2012} and
unable to comprehend what the other is saying.

@section{OO isn’t Message Passing}
@epigraph{Name the greatest of all inventors. Accident.
  @|#:-"Mark Twain"|
}
Alan Kay, who invented Smalltalk and coined the term “Object-Oriented Programming” circa 1967,
notably explained@~cite{Kay2020} that he originally meant
a metaphor of computation through independent (concurrent, isolated) processes
communicating by passing asynchronous messages.
This metaphor also guided the modifications originally
brought to Algol by Simula@~cite{Simula1966}.
It is also present in notable early object systems such as
Director @~cite{Kahn1976 Kahn1979Ani Kahn1979Director} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}.

However, neither Simula nor Smalltalk nor any popular OO language
actually fits that metaphor.
Some less popular Actor languages might @~cite{Hewitt1979Security};
but they remain marginal in the tradition.
@; TODO cite Yonezawa ?
Instead, the only widely-used language to truly embody this metaphor
is Erlang@~cite{OOP2010};
yet Erlang is not part of the OO tradition,
and its authors have instead described its paradigm as “Concurrency-Oriented Programming”.
Meanwhile the theory of computation through message-passing processes
was studied with various “process calculi”,
@; TODO cite pi calculus, join calculus, rho calculus, CHAM, etc.
that are also foreign to the OO tradition,
and largely unacknowledged by the OO community.
Indeed Erlang crucially lacks inheritance, or support for the “extreme late binding of all things”
that Alan Kay also once mentioned was essential for OO@xnote["."]{
  In Erlang, each process is a dynamic pure applicative functional language
  enriched with the ability to exchange messages with other processes.
  Now, as we’ll see, you need fixpoints to express the semantics of OO;
  but in a pure applicative context, you cannot directly express sharing the results of a computation,
  so the pure fixpoint combinators lead to exponential recomputations
  as deeper self-references are involved (see @secref{DSF}).
  OO is therefore possible using the applicative pure functional fragment of the language
  within an Erlang process, but the result will not scale very well;
  see for instance the example “object-via-closure” that Duncan McGreggor wrote as part of LFE.
  @; https://github.com/lfe/lfe/blob/1d0de5e04a9d5b8f1430063c45b561f08123a4e0/examples/object-via-closure.lfe
  Or OO could be achieved indirectly, by using a preprocessor that expands it away,
  or a compile-time only extension to the compiler, as in most static Class OO languages.
  Or OO could be achieved as a design pattern of maintaining some global table
  to store the state of the many shared lazy computations in each process.
  Or, more in line with the Actor model that Erlang embodies,
  OO could be achieved by spawning one or multiple processes
  for each shared lazy or stateful computation (including each super-object of each object),
  which might require some strict object lifetime discipline (not colloquial in Erlang),
  or garbage collection of processes (not part of the Erlang language, beyond the process tree);
  see for instance the example “object-via-process” that Duncan McGreggor wrote as part of LFE.
  @; https://github.com/lfe/lfe/blob/1d0de5e04a9d5b8f1430063c45b561f08123a4e0/examples/object-via-process.lfe
  None of these solutions would qualify as supporting OO much more than
  assembly language “supports” OO or any Turing-universal language “supports” any paradigm, though.
  In the end, the essence of OO, which is Prototype OO,
  directly fits in the pure lazy functional paradigm,
  but only fits indirectly in other paradigms,
  including the pure applicative functional paradigm.
}

Most OO languages have no support whatsoever for concurrency,
or then again only as an afterthought added years or decades
after the language was originally designed,
and not integrated in any meaningful way with OO message dispatch.
Moreover, many OO languages generalize and extend their method dispatch mechanism
from “single dispatch” to “multiple dispatch”@~cite{
  Bobrow1986CommonLoops Bobrow1988CLOS CecilMultimethods Allen2011Type}.
Their “multimethods” are attached to tuples of prototypes or classes,
and there is no single prototype, class, or single independent entity of any kind
capable of either “receiving” or “sending” a message.
Instead, they are attached to a “generic function”
that handles the dispatch based on the types of its arguments@xnote["."]{
  @; TODO move generic functions to chapter 8, and leave a secref here.
  The “generic function” functionality from the Common Lisp Object System (CLOS) @; TODO cite
  can be viewed as isomorphic to the “protocols” functionality of Clojure;
  and Common Lispers also use the word “protocol” informally to designate a set of generic functions.
  They would in turn be isomorphic to the “typeclasses” of Haskell
  or the “traits” of Rust... @; TODO cite
  if only these latter two supported inheritance, which they don’t.
  These idioms all denote a set of related function names and type signatures,
  that are implemented differently for different configurations,
  where each configuration is associated to @emph{one or multiple} types of arguments
  (and, in Haskell, also different types of expected results).
  Other crucial property of these idioms: these traits, typeclasses or protocols
  can be defined @emph{after the fact},
  so that new traits, typeclasses or protocols can be defined for configurations of existing types,
  and new types can be added to existing typeclasses, etc.
  This second property is in sharp contrast with “interfaces” in Java or C#,
  wherein the author of the class must specify in advance
  all the interfaces that the class will implement,
  yet cannot anticipate any of the future extensions that users will need.
  Users with needs for new protocols will then have to keep reinventing
  variants of existing classes, or wrappers around existing classes, etc.
  — and again when yet another protocol is needed.
  Protocols are therefore much more modular than Java-style “interfaces”,
  and more extensible than Rust “traits” or Haskell “typeclasses”,
  making them modular at a finer grain (protocol extensions rather than protocol definitions),
  which in turn makes them more modular.
  Note also how what Rust recently popularized as “trait” is
  something completely different from what Smalltalk, and after it Mesa or Scala, call “trait”.
  In these languages, with an anterior claim to the word,
  a “trait” is just a class that partakes in multiple inheritance,
  defining a single type and associated methods, and not after the fact.
  Once again, be careful that there is no common vocabulary
  across programming language communities.
}
While multimethods are obviously not essential to OO
since there are plenty of OO languages without them,
they are a well-liked, age-old extension
in many OO languages (CLOS, CECIL, Dylan, Fortress, Clojure, Julia)
and extensions exist for C++, Java, JavaScript, TypeScript, C#, Python, Ruby, etc.
@; TODO cite stroustrup2007multimethods https://en.wikipedia.org/wiki/Multiple_dispatch
The “message passing” paradigm, having no place for multimethods,
thus falls short compared to other explanations of OO that accommodate them@xnote["."]{
  Now, the message passing paradigm @; TODO cite PLANNER, Actors
  can be extended with a notion of “group messaging”
  where one object sends a “message” to a “group” of objects as a collective entity
  (rather than each member of the target group)
  @; TODO cite ABCL group messaging
  or to a “chemical” paradigm where a “chemical reaction” may involve
  multiple entities in and multiple entities out, with “message” entities
  conveying the changes in intermediary steps. @; TODO cite CHAM
  But even with these extensions to the paradigm,
  you would still have to also specifically shoe-horn extensibility and method resolution
  into the paradigm to fit OO and its method inheritance,
  whether with single dispatch or multiple dispatch.
}

In conclusion, whatever historical role it may have had in inspiring the discovery of OO,
@principle{the paradigm of message-passing processes is wholly distinct from OO},
with its own mostly disjoint tradition and very different concerns,
that describes a different set of programming languages and patterns@xnote["."]{
  Now, there is no doubt, from their later testimonies as well as then published papers,
  that Erlang’s Concurrency Oriented Programming is clearly
  what the authors of Simula, Smalltalk, Actors, etc., were all @emph{aiming at}.
  But, due to hardware as well as software limitations of the 1960s and 1970s,
  they all failed to actually reach that goal until the mid 1980s.
  However, on their way to an intended destination, they instead serendipitously
  stumbled on something altogether different, inheritance,
  that would soon become (pun intended) a vastly successful programming language feature,
  as often misunderstood, abused and hated as understood, well-used and loved,
  that came to define a new style of programming, called “Object-Oriented Programming”.

  That’s how invention always works:
  if you knew beforehand what you would discover, you would already have discovered it.
  An invention is always surprising, original, and never, ever,
  exactly what you knew in advance it would be—or else
  the invention happened earlier, back when it was still surprising and original indeed.
  Also, an invention is shaped by the technical constraints of its time—some of which
  the inventor may lift, but not always those anticipated.
}

@section[#:tag "OiaMotW"]{OO isn’t a Model of the World}
@epigraph{If you call a tail a leg, how many legs has a dog? Five?
  No! Calling a tail a leg doesn’t make it a leg.
  @|#:- @elem{Abraham Lincoln, explaining the difference between
                 lexical scoping and dynamic scoping}|
}
Some have claimed that OO is meant to be @emph{the} way to model the world,
or at least @emph{a} way,
often in association with the concurrent message passing model
I already established above was not quite OO,
or with some class-based OO framework they sell.

However, while OO can indeed be of great use in modeling a lot of problems,
especially where the modeling language needs modularity and extensibility,
it by no means is supposed to be a Theory of Everything that subsumes
Relativity and Quantum Mechanics, Constitutional Law, Darwinism, Aristotelian Poetics, etc.
Even if I stick to software, there are plenty of paradigms other than OO that OO does not subsume:
functional programming, logic programming, machine learning,
operational research, relational databases, reactive programming, temporal logic,
concurrent programming, dataflow programming, homomorphic encryption, etc.
Inasmuch as OO languages can be used to implement any of these paradigms,
so can any Turing Tar-Pit. And inasmuch as any of these paradigms
can be harmoniously combined with OO, that does not make either a subset of the other.
People seriously studying OO should not take at face value the claims of
Snake Oil and Silver Bullet salesmen, either about what their products can do,
or about whether these products indeed embody OO. Mostly, they do not.

Consider methodologies such as UML that claim to do OO modeling, @; TODO cite
drawing diagrams of relations between classes including inheritance.
Besides the fact that classes are not essential to OO as seen previously,
UML and similar languages do not even meaningfully have classes:
there is no proper semantics to inheritance,
especially in presence of fields that recursively refer back to a class:
should the child class have a link to the parent class or to the child class?
Assume a classic case of modeling humans as animals,
wherein animals can have offspring that are animals of the same kind:
Should human offspring be modeled as arbitrary animals,
or should they be modeled as human only?
Conversely, if some animals eat other animals,
does that mean that humans automatically eat humans, or only some other animals?
In presence of recursion, UML falls apart,
by failing to distinguish between subclassing and subtyping,
between self-reference and reference to a constant (see @secref{TfOO}).

Interestingly, Amílcar Sernadas’s or Bart Jacobs’s categorical theories
of “objects” and “inheritance”
@~cite{sernadas1994 Jacobs1995ObjectsAC Jacobs1996InheritanceAC}
actually model UML and refinement,
and not at all actual objects and inheritance as used in Programming Languages;
a hijacking of the same words for completely different meanings,
with the only similarity being that both sets of meanings
involve arrows between specifications.
At least Jacobs early on explicitly embraces the limitation whereby
self-reference or recursion is prohibited from field definitions.
Just like UML, his co-algebra utterly fails to model OO;
but at least his theory is internally consistent if not externally.

@principle{UML, co-algebras and other similar methodologies
are actually relational data modeling @; TODO cite
disguised as OO}.
As we’ll see later, their “classes” are extensible indeed,
but in a trivial way that fails to support modularity@xnote["."]{
  Note that there is nothing wrong at all with relational data modeling as such:
  it is a fine technique for many purposes,
  despite being deliberately limited in abstraction, and, therefore, in modularity—and
  sometimes @emph{thanks to this limitation}.
  Restrictions to expressiveness can be very useful,
  in the necessarily restricted or imprecise cases that they apply.
  Indeed, in some cases, relational data modeling, not OO,
  is what you need to organize your data and your code.
  Moreover, Category Theory are a great way to improve on previous approaches
  to relational data, as witness by the field of Categorical Databases. @; TODO cite
  However, what is very wrong, and intellectually dishonest,
  was to sell relational data modeling as OO back when OO was trendy, based on
  a superficial and at times deliberate misunderstanding of OO
  by either or both sellers and buyers, resulting in more confusion.
}
UML and co-algebras describe the “easy case” of OO, where objects are just a convenient way
of merging records of elementary data types
(or “constant” data types, for co-algebras)
— an easy case without recursion, where subclassing indeed coincides with subtyping.
But these methodologies avoid crucial features of OO programming,
where records can recursively refer to other records,
where the operations of interest are higher-level than getting or setting fields,
where you incrementally extend not just data types but also algorithms,
where there are “binary methods” that involve two objects at once,
or even more elaborate higher-order functions, etc.
More broadly, these methodologies lack any effective semantics of inheritance,
of method resolution in computing properties of objects along their class hierarchies,
or of anything that has the precision required to specify code
that can actually run and be reasoned about.
But specifying code is exactly where the conceptual difficulties and gains of OO
are both to be found with respect to software construction.
In fact, these handwaving methodologies@xnote[""]{
  @; TODO: maybe give Goguen his own section?
  Not all uses of Category Theory in “OO” are handwaving.
  Goguen, who invokes Category Theory in his papers,
  and is cited by these later categorical imitators,
  develops precise formal specification of code, refinement of such specifications,
  and actual implementation of executable code. @;{TODO cite}
  On the other hand, Goguen’s claim to do “OO” is dubious,
  despite some attempts in later works to retrofit some actual OO concepts into his systems;
  instead, what he developed turns out to be
  a completely different and orthogonal paradigm—term rewriting.
  Term rewriting is a wonderfully interesting paradigm to study,
  but has never seen any adoption for practical programming,
  though it has found use in reasoning about programs.
  What Goguen calls “inheritance” most of the time is actually code refinement,
  a technique that can be used to build proven-correct compilers,
  though it is not a general theory of code implementation applicable
  to arbitrary such compilers.
}
are specifically designed to fool
those incapable or unwilling to wrestle with computation
into believing they understand all there is to know about software modeling.
Yet the nature and correctness of software lies precisely
in this gap they are unable or unwilling to explore.

An actual theory of types for OO must confront not just products of elementary data types,
but sum types, function types, subtyping, constrained type parameters,
existential and universal types, and more—including, especially, fixpoints (recursion).
And you can always go beyond with session types, substructural types, temporal types,
separation types, dependent types, etc.
In the end, if you care about modeling the types in your software (and you usually should),
you should write your software in a language with a rich and strong typesystem,
one that is logically consistent or at least whose inconsistencies are well mapped and can be avoided,
one that is statically enforced by the compiler or at least
that you will systematically enforce socially.
Then you should use that typesystem to describe not just
records of elementary data types over the wire or on disk,
but all the rich entities within your software, their interactions and interrelations.
This will provide much more help with design and safety than any code-less methodology can.
And if you picked an OO-capable language like C++, Java, C# or Scala,
(or, with manually enforced dynamic types, Lisp, Ruby or Python),
you can actually use OO as you do it.

@exercise[#:difficulty "Easy"]{
  Identify cases where what I claim OO is @emph{not},
  contradicts your prior assumptions about what OO was.
}

@exercise[#:difficulty "Easy"]{
  Use your experience, or AI, to identify—for each section of this chapter
  (or at least a couple of them picked at random)—examples
  of how OO and the concept often wrongly identified with it do not coincide:
  either something that is an instance of OO and not the other,
  or something that is an instance of the other and not OO.
  Be careful not to blindly trust the answer of AI,
  especially as your prompt may lead it to slightly misunderstand the question,
  if the context of this chapter’s text is not included.
  Notably, it’s not just C++ or UML that are not OO, but their object model.
}

@exercise[#:difficulty "Easy"]{
  With the help of a search engine or an AI,
  find OO languages or libraries that illustrate each possible combination
  in the space defined by the following axes:
  (a) having an inheritance mechanism poorer than that of C++, equivalent, or richer;
  (b) having or not having classes;
  (c) using or not using mutable state;
  (d) with or without notions of “public” or “private” methods;
  (e) with or without the ability to express functional programs;
  (f) with or without a notion message passing;
  (g) with or without its authors claiming that it is OO.
  Note that some combinations may not be found, and that is fine;
  try to find at least one answer on each side of each axis.
}

@exercise[#:difficulty "Medium"]{
  In each of the above cases, or at least for a couple that you’re least familiar with,
  dig deeper into the counter-example, and build an OO program
  that doesn’t match the other paradigm, or a program in that other paradigm
  that doesn’t match OO at all.
}

@exercise[#:difficulty "Medium"]{
  Use a search engine to find online documents criticizing OO,
  or ask some AI to criticize OO for you.
  Identify which of the points made actually apply or do not apply
  to OO as such, as opposed to things I identified as not being OO,
  or other specific uses of OO that do not represent OO in general.

  Bonus: Take one of the criticisms you found that does not actually apply to OO.
  Rewrite it to address what the critic was probably actually concerned about
  (e.g., C++ specifically, imperative programming, etc.)
  without incorrectly blaming "OO" for it.
}

@exercise[#:difficulty "Hard"]{
  Find some criticism (valid or invalid) that actually pertains to OO,
  rather than to something else wrongly identified with OO.
  Wrong targets for which criticism will not count, include:
  the things I denounced as not OO,
  particular systems written in OO style,
  not just to a particular substyle of OO,
  and not to mistakes that are not specific to OO
  but are being done in plenty of non-OO systems too.
  (This is notably harder than the previous exercise!)
}

@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{01to02},
  compare your previous answers to mine.
  See what surprised you, and how your understanding evolved.
}

@exercise[#:difficulty "Hard, Recommended" #:tag "02to03"]{
  After reading this chapter, but before you read the next,
  try to characterize what you think OO @emph{is} about in the end,
  or at least my definition of it, that isn’t any of the things I denounced.
  While this exercise is somewhat hard, it will make next chapter more enlightening,
  so save your answers to compare them to the treatment in @secref{WOOiIO}.
}

@exercise[#:difficulty "Research"]{
  Find some other technique, field of knowledge, school of thought, ideology, etc., beside OO,
  that, having once been trendy or popular,
  was overtaken by plenty of people wrongly claiming its name,
  to advance very different sets of ideas.
  Characterize the real thing under the original name,
  and the main variants that corrupt the name
  (though they may have interesting contributions of their own beside this corruption).
} @; Solution: consider the word “ideology” itself.
