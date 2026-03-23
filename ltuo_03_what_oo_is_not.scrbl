#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 3)

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

@section{Things OO isn’t (that many claim it is)}
@epigraph{When words are unfit, speech is unadapted and actions are unsuccessful.
@|#:- "Confucius"|
}

@subsection{OO isn’t Whatever C++ is}
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
  Interestingly, the design of C++ non-virtual superclasses is very similar
  to the solution from Snyder’s CommonObjects @~cite{Snyder1986},
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
(for non-@c{virtual} superclasses, with members copied/renamed along the inheritance tree),
and a subset of multiple inheritance (for @c{virtual} superclasses and member functions,
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
unless you explicitly tag your superclasses and their member functions @c{virtual},
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
  including Common Lisp (1988), Python (1991), Ruby (1995), Scala (2004).
}

Of course, neither is OO defined by CLOS, Ruby, Python, Java, C#, Scala,
or any particular language and its object system, however advanced, popular,
or declared as prototypical by any particular pundit.

@subsection[#:tag "OOiCO"]{OO isn’t Classes Only}
@epigraph{
  The class/instance distinction is not needed if the alternative of using prototypes is adopted.
  @|#:- @citet{Lieberman1986}|
}
Many claim that classes, as first implemented by Simula 67@~cite{Simula1967}
(though implementing a concept previously named by Hoare@~cite{Hoare1965}),
are essential to OO, and only ever care to implement, use, formalize,
study, teach, promote, or criticize class-based OO (a.k.a. Class OO).
Books from luminaries in Programming Languages @~cite{Pierce2002TAPL plai eopl3},
in their chapter about OO, barely even mention any other kind of OO if at all,
much less study it.

Yet KRL@~cite{Bobrow1976},
the very first system@Note{
  KRL, though its name stands for “Knowledge Representation Language”,
  was arguably not a @emph{programming} language in most people’s understanding,
  though it was one in the then understanding of Lispers:
  it is a layer on top of Lisp, what one would now call an “object system”.
  But Lisp, being extensible, blurs the distinction between a new language
  and an extension to the existing language, and KRL was certainly a language by that standard.
  Also, while the original intention in the original 1975 paper @~cite{Winograd1975} was
  to model knowledge in terms of Frames @~cite{Minsky1974}, and not to write programs;
  but by the 1976 report @~cite{Bobrow1976} the concern has shifted, and
  while the frames model is still present and cited, it has taken a secondary role,
  while defining procedures has taken a prominent role.
  The 1975 article introduces the expression “inheritance of properties” descriptively,
  without implementation;
  the 1976 article uses it as a more formal definition, with an implementation.
}
that claimed the words “object-oriented” in print with the modern sense
(though the choice of expression itself was likely influenced by Alan Kay), and
that also introduced the words “inheritance” and “prototype” in their OO meaning,
has what is now called prototype-based OO (a.k.a. Prototype OO).
The modern concept of OO can be traced back to the interaction between
Bobrow’s KRL team and Kay’s Smalltalk team at PARC around 1976,
both informed not just by Simula but also by many other predecessors.
Kay took KRL’s inheritance, made it a well-defined concept specifically for @emph{programming}
(which it was not originally in KRL) by identifying it with the prefix mechanism in Simula,
that he replaced with the better resend mechanism, and popularized
the word and concept of inheritance as well as the term “object-oriented”;
Bobrow adopted Kay’s improvements together with his own (hard to say which is whose),
and was first at the publish line.
Then others at PARC, at MIT, and eventually Stroustrup at Bell Labs, adopted OO,
and the rest is history.
Certainly, Smalltalk was class-based, unlike KRL.
Yet contemporary with Smalltalk or immediately after it
were prototype-based languages Director @~cite{Kahn1976 Kahn1979Ani Kahn1979Director} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}@xnote["."]{
  ThingLab was built on top of Smalltalk by members of the same team at PARC,
  and oscillated between having or not having classes in addition to prototypes.}
Plenty more Prototype OO or “class-less” OO languages followed
@~cite{Hewitt1979 Rees1982T Adams1988OOPScheme Ungar1987 Chambers1989 Lawall89SelfInScheme Salzman2005 jsonnet nix2015 poof2021}.
There are a lot more Prototype OO languages than I could have time to review @~cite{WikiProto},
but prominent among them is JavaScript @~cite{Eich1996JavaScript},
one of the most used programming languages in the world @~cite{TopPL2022},
maybe the top one by users
(though it relatively recently also adopted classes on top of prototypes @~cite{EcmaScript2015}).

Moreover, I will argue that Prototype OO @~cite{Borning1986}
is more general than Class OO, that is but a special case of it @~cite{Lieberman1986}
(see @secref{CaPfT}, @secref{RCOO}).
And I will even argue that you can recognizably have OO
with neither classes nor even prototypes
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

@subsection{OO isn’t Imperative Programming}
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
“objects are a poor man’s closures”@~cite{Dickey1992}, and
“closures are a poor man’s objects”@~cite{Queinnec1996LiSP},
the problem back then (and as early as at least Yale T Scheme @~cite{Rees1982T},
that developed the underlying concepts and implemented an entire system on them),
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
Kamin, Reddy, Cook and Bracha@~cite{Kamin1988 ObjectsAsClosures Cook1989 Cook1989Denotational bracha1990mixin},
(b) pure lazy dynamic OO languages such as Jsonnet or Nix@~cite{jsonnet dolstra2008nixos nix2015},
and pure lazy OO systems for Scheme@~cite{poof2021},
(c) languages happily combining OO and FP such as Common Lisp or Scala,
with plenty of libraries restricting themselves
to pure functional objects only @~cite{LIL2012 Chiusano2014FPScala}, and
(d) last but not least, Oleg Kiselyov’s or Michael Gale’s implementations of
statically typed OO both stateful and pure in the pure FP language Haskell(!)
@~cite{Kiselyov2005HaskellOOS Hoop}.

These provide ample evidence that OO does not at all require mutation,
but is very compatible with FP, purity, and even with laziness and consistent static typing.
(This does not mean that Haskell typeclasses or Rust traits are OO; they are not—see @secref{CSvTS}.)
Actually, I will argue based on studying of the semantics of OO that
@principle{Pure Lazy Functional Programming is the natural setting for OO}. @;{TODO secref}

@subsection{OO isn’t Encapsulation}
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
thus fail to properly identify OO@xnote["."]{
  Someone who disregards the extensibility aspect of OO to only consider modularity
  would count as OO any language with a module system, or encapsulated entities of any kind,
  to the great surprise of the users and implementers of these languages.
  “OO” languages by this standard would include
  not just languages like Go that explicitly reject OO, but also
  all Functional Programming (FP) languages,
  since lexical scoping is enough to provide encapsulation.
  See in the introduction (@seclink{aToOO}) the footnote about William Cook,
  who held such views, and the notes on some of his more egregious writings
  in my annotated bibliography @~cite{Cook1991 Cook2009 Cook2012}.
}

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

@subsection{OO isn’t opposite to FP}
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
wherein one has to decide whether some aspect of a class@Note{
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

@subsection{OO isn’t Message Passing}
@epigraph{Name the greatest of all inventors. Accident.
  @|#:-"Mark Twain"|
}
Alan Kay, who invented Smalltalk and coined the term “Object-Oriented Programming” circa 1967,
before the modern concept of OO was fully formed (in 1976)@xnote[","]{
  While Kay had a crucial role in the invention and naming of OO,
  it is important not to put too much weight in the name without the full concept.
  Back in the 1960s and 1970s, many things were user-oriented,
  calculus-oriented, terminal-oriented, etc.
  The word “object-oriented” appears in print in works about psychology, sociology, and
  even in a few papers in Computer Science, before Smalltalk and KRL,
  the earliest I could find being written in 1971 by Bobrow’s brother @~cite{Bobrow1972},
  but not quite with that meaning. @; TODO Goodenough. Ross? @citet{Ross1976} -- earlier?
  @citet{Jones1976} also come close,
  using the word with respect to encapsulating types and code together,
  though without the extensibility aspect that has been the defining trait
  of what programmers expect from OO languages.
  Jones and Liskov published that paper months
  before the Bobrow and Winograd memo @~cite{Bobrow1976}
  that first uses “object-oriented” the modern way, presumably after Kay.
  Were these earlier uses in print legitimate?
  Yes, but they are not the ones whose meaning took on. Kay’s meaning won—for good reasons.
  However, for the same reason, while Kay’s personal use of the term dates back to 1967,
  we should still only credit the invention at the date that the concept became fully formed,
  which is with Kay’s Smalltalk-76 and Bobrow’s KRL-0, both in 1976,
  because of each other (they were across-the-hall colleagues at PARC and cite each other).
}
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
Actor languages actually do @~cite{Hewitt1979},
but though somewhat influential on paper, they never got popular
and always remained somewhat marginal in the tradition.
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
  including the pure applicative functional paradigm of Erlang in-process,
  or the process-oriented paradigm of Erlang between-processes.
}

Most OO languages have no support whatsoever for concurrency,
or then again only as an afterthought added years or decades
after the language was originally designed,
and not integrated in any meaningful way with OO message dispatch.
Moreover, many OO languages generalize and extend their method dispatch mechanism
from “single dispatch” to “multiple dispatch”@~cite{
  Bobrow1986CommonLoops Bobrow1988CLOS Chambers1992 Allen2011}.
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

@subsection[#:tag "OiaMotW"]{OO isn’t a Model of the World}
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
In fact, these handwaving methodologies@Note{
  @; TODO: give Goguen his own subsection?
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

@section{More Frequent Misunderstandings about OO}

@subsection{False dichotomy between Inheritance and Delegation}
Many authors have called “delegation” the mechanism used by Prototype OO
@; TODO CITE Self, Castagna Cardelli 1996, …
as distinct from the “inheritance” mechanism of Class OO.
This wrongheaded distinction started with @citet{Hewitt1979},
in whose ACT1 language the two concepts were both implemented,
but through separate implementation paths.
The distinction was further popularized by @citet{Lieberman1986},
who contrasts the two joined concepts of prototype-delegation vs class-inheritance.

Yet, identifying inheritance with classes to the exclusion of prototypes
is historically counterfactual:
the words “inheritance” and “prototype” were both simultaneously introduced
by KRL @~cite{Winograd1975 Bobrow1976},
a system with (multiple) inheritance and prototype OO—from before
the word Object Oriented was popular.
Indeed, KRL was instrumental as an inspiration to Smalltalk-76,
the system that made OO popular.

Opposing inheritance and delegation is also logically counterfactual:
@citet{Lieberman1986} itself explains how “inheritance” (i.e. classes)
can be expressed as a special use of “delegation” (i.e. prototypes).
On the other hand, paper also explains you cannot go the other way around
and express prototypes in terms of classes:
prototypes enable dynamic extension of individual “objects” (prototypes) at runtime,
while classes only allow extension at compile-time, and only
for an entire type (“class”) of “objects” (elements of the type).

In the end, the inheritance mechanism is indeed the same, and it is very wrong to
give it two different names depending on whether it is used for prototypes or for classes.
Even Self, that became the most popular language with “delegation” in academia,
uses the word “inheritance” in its papers @~cite{Ungar1987 Chambers1989 parentsSharedParts1991}.
And @citet{Stein1987} argues that delegation and inheritance are the same concept,
and notes that prototypes map to classes, not class instances
(though strictly speaking she gets the mathematical direction of the map wrong).
The real distinction and comparison that should have been made was between the relative
expressiveness of prototypes and classes, especially if considered as second-class entities
and in absence of reflection (or refraint from using it).
But that is the conclusion that none of the authors who wrote on the topic made explicit,
even though it is implicit in both.
And so the authors focus on arguing about different ways to name the same concept in two contexts
while failing to argue on the different contextual concepts that do matter@xnote["."]{
  If irrelevant changes in the context are a valid excuse to give an existing concept a new name
  and get a publication with hundreds of citations based on such a great original discovery,
  I here dub “ainheritance” the concept of “inheritance”
  when the name of the entity inheriting from others starts with “a”,
  “binheritance” the concept of “inheritance” when the programmer’s console is blue,
  “cinheritance” the concept of “inheritance” when the programmer is in China,
  and “sinheritance” the concept of “inheritance”
  when the specification is not conflated with its target,
  therefore neither a class nor a prototype.
  Also “ninheritance” when there is no actual inheritance, and
  “tinheritance” when it looks like inheritance, but is not real inheritance,
  just target extension without open recursion through a module context.
  I am also reserving the namespace for variants of the name starting
  with a heretofore unused letter, unicode character, or prefix of any kind,
  and launching the Interplanetary Xinheritance Foundation to auction the namespace away,
  as well as the related Intergalactic Zelegation Alliance.
  I am impatiently awaiting my Turing Award, or at least Dahl Nygaard prize,
  for all these never discussed before original inventions related to OO.

  The Lieberman paper deserves its thousands of citations because it is a great paper.
  However, a lot of citers seem to fixate only on the unfortunate choice
  of concept delineation and naming by Lieberman,
  who probably did not anticipate that he would set a bad trend with it.
  The delineation made sense in the historical context of the Actor team
  separately implementing prototypes and classes with related yet distinct mechanisms
  in their ACT1 language @~cite{Hewitt1979}, way before they or anyone understood
  how classes were a special case of prototypes.
  But too many readers took this historical artifact as an essential distinction,
  and thereafter focused on studying or tweaking low-level “message passing” mechanisms
  on a wild goose chase for tricks and features,
  instead of looking at the big picture of the semantics of inheritance,
  what it actually is or should be and why,
  what is or isn’t relevant to its semantics.
  Concept delineation and naming is tremendously important;
  it can bring clarity, or it can mislead hundreds of researchers into a dead end.
}

One confounding factor is that of mutable state.
Early OO, just like early FP, was usually part of systems with ubiquitous mutable state;
prototype inheritance (or “delegation”) algorithms thus often explicitly allow or cope with
interaction with such state, including
mutation and sharing or non-sharing of per-object or per-class variables,
and especially tricky, mutation and sharing of a prototype’s inheritance structure.
However, class systems often had all their inheritance semantics resolved at compile-time,
during which there is no interaction with user-visible side-effects, and
it doesn’t matter whether the compiler does or doesn’t itself use mutable state:
from the user point of view it is as if it were pure functional and there is no mutation
in the inheritance structure or state-sharing structure of classes,
at least not without using “magic” reflection primitives.
One may then have been tempted then to see Prototype Delegation as intrinsically stateful,
and class inheritance as intrinsically pure (though at compile-time).

Yet, recent pure functional Prototype OO systems @~cite{jsonnet nix2015 poof2021}
prove constructively that prototypes can be pure, and that they use
the very same inheritance mechanisms as classes,
indeed with classes as a particular case of prototypes with the usual construction.
Meanwhile, old reflective Class OO systems like Lisp and Smalltalk
@~cite{Kahn1976 Kay1993EHoS Gabriel1991CLOS AMOP}
also support mutable state to modify the inheritance structure at runtime,
for the sake of dynamic redefinition of classes at runtime,
in what remains semantically a pure functional model once when the structure is set.
See how in CLOS you can define methods on generic function
@c{update-instance-for-redefined-class} to control how data is preserved, dropped or transformed
when a class is redefined. @;{TODO XXX @~cite{}}
Mutable state and mutable inheritance structure in particular are therefore
clearly an independent issue from prototypes vs classes,
though it might not have been obvious at the time.
As I introduce formal models of OO,
I will start with pure functional models (see @secref{MOO}), and
will only discuss the confounding matter of side-effects much later
(see @secref{SOO})@xnote["."]{
  It might be interesting to explain @emph{why} many authors failed so systematically to
  identify delegation and inheritance, when the similarities are frankly obvious,
  and the relationship between classes and prototypes is well-known
  to anyone who implemented classes atop prototypes.
  But lacking direct access to those authors’ brains, my explanations must remain speculative.

  First, pioneers are eager to conceptualize and present their experiments as original
  and not just the same concept in a different context.
  They necessarily have to sell their ideas as historical package deals,
  before the underlying concepts are clearly identified and separated from each other.
  They are too close to the matter to tell which of the features they built would be immortalized
  through the ages as fundamental concepts
  vs just contingent implementation details soon to be forgotten.
  In the brief time that publishing about Prototypes was trendy,
  scientists studying pioneering works may have focused too much
  on the specifics of Actors, Self, or other successful Prototype language du jour,
  and failed to properly conceptualize a general notion of Prototype.
  Unlike the pioneers themselves, they deserve blame for their myopia,
  and so do the followers who cite and repeat their “findings” without criticism.
  However this explanation is not specific to the topic at hand,
  and is valid for every field of knowledge.

  Second, and with more specificity to Prototypes,
  Computer Scientists following the Programming Language (PL) paradigm@~cite{Gabriel2012}
  might have been unable to unify Prototypes and Classes
  when delegation happens at runtime while inheritance happens at compile-time:
  not only does the machinery look very different to users and somewhat different as implementers,
  written in different languages with different formalisms,
  but PL people tend to deeply compartmentalize the two.
  They may have looked at low-level mutable state
  (omnipresent in any practical language until the mid-2000s)
  as essential when happening at runtime,
  when they could clearly conceptualize it away as an implementation detail
  when happening at compile-time.
  Systems paradigm people (including the old Lisp, Smalltalk and Self communities)
  who freely mix or interleave runtime and compile-time in the very same language,
  might have had no trouble unifying the two across evaluation times,
  but they tend not to publish articles about PL semantics,
  and not to be read by most PL semanticians, or
  not understood by those that do read the articles.

  Revisiting these topics several decades after they were in vogue,
  and finding their then-treatment lacking, with errors from the time still uncorrected to this day,
  makes me wonder about what other false ideas I, like most people, assume are true
  in all the other topics I haven’t revisited, whether in Computer Science or not,
  where I just blindly assume the “experts” to be correct due to Gell-Mann amnesia.
}

As for which words to keep, the word “inheritance” was used first for the general concept,
in a language with “prototypes”.
The word “delegation” stems from the Actor message-passing model,
and is both later and less general,
from after the words “inheritance” and “prototypes” were better established,
and is strongly connoted to specific implementations using the message-passing paradigm.
It also fell out of fashion some time in the 1990s,
after JavaScript became a worldwide phenomenon, and (correctly) used the term “inheritance”
rather than delegation (as it isn’t particularly “message passing”, just calling functions).
@~cite{ecmascript1997}

@section{Epistemological Digression}
@epigraph{
  Knowledge is something which you can use.
  Belief is something which uses you.
  @|#:- "Idries Shah"|
}
Many people will inevitably quibble about my definition or characterization of OO
as opposed to their own or someone else’s.
Though a treatise of epistemology is beyond the scope of this book, @;{TODO cite}
I can briefly answer the most frequent epistemological questions as follows.

This section is not essential to the formalization of OO in the chapters that follow,
and can be skipped.
I am aware that my answers may shock and turn off some of my readers.
Nevertheless, I believe this section is very relevant to the debate at hand,
and worth publishing as is.

If a philosophical disagreement with this section
will turn you off from reading subsequent technical chapters,
maybe you should skip this section, or only return to it
after you read those more technical chapters.
If so, you should also be careful never to ask about the philosophical or religious opinions
of authors, inventors, colleagues, etc., in your technical field, or any field,
least you find yourself alienated from much of human knowledge.


@subsection[#:tag "Imdc"]{Is my definition correct?}
@epigraph{
  The truth or falsehood of all of man’s conclusions, inferences, thought and knowledge
  rests on the truth or falsehood of his definitions.
  @|#:- "Ayn Rand"|
}
Yes, my definition is correct:
it accurately identifies what people usually mean by those words,
and distinguishes situations where they apply from situations where they do not,
in the contexts that people care about.
People using my definition will be able to make good decisions,
whereas those using other definitions will make bad decisions where their definitions differ.

@subsection{What does it even mean for a definition to be correct?}
@epigraph{
  “When I use a word,” Humpty Dumpty said, in rather a scornful tone,
  “it means just what I choose it to mean—neither more nor less.”
  @linebreak[]
  “The question is,” said Alice, “whether you can make words mean so many different things.”
  @linebreak[]
  “The question is,” said Humpty Dumpty, “which is to be master—that’s all.”
  @|#:-"Lewis Carroll"|
}
Some people will argue that definitions are “just” arbitrary conventions,
and that there is therefore no rational criterion of correctness,
only arbitrary political power of the strong over the weak,
to determine what the definitions of words are or should be.

But no, such a point of view is worse than wrong—it is outright evil.
The phenomena that effectively affect people,
that they care to name, discuss, think about and act on, are not arbitrary.
Thus the important part of definitions isn’t convention at all:
it is the structure and understanding of these phenomena, rather than the labels used for them.
A correct definition precisely identifies the concepts that are relevant to people’s concerns,
that help them make better decisions that improve their lives,
whereas an incorrect definition misleads them into counterproductive choices.
Specifically overriding your reason with power is an act of war against you,
and generally overriding all reason with power is the very definition of evil.

@subsection{Is there an authority on those words?}
@epigraph{Those who need leaders aren’t qualified to choose them.
  @|#:- "Michael Malice"|
}
No, there is no authority on vocabulary, person or committee,
that can decree different words for others to use,
or different phenomena for others to care about,
whether about computing or any other field of human endeavor.
People care about a phenomenon currently identified under the moniker OO,
and even if some “authority” manages to change the name for it,
or to denature the name “OO” not to identify the same phenomenon anymore,
then people will keep caring about what they now call OO under a different name,
rather than care about whatever those who corrupt the name may want them to.

@subsection{Shouldn’t I just use the same definition as Alan Kay?}
@epigraph{OOP to me means only messaging,
local retention and protection and hiding of state-process,
and extreme late-binding of all things. @|#:- "Alan Kay"|
}
No, that isn’t possible, nor would it be appropriate if it were.
Alan Kay coined the expression “Object Oriented Programming” in 1967.
Originalists might say everyone must take it to mean whatever He defined It to mean,
and sometimes cite him as in the epigraph above.

But neither the above @~cite{Kay2003} nor any of Kay’s pronouncement on OO constitutes
a precise definition with an objective criteria,
if a definition at all@xnote["."]{
  My interpretation is that the first part of this definition (until the last comma)
  corresponds to modularity, the ability to think about programs in terms of separate
  “local” entities each with its own “state-process” wherein interactions only happen
  through well-delimited interfaces (“messaging”).
  The second part “extreme late-binding of all things” indirectly references
  the in-language and extensible aspect of modules:
  extreme late-binding means that the value of those units of modularity may change at runtime,
  which means not only dynamic dispatch of method invocation
  depending on the runtime class of an object,
  but also the ability to dynamically
  define, incrementally extend, refine or combine those units in the language.
  Those units may be first-class prototypes, and even when they are only second-class classes,
  there is a first-class reflection mechanism to define and modify them.
  When this extensibility is only available at compile-time,
  as in the object system of many static languages, then
  the OOP only happens in the meta-language (as in e.g. C++ templates),
  or the language lacks complete support for OOP.

  Note that Kay didn’t immediately adopt Simula’s inheritance mechanism in Smalltalk-72
  (it wasn’t called that yet in Simula, either);
  but he did adopt it eventually in Smalltalk-76,
  notably under the push of Larry Tesler
  (who previously used “slot inheritance” on early desktop publishing applications),
  and this adoption is what launched OO as a phenomenon.
  Kay stated adopting single inheritance over multiple inheritance
  was a compromise @~cite{Kay1993EHoS};
  his team later added multiple inheritance to Smalltalk @~cite{Goldstein1980Extending}, but
  it is unclear that Kay had much to do with that addition, that never became standard.
  More broadly, Kay didn’t endorse any specific inheritance mechanism,
  and never focused on that part of the design. To Kay it was only a means to an end,
  which is what Kay called “extreme late binding”: the fact that behavior definition
  happens and takes effect dynamically up to the last moment based on values computed at runtime.
  Inheritance, the practical means behind the late behavior definition that is late bound,
  and the precise form it takes, is secondary to Kay;
  what matters to Kay is the role it plays in enabling dynamic code specialization.
  But inheritance becomes a primary concern to whoever wants to formalize the concepts behind OO,
  and must refine the intuitions of a pioneer into codified knowledge after decades of practice.
  And if other means are found to arguably satisfy Kay’s “extreme late binding”,
  then they’ll have to be given a name that distinguishes them from what is now called OO.
}
And even if he had at some point given a definition,
one still should remain skeptical of what Kay, and other pioneers, said,
if only to recursively apply the same semantic attention to the definition of the words
they used in their definitions.
Now, one should certainly pay close attention to what pioneers say,
but one should pay even closer attention to what they @emph{do}.
The pioneer’s authority lies not in precise words, but in inspiring or insightful ones;
not in well-rounded neatly-conceptualized theories,
but in the discovery of successful new practices that are not yet well understood.
Solid theories arise only after lots of experience, filtering, and reformulation.

@subsection{Shouldn’t I just let others define “OO” however they want?}
@epigraph{The opinion of 10,000 men is of no value
  if none of them know anything about the subject. @|#:- "Marcus Aurelius"|
}
Not at all.
Some people are reluctant to fight over the meaning of words,
and are ready to cave to popular opinion or spurious authorities
when they define and redefine “OO” or any word to have whatever precise or murky meaning.
Instead they propose that I should stick to “inheritance”
when discussing the field characterized by the use of inheritance.

But it is no good to let an ignorant majority “define” the term “Object Orientation”
to mean what little they know of it—for instance, to pick the most popular elements:
Class OO only, always mutable records,
only single inheritance or C++ style flavorless “multiple inheritance”,
only single dispatch, no method combination, etc.
Letting those who don’t know and don’t care define technical words
would be knowledge bowing to ignorance;
it would be for those who know and care to abdicate their responsibility
and follow the masses when they should instead lead them;
it would be ceding terrain to the Enemy—snake oil salesmen, chaosmongers,
corrupters of language, manipulators, proud spreaders of ignorance, etc.—who if let loose
would endlessly destroy the value of language and make clear meaning incommunicable.
Beside, if you retreat to “inheritance” in the hope that at least for that term
you can get people to agree on a clear unambiguous meaning@xnote[","]{
  The term “inheritance” is already corrupted,
  since Goguen uses it at times to mean refinement @~cite{Goguen1992Sheaf}
  while claiming to do OO,
  and others use it to mean the (non-modular) extension of database tables or equivalent.
  Moreover, the term “inheritance”, that originated in KRL,
  in parallel to the adoption and evolution it saw in the field of OO,
  also had its evolution in the field of
  Knowledge Representation, Description Logics, Semantic Web, etc.
  And there are plenty of further legitimate non-OO uses of the word “inherit”, to
  mean that some entity derives some property from a historical origin, an enclosing context, etc.
}
you’ll find that if you have any success defining a useful term that way,
the agents of entropy will rush to try to defile it in direct proportion to your success;
you will have given up precious lexical real estate for no gain whatsoever,
only terrible loss@xnote["."]{
  Indeed, if you don’t know to stand your ground, you will constantly retreat,
  and be made to use ever more flowery “politically correct” vocabulary
  as a humiliation ritual before those who will wantonly take your words away
  to abuse you and thereby assert their dominance over you.
}

@subsection{So what phenomena count as OO?}
@epigraph{The medium is the message.
  @|#:- "Marshall McLuhan"|
}
What defines OO is not the metaphors of those who invent, implement, or comment about it
as much as the design patterns used by programmers when they write code in an OO language;
the interactions they have with computers and with each other;
the decision trees that are enabled or disabled when evolving a program into another—these
phenomena are what OO is.
What programmers do, not what programmers say.

And these phenomena are what is captured by
the intra-linguistic extensible modularity as defined above:
(a) the ability to “code against an interface” and
pass any value of any type that satisfies the interface
(modularity, whether following structural or nominative rules),
(b) the ability to extend and specialize existing code by creating a new entity
that “inherits” the properties of existing entities and only needs specify
additions and overrides in their behavior rather than repeat their specifications,
wherein each extension can modularly refer to functionality defined
in other yet-unapplied extensions; and
(c) the fact that these entities and the primitives to define, use and specialize them
exist @emph{within} the programming language rather than in an external preprocessing layer.

I contend that the above is what is usually meant by OO,
that matches the variety of OO languages and systems
without including systems that are decidedly not OO, like Erlang, SML or UML.
Whatever clear or murky correspondence between names and concepts others may use,
this paradigm is what matters, and is what I will call OO—it is what I will discuss in this book,
and will systematically reduce to elementary concepts.

As to why should this particular meaning of “object-oriented” should win over
other plausible meanings offered before or after Kay’s and Bobrow’s 1976 invention,
or other names for the concept, I will conclude with this tweet by Harrison Ainsworth:
@principle{Naming is two-way: a strong name changes the meaning of a thing, and
a strong thing changes the meaning of a name.}


@exercise[#:difficulty "Easy"]{
  Identify cases where what I claim OO is @emph{not},
  contradicts your prior assumptions about what OO was.
}

@exercise[#:difficulty "Easy"]{
  Use your experience, or AI, to identify—for each (sub)section of this chapter
  (or at least a couple of them picked at random)—examples
  of how OO (as I described in the previous chapter)
  and the concept often wrongly identified with it (as described in this chapter) do not coincide:
  either something that is an instance of OO and not the other,
  or something that is an instance of the other and not OO.
  Be careful not to blindly trust the answer of AI,
  especially as your prompt may lead it to slightly misunderstand the question,
  if the context of this chapter’s text is not included.
  Notably, it’s not just C++ or UML that are not OO, but their object model.
  Consider (sub)sections of the present chapter in a random order, so that,
  if you stop before the end, you didn’t just do the first few (sub)sections like everyone else.
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
  Wrong targets for which criticism will not count as answers to this exercise
  (though they may be otherwise interesting), include:
  (1) criticism of things I already denounced as not being OO,
  (2) criticism of particular systems that happen to written with OO,
    but for which this aspect of the system is irrelevant,
  (3) criticism of mistakes that do apply to OO,
    but actually, much more broadly, apply unchanged to software development in general.
  (This is notably harder than the previous exercise!)@Note{
    As a hint, you may consider the criticism from @citet{Graham2001noop},
    and the reply by Rees; or @citet{Armstrong2001}, or @citet{Gabriel2002}.
    Which of the points actually pertain to OO and which don’t?
    You don’t have to have perfect answers, especially before I even explained in detail what OO is.
    The point of the exercise is to engage critically with what OO is or isn’t,
    and what its costs and benefits may be.
    You can revisit this exercise after you’re done reading the book.
  }
}

@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{02to03},
  compare your previous answers to mine.
  See what surprised you, and how your understanding evolved.
}

@exercise[#:difficulty "Hard, Recommended" #:tag "03to04"]{
  Based on this informal overview, and before you read the next chapter,
  try to write down your own short theory of what the main concepts like “modularity”,
  “extensibility” and “internality” might mean, and what formalizing them might look like.
  Bonus if you can then explain how the three together
  can mean something more than the same three apart.
  Save your answer to compare with the treatment in @secref{OOaIEM}.
}

@exercise[#:difficulty "Research"]{
  Find some other technique, field of knowledge, school of thought, ideology, etc., beside OO,
  that, having once been trendy or popular,
  was overtaken by plenty of people wrongly claiming its name,
  to advance very different sets of ideas.
  Characterize the real thing under the original name,
  and the main variants that corrupt the name
  (though they may have interesting contributions of their own beside this corruption)@xnote["."]{
If you have trouble with this question, you may consider digging on the etymology
and early history of the word “ideology” itself.
But it’s much better if you manage to find your own example of such hostile take-over of a word.
}}
