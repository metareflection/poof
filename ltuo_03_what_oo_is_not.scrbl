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

On the other hand, if you are innocent of preconceptions about OO
and come without having heard commentators explain what they think OO is
and what they love or hate about it
(which is unlikely if you’re interested enough in OO to read the present book),
then you may wholly skip this chapter,
and only come back after you encounter bad opinions about OO that need to be debunked.

@section{Things OO isn’t (that many claim it is)}
@epigraph{When words are unfit, speech is unadapted and actions are unsuccessful.
@|#:- "Confucius"|
}

@subsection{OO isn’t Whatever C++ is}
@epigraph{
  I made up the term ‘object-oriented’, and I can tell you I didn’t have C++ in mind.
  @|#:- "Alan Kay, at OOPSLA ’97 (near peak C++ popularity)"|
}
@subsubsection{Influential, Yet Atypical.}
The most popular OO language during the decades that OO was a popular trend (roughly 1980 to 2010),
C++ indeed supports some form of OO.
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
Now, C++ supports single inheritance well;
but what it calls “multiple inheritance” @~cite{Stroustrup1989}
is not at all the same as what almost everyone else calls “multiple inheritance”@xnote[":"]{
  Interestingly, the design of C++ non-virtual superclasses is very similar
  to the solution from Snyder’s CommonObjects @~cite{Snyder1986},
  even though Stroustrup does not cite Snyder:
  redefine the problem to be whatever the desired “solution” does—a Tree instead of a DAG—and
  hope the users won’t notice the difference.
  On the other hand, Stroustrup does cite the Lisp Machine Manual @~cite{Weinreb1981},
  and rejects Flavors because it is not
  “sufficiently simple, general and efficient enough to warrant the complexity it would add to C++”;
  this is exceedingly ironic considering Flavors was 1.4kloc (in October 1980, when cited),
  and C++ ~100kloc (in 1989, when citing),
  with 1980 Flavors having much richer and more general OO functionality than C++,
  not to mention its distant 1988 successor CLOS.
}
It is actually a modified kind of mixin inheritance
with some kind of “duplication and renaming” of superclasses
(for non-@c{virtual} superclasses, with members copied/renamed along the inheritance tree),
and a subset of multiple inheritance (for @c{virtual} superclasses and member functions,
with restriction from a “conflict” view of inheritance, see @secref{DMRMI}).
Notably, C++ lacks the proper method resolution that enables a lot of
the modularity of multiple inheritance in other languages@xnote["."]{
  The situation is somewhat similar for PHP,
  that adopted a form of multiple inheritance in 2012
  that follows the “conflict” approach of C++ for its “traits”
  on top of its single inheritance “classes” (@secref{SotAiMSaMI}).
  Now even when C++ got multiple inheritance wrong,
  ignorance was no valid excuse,
  since Lisp got it right ten years earlier@~cite{Cannon1979}
  and Stroustrup even cited it via @~cite{Weinreb1981}.
  Ignorance is even less excusable in the case of PHP
  copying C++’s “multiple inheritance” over two decades later.
  By contrast, many languages got it right in the same time frame,
  including Common Lisp (1988), Python (1991), Ruby (1995), Scala (2004).
}

Now, you can use C++’s powerful template language to reconstitute actual mixin inheritance
and its method resolution on top of C++’s weird variant of inheritance@~cite{Smaragdakis2000};
and you could no doubt further implement proper multiple inheritance on top of that@xnote["."]{
  One could achieve multiple inheritance as a design pattern on top of mixin inheritance,
  as I will describe later in this book:
  developers would manually compute and specify
  each class’s superclass precedence list.
  But this cancels some of the modularity benefits of multiple inheritance
  versus single and mixin inheritance.
  Alternatively, someone could extend the above technique to also reimplement
  the entire superclass linearization apparatus
  within the C++ template metaprogramming language.
  Template metaprogramming is most definitely powerful enough for the task,
  though it will take a very motivated developer (or an AI) to do the hard work @~cite{Rideau2026cxx}.
  Moreover, the result will still be a slight syntactic burden for any developer who wants to use it,
  and classes defined that way would only interoperate
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
In the end, C++ is many great and not-so-great things, but only a few of those things are OO;
and even most of what looks like OO in C++ is often different enough from OO that
@principle{C++ does not reliably inform about OO in general}.

@subsubsection{OO isn’t Defined by Any Particular Language or System}
OO is a general concept that is not defined by any single particular instantiation of it,
however advanced, popular, or declared as prototypical by any particular pundit.
OO is especially not whichever OO language you, the reader, first learned,
or got to most associate with OO in your mind.
Smalltalk, CLOS, Ruby, Python, Java, JavaScript, C#, Scala, and many more...
however great or lame in many ways, none of these language is OO.
Instead, OO is precisely what these languages have in common.
No language can embody OO and be “all OO, and nothing but OO”,
because @principle{OO is not a language design, it’s a language @emph{feature}}.
Yet if I had to exhibit a minimal, “prototypical” OO language (pun intended),
I’d pick... Jsonnet (@secref{RPOO}).

@subsection[#:tag "OOiCO"]{OO isn’t Classes Only}
@epigraph{
  The class/instance distinction is not needed if the alternative of using prototypes is adopted.
  @|#:- @citet{Lieberman1986}|
}
Many claim that classes, as first implemented by Simula 67@~cite{Dahl1967}
(though implementing a concept previously named by Hoare@~cite{Hoare1965}),
are essential to OO, and only ever care to implement, use, formalize,
study, teach, promote, or criticize class-based OO (a.k.a. Class OO).
Books from luminaries in Programming Languages @~cite{Pierce2002 Khrisnamurthi2008 Friedman2008},
in their chapter about OO, barely even mention any other kind of OO if at all,
much less study it.

Yet KRL@~cite{Bobrow1976}, the very first system
that claimed the words “object-oriented” in print with the modern sense (@secref{OOnaming}), and
also introduced the words “inheritance” and “prototype” in their OO meaning,
has what is now called prototype-based OO (a.k.a. Prototype OO).

Certainly, Smalltalk, that made OO popular, was class-based, unlike KRL.
But Smalltalk adopted inheritance after KRL.
And the very next OO languages immediately after Smalltalk
were both prototype-based:
Director @~cite{Kahn1976 Kahn1979Ani Kahn1979Director} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}@xnote["."]{
  ThingLab was built on top of Smalltalk by members of the same team at PARC,
  and oscillated between having or not having classes in addition to prototypes.
  Meanwhile, Kahn, who wrote Director, joined PARC after graduating from MIT.}
Plenty more Prototype OO or “class-less” OO languages followed
@~cite{Hewitt1979 Rees1982 Adams1988 Ungar1987 Chambers1989 Lawall1989 Salzman2005 Cunningham2014 Simons2015 Rideau2021}. @; TODO Dekorte2005
There are a lot more Prototype OO languages than I could have time to review @~cite{WikiProto},
but prominent among them is JavaScript @~cite{Eich1996},
one of the most used programming languages in the world @~cite{GitHub2022},
maybe the top one by users
(though it relatively recently also adopted classes on top of prototypes @~cite{ECMA2015}).

Moreover, I will argue that Prototype OO @~cite{Borning1986}
is more general than Class OO, that is but a special case of it @~cite{Lieberman1986}
(@secref{CaPfT}, @secref{RCOO}).
And I will even argue that you can recognizably have OO
with neither classes nor even prototypes
(@secref{MFtPaC}, @secref{MOO}, @secref{ROOfiMC}).
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
classes are precisely an application of OO to types (@secref{P&C}, @secref{RCOO}).

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
“closures are a poor man’s objects”@~cite{Queinnec1996},
the problem back then (and as early as at least Yale T Scheme @~cite{Rees1982},
that developed the underlying concepts and implemented an entire system on them),
was clearly not whether OO could be done purely with functions—obviously it could—but
whether it made practical sense to program purely without side-effects in general.
That question would only be slowly answered positively,
in theory in the early 1990s @~cite{Moggi1991}
and in practice in the mid 2000s to mid 2010s,
as Haskell grew up to become a practical language@xnote["."]{
  Some identify darcs (2003) as the first widely used real-world application written in Haskell.
  After it came innovations such as bytestring (2005), cabal (2005)
  (and the “cabal hell” it started causing around 2006 until later solved by Stack),
  ghc6 (2006), that made Haskell much more practical to use, and
  new notable applications appeared like pandoc (2006), or xmonad (2007).
  A turning point was perhaps the publication of “Real World Haskell” @~cite{OSullivan2008}.
  Eventually, Stack (2015) made non-trivial Haskell programs and scripts repeatable.
  Now there’s obviously a lot of subjectivity in deciding
  when exactly Haskell became “practical”—but one should expect
  the transition to practicality to be an S curve, such that
  whichever reasonable yet somewhat arbitrary threshold criteria you choose,
  the answer would be at about the same time.

  In any case, making a practical language pure functional was just not an option before 2005 or so,
  and it is absurd to declare any programming language concept intrinsically stateful
  merely because all its practical implementations before 2005 were stateful.
  You could similarly make the absurd claim that logic programming, functional programming,
  or linear algebra are intrinsically stateful.
}

Yet, there are:
@itemize[
  @item{Pure models of OO such as those of
    Kamin, Reddy, Cook and Bracha@~cite{Kamin1988 Reddy1988 Cook1989 CookPalsberg1989 Bracha1990},}
  @item{pure lazy dynamic OO languages such as Jsonnet or Nix@~cite{Cunningham2014 Dolstra2008 Simons2015},
    and pure lazy OO systems for Scheme@~cite{Rideau2021},}
  @item{languages happily combining OO and FP such as Common Lisp or Scala,
    with plenty of libraries restricting themselves
    to pure functional objects only @~cite{Rideau2012 Chiusano2014}, and}
  @item{last but not least, Oleg Kiselyov’s or Michael Gale’s implementations of
    statically typed OO both stateful and pure as libraries in the pure FP language Haskell(!)
    @~cite{Kiselyov2005 Gale2015}.}]

These provide ample evidence that OO does not at all require mutation,
but is very compatible with FP, purity, and even with laziness and consistent static typing.
(But this does not mean that Haskell typeclasses or Rust traits are OO;
they are not—see @secref{CSvTS}.)
Actually, I will argue from the semantics of OO, that
@principle{Pure Lazy Functional Programming is the natural setting for OO}
(@secref{UPSLC}).

@subsection{OO isn’t Encapsulation}
@epigraph{A half-truth is a whole lie. @|#:- "Yiddish proverb"|
}
@subsubsection{The Information Hidden: Modularity}
Many OO pundits claim that an essential concept in OO
is “encapsulation” or “information hiding”@~cite{DeRemer1975}.
Some instead speak of “data abstraction” or some other kind of “abstraction”.
There is no consensus as to what this or these concepts mean, and no clear definition,
@; TODO{CITE} @; XXX cite Liskov??? Mary Shaw???
but overall, these words refer either (a) to part or all of what I call @emph{modularity}
(@secref{MO}, @secref{M}),
or (b) to some specific set of visibility primitives in some OO languages.

Indeed, “encapsulation” usually denotes the ability to code against an interface,
with code on either side not caring which way the other side implements its part of the interface,
not even being able to distinguish between multiple such implementations,
even less to look inside at the state of the other module.
Viewed broadly, @principle{encapsulation is another name for modularity, which is only half of OO}.
Meanwhile the word modularity much better than “encapsulation” identifies
the broader purpose of the concept, beyond a mere technical property.
Whichever way you name it, modularity only characterizes half of OO,
so that people who try to equate OO with only that half
crucially miss the other half—@emph{extensibility} (@secref{EO}, @secref{E})—and
thus fail to properly identify OO.

@subsubsection{Tunnel Vision: Visibility}
Now, insofar as some people identify encapsulation narrowly as the presence
of specific visibility mechanisms such as found in C++ or Java
(with some attributes or methods being @c{public}, @c{private} or something in-between,
whose precise semantics the designers of different languages cannot agree on;
see @secref{ST}),
I’ll easily dismiss such mechanisms as not essential to OO:
indeed many quintessential OO languages like Smalltalk or Common Lisp
lack any such specific mechanism,
whereas many non-OO languages possess mechanisms to achieve the same effect,
in the form of modules defining but not exporting identifiers
(e.g. not declaring them @c{extern} in C),
or simply lexical scoping@~cite{Rees1995}.
@; TODO{cite Simula? JS?}

Certainly, these mechanisms can be very useful,
worthy features to add to an OO language.
They are just not essential to OO and not specific to it,
though of course their adaptation to OO languages will follow
the specific shape of OO constructs not found in non-OO languages.
Misidentifying OO as being about these mechanisms rather
than about the modularity they are meant to support can only lead to
sacrificing the ends to the means.

@subsubsection[#:tag "Cook"]{William R. Cook}
One remarkable researcher made a particularly eloquent case
for reducing “OO” to mere first-class modularity (my word, but with equivalent definitions):
William R. Cook, the man who, each time with suitable collaborators,
first formalized single inheritance in the λ-calculus @~cite{Cook1989 CookPalsberg1989},
dispelled the long running belief that inheritance was subtyping @~cite{Cook1989Inheritance},
introduced the first correct typesystem for inheritance @~cite{Canning1989},
invented and formalized mixin inheritance @~cite{Bracha1990},
and even studied uses of inheritance beyond what he considered OO @~cite{Brown2009}.
That same man who did more than anyone to further
the understanding of the formal semantics of inheritance,
repeatedly claimed that inheritance was not essential to OO @~cite{Cook1991 Cook2009 Cook2012},
and that its uses beyond first-class modularity were not OO.

However, coding against first-class SML modules would count as OO by Cook’s criteria,
while static dispatch in C++ or C# would count as not-OO.
Indeed Cook explicitly calls the untyped λ-calculus “the first object-oriented language”
@~cite{Cook2009 Cook2012},
and classifies Go as OO @~cite{Cook2012},
while downplaying Smalltalk as not OO enough because its integers are not pure objects@~cite{Cook2009}.
Thus Cook’s definition, that embraces the modular aspect of OO (when first-class) while rejecting
its extensibility aspect or its static variants, runs contrary to common practice.
It brings no light on any of the languages commonly considered OO
yet derided by Cook as not being OO enough,
no light on any of the Functional Programming (FP) languages blessed by Cook as actually being OO
to the likely surprise of their users, and no light on the difference between the two.
It is a typical case of an expert (and what an expert!) being so right in his analysis,
yet so wrong in his delineation of concepts (@secref{Experts}).

In the end, Cook’s PhD and subsequent academic career grew out of
brilliantly modeling the key mechanism of OO (Inheritance)
from the foreign point of view of FP—its What and How.
But his lack of appreciation and understanding for the OO tradition,
indeed missing the point of it all—its Why and Wherefore—were such
that they have become proverbial: immortalized in Gabriel’s essay
“The Structure of a Programming Language Revolution” @~cite{Gabriel2012}
as a prototypical failure to understand a phenomenon when viewed
through a scientific paradigm incommensurable with the one that produced it.
The problem is not just that Cook solved Inheritance as frog and
failed to take the big picture as a bird: he did take a bird’s view,
and still couldn’t see what his paradigm couldn’t express.

Cook’s view is marginal, and goes against the vast majority of OO practitioners.
And I will keep arguing why that view is objectively incorrect,
even though it is positively cited by many academics.
It is not just some marginal author using unusual vocabulary:
the view is worth mentioning precisely because Cook is so influential—indeed landmark.
Cook’s many works on OO over the years also systematically neglect or downplay
important concepts in OO, such as prototypes, multiple inheritance,
method combination or multiple dispatch—a disdain actually shared by most academics.
And the mismatch is a symptom that the field lacks a consensual vocabulary, and
commonly accepted concepts, definitions—and more deeply, a paradigm—adequate
to reconcile theory, practice and history.
And yet, there are undeniably common practices, common phenomena, common concepts,
common language features, common design patterns, common goals, common aspirations,
worth understanding, conceptualizing, defining and naming
in the rich (though sometimes mutually conflicting) traditions
that grew around OO.

@subsection{OO isn’t Opposite to FP}
@; I destroy my enemies when I make them my friends. — Lincoln
@; What about side by side with a friend? — Legolas in The Two Towers (movie)
@epigraph{¿Por qué no los dos? (Why not both?)
  @|#:- "Old El Paso"|
}
@subsubsection{The Long Flamewar}
For as long as there has been social media,
dating back at least to USENET in the late 1980s or very early 1990s,
there have been mutually hostile exchanges, a.k.a. “flamewars”,
between those I call zealOOts and FPanatics (respective proponents of OO and FP).
These heated exchanges assume or argue that there is an essential conflict between OO and FP,
between Inheritance and Composition,
wherein OO is about modeling every possible domain in terms of inheritance,
and FP is about modeling every possible domain in terms of composition,
and the two must somehow duel to the death.

But OO and FP, inheritance and composition, are just pairs of distinct concepts.
Neither subsumes the other; each fits a distinct set of situations@xnote["."]{
  Each distinct concept has its set of situations that it fits,
  distinct from that of any other concept (or else they are actually the same concept).
  A concept that fits all situations has no content and is useless;
  and two concepts like OO and FP neither of which subsumes the other,
  cover sets of situations neither of which is a subset of the other.
}
It makes no sense to oppose them, especially not when one can see that
OO can be implemented in a few lines of FP, whereas
most modern OO languages contain FP as a subset—and
Lisp has harmoniously combined OO and FP together ever since they both emerged in the 1970s,
decades before anyone had the idea to fantasize a conflict between the two.
As for those who argue that FP requires rich static types:
Scala has popularly combined OO, FP and types since 2005, followed by TypeScript in 2012,
not to mention earlier research experiments.

@subsubsection{Composition vs Inheritance}
Often, a FPanatic will raise an argument from authority,
citing the “second principle of object-oriented design”
from the famous “Gang of Four” (“GoF”) book @~cite{Gamma1994}:
“favor object composition over class inheritance”.
The FPanatic will thus claim that even famous OO pundits recommend composition over inheritance.
Checkmate, zealOOt!

However the original says “object composition” and not “function composition”@xnote[":"]{
  If you squint a bit, object composition can be seen as
  a special case of function composition where the functions are the object constructors.
  But it’s not a stand-in for the general case, and the “favor” does not mean that
  composition can substitute for inheritance in every situation.
  Quite the contrary, the “principle” crucially assumes and implicitly states
  that there are situations where class inheritance applies and object composition doesn’t.
}
it is not at all an argument about FP vs OO—it is a heuristic for using OO effectively,
that assumes OO either way.
Citing the slogan out of context, misquoting it as “favor composition over inheritance”,
and repeating it without checking the original, speaks poorly of the utterer.

This “principle” is worth examining for the lessons it teaches us about OO.
It compares “[t]he two most common techniques for reusing functionality in object-oriented systems”,
“class inheritance and object composition”.
The first approach, class inheritance, the authors describe as “white-box reuse”,
wherein the new class has access to the internals of the old one.
The second approach, object composition, the authors describe as “black-box reuse”,
wherein the new class does not have that access.

GoF argues not to create an exponential number of subclasses
that specialize based on static information about what is or could be a runtime value,
because classes are compile-time and human-developer-time objects
that are less flexible and costlier in human effort than runtime entities.
However, this argument, while correct in the context of Class OO,
does not apply for Prototype OO, wherein umpteen combinations of prototypes
(and classes as a particular case) can be generated at runtime
at no additional cost in terms of human effort.

Once you understand the notion that prototypes in general and classes in particular
are the conflation of two distinct entities, a specification and the target you may compute from it,
a better argument can be made for the “principle”
as a special case of the “Principle of Least Privilege” @~cite{Saltzer1973}:
@principle{every program should be granted the least privilege necessary to get its job done}—including
every module of a larger program.
It’s easy enough to add a new privilege to an existing program when you need it,
it’s much harder to audit every use case of an existing program
to see if you can safely remove an old privilege—or to fail to
and later deal with bugs and vulnerabilities.

In this case, since you can compute a target from its specification,
accessing the specification is a greater privilege than just holding a target;
therefore sharing just a target, through “composition”,
is preferable to sharing a specification through “inheritance”—when possible.
Hence, if a new specification doesn’t need to reuse the computation of the old one,
it should just reuse its target. Moreover, even if you must reuse the computation,
you should, when that makes sense, do so as part of a smaller specification
that you then compose as a target.
Still, the above is a heuristic. “When that makes sense.” But what makes sense when?

@subsubsection{Beyond Heuristics: Understanding}

It is always preferable to base decisions
on an actual understanding of the domain being modeled,
rather than let a heuristic substitute for lack of understanding.
That is where it really helps to think of “inheritance” vs “composition”
in terms of these more directly usable names: “is-a” vs “has-a” relationships@xnote["."]{
  I’m not sure who first introduced is-a and has-a in the vocabulary.
  “is-a” goes back to early semantic networks in the 1960s to the point
  that over a decade later, @citet{Brachman1982} criticizes abuse of the term.
  @citet{Smith1977} compare and contrast close cousins of is-a and has-a together
  as “generalization” and “aggregation” respectively,
  though he does not use the hyphenated short-hand names,
  and studies these variants in the context of data modeling for databases
  where they mean something subtly different (@secref{OiaMotW}).
  @citet{Wegner1987} uses is-a but not has-a,
  and @citet{Blake1987} use is-a and has-a-part together
  (the latter a reverse of is-a-part-of).
  @citet{Meyers1992} has is-a (that he spells “isa”) and has-a firmly established.
}
The simple informal question “is an X a Y, or does an X have a Y?”
then gives the answer as to whether to use inheritance (if the answer was the former),
or composition (if the answer was the latter) in defining X from Y.

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

Crucially, the design decision between inheritance and composition depends not just
on the current structure of the program, but also on expectations for its future evolution,
within a static or dynamically evolving schema of data structures and algorithms.
If the schema is small, static, well-understood and won’t need to evolve
it doesn’t really matter much which technique is used to model it.
But as it grows, evolves and boggles the mind,
a more modular and extensible approach is more likely to enable adapting the software
to changing situations, at which point thoughtful uses of inheritance can help a lot.

@subsubsection{The Real Opposition between OO and FP}

In the end, @principle{OO and FP are complementary, not opposite}.
If there is a real opposition, it is not between two perfectly compatible techniques,
but between two mindsets, between two tribes of programmers each locked
into their narrow paradigm@~cite{Gabriel2012 Petricek2017 Petricek2025} and
unable to comprehend what the other is saying.

In particular, there is an industrial bias towards OO vs an academic bias towards FP,
that matches the respective concerns of those milieus.
The entire point of OO is internal modular extensibility,
i.e. system support for programming in minimal increments
that require minimal understanding of the rest of the program.
OO is the exact tool that enables limited humans to collaborate
on projects wider than what any of them could deal with at once, covering more ground,
which is what industrial scaling demands.
Meanwhile, the entire point of FP is its deep connection to formal logic,
i.e. calculus support for precisely understanding the exact semantics of entire programs.
FP is the exact tool that enables limited humans to individually
solve problems as tall as can entirely fit in their brains, neatly decomposing them
into layers wherein each is reduced to the one below,
which is what academic publishing rewards.

A lot of opposition between OO and FP is thus struggle between various tribes of programmers,
as much as clashes between people with different temperaments,
in which each kind of programmer proves unable to fathom much less appreciate
the kind of problems that the other kind faces and solves.
And people capable of mastering and appreciating both OO and FP, like me, and like you, gentle reader,
will tend to be outcasts, who dare venture beyond what their tribes consider normal and good.

Funnily, there are always more advanced experts in OO who will use modular extensibility
to build large fleets of distributed computers each having its complete software installation
specified and configured—way beyond what ordinary OO Programmers can fathom.
And there will be Category Theorists, who invoke deep correspondences between logical and mathematical
structures to generate software solutions that will amaze
all but the most advanced Functional Programmer.
I shudder at what scale AIs will take software when they confidently surpass us in this regard,
both in width and in depth.
Happily for me and for you, the basic understanding of OO that I’m trying to communicate in this book
only requires a basic understanding of FP, so I can write it, and you can read it.

@subsection[#:tag "OOinMP"]{OO isn’t Message Passing}
@epigraph{Name the greatest of all inventors. Accident. @|#:- "Mark Twain"|
}
Alan Kay, who coined the term “Object-Oriented Programming” circa 1967,
and subsequently invented Smalltalk in the early 1970s,
before the modern concept of OO was fully formed (in 1976—see @secref{OOnaming}),
notably explained@~cite{Kay2020} that he originally meant
a metaphor of computation through independent (concurrent, isolated) processes
communicating by passing asynchronous messages.
This metaphor also guided the modifications originally
brought to Algol by Simula@~cite{Dahl1966}.
It is also present in notable early object systems such as
Director @~cite{Kahn1976 Kahn1979Ani Kahn1979Director} and
ThingLab @~cite{Borning1977 Borning1979 Borning1981}.

However, neither Simula nor Smalltalk nor any popular OO language
actually fits that metaphor:
Simula virtual procedure calls, Smalltalk message sends, and
other forms of method invocation in nearly all OO languages are synchronous.
Actor languages actually do support asynchronous message passing @~cite{Hewitt1979},
but though somewhat influential on paper, they never got popular
and always remained somewhat marginal in the tradition;
and they only acquired OO a decade after Actors were invented.
@; TODO cite Yonezawa ?
Instead, the only widely-used language to truly embody this metaphor
is Erlang@~cite{Johnson2010};
yet Erlang is not part of the OO tradition,
and its authors have instead described its paradigm as “Concurrency-Oriented Programming”.
Meanwhile the theory of computation through message-passing processes
was studied through various “process calculi”,
@; TODO cite pi calculus, join calculus, rho calculus, CHAM, etc. --- or retrospective on such?
that are also foreign to the OO tradition,
and largely unacknowledged by the OO community.
Indeed Erlang crucially lacks inheritance, or support for the “extreme late binding of all things”
that Alan Kay also identified as essential for OO@xnote["."]{
  In Erlang, each process is a dynamic pure applicative functional language
  enriched with the ability to exchange messages with other processes.
  Now, as we’ll see, you need fixpoints to express the semantics of OO;
  but in a pure applicative context, you cannot directly express sharing the results of a computation,
  so the pure fixpoint combinators lead to exponential recomputations
  as deeper self-references are involved (@secref{UPSLC}).
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
  See also the discussion in @secref{UPSLC}.
}
Most OO languages have no support whatsoever for concurrency,
or only support it as an afterthought added years or decades
after the language was originally designed,
and not integrated in any meaningful way with OO message dispatch.

Moreover, many OO languages generalize and extend their method dispatch mechanism
from “single dispatch” to “multiple dispatch”@~cite{
  Bobrow1986 Bobrow1988 Chambers1992 Allen2011}.
Their “multimethods” are attached to tuples of prototypes or classes,
and there is no single prototype, class, or single independent entity of any kind
capable of either “receiving” or “sending” a message.
Instead, they are attached to a “generic function”
that handles the dispatch based on the types of its arguments@xnote["."]{
  The “generic function” functionality from the Common Lisp Object System (CLOS)
  @~cite{Bobrow1988} (@secref{GF})
  can be viewed as isomorphic to the “protocols” functionality of Clojure;
  and Common Lispers also use the word “protocol” informally to designate a set of generic functions.
  They would in turn be isomorphic to the “typeclasses” of Haskell
  or the “traits” of Rust...
  if only these latter two supported inheritance, which they don’t (@secref{CSvTS}).
  These idioms all denote a set of related function names and type signatures,
  that are implemented differently for different configurations,
  where each configuration is associated to @emph{one or multiple} types of arguments
  (and, in Haskell, also different types of expected results).
  Another crucial property of these idioms: these traits, typeclasses or protocols
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
@;{ TODO cite stroustrup2007multimethods https://en.wikipedia.org/wiki/Multiple_dispatch }
The “message passing” paradigm, having no place for multimethods,
thus falls short compared to other explanations of OO that accommodate them@xnote["."]{
  Now, the message passing paradigm @; TODO cite PLANNER, Actors
  can be extended with a notion of “group messaging”
  where one object sends a “message” to a “group” of objects as a collective entity
  (rather than each member of the target group)
  @; TODO cite ABCL group messaging ?
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
@subsubsection{OO is No Theory of Everything}
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

@subsubsection[#:tag "OOinDM"]{OO is not Data Modeling}
Consider methodologies such as UML,
and various schools or gurus that claim to teach OO “Design”, “Analysis”, “Modeling”, etc.,
drawing diagrams of relations between classes that ostensibly include inheritance.
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
When trying to reason about recursion, UML falls apart,
by failing to distinguish between subclassing and subtyping,
between self-reference and reference to a constant (@secref{TfOO}).

Interestingly, Amílcar Sernadas’s or Bart Jacobs’s categorical theories
of “objects” and “inheritance”
@~cite{Sernadas1994 Jacobs1995 Jacobs1996InheritanceAC}
actually model UML, and also Goguen’s refinement (@secref{Goguen}),
but not at all actual objects and inheritance as used in Programming Languages:
it’s a hijacking of the same words for completely different meanings,
with the only similarity being that both sets of meanings
involve arrows between specifications.
At least Jacobs early on explicitly embraces the limitation whereby
self-reference or recursion is prohibited from field definitions (@secref{LotN}).
Just like UML, his co-algebra utterly fails to model OO;
but at least his theory is internally consistent if not externally.

@principle{UML, co-algebras and other similar methodologies
are actually relational data modeling disguised as OO}. @; TODO cite
As we’ll see later, their “classes” are extensible indeed,
but in a trivial way that fails to support modularity@xnote["."]{
  Note that there is nothing wrong at all with relational data modeling as such:
  it is a fine technique for many purposes,
  despite being deliberately limited in abstraction, and, therefore, in modularity—and
  sometimes @emph{thanks to this limitation}.
  Restrictions to expressiveness can be very useful,
  in the necessarily restricted or imprecise cases that they apply to.
  Indeed, in some cases, relational data modeling, not OO,
  is what you need to organize your data and your code.
  Moreover, Category Theory is a great way to improve on previous approaches
  to relational data, as witnessed by the field of Categorical Databases, @; TODO cite
  which, unlike the works above, makes genuine and non-trivial use of Category Theory
  to automate computations.
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
Yet specifying code is exactly where the conceptual difficulties and gains of OO
are both to be found with respect to software construction.
In fact, these handwaving methodologies@Note{
  Not all approaches involving “object-orientation” and “category theory” are vacuous.
  Indeed, see the section about Goguen below,
  or simply libraries that combine the object and functional aspects of Scala or TypeScript
  to import concepts from Category Theory to automate the handling of data structures
  or side-effects. @; TODO cite?
}
serve to lull those incapable or unwilling to wrestle with computation
into believing that they understand all there is to know about software modeling.
Yet the nature and correctness of software lies precisely
in this gap they are unable or unwilling to explore.

To see what these methodologies lack, consider
what actually modeling a computation with types requires.
An actual theory of types for computations must confront not just products of elementary data types,
but sum types, function types, subtyping, constrained type parameters,
existential and universal types, and more—including, especially, fixpoints (recursion):
in the example above, they should be able to express that human offspring are human.
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
And if you picked an OO-capable language like C++, Java, C#, Scala or TypeScript,
(or, with manually enforced dynamic types, Lisp, Ruby, Python or JavaScript),
you can actually use OO as you do it.
The ability to actually apply to OO programs is the standard by which
any methodology that claims to be “OO” should be judged.

@subsubsection[#:tag "Goguen"]{OO is not what Goguen did}

Goguen spent decades developing what he eventually called “Hidden Algebra”,
a rightfully influential formalism based on @emph{term rewriting},
that involves interactions between “objects” with private “hidden” state,
and can precisely describe the operational semantics of all kinds of computation systems.

In doing so, Goguen reuses a lot of vocabulary from OO;
but most of the time, what he describes with those words is not at all
what actual OO practitioners mean when they speak of “object”, “class”, “inheritance”, etc.
He uses the word “inheritance” to mean @emph{refinement}, a very different kind of relationship
between computations, wherein the refined computation
reproduces every detail of the original computation,
and possibly adds further details of its own.

By contrast, Goguen also reuses a lot of vocabulary from Category Theory,
and this time in a competent way, that matches the usage made by other Category Theorists.
Hidden Algebra does not advance Category Theory,
but makes genuine (though somewhat shallow) use of it
to specify observational semantics of computations in a “final, coalgebraic” style.
Unlike the handwaving categorical imitators discussed above, who cite him,
the framework Goguen develops enables precise formal specification of code,
and refinement of such specifications, down to actual implementation of executable code.

In Hidden Algebra (HA) as in Category Theory (CT) and in Object-Orientation (OO),
you get diagrams with arrows between nodes, but they mean different things.
In CT, the nodes are called “objects” (adding to the potential for confusion),
and the arrows are called “homomorphisms” or just “morphisms” with uniform rules for composition;
though when the nodes themselves correspond to structures of objects and morphisms,
they are called “categories” and the arrows are called “functors”, with a few additional rules.
In HA, the nodes are “algebras” over “signatures”
that represent the syntax of some data and operations,
and the arrows are refinement relations between those algebras,
that indeed follow the rules of CT.
In OO, the nodes are “classes” and the arrows are “inheritance”, and
they mean something completely different.

To add to the confusion, Goguen himself repeatedly invokes the analogy of his algebras
to classes, of the states of computations within his algebras to objects,
and of his refinements to inheritance.
His use of CT is beyond reproach, but his analogy to OO does not survive examination.
Goguen develops a remarkable and very valuable paradigm for specifying computations,
that leads to working tools like OBJ, OBJ3, CafeOBJ, Maude,
that can actually interpret or compile code, or generate formal systems to reason about the code.
His students and followers themselves have used his tools and concepts
to further advance technology.
But Goguen’s use of the trappings of OO is a case study in stealing the vocabulary from one
domain to misapply it to another, which may have started as confused belief
that one approach (CT) directly applied to modeling another domain (OO),
but ended up generating lots of confusing prose that fooled many superficial readers
into believing his was indeed a faithful model of OO.
To be fair, Goguen never explicitly made such a claim, yet he never explicitly disclaimed it,
and constantly used it implicitly to market his work to OO conferences at a time when OO was hip.

In conclusion, there have been many groups of people claiming to do “modeling” with or for OO,
some informally, some formally. But the best that can be said about them is that they were
right about the wrong thing (as far as actual OO is concerned),
and whatever their intent, ended up diluting the meaning of a good word.

@section{OO vs Objects}
@epigraph{
  Computer Science is no more about computers than astronomy is about telescopes.
  @|#:-"E. W. Dijkstra"|
}
Remarkably, counter-intuitively, and despite the name,
I found that Object-Orientation is not about objects,
that you can have OO without objects and objects without OO.

@subsection[#:tag "AAW"]{An Ambiguous Word}
First, notice that the word “object” does not even actually have
a single precise meaning within OO.

In Prototype OO, a prototype,
conflation of a specification and its target (@secref{PaC}),
is usually called an “object” or at times an “instance”, especially if the target is a record.
Note that some form of laziness is essential in computing the target record or its attributes,
since most specifications, being partial, do not specify
a complete computation that terminates in finite time without error;
yet this expected non-termination should not prevent the use
of the conflated entity to extract and extend its specification
(@secref{RPOO}).

In Class OO, a prototype, conflation of a specification and its target,
is instead called a “class”,
and the target is specifically a type descriptor
rather than an arbitrary record, or than a non-record value.
In Class OO, what is called an “object” or an “instance” is
an @emph{element} of some target type as described.
A class being a prototype, its regular prototype fields and methods
are called “class fields” or “class methods”
(or “static” fields and methods, after the keyword used in C++ and Java)—but
be mindful that they only involve the target type, not the specification.
“Object methods” are semantically regular methods that take one implicit argument in front,
the object (i.e. element of the target type).
“Object fields” are regular fields of the object as a record
(@secref{RCOO}).

Finally, many languages, systems, databases, articles or books call “object” some or all
of the regular runtime values they manipulate@xnote[":"]{
  Alan Kay, in his Turing Award lecture, remarks:
  “By the way, I should mention that, you know, the name,
  the term object predates object-oriented programming.
  Object, in the early 60s, was a general term that was used to describe
  compound data structures, especially if they had pointers in them.”
}
these “objects” may or may not be records, and are in no way part of
an actual OO system extensible with inheritance.
The authors will not usually claim that these objects are part
of an OO framework actual or imagined, but then again sometimes they may@xnote["."]{
  This situation can be muddled by layers of language:
  Consider a language without OO itself implemented in an OO language.
  The word “object” might then validly denote OO
  from the point of view of the implementer using the OO meta-language,
  yet not from the point of view of the user using the non-OO language.
  Conversely, when an OO language is implemented using a non-OO language,
  calling some values “objects” may validly denote OO for the user
  yet not for the implementer.
}
For instance, Yale T Scheme has a class-less object system @~cite{Rees1982 Adams1988};
but the authors call “object” any language value,
and specifically use “instance” to denote the prototypes in their object system.

@subsection[#:tag "OOwoO"]{OO without Objects}

After identifying the foundations of OO (@secref{MFtPaC}),
I found that not only the word “object”, but also the very concept of object,
is actually unnecessary to OO:
the characteristic patterns of OO can exist and be usefully leveraged in a language
that lacks any notion of object, merely with the notions of specification and target,
as I will show in @secref{MOO}.

Therefore, @principle{when discussing OO in general, the word “object” is worse than useless}:
It is actively misleading.
It should never be used without a qualifier or outside the context of a specific
document, program, system, language, ecosystem or at least variant of OO,
that narrows down the many ambiguities
around its many possible mutually incompatible meanings@xnote["."]{
  It’s a bit as if you had to discuss Linear Algebra without being able to talk about lines,
  or had to discuss Imperative Programming without being able to talk about the Emperor.
  Ridiculous.
  Or perhaps just an artifact of etymology.
}
Meanwhile, the word “class” is also practically useless
in most discussions of the foundations of OO,
since it denotes a rather uninteresting special case of a prototype.

Even the word “prototype”, while meaningful, is uncommon to use when discussing OO in general.
If discussing inheritance, I will only speak of “specifications”.
And if discussing instantiation, I will speak of “specification” and “target”.
Prototypes only arise when specifically discussing conflation.
To avoid confusion, I will be careful in this book to only speak of
“specification”, “target”, “prototype”, “type descriptor”, and (target type) “element”
and to avoid the words “object” or “class” unless necessary, and then
only in narrowly defined contexts@xnote["."]{
  I am, however, under no illusion that my chosen words would remain unambiguous very long
  if my works were to find any success. They would soon be rallying targets
  not just for honest people to use, but also for ignoramuses, spammers, cranks, and frauds
  to subvert—and hopefully for pioneers to creatively misuse
  as they make some unforeseen discovery.
}

This is all particularly ironic when the field I am studying is called “Object Orientation”,
in which the most popular variant involves classes.
But fields of knowledge are usually named as soon as the need is felt
to distinguish them from other fields,
often based on their more popular or salient features,
long before they are well-understood, and thus based on misunderstandings.
This misnomer is thus par for the course@xnote["."]{
  The wider field of study is similarly misnamed.
  Edsger W. Dijkstra famously said that Computer Science is not about computers.
  Hal Abelson completed that it is not a science, either.
}

On the other hand, this book is rare in trying to study OO in its most general form.
Most people instead try to @emph{use} OO,
at which point they soon enough must go from the general to the particular:
before a programmer may even write any OO code, they have to pick
a specific OO language or system in which to write their software.
At that point, the context of the language and its ecosystem
as wide as it may be, is still narrow enough to disambiguate the meanings of all those words:
Likely, “object”, and either or both of “prototype” or “class”
will both be well-defined and very relevant within that context.
Suddenly, the programmer becomes able to utter their thought and communicate
with everyone else within that same ecosystem...
and at the same time becomes more likely to misunderstand programmers from other ecosystems,
who use the same words with different meanings.
Hence the tribal turn of many online “debates”.

@subsection[#:tag "OwoOO"]{Objects without OO}

Conversely, the word “object” has many valid uses outside of OO,
to denote embodiments of first-class modularity even in the absence of inheritance.
I concur with @citet{Wegner1987} in calling “object-based”
those languages and systems that provide internal modularity (first- or second- class)
without internal modular extensibility,
while reserving “object-oriented” for languages that do also offer inheritance.

Furthermore, and as discussed above (@secref{AAW}), there is an even earlier,
less interesting meaning of “object” in the narrow context of some programming languages
to mean “any record” or even “any value” manipulated by the language.
@; Alan Kay mentions: «the term “object” was used as early as the 50s: to mean a block of storage with multiple fields» -- see also HOPL-II
@; https://www.quora.com/How-did-Alan-Kays-vision-of-object-oriented-circulate-before-Smalltalk-76-I-saw-papers-by-Bobrow-1971-Robert-the-younger-brother-Goodenough-1975-Ross-1976-Jones-Liskov-1976-ab-using-the-term-I-guess-inspired-by-Kay/answer/Alan-Kay-11

These are two kinds of situations where there are meaningfully (more so in the first case)
“objects” without OO.

@subsection[#:tag "OOnaming"]{The Naming of OO}

A discussion of when objects are or aren’t OO often devolves into
an argument about what is meant by OO to begin with.

I have made my definition clear enough in the previous chapter, and
will further elaborate on it in the following chapters.
From my exposition you will conclude my definition is correct
(as for what correct means, see below @secref{Epistemology}).
But while most people today will correctly identify the concept of OO with the word OO,
this was not always the case, and many still struggle with it.

Alan Kay made up the term “object-oriented” around 1967 @~cite{Kay2003}.
I found no one contesting that. But Kay provided no definition.
The meaning of the term was to be inferred from Kay’s vision.
Even Kay’s Smalltalk didn’t fully embody OO until Smalltalk-76,
and his team did not publish any text with the word “object-oriented” until @citet{Ingalls1978}.
The first published account of OO was not even from Kay’s team:
it was @citet{Bobrow1976} that introduced the expression “object-oriented programming”
to print with a modern definition, as well as the words “inheritance” and “prototype”,
while explaining how Simula-style “classes” were a special case of prototypes.
Daniel Bobrow was Kay’s friend and same-corridor colleague.
After 1976 was the explosion of OO (@secref{OOitL}).

Before then, OO was all word of mouth, in the small world of
(mostly US) programming language researchers.
Unsurprisingly, early appearances of the word in print didn’t use the latter definition,
but were used to denote something closer to what I after Wegner call “object-based”
@~cite{Bobrow1972 Goodenough1975 Ross1976 Jones1976}.
Possibly, some or all may have been independent reinventions,
since the word “object” had previous common meaning (as mentioned above),
and the suffix “oriented” was common in those days @~cite{Bobrow1972}@xnote["."]{
  The word “object-oriented” in those times appears in print
  in works about psychology and sociology, obviously with unrelated meanings.
}
The historical record doesn’t tell for sure, and I haven’t been able
to reach the few remaining witnesses for comments.
Still, the later mentions are much more likely to have been influenced by Kay
than the earlier one @~cite{Bobrow1972}.
@; TODO cite https://www.quora.com/How-did-Alan-Kays-vision-of-object-oriented-circulate-before-Smalltalk-76-I-saw-papers-by-Bobrow-1971-Robert-the-younger-brother-Goodenough-1975-Ross-1976-Jones-Liskov-1976-ab-using-the-term-I-guess-inspired-by-Kay/answer/Alan-Kay-11

Note that there are also dissidents who proudly argue that “object-oriented”
does or should mean what I call “object-based”.
The most prominent one being Cook (@secref{Cook}),
but, after him and still alive today, there is for instance Aldrich @~cite{Aldrich2013}@xnote["."]{
  Interestingly, Aldrich speaks of “extensibility” to mean replacing a module by a different one
  in a modular design, without the extension itself being modular.
  Remarkably, a system that supports modularity without modular extensibility
  therefore also supports extensibility without modular extensibility.
  The two can coexist and complement each other; but it is only when they are combined
  that they become a much more powerful paradigm—see @secref{ME4}.
}
See below why and how they are wrong (@secref{Epistemology}), in addition to being the minority:
@principle{A definition is not correct because an authority decrees it,
nor because a majority repeats it,
nor because a pioneer once used a word in a particular way, but
because it identifies a phenomenon people actually need to distinguish,
explains the cases they care about, and helps them make better decisions.}
That is the standard by which I ask my definition of OO to be judged.

I also discussed the case of Kay in @secref{OOinMP} and @secref{Kay}.
As for Liskov, she is always careful to distinguish
the meaning in which CLU was object-oriented
(Wegner would say “object-based”; in my terms, it more precisely has @emph{second-class} modularity)
from “what are commonly called object-oriented languages” @~cite{Liskov1993}
that she reckons is more common and includes inheritance,
recognizing the significant difference between the two meanings
without relinquishing her early claim to the word.

Now, just because the word “object-oriented” was only defined in 1976
doesn’t mean OO wasn’t discovered earlier.
Simula 67 from Denmark @~cite{Dahl1967} was the first language to implement OO:
inspired by the idea of class from @citet{Hoare1965},
they implemented sub-classes for which they invented “concatenation semantics”,
an early low-level variant of what would later be called “inheritance” (@secref{Inner}).
But Dahl and Nygaard didn’t conceptualize OO as a general mechanism at the time
(and certainly didn’t use the word “object-oriented” that Kay had barely started circulating
in the US at about the same time).
Dahl and Nygaard realized they had found something great, but that unique thing at the time
to them was just the first implementation of Hoare’s classes,
in their new version of Simula@xnote["."]{
  It didn’t help that after Simula 1967, Nygaard, the team leader, took
  a break from computer science research for a few years to focus on trade union work,
  only to return in 1976 @~cite{Dahl2001}.
  Who knows what name we’d use instead of OO, and how the field would have been changed,
  if the Simula team hadn’t squandered its advance.
}

Dahl and Nygaard made the crucial technical discovery
of a domain they did not name, did not identify—and arguably did not explore:
their research always remained disconnected from the mainstream of OO.
Rather OO as such was identified 9 years later by Bobrow and Winograd.
Just like Columbus didn’t recognize the continent of America as such,
which was rather discovered (arguably) and identified (surely) years later by Amerigo Vespucci.
And the continent is named after the latter, even though Columbus is respected
as the man who dared take the harder leap of faith,
cross an uncharted ocean the vast extent of which no one suspected,
beyond horizons past which no one dared venture,
to find a domain no one else dreamed existed—and change the world.

Despite all the greatness of the very first pioneers,
it is appropriate that a concept be named by or after those who identified it,
and that a name shall refer principally to the concept that matters most
among all those it is used or abused for.

@section{Misunderstandings about Inheritance}

The word “object” is not the only one that is contentious in the study of OO.
The word “inheritance” has its own controversies, though smaller in scale.

@subsection{Absurd Rejection of “Inheritance of Implementation”}
@epigraph{
  It is no crime to be ignorant of economics, which is, after all,
  a specialized discipline and one that most people consider to be a “dismal science”.
  But it @emph{is} totally irresponsible to have a loud and vociferous opinion on economic subjects
  while remaining in this state of ignorance.
  @|#:-"Murray Rothbard"|
}
Some pundits praise “inheritance of interface”, “subtyping”, and/or “polymorphism”,
while disparaging “inheritance of implementation”, “subclassing”, and/or “code reuse”.
There are many variants of this trope, but it is once again a case of accepting modularity
yet rejecting modular extensibility, usually due to a misunderstanding of both—or else
they would be properly named and conceptualized.

Inheritance @emph{is} inheritance of implementation, subclassing and code reuse.
In some very narrow cases, it may be made to
match interfaces, subtyping or some form of polymorphism;
and far too many people desperately try to use it as if the two were the same,
which I describe in @secref{NNOOTT}.
But that is actually deeply wrong, because inheritance crucially depends
on computing a fixpoint after you compose extensions to an initial generator,
and involves those nice things (interfaces, subtyping, polymorphism)
@emph{before} the fixpoint, which does not translate to those same things @emph{after} the fixpoint;
yet the above pundits fail to understand the difference (@secref{BtN}).

Interfaces, subtyping and polymorphism are important concepts that absolutely matter a lot.
But trying to fit inheritance into these concepts, then
rejecting inheritance because it fails to fit into them, is absurd,
akin to babies crying when they can’t fit a square peg in a round hole,
and celebrating when they can.
It demonstrates a deep misunderstanding of both the nature and purpose
of both subtyping and inheritance.

@subsection{False dichotomy between Inheritance and Delegation}
@epigraph{
  A rose by any other name would smell as sweet.
  @|#:- "William Shakespeare"|
}
Many authors have called “delegation” the mechanism used by Prototype OO
@; TODO CITE Self, Castagna Cardelli 1996, …
as distinct from the “inheritance” mechanism of Class OO.
This wrongheaded distinction started with @citet{Hewitt1979},
in whose ACT1 language the two concepts had both been implemented,
but through separate implementation paths.
The distinction was further popularized by @citet{Lieberman1986},
who contrasts the two joined concepts of prototype-delegation vs class-inheritance.

Yet, identifying inheritance with classes to the exclusion of prototypes
is historically counterfactual:
the words “inheritance” and “prototype” were both simultaneously introduced
by KRL @~cite{Winograd1975 Bobrow1976},
a system with (multiple) inheritance and prototype OO—from before
the word Object-Oriented was popular
(indeed they introduced the word to print in its modern definition).
Indeed, KRL was instrumental as an inspiration to Smalltalk-76,
the system that made OO popular.

Opposing inheritance and delegation is also logically counterfactual:
@citet{Lieberman1986} itself explains how “inheritance” (i.e. classes)
can be expressed as a special use of “delegation” (i.e. prototypes).
On the other hand, the paper also explains you cannot go the other way around
and express prototypes in terms of classes:
prototypes enable dynamic extension of individual “objects” (prototypes) at runtime,
while classes only allow extension at compile-time, and only
for an entire type (“class”) of “objects” (elements of the type).
@citet{Wegner1987} after @citet{Cook1987} also recognizes that “inheritance” of classes
can be expressed in terms of a more general mechanism, that he dubs “delegation”.

In the end, the inheritance mechanism is indeed the same, and it is very wrong to
give it two different names depending on whether it is used for prototypes or for classes.
Even Self, that became the most popular language with “delegation” in academia,
uses the word “inheritance” in its papers @~cite{Ungar1987 Chambers1989 Chambers1991}.
And @citet{Stein1987} argues that delegation and inheritance are the same concept,
and notes that prototypes map to classes, not class instances
(though strictly speaking she gets the mathematical direction of the map wrong).
The real distinction and comparison that should have been made was between the relative
expressiveness of prototypes and classes, especially if considered as second-class entities
and in absence of reflection (or refraining from using it).
But that is the conclusion that none of the authors who wrote on the topic made explicit,
even though it is implicit in both.
And so the authors focus on arguing about different ways to name the same concept in two contexts;
meanwhile they fail to argue on the different contextual concepts that do matter@xnote["."]{
  If irrelevant changes in the context are a valid excuse to give an existing concept a new name
  and get a publication with hundreds of citations
  based on such a great original discovery@~cite{Ringard1990},
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
  for all these never-discussed-before original inventions related to OO.

  The Lieberman paper deserves its thousands of citations because it is a great paper.
  However, a lot of citers seem to fixate only on the unfortunate choice
  of concept delineation and naming by Lieberman,
  who probably did not anticipate that he would set a bad trend with it.
  The delineation made sense in the historical context of the Actor team
  separately implementing prototypes and classes with related yet distinct mechanisms
  in their ACT1 language @~cite{Hewitt1979}, way before they or anyone (except maybe Winograd)
  understood how exactly classes were a special case of prototypes.
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

Yet, recent pure functional Prototype OO systems @~cite{Cunningham2014 Simons2015 Rideau2021}
prove constructively that prototypes can be pure, and that they use
the very same inheritance mechanisms as classes,
indeed with classes as a particular case of prototypes with the usual construction.
Meanwhile, old reflective Class OO systems like Lisp and Smalltalk
@~cite{Kahn1976 Kay1993 Gabriel1991 Kiczales1991}
also support mutable state to modify the inheritance structure at runtime,
for the sake of dynamic redefinition of classes at runtime,
in what remains semantically a pure functional model once when the structure is set.
See how in CLOS you can define methods on generic function
@c{update-instance-for-redefined-class} to control how data is preserved, dropped or transformed
when a class is redefined (@secref{MoIaCU}).
Mutable state and mutable inheritance structure in particular are therefore
clearly an independent issue from prototypes vs classes,
though it might not have been obvious at the time.
As I introduce formal models of OO,
I will start with pure functional models (@secref{MOO}), and
will only discuss the confounding matter of side-effects much later
(@secref{SOO})@xnote["."]{
  It might be interesting to explain @emph{why} many authors failed so systematically to
  identify delegation and inheritance, when the similarities are frankly obvious,
  and the relationship between classes and prototypes is well-known
  to anyone who has implemented classes atop prototypes.
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

As for which words to keep for the more general concept,
the word “inheritance” was used first for the general concept,
in a language with “prototypes”.
The word “delegation” stems from the Actor message-passing model,
and is both later and less general,
from after the words “inheritance” and “prototypes” were better established,
and is strongly connoted to specific implementations using the message-passing paradigm.
It also fell out of fashion some time in the 1990s,
after JavaScript became a worldwide phenomenon, and (correctly) used the term “inheritance”
rather than delegation (as it isn’t particularly “message passing”, just calling functions).
@~cite{ECMA1997}

@section[#:tag "Epistemology"]{Epistemological Digression}
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
lest you find yourself alienated from much of human knowledge.

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

@subsection{Does it even mean anything for a definition to be correct?}
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
then people will keep caring about what they currently call OO under a different name,
rather than care about whatever those who corrupt the name may want them to.

@subsection[#:tag "Kay"]{Shouldn’t I just use the same definition as Alan Kay?}
@epigraph{OOP to me means only messaging,
local retention and protection and hiding of state-process,
and extreme late-binding of all things. @|#:- "Alan Kay"|
}
No, that isn’t possible, nor would it be appropriate if it were.
Alan Kay coined the expression “Object Oriented Programming” circa 1967.
Originalists might say everyone must take it to mean whatever He defined It to mean,
and sometimes cite him as in the epigraph above.

But neither the above @~cite{Kay2003} nor any of Kay’s pronouncement on OO constitutes
a precise definition with objective criteria,
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
  (it wasn’t called that yet in Simula, either).
  But he did adopt it eventually in Smalltalk-76,
  notably under the push of Larry Tesler
  (who previously used “slot inheritance” on early desktop publishing applications),
  and this adoption is what launched OO as a phenomenon.
  Kay stated adopting single inheritance over multiple inheritance
  was a compromise @~cite{Kay1993};
  his team later added multiple inheritance to Smalltalk @~cite{Goldstein1980}, but
  it is unclear that Kay had much to do with that addition, that never became standard.
  More broadly, Kay didn’t endorse any specific inheritance mechanism,
  never focused on that part of the design, and explicitly discounted inheritance
  as a primary constraint that defines his concept @~cite{Kay2003 Kay2020}.
  To Kay it was only a means to an end,
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
if only to recursively apply the same semantic scrutiny
to the words they used in their own definitions.
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
when they define and redefine “OO” or any word
to have whatever precise or murky meaning they loudly assert.
Instead, some cowards propose that I should stick to “inheritance”
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
Besides, if you retreat to “inheritance” in the hope that at least for that term
you can get people to agree on a clear unambiguous meaning@xnote[","]{
  The term “inheritance” is already corrupted,
  since Goguen uses it at times to mean refinement @~cite{Goguen1992}
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

@subsection[#:tag "Experts"]{Can’t we at least let experts determine what concepts to name?}
@epigraph{
  There could hardly be a more unbearable — and more irrational — world than
  one in which the most eminent specialists in each field were allowed to proceed unchecked
  with the realization of their ideals.
  @|#:- "Friedrich A. Hayek"|
}
No, experts, even after excluding the all too common outright fraudulent ones,
remain a terrible authority on what precisely the right concepts to care about are:
Some, more interested in the methods they master than in the problems people have,
will happily define down a topic to whatever their limited methods can address.
Others, interested in their impact upon people, will offer convincing sounding explanations
to manipulate people and try to sway them from what they actually care about
to what the expert would prefer for them to care about instead.
The more ideologically motivated will happily lie, to the point of changing
the official definitions of words to equivocate between the common meaning that people use
and the made up meaning that makes their lies sound true.

You might hope that at least on topics with little economic or political impact,
there would be fewer incentives for bias. Unhappily, as per Sayre’s Law:
@principle{Academic politics is the most vicious and bitter form of politics,
because the stakes are so low.}
The failure modes are many, and those experts most autistically interested in the truth
are likely to be those whose findings are least promoted (by themselves or others),
at least in terms intelligible by the public at large.
You sadly cannot just trust “the experts”, and especially not so
with respect to the precise delimitation of concepts.

A rough public consensus about what a concept does and does not cover,
while not precise around the edges, is actually much more reliable
than any expert opinion regarding what people actually care about—because it matters to them.
On the other hand, a rough public consensus is a feeling, incapable of either precision or logic.
It can measure what concept people actually care about,
but provides no consistent explanation for them.
For that you will have to consider what experts say,
who alone can get it right (though they more often than not get it terribly wrong).
Even there you still cannot trust the experts,
but must consider their utterances critically, with a big grain of salt.

@subsection{So what phenomena count as OO?}
@epigraph{The medium is the message.
  @|#:- "Marshall McLuhan"|
}
What defines OO is not the metaphors of those who invent, implement, or comment about it
but the design patterns programmers apply when they write code in an OO language;
the interactions they have with computers and with each other;
the decision trees that are enabled or disabled when evolving a program into another.
These phenomena are what OO is:
what programmers do, not what programmers say.

And these phenomena are what is captured by
the internal modular extensibility as defined in the previous chapter:
@itemize[
  @item{The ability to “code against an interface” and
    pass any value of any type that satisfies the interface
    (modularity, whether following structural or nominative rules).}
  @item{The ability to extend and specialize existing code by creating a new entity
    that “inherits” the properties of existing entities and only needs to specify
    additions and overrides in their behavior rather than repeat their specifications,
    wherein each extension can modularly refer to functionality defined
    in other yet-unapplied extensions.}
  @item{The fact that these entities and the primitives to define, use and specialize them
    exist @emph{within} the programming language rather than in an external preprocessing layer.}]

I contend that the above is what is usually meant by OO,
that matches the variety of OO languages, systems and idioms,
without including systems that are decidedly not OO, like the languages
Erlang, Go, Rust, SML or UML (as of 2026 at least).
Whatever clear or murky correspondence between names and concepts others may use,
@emph{this paradigm is what matters, and is what I will call OO}—it is
what I will discuss in this book,
and will systematically reduce to elementary concepts.
I claim that the public has correctly identified a domain of worthwhile concern,
and that as an expert building on the work of previous experts,
I have identified the correct conceptual map of that domain
that I am not choosing arbitrarily but acknowledging is what programmers care about.

As to why this particular meaning of “object-oriented” should win over
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
  find OO languages or libraries that illustrate points
  in the space defined by the following axes:
  (a) having an inheritance mechanism poorer than that of C++, equivalent, or richer;
  (b) having or not having classes;
  (c) using or not using mutable state;
  (d) with or without notions of “public” or “private” methods;
  (e) with or without the ability to express functional programs;
  (f) with or without the metaphor of message passing;
  (g) with or without its authors claiming that it is OO.
  You don’t have to fill the entire grid by hand,
  though you may use AI to assist you in doing it.
  Even then, some combinations might not have any current example.
  Still, find at least one interesting answer on each side of each axis.
}

@exercise[#:difficulty "Medium"]{
  Identify an OO language that you’re familiar with (or else, one that is popular),
  and place it against each of the above axes.
  Then, for each axis, find an example of OO language on the other side of the axis;
  and find a non-OO language on this side of the axis (if possible).
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
  without incorrectly blaming “OO” for it.
}

@exercise[#:difficulty "Hard"]{
  Find some criticism (valid or invalid) that actually pertains to OO,
  rather than to something else wrongly identified with OO.
  Wrong targets for which criticism will not count as answers to this exercise
  (though they may be otherwise interesting), include:
  (1) criticism of things I already denounced as not being OO,
  (2) criticism of particular systems that happen to be written with OO,
    but for which this aspect of the system is irrelevant,
  (3) criticism of mistakes that do apply to OO,
    but actually, much more broadly, apply unchanged to software development in general.
  (This is notably harder than the previous exercise!)@Note{
    As a hint, you may consider the criticism from @citet{Graham2001noop},
    and the reply by @citet{Rees2003};
    or @citet{Armstrong2001}, @citet{Gabriel2002}, or @citet{Nierstrasz2010}.
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
  Find some other technique, field of knowledge, school of thought, ideology, etc., besides OO,
  that, having once been trendy or popular,
  was overtaken by plenty of people wrongly claiming its name,
  to advance very different sets of ideas.
  Characterize the real thing under the original name,
  and the main variants that corrupt the name
  (though they may have interesting contributions of their own besides this corruption)@xnote["."]{
If you have trouble with this question, you may consider digging on the etymology
and early history of the word “ideology” itself.
But it’s much better if you manage to find your own example of such hostile takeover of a word.
}}
