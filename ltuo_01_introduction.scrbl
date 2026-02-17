#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 1)

@title[#:tag "Intro"]{Introduction}

@section{Wherefore this Book}
@subsection{Curiosity about OO, Familiarity with FP}
@epigraph{It was probably in 1967 when someone asked me what I was doing,
 and I said: “It’s object-oriented programming”. @|#:- "Alan Kay"|
}
Object-Oriented Programming (OOP), or Object Orientation (OO),
is a paradigm for programming in terms of “objects”.

What even are objects? What aren’t? What is OO? What isn’t?
What is it for? What is a paradigm to begin with?
How do I use OO to write programs? How do I make sense of existing OO programs?
How may I best think about OO programs, to design them?
How may I best reason about them, to debug them?
What are variants of OO? How do I compare them?
How can I build OO if I don’t have it yet (or not a good variant of it)?
And why are so many people so into OO, and so many others so against it?
Which of their arguments is right or wrong when?
Am I missing something by not using OO, or by using it?

These are the kinds of questions this book will help you answer.
To get there, I will have to introduce many concepts.
Don’t worry: when others may relish in complexity, I instead aspire to simplicity.
You will learn some new words and new ideas.
But if you practice programming, and think about your practice,
then you are in my target audience;
and you will find it will be easier to program
with the right ideas than with the wrong ones.
And the Internet is certainly full of wrong and sometimes toxic ideas,
about OO as about anything.

To answer those questions,
I will assume from my readers a passing familiarity with Functional Programming (FP).
You don’t have to be an expert at FP;
you just need basic knowledge about how to read and write
“anonymous higher-order” functions
in your favorite programming language@xnote["."]{
  Anonymous functions are just functions that do not need to have a name.
  Higher-Order means that they can take other functions as arguments,
  and return functions as results,
  both old and new (by e.g. applying or composing previous functions).
  These days, in 2026, most mainstream languages have such functions,
  quite unlike 20 years ago.
  It is also possible to emulate such functions in languages that do not have them;
  that is a topic I will not cover, but I can refer you
  to classic books about programming languages that do it well
  @~cite{EOPL3 SICP2 Queinnec1996LiSP Pierce2002TAPL PLAI}.
  I do love these books, however I find their treatment of OO lacking—otherwise
  I wouldn't be writing the present book.
}

@subsection{Decades too late, but still decades ahead}
@epigraph{Each new generation born is in effect
  an invasion of civilization by little barbarians,
  who must be civilized before it is too late.
  @|#:- "Thomas Sowell"|
}
Why write a book about OO in 2026?
It is present year;
don’t people know everything they need to know by now (about OO or otherwise),
unlike the barbarians of times past?
No, people of past years were not barbarians
though they were ignorant of what we now know;
and neither are we barbarians for failing to know what our successors will.
Every mind is just too busy with knowledge from its time,
that would have been useless earlier, and will soon be useless again.

Conceived around 1967 with Dahl and Nygaard’s Simula and Alan Kay’s musings,
OO was actually born in 1976 when these two collided with each other and
with Bobrow’s Lisp AI work, resulting in Smalltalk-76 and KRL-0.
OO took off from there, at first reserved to the happy few
who could use the most high-end systems from Xerox, BBN or MIT.
OO became popular among researchers in the 1980s, and at some point was the Next Big Thing™.
In the 1990s, OO finally became available to every programmer,
accompanied by endless industry hype to promote it.
By the mid 2000s it had become the normal paradigm to program in.
Then, at some point in the mid 2010s it started to become as boring as ubiquitous.
Now in the mid 2020s it is on its way to become forgotten, at least among the Cool Kids.
Yet, one thing OO never was, was understood.
Until now.

As a USENET wiseman wrote: “Every technique is first developed, then used,
important, obsolete, normalized, and finally understood.”
This book then is here to bring the final nail on the coffin of OO: understanding it.
Too bad no one reads books anymore, except AIs.
If that book, and most importantly its understanding, had come a few decades earlier,
it could have saved a lot of people a lot of trouble.
OO sure baffled me for a long time, and many around me.
Now that I am not baffled anymore,
I can bring you all my explanations—but long after the battle.

It would be nice to hear a few of my old colleagues tell me:
“so @emph{that} is what OO was about all along!”
But I have little hope of convincing many in the old generations
of the benefits of OO done right
(yes, I am one of those Lispers bragging about how their 1988 OO system
is still decades ahead of yours).
And even if I did, they will be retiring soon.
However, a new generation of programmers is born every year,
and it is always time to inspire and educate the new generation,
that they do not fall as low as their predecessors, or lower.
And even if AIs take over programming, they too will need education.

@subsection{Towards a Rebirth of OO}
@epigraph{If you want to build a ship,
don’t drum up the men to gather wood, divide the work, and give orders.
Instead, teach them to yearn for the vast and endless sea.
@|#:- "Antoine de Saint-Exupéry, creatively misquoted."|
}
I actually think OO is a fantastic programming paradigm to build software,
one that I tremendously enjoy when I use it in Lisp, and miss when I can’t.
But what passes for OO in mainstream programming languages disappoints me.

When I mention OO becoming boring, then forgotten, or talk of nailing its coffin,
I’m not celebrating OO’s decline.
I’m lamenting that a bad version of OO became popular, then faded.
Meanwhile, there’s little interest or funding in either industry or academia
to further improvement to OO—a topic wrongly considered already understood.

These days, bright programmers gravitate toward Functional Programming (FP),
a paradigm unjustly neglected in the industry during OO’s heyday.
As distributed systems became widespread, FP proved more practical
than imperative programming for managing state and avoiding the problematic interactions
that make concurrent programs slow and buggy.
Because OO had been sold in a package deal with imperative programming,
it fell out of fashion alongside it.

Grumpy old Lispers like me yell at the clouds that
there was never an opposition between OO and FP—that
we Lispers have been enjoying both together since the 1970s, and that
OO can be so much more than you “blub” programmers can even imagine@xnote["."]{
  Blub is how Lispers disparagingly call less expressive languages, after @citet{Graham2001avg}.
  Graham notably wrote two books on Lisp, with interesting chapters on OO
  @~cite{Graham1994 Graham1995}.
  He is however rightfully skeptical of the overuse of OO in corporate software
  @~cite{Graham2001noop}.
}

My hope—my faith, even, despite available evidence—is that
a better OO can and will rise from the dead:
Simpler than what was once popular. Unbundled from imperative programming. More powerful.
With the advanced features of Lisp OO at long last adopted by the mainstream.

But I have to admit my defeat so far:
I have yet to build a system to my liking
that would attract a critical mass of adopters to become self-sustaining.
And so, like all researchers who title their papers “Towards a …”
when they fail to achieve their goals, I am switching to plan B:
trying to convince others that there’s gold in them thar hills,
so they will go dig it—because I can’t dig it all by myself.
In writing this book, I am sharing the treasure map with you.

@exercise[#:difficulty "Easy"]{
  Rate from 0-10 (a) how well you understand OO, and
  (b) how well you understand FP.
  You can redo the rating after reading this book
  (and reflect on your earlier opinions), to see if it improved.
}

@exercise[#:difficulty "Medium"]{
  In a paragraph, write what you expect from reading this book.
  You can later use this paragraph to decide which sections to skim and which to focus on.
  But also after you read the book, to decide whether you were satisfied,
  and whether you were surprised.
}

@exercise[#:difficulty "Hard"]{
  In one sentence to one page maximum, draft what you think is the essence of OO,
  as if you had to explain it to a junior programmer.
}

@exercise[#:difficulty "Research"]{
  Write your own book about OO. Show me how it’s done.
}

@section{Why this Book}
@epigraph{If there’s a book you really want to read but it hasn’t been written yet,
  then you must write it. @|#:- "Toni Morrison"|
}
This section is about me, not you. So feel free to skip to the next section.
Come back if you’re ever curious about the backstory of my theory of OO.

@subsection{Proximate Cause}
@epigraph{
  The world will never starve for want of wonders, but for want of wonder.
  @|#:- "Gilbert K. Chesterton"|
}
After I used the Prototype OO programming language Jsonnet @~cite{jsonnet} in production,
then discovered that Nix @~cite{nix2015} implemented the very same object model in two lines of code,
OO finally clicked for me.
After all those years of experiencing how great or terrible OO in various forms could be,
yet never quite being able to explain to myself or others what OO even was,
certainly not in clear and simple terms—at last, I understood.
And then I realized I might be the only one who understood OO from both
the theoretical side of programming language semantics, and
the practical side of actually building large systems—with the advanced features of untyped Lisp OO
as well as with the advanced types of feature-poorer OO systems like Scala’s.
At least the only one who cared enough to write about it.

So I tried to get the Good News out, by getting a paper published.
And I did get a paper published eventually @~cite{poof2021},
but only at the Scheme Workshop, a small venue of sympathetic Lispers,
who already understood half of it and did not need much effort to understand the rest,
but who already had plenty of good object systems to play with.
Meanwhile, my repeated attempts at publishing in more mainstream
Computer Science conferences or journals were met with incomprehension—also
with great technical feedback, of course, that helped me learn and improve a lot,
and for which I am most grateful;
but fundamentally, with a deep misunderstanding about what I was even talking about.
As @citet{Gabriel2012} would say, my reviewers were trying to evaluate my work
while making sense of it from an @emph{Incommensurable Paradigm}.

Certainly, I could try to explain myself, to translate between their language and mine;
spend time explaining the denotations and connotations of my words as I was using them,
and defusing those that may mistakenly be heard by various readers from different communities;
tell my readers to put aside the concepts they think they know,
and somehow teach them the concepts I am putting behind the words, so they understand.
But that takes a lot of time and space. For me. And for my readers.
Resources that we both lack, especially scientific publications
limited to 12-25 pages, depending on the venue@xnote["."]{
  Even a journal, with articles sometimes up to 70 pages,
  would require splitting the content of this book into four parts;
  maybe more, because each part would have to spend a good chunk of its page limit
  to summarize enough of what comes before so that the current part is understandable,
  and enough of what comes after so that it doesn’t appear pointless or petty.
  This is much more effort than just writing a book,
  an already costly and time-consuming endeavor.
  Splitting the same content over a dozen or more 25-page papers would be even more work.
  And so, as Pascal wrote about one of his letters:
  @italic{Je n’ai fait celle-ci plus longue que parce que
          je n’ai pas eu le loisir de la faire plus courte.}
  (I have made this longer than usual because I have not had time to make it shorter.)
}
That was barely enough to address actual misunderstandings experienced by previous reviewers,
and make my claims clear—see how much that takes
in @secref{WOOin} and @secref{WOOiIO} respectively—with
no space left to properly explain and substantiate those claims.
Attempts to compress that information into this kind of format
would again lead to loss of clarity, and the inevitable misunderstanding by reviewers,
and frankly, the readers they rightfully stand for.

Or, I could take the time and space to explain things right.
But then, I’d have to abandon the hope of fitting in existing venues.
I would have to write a book. This book.

@subsection{Ultimate Cause}
@epigraph{
  Some mathematicians are birds, others are frogs.
  Birds fly high in the air and survey broad vistas of mathematics out to the far horizon.
  They delight in concepts that unify our thinking and bring together
  diverse problems from different parts of the landscape.
  Frogs live in the mud below and see only the flowers that grow nearby.
  They delight in the details of particular objects, and
  they solve problems one at a time.
  Manin is a bird.
  I happen to be a frog, but I am happy to introduce this book
  which shows us his bird’s-eye view of mathematics.
  @|#:- @elem{Freeman Dyson, in his foreword to @citet{Manin2007}}|
}
I am not new to ideas that are hard to publish.
My thesis on Reconciling Reflection and Semantics
not only remains unpublished@~cite{FarePhD},
none of the many ideas within it could be published in an academic venue,
except for a very short summary in a small workshop@~cite{Rideau2018Climbing}.
One explanation is that these ideas are hard to compress to
fit within the size limits of publishable papers:
out of the four parts in my thesis,
the earlier parts can seem trivial, and mostly pointless
(except for a few useful definitions and some cool insight),
unless you understand the applications in the latter parts;
but the applications in the latter parts seem impossible or don’t even make sense
unless I introduce the concepts from the first parts.

I repeatedly develop theories too large to publish in parts
because I tend to think in terms of big pictures—or
what others call big pictures, for I am also an aphantasiac:
one with no mind’s eye except when dreaming.
A “bird”, I like to explain large-scale human behavior,
or tie together seemingly disparate phenomena,
by identifying common causal patterns that you can observe from “above”.
Ideas that are hard to communicate to “frogs”,
who don’t think at a high enough level of abstraction.
And I suspect that even other “birds” who do think at a high enough level, sometimes higher,
not being aphantasiac, are overwhelmed or distracted by their visual imagery,
and miss structural patterns I perceive non-visually.

However in the case of this book on Object Orientation,
there is the difference that I actually have
three decades of practical as well as theoretical experience with OO.
I studied the semantics of programming languages in college.
I professionally wrote or maintained OO programs
in Lisp, Python, Java, JavaScript, Jsonnet, Scala, C++.
I kept writing papers about OO while working in the industry@~cite{LIL2012 poof2021}.
And for many years, I have been implementing OO,
and maintaining two object systems for Gerbil Scheme@~cite{GerbilScheme GerbilPOO}.
OO is a topic both easier and more concrete, in general and for me in particular.
A topic in which I have direct experience as a frog,
and where I can stand as a bird on the shoulders of giants
who already solved many of the foundational problems.
On this mature topic, I am ready and capable, and can explain a complete Theory of OO
that is also fully implemented and immediately usable.

Ultimately, then, OO is the topic where my bird’s view and frog’s experience meet,
where I made worthy findings that won’t fit in a short publication,
but that I can share in the form of a book.

@exercise[#:difficulty "Easy"]{
  Laugh at me.
  I am spending so much time being serious about a topic
  that is relatively silly to anyone else.
  I feel smug about my opinions, yet I mostly can’t get my message through.
}

@exercise[#:difficulty "Medium"]{
  Laugh at the reviewers, the other readers.
  Their preconceptions, their paradigms, wouldn’t let them understand what I wrote,
  even after reading it.
}

@exercise[#:difficulty "Medium"]{
  Laugh at other authors who wrote about OO.
  They like me spent lot of time serious about the topic (or pretending to be),
  yet failed to see the simple things I will explain.
}

@exercise[#:difficulty "Hard"]{
  Laugh at yourself.
  You care enough about this topic and my opinions to be reading this book.
  Congratulations.
  Yet your own preconceptions are clouding your judgement and you still won’t get it all.
}

@exercise[#:difficulty "Hard"]{
  Love the other authors who wrote about OO, the reviewers, the other readers,
  and maybe even me—and of course yourself.
  Beyond our differences, we all share an interest in this same topic.
  But we all have a limited ability to focus, with finite time and energy,
  necessary preconceptions, to care about whatever it is we each care about.
  Most everyone means well and does their best—and even the few who don’t could use a hug.
}

@exercise[#:difficulty "Research"]{
  Find out what topic or subtopic you really care about.
  Get to understand it better than most other people.
  Become even better at that topic than those who teach it.
  Write a book about it.
}

@exercise[#:difficulty "Research"]{
  Make the world a better place.
  The world can use your positive contributions.
  They need not be books, and need not be about OO.
}

@section{What this Book}

@subsection[#:tag "aToOO"]{A Theory of OO}
@epigraph{There is nothing so practical as a good theory.
  @|#:- "Kurt Lewin"|
}
@principle{OO is the Paradigm of Programming with Inheritance.}
Despite what its name says,
the actual central concept in Object Orientation is @emph{Inheritance},
a mechanism for programming by modularly extending
partial specifications of code.
OO usually depends on explicit support from the Programming Language (PL) at hand,
then called an Object-Oriented (Programming) Language (OOPL).

This characterization of OO should be retrospectively obvious to all familiar with OO.
Yet remarkably, some programmers explicitly reject it, eminent professors even@xnote["."]{
  A notable dissident to this characterization is William Cook,
  a respected academic who made many key contributions to understanding the semantics of inheritance
  @~cite{Cook1989 Cook1989Inheritance cook1989denotational bracha1990mixin Cook1994}
  yet also argued that Inheritance was orthogonal to OO @; Also Cook1989?
  and that OO is about “classes” of “objects” that can only be accessed through “interfaces”
  @~cite{Cook1991PDAvsADT Cook2009 Cook2012}.

  However, coding against an SML module would count as OO by Cook’s criteria,
  and indeed Cook explicitly calls the untyped λ-calculus “the first object-oriented language”,
  while dismissing Smalltalk as not OO enough because its integers are not pure objects@~cite{Cook2009}.
  Cook’s definition, that embraces the modular aspect of OO while rejecting
  its extensible or dynamic aspect, runs contrary to common practice.
  It brings no light on any of the languages commonly considered OO
  yet derided by Cook as not being OO enough,
  no light on any of the Functional Programming (FP) languages blessed by Cook as actually being OO
  to the surprise of their users, and no light on the difference between the two.

  Cook’s many works on OO over the years also systematically neglect important concepts in OO,
  such as prototypes, multiple inheritance, method combination or multiple dispatch.
  In the end, Cook’s PhD and subsequent academic career grew out of
  brilliantly modeling the key mechanism of OO (Inheritance)
  from the foreign point of view of FP;
  but his lack of appreciation and understanding for the OO tradition,
  indeed missing the point of it all,
  were such, that they have become proverbial: immortalized in Gabriel’s essay
  “The Structure of a Programming Language Revolution” @~cite{Gabriel2012}
  as a prototypical failure to understand a phenomenon when viewed
  through a scientific paradigm incommensurable with the one that produced it.
  The problem is not just that Cook solved Inheritance as frog and
  failed to take the big picture as a bird: he did take a bird’s view,
  and still couldn’t see what his paradigm couldn’t express.

  Cook is well worth mentioning precisely to illustrate the lack of
  common vocabulary, common concepts, and common paradigms among those
  who practice and study OO, even or especially
  among notable academics with deep expertise in the field.
  And yet, there are undeniably common practices, common phenomena, common concepts,
  common language features, common design patterns, common goals, common aspirations,
  worth understanding, conceptualizing, defining and naming
  in the rich (though sometimes mutually conflicting) traditions
  that grew around OO.
}
There is thus a need to elucidate the words and concepts of OO,
behind the hype and confusion, and justify the choices made by OO
(or then again, their amendment).
Such is the main purpose of this book:
to offer a @emph{Theory of OO}.

@principle{This Theory of OO is Meaningful}.
A theory is @emph{meaningful} if it is a body of explanations
necessary and sufficient to explain what we do when
we do OO, and we don’t when we don’t.
It can clearly state the problems to be solved by or for OO, and
the criteria by which we can judge some solutions as better than others,
good or bad, acceptable or unacceptable.
In these explanatory abilities, the negative is as important as the positive:
it allows to demarcate the concept of OO from other concepts;
to distinguish that which partakes in it, that explains it,
from that which doesn’t, and would only corrupt it if accepted as part of OO.

@principle{This Theory of OO is Consistent}:
to remain @emph{consistent}, a theory shall not contain internal contradictions.
Consistency as such is easy: just avoid saying much,
stick to tautologies and things already known for sure.
Consistency, however, is much harder when you want to actually
say a lot of useful things that interact with each other—which
is why we want the theory to also be relevant.

@principle{This Theory of OO is Relevant}
A theory of OO is @emph{relevant} to OO, and not of something else,
if it matches the lore of OO:
it will restate most if not all the things we know and care about OO,
especially what has been well-known for decades.

To be both consistent and relevant at the same time in a lore full
of controversial and mutually contradictory opinions,
a theory must be critical when recalling the existing lore:
it must organize the information, distinguish the wheat from the chaff,
promoting some views deemed correct and denouncing incorrect ones,
whether or not they are backed by majority opinions.
Even if it contradicts the explanations of previous theories though,
a theory must still account for and be consistent with
the same observed and verifiable phenomena;
or else it is a theory of something different altogether.

@principle{This Theory of OO is Productive}.
To be actually interesting, a theory must be @emph{productive}:
it must positively contribute new information.
Just contributing new criteria to make sense of the existing lore
can be enough to be productive.
But I will go further and contribute more lore, too:
useful ideas never published about OO, that make it simpler.

@principle{This Theory of OO is Constructive}.
While I will discuss informal principles, I will include running code in Scheme
that you can easily adapt to your favorite programming language
(see @secref{WS} regarding choosing Scheme).
I will explain which features are needed beyond the mere applicative λ-calculus, why,
and how to typically implement them in existing programming languages.
Remarkably, the main feature needed is lazy evaluation, or ways to emulate it, @; TODO cite
as OO is most naturally defined in a pure lazy functional setting,
and eager evaluation of OO without side-effects leads to exponential recomputations.

One aspect for which I do not provide a construction, however, is static typing.
I am a proficient user of types, but am no expert at the design, implementation or theory of types.
Therefore I will only provide semi-formal designs
for what better static types for OO should look like.
And I will refer to the better papers among the many I have read,
for what I believe are good foundations for typing OO@~cite{isoop1995 iloop1995 Allen2011Type}.
Among other things, good OO types should work not just for Second-Class Classes,
but also for First-Class Prototypes;
they require recursive types, subtyping, and some form of existential types.
Dependent types are not necessary.

@subsection{Multiple Variants of Inheritance}
@epigraph{
  When you come to a fork in the road, take it. @|#:-"Yogi Berra"|
}
Now since nearly the very beginning of OO,
there have been multiple variants of inheritance to choose from @~cite{Inheritance1996}.
Many prefer Single Inheritance for its simplicity and performance
@~cite{Simula1967 Kay1993EHoS}.
Others prefer Multiple Inheritance, for its greater expressiveness and modularity,
and this multiple inheritance itself comes in multiple flavors,
notably divided on whether to use a technique called “linearization”
@~cite{Bobrow1976 Cannon1979 Traits}.
A few prefer Mixin Inheritance,
a variant in some sense intermediary between the two above,
but in another sense more fundamental, more composable @~cite{bracha1990mixin}.

With this variety of options, programmers (respectively programming language designers)
face a choice of which of several variants of inheritance to use (respectively implement),
if any at all@xnote["."]{
  And then there are dubious variants published in obscure papers, that I will not discuss.
  @; e.g. "Reverse Inheritance"
  Hopefully, after reading @secref{IMSMO},
  you will be able to understand why they are either trivially expressible
  in terms of the above variants, or fundamentally flawed,
  if you should come across them.
  That said, when, in old papers, I see pioneers struggling to find solutions to problems
  few if anyone else suspected existed, then make mistakes, or take wrong turns—I
  laugh, I cry, but I root for them.
  On the other hand, when, in more recent papers, I see researchers propose wrong solutions
  to problems that were solved long ago, I just shake my head, and express sadness that
  scientific communication and education are not working well.
}
Is there an objectively superior form of
inheritance—whether an existing variant or some combination—with respect
to expressiveness, modularity, extensibility, runtime performance,
and whatever else might matter?
Is one of the usual variants superior to the others in every way?
If not, is there a combination of them, or a superset of them, that is?
Some languages notably support forms of both single inheritance and multiple inheritance,
though with some constraints: Lisp, Ruby, Scala.
Would the best way to do inheritance subsume these combinations?
If so, how does it relate to the multiple flavors of multiple inheritance?

And of course, critics of OO argue against using inheritance at all.
What are the reasons to use or not use inheritance to begin with?
I will use the absence of inheritance as a baseline against which to evaluate our variants.
@;{ Cite https://freedium.cfd/https://medium.com/better-programming/object-oriented-programming-the-trillion-dollar-disaster-92a4b666c7c7 } @;

@subsection{Optimal Inheritance}
@epigraph{
  An extreme optimist is a man who believes that humanity will probably survive
  even if it doesn’t take his advice. @|#:-"John McCarthy"|
}
I will claim that indeed (a) there is a best way to combine single and multiple inheritance,
that (b) it involves linearization of the inheritance graph,
that (c) there are enough constraints on linearization for the optimal algorithm
to be well-defined up to some heuristic, and
that (d) there are good reasons to prefer a specific heuristic.
@principle{The C4 algorithm implements this Optimal Inheritance.}
I implemented C4 as part of
the builtin object system of @(GerbilScheme) @~cite{GerbilScheme}@xnote["."]{
  Scheme @; XXX CITE lambda-the-ultimate r4rs r7rs-small
  is a language with a rather minimalist definition.
  Dozens of mutually incompatible implementations of Scheme exist
  that each provide their own extensions on top of this common minimal core,
  with various degrees of compliance to various standards,
  to offer a usable programming environment.
  However, there is no common object system,
  instead plenty of different object systems that span the entire design space for OO—except for
  their generally lacking static types.
  @(GerbilScheme) provides its own builtin object system, not compatible with any standard,
  but with arguably the best inheritance of any object system to date (as of 2026).
}

It is unusual for a book to claim some significant innovation like that:
usually, a researcher would publish it at some conference.
However, C4 in isolation might only look mildly interesting:
it “just” combines a couple well-known ideas and a speed optimization.
The concepts I develop are a prerequisite for the claim of optimality of C4 to even make sense,
yet require this book to properly articulate.
C4 itself is a notable improvement, that crowns the theory as productive.
But the theory behind it is the real achievement.

What that means for you in practice, though, is that a good theory brought you
a better inheritance algorithm with which to improve your existing (or future) languages.

@exercise[#:difficulty "Easy"]{
  Pick a theory you hate on whatever topic you know about.
  Analyze how this theory satisfies or fails to satisfy the criteria I have set for mine.
}

@exercise[#:difficulty "Medium"]{
  For each of the criteria that I claim my theory satisfies,
  identify a theory you know about that fails this criterium.
  With regards to productivity, mind that it is enough for the theory
  to have produced novel results @emph{at the time it was first proposed}—it is fine
  if the results of that theory are not novel anymore.
}

@exercise[#:difficulty "Hard"]{
  Pick a theory you believe in that is controversial in the public at large.
  Identify how at least the way it is presented by people on your side,
  that theory is failing one of the criteria I set for my theory.
}

@exercise[#:difficulty "Research"]{
  Pick some phenomenon you’re interested in and able to observe.
  It can be something small or large, but has to be something you can repeatedly interact with.
  Some of your behavior or that of a member of your direct family or colleague at work is fine,
  but not the behavior of a random stranger you won’t meet again, unless it’s a general enough
  behavior that many people exhibit around you every week.
  Develop a meaningful, consistent, relevant, productive and constructive theory of that phenomenon.
}

@exercise[#:difficulty "Research"]{
  Identify some technique that comes in many variants,
  with divergent opinions on which variant is better,
  in a field you’re interested in.
  Determine whether you can devise an optimal variant of that technique,
  or an optimal strategy to pick which variant in which circumstances.
}

@section{How this Book}

@subsection{Plan of the Book}
@epigraph{
  — Would you tell me, please, which way I ought to go from here? @linebreak[]
  — That depends a good deal on where you want to get to.
  @|#:- "Lewis Carroll"|
}
In @seclink["Intro"]{chapter 1}, which you are presently reading, I introduce the book itself.
The remaining chapters focus on OO as such, until the conclusion, where I again take a step back.

In @seclink["WOOiIO"]{chapter 2}, I provide
a quick overview of Object Orientation,
and the three variants of inheritance in common use.
This chapter serves as a map of the concepts and of the words I use to describe them,
necessary because there is no common theory of OO, and
no unambiguous shared vocabulary to name what common concepts there are.
Importantly, I introduce the essential yet oft-ignored notion of
Conflation between Specification and Target value.
I then describe the relationship between Specifications, Prototypes, Classes and Objects.

In @seclink["WOOin"]{chapter 3}, I dispel
common misconceptions about OO,
to ensure that my theory isn’t met with misunderstanding
due to these misconceptions or to disagreements about what is or isn’t being theorized.

In @seclink["OOaIEM"]{chapter 4}, I explain
what I mean by Internal Extensible Modularity,
the rationale for OO, its motivation and purpose.
This chapter remains informal, but lays the conceptual groundwork
for the formal approach I take in the rest of this book.

In @seclink["MOO"]{chapter 5}, I introduce
minimal formal models of Modularity and Extensibility.
Using pure Functional Programming (FP) as a foundation, with Scheme syntax,
I derive from first principles a minimal OO system, in two lines of code.
This minimal OO system uses mixin inheritance, and, remarkably,
has neither objects nor prototypes, much less classes,
only specifications and targets.

In @seclink["ROOfiMC"]{chapter 6}, I rebuild
all the mainstream features and appurtenances of popular OO systems
as additions or modifications to the minimal system from chapter 5:
prototypes, classes, mutation, etc.
I notably discuss the actual relationship between OO and imperative programming,
when the natural framework for OO is actually pure lazy functional programming.

In @seclink["IMSMO"]{chapter 7}, I discuss in detail the main forms of inheritance:
single inheritance, multiple inheritance and mixin inheritance.
I examine issues surrounding method conflict and resolution, or harmonious combination.
I explain the known consistency constraints that matter
for linearization algorithms in the context of multiple inheritance,
and the state-of-the-art in satisfying them, the C3 algorithm.
Finally, I discuss how to combine multiple and single inheritance,
and examine the existing solutions adopted by Common Lisp, Ruby and Scala.
I then propose my solution, a linearization algorithm I call C4,
that satisfies all the constraints of C3 plus
those for combining single and multiple inheritance.
I explain why the residual heuristic I also adopt from C3 is arguably the best one.

In @seclink["TfOO"]{chapter 8}, I study the kind of types and typesystems
that are suitable to reason about OO.
I notably clarify the all-too-common confusion between subtyping and subclassing.
This chapter is somewhat less constructive than the others, as
I do not actually implement a typesystem.

In @seclink["EtSoO"]{chapter 9}, I discuss
more advanced topics including
Focused Modular Extensions, Method Combination, Multiple Dispatch (Multimethods),
Monotonicity, Orphan Typeclasses, and Global Fixpoints.

In @seclink["EOI"]{chapter 10}, I discuss object representation and meta-object protocols.

Finally, in @seclink["Conclusion"]{chapter 11},
I conclude by recapitulating my original findings.
If you want to check whether there’s anything new for you in this book,
or are a future researcher interested in when now-well-known ideas were introduced,
you may peek there first.

Follows an @seclink["AB"]{Annotated Bibliography},
with notes about each of the works cited to explain what makes them relevant.

@subsection{Stop and Go}
@epigraph{Begin at the beginning and go on till you come to the end: then stop.
  @|#:-"Lewis Carroll"|
}
This book has a mostly linear narrative:
Every chapter builds on the previous ones.
The end of every chapter is a nice place to stop reading,
and restart later if you are still engaged.

The narrative goes from the most informal, most generic and most basic information about OO,
to the most formal, most specific and most advanced.
Some readers may prefer to stop when the material becomes more formal.
Others may want to skip the informal discussion and jump directly to the formal parts
at @secref{MOO}, about a third into the book.

The most enthusiastic among you will read the book cover to cover,
including footnotes and bibliographical notes, and
go all the way into using and implementing
the most advanced OO techniques of the later chapters (see @secref{EtSoO}, @secref{EOI}),
end up building your OO system, and writing a sequel to this book.
If you do, why not contact me and join me to build and write them together?

But you don’t have to be that enthusiastic to read and hopefully enjoy this book.
You may read casually, skip parts that don’t interest you.
You can look only at the section titles and the informal principles in bold italic.
You can focus on the formal code definitions, or their type declarations.
You can search for an argument on a specific controversy.
You can scan the bibliography for more things to read.

What will enhance your experience, however, will be the ability
to interact with a computer and play with the code.
If you have an electronic copy, you may copy/paste the code,
use text search to compensate for the lack of a word index,
click directly into the sections that interest you,
and even ask AI assistants for navigation help.

@subsection{Self-Description}
@epigraph{
  An adjective is autological if it describes itself (e.g., "short" is short).
  An adjective is heterological if it does not describe itself (e.g., "long" is not long).
  Now consider the adjective "heterological": Is it heterological?
  @|#:-"Grelling–Nelson paradox"|
}
This book includes enough self-descriptions ahead of each section
that you hopefully may make reasonable decisions of which parts to read,
to skip, to skim, to read attentively,
to keep, to throw away—or, if you’re ambitious, rewrite.

At times, I will highlight some short sentence in bold italic,
to make it clear I believe it is an important principle, as follows:
@principle{“If you have exceptions to your stated principle,
ask yourself by what standard you make the exceptions.
That standard is the principle you really hold.”}
The above principle describes principles;
in this case, it was written not by me, but by my friend Jesse Forgione.

I intend to include more examples in a future edition.
But good examples take time to write, and space in the book;
they can be too much for some readers and not enough for others.
I am seeking the perfect concise teachable example in each case,
that neatly illustrates what I mean without taking too much space or explanations.
Until I find it, I must direct you to online resources, where OO code in general is abundant.
If you are looking specifically for code that uses Prototype OO and multiple inheritance,
you may look at my library Gerbil-POO @~cite{GerbilPOO}:
it provides a practical but short implementation of a prototype object system;
and it builds interesting type descriptors on top of that object system,
including a nice trie data structure,
that is further specialized in the gerbil-persist library.
AI assistants may also be able to find examples tailored to your needs.
If you struggle with a particular concept that lacks an example, please tell me.
And if you find good illustrative examples for ideas you or others struggled with,
please send them my way.

@subsection{Being Objectively Subjective}
@epigraph{
  Be… suspicious… of all those who employ the term ‘we’ or ‘us’ without your permission.
  This is [a] form of surreptitious conscription… Always ask who this ‘we’ is;
  as often as not it’s an attempt to smuggle tribalism through the customs.
  @|#:- "Christopher Hitchens"|
}
You will see me using the first person singular a lot in this book.
That doesn’t mean I don’t want to include you in my narrative.
Believe me, it would most delight me if you could feel the same joy at exploring this topic as I do.
Every sentence of this very book is an invitation for you to see and practice OO my way.
That doesn’t mean I am bragging, either.
That means I am taking responsibility for my actions.

Too many authors hide the responsibility for a decision among multiple authors
(even when there is only one),
or include the readers in a collective decision they did not make.
Using an unwarranted “we” is a trick commonly used by conmen, narcissists and politicians
(but I repeat myself), and I find it misleading even when done innocently.
I once set myself and my friends a “cuss box” in which to drop a dollar
when any of us did it, even on social media.
But maybe Mark Twain put it best: “Only presidents, editors, and people with tapeworms
have the right to use the editorial ‘we’.”
And I don’t think presidents have that right, either.

I will still say “we” on occasions,
speaking for me and you readers, or for all humans.
That “we” will then be passive, as things that we are, experience, or happen to us,
or are constrained by the laws of logic and history:
“we saw that example in a previous section” (you who read and I who wrote),
“that experiment tells us” (us who saw it),
“we cannot solve the termination problem” (we subject to logic).
It will not be a trick to hide an action or decision that some among us made
while shifting praise or blame or responsibility onto others:
“we got one Nobel prize each—on average” (Marie Curie and I, but she did all the work),
“we killed that poor man” (I did, but I’m trying to implicate you),
“we must help that poor widow” (you all must, I’ll take a large cut of the funds).

@subsection{Technical Nomenclature}
@epigraph{When words are unfit, speech is unadapted and actions are unsuccessful.
@|#:- "Confucius"|
}
As I restate well-known and less-known lore of Object Orientation,
I will endeavor to precisely define the terms I use.
Defining terms is especially important since various authors
from diverse communities around many OO languages
each use conflicting terminologies,
with different words for the same concepts,
or—which is worse—the same words for different concepts.
This tower of Babel can cause much confusion
when trying to communicate ideas across communities,
as people ascribe conflicting assumptions and connotations to the words
used by other people, and talk past each other
while incorrectly believing they understand what the other said.

Thus, when multiple nomenclatures conflict,
I will try to identify the @emph{least ambiguous} word for each concept,
even if it is neither the most popular word for the concept, nor the oldest,
even if I sometimes make one up just for this book.
Ideally, I can find a word that will be unambiguously understood by all my readers;
but if that is not the case, I will prefer
an awkward word that causes readers to pause and reflect,
to a deceptively familiar word that causes them
to unwittingly misunderstand the sometimes subtle points I make.

In particular, I will conspicuously avoid using the unqualified words
“object” and “class” unless strictly necessary,
because they carry different connotations for each reader,
depending on their adopted traditions,
that are at odds with the theory I am laying out.
I will also avoid the word “class” when talking about
the most general kind of entity subject to inheritance,
since a class is but a quite limited special case of a @emph{prototype},
that is itself derivative of what I’ll call a @emph{specification}.
I will define those terms precisely in
@secref{WOOiIO}, @secref{OOaIEM}, @secref{MOO}, @secref{ROOfiMC}.

@exercise[#:difficulty "Easy"]{
  Note that this book addresses readers with some familiarity with programming,
  and that it starts having code at @seclink["MOO"]{chapter 5}.
  Adjust your expectations and reading pattern accordingly.
}

@exercise[#:difficulty "Easy"]{
  What does the word “function” mean to a mathematician?
  To a C programmer? A Scheme programmer? Haskell programmer? Rocq user?
  Are they the same thing?
}

@exercise[#:difficulty "Easy"]{
  Identify a case where someone tries to bamboozle others by using the word “we” or “us”.
}

@exercise[#:difficulty "Medium"]{
  Identify in your domain of expertise, a technical word that is used to mean different things
  by practitioners of different backgrounds and communities.
}

@exercise[#:difficulty "Medium"]{
  State a principle that pertains to writing good programs,
  that many seasoned professionals know (though not necessarily all, or the majority, of them),
  and that uninitiated beginners will have trouble figuring out by themselves.
}

@exercise[#:difficulty "Hard"]{
  Identify a case where you were once bamboozled by someone using the word “we”
  and your granting undue credit or accepting undue responsibility—or
  a case were you bamboozled someone (typically child or spouse)
  with the same technique.
}

@exercise[#:difficulty "Hard, Recommended" #:tag "01to02"]{
  After reading this chapter, but before you read the next,
  try to characterize what you think OO @emph{is} about in the end.
  Can you make a mind map, a list of concepts that matter,
  how they are defined, how they relate to each other?
  Choose your level of precision, but try to cover all you know about OO.
  While this exercise is somewhat hard, it will make next chapter more enlightening,
  so save your answers to compare them to the treatment in @secref{WOOiIO}.
}

@exercise[#:difficulty "Research"]{
  Articulate some principle of good program design,
  that stems from your experience, that few other people really master,
  and that hasn’t been fully articulated yet.
}

@exercise[#:difficulty "Research"]{
  Help a friend or family member who has been bamboozled by words
  see through the deception they fell for.
}

@exercise[#:difficulty "Research"]{
  Hardest of all: let apparent enemies help you identify
  where you yourself are being bamboozled right now.
}
