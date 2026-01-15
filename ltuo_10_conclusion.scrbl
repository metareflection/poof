#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")

@title{Conclusion}

@section{Scientific Contributions}
@epigraph{Early in life I had to choose between honest arrogance and hypocritical humility.
I chose honest arrogance and have seen no occasion to change.
@|#:- "Frank Lloyd Wright"|
}
Here is the part of this book where I actually do the bragging,
with a list of never-done-before feats I achieved in its book
or the work that immediately preceded it:

@;TODO add secref's everywhere

@subsection{OO is Internal Modular Extensibility}
I rebuilt Object Orientation (OO) from First Principles,
offering an explanation of how the basic mechanisms of OO
directly stem from Modularity, Extensibility, and Internality.
The equations of @citet{Bracha1990Mixin} were not arbitrary axioms,
but necessary theorems, that I could further simplify and generalize.

@subsection{My Theory of OO is Constructive}
I built minimal OO in two lines of code directly relating principles to code;
my code is both simpler and more general than the formulas from the 1990s;
it can be used not just as a “semantic model”,
but as a practical implementation usable in actual applications.
A few more lines gave you recursive conflation;
yet a few more tens of lines give you multiple inheritance.
None of it is just theory, none of it is magic, none of it is ad hoc,
it’s all justified.
And it’s portable to any language with higher-order functions.

@subsection{Precise Characterization of those Principles}
So I may derive OO from them, I first gave novel and precise, though informal,
characterizations of Modularity and Extensibility, based on objective criteria,
when familiar notions previously were often invoked a lot but never well defined.
For Internality, I extended the familiar but not always understood
notions of “first-class” and “second-class”
with new notions of “third-class” and “fourth-class”.

@subsection{Demarcation of OO and non-OO}
Crucially, my Theory of OO explains not only what OO is, but also what it is not.
This is important to debunk honest mistakes, incompetent errors,
ignorant claims, and outright frauds, that any successful activity attracts,
harming actual or potential OO practitioners.
OO is not C++, OO is not based on Classes, OO is not imperative,
OO is not about “encapsulation”, OO is not opposed to Functional Programming (FP),
OO is not about message passing, OO is not a data model, OO is not rewrite logic.

@subsection{OO is naturally Pure Lazy FP with first-class Prototypes}
Remarkably, and contrary to popular belief,
I proved the natural paradigm for OO is Pure Lazy Functional Programming.
This is the very opposite of the eager imperative model
that almost everyone associates to OO,
indeed used by currently popular second-class Class OO languages.
Yet in these languages, OO only happens at compile-time,
indeed in a pure lazy dynamic functional programming language,
though often a severely stunted one.

@subsection{Conflation of Specification and Target as All-Important}
I elucidated the concept of conflation,
latent in all OO since @citet{Hoare1965Record},
necessarily though implicitly addressed by each and every one of my predecessors,
yet never a single time once explicitly documented before. Shame on them.
Before making this concept explicit,
the semantics of objects was extremely complex and ad hoc.
After making it explicit, the semantics of objects is astoundingly simple,
it just involves a regular use of the simplest of recursion operators, the fixpoint.
Plus an implicit pair to bundle specification and target together.
I also argued why conflation, if properly understood, can increase modularity,
even though when misunderstood it brings lost of harmful confusion.

@subsection{Open Modular Extensions as Fundamental}
Compared to previous theories that only consider @emph{closed} modular extensions
(where the extension focus coincides with the module context),
or worse, closed modular definitions (with no notion of extension),
my @emph{open} modular extensions @emph{vastly} simplify OO, by enabling:
@itemlist[
  @item{Simpler more universal types with no ad hoc construct, just plain recursive subtypes}
  @item{Combining, composing and decomposing open specifications with rich algebraic tools}
  @item{Using optics to zoom semantics at all scales, down from individual method declarations
        up to entire ecosystems of mutually recursive prototypes}]

@subsection{Flavorful Multiple Inheritance is Most Modular}
Using my characterization of modularity, I could prove that the following variants of inheritance
are in order of strictly decreasing modularity:
(1) flavorful multiple inheritance with local order and monotonicity,
(2) less consistent flavorful multiple inheritance,
(3) mixin inheritance,
(4) flavorless “conflict” multiple inheritance,
(5) single inheritance, and
(6) no inheritance.
And I explained why so many great computer scientists
got stuck into the flavorless “conflict” multiple inheritance
and how and why the “harmonious combination” view is so much better.

@subsection{The Prefix Property, not Single Inheritance, matters}
I explained why exactly single inheritance is more performant
than other kind of inheritance so far.
I precisely identified the @emph{semantic} constraint that enables the extra performance:
the @emph{prefix property},
when other authors were incorrectly associating the performance with the
more @emph{syntactic} constraint of single inheritance@xnote["."]{
  More precisely, the prefix property is a semantic constraint
  on the context of future use of a specification.
  While single inheritance is either a syntactic constraint on how the specification is defined.
  You could insist on calling that a semantic constraint, too:
  a semantic constraint on the text of past specification construction.
  But beware not to make the concept of “semantic” completely useless by
  declaring everything semantic, thus making it incapable of discrimination.
  And what is a syntactic constraint if not such semantic constraint
  on the text of past specification construction?
  Anyway, this shift from text/past to context/future is what matters, call it what you may.
}
I found that this property is actually compatible with mixin inheritance and multiple inheritance,
wherein it can enable the same performance improvements for specifications
for which the system enforces the property.
I realized that some languages (including Ruby, Strongtalk, Racket, Scala)
may have relied on this property before in contexts beyond mere single inheritance;
yet so far as I can tell their authors never precisely identified the property.

@subsection{There is an Optimal Inheritance}
I implemented a new variant of inheritance.
It is more than just combining previous ideas,
such as tucking C3 onto the design of Ruby or Scala,
though even that, or just my optimization of C3 from O(d²n²) to O(dn),
would have been a (modest) contribution:
I also showed that the resulting design is necessary to fulfill a higher purpose of optimality:
it subsumes multiple inheritance, from which it keeps the maximum modularity,
and single inheritance, from which it keeps the maximum performance.
It is not arbitrary, not just a clever hack made necessary
for backward compatibility with existing infrastructure.
This Optimal Inheritance, as implemented by my C4 algorithm, is now part of @(GerbilScheme);
you can easily port my code to add it to your own language.

@section{Why Bragging Matters}
@subsection{Spreading the New Ideas}
@epigraph{Don’t have good ideas if you aren’t willing to be responsible for them.
@|#:- "Alan Perlis"|
}
What’s the use of having good ideas and writing about them if the readers don’t even notice?

Writing this book won’t bring me fame and status.
But if it can convince future programming language implementers
to add OO to their language, and add the best variant of it
rather than the pathetic variants that are mainstream today,
then my work will not have been in vain.
By providing a clear list, I am making sure they know where to start from, and what not to forget.
Readers, @principle{the list of achievements above is a list of opportunities for you to improve
the software you use and build}.

I actually started the list of achievements above back when this book was supposed to be a short paper
to submit to a conference or journal.
Reviewers are overwhelmed with papers to review,
most of them of bad quality, hiding vacuity under a heap of verbiage.
They are not paid, and it is draining to say no, even to outright bad papers,
and even more so to papers that have some good in it,
but that are not yet worth the reader’s time.
An author has to make extra effort to make his contributions clear,
even though it’s hard on him, even though some authors will give up
before they make their paper publishable, and their original contributions are then sadly lost.
I thank the rejecting reviewer who once told me to make my claims clearer.

@subsection{Spreading Coherent Theories}
@epigraph{@emph{Let theory guide your observations},
but till your reputation is well established,
be sparing in publishing theory.
It makes persons doubt your observations.
@|#:- "Charles Darwin to a young botanist"|
}
My mentor Jacques Pitrat once told me that when a French researcher has three ideas,
he writes one paper. When an American researcher has one idea, he writes three papers.
If you want your paper published in an American conference,
you need not only use the American language, as explicitly demanded,
but also the American style, as tacitly required.
Otherwise, the reviewers won’t be able to understand you,
and even with the best intentions, they will reject your paper.
So split your paper in nine parts, and submit each of them independently.

However, some ideas are not worth much by themselves, if they make sense at all.
Some ideas only become valuable because of the other ideas that follow.
Some other ideas don’t make sense without all the ideas that precede.
What is the value of debunking bad ideas about OO and
saying mean words about a lot of good scientists?
Yet how can I even explain the basic principles of OO if the readers misinterpret what I say
because they are confused by those bad ideas about OO, spread by good people they respect?
And what is the value of those basic principles if they have no correspondance to actual code?
But how could I derive a minimal model from the principles without having examined them? etc.

Each chapter of this book is worth something but only so much by itself,
each of my innovations may only seem so modest by itself.
Trying to publish them separately would be a lot of effort,
each article spending most of its time recapitulating knowledge
and fighting false ideas before it could establish a result
the utility of which would be far from obvious.
Even if possible, getting all the ideas in this book published in academic venues
would take ten times the effort of writing this book,
for a result that is overall more repetitive, yet locally too concise and less clear.
By bringing all these ideas together, and taking time to expose them in detail,
I built a solid coherent Theory of OO that I hope you’ll agree is compelling.

@section{OO in the age of AI}

@subsection{Farewell to the OO you know}
@epigraph{
  AI is a collective name for problems which we do not yet know how to solve properly by computer.
  @|#:- "Bertram Raphael"|
  @; https://quoteinvestigator.com/2024/06/20/not-ai/
  @; https://en.wikipedia.org/wiki/AI_effect
}
AIs are not as limited as humans in terms of mental context.
They do not experience the same pressure towards reusable code as humans do.
And so in many cases, AIs may prefer to deal directly with a lot of low-level details at once,
tangle many aspects of a problem, and embrace the complexity of it all,
so as to achieve more efficient results.

As a result, AI may write programs that are much less modular than those human write.
As the size of modules might gets larger,
there will also be fewer opportunities for meaningful incremental specifications:
the overhead of each increment might not be worth making a lot of them.
But sparser bigger increments might also mean fewer opportunities
to share code between programs.

OO languages and frameworks currently popular among humans
may prove especially not interesting for AIs:
AI ways of thinking will differ from the humans who produced them;
and AIs can evolve code faster than humans,
meaning that whatever languages and frameworks they use needn’t be tethered
to the path-dependence that led to the historical artifacts humans currently use.

Less need for modularity and extensibility mean that AIs
will have less need for OO in general.
Different and faster capabilities mean that AIs
will be less interested in current OO code in particular.

And yet…

@subsection{OO will still matter in the end}
@epigraph{
  “reasoning about the code” means that you can draw conclusions
  using only the information that you have right in front of you,
  rather than having to delve into other parts of the codebase.
  @|#:- @elem{Scott Wlashcin, @citet{Wlaschin2015}}|
}
@emph{Modularity will still matter}:
AIs too will eventually hit complexity walls;
factoring code in good ways that promote reuse across agents will be essential
for AIs to cooperate together on larger, better, safer software.

@emph{Extensibility will still matter}:
The code that AIs produce will still have to fit into resource-constrained machines.
Code reuse will thus remain instrumental in making efficient use of limited resources.

@emph{Internality will still matter}:
Systems that evolve alone may only also need external extensibility;
but inasmuch as they branch from modules shared between AIs,
this extensibility will better be internal rather than merely external.

And so, all the reasons that make OO useful and necessary will still exist.
The very reason that makes my definition of OO correct (@secref{Imdc})
is what keeps it relevant into the future:
OO is a phenomenon that programmers (humans or AIs) care about because
we can write much better programs if we understand and use it properly than if we don’t.

For in the end, OO is about @emph{compositional semantics},
and these matter not merely for human convenience, but for the sake any intelligent entity
that wants to reason about programs in ways simple enough to manage.
Complexity leads to exponential explosion of program spaces to reason about,
and even orders of magnitude more context will only give AIs
a few layers of complexity more than humans before they too need to actively seek simplicity.
If anything, at least some AIs in some contexts may be more adamant than humans about
software security (a more direct matter of life and death, for them),
and thus about reasoning about software, and thus about simplicity.

@subsection{Not your grandfather’s OO}
@epigraph{It is difficult to make predictions, especially about the future.
 @|#:- "Karl Kristian Steincke"|
}
Who can say what software will look like in the future?
By the time you can precisely describe a piece of software, it is not in the future anymore.
It is very unlikely that the OO languages, programs and libraries that are popular today
will survive very long in the future—although paradoxically, in the short term,
increased AI capabilities also means increased ability to survive the nonsense of it all.

But my safe bet is that in the future not only there will still be OO, but much more OO than today,
because OO is useful to grow @emph{reasonable} ecosystems,
for which the demand will only increase.
However, I also bet that OO style will end up covering
a smaller overall share of the software than it does today,
because AIs can manage a level complexity that reduces the relative demand
for modularity and extensibility within a programming language.
Moreover, I will also bet that today’s popular forms of OO
will keep decreasing in relative popularity:
mutable eager programs, specified using second-class Class OO
with single inheritance or flavorless multiple inheritance,
are just too unreasonable.

Now if my book has any influence, and quite possibly even if it has none,
a better form of OO will increase in relative popularity:
pure functional lazy OO, either dynamically typed or with recursively constrained subtyping,
in typeclass-style more so than in class-style,
with flavorful optimal inheritance, method combinations and multiple dispatch.

May you live free, and enjoy software written with Object Orientation!

