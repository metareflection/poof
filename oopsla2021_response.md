We thank all the reviewers for their insightful reviews and useful references.
We particularly thank reviewer A for walking us through issues in our presentation.
We particularly thank reviewer C for the encouragement and the many useful suggestions.

Though we believe the paper does have more valuable contributions
than was recognized by the reviewers,
we readily admit that the failure to make our contributions clear was entirely ours, and that
we probably cannot fix those failures and have the paper fit in the required format
within the time available for revising papers for this particular conference.

## Making our claims clearer

We crucially failed to make our claims clear and precise enough in our introduction.
This notably caused reviewers A and B to build expectations we didn't mean and couldn't meet.

Reviewer A in his summary infers that we try to "create *a* flexible object system"
(emphasis ours), then expects in vain "the" definition for our objects.
Reviewer B summarizes that we talk "about an object encoding in a functional language",
then later squarely states "the purpose of this work is unclear".

Instead, we were trying to constructively identify a minimal basis of combinators
with which to build not one object system but all of them.
We started from the simplest system recognizably implementing OOP,
so simple it arguably didn't even have "objects" as such.
We then increased sophistication, showing how to reproduce a growing subset
of features found in past object systems.

As contrasted to previous models, our approach puts emphasis on
- (a) the conceptual distinction between *instances*, *prototypes*, *prototype functions*,
and *prototype objects* (that combine prototype and instance together),
then further *classes* (distinguishing between *class-as-prototype* for a type,
and *class-as-instance* of such a prototype), and *objects as elements of a class-as-instance*,
as separate though related entities of interest,
- (b) *composition* of prototype functions rather than *application*
of one to a "generator" as the algebraic structure of interest,
- (c) explanations of both *why* multiple inheritance is useful and *how* to formalize it,
- (d) *prototype* OOP being more primitive than *class* OOP, and
how the latter can be simply derived from the former, but not the other way around,
- (e) a pure functional approach that provides not only denotational semantics
atop the pure untyped lambda-calculus, but also a *constructive* implementation,
- (f) a constructive model that doesn't rely on mutation as common in OOP implementations,
yet, that we discuss how to extend to play well with mutation,
which we actually implemented (though in a cited library, not the paper itself).

So far as we can tell, each of these claims is original to our paper,
compared to previously published academic attempts at formalizing OOP,
including those cited by reviewers B and C
(but see below our discussion of the Oliveira 2009 paper).

## Type issues

Reviewer B correctly notes that "the actual challenge of object encodings is usually
in the static typing", but incorrectly adds "which isn't addressed in the paper".

In our paper, we assign dependent types to our proposed primitives,
though in a semi-formal way.
We also explain how these types collapse to something rather trivial without dependent types,
yet how these collapsed types are enough to account for the most common use of prototypes:
to represent classes as prototypes and/or instances for type-descriptors,
at a staged meta-level, in compilers for class-based object systems
(and also see below our discussion of the Oliveira 2009 paper).

Previous formalization attempts systematically made choices opposite ours:
classes rather than prototypes, application rather than composition,
no attempt to formalize multiple inheritance, multiple dispatch, method combination,
no discussion of mutation.
This is not a coincidence: these deliberate simplifying choices enabled previous formalizers
to use simple type systems to assign types to objects,
when these same type systems cannot handle the general case of prototype object systems.
If anything this is an indictment of the limitations of the previous typing approaches,
and not of the utility of prototype object systems,
which was amply demonstrated by their practical use in the industry,
in many programming languages and configuration languages.

We chose to formalize the general case precisely because we seek to reconstruct all OOP.
We immediately found these type systems too limited for the purpose.
Instead, we used dependent types to assign semi-formal types to our primitives.
We agree with reviewer A's criticism that our types are insufficiently formal,
and that even our informal explanations are lacking.
Fully formalizing these types in a dependent type system like Coq, Lean, Agda, or Idris,
or maybe better in a Curry-style system like Cedille,
would be a great topic for a future work.
Still, we will definitely improve our informal explanations as we rewrite our paper
for eventual publication.

## Originality

We could trace each of the techniques we present back to various
object systems actually implemented over the past fifty years.
Now, these practical implementations tended to focus on efficiency,
and made no attempt at clear, general, reusable, formal concepts.
Our claim of originality is therefore not in any of these techniques as such,
but in
- (a) a simple and general formalism in which to express them,
and
- (b) the new combinations that our formalism enables programmers to easily build,
in a few tens of lines, in any language that includes
the untyped (or dependently-typed) lambda calculus.

To illustrate this dual claim, we implemented not one but several object systems in our paper.
These systems culminate in section 4.3 with an original system that combines
features never previously seen together in a same object system:
- pure functional setting, no side-effect needed, but laziness or a delay/force primitive for sharing;
- prototype-based, which is more expressive than class-based;
- multiple inheritance, which is more modular than single-inheritance.

(The code for all our object systems is listed _in extenso_ in the paper and its appendices.
The source file is actually a runnable artifact in addition to being printable,
and provides a reusable library.
Still, for actual use we instead recommend the Nix and Scheme libraries we maintain.)

Yet we do not claim this particular object system as the achievement aimed at by our paper.
It is merely an *illustration* of how to use the combinators we introduced to build,
in a few lines of code, a system that had never been implemented before.
In the subsequent sections we propose many ways to generalize or extend this system.

## Discursive Style

We apologize for the discursive style of our paper, as justly blamed by reviewer A.
At a very minimum, we should have made our approach as well as our claims
clearer in the very introduction.

Indeed, as as reviewer C suspected, we aimed more for a "pearl" on
how to reconstruct OOP in a functional setting than for a
"research paper" focusing exclusively on our original contributions.
On the other hand, it might be hard to explain these contributions and their value
outside the context of such a reconstruction.

We also got squeezed between all the topics we wanted to explore
and the size and time limitations for writing the paper.
Reviewer A might be right that sections 4.3 to 6 deserved to be treated
in a separate paper (or maybe several), and then more extensively.
Yet, section 4.3 was instrumental to illustrate the *originality* of our approach (see above),
while section 4.4 to 6 are essential to argue its full *generality*.
Still, some or all of these sections could be cut and/or moved to
Related Works, Future Works and/or Appendices.

## Related Works

It was our mistake not to have a separate section for past Related Works.

One issue is that given our ambition to provide a basis for
the constructive formalization of all variants of OOP,
we would have to go over fifty years of both formal-but-not-usually-constructive
academic research and constructive-but-not-usually-formal industry practice
to comprehensively relate our work to previous ones.

We are grateful reviewers B and especially C for the many relevant works they cite.

In his 1989 thesis cited by reviewer C,
Cook indeed touches all the basic concepts that we develop.
Yet Cook still falls short with respect to all the claims we make above.
What we call "prototype function", Cook notably calls "wrapper",
borrowing a term from Flavors while generalizing its meaning.
The model in our section 1 is a slight generalization of Cook's beyond records as instances.
Cook's model is in turn a slight generalization of the latter independent-seeming reinvention
in the Nix extension system by Peter Simons (who was not available for comments so far)
(as for Andres Löh, cited by Reviewer C, it isn't obvious what part he had, if any,
in creating the Nix extension system, though he used it after it was created).
Then, in sections 3 to 6, we formally extend our model in ways that Cook only discusses informally
when he glosses over multiple inheritance and later over other features of Flavors.

Another very relevant paper cited by Reviewer C is the unpublished Oliveira 2009 paper.
Oliveira indeed uses essentially the same prototype construction as we propose in our section 1,
in the same generalization of Cook's 1989 formalization.
Oliveira calls "mixin" what we call "prototype function"
(which coincides with "prototype" in the model of our section 1, but not in our later models).
Oliveira makes a dubious choice in not passing a base object to his fixed-point operator,
instead using the fixed-point itself as pseudo-base-object; but that is a very minor defect.
More importantly, Oliveira correctly identifies composition rather than application
as the interesting algebraic structure
(yet in section 4.4, incorrectly equates it with boxed composition in Cook's formalism).
However, the main limitation in Oliveira's paper is that,
by the lack of dependent types in Haskell, the proposed construction is restricted to
the "monomorphic" case we discuss in our section 3.4
(in which this would be an excellent citation),
and is unable to fully express prototype OOP, or even just prototypes for usual OOP typed records.
That is why we will still claim originality for our point (b) of identifying composition
as the interesting algebraic primitive in the general case of prototype OOP.
(Then again, outside academia, Dave Cunningham has a better claim to priority,
when he designed and implemented Jsonnet in 2014 with such composition as primary operator,
though without any academic publication; and
the designers of GCL might have done something similar before him.)
Despite the limitations of Haskell’s type system, Oliveira leverages
constraints on higher-kinded types to push his concept of mixins quite far,
indeed mind-blowingly further than we could imagine.
It would be interesting to establish what exactly can or cannot be done
within the constraints of the Haskell type system
(or families of type systems implemented via various combinations of GHC pragmas).
Note that the limitations of Haskell-like type systems is why the two pure lazy functional languages
with successful prototype object systems so far (Nix and Jsonnet) are both dynamically typed,
a combination unjustly neglected by pure functional programmers until recently.

Reviewer C also cites several other works, that we haven't had time to look at in detail,
but that also look very relevant.

Reviewer B, who concluded our paper was "about an object encoding in a functional language",
usefully cites the Bruce/Cardelli/Pierce 1999 paper on that topic.
That paper makes no attempt whatsoever at capturing the semantics of prototype composition,
instances vs prototypes vs objects, multiple inheritance, multiple dispatch,
method combination, mutation, initialization protocols, or even
the fine details of instance representation—all aspects that we discuss.
That paper only focuses on one aspect of object encoding:
the structural types to assign them and the type systems required to express these.
The basic model of our section 1, like Cook's,
corresponds to the OR encoding in that paper;
or maybe we should more precisely call it ORB, since in a refinement to OR,
our semi-formal type explicitly includes a subtyping (B)ound as constraint to ensure a fixed-point.
The three other representations in that paper seem to be tailored to class-based OOP;
we believe they do not apply to prototype-based OOP.
Yet, we conjecture they naturally emerge when using our reduction of class-based OOP
to prototype-based OOP. We believe the differences in wrapping
by their OE, ORE and ORBE encodings could be expressed as different applications
of the lens-based approach we propose in our section 4.5.2.
