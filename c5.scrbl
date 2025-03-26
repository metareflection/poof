#lang scribble/acmart @manuscript @anonymous
@; Default already: @10pt @natbib @screen @nonacm @acmsmall
@; @anonymous @authordraft @authorversion @timestamp
@; Submitted to OOPSLA 2025 https://2025.splashcon.org/track/OOPSLA

@title{C5: The best of single and multiple inheritance}

@author[
  #:email (email "fare@mukn.com")
  #:affiliation (affiliation #:institution @institution{@emph{MUKN, Inc.}}
                             #:country "USA")
]{François-René Rideau}

@abstract{
We discuss how best to combine single inheritance and multiple inheritance
in an Object-Oriented (OO) language.
For that, we review the essential concepts of OO, what makes each kind of inheritance
desirable or not, and the relevant challenges with using, implementing or combining them.
Importantly, we discuss why our solution preserves what matters of single inheritance,
even though there are other aspects of single inheritance it doesn’t preserve.
We particularly study the notion of linearization of inheritance hierarchies,
and offer an algorithm to compute this linearization while combining both kinds of inheritance.
We call our algorithm C5, because it is a successor to
Barrett et al.’s C3 algorithm @~cite{Barrett96amonotonic}
that preserves additional constraints.
We compare our solution to that of other languages, most notably Common Lisp and Scala.
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

@(define (anonymize x . y) x)

@;@(define (nix . foo) (apply minted "nix" foo))
@(define (nix . foo) (apply verbatim foo))
@(define (~nocite . x) (let ((_ (apply @~cite x))) (void)))

@;; Suppress the page count for the Camera-ready version by uncommenting the below.
@;@tex{\thispagestyle{empty}\pagestyle{empty}}
@;; Instead, we could set the start page for the document with:
@;@pageStart{42}

@(define-bibtex-cite "poof.bib" ~cite citet generate-bibliography)

@(define-simple-macro (defsection name tag text) (define (name (x text)) (seclink tag x)))
@(defsection section1 "Prototypes_bottom_up" "section 1")

@pretitle{
@tex{
\acmYear{2025}
\copyrightyear{2025}
\acmConference[OOPLSA]{OOPSLA}{2025}{online}
\acmISBN{978-1-xxxx-xxxx-x/21/10}
\acmPrice{00.00}
\acmDOI{https://dx.doi.org/xx.xxxx/xxxxxxx.xxxxxxx}
\setcopyright{none}
}}

@section[#:tag "Intro"]{Introduction}

@subsection{Our contribution: An Optimal Inheritance Mechanism}

@subsubsection{Context: Inheritance}
Object-Oriented (“OO”) language are characterized by their use of
@emph{inheritance} @~cite{inheritance1996},
a mechanism to modularly combine partial specifications of programs.
Now language designers have faced a dilemma since almost the very beginning,
between several variants of inheritance:
some languages choose single inheritance for its simplicity and performance
@~cite{Simula1967 kay1996early},
whereas others prefer multiple inheritance
for its greater expressiveness and modularity @~cite{Bobrow1976}.
A few outliers use mixin inheritance,
that can be seen as a variant between the two @~cite{bracha1990mixin}.

@subsubsection{Problem: Optimal Inheritance?}
Some languages combine both single inheritance and multiple inheritance,
though with some constraints @~cite{cltl2 scalableComponentAbstractions2005}.
The question then is whether some ways of combining the two are better than others,
what makes these ways better, and is there a best way of combining them?
Importantly, multiple inheritance usually involves a computation called
the @emph{linearization} of the inheritance graph.
Is that linearization necessary?
Is there an optimal algorithm for this linearization
when combining single and multiple inheritance?

@subsubsection{Claim: C5 is Optimal}
We claim that (a) indeed there is a best way to combine single and multiple inheritance,
that (b) indeed it involves linearization of the inheritance graph,
that (c) there are enough constraints on linearization for the optimal algorithm
to be well-defined up to some heuristic, and
that (d) even then there are good reasons to prefer a specific heuristic.
We call C5 the resulting algorithm, that we implemented, and that
will be included in next release of @anonymize["our"]{Gerbil} Scheme
as part of its builtin object system.

@subsection{Plan of the Article}

In section 2, we will provide a quick summary of the issues at stake
with Object Orientation, and the three variants of Inheritance in common use:
what are the important concepts, and why they matter.

Along the way, we will define the terms we will use in the rest of the article,
especially since various authors from the communities around various OO languages
each use slightly different terminologies.
When multiple nomenclatures conflict, we will give precedence
to the terminology of the Lisp tradition, because it is
the oldest tradition that has been tackling those problems.@note{
Although it came first and directly or indirectly inspired
all OO languages and systems that followed,
SIMULA has many idiosyncrasies that set it and its successor BETA
apart from the wider OO tradition.
Also, neither tackled multiple inheritance,
making them less relevant to the issue at stake.
}

In section 3, we examine the known consistency constraints that matter
for linearization algorithms in the context of multiple inheritance,
and state of the art in satisfying them, the C3 algorithm.

In section 4, we describe the state of the art in combining
multiple and single inheritance,
with the solutions respectively adopted by Common Lisp @~cite{cltl2}
and Scala @~cite{scalableComponentAbstractions2005}.

In section 5, we propose a linearization algorithm we call C5,
that satisfies all the constraints we discussed for a good linearization algorithm,
and for combining single and multiple inheritance.
We explain why the residual heuristic we chose is arguably the best one,
though it goes contrary to the Lisp tradition.

@;{In section 6, we examine how our algorithm behaves on a few examples
lifted from relevant literature.}

Finally, in section 6 we conclude by recapitulating our findings.

@section{Object-Orientation and Inheritance}

@subsection{Object-Orientation}

@subsubsection{Intralinguistic Incremental Modular Specification}
Object-Orientation (“OO”) is a technique that enables the specification of programs
through incremental and modular @emph{partial} specifications,
embodied as entities @emph{within} a programming language, as opposed to outside it
(e.g. by editing, generating, or otherwise combining files with
preprocessors and metaprograms before to invoke the regular language implementation).

@subsubsection{Prototypes and Classes}
These in-language entities are called @emph{prototypes} if first-class
(manipulated at runtime, and specifying values, most usually records),
and @emph{classes} if second-class
(manipulated at compile-time only, and specifying types, most usually record types).
A language offers prototype-based object orientation (“Prototype OO”)
if it has prototypes, and class-based object orientation (“Class OO”) if it has classes.

The first OO language used classes @~cite{Simula1967},
but the second one used prototypes @~cite{Winograd1975},
and some provide both @~cite{kristensen1987beta}. @; BEWARE: I don’t grok BETA.
Class OO being the more popular form of OO,
we will use the terminology of Class OO in the rest of this article.
But all our discussion of inheritance applies just as well
to the more general Prototype OO.

@subsubsection{Objects and Conflation}
Also note that in Prototype OO, an “object” is the conflation
(cartesian product with implicit cast)
of a prototype and the instance value it specifies (via least fixed point).

Meanwhile in Class OO, an “object” is an element of the type specified by a class.
Meanwhile the class is a conflation of the partial specification
of a type (and accompanying algorithms), and the type it specifies as least fixed point.
Class OO can thus be viewed as Prototype OO for type descriptors,
which typically happens at compile-time in static languages@note{
Indeed, C++ templates at compile-time offer a pure functional programming language,
dynamically typed and lazy with pattern-matching, with builtin Prototype OO.
It is the same programming paradigm as Jsonnet or Nix, though for a very different domain target.
}
but may also happen runtime in dynamic languages, or in static languages using reflection.

Finally, it is also possible to use inheritance, and thus have “object orientation”,
without objects: by not conflating either prototype and instance or class and type,
and instead keeping them cleanly separate. @~cite{poof2021}

Lack of awareness of this conflation
can cause much confusion among students of OO.
So can, to a lesser extent,
ignorance of the relationship between prototypes and classes,
or of the distinct meanings of “object” in Prototype OO vs Class OO.

@subsubsection{Inheritance}
The way that those increments of specification, whether prototypes or classes,
are combined with other such increments of specification, is called
@emph{inheritance} @~cite{Winograd1975 inheritance1996}.
There are three kinds of inheritance in widespread use:
single-inheritance, multiple-inheritance, and mixin inheritance.

@subsection{Single Inheritance}

@subsubsection{Direct superclasses and subclasses}
In single inheritance, each class has a single direct @emph{superclass}
that it inherits from, of which it is a (direct) subclass,
to which it contributes its increment of specification.
We say that the superclass is more generic, and the subclass more specific.

@subsubsection{Global structure of single inheritance}
The transitive (direct and indirect) superclasses of a given class form a list.
The set of all classes has the structure of a tree with some base class at its root
(or forest, i.e. set of disjoint trees, if there is no common base class).

@subsubsection{Prefix}
In SIMULA 67 @~cite{Simula1967}, in which inheritance was first implemented though not thus called,
the superclass was called a @emph{prefix class},
as its specification was notionally a prefix to the complete specification
for the subclass,
taken literally as concatenating definitions of functions and variables
with scope going in text order, with the subclass’s more specific code added
at the end within the scope of the prefix class’s more generic code.

@subsubsection{Suffix}
Already in 1967 though, the “prefix” class could also add a suffix
to the body of the subclass, the code of which would be evaluated
where an @code{inner} placeholder was “splitting” prefix and suffix.
Moreover, SIMULA 67 called “prefix sequence” the list of
a class and its transitive superclasses, but kept it in most-specific-first order,
which is contravariant with the notion of prefix coming before the suffix.

@subsubsection{Method Resolution}
When multiple superclasses define a same method, the most specific definition is used.
Most languages allow the body of this definition to in turn invoke
the method defined by the next most specific definition,
via a keyword @code{super} (Smalltalk, Java) or some other similar mechanism.@note{
SIMULA and its successor BETA @~cite{kristensen1987beta}
were special in instead having a superclass’s body specify
where subclass bodies will be inserted, via the @code{inner} keyword.
BETA generalized classes to “patterns” that also covered method definitions the same way.
No other known language seems to use this technique,
although it is easily expressible using user-defined method combinations
in e.g. CLOS @~cite{bobrow88clos}.
}

@subsubsection{Simplicity}
Single inheritance is the simplest form of inheritance,
at least in the context of first-order code and logic,
which explains why it was discovered first.

@subsection{Multiple Inheritance}

@subsubsection{Early History}
Discovered a few years later, and initially just called @emph{inheritance},
in what in retrospect was prototype OO, in KRL @~cite{Winograd1975 Bobrow1976},
multiple inheritance allows a class to have multiple direct superclasses.
The notion of multiple inheritance thus predates Smalltalk 1976
implementing single inheritance as a compromise throwback to SIMULA 1967 @~cite{kay1996early},
thus making the name and notion popular.
Although some more early systems @~cite{Kahn1976 Borning1977 Traits}
used multiple inheritance, it is only with Flavors in 1979 @~cite{Cannon1980}
that it became really usable and somewhat popular.

@subsubsection{Global Structure of Multiple Inheritance}
The structure of a class and its transitive superclasses is
a Directed Acyclic Graph (“DAG”).
The set of all classes is also a DAG.
Most OO systems include a common system-wide base class
at the end of their DAG; but it is conceivable to do without one.

@subsubsection{Method Resolution in Multiple Inheritance}
When each of multiple superclasses define a same method,
the simple resolution strategy used by single inheritance doesn’t directly apply
because the inheritance DAG defines a partial order, not a total order,
so there may be several potentially “conflicting” method definitions to choose from.

Some languages @~cite{Borning1979 Traits stroustrup1989multiple CecilMultimethods}
issue an error in case of such method definition conflict;
programmers must resolve conflict by having relevant subclasses explicitly define a method override.
This is a consistent strategy, but the least useful among all consistent strategies:
@itemize[
@item{Overrides fail the incrementality purpose of OO whenever they require users
to reimplement part or all of the functionality from superclasses.}
@item{Overrides might want recursively call the methods of the class’s direct superclasses,
but that could lead to their transitively calling the method of a common indirect superclass multiple times,
and exponentially so as the inheritance DAG contains more such “diamond” configurations.}]
@; TODO cite Diamond Problem in C++ and learn about "virtual inheritance" and other C++ solutions.
@; C++ embraces exponential explosion unless you use virtual base classes.
@; your methods might also quickly check for multiple invocation and immediately return after the first time.
@; in the end you can try to layer as conventions the features the language doesn't directly offer.

Instead, we may realize that any solution that ensures each potentially applicable method
will be considered once and only once (or at most once)
in computing the @emph{effective method} (semantics of calling the named method)
will necessarily be establishing a total “linear” ordering between these methods.

@subsubsection{Class linearization}
With @emph{class linearization}, a total ordering, or linearization, is chosen
that extends the partial ordering defined by each class’s inheritance DAG,
and also preserves the @emph{local ordering} of each class’s declaration
of its direct superclasses.
The resulting @emph{class precedence list} @~cite{bobrow86commonloops},
traditionally kept in most-specific-first order
(starting with the class, ending with the base class),
is then used to resolve methods as if the class had been defined through
single inheritance with that list of classes as its “prefix sequence”.
This approach was introduced by Flavors @~cite{Cannon1980},
and named by New Flavors @~cite{Moon1986Flavors}.

@subsubsection{Method Combinations}
Another innovation of Flavors
was the notion of method combinations:
users can specify for each method name,
how the methods with that name defined in all of a class’s class precedence list
would be combined into an @emph{effective method} for that class.

The method combination can be the standard combination after linearization above
(enriched with @code{before after} methods,
that can notably be used to emulate the behavior of SIMULA);
or it can apply a simple monoidal operation on their result
(@code{+ max min or and list append nconc progn});
or it can be whatever the user specifies,
including an error-on-conflict strategy if desired.

Later, the introduction of builtin @code{around} methods
would also enable code wrappings not expressible
merely with before and after methods.
Furthermore, in New Flavors @~cite{Moon1986Flavors},
CommonLOOPS @~cite{bobrow86commonloops} and
CLOS @~cite{gabriel1991clos},
as inspired by T’s unification of functions and objects @~cite{Rees82t:a adams88oopscheme},
a “generic function” would embody the “calling a method of a given name”
and become the locus for this specification of a method combination.

Importantly for our discussion, the use of class-wide class precedence lists
ensures consistency of semantics across all classes, methods, method combinations,
effective methods, and generic functions.
Class precedence lists also offer a simple interface
for users defining their own method combinations.

@subsection{Comparison between single and multiple inheritance}

@subsubsection{Modularity}
Multiple inheritance is more modular than single inheritance,
allowing to divide program specifications
into more, smaller, more reusable classes, also commonly called “mixins” or “traits”
(the name “flavors” didn’t stick), such that each partial program specification
can be written with a smaller amount of information in the head of the programmer.

@subsubsection{Expressiveness}
Multiple inheritance is more expressive than single inheritance,
allowing partial specifications to be conceptualized
that would have previously required code duplication or roundabout protocols
that break modularity or incrementality.

@subsubsection{Performance}
Multiple inheritance is more expensive to implement than single-inheritance.
Notably, access to methods and slots with single inheritance can use
a same statically computed index for all subclasses of the class defining them.
By comparison, multiple inheritance in the most general case requires method and slot access
to lookup some kind of hash-table,
which, while still practically constant-time, is significantly more expensive.

A lot of work has been done to improve the performance of multiple inheritance,
through static method resolution when possible, @; TODO cite C++ ? sealing ?
and otherwise through caching @~cite{bobrow86commonloops}.
But these improvements introduce complexity, and caching
increases memory pressure and still incurs a small runtime overhead,
even when successful at avoiding the full cost of the general case.
For this reason, many performance-conscious programmers
will prefer to use or implement single inheritance when offered the choice.

@subsection{Mixin Inheritance}

@subsubsection{Last but not least}
Mixin inheritance was discovered last, in 1990 @~cite{bracha1990mixin},
while attempting to elucidate inheritance in the paradigm of programming language semantics
as opposed to the previously prevalent paradigm of computing systems @~cite{gabriel2012}.
It is actually the simplest form of inheritance
in the context of higher-order functions:
a @emph{practical} implementation literally takes but two short function definitions
@~cite{nix2015 poof2021}.

@subsubsection{Composing Mixins}
With mixin inheritance, classes (or “mixins”) are defined as
the composition of two or more superclasses
whose method definitions each override those of the superclasses it inherits from.
Since composition is monoidal, the semantics of the resulting class
is the same as if defined using single inheritance
from the precedence list obtained by flattening all composed superclasses in order.

@subsubsection{Comparative Expressiveness}
Mixin inheritance is more expressive than single inheritance, and
just as expressive as multiple inheritance, in that it enables
classes (mixins or traits) to be defined once without being tethered
to a single superclass (or chain of superclasses), and
combined and recombined in many compositions.
Conceptually, composition of mixins allows to @emph{append} lists of classes,
when single inheritance only allows to @emph{cons} a class to a fixed list.

@subsubsection{Comparative Modularity}
Mixin inheritance is less modular than multiple inheritance,
because it makes the programmer responsible for ensuring
there are no missing, repeated or misordered superclasses,
manually doing what multiple inheritance does for you
when computing its class precedence lists.
A notable bad situation is when the list of superclasses of a class is modified,
at which point all its transitive subclasses must be updated accordingly,
even if defined in completely different modules
that the author has no idea exists and no access to.
This makes changes brittle, breaks modularity, and
effectively forces the entire inheritance DAG of a class to become part of its interface.
By contrast, multiple inheritance can automate all these troubles away,
and let programmers only have to worry about their own classes’s direct superclasses.

@subsubsection{Popularity}
Both due to having been discovered later and being less modular,
mixin inheritance is less popular than the older alternatives.
Nevertheless it lives on notably in Racket @~cite{Mixins1998 Flatt06schemewith},
Newspeak @~cite{bracha2008newspeak}, GCL @~cite{gclviewer2008}, Jsonnet @~cite{jsonnet},
and Nix @~cite{nix2015}.
Just the use of GCL at Google means a large part of the world computing infrastructure
is built upon configurations written using mixin inheritance.

@subsubsection{No Further Comment}
Mixin inheritance definitely has its uses, if only as
a lower-level building block used in implementing more elaborate object systems.
Nevertheless, in the rest of this document, we will dismiss mixin inheritance
for being a less modular and less performant alternative
to the combination of multiple inheritance and single inheritance we are seeking.

@section{Constraints on Linearization}

@subsection{Consistency Matters}

@subsubsection{Consistency Constraints}
Cannon @~cite{Cannon1980}, Moon @~cite{Moon1986Flavors} then
Ducournau et al. @~cite{ducournau1992monotonic ProposalMonotonicMultipleInheritance1994}
have discussed the good consistency properties that one may (or may not) expect
from a class linearization algorithm.

These properties ensure that an object system shall compute a class’s class precedence list
in a way that provides a consistent ordering of all methods
across all generic functions and all classes — and all tuples of classes,
when using multimethods @~cite{bobrow86commonloops bobrow88clos gabriel1991clos CecilMultimethods allen2011type}.

@subsubsection{Matching Methods}
This consistency notably matters when resources or locks
are acquired by some methods of some objects,
that must be updated in matching order,
or released in matching reverse order,
by the same methods or other methods,
in the same objects or other related objects.
Inconsistency can lead to resource leak, use-before-initialization, use-after-free, deadlock,
data corruption, and other catastrophic failures.

@subsection{Ordering Consistency}

@subsubsection{Linearization}
The most important constraint, @emph{linearization} @~cite{Cannon1980},
states that the precedence list of a class
is a linearization (total ordering extension)
of its inheritance DAG (viewed as a partial ordering).
All languages that use linearization for method resolution follow this constraint;
those that don’t fall short on the OO purposes of incrementality and modularity.

@subsubsection{Local Ordering}
The second constraint, the
(preservation of the) @emph{local precedence order} @~cite{Moon1986Flavors},
states that the list of a class’s direct super classes
(as specified by the programmer)
is a sublist of its precedence list
(seen as an ordering, e.g. all elements are present in the same order,
but not necessarily consecutively).
Many languages fail to follow this constraint.

@subsubsection{Monotonicity}
The third constraint, @emph{monotonicity} @~cite{ducournau1992monotonic},
states that a class’s precedence list is included as a sublist
(seen as an ordering, as above) in the class precedence list of each of its subclasses.

@subsection{Shape Determinism}

@subsubsection{Only Shape Matters}
A fourth constraint, that we will call @emph{Shape Determinism},
states that the result of the linearization algorithm
must only depend on the @emph{shape} of the inheritance graph,
and may not depend on the names of the classes or any global information.
Thus, class inheritance graphs that are isomorphic up to some renaming of classes
must have identical linearizations up to the same renaming of classes.

@subsubsection{Original Name}
This constraint was called @emph{acceptability} by Ducournau et al.
who introduced it @~cite{ducournau1992monotonic}.
However, this name is too generic and fails to convey
either the intent or content of the constraint.
We considered the name “stability”, but Ducournau et al. use that name
for another property of multiple inheritance
that is subsumed by linearization.

@subsubsection{Rationale}
Shape Determinism matters for code maintainability:
thanks to it, renaming a class, moving it in the file hierarchy,
or otherwise modifying it without changing the shape of the inheritance hierarchy
shall not introduce unstable semantic change in the program,
especially if such a change is itself somehow necessary as part of debugging.

@subsubsection{Alternatives to Shape Determinism}
Instead of Shape Determinism, a global ordering could be established between
all defined classes across a program,
e.g. lexicographically by their name or full path,
or by assigning a number in some traversal order,
or from a hash of their names or definitions, etc.
This ordering could then be used by a linearization algorithm
as a tie-breaking heuristic to choose which superclass to pick next
while computing a precedence list,
whenever the constraints otherwise allow multiple solutions.
But the instability of such a heuristic when the code changes
would lead to many @emph{heisenbugs}.

@subsection{Constraint-Respecting Algorithms}

@subsubsection{Inconsistent Algorithms}
While all OO languages with linearization respect the first constraint;
most fail some or all of the latter constraints.

@subsubsection{First Solution}
The first proposed solution that satisfies all the above constraints
@~cite{ProposalMonotonicMultipleInheritance1994} was a bit complex,
building upon previous partial solutions.

@subsubsection{C3}
A latter solution, the C3 algorithm @~cite{Barrett96amonotonic},
“simply” follows the constraints,
computing the precedence list head-first with a tie breaking heuristic used
when multiple linearizations are compatible with the constraints.

@subsubsection{Head-Bias Heuristic}
The heuristic followed by C3 is to build the precedence list
from the front (most specific superclass),
each time choosing the next element among the possible candidates
by picking the one that appears earliest in the list of precedence lists
followed by the direct superclass list.

This Head-Bias Heuristic seems consistent with that of Flavors,
that similarly places a class
“as close to the beginning of the ordering as possible,
while still obeying the other rules” @~cite{Moon1986Flavors}.
The other rules of Flavors are to enforce Linearization and Local Precedence Order,
though also, implicitly, Shape Determinism.
The heuristic is also present in the LOOPS algorithm @~cite{ducournau1992monotonic}.

@subsubsection{Naming}
C3 was named after the fact that it respects three ordering constraints it purports to enforce,
citing Ducournau et al. @~cite{ducournau1992monotonic ProposalMonotonicMultipleInheritance1994}.
The authors did not count Shape Determinism among these constraints,
though, implicitly, C3 enforces it.
There are thus are four constraints enforced by C3,
just like there are four musketeers in The Three Musketeers @~cite{Dumas1844}.

@subsubsection{Adoption}
C3 has since been adopted by Dylan, Python, Raku, Parrot, Solidity, PGF/TikZ, and more.

@section{State of the Art in Combining Single and Multiple Inheritance}

@subsection{Prolegomena}

@subsubsection{Previous Art}
Many languages adopted single inheritance for its performance advantages,
yet were later extended with multiple inheritance for its expressiveness,
while trying to preserve the advantages of single inheritance where appropriate.

We will examine the cases of Common Lisp (extending earlier Lisp systems)
and Scala (extending Java’s class system).

@subsubsection{Terminology}
Beware that the word “class” weakly implies multiple inheritance in the Lisp tradition,
where it contrasts with “struct” that strongly implies single inheritance.

By contrast, “class” strong implies single inheritance
in the Smalltalk, Java and Scala tradition,
where it contrasts with “trait” for multiple inheritance.

To confuse things further, in C++ tradition,
a @code{struct} is just a way to define a class
wherein all members (methods and variables) are public by default,
which has nothing to do with either single or multiple inheritance.
C++ always has multiple inheritance, although
superclasses reached along many paths are duplicated unless declared “virtual”.

This document follows the Lisp tradition in its terminology,
except in the section on Scala below where we will use Scala terminology.

@subsection{Common Lisp}

@subsubsection{Separate Class and Struct Hierarchies}
Common Lisp @~cite{cltl2} has @emph{structs},
that sport single inheritance and are quite efficient,
as well as @emph{classes} @~cite{gabriel1991clos},
that sport multiple inheritance and are more expressive but slower.

However, to avoid inconsistencies, Common Lisp wholly prevents
a class from inheriting from a struct, or vice versa,
keeping the two hierarchies separate,
though offering a uniform interface to both.

@subsubsection{User-defined Hierarchies}
Through its MOP @~cite{AMOP},
Common Lisp also enables users to define metaclasses
wherein users can define their own class hierarchies,
that could conceivably combine single and multiple inheritance.
We are not aware of anyone using this mechanism to do so;
if someone did, it is unclear whether the mechanisms provided
would allow them implement the usual performance optimizations
allowed on its single inheritance fragment.

@subsection{Scala}

@subsubsection{Traits}
Scala extends Java’s classes that only support single inheritance
with traits @~cite{scalableComponentAbstractions2005}
that support multiple inheritance.

Scala “classes” and “traits” definitions may specify
at most one direct superclass and potentially many direct supertraits
that it “extends”.
Syntactically, they are specified in most-generic-first order,
which is the reverse of the local precedence order.
But semantically, the Scala specification still discusses
class precedence lists in the traditional most-specific-first order.

@subsubsection{Scala superclasses}
Scala 2.13 requires that if a trait or class has a (single inheritance) superclass
in its inheritance hierarchy, then it shall declare as the first entity it extends
a (single inheritance) class that is more specific
than any of its indirect (single inheritance) superclasses.

Scala 3.3 relaxes this restriction by automatically computing
this most specific superclass from all the superclasses in its inheritance hierarchy,
so you can declare directly extending a superclass of it, or only traits.

We have been unable to find a documentation of this feature in Scala 3,
its precise behavior, design rationale, and implementation timeline,
even after contacting members of the Scala team.
However, this is clearly the Right Thing to do in this case,
as we’ll explain when discussing our solution.

@subsubsection{Scala linearization}
Scala uses a variant of the original LOOPS linearization algorithm @~cite{ducournau1992monotonic}:
The LOOPS algorithm simply concatenates all the class precedence lists
of a class’s direct superclasses, then removes all duplicates,
keeping the @emph{first} one (in most specific order) and removing latter copies,
to follow the Head-Bias Heuristic of Flavors.
The Scala algorithm does as much but keeps the @emph{last} duplicate instead,
following an opposite heuristic.

Note that neither the LOOPS algorithm nor its Scala variant preserves
local precedence order, unlike more elaborate algorithms
adopted by New Flavors @~cite{Moon1986Flavors},
CommonLOOPS @~cite{bobrow86commonloops} and their successors;
and neither preserves monotonicity.

Interestingly, the Scala variant unlike the LOOPS original @emph{does} preserve
the single-inheritance suffix of the precedence list, within the assumption that
the most specific superclass appears last in the direct superclass and supertrait list.
This assumption is enforced syntactically by Scala 2.13, and semantically by Scala 3.3.
This importantly allows the combination of traits and classes to work in Scala,
while keeping the algorithm simple.

We suspect the switch in heuristic was specifically designed
to make traits work with classes while keeping a simple algorithm.
But we failed to get confirmation after contacting the authors.

@section{Our C5 Algorithm}

@subsection{Best Combining Single and Multiple Inheritance}

@subsubsection{Unifying Classes and Structs}
In modernizing the builtin object system of
@anonymize["our Scheme implementation"]{Gerbil Scheme},
we decided to unify hierarchies
of single inheritance structs and multiple inheritance classes,
that were theretofore separate, as in Common Lisp.
In doing so, we identified a maximally expressive way to combine them.

@subsubsection{Adding a Fifth Constraint}
We had recently adopted the C3 algorithm for class linearization,
and its four constraints.
We decided to minimally complement it with an additional fifth constraint,
necessary and sufficient to support integration of single inheritance @emph{structs}
into multiple inheritance precedence lists.

@subsection{The Struct Suffix Constraint}

@subsubsection{Constraint}
@bold{The precedence list of a struct is a suffix of
the precedence list of all its subclasses.}

@subsubsection{Why the Suffix Constraint}
The above suffix constraint is precisely
the semantic prerequisite for the efficiency optimizations
enabled by single-inheritance:
access to methods can use a same statically computed index
for all subclasses of a class.

@subsubsection{Special Treatment of Struct Suffix}
Our algorithm enforces the Struct Suffix Constraint
by treating each class’s most specific struct superclass specially:
First, determine this most specific struct superclass,
and include its class precedence list as the tail of the one being computed.

Issue an error if multiple incompatible structs are used
where neither is a superclass of the other.
Of the remaining classes not in this precedence list, none is a struct;
apply the regular class linearization algorithm on those, based on C3.
Prepend the result to the struct suffix.

@subsection{Best Heuristic: Tail-Bias}

@subsubsection{Inverting the Head-Bias Heuristic}
Though our C5 algorithm is based on C3, we invert the Head-Bias Heuristic
that C3 inherited from Flavors and LOOPS,
and instead adopt the opposite Tail-Bias Heuristic, like Scala.

@subsubsection{Why Tail-Bias?}

The Tail-Bias Heuristic maximizes sharing of suffixes
between the precedence lists of classes and their subclasses or superclasses,
thereby maximizing the reuse of method and slot indexes,
even in the absence of explicit declaration of struct classes.
Tail-Bias therefore offers better performance than Head-Bias and other heuristics.

@subsection{Advantages of C5}

@subsubsection{Struct declarations optional}
One advantage of the Tail-Bias Heuristic is that
it will preserve the tail of a struct’s precedence list
just by virtue of following as a convention the Scala 2 discipline
of putting your most-specific struct at the end of your local precedence order
(which in most Lisp object systems is a specified syntactically at the end,
unlike in Scala).
This discipline works even if when are no system-supported struct declarations.

@subsubsection{Common Tail Discovery}
More than that, the Tail-Bias Heuristic maximizes
sharing of class precedence list tails among classes within a hierarchy.
This sharing in turn increases the effectiveness of caches
for partial computations of effective methods
if any such caches exist for performance purposes.
This tail-sharing also increases the chances that method and slot indexes
are shared between classes, maximizing code sharing and increasing performance
for slot access almost as fast as in single inheritance,
even without explicit struct declaration.

@subsubsection{Coherent Naming}
Our C5 algorithm correctly counts the number of constraints it enforces
(five, plus a heuristic),
in addition to acknowledging its being a direct successor to C3
(that enforces four constraints plus a heuristic).

@subsection{Single-Inheritance Yet Not Quite}

@subsubsection{Single-Inheritance among Structs}
When following the @emph{struct suffix} constraint,
the superstructs of a struct, i.e. the subset of its superclasses that are structs,
are totally ordered by @emph{transitive} inheritance:
the partial order induced by the inheritance DAG on the struct subset is total,
i.e. it is a list, and the inheritance graph of all structs is a tree
if there is a common base struct, or a forest otherwise.

@subsubsection{No Single-Inheritance among Structs plus Classes}
Yet the larger set of superclasses of a struct,
including its direct and indirect non-struct classes,
is a DAG that is not usually totally ordered.
In @emph{that} sense, the inheritance hierarchy of structs is @emph{not} single-inheritance;
a struct’s superclasses are not a list, the hierarchy of all structs is neither tree nor forest.

And that is actually a good feature: it means that structs can inherit from classes,
and benefit from the expressiveness and modularity of those classes in their definition.

@subsubsection{Preserving the Property that Matters}
Thus when “combining single and multiple inheritance”, it is not exactly “single inheritance”
that we preserve, but the more important struct suffix property.
Not the syntactic constraint on building a class,
but the semantic constraint on the invariants its subclasses will respect,
that themselves enable various optimizations.

It may be a surprising conclusion to the debate between proponents of multiple inheritance
and of single inheritance that in the end,
single inheritance kind of mattered, but it was not exactly single inheritance that mattered.
The debate was not framed properly, and a suitable reframing solves the problem
hopefully to everyone’s satisfaction.

@;{
@section{Inheritance Examples}
@subsection{Example 1}

@(noindent) @image[#:scale 0.67]{C3_linearization_example.eps}

In this example lifted from Wikipedia @~cite{WikiC3},
we define a base class @code{O},
classes @code{A B C D E} that each inherit only from @code{O},
classes @code{K1} with direct superclasses @code{A B C},
@code{K2} with @code{D B E},
@code{K3} with @code{D A}, and @code{Z} with @code{K1 K2 K3}.
Using the C3 algorithm, we get the precedence list @code{Z K1 K2 K3 D A B C E O},
with each subclass having its subset of superclasses in the same order
in its own precedence list.
@; C5, with its Tail-Bias Heuristic, yields @code{Z K1 K2 K3 D A B E C O},

Now, if @code{C} were declared a struct, then the suffix @code{C O} must be preserved,
and the precedence list would be changed to @code{Z K1 K2 K3 D A B E C O}.

If both @code{C} and @code{E} were declared structs, then there would be a conflict
between the suffixes @code{C O} and @code{E O}, and
the definition of @code{Z} would fail with an error.

Only @code{O}, one of @code{C E}, and @code{Z}, may be declared struct without causing an error
due to violation of the local precedence order.
Indeed, a class may not be declared a struct if it appears in a direct superclass list
before a class that is not one of its superclasses.
However, this criterion is not necessary to prohibit struct-ability,
and @code{K3} cannot be a struct either,
because its superclass @code{D} appears before @code{B E} among the direct superclasses of @code{K2},
which would break the struct suffix of @code{K3} when @code{Z} inherits from both @code{K2} and @code{K3}.

@subsection{Example 2}

@(noindent) @image[#:scale 0.066]{C3_linearization_example_2.png}
@;{
https://www.mermaidchart.com/app/projects/0d7dd2c2-0762-428e-a4be-2063fd491789/diagrams/26327f0d-1e07-4b9a-ba0e-94557d2ceac1/version/v0.1/edit
---
config:
  theme: mc
  look: classic
---
flowchart BT
    B("Boat")
    C("DayBoat")
    D("WheelBoat")
    E("EngineLess")
    F("SmallMultiHull")
    G("PedalWheelBoat")
    H("SmallCatamaran")
    I("Pedalo")
    I --> G --> E --> C --> B
    I --> H --> F --> C
    G --> D --> B
}

In this example from Ducournau et al. @~cite{ProposalMonotonicMultipleInheritance1994},
we have the class and direct superclass lists:
@code{Boat},
@code{DayBoat Boat},
@code{DayBoat WheelBoat},
@code{EngineLess DayBoat},
@code{PedalWheelBoat EngineLess WheelBoat},
@code{SmallMultihull DayBoat},
@code{SmallCatamaran SmallMultihull},
@code{Pedalo PedalWheelBoat SmallCatamaran}.

The C3 and C5 algorithms both compute the following precedence list for this class hierarchy:

@code{Pedalo PedalWheelBoat EngineLess SmallCatamaran SmallMultihull DayBoat WheelBoat Boat}.

Due to precedence constraints, any of @code{Pedalo Boat}, and
at most one of @code{WheelBoat DayBoat} could be declared a struct,
with @code{DayBoat} being the only one to change the precedence list, to:

@code{Pedalo PedalWheelBoat EngineLess SmallCatamaran SmallMultihull WheelBoat DayBoat Boat}.

Interestingly, either @code{WheelBoat} or @code{DayBoat} can be made a struct,
because they don’t appear directly in a same class’s direct-superclass list,
so there is no local precedence order constraint between the two.

If there were no @code{EngineLess} between @code{PedalWheelBoat} and @code{DayBoat},
then @code{DayBoat} appearing before @code{WheelBoat} would prevent the former
from being made a struct, with the definition of @code{PedalWheelBoat} triggering an error.

A general solution that can be used to ensure @code{DayBoat} is a struct
would be to swap the order of superclasses in the conflicting definition;
when the methods defined or overridden by the swapped superclasses are disjoint,
the swap will not otherwise change the semantics;
otherwise, the subclass can suitably override methods to compensate for the change.
And the other general solution in last resort is to introduce a do-nothing wrapper class
to shield a superclass from a local local precedence order constraint,
just like the @code{EngineLess} shields @code{DayBoat}.
}

@section{Conclusion: Best of Both Worlds}
@subsection{Findings}
@subsubsection{Restating the Obvious}
Our presentation of Object-Orientation and Inheritance
only included what should have been obvious and well-known lore by now.
Yet so far as our bibliographical search could identify,
a lot of it seems to be
unstated in academic literature,
or implicitly assumed by ones and blatantly ignored by others,
or once mentioned in an otherwise obscure uncited paper
— and overall largely acted against in practice
by most language designers, implementers and users.

Without claiming originality in that part of this article, we would like to insist on:
@itemize[
@item{The relationship between OO, modularity and incrementality.}
@item{The relationship between prototypes, classes, objects and conflation.}
@item{The comparative advantages and downsides of single, multiple and mixin inheritance.}
@item{Why linearization beats manual conflict resolution.}
@item{The importance of well-documented yet oft-ignored consistency constraints on linearization.}]

@subsubsection{Struct Suffix}
We identified the @emph{struct suffix} constraint as
the one semantic constraint necessary and sufficient
to achieve in the optimizations associated with single inheritance,
even in the context of multiple inheritance.
The constraint was implicitly enforced by Scala,
but does not seem to have been identified and made explicit in any publication yet.

@subsubsection{Tail-Bias Heuristic}
We identified why the age-old Head-Bias Heuristic (our name) dating back to Flavors
is actually suboptimal, and why its opposite the Tail-Bias Heuristic,
once again used by Scala without much explanation,
helps with optimizations based on the tail of the precedence list.
This reverses a 46 year old tradition.

@subsubsection{C5 Algorithm}
We implemented a new C5 Algorithm that combines all the above features.
While each of these features may have been implemented separately in the past,
ours seems to be the first implementation to combine them.

@subsection{Implementation}
@subsubsection{Our Scheme}
We have implemented the C5 algorithm
in our open-source dialect of the Scheme programming language, @anonymize[""]{Gerbil Scheme,}
and it will be available in the next release@anonymize[""]{ 0.18.3}.

Our users can enjoy the benefits, as our language can legitimately claim
to have the single Best Inheritance mechanism of them all.
At least until other language implementers copy our language.

@subsubsection{Code Size}
The C5 algorithm itself is about 200 lines of code with lots of explanatory comments.

The entire object system is about 1400 lines of commented code for its runtime,
including all runtime optimizations enabled by single inheritance where appropriate.

The compile-time and macro-expansion-time support for the object system are harder to account for,
not being isolated in their own files but spliced all over.
We estimate they total between one and two thousand lines of code,
wherein the entire compiler and the language prelude each total a bit over 8000 lines of code.
However, they are also the parts of our system that implementers of other languages
will least care to reuse.

Overall, the complexity of our implementation is quite low, and
it shouldn’t be too much effort for a dedicated language implementer
to port our technology to their language.

@subsubsection{Open Source}
We invite all language implementers to likewise adopt C5
in the next version of their object system,
whether class-based or prototype-based, static or dynamic, etc.
Then your users too can enjoy the Best Inheritance in the World.

@section{Data-Availability Statement} @appendix

Our code is available in our github repository as part of our Scheme implementation.
We will reveal the address after deanonymization.

For the sake of artifact review, we will build an anonymized implementation
of the C5 algorithm isolated from the rest of our object system.
We will include a few execution test cases.
We will not attempt to anonymize a complete variant of our object system,
which would be overly costly and would easily fail to be anonymous.

The algorithm description we give above
should already be sufficient for any person skilled in the art
to reimplement and adapt the C5 algorithm to their own object system.
Furthermore, the artifact we provide will only allow a language implementer
to compare their implementation to ours and check for any bugs in their reimplementation.

@(generate-bibliography)
