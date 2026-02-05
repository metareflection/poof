#lang scribble/base
@; -*- Scheme -*-
@(require "util/ltuo_lib.rkt")
@(set-chapter-number 4)

@title[#:tag "OOaIEM"]{OO as Internal Extensible Modularity}
@section[#:tag "M"]{Modularity}
@subsection{Division of Labor}
@epigraph{The benefits expected of modular programming are:
(1) managerial—development time should be shortened because separate groups
would work on each module with little need for communication;
(2) product flexibility—it should be possible to make drastic changes to one module
without a need to change others;
(3) comprehensibility—it should be possible to study the system one module at a time.
  @|#:- "David Parnas"|
}
Modularity@~cite{Parnas1972 Dennis1975} is the organization of software source code
in order to support division of labor, dividing it into “modules” that can each be
understood and worked on mostly independently from other modules,
by one or multiple developers over time.
The “interface” of each module defines a semi-formal contract between the users and implementers
of the module, such that users need only know and understand the interface to enjoy the functionality
of the module, whereas the implementers have freedom to build and modify the module internals
as long as they satisfy the interface.


@subsection{First- to Fourth-class, Internal or External}
@epigraph{I object to doing things that computers can do.
  @|#:- "Olin Shivers"|
}
A few languages offer a builtin notion of modules as @emph{first-class} entities:
@principle{Entities are first-class if they can be manipulated as values at runtime}.
But popular modern programming languages usually only offer
@emph{some} builtin notion of modules as @emph{second-class} entities:
@principle{Entities are second-class if they exist at compile-time
but are not available as regular runtime values}@xnote["."]{
  In between the two, some languages offer a “reflection” API that gives some often limited
  runtime access to representations of the module entities.
  This API is often limited to introspection only or mostly;
  for instance, it won’t normally let you call the compiler
  to define new modules or the linker to load them.
  Yet some languages support APIs to dynamically evaluate code,
  that can be used to define new modules;
  and some clever hackers find ways to call a compiler and a dynamic linker,
  even in languages that don’t otherwise provide support APIs for it. @; TODO cite Goo
}
Either first-class or second-class entities are considered @emph{internal} to the language,
part of its semantics, handled by its processors (compiler, interpreter, typesystem, etc.).

However, many languages offer no such internal notion of modules.
Indeed modules are a complex and costly feature to design and implement,
and few language designers and implementers will expend the necessary efforts toward it
at the start of their language’s development;
only the few that have success and see their codebase grow
in size and complexity will generally bother to add a module system@xnote["."]{
  Unless they develop their language within an existing modular framework
  for language-oriented programming, such as Racket,
  @;TODO{cite. Also Stratego http://strategoxt.org/ ? Pypy https://pypy.org/ ?}
  from which they inherit the module system.
}

Now modularity is foremost a @emph{meta-linguistic} concept:
Even in a language that provides no support whatsoever for modules
@emph{within} the language itself (such as C),
programmers will find means to express modules as @emph{third-class} entities:
@principle{Entities are third-class if they are automated by tools
outside the language process itself}.
Those tools, known as metaprograms, include (but are not limited to)
preprocessors, object file linkers, editor macros, “wizards” or LLMs.
And even if programmers somehow don’t use such automation,
because they can’t or won’t afford to build or acquire or lease it,
developers may achieve modules as @emph{fourth-class} entities:
@principle{Entities are fourth-class if they are notions in the programmer’s head,
manually translated into tool usage and program modifications}.
Fourth-class entities include design patterns, as translated into code using
editors, copy-paste, and lots of debugging.
Either third-class or fourth-class entities are considered @emph{external} to the language,
not part of its semantics, not handled by its processors,
yet conceptually present in the minds of the programmers@xnote["."]{
  Whereas the terms “first-class” and “second-class” are well-established in the literature,
  @;{TODO cite}
  I am hereby introducing the terms “third-class” and “fourth-class”,
  as well as the distinction between “internal” and “external” entities.
  Of course, depending on where you draw the line for “the language”,
  the very same entity processed by the very same tools may shift between these classes:
  thus, in C++ classes are second-class entities;
  but if the language being considered is C, and C++ is seen as an external metalanguage
  (as was the case in the original implementation @c{cfront} of C++,
  a preprocessor that generated C code),
  then the very same classes are third-class entities for C;
  and if @c{cfront} were updated to support templates, classes would be first-class entities
  for the compile-time C++ template language;
  and if done by hand as a set of conventions on top of C, they would be fourth-class entities.
}
Thus, programmers will:
@itemize[
@item{copy and paste sections of code, or prefix identifiers, as poor man’s modules;}
@item{preprocess files to concatenate, transform and generate code fragments;}
@item{transclude “include” files to serve as libraries or interfaces to later-linked libraries;}
@item{divide code in files they independently compile then “link” or “load” together;}
@item{orchestrate incremental (re)building of software with utilities such as “make”;}
@item{bundle software into “packages” they exchange and distribute with “package managers”;}
@item{“containerize” consistent package installations into “images”.}]

When for the sake of “simplicity”, “elegance”, or ease of development or maintenance,
support for modularity is left out of a language,
or of an ecosystem around one or multiple languages,
this language or ecosystem becomes but
the weak kernel of a haphazard collection of tools and design patterns cobbled together
to palliate this lack of internal modularity with external modularity.
The result inevitably ends up growing increasingly
complex, ugly, and hard to use, develop and maintain.

@subsection[#:tag "CfM"]{Criterion for Modularity}
@principle{A design is more modular if it enables developers to cooperate more while coordinating less}
compared to alternative designs that enable less cooperation or require more coordination,
given some goals for developers, a space of changes they may be expected to enact in the future, etc.
More ability to code one’s part, while requiring less knowledge about other people’s parts@xnote["."]{
  Note how, in the great ledger of software development costs and benefits,
  coordination counts negatively, in the column of costs:
  coordination, whether by one-on-one communication, meetings, emails, documents, chat rooms,
  code comments, edicts principled or whimsical, reports, processes, etc.,
  all take their toll on the developers’ resources.
  Hopefully, the coordination methods used come not only with benefits greater than the costs,
  but also with a greater profit (difference between benefits and costs) than
  possible alternative methods considering the total capital immobilized in the development process.
  However there is nothing automatic to that, contrary to the apparent belief of many a manager.
  If the benefits of active business practices in general
  tend to be commensurate with and superior to their costs,
  that is only the precarious result of market forces,
  through the feedback of businesses persisting in their bad practices losing resources
  and eventually stopping activity. Selection bias: we only observe the businesses still alive.
  But these market forces do not happen magically outside of human activity, or outside yourself:
  your actions partake in those market forces, and so will your going bankrupt
  if you persist in pursuing counter-productive practices.
}

For instance, the object-oriented design of the Common Lisp build system
ASDF@~cite{ASDF2 ASDF3}
made it simple to configure, to extend, and
to refactor to use algorithms in @emph{O(n)} rather than @emph{O(n³)} or worse,
all without any of the clients having to change their code.
This makes it arguably more modular than its predecessor MK-DEFSYSTEM@~cite{kantrowitz1991}
that shunned the use of objects (possibly for portability reasons at the time),
was notably hard to configure, and resisted several attempts to extend or refactor it.

Note that while external modularity enables cooperation during development,
internal modularity also enables cooperation during deployment,
with conditional selection and reconfiguration of modules,
gradual deployment of features, user-defined composition of modules,
automated A/B testing, and more.

For a more complete theory of Modularity, see @citet{ngnghm9}.

@subsection{Historical Modularity Breakthroughs}

Programmers these days are very aware of files, and file hierarchies,
as units of modularity they have to think about quite often,
naming, opening, visiting and closing them in their editors,
manipulating them in their source control, etc.
Although files as such embody a form of modularity external
to most simple programming languages that they use,
the more advanced programming languages, that they use most often,
tend to have some notion of those files,
runtime and/or compile-time representation for those files,
and often some correspondence between file names and names of internal module entities
that result from compiling those files.
In other words, for most languages that matter,
files embody modularity internal to programming languages@xnote["."]{
  Note that while a file may be a unit of modularity, this is not always the case:
  often, actual modules span multiple files;
  at other times, there may be multiple modules in a single file.
  For instance, compiling a C program typically requires each file to be registered
  into some “build” file (e.g. a @c{Makefile}) to be considered as part of the software;
  a C library also comes with an accompanying “header” file, though there need not be
  a 1-to-1 correspondence between C source files and C header files
  (as contrasted with source and interface files in OCaml).
  A C “module” therefore typically spans multiple files,
  and some files (headers and build files) can have bits from multiple modules.
  Of course, having to manually ensure consistency of information between multiple files
  makes the setup less modular than a language that doesn’t require as much maintenance.
  Then again, the C language also recently adopted some notion of namespace,
  which, though it doesn’t seem used that much in practice,
  can constitute yet another notion of modules, several of which can be present in a file.
  At a bigger scale, groups of files in one or multiple directories may constitute a library,
  and a “package” in a software “distribution” may include one or several libraries, etc.
  Thus, even within a single language, there can be many notions of modularity
  at several different scales of software construction,
  enabling division of labor across time for a single person, for a small team, for a large team,
  between several teams, etc., each time with different correspondences
  between modules and files, that are never quite 1-to-1:
  even “just a file” is not merely a blob of data, but also the meta-data of
  a filename registered in a broader namespace, and some intended usage.
}

Now, while files go back to the 1950s and hierarchical filesystems to the 1960s,
@; First filesystem??? IBM disk???
@; Semi-Hierarchical: ERMA Mark 1 1958 https://dl.acm.org/doi/10.1145/1458043.1458058 - but 100 fixed categories
@; Hierarchical: Multics 1965 https://www.multicians.org/fjcc4.html OS/360, Burroughs B5000
there was an even earlier breakthrough in modularity,
so pervasive that programmers use it without even thinking about using it:
subroutines,
already conceptualized by Ada Lovelace as “operations”@~cite{Lovelace1843}, @;
formalized early on by Wilkes et al. in the 1940s @; TODO cite
used by the earliest programmers in manually assembled code,
part of the first programming language Plankalkül, @; TODO cite
and of FORTRAN II. @; TODO cite
Subroutines allow programmers to divide bigger problems into smaller ones
at a fine grain, that can each be solved by a different programmer,
while other programmers only have to know its calling convention.

No less important than subroutines as such yet also taken for granted
was the concept of a return address allowing
calls to a single copy of a subroutine from multiple sites,
@; TODO CITE Turing 1946 report on ACE; implemented by Wilkes
and later of a call stack of such return addresses and other caller and callee data.
The call stack enabled reentrancy of subroutines,
such that a subroutine can be recursively called by a subroutine it itself called;
@; that only became common after LISP and ALGOL in the late 1950s.
@; TODO cite McCarthy LISP 1957, Dijkstra ALGOL 1958
thus, programmers do not have to care that their subroutines
should not be reentered while being executed
due to a direct or indirect subroutine call.
Indeed, subroutines can now be reentered, not just accidentally, but intentionally,
to express simpler recursive solutions to problems.
The ability to cooperate more with less coordination
with programmers from other parts of the software:
that’s the very definition of modularity.

Sadly, the generalization of call stacks to first-class continuations in the 1970s,
@; TODO cite Steve Russell 1960, van Wijngaarden 1964, Landin 1964, Strachey 1974, Steele 1978
and later to delimited control in 1988 and beyond @; TODO cite Felleisen 1988
is still not available @; TODO cite
in most programming languages, eschewing another advance in modularity,
wherein programmers could otherwise abstract over the execution of some fragment of code
without having to worry about transformations to their control structure,
e.g. for the sake of non-deterministic search, @; TODO cite
robust replaying of code in case of failure, @; TODO cite
security checks of code behavior, @; TODO cite
dynamic discovery and management of side-effects, etc. @;{TODO cite}

On a different dimension,
separately compiled object files, as implemented by FORTRAN (1956), @; TODO cite
provided third-class modularity through an external linker.
Later developments like
typechecked second-class Modules à la Modula-2 (1978), @; TODO cite Wirth;
@; TODO also look into Liskov’s CLU, Mary Shaw’s Alphard, etc.
Higher-Order Modules à la ML (1985), @; TODO cite David MacQueen 1985
typeclasses à la Haskell (or traits in Rust),
interfaces à la Java, and much more,
@; TODO cite
provided second-class modularity as part of a language itself.
Finally, objects in Smalltalk-72 (even before Smalltalk adopted inheritance in 1976),
or first-class modules in ML @~cite{Russo1998},
provided first-class modularity.


@subsection{Modularity and Complexity}

Modularity used correctly can tremendously simplify programs and improve their quality,
by enabling the solutions to common problems to be solved once by experts at their best,
and then shared by everyone,
instead of the same problems being solved over and over, often badly,
by amateurs or tired experts.

Use of modules also introduces overhead, though,
at development time, compile-time and runtime alike:
boiler plate to define and use modules,
name management to juggle with all the modules,
missing simplifications in using overly general solutions
that are not specialized enough to the problems at hand,
duplication of entities on both sides of each module’s interface,
extra work to cover the “impedance mismatch” between
the representation used by the common solution
and that used by the actual problems, etc.

Modularity used incorrectly, with too many module boundaries,
module boundaries drawn badly,
resolved at the wrong evaluation stage,
can increase complexity rather than reduce it:
more parts and more crossings between parts cause more overhead;
the factoring may fail to bring simplification in how programmers may reason about the system;
subtle underdocumented interactions between features,
especially involving implicit or mutable state,
can increase the cognitive load of using the system;
attempts at unifying mismatched concepts only to then re-distinguish them
with lots of conditional behavior will cause confusion rather than enlightenment;
modules may fail to offer code reuse between different situations;
programmers may have to manage large interfaces to achieve small results@xnote["."]{
  A notable trend in wrongheaded “modularity” is @emph{myopic} designs,
  that achieve modest savings in a few modules under focus
  by vastly increasing the development costs left out of focus,
  in extra boilerplate and friction, in other modules having to adapt to the design,
  in inter-module glue being made more complex, or
  in module namespace curation getting more contentious.

  For instance, the once vastly overrated and overfunded notions “microkernels” or “microservices”
  consist in dividing a system in a lot of modules, “servers” or “services”
  each doing a relatively small task within a larger system,
  separated by extremely expensive hardware barriers
  wherein data is marshalled and unmarshalled across processes or across machines.
  Each service in isolation, that you may focus on,
  looks and is indeed simpler than the overall software combining those services;
  but this overall software kept out of focus
  was made significantly more complex by dividing it into those services.
  Another myopic design that is much rarer these days is to rewrite a program
  from a big monolithic heap of code in a higher level language
  into a lot of subroutines in a lower-level language (sometimes assembly language),
  boasting how vastly simpler each of these subroutines is to the previous program;
  yet obviously the sum total of those routines is much more complex to write and maintain
  than a similarly factored program written in an appropriate higher level language
  (that indeed the original language might not be).

  While myopic designs might offer some small performance improvement in terms of
  pipelining, locality and load-balancing for very large worksets,
  they offer no improvement whatsoever in terms of modularity, quite the contrary:
  @Xitemize[@;#:style enumparenalph
  @Xitem{(1)
    The modules must already exist or be made to exist at the language level,
    before the division into services may be enacted.
    Far from @emph{providing} a solution to any problem for the programmer,
    these techniques @emph{require} programmers to already have solved
    the important problem of module factoring before you can apply them,
    at which point they hinder rather than help.
  }
  @Xitem{(2)
    Given the factoring of a program into modules,
    these techniques add runtime barriers
    that do not improve safety compared to compile-time barriers
    (e.g. using strong types, whether static or dynamic).
  }
  @Xitem{(3)
    Yet those runtime barriers vastly decrease performance, and even more so
    by the compile-time optimizations they prevent (whether builtin or user-defined with e.g. macros).
  }
  @Xitem{(4)
    The interface of each module is made larger for having to include a specific marshalling,
    and the constraints related to being marshalled.
    This problem can be alleviated by the use of “interface description languages”,
    but these languages tend to be both poorer and more complex
    than a good programming language’s typesystem,
    and introduce an impedance mismatch of their own.
  }
  @Xitem{(5)
    Any alleged benefits from dividing in multiple distributed processes,
    whether in terms of failure-resistance, modularity, safety or performance
    are wholly lost if and when (as is often the case) persistent data
    all ends up being centralized in database services,
    that become a bottleneck for the dataflow.
  }
  @Xitem{(6)
    The overall system is not made any smaller for being divided in smaller parts;
    actually, all the artificial process crossings, marshallings and unmarshallings,
    actually make the overall system noticeably larger in proportion to how small those parts are.
    Lots of small problems are added at both runtime and compile-time,
    while no actual problem whatsoever is solved.
  }
  @Xitem{(7)
    These designs are thus actually detrimental in direct proportion to how much they are followed,
    and in proportion to how beneficial they claim to be. They are, technically, a lie.
  }
  @Xitem{(8)
    In practice, “successful” microkernels import all services into a monolithic “single server”,
    and successful microservices are eventually successfully rebuilt
    as vertically integrated single services.
  }]

  In the end, the technical justification for these misguided designs stem from the inability
  to think about meta-levels and distinguish between compile-time and runtime organization of code:
  Modularity is a matter of semantics, as manipulated by programmers at programming-time,
  and by compilers at compile-time;
  the runtime barrier crossings of these myopic designs
  cannot conceivably do anything to help about that, only hinder.
  @;{ TODO cite something about HURD? Footnote? }

  Now, there are sometimes quite valid socio-economical (and not technical) justifications
  to dividing a program into multiple services:
  when there are many teams having distinct incentives, feedback loops, responsibilities,
  service level agreements they are financially accountable for, etc.,
  it makes sense for them to deploy distinct services.
  Indeed, Conway’s Law @;TODO CITE ?
  states that the technical architecture of software follows its business or management architecture.

  Finally, it is of note that some systems take a radically opposite approach
  to modularity, or lack thereof:
  instead of gratuitous division into ever more counter-productive modules,
  some prefer wanton avoidance of having to divide code into modules at all.
  For instance, APL reduces the @emph{need} for modules or even subroutines by being extremely terse,
  and by shunning abstraction in favor of concrete low-level data representations
  (on top of its high-level virtual machine rather than of the lower-level model
  of more popular languages),
  to the point of replacing many routine names
  by common idioms—known sequences of combinators.
  (The many talks by Aaron Hsu at LambdaConf are an amazing glimpse at this style of programming.)
  @;{TODO cite - ask arcfide / sacrideo}
  The programmer can then directly express the concepts of the domain being implemented
  in terms of concrete APL’s tables and functions, stripped of all the unnecessary abstractions,
  until the solution is both so simple and task-specific that there is no need for shared modules.
  By using his terse combinators to directly manipulate entire tables of data at a time,
  a competent APLer can find a complete solution to a problem
  in fewer lines of code than it takes for programmers in other
  languages just to name the structures, their fields and accessors.
  Admittedly, there is only so much room in this direction:
  as the software grows in scope, and the required features grow in intrinsic complexity,
  there is a point at which it is becomes big to fit wholly in any programmer’s mind,
  then it must be chipped away by moving parts into other modules,
  notably by reusing common algorithms and data structures from libraries
  rather than inline specialized versions.
  But in practice, a lot of problems can be tackled this way by bright enough developers,
  especially after having been divided into subproblems for socio-economic reasons.
}

@subsection{Implementing Modularity}

To achieve modularity, the specification for a software computation
is divided into independently specified modules.
Modules may reference other modules and the subcomputations they define,
potentially specified by many different people at different times,
including in the future.
These references happen through a module context, a data structure
with many fields or subfields that refer to each module, submodule,
or subcomputation specified therein.
When comes time to integrate all the module specifications into a complete computation,
then comes the problem of implementing the circularity between the definition of the module context
and the definitions of those modules.

With third-class modularity, the module context and modules are resolved
before the language processor starts,
by whatever preprocessor external to the language generates the program.
With second-class internal modularity, they are resolved by some passes of the
language-internal processor, before the program starts.
In either of the above cases, a whole-program optimizer might fully resolve
those modules such that no trace of them is left at runtime.
Without a whole-program optimizer, compilers usually generate global linkage tables
for the module context, that will be resolved
statically by a static linker when producing an executable file,
or dynamically by a dynamic loader when loading shared object files,
in either case, before any of the programmer’s regular code is run
(though some hooks may be provided for low-level programmer-specified initialization).

At the other extreme, all third-class or second-class module evaluation
may be deferred until runtime, the runtime result being no different
than if first-class internal modularity was used,
though there might be benefits to keeping modularity compile-time only
in terms of program analysis, synthesis or transformation.
For local or first-class modules, compilers usually generate some kind of
“virtual method dispatch table”
(or simply “virtual table” or “dispatch table” or some such)
for each modularly defined entity, that, if it cannot resolve statically,
will exist as such at runtime.
Thus, Haskell typeclasses become “dictionaries” that may or may not be fully inlined.
In the case of OO, prototypes are indeed typically represented
by such “dispatch table” at runtime—which in Class OO
would be the type descriptor for each object, carried at runtime for dynamic dispatch.

In a low-level computation with pointers into mutable memory,
the module context is often being accessed through a global variable,
or if not global, passed to functions implementing each module as a special context argument,
and each field or subfield is initialized through side-effects,
often with some static protocol to ensure that they are (hopefully, often all) initialized
before they are used.
In a high-level computation without mutation,
each module is implemented as a function taking the module context as argument,
the fields are implemented as functional lenses@;{TODO cite},
and the mutual recursion is achieved using a fixpoint combinator@;{TODO cite};
lazy evaluation may be used as a dynamic protocol to ensure that
each field is initialized before it is used@xnote["."]{
  It is hard to ensure initialization before use;
  a powerful enough language makes it impossible to predict whether that will be the case.

  In unsafe languages, your program will happily load nonsensical values from
  uninitialized bindings, then silently corrupt the entire memory image,
  “dance a fandango on core” @~cite{Raymond1996},
  cause a segmentation fault and other low-level failures,
  or even worse, veer off into arbitrary undefined behavior and yield
  catastrophically wrong results to unsuspecting users.
  The symptoms, if any, are seen long after the invalid use-before-init,
  making the issue hard to pin-point and debugging extremely difficult
  unless you have access to time-travel debugging at many levels of abstraction.

  In not-so-safe languages, a magic NULL value is used, or for the same semantics
  just with additional syntactic hurdles, you may be forced to use some option type;
  or you may have to use (and sometimes provide out of thin air) some default value
  that will eventually prove wrong.
  In the end, your program will fail with some exception,
  which may happen long after the invalid use-before-init,
  but at least the symptoms will be visible and you’ll have a vague idea
  what happened, just no idea where or when.

  Actually safe languages, when they can’t prove init-before-use,
  will maintain and check a special “unbound” marker in the binding cell or next to it,
  possibly even in a concurrency-protected way if appropriate,
  to identify which bindings were or weren’t initialized already, and
  eagerly raise an exception immediately at the point of use-before-init,
  ensuring the programmer can then easily identify where and when the issue is happening,
  with complete contextual information.

  In even safer languages, lazy evaluation automatically ensures that you always have init-before-use,
  and if the compiler is well done may even detect and properly report finite dependency cycles;
  you may still experience resource exhaustion if you generate infinite new dynamic dependencies,
  but so would you in the less safe alternatives if you could reach that point.

  Safest languages may require you to statically prove init-before-use in finite time,
  which may be very hard with full dependent types,
  or very constraining with a more limited proof system,
  possibly forcing you to fall back to only the “not-so-safe” alternative.
  But this technology is onerous and not usable by programmers at large.

  Some languages may let you choose between several of these operation modes depending
  on compilation settings or program annotations.

  Interestingly, whichever safety mode is used, programmers have to manually follow
  some protocol to ensure init-before-use.
  However the lazy evaluation approach minimizes artificial constraints on such protocol,
  that when not sensible might force them to fall back to the not-so-safe variant.

  The init-before-use issue is well-known and exists outside of OO: it may happen
  whenever there is mutual recursion between variables or initial elements of data structures.
  However, we’ll see that “open recursion” @~cite{Cardelli1992 Pierce2002TAPL},
  i.e. the use of operators meant to be the argument of a fixpoint combinator,
  but also possibly composition before fixpointing,
  is ubiquitous in OO,
  wherein partial specifications define methods that use other methods that are yet to be defined
  in other partial specifications, that may or may not follow any particular protocol
  for initialization order.
  Thus we see that the simplest, most natural and safest usable setting for OO is:
  lazy evaluation.

  This may come at a surprise to many who mistakenly believe the essence of OO
  is in the domain it is commonly applied to
  (defining mutable data structures and accompanying functions as “classes”),
  rather than in the semantic mechanism that is being applied to these domains
  (extensible modular definitions of arbitrary code using inheritance).
  But this is no surprise to those who are deeply familiar with C++ templates, Jsonnet or Nix,
  and other systems that allow programmers to directly use unrestricted OO
  in a more fundamental purely functional setting wherein OO can be leveraged
  into arbitrary programming and metaprogramming, not just for “classes” stricto sensu.
  In the next subsection, I argue the case from the point of view of modularity
  rather than initialization safety;
  however, both can be seen as two aspects of the same argument about semantics.
}
The more programmers must agree on a protocol to follow, the less modular the approach.
In a stateful applicative language, where the various modular definitions
are initialized by side-effects, programmers need to follow some rigid protocol
that may not be expressive enough to capture the modular dependencies between internal definitions,
often leading to indirect solutions like “builder” classes in Java,
that stage all the complex computations before
the initialization of objects of the actually desired class.

@exercise[#:difficulty "Easy"]{
  Identify languages with each of first-class, second-class, third-class and fourth-class
  module systems. Maybe the same language across the years.
}

@exercise[#:difficulty "Easy"]{
  Pick one of the modularity breakthroughs I listed.
  Does it actually make code more modular?
  Compare code written without said breakthrough to code written with it,
  and how much that code needs to be modified to accommodate for a desired change.
}

@exercise[#:difficulty "Easy"]{
  Consider software projects you have written or used.
  Identify what are the units of modularity, and for each unit,
  what are its interface vs its implementation.
  How much of the interface is formal, versus how much is informal?
}

@exercise[#:difficulty "Easy"]{
  Consider various programming interactions you are having with colleagues,
  members of a community, or authors of libraries.
  How much do you and other people have to synchronize for your software to work well with each other?
  Did you have to modify the code of other people’s software? To read the code?
  To read only an interface? What did they need to do with respect to your code?
  How much cooperation could you achieve, with how much coordination?
}

@exercise[#:difficulty "Medium"]{
  Use a preprocessor to achieve third-class modularity for a language lacking internal modularity,
  splitting code into many files.
  For instance, generate instructions for your LLM from a template
  that mixes project-specific instructions with generic and conditional shared instructions.
  Or, if you manage several Unix machines,
  generate some machine-wide (in @c{/etc}) or personal configuration file
  (in @c{~/.*} of @c{~/.config/*}) from a script specialized with machine-specific parameters.
}

@exercise[#:difficulty "Medium"]{
  Pick one of the features I list as making a language more modular, at random.
  Pick a language with the feature, and a program using that feature heavily.
  Explain how to reproduce the effect of that program without using the language feature.
  Does it indeed involve authors of part of a program having to learn more
  about other parts of the program, having to manually enforce more complex invariants?
}

@exercise[#:difficulty "Medium"]{
  Find an example of a system with too much modularity,
  or modularity where it doesn’t belong,
  actually making things worse.
}

@exercise[#:difficulty "Hard"]{
  Identify a language with some modularity features that are less modular than could be:
  the feature still requires programmers to manually curate redundancies within or across files,
  to tightly couple changes in multiple locations, etc.
  Use macros or a preprocessor to reduce the coupling and offer a more modular abstraction
  (though the language ecosystem will not become more modular
  without other people adopting this innovation).
}

@exercise[#:difficulty "Research"]{
  Find a more modular design pattern to use in an existing language ecosystem;
  possibly patch the language implementation, standard library, or key piece of infrastructure,
  to support this pattern.
  Provide backward compatibility for better results.
  See how hard it is not just to implement, but to get it adopted.
}

@section[#:tag "E"]{Extensibility}
@epigraph{Malum est consilium, quod mutari non potest. @linebreak[]
@~ @~ (It is a bad plan that admits of no modification.)
@|#:- "Publius Syrus (1st century BC)"|}
@subsection{Extending an Entity}
Extensibility is the ability to take a software entity and create a new entity
that includes all the functionality of the previous entity,
and adds new functionality, refines existing functionality,
or otherwise modifies the previous entity.
Extensibility can be realized inside or outside of a programming language.

@subsection{First-class to Fourth-class Extensibility}

External Extensibility is the ability to extend some software entity
by taking its code then reading it, copying it, editing it,
processing it, modifying it, (re)building it, etc.
Extensibility is much easier given source code meant to be thus read and written,
but is still possible to reverse-engineer and patch binary code,
though the process can be harder, less robust, less flexible and less reusable.
Thus, external extensibility is always possible for any software written in any language,
though it can be very costly, in the worst case
as costly as rewriting the entire extended program from scratch.
When done automatically, it’s third-class extensibility.
When done manually, it’s fourth-class extensibility.
The automation need not be written in the same language as the software being extended,
or as its usual language processor, though either may often be the case.

Internal or Intra-linguistic extensibility, either first-class or second-class, by contrast,
is the ability to extend, refine or modify a software entity
without having to read, rewrite, or modify any of the previous functionality,
indeed without having to know how it works or have access to its source code,
only having to write the additions, refinements and modifications.
Second-class extensibility happens only at compile-time, and is quite restricted,
but enables more compiler optimizations and better developer tooling.
First-class extensibility may happen at runtime, and is less restricted
than second-class extensibility but more so than external extensibility
in terms of what modifications it affordably enables,
yet less restricted than either in terms of enabling last-minute customization
based on all the information available to the program.

These notions of extensibility are complementary and not opposite, for often to extend a program,
you will first extract from the existing code a shared subset that can be used as a library
(which is an extra-linguistic extension),
and rewrite both the old and new functionality as extensions of that library
(which is an intra-linguistic extension),
as much as possible at compile-time for efficiency (which is second-class extensibility),
yet enough at runtime to cover your needs (which is first-class extensibility).

Finally, as with modularity, the lines between “external” and “internal”, or
between “second-class” and “first-class”, can often be blurred by
reflection and “live” interactive environments.
But these demarcations are not clear-cut objective features of physical human-computer interactions.
They are subjective features of the way humans plan and analyze those interactions.
To end-users who won’t look inside programs, first-class, second-class, third-class,
and if sufficiently helpless even fourth-class, are all the same: things they cannot change.
For adventurous hackers willing to look under the hood of the compiler and the operating system,
first-class to fourth-class are also all the same: things they can change and automate,
even when playing with otherwise “dead” programs on “regular” computing systems.
The demarcations are still useful though:
division of labor can be facilitated or inhibited by software architecture.
A system that empowers an end-user to make the simple changes they need and understand,
might prove much more economical than a largely equivalent system that requires
a high-agency world expert to make an analogous change.


@subsection[#:tag "CfE"]{A Criterion for Extensibility}

@principle{A design is more extensible if it enables developers
to enact more kinds of change through smaller, more local modifications}
compared to alternative designs that require larger (costlier) rewrites
or more global modifications, or that prohibit change (equivalent to making its cost infinite).

Extensibility should be understood within a framework of what changes
are or aren’t “small” for a human (or AI?) developer, rather than
for a fast and mindless algorithm.

Thus, for instance, changing some arithmetic calculations to use
bignums (large variable-size integers) instead of fixnums (builtin fixed-size integers)
in C demands a whole-program rewrite with non-local modifications to the program structure;
in Java it involves changes throughout the code—straightforward but numerous—while
preserving the local program structure;
in Lisp it requires minimal local changes; and
in Haskell it requires one local change only.
Thus, with respect to this and similar kinds of change, if expected,
Haskell is more extensible than Lisp, which is more extensible than Java,
which is more extensible than C.
@;{TODO examples for C, Java, Lisp, Haskell}

Extensibility does not necessarily mean that a complex addition or refactoring
can be done in a single small change.
Rather, code evolution can be achieved through many small changes, where
the system assists developers by allowing them to focus
on only one small change at a time,
while the system tracks down the remaining necessary adjustments.

For instance, a rich static typesystem can often serve as a tool
to guide large refactorings by dividing them
into manageably small steps—as extra-linguistic code modifications—making the typechecker
happy one redefinition at a time after an initial type modification.
This example also illustrates how
@principle{Extensibility and Modularity usually happen through
meta-linguistic mechanisms rather than linguistic ones},
i.e. through tooling outside the language rather than
through expressions inside the language.
Even then, internalizing the locus of such extensible modularity within the language
enables dynamic extension, runtime code sharing and user-guided specialization
as intra-linguistic deployment processes that leverage the result of
the extra-linguistic development process.

@subsection{Historical Extensibility Breakthroughs}

While any software is externally extensible by patching the executable binary,
the invention of assembly code made it far easier to extend programs,
especially with automatic offset calculations from labels.
Compilers and interpreters that process source code meant for human consumption and production,
along with preprocessors, interactive editors, and all kinds of software tooling,
greatly facilitated external extensibility, too.
Meanwhile, software that requires configuration through many files
that must be carefully and manually kept in synch
is less extensible than one configurable through a single file
from which all required changes are automatically propagated coherently
(whether that configuration is internal or external, first-class to fourth-class).
Whenever multiple files must be modified together
(such as interface and implementation files in many languages),
the true units of modularity (or in this case extensibility)
are not the individual files—but each group of files that must be modified in tandem;
and sometimes, the units of extensibility are entities that span parts of multiple files,
which is even more cumbersome.

Higher-level languages facilitate external extensibility compared to lower-level ones,
by enabling programmers to achieve larger effects they desire
through smaller, more local changes that cost them less.
Thus, FORTRAN enables more code extensibility than assembly, and Haskell more than FORTRAN.
Languages with higher-order functions or object orientation enable greater internal extensibility
by allowing the creation of new in-language entities built from existing ones
in more powerful ways.
To demonstrate how OO enables more extensibility, consider
a course or library about increasingly more sophisticated data structures:
when using a functional language with simple Hindley-Milner typechecking, @; TODO cite Okasaki book?
each variant requires a near-complete rewrite from scratch
of the data structure and all associated functions;
but when using OO,
each variant can be expressed as a refinement of previous variants
with only a few targeted changes.

@subsection{Extensibility without Modularity}

Before delving deeply into how OO brings together extensibility and modularity,
it is worth explaining what extensibility without modularity means.

Operating Systems, applications or games sometimes deliver updates via binary patch formats
that minimize data transmitted while maximizing changes effected to the software.
Such patches embody pure extensibility with total lack of modularity:
they are meaningful only when applied to the exact previous version of the software.
Text-based diff files can similarly serve as patches for source code,
and are somewhat more modular, remaining applicable even in the presence
of certain independent changes to the source code, yet are still not very modular overall:
they are fragile and operate without respecting any semantic code interface;
indeed their power to extend software lies precisely in their not having to respect such interfaces.

The ultimate in extensibility without modularity would be to specify modifications to the software
as bytes concatenated at the end of a compressed stream (e.g. using @c{gzip} or some AI model)
of the previous software:
a few bits could specify maximally large changes, ones that can reuse large amounts of information
from the compressor’s model of the software so far;
yet those compressed bits would mean nothing outside the exact context
of the compressor—maximum extensibility, minimum modularity.

Interestingly, these forms of external extensibility without modularity,
while good for distributing software, are totally infeasible for humans to directly create:
they vastly increase rather than decrease the cognitive load required to write software.
Instead, humans use modular forms of extensibility, such as editing source code
as part of an edit-evaluate-debug development loop, until they reach a new version
of the software they want to release, after which point they use automated tools
to extract from the modularly-achieved new version some non-modular compressed patches.

As for internal extensibility without modularity,
UML, co-Algebras or relational modeling, as previously discussed in @secref{OiaMotW},
fail to model OO precisely because the “classes” they specify are mere types:
they lack the module context that enables self-reference in actual OO classes.
As in-language entities, they can be extended by adding new attributes,
but these attributes have to be constants:
they may not contain any self-reference to the entity being defined and its attributes.


@subsection{Extensibility and Complexity}

Extensibility of software entities means that variants of these software entities
may be reused many times in as many different contexts,
so more software can be achieved for fewer efforts.

Now, external extensibility through editing (as opposed to e.g. preprocessing)
means that these entities are “forked”,
which in turn reintroduces complexity in the maintenance process,
as propagating bug fixes and other changes to all the forks
becomes all the harder as the number of copies increases.
External extensibility through preprocessing,
and internal extensibility, do not have this issue,
and, if offered along a dimension that matters to users,
enable the development of rich reusable libraries
that overall decrease the complexity of the software development process@xnote["."]{
  Interestingly, detractors of OO will often count “code reuse” as a bad aspect of OO:
  they will claim that they appreciate inheritance of interfaces, but not of implementation.
  Many OO language designers, as of Java or C#, will say the same of multiple inheritance
  though not of single inheritance.
  Yet, many practitioners of OO have no trouble reusing and extending
  libraries of OO classes and prototypes,
  including libraries that rely heavily on multiple inheritance for implementation.

  It is quite likely that users bitten by the complexities yet limitations
  of multiple inheritance in C++ or Ada may see that
  it does not bring benefits commensurable with its costs;
  and it is also quite likely that users fooled by the absurd “inheritance is subtyping” slogan
  found that code written under this premise does not quite work as advertised.
  These disgruntled users may then blame OO in general
  for the failings of these languages to implement OO,
  or for their believing false slogans and false lessons propagated by bad teachers of OO.
}

Note that often, the right way to reuse an existing software entity
is not to reuse it as is (internal extensions),
but first to refactor the code (external extensions) so that both the old and new code
are extensions of some common library (internal extensions).
Then the resulting code can be kept simple and easy to reason with.
Internal and external extensibility are thus mutual friends, not mutual enemies.

By decreasing not only overall complexity, but the complexity of @emph{incremental} changes,
extensibility also supports shorter software development feedback loops,
therefore more agile development.
By contrast, without extensibility, developers may require more effort
before any tangible incremental progress is achieved,
leading to loss of direction, of motivation, of support from management
and of buy-in from investors and customers.
Extensibility can thus be essential to successful software development.

@subsection{Implementing Extensibility}

To enable extensibility, a software entity must be represented in a way
that allows semantic manipulation in dimensions meaningful to programmers.
By the very nature of programming, any such representation is enough to
enable arbitrary extensions, given a universal programming language,
though some representations may be simpler to work with than others:
they may enable programmers to express concisely and precisely the changes they want,
at the correct level of abstraction, detailed enough that they expose the concepts at stake,
yet not so detailed that the programmer is drowned in minutiae.

Now, in the case of second-class internal extensibility,
or of first-class internal extensibility in a less-than-universal language,
representations are no longer equivalent,
as only a subset of computable extensions are possible.
This kind of limited extensibility is not uncommon in computing systems;
the restrictions can help keep the developers safe and simplify the work of language implementers;
on the other hand, they may push developers with more advanced needs towards
relying external extensibility instead, or picking an altogether different language.

In a pure functional model of extensibility,
an extension is a function that takes the original unextended value
and returns the modified extended value.
In a stateful model of extensibility,
an extension can instead be a procedure that takes a mutable reference, table or structure
containing a value, and modifies it in-place to extend that value—sometimes
using “hot-patches” that were not foreseen by the original programmer.

@;{TODO examples}

@exercise[#:difficulty "Easy"]{
  Find examples for first-class, second-class, third-class and third-class extensibility.
}

@exercise[#:difficulty "Easy"]{
  For each of the following, identify whether it is an example of
  first-class, second-class, third-class, or fourth-class extensibility,
  or not extensibility at all:
}
@itemize[
  @item{
     Downloading a new version of some software.
  }@item{
     Downloading an update to some software that modifies it in place.
  }@item{
     Manually fixing a bug in source code bug.
  }@item{
     Having an AI fix a bug in source code bug.
  }@item{
     Applying a patch file to modify some pristine source code release to fix bugs.
  }@item{
     Filing a bug report in a bug system.
  }@item{
     Manually modifying a binary executable to add extra player health,
     defeat copy protection, disable remote telemetry, etc.
  }@item{
     Publishing a script that can automatically modify some executable,
     so you can share your fix with others without having to republish
     the proprietary software (or a direct derivative thereof)
     and violate its license terms.
  }@item{
     Using a “decorator”, “advice”, “aspect”, “around method”, or “newtype”, etc.,
     to add some functionality to a function, class or type.
  }@item{
     Passing a first-class function to a higher-order wrapper that modifies its behavior.
  }@item{
     Using subclassing to specialize a class.
  }@item{
     Calling a function in a locally modified environment that overrides
     the values of variables it relies on.
}]

@exercise[#:difficulty "Medium"]{
  Which variants of extensibility above have you used?
  If you haven't tried them all, can you try one you haven't used before?
}

@exercise[#:difficulty "Medium"]{
  Consider various programming interactions you are having with colleagues,
  members of a community, or authors of libraries.
  What are pieces of software you might want to extend
  but cannot extend in practice without reimplementing it completely?
  What prevents you?
  How could you (or the original authors) modify the software to make it more extensible?
}

@exercise[#:difficulty "Hard"]{
  Use some text-processing tools to achieve third-class extensibility
  for a language lacking internal extensibility,
  by programmatically extending some existing code without modifying the original function.
  You may insert markers in the original code for where things should be edited,
  without modifying its semantics;
  the third-class extension should use those markers, or contextual information,
  to generate modified code that extends the original one;
  simple modifications to the original file should be automatically reflected
  in the extended file after re-running the third-class extension.
  @; TODO make a more concrete example

  Notice how much harder this is than with modularity,
  because you have to deal non-trivially with programs as input, not just output.
}

@exercise[#:difficulty "Hard"]{
  With the help of an AI if needed, write a simple numerical program,
  say polynomial interpolation using Newton’s or Lagrange’s method,
  with floating point numbers.
  How much do you need to modify it to work with arbitrary-precision rational numbers?
  With numbers from finite field @c{F_q}, yielding a Reed-Solomon code (say with q=256)?
  Can you write a version that shares the logic between different number fields,
  and gets instantiated into a variant with a specific field in a single line, or two to the most?
  Compare the situation in C, Java, Lisp, Haskell.
}

@exercise[#:difficulty "Research"]{
  Identify some existing software you actually use,
  that should be internally extensible in some way, but isn’t.
  Formalize a reasonable use case as code that ought to work if it were indeed extensible.
  Propose a change making the software extensible that way,
  that would make your use case actually work.
  Implement the change, enjoy your extension.
  Get your change accepted upstream by the maintainers.
}

@section[#:tag "extensible_modularity"]{Extensible Modularity}
@epigraph{
  Power Couple:
    Two individuals that are super heroes by themselves
    yet when powers combine become an unstoppable, complementary unit.
      @|#:-"Urban Dictionary"|
}
@subsection{A Dynamic Duo}
Modularity and Extensibility work hand in hand:
@principle{Modularity means you only need to know
a small amount of old information to make software progress.
Extensibility means you only need to contribute
a small amount of new information to make software progress.}
Together they mean that one or many finite-brained developers can better divide work
across time and between each other,
and make more software progress with a modular and extensible design,
tackling larger problems while using less mental power each,
compared with using less-modular and less-extensible programming language designs.
Together they enable the organic development of cooperative systems
that grow with each programmer’s needs without getting bogged down in coordination issues.

Remarkably, though, you can have each of modularity and extensibility separately, and
that isn’t the same as having the two together.
For instance, first-class modules in ML are units of modularity,
on which you can do dynamic dispatch, and that you can transform with “functors”,
functions that take modules as parameters and return modules as a result.
But somehow you cannot define an extension in a modular way.
First-class modules, while units of modularity with respect to dispatch,
and units of extension with respect to functors, remain second-class
with respect one crucial aspect of modularity:
resolving many modular definitions from many people together into a single program.

Early examples of Modularity and Extensibility together that pre-date fully-formed OO include
of course classes in Simula 1967 @~cite{Simula1967}, but also precursor breakthroughs like
the “locator words” of the Burroughs B5000 @~cite{lonergan1961 barton1961}, and
Ivan Sutherland’s Sketchpad’s “masters and instances” @~cite{sketchpad1963},
that both inspired Kay, or Warren Teitelman’s Pilot’s ADVISE facility @~cite{teitelman1966},
that was influential at least in the Lisp community and led to method combination
in Flavors and CLOS@xnote["."]{
  There were definitely exchanges between the Smalltalk and Interlisp teams at PARC:
  In the 1970s, Kay got “inheritance” from Bobrow’s KRL,
  who in return got “object-oriented” and applied inheritance to procedures.
  Lispers quickly copied Kay’s OO design as a mechanism
  to define code rather than just to represent data.
  In the 1980s Bobrow did work on Smalltalk projects like PIE as well as Lisp projects like LOOPS,
  but it seems that—maybe after Kay’s and many others’ departure from PARC—the Smalltalk team
  stopped following or understanding OO developments from the Lisp world,
  and Interlisp had much more influence on the MIT Lispers than on the co-located Smalltalkers.
}

@subsection{Modular Extensible Specifications}

A modular extensible specification specifies how to extend a previous value,
but in a modular way: the extension is able to use some modular context
to refer to values defined and extended by other pieces of code.

At this point, I reach the end of what can be clearly explained while remaining informal.
I must introduce a formal model of modularity, extensibility,
and the two together, to further convey the ideas behind OO without resorting to handwaving.
The last task to carry informally is therefore a justification of my approach to formalization.

@section{Why a Minimal Model of First-Class OO using FP?}

@subsection{Why a Formal Model?}

The current section was largely appealing to well-established concepts in Computing.
Inasmuch as it can be understood, it is only because I expect my readers
to be seasoned programmers and scientists familiar with those concepts.
My most complex explanations were in terms of functions from a modular context to a value,
or from some (original) value to another (extended) value (of essentially the same type).
As I try to combine these kinds of functions, the complexity is greater
than can be explained away in a few words, and the devil will be in the details.

A formal model allows one to precisely define and discuss the building blocks of OO,
in a way that is clear and unambiguous to everyone.
This is all the more important when prevailing discourse about OO is full of
handwaving, imprecision, ambiguity, confusions, and
identical words used with crucially different meanings by different people.

@subsection{Why a Minimal Model?}
@epigraph{Actually a person does not @emph{really} understand something
until teaching it to a @emph{computer}, i.e. expressing it as an algorithm.
  @|#:- "Donald Knuth"|
}
I seek a @emph{minimal} model because a non-minimal model means there are still concepts
that haven’t been teased apart from each other, but are confused and confusing.

If I @emph{can} express classes in terms of prototypes, prototypes in terms of specifications,
or multiple and single inheritance in terms of mixin inheritance,
then I @emph{must} do so, until I reduce OO to its simplest expression,
and identify the most fundamental building blocks within it,
from which all the usual concepts can be reconstituted, explained, justified, evaluated,
generalized, and maybe even improved upon.

@subsection{Why Functional Programming?}

Functional Programming (FP) is a computational model
directly related to formal or mathematical logic
whereby one can precisely reason about programs, their semantics (what they mean),
how they behave, etc.
The famous Curry-Howard Correspondence establishes a direct relationship
between the terms of the λ-calculus that is the foundation of FP,
and the rules of logical deduction. That correspondence can also be extended
to cover the Categories of mathematics, and more.

Therefore, in an essential sense, FP is indeed the “simplest” paradigm
in which to describe the semantics of OO and other programming paradigms:
it is simultaneously amenable to both computation and reasoning
with the least amount of scaffolding for bridging between the two.

@subsection{Why an Executable Model?}
While I could achieve a slightly simpler algorithmic model
by using a theoretical variant of the λ-calculus,
I and other people would not be able to directly run and test my algorithms
without a costly and error-prone layer of translation or interpretation.

A actual programming language that while very close to the λ-calculus
comes with both additional features and additional restrictions,
will introduce some complexity, and
a barrier to entry to people not familiar with this particular language.
But it will make it easy for me and my readers
to run, to test, to debug, and to interact with
the code I offer in this book,
and to develop an intuition on how it runs and what it does,
and later to extend and to adapt it.

My code will also provide a baseline for implementers who would want to use my ideas,
and who may just port my code to their programming language of choice,
and be able to debug their port by comparing its behavior to that of the original I provide.
They can implement basic OO in two lines of code in any modern programming language,
and have a full-featured OO system in a few hundreds of lines of code.

@subsection{Why First-Class?}

The most popular form of OO is second-class, wherein
all inheritance happens at compile-time when defining classes.
But for some programmers to use OO as a second-class programming construct,
language implementers still have to implement OO as a first-class construct
within their compilers and other semantic processors.
@principle{Anyone’s second-class entities are someone else’s first-class entities}.
And you still don’t fully understand those entities until you have implemented them,
at which point they are first class.
Thus, every useful minimal semantic model is always a first-class model,
even if “only” for the implementation of a meta-level tool such as a typechecker.

@subsection{What Precedents?}

I was flabbergasted when I first saw
basic OO actually implemented in two function definitions,
in the Nix standard library @~cite{nix2015}.
These two definitions can be ultimately traced in a long indirect line
to the pioneering formalization by Bracha and Cook @~cite{bracha1990mixin},
though the author wasn’t aware of the lineage, or indeed even that he was doing OO@xnote[";"]{
  Peter Simons, who implemented prototypes as a user-level library in Nix
  as “extensions”, wrote in a private communication that
  he did not not know anything about their relationship to Prototypes, Mixins or OO,
  but semi-independently reinvented them and their use,
  inspired by the Haskell support code by Russell O’Connor,
  and by examples and discussions with Andres Löh and Conor McBride;
  These inspirers unlike Simons were well-versed in OO literature,
  though they are usually known to advocate FP over OO.
}
however, Nix also implements conflation, a crucial element missing from Bracha and Cook.

I will deconstruct and reconstruct this formalization.
Then, on top of this foundation, I will be able to add all the usual concepts of OO,
developed by their respective authors, whom I will cite.

@subsection[#:tag "WS"]{Why Scheme?}

The Algorithmic Language Scheme @; TODO cite original paper, Steele thesis, R{1-7}RS
is a minimal language built around a variant of the applicative λ-calculus,
as a dialect in the wider tradition of LISP. @; CITE
It has many implementations, dialects and close cousins, @; cite Racket
a lot of documentation, modern libraries, @; TODO cite SRFIs
decades of established lore, code base, user base, academic recognition.
It also has macro systems @; TODO cite
that allow you to tailor the syntax of the language to your needs.

Some other languages, like ML or Haskell, @; CITE
are closer to the theoretical λ-calculus, but come with builtin typesystems
that are way too inexpressive to accept the λ-terms I use.
I suspect that systems with dependent types, such as in Rocq, Agda or Lean, @; CITE
are sufficiently expressive, but the types involved might be unwieldy
and would make it harder to explain the basic concepts I present.
I leave it as an exercise to the reader to port my code to such platforms,
and look forward to the result.

Nix, that directly provides λ-calculus with dynamic typing,
is lazy, which actually makes the basic concepts simpler.
But it would require more care for implementers trying to port such an implementation
to most programming contexts that are applicative.
Also, Nix is more recent, less well-known, its syntax and semantics less recognizable;
the Lindy effect @; TODO cite
means it will probably disappear sooner than Scheme,
making this book harder to read for potential readers across time.
Finally, Nix as compared to Scheme, is missing a key feature beyond the basic λ-calculus,
that I will use when building multiple inheritance:
the ability to test two specifications for equality.

Therefore, I pick Scheme as the best compromise in which to formalize OO.

@exercise[#:difficulty "Easy, Required"]{
  Install on your computer an implementation of the programming language Scheme,
  read the tutorial (if necessary), and play with it.
  I personally use Gerbil Scheme @~cite{GerbilScheme};
  but if you are a beginner, you will probably find it much easier to use
  the closely related language Racket @~cite{Felleisen2015};
  or for a plain Scheme experience, we recommend Chez Scheme, that is very fast.
  You can also play with it on websites like replit.
  Another option is for you to do all exercises in your own programming language of choice,
  which will be much easier if your language at least support first-class higher-order functions,
  and either dynamic typing, or recursively constrained (sub)types;
  you’re on your own to translate the problems and their solutions in said language of your choice.
}

@exercise[#:difficulty "Easy, Required"]{
  Locate file @c{util/pommette.scm} that comes with the source code for this book,
  e.g. at @url{https://github.com/metareflection/poof}.
  It contains all the examples in the book, and more, so you can run them,
  copy/paste them, modify them, play with with them, etc.
  See in the @c{Makefile} how to run it with your favorite implementation
  (currently supported: Gerbil Scheme, Chez Scheme, Racket).
}

@exercise[#:difficulty "Medium, Recommended"]{
  If you are not yet familiar with the λ-calculus,
  read a tutorial about it, for instance @citet{Bertot2015}.
  You don’t have to absorb the entire @citet{Barendregt1984} though.
  Just understand the basics, how they map to higher-order functions in your favorite language,
  or how to implement them (e.g. as “closures”) if not available in said favorite language.
  Your language tutorial might already include a section on such functions.
}

@exercise[#:difficulty "Medium, Recommended"]{
  If you did exercise @exercise-ref{03to04}, compare your previous answers with mine.
  See what surprised you—what you agreed and disagreed with before you read this chapter,
  and how your understanding has evolved already.
}

@exercise[#:difficulty "Hard, Recommended" #:tag "04to05"]{
  Based on this informal explanation of modularity and extensibility, and
  before you read the next chapter, build your own minimal models
  of what “modularity” and “extensibility” could mean
  in terms of pure functional programming with first-class entities,
  which would be “internality”.
  In each case, make a first model that can adequately describe what you think it means;
  then strip it to the barest minimum, removing any feature not strictly necessary,
  anything that couldn’t be implemented by specializing a function argument,
  refactoring it down until nothing can be removed.
  Bonus if you can then put them together to mean something greater than either apart.
  Save your answer to compare with the treatment in @secref{MOO}.
}
