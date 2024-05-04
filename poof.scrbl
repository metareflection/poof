#lang scribble/acmart @acmsmall @screen @nonacm
@; Default already: @10pt @natbib
@; @anonymous @authordraft @authorversion @timestamp
@; Accepted at the Scheme Workshop 2021 https://icfp21.sigplan.org/home/scheme-2021

@title{Prototypes: Object-Orientation, Functionally}

@author[
  #:email (email "fare@mukn.io")
  #:affiliation (affiliation #:institution @institution{@emph{Mutual Knowledge Systems, Inc.}}
                             #:country "USA")
]{François-René Rideau}
@author[
  #:email (email "alexknauth@mukn.io")
  #:affiliation (affiliation #:institution @institution{@emph{Mutual Knowledge Systems, Inc.}}
                             #:country "USA")
]{Alex Knauth}
@author[
  #:email (email "namin@seas.harvard.edu")
  #:affiliation (affiliation #:institution @institution{@emph{Harvard University}}
                             #:country "USA")
]{Nada Amin}

@abstract{
This paper elucidates the essence of Object-Oriented Programming (OOP),
using a constructive approach:
we identify a minimal basis of concepts with which to synthesize
existing and potential object systems.
We reduce them to constructions atop the pure untyped lambda calculus,
thereby obtaining both denotational semantics and effective implementation.
We start from the simplest recognizable model of prototype-based OOP,
so simple it arguably does not even have “objects” as such.
We build further models of increasing sophistication, reproducing a growing subset of features
found in past object systems, including original combinations.
We also examine how our approach can deal with issues like typing, modularity, classes, mutation.
We use Scheme to illustrate our approach.
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

@;@(define (nix . foo) (apply minted "nix" foo))
@(define (nix . foo) (apply verbatim foo))
@(define (~nocite . x) (let ((_ (apply @~cite x))) (void)))

@;; Suppress the page count for the Camera-ready version by uncommenting the below.
@;@tex{\thispagestyle{empty}\pagestyle{empty}}
@;; Instead, we could set the start page for the document with:
@;@pageStart{42}

@(define-simple-macro (r a ...) (racket a ...))
@(define-simple-macro (Definitions a ...) (examples/module poof #:no-result a ...))
@(define-simple-macro (Examples a ...) (examples #:eval poof #:no-result a ...))
@(define-simple-macro (Checks a ...) (examples #:eval poof #:label #f a ...))

@(define-bibtex-cite "poof.bib" ~cite citet generate-bibliography)

@(define-simple-macro (defsection name tag text) (define (name (x text)) (seclink tag x)))
@(defsection section1 "Prototypes_bottom_up" "section 1")
@(defsection section2 "pure_objective_fun" "section 2")
@(defsection section3 "beyond_objects" "section 3")
@(defsection section34 "without_subtyping" "section 3.4")
@(defsection section4 "Better_objects" "section 4")
@(defsection section42 "unifying_instance_prototype" "section 4.2")
@(defsection section43 "multiple_inheritance" "section 4.3")
@(defsection section5 "Classes" "section 5")
@(defsection section6 "Mutability" "section 6")
@(defsection section7 "Related_Work" "section 7")
@(defsection section8 "Future_Work" "section 8")
@(defsection AppendixA "Appendix_A" "Appendix A")
@(defsection AppendixB "Appendix_B" "Appendix B")
@(defsection AppendixC "Appendix_C" "Appendix C")
@(defsection AppendixD "Appendix_D" "Appendix D")

@(declare-examples/module poof racket
   (provide (all-defined-out)))
@examples/module[poof #:hidden
(require (only-in racket/base [define def])
         srfi/1
         "util/eval-check.rkt")
@;{TODO:
Some lighter syntax than code:comment.
Get scribble/comment-reader to work?
Manual traversal of the syntax object?
}
]

@;{TODO: For Checks, instead of
> (+ 1 1)
2
rather have
(+ 1 1)
;=> 2
}

@;@(current-pygmentize-default-style 'colorful)
@pretitle{
@tex{
\acmYear{2021}
\copyrightyear{2021}
\acmConference[Scheme]{Scheme and Functional Programming Workshop}{2021}{online}
\acmISBN{978-1-xxxx-xxxx-x/21/10}
\acmPrice{00.00}
\acmDOI{https://dx.doi.org/xx.xxxx/xxxxxxx.xxxxxxx}
\setcopyright{none}
}}

@section[#:tag "Prototypes_bottom_up"]{Prototypes, bottom up}

@subsection{The Essence of OOP}

@subsubsection{Object Orientation in 38 @r[cons] cells of standard Scheme}

@Definitions[
(define (fix p b) (define f (p (lambda i (apply f i)) b)) f)
(define (mix c p) (lambda (f b) (c f (p f b))))
]

@(noindent)
We will make the case that the above two definitions summarize
the essence of Object-Oriented Programming (OOP), and that
all the usual OOP concepts can be easily recovered from them—all while
staying within the framework of pure Functional Programming (FP).

@subsubsection{Claims}
Our approach emphasizes the following original contributions:
@itemlist[
#:style enumparenalph
@item{the explicit conceptual distinction between
  instances, prototypes, wrappers and generators (section @section1{1}),
  objects (section @section42{4.2}), classes and elements (section @section5{5}),}
@item{promoting @emph{composition} of wrappers rather than their @emph{application} to a generator
  as the algebraic structure of interest (sections @section1{1}, @section3{3})}
@item{both @emph{how} to implement multiple inheritance with pure prototypes and
  @emph{why} use them based on a model of modularity (section @section43{4.3}),}
@item{how to derive class OOP from the more primitive prototype OOP (section @section5{5}),}
@item{a constructive model that does not rely on mutation
  (sections @section1{1}, @section2{2}, @section3{3}),
  yet that can be extended to play well with it (section @section6{6}),}
@item{overall, a pure functional approach that provides not only denotational semantics
  atop the untyped lambda-calculus, but also a practical constructive implementation.}
]

@subsubsection{Plan}

In @(section1), we explain how the above functions
implement a minimal but recognizable model of OOP,
with open recursion and inheritance.
In @(section2), we illustrate this OOP model actually enables
incremental definition of complex data structures.
In @(section3), we show how prototypes are useful beyond traditional notions of objects,
and identify how they have a special place in computer science.
In @(section4), we examine how our model can be extended to support
more advanced features of OOP.
In @(section5), we demonstrate how classes are “just” prototypes for type descriptors.
In @(section6), we discuss our pure functional model relates
to models with mutable objects.
In @(section7), we compare our work to salient papers in fifty years of research on OOP.
Finally in @(section8), we propose a path for further research.
Along the way, we relate our approach to both OOP and FP traditions, both decades-old and recent.

@subsubsection{Modular Increments of Computation}
OOP consists in specifying computations in modular increments
each contributing their part based on the combined whole and the computation so far.
The ability to abstract over which increments are part of a computation is called
@emph{ad hoc} polymorphism@~cite{Strachey67}.

In general, we will call @emph{prototype} such a computation increment.
In the rest of @(section1), we will present an initial model
where a @emph{prototype} will be encoded as prototype @emph{wrapper}:
a function of two arguments, @r[self] and @r[super]
(or, above, @r[f] and @r[b], for fixed-point and base value).
In @(section4), we will enrich prototypes to be more than prototype wrappers,
so as to model more advanced OOP features;
but until then, we will use the two interchangeably.

Prototypes come with two primary operations.
Function @r[fix] @emph{instantiates} a prototype @r[p]
given a super/base value @r[b].
Function @r[mix] has @emph{child} prototype @r[c] @emph{inherit}
from @emph{parent} prototype @r[p]
so they operate on a same fixed-point @r[f]
while chaining their effects on @r[b], yielding a new prototype.

Given some instance type @r[Self]
and a super-type @r[Super] of @r[Self],
a prototype for @r[Self] from @r[Super] will thus be
a function from @r[Self] and @r[Super] to @r[Self].
In the type notation of @(AppendixC):
@racketblock[
(code:comment "(deftype (Proto Self Super) (Fun Self Super -> Self st: (<: Self Super)))")
(code:comment "fix : (Fun (Proto Self Super) Super -> Self st: (<: Self Super))")
(code:comment "mix : (Fun (Proto Self Super) (Proto Super Sup2) -> (Proto Self Sup2))")
]

@(noindent)
The first argument @r[self] of type @r[Self] will hold the instance
resulting as a fixed point from the entire computation.
When composing multiple prototypes, every prototype will receive
the @emph{same} value as their @r[self] argument:
the complete instance that results from applying each prototype in order.
This allows prototypes to “cooperate” with each other
on @emph{different} aspects of the computation,
wherein one prototype defines some aspects (e.g. “methods” in some dictionary)
while relying on aspects to be defined by other prototypes (e.g. other methods),
accessed through the @r[self] argument in what is called “late binding”.

The second argument @r[super] by contrast holds the partial result of the
fixed-point computation after applying only the “next” prototypes.
When composing multiple prototypes, each prototype will (presumably) receive
a different value. The last prototype in the list (rightmost, most ancestral
parent) will receive the “base” or “bottom” value from the fix function
(often literally the bottom value or function in the language), then the
"previous" prototype (its child, to the left) will receive ("inherit")
the result of that “next” computation (its parent, to the right), and so on
until the first prototype (leftmost, most recent child) inherits
its @r[super] value from the rest and computes the final instance.
This allows prototypes to cooperate with other prototypes on a @emph{same} aspect
of the instance computation, wherein children prototypes can accumulate, modify
or override the method values inherited from the parent prototypes.

@subsubsection{Applicability to other Programming Languages}

The two definitions above can be easily translated to any language with closures
and either dynamic types or dependent types. However, their potential is not
fully realized in languages with mere parametric polymorphism (see @(section34)).
@; TODO: insert above reference to type discussion?
Furthermore, for an @emph{efficient} implementation of objects with our formulas,
we will also require lazy evaluation (see @(section3)),
whether an ubiquitous language feature or an optional one
(possibly user-implemented with side-effects).
We do not otherwise require side-effects---though they can be used
for the usual optimizations in the common “linear” case (see @(section6)).

@subsection{A minimal object system}

@subsubsection{Records as functions}

Let us relate the above two functions to objects,
by first encoding “records” of multiple named values as functions
from symbol (the name of a slot) to value (bound to the slot).
In accordance with Lisp tradition,
we will say @emph{slot} where others may say “field” or “member” or “method”,
and say that the slot is @emph{bound} to the given value
rather than it “containing” the value or any such thing.

Thus function @r[x1-y2] below encodes a record with two slots
@r[x], @r[y] bound respectively to @r[1] and @r[2].

@Examples[
(code:comment "x1-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define (x1-y2 msg) (case msg ((x) 1)
                              ((y) 2)
                              (else (error "unbound slot" msg))))
]

@(noindent)
We can check that we can indeed access the record slots
and get the expected values:

@Checks[
(eval:check (list (x1-y2 'x) (x1-y2 'y)) '(1 2))
]

@(noindent)
Note that we use Scheme symbols for legibility.
Any large enough type with decidable equality could be used instead.
Thus any language with integers or strings could use this construction.
Languages with hygienic identifiers, such as a Scheme with @r[syntax-case],
could use them instead to avoid accidental clashes.

@subsubsection{Prototypes for Records}

A @emph{prototype} for a record is a function of two record arguments @r[self],
@r[super] that returns a record extending @r[super].
To easily distinguish prototypes from instances and other values,
we will follow the convention of prefixing with @litchar{$} the names of prototypes.
Thus, the following prototype extends or overrides its super-record with
slot @r[x] bound to @r[3]:
@Examples[
(code:comment "$x3 : (Proto (Fun 'x -> Nat | A) A)")
(define ($x3 self super) (λ (msg) (if (eq? msg 'x) 3 (super msg))))
]

@(noindent)
This prototype extends a record with a new slot @r[z]
bound to a complex number computed from real and imaginary values
bound to the respective slots @r[x] and @r[y] of its @r[self]:
@Examples[
(code:comment "$z<-xy : (Proto (Fun (Or 'x 'y) -> Real | 'z -> Complex | A)")
(code:comment "                (Fun (Or 'x 'y) -> Real | A))")
(define ($z<-xy self super)
  (λ (msg) (case msg
             ((z) (+ (self 'x) (* 0+1i (self 'y))))
             (else (super msg)))))
]

@(noindent)
That prototype doubles the number in slot @r[x] that it @emph{inherits} from its @r[super] record:
@Examples[
(code:comment "$double-x : (Proto (Fun 'x -> Number | A) (Fun 'x -> Number | A))")
(define ($double-x self super)
  (λ (msg) (if (eq? msg 'x) (* 2 (super 'x)) (super msg))))
]

@(noindent)
More generally a record prototype extends its @r[super] record with new slots
and/or overrides the values bound to its existing slots, and may in the
process refer to both the records @r[self] and @r[super] and their slots,
with some obvious restrictions to avoid infinite loops from circular definitions.

@subsubsection{Basic testing} How to test these prototypes?
With the @r[fix] operator using the above record @r[x1-y2] as base value:

@Checks[
(code:comment "x3-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x3-y2 (fix $x3 x1-y2))
(eval:check (list (x3-y2 'x) (x3-y2 'y)) '(3 2))

(code:comment "z1+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)")
(define z1+2i (fix $z<-xy x1-y2))
(eval:check (list (z1+2i 'x) (z1+2i 'y) (z1+2i 'z)) '(1 2 1+2i))

(code:comment "x2-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x2-y2 (fix $double-x x1-y2))
(eval:check (list (x2-y2 'x) (x2-y2 'y)) '(2 2))
]

@(noindent)
We can also @r[mix] these prototypes together before to compute the @r[fix]:
@Checks[
(code:comment "z6+2i : (Fun 'x -> Nat | 'y -> Nat | 'z -> Complex)")
(define z6+2i (fix (mix $z<-xy (mix $double-x $x3)) x1-y2))
(eval:check (map z6+2i '(x y z)) '(6 2 6+2i))
]

@(noindent)
And since the @r[$z<-xy] prototype got the @r[x] and @r[y] values
from @r[self] and not @r[super], we can freely commute it with the other two prototypes
that do not affect either override slot @r[z] or inherit from it:
@Checks[
(eval:check (list ((fix (mix $z<-xy (mix $double-x $x3)) x1-y2) 'z)
                  ((fix (mix $double-x (mix $z<-xy $x3)) x1-y2) 'z)
                  ((fix (mix $double-x (mix $x3 $z<-xy)) x1-y2) 'z))
            '(6+2i 6+2i 6+2i))
]

@(noindent)
@r[mix] is associative, and therefore the following forms are equivalent to the previous ones,
though the forms above (folding right) are slightly more efficient than the forms below (folding left):
@Checks[
(eval:check (list ((fix (mix (mix $z<-xy $double-x) $x3) x1-y2) 'z)
                  ((fix (mix (mix $double-x $z<-xy) $x3) x1-y2) 'z)
                  ((fix (mix (mix $double-x $x3) $z<-xy) x1-y2) 'z))
            '(6+2i 6+2i 6+2i))
]

@(noindent)
Now, since @r[$double-x] inherits slot @r[x] that @r[$x3] overrides,
there is clearly a dependency between the two that prevents them from commuting:
@Checks[
(code:comment "x6-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x6-y2 (fix (mix $double-x $x3) x1-y2))
(code:comment "x3-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x3-y2 (fix (mix $x3 $double-x) x1-y2))
(eval:check (list (x6-y2 'x) (x3-y2 'x)) '(6 3))
]

@subsubsection{Building record prototypes}

Here are general utility functions to build record prototypes.
To define a record with a slot @r[k] mapped to a value @r[v], use:
@Definitions[
(code:comment "$slot : (Fun k:Symbol V -> (Proto (Fun 'k -> V | A) A))")
(define ($slot k v) (code:comment "k v: constant key and value for this defined slot")
  (λ (self super) (code:comment "self super: usual prototype variables")
    (λ (msg) (code:comment "msg: message received by the instance, a.k.a. method name.")
      (if (equal? msg k) v (code:comment "if the message matches the key, return the value")
        (super msg))))) (code:comment "otherwise, recurse to the super instance")
]

@(noindent)
What of inheritance? Well, we can modify an inherited slot using:
@Definitions[
(code:comment "$slot-modify : (Fun k:Symbol (Fun V -> W)")
(code:comment "  -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))")
(define ($slot-modify k modify) (code:comment "modify: function from super-value to value")
  (λ (self super) (λ (msg)
    (if (equal? msg k) (modify (super msg))
      (super msg)))))
]

@(noindent)
What if a slot depends on other slots? We can use this function
@Definitions[
(code:comment "$slot-compute : (Fun k:Symbol (Fun A -> V) -> (Proto (Fun 'k -> V | A) A))")
(define ($slot-compute k fun) (code:comment "fun: function from self to value.")
  (λ (self super)
    (λ (msg)
      (if (equal? msg k) (fun self)
        (super msg)))))
]

@(noindent)
A general function to compute and override a slot would take as arguments both @r[self]
and a function to @r[inherit] the super slot value,
akin to @r[call-next-method] in CLOS@~cite{gabriel1991clos}:
@Definitions[
(code:comment "$slot-gen : (Fun k:Symbol (Fun A (Fun -> V) -> W)")
(code:comment "  -> (Proto (Fun 'k -> W | A) (Fun 'k -> V | A) A) st: (<: V W))")
(define ($slot-gen k fun) (code:comment "fun: from self and super-value thunk to value.")
  (λ (self super)
    (λ (msg)
      (define (inherit) (super msg))
      (if (equal? msg k) (fun self inherit) (inherit)))))
]

@(noindent)
We could redefine the former ones in terms of that latter:
@Examples[
(define ($slot k v) ($slot-gen k (λ (__self __inherit) v)))
(define ($slot-modify k modify) ($slot-gen k (λ (__ inherit) (modify (inherit)))))
(define ($slot-compute k fun) ($slot-gen k (λ (self __) (fun self))))
]

@(noindent)
Thus you can re-define the above prototypes as:
@Examples[
(define $x3 ($slot 'x 3))
(define $double-x ($slot-modify 'x (λ (x) (* 2 x))))
(define $z<-xy ($slot-compute 'z (λ (self) (+ (self 'x) (* 0+1i (self 'y))))))
]

@(noindent)
Here is a universal bottom function to use as the base for fix:
@Definitions[
(code:comment "bottom-record : (Fun Symbol -> _)")
(define (bottom-record msg) (error "unbound slot" msg))
]

@(noindent)
To define a record with a single slot @r[x] bound to @r[3], we can use:
@Checks[
(code:comment "x3 : (Fun 'x -> Nat)")
(define x3 (fix $x3 bottom-record))
(eval:check (x3 'x) 3)
]

@(noindent)
To define a record with two slots @r[x] and @r[y] bound to @r[1]
and @r[2] respectively, we can use:
@Checks[
(code:comment "x1-y2 : (Fun 'x -> Nat | 'y -> Nat)")
(define x1-y2 (fix (mix ($slot 'x 1) ($slot 'y 2)) bottom-record))
(eval:check (map x1-y2 '(x y)) '(1 2))
]

@subsection{Prototype Basics}

@subsubsection{Long-form Basics}

Let's rewrite @r[fix] and @r[mix] in long-form
with variables @r[self] and @r[super] instead of @r[f] and @r[b], etc.
Thus, the instantiation function @r[(fix p b)] becomes:
@Definitions[
(code:comment "instantiate-prototype : (Fun (Proto Self Super) Super -> Self)")
(define (instantiate-prototype prototype base-super)
  (define self (prototype (λ i (apply self i)) base-super))
  self)
]

@(noindent)
A more thorough explanation of the above fixed-point function is in @(AppendixD).
Meanwhile, the composition function @r[(mix p q)] becomes:
@Definitions[
(code:comment "compose-prototypes : (Fun (Proto Self Super) (Proto Super Super2)")
(code:comment "  -> (Proto Self Super2) st: (<: Self Super Super2))")
(define (compose-prototypes child parent)
  (λ (self super2) (child self (parent self super2))))
]

@(noindent)
Note the types of the variables and intermediate expressions:
@tabular[
(list (list
@racketblock[
child : (Proto Self Super)
self : Self
(child self (parent self super2)) : Self]
@racketblock[
parent : (Proto Super Super2)
super2 : Super2
(parent self super2) : Super]))]

@(noindent)
When writing long-form functions, instead of vying for conciseness, we will
use the same naming conventions as in the function above:
@itemize[
 @item{@r[child] for a @emph{prototype} at hand, in leftmost position;}
 @item{@r[parent] for a @emph{prototype} that is being mixed with, in latter position;}
 @item{@r[self] for the @emph{instance} that is a fixed point of the computation;}
 @item{@r[super] for the @emph{instance} so-far accumulated or at the base of the computation.}
]

Note the important distinction between @emph{prototypes} and @emph{instances}.
Instances are the non-composable complete outputs of instantiation,
whereas prototypes are the composable partial inputs of instantiation.
Prototypes, increments of computation, are functions from an instance (of a
subtype @r[Self]) and another instance (of a supertype @r[Super])
to yet another instance of the subtype @r[Self].

@subsubsection{Composing prototypes}

The identity prototype as follows is a neutral element for mix.
It does not override any information from the super/base instance,
but only passes it through. It also does not consult information in
the final fixed-point nor refers to it.
@Definitions[
(code:comment "identity-prototype : (Proto Instance Instance)")
(define (identity-prototype self super) super)
]

@(noindent)
Now, prototypes are interesting because @r[compose-prototypes] (a.k.a. @r[mix])
is an associative operator with neutral element @r[identity-prototype].
Thus prototypes form a monoid, and you can compose or instantiate a list of prototypes:
@Definitions[
(code:comment "compose-prototype-list : (Fun (IndexedList I (λ (i)")
(code:comment "  (Proto (A_ i) (A_ (1+ i))))) -> (Proto (A_ 0) (A_ (Card I))))")
(define (compose-prototype-list prototype-list)
  (foldr compose-prototypes identity-prototype prototype-list))
(code:comment "instantiate-prototype-list : (Fun (IndexedList I (λ (i)")
(code:comment "  (Proto (A_ i) (A_ (1+ i))))) (A_ (Card I)) -> (A_ 0))")
(define (instantiate-prototype-list prototype-list base-super)
  (instantiate-prototype (compose-prototype-list prototype-list) base-super))
]

@;|{@Definitions[
(code:comment "compose-prototype-list : (Fun (IndexedList I (λ (i)")
(code:comment "  (Proto (A_ i) (A_ (1+ i))))) -> (Proto (A_ 0) (A_ (Card I))))")
(define (compose-prototype-list l)
  (cond
   ((null? l) identity-prototype)
   ((null? (cdr l)) (car l))
   ((null? (cddr l)) (compose-prototypes (car l) (cadr l)))
   (else (compose-prototypes (car l) (compose-prototype-list (cdr l))))))
]
@; TODO: if we need space, delete the above or move it to an appendix.

@(noindent)
A more succint way to write the same function is:
@Examples[
(define (compose-prototype-list prototype-list)
  (foldr compose-prototypes identity-prototype prototype-list))
]

@(noindent)
You can also instantiate a list of prototypes:
@Definitions[
(code:comment "instantiate-prototype-list : (Fun (IndexedList I (λ (i)")
(code:comment "  (Proto (A_ i) (A_ (1+ i))))) (A_ (Card I)) -> (A_ 0))")
(define (instantiate-prototype-list prototype-list base-super)
  (instantiate-prototype (compose-prototype-list prototype-list) base-super))
]

@(noindent)
And we can define a syntactic short-cut that composes prototypes in its list of arguments:
@Definitions[
(define ($compose . proto-list) (compose-prototype-list proto-list))
]
}|

@subsubsection{The Bottom of it}

In a language with partial functions, such as Scheme, there is a practical
choice for a universal base super instance to pass to
@r[instantiate-prototype]: the @r[bottom] function, that never returns,
but instead, for enhanced usability, may throw a helpful error.
@Definitions[
(code:comment "bottom : (Fun I ... -> O ...)")
(define (bottom . args) (error "bottom" args))
]

@(noindent)
Thus, in dialects with optional arguments, we could make @r[bottom] the default
value for @r[base-super]. Furthermore, in any variant of Scheme, we can define
the following function @r[instance] that takes the rest of its arguments as
a list of prototypes, and instantiates the composition of them:
@Definitions[
(code:comment "instance : (Fun (IndexedList I (λ (i) (Proto (A_ i) (A_ (1+ i)))))...")
(code:comment "  -> (A_ 0)))")
(define (instance . prototype-list)
  (instantiate-prototype-list prototype-list bottom))
]

@(noindent)
What if you @emph{really} wanted to instantiate your list of prototypes with some
value @r[b] as the base super instance? “Just” tuck
@r[($constant-prototype b)] at the tail end of your prototype list:
@Definitions[
(code:comment "$constant-prototype : (Fun A -> (Proto A _))")
(define ($constant-prototype base-super) (λ (__self __super) base-super))
]
@;{
@(noindent)
Or the same with a shorter name and a familiar definition as a combinator
@Definitions[
(code:comment "$const : (Fun A -> (Proto A _))")
(define ($const b) (λ _ b))
]}

@(noindent)
Small puzzle for the points-free Haskellers reading this essay:
what change of representation will enable prototypes to be composed like regular functions
without having to apply a binary function like @r[mix]? Solution in footnote.@note{
A general purpose trick to embed an arbitrary monoid into usual composable functions is
to curry the composition function (here, @r[mix]) with the value to be composed.
Thus, the functional embedding of prototype @r[p] will be
@r[(λ (q) (mix p q)) : (Fun (Proto Super S2) -> (Proto Self S2))].
To recover @r[p] from that embedding, just apply it to @r[identity-prototype].
}

@subsubsection{Single Inheritance}
Most object systems do not offer programmers prototype composition,
but only a much less expressive operation: prototype application.

Consider a @emph{generator} to be a function from a function type to itself.
To instantiate a generator is to compute its fixed point.
There is a surjection from prototype and base value to generator,
such that instantiating the generator is same as
instantiating the prototype with the base value.
Applying prototype @r[p] to the generator obtained
from prototype @r[q] and base value @r[b] is the same as
obtaining a generator from @r[(mix p q)] and @r[b].
@Definitions[
(code:comment "(deftype (Generator A) (Fun A -> A))")
(code:comment "fix-generator : (Fun (Generator A) -> A)")
(define (fix-generator g) (define f (g (λ i (apply f i)))) f)
(code:comment "proto->generator : (Fun (Proto A B) B -> (Generator A))")
(define (proto->generator p b) (λ (f) (p f b)))
(code:comment "(== (fix-generator (proto->generator p b)) (fix p b))")
(code:comment "apply-proto : (Fun (Proto A B) (Generator B) -> (Generator A))")
(define (apply-proto p g) (λ (f) (p f (g f))))
(code:comment "(== (apply-proto p (proto->generator q b)) (proto->generator (mix p q) b))")
]

“Single inheritance” consists in prototypes being second-class entities
that you can only apply to generators immediately after having defined them,
with a fixed base instance for all generators.
Instead of being able to abstract over prototypes and compose them,
you can only use constant prototypes and apply them to generators
(that themselves are usually also second-class entities but can be abstracted over somewhat).
To incrementally define an instance, you must @r[cons] prototypes one by one onto
the list to instantiate.

Composition of first-class prototypes is obviously more expressive than single-inheritance.
You can @r[mix] prototypes or @r[append] prototype lists, and
programmatically compose code from many increments of code.
Prototypes are thus more akin to the “mixins” or “traits” of more advanced
objects systems@~cite{Cannon82 bracha1990mixin Flatt06schemewith}.
Prototype composition however, does not by itself subsume multiple inheritance,
that we will study in @(section4).

@section[#:tag "pure_objective_fun"]{Pure Objective Fun}

@subsection{Using prototypes to incrementally define simple data structures}

@subsubsection{Prototypes for Order}

Let's use prototypes to build some simple data structures.
First, we'll write prototypes that offer an abstraction for the ability
to compare elements of a same type at hand, in this case,
either numbers or strings.
@Definitions[
(define ($number-order self super)
  (λ (msg) (case msg ((<) (λ (x y) (< x y)))
                     ((=) (λ (x y) (= x y)))
                     ((>) (λ (x y) (> x y)))
                     (else (super msg)))))
(define ($string-order self super)
  (λ (msg) (case msg ((<) (λ (x y) (string<? x y)))
                     ((=) (λ (x y) (string=? x y)))
                     ((>) (λ (x y) (string>? x y)))
                     (else (super msg)))))
]

@(noindent)
We can add a “mixin” for a @r[compare] operator that summarizes in one call
the result of comparing two elements of the type being described.
A mixin is a prototype meant to extend other prototypes.
See how this mixin can be used to extend either of the prototypes above.
Also notice how, to refer to other slots in the eventual instance,
we call @r[(self '<)] and such.
@Definitions[
(define ($compare<-order self super)
  (λ (msg) (case msg
             ((compare) (λ (x y) (cond (((self '<) x y) '<)
                                       (((self '>) x y) '>)
                                       (((self '=) x y) '=)
                                       (else (error "incomparable" x y)))))
             (else (super msg)))))
(define number-order (instance $number-order $compare<-order))
(define string-order (instance $string-order $compare<-order))]
@Checks[
(eval:check (list ((number-order '<) 23 42) ((number-order 'compare) 8 4)
  ((string-order '<) "Hello" "World") ((string-order 'compare) "Foo" "FOO")
  ((string-order 'compare) "42" "42"))
  '(#t > #t > =))
]

@(noindent)
We can define an order on symbols by delegating to strings!
@Definitions[
(define ($symbol-order self super)
  (λ (msg) (case msg
             ((< = > compare)
              (λ (x y) ((string-order msg) (symbol->string x) (symbol->string y))))
             (else (super msg)))))
(define symbol-order (instance $symbol-order))]
@Checks[
(eval:check (list ((symbol-order '<) 'aardvark 'aaron) ((symbol-order '=) 'zzz 'zzz)
  ((symbol-order '>) 'aa 'a) ((symbol-order 'compare) 'alice 'bob)
  ((symbol-order 'compare) 'b 'c) ((symbol-order 'compare) 'c 'a))
  '(#t #t #t < < >))
]

@subsubsection{Prototypes for Binary Trees}

We can use the above @r[order] prototypes to build binary trees
over a suitable ordered key type @r[Key].
We'll represent a tree as a list of left-branch,
list of key-value pair and ancillary data, and right-branch,
which preserves the order of keys when printed:
@Definitions[
(define ($binary-tree-map self super)
  (λ (msg)
    (define (node l kv r) ((self 'node) l kv r))
    (case msg
      ((empty) '())
      ((empty?) null?)
      ((node) (λ (l kv r) (list l (list kv) r)))
      ((singleton) (λ (k v) (node '() (cons k v) '())))
      ((acons)
       (λ (k v t)
         (if ((self 'empty?) t) ((self 'singleton) k v)
             (let* ((tl (car t)) (tkv (caadr t)) (tk (car tkv)) (tr (caddr t)))
               (case (((self 'Key) 'compare) k tk)
                 ((=) (node tl (cons k v) tr))
                 ((<) (node ((self 'acons) k v tl) tkv tr))
                 ((>) (node tl tkv ((self 'acons) k v tr))))))))
      ((ref)
       (λ (t k e)
         (if ((self 'empty?) t) (e)
             (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
               (case (((self 'Key) 'compare) k tk)
                 ((=) tv)
                 ((<) ((self 'ref) tl k e))
                 ((>) ((self 'ref) tr k e)))))))
      ((afoldr)
       (λ (acons empty t)
         (if ((self 'empty?) t) empty
           (let ((tl (car t)) (tk (caaadr t)) (tv (cdaadr t)) (tr (caddr t)))
             ((self 'afoldr)
              acons (acons tk tv ((self 'afoldr) acons empty tl)) tr)))))
      (else (super msg)))))]

@(noindent)
With this scaffolding, we can define a dictionary data structure
that we can use later to differently represent objects:
@Definitions[
(define symbol-tree-map (instance ($slot 'Key symbol-order) $binary-tree-map))
]
@(noindent)
However, when we use it, we immediately find an issue:
trees will too often be skewed, leading to long access times,
especially so when building them from an already ordered list:
@Checks[
(define my-binary-dict (code:comment "heavily skewed right, height 5")
  (foldl (λ (kv t) ((symbol-tree-map 'acons) (car kv) (cdr kv) t))
         (symbol-tree-map 'empty)
         '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))
(eval:check my-binary-dict '(() ((a . "I")) (() ((b . "II")) (() ((c . "III")) (() ((d . "IV")) (() ((e . "V")) ()))))))]

@(noindent)
But binary trees otherwise work:
@Checks[
(eval:check (map (λ (k) ((symbol-tree-map 'ref) my-binary-dict k (λ () #f))) '(a b c d e z))
            '("I" "II" "III" "IV" "V" #f))]

@subsubsection{Prototypes for @emph{Balanced} Binary Trees}

We can incrementally define a balanced tree data structure
(in this case, using the AVL balancing algorithm)
by overriding a single method of the original binary tree prototype:
@Definitions[
(define ($avl-tree-rebalance self super)
  (λ (msg)
    (define (left t) (car t))
    (define (kv t) (caadr t))
    (define (height t) (if (null? t) 0 (cdadr t)))
    (define (right t) (caddr t))
    (define (balance t) (if (null? t) 0 (- (height (right t)) (height (left t)))))
    (define (mk l kv r)
      (let ((lh (height l)) (rh (height r)))
        (or (member (- rh lh) '(-1 0 1)) (error "tree unbalanced!"))
        (list l (cons kv (+ 1 (max lh rh))) r)))
    (define (node l ckv r)
      (case (- (height r) (height l))
        ((-1 0 1) (mk l ckv r))
        ((-2) (case (balance l)
                ((-1 0) (mk (left l) (kv l) (mk (right l) ckv r))) ;; LL rebalance
                ((1) (mk (mk (left l) (kv l) (left (right l))) ;; LR rebalance
                         (kv (right l)) (mk (right (right l)) ckv r)))))
        ((2) (case (balance r)
               ((-1) (mk (mk l ckv (left (left r))) ;; RL rebalance
                         (kv (left r)) (mk (right (left r)) (kv r) (right r))))
               ((0 1) (mk (mk l ckv (left r)) (kv r) (right r))))))) ;; RR rebalance
    (case msg ((node) node) (else (super msg)))))
(define Dict
  (instance $avl-tree-rebalance $binary-tree-map ($slot 'Key symbol-order)))]

@(noindent)
Our dictionary is now well-balanced, height 3, and the tests still pass:
@Checks[
(define my-avl-dict
  (foldl (λ (kv t) ((Dict 'acons) (car kv) (cdr kv) t))
         (Dict 'empty)
         '((a . "I") (b . "II") (c . "III") (d . "IV") (e . "V"))))
(eval:check my-avl-dict
                '((() ((a . "I") . 1) ()) ((b . "II") . 3)
                  ((() ((c . "III") . 1) ()) ((d . "IV") . 2) (() ((e . "V") . 1) ()))))
(eval:check (map (λ (k) ((Dict 'ref) my-avl-dict k (λ () #f))) '(a b c d e z))
            '("I" "II" "III" "IV" "V" #f))
]

@section[#:tag "beyond_objects"]{Beyond Objects and back}

Prototypes are not just for records as functions from symbol to value:
they can be used to incrementally specify any kind of functions!

@subsection{Prototypes for Numeric Functions}

Let's see how prototypes can be used to build functions from real numbers to real numbers.
The following prototype is a mixin for an even function,
that delegates to other prototypes the images of positive values
and returns the image of the opposite for negative values:
@Examples[
(define ($even self super) (λ (x) (if (< x 0) (self (- x)) (super x))))
]
@(noindent)
The following prototype is a mixin for taking the cube of the parent value:
@Examples[
(define ($cube self super) (λ (x) (let ((y (super x))) (* y y y))))
]
@(noindent)
We can instantiate a function out of those of prototypes, and test it:
@Checks[
(define absx3 (instance $even $cube ($constant-prototype (λ (x) x))))
(eval:check (map absx3 '(3 -2 0 -1)) '(27 8 0 1))
]

@subsubsection{Number Thunks}

Now, the simplest numeric functions are thunks: nullary functions that yield a number.
@Checks[
(define (p1+ __ b) (λ () (+ 1 (b))))
(define (p2* __ b) (λ () (* 2 (b))))
(eval:check (list ((fix (mix p1+ p2*) (λ () 30)))
                  ((fix (mix p2* p1+) (λ () 30))))
            '(61 62))
]

@subsection[#:tag "computations_not_values"]{Prototypes are for Computations not Values}

Prototypes for number thunks can be generalized to prototypes for any kind of thunks:
you may incrementally specify instances of arbitrary types using prototypes,
by first wrapping values of that type into a thunk.
An alternative to thunks would be to use Scheme's @r[delay] special form,
or whatever form of @emph{lazy evaluation} is available in the language at hand.

To reprise the Call-By-Push-Value paradigm@~cite{conf/tlca/Levy99},
prototypes incrementally specify @emph{computations} rather than @emph{values}.
In applicative languages we can reify these computations as values
either as functions (as in @(section1)), or as delayed values as follows:

@Definitions[
(code:comment "(deftype (δProto Self Super) (Fun (Delayed Self) (Delayed Super) -> Self))")
(code:comment "δfix : (Fun (δProto Self Super) (Delayed Super) -> (Delayed Self))")
(define (δfix p b) (define f (delay (p f b))) f)
(code:comment "δmix : (Fun (δProto Self Mid) (δProto Mid Super) -> (δProto Self Super))")
(define (δmix c p) (λ (f b) (c f (delay (p f b)))))
(code:comment "δ$id : (δProto X X)")
(define (δ$id f b) (λ (__self super) (force super)))
]

@(noindent)
Now, most interesting prototypes will only lead to error or divergence if you try
to instantiate them by themselves---they are “mixins”,
just like @r[$compare<-order] or @r[$avl-tree-rebalance] above,
designed to be combined with other prototypes.
Indeed, the entire point of incremental programming is that you want to
define and manipulate fragments that are not complete specifications.
To use these fragments in a total language where all computations terminate
will require attaching to each prototype some side-condition as to
which aspects of a computation it provides that other prototypes may rely on,
and which aspects of a computation it requires that other prototypes must provide.

@subsection[#:tag "without_subtyping"]{Prototypes are Useful Even Without Subtyping}

The above prototypes for numeric functions also illustrate that
even if a language's only subtyping relationship is the identity
whereby each type is the one and only subtype or itself
(or something slightly richer with syntactic variable substitution in parametric polymorphism),
then you can use prototypes to incrementally specify computations,
with the following monomorphic types:
@racketblock[
(code:comment "(deftype (MProto A) (Fun A A -> A))")
(code:comment "fix : (Fun (MProto A) A -> A)")
(code:comment "mix : (Fun (MProto A) (MProto A) -> (MProto A))")
]
@(noindent)
As per the previous discussion, if the language does not use lazy evaluation,
@r[A]'s may be constrained to be functions, or to they may have to be wrapped
inside a @r[Lazy] or @r[delay] special form.

Lack of subtyping greatly reduces the expressive power of prototypes;
yet, as we'll see in @(section5),
the most popular use of prototypes in Object-Oriented Programming is completely monomorphic:
prototypes for type descriptors, a.k.a. classes;
only this common use of prototypes happens at the meta-level,
classes being second-class objects (pun not intended by unaware practitioners).

@section[#:tag "Better_objects"]{Better objects, still pure}

In @(section1),
we implemented a rudimentary object system on top of prototypes.
Compared to mainstream object systems, it not only was much simpler to define,
but also enabled mixins, making it more powerful than common single-inheritance systems.
Still, many bells and whistles found in other object systems are missing.
Now that we have a nice and simple semantic framework for “objects”,
can we also reimplement the more advanced features of object systems of yore?

@subsection[#:tag "slot_introspection"]{Slot Introspection}

@subsubsection{Bug and feature}
When representing records as functions from symbol to value,
it is not generally possible to access the list of symbols that constitute valid keys.
Most object systems do not allow for this kind of introspection at runtime,
and further introduce scoping or access control rules to enable modular reasoning about slot accesses,
advertising such restrictions as “encapsulation” or “information hiding” features.
Yet, introspection can be useful to e.g. automatically input and output
human-readable or network-exchangeable representations of an instance,
and has also been advertised as a “reflection” feature.
Some languages even offer both contradictory sets of features.

@subsubsection{Same concrete representation, abstract constructor}
Slot introspection can be achieved while keeping records as functions from keys to values,
by adding a “special” key, e.g. @r[keys],
that will be bound to the list of valid keys.
To save themselves the error-prone burden of maintaining the list of keys by hand,
programmers would then use a variant of @r[$slot-gen] that maintains this list, as in:
@Definitions[
(define ($slot-gen/keys k fun)
  (λ (self super)
    (λ (msg) (cond ((equal? msg k) (fun self (λ () (super msg))))
                   ((equal? msg 'keys) (cons k (super 'keys)))
                   (else (super msg))))))
]

@subsubsection{Different instance representation}
Slot introspection can also be achieved by using a different representation for records.
Instead of functions, mappings from symbols to value could be represented
using hash-tables, or, preferrably in a pure setting, balanced trees.
Purposefully, we implemented such balanced trees in @(section2).
Thus, we could represent records as thunks that return a @r[symbol-avl-map], as follows:
@Definitions[
(define ($slot-gen/dict k fun)
  (λ (self super)
    (define (inherit) ((Dict 'ref) (super) k bottom))
    (λ () ((Dict 'acons) k (fun self inherit) (super)))))
]
@(noindent)
However, while the above definition yields the correct result,
it potentially recomputes the entire dictionary @r[(super)] twice at every symbol lookup,
with exponential explosion as the number of super prototypes increases.
We could carefully implement sharing by precomputing @r[(define super-dict (super))].
But the instance is itself a thunk that would be recomputing the entire dictionary at every call,
and is better evaluated only once.
Now, if we adopt lazy evaluation, we can automatically share results across
multiple copies of a computation as well as multiple consumers of a same copy,
without having to manually reimplement this caching every time.
Thus, using @r[δfix] and @r[δmix] instead of @r[fix] and @r[mix], we can have:
@Definitions[
(define (δ$slot-gen/dict k fun)
  (λ (self super)
    (delay (let ((inherit ((Dict 'ref) (force super) k (λ () (delay (bottom))))))
             ((Dict 'acons) k (delay (fun self inherit)) (force super))))))
]

@subsubsection[#:tag "different_prototype"]{Same instance representation, different prototype representation}
Finally, slot introspection can be achieved while preserving the same instance representation
by instead changing the representation for @emph{prototypes}.
Though the concise functional representation we offered gives an enlightening
expression of the essence of object systems, we can maintain equivalent semantics
with a more efficient implementation, for instance a pair @r[pk] of
the same prototype as before and a list of keys. The @r[mix] and @r[fix] functions become:
@Definitions[
(define (mix/pk child parent)
  (cons (mix (car child) (car parent))
        (append (cdr child) (cdr parent))))
(define (fix/pk proto base)
  (fix (mix ($slot 'keys (cdr proto)) (car proto)) base))
]
@(noindent)
One could represent sets of keys by a data structure with a union more efficient than list @r[append].
One could also put @r[$slot] invocation after the @r[(car proto)] rather than before it,
to allow prototypes to intercept slot introspection.

@subsubsection{Abstracting over representations}
We can generalize the above solutions by realizing that
both instances and prototypes may be
may be wrapped, unwrapped or otherwise represented in a variety of ways,
and augmented with various other information,
when designing and implementing an object system for usability and performance.
Yet, the representation given in @(section1) is a good guide to the semantics of OOP,
and the conceptual distinction between instance and prototype is instrumental to keeping this semantics
simple and understandable.

@subsection[#:tag "unifying_instance_prototype"]{Unifying Instances and Prototypes}

@subsubsection{OOP without Objects}
We have so far insisted on the distinction between instance and prototype.
Each embody one aspect of what is usually meant by “object”,
yet neither embody all of it.
We are thus in a strange situation wherein we have been doing
“Object-Oriented Programming” without any actual object!

This is a practical problem, not a mere curiosity, as this distinction
makes it painful to write extensible specifications for nested or recursive data structures:
you need to decide for each slot of each data structure at each stage of computation
whether it contains an extensible prototype or an instance to call the proper API.
This incurs both a psychological cost to programmers and a runtime cost to evaluation,
that without careful design may in the worst case cause an exponential explosion
of the code the programmers must write and/or of the recomputations of sub-expressions
that the evaluator will do at runtime.

@subsubsection{Conflation without confusion}
Jsonnet and Nix (see @(section7)) both confront and elegantly solve the above issue,
and in the very same way, with one small trick:
they bundle and conflate together instance and prototype in a same single entity, the “object”.
Indeed, in a pure context, and given the base super value for the given prototype representation,
there is one and only one instance associated to a prototype up to any testable equality,
and so we may usefully cache the computation of this instance together with the prototype.
The same “object” entity can thus be seen as an instance and queried for method values,
or seen as a prototype and composed with other objects (also seen as prototypes) into a new object.

Thanks to the conflation of instance and prototype as two aspects of a same object,
configurations can be written that can refer to other parts of the configuration
without having to track and distinguish which parts are instantiated at which point,
and it all just works.
Still, distinguishing the two concepts of instance and prototype is important
to dispel the confusion that can often reign in even the most experienced OO practitioner
regarding the fine behavior of objects when trying to assemble or debug programs.

In Jsonnet, this conflation is done implicitly as part of the builtin object system implementation.
In Nix, interestingly, there are several unassuming variants of the same object system,
that each store the prototype information in one or several special fields of the @r[attrset]
that is otherwise used for instance information.
Thus, in the simplest Nix object system, one could write:
@nix|{
  fix' (self: { x = 1; y = 2 + self.x; })
}|
and it would evaluate to an attrset equivalent to:
@nix|{
  { x = 1; y = 3; __unfix__ = self: { x = 1; y = 2 + self.x; }; }
}|
Nix contains many variants of the same object system, that use one or several of
@r[extend], @r[override], @r[overrideDerivation], @r[meta], etc., instead of @tt{__unfix__}
to store composable prototype information.

@subsubsection{Practical Conflation for Fun and Profit}
In the rest of this article, we'll represent an object as a pair of an instance and a prototype:
@Definitions[
(define (make-object instance prototype) (cons instance prototype))
(define (object-instance object) (car object))
(define (object-prototype object) (cdr object))
]

@(noindent)
In a more robust implementation, we would use an extension to the language Scheme
to define a special structure type for objects as pairs of instance and prototype,
disjoint from the regular pair type.
Thus, we can distinguish objects from regular lists, and
hook into the printer to offer a nice way to print instance information
that users are usually interested in while skipping prototype information
that they usually are not.

To reproduce the semantics of Jsonnet,
instances will be a delayed @r[Dict] (as per @(section2)),
mapping symbols as slot names to delayed values as slot computations;
meanwhile the prototype will be a prototype wrapper of type @r[(δProto Object Object)]
(as in @seclink["computations_not_values"]{section 4.1.3}),
that preserves the prototype while acting on the instance.
Thus the basic function @r[slot-ref] to access a slot, and
the basic prototypes to define one, analogous to the above
@r[$slot-gen], @r[$slot], @r[$slot-modify], @r[$slot-compute] are as follow:
@Definitions[
(define (slot-ref object slot)
  (force ((Dict 'ref) (force (object-instance object)) slot bottom)))
(define ($slot/gen k fun)
  (λ (self super)
    (make-object ((Dict 'acons) k (fun self (delay (slot-ref super k)))
                  (object-instance (force super)))
                 (object-prototype (force super)))))
(define ($slot/value k v) ($slot/gen k (λ (__self __inherit) (delay v))))
(define ($slot/modify k modify)
  ($slot/gen k (λ (__ inherit) (delay (modify (force inherit))))))
(define ($slot/compute k fun) ($slot/gen k (λ (self __) (delay (fun self)))))
]

@(noindent)
And for the common case of just overriding some slots with constant values, we could use:
@Definitions[
(define ($slot/values . kvs)
  (if (null? kvs) identity-prototype
    (compose-prototypes ($slot/value (car kvs) (cadr kvs))
                        (apply $slot/values (cddr kvs)))))
]

@subsection[#:tag "multiple_inheritance"]{Multiple Inheritance}

@; TODO: citations required on modularity, inheritance, multiple inheritance
@subsubsection{The inheritance modularity issue}
To write incremental OO programs, developers need to be able to express dependencies
between objects, such that an object @r[Z]
depends on super objects @r[K1], @r[K2], @r[K3] being present after it
in the list of prototypes to be instantiated.
We'll also say that @r[Z] @emph{inherits from} from these super objects,
or @emph{extends} them, or that they are its direct super objects.

But what if @r[K1], @r[K2], @r[K3] themselves
inherit from super objects @r[A], @r[B], @r[C], @r[D], @r[E],
e.g. with @r[K1] inheriting from direct supers @r[A B C],
@r[K2] inheriting from direct supers @r[D B E], and
@r[K3] inheriting from direct supers @r[D A], and what more
each of @r[A], @r[B], @r[C], @r[D], @r[E] inheriting from a base super object @r[O]?
(See @(AppendixA) for details on this example.)

With the basic object model offered by Nix, Jsonnet, or our @(section1),
these dependencies could not be represented in the prototype itself.
If the programmer tried to “always pre-mix” its dependencies into a prototype,
then @r[K1] would be a pre-mix @r[K1 A B C O],
@r[K2] would be a pre-mix @r[K2 D B E O],
@r[K3] would be a pre-mix @r[K3 D A O],
and when trying to specify @r[Z], the pre-mix
@r[Z K1 A B C O K2 D B E O K3 D A O] would be incorrect,
with unwanted repetitions of @r[A], @r[B], @r[D], @r[O]
redoing their effects too many times and possibly undoing overrides made by @r[K2] and @r[K3].
Instead, the programmer would have to somehow remember and track those dependencies,
such that when he instantiates @r[Z], he will not write just @r[Z],
but @r[Z] followed a topologically sorted @emph{precedence list}
where each of the transitive dependencies appears once and only once,
e.g. @r[Z K1 K2 K3 D A B C E O].

Not only does this activity entail a lot of tedious and error-prone bookkeeping,
it is not modular.
If these various objects are maintained by different people as part of separate libraries,
each object's author must keep track not just of their direct dependencies,
but all their transitive indirect dependencies, with a proper ordering.
Then they must not only propagate those changes to their own objects,
but notify the authors of objects that depend on theirs.
To get a change fully propagated might required hundreds of modifications
being sent and accepted by tens of different maintainers, some of whom might not be responsive.
Even when the sets of dependencies are properly propagated, inconsistencies between
the orders chosen by different maintainers at different times may cause subtle miscalculations
that are hard to detect or debug.
In other words, while possible, manual maintenance of precedence lists is a modularity nightmare.

@subsubsection{Multiple inheritance to the rescue}
With multiple inheritance, programmers only need declare the dependencies
between objects and their direct super objects:
the object system will automatically compute
a suitable precedence list in which order to compose the object prototypes.
Thus, defining objects with dependencies becomes modular.

The algorithm that computes this precedence list is called a linearization:
It considers the dependencies as defining a directed acyclic graph (DAG),
or equivalently, a partial order, and
it completes this partial order into a total (or linear) order,
that is a superset of the ordering relations in the partial order.
The algorithm can also detect any ordering inconsistency or circular dependency
whereby the dependencies as declared fail to constitute a DAG;
in such a situation, no precedence list can satisfy all the ordering constraints,
and instead an error is raised.
Recent modern object systems seem to have settled on the C3 linearization algorithm,
as described in @(AppendixC).

@(define/local-expand c3-definitions @Definitions[
(code:comment "The (require srfi/1) below imports SRFI 1 list functions into Racket")
(code:comment "YMMV if you use another Scheme implementation")
(require srfi/1)

(code:comment "not-null? : Any -> Bool")
(define (not-null? l) (not (null? l)))

(code:comment "remove-nulls : (List (List X)) -> (List (NonEmptyList X))")
(define (remove-nulls l) (filter not-null? l))

(code:comment "remove-next : X (List (NonEmptyList X)) -> (List (NonEmptyList X))")
(define (remove-next next tails)
  (remove-nulls (map (λ (l) (if (equal? (car l) next) (cdr l) l)) tails)))

(code:comment "c3-compute-precedence-list : A (A -> (List A)) (A -> (NonEmptyList A))")
(code:comment "  -> (NonEmptyList A)")
(define (c3-compute-precedence-list x get-supers get-precedence-list)
  (define supers (get-supers x)) ;; : (List A)
  (define super-precedence-lists (map get-precedence-list supers)) ;; : (List (NonEmptyList A))
  (define (c3-select-next tails) ;; : (NonEmptyList (NonEmptyList A)) -> A
    (define (candidate? c) (every (λ (tail) (not (member c (cdr tail)))) tails)) ;; : A -> Bool
    (let loop ((ts tails))
      (when (null? ts) (error "Inconsistent precedence graph"))
      (define c (caar ts))
      (if (candidate? c) c (loop (cdr ts)))))
  (let loop ((rhead (list x)) ;; : (NonEmptyList X)
             (tails (remove-nulls (append super-precedence-lists (list supers))))) ;; : (List (NonEmptyList X))
    (cond ((null? tails) (reverse rhead))
          ((null? (cdr tails)) (append-reverse rhead (car tails)))
          (else (let ((next (c3-select-next tails)))
                  (loop (cons next rhead) (remove-next next tails)))))))
])

@(define/local-expand c3-extra-definitions @Definitions[
(define (alist->Dict alist)
  (foldl (λ (kv a) ((Dict 'acons) (car kv) (cdr kv) a)) (Dict 'empty) alist))

(define (Dict->alist dict)
  ((Dict 'afoldr) (λ (k v a) (cons (cons k v) a)) '() dict))

(define (Dict-merge override-dict base-dict)
  ((Dict 'afoldr) (Dict 'acons) base-dict override-dict))
])


@subsubsection{A prototype is more than a function}
But where is the inheritance information to be stored?
A prototype must contain more than the function being composed to implement open-recursion.
@italic{A minima} it must also contain the list of direct super objects
that the current object depends on.
We saw in @seclink["different_prototype"]{4.1.4} how we can and sometimes must
indeed include additional information in a prototype.
Such additional information will include a precomputed cache for the precedence list below,
but could also conceivably include
type declarations, method combinations, and support for any imaginable future feature.

We could start by making the prototype a pair of prototype wrapper and list of supers;
if more data elements are later needed, we could use a vector with every element at a fixed location.
But since we may be adding further features to the object system,
we will instead make the prototype itself an object,
with a special base case to break the infinite recursion.
Thus, the same functions as used to query the slot values of an object's instance
can be used to query the data elements of the prototype (modulo this special case).
But also, the functions used to construct an object can be used to construct a prototype,
which lays the foundation for a meta-object protocol@~cite{amop},
wherein object implementation can be extended from the inside.

When it is an object, a prototype will have
slot @r[function] bound to a prototype wrapper as previously, and
slot @r[supers] bound to a list of super objects to inherit from,
as well as a slot @r[precedence-list] bound to a precomputed cache of the precedence list.
When it is the special base case, for which we below chose the empty list,
slots @r[supers] and @r[precedence-list] are bound to empty lists, and slot @r[function]
is bound to a function that overrides its super with constant slots from the instance.

@Definitions[
(define (Dict->Object dict) (make-object dict '()))
(define (object-prototype-function object)
  (define prototype (object-prototype object))
  (if (null? prototype)
    (λ (self super)
      (make-object (Dict-merge (object-instance object) (object-instance super))
                   (object-prototype super)))
    (slot-ref prototype 'function)))
(define (object-supers object)
  (define prototype (object-prototype object))
  (if (null? prototype) '() (slot-ref prototype 'supers)))
(define (object-precedence-list object)
  (define prototype (object-prototype object))
  (if (null? prototype) '() (slot-ref prototype 'precedence-list)))
(define (compute-precedence-list object)
  (c3-compute-precedence-list object object-supers object-precedence-list))
(define base-dict (Dict 'empty))
(define (instantiate supers function)
  (define proto
    (Dict->Object ((Dict 'acons) 'function (delay function)
                   ((Dict 'acons) 'supers (delay supers)
                    ((Dict 'acons) 'precedence-list (delay precedence-list)
                     (Dict 'empty))))))
  (define base (make-object base-dict proto))
  (define precedence-list (compute-precedence-list base))
  (define prototype-functions (map object-prototype-function precedence-list))
  (instantiate-prototype-list prototype-functions (delay base)))
(define (object function . supers) (instantiate supers function))
]

@subsection{Further Features}

Other advanced OOP features found in CLOS@~cite{bobrow88clos},
such as multi-methods (a.k.a multiple dispatch) or method combinations,
can also be expressed in a pure setting.
However, they require further extensions to the model we propose,
that we discuss in our @(AppendixB).

@section{Classes}

@subsection{Classes on top of Prototypes}
@subsubsection{Layering Language Features}
So far we have reconstructed prototype-based OOP;
yet class-based OOP comes first both in history and in popularity.
@; TODO cite Simula? Smalltalk? C++? Newspeak?
There have been implementations of classes on top of prototypes in the past,
notably for JavaScript@~cite{EcmaScript:15}. @; TODO: CECIL?
But these implementations relied heavily on side-effects over some object encoding,
to achieve efficiency within some existing language.
Instead we will propose our own reconstruction on how to
@emph{macro-express}@~cite{eppl91} classes on top of our pure functional prototypes.
This reconstruction will shed light on the essence of the relationship between classes and prototypes.

@subsubsection{Prototypes for Type Descriptors}
In our reconstruction, a @emph{class} is “just” a prototype for a type descriptor.
Type descriptors, as detailed below, are runtime data structures
describing what operations are available to recognize and deal with elements of the given type.
Type descriptors therefore do not have to themselves be objects,
and no mention of objects is required to describe type descriptors themselves.
They can be just a type on which to apply the monomorphic prototypes of @(section34).
Still, it is typical in OOP to conflate into a “class” both the instance of a type descriptor
and the prototype for the type descriptor. Our distinction of the two concepts can then help avoid
a lot of the confusion present in “classical” presentations of OOP (pun not intended but adopted).

Every compiler or language processor for a statically typed language (even without OOP)
necessarily has type descriptors, since the language's compile-time is the compiler's runtime.
But in most statically typed languages, there are no dependent types,
and the types themselves (and the classes that are their prototypes)
are not first-class entities@~cite{Strachey67}
that can be arguments and results of runtime computations,
only second-class entities that are fully resolved at compile-time.
Therefore, class-based languages only have second-class classes
whereas only prototype-based languages have first-class classes.

@subsection{Type Descriptors}
@subsubsection{I/O Validation and Beyond}
One common programming problem is validation of data at the inputs and outputs of programs.
Static types solve the easy cases of validation in languages that have them.
Dynamically typed languages cannot use this tool so often grow libraries
of “type descriptors” or “data schemas”, etc.

These runtime type descriptors often contain more information
than typically available in a static type, such as:
additional constraints on values beside principal type membership;
methods to encode and decode values, to print and parse them,
to display them or interact with them in a graphical interface;
default values for use in user interfaces or simple tests;
pseudo-random value generators and value compressors
to enable property-based testing and other search algorithms;
algebraic operations whereby this type implements an interface,
satisfies a constraint, or instantiates a typeclass; etc.
Types used at compile-time would have additional data to deal with
how to infer them, how they lead to further inferences,
how to compute unions or intersections with other types,
how to generate code for operations that deals with them,
etc.

Therefore, even statically typed languages often involve runtime type descriptors.
Better languages will provide “reflection” facilities or “macros”
to automatically generate descriptors without humans having to
try keeping two different representations in synch as programs evolve.
@; TODO cite reflection mechanism for Java? C#? Scala?

@subsubsection{Simple Types}
In a dynamic language, all types would have at least a recognizer slot @r[is?],
bound to a function to recognize whether a runtime value is element of the type.
In the most basic types, @r[Top] (that tells nothing about everything) and
@r[Bottom] (that tells everything about nothing),
this might be the only slot (though an additional @r[name] slot would help), with trivial values:
@;@Definitions[
@verbatim{
(define Top (object ($slot/value 'is? (λ (_) #t))))
(define Bottom (object ($slot/value 'is? (λ (_) #f))))
}@;]
Other simple types might include the type of representable values;
here they will sport a single additional method @r[->sexp] to turn a value into
a type-specific source expression, but a real library would have many more I/O operations.
Numbers would also have various arithmetic operations.
@;@verbatim{(define Representable (object ($slot/value '->sexp (λ (x) (list 'quote x))) Top))
@Definitions[
(define Number (object ($slot/values 'is? number? '->sexp identity
                          '+ + '- - 'zero 0 'one 1)))
]

@subsubsection{Parameterized Types}
A parameterized type at runtime can be function that returns a type descriptor
given its parameter, itself a type descriptor,
or any runtime value (yielding a runtime dependent type).
Thus, monomorphic lists might be:
@Definitions[
(define (list-of? t x) (or (null? x)
  (and (pair? x) ((slot-ref t 'is?) (car x)) (list-of? t (cdr x)))))
(define (ListOf t) (object ($slot/value 'is? (λ (x) (list-of? t x)))))
]

@subsubsection{More Elaborate Types}
By using object prototypes, we have, in a few hundreds of lines of code@~cite{GerbilPOO},
defined type descriptors for functions with given input and output types;
for dicts, objects, and objects having specific slots with values of specific types;
for slot and type descriptors, and functions that compute type descriptors
from type descriptors or other values; etc.
@; TODO: add that in appendix? Cite GerbilPOO again?
The type descriptor system can describe itself as well as all pieces of the system,
generating routines for all kinds of automation or user interaction.

Along the way, prototype-based OOP enables a compact and extensible style of programming,
leveraging all the advantages of classes or typeclasses
for defining pure functional types.

@subsubsection{Classes and Typeclasses}
Runtime type descriptors correspond to the “dictionaries”
passed around at runtime in implementations of typeclasses@~cite{ImplementingTypeClasses}
in various FP languages.
They also correspond to the “vtable” describing an “object”'s type at runtime
in implementations of class-based OOP languages.
Indeed, the correspondance can be made formal, with automated transformations
between the two styles@~cite{LIL2012}.
Note however how “object” denotes very different notions in the two styles:
what is called “object” in class-based OOP is the notional pair of
the type descriptor and an element of the described type,
whereas the type descriptor is (an instance of) an “object” in our prototype-based formalism,
handled independently from the elements of the described type,
that themselves may or may not be objects.

@section[#:tag "Mutability"]{Mutability}

@subsection{Mutability as Pure Linearity}
@subsubsection{From Pure to Mutable and Back}
Note how all the objects and functions defined in previous sections were pure.
They did not use any side-effect.
No @r[set!]. No @r[call/cc]. Only laziness at times, which still counts as pure.
This contrasts with all OOP literature from the 1960s to the 1980s, and most since.
But what if we are OK with side-effects? How much does that simplify computations?
What insights does the pure case offer on the mutable case, and vice versa?

A simple implementation of mutable objects is to store pure object values into mutable cells.
Conversely, a mutable object can be viewed as a monadic thread of pure object state references,
with an enforced linearity constraint wherein effectful operations return a new state reference
after invalidating the previous one.
Indeed, this isomorphism is not just a curiosity from category theory,
it is a pair of transformations that can be and have been automated
for practical purposes@~cite{LIL2012}.

@subsubsection{Combining Mutable and Functional Wins}
Mutability allows for various optimizations,
wherein objects follow common linearity constraints of consuming some arguments
that are never referenced again after use, at which point their parts
can be transformed or recycled “in place” with local modifications in @${O(1)} machine operations,
rather than global copies in @${O(\log n)} or @${O(n)} machine operations.
@;TODO cite regarding state vs linearity
As a notable simplification, the “super” computation as inherited by a prototype is linear
(or more precisely, affine: the prototype may either consume the super value, or ignore it).
Thus, the entire fixed-point computation can be done in-place by updating slots as they are defined.
This is a win for mutability over purity.

Yet, even mutable (or linear) rather than pure (and persistent),
the functional approach solves an important problem with the traditional imperative approach:
traditionally, programmers must be very careful to initialize object slots in a suitable order,
even though there is no universal solution that possibly works
for all future definitions of inheriting objects.
The traditional approach is thus programmer-intensive in a subtle way,
and leads to many errors from handling of unbound slots.
Depending on the quality of the implementation, the consequences may range from
a error being raised with a helpful message at compile-time or at runtime,
to useless results, wrong results, memory corruption, or catastrophic failures.
@; TODO: cite Tony Hoare regarding the billion-dollar mistake of NULL?
By defining objects functionally or lazily rather than imperatively and eagerly,
programmers can let the implementation gracefully handle mutual references between slots;
an initialization order can be automatically inferred wherein slots are defined before they are used,
with a useful error detected and raised (at runtime) if no such order exists.

@subsubsection{Simplified Prototype Protocol}
A prototype for a mutable object can be implemented
with a single mutable data structure argument @r[self]
instead of two immutable value arguments @r[self] and @r[super]:
the @emph{identity} of that data structure
provides the handle to the future complete computation,
as previously embodied in the @r[self] argument;
and the current @emph{storage} of the data structure provides the state of the computation so far,
as previously embodied in the @r[super] argument.

A mutable prototype wrapper would then be of type @r[(deftype μProto (Fun Object ->))],
where the @r[Object]'s instance component would typically contain a mutable hash-table
mapping slot names (symbols) to effective methods to compute each slot value.
These effective methods would be either thunks or lazy computations,
and would already close over the identity of the object as well as reuse its previous state.
Overriding a slot would update the effective method in place,
based on the new method, the self (identity of the object) and
the super-inherited entry previously in the hash-table.
Since this protocol is based on side-effects, no need to return @r[self] in the end;
the fix operator variant will also rely on side-effects.

@Definitions[
(define (fix! p b) (define f (hash-copy b)) (p f) f)
(define (mix! p q) (λ (f) (q f) (p f)))
(define ($slot-gen! k fun)
  (λ (self) (define inherit (hash-ref self k (delay (bottom))))
            (hash-set! self k (fun self inherit))))
]

@subsection{Cache invalidation}
@italic{There are two hard things in computer science:
cache invalidation, naming things, and off-by-one errors} (Phil Karton).
One issue with making objects mutable is that modifications may make
some previously computed results invalid. There are several options then,
none universally satisfactory.

The object system may embrace mutation and just
never implicitly cache the result of any computation,
and let the users explicitly insert any cache desired.
That's the design commonly followed by prototype systems
in effectful languages from T to JavaScript.
But constant recomputation can be extremely expensive;
moreover, many heavy computations may be below the level at which users may intervene—including
computing precedence lists. This probably explains why these languages tend
not to support multiple inheritance, and if so to handle it specially.

At the opposite end of the spectrum, the object system may assume purity-by-default
and cache all the computations it can.
It is then up to users to explicitly create cells to make some things mutable,
or flush some caches when appropriate.
We used this option in @citet{GerbilPOO}.

In between these two opposites, the system can automatically track which mutations happen,
and invalidate those caches that need be, with a tradeoff between how cheap it will be
to use caches versus to mutate objects.
However, to be completely correct, this mutation tracking and cache invalidation
must be pervasive in the entire language, not just the object system implementation,
at which point the language is more in the style of higher-order
reactive or incremental programming. @;TODO: @~cite{}

@;;; Here, we silently cite things that only appear in the appendices,
@;;; so they appear in the bibliography, that is being computed before the appendices.
@~nocite{chambers92objectoriented Salzman05prototypeswith Barrett96amonotonic WikiC3 aop97 Foster2007CombinatorsFB Pickering_2017}

@section[#:tag "Related_Work"]{Related Work}

@subsection{Prototype Object Systems}

@subsubsection{Director and ThingLab}
The first prototype object system might have been Director @~cite{Kahn1976},
an actor system to create animations from story constraints, written in MacLisp at MIT.
@; interoperating with LOGO, with inspiration from Smalltalk and AI research.
The system is informally described as a modifiable hierarchy of message-passing objects
that inherit from other objects to which they “pass the buck” when they receive a message
that doesn't match any of the patterns they directly have rules for.
At about the same time, ThingLab@~cite{Borning1977 Borning1979 Borning1981}
implemented a classless object system atop Smalltalk, and introduced the term “prototype”,
as part of a similar project to specify the constraints of an environment to simulate.
Both systems were followed by many others in their respective traditions.
@; TODO. Mention inspiration by KRL and its frames? Prototype systems by Luc Steels?

@subsubsection{T}
In 1981, Yale T Scheme@~cite{Rees82t:a} included a mature variant of such an object system
as part of a general-purpose programming environment.
A paper was later published describing the object system@~cite{adams88oopscheme}.
@; The implementation was optimized for efficiency using low-level tricks;
@; no attempt was made at either generality or clean simple formal semantics.
The T design was notably reprised by YASOS in 1992@~cite{dickey1992scheming}
and distributed with SLIB since.
@; TODO: cite!

T's object system is already essentially equivalent to the initial design in our @(section1),
though not exactly isomorphic.
The major differences stem from T's heavy reliance on mutation whereas we aim at purity:
T was running on resource-limited platforms by today's standards, and
mutation was naturally ubiquitous, with explicit state management, and
no attempt at implicitly caching effective method values.

There are minor differences between our model and T's.
Importantly, T lives by the slogans that “closures are a poor man's objects”
and “objects are a poor man's closures”:
T objects are callable functions that have extra named entry points;
in our model we can add a special method for the default entry point,
but we need language support to hook the regular function call syntax into that method.
As for nomenclature, what we call “instance”, T calls “object” or “instance”,
and instead of “prototypes” it has “components” with a slightly different API.
Also, T assumes methods are all function-valued, to be immediately called when looked up,
as if all accesses to an object in our model went through the @r[operate] function below.
Methods in T can be directly represented as slots with a function value in our approach.
Non-function-valued slots in our approach above can be represented in T
by nullary methods returning a constant value.
@Definitions[
(define (operate instance selector . args) (apply (instance selector) args))
]

@subsubsection{Prototypes become mainstream}
In 1986, several publications@~cite{Lieberman1986 Borning1986}
popularized the term and concept of prototypes,
and to a point that of “delegation” for the variant of inheritance associated with them.
@;delegation is already in Hewitt_Attardi_Lieberman 1979, maybe in earlier Kahn and Borning papers?
@;TODO: cite!

That year also appeared SELF, a language in the Smalltalk tradition
but with prototypes instead of classes. @; TODO CITE
SELF was influential, notably thanks to its complete graphical environment
that could run on workstations available at many universities and research centers.
However, it was only usable on high-end workstations,
despite many innovations to make it efficient@~cite{chambers1989efficient}.
The optimization effort and ubiquitous mutation including of the inheritance hierarchy
contributed to making its semantic model complex, though still simpler than that of class-based OOP.

JavaScript came out in 1994,
with a prototype object system @~cite{EcmaScript:15 DBLP:journals/corr/GuhaSK15},
and brought prototype object orientation to the masses.

All the above efforts happened in a stateful rather than pure context,
optimizing for efficient implementation where memory and computation are expensive.
None of them sought to establish a general model for all OOP, what is more with simple semantics.

@subsection{The Pure Functional Tradition}

@subsubsection{Denotational and Operational Semantics}
@; Kamin and @; TODO Cite
Reddy@~cite{ObjectsAsClosures} used objects as closures
to offer denotational semantics for Smalltalk.
Cook@~cite{Cook1989} independently took the same approach, but made it more general.
Cook uses the same model as in our @(section1),
just restricted to records (unlike our @(section3)),
and with single inheritance only, no composition.
Cook then shows how to adapt this model to describe many class-based languages of his day.
He notably factors the record extension mechanism as a parameter of his protocol.
Cook doesn't address prototype systems, and mentions but doesn't adequately tackle
multiple inheritance and other advanced features from Flavors.

Bracha first documented and implemented prototype composition,
which he called mixin composition @~cite{bracha1990mixin},
then in his thesis @~cite{bracha1992jigsaw} generalized the concept and vastly expanded on it.
He notably explained how to express single and multiple inheritance
in terms of this composition, and relating it to both previous class and prototype OOP.
A lot of the concepts in the present paper are explicitly present or a least latent in
Bracha's work, though we arrange them in slightly different ways:
Bracha has more of a deconstructive approach, breaking down the elementary mechanisms
with which to reconstruct any past and future system of modules, objects or first-class environments;
our paper has a more constructive presentation of the same ideas,
emphasizing the simplicity of minimal definitions of object systems on top of FP.

@subsubsection{GCL, Jsonnet, Nix}

Jsonnet@~cite{jsonnet} in 2014 was
the first publicly available language with prototype objects
in the context of a pure lazy functional language with dynamic types.
It was also the first language used beyond its creating institution
that made prototype composition the primary algebraic operation on objects:
the model of our @(section1), restricted to records.
Jsonnet objects also combine in a same entity the two aspects, instance and prototype,
as in our @(section42).
Note that Jsonnet uses the empty object as the implicit base super object for inheritance;
this is equivalent to using the bottom function in the representation from our @(section1),
but not in theirs!
Jsonnet also has field introspection, and flags fields as either visible or hidden
for the sake of exporting JSON.
Jsonnet itself started as a simplified reconstruction and cleanup of GCL @~cite{gclviewer2008} (née BCL),
the decade-older (circa 2004) Google Configuration Language, which remains unpublished,
and is reputedly clunky due to its dynamic scoping.

Nix@~cite{dolstra2008nixos}, the pure lazy dynamic functional configuration language for NixOS,
has several variations of an “extension system”, all of them
essentially equivalent to Jsonnet's object system (minus field visibility flagging),
except done as a handful of user-defined functions, rather than as builtin primitives.
Peter Simons wrote the initial one in 2015 to support for multiple versions of the GHC ecosystem.
However, neither the code nor its documentation describe to these systems as object systems,
though each has a small comment about object-orientation;
furthermore, while prototype composition is implemented,
the codebase tends to stick to single inheritance.
@;{ Peter says (private communication) his was an independent reinvention,
however, he was inspired by some discussions with Conor McBride
and previous code by Andres Löh who worked on an OOHaskell with Ralf Hinze
who built it with Oleg Kiselyov who knew about T;
both Conor and Andres were also familiar with the essay by Bruno de Oliveira.
}

@;{
As in these previous inventions, Jsonnet and Nix use first-class functions to construct objects.
They also rely on dynamic types to avoid the need for dependent types and internal type theories
required to statically type first-class prototypes in the most general case.
But unlike previous incarnations, they also rely on lazy evaluation as a way to express
first-class computations that can be manipulated whether or not they terminate,
without a clumsy explicit wrapping in a thunk or a lazy constructor.
They also do without having to resort to side-effects;
the pure functional semantics are thus not only especially clean and simple,
fitting in a few hundreds of characters,
but reveal an underlying mathematical structure of universal interest.

The success of Jsonnet and Nix at modularly managing complex configurations
through object extensions suggest that pure lazy functional programming is useful
way beyond the popular package offered by Haskell, of this programming model
with ML-like static typesystem that sports parametric polymorphism but no dependent types.
That typesystem rejects most interesting uses of prototypes,
thereby neglecting an important application domain for pure lazy functional programming.
Thus, though some claim the ML typesystem is at a "sweet spot",
wherein it provides good expressiveness while still being computable
and providing understandable error messages@~cite{minsky08},
maybe the spot is too sweet to be healthy, and
is lacking in proteins, or at least in prototypes.
}

Jsonnet and Nix illustrate how prototype objects are a great fit
to simply express flexible configurations.
Pure functional programmers may have unjustly neglected prototypes until recently
because of the limitations of their languages' type systems.
These two object systems in turn inspired the present authors
to use, study and improve prototype object systems.

@; TODO: include information from personal communication with mpvl
No academic publication was made on any of GCL, Jsonnet or Nix, and it is unclear
which of their design elements were influenced by which previous systems,
and which were original inventions or independent reconstructions.
Still, we suspect that the influence of previous prototype systems on these three languages
was indirect and weak. If this suspicion is correct, then the fact that, across several decades,
closely matching designs were independently reinvented many times without explicit intent to do so,
is a good sign that this design is a “fixed point in design space”,
akin to the notion of “fixed point in time” in Dr Who@~cite{DrWhoFPIT}:
however you may try to reach a different conclusion, you still end-up with that fixed-point eventually.

@subsubsection{Types for Objects}
Object Systems present many challenges
to authors of static type systems@~cite{Abadi97atheory tapl}.
Proper treatment of common usage requires existential types to deal with fixed-points,
subtypes for ad hoc polymorphism,
covariance or contravariance to commute subtypes with fixed-points,
row polymorphism to support records, etc.
One logical feature too many and the type system will become undecidable, or worse, inconsistent.
@;TODO CITE

Bruce et al.@~cite{BruceCardelliPierce2006} summarize one basic challenge in encoding objects.
Their first model, OR, fits our @(section1).
Their other three, OE, OBE, ORBE, do not apply to prototype-based OOP,
but are tailored to class-based OOP.
OE and ORBE naturally emerge when using our reduction of class-based OOP to prototype-based OOP,
OE as our “typeclass” representation, ORBE as our “class” representation.

Most object typing papers, like the one above, focus on class-based OOP and generators only,
because it makes for much simpler types.
Prototypes and wrappers are reduced away at the type-level at compile-time,
distinction between instance and prototype is static, etc.
Handling full prototype OOP would require dependent types or some notion of staged computations.

@subsection{Other Relevant Work}

@subsubsection{The Lisp tradition}
The Lisp tradition includes many object systems that were class-based
but innovated in other ways:
Flavors@~cite{Cannon82} brought multiple inheritance, mixins, extensible method combinations,
default initialization values, etc.
CommonLoops@~cite{bobrow86commonloops} brought generic functions and multiple dispatch,
meta-object protocols, etc.
CLOS@~cite{bobrow88clos} standardized it all.
Dylan brought sealing @;TODO CITE
and the C3 linearization algorithm@~cite{Barrett96amonotonic}.
Some of these innovations have been individually copied here and there,
but by and large, CLOS remains decades ahead of object systems outside the Lisp tradition
in most ways but static typing.

@subsubsection{The Haskell tradition}
The limited type system of Haskell does not support OOP for first-class entities.
Yet Haskell popularized a form of second-class @emph{ad hoc} polymorphism
in the form of typeclasses@~cite{ImplementingTypeClasses}.
@;TODO CITE
@;TODO CITE OOHaskell and Oleg Kiselyov's works
Despite these limitations, Oliveira@~cite{MonadsMixins} uses
essentially our @(section1) construction to amazing effects.
Remarkably, he doesn't try at all to model records, which wouldn't work,
but instead uses prototype wrappers (that he calls “mixins”)
for arbitrary function types, as in our @(section3).
He then leverages typeclass constraints on the context
to incrementally specify the @emph{means} of a computation,
even though the monomorphic type limitation won't allow
incremental specification of a computation's @emph{results}.
Oliveira explicitly relates his work to Cook's@~cite{Cook1989},
yet importantly identifies composition rather than application
as the interesting algebraic structure.
@; (but incorrectly equates it with Cook's boxed composition).
@; He takes an ugly though minor shortcut in omitting a base object to his fixed-point function
@; and passing the “self” a second time instead.

@section[#:tag "Future_Work"]{Future Work}

@;{Using the ideas in this essay, we have implemented in both Nix and Scheme
object systems with unified instances and prototypes and multiple inheritance,
that are being used in an actual application.}

In the future, we would like to explore the Generalized Prototypes mentioned in @(AppendixB)
to also implement multiple dispatch and method combination.
Multiple dispatch raises interesting questions regarding the semantics of extending existing objects.
Method combination meanwhile may require attaching meta-data to method prototypes
regarding how they are to be combined in the end,
which means we will have to explore what insights our approach may bring into
Meta-Object Protocols@~cite{amop}.

Another important kind of meta-data we may want to attach to prototypes is type information
and/or logical constraints, that may be enforced either at runtime or at compile-time.
For a fully formal treatment, we would have to formalize such type information
in a dependent type system.
While Church-style systems like Coq, Lean, Agda, or Idris are more popular,
we suspect that a Curry-style system like Cedille might be more appropriate
to seamlessly extend our untyped approach into a dependently typed one.
@; TODO cite ?

Finally, we may want to study how prototype OOP interacts
with staged computations or cache invalidation
so as to require less-than-dependent types, or enable various optimizations at compile-time
especially with respect to object representation.

@(generate-bibliography)

@section[#:tag "Appendix_A"]{Code Library} @appendix

@emph{We have used Racket to develop this document in such a way that the very same file is used
as source file for a reusable Racket library, a test module, and the printable document.}
The code is available under the Apache License, version 2.0.
You should be able to adapt it with minimal effort to run on any dialect of Scheme,
or on any language that contains the untyped lambda calculus.
Thus, any capable language can gain a usable object system in about 30 lines of code,
and to extend it with multiple inheritance in about 50 more lines of code.

Alternatively, you could use the full-fledged library we built on the same general model
in another Scheme dialect@~cite{GerbilPOO}:
it features many practical optimizations and syntactic abstractions for enhanced usability,
as well as a extensive support for type descriptors.

In this appendix, we include library code of relatively obvious or well-known functions,
that provide no original insight, yet that we rely upon in the implementation of the object system.
This code is included for the sake of completeness and reproducibility.
It is also required to get this file running, though we could have kept the code hidden.

@subsection{C3 Linearization Algorithm}

Below is the C3 Linearization algorithm to topologically sort an inheritance DAG
into a precedence list such that direct supers are all included before indirect supers.
Initially introduced in Dylan@~cite{Barrett96amonotonic},
it has since been adopted by many modern languages, including
Python, Raku, Parrot, Solidity, PGF/TikZ.

The algorithm ensures that the precedence list of an object always contains as ordered sub-lists
(though not necessarily with consecutive elements) the precedence list of
each of the object's super-objects, as well as the list of direct supers.
It also favors direct supers appearing as early as possible in the precedence list.

We import the standard library @r[srfi/1] for the sake of
the standard functions @r[every] and @r[filter].

@c3-definitions

@(noindent)
We will now test this linearization algorithm on the inheritance graph from @(section43).
This test case was originally taken from the Wikipedia article on C3 linearization@~cite{wikiC3},
that usefully includes the following diagram
(that nevertheless fails to represent the ordering constraints imposed on A, B, C, D, E
by their order of appearance in the supers list of K1, K2, K3):

@(noindent) @image[#:scale 0.67]{C3_linearization_example.eps}
@;;; NB: The EPS file was converted using inkscape from the SVG originally at
@;;; https://en.wikipedia.org/wiki/C3_linearization

@(noindent)
The definitions for this test case are as follows:

@Examples[
(define test-inheritance-dag-alist
  '((O) (A O) (B O) (C O) (D O) (E O)
    (K1 A B C) (K2 D B E) (K3 D A) (Z K1 K2 K3)))
(define (test-get-supers x) (cdr (assoc x test-inheritance-dag-alist)))
(define (test-compute-precedence-list x)
  (c3-compute-precedence-list x test-get-supers test-compute-precedence-list))
]

@(noindent)
And the test are thus:
@Checks[
(eval:check (map not-null? '(() (1) (a b c) nil)) '(#f #t #t #t))
(eval:check (remove-nulls '((a b c) () (d e) () (f) () ())) '((a b c) (d e) (f)))
(eval:check (map test-compute-precedence-list '(O A K1 Z))
  '((O) (A O) (K1 A B C O) (Z K1 K2 K3 D A B C E O)))
]

@subsection{More Helper Functions}

To help with defining multiple inheritance, we'll also define the following helper functions:

@c3-extra-definitions

@;{ Memoizing?
;; II.1.2- Memoizing values, so slot access is not O(n) every time.
;; Usage: Put memoize-proto as first prototype.
;; NB1: Memoization uses side-effects internally, but does not expose them.
;; NB2: It's still O(n^2) overall rather than O(n); we can do better, later.
(define (memoize f)
  (nest (let ((cache (make-hash)))) (λ x) (apply values) (hash-ref! cache x)
        (λ ()) (call-with-values (λ () (apply f x))) list))

(define (make-counter)
  (nest (let ((count 0))) (λ ())
        (let ((result count)) (set! count (+ count 1)) result)))

(define my-counter (make-counter))
(define my-memo-counter (memoize my-counter))

(check! (= (my-counter) 0)
(check! (= (my-memo-counter) 1)
(check! (= (my-counter) 2)
(check! (= (my-memo-counter) 1)

(define (memoize-proto self super) (memoize super))

(define (count-proto self super)
  (make-counter))

(define count-fun (instance count-proto))
(check! (= (count-fun) 0)
(check! (= (count-fun) 1)
(check! (= (count-fun) 2)

(define zero-fun (instance memoize-proto count-proto))
(check! (= (zero-fun) 0))
(check! (= (zero-fun) 0))
}

@subsection{Extra tests}

Here are some tests to check that our functions are working.

@Examples[
(define test-instance
  (alist->Dict `((a . ,(delay 1)) (b . ,(delay 2)) (c . ,(delay 3)))))
(define test-prototype
  (alist->Dict `((function . ,(delay (λ (self super)
                                (Dict-merge (force test-instance) (force super)))))
                 (supers . ,(delay '()))
                 (precedence-list . ,(delay '())))))
(define test-object (make-object test-instance test-prototype))
(define test-p1 ($slot/value 'foo 1))
]

@;{
(define (pair-tree-for-each! x f)
  (let loop ((x x))
    (cond ((pair? x) (loop (car x)) (loop (cdr x)))
          ((null? x) (void))
          (else (f x)))))
(define (call-with-list-builder f)
  (define l '())
  (f (λ (x) (set! l (cons x l))))
  (reverse l))
(define (flatten-pair-tree x)
  (call-with-list-builder (λ (c) (pair-tree-for-each! x c))))
}

@Checks[
(eval:check (map (λ (x) (slot-ref test-object x)) '(a b c)) '(1 2 3))
(eval:check (slot-ref (instantiate '() test-p1) 'foo) 1)
(eval:check (map (slot-ref (ListOf Number) 'is?) '(() (1 2 3) (1 a 2) (1 . 2)))
  '(#t #t #f #f))
]

@section[#:tag "Appendix_B"]{Advanced OOP}

@subsection{Multiple Dispatch}

@subsubsection{Generic Functions}
Some object systems, starting with CommonLoops@~cite{bobrow86commonloops}
then CLOS@~cite{bobrow88clos},
feature the ability to define “generic functions” whose behavior
can be specialized on each of multiple arguments.
For instance, a generic multiplication operation
will invoke different methods when called with two integers, two complex numbers,
an integer and a floating-point number, a complex number and a vector of complex numbers, etc.

Selection of behavior based on multiple arguments is called “multiple dispatch”,
as contrasted with “single dispatch” which is selection of behavior based on a single argument.
Methods that may specialize on multiple arguments are sometimes called “multi-methods”
to distinguish them from single dispatch methods.
It is possible to macro-expand multiple dispatch into single dispatch,
@; TODO @~cite{} double dispatch?
by chaining dispatch of the first argument into a collection of functions
that each dispatch on the second argument, and so on.
But the process is tedious and non-local, and better left to be
handled by an automated implementation of multiple dispatch.

Since this feature is rather involved yet was already implemented in
previous prototype systems@~cite{chambers92objectoriented Salzman05prototypeswith},
we'll restrict our discussion to additional challenges presented in
a pure or mostly pure functional setting.

@subsubsection{Extending previous objects}
Generic functions and their multi-methods are associated to multiple objects,
thus they cannot be defined as part of the definition of a single object;
these methods, and the generic function that they are part of,
are necessarily defined outside of (some) objects.
Therefore, language designers and implementers must resolve a first issue
before they may implement generic functions:
adding “multi-methods” to existing functions or objects
after they have been initially defined.
Indeed, generic functions when introduced, involve adding new methods
that specialize the behavior of the new function on existing objects;
and when new objects are introduced, they often involve adding new methods
to specialized the behavior of the existing functions on the new objects.

Moreover, when generic functions and objects are introduced in independent code libraries
(e.g. some graphical user-interface library and some data structure library),
the specialized behavior might be introduced in yet another library
(e.g. that defines a graphical interface for this data structure).
Worse, there might be conflicts between several such libraries.
Even without conflicting definitions,
a definition might conflict in time with the absence of the same definition,
if there is any chance that some code depending on its presence is compiled or run
both before and after the definition was evaluated.

That is why, for instance, the Glasgow Haskell Compiler @;TODO @~cite{GHC}
issues warnings if you declare an “orphan instance”:
a typeclass instance (rough analogue to methods of generic functions)
in a file other than one where either the typeclass is defined (analogous to generic functions)
or the type is defined (analogous to objects).
The language offers weak guarantees in such situations.
One way to avoid “orphan” situations might be to declare
either new typeclasses (new generic functions) or newtype aliases (new objects)
that will shadow or replace the previous ones in the rest of the program;
but this is a non-local and non-composable transformation
that potentially involves wrapping over all the transitive dependencies of an application,
and defeat the purpose of incremental program specification.
Another approach suitable in a more dynamic language would be to maintain at runtime
a “negative” cache of methods previously assumed to be absent,
and issue a warning or error when a new method is introduced that conflicts with such an assumption.

Then again, a solution might be to eschew purity and embrace side-effects:
the original T just did not cache method invocation results, and
re-ran fixed-point computations, with their possible side-effects,
at every method invocation (objects can specify their own explicit cache when desired).
The behavior of new generic functions on previously existing objects would be optionally specified
in a default method, to be called in lieu of raising a “method not found” error.
The T object paper@~cite{adams88oopscheme} mentions an alternate approach
that was rejected in its implementation, though it is essentially equivalent in behavior,
wherein default methods are added to a “default” object used as the base super value
instead of an empty object when (re)computing instance fixed-points.

@subsection{Method Combinations}

@subsubsection{Combining Method Fragments}
With method combination, as pioneered in Flavors@~cite{Cannon82},
then standardized by CLOS@~cite{bobrow88clos} via @~cite{bobrow86commonloops}
and made slightly popular outside the Lisp tradition by Aspect-Oriented Programming@~cite{aop97},
object methods to be specified in multiple fragments
that can be subsequently combined into the “effective method” that will be called.

Thus, in CLOS, “primary” methods are composed the usual way,
but “before” methods are executed beforehand for side-effects from most-specific to least-specific,
and “after” methods are executed afterwards for side-effects from least-specific to most-specific,
and “around” methods wrap all the above, with from most-specific wrapper outermost
to least-specific inner-most.

Furthermore, programmers may override the above standard “method combinations”
with alternative method combinations, either builtin or user-specified.
The builtin method combinations, @r[progn + list nconc and max or append min] in CLOS,
behave as if the calls to the (suitably sorted) methods had been wrapped in
one of the corresponding Lisp special form or function.
User-specified method combinations allow for arbitrary behavior based on
an arbitrary set of sorted, labelled (multi)methods
(though, in CLOS, with a fixed finite set of labels).

@subsubsection[#:tag "generalized_prototype_combination"]{Generalized Prototype Combination}

As determined above, generic functions are made of many labelled fragments (multi-methods);
fragments of a same label are sorted according to some partial or total order
(and, in the case of multiple dispatch, filtered for applicability);
they are then composed via some representation-dependent mixing function;
a fixed-point is extracted from the composed fragments, with some base value;
finally a representation-dependent wrapper is applied to the fixed-point
to instantiate the effective method...
this is very much like an object!
Actually, since we have accepted in @(section3) that prototypes and “object orientation”
are not just to compute records that map slot names to values, but for arbitrary computations,
then we realize there is a more general protocol for computing with prototypes.

A full treatment of generalized prototypes is a topic for future work.
Still, here are a few hints as to what they would be.
A generalized prototype would involve:
(a) a @emph{lens}@~cite{Foster2007CombinatorsFB Pickering_2017}
@; TODO: re-cite Kmett, Laarhoven from the Pickering_2017 article? Cite 2006 Pierce on Lens rather than 2007, see later Gibbons article
that can extract or update the method from the current partial computation
of the raw prototype fixed-point;
(b) an object-setter that “cooks” the raw prototype fixed-point into a referenceable
@r[self] object;
(c) a method-wrapper that turns the user-provided “method” into a composable prototype.

The lens, passed below as two function arguments @r[getter] and @r[setter],
generalizes the fetching or storing of a method as the entry in a @r[Dict],
or as the response to a message;
it expresses how a prototype overrides some specific fragment of some specific method
in some specific sub-sub-object, encoded in some specific way.
The object-setter may apply a method combination to extract a function from fragments;
it may transcode an object from a representation suitable for object production
to a representation suitable for object consumption;
it may be the setter from a lens making the prototype-based computation
that of a narrow component in a wider computation,
at which point the corresponding getter allows a method to retrieve the computation at hand
from the overall computation;
it may be a composition of some of the above and more.
The method-wrapper may to automate some of the builtin method combinations;
for instance, in a @r[+] combination, it would, given an number,
return the prototype that increments the super result by that number;
it may also handle the merging of a @r[Dict] override returned by the method
into the super @r[Dict] provided by its super method; etc.

Here then is a generalization that subsumes the above @r[$slot-gen] or @r[$slot/gen]:
@Definitions[
(define ($lens-gen setter getter wrapper method)
  (λ (cooked-self raw-super)
    (setter ((wrapper method) cooked-self (delay (getter raw-super)))
            raw-super)))
]

@;{ @para{
The types might look a bit as follows:
@Definitions[
(code:comment "(deftype (ObjectWrapper Raw Cooked)")
(code:comment "  (Forall (Object) (Fun (Raw Object) -> (Cooked Object))))")
(code:comment "(deftype (CookedProto Cooked)")
(code:comment "  (Forall (ObjectSuper ObjectSelf MethodSuper MethodSelf) ; s t a b")
(code:comment "    (Fun (Cooked ObjectSelf) (Delayed MethodSuper) -> MethodSelf)))")
(code:comment "(deftype (MethodSetter Raw)")
(code:comment "  (Forall (ObjectSuper ObjectSelf MethodSelf) ; s t b")
(code:comment "    (Fun MethodSelf (Raw ObjectSuper) -> (Raw ObjectSelf))))")
(code:comment "(deftype (MethodGetter Raw)")
(code:comment "  (Forall (Object Method) ; s a")
(code:comment "    (Fun (Raw Object) -> Method)))")
(code:comment "(deftype (MethodWrapper Cooked RawProto)")
(code:comment "  (Fun (RawProto Cooked) -> (CookedProto Cooked)))")
]
}}

@section[#:tag "Appendix_C"]{Digression about type notation}

There is no standard type system for the language Scheme,
so we will keep our type declarations as semi-formal comments
that are unenforced by the compiler,
yet that document what the types for our functions would be
in a hypothetical dependent type system (with possible effect extensions?)
that would be powerful enough to describe our computations.

@subsection{Variables and Type Variables}

We will use lowercase letters, such as @r[a], @r[f], @r[x], to denote variables,
that may be bound any value in the language.
We will use uppercase letters, such as @r[A], @r[F], @r[X], to denote @emph{type variables}.
That is, variables representing a type, that is not currently specified,
but such that the formulas we write must hold for any type,
in a hypothetical type system that one could layer on top of the language.

As common in programming languages such as ML and Haskell, we will assume that
each top-level function type declaration introduces its own scope,
wherein free variables are implicitly universally quantified.

@subsection{Variable Rows and Type Variable Rows}

We will write a @r[...] for a “row” of multiple values, such as may be used
as input arguments to a function, or return values of a function.
We will write A @r[...] for a “row” of multiple types, such as may be used
to type the inputs or outputs of a function.
Indeed, in Scheme, a function may take multiple inputs arguments and
and return multiple output values.

For instance, a row of types could be:
@racketblock[Integer]
@(noindent)
for the single type of integers, or:
@racketblock[String]
@(noindent)
for the single type of strings, or:
@racketblock[Integer String]
@(noindent)
for the two types (in order) @r[Integer] and @r[String], or:
@racketblock[Integer Symbol ...]
@(noindent)
for the types of one integer followed by zero or many symbols.

@subsection{Function Types}

The type of functions that take inputs @r[I ...] and return outputs @r[O ...]
we will write as any one of the following:
@racketblock[
  (Fun I ... -> O ...)
]
@;{
  I ... -> O ...
  O ... <- I ...
  (I ... -> O ...)
  (O ... <- I ...)
  (Fun O ... <- I ...)
}

@(noindent)
As usual, the arrows are associative such that these denote the same type:
@racketblock[
  (Fun A -> B -> C)
  (Fun A -> (Fun B -> C))
]
@;{
  C <- B <- A
  (C <- B) <- A
}

@(noindent)
Note that in this paper we follow the common convention of left-to-right arrows, as above.
We also follow this convention when contributing to other people's code bases that follow it.
However, in our own code base, we favor an opposite convention of right-to-left arrows,
that makes them covariant with the right-to-left flow of information from arguments to function
in the traditional prefix notation for function application.
Still, we revert to left-to-right arrows when we use concatenative languages
or stack virtual machines that use the “Reverse Polish Notation”
as in FORTH, PostScript or the much missed HP RPL.

@subsection{Type Constraints}

We will use the keyword @r[st:] (being a short form for “such that”)
to denote type constraints, as in:
@racketblock[
  st: Constraint1 Constraint2 ...
]
@(noindent)
The constraints we will consider will be subtyping constraints of the form:
@racketblock[
  (<: A B C ...)
]
meaning @r[A] is a subtype of @r[B], which is a subtype of @r[C], etc.

Thus, the type @r[(Fun Self Super -> Self st: (<: Self Super))] means
“for all types @r[Self] and @r[Super] such that @r[Self] is a subtype of @r[Super],
a function of two arguments of respective types @r[Self] and @r[Super],
returning a value of type @r[Self]”,
the universal quantification being implicit, at the top-level declaration.

@section[#:tag "Appendix_D"]{Fixed-Point functions}

In @(section1), we quickly went over the fixed-point function @r[fix]
that we used to instantiate prototypes.

Reminder:
A function prototype @r[(Proto A B)] is function @r[p : (Fun A B -> A)]
where @r[A] and @r[B] are function types, and @r[A] is a subtype of @r[B].
Thus, @r[p] takes a function @r[f] of type @r[A], and a function @r[B] of type @r[B],
and returns a new function @r[g] of same type @r[A].

To instantiate a prototype is get a function of type @r[A]
from a function of type @r[B].
Given these premises, there is one and only one construction that
allows us to and only one way to get an @r[A]: it's by calling @r[p].
But to call @r[p], we need to already have an @r[A]!
Where do we get this function of type @r[A] to begin with?
That's where the magic of fixed-point functions comes in:
they will somehow @emph{tie a knot}, and get a reference to the
function being defined, even before the function is defined.

Here is how we want a fixed point to look like:
@Examples[
(define (overly-simple-fix p b)
  (define f (p f b))
  f)
]
@(noindent)
Unhappily, this does not work in Scheme, because Scheme is eager:
the call to @r[p] needs to fully evaluate its arguments,
but @r[f] has not been fully defined yet, so the call is invalid.
Some Scheme implementations may detect that this definition tries to use
@r[f] before it is defined and raise an error at compile-time.
Some implementations will initially bind @r[f] to a magic “unbound” marker,
and trying to use @r[f] before it is defined will result in an error at runtime.
In yet other implementations, @r[f] will initially be bound to
some default “null” value such as @r[#f] or @r[(void)]
that will be used without immediately raising an error---until
you try to call @r[f] while expecting it to be a function, and then
the implementation will raise a runtime error while you are left wondering
why the program is trying to call this null value.
Finally, some reckless implementations may try to use @r[f]
before the data frame was even properly initialized at all,
and some random low-level value is used that might not make sense with
respect to the garbage collector, and you'll eventually dance fandango on core.

Yet, in a lazy language, the above definition works!
Indeed in Nix, you can write the equivalent definition:
@nix{
  let fix = p: b: let f = p f b; in f
}
@(noindent)
In Scheme, we can similarly write:
@Examples[
(define (delayed-fix p b)
  (define f (delay (p f b)))
  f)
]
@(noindent)
But in this only works if @r[p] accepts delayed computations as arguments
rather than direct function values (and still eagerly computes the result from them).
Then we have will have:
@racketblock[
  (code:comment "(deftype (DelayedProto A B) (Fun (Delayed A) (Delayed B) -> A))")
  (code:comment "delayed-fix : (Fun (DelayedProto A B) (Delayed B) -> (Delayed A))")
]
@(noindent)
On the other hand, this works for arbitrary types @r[A] and @r[B],
and not just for function types!

So, how do we get around this issue without delay?
One solution would be as follows---can you tell why it works,
and why it is not fully satisfactory?
@Examples[
(define (fix--0 p b)
  (define (f . i) (apply (p f b) i))
  f)
]
@(noindent)
First, why it works: by making @r[f] a function, we can recursively refer to @r[f]
from within the body of function @r[f] itself, since by the time this reference
is used, @r[f] was called, and by the time @r[f] was called, @r[f] was defined.
Thus we can give @r[f] to @r[p] to compute the fixed-point value @r[(p f b)].
But by that time, we're trying to call the fixed point, so
we take all the input arguments passed to @r[f] in a list @r[i],
and we pass them all forward to the fixed-point expression
using the builtin function @r[apply].
All is well. Try it, it works.

However, there is an issue:
@r[fix--0] calls @r[p] again at every invocation of the fixed-point @r[f].
Therefore, if @r[p] makes expensive computations,
it will pay to recompute them every time from scratch.
Worse, if @r[p] wants to build data structures
meant to be shared between invocations, such as a cache,
this sharing will be lost between calls to @r[f].
There can be no sharing of information between calls to @r[f].
No pre-computation, no cacheing, no memoization, no shared mutable state.

Therefore a better solution, that does allow for sharing computations
and state between invocations of the fixed-point result, is:
@Examples[
(define (fix--1 p b)
  (define f (p (λ i (apply f i)) b))
  f)
]
@(noindent)
That's the same as the fix function from @(section1).
Note how the anonymous lambda closure does part of the “protection”
or “delay” whereby the recursive data structure will only be called
after @r[f] is defined, but relies on @r[p] not causing its first argument
to be called during its evaluation, only stored in a data structure
or in a closure to be called later after @r[p] has returned.

If you do not like internal defines, you can write the same function
equivalently using @r[letrec], as:
@Examples[
(define (fix--2 p b)
  (letrec ((f (p (λ i (apply f i)) b)))
    f))
]

@(noindent)
And if you do not even like @r[letrec], you can use a Y-combinator variant: @; TODO cite
@Examples[
(define (fix--3 p b)
  ((λ (yf) (yf yf)) (λ (yf) (p (λ i (apply (yf yf) i)) b))))
]

@(noindent)
In practice, a lazy variant such as the one using @r[delay] above
will be the most usable as well as the most general,
though laziness is not colloquial in the applicative language Scheme.

@section[#:tag "Appendix_E"]{Note for code minimalists}

In our introduction, we described the @r[fix] and @r[mix] functions
in only 38 @r[cons] cells or 109 characters of Scheme
(counting one extra @r[cons] cell per top-level form,
but no @r[cons] cell for the implicit @r[begin]).
We can do even shorter with various extensions.
MIT Scheme and after it Racket, Gerbil Scheme, and more,
allow you to write curried function definitions, to cut 2 @r[cons] cells and 9 characters.
@Examples[
(define ((mix p q) f b) (p f (q f b)))
]
@(noindent)
And then we'd have Object Orientation in only 36 @r[cons] cells, 100 characters.

Then again, in Gerbil Scheme, we could get it down to only 34 @r[cons] cells,
and 88 characters, counting newline:
@racketblock[
(def (fix p b) (def f (p (cut apply f <>) b)) f)
]

@(noindent)
Or, compressing spaces, to 24 @r[cons] cells and 73 characters,
not counting newline, since we elide unnecessary spaces:
@racketblock[
(def(fix p b)(def f(p(cut apply f <>)b))f)(def((mix p q)f b)(p f(q f b)))
]

Science in general, and Computer Science in particular,
is about conceptual minimalism.
@; TODO cite Occam's Razor? Charles Sanders Pierce on abductive reasoning?
While algorithmic complexity @; TODO cite Li & Vitanyi about Kolmogorov Complexity?
is defined only up to an additive constant
determined up to a choice of a base computing system,
simple variants of the λ-calculus provide sensible conventional such bases,
and the pure subset of Scheme we use is one of them.
The above “code golf” then suggests that our proposed approach
to formalizing Object-Oriented Programming indeed exhibits
simple foundational principles.

@(finalize-examples/module poof)

@;;; Make sure to comment this out before to submit:
@;@table-of-contents[]
@;;;only works for HTML output: @local-table-of-contents[#:style 'immediate-only]
