# Prototypes: Object Orientation, Functionally

This directory contains the source for our paper
“Prototypes: Object Orientation, Functionally”.
View the [precompiled PDF](http://fare.tunes.org/files/cs/poof.pdf).
The same [Racket Scribble source](poof.scrbl) can produces
simultaneously a printable document and a runnable library.

One version of this paper was published and presented at the
[Scheme and Functional Programming Workshop 2021](https://icfp21.sigplan.org/home/scheme-2021),
see the [presentation video](https://www.youtube.com/watch?v=2szKoUQoNm8&list=PLyrlk8Xaylp7NvZ1r-eTIUHdyHQg0auvo&index=10),
with the source code at git tags `Scheme-Workshop-2021-paper` for the paper as published then,
and `Scheme-Workshop-2021-slides` for the slides as presented.

The current version of this paper was modified from using the acmart style to the lipics style
to submit it to [ECOOP 2022](https://2022.ecoop.org/track/ecoop-2022-papers).

## Abstract

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

## License

The code herein is available as free software under the Apache License, version 2.0.
See the file [LICENSE](LICENSE).

## Build instructions

To install dependencies:

    make prerequisites

To create the document:

    make

To run the tests:

    make test

To load the library and experiment with it at the REPL:

    make repl

## Related Talk by Same Authors

- François-René Rideau, *Prototype Object Programming in Gerbil Scheme*,
  talk given on 2020-11-07 at LispNYC,
  [video](https://vimeo.com/495817581),
  [notes](https://github.com/fare/gerbil-poo/blob/master/doc/prototypes.md).

## Proposed renamings

Many concepts in the paper that have names that could bring confusion
to those used to class-based object systems.
In particular, what we call "object" in this paper
fits well the paradigm of prototype object orientation,
but not at all the paradigm of class-based object orientation.

In a future version of this paper, or in future works,
we might use the following proposed renaming, or some variant thereof:

| proposed new name | name in current paper | other possibilities | meaning |
|------|-----|-----|-----|
| target | instance | computation, value, fixed-point, self, result | the complete computation that was incrementally specified, or the value resulting from evaluating this computation (further distinguish them as "target computation" and "target value"?) |
| mixin | prototype | increment, specification, spec, component, trait | an increment of specification for a computation (name "mixin" taken from Flavors, popularized by Bracha) |
| prototype | object | mixin, trait | the conflation (conjunction with magic implicit coercions) of mixin (formerly prototype) and target (formerly instance) in a single entity |
| wrapper | prototype function | | the composable function that takes the partial computation so far and extends it (name "wrapper" taken from Cook) |
| element | class instance | object | what is called "object" in class OO, an element of the type incrementally specified as a class (that is itself the prototype/object whose target/instance is a type) |
| record | record | structure, instance, labeled product, table, object | a mapping from name/symbol to values, that typically serves as both targets and elements above, but can be considered without any OO |
| tagged record | | | a record with special entries (mapping for special labels?) for the metadata such as the mixin used to compute the record or the variant of a sum type or caches used by the MOP. |
