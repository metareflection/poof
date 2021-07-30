# Prototypes: Object Orientation, Functionally

This directory contains the source for our paper
“Prototypes: Object Orientation, Functionally”, as accepted at the
[Scheme and Functional Programming Workshop 2021](https://icfp21.sigplan.org/home/scheme-2021).
View the [precompiled PDF](http://fare.tunes.org/files/cs/poof.pdf).
The same [Racket Scribble source](poof.scrbl) can produces
simultaneously a printable document and a runnable library.

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
