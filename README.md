# Prototypes: Object Orientation, Functionally

This directory contains the source for our paper
“Prototypes: Object Orientation, Functionally”.
View the [precompiled PDF](http://fare.tunes.org/files/cs/poof.pdf).
The same [Racket Scribble source](poof.scrbl) can produces
simultaneously a printable document and a runnable library.

One version of this paper was published and presented at the
[Scheme and Functional Programming Workshop 2021](https://icfp21.sigplan.org/home/scheme-2021),
see the [presentation video](https://www.youtube.com/watch?v=2szKoUQoNm8&list=PLyrlk8Xaylp7NvZ1r-eTIUHdyHQg0auvo&index=10),
with the source code at git tags [`Scheme-Workshop-2021-paper`](https://github.com/metareflection/poof/tree/Scheme-Workshop-2021-paper) for the paper as published then,
and `Scheme-Workshop-2021-slides` for the [slides](slides-2021-scheme-workshop.rkt)) as presented.

A presentation was made at LambdaConf 2024, with a longer variant later made online for
[Legends of LambdaConf](https://youtu.be/oAzBfY8OtG0).

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

## Citation
```bibtex
@Conference{poof2021,
  title = {Prototypes: Object-Orientation, Functionally},
  author = {François-René Rideau and Alex Knauth and Nada Amin},
  year = {2021},
  booktitle = {Scheme and Functional Programming Workshop},
  url = {https://github.com/metareflection/poof},
}
```
