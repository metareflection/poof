# Prototypes: Object Orientation, Functionally

This directory contains a Racket Scribble document [`poof.scrbl`](poof.scrbl),
simultaneously a printable document and a runnable library,
that reconstructs the essence of Object-Oriented Programming (OOP)
from the point of view of pure Functional Programming (FP), with dynamic or dependent types,
using Prototypes as the fundamental abstraction,
and Scheme as the illustration language.

See [this PDF](http://fare.tunes.org/files/cs/poof.pdf).

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
