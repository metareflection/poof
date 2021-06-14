# Prototypes: Object Orientation, Functionally

This directory contains a Racket Scribble document [`poof.scrbl`](poof.scrbl),
simultaneously a printable document and a runnable library,
that reconstructs the essence of Object-Oriented Programming (OOP)
from the point of view of pure Functional Programming (FP), with dynamic or dependent types,
using Prototypes as the fundamental abstraction,
and Scheme as the illustration language.

The code herein is available as free software under the Apache License, version 2.0.
See the file [LICENSE](LICENSE).

To create the document:

    make


To run the tests:

    make test


To load the library and experiment with it at the REPL:

    make repl
