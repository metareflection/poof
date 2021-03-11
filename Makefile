all: view

.DUMMY: all test pdf PDF eval

pdf: poof.pdf

poof.pdf: poof.scrbl poof.bib util/eval-check.rkt util/examples-module.rkt
	scribble --pdf poof.scrbl

test: poof.scrbl
	racket poof.scrbl

PDFVIEWER=evince --presentation

view: poof.pdf
	$(PDFVIEWER) $<
