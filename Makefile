all: view

.DUMMY: all test pdf PDF eval

pdf: poof.pdf

poof.pdf: poof.scrbl
	scribble --pdf poof.scrbl

test: poof.scrbl
	racket poof.scrbl

scm:
	scheme 00-all-poof.scm 99-exit.scm

PDFVIEWER=evince --presentation

view: poof.pdf
	$(PDFVIEWER) $<
