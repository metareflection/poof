all: view

.DUMMY: all test pdf PDF eval

pdf: poof.pdf

poof.pdf: poof.scrbl
	scribble --pdf poof.scrbl

test: poof.scrbl
	racket poof.scrbl

PDFVIEWER=evince --presentation

view: poof.pdf
	$(PDFVIEWER) $<
