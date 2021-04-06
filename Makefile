all: view

.DUMMY: all test pdf PDF eval prerequisites

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
PDFVIEWER=evince --presentation
endif
ifeq ($(UNAME_S),Darwin)
PDFVIEWER=open
endif

pdf: poof.pdf

poof.pdf: poof.scrbl poof.bib util/eval-check.rkt util/examples-module.rkt
	scribble --pdf poof.scrbl

test: poof.scrbl
	racket poof.scrbl

view: poof.pdf
	$(PDFVIEWER) $<

prerequisites:
	for i in scribble-abbrevs scribble-minted scribble-math ; do \
	  raco pkg install $$i ; \
	done
