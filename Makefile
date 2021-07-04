all: view

.DUMMY: all pdf view test repl prerequisites

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
PDFVIEWER=evince --presentation
endif
ifeq ($(UNAME_S),Darwin)
PDFVIEWER=open
endif

poof.pdf: poof.scrbl poof.bib header.tex util/eval-check.rkt util/examples-module.rkt
#scribble --pdf --style header.tex poof.scrbl
	scribble --pdf poof.scrbl

pdf: poof.pdf

view: poof.pdf
	$(PDFVIEWER) $<

test: poof.scrbl
	racket poof.scrbl

repl:
	racket --lib racket/base --require main.rkt --repl

prerequisites:
	for i in scribble-abbrevs scribble-minted scribble-math ; do \
	  raco pkg install --auto --update-deps $$i ; \
	done

# Publishing on fare's server http://fare.tunes.org/files/cs/poof.pdf
fare: poof.pdf
	cp $< ~/files/cs/
	rsync -av $< bespin:files/cs/
