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

# Previewing the slides. make preslides p=14 to start at page 14.
# NB: Alt-M for the mouse!
preslides: poof-slides.rkt
	slideshow --comment-on-slide --elapsed-time --start $${p:-1} poof-slides.rkt

slides: poof-slides.rkt
	slideshow --preview --elapsed-time poof-slides.rkt

slides-pdf: poof-slides.rkt
	slideshow --pdf poof-slides.rkt

view: poof.pdf
	$(PDFVIEWER) $<

test: poof.scrbl
	racket poof.scrbl

repl:
	racket --lib racket/base --require main.rkt --repl

prerequisites:
	for i in scribble-abbrevs scribble-minted scribble-math unstable-lib slideshow-text-style ; do \
	  raco pkg install --auto --update-deps $$i ; \
	done

# Publishing on fare's server http://fare.tunes.org/files/cs/poof.pdf
fare: poof.pdf
	cp $< ~/files/cs/
	rsync -av $< bespin:files/cs/
