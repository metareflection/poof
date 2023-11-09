## NB: If racket complains about some modules missing, try: make prerequisites

# Default target: the latest slides
all: slides

# Default slides: the next talk
slides: slides-2023-njpls # slides-2024-lambdaconf

.DUMMY: all pdf view test repl prerequisites fare \
  preslides slides \
  slides-2021-scheme-workshop slides-2023-njpls slides-2024-lambdaconf \
  %.preview %.view

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
PDFVIEWER=evince --presentation
endif
ifeq ($(UNAME_S),Darwin)
PDFVIEWER=open
endif

# Installing all the prerequisite Racket modules necessary to compile the paper and slides
prerequisites:
	for i in scribble-abbrevs scribble-minted scribble-math unstable-lib slideshow-text-style ; do \
	  raco pkg install --auto --update-deps $$i ; \
	done

# Main dish: The Scheme Workshop 2021 paper
poof.pdf: poof.scrbl poof.bib header.tex util/eval-check.rkt util/examples-module.rkt
#scribble --pdf --style header.tex poof.scrbl
	scribble --pdf poof.scrbl
view: poof.pdf
	$(PDFVIEWER) $<
pdf: poof.pdf

# Testing the code in the paper itself
test: poof.scrbl
	racket poof.scrbl

# A Racket REPL with the code in the paper
repl:
	racket --lib racket/base --require main.rkt --repl

# Slides for the Scheme Workshop 2021, using Racket Slideshow
# Previewing the slides. make slides-2021-scheme-workshop.preview p=14 to start at page 14.
# NB: Alt-M for the mouse!
slides-2021-scheme-workshop: slides-2021-scheme-workshop.preview
slides-2021-scheme-workshop.preview: slides-2021-scheme-workshop.rkt
	slideshow --comment-on-slide --elapsed-time --start $${p:-1} $<
slides-2021-scheme-workshop.view: slides-2021-scheme-workshop.rkt
	slideshow --preview --elapsed-time $<
slides-2021-scheme-workshop.pdf: slides-2021-scheme-workshop.rkt
	slideshow --pdf $<

# Publishing it all on fare's server http://fare.tunes.org/files/cs/poof.pdf
fare: poof.pdf
	cp $< ~/files/cs/
	rsync -av $< bespin:files/cs/

# Slide for njpls2023
slides-2023-njpls: slides-2023-njpls.html
slides-2023-njpls.html: slides-2023-njpls.rkt util/reveal.rkt util/util.rkt util/coop.rkt util/protodoc.rkt util/coop.scm
	racket $< > $@.tmp && mv $@.tmp $@ || { rm -f $@.tmp ; exit 42;}

# Slides for LambdaConf 2024
slides-2024-lambdaconf: slides-2024-lambdaconf.html
slides-2024-lambdaconf.html: slides-2024-lambdaconf.rkt util/reveal.rkt util/util.rkt util/coop.rkt util/protodoc.rkt util/coop.scm
	racket $< > $@.tmp && mv $@.tmp $@ || { rm -f $@.tmp ; exit 42;}
