## NB: If racket complains about some modules missing, try: make prerequisites

# Default target: the latest slides
all: ltuo # slides eoomi

# Default slides: the next talk
slides: slides-2025-racketcon # slides-2023-njpls slides-2024-lambdaconf slides-2025-shu

.DUMMY: all pdf view test repl prerequisites fare links \
  preslides slides \
  slides-2021-scheme-workshop slides-2023-njpls slides-2024-lambdaconf \
  %.preview %.view eoomi

UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
PDFVIEWER=xpdf -z width -fullscreen
#PDFVIEWER=evince --presentation
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
build/poof.pdf: poof.scrbl poof.bib header.tex util/eval-check.rkt util/examples-module.rkt util/util.rkt build/resources
#scribble --dest build --pdf --style header.tex poof.scrbl
#	cd build ; scribble --pdf ../poof.scrbl
	scribble --dest build --pdf poof.scrbl
view: build/poof.pdf
	$(PDFVIEWER) $<
pdf: build/poof.pdf

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
build/slides-2021-scheme-workshop.pdf: slides-2021-scheme-workshop.rkt
	slideshow -o $@ --pdf $<

# Publishing it all on fare's server http://fare.tunes.org/files/cs/poof.pdf
fare: poof.pdf
	cp $< ~/files/cs/
	rsync -av $< bespin:files/cs/

links:
	mkdir -p build ; ln -sf ../resources ../util ../poof.bib ../header.tex ../C3_linearization_example.eps build/

# Slide for njpls2023
slides-2023-njpls: build/slides-2023-njpls.html
build/slides-2023-njpls.html: slides-2023-njpls.rkt util/reveal.rkt util/util.rkt util/coop.rkt util/protodoc.rkt util/coop.scm build/resources
	racket $< > $@.tmp && mv $@.tmp $@ || { rm -f $@.tmp ; exit 42;}

# Slides for LambdaConf 2024
slides-2024-lambdaconf: build/slides-2024-lambdaconf.html
build/slides-2024-lambdaconf.html: slides-2024-lambdaconf.rkt util/reveal.rkt util/util.rkt util/coop.rkt util/protodoc.rkt util/coop.scm
	racket $< > $@.tmp && mv $@.tmp $@ || { rm -f $@.tmp ; exit 42;}

# Slides for visit to SHU in 2025
slides-2025-shu: build/slides-2025-shu.html
build/slides-2025-shu.html: slides-2025-shu.rkt util/reveal.rkt util/util.rkt util/coop.rkt util/protodoc.rkt util/coop.scm
	racket $< > $@.tmp && mv $@.tmp $@ || { rm -f $@.tmp ; exit 42;}

slides-2025-racketcon: build/slides-2025-racketcon.html
build/slides-2025-racketcon.html: slides-2025-racketcon.rkt util/reveal.rkt util/util.rkt util/coop.rkt util/protodoc.rkt util/coop.scm
	racket $< > $@.tmp && mv $@.tmp $@ || { rm -f $@.tmp ; exit 42;}

# New paper for 2024 (?)
build/eoomi2024.pdf: eoomi2024.scrbl poof.bib header.tex util/eval-check.rkt util/examples-module.rkt util/util.rkt build/resources
	scribble --dest build --style header.tex --pdf $<
eoomi: build/eoomi2024.pdf
	$(PDFVIEWER) $<

# Side paper: LTUO
build/ltuo.pdf: ltuo.scrbl ltuo.bib header.tex util/eval-check.rkt util/examples-module.rkt util/util.rkt build/resources
#scribble --dest build --pdf --style header.tex ltuo.scrbl
#	cd build ; scribble --pdf ../ltuo.scrbl
	scribble --dest build --pdf ltuo.scrbl
build/ltuo.html: ltuo.scrbl ltuo.bib util/eval-check.rkt util/examples-module.rkt util/util.rkt build/resources
	scribble --dest build --html ltuo.scrbl
ltuo: build/ltuo.pdf
	$(PDFVIEWER) $<
ltuopdf: build/ltuo.pdf
