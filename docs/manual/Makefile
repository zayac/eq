DOC	  := eq
SOURCES	  := eq.texi macros.texi

TEXI2PDF  := texi2pdf
TEXI2POD  := perl texi2pod.pl
POD2MAN	  := pod2man --center "Eq" \
		     --release="eq-0.1.1"\
		     --date=$(shell date +%F)
TEXI2HTML := makeinfo --html --no-split \
		      --no-number-section \
		      --css-ref=eq.css

all:
	cp *.texi *.js texi2pod.pl eq.css  Makefile ./build
	cd ./build && make doc

doc: pdf-docs man-docs html-docs

pdf-docs: $(SOURCES)
	$(TEXI2PDF) $(DOC).texi

man-docs: $(SOURCES)
	$(TEXI2POD) $< > $(DOC).pod
	$(POD2MAN) $(DOC).pod > $(DOC).1

html-docs: $(SOURCES)
	$(TEXI2HTML) $(DOC).texi

clean:
	if [ -d build ]; then  \
		rm -rf build;  \
		mkdir build;   \
	fi;
