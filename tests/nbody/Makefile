all: view

CPP := cpp
CPPFLAGS := -P
CPPINPUT := nbody.tex
CPPOUTPUT := nbody.prep.tex
PDFOUTPUT := $(CPPOUTPUT:.tex=.pdf)
TOPDF := pdflatex
VIEWER := evince

preprocess:
	$(CPP) $(CPPFLAGS) $(CPPINPUT) > $(CPPOUTPUT)

makepdf: preprocess
	 $(TOPDF) $(CPPOUTPUT)

view: makepdf
	$(VIEWER) $(PDFOUTPUT)
