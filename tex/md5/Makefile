all: view

CPP := cpp
CPPFLAGS := -P
CPPINPUT := md5.tex
CPPOUTPUT := md5.prep.tex
PDFOUTPUT := $(CPPOUTPUT:.tex=.pdf)
TOPDF := pdflatex
VIEWER := evince

preprocess:
	$(CPP) $(CPPFLAGS) $(CPPINPUT) > $(CPPOUTPUT)

makepdf: preprocess
	 $(TOPDF) $(CPPOUTPUT)

view: makepdf
	$(VIEWER) $(PDFOUTPUT)
