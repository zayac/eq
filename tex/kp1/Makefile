all: view

CPP := cpp
CPPFLAGS := -P
CPPINPUT := kp1.tex
CPPOUTPUT := kp1.prep.tex
PDFOUTPUT := $(CPPOUTPUT:.tex=.pdf)
TOPDF := pdflatex

# Any PDF viewer
VIEWER := evince

preprocess:
	$(CPP) $(CPPFLAGS) $(CPPINPUT) > $(CPPOUTPUT)

makepdf: preprocess
	 $(TOPDF) $(CPPOUTPUT)

view: makepdf
	$(VIEWER) $(PDFOUTPUT)
