############################################################################
#                             Templates_Parser                             #
#                                                                          #
#                         Copyright (C) 2003-2008                          #
#                                 AdaCore                                  #
#                                                                          #
#  This library is free software; you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation; either version 2 of the License, or (at   #
#  your option) any later version.                                         #
#                                                                          #
#  This library is distributed in the hope that it will be useful, but     #
#  WITHOUT ANY WARRANTY; without even the implied warranty of              #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       #
#  General Public License for more details.                                #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this library; if not, write to the Free Software Foundation, #
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          #
#                                                                          #
#  As a special exception, if other files instantiate generics from this   #
#  unit, or you link this unit with other files to produce an executable,  #
#  this  unit  does not  by itself cause  the resulting executable to be   #
#  covered by the GNU General Public License. This exception does not      #
#  however invalidate any other reasons why the executable file  might be  #
#  covered by the  GNU Public License.                                     #
############################################################################

.SILENT: ada.sed doc clean

MAKEINFO        = $(shell which makeinfo 2> /dev/null)
DVIPS           = $(shell which dvips 2> /dev/null)
TEXI2DVI        = $(shell which texi2dvi 2> /dev/null)
PDFTEX          = $(shell which pdftex 2> /dev/null)


TMPLTTEXI = $(TMPLT:=.texi)


TEXIFILES = $(ADBFILES:=.texi) $(ADSFILES:=.texi) $(ADAFILES_NORUN:=.texi) \
		$(TMPLTTEXI)

GFLAGS	  = -I../src -I../include

DOCS	  = ssprep_ug.dvi ssprep_ug.info ssprep_ug.html \
		ssprep_ug.txt ssprep_ug.ps ssprep_ug.pdf



ALLDEPS = $(ADBFILES) $(ADSFILES) $(EXEFILES) $(TEXIFILES) $(TMPLTTEXI) \
	$(RESFILES) $(APIFILES)

doc: $(ALLDEPS) $(DOCS)

%.dvi: %.texi
ifneq (${TEXI2DVI},)
	echo Building $@
	-${TEXI2DVI} --expand --clean --quiet $<
else
	@echo "--------------------------------------------------------"
	@echo "texi2dvi not found, cannot build DVI or PS documentation"
	@echo "--------------------------------------------------------"
endif

%.pdf: %.texi
ifneq (${TEXI2DVI},)
ifneq (${PDFTEX},)
	echo Building $@
	${TEXI2DVI} -p --expand --clean --quiet $<
else
	@echo "-----------------------------------------------"
	@echo "pdftex not fond, cannot build PDF documentation"
	@echo "-----------------------------------------------"
endif
else
	@echo "--------------------------------------------------"
	@echo "texi2dvi not found, cannot build PDF documentation"
	@echo "--------------------------------------------------"
endif

%.ps: %.dvi
ifneq (${DVIPS},)
	echo Building $@
	-${DVIPS} -q $< -o $@
else
	@echo "------------------------------------------------------"
	@echo "dvips not found, cannot build POSTSCRIPT documentation"
	@echo "------------------------------------------------------"
endif

%.info: %.texi 
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build INFO documentation"
	@echo "---------------------------------------------------"
endif

%.html: %.texi 
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --html --no-split --css-include=tp.css --ifinfo $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build HTML documentation"
	@echo "---------------------------------------------------"
endif

%.txt: %.texi $(ADBFILES)
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --plaintext --no-headers $< --output $@
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build TEXT documentation"
	@echo "---------------------------------------------------"
endif

force:

clean:
	-rm -f $(EXEFILES) $(ADAFILES) $(TEXIFILES) *.res *.o *.ali genout \
		*.aux *.cp* *.fn *.ky *.pg *.toc *.tp *.vr *.dvi *.log *.ps \
		*.exe templates_parser*.ads.texi ada.sed \
		templates_parser.tar.gz
	-rm -f templates_parser.html templates_parser.pdf \
		templates_parser.info* templates_parser.txt
