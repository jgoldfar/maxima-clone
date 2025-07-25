# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#       $Id: Makefile.am,v 1.50 2011-03-09 11:30:45 villate Exp $
#

include $(top_srcdir)/common.mk

if ENABLE_BUILD_DOCS
   DOC_DIR = doc
endif

SUBDIRS = msgs $(DOC_DIR)
if XMAXIMA_EXE
SUBDIRS += win32
else
if WINKILL_EXE
SUBDIRS += win32
endif
endif

genericdir = $(xmaximadir)

genericdirDATA = demos.html examples.html primer.html intro.html plotmaxima.html net.sourceforge.maxima.png net.sourceforge.maxima.svg net.sourceforge.maxima.gif net.sourceforge.maxima.ico

appicondir = $(datarootdir)/icons
dist_appicon_DATA = xmaxima.png xmaxima.svg

bin_SCRIPTS = xmaxima

TKSOURCES = \
	Tkmaxima/COPYING.tcl \
	Tkmaxima/Cygwin.tcl \
	Tkmaxima/Constants.tcl \
	Tkmaxima/Preamble.tcl \
	Tkmaxima/Readdata.tcl \
	Tkmaxima/Getdata1.tcl \
	Tkmaxima/Macros.tcl \
	Tkmaxima/Proxy.tcl \
	Tkmaxima/Send-some.tcl \
	Tkmaxima/Plotting.tcl \
	Tkmaxima/Fonts.tcl \
	Tkmaxima/colors.tcl \
	Tkmaxima/Private.tcl \
	Tkmaxima/Getopt.tcl \
	Tkmaxima/Parse.tcl \
	Tkmaxima/Textinsert.tcl \
	Tkmaxima/Printops.tcl \
	Tkmaxima/Push.tcl \
	Tkmaxima/Plotconf.tcl \
	Tkmaxima/Adams.tcl \
	Tkmaxima/Rk.tcl \
	Tkmaxima/Plotdf.tcl \
	Tkmaxima/Plot2d.tcl \
	Tkmaxima/Matrix.tcl \
	Tkmaxima/Plot3d.tcl \
	Tkmaxima/scene.tcl \
	Tkmaxima/NPlot3d.tcl \
	Tkmaxima/EOpenplot.tcl \
	Tkmaxima/EMaxima.tcl \
	Tkmaxima/EHref.tcl \
	Tkmaxima/Browser.tcl \
	Tkmaxima/Bindings.tcl \
	Tkmaxima/Wmenu.tcl \
	Tkmaxima/Myhtml.tcl \
	Tkmaxima/Myhtml1.tcl \
	Tkmaxima/Base64.tcl \
	Tkmaxima/Bitmaps.tcl \
	Tkmaxima/Tryembed.tcl \
	Tkmaxima/OpenMath.tcl \
	Tkmaxima/NConsole.tcl \
	Tkmaxima/String.tcl \
	Tkmaxima/RunMaxima.tcl \
	Tkmaxima/Menu.tcl \
	Tkmaxima/Paths.tcl \
	Tkmaxima/Gui.tcl \
	Tkmaxima/Tkmaxima.tcl 


XMAXIMA_FILES = $(TKSOURCES)
XMAXIMASOURCES = Tkmaxima/Header.tcl autoconf-variables.tcl $(TKSOURCES) xmaxima-trailer.tcl

xmaxima: $(XMAXIMASOURCES)
	rm -f xmaxima && \
	cat Tkmaxima/Header.tcl > xmaxima && \
	(cd "$(abs_top_srcdir)/interfaces/xmaxima/" ; cat $(XMAXIMA_FILES)) >> xmaxima && \
	cat autoconf-variables.tcl >> xmaxima && \
	cat "$(abs_top_srcdir)/interfaces/xmaxima/xmaxima-trailer.tcl" >> xmaxima && \
	chmod +x xmaxima

EXTRA_DIST = $(genericdirDATA) $(XMAXIMA_FILES) Tkmaxima/Header.tcl.in xmaxima-trailer.tcl


# Developer convenience target
# ETAGS for use with Emacs TAGS, epecially for tags-replace

TCLSH = tclsh

TAGS:
	echo x*.tcl Tkmaxima/*.tcl | xargs $(TCLSH) ./Utils/etags.tcl

tclIndex:: $(TKSOURCES)
	echo "source ../ObjTcl/Object.tcl; auto_mkindex . $(TKSOURCES)" | $(TCLSH)

# Create Tkmaxima/tclIndex if tclsh is available. Otherwise, create empty file
# to satisfy make.
Tkmaxima/tclIndex: $(TKSOURCES)
	$(TCLSH) < /dev/null > /dev/null 2>&1 && \
          (cd Tkmaxima ; \
           echo "source ../ObjTcl/Object.tcl; auto_mkindex . $(ALLFILES)" | \
           $(TCLSH)) || \
         touch Tkmaxima/tclIndex

clean-local:
	rm -f xmaxima
	rm -f Tkmaxima/tclIndex

FORCE:
