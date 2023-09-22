#
# $Id: Makefile.am 3399 2021-10-26 20:05:16Z barrettj $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@ -DPS2PDF=\"@PS2PDF@\"

noinst_HEADERS = fplot.h

bin_PROGRAMS = fplot

if HAVE_X11

fplot_SOURCES = fplot.c parse_cmdline.c

fplot_LDADD = @MSG_LIB@ @DFIO_LIB@ @UTIL_LIB@ @X_FPLOT_LIB@ @X11_LIB@
fplot_DEPENDENCIES = @MSG_DEP@ @DFIO_DEP@ @UTIL_DEP@

else !HAVE_X11
fplot_SOURCES = you_lose.c
endif !HAVE_X11

you_lose.c: Makefile.am
	echo 'main(){return(puts("Missing X11 or PGPLOT\n"));}' > $@

CLEANFILES = you_lose.c

#
# eof
#
