#
# $Id: Makefile.am 3300 2021-08-27 19:40:06Z gbc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@ -DPS2PDF=\"@PS2PDF@\"

noinst_HEADERS = fplot.h

bin_PROGRAMS = fplot

if HAVE_X11

fplot_SOURCES = fplot.c parse_cmdline.c

fplot_LDADD = @DFIO_LIB@ @UTIL_LIB@ @X_FPLOT_LIB@ @X11_LIB@
fplot_DEPENDENCIES = @DFIO_DEP@ @UTIL_DEP@

else !HAVE_X11
fplot_SOURCES = you_lose.c
endif !HAVE_X11

you_lose.c: Makefile.am
	echo 'main(){return(puts("Missing X11 or PGPLOT\n"));}' > $@

CLEANFILES = you_lose.c

#
# eof
#
