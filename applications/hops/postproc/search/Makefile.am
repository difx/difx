#
# $Id: Makefile.am 4512 2026-05-10 21:48:06Z gbc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

# cpgplot.h
noinst_HEADERS = search.h

bin_PROGRAMS = search
check_PROGRAMS = soirch
TESTS = soirch
TESTS_ENVIRONMENT = LD_LIBRARY_PATH=@PGPLOT_DIR@:${LD_LIBRARY_PATH}

if HAVE_PGPLOT
if HAVE_X11
if HAVE_PNG

search_c = clear_srchdata.c parabola.c read_data.c sorter.c \
		 fill_grids.c grid_filler.c parse_cmdline.c search.c \
		 write_srchdata.c fit_peaks.c plot_srchdata.c sort_data.c

# revised code
search_CPPFLAGS = -DBIGGER=1 $(AM_CPPFLAGS)
search_SOURCES = $(search_c)

# original code
soirch_CPPFLAGS = -DBIGGER=0 $(AM_CPPFLAGS)
soirch_SOURCES = $(search_c)

search_LDADD = @MSG_LIB@ @AFIO_LIB@ @UTIL_LIB@ @PGP_LIB@ @PNG_LIB@ @X11_LIB@
search_DEPENDENCIES = @MSG_DEP@ @AFIO_DEP@ @UTIL_DEP@

soirch_LDADD = @MSG_LIB@ @AFIO_LIB@ @UTIL_LIB@ @PGP_LIB@ @PNG_LIB@ @X11_LIB@
soirch_DEPENDENCIES = @MSG_DEP@ @AFIO_DEP@ @UTIL_DEP@

else !HAVE_PNG
search_SOURCES = you_lose.c
soirch_SOURCES = you_lose.c
endif !HAVE_PNG
else !HAVE_X11
search_SOURCES = you_lose.c
soirch_SOURCES = you_lose.c
endif !HAVE_X11
else !HAVE_PGPLOT
search_SOURCES = you_lose.c
soirch_SOURCES = you_lose.c
endif !HAVE_PGPLOT

you_lose.c: Makefile.am
	echo '#include <stdio.h>' > $@
	echo 'int main()' >> $@
	echo '{return(puts("Missing PNG, X11, PGPLOT or GSL\n"));}' >> $@

CLEANFILES = you_lose.c

#
# eof
#
