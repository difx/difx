
bin_PROGRAMS = \
	analysis

LDADD = \
	$(top_builddir)/src/libbeamformer.la \
	-larmadillo -lblas -llapack

analysis_CPPFLAGS = -I../src/

analysis_SOURCES = \
	analysis.cpp

