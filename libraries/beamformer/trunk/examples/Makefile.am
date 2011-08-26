
bin_PROGRAMS = \
	analysis \
	benchmark

LDADD = \
	$(top_builddir)/src/libbeamformer.la \
	-larmadillo -lblas -llapack

analysis_CPPFLAGS = -I../src/

analysis_SOURCES = analysis.cpp

benchmark_CPPFLAGS = -I../src/

benchmark_SOURCES = benchmark.cpp
