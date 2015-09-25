h_sources = \
	difx_input.h \
	parsedifx.h \
	parsevis.h \
	difx_write.h \
	difx_calculator.h \
	difx_tcal.h \
	difx_radioastron.h
c_sources = \
	difx_input.c \
	difx_input_merge.c \
	difx_job.c \
	difx_freq.c \
	difx_antenna.c \
	difx_datastream.c \
	difx_baseline.c \
	difx_config.c \
	difx_poly_model.c \
	difx_scan.c \
	difx_eop.c \
	difx_spacecraft.c \
	difx_if.c \
	difx_source.c \
	difx_flag.c \
	difx_polyco.c \
	difx_pulsar.c \
	difx_phasedarray.c \
	difx_rule.c \
	difx_write.c \
	difx_write_im.c \
	difx_write_calc.c \
	difx_write_input.c \
	difx_write_pulsar.c \
	difx_calculator.c \
	difx_threads.c \
	difx_tcal.c \
	remap.c \
	parsedifx.c \
	parsevis.c

library_includedir = $(includedir)/difxio
library_include_HEADERS = $(h_sources)

AM_CPPFLAGS = -I$(top_srcdir)

lib_LTLIBRARIES = \
	libdifxio.la

libdifxio_la_SOURCES = $(h_sources) $(c_sources)
libdifxio_la_LDFLAGS = $(spicelibs) -version-info $(LIBRARY_VERSION)

