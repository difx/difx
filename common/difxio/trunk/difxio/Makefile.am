h_sources = \
	difx_input.h \
	parsedifx.h \
	parsevis.h \
	difx_write.h
c_sources = \
	difx_input.c \
	difx_input_merge.c \
	difx_input_job.c \
	difx_input_freq.c \
	difx_input_antenna.c \
	difx_input_datastream.c \
	difx_input_baseline.c \
	difx_input_config.c \
	difx_input_model.c \
	difx_input_poly_model.c \
	difx_input_scan.c \
	difx_input_eop.c \
	difx_input_spacecraft.c \
	difx_input_if.c \
	difx_input_source.c \
	difx_input_flag.c \
	difx_input_polyco.c \
	difx_input_pulsar.c \
	difx_write.c \
	difx_write_delay.c \
	difx_write_uvw.c \
	difx_write_rate.c \
	difx_write_im.c \
	difx_write_calc.c \
	parsedifx.c \
	parsevis.c

library_includedir = $(includedir)/difxio
library_include_HEADERS = $(h_sources)

INCLUDES = -I$(top_srcdir)

lib_LTLIBRARIES = \
	libdifxio.la

libdifxio_la_SOURCES = $(h_sources) $(c_sources)
libdifxio_la_LDFLAGS = -version-info $(LIBRARY_VERSION)

