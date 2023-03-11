AM_CPPFLAGS = \
	-DDATADIR=\"$(pkgdatadir)\" \
	-I$(top_srcdir)/libfitdelay


bin_PROGRAMS = \
	vdifsim

dist_bin_SCRIPTS = \
	makesimfilelist \
	runvdifsim

noinst_PROGRAMS = \
	testccr

vdifsim_SOURCES = \
	common.c \
	common.h \
	configuration.c \
	configuration.h \
	datastream.c \
	datastream.h \
	model.c \
	model.h \
	options.c \
	options.h \
	poly.c \
	poly.h \
	sim.c \
	sim.h \
	vdifsim.c

testccr_SOURCES = \
	testccr.c
