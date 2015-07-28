bin_PROGRAMS = \
	vexpeek \
	vex2difx

LDADD = \
	$(top_builddir)/vex/libvex.la

VEX_DATAMODEL_SRC = \
	vex_antenna.cpp \
	vex_antenna.h \
	vex_basebanddata.cpp \
	vex_basebanddata.h \
	vex_channel.cpp \
	vex_channel.h \
	vex_clock.cpp \
	vex_clock.h \
	vex_eop.cpp \
	vex_eop.h \
	vex_exper.h \
	vex_if.cpp \
	vex_if.h \
	vex_mode.cpp \
	vex_mode.h \
	vex_networkdata.cpp \
	vex_networkdata.h \
	vex_scan.cpp \
	vex_scan.h \
	vex_setup.cpp \
	vex_setup.h \
	vex_source.cpp \
	vex_source.h \
	vex_stream.cpp \
	vex_stream.h \
	vex_subband.cpp \
	vex_subband.h \
	vex_data.h \
	vex_data.cpp

vex2difx_SOURCES = \
	$(VEX_DATAMODEL_SRC) \
	applycorrparams.cpp \
	applycorrparams.h \
	event.cpp \
	event.h \
	vexload.cpp \
	vexload.h \
	corrparams.cpp \
	corrparams.h \
	freq.cpp \
	freq.h \
	interval.cpp \
	interval.h \
	job.cpp \
	job.h \
	jobflag.cpp \
	jobflag.h \
	jobgroup.cpp \
	jobgroup.h \
	makejobs.cpp \
	makejobs.h \
	mediachange.cpp \
	mediachange.h \
	parserhelp.cpp \
	parserhelp.h \
	sanitycheck.cpp \
	sanitycheck.h \
	shelves.cpp \
	shelves.h \
	timeutils.cpp \
	timeutils.h \
	util.cpp \
	util.h \
	vex2difx.cpp


# eventually remove event.* from below:

vexpeek_SOURCES = \
	$(VEX_DATAMODEL_SRC) \
	event.cpp \
	event.h \
	vexload.cpp \
	vexload.h \
	timeutils.h \
	timeutils.cpp \
	interval.h \
	interval.cpp \
	util.h \
	util.cpp \
	vexpeek.cpp
