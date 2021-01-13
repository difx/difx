AM_CPPFLAGS = -I$(top_srcdir)/vexdatamodel

bin_PROGRAMS = \
	vexpeek \
	vex2difx \
	vex2v2d

LDADD = \
	$(top_builddir)/vexdatamodel/libvexdatamodel.la \
	$(top_builddir)/vex/libvex.la

vex2difx_SOURCES = \
	applycorrparams.cpp \
	applycorrparams.h \
	corrparams.cpp \
	corrparams.h \
	freq.cpp \
	freq.h \
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
	vex2difx.cpp

vex2v2d_SOURCES = \
	testvex.h \
	testvex.cpp \
	vex2v2d.cpp

vexpeek_SOURCES = \
	testvex.h \
	testvex.cpp \
	timeutils.h \
	timeutils.cpp \
	vexpeek.cpp
