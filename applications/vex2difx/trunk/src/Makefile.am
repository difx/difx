bin_PROGRAMS = \
	vex2difx \
	vex2script \
	vex2opt \
	rdbetsys \
	vlatsys \
	vexpeek

LDADD = \
	$(top_builddir)/vex/libvex.la

vex2difx_SOURCES = \
	vextables.h \
	vextables.cpp \
	vexload.h \
	vexload.cpp \
	corrparams.h \
	corrparams.cpp \
	util.h \
	util.cpp \
	vex2difx.cpp 

vlatsys_SOURCES = \
	vextables.h \
	vextables.cpp \
	vexload.h \
	vexload.cpp \
	corrparams.h \
	corrparams.cpp \
	vlatsys.cpp

rdbetsys_SOURCES = \
	vextables.h \
	vextables.cpp \
	vexload.h \
	vexload.cpp \
	corrparams.h \
	corrparams.cpp \
	rdbetsys.cpp

if USEMYGETLINE
vex2script_SOURCES = \
	vextables.h \
	vextables.cpp \
	vexload.h \
	vexload.cpp \
	corrparams.h \
	corrparams.cpp \
	pystream.h \
	pystream.cpp \
	vex2script.cpp \
	getline.c
else
vex2script_SOURCES = \
	vextables.h \
	vextables.cpp \
	vexload.h \
	vexload.cpp \
	corrparams.h \
	corrparams.cpp \
	pystream.h \
	pystream.cpp \
	vex2script.cpp
endif

vex2opt_SOURCES = \
	vextables.h \
	vextables.cpp \
	vexload.h \
	vexload.cpp \
	corrparams.h \
	corrparams.cpp \
	optscans.h \
	optscans.cpp \
	optsources.cpp \
	optsources.h \
	optresources.cpp \
	optresources.h \
	vex2opt.cpp 

vexpeek_SOURCES = \
	vexpeek.cpp \
	vextables.cpp \
	vextables.h \
	vexload.cpp \
	vexload.h \
	corrparams.cpp \
	corrparams.h
