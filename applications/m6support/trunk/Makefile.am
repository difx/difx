#
# Makefile for Mark6 & ALMA support
#

SUBDIRS = .

AM_CPPFLAGS = -Wall

EXTRA_DIST = readme.sh gpl-3.0.txt

if WITH_FUSE
if HAVE_FUSE
if HAVE_LINUX
vdifuse = vdifuse
vdifuse_h = vdifuse.h vdifsg2.h vdifsig.h
prepone = prep-one-scan.sh
else
vdifuse =
vdifuse_h =
prepone =
endif
else
vdifuse =
vdifuse_h =
prepone =
endif
else
vdifuse =
vdifuse_h =
prepone = 
endif

dist_noinst_HEADERS = vdif.h vdif_epochs.h vdif_time_util.h \
	sg_access.h sc_stats.h exthdr.h m6scmodule.h $(vdifuse_h)
test_scripts = hammer.sh hammerplot.sh fillerfrac.sh statsplot.py glitchplot.sh

check_PROGRAMS =
TESTS = 

if HAVE_LINUX
scanext=exthdr.c almahdr.c r2dbehdr.c
scan_check_SOURCES = scan_check.c per_file.c sg_access.c sc_stats.c $(scanext)
scan_check_CPPFLAGS = -DEXTEND_HCHK=1
vdif_time_SOURCES   = vdif_time_main.c vdif_time.c
else
scan_check_SOURCES =
scan_check_CPPFLAGS =
vdif_time_SOURCES   =
endif

if WITH_FUSE
if HAVE_FUSE
if HAVE_LINUX
vdsrc = vdifuse.c vdifopt.c vdifsup.c vdiftst.c vdifseq.c vdiftrc.c vdifsig.c \
	vdiforr.c vdifsg2.c sg_access.c sg_advice.c sg_spwrda.c \
	vdiflat.c vdifrex.c
dist_vdifuse_SOURCES = $(vdsrc)
else
dist_vdifuse_SOURCES =
endif
else
dist_vdifuse_SOURCES =
endif
else
dist_vdifuse_SOURCES =
endif
vdifuse_CPPFLAGS = $(AM_CPPFLAGS) -DVDIFUSE_VERSION=$(PACKAGE_VERSION)
vdifuse_CFLAGS = $(FUSE_CFLAGS)
vdifuse_LDADD = $(FUSE_LIBS)

if HAVE_LINUX
bin_PROGRAMS = scan_check vdif_time $(vdifuse)
dist_bin_SCRIPTS = $(prepone) $(test_scripts)
else
bin_PROGRAMS =
dist_bin_SCRIPTS =
endif

CLEANFILES = 
DISTCLEANFILES = \
	depcomp install-sh test-driver missing compile config.guess config.sub

#
# eof
#
