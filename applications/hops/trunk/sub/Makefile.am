#
# $Id: Makefile.am 1304 2016-02-12 13:43:45Z gbc $
#

if HAVE_DEVEL
if HAVE_PYTHON
SUBDIRS = . afio dfio util vex @M4PYDIR@
else
SUBDIRS = . afio dfio util vex
endif
else
SUBDIRS = . afio dfio util vex
endif
DIST_SUBDIRS = $(SUBDIRS)

#
# eof
#
