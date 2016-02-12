#
# $Id: Makefile.am 1288 2016-01-14 13:42:47Z gbc $
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
