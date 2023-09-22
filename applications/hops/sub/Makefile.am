#
# $Id: Makefile.am 3399 2021-10-26 20:05:16Z barrettj $
#

if HAVE_DEVEL
if HAVE_PYTHON
SUBDIRS = . msg afio dfio util vex @M4PYDIR@
else
SUBDIRS = . msg afio dfio util vex
endif
else
SUBDIRS = . msg afio dfio util vex
endif
DIST_SUBDIRS = $(SUBDIRS)

#
# eof
#
