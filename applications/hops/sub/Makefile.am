#
# $Id: Makefile.am 4512 2026-05-10 21:48:06Z gbc $
#

if HAVE_PYTHON
SUBDIRS = . msg afio dfio util vex mk4py
TESTS = importcheck.py
else
SUBDIRS = . msg afio dfio util vex
TESTS =
endif

DIST_SUBDIRS = $(SUBDIRS)
check_SCRIPTS = importcheck.py
dist_check_SCRIPTS = importcheck.py

#
# eof
#
