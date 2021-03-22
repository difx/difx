#
# $Id$
#

EXTRA_DIST = 3593.tar.gz

dist_pkgdata_DATA = $(EXTRA_DIST)

dist_check_SCRIPTS = test_harness.sh test_mk4b.py \
	test_ffres2pcp.py test_fourphase.py test_pcc_generate.py

if HAVE_PYTHON_EXTRA

TESTS = test_harness.sh
CLEANFILES = *.pc *.pyc *.tar.gz

else

TESTS =
CLEANFILES =

endif

#
# eof
#
