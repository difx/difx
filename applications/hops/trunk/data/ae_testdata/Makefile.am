#
# $Id: Makefile.am 1384 2016-08-02 14:26:30Z gbc $
#

SUBDIRS =

AEDATA1 = testdata/jobs-339-394-v5.fsumm \
	  testdata/jobs-339-394-v5.tsumm \
	  testdata/jobs-339-394-v6.fsumm \
	  testdata/jobs-339-394-v6.tsumm
AEDATA2 = testdata/jobs-395-422-v5.fsumm \
	  testdata/jobs-395-422-v5.tsumm \
	  testdata/jobs-395-422-v6.fsumm \
	  testdata/jobs-395-422-v6.tsumm

test_gs_device = nullpage

TESTS_ENVIRONMENT = \
	srcdir=$(srcdir) bindir=$(bindir) \
	GS_DEVICE=$(test_gs_device) PGPLOT_DIR=$(PGPLOT_DIR)

if HAVE_DEVEL
TESTS = chk_env.sh chk_fsumm.sh
expdata = $(AEDATA1) $(AEDATA2)
else
TESTS = chk_env.sh chk_fsumm.sh
expdata = $(AEDATA1)
endif

EXTRA_DIST = $(expdata)

dist_check_SCRIPTS = $(TESTS)

CLEANFILES = blk_stmt.txt pformat.txt *.?summ

clean-local:

#
# eof
#
