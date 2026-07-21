# Makefile for chops python sources
DIST_SUBDIRS = mk4_module hopstest_module ffcontrol_module \
	  vpal_module vex_module scripts tests
if HAVE_PYTHON
SUBDIRS = $(DIST_SUBDIRS)
else
SUBDIRS =
endif

# this file is common to all of the above
uninstall-hook:
	rm -f $(pythondir)/easy-install.pth

# eof
