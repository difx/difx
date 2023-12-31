#
# $Id: Makefile.am 3386 2021-10-15 13:14:34Z gbc $
#

#
# Most of this is the HOPS postprocessing tools, but there
# are some related, optional pieces that aren't an exported
# part of the package (at least not at present).
#
# CONFIGURE_DEPENDENCIES breaks distcheck unless one writes
# more dist*-hook rules to excise it from the Makefiles....
#

# turn on Mark5 utilities
if HAVE_MARK5
MARK5DIRS = @MARK5DIR@
# CONFIGURE_DEPENDENCIES = mark5/stubs/streamstor.pc
TAR_MARK5 = -mk5
DCF_MARK5 = --enable-mark5
else
MARK5DIRS =
# CONFIGURE_DEPENDENCIES =
TAR_MARK5 =
DCF_MARK5 =
endif

# turn on DiFX utilities
if HAVE_DIFX
DIFXDIRS = @DIFXDIR@
TAR_DIFX = -difx
DCF_DIFX = --enable-difx
else
DIFXDIRS =
TAR_DIFX =
DCF_DIFX =
endif

# turn on OMPI test stuff -- not part of tar or distcheck
if HAVE_OMPI
OMPIDIRS = @OMPIDIR@
else
OMPIDIRS =
endif

# turn on DOCS stuff
if HAVE_DOCS
DOCSDIRS = @DOCSDIR@
else
DOCSDIRS =
endif

# turn on code that doesn't work, yet and a few other things
if HAVE_DEVEL
DEVLDIRS = @DEVLDIR@
TAR_DEVL = -dv
DCF_DEVL = --enable-devel
else
DEVLDIRS =
TAR_DEVL =
DCF_DEVL =
endif

# this is sine qua non, except in special cases
if HAVE_HOPS
HOPSDIRS = help include scripts sub postproc correlator
HOPSDATA = data
TAR_HOPS = hops
DCF_HOPS =
else
HOPSDIRS =
HOPSDATA =
TAR_HOPS = h
DCF_HOPS = --disable-hops
endif

#
# this is for VEX2XML
if HAVE_VEX2XML
VXMLDIRS = @VXMLDIR@
TAR_VXML = -v
DCF_VXML = --enable-vex2xml
else
VXMLDIRS =
TAR_VXML =
DCF_VXML = --disable-vex2xml
endif


# this is for transitioning to a new world order
if HAVE_CHOPS
CHOPDIRS = @CHOPDIR@
TAR_CHOP = -chops
DCF_CHOP = --enable-chops
else
CHOPDIRS =
TAR_CHOP =
DCF_CHOP = --disable-chops
endif

DISTCHECK_CONFIGURE_FLAGS = \
	$(DCF_HOPS) $(DCF_DEVL) $(DCF_DIFX) $(DCF_MARK5) \
	$(DCF_CHOP) $(DCF_VXML)


# rev is -something
# vers is a, b, test, &c
TARNAME = $(TAR_HOPS)$(TAR_DEVL)$(TAR_DIFX)$(TAR_MARK5)
TARVERS = $(TARNAME)${rev}-@VERSION@${vers}
TARBALL = $(TARVERS).tar.gz
PKGVERS = $(PACKAGE)-@VERSION@
READMEV = README-@VERSION@${vers}

SUBDIRS = . $(HOPSDIRS) \
	  $(CHOPDIRS) \
	  $(DIFXDIRS) \
	  $(DEVLDIRS) \
	  $(MARK5DIRS) \
	  $(HOPSDATA) \
	  $(VXMLDIRS) \
	  $(OMPIDIRS) \
	  $(DOCSDIRS)

EXTRA_DIST = README.txt Fink.txt MacPorts.txt svnrev.txt Copyright \
	apt-packages.txt configure_ac ChangeLog.txt capture-log.py \
	pgplot-drivers.list README.pgplot.txt autogen.sh
DIST_SUBDIRS = $(SUBDIRS)

# m4/*.m4 macros scanned by aclocal
ACLOCAL_AMFLAGS = -I m4

pkginclude_HEADERS = hops_config.h

bin_SCRIPTS = hops.bash

configure_ac:	$(srcdir)/configure.ac
	sed '/^###HEAD/,/^###TAIL/c ### option disabled' \
	    $(srcdir)/configure.ac > $@

install-exec-hook:
	-@[ -d ${HOME}/bin -a ! -h ${HOME}/bin/hops.bash ] && \
	    echo ${PATH} | grep ${HOME}/bin >/dev/null && \
	    cp -p hops.bash ${HOME}/bin && \
	    echo hops.bash was copied to ${HOME}/bin || \
	    echo Put hops.bash where you can conveniently find it.

uninstall-hook:
	-@[ -d ${HOME}/bin -a ! -h ${HOME}/bin/hops.bash ] && \
	    echo ${PATH} | grep ${HOME}/bin >/dev/null && \
	    rm -f ${HOME}/bin/hops.bash || \
	    echo hops.bash was not removed as it is not in your PATH

#
# Various distribution options; recall that
# TARVERS = $(TARNAME)${rev}-@VERSION@${vers}
# E.g:   vers=Feb18 make tardist tardifx
#

tardist: dist
	@[ "$(PKGVERS)" = "$(TARVERS)" ] || \
	mv $(PKGVERS).tar.gz $(TARVERS).tar.gz && \
	echo $(PKGVERS).tar.gz renamed $(TARVERS).tar.gz

tarcheck: distcheck
	@[ "$(PKGVERS)" = "$(TARVERS)" ] || \
	mv $(PKGVERS).tar.gz $(TARVERS).tar.gz && \
	echo $(PKGVERS).tar.gz renamed $(TARVERS).tar.gz

$(TARVERS).tar.gz:
	@[ -f $(TARVERS).tar.gz ] || \
	    echo You need to make one of tardist or tarcheck && exit 1

# for revision tracking in distributions (provided we have a .svn dir)
svnrev.txt:
	-[ -d $(srcdir)/.svn ] && \
	    svn info $(srcdir) | grep Revision | cut -d' ' -f2 > svnrev.txt ||\
	    echo 0 > svnrev.txt

# for generating a Changelog (provided we have a .svn dir and an /swc dir
# which only exists at the Haystack correlator); we commit the new version
# only if it has changed.
ChangeLog.txt: svnrev.txt $(srcdir)/capture-log.py
	-[ -d $(srcdir)/.svn -a -d "/swc" ] && \
	 [ `hostname` = 'po' -a x$(USER) = xgbc ] && \
	    python $(srcdir)/capture-log.py $(srcdir) > ChangeLog.tmp && \
	    { cmp -s $(srcdir)/ChangeLog.txt ChangeLog.tmp || \
	      ( mv ChangeLog.tmp $(srcdir)/ChangeLog.txt && \
	        svn commit -m'updated for changes' $(srcdir)/ChangeLog.txt ); }

reversion:
	cd $(srcdir) && svn update
	rm -f hops_config.h stamp-h1
	./config.status --recheck
	./config.status
	$(MAKE) install

CLEANFILES = config.summary

#
# eof
#
