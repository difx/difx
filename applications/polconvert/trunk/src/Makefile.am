#
# Makefile for PolConvert within DiFX tree
#
.NOTPARALLEL:

if HAVE_IT_ALL
PolConvertSources = \
	CalTable.cpp CalTable.h \
	DataIO.cpp DataIO.h \
	DataIOFITS.cpp DataIOFITS.h \
	DataIOSWIN.cpp DataIOSWIN.h \
	Weighter.cpp Weighter.h \
	_PolConvert.cpp _getAntInfo.cpp _PolGainSolve.cpp _XPCal.cpp \
	polconvert.xml setup.py task_polconvert.py
PostPolScripts = PP/runpolconvert.py PP/README.POLCONVERT \
	PP/Estimate_DPFU.py PP/DPFU_scanner.py
ScriptCopies = runpolconvert.py README.POLCONVERT \
	Estimate_DPFU.py DPFU_scanner.py

# BUILT_SOURCES doesn't help here.
BuiltSources = $(PolConvertSources) $(PostPolScripts)

bin_SCRIPTS = PP/prepolconvert.py PP/drivepolconvert.py \
	PP/checkpolconvert.py PP/comparepolconvert.py \
	PP/singlepolsolve.py
pkgdata_DATA = _PolConvert.so _getAntInfo.so _PolGainSolve.so _XPCal.so \
	PP/runpolconvert.py task_polconvert.py polconvert.xml \
	PP/README.POLCONVERT PP/Estimate_DPFU.py PP/DPFU_scanner.py \
	drivepclib.py solvepclib.py

# need to get the install first, then the local data install task
install-data-am: install-pkgdataDATA install-data-local

#
# One of casapy2 or casapy3 will be /bin/false based
# on seaching for python or python3 in $DIFXCASAPATH
#
# @BUILDMYTASKS@ &c are provided by configure to handle the two
# cases.  The python2 method works through 5.8.  The python3 method
# was broken in 6.1 and possibly fixed in 6.2.  Tested in 6.4.
#
# The configure script for python2 will have set @BMTASKXML@ so that
# the chain of commands stops after one invocation.
#
# The standalone rewrite is not yet supported here.
#
if HAVE_CASA6
# the command invokes a script that uses buildmytasks in ...share/polconvert
bpt=$(srcdir)/PP/buildpy3tasks.sh
else
# the command below reduces to executing buildmytasks in ...share/polconvert
bpt=
endif
install-data-local: install-pkgdataDATA $(bpt)
	@echo first statement -- run buildmytasks
	-cd $(pkgdatadir) ;\
	export PATH=@DIFXCASAPATH@${PATH_SEPARATOR}${PATH} ;\
	cslp=@DIFXCASALIBPATH@${PATH_SEPARATOR}${LD_LIBRARY_PATH} ;\
	export CASALD_LIBRARY_PATH=$$cslp ;\
	$(bpt) @BUILDMYTASKS@ @BMTASKXML@ @BMTASKMOD@
	-[ -f $(pkgdatadir)/polconvert_cli.py ] && \
	echo cleaning up some weird casacore validation issue && \
	sh -c "cd $(pkgdatadir) ; mv polconvert_cli.py polconvert_cli.py.bak ;\
		sed '/casac.casac.utils...verify/s/casac/#casac/' \
		polconvert_cli.py.bak > polconvert_cli.py"

#
# massive Kluge here to share code between scripts
#
drivepclib.py: $(srcdir)/PP/drivepolconvert.py
	sed -e '/if __name__ == .__main__./,/eof/d' \
	    -e '/import solvepclib/s/^\#.*import/import/' \
	    -e '/def createCasaInput(/s/create/xxx_create/' \
	    -e '/if createCasaInput/s/create/spc.create/' \
	    -e '/enter.here/s/^.*/\# eof/' $< > $@

#sed -e '/if __name__ == .__main__./,/eof/d'
solvepclib.py: $(srcdir)/PP/singlepolsolve.py
	sed -e '/if __name__ == .__main__./,9999d' \
	    -e '/enter.here/s/^.*/\# eof/' $< > $@

# brutal, but hey..
uninstall-hook:
	rm -rf $(pkgdatadir)

# things created in pkgdata by buildmytasks
bmt = polconvert.py polconvert_cli.py mytasks.py *.pyc
uninstall-local:
	cd $(pkgdatadir) ; rm -f $(bmt)

check-local:
	-ls -l $(bindir)/*polconvert* $(pkgdatadir)

# no work for in-place builds, otherwise import the sources
copy.them:
	[ $(abs_builddir) = $(abs_srcdir) ] || {\
	  for s in $(PolConvertSources); do cp -p $(srcdir)/$$s . ; done ;\
	  touch copy.them ;\
	}

$(BuiltSources): copy.them

# python does an inplace build in the directory
# note that all .so files are build with this
build.sos: copy.them
	export PATH=@DIFXCASAPATH@${PATH_SEPARATOR}${PATH} ;\
	export LD_LIBRARY_PATH=@DIFXCASALIBPATH@${PATH_SEPARATOR}${LD_LIBRARY_PATH} ;\
	[ -n "${DIFXCASAVENV}" ] && source ${DIFXCASAVENV} ;\
	@CASAPYTHON@ setup.py build_ext --inplace
	ls _*.so > build.sos

# Py3 generates complex .so names that we perhaps do not need to follow.
_PolConvert.so: build.sos
	[ -f $@ ] || { pcso=`grep _PolConvert $<`   ; cp -p $$pcso $@ ; }
_XPCal.so: build.sos
	[ -f $@ ] || { pcso=`grep _XPCal $<`        ; cp -p $$pcso $@ ; }
_PolGainSolve.so: build.sos
	[ -f $@ ] || { pcso=`grep _PolGainSolve $<` ; cp -p $$pcso $@ ; }
_getAntInfo.so: build.sos
	[ -f $@ ] || { pcso=`grep _getAntInfo $<`   ; cp -p $$pcso $@ ; }

# use copy.them to distinguish build and source dirs
clean-local:
	rm -rf build *.so *.pyc
	rm -rf polconvert.py polconvert_cli.py mytasks.py
	rm -f $(ScriptCopies)
	-[ -f copy.them ] && rm -f $(PolConvertSources) && rm -f copy.them
	rm -f build.sos drivepclib.py solvepclib.py
endif
#
# eof
#
