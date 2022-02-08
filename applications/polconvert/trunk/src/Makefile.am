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

# Run the CASA local build in the install directory
# buildmytasks is a PERL(!) script living in $DIFXCASAPATH through 5.x
# converted to Python3 for 6.x series which requires an upgrade step
# in order to share the same 5.x series xml file.
# The configure script for python2 will have set @BMTASKXML@ so that
# the second statement is stops after the initial test.
# The third statement covers some weird bug in casacore validation.
install-data-local: install-pkgdataDATA
	@echo first statement -- run buildmytasks
	-cd $(pkgdatadir) ;\
	export PATH=@DIFXCASAPATH@${PATH_SEPARATOR}${PATH} ;\
	export CASALD_LIBRARY_PATH=@DIFXCASALIBPATH@${PATH_SEPARATOR}${LD_LIBRARY_PATH} ;\
	@BUILDMYTASKS@ @BMTASKARG@ && @BUILDMYTASKS@ @BMTASKXML@
	@echo ok to ignore the mv errors--these are within buildmytasks
	@echo second statement -- cleanup after buildmytasks "(py3 only)"
	[ "@BMTASKXML@" = '&& false' ] || \
	sh -c "cd $(pkgdatadir) && [ -f polconvert.xml.bak ] && \
		mv polconvert.py polconvert.bak && \
		sed '/from..private.task/s/.private.//' polconvert.bak \
			> polconvert.py ; "
	@echo third statement
	-[ -f $(pkgdatadir)/polconvert_cli.py ] && \
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
