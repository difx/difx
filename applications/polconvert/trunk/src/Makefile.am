#
# Makefile for PolConvert within DiFX tree
#
if HAVE_IT_ALL
PolConvertSources = \
	CalTable.cpp CalTable.h \
	DataIO.cpp DataIO.h \
	DataIOFITS.cpp DataIOFITS.h \
	DataIOSWIN.cpp DataIOSWIN.h \
	Weighter.cpp Weighter.h \
	_PolConvert.cpp polconvert.xml \
	setup.py task_polconvert.py
PostPolScripts = PP/prepolconvert.py PP/runpolconvert.py PP/README.POLCONVERT
ScriptCopies = prepolconvert.py runpolconvert.py README.POLCONVERT 

# BUILT_SOURCES doesn't help here.
BuiltSources = $(PolConvertSources) $(PostPolScripts)

bin_SCRIPTS = PP/prepolconvert.py PP/drivepolconvert.py
pkgdata_DATA = _PolConvert.so \
	PP/runpolconvert.py task_polconvert.py polconvert.xml \
	PP/README.POLCONVERT

# need to get the install first, then the local data install task
install-data-am: install-pkgdataDATA install-data-local

# runs the CASA local build in the install directory
install-data-local:
	cd $(pkgdatadir) ;\
	export PATH=@DIFXCASAPATH@${PATH_SEPARATOR}${PATH} ;\
	buildmytasks

# things created in pkgdata by buildmytasks
bmt = polconvert.py polconvert_cli.py mytasks.py *.pyc
uninstall-local:
	cd $(pkgdatadir) ; rm -f $(bmt)

# no work for in-place builds, otherwise import the sources
copy.them:
	[ $(abs_builddir) = $(abs_srcdir) ] || {\
	  for s in $(PolConvertSources); do cp -p $(srcdir)/$$s . ; done ;\
	  touch copy.them ;\
	}

$(BuiltSources): copy.them

# python does an inplace build in the directory
_PolConvert.so: copy.them
	export PATH=${PATH}${PATH_SEPARATOR}@DIFXCASAPATH@ ;\
	python setup.py build_ext --inplace

# use copy.them to distinguish build and source dirs
clean-local:
	rm -rf build _PolConvert.so *.pyc
	rm -rf polconvert.py polconvert_cli.py mytasks.py
	rm -f $(ScriptCopies)
	-[ -f copy.them ] && rm -f $(PolConvertSources) && rm -f copy.them
endif
#
# eof
#
