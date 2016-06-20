#
# Makefile for PolConvert within DiFX tree
#
if HAVE_IT_ALL
bin_SCRIPTS = PP/prepolconvert.py

pkgdata_DATA = _PolConvert.so \
	runpolconvert.py task_polconvert.py polconvert.xml

install-data-am: install-pkgdataDATA install-data-local

install-data-local:
	cd $(pkgdatadir) ;\
	export PATH=@DIFXCASAPATH@${PATH_SEPARATOR}${PATH} ;\
	buildmytasks

bmt = polconvert.py polconvert_cli.py mytasks.py *.pyc

uninstall-local:
	cd $(pkgdatadir) ; rm -f $(bmt)

# use INSTALL to distinguish build and source dirs
copy.them:
	[ -f ./INSTALL ] || cp -p $(srcdir)/*.* .
	touch copy.them

runpolconvert.py task_polconvert.py polconvert.xml: copy.them

_PolConvert.so: copy.them
	export PATH=${PATH}${PATH_SEPARATOR}@DIFXCASAPATH@ ;\
	python setup.py build_ext --inplace && buildmytasks

# use INSTALL to distinguish build and source dirs
clean-local:
	rm -rf build _PolConvert.so *.pyc
	rm -rf polconvert.py polconvert_cli.py mytasks.py
	[ -f ./INSTALL ] || rm -f *.*
endif
#
# eof
#
