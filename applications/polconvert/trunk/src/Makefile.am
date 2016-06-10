#
# Makefile for PolConvert within DiFX tree
#
if HAVE_IT_ALL
bin_SCRIPTS = prepolconvert.py runpolconvert.py

polconvertdir = $(pkgdatadir)/polconvert

polconvert_DATA = _PolConvert.so task_polconvert.py polconvert.xml

install-local:
	cd $(polconvertdir)
	buildmytasks

_PolConvert.so:
	python setup.py build_ext --inplace
	buildmytasks

clean-local:
	rm -rf build _PolConvert.so *.pyc
	rm -rf polconvert.py polconvert_cli.py mytasks.py
endif
#
# eof
#
