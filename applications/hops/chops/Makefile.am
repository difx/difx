ACLOCAL_AMFLAGS = -I m4

if HAVE_PYTHON
TESTS = run_checksum.sh importcheck.py
else
TESTS = run_checksum.sh
endif

SUBDIRS = source

dist_check_SCRIPTS = run_checksum.sh hops_checksum.sh importcheck.py

