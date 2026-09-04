#
# Makefile for vex module
#

AM_CPPFLAGS = -I. -I$(srcdir)/include \
	-I$(srcdir)/../../c_src/msg/include \
	-I$(srcdir)/../../c_src/mk4util/include \
	-I$(srcdir)/../../c_src/vex/include  -Wall -Wextra -DHAVE_CONFIG_H

pkginclude_HEADERS = ./include/vexpy_module_extern.h
pkglib_LTLIBRARIES = libvexpyb.la

# this one is real but imported for simplicity?
vexyorigsources = src/vexpy_module_extern.c
# these are imported via rsync
vexyhacksources = src/block_params.c \
	src/check_intrange.c src/check_realrange.c \
	src/check_scan.c src/check_stloc.c \
	src/check_strrange.c src/cvex_info.c \
	src/decode_pval.c src/do_antenna.c \
	src/do_bbc.c src/do_clock.c \
	src/do_das.c src/do_eop.c \
	src/do_exper.c src/do_freq.c \
	src/do_head_pos.c src/do_if.c \
	src/do_pass_order.c src/do_phase_cal_detect.c \
	src/do_roll.c src/do_site.c \
	src/do_source.c src/do_track.c \
	src/evex_info.c src/fill_deflists.c \
	src/fill_scanlist.c src/fill_station_parms.c \
	src/find_statements.c src/get_block_mode.c \
	src/get_chip_mode.c src/get_corr_bd_parms.c \
	src/get_corr_mode.c src/get_def.c \
	src/get_drive_init.c src/get_global_deflist.c \
	src/get_logscan.c src/get_mode_deflist.c \
	src/get_pbs_init.c src/get_pcm_config.c \
	src/get_pcm_tables.c src/get_section_mode.c \
	src/get_statement.c src/get_station_deflist.c \
	src/get_su_chan_out.c src/get_su_connect.c \
	src/get_trm_config.c src/get_val_list.c \
	src/get_version.c src/get_vex.c \
	src/in_comment.c src/in_quote.c \
	src/init_scan.c src/ivex_info.c \
	src/locate_blocks.c src/locate_cq.c \
	src/lvex_info.c src/nextchar.c \
	src/param_formats.c src/parse_date.c \
	src/parse_dec.c src/parse_pval.c \
	src/parse_ra.c src/parse_ref.c \
	src/parse_units.c src/parse_vexfile.c \
	src/print_location.c src/process_qstring.c \
	src/read_file.c src/scan_info.c \
	src/strip_text.c src/svex_info.c \
	src/vex_init.c src/write_vexfile.c

# give automake a clue it is in for trouble
BUILT_SOURCES = $(vexyorigsources) $(vexyhacksources)
$(vexyorigsources) $(vexyhacksources): importstamp

importstamp: $(srcdir)/src $(srcdir)/../../c_src/vex/src
	[ -f Makefile.am ] || {\
		[ -d src ] || mkdir src ;\
		[ -f src/vexpy_module_extern.c ] || \
			cp -p $(srcdir)/src/vexpy_module_extern.c src ;\
		for s in $(vexyhacksources);\
			do [ -f $$s ] || cp -p $(srcdir)/../../c_src/vex/$$s src ; done ; }
	touch importstamp

nodist_libvexpyb_la_SOURCES = $(vexyorigsources) $(vexyhacksources)

libvexpyb_la_LIBADD = ../../c_src/msg/libmsgb.la \
	../../c_src/mk4util/libmk4utilb.la

libvexpyb_la_LDFLAGS = -avoid-version

SRC_FILES = vexpy/__init__.py
CFG_FILES = vexpy/vexpy.py

# vexpy_module_extern.c needed here as it the 'SOURCE' copy is nodist
EXTRA_DIST = $(SRC_FILES) setup.py.in $(srcdir)/src/vexpy_module_extern.c

CLEANFILES = importstamp

DISTCLEANFILES = setup.py $(SRC_FILES) $(CFG_FILES)

clean-local:
	rm -rf ./build ./dist vexpy.egg-info
	[ -f Makefile.am ] || rm -rf src include setup.py vexpy CMakeLists.txt

#if HAVE_PYTHON
if HAVE_PYTHON_EXTRA
# note the above library is useless without python

$(CFG_FILES): $(top_builddir)/config.status
	cd $(top_builddir) && $(SHELL) ./config.status $(subdir)/$@

all-local: $(CFG_FILES) setup.py
	(mkdir -p ./build)
	(rsync -av --exclude 'Makefile.*' --exclude *.swp \
		--exclude CMake* --exclude \*.in $(srcdir)/ .)
	(chmod -R u+w .)
	($(PYTHON) setup.py build --verbose)

#the awk gibberish is necessary to extract a list of files installed by pip
install-exec-local:
	[ -d $(DESTDIR)$(pythondir) ] || mkdir -p $(DESTDIR)$(pythondir)
	$(PYTHON) -m pip install . --upgrade --target=$(DESTDIR)$(pythondir)
	PYTHONUSERBASE=@prefix@ pip show -f vexpy |\
	awk '/^Location:/ {sub(/^Location: /, "", $$0); loc=$$0} \
		 /^Files:/ {flag=1; next} \
		 flag && NF && gsub(" |\t","") {print loc "/" $$0}' |\
		 tee $(DESTDIR)$(pythondir)/install_vexpy_files.txt

uninstall-local:
	cat $(DESTDIR)$(pythondir)/install_vexpy_files.txt | xargs rm -rf
	rm -rf $(DESTDIR)$(pythondir)/install_vexpy_files.txt
	rm -rf $(DESTDIR)$(pythondir)/vexpy*
endif

#
# eof
#
