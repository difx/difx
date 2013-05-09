bin_PROGRAMS = difx2mark4
check_PROGRAMS = update_station_chk

TESTS = update_station_chk
TESTS_ENVIRONMENT = srcdir=$(srcdir) HOPS_STATION_CODE=debug.envir

difx2mark4_SOURCES = \
	difx2mark4.c \
	createRoot.c \
	createType1s.c \
    new_type1.c \
	createType3s.c \
	root_id.c \
	single_code.c \
	update_stations.c \
	conv2date.c \
	get_vis.c \
	swabr.c \
	write_t100.c \
	write_t101.c \
	write_t120.c \
	write_t300.c \
	write_t301.c \
	write_t302.c \
	write_t303.c \
	write_t309.c \
	other.c \
    normalize.c \
    fill_fblock.c \
	mk4_typedefs.h \
	type_000.h \
	type_100.h \
	type_101.h \
	type_120.h \
	type_300.h \
	type_301.h \
	type_302.h \
	type_303.h \
	type_309.h \
	difx2mark4.h \
	other.h

update_station_chk_SOURCES = update_stations.c single_code.c
update_station_chk_CPPFLAGS = -DSTANDALONE_CHECK=1 -DAWK=\"@AWK@\"

CLEANFILES = *debug*
