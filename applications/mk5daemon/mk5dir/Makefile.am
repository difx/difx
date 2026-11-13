
bin_PROGRAMS =

if WITH_MARK6
bin_PROGRAMS += \
	mk6dir
endif

dist_bin_SCRIPTS = \
	checkdir \
        mk6state

mk6dir_LDADD = \
	${DIFXMESSAGE_LIBS} ${VDIFIO_LIBS}

mk6dir_SOURCES = \
	mk6dir.cpp
