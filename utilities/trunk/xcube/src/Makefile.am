AM_CPPFLAGS = -I$(top_srcdir)/x3c/include $(VDIFIO_CFLAGS)

bin_PROGRAMS = \
	lf1extract \
	lf1list 

lf1extract_LDADD = \
	../x3c/lib/libpacketReader.a \
	../x3c/lib/libprojectManagement.a \
	../x3c/lib/libIndexer.a \
	../x3c/lib/libx3cCommon.a

lf1extract_SOURCES = \
	lf1extract.cpp 

lf1list_LDADD = \
	../x3c/lib/libpacketReader.a \
	../x3c/lib/libprojectManagement.a \
	../x3c/lib/libIndexer.a \
	../x3c/lib/libx3cCommon.a \
	$(VDIFIO_LIBS)

lf1list_SOURCES = \
	lf1list.cpp

