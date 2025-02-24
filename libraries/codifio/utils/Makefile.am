AM_CPPFLAGS  = \
	-I$(top_srcdir)/src -D_FILE_OFFSET_BITS=64

LDADD = \
	$(top_builddir)/src/libcodifio.la

bin_PROGRAMS = \
	codifsum codif_write codifSplitChan fixcodif codif_dumpheaders codif_dsb2ssb

codifsum_SOURCES = \
	codifsum.c

codif_write_SOURCES = \
	codif_write.c

codifSplitChan_SOURCES = \
	codifSplitChan.c

fixcodif_SOURCES = \
	fixcodif.c

codif_dumpheaders_SOURCES = \
	codif_dumpheaders.c

codif_dsb2ssb_SOURCES = \
	codif_dsb2ssb.c

