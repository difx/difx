# Defines which program executables will be produced
bin_PROGRAMS = datasim

# Defines which source files are used to produce the specificed executable
datasim_SOURCES = subband.h \
                  subband.cpp \
                  util.h \
                  util.cpp \
                  vdifzipper.h \
                  vdifzipper.cpp \
                  catvdif.cpp \
                  datasim.h \
                  datasim.cpp

AM_CFLAGS = -Wall -std=c++0x

# DiFX library specific flags
# using pkg-config
datasim_CFLAGS = $(MPIFXCORR_CFLAGS)
datasim_CFLAGS += $(MARK5ACCESS_CFLAGS)
datasim_CFLAGS += $(DIFXMESSAGE_CFLAGS)
datasim_CFLAGS += $(VDIFIO_CFLAGS)

datasim_LDADD = $(MPIFXCORR_LIBS)
datasim_LDADD += $(MARK5ACCESS_LIBS)
datasim_LDADD += $(DIFXMESSAGE_LIBS)
datasim_LDADD += $(VDIFIO_LIBS)
datasim_LDADD += -lgsl -lgslcblas

