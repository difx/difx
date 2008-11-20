# Darwin Intel x86 Makefile for RPFITS (cancer, MacOSX 8.9.1).

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ../code/$(RPARCH)
# (../code/darwin_x86 is currently symlink'd to ../code/linux).
RPARCH := darwin_x86

# GNU Fortran 77 and C compiler flags.  The definitions here are for the
# g77 3.4 compiler available from hpc.sourceforge.net.  Note that
# /usr/bin/gcc-3.3 supplied in XCode is incomplete and unusable.
FC      := /usr/local/bin/g77-3.4
FFLAGS  := -g -O -fno-automatic -Wall -Wimplicit -Wno-globals
CC      := /usr/bin/gcc-4.0
CFLAGS  := -g -O -Wall
RANLIB  := /usr/bin/ranlib

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
