# Darwin Makefile for RPFITS (Darwin is the unix environment that underlies
# MacOSX).

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ./code/$(ARCH).
ARCH := darwin_ppc

# GNU Fortran and C compiler flags.
FC     := g77
FFLAGS := -g -O -fno-automatic -Wall -Wno-globals
CFLAGS := -g -O -Wall -Wno-parentheses
RANLIB := ranlib

LDFLAGS :=

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
