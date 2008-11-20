# Darwin Intel x86 Makefile for RPFITS.

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ../code/$(ARCH)
# (../code/darwin_x86 is currently symlink'd to ../code/linux).
ARCH := darwin_x86

# GNU Fortran 95 and C compiler flags.  As of 2006/10/24 the port of GNU/linux
# utilities on the Intel Macs is in an unstable development phase and thus it
# is important to use a consistent set of compilers.  The definitions here
# assume the Fink distribution of gcc/gfortran 4.2.0.  This is not necessarily
# the same as other distributions that claim to be 4.2.0, mixing them is
# likely to result in the linker reporting symbols that are defined in one
# version of libgfortran but not another.  Note that the -a option is used on
# ranlib to satisfy the requirements of the "odcctools" linker
# (/sw/lib/odcctools/bin/ld) which, if installed, is used by default by Fink
# gfortran.
FC      := /sw/bin/gfortran
FFLAGS  := -g -O -fno-automatic -Wall
CC      := /sw/bin/gcc-4
CFLAGS  := -g -O -Wall -Wno-parentheses
RANLIB  := /usr/bin/ranlib -a

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
