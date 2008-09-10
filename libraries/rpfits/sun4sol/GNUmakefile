# Sun Solaris Makefile for RPFITS.

# The RPFITS root directory.
RPFITSROOT := ..

# Only used for locating architecture-specific code in ./code/$(ARCH).
ARCH := sun4sol

# Sun Fortran and C compiler flags...
FFLAGS := -O -u -xs
CFLAGS := -O -xs

# ...or if you have the GNU compilers installed and want to use them.
# FC     := g77
# FFLAGS := -g -O -fno-automatic -Wall -Wno-globals
# CC     := gcc
# CFLAGS := -g -O -Wall -Wno-parentheses

RANLIB := ranlib

LDFLAGS :=

# Where to install stuff.
PREFIX := /usr/local

#-----------------------------------------------------------------------------
# Include generic RPFITS makefile.
-include $(RPFITSROOT)/GNUmakefile
