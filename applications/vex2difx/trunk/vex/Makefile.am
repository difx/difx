
AM_YFLAGS = -d -y
AM_CPPFLAGS = -Dyyerror=vexerror -Dyylval=vexlval -Dyyin=vexin -Dyyparse=vexparse -Dyylex=vexlex

BUILT_SOURCES = \
	vex_parse.c 

library_includedir = $(includedir)
library_include_HEADERS = \
	vex_parse.h \
	vex.h 

lib_LTLIBRARIES = \
	libvex.la

vex_parse.tab.h: vex_parse.y
	bison -d vex_parse.y

print_vex.o: vex_parse.tab.h

libvex_la_SOURCES = \
	print_vex.c \
	vexf.c \
	vexf_put.c \
	vex_get.c \
	vex_put.c \
	vex_util.c \
	vex_parse.y \
	vex.yy.l

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = vex.pc

