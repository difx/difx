
AM_YFLAGS = -d
AM_CPPFLAGS = -Dyyerror=vexerror -Dyylval=vexlval -Dyyin=vexin -Dyyparse=vexparse -Dyylex=vexlex

BUILT_SOURCES = \
	vex.tab.h \
	vex.tab.c \
	vex.c \
	vex.yy.c

library_includedir = $(includedir)
library_include_HEADERS = vex.h

lib_LTLIBRARIES = \
	libvex.la

vex.tab.h: vex.y
	bison -d vex.y

print_vex.o: vex.tab.h

libvex_la_SOURCES = \
	print_vex.c \
	vexf.c \
	vexf_put.c \
	vex_get.c \
	vex_put.c \
	vex_util.c \
	vex.y \
	vex.yy.l


