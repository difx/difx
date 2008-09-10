#
YACC=bison
YFLAGS=-d -y
LEX=flex
OBJECTS=vex.o vexf.o vexf_put.o vex.yy.o vex_util.o print_vex.o vex_get.o vex_put.o
CC=gcc -g
#
# F2C needs this others don't
#CFLAGS=-fPIC -DF2C
CFLAGS=-fPIC
#

default: libvex.a libvex.so

libvex.a: $(OBJECTS)
	touch $@
	rm $@
	ar -qc $@ $(OBJECTS)
	rm -f vex.c vex.yy.c

libvex.so: $(OBJECTS)
	ld -G -ztext -o $@ -share $(OBJECTS)

vex.yy.o:	vex.yy.l y.tab.h

y.tab.h:	vex.y

vex_util.o:	y.tab.h

print_vex.o:	y.tab.h

vex_get.o:	y.tab.h

vex_put.o:	y.tab.h

vexf.o:		y.tab.h

clean:
	-@rm -f *.o libvex.a *~ libvex.so

dist:
	make clean
	tar zhcvf vex.tar.gz Makefile *.c *.h *.l *.y
