#
# Makefile for VEX2XML.
#
# Since JAVA is deprecated, we rely on a helper script
#
if HAVE_VEX2XML
if HAVE_JAVAS
dist_bin_SCRIPTS = VEX2XML
my_pkgdatadir = $(pkglibdir)
my_pkgdata_DATA = VEX2XML.jar \
	lib/commons-cli-1.2.jar \
	lib/antlr-3.5.2-complete.jar

TESTS_ENVIRONMENT = srcdir=$(srcdir)
TESTS = chk_v2v.sh
dist_check_SCRIPTS = chk_env.sh $(TESTS)

vtdata= testcases/vex/a25gl.vex \
	testcases/vex/b23gl.vex \
	testcases/vex/bm452c.vex \
	testcases/vex/c151a.vex \
	testcases/vex/c151b.vex \
	testcases/vex/c152a.vex \
	testcases/vex/c152b.vex \
	testcases/vex/c22gl.vex \
	testcases/vex/d21us.vex \
	testcases/vex/e16b08.vex \
	testcases/vex/g26us.vex \
	testcases/vex/gbc1.vex \
	testcases/vex/new27.vex \
	testcases/xml/a25gl.xml \
	testcases/xml/b23gl.xml \
	testcases/xml/bm452c.xml \
	testcases/xml/c151a.xml \
	testcases/xml/c151b.xml \
	testcases/xml/c152a.xml \
	testcases/xml/c152b.xml \
	testcases/xml/c22gl.xml \
	testcases/xml/d21us.xml \
	testcases/xml/e16b08.xml \
	testcases/xml/g26us.xml \
	testcases/xml/gbc1.xml \
	testcases/xml/new27.xml

EXTRA_DIST = VEX2XML                  \
	doc/VEX-visualGrammar.pdf     \
	doc/Apache-LICENSE-ver2.0.txt \
	doc/Release-Notes.txt         \
	doc/Antlr-LICENSE-ver3.txt    \
	src/vexGrammar.g              \
	src/manifest.txt              \
	src/VEX2XML.java              \
	lib/commons-cli-1.2.jar       \
	lib/antlr-3.5.2-complete.jar  \
	$(vtdata)

dist_pkgdata_DATA = $(EXTRA_DIST)

CLEANFILES = *.g *.tokens *.java *.class *.jar *.txt *.vex *.xml

clean-local:
	rm -rf lib
	rm -rf jcl

antlr=$(srcdir)/lib/antlr-3.5.2-complete.jar
cmmns=$(srcdir)/lib/commons-cli-1.2.jar
gramr=$(srcdir)/src/vexGrammar.g
manif=$(srcdir)/src/manifest.txt

vexGrammar.g:
	cp -p $(gramr) .

manifest.txt:
	sed s+../lib/++g $(manif) > manifest.txt

VEX2XML.jar: $(antlr) $(srcdir)/src/VEX2XML.java vexGrammar.g manifest.txt
	java -jar $(antlr) vexGrammar.g
	javac $(srcdir)/src/VEX2XML.java -cp ".:$(srcdir)/lib/*" -d .
	cp -p $(antlr) $(cmmns) .
	jar -cvfm ./VEX2XML.jar manifest.txt *.class > jarring.txt 2>&1

endif
endif
#
# eof
#
