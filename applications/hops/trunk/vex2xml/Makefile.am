#
# Makefile for VEX2XML.
#
# Since JAVA is deprecated, we rely on a helper script
# 
# both 3.5.2/1.2 and 4.8/1.4 are intended to be supportable.
#
if HAVE_VEX2XML
if HAVE_JAVAS

TESTS_ENVIRONMENT = srcdir=$(srcdir)
TESTS = chk_v2v.sh
dist_check_SCRIPTS = chk_env.sh $(TESTS)

my_pkgdatadir = $(pkglibdir)
my_pkgdata_DATA = VEX2XML.jar \
	@ANTLR4LIB@/commons-cli-@APACHECLI@.jar \
	@ANTLR4LIB@/antlr-@ANTLR4VER@-complete.jar
dist_bin_SCRIPTS = VEX2XML

vexdocs=doc/Antlr-LICENSE-ver3.txt    \
        doc/Antlr-LICENSE-ver4.txt    \
        doc/Apache-LICENSE-ver2.0.txt \
        doc/Apache-cli-1.4-LICENSE.txt\
        doc/Apache-cli-1.4-NOTICE.txt \
        doc/Apache-cli-1.4-README.md  \
        doc/Release-Notes.txt         \
        doc/VEX-visualGrammar.pdf
nobase_dist_pkgdata_DATA = $(vexdocs)

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
	src/vexGrammar.g              \
	src/manifest.txt              \
	src/VEX2XML.java              \
	lib/commons-cli-1.2.jar       \
	lib/antlr-3.5.2-complete.jar  \
	src4/vexGrammar.g             \
	src4/manifest.txt             \
	src4/VEX2XML.java             \
	lib4/antlr-4.8-complete.jar   \
	lib4/commons-cli-1.4.jar      \
	$(vtdata) $(vexdocs)

CLEANFILES = *.g *.tokens *.java *.class *.jar *.txt *.vex *.xml

clean-local:
	rm -rf lib lib4
	rm -rf jcl

# only 3.5.2 works, but we'll try to get 4.8 to work and the
# makefile work is the same for both, I believe
antlr=$(srcdir)/@ANTLR4LIB@/antlr-@ANTLR4VER@-complete.jar
cmmns=$(srcdir)/@ANTLR4LIB@/commons-cli-@APACHECLI@.jar
gramr=$(srcdir)/@ANTLR4SRC@/vexGrammar.g
manif=$(srcdir)/@ANTLR4SRC@/manifest.txt

vexGrammar.g:
	cp -p $(gramr) .

manifest.txt:
	sed s+../@ANTLR4LIB@/++g $(manif) > manifest.txt

VEX2XML.jar: $(antlr) $(srcdir)/src/VEX2XML.java vexGrammar.g manifest.txt
	java -jar $(antlr) vexGrammar.g
	javac $(srcdir)/src/VEX2XML.java -cp ".:$(srcdir)/@ANTLR4LIB@/*" -d .
	cp -p $(antlr) $(cmmns) .
	jar -cvfm ./VEX2XML.jar manifest.txt *.class > jarring.txt 2>&1
	@echo being tidy...
	rm -f vexGrammarParser*
	rm -f vexGrammarLexer.*

endif
endif
#
# eof
#
