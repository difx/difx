#
# $Id: Makefile.am 577 2012-01-04 21:42:51Z gbc $
#

SUBDIRS = .

helpdir = $(pkgdatadir)/vhelp

dist_bin_SCRIPTS =  vhelp env_check vhelper.sh pplot_print \
		    hops_data_links.pl vhelp.sh
dist_help_DATA   =  vhelp1.awk vhelp2.awk

orig_scripts = vhelp.dist
older_scripts = chipid cleanmess clkm clkmlink composer count \
		  counts doa endall endmailers features fix_eps.pl \
		  llog lmess lproc lsock Makefile.am mod opera pcal \
		  rdoa runall.hay runmailers \
		  start-corrman tapit tspm tuba
other_scripts =

if HAVE_DEVEL
noinst_SCRIPTS = $(older_scripts) $(orig_scripts)
EXTRA_DIST = $(older_scripts) $(orig_scripts)
else
noinst_SCRIPTS = $(other_scripts) $(orig_scripts)
EXTRA_DIST = $(other_scripts) $(orig_scripts)
endif

env_check: $(srcdir)/Makefile.am
	rm -f $@
	echo '[ -n "$${HOPS_QUIET}" ] &&' >> $@
	echo 'echo $${1-"$@"}: Checking on your HOPS environment' >> $@
	echo '$${HOPS_SETUP-false} ||' >> $@
	echo 'echo $${1-"$@"}: You must set up your HOPS environment' >> $@
	chmod +x $@

vhelp1.awk: $(srcdir)/Makefile.am
	rm -f $@
	echo '#{sub(/.SYNOPSIS:/,"\t");print "vhelp:  " $$0}' >> $@
	echo '{split($$0,x,/:/);sub(/.doc/,"",x[1]);\
	       printf "vhelp:  %-11.11s %s\n",x[1],x[3]}'     >> $@
#	echo '{sub(/.SYNOPSIS:/,"\t");print "vhelp:  " $$0}' >> $@

vhelp2.awk: $(srcdir)/Makefile.am
	rm -f $@
	echo 'BEGIN {IGNORECASE=1;here=0; ok=0;}' >> $@
	echo 'match($$1,section)      { here=1; }' >> $@
	echo 'match($$0,/^[A-Z]/)&&ok { here=0; }' >> $@
	echo 'here == 1          {print $$0;ok++}' >> $@

vhelp vhelper.sh:	$(srcdir)/Makefile.am $(srcdir)/vhelp.dist
	-sed -e 's,$$SRC/awk_scripts/,,' $(srcdir)/vhelp.dist > vhelp
	-xx=`type -p less` && \
	    echo 'exec vhelp.sh "$$@" | '$$xx > vhelp
	-xx=`type -p less` && yy=`type -p csh` && \
	    sed -e '/template/s+|.more++' \
		-e '/more.vhelp.doc/s/more/cat/' \
		-e '/more..progname/s/more/cat/' \
		-e 's,$$SRC/awk_scripts/,,' \
		$(srcdir)/vhelp.dist \
		    > vhelper.sh && \
	    echo 'exec vhelper.sh "$$@" | '$$xx > vhelp
	touch vhelper.sh
	chmod +x vhelp vhelper.sh

CLEANFILES = vhelp vhelp1.awk vhelp2.awk env_check vhelper.sh

#
# eof
#
