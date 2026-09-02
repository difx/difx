#!/bin/bash
# configure will re-generate these:
rm -rf ompi/autom4te.cache ompi/configure ompi/aclocal.m4 ompi/Makefile.in
rm -rf difx2mark4/autom4te.cache difx2mark4/configure difx2mark4/aclocal.m4
[ $# -gt 0 ] && {
    find . -name Makefile.in -delete
    rm -f aclocal.m4 compile config.guess config.sub configure depcomp \
          install-sh ltmain.sh m4/libtool.m4 m4/ltoptions.m4 m4/ltsugar.m4 \
          m4/ltargz.m4 m4/ltdl.m4 m4/pkg.m4 \
          m4/ltversion.m4 m4/lt~obsolete.m4 missing py-compile test-driver \
          hops_config.h.in difx2mark4/config.h.in *~ */*~ autoscan.log
    rm -rf autom4te.cache libltdl
    echo
    echo autotrash purged
    echo
    exit 0
}
libtoolize -cf
aclocal --install -I m4
autoconf
autoheader
automake -acf
echo
echo autoconfiguration complete
echo
# eof
