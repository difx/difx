#!/bin/bash
#
# script to deploy a difx-trunk installation of polconvert,
# build it using the automake machinery, and install it.
#
TLDR="Assuming success is possible and you are in the git repo dir:

    DIFXCASAPATH=/path-to-casa-bin DIFXPC=DIFXPC PP/deploy.sh build

will deploy and build PolConvert in DIFXPC.  Use 'help' for more info.
"
USAGE="Usage: $0 [check|deploy|config|all|install|build] [options]

    check   checks that automake tools exist
    deploy  specified a directory to play the role of
            applications/polconvert/trunk
            (options: source=dir, trunk=dir)
    config  configures into a build directory
            (options: trunk=dir, build=dir, prefix=dir)
    all     builds the targets in the build directory
    install installs the targets
    build   does all of the above

    casa    details on CASA startup
    help    this help
    tldr    for the impatient

The options specify where things are to be done.  Without options,
a local directory difx-pc is created taking sources from the current
directory (if .git is present) or else the directory referenced by
the source option:

    difx-pc/trunk   sources
    difx-pc/build   build area created by configure

and installation will be into difx-pc (bin, share, ...).  You may
also set DIFXPC in your environment to point to difx-pc.  Also in
your environment you must set DIFXCASAPATH to the bin directory where
the CASA executable(s) are found.

To use the tools, you'll need to adjust these paths

    PATH=...:\$DIFXPC/bin:...
    DIFXROOT=\$DIFXPC

and adjust CASA startup scripts to load the polconvert package.

Note that re-running this script will nuke the previous directories
and start clean each time.  If you want to save things change DIFXPC.
"
CASAHELP="
The following scrap of code likely works in most versions of CASA,
when placed in \$HOME/.casa/startup.py (or \$HOME/.casa/init.py):

import sys, os
loadPolConvert = True

if loadPolConvert:
  try:
    sys.path.append(os.environ['DIFXROOT'] + '/share/polconvert')
    from polconvert import polconvert
    print('polconvert py2 loaded')
  except:
    try:
        from polconvertpkg.gotasks.polconvert import polconvert
        print('polconvert py3 loaded')
    except:
        print('Could not load polconvert from this path')
        print(\"os.environ['DIFXROOT']\" + '/share/polconvert')

More generally, the code is available via:
    from polconvertpkg.private.task_polconvert import polconvert as polconvert
"

case ${1-'help'} in
tldr)   echo "$TLDR" ; exit 0 ;;
--help) echo "$USAGE" ; exit 0 ;;
help)   echo "$USAGE" ; exit 0 ;;
casa)   echo "$CASAHELP" ; exit 0 ;;
check)  action='check'  ;;
deploy) action='deploy' ;;
config) action='config' ;;
all)    action='all'    ;;
install)action='install';;
build)  action='build'  ;;
coda)   action='coda'   ;;
esac
shift 1

# paths may be specified relative to here, so we need to remember it
export STARTERPWD=`pwd`

#
# setup up arguments, ensuring that DIFXPC is an absolute path
#
[ -n "$DIFXPC" -a `expr "$DIFXPC" : '[^/].*'` -ge 1 ] &&
    DIFXPC=$STARTERPWD/DIFXPC
[ -n "$DIFXPC" -a `expr "$DIFXPC" : '/.*'` -ge 1 ] &&
    trunk=$DIFXPC/trunk && build=$DIFXPC/build && prefix=$DIFXPC
export DIFXPC

while [ $# -ge 1 ]
do
    [ `expr "$1" : '.*=.*'` -gt 3 ] && eval "$1" || {
        echo ignoring bogus argument "$1" ; exit 1 ; }
    shift
done

#
# the do it all rule, which does not return
#
[ "$action" = 'build' ] && {
    ok=false
    $0 check &&
    $0 deploy  source=$source trunk="$trunk" &&
    $0 config  trunk="$trunk" build="$build" prefix="$prefix" &&
    $0 all     build="$build" &&
    $0 install build="$build" && ok=true
    exec $0 coda ok=$ok
}

#
# pre-configure checking
#
[ "$action" = 'check' ] && {
    ok=0
    for script in aclocal autoconf autoheader automake
    do
        x=`type -p $script`
        [ -x "$x" ] && ok=$(($ok + 1)) && continue
        echo $script is missing, you need to install it
    done
    [ -z "$DIFXCASAPATH" ] && echo DIFXCASAPATH not set in environment
    [ -x "$DIFXCASAPATH/casa" ] &&
        echo CASA is $DIFXCASAPATH/casa && ok=$(($ok + 1)) ||
        echo CASA not found as $DIFXCASAPATH/casa
    [ "$ok" -eq 5 ] && echo echo tools appear to present && exit 0
    exit 2
}

#
# deploy sources using PP/difxcmp.sh (to replicate DiFX trunk layout)
#
[ "$action" = 'deploy' ] && {
    dfxcmp="$STARTERPWD/PP/difxcmp.sh"
    [ -z "$source" ] && {
        [ -d .git ] && source=`pwd` && echo using "source=$source" || {
            echo WARNING: no source is option...you cannot deploy
            exit 3 ; }
    }
    [ -d "$source" ] || { echo 'No source dir exists...' ; exit 4 ; }
    [ -z "$trunk" ] && echo 'No trunk dir specified...' && exit 5
    [ -x "$dfxcmp" ] || { echo 'PP/difxcmp.sh not found' ; exit 6 ; }
    echo "nuking/creating $trunk"
    rm -rf "$trunk"
    mkdir -p "$trunk/src" "$trunk/src/PP"

    echo DFX=/ git="$source" dfx="$trunk/src" $dfxcmp cp ...
    ( cd "$source" ;
      DFX=/ git="$source" dfx="$trunk/src" $dfxcmp cp * */* ) |\
    grep -v skipping
    echo "SUBDIRS = . src" > $trunk/Makefile.am

    [ -f "$trunk/src/PP/difxcmp.sh" ] && echo "deploy succeeded" && exit 0
    echo "action 'deploy' failed" && exit 7
}

#
# run the appropriate steps of difx-svn/setup/install-difx
#
[ "$action" = 'config' ] && {
    [ -z "$trunk" ] && echo 'No trunk dir specified...' && exit 8
    [ -z "$build" ] && echo 'No build dir specified...' && exit 9
    [ -z "$prefix" ] && echo 'No prefix dir specified...' && exit 10
    (cd $trunk && aclocal && autoconf && autoheader && automake -acf) || {
        echo auto tools failed... ; exit 11 ; }
    configscript=`cd $trunk ; eval "ls \`pwd\`/configure"`
    cd $STARTERPWD
    [ -x "$configscript" ] || { echo configure not built... ; exit 12 ; }
    [ `expr "$prefix" : '[^/].*'` -ge 1 ] && prefix=$STARTERPWD/$prefix
    rm -rf $build
    echo building in $build with $configscript for install in $prefix
    mkdir $build && ( cd $build && $configscript --prefix=$prefix )
    [ -f "$build/config.status" -a -f "$build/config.h" ] &&
        echo configure succeeded && exit 0
    echo action 'config' failed && exit 13
}

[ "$action" = 'all' ] && {
    [ -z "$build" ] && echo 'No build dir specified...' && exit 14
    cd $build && make all && echo action 'all' succeeded && exit 0
    echo "action 'all' failed" && exit 15
}

[ "$action" = 'install' ] && {
    [ -z "$build" ] && echo 'No build dir specified...' && exit 16
    cd $build && make install && echo action 'install' succeeded && exit 0
    echo "action 'install' failed" && exit 17
}

[ "$action" = 'coda' ] && {
    ${ok-'false'} && cat <<....EOF

    Installation complete; to use these tools

    export DIFXCASAPATH=$DIFXCASAPATH
    export DIFXROOT=$DIFXPC
    export PATH=$DIFXPC/bin:\$PATH

    and adjust your CASA startup to load polconvert

....EOF
    # hack for institutions that do not have py3 available
    pv=`python --version 2>&1`
    [ `expr "$pv" : 'Python 2'` -eq 8 ] && {
        echo fixing python scripts use CASA python3...
        for pp in checkpolconvertfringe.py
        do
            mv $DIFXPC/bin/$pp $DIFXPC/bin/$pp.orig
            echo "#!$DIFXCASAPATH/python3" > $DIFXPC/bin/$pp
            cat $DIFXPC/bin/$pp.orig >> $DIFXPC/bin/$pp
            chmod +x $DIFXPC/bin/$pp
            ls -l $DIFXPC/bin/$pp
        done
    }
    ${ok-'false'} && exit 0
    ${ok-'false'} || { echo something went wrong... ; exit 18 ; }
}

echo end of script should not have been reached
exit 19

#
# eof vim: nospell
#
