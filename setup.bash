####### DIFX VERSION ########################
export DIFX_VERSION=DEV

####### ROOT PATHS ##########################
export DIFXROOT=/usr/local/difx
export DIFX_PREFIX=$DIFXROOT
export PGPLOTDIR=
export IPPROOT=/opt/intel

####### COMPILER ############################
export DIFXMPIDIR=/usr
export MPICXX="${DIFXMPIDIR}"/bin/mpicxx

####### LIBRARY PATHS #######################
####### Uncomment and modify if needed, #####
####### such as 64-bit OpenSuSE #############
# export IPP_LIBRARY_PATH="${IPPROOT}"/ipp/lib/intel64:"${IPPROOT}"/compiler/lib/intel64
# export MPI_LIBRARY_PATH="${DIFXMPIDIR}"/lib64

####### USE GFORTRAN IN PREFERENCE TO G77? ##
####### Comment out if not desired ##########
export USEGFORTRAN="yes"
# you may need this for calcserver,difxcalc11
# export FFLAGS=-fallow-argument-mismatch

####### PERL VERSION/SUBVERSION #############
perlver="5"
perlsver="5.10.1"

####### PORTS FOR DIFXMESSAGE ###############
# Uncomment these to enable DIFX_MESSAGES
export DIFX_MESSAGE_GROUP=224.2.2.1
export DIFX_MESSAGE_PORT=50201
export DIFX_BINARY_GROUP=224.2.2.1
export DIFX_BINARY_PORT=50202

####### CALC SERVER NAME ######### 
export CALC_SERVER=localhost

####### HOPS ENVIRONMENT #########
# uncomment/modify these lines if you have enabled HOPS
# or alternatively be sure to source $DIFXROOT/bin/hops.bash
# export GS_DEVICE=x11
# export PGPLOT_TYPE=/xw
# export DEF_CONTROL=/dev/null
# export DATADIR=/tmp
# export HOPS_DOCS=${DIFXROOT}/share/hops
# export PROGDOC=${HOPS_DOCS}/vhelp
# export AHELP=${HOPS_DOCS}/vhelp/aedit
# export TEXT=${HOPS_DOCS}/text

####### MPI RUNTIME OPTIONS #################
####### Uncomment and modify if needed, #####
####### such as Open MPI 1.8.4 ##############
# export DIFX_MPIRUNOPTIONS="--mca mpi_yield_when_idle 1 --mca rmaps seq"

####### CUDA (GPU builds only) ##############
# export CUDAROOT=/usr/local/cuda
# export NVCCFLAGS="-arch=sm_75"

####### SITE-SPECIFIC SETTINGS ##############
# Rather than editing the values above, put your own settings (DIFXROOT,
# DIFXMPIDIR, IPPROOT, CUDAROOT, ...) in setup.local.bash beside this file, or
# in the file named by $DIFX_SETUP_LOCAL.  It is read here, so it overrides
# anything above.  setup.local.bash is ignored by git, so local paths never end
# up in a commit; see setup.local.bash.example.  DIFX_PREFIX and MPICXX follow
# DIFXROOT and DIFXMPIDIR unless the local file sets them itself.
_difxlocal="${DIFX_SETUP_LOCAL:-$(dirname "${BASH_SOURCE[0]}")/setup.local.bash}"
if [ -f "$_difxlocal" ]; then
  unset DIFX_PREFIX MPICXX
  . "$_difxlocal"
  export DIFX_PREFIX="${DIFX_PREFIX:-$DIFXROOT}"
  export MPICXX="${MPICXX:-${DIFXMPIDIR}/bin/mpicxx}"
elif [ -n "$DIFX_SETUP_LOCAL" ]; then
  echo "Warning: DIFX_SETUP_LOCAL=$DIFX_SETUP_LOCAL does not exist; using defaults"
fi
unset _difxlocal

####### No User configurable values below here

####### Operating System, use $OSTYPE
if [ ${OSTYPE:0:6} = "darwin" ]
then
 OS=darwin
elif [ "${OSTYPE:0:5}" = "linux" ] 
then
  OS=linux
else
  echo "Warning unsupported O/S $OSTYPE"
  return
fi

#################################################################
# DiFXInstallRoot DIR: if DIR is the bin, lib, lib64, pkgconfig,
# python or perl directory of a DiFX installation, print the
# installation's root.  An installation is recognised by its
# contents (bin/mpifxcorr or a difxio.pc), not by its name.
#################################################################
DiFXInstallRoot()
{
local d="$1" r
while [ "${d%/}" != "$d" ]; do d="${d%/}"; done
case "$d" in
  */perl/*)                        r="${d%%/perl/*}" ;;
  */lib/perl*|*/lib64/perl*)       r="${d%/lib*/perl*}" ;;
  */lib/python*|*/lib64/python*)   r="${d%/lib*/python*}" ;;
  */lib/pkgconfig|*/lib64/pkgconfig) r="${d%/lib*/pkgconfig}" ;;
  */bin|*/lib|*/lib64)             r="${d%/*}" ;;
  *) return 0 ;;
esac
while [ "${r%/}" != "$r" ]; do r="${r%/}"; done
if [ -x "$r/bin/mpifxcorr" ] || [ -f "$r/lib/pkgconfig/difxio.pc" ] || [ -f "$r/lib64/pkgconfig/difxio.pc" ]
then
  echo "$r"
fi
}

#################################################################
# PurgePath VAR: remove every DiFX installation from the colon-
# separated list in VAR, so that headers, libraries and programs
# of another installation (an older one, or a branch's) are not
# picked up after switching.  An entry is removed if it lies in an
# installation that DiFXInstallRoot recognises, or if its name has
# "difx" as a word (the older rule, kept for installations that
# have not been built yet).  Installations in a shared prefix (/usr,
# /usr/local, /opt) are not removed, because that would remove
# unrelated software too; a warning is printed instead.  What was
# removed, apart from DIFX_SELECTED_ROOT, is collected in
# DIFX_PURGED.
#################################################################
PurgePath()
{
local Path="$1" CurPath NewPath="" Sep="" Item Root Gone Items
local NameRule='(^|[^[:alnum:]_])[Dd][Ii][Ff][Xx]([^[:alnum:]_]|$)'
eval CurPath=\${$Path:-}
[ -n "$CurPath" ] || return 0
IFS=: read -r -a Items <<< "$CurPath"
for Item in "${Items[@]}"
do
  Gone=""
  Root=$(DiFXInstallRoot "$Item")
  case "$Root" in
    /usr|/usr/local|/opt)
      echo "Warning: $Path contains $Item, which belongs to a DiFX installation in the shared prefix $Root; it is left in place" >&2 ;;
    ?*) Gone="$Root" ;;
    *) if [[ $Item =~ $NameRule ]]; then Gone="$Item"; fi ;;
  esac
  if [ -z "$Gone" ]
  then
    NewPath="$NewPath$Sep$Item"
    Sep=":"
  else
    case ":$DIFX_PURGED:" in
      *":$Gone:"*) ;;
      *) case "$Gone" in
           "$DIFX_SELECTED_ROOT"|"$DIFX_SELECTED_ROOT"/*) ;;
           *) DIFX_PURGED="${DIFX_PURGED:+$DIFX_PURGED:}$Gone" ;;
         esac ;;
    esac
  fi
done
eval export $Path=\"\$NewPath\"
}

PrependPath()
{
Path="$1"
NewItem="$2"
eval CurPath=\$"$Path"

#################################################################
# Add the item.  If the path is currently empty, just set it to
# the new item, otherwise, prepend the new item and colon
# separator.
#################################################################
if [ -n "$CurPath" ]
then
    #################################################################
    # Check to see if the item is already in the list
    #################################################################
    if [ `expr "$CurPath" ':' ".*$NewItem\$"` -eq '0'  -a \
         `expr "$CurPath" ':' ".*$NewItem\:.*"` -eq '0' ]
    then
        eval $Path=\"$NewItem\:$CurPath\"
    fi
else
    eval export $Path="$NewItem"
fi
}

####### PURGE EXISTING DIFX FROM PATHS ######
DIFX_PURGED=""
DIFX_SELECTED_ROOT="$DIFXROOT"
while [ "${DIFX_SELECTED_ROOT%/}" != "$DIFX_SELECTED_ROOT" ]; do DIFX_SELECTED_ROOT="${DIFX_SELECTED_ROOT%/}"; done
PurgePath PERL5LIB
PurgePath PATH
PurgePath LD_LIBRARY_PATH
PurgePath DYLD_LIBRARY_PATH
PurgePath PYTHONPATH
PurgePath PKG_CONFIG_PATH
if test "${PS1:-}" != "" && test -n "$DIFX_PURGED"; then
  echo " Removed from paths (other DiFX installations): ${DIFX_PURGED//:/ }"
fi
unset DIFX_PURGED DIFX_SELECTED_ROOT

####### 32/64 BIT DEPENDENT MODIFICATIONS ###
arch=(`uname -m`)
if [ "$arch" = "i386" -o "$arch" = "i686" ] #32 bit
then
  export DIFXBITS=32
  PrependPath PERL5LIB         "${DIFXROOT}/perl/lib/perl$perlver/site_perl/$perlsver"
elif [ "$arch" = "x86_64" ] #64 bit
then
  export DIFXBITS=64
  PrependPath PERL5LIB         "${DIFXROOT}/perl/lib64/perl$perlver/site_perl/$perlsver/x86_64-linux-thread-multi"
elif [ "$arch" = "arm64" ] #64 bit
then
  export DIFXBITS=64
  PrependPath PERL5LIB     "${DIFXROOT}/lib/perl$perlver/site_perl/$perlsver/darwin-thread-multi-2level:${DIFXROOT}/lib/perl$perlver/site_perl"
else
  echo "Unknown architecture $arch - leaving paths unaltered"
fi

####### LIBRARY/EXECUTABLE PATHS ############
PrependPath PATH             "${DIFXMPIDIR}"/bin
PrependPath PATH             "${DIFXROOT}"/bin
if [ -n "$CUDAROOT" ] && [ -d "$CUDAROOT" ]; then
  PrependPath PATH            "${CUDAROOT}"/bin
  PrependPath LD_LIBRARY_PATH "${CUDAROOT}"/lib64
fi
if [ -n "${IPP_LIBRARY_PATH}" ]; then
    PrependPath LD_LIBRARY_PATH "${IPP_LIBRARY_PATH}"
fi
if [ -n "${MPI_LIBRARY_PATH}" ]; then
    PrependPath LD_LIBRARY_PATH "${MPI_LIBRARY_PATH}"
fi
if [ "$OS" = "darwin" ] 
then
  PrependPath DYLD_LIBRARY_PATH  "${DIFXROOT}/lib"
  PrependPath DYLD_LIBRARY_PATH  "${PGPLOTDIR}"
else
  PrependPath LD_LIBRARY_PATH  "${DIFXROOT}/lib"
  PrependPath LD_LIBRARY_PATH  "${PGPLOTDIR}"
  if [ "$arch" = "x86_64" ] #64 bit
  then
    PrependPath LD_LIBRARY_PATH  "${DIFXROOT}/lib64"
  fi  
fi
PrependPath PKG_CONFIG_PATH  "${DIFXROOT}/lib/pkgconfig"
PrependPath PYTHONPATH  "${DIFXROOT}/lib/python"
if [ "$arch" = "x86_64" ] #64 bit
then
  PrependPath PKG_CONFIG_PATH  "${DIFXROOT}/lib64/pkgconfig"
  PrependPath PYTHONPATH  "${DIFXROOT}/lib64/python"
fi  
if test "$PS1" != ""; then
  echo " DiFX version $DIFX_VERSION is selected"
  export PS1="\u@\h $DIFX_VERSION \W> "
fi
