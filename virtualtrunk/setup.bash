####### DIFX VERSION ########################
export DIFX_VERSION=trunk

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

PurgePath()
{
Path="$1"
eval CurPath=\$"$Path"
if [ -x /bin/awk ] && [ -x /bin/sed ];
then
  eval export $Path=$(echo ${CurPath} | /bin/awk -v RS=: -v ORS=: '/\<DiFX\>/ {next} {print}' IGNORECASE=1 | /bin/sed 's/:*$//')
fi
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
        eval $Path="$NewItem"\:"$CurPath"
    fi
else
    eval export $Path="$NewItem"
fi
}

####### PURGE EXISTING DIFX FROM PATHS ######
PurgePath PERL5LIB
PurgePath PATH
PurgePath LD_LIBRARY_PATH
PurgePath DYLD_LIBRARY_PATH
PurgePath PYTHONPATH
PurgePath PKG_CONFIG_PATH

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
else
  echo "Unknown architecture $arch - leaving paths unaltered"
fi

####### LIBRARY/EXECUTABLE PATHS ############
PrependPath PATH             "${DIFXMPIDIR}"/bin
PrependPath PATH             "${DIFXROOT}"/bin
if [ -z "${IPP_LIBRARY_PATH}" ]; then
    PrependPath LD_LIBRARY_PATH "${IPP_LIBRARY_PATH}"
fi
if [ -z "${MPI_LIBRARY_PATH}" ]; then
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
