####### DIFX VERSION ########################
export DIFX_VERSION=trunk

####### ROOT PATHS ##########################
export DIFXROOT=$HOME/DiFX/$DIFX_VERSION
export DIFX_PREFIX=$DIFXROOT
export PGPLOTDIR=/share/apps/pgplot
export IPPROOT=/share/apps/intel/ipp/6.1.2.051/em64t

####### COMPILER ############################
export MPICXX=/share/apps/openmpi/bin/mpicxx

####### USE GFORTRAN IN PREFERENCE TO G77? ##
####### Comment out if not desired ##########
export USEGFORTRAN="yes"

####### IPP libraries needed for linking #############
## Alternate lines may be needed for old versions ####
## of IPP (<=4 for 32bit, <=5 for 64 bit #############
#IPPLIB32="-lipps -lguide -lippvm -lippcore"
IPPLIB64="-lippsem64t -lguide -lippvmem64t -liomp5 -lippcoreem64t"
## Uncomment the following for old 32 bit IPP
#PrependPath LD_LIBRARY_PATH  ${IPPROOT}/sharedlib/linux
## Uncomment the following (and comment other IPPLIB64 line) 
## for old 64 bit IPP
#IPPLIB64="-lippsem64t -lguide -lippvmem64t -lippcoreem64t"

####### PERL VERSION/SUBVERSION #############
perlver="5"
perlsver="5.8.8"

####### PORTS FOR DIFXMESSAGE ###############
# Uncomment these to enable DIFX_MESSAGES
#export DIFX_MESSAGE_GROUP=224.2.2.1
#export DIFX_MESSAGE_PORT=50201
#export DIFX_BINARY_GROUP=224.2.2.1
#export DIFX_BINARY_PORT=50202

####### No User configurable values below here

####### Operating System, use $OSTYPE
if [ $OSTYPE = "darwin" -o $OSTYPE = "darwin9.0" -o $OSTYPE = "linux" -o $OSTYPE = "linux-gnu" ] 
then
  OS=$OSTYPE
else
  echo "Warning unsupported O/S $OSTYPE"
  exit 1
fi

PrependPath()
{
Path="$1"
NewItem="$2"
eval CurPath=\$$Path

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
        eval $Path=$NewItem\:$CurPath
    fi
else
    eval export $Path=$NewItem
fi
}

####### 32/64 BIT DEPENDENT MODIFICATIONS ###
arch=(`uname -m`)
if [ $arch = "i386" -o $arch = "i686" ] #32 bit
then
  export DIFXBITS=32
  PrependPath PERL5LIB         ${DIFXROOT}/perl/lib/perl$perlver/site_perl/$perlsver
  export IPPLINKLIBS="$IPPLIB32"
elif [ $arch = "x86_64" ] #64 bit
then
  export DIFXBITS=64
  PrependPath PERL5LIB         ${DIFXROOT}/perl/lib64/perl$perlver/site_perl/$perlsver/x86_64-linux-thread-multi
  export IPPLINKLIBS="$IPPLIB64"
else
  echo "Unknown architecture $arch - leaving paths unaltered"
fi

####### LIBRARY/EXECUTABLE PATHS ############
PrependPath PATH             ${DIFXROOT}/bin
if [ $OS = "darwin" -o $OS = "darwin9.0" ] 
then
  PrependPath DYLD_LIBRARY_PATH  ${DIFXROOT}/lib
  PrependPath DYLD_LIBRARY_PATH  ${PGPLOTDIR}
  PrependPath DYLD_LIBRARY_PATH  ${IPPROOT}/Libraries
else
  PrependPath LD_LIBRARY_PATH  ${DIFXROOT}/lib
  PrependPath LD_LIBRARY_PATH  ${PGPLOTDIR}
  PrependPath LD_LIBRARY_PATH  ${IPPROOT}/sharedlib
fi
PrependPath PKG_CONFIG_PATH  ${DIFXROOT}/lib/pkgconfig
if test "$PS1" != ""; then
  echo " DiFX version $DIFX_VERSION is selected"
  export PS1="\u@\h $DIFX_VERSION \W> "
fi
