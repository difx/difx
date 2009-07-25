####### DIFX VERSION ########################
export DIFX_VERSION=difx-1.5

####### ROOT PATHS ##########################
export DIFXROOT=/usr/local/difx
export PGPLOTDIR=/usr/local/pgplot
export IPPROOT=/opt/intel/ipp/5.2/ia32

####### COMPILER ############################
export MPICXX=/usr/bin/mpicxx

####### IPP libraries needed for linking #############
IPPLIB32="-lipps -lguide -lippvm -lippcore"
IPPLIB64="-lippsem64t -lguide -lippvmem64t -liomp5 -lippcoreem64t"

####### PERL VERSION/SUBVERSION #############
perlver="5"
perlsver="5.8.8"

####### No User configurable values below here

####### Operating System, use $OSTYPE
if [ $OSTYPE = "darwin" -o $OSTYPE = "linux" -o $OSTYPE = "linux-gnu" ] 
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
    eval $Path=$NewItem
fi
}

####### 32/64 BIT DEPENDENT MODIFICATIONS ###
####### COMMENT OUT SVN LINE IF #############
####### ONLY ONE INSTALL ####################
arch=(`uname -m`)
if [ $arch = "i386" -o $arch = "i686" ] #32 bit
then
  #echo "Adjusting paths for 32 bit machines"
  #export DIFXROOT=${DIFXROOT}/32
  #export SVNROOT=${SVNROOT}/32
  #export IPPROOT=${IPPROOT}/5.1/ia32/
  export DIFXBITS=32
  PrependPath PERL5LIB         ${DIFXROOT}/perl/lib/perl$perlver/site_perl/$perlsver
  export IPPLINKLIBS="$IPPLIB32"
elif [ $arch = "x86_64" ] #64 bit
then
  #echo "Adjusting paths for 64 bit machines"
  #export DIFXROOT=${DIFXROOT}/64
  #export SVNROOT=${SVNROOT}/64
  #export IPPROOT=${IPPROOT}/6.0.0.063/em64t/
  export DIFXBITS=64
  PrependPath PERL5LIB         ${DIFXROOT}/perl/lib64/perl$perlver/site_perl/$perlsver/x86_64-linux-thread-multi
  export IPPLINKLIBS="$IPPLIB64"
else
  echo "Unknown architecture $arch - leaving paths unaltered"
fi

####### LIBRARY/EXECUTABLE PATHS ############
PrependPath PATH             ${DIFXROOT}/bin
if [ $OS = "darwin" ] 
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

####### PORTS FOR DIFXMESSAGE ###############
export DIFX_MESSAGE_GROUP=224.2.2.1
export DIFX_MESSAGE_PORT=50201
export DIFX_BINARY_GROUP=224.2.2.1
export DIFX_BINARY_PORT=50202

####### CALC SERVER NAME - HARMLESS #########
export CALC_SERVER=swc000

####### NRAO SPECIFIC PATHS - HARMLESS ######
export GAIN_CURVE_PATH=/home/swc/difx/gaincurves 
export DIFX_HEAD_NODE=swc000 
export DIFX_ARCHIVE_ROOT=/home/swc/difx/archive 
export MANPATH=/usr/share/man:/opt/local/man:/home/swc/NRAO-DiFX-1.1/share/man 
export DIFX_GROUP_ID=difx 
export G2CDIR=/usr/lib/gcc/x86_64-redhat-linux/3.4.6/
echo "DiFX version $DIFX_VERSION is selected"
