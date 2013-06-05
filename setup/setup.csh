alias PREPEND 'setenv \!^ {\!:2}:{$\!^}'

####### DIFX VERSION ########################
setenv DIFX_VERSION trunk

####### ROOT PATHS ##########################
setenv DIFXROOT /home/vlbi/difx
setenv DIFX_PREFIX $DIFXROOT
setenv PGPLOTDIR 
setenv IPPROOT /opt/intel/ipp/5.2/ia32

####### COMPILER ############################
#setenv MPICXX /home/vlbi/openmpi/bin/mpicxx
setenv MPICXX /usr/bin/mpicxx

####### USE GFORTRAN IN PREFERENCE TO G77? ##
####### Comment out if not desired ##########
setenv USEGFORTRAN "yes"

####### IPP libraries needed for linking #############
set IPPLIB32="-lipps -lippvm -lippcore"
set IPPLIB64="-lippsem64t -lippvmem64t -lippcoreem64t"
#Comment out the following for very new IPP (version > 6)
set IPPLIB32="${IPPLIB32} -lguide"
set IPPLIB64="${IPPLIB64} -lguide"
#Comment out the following for older (pre version 6) IPP
#set IPPLIB32="${IPPLIB32} -liomp5"
#set IPPLIB64="${IPPLIB64} -liomp5"
#Uncomment the following for very old (pre version 5) IPP
#PREPEND LD_LIBRARY_PATH  ${IPPROOT}/sharedlib/linux

####### PERL VERSION/SUBVERSION #############
set perlver="5"
set perlsver="5.10.1"
PREPEND PERL5LIB         ${DIFXROOT}/share/perl/$perlver

####### PORTS FOR DIFXMESSAGE ###############
# Uncomment these to enable DIFX_MESSAGES
#setenv DIFX_MESSAGE_GROUP 224.2.2.1
#setenv DIFX_MESSAGE_PORT 50201
#setenv DIFX_BINARY_GROUP 224.2.2.1
#setenv DIFX_BINARY_PORT 50202

####### CALC SERVER NAME - HARMLESS ######### 
setenv CALC_SERVER swc000

####### No User configurable values below here

####### Operating System, use $OSTYPE

if ( $OSTYPE == "darwin" || $OSTYPE == "linux" || $OSTYPE == "linux-gnu") then
  set OS=$OSTYPE
else if ( $OSTYPE == "darwin9.0" ) then
  set OS="darwin"
else
  echo "Warning supported O/S $OSTYPE";
  exit 1
endif
setenv DIFXOS $OS

####### 32/64 BIT DEPENDENT MODIFICATIONS ###
set arch=`uname -m`
if ( $arch == "i386" || $arch == "i686" ) then #32 bit
  setenv DIFXBITS 32
  setenv IPPLINKLIBS "$IPPLIB32"
else if ( $arch == "x86_64" ) then #64 bit
  setenv DIFXBITS 64
  setenv IPPLINKLIBS "$IPPLIB64"
else
  echo "Unknown architecture $arch - leaving paths unaltered"
endif

####### LIBRARY/EXECUTABLE PATHS ############
PREPEND PATH             ${DIFXROOT}/bin
if ($DIFXOS == "darwin") then
  PREPEND DYLD_LIBRARY_PATH  ${DIFXROOT}/lib
  PREPEND DYLD_LIBRARY_PATH  ${PGPLOTDIR}
  #PREPEND DYLD_LIBRARY_PATH  ${IPPROOT}/Libraries
else
  PREPEND LD_LIBRARY_PATH  ${DIFXROOT}/lib
  PREPEND LD_LIBRARY_PATH  ${PGPLOTDIR}
  #PREPEND LD_LIBRARY_PATH  ${IPPROOT}/sharedlib
endif
if ($?PKG_CONFIG_PATH) then
  PREPEND PKG_CONFIG_PATH  ${DIFXROOT}/lib/pkgconfig
else
  setenv PKG_CONFIG_PATH  ${DIFXROOT}/lib/pkgconfig
endif

echo " DiFX version $DIFX_VERSION is selected"

unalias PREPEND
