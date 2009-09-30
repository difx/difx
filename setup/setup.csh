alias PREPEND 'setenv \!^ {$\!^}:{\!:2}'

####### DIFX VERSION ########################
setenv DIFX_VERSION difx-1.5

####### ROOT PATHS ##########################
setenv DIFXROOT /usr/local/difx
setenv PGPLOTDIR /usr/local/pgplot
setenv IPPROOT /opt/intel/ipp/5.2/ia32

####### COMPILER ############################
setenv MPICXX /usr/bin/mpicxx

####### IPP libraries needed for linking #############
## Alternate lines may be needed for old versions ####
## of IPP (<=4 for 32bit, <=5 for 64 bit #############
set IPPLIB32="-lipps -lguide -lippvm -lippcore"
set IPPLIB64="-lippsem64t -lguide -lippvmem64t -liomp5 -lippcoreem64t"
## Uncomment the following for old 32 bit IPP
#PREPEND LD_LIBRARY_PATH  ${IPPROOT}/sharedlib/linux
## Uncomment the following (and comment other IPPLIB64 line) 
## for old 64 bit IPP
#set IPPLIB64="-lippsem64t -lguide -lippvmem64t -lippcoreem64t"

####### PERL VERSION/SUBVERSION #############
set perlver="5"
set perlsver="5.8.8"

####### PORTS FOR DIFXMESSAGE ###############
setenv DIFX_MESSAGE_GROUP 224.2.2.1
setenv DIFX_MESSAGE_PORT 50201
setenv DIFX_BINARY_GROUP 224.2.2.1
setenv DIFX_BINARY_PORT 50202
   
####### CALC SERVER NAME - HARMLESS #########
setenv CALC_SERVER swc000

####### No User configurable values below here

####### Operating System, use $OSTYPE

if ( $OSTYPE == "darwin" || $OSTYPE == "linux") then
  set OS=$OSTYPE
else
  echo "Warning supported O/S $OSTYPE";
  exit 1
endif

####### 32/64 BIT DEPENDENT MODIFICATIONS ###
set arch=`uname -m`
if ( $arch == "i386" || $arch == "i686" ) then #32 bit
  #echo "Adjusting paths for 32 bit machines"
  setenv DIFXBITS 32
  PREPEND PERL5LIB         ${DIFXROOT}/lib/lib/perl$perlver/site_perl/$perlsver
  setenv IPPLINKLIBS "$IPPLIB32"
else if ( $arch == "x86_64" ) then #64 bit
  #echo "Adjusting paths for 64 bit machines"
  setenv DIFXBITS 64
  PREPEND PERL5LIB         ${DIFXROOT}/perl/lib64/perl$perlver/site_perl/$perlsver/x86_64-linux-thread-multi
  setenv IPPLINKLIBS "$IPPLIB64"
else
  echo "Unknown architecture $arch - leaving paths unaltered"
endif

####### LIBRARY/EXECUTABLE PATHS ############
PREPEND PATH             ${DIFXROOT}/bin
if ($OS == "darwin") then
  PREPEND DYLD_LIBRARY_PATH  ${DIFXROOT}/lib
  PREPEND DYLD_LIBRARY_PATH  ${PGPLOTDIR}
  PREPEND DYLD_LIBRARY_PATH  ${IPPROOT}/Libraries
else
  PREPEND LD_LIBRARY_PATH  ${DIFXROOT}/lib
  PREPEND LD_LIBRARY_PATH  ${PGPLOTDIR}
  PREPEND LD_LIBRARY_PATH  ${IPPROOT}/sharedlib
endif
if ($?PKG_CONFIG_PATH) then
  PREPEND PKG_CONFIG_PATH  ${DIFXROOT}/lib/pkgconfig
else
  setenv PKG_CONFIG_PATH  ${DIFXROOT}/lib/pkgconfig
endif

echo " DiFX version $DIFX_VERSION is selected"

unalias PREPEND
