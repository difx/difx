####### ROOT PATHS ##########################
export DIFXROOT=/users/adeller/difx/
export PGPLOTDIR=/usr/lib64/
export IPPROOT=/users/adeller/intel/ipp/

####### COMPILER ############################
export MPICXX=/usr/local/bin/mpicxx

####### PERL VERSION/SUBVERSION #############
perlver="5"
perlsver="5.8.8"

####### Operating System, use $OSTYPE
if [ $OSTYPE == "darwin" || $OSTYPE == "linux" ] 
then
  OS=$OSTYPE
else
  echo "Warning supported O/S $OSTYPE";
  exit 1
fi

####### 32/64 BIT DEPENDENT MODIFICATIONS ###
####### COMMENT OUT SVN LINE IF #############
####### ONLY ONE INSTALL ####################
arch=(`uname -m`)
if [ $arch == "i386" || $arch == "i686" ] #32 bit
then
  echo "Adjusting paths for 32 bit machines"
  #export DIFXROOT=${DIFXROOT}/32
  #export SVNROOT=${SVNROOT}/32
  #export IPPROOT=${IPPROOT}/5.1/ia32/
  export DIFXBITS=32
  PrependPath PERL5LIB         ${DIFXROOT}/perl/lib/perl$perlver/site_perl/$perlsver
elif [ $arch == "x86_64" ] #64 bit
then
  echo "Adjusting paths for 64 bit machines"
  #export DIFXROOT=${DIFXROOT}/64
  #export SVNROOT=${SVNROOT}/64
  #export IPPROOT=${IPPROOT}/6.0.0.063/em64t/
  export DIFXBITS=64
  PrependPath PERL5LIB         ${DIFXROOT}/perl/lib64/perl$perlver/site_perl/$perlsver/x86_64-linux-thread-multi
else
  echo "Unknown architecture $arch - leaving paths unaltered"
fi

####### LIBRARY/EXECUTABLE PATHS ############
PrependPath PATH             ${DIFXROOT}/bin
if [ $OS eq "darwin" ] 
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
PrependPath PERL5LIB         ${DIFXROOT}/perl/Astro-0.69

####### PORTS FOR DIFXMESSAGE ###############
export DIFX_MESSAGE_GROUP=224.2.2.1
export DIFX_MESSAGE_PORT=50201
export DIFX_BINARY_GROUP=224.2.2.1
export DIFX_BINARY_PORT=50202

####### CALC SERVER NAME - HARMLESS #########
export CALC_SERVER=swc000

####### NRAO SPECIFIC PATHS - HARMLESS ######
export JOB_ROOT=/home/ssi/adeller/difx/projects 
export TESTS=/home/ssi/adeller/difx/tests 
export MARK5_DIR_PATH=/home/ssi/adeller/difx/directories 
export GAIN_CURVE_PATH=/home/swc/difx/gaincurves 
export DIFX_MACHINES=/home/ssi/adeller/difx/machines.difx 
export DIFX_HEAD_NODE=swc000 
export DIFX_ARCHIVE_ROOT=/home/swc/difx/archive 
export MANPATH=/usr/share/man:/opt/local/man:/home/swc/NRAO-DiFX-1.1/share/man 
export DIFX_VERSION=1.1 
export DIFX_GROUP_ID=difx 
export G2CDIR=/usr/lib/gcc/x86_64-redhat-linux/3.4.6/
echo "DiFX version $DIFX_VERSION is selected"
