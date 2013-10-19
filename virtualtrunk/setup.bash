####### DIFX VERSION ########################
export DIFX_VERSION=trunk

####### ROOT PATHS ##########################
export DIFXROOT=/usr/local/difx
export DIFX_PREFIX=$DIFXROOT
export PGPLOTDIR=
export IPPROOT=/opt/intel/ipp/5.2/ia32

####### COMPILER ############################
export MPICXX=/usr/bin/mpicxx

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
elif [ $arch = "x86_64" ] #64 bit
then
  export DIFXBITS=64
  PrependPath PERL5LIB         ${DIFXROOT}/perl/lib64/perl$perlver/site_perl/$perlsver/x86_64-linux-thread-multi
else
  echo "Unknown architecture $arch - leaving paths unaltered"
fi

####### LIBRARY/EXECUTABLE PATHS ############
PrependPath PATH             ${DIFXROOT}/bin
if [ $OS = "darwin" -o $OS = "darwin9.0" ] 
then
  PrependPath DYLD_LIBRARY_PATH  ${DIFXROOT}/lib
  PrependPath DYLD_LIBRARY_PATH  ${PGPLOTDIR}
else
  PrependPath LD_LIBRARY_PATH  ${DIFXROOT}/lib
  PrependPath LD_LIBRARY_PATH  ${PGPLOTDIR}
fi
PrependPath PKG_CONFIG_PATH  ${DIFXROOT}/lib/pkgconfig
if test "$PS1" != ""; then
  echo " DiFX version $DIFX_VERSION is selected"
  export PS1="\u@\h $DIFX_VERSION \W> "
fi
