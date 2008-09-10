#!/bin/csh
#
# new version that attempts a 64-bit build on x86_64 systems

set varch=`uname -p`
echo " * CPU arch is $varch."

if (!(${?AIPSPATH})) then
  echo "AIPSPATH environment variable not set!"
  exit
endif 

if (!(${?LOCAL})) then
  set local=/usr/local
else
  set local=$LOCAL
endif

# fetch aips++ root directory and architecture and version
set aroot=`echo $AIPSPATH | cut -f 1 -d " "`
source $aroot/aipsinit.csh
if ($varch == "x86_64") then
  aipsinit linux_gnu64
else
  aipsinit linux_gnu4x
endif

set aarch=`echo $AIPSPATH | cut -f 2 -d " "`
set avers=`avers | cut -f 1 -d " "`
set vmaj=`echo $avers | cut -f 1 -d "."`
set vmin=`echo $avers | cut -f 2 -d "."`

# set aips++ compilation flags based on architecture
# - could be smarter by using aips++ makedefs...
# - also set define if difx2ms needs to swap ends...
if ($aarch == darwin) then
  echo " * configuring DARWIN build."
  set acflags="-DAIPS_STDLIB -DAIPS_DARWIN -DAIPS_BIG_ENDIAN -D_GLIBCPP_DEPRECATED -DAIPS_NO_LEA_MALLOC -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -DAIPS_AUTO_STL -DNEED_FORTRAN_UNDERSCORES=1 -Wno-long-double"
  set acflags="-D_EDSWAPENDS ${acflags}"
else if (($aarch == linux) || ($aarch == linux_gnu4x)) then 
  echo " * configuring LINUX build."
  set acflags="-Wno-deprecated  -DAIPS_LINUX -DAIPS_LITTLE_ENDIAN -DAIPS_STDLIB -DAIPS_NO_LEA_MALLOC  -D_GLIBCPP_DEPRECATED -DAIPS_AUTO_STL -DAIPS_DEBUG -DAIPS_ARRAY_INDEX_CHECK     -DAIPS_NO_TEMPLATE_SRC -DAIPS_NO_TEMPLATE_SRC -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE"
else if (($aarch == linux_gnu64)) then
  echo " * configuring LINUX 64-bit build."
  set acflags="-Wno-deprecated  -DAIPS_LINUX -DAIPS_LITTLE_ENDIAN -DAIPS_STDLIB -DAIPS_NO_LEA_MALLOC  -D_GLIBCPP_DEPRECATED -DAIPS_AUTO_STL -DAIPS_DEBUG -DAIPS_ARRAY_INDEX_CHECK     -DAIPS_NO_TEMPLATE_SRC -DAIPS_NO_TEMPLATE_SRC -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -DAIPS_64B"  
else
  echo "Unknown architecture!"
  exit
endif

# if aips++ looks like casa, add the CASABUILD define and
# make the list of libraries to link against
if (($vmaj > 18) && ($vmin > 461)) then
  echo " * configuring CASA build."
  set acflags="-DCASABUILD ${acflags}"
  set libs="-lms -lfits -lmeasures -ltables -lscimath -lcasa"
else 
  echo " * configuring AIPS++ build."
  set libs="-ltrial -laips"
endif

# add the standard libs to the list
#set libs="${libs} -L/usr/local/cfitsio -L/usr/local/CCfits/lib -lCCfits -lcfitsio -lm"
set libs="${libs} -L${local}/cfitsio/lib -lcfitsio -lm"

/bin/rm -f difx2ms.o difx2ms

#set croot1=$aroot/external/gcc-3.4.6
#set croot2=/usr/local/gcc-4.1.0
set croot3=/usr/local/gcc-4.1.2
#set croot4=/usr/local/gcc-3.4.6
#set croot5=/usr

set extraargs="-O -Wall"
#set extraargs="-g -Wall fprofile-arcs -ftest-coverage"

if ($varch == "x86_64") then
  echo "compiling ..."
  $croot3/bin/g++ -m64 -c $extraargs $acflags -I${aroot}/code/include -I${local}/cfitsio  difx2ms.cc
  echo "linking ..."
  $croot3/bin/g++ -o difx2ms -m64 $extraargs $acflags difx2ms.o -L/usr/local/gnu/x86_64/cfitsio/lib -L${aroot}/${aarch}/lib $libs
else 
#  $croot1/bin/g++ -m32 -c -g -Wall $acflags -I${aroot}/code/include -I${local}/cfitsio  difx2ms.cc
#  echo "linking ..."
#  $croot2/bin/g++ -o difx2ms -g -m32 $acflags difx2ms.o -L${aroot}/${aarch}/lib $libs
    echo "er oops"
endif

file difx2ms

echo "stripping ..."
strip difx2ms

echo "done"

