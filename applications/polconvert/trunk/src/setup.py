from __future__ import absolute_import
from __future__ import print_function
from distutils.core import setup, Extension
import os

try:
    import numpy as np
    # make sure we're compiling with whatever CASA gave us
    libdirs = [ os.environ['DIFXCASAPATH'] + '/../lib' ]
    print('')
    print('###################################################################')
    print('# Compiling with numpy version', np.__version__)
    print('#',                              np.__file__)
    print('#',                              os.environ['DIFXCASAPATH'])
    print('#',                              libdirs)
    print('###################################################################')
    print('')
except Exception as ex:
    print('#######################################################')
    print('# DIFXCASAPATH must be set in the environment, and/or #')
    print('# numpy must be be an installed python package        #')
    print('#######################################################')
    raise ex

if not os.path.exists(libdirs[0]):
    raise Exception('\n\n*** No library directory ' + libdirs[0] + '\n\n')

# COMPILE THE GLOBAL CROSS-POLARIZATION FRINGE FITTING.
# IT NEEDS FFTW AND GSL:
DO_SOLVE = True


# it is not clear if include_dirs is needed or not


sourcefiles1 = ['CalTable.cpp', 'DataIO.cpp', 'DataIOFITS.cpp',
                'DataIOSWIN.cpp', 'Weighter.cpp', '_PolConvert.cpp']

sourcefiles2 = ['_PolGainSolve.cpp']

sourcefiles3 = ['_getAntInfo.cpp']

sourcefiles4 = ['_XPCal.cpp']

c_ext1 = Extension("_PolConvert", sources=sourcefiles1,
                  language='c++',
                  extra_compile_args=["-Wno-deprecated","-O3"],
                  library_dirs=libdirs,
                  libraries=['cfitsio'],
                  include_dirs=[np.get_include()],
                  extra_link_args=["-Xlinker", "-export-dynamic"])

c_ext3 = Extension("_getAntInfo", sources=sourcefiles3,
                  language='c++',
                  extra_compile_args=["-Wno-deprecated","-O3"],
                  library_dirs=libdirs,
                  libraries=['cfitsio'],
                  include_dirs=[np.get_include()],
                  extra_link_args=["-Xlinker", "-export-dynamic"])

c_ext4 = Extension("_XPCal",sources=sourcefiles4,
                  language='c++',
                  extra_compile_args=["-Wno-deprecated","-O3"],
                  include_dirs=[np.get_include()],
                  extra_link_args=["-Xlinker","-export-dynamic"])


if DO_SOLVE:
  # gsl depends on cblas on some machines
  # however, cblas needs to be installed so configure tests
  # are needed.  Commenting this out until I have time to fix this.
  # libraries=['gsl','cblas','fftw3']
  # libraries=['gsl','fftw3']
  try:    liblist = os.environ['POLGAINSOLVELIBS'].split(',')
  except: liblist =['gsl','fftw3']
  print('### for PolGainSolve, libraries is',liblist)
  c_ext2 = Extension("_PolGainSolve", sources=sourcefiles2,
                  library_dirs=libdirs,
                  libraries=liblist,
                  include_dirs=[np.get_include()],
                  extra_compile_args=["-Wno-deprecated","-O3"],
                  extra_link_args=["-Xlinker", "-export-dynamic"])

cfitsio='/usr/include/cfitsio'
setup(
    ext_modules=[c_ext1], include_dirs=[cfitsio,'./'],
)


setup(
    ext_modules=[c_ext3], include_dirs=[cfitsio,'./'],
)


setup(
    ext_modules=[c_ext4],include_dirs=['./'],
)


if DO_SOLVE:
  setup(
    ext_modules=[c_ext2],
  )





