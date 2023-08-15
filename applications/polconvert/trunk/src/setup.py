from __future__ import absolute_import
from __future__ import print_function
from distutils.core import setup, Extension
# DeprecationWarning: The distutils package is deprecated and slated for
# removal in Python 3.12. Use setuptools or check PEP 632 for potential
# alternatives
import os

try:
    import numpy as np
    # make sure we're compiling with whatever CASA gave us
    if 'DIFXCASAPATH' in os.environ:
        libdirs = [ os.environ['DIFXCASAPATH'] + '/../lib' ]
    else:
        libdirs = None
    print('')
    print('##################################################################')
    print('# Compiling with numpy version', np.__version__)
    print('#',np.__file__)
    print('#',libdirs)
    print('##################################################################')
    print('')
except Exception as ex:
    print('##################################################')
    print('# numpy must be be an installed python package.  #')
    print('# If ALMA mode is used DIFXCASAPATH must be in   #')
    print('# the environment pointing to the dir with CASA. #')
    print('##################################################')
    raise ex

if not libdirs is None:
  for pth in libdirs:
    if not os.path.exists(pth):
        raise Exception("Requested library dir %s  does not exist"%pth)

# if fitsio.h is found elsewhere, use POLCONVERTCFITSIODIR:
try:    cfitsio = os.environ['POLCONVERTCFITSIODIR']
except: cfitsio = '/usr/include/cfitsio'

# an option to compile the global cross-polarization fringe fitting.
# POLCONVERTDOSOLVE should be True or False
try:    DO_SOLVE = bool(os.environ['POLCONVERTDOSOLVE'])
except: DO_SOLVE = True

if DO_SOLVE:
  # gsl depends on cblas on some installations/distros and libraries
  # needs to reflect this as one of these:
  #   libraries=['gsl','cblas','fftw3']
  #   libraries=['gsl','fftw3']
  # you can set POLGAINSOLVELIBS in your environnment as one of these:
  #   export POLGAINSOLVELIBS='gsl,cblas,fftw3'
  #   export POLGAINSOLVELIBS='gsl,fftw3'
  try:    pgsliblist = os.environ['POLGAINSOLVELIBS'].split(',')
  except: pgsliblist =['gsl','cblas','fftw3']
  print('# for PolGainSolve, libraries is',pgsliblist)

sourcefiles1 = ['CalTable.cpp', 'DataIO.cpp', 'DataIOFITS.cpp',
                'DataIOSWIN.cpp', 'Weighter.cpp', '_PolConvert.cpp']

sourcefiles2 = ['_PolGainSolve.cpp']

sourcefiles3 = ['_getAntInfo.cpp']

sourcefiles4 = ['_XPCal.cpp']

sourcefiles5 = ['_XPCalMF.cpp']

c_ext1 = Extension("_PolConvert", sources=sourcefiles1,
                  extra_compile_args=["-Wno-deprecated","-O3","-std=c++11"],
                  library_dirs=libdirs,
                  libraries=['cfitsio'],
                  include_dirs=[np.get_include()],
                  extra_link_args=["-Xlinker", "-export-dynamic"])

if DO_SOLVE:
  c_ext2 = Extension("_PolGainSolve", sources=sourcefiles2,
                  library_dirs=libdirs,
                  libraries=pgsliblist,
                  include_dirs=[np.get_include()],
                  extra_compile_args=["-Wno-deprecated","-O3","-std=c++11"],
                  extra_link_args=["-Xlinker", "-export-dynamic"])

c_ext3 = Extension("_getAntInfo", sources=sourcefiles3,
                  extra_compile_args=["-Wno-deprecated","-O3","-std=c++11"],
                  library_dirs=libdirs,
                  libraries=['cfitsio'],
                  include_dirs=[np.get_include()],
                  extra_link_args=["-Xlinker", "-export-dynamic"])

c_ext4 = Extension("_XPCal",sources=sourcefiles4,
                  extra_compile_args=["-Wno-deprecated","-O3","-std=c++11"],
                  include_dirs=[np.get_include()],
                  extra_link_args=["-Xlinker","-export-dynamic"])

c_ext5 = Extension("_XPCalMF",sources=sourcefiles5,
                  extra_compile_args=["-Wno-deprecated","-O3","-std=c++11"],
                  include_dirs=[np.get_include()],
                  extra_link_args=["-Xlinker","-export-dynamic"])

setup(
    ext_modules=[c_ext1], include_dirs=[cfitsio,'./'],
)

setup(
    ext_modules=[c_ext3], include_dirs=[cfitsio,'./'],
)

if DO_SOLVE:
  setup(
    ext_modules=[c_ext2],
  )

setup(
    ext_modules=[c_ext4],include_dirs=['./'],
)

setup(
    ext_modules=[c_ext5],include_dirs=['./'],
)

# vim: set nospell:
# eof
