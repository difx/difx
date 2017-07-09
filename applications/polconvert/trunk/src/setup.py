from distutils.core import setup, Extension
import numpy as np


# COMPILE THE GLOBAL CROSS-POLARIZATION FRINGE FITTING.
# IT NEEDS FFTW AND GSL:
DO_SOLVE = False




sourcefiles = ['_PolConvert.cpp','CalTable.cpp','DataIO.cpp',
               'DataIOFITS.cpp','DataIOSWIN.cpp','Weighter.cpp']

sourcefiles2 = ['_PolGainSolve.cpp']


c_ext = Extension("_PolConvert", sources=sourcefiles,
                  extra_compile_args=["-Wno-deprecated","-O3"],
                  libraries=['cfitsio'],
                  extra_link_args=["-Xlinker", "-export-dynamic"])


if DO_SOLVE:
  c_ext2 = Extension("_PolGainSolve", sources=sourcefiles2,
                  libraries=['gsl','fftw'],
                  extra_compile_args=["-Wno-deprecated","-O3"],
                  extra_link_args=["-Xlinker", "-export-dynamic"])

cfitsio='/usr/include/cfitsio'
setup(
    ext_modules=[c_ext], include_dirs=[cfitsio],
)


if DO_SOLVE:
  setup(
    ext_modules=[c_ext2],
  )





