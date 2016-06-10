from distutils.core import setup, Extension
import numpy as np

sourcefiles = ['_PolConvert.cpp','CalTable.cpp','DataIO.cpp',
               'DataIOFITS.cpp','DataIOSWIN.cpp','Weighter.cpp']

c_ext = Extension("_PolConvert", sources=sourcefiles,
                  extra_compile_args=["-Wno-deprecated","-O3"],
                  libraries=['cfitsio'],
                  extra_link_args=["-Xlinker", "-export-dynamic"])

cfitsio='/usr/include/cfitsio'
setup(
##  ext_modules=[c_ext] #, include_dirs=[include_gsl_dir] + [np.get_include()]
    ext_modules=[c_ext], include_dirs=[cfitsio],
)



