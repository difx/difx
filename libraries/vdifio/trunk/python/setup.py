import os
try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

setup(
    name = 'vdifio',
    packages=['vdifio'],
    version = '1.1',
    description = ('A ctypes-based Python wrapper to the vdifio C/C++ library'),
    long_description=open('README').read(),
    license = 'LICENSE',
    # install_requires = 'ctypes>=1.1.0',
    requires = 'ctypes',
)
