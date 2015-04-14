import os
try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

setup(
    name = 'mark5access',
    packages=['mark5access'],
    version = '1.5.3',
    description = ('A ctypes-based Python wrapper to the mark5access C/C++ library'),
    long_description=open('README').read(),
    license = 'LICENSE',
    # install_requires = 'ctypes>=1.1.0',
    requires = 'ctypes',
)
