import os

from setuptools import setup

SRC_PATH = os.path.relpath(os.path.join(os.path.dirname(__file__), "src"))

setup(name='phorc',
      version='0.1',
      description='none',
      url='none',
      author='J. Barrett',
      author_email='barrettj@haystack.mit.edu',
      license='MIT',
      packages=['phorc'],
      package_dir={"": SRC_PATH,},
      zip_safe=False)
