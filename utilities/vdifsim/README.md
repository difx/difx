# vdifsim

## Requirements

- fftw3
- mpich

You can using `sudo apt install libfftw3-dev libmpich-dev` to install the above libraries on Ubuntu.

## Install

To install from a fresh svn checkout, you should need install difxio,vdifio and difxmessage at first, then run the following commands:

```bash
$ libtoolize
$ aclocal
$ autoconf
$ autoheader
$ automake -a -c

$ CC=mpicc ./configure --prefix=${DIFXROOT}
$ make
# (su root?)
$ make install
```
