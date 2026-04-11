# Mark5access

Please see more documentation in the doc/ directory.

For info on installing keep reading:


## Building with CMake toolchain

```bash
$  mkdir build && cd build
$  cmake .. -DCMAKE_INSTALL_PREFIX=<prefix>
$  make
$  make install
```

### Common CMake options:

- `-DCMAKE_INSTALL_PREFIX=<prefix>`   Installation prefix (default /usr/local)
- `-DCMAKE_BUILD_TYPE=Release`        Build type (Release, Debug, RelWithDebInfo)
- `-DDIFX_BUILD_UTILS=ON`             Build utility programs (automatic when building mark5access standalone)

If codifio or mark6sg are installed in a non-standard location, point
CMake at them with `-DCMAKE_PREFIX_PATH=<path>`.

To use the installed library from another CMake project:

```cmake
  find_package(Mark5access REQUIRED)
  target_link_libraries(yourapp PRIVATE DiFX::mark5access)
```

pkg-config also works after a CMake install:

```bash
  pkg-config --cflags --libs mark5access
```


## Building with Autotools toolchain

mark5access installs in a way that should be familiar to most that 
have installed modern Gnu software.

To install from a .tar.gz distribution:

```bash
$  tar zxvf mark5access-XXX.tar.gz
$  cd mark5access-XXX
$  ./configure --prefix=<prefix>     (replace <prefix> with the base install dir)
$  make
$  (maybe become root here)
$  make install
```

To install from a fresh git checkout, run the following:

```bash
aclocal
libtoolize --copy --force
autoconf
autoheader
automake -a -c

./configure --enable-python --prefix=${DIFXROOT}
make
# (su root?)
make install
```

After an git update, these steps do not need to be rerun.  usually just 
"make" will update everything properly, but sometimes it is necessary
to run "autoreconf" before compiling again.  If that doesn't work,
one can always fall back on the above sequence of commands.


### Post-install notes

A `make install` will copy libraries into `<prefix>/lib` .  Make sure this
path is either in `/etc/ld.so.conf` (ie, for `/usr/local` and `/usr`, usually),
or in your LD_LIBRARY_PATH.  In the former case, running /sbin/ldconfig
might be necessary to update some files.

`make install` will also copy a package config file into 
<prefix>/lib/pkgconfig.  Make sure this path is in your PKG_CONFIG_PATH
if you are to compile against this library.  you can test this by
running

```bash
pkg-config --cflags mark5access
```

If you get a sensible answer then you are set.
