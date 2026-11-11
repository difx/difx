---
name: rebuild-and-verify
description: Rebuild and install mpifxcorr (or another DiFX component) after a source change, then prove the installed binary is the new one and links the right libraries. Use before every test run that follows a code change, and whenever a fix seems to change nothing or a bug comes and goes between builds.
---

# Rebuild and verify

A stale installed `mpifxcorr` looks exactly like a new build that did not fix anything.
On this project that has cost whole debugging sessions. So every rebuild ends with a
check that what will run is what was just built.

## 1. Environment

```bash
source setup.bash          # from the repository root
echo "$DIFXROOT"; which mpifxcorr nvcc mpicxx
```

- Site paths (`DIFXROOT`, `IPPROOT`, `CUDAROOT`, `NVCCFLAGS`, MPI) belong in the
  git-ignored `setup.local.bash` (template: `setup.local.bash.example`), never in
  `setup.bash`. See README.md.
- If your login shell already puts another DiFX install on `PATH`, `LD_LIBRARY_PATH` or
  `PKG_CONFIG_PATH`, `setup.bash` adds the new one in front but does not remove the old
  one. A component that fails to install can then make later components configure
  against the old install's headers and `.pc` files without any error. Take the other
  install out of your login environment, or check `ldd` carefully (step 4).
- CUDA is enabled whenever `nvcc` is on `PATH` when mpifxcorr is configured. For a GPU
  build, set `CUDAROOT` and the right `-arch` in `NVCCFLAGS` (for example `sm_75` for
  Turing, `sm_80` for an A100).

## 2. Find the build directory

Build out of tree: run `install-difx` from an empty directory outside the repository.
An in-tree build fills the source tree with build products that then show up in
`git status`.

- Out of tree: the build directory is wherever `install-difx` was run from; it holds
  `mpifxcorr/src/Makefile`. If you don't know where it is, ask rather than guess.
- In tree (older checkouts): `mpifxcorr/src/Makefile` exists in the repository.

First build of a new install:

```bash
mkdir -p <builddir> && cd <builddir>
<repo>/install-difx --withcuda --withpython --nodoc --makeflags='-j 8'
```

## 3. Rebuild

```bash
cd <builddir>/mpifxcorr/src && make -j8 && make install
```

- **Always `make install`.** Tests run the installed binary, not the one in the build
  directory.
- Header changes are tracked for both `.cpp` and `.cu` files, so a plain `make` is
  enough after editing a `.h` or `.cuh`.
- If you changed a library (`mark5access`, `vdifio`, `difxio`, ...), rebuild and install
  that library first, then mpifxcorr:
  `cd <builddir> && <repo>/install-difx --doonly='mark5access,mpifxcorr' --noconf`.
- If you changed `configure.ac` or a `Makefile.am`, rerun `install-difx` for that
  component without `--noconf`.
- Unit tests: `make check` in `<builddir>/mpifxcorr/src` builds `sysutil_test`, `pcaltest`
  and `m5pcal_test` but does not run them. Run `./pcaltest auto` there: 108 pass, plus 2
  failures that the test itself marks "ought to fail" (same as dev).

## 4. Verify the install

```bash
ls -l --time-style=+%F\ %T "$(which mpifxcorr)"     # just now?
[ "$(which mpifxcorr)" = "${DIFXROOT%/}/bin/mpifxcorr" ] && echo "on PATH: OK"
ldd "$(which mpifxcorr)" | grep -E 'difx|mark5|vdif|cuda|ipp|mpi'
```

- The binary's time must be after your last source edit.
- `ldd` must show the DiFX libraries from `$DIFXROOT/lib`, not from another install, and
  the CUDA and MPI libraries you expect.
- For a change that is hard to see in the output, check for a new string:
  `strings "$(which mpifxcorr)" | grep '<new message>'`.

## 5. When results make no sense

If a bug toggles between rebuilds, disappears under compute-sanitizer or ASan, or
changes when you add an unrelated kernel launch, **suspect stale or mismatched objects
before a race**: `make clean && make && make install` in `mpifxcorr/src`, then verify
again. Check that a copy or patch you applied really took effect (`diff` or `grep` the
file) before rebuilding.

Then run the `gpu-parity-test` skill.
