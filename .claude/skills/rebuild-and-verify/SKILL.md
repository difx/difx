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
- **Only one DiFX install may be on your paths.** Another install on `PATH`,
  `LD_LIBRARY_PATH` or `PKG_CONFIG_PATH` (an older one from your login shell, or the other
  branch's) lets a build pick up its headers, `.pc` files and libraries without any error.
  That is how vdifio's utilities came to be compiled against an old `codifio.h`.
  `setup.bash` removes every other DiFX install it recognises: by contents (`bin/mpifxcorr`
  or a `difxio.pc`), not just by "difx" in the name. When run interactively it prints what
  it removed. Installs in `/usr`, `/usr/local` or `/opt` are left in place with a warning;
  remove those from your login environment yourself. Check after sourcing; this must
  print nothing:

  ```bash
  for v in PATH LD_LIBRARY_PATH PKG_CONFIG_PATH PYTHONPATH; do
    tr ':' '\n' <<< "${!v}" | while read -r d; do
      r=$(DiFXInstallRoot "$d"); [ -n "$r" ] && [ "$r" != "${DIFXROOT%/}" ] && echo "$v: $d"
    done
  done
  ```

  To switch between installs (gpudifx and a dev reference build, say), source the other
  checkout's `setup.bash`; a new shell is safest.
- **Don't borrow components from another install.** Build what you need into this
  install. For example, mark6sg is optional (`--also=mark6sg`). If it is missing, vdifio,
  mark5access and mpifxcorr build without Mark6 support, rather than taking it from
  elsewhere.
- CUDA is opt-in: `install-difx --withcuda` (mpifxcorr `configure --enable-cuda`). Then
  `nvcc` must be on `PATH`, or configure stops. Set `CUDAROOT` and the right `-arch` in
  `NVCCFLAGS` (for example `sm_75` for Turing, `sm_80` for an A100).

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
<repo>/install-difx --withcuda --withpython --nodoc --makeflags='-j 8' --also=mark6sg
```

After configuring, check that no component took headers or libraries from another
install. Every `-I` and `-L` printed must be `$DIFXROOT`, the system, IPP, CUDA or MPI:

```bash
grep -h -E '^[A-Z0-9_]*(CFLAGS|CPPFLAGS|CXXFLAGS|LDFLAGS|LIBS) =' $(find <builddir> -name Makefile) \
  | grep -o -E -- '-[IL][^ ]+' | sort -u
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
  and `m5pcal_test` (and, in a CUDA build, `gpudecode_test`) but does not run them. Run
  `./pcaltest auto` there: 108 pass, plus 2 failures that the test itself marks "ought to
  fail" (same as dev).
- After any change to `gpudecode.cu` or to mark5access's decoders, run `./gpudecode_test`
  there: the GPU decoder against mark5access, every sample of every VDIF/CODIF mode. Exit 0
  means all agree (3 modes are KNOWN mark5access bugs, listed in the test).

## 4. Verify the install

```bash
ls -l --time-style=+%F\ %T "$(which mpifxcorr)"     # just now?
[ "$(which mpifxcorr)" = "${DIFXROOT%/}/bin/mpifxcorr" ] && echo "on PATH: OK"
ldd "$(which mpifxcorr)" | grep -E 'difx|mark5|vdif|cuda|ipp|mpi'
```

- The binary's time must be after your last source edit.
- `ldd` must show the DiFX libraries from `$DIFXROOT/lib`, not from another install, and
  the CUDA and MPI libraries you expect. That includes `libmark6sg` and `libmark6gather`.
- For a change that is hard to see in the output, check for a new string:
  `strings "$(which mpifxcorr)" | grep '<new message>'`.

## 5. When results make no sense

If a bug toggles between rebuilds, disappears under compute-sanitizer or ASan, or
changes when you add an unrelated kernel launch, **suspect stale or mismatched objects
before a race**: `make clean && make && make install` in `mpifxcorr/src`, then verify
again. Check that a copy or patch you applied really took effect (`diff` or `grep` the
file) before rebuilding.

Then run the `gpu-parity-test` skill.
