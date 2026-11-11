# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

DiFX is a software correlator for Very Long Baseline Interferometry (VLBI): it combines raw
baseband voltage data recorded at multiple radio telescopes into cross-correlated visibilities
for astronomical imaging/analysis. This repo is the full DiFX distribution: the correlator engine,
job-preparation tools, calc/model tools, post-processing/format-conversion tools, shared C
libraries, and site-specific configs.

## Build system

The whole tree uses GNU autotools (`configure.ac` + `Makefile.am` per component — there is no
top-level `configure`). The supported way to build everything is the top-level `install-difx`
script, driven by environment variables set in `setup.bash` (or `setup.csh`).

```bash
# one-time: copy/edit setup.bash for local paths (DIFXROOT, IPPROOT, MPICXX, PGPLOTDIR, ...)
source setup.bash

# build + install everything essential (stops on first error by default)
./install-difx

# build out-of-tree (recommended, avoids littering the source tree with build artifacts)
cd /some/build/dir && /path/to/repo/install-difx

# useful flags
./install-difx --help              # summary
./install-difx --help-options      # full option list incl. --doonly/--skip/--also
./install-difx --doonly='difxio,mpifxcorr,vex2difx'   # build only specific components
./install-difx --noipp             # build without Intel IPP (uses generic/FFTW math backend)
./install-difx --makeflags='-j 8'  # parallel make
./install-difx -f                  # keep going after errors, report failures at the end
./install-difx --clean             # make clean across all components
```

Core dependencies: MPI (`mpicxx`), and either Intel IPP or FFTW (generic backend). PGPLOT is
needed only for optional components (`--withmonitor`, `--withfb`, `--withhops`). Many components
(HOPS, PolConvert, Mark6 support, GUI server, datasim, etc.) are opt-in via `--with*` flags and
are NOT built by default.

To build/rebuild a single component directly (once `setup.bash` has been sourced and the tree has
been configured at least once via `install-difx`), `cd` into that component's directory and run
the normal autotools cycle (`autoreconf -i && ./configure ... && make && make install`), or rerun
`install-difx --doonly='<component>' --noconf` to reuse existing configuration.

## Tests

- `mpifxcorr/test/`: small C++ unit tests (`configuration_test`, `sysutil_test`) built via
  `check_PROGRAMS` in `mpifxcorr/test/Makefile.am` — build with `make check` inside that directory
  once configured.
- `tests/Synthetic/`: end-to-end correlator tests. Each `test-*.vex` / `.v2d` pair defines a
  scenario (USB/LSB, single/double sideband, complex sampling); `createData.sh` generates
  synthetic VDIF. Two ways to run them, both needing a working installed DiFX (`mpifxcorr`,
  `vex2difx`, `generateVDIF`, etc. on `PATH`):
  - Local, no correctness check: `run-*.sh` / `run-all.sh` correlate each scenario via
    `mpirun -machinefile machines` and just produce FITS — they do NOT verify the result.
  - SLURM regression suite: `run-slurm.sh` (config in `slurm.conf`, copied from
    `slurm.conf.example`; helper `gen-sbatch.sh`) correlates every scenario on CPU and GPU
    (and optionally a reference DiFX build), then diffs the visibilities with `diffDiFX.py` and
    reports PASS/FAIL. This is the correctness safety net for the GPU correlator work — CPU and
    GPU output are expected to match. Note `diffDiFX.py` always exits 0; it flags problems only
    via printed `THRESHOLD EXCEEDED!` / header-disagreement lines, which `run-slurm.sh` parses.
- `tests/DiFXtest/DiFXtest.py`: a benchmark/regression harness that runs `mpifxcorr` against
  reference `.input`/`.difx` datasets and diffs FITS output (fetches reference data from a GitHub
  release archive). Requires `astropy` and a working DiFX install on `PATH`.
- `libraries/difxio/tests/` similarly contains library-level tests for the difxio format.

There is no single "run all tests" command across the whole repo — tests are per-component.

## High-level architecture

### The correlation pipeline

1. **vex2difx** (`applications/vex2difx`) reads a VEX schedule file plus a `.v2d` control file and
   emits one or more DiFX **jobs**: `.input`, `.calc`, `.im`, `.flag`, `.threads` files that fully
   describe a correlator run (which antennas/scans/frequencies, integration time, model params,
   etc.). Key classes: `CorrParams`/`VexInfo` parsing (`corrparams.cpp`), job assembly
   (`makejobs.cpp`, `job.cpp`, `jobgroup.cpp`), band/zoom handling (`freq.cpp`, `zoomfreq.cpp`,
   `autobands.cpp`).
2. **calcserver / difxcalc11** (`applications/calcserver`, `applications/difxcalc11`) compute the
   delay/geometric model (using CALC/calcserver or the SPICE-based difxcalc11) that gets baked into
   the `.im` polynomial model files consumed by mpifxcorr.
3. **mpifxcorr** (`mpifxcorr/src`) is the actual correlator engine — see below.
4. **difx2fits / difx2mark4 / difx2ms** (`applications/`) convert raw DiFX visibility output into
   FITS-IDI, Mark4, or CASA Measurement Set formats for downstream analysis (AIPS/CASA/HOPS).
5. **difxio** (`libraries/difxio`) is the shared C library that reads/writes/represents the DiFX
   `.input`/`.calc`/`.im`/output file formats; nearly every DiFX tool links against it. Treat it as
   the canonical definition of the on-disk job/model format — check it before hand-parsing DiFX
   files elsewhere.

### mpifxcorr: the correlator engine

`mpifxcorr` is an MPI application (`mpifxcorr/src/mpifxcorr.cpp`) where each MPI rank plays one of
three roles, assigned purely by rank number (see `fxcorr::` constants in `mpifxcorr.h` and the
role dispatch in `mpifxcorr.cpp`):

- **rank 0 — FxManager** (`fxmanager.cpp`/`.h`): coordinates the run, hands out work, collects and
  writes out visibilities.
- **ranks 1..numdatastreams — DataStream** (`datastream.cpp` + format-specific subclasses:
  `vdiffile.cpp`, `vdifnetwork.cpp`, `vdiffake.cpp`, `mark5bfile.cpp`, `nativemk5.cpp`,
  `vdifmark5.cpp`, `mark5bmark5.cpp`, `vdifmark6_datastream.cpp`, `mark5bmark6_datastream.cpp`,
  ...): reads/streams raw baseband data from disk, network, or Mark5/Mark6 modules for one
  antenna, and feeds it to Cores. `datamuxer.cpp` demultiplexes multi-thread VDIF.
- **remaining ranks — Core** (`core.cpp`/`.h`, or `gpucore.cu`/`.cuh` when built `WITH_CUDA`):
  do the actual per-antenna processing (delay/fringe rotation, filtering, FFT) via `Mode` objects,
  then cross-multiply (XMAC) and accumulate visibilities per baseline, sent back to the FxManager.

`Configuration` (`configuration.cpp`, largest file in the tree) parses the `.input`/`.calc`/`.im`
files (via difxio) into the in-memory description of the job that every other class queries.
`Model` (`model.cpp`) evaluates the delay polynomials. `Polyco` (`polyco.cpp`) handles pulsar
gating/binning. `PCal` (`pcal.cpp`) extracts phase-cal tones, including complex-sampled data.
`Visibility` (`visibility.cpp`) accumulates and writes out the correlated output.

**CPU vs GPU processing**: `Mode` (`mode.h`/`.cpp`) is the abstract per-antenna processing base
class; `CPUMode` (`cpumode.h`/`.cpp`, plus format-specific subclasses like `LBA_CPUMode`) is the
default IPP/FFTW-based implementation. When built `WITH_CUDA`, `GPUMode`
(`gpumode.cuh`/`gpumode.cu`) and `GPUCore` (`gpucore.cuh`/`gpucore.cu`, subclass of `Core`) provide
a CUDA-accelerated path (unpacking in `gpudecode.cu`, fringe rotation/mode setup in
`mk5mode_gpu.cu`). GPU work is under active development on this branch — recent history is mostly
XMAC kernel launch/grid tuning and pcal-on-GPU correctness (see `git log --oneline` in
`mpifxcorr/src`). When touching GPU code, check both the CPU (`cpumode.cpp`/`core.cpp`) and GPU
(`gpumode.cu`/`gpucore.cu`) paths for behavioral parity — they are expected to produce matching
results, and CPU-vs-GPU divergence has been a recurring bug class here (see e.g. commit
`f3c0dbd9f`).

### The architecture.h vector-math abstraction

`mpifxcorr/src/architecture.h` (generated from `architecture.h.in` at configure time) is a macro
layer (`vectorAdd_cf32`, `vectorFFT_RtoC_f32`, `u8`/`s16`/`cf32`/... typedefs, etc.) that maps a
common vector-math API onto either Intel IPP (`ARCH == INTEL`, used when IPP is available) or a
generic FFTW/hand-rolled implementation (`ARCH == GENERIC`, the `--noipp` build). Any code doing
bulk numeric work on baseband/spectral data goes through this layer rather than calling IPP or
FFTW directly, so it works in both configurations — check both branches of the `#if(ARCH == ...)`
when changing anything that uses these macros.

### Other libraries/tools of note

- `libraries/vdifio`, `libraries/mark5access`: low-level readers for the VDIF and Mark5B/Mark6
  baseband data formats, used by both mpifxcorr datastreams and standalone utilities.
- `libraries/difxmessage`: multicast status/monitoring message bus used across the DiFX
  system (`DIFX_MESSAGE_GROUP`/`DIFX_MESSAGE_PORT` in `setup.bash`); `applications/difx_monitor`
  and `applications/guiServer`/`applications/DiFXGUI`/`applications/gui` consume it.
- `libraries/vex`: VEX schedule file parser used by `vex2difx`.
- `utilities/`: standalone helper tools (data simulation, format manipulation, benchmarking,
  pulsar folding, etc.) — mostly thin wrappers around the libraries above.
- `sites/`: site-specific configuration/scripts for observatories running DiFX operationally
  (ASKAP, ATNF, Haystack, MPIfR, NRAO, SHAO, Swinburne, USNO) — reference material, not shared code.

## Contribution workflow

- Never commit directly to `main`. Branch from `origin/main` or `origin/dev` for any change
  (`git checkout -b feature-xxx origin/main`), then open a PR.
- See `CONTRIBUTION.md` for the full contributor workflow (joining the project vs. forking).
