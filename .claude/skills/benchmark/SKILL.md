---
name: benchmark
description: Measure correlator speed with the tests/FakeData benchmark (run-bench.sh, the T5-T1 metric, GPU or CPU path) and store the result in the right place - the development ledger tests/FakeData/BENCHMARKS.md for every measurement worth keeping, the DiFX wiki benchmarks page for headline results per system. Use after a change that could affect speed, when measuring a new machine, or when asked for benchmark numbers.
---

# Benchmark and record the result

A speed number is only useful later if someone can tell exactly what was measured, on
what, and at which commit. This skill is mostly about that.

## 1. Before running

1. **Correct first.** Run `gpu-parity-test` on the same build. A speed number from a build
   that fails it is meaningless.
2. **The installed binary is the one you mean** (`rebuild-and-verify`), and it is
   **committed and pushed**: every recorded result cites a commit that others can check out.
   If you must measure uncommitted code (an A/B before deciding), say so in the notes and
   do not put it on the wiki.
3. **The machine is idle.** No builds, profilers or other `mpifxcorr` (`nvidia-smi`,
   `uptime`, `ps`). On a desktop, close heavy applications. On a cluster, follow the rules
   at the top of `tests/FakeData/BENCHMARKS.md` (`--cpus-per-task=5`, `--nodes=1`, check
   the "node ownership" line); a shared node silently costs ~10%.

## 2. Run

```bash
cd tests/FakeData
./run-bench.sh 3                      # GPU path, best of 3
USEGPU=0 ./run-bench.sh 3             # CPU path
EXTRA_ENV="DIFX_GPU_PIN_INPUT=0" ./run-bench.sh 3   # an A/B leg
```

- The metric is **T5 − T1**: wall time for 5 s of data minus 1 s, i.e. the cost of 4 s
  of data without start-up and shut-down. "× real time" is 4 / (T5 − T1).
- The fake data are rewritten to single-thread VDIF by default (`SINGLE_THREAD_VDIF=0`
  keeps the interlaced corner-turn). Results from the two are not comparable.
- The script prints a ready-made ledger row.
- **FakeData measures speed only, never correctness**: see `tests/FakeData/README.md`.
- **A/B comparisons**: same session, and the same binary switched by an environment
  variable where possible. Don't always run the same leg first; the first run of a pair
  pays a warm-up cost (see the "Measurement pitfall" note referenced in `BENCHMARKS.md`).
- The startup lines in the difxlog (`vector library`, `GPU support`, `using CUDA device`)
  give the IPP/FFTW, CUDA runtime, driver and GPU details to record.

## 3. Record it in the right place

| Where | What goes there | When |
|---|---|---|
| `tests/FakeData/BENCHMARKS.md` (repository) | Every measurement worth keeping: each change that lands, both legs of an A/B, kernel profiles, with a note on what changed and why it matters. | With the change, or in the commit right after it. |
| [DiFX wiki benchmarks page](https://github.com/difx/difx/wiki/benchmarks), section "FakeData 10-station VLBA benchmark" | Headline results only: one row per system at a milestone (a merge, a release, a large speed-up, new hardware). | When the user agrees the result is a headline. |

Rules for the wiki section (they are also written on the page):

- A new machine gets one row in the **systems table**: a short ID, CPU (model, cores and
  threads), RAM, GPU (model, memory, compute capability, SMs), and software (OS, MPI,
  IPP or FFTW version, CUDA runtime and driver).
- Each measurement is a new row in the **results table**: date, `branch @ commit`, system
  ID, path (GPU/CPU and thread count), T1, T5, T5 − T1, × real time, notes. Rows stay in
  date order and are **never edited**; a re-measurement is a new row.
- Note anything that limits what the number means (e.g. a CPU leg that shares cores
  with the datastream processes).
- The wiki is public and outside the repository. Edit it through its own git repository
  (`git@github.com:difx/difx.wiki.git`) and **show the user the diff before pushing**, as
  for a commit.

## 4. Report

Give the result rows, the commit and machine, whether the machine was idle, and where you
recorded them. If anything was not recorded (an uncommitted A/B, say), say so.
