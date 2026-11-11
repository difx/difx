---
name: difx-bug-review
description: Review part of the DiFX codebase (a library, mpifxcorr, an application, or some files) for serious defects - memory errors, logic errors, wrong maths or units, races - not style, and report each as an issue with file:line, a code snippet, how it is triggered, confidence and a suggested fix. Used headless by `sa.py review`; can also be run by hand on a directory or files.
---

# DiFX bug review

Find defects that make DiFX crash, corrupt memory, produce wrong correlator output or
wrong metadata, or fail under realistic conditions.  This is **not** a style review.
Effort is limited: concentrate on what matters, verify it, and report a few good
issues rather than many doubtful ones.

## Context

DiFX is a VLBI software correlator.  The usual data flow is
VEX file → `vex2difx` → `.input`/`.calc` → `calcif2`/`difxcalc` → `.im` → `mpifxcorr`
→ `.difx` output → `difx2fits` → FITS-IDI.

- `libraries/difxio`: reads and writes `.input`, `.calc`, `.im`, `.flag` and related files
  into the `DifxInput` structure; used by most applications.
- `libraries/vdifio`, `libraries/mark5access`, `libraries/codifio`: baseband data formats
  (frame headers, bit unpacking, fill patterns, multi-thread VDIF).
- `libraries/difxmessage`: multicast XML status/control messages.
- `libraries/dirlist`: module/file directory listings.
- `mpifxcorr`: the correlator (C++, MPI + pthreads + OpenMP).  `FxManager` hands out work,
  `DataStream`s read and send baseband data, `Core`s run `Mode` objects (unpack, fringe
  rotation, FFT, cross-multiply) and accumulate `Visibility`s.  Vector maths is either
  Intel IPP or DiFX's own replacement; **IPP semantics are the reference** for what
  `ipps*` / `vector*` calls must do.
- `applications/vex2difx`: VEX + `.v2d` → jobs and `.input` files.
- `applications/difx2fits`: DiFX output → FITS-IDI tables.

## What to look for, in priority order

1. **Memory safety**: buffer overruns (arrays; fixed-size `char` buffers with
   `strcpy`/`sprintf`/`strcat`/`sscanf("%s")`), off-by-one, wrong size in
   `malloc`/`memcpy`/`memset` (`sizeof` of a pointer instead of the element), use after
   free, double free, NULL dereference of results that can realistically be NULL
   (`fopen`, `malloc` of large buffers, failed lookups).
2. **Integer problems**: 32-bit overflow in products (samples × channels × bytes, file
   offsets beyond 2 GB in `int`/`long` instead of `off_t`/`int64`), signed/unsigned
   mixing in comparisons and loops, integer division where floating point was meant,
   shifts by ≥ the type width.
3. **Logic errors**: wrong variable or index (copy-paste: `i`/`j`, antenna 1/2, pol,
   band), inverted or always-true conditions, missing `break`, `=` for `==`, wrong loop
   bounds, unhandled enum value, ignored return value that signals an error, error paths
   that leave a structure half-initialised.
4. **Numbers and units**: MJD / day / second conversions, truncation of time to integer
   seconds, `float` used where `double` precision is needed (MJD, delays, frequencies),
   ns vs µs vs s, Hz vs MHz, degrees vs radians, sign conventions (LSB/USB, delay and
   rate signs), channel/frequency indexing.
5. **Concurrency** (mpifxcorr, difxmessage): data shared between threads without the
   lock that protects it elsewhere, inconsistent lock order, condition-variable misuse,
   MPI buffers reused before the send/receive completes.
6. **Resources that matter**: leaks per scan, job, frame or loop iteration, or in library
   code used by long-running programs; file descriptors not closed on error paths.
   Not one-off leaks at program exit.
7. **Data-format decoding**: header bit fields (masks and shifts), endianness, assumed
   frame sizes, invalid-frame and fill-pattern handling.

## On the gpudifx branch: CPU/GPU parity

When built `WITH_CUDA`, mpifxcorr has a second implementation of most station and
baseline processing. The two must give the same visibilities to floating-point
rounding, and **one path changed without the other is the most common bug on this
branch**. When the code under review is on either side of this map, read the other
side too, and report any difference in behaviour (units, sample offsets, sign
conventions, edge handling, which samples or windows are included) as a `logic` or
`units` issue, citing both locations.

| Stage | CPU | GPU |
|---|---|---|
| Unpacking, invalid and blank frames | `Mk5_CPUMode::unpack` (`mk5mode.cpp`) | `gpu_blank_frames`, `Mk5_GPUMode::blankFrames` (`mk5mode_gpu.cu`), unpack in `gpu_fused_fringe*` (`gpudecode.cu`) |
| Data weights per FFT window | `CPUMode::process` (`cpumode.cpp`) | `GPUMode::set_weights`, `gpu_set_weights`, `finishWeights` (`gpumode.cu`) |
| Fringe rotation, delay | `CPUMode::process` | `gpu_precompute_fringe_rotator`, `GPUMode::fringeRotation` (`gpumode.cu`), `gpu_fused_fringe*` (`gpudecode.cu`) |
| Fractional-sample correction | `CPUMode::process` | `GPUMode::fractionalRotation`, `gpu_resultsrotatorMultiply` (`gpumode.cu`) |
| Phase-cal extraction | `PCal` extractors (`pcal.cpp`), called from `CPUMode::process` | `DOPCAL` fold in `gpu_fused_fringe*` (`gpudecode.cu`), copied back in `GPUMode::process_gpu_afterfft`, handed over via `PCal::setPcalReal`/`setPcalComplex`; `mode.cpp` branches on `config->get_use_gpu()` |
| Cross-multiply, autocorrelations, averaging | `Core::processdata` (`core.cpp`) | `GPUCore` (`gpucore.cu`): `gpu_fuse_xmac_and_average`, `completegpudata`, XMAC plans |
| Baseline weights | `Core::processdata` | `gpu_baseline_weights` (`gpucore.cu`) |
| Post-processing, uv shift | `Core::uvshiftAndAverage*` (`core.cpp`) | shared; check that GPU results reach it in the same layout |

Further rules:
- Anything the GPU path does not implement must be refused with `NOT_SUPPORTED` at
  start-up, not silently produce output (e.g. lower sideband, multiple phase centres,
  pulsar binning). A configuration that reaches GPU code it does not support is a
  `critical` issue.
- Host/device buffer ownership: who allocates, who frees, and whether a buffer is reset
  between subints (`copyToDevice`/`copyToHost` order). Compare with what the CPU path
  resets.
- Precision: float32 where the CPU uses double (sample offsets, phases, frequencies in Hz)
  and float32 `atomicAdd` accumulation over many terms.
- Moved code: `Mode::process` on dev is `CPUMode::process` here, and dev's `Mk5Mode` is
  `Mk5_CPUMode`. A fix made on dev to the old name may still be missing here, and on the
  GPU twin.
- Known, deliberately unfixed issues are listed in `TODO.md` and
  `mpifxcorr/GPU_MERGE_NOTES.md`; mention one only if you find it has a worse effect
  than recorded there.

## Do not report

- Style, naming, formatting, comments, modernisation (`auto`, `nullptr`, smart pointers),
  const-correctness, performance unless pathological.
- Checks missing on things that cannot realistically fail; problems with no plausible
  input that triggers them.
- House style: C89 declarations at block start, partial struct initialisers, unused
  parameters, Python 2 scripts (already known).
- Anything you have not read the code for.
- Several instances of one pattern as separate issues: report it once, with the other
  locations in `also_at`.

## Method

1. **Orient** (no more than about a tenth of your turns): look at the file list; read the
   component's main header(s) for its data structures, ownership rules and conventions.
2. **Leads**: if a static-analysis leads file is given, check those findings first.  They
   are cheap to confirm or dismiss and they point at suspicious code, but most are noise.
   Only report a lead you have confirmed in the code.
3. **Read** the files, most important first: parsers of external input (files, VEX,
   frame headers, network messages), unpackers, the main processing loops, index and
   offset arithmetic, buffer handling.  Skim repetitive or table-driven code (unpacker
   lookup tables, generated parsers): check a few instances and the pattern, not every
   line.  Use Grep to find definitions and callers rather than reading whole files you
   do not need.
4. **Verify** every candidate before reporting it: re-read the exact lines, trace where
   the values come from, and check whether a caller or an earlier check already prevents
   the problem.  If you cannot make it concrete, drop it or report it at low confidence.
5. **Cover the whole unit.**  Every file should at least be skimmed; do not stop while
   files remain and turns are left.  Stop when the files are covered or your turn
   budget is nearly used, and say what you did not review.

Report **at most 10 issues**, most serious first.

## Severity and confidence

- `critical`: crash, memory corruption, or silently wrong science data in normal use.
- `major`: crash or wrong results in plausible but less common cases; a leak in a
  long-running process.
- `minor`: a real defect with limited effect (unlikely edge case, wrong error message
  that misleads).

Confidence: `high` - traced and certain; `medium` - likely, depends on input or on what
callers do; `low` - suspicious, needs someone who knows the code.

## Issue fields

- `title`: short and specific, e.g. "psrflag reads past the end of Fgate".
- `severity`, `confidence` (as above); `category`: one of `memory`, `integer`, `logic`,
  `units`, `concurrency`, `resource`, `format`, `error-handling`.
- `file` (relative to the repository root), `function`.
- `start_line`, `end_line`: the smallest range (at most 20 lines) at the reviewed commit
  that shows the problem; it is quoted as the issue's code snippet.  `line`: the key line.
- `description`: what is wrong and why, 2-5 sentences; cite other places as `file:line`.
- `trigger`: the concrete input or state that makes it go wrong.
- `fix`: a one- or two-line suggested fix (code is fine).
- `also_at`: other locations of the same pattern, as `"file:line"` strings (may be empty).

## Headless output (sa.py review)

Do not modify any files.  Reply with exactly one JSON object and nothing around it:

```json
{
  "unit": "<unit name from the prompt>",
  "summary": "2-4 sentences: what this code does, overall impression, where the risk is",
  "not_reviewed": ["file or part not covered, and why"],
  "issues": [
    {"title": "", "severity": "major", "confidence": "high", "category": "memory",
     "file": "", "function": "", "start_line": 0, "end_line": 0, "line": 0,
     "description": "", "trigger": "", "fix": "", "also_at": []}
  ]
}
```

## Interactive use

When someone runs this skill in an editor session on a directory or some files, use the
same method and present each issue in markdown instead of JSON:

    ### <title>
    **Severity:** … · **Confidence:** … · **Where:** `file:line` in `function()`
    ```c
    <snippet>
    ```
    <description>
    **When it goes wrong:** <trigger>
    **Suggested fix:** <fix>
