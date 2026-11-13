# ASKAP CRAFT voltage (CODIF) correlation test

An end-to-end correlation of real ASKAP data in CODIF format, checked at the fringe level:
it must give fringes on every baseline at the expected delay, with closure phases near
zero. It complements `tests/Synthetic` (CPU vs GPU visibility agreement on synthetic
VDIF) by covering CODIF, oversampled filterbank channels, 1- and 4-bit complex sampling,
many stations and two polarisations per antenna in separate files. Two datasets:
`askap1934-lsb-1bit` (the default) and `askap0407-lsb-4bit`.

## The data: `askap1934-lsb-1bit`

PKS 1934-638, observed as the flux calibrator for FRB 20260105 (SB80896, beam 29).

Jobs are named `askap<source>-<sideband>-<bits>bit`, so that datasets which differ in
sideband sense or sample depth can sit side by side (vex2difx does not allow `_` in the
name).

| | |
|---|---|
| Antennas | 23 (AK06 ... AK36) |
| Polarisations | 2, one file each: beam 58 = X (`<ant>.p0.codif`), beam 59 = Y (`<ant>.p1.codif`) |
| Channels | 8 coarse channels of 1 MHz, oversampled by 32/27: 1.185185 MHz complex bands centred on 800 ... 807 MHz, lower sideband |
| Sampling | 1-bit complex (CODIF `CODIFC/27/8064/1`: 8064-byte frames, 27 s period) |
| Time | 2026-01-06 04:04:26 to 04:04:38 UTC (12 s of the 12.6 s dump) |
| Size | about 1.3 GB of CODIF (46 files) |

The data are not in the repository. `DATA` points the test at a directory holding the 46
`.codif` files.

### Getting the CODIF files

Either:

- **use already converted CODIF files**: a copy is kept on OzSTAR and can be provided on
  request (contact any of the DiFX GPU development team); or
- **convert the original `.vcraft` voltage dumps yourself** with `CRAFTConverter`, from the
  CRAFT repository (CSIRO; ask the ASKAP CRAFT team for access). It is not part of CELEBI.
  Convert both beams in the same, initially empty, directory: the first conversion writes
  a `.bat0` time reference that all later ones reuse, so every file shares one time origin.

  ```bash
  mkdir codif && cd codif
  for f in ../beam58/ak*_c3_f3.vcraft; do a=$(basename $f | cut -d_ -f1); CRAFTConverter $f $a.p0.codif; done
  for f in ../beam59/ak*_c3_f3.vcraft; do a=$(basename $f | cut -d_ -f1); CRAFTConverter $f $a.p1.codif; done
  ```

  Messages such as `Will skip 900 samples for frame alignment` are normal. `codifsum`
  (from DiFX's codifio) prints the CODIF frame headers if you need to check a file.

## The data: `askap0407-lsb-4bit`

PKS 0407-658, observed as the flux calibrator for FRB 20240310 (SB59893, beam 31 in
CELEBI's processing; the voltage dumps used here are beams 62 and 63).

| | |
|---|---|
| Antennas | 25 (AK07 ... AK36) |
| Polarisations | 2, one file each: beam 62 = X (`<ant>.p0.codif`), beam 63 = Y (`<ant>.p1.codif`) |
| Channels | 8 coarse channels of 1 MHz, oversampled by 32/27: 1.185185 MHz complex bands centred on 888 ... 895 MHz, lower sideband |
| Sampling | 4-bit complex (CODIF `CODIFC/27/8256/4`: 8256-byte frames, 27 s period) |
| Time | 2024-03-10 11:06:05 to 11:06:07 UTC (2 s of the 3.1 s dump; 4-bit dumps are short) |
| Size | about 1.4 GB of CODIF (50 files) |
| Phase centre | 04:08:20.380, -65:45:09.08 (J2000; CELEBI's `0407.config`) |

The 4-bit values are two's complement, symmetric about zero with a zero state: in the
files checked, code k and code -k occur equally often. The CODIF header's `representation`
field is 1; neither mark5access nor the GPU decoder reads it. ASKAP scales the 4-bit data
low (rms about 0.5 of a step: most samples are 0 or +-1), which affects only the amplitude
scale.

Converted like the 1934 data, from the `.vcraft` dumps of FRB 20240310's flux calibrator
(beams 62 and 63, `ak*_c3_f3.vcraft`), with `p0` from beam 62 and `p1` from beam 63. As
for the 1934 data, converted CODIF files are kept on OzSTAR and can be provided on request.

## Running

```bash
source ../../setup.bash
DATA=/path/to/codif ./run-askap.sh                        # CPU path, askap1934-lsb-1bit
DATA=/path/to/codif USEGPU=1 ./run-askap.sh               # GPU path
DATA=/path/to/codif ./run-askap.sh askap0407-lsb-4bit     # another job
DATA=/path/to/codif USEGPU=1 DIFX_GPU_SIDEBAND_CPUCOMPAT=1 \
    WORKDIR=local-runs/askap1934-lsb-1bit-gpucompat ./run-askap.sh  # GPU, CPU-compatible sideband test path
```

The GPU output passes the same fringe check. Its visibilities differ from the CPU's by a
few thousandths of a percent, because the GPU handles the lower sideband at decode time
(see `tests/Synthetic`); the CPU-compatible test path matches the CPU to the rounding
level (`diffDiFX.py`, under 5e-4 %).

### Comparing the GPU with the CPU (`COMPARE_WITH`)

Set `COMPARE_WITH` to the directory of an earlier run of the same job (normally the CPU
run) to diff the new run's visibilities against it with `diffDiFX.py` at `DIFF_THRESHOLD`
(default 0.0005 %):

```bash
DATA=/path/to/codif ./run-askap.sh                                    # CPU, local-runs/askap1934-lsb-1bit
DATA=/path/to/codif USEGPU=1 DIFX_GPU_SIDEBAND_CPUCOMPAT=1 WORKDIR=local-runs/gpucompat \
    COMPARE_WITH=local-runs/askap1934-lsb-1bit ./run-askap.sh         # PASS/FAIL
DATA=/path/to/codif USEGPU=1 COMPARE_WITH=local-runs/askap1934-lsb-1bit ./run-askap.sh   # INFO
```

The result (`compare  : PASS|FAIL|INFO`) counts towards the exit status, except for the
production GPU path against a CPU-style run (CPU or CPU-compatible GPU): ASKAP data are
lower sideband, which the production GPU handles at decode time and the CPU after the
FFT, so that comparison is reported as INFO with channel 0 excluded, as `run-local.sh`
does on its sideband scenarios. Each run records its path (`cpu`, `gpu`, `gpucompat`) in
`run-mode` in its directory.

### Speed (`bench-askap.sh`)

```bash
DATA=/path/to/1934/codif ./bench-askap.sh askap1934-lsb-1bit 2026y006d04h04m28s 3
DATA=/path/to/0407/codif ./bench-askap.sh askap0407-lsb-4bit - 3
```

For each of the CPU and GPU paths it times mpifxcorr on the whole job and on the job cut
at the given stop time (`STOP` in `run-askap.sh`), and reports T(long) - T(short): the cost
of the data in between, without start-up and shut-down, as T5 - T1 in `tests/FakeData`.
The 4-bit dump is too short to cut (a cut must hold a whole integration to be checked), so
it is timed whole (`-`). The data are read into the page cache before every run (`WARM=1`).
Timing runs keep the fringe check but switch off its thresholds.

The jobs have 46-50 DataStream processes, so a small machine measures contention, not the
correlator: on a 4-core desktop with the data on a spinning disk the GPU timings varied by
a factor of three between identical runs. Run it on a node with many cores and fast
storage.

The job runs in `local-runs/askap1934-lsb-1bit/` (git-ignored; set `WORKDIR` to change it; the
script refuses to reuse an existing directory). It runs vex2difx, difxcalc and mpifxcorr
there, keeps mpifxcorr's alerts in `askap1934-lsb-1bit.difxlog`, and then runs
`fringeCheckDiFX.py` (`utilities/vis2screen`) on the output. The script exits non-zero if
any step fails, if the difxlog has ERROR, SEVERE or FATAL lines, or if the fringe check
fails: lowest baseline S/N below `MINSNR` (default 10), or rms closure phase above
`MAXCLOSURE` (default 10 degrees). Other settings are listed at the top of the script.

Expected result (DiFX dev, CPU, 8 threads, about 3 minutes on a 4-core desktop):

| | median S/N per band | rms closure phase | largest residual delay |
|---|---|---|---|
| XX | 21 | 4.0 deg | 0.2 us |
| YY | 20 | 3.9 deg | 0.1 us |
| XY, YX | about 3.5 (noise: 1934-638 is unpolarised) | | |

The weakest baseline (AK06-AK13) has a parallel-hand S/N of 16 per band (median over
bands and both hands), against the default threshold of 10.

`askap0407-lsb-4bit` (gpudifx `2915203d5`, 2026-11-13; CPU, GPU and GPU-CPUCOMPAT give the
same figures):

| | median S/N per band | rms closure phase | largest residual delay |
|---|---|---|---|
| XX | 21 | 3.2 deg | 0.1 us |
| YY | 20 | 3.1 deg | 0.1 us |
| XY, YX | about 3.5 (noise) | | |

The weakest baseline (AK07-AK16) has a parallel-hand S/N of 17.8. Visibilities: GPU vs CPU
0.0040 %, GPU-CPUCOMPAT vs CPU 0.000050 % (`diffDiFX.py`).

All integrations have weight 1.00 except the last, which is partial (12 s is not a whole
number of 1.3824 s integrations).

## How the job was made

`askap1934-lsb-1bit.vex` and `askap1934-lsb-1bit.v2d` are written by `make_askap_job.py`, which needs only
the `.vcraft` headers (antennas, frequencies, times), the ASKAP `fcm` file (antenna
positions and delays) and an EOP file (`getEOP.py 61046`). It does not use SCHED or
CELEBI. To regenerate, from this directory:

```bash
./make_askap_job.py <vcraft dir>/beam58 <vcraft dir>/beam59 --codifdir @DATA@ \
    --fcm <fcm file> --eop <file with EOP lines>
```

The job name is made from `--source`, `--usb` and `--bits` unless `--name` is given.
`askap0407-lsb-4bit` was made with

```bash
./make_askap_job.py <vcraft dir>/beam62 <vcraft dir>/beam63 --codifdir @DATA@ \
    --fcm fcm_delayfix.txt --eop <getEOP.py 60379 output> \
    --source 0407-658 --ra 04:08:20.380 --dec -65:45:09.08 --bits 4 --framebytes 8256
```

It follows the conventions of CELEBI (`askap2difx.py`, `vcraft2obs.py`,
`updatefreqs.py`):

- format `CODIFC/27/8064/<bits>`, sampling `COMPLEX_DSB`;
- coarse channel bandwidth 32/27 MHz; the `FREQS` in the vcraft header are 1 MHz high,
  so each channel is centred on `FREQS - 1` MHz;
- lower sideband (CELEBI uses upper sideband only for high-band data: `--usb`);
- `clockOffset` (us) = -(fcm delay in ns) / 1000;
- 128 channels per coarse channel, `tInt = 1.3824 s`, `subintNS = 13824000` (128 FFTs).

Two vex details matter, because vex2difx otherwise rejects or misreads the job: `$TRACKS`
must give `track_frame_format = CODIF` with one `fanout_def` per recorded channel (the
v2d `format` then sets the real bit depth), and frequencies must have 15 decimal places,
or mpifxcorr stops with "FFT chunk time ... is not a whole number of nanoseconds".

## Planned

- Output bands: combining the oversampled 1.185 MHz channels into contiguous 2 x 4 MHz
  bands with DiFX output bands (needs output band support on the GPU path too).
- More datasets: one with the opposite sideband sense (high-band, USB), and one longer 4-bit dump.
- Fringe-fit level checks (delay and rate solutions, S/N after fringe fitting) for
  accuracy work such as FP16 and windowed overlap-add.
