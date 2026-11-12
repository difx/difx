# ASKAP CRAFT voltage (CODIF) correlation test

An end-to-end correlation of real ASKAP data in CODIF format, checked at the fringe level:
it must give fringes on every baseline at the expected delay, with closure phases near
zero. It complements `tests/Synthetic` (CPU vs GPU visibility agreement on synthetic
VDIF) by covering CODIF, oversampled filterbank channels, 1-bit complex sampling, many
stations and two polarisations per antenna in separate files.

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

## Running

```bash
source ../../setup.bash
DATA=/path/to/codif ./run-askap.sh            # CPU path
DATA=/path/to/codif USEGPU=1 ./run-askap.sh   # GPU path (once the GPU correlator supports CODIF)
```

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

- GPU: the GPU correlator does not support CODIF yet; when it does, `USEGPU=1` here is
  its end-to-end check.
- Output bands: combining the oversampled 1.185 MHz channels into contiguous 2 x 4 MHz
  bands with DiFX output bands (needs output band support on the GPU path too).
- More datasets: a 4-bit one (FRB 20240310), and one with the opposite sideband sense.
- Fringe-fit level checks (delay and rate solutions, S/N after fringe fitting) for
  accuracy work such as FP16 and windowed overlap-add.
