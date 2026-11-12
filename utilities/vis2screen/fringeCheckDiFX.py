#!/usr/bin/env python3
"""
fringeCheckDiFX.py - summarise fringe detections in a DiFX visibility file.

For every cross-correlation (baseline, band, polarisation product) all records
are averaged in time, and the delay transform of the band (zero-padded 8x) gives
a fringe peak, its S/N against the rest of the delay spectrum, and the residual
delay of the peak.  Closure phases are formed from the band-averaged phases of
the parallel hands, which for a compact source should be close to zero.

It is meant as a quick sanity check of a correlation (are there fringes on every
baseline, at the expected delay, with consistent phases?), not as a substitute
for fringe fitting.  With --minsnr and/or --maxclosure it exits with status 1 if
the data fail the check, so it can be used in tests.

Usage: fringeCheckDiFX.py [options] <job.input> <DIFX_xxx file>
"""

import argparse
import itertools
import struct
import sys

import numpy as np

# DiFX binary visibility record header (version 1)
HDR = struct.Struct("<IiiidiiI2sid3d")
SYNC = 0xFF00FF00
ZEROPAD = 8


def read_input(path):
    """Channels and bandwidth per frequency index, and telescope names, from the .input file."""
    nchan, bw, tels = {}, {}, {}
    for line in open(path):
        if ":" not in line:
            continue
        key, val = line.split(":", 1)
        key, val = key.strip(), val.strip()
        if key.startswith("NUM CHANNELS "):
            nchan[int(key.split()[-1])] = int(val)
        elif key.startswith("BW (MHZ) "):
            bw[int(key.split()[-1])] = float(val)
        elif key.startswith("TELESCOPE NAME "):
            tels[int(key.split()[-1])] = val
    return nchan, bw, tels


def accumulate(path, nchan):
    """Time-averaged visibility spectrum per (ant1, ant2, freq, polpair), cross-correlations only."""
    acc = {}
    with open(path, "rb") as f:
        while True:
            h = f.read(HDR.size)
            if len(h) < HDR.size:
                break
            sync, version, bl, mjd, sec, cfg, src, fr, pp, pbin, wt, u, v, w = HDR.unpack(h)
            if sync != SYNC:
                sys.exit("Error: lost sync at byte %d of %s" % (f.tell() - HDR.size, path))
            n = nchan[fr]
            vis = np.frombuffer(f.read(8 * n), dtype=np.complex64)
            a1, a2 = bl // 256 - 1, bl % 256 - 1
            if a1 == a2:
                continue
            key = (a1, a2, fr, pp.decode())
            acc[key] = acc.get(key, 0) + vis.astype(np.complex128)
    return acc


def fringe(spec, bw_mhz):
    """S/N and delay (us) of the delay-transform peak of one band."""
    n = len(spec)
    lag = np.abs(np.fft.fftshift(np.fft.fft(spec, ZEROPAD * n)))
    k = int(np.argmax(lag))
    mask = np.ones(len(lag), bool)
    mask[max(0, k - 2 * ZEROPAD):k + 2 * ZEROPAD + 1] = False
    snr = (lag[k] - np.mean(lag[mask])) / np.std(lag[mask])
    delay_us = (k - len(lag) // 2) / (ZEROPAD * bw_mhz)    # one lag step = 1/(ZEROPAD * bandwidth)
    return snr, delay_us


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("inputfile", help="the job's .input file")
    p.add_argument("difxfile", help="a DIFX_* visibility file from the job's .difx directory")
    p.add_argument("--edgefrac", type=float, default=0.0,
                   help="fraction of channels to drop at each band edge when averaging phases (e.g. 0.08 for 32/27 oversampled bands)")
    p.add_argument("--minsnr", type=float, help="fail if any baseline's median parallel-hand S/N is below this")
    p.add_argument("--maxclosure", type=float, help="fail if the rms parallel-hand closure phase (deg) exceeds this")
    p.add_argument("-v", "--verbose", action="store_true", help="list every baseline")
    args = p.parse_args()

    nchan, bw, tels = read_input(args.inputfile)
    acc = accumulate(args.difxfile, nchan)
    if not acc:
        sys.exit("Error: no cross-correlations found")

    res = {}   # (a1, a2, fr, pp) -> (snr, delay, band-averaged visibility)
    for key, spec in acc.items():
        snr, delay = fringe(spec, bw[key[2]])
        e = int(round(args.edgefrac * len(spec)))
        res[key] = (snr, delay, np.mean(spec[e:len(spec) - e]))

    pps = sorted({k[3] for k in res})
    parallel = [pp for pp in pps if pp[0] == pp[1]]
    print("%d baseline/band/polarisation combinations, products %s" % (len(res), " ".join(pps)))
    print("\nS/N per band (median, min, max) and |residual delay| (median, max, us):")
    for pp in pps:
        s = np.array([r[0] for k, r in res.items() if k[3] == pp])
        d = np.abs([r[1] for k, r in res.items() if k[3] == pp])
        print("  %s: S/N %6.1f %6.1f %6.1f   delay %.3f %.3f" % (pp, np.median(s), s.min(), s.max(), np.median(d), d.max()))

    print("\nparallel-hand S/N per band (median over baselines):")
    for fr in sorted({k[2] for k in res}):
        print("  band %d: %s" % (fr, "  ".join("%s %.1f" % (pp, np.median([r[0] for k, r in res.items() if k[2] == fr and k[3] == pp])) for pp in parallel)))

    # per baseline: median over bands of the parallel hands
    bl = {}
    for (a1, a2, fr, pp), r in res.items():
        if pp in parallel:
            bl.setdefault((a1, a2), []).append(r)
    blsnr = {k: np.median([r[0] for r in v]) for k, v in bl.items()}
    worst = min(blsnr, key=blsnr.get)
    print("\nbaselines: %d, parallel-hand S/N per baseline: median %.1f, lowest %.1f (%s-%s)"
          % (len(bl), np.median(list(blsnr.values())), blsnr[worst], tels.get(worst[0], worst[0]), tels.get(worst[1], worst[1])))
    if args.verbose:
        for (a1, a2), s in sorted(blsnr.items()):
            d = np.median([r[1] for r in bl[(a1, a2)]])
            print("  %s-%s  S/N %6.1f  delay %+.3f us" % (tels.get(a1, a1), tels.get(a2, a2), s, d))

    # closure phases from band-averaged phases, per parallel hand
    ants = sorted({k[0] for k in res} | {k[1] for k in res})
    closure_rms = 0.0
    print("\nclosure phases (band-averaged, parallel hands):")
    for pp in parallel:
        ph = {}
        for (a1, a2, fr, q), r in res.items():
            if q == pp:
                ph.setdefault((a1, a2), []).append(r[2])
        ph = {k: np.angle(np.mean(v)) for k, v in ph.items()}
        cl = []
        for i, j, k in itertools.combinations(ants, 3):
            if (i, j) in ph and (j, k) in ph and (i, k) in ph:
                cl.append(np.angle(np.exp(1j * (ph[(i, j)] + ph[(j, k)] - ph[(i, k)]))))
        if not cl:
            continue
        cl = np.degrees(np.array(cl))
        rms = np.sqrt(np.mean(cl ** 2))
        closure_rms = max(closure_rms, rms)
        print("  %s: %d triangles, median |closure| %.1f deg, 90th percentile %.1f deg, rms %.1f deg"
              % (pp, len(cl), np.median(np.abs(cl)), np.percentile(np.abs(cl), 90), rms))

    failed = []
    if args.minsnr is not None and blsnr[worst] < args.minsnr:
        failed.append("lowest baseline S/N %.1f < %.1f" % (blsnr[worst], args.minsnr))
    if args.maxclosure is not None and closure_rms > args.maxclosure:
        failed.append("closure phase rms %.1f deg > %.1f deg" % (closure_rms, args.maxclosure))
    if failed:
        print("\nFAIL: " + "; ".join(failed))
        sys.exit(1)
    if args.minsnr is not None or args.maxclosure is not None:
        print("\nPASS")


if __name__ == "__main__":
    main()
