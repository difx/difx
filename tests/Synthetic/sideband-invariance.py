#!/usr/bin/env python3
"""Compare two correlations of the same signal recorded in different sidebands.

    sideband-invariance.py [-t THRESHOLD] <reference .difx dir> <test .difx dir>

The lb-* scenarios contain the same sky signal at T2 recorded as USB and as
LSB (see createLongBaseline.sh), so a correct correlator produces the same
visibilities from either.  For every record present in both outputs (matched
on baseline, time and polarisation pair; the frequency index differs between
the jobs and is ignored), this reports the mean absolute difference relative
to the mean reference amplitude (channel 0 excluded), in percent, as
diffDiFX.py does.  It also prints the coherent (vector-averaged) correlation
coefficient and phase of the cross-correlation, which is what the difference
does to a fringe.

Prints PASS if every record is within THRESHOLD percent (default 0.0005, the
run-local.sh threshold), FAIL otherwise; always exits 0 except on errors.
"""
import argparse
import glob
import struct
import sys

import numpy as np

HDR = struct.Struct("<IiiidiiI2sid3d")


def read(difxdir):
    files = sorted(glob.glob(difxdir + "/DIFX_*"))
    if not files:
        sys.exit("no DIFX_* files in " + difxdir)
    out = {}
    for path in files:
        with open(path, "rb") as f:
            raw = f.read()
        pos = 0
        # every record has the same number of channels in these jobs
        nchan = None
        while pos + HDR.size <= len(raw):
            sync, ver, bl, mjd, sec, cfg, src, fr, pp, pb, wt, u, v, w = HDR.unpack_from(raw, pos)
            pos += HDR.size
            if nchan is None:
                # find the next sync word to get the record length
                nxt = raw.find(struct.pack("<I", sync), pos)
                nchan = ((nxt if nxt > 0 else len(raw)) - pos) // 8
            vis = np.frombuffer(raw, dtype=np.complex64, count=nchan, offset=pos).astype(np.complex128)
            pos += 8 * nchan
            out[(bl, mjd, round(sec, 3), pp)] = vis
    return out


def coherent(recs, cross, autos, ch):
    keys = sorted(k for k in recs if k[0] == cross)
    V = sum(recs[k][ch].sum() for k in keys)
    A = 0.0
    for k in keys:
        a1 = recs.get((autos[0],) + k[1:])
        a2 = recs.get((autos[1],) + k[1:])
        if a1 is None or a2 is None:
            return None
        A += np.sqrt(np.abs(a1[ch]) * np.abs(a2[ch])).sum()
    return abs(V) / A, np.angle(V, deg=True), V


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("-t", "--threshold", type=float, default=0.0005, help="percent [%(default)s]")
    p.add_argument("reference")
    p.add_argument("test")
    a = p.parse_args()

    ref, tst = read(a.reference), read(a.test)
    keys = sorted(k for k in ref if k in tst)
    if not keys:
        sys.exit("no records in common")
    worst = 0.0
    total = 0.0
    for k in keys:
        r, t = ref[k][1:], tst[k][1:]
        d = 100.0 * np.abs(t - r).mean() / np.abs(r).mean()
        worst = max(worst, d)
        total += d
    print("records compared: %d (reference %d, test %d)" % (len(keys), len(ref), len(tst)))
    print("mean percentage absolute difference %.8f, worst record %.8f (channel 0 excluded)" % (total / len(keys), worst))

    # cross-correlation baseline 258 (antennas 1 and 2), autos 257 and 514
    nch = len(ref[keys[0]])
    ch = slice(nch // 20, nch - nch // 20)   # inner 90% of the band
    cr = coherent(ref, 258, (257, 514), ch)
    ct = coherent(tst, 258, (257, 514), ch)
    if cr and ct:
        print("coherent correlation coefficient: reference %.8f, test %.8f (test/reference - 1 = %+.2e)" % (cr[0], ct[0], ct[0] / cr[0] - 1))
        print("coherent phase: reference %+.5f deg, test %+.5f deg (difference %+.5f deg)" % (cr[1], ct[1], np.angle(ct[2] / cr[2], deg=True)))
    print("PASS" if worst <= a.threshold else "FAIL", "(threshold %g percent)" % a.threshold)


if __name__ == "__main__":
    main()
