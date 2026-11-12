#!/usr/bin/env python3
"""
Write a vex + v2d pair to correlate ASKAP CRAFT voltage dumps (converted to
CODIF by CRAFTConverter) with DiFX, without SCHED or CELEBI.

Inputs:
  - one directory of .vcraft files per polarisation, in polarisation order
    (X first, then Y; for ASKAP beam 2n is X and 2n+1 is Y). Only the 4096-byte
    text headers are read: antenna list, frequencies, sample rate, start/stop;
  - the ASKAP fcm file, for antenna ITRF positions and delays;
  - an EOP file in v2d format (lines "EOP <mjd> { ... }", e.g. from getEOP.py).

The CODIF files are expected as <codifdir>/<ant>.p<pol>.codif (CRAFTConverter
output, as named by CELEBI). --codifdir may be a placeholder such as @DATA@,
which a run script substitutes later.

Conventions follow CELEBI's askap2difx.py / vcraft2obs.py / updatefreqs.py:
  - CODIF format CODIFC/27/<framebytes>/<bits>, sampling COMPLEX_DSB;
  - each coarse channel is 1 MHz oversampled by 32/27 (bandwidth 1.185185 MHz);
  - the vcraft FREQS header is 1 MHz high: channel centre = FREQS - 1;
  - lower sideband unless --usb (CELEBI: only high-band data are USB);
  - clockOffset (us) = -(fcm delay in ns) / 1000.
"""

import argparse
import datetime
import glob
import math
import os
import re
import sys

OVERSAMPLE_NUM, OVERSAMPLE_DEN = 32, 27
COARSE_MHZ = 1.0
POLS = "XY"
CODES = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"


def read_vcraft_header(path):
    with open(path, "rb") as f:
        raw = f.read(4096).decode("ascii", errors="replace")
    vals = {}
    for line in raw.split("\n"):
        line = line.split("#")[0].strip()
        parts = line.split(None, 1)
        if len(parts) == 2:
            vals[parts[0]] = parts[1].strip()
    return vals


def read_fcm(path):
    """Return {antenna number: {'name', 'itrf': [x,y,z], 'delay_ns'}}."""
    ants = {}
    pat = re.compile(r"^common\.antenna\.ant(\d+)\.(name|delay|location\.itrf)\s*=\s*(.*)$")
    with open(path) as f:
        for line in f:
            m = pat.match(line.strip())
            if not m:
                continue
            num, key, value = int(m.group(1)), m.group(2), m.group(3).strip()
            a = ants.setdefault(num, {})
            if key == "name":
                a["name"] = value
            elif key == "delay":
                if not value.endswith("ns"):
                    sys.exit(f"fcm: delay for ant{num} is not in ns: {value}")
                a["delay_ns"] = float(value[:-2])
            else:
                a["itrf"] = [float(v) for v in value.strip("[]").split(",")]
    return ants


def mjd2vex(mjd):
    """MJD -> vex time string, e.g. 2026y006d04h04m26s (rounded to whole seconds)."""
    t = datetime.datetime(1858, 11, 17) + datetime.timedelta(seconds=round(mjd * 86400))
    return t.strftime("%Yy%jd%Hh%Mm%Ss")


def radec_vex(ra, dec):
    h, m, s = ra.split(":")
    d, dm, ds = dec.split(":")
    return "%02dh%02dm%ss" % (int(h), int(m), s), "%sd%02d'%s\"" % (d, int(dm), ds)


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("beamdirs", nargs="+", help="vcraft directory per polarisation, X first")
    p.add_argument("--codifdir", required=True, help="directory with <ant>.p<pol>.codif, or a placeholder like @DATA@")
    p.add_argument("--fcm", required=True)
    p.add_argument("--eop", required=True, help="file with v2d EOP lines")
    p.add_argument("--fileglob", default="ak*.vcraft", help="vcraft files to use in each beam directory")
    p.add_argument("--source", default="1934-638")
    p.add_argument("--ra", default="19:39:25.02671")
    p.add_argument("--dec", default="-63:42:45.6255")
    p.add_argument("--name", help="job name (default askap<source>-<lsb|usb>-<bits>bit, e.g. askap1934-lsb-1bit)")
    p.add_argument("--bits", type=int, default=1)
    p.add_argument("--framebytes", type=int, default=8064)
    p.add_argument("--nchan", type=int, default=128, help="spectral points per coarse channel")
    p.add_argument("--subintns", type=int, default=13824000)
    p.add_argument("--tint", type=float, default=1.3824)
    p.add_argument("--usb", action="store_true", help="upper sideband (high-band data)")
    args = p.parse_args()
    if args.name is None:
        args.name = "askap%s-%s-%dbit" % (re.split(r"[^0-9A-Za-z]", args.source)[0],
                                          "usb" if args.usb else "lsb", args.bits)
    if "_" in args.name:
        sys.exit("vex2difx does not allow '_' in the job name")

    npol = len(args.beamdirs)
    if npol > 2:
        sys.exit("at most two polarisations")
    fcm = read_fcm(args.fcm)
    check_files = "@" not in args.codifdir

    ants = None   # ordered list of ASKAP antenna numbers
    freqs = None
    start, stop = None, None
    for pol, bdir in enumerate(args.beamdirs):
        files = sorted(glob.glob(os.path.join(bdir, args.fileglob)))
        if not files:
            sys.exit(f"no vcraft files in {bdir}")
        nums = []
        for f in files:
            h = read_vcraft_header(f)
            if int(h.get("NPOL", 1)) != 1:
                sys.exit(f"{f}: expected one polarisation per file")
            nums.append(int(h["ANTENNA_NO"]))
            these = [int(x) for x in h["FREQS"].split(",")]
            if freqs is None:
                freqs = these
            elif these != freqs:
                sys.exit(f"{f}: FREQS differ from the first file")
            t1 = float(h["TRIGGER_MJD"])
            t0 = t1 - float(h["NSAMPS_REQUEST"]) / float(h["SAMP_RATE"]) / 86400.0
            start = t0 if start is None else max(start, t0)
            stop = t1 if stop is None else min(stop, t1)
        if ants is None:
            ants = nums
        elif nums != ants:
            sys.exit(f"{bdir}: antenna list differs from {args.beamdirs[0]}")
    if len(ants) > len(CODES):
        sys.exit("too many antennas for two-character station codes")

    stations = []   # (code, NAME, fcm entry, [codif file per pol])
    for i, num in enumerate(ants):
        a = fcm.get(num)
        if not a or "itrf" not in a or "delay_ns" not in a:
            sys.exit(f"antenna {num} is missing from the fcm")
        name = "AK%02d" % num
        codifs = [os.path.join(args.codifdir, "%s.p%d.codif" % (name.lower(), pol)) for pol in range(npol)]
        for c in codifs:
            if check_files and not os.path.exists(c):
                sys.exit(f"missing {c}")
        stations.append(("A" + CODES[i], name, a, codifs))

    # whole seconds inside the data on every antenna and polarisation
    start = math.ceil(start * 86400) / 86400.0
    stop = math.floor(stop * 86400) / 86400.0
    dur = int(round((stop - start) * 86400))
    if dur < 1:
        sys.exit("less than 1 s of common data")

    bw = COARSE_MHZ * OVERSAMPLE_NUM / OVERSAMPLE_DEN
    sb = "U" if args.usb else "L"
    edges = []
    for fhdr in freqs:
        centre = fhdr - 1.0                        # vcraft header offset (CELEBI)
        edges.append(centre - bw / 2 if sb == "U" else centre + bw / 2)
    nfreq = len(edges)

    codes = ":".join(s[0] for s in stations)
    rav, decv = radec_vex(args.ra, args.dec)
    vstart, vstop = mjd2vex(start), mjd2vex(stop)

    with open(args.name + ".vex", "w") as v:
        v.write("VEX_rev = 1.5;\n")
        v.write("* Written by tests/ASKAP/make_askap_job.py: ASKAP CRAFT voltages (CRAFTConverter -> CODIF)\n")
        v.write("* %d polarisation(s) x %d coarse channels of %.6f MHz (1 MHz oversampled 32/27), %s sideband\n\n"
                % (npol, nfreq, bw, sb))
        v.write("$GLOBAL;\n  ref $EXPER = %s;\n\n" % args.name)
        v.write("$EXPER;\ndef %s;\n  exper_name = %s;\n  exper_nominal_start = %s;\n"
                "  exper_nominal_stop = %s;\n  target_correlator = DIFX;\nenddef;\n\n"
                % (args.name, args.name, vstart, vstop))
        v.write("$MODE;\ndef askap;\n")
        for block in ("FREQ", "BBC", "IF", "TRACKS"):
            v.write("  ref $%s = askap%s:%s;\n" % (block, block.lower(), codes))
        v.write("enddef;\n\n$STATION;\n")
        for code, name, _, _ in stations:
            v.write("def %s;\n  ref $SITE = %s;\n  ref $ANTENNA = %s;\nenddef;\n" % (code, name, name))
        v.write("\n$SITE;\n")
        for code, name, a, _ in stations:
            x, y, z = a["itrf"]
            v.write("def %s;\n  site_type = fixed;\n  site_name = %s;\n  site_ID = %s;\n"
                    "  site_position = %.6f m : %.6f m : %.6f m;\nenddef;\n" % (name, name, code, x, y, z))
        v.write("\n$ANTENNA;\n")
        for _, name, _, _ in stations:
            v.write("def %s;\n  axis_type = az : el;\n  axis_offset = 0.0 m;\nenddef;\n" % name)
        v.write("\n$SOURCE;\ndef %s;\n  source_name = %s;\n  ra = %s; dec = %s; ref_coord_frame = J2000;\nenddef;\n\n"
                % (args.source, args.source, rav, decv))
        # channels: all frequencies of pol X, then all of pol Y (the order of the datastreams)
        v.write("$FREQ;\ndef askapfreq;\n  sample_rate = %.15f Ms/sec;\n" % (2 * bw))
        for pol in range(npol):
            for k, edge in enumerate(edges):
                c = pol * nfreq + k + 1
                v.write("  chan_def = : %.15f MHz : %s : %.15f MHz : &CH%02d : &BBC%02d : &NoCal;  * %s\n"
                        % (edge, sb, bw, c, c, POLS[pol]))
        v.write("enddef;\n\n$BBC;\ndef askapbbc;\n")
        for pol in range(npol):
            for k in range(nfreq):
                c = pol * nfreq + k + 1
                v.write("  BBC_assign = &BBC%02d : %d : &IF_%s;\n" % (c, c, POLS[pol]))
        v.write("enddef;\n\n$IF;\ndef askapif;\n")
        for pol in range(npol):
            v.write("  if_def = &IF_%s : %s : %s : 2100.0 MHz : L;\n" % (POLS[pol], "AB"[pol], POLS[pol]))
        # vex2difx needs a track format and one fanout_def per recorded channel; the
        # v2d format (CODIFC/...) supplies the real bit depth and frame size
        v.write("enddef;\n\n$TRACKS;\ndef askaptracks;\n  track_frame_format = CODIF;\n")
        for c in range(npol * nfreq):
            v.write("  fanout_def = A : &CH%02d : sign : 1 : %02d;\n" % (c + 1, c))
        v.write("enddef;\n\n$SCHED;\nscan No0001;\n  start = %s;\n  mode = askap;\n  source = %s;\n"
                % (vstart, args.source))
        for code, _, _, _ in stations:
            v.write("  station = %s : 0 sec : %d sec : 0 ft : 1A : &ccw : 1;\n" % (code, dur))
        v.write("endscan;\n")

    fmt = "CODIFC/27/%d/%d" % (args.framebytes, args.bits)
    with open(args.name + ".v2d", "w") as v:
        v.write("# Written by tests/ASKAP/make_askap_job.py\nvex = %s.vex\n\n" % args.name)
        v.write("startSeries = 0\nminLength = 1\nallowAllClockOffsets = True\ntweakIntTime = True\n"
                "exhaustiveAutocorrs = True\nvisBufferLength = 8\nthreadsFile = %s.threads\n\n" % args.name)
        v.write("antennas = %s\n\n" % ", ".join(s[0] for s in stations))
        for code, name, a, codifs in stations:
            v.write("ANTENNA %s\n{\n  name = %s\n  clockOffset = %.6f\n  clockRate = 0\n  clockEpoch = 57000.0\n"
                    "  phaseCalInt = 0\n  toneSelection = none\n"
                    % (code, name, -a["delay_ns"] / 1000.0))
            if npol == 1:
                v.write("  format = %s\n  sampling = COMPLEX_DSB\n  file = %s\n}\n" % (fmt, codifs[0]))
            else:
                v.write("  datastreams = %s\n}\n" % ", ".join("%s-P%d" % (code, pol) for pol in range(npol)))
                for pol in range(npol):
                    v.write("DATASTREAM %s-P%d\n{\n  format = %s\n  sampling = COMPLEX_DSB\n  file = %s\n}\n"
                            % (code, pol, fmt, codifs[pol]))
        v.write("\nSETUP default\n{\n  tInt = %.6f\n  subintNS = %d\n  nFFTChan = %d\n  nChan = %d\n%s}\n\n"
                % (args.tint, args.subintns, args.nchan, args.nchan, "  doPolar = True\n" if npol == 2 else ""))
        v.write("RULE default\n{\n  setup = default\n}\n\nSOURCE %s { }\n\n" % args.source)
        with open(args.eop) as e:
            for line in e:
                if line.startswith("EOP") or line.startswith("# EOP"):
                    v.write(line)

    print("%d antennas x %d pol, %d coarse channels (%s), %s .. %s (%d s)"
          % (len(stations), npol, nfreq, sb, vstart, vstop, dur))


if __name__ == "__main__":
    main()
