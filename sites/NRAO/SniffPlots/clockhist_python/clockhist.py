#!/usr/bin/env python3
"""
clockhist.py - Python port of the NRAO SniffPlots program CLOCKHIST.

Builds a history of station clock offsets from many sniffer runs.  For each
datafile.lis file (the amplitude/phase/delay files merged by difxsniff)
listed in FILELIST it finds, for every baseline, the delay of the point with
the highest amplitude.  The results are kept in an accumulation file so that
files already processed are not read again.  All delays are then referred
to one reference station and plotted against time for the chosen stations.

Accumulation file format:
    File: /path/to/datafile.lis
    SOURCE      S1 S2   TIME(MJD-50000)  MAXAMP  DELAY(ns)  RATE(mHz)
    ...

Unlike the Fortran original, this version never prompts: everything is set
with command line options.  See "clockhist.py --help".
"""

import argparse
import datetime
import getpass
import math
import os
import sys

try:
    import numpy as np
    import matplotlib
except ImportError as exc:
    sys.exit('clockhist.py needs numpy and matplotlib (%s).  Install them with, '
             'for example:  python3 -m pip install --user numpy matplotlib' % exc)

F32 = np.float32
# Stations known to CLOCKHIST (the VLBA and the VLA), in the original order.
NAMES = ['SC', 'HN', 'NL', 'FD', 'LA', 'PT', 'KP', 'OV', 'BR', 'MK', 'Y']
MAXPLT = 10

INTERACTIVE_DEVICES = {'x', 'xs', 'xw', 'xwin', 'xwindow', 'xserve'}
PS_DEVICES = {'ps', 'vps', 'cps', 'vcps'}
LANDSCAPE_DEVICES = {'ps', 'cps'}
RASTER_DEVICES = {'png', 'tpng', 'gif', 'vgif'}
KNOWN_DEVICES = INTERACTIVE_DEVICES | PS_DEVICES | RASTER_DEVICES | {'pdf'}

# PGPLOT marker numbers 3..12 used for plot stations 1..10.
PGMARKERS = {3: ('*', True), 4: ('o', False), 5: ('x', True), 6: ('s', False),
             7: ('^', False), 8: ('P', False), 9: ('o', True), 10: ('D', False),
             11: ('d', False), 12: ('*', False)}


def warn(msg):
    print(msg, file=sys.stderr)


def which(name):
    """Index (1 based) of a station in NAMES, 0 if unknown."""
    name = name.strip()
    return NAMES.index(name) + 1 if name in NAMES else 0


def getnum(word, where):
    try:
        v = float(word)
    except ValueError:
        warn(' GETNUM: cannot decode "%s" in %s.  Using 0.' % (word, where))
        return 0.0
    return 0.0 if math.isnan(v) else v


def fF(v, w, d):
    s = '%.*f' % (d, v)
    return s.rjust(w) if len(s) <= w else '*' * w


class Experiment:
    def __init__(self, path):
        self.path = path
        self.bas = []        # list of dicts: source, s1, s2, time, amp, delay, rate
        self.nref = 0


# ----------------------------------------------------------------------------
# Reading
# ----------------------------------------------------------------------------
def read_accum(path):
    exps = []
    if not os.path.exists(path):
        return exps
    with open(path, 'r', errors='replace') as f:
        for line in f:
            w = line.split()
            if not w:
                print(' There is a bad format line in %s' % path)
                print(line.rstrip())
                continue
            if w[0] == 'File:':
                exps.append(Experiment(w[1] if len(w) > 1 else ''))
            elif len(w) == 7 and exps:
                exps[-1].bas.append(dict(
                    source=w[0][:12], s1=which(w[1]), s2=which(w[2]),
                    time=F32(getnum(w[3], path)), amp=F32(getnum(w[4], path)),
                    delay=F32(getnum(w[5], path)), rate=F32(getnum(w[6], path))))
            else:
                print(' There is a bad format line in %s' % path)
                print(line.rstrip())
    return exps


def read_datafile(exp):
    """GETCLK: for each baseline, the delay of the highest amplitude point."""
    try:
        f = open(exp.path, 'r', errors='replace')
    except OSError:
        return False
    with f:
        header = f.readline()
        if header == '':
            print(' Input file %s is empty - skipped.' % exp.path)
            return True
        first = True
        nskip = 0
        index = {}
        for line in f:
            w = line.split()
            if not w:
                continue
            if first:
                first = False
                if len(w) > 4 and which(w[4]):
                    print(' Old datafile.lis format in file: %s' % exp.path)
                    return True
            st1 = which(w[6]) if len(w) > 6 else 0
            st2 = which(w[7]) if len(w) > 7 else 0
            if not (st1 and st2):
                if nskip <= 10:
                    print('  Skipped %s %s%s' % ((w[6] if len(w) > 6 else '')[:2].ljust(2),
                                                 (w[7] if len(w) > 7 else '')[:2].ljust(2),
                                                 exp.path))
                    nskip += 1
                continue
            nchan = int(getnum(w[8], exp.path)) if len(w) > 8 else 0
            key = (st1, st2)
            if key not in index:
                index[key] = len(exp.bas)
                exp.bas.append(dict(source='', s1=st1, s2=st2, time=F32(0.0),
                                    amp=F32(-1.0e4), delay=F32(0.0), rate=F32(0.0)))
            b = exp.bas[index[key]]
            for i in range(1, nchan + 1):
                iamp = 7 + 4 * i                      # 1 based word number
                if iamp + 2 > len(w):
                    break
                amp = F32(getnum(w[iamp - 1], exp.path))
                if amp > b['amp']:
                    b['amp'] = amp
                    b['source'] = w[3][:12]
                    b['delay'] = F32(getnum(w[iamp - 2], exp.path))
                    b['rate'] = F32(getnum(w[iamp + 1], exp.path) * 1000.0)
                    b['time'] = F32(getnum(w[0], exp.path) - 50000 +
                                    getnum(w[1], exp.path) / 24.0)
    return True


def accum_line(b):
    """'(A, T13, A, T16, A, T20, F12.5, F10.5, F9.1, F9.1)' with Fortran tab
    (overwrite) semantics."""
    buf = []

    def put(col, text):
        while len(buf) < col - 1:
            buf.append(' ')
        for k, ch in enumerate(text):
            pos = col - 1 + k
            if pos < len(buf):
                buf[pos] = ch
            else:
                buf.append(ch)
    put(1, b['source'].strip())
    put(13, NAMES[b['s1'] - 1])
    put(16, NAMES[b['s2'] - 1])
    put(20, fF(b['time'], 12, 5) + fF(b['amp'], 10, 5) + fF(b['delay'], 9, 1) +
        fF(b['rate'], 9, 1))
    return ''.join(buf)


def get_data(filelist, accfile):
    """GETDAT: previously accumulated data plus any new files in the list."""
    exps = []
    if accfile:
        print(' Opening accumulated data file: %s' % accfile)
        exps = read_accum(accfile)
    nacc = len(exps)
    print(' Getting file list from: %s' % filelist)
    known = {e.path for e in exps}
    with open(filelist, 'r', errors='replace') as f:
        for line in f:
            infile = line.rstrip('\n').strip()
            if not infile:
                continue
            if infile.startswith('/nfs'):
                infile = infile[4:]
            if infile in known:
                continue
            exp = Experiment(infile)
            print('%12d %s' % (len(exps) + 1, infile))
            if not read_datafile(exp):
                print(' Could not open file: %s' % infile)
                continue
            exps.append(exp)
            known.add(infile)
    if accfile:
        if len(exps) > nacc:
            with open(accfile, 'a') as out:
                for exp in exps[nacc:]:
                    out.write('File: %s\n' % exp.path)
                    for b in exp.bas:
                        if b['amp'] > 0.0:
                            out.write(accum_line(b) + '\n')
            print(' Done adding data to accum file. %11d' % len(exps))
        else:
            print(' All requested data was in accum file. %11d' % len(exps))
    return exps


# ----------------------------------------------------------------------------
# Re-referencing (REFSET) and plotting (PLTCLK)
# ----------------------------------------------------------------------------
def refset(exps, iref):
    for exp in exps:
        exp.nref = len(exp.bas)
        if exp.bas and exp.bas[0]['s1'] != iref:
            irb = next((j for j, b in enumerate(exp.bas) if b['s2'] == iref), None)
            if irb is None:
                exp.nref = 0
                print(' Reference station not present for %s' % exp.path)
            else:
                rdelay = exp.bas[irb]['delay']
                for j, b in enumerate(exp.bas[:exp.nref]):
                    if j == irb:
                        b['delay'] = F32(-rdelay)
                        b['s2'] = b['s1']
                    else:
                        b['delay'] = F32(b['delay'] - rdelay)
                    b['s1'] = iref


def fixlim(lo, hi, lofr, hifr):
    rng = hi - lo
    if rng == 0.0:
        rng = 1.0
    return lo - rng * lofr, hi + rng * hifr


def parse_plot_target(spec):
    name, dev = spec, ''
    if '/' in spec:
        head, tail = spec.rsplit('/', 1)
        if tail.lower() in KNOWN_DEVICES:
            name, dev = head, tail.lower()
    interactive = dev in INTERACTIVE_DEVICES
    landscape = dev in LANDSCAPE_DEVICES
    if not interactive:
        if name == '':
            raise SystemExit('No plot file name given in "%s"' % spec)
        if dev in PS_DEVICES and os.path.splitext(name)[1] == '':
            name += '.ps'
        elif dev in RASTER_DEVICES and os.path.splitext(name)[1] == '':
            name += '.png'
    return (None if interactive else name), interactive, landscape


def main(argv=None):
    p = argparse.ArgumentParser(
        description='Plot the history of sniffer clock offsets relative to a '
                    'reference station (Python port of CLOCKHIST).',
        epilog='Examples:\n'
               '  clockhist.py files.lis clocks.ps/ps -A clock.acc -p FD PT KP\n'
               '  clockhist.py files.lis clocks.pdf -A clock.acc -r PT -p LA OV '
               '--tmin 9000 --tmax 9100\n'
               '\nKnown stations: ' + ' '.join(NAMES) + '\n',
        formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('filelist', help='file listing the datafile.lis files to use, one per line')
    p.add_argument('plotfile', nargs='?', default='clockhist.pdf',
                   help='output plot file (.pdf, .ps, .png, ...; a PGPLOT style '
                        'name.ps/ps or /xs is also accepted).  Default clockhist.pdf')
    p.add_argument('-A', '--accfile',
                   help='accumulation file: data already extracted is read from '
                        'it and new data is appended (created if needed)')
    p.add_argument('-r', '--refsta', default='LA', help='reference station (default LA)')
    p.add_argument('-p', '--plot-stations', nargs='+', metavar='STA',
                   help='stations to plot, up to %d (default: all known stations '
                        'except the reference)' % MAXPLT)
    p.add_argument('--tmin', type=float, default=0.0, help='plot start, MJD-50000')
    p.add_argument('--tmax', type=float, default=0.0, help='plot end, MJD-50000')
    p.add_argument('--ymin', type=float, default=0.0, help='plot minimum clock (ns)')
    p.add_argument('--ymax', type=float, default=0.0, help='plot maximum clock (ns)')
    p.add_argument('--minamp', type=float, default=1.0e-4,
                   help='ignore points with lower amplitude (default 1e-4)')
    p.add_argument('--maxrate', type=float, default=20.0,
                   help='ignore points with a larger absolute rate in mHz (default 20)')
    args = p.parse_args(argv)

    refsta = args.refsta.upper()
    iref = which(refsta)
    if iref == 0:
        raise SystemExit(' Invalid reference antenna %s (known: %s)' % (refsta, ' '.join(NAMES)))
    if args.plot_stations:
        plt_names = [s.upper() for s in args.plot_stations]
        bad = [s for s in plt_names if not which(s)]
        for s in bad:
            print(' Invalid plot station: %s' % s)
        plt_names = [s for s in plt_names if which(s)]
        if not plt_names:
            raise SystemExit(' Need antennas to plot ')
    else:
        plt_names = [n for n in NAMES if n != refsta]
    if len(plt_names) > MAXPLT:
        raise SystemExit('At most %d plot stations' % MAXPLT)
    pltsta = [which(s) for s in plt_names]

    plotname, interactive, landscape = parse_plot_target(args.plotfile)
    if not os.path.isfile(args.filelist):
        raise SystemExit(" Can't open datafiles file: %s" % args.filelist)
    with open(args.filelist, errors='replace') as f:
        if not f.read().strip():
            raise SystemExit('File list %s is empty - nothing to plot.' % args.filelist)
    for path in ([] if interactive else [plotname]) + ([args.accfile] if args.accfile else []):
        d = os.path.dirname(path)
        if d and not os.path.isdir(d):
            raise SystemExit('Output directory does not exist: %s' % d)

    exps = get_data(args.filelist, args.accfile)
    refset(exps, iref)

    uminamp, umaxrate = F32(args.minamp), F32(args.maxrate)
    pts = {jb: ([], []) for jb in range(len(pltsta))}
    xmin = ymin = 1.0e10
    xmax = ymax = -1.0e10
    for exp in exps:
        if exp.nref == 0:
            continue
        for jb, sta in enumerate(pltsta):
            for b in exp.bas[:exp.nref]:
                if sta == b['s2'] and b['amp'] > uminamp and abs(b['rate']) < umaxrate:
                    pts[jb][0].append(float(b['time']))
                    pts[jb][1].append(float(b['delay']))
                    xmin, xmax = min(xmin, b['time']), max(xmax, b['time'])
                    ymin, ymax = min(ymin, b['delay']), max(ymax, b['delay'])
    npts = sum(len(v[0]) for v in pts.values())
    if npts == 0:
        raise SystemExit('No clock data for stations %s referenced to %s - nothing to plot.'
                         % (' '.join(plt_names), refsta))
    if args.tmin != 0.0 or args.tmax != 0.0:
        xmin, xmax = args.tmin, args.tmax
    else:
        xmin, xmax = fixlim(float(F32(xmin)), float(F32(xmax)), 0.05, 0.05)
    if args.ymin != 0.0 or args.ymax != 0.0:
        ymin, ymax = args.ymin, args.ymax
    else:
        ymin, ymax = fixlim(float(F32(ymin)), float(F32(ymax)), 0.05, 0.05)

    if not interactive:
        matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots(figsize=(10.5, 8.0) if not interactive else (11, 8.5))
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)
    ax.axhline(0.0, color='black', linewidth=0.5)
    ax.axvline(0.0, color='black', linewidth=0.5)
    ax.tick_params(which='both', direction='in', top=True, right=True)
    ax.set_xlabel('Julian Day - 50000')
    ax.set_ylabel('Clock (ns)')
    ax.set_title('Sniffer Clocks - Referenced to ' + refsta)
    for jb, sta in enumerate(pltsta):
        x, y = pts[jb]
        if not x:
            continue
        marker, filled = PGMARKERS[jb + 3]
        kw = dict(linestyle='none', marker=marker, markersize=6, color='black')
        if not filled:
            kw['markerfacecolor'] = 'none'
        ax.plot(x, y, **kw)
        xpt = xmin + 0.05 * (xmax - xmin)
        ypt = ymax - 0.04 * (jb + 1) * (ymax - ymin)
        ax.plot([xpt], [ypt], **kw)
        ax.text(xpt, ypt - 0.01 * (ymax - ymin), ' ' + NAMES[sta - 1])
    try:
        user = getpass.getuser()
    except Exception:
        user = ''
    fig.text(0.99, 0.005, '%s %s' % (user, datetime.datetime.now().strftime('%Y-%m-%d %H:%M')),
             ha='right', va='bottom', fontsize=7)
    if interactive:
        plt.show()
    else:
        if os.path.splitext(plotname)[1].lower() == '.ps':
            fig.savefig(plotname, format='ps', papertype='letter',
                        orientation='landscape' if landscape else 'portrait')
        else:
            fig.savefig(plotname)
        print('Plotted %d points: %s' % (npts, plotname))
    return 0


if __name__ == '__main__':
    sys.exit(main())
