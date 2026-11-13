#!/usr/bin/env python3
"""
plotwt.py - Python port of the NRAO SniffPlots program PLOTWT.

Plots data weights versus time for every station: for each averaging
interval the average weight over all baseband channels is drawn as a point,
with a bar from the lowest to the highest weight.  Up to 12 stations are
drawn per page.

The input is either
  * a summary file (first line "PLOTWT summary: <EXP>"), which is what
    difx2fits writes as <job>.wts and difxsniff merges into wtsfile.lis.sum.
    Each line has the average, minimum and maximum weight for each channel:
        MJD  UT_hours  staId  sta  nChan  avg1..avgN  min1..minN  max1..maxN
  * or an original sniffer weight file (any other first line), with one
    weight per channel:
        MJD  UT_hours  staId  sta  nChan  wt1..wtN
    For this kind of input a summary file <input>.sum is also written, which
    can itself be plotted later.

Unlike the Fortran original, this version never prompts: everything is set
with command line options.  See "plotwt.py --help".
"""

import argparse
import datetime
import getpass
import io
import math
import os
import sys

try:
    import numpy as np
    import matplotlib
except ImportError as exc:
    sys.exit('plotwt.py needs numpy and matplotlib (%s).  Install them with, '
             'for example:  python3 -m pip install --user numpy matplotlib' % exc)

F32 = np.float32
TMAX_ALL = 1.0e10        # "No upper time limit" flag value used by the Fortran.
MAX_PER_PAGE = 12        # Stations per page.

# Page layout (normalised page coordinates), from pltwt.f.
LEFT, RIGHT, BOTTOM, TOP = 0.07, 0.98, 0.07, 0.92

INTERACTIVE_DEVICES = {'x', 'xs', 'xw', 'xwin', 'xwindow', 'xserve'}
PS_DEVICES = {'ps', 'vps', 'cps', 'vcps'}
LANDSCAPE_DEVICES = {'ps', 'cps'}
RASTER_DEVICES = {'png', 'tpng', 'gif', 'vgif'}
KNOWN_DEVICES = INTERACTIVE_DEVICES | PS_DEVICES | RASTER_DEVICES | {'pdf'}


def warn(msg):
    print(msg, file=sys.stderr)


def getnum(word, lineno):
    """rcwlib GETNUM: bad numbers are reported and read as 0."""
    try:
        v = float(word)
    except ValueError:
        warn(' GETNUM: cannot decode "%s" on line %d.  Using 0.' % (word, lineno))
        return 0.0
    if math.isnan(v):
        warn(' GETNUM: NaN found on line %d.  Return 0.' % lineno)
        return 0.0
    return v


def fI(v, w):
    s = '%d' % v
    return s.rjust(w) if len(s) <= w else '*' * w


def fF(v, w, d):
    s = '%.*f' % (d, v)
    return s.rjust(w) if len(s) <= w else '*' * w


# ----------------------------------------------------------------------------
# Reading the data (GETWT)
# ----------------------------------------------------------------------------
class Cell:
    """Accumulator for one station in one averaging interval."""
    __slots__ = ('n', 'time', 'avg', 'mn', 'mx', 'cn', 'ctime', 'cavg', 'cmin', 'cmax')

    def __init__(self, nchan_max):
        self.n = 0
        self.time = F32(0.0)
        self.avg = F32(0.0)
        self.mn = F32(99.0)
        self.mx = F32(-1.0)
        # Per channel values, only used when writing a summary file.
        self.cn = None


def read_weights(path, tmin_req, tmax_req, tavg, gotsum, write_sum):
    """Read the weights file.  Mirrors GETWT, including its single precision
    arithmetic, so the summary file it writes matches the Fortran's.

    Returns a dict with the stations (in order of first appearance), the
    averaged points, the plot time range, and the summary records."""
    tavg = F32(tavg)
    stations = []
    index = {}
    cells = {}            # (it, is) -> Cell
    ntime = 0
    nchmax = 0
    nchan = 0
    jday1 = None
    tmin = None
    keep1 = True

    with open(path, 'r', errors='replace') as f:
        f.readline()                      # header, already examined
        lineno = 1
        for line in f:
            lineno += 1
            words = line.split()
            if len(words) <= 3:
                continue                  # blank line or a header of a concatenated file
            jday = int(getnum(words[0], lineno))
            sec = F32(getnum(words[1], lineno) * 3600.0)
            csta = words[3][:8]
            if jday1 is None:
                jday1 = jday
            sec = F32((jday - jday1) * 24.0 * 3600.0 + float(sec))

            if sec < F32(tmin_req):
                continue
            if sec > F32(tmax_req):
                break                     # data assumed to be in time order

            nchan = int(getnum(words[4], lineno)) if len(words) > 4 else 0
            if (gotsum and len(words) != 3 * nchan + 5) or \
               (not gotsum and len(words) != nchan + 5):
                warn(' Number of channels and number of values don\'t match for: ')
                warn(' Day, time: %d %.5f  at %s' % (jday, sec / 3600.0, csta))
                continue

            if keep1:
                keep1 = False
                tmin = F32(sec - tavg / F32(2.0)) if tmin_req == 0.0 else F32(tmin_req)

            if csta not in index:
                index[csta] = len(stations)
                stations.append(csta)
            ista = index[csta]

            it = int(F32(F32(sec - tmin) / tavg) + F32(1.0))   # Fortran INT truncates
            if it <= 0:
                if float(sec) - float(tmin) > -30.0:
                    continue
                warn(' Bad time (out of order?): %d' % it)
                warn(' If data file is not in time order, give start (non-zero) '
                     'and stop times with --timerange.')
                break
            ntime = max(ntime, it)

            nchmax = max(nchan, nchmax)
            wt = [F32(getnum(words[i + 5], lineno)) for i in range(nchan)]
            if gotsum:
                low = [F32(getnum(words[i + nchan + 5], lineno)) for i in range(nchan)]
                high = [F32(getnum(words[i + 2 * nchan + 5], lineno)) for i in range(nchan)]

            cell = cells.get((it, ista))
            if cell is None:
                cell = cells[(it, ista)] = Cell(nchan)
            if write_sum and cell.cn is None:
                cell.cn = {}
            for ic in range(nchan):
                cell.n += 1
                cell.time = F32(cell.time + sec)
                cell.avg = F32(cell.avg + wt[ic])
                if gotsum:
                    cell.mn = min(cell.mn, low[ic])
                    cell.mx = max(cell.mx, high[ic])
                else:
                    cell.mn = min(cell.mn, wt[ic])
                    cell.mx = max(cell.mx, wt[ic])
                    if cell.cn is not None:
                        c = cell.cn.get(ic)
                        if c is None:
                            c = cell.cn[ic] = [0, F32(0.0), F32(0.0), F32(99.0), F32(-1.0)]
                        c[0] += 1
                        c[1] = F32(c[1] + sec)
                        c[2] = F32(c[2] + wt[ic])
                        c[3] = min(c[3], wt[ic])
                        c[4] = max(c[4], wt[ic])

    if jday1 is None or not cells:
        return None

    # Normalise, in the Fortran's order (time cell, then station), and build
    # the summary records.
    tmin = F32(99.0e9)
    tmax = F32(-99.0e9)
    nsav = 0
    summary = []
    points = {i: [] for i in range(len(stations))}
    for it in range(1, ntime + 1):
        for ista in range(len(stations)):
            cell = cells.get((it, ista))
            if cell is None or cell.n == 0:
                continue
            if cell.n != nchan and gotsum:
                nsav += 1
            t = F32(cell.time / F32(cell.n))
            avg = F32(cell.avg / F32(cell.n))
            tmin = min(tmin, t)
            tmax = max(tmax, t)
            points[ista].append((t, avg, cell.mn, cell.mx))
            if write_sum:
                cavg, cmin, cmax = [], [], []
                for ic in range(nchan):
                    c = cell.cn.get(ic) if cell.cn else None
                    if c is not None and c[0] > 0:
                        cavg.append(F32(c[2] / F32(c[0])))
                        cmin.append(c[3])
                        cmax.append(c[4])
                    else:
                        cavg.append(F32(0.0))
                        cmin.append(F32(99.0) if c is None else c[3])
                        cmax.append(F32(-1.0) if c is None else c[4])
                jday = jday1 + int(F32(t / F32(86400.0)))
                hr = F32(F32(math.fmod(t, F32(86400.0))) / F32(3600.0))
                summary.append(fI(jday, 5) + fF(hr, 9, 5) + fI(ista + 1, 3) + ' ' +
                               stations[ista][:8].ljust(8) + fI(nchan, 3) +
                               ''.join(fF(v, 7, 3) for v in cavg + cmin + cmax))

    if nsav > 0 and gotsum:
        print(' %d output points represent an average of more than one '
              'summary input point' % nsav)

    if tmin == tmax:
        tmin, tmax = F32(tmin - F32(1800.0)), F32(tmax + F32(1800.0))
    else:
        tpad = F32((tmax - tmin) * F32(0.02))
        tmin, tmax = F32(tmin - tpad), F32(tmax + tpad)

    return dict(stations=stations, points=points, jday1=jday1,
                tmin=float(tmin), tmax=float(tmax), summary=summary)


# ----------------------------------------------------------------------------
# Plotting (PLTWT)
# ----------------------------------------------------------------------------
def mjd_to_jdate(mjd):
    """rcwlib JDATE: MJD -> e.g. 1994oct25."""
    d = datetime.date(1858, 11, 17) + datetime.timedelta(days=int(mjd))
    return '%04d%s%02d' % (d.year, d.strftime('%b').lower(), d.day)


def time_axis(tmin, tmax):
    """Tick locator and hh:mm (or d/hh:mm) formatter for seconds since 0h
    UT on the first day."""
    from matplotlib.ticker import FixedLocator, FuncFormatter
    span = max(tmax - tmin, 1.0)
    steps = [1, 2, 5, 10, 15, 20, 30, 60, 120, 300, 600, 900, 1200, 1800,
             3600, 7200, 10800, 14400, 21600, 43200, 86400, 172800, 432000]
    step = steps[-1]
    for s in steps:
        if span / s <= 8:
            step = s
            break
    ticks = np.arange(math.ceil(tmin / step) * step, tmax + 0.5 * step, step)
    show_days = tmax >= 86400.0

    def fmt(x, pos=None):
        sign = '-' if x < 0 else ''
        x = int(round(abs(x)))
        d, r = divmod(x, 86400)
        h, r = divmod(r, 3600)
        m, s = divmod(r, 60)
        txt = '%02d:%02d' % (h, m) + (':%02d' % s if step < 60 else '')
        return sign + ('%d/%s' % (d, txt) if show_days else txt)

    return FixedLocator(ticks), FuncFormatter(fmt)


def page_iden(fig, base_pt):
    """Equivalent of PGIDEN: user name and date at the bottom right."""
    try:
        user = getpass.getuser()
    except Exception:
        user = ''
    stamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')
    fig.text(0.99, 0.003, ('%s %s' % (user, stamp)).strip(), ha='right',
             va='bottom', fontsize=0.8 * base_pt)


def plot_page(fig, data, st1, st2, expname, base_pt):
    """One page of station panels, stations st1..st2 (0 based, inclusive)."""
    from matplotlib.ticker import AutoMinorLocator, MultipleLocator
    np_ = st2 - st1 + 1
    pheight = (TOP - BOTTOM) / np_
    tmin, tmax = data['tmin'], data['tmax']
    loc, fmt = time_axis(tmin, tmax)
    jdate = mjd_to_jdate(data['jday1'])
    fig.text(0.5, TOP + 0.02, 'WEIGHTS     %s     %s' % (expname.strip(), jdate),
             ha='center', va='bottom', fontsize=1.2 * base_pt)
    mid = (st1 + st2) // 2
    for ista in range(st1, st2 + 1):
        pbottom = TOP - (ista - st1 + 1) * pheight
        ax = fig.add_axes([LEFT, pbottom, RIGHT - LEFT, pheight])
        ax.set_xlim(tmin, tmax)
        ax.set_ylim(0.0, 1.25)
        ax.xaxis.set_major_locator(loc)
        ax.xaxis.set_major_formatter(fmt)
        ax.xaxis.set_minor_locator(AutoMinorLocator())
        ax.yaxis.set_major_locator(MultipleLocator(0.5))
        ax.yaxis.set_minor_locator(AutoMinorLocator(5))
        ax.tick_params(which='both', direction='in', top=True, right=True,
                       labelsize=0.8 * base_pt)
        if ista != st2:
            ax.tick_params(labelbottom=False)
        else:
            ax.set_xlabel('Time', fontsize=base_pt)
        if ista == mid:
            ax.set_ylabel('Weight', fontsize=base_pt)
        ax.text(tmin + (tmax - tmin) / 30.0, 1.08, data['stations'][ista],
                fontsize=0.8 * base_pt, va='baseline')
        pts = data['points'][ista]
        if pts:
            t = np.array([p[0] for p in pts], dtype=float)
            avg = np.array([p[1] for p in pts], dtype=float)
            lo = np.array([p[2] for p in pts], dtype=float)
            hi = np.array([p[3] for p in pts], dtype=float)
            ax.vlines(t, lo, hi, colors='black', linewidth=0.5)
            ax.plot(t, avg, linestyle='none', marker='o', markersize=1.8,
                    markerfacecolor='none', markeredgecolor='black',
                    markeredgewidth=0.5)


# ----------------------------------------------------------------------------
# Output handling (shared with the other SniffPlots ports)
# ----------------------------------------------------------------------------
def parse_plot_target(spec):
    """Accept a plain file name or a PGPLOT style "name/DEV" specification.
    Returns (filename or None, interactive, landscape)."""
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


def merge_ps_pages(pages, out):
    """Combine single-page PostScript documents written by matplotlib into
    one multi-page DSC document.  Each page keeps its own prolog (fonts are
    subsetted per page) inside save/restore."""
    header = []
    for line in pages[0].splitlines():
        if line.startswith('%%EndComments'):
            break
        if line.startswith('%%Pages:'):
            line = '%%%%Pages: %d' % len(pages)
        elif line.startswith('%%Title:'):
            line = '%%Title: plotwt'
        header.append(line)
    out.write('\n'.join(header) + '\n%%EndComments\n')
    for n, page in enumerate(pages, 1):
        lines = page.splitlines()
        start = next(i for i, l in enumerate(lines) if l.startswith('%%EndComments')) + 1
        body = [l for l in lines[start:] if not l.startswith(('%%', '%!'))]
        # matplotlib's page code leaves values on the operand stack, so keep
        # the save object in a name and clear the stacks before restoring.
        out.write('%%%%Page: %d %d\n/plotwt_save save def\n' % (n, n))
        out.write('\n'.join(body))
        out.write('\nclear cleardictstack\nplotwt_save restore\n')
    out.write('%%Trailer\n%%EOF\n')


class PageWriter:
    """Multi-page output: PDF and PostScript as one multi-page file, other
    formats (png, svg, eps, ...) as one file per page."""

    def __init__(self, filename, landscape):
        self.filename = filename
        self.landscape = landscape
        ext = os.path.splitext(filename)[1].lower()
        self.npages = 0
        if ext in ('.pdf', ''):
            from matplotlib.backends.backend_pdf import PdfPages
            self.mode = 'pdf'
            self.pdf = PdfPages(filename)
        elif ext == '.ps':
            self.mode = 'ps'
            self.pages = []
        else:
            self.mode = 'single'
            self.stem, self.ext = os.path.splitext(filename)
            self.written = []

    def save(self, fig):
        self.npages += 1
        if self.mode == 'pdf':
            self.pdf.savefig(fig)
        elif self.mode == 'ps':
            buf = io.StringIO()
            fig.savefig(buf, format='ps', papertype='letter',
                        orientation='landscape' if self.landscape else 'portrait')
            self.pages.append(buf.getvalue())
        else:
            name = '%s_p%02d%s' % (self.stem, self.npages, self.ext)
            fig.savefig(name, dpi=150)
            self.written.append(name)

    def close(self):
        if self.mode == 'pdf':
            self.pdf.close()
        elif self.mode == 'ps':
            with open(self.filename, 'w') as out:
                merge_ps_pages(self.pages, out)
        else:
            if len(self.written) == 1:
                os.replace(self.written[0], self.filename)
                self.written = [self.filename]
            return ', '.join(self.written)
        return self.filename


# ----------------------------------------------------------------------------
# Command line
# ----------------------------------------------------------------------------
def parse_timerange(values):
    """Up to 8 integers: start dd hh mm ss, stop dd hh mm ss (day 0 is the
    first day).  Missing values are 0; a stop of 0 means no upper limit."""
    if not values:
        return 0.0, TMAX_ALL
    if len(values) > 8:
        raise SystemExit('--timerange takes at most 8 numbers')
    try:
        tin = [int(v) for v in values] + [0] * (8 - len(values))
    except ValueError:
        raise SystemExit('Bad --timerange value: %s' % ' '.join(values))
    tmin = tin[0] * 86400 + tin[1] * 3600 + tin[2] * 60 + tin[3]
    tmax = tin[4] * 86400 + tin[5] * 3600 + tin[6] * 60 + tin[7]
    return float(tmin), (float(tmax) if tmax != 0 else TMAX_ALL)


def main(argv=None):
    p = argparse.ArgumentParser(
        description='Plot data weights versus time for each station '
                    '(Python port of PLOTWT).',
        epilog='Examples:\n'
               '  plotwt.py wtsfile.lis.sum wtsfile.ps/vps\n'
               '  plotwt.py bm14.wts bm14_wts.pdf -t 0 10 0 0 0 12 0 0 -a 60\n',
        formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('wtfile', help='weights file: a summary (.wts from difx2fits, '
                                  'wtsfile.lis.sum from difxsniff) or an original '
                                  'sniffer weights file')
    p.add_argument('plotfile', nargs='?',
                   help='output plot file.  The extension sets the format '
                        '(.pdf, .ps, .png, .svg, ...; multi-page for .pdf/.ps, '
                        'one file per page otherwise).  A PGPLOT style device '
                        'suffix is also accepted, e.g. name.ps/vps (portrait), '
                        'name.ps/ps (landscape), or /xs to display on screen.  '
                        'Default: <wtfile>_wts.pdf')
    p.add_argument('-t', '--timerange', nargs='+', metavar='N',
                   help='time range as up to 8 integers: start dd hh mm ss '
                        'stop dd hh mm ss, where the first day is day 0 '
                        '(default: everything)')
    p.add_argument('-a', '--average', type=float, default=30.0, metavar='SEC',
                   help='averaging time for each plotted point in seconds '
                        '(default 30)')
    p.add_argument('-s', '--summary-file',
                   help='summary file to write when the input is not already a '
                        'summary (default <wtfile>.sum)')
    args = p.parse_args(argv)
    if args.average <= 0.0:
        raise SystemExit('--average must be positive')

    tmin_req, tmax_req = parse_timerange(args.timerange)
    if args.plotfile is None:
        args.plotfile = os.path.splitext(os.path.basename(args.wtfile))[0] + '_wts.pdf'
    plotname, interactive, landscape = parse_plot_target(args.plotfile)

    if not os.path.isfile(args.wtfile):
        raise SystemExit(' File not found: %s' % args.wtfile)
    with open(args.wtfile, 'r', errors='replace') as f:
        header = f.readline()
        if not header.strip() and not any(line.strip() for line in f):
            raise SystemExit('Input file %s is empty - nothing to plot.' % args.wtfile)
    hw = header.split()
    expname = (hw[-1] if hw else '')[:8].ljust(8)
    gotsum = len(hw) > 1 and hw[1] == 'summary:'
    sumfile = None if gotsum else (args.summary_file or args.wtfile + '.sum')

    for path in ([] if interactive else [plotname]) + ([sumfile] if sumfile else []):
        outdir = os.path.dirname(path)
        if outdir and not os.path.isdir(outdir):
            raise SystemExit('Output directory does not exist: %s' % outdir)

    if not interactive:
        matplotlib.use('Agg')
    import matplotlib.pyplot as plt

    data = read_weights(args.wtfile, tmin_req, tmax_req, args.average, gotsum,
                        write_sum=sumfile is not None)
    if data is None:
        raise SystemExit('Input file %s contains no weight data%s - nothing to plot.'
                         % (args.wtfile, ' in the requested time range'
                            if args.timerange else ''))

    if sumfile:
        with open(sumfile, 'w') as out:
            out.write('PLOTWT summary: %s\n' % expname)
            for line in data['summary']:
                out.write(line + '\n')
        print('Summary written to %s' % sumfile)

    # Split reasonably evenly between pages with up to 12 per page.
    nsta = len(data['stations'])
    npage = (nsta - 1) // MAX_PER_PAGE + 1
    nperpage = (nsta + npage - 1) // npage

    figsize = (10.5, 8.0) if landscape else (8.0, 10.5)
    if interactive:
        figsize = (11.0, 8.5)
    # PGPLOT's character height is 1/40 of the page height; matplotlib's
    # font is wider, so scale it down to keep the labels inside the page.
    base_pt = figsize[1] / 40.0 * 72.0 * 0.7

    writer = None if interactive else PageWriter(plotname, landscape)
    for ip in range(npage):
        st1 = ip * nperpage
        st2 = min(st1 + nperpage, nsta) - 1
        fig = plt.figure(figsize=figsize)
        plot_page(fig, data, st1, st2, expname, base_pt)
        page_iden(fig, base_pt)
        if interactive:
            plt.show()
        else:
            writer.save(fig)
        plt.close(fig)

    if writer is not None:
        where = writer.close()
        print('Plotted %d station%s on %d page%s: %s'
              % (nsta, '' if nsta == 1 else 's', writer.npages,
                 '' if writer.npages == 1 else 's', where))
    return 0


if __name__ == '__main__':
    sys.exit(main())
