#!/usr/bin/env python3
"""
plotapd.py - Python port of the NRAO SniffPlots program PLOTAPD.

Reads a sniffer amplitude/phase/delay/rate file (a ".apd" file written by
difx2fits, or the merged "datafile.lis" written by difxsniff) and

  * plots amplitude, phase, delay and rate versus time for every baseline,
    one baseline per panel (3 panels per page for file output), and
  * writes a text table (default "apd.delays") giving, for each baseline and
    baseband channel, the delay and rate of the point with the most close
    neighbours, plus a weighted average over the selected channels.

Unlike the Fortran original, this version never prompts: everything is set
with command line options.  See "plotapd.py --help".

Input format (one record per line, whitespace separated):
    line 1:  obscode:  <EXPNAME>
    data:    MJD  UT_hours  srcId  srcName  ant1Id  ant2Id  ant1  ant2  nBBC
             followed by nBBC groups of:  delay(ns) amp phase(deg) rate(Hz)
Lines with exactly 2 words (extra headers in concatenated files) are skipped.
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
    sys.exit('plotapd.py needs numpy and matplotlib (%s).  Install them with, '
             'for example:  python3 -m pip install --user numpy matplotlib' % exc)

# ----------------------------------------------------------------------------
# Constants carried over from the Fortran (plotapd.inc / getapd.f / pltapd.f /
# hiwt.f).
# ----------------------------------------------------------------------------
DMINR = 50.0        # Minimum delay plot range (ns).
RMINR = 15.0        # Minimum rate plot range (mHz).
MLABEL = 60         # Max number of source names to write on a rate plot.
MTLAB = 120.0       # Relabel interval for the same source (s).
DELSIG = 5.0        # HIWT: gaussian width for delay (ns).
RATSIG = 2.0        # HIWT: gaussian width for rate (mHz).
TMAX_ALL = 1.0e10   # "No upper time limit" flag value used by the Fortran.

# Panel geometry within each baseline sub-page (normalised sub-page coords).
LEFT, RIGHT = 0.1, 0.98
BOTTOM = (0.1, 0.35, 0.5, 0.7)   # amp, phase, delay, rate
TOP = (0.35, 0.5, 0.7, 0.9)

# PGPLOT devices that may be given as a "/DEV" suffix on the plot file name.
INTERACTIVE_DEVICES = {'x', 'xs', 'xw', 'xwin', 'xwindow', 'xserve'}
PS_DEVICES = {'ps', 'vps', 'cps', 'vcps'}
LANDSCAPE_DEVICES = {'ps', 'cps'}
RASTER_DEVICES = {'png', 'tpng', 'gif', 'vgif'}
KNOWN_DEVICES = INTERACTIVE_DEVICES | PS_DEVICES | RASTER_DEVICES | {'pdf'}

# PGPLOT symbol numbers used by PLTAPD, mapped to matplotlib markers:
#   (marker, filled)
PGSYM = {
    0: ('s', False),    # open square (Fortran DSYM beyond 4 is 0)
    1: ('.', True),     # dot - used for "other" channels
    4: ('o', False),    # open circle
    5: ('x', True),     # cross
    7: ('^', False),    # open triangle
    17: ('o', True),    # filled circle
}
DSYM = {1: 17, 2: 4, 3: 5, 4: 7}   # default symbols for channels 1-4


def warn(msg):
    print(msg, file=sys.stderr)


# ----------------------------------------------------------------------------
# Reading the data (GETAPD / BASNUM)
# ----------------------------------------------------------------------------
class Baseline:
    """All data points for one baseline, in file order."""

    def __init__(self, ant_a, ant_b):
        self.ant_a = ant_a
        self.ant_b = ant_b
        self.time = []      # seconds from 0h UT on the first day (float32)
        self.source = []
        self.rows = []      # per point: (delay, amp, phase, rate) lists

    def finish(self, nchan):
        """Convert to (npts, nchan) float32 arrays.  Channels missing from a
        record get amp 0, which flags them as "do not plot" exactly like a
        bad solution in the original."""
        n = len(self.rows)
        self.time = np.array(self.time, dtype=np.float32)
        self.delay = np.zeros((n, nchan), dtype=np.float32)
        self.amp = np.zeros((n, nchan), dtype=np.float32)
        self.phase = np.zeros((n, nchan), dtype=np.float32)
        self.rate = np.zeros((n, nchan), dtype=np.float32)
        for i, (d, a, p, r) in enumerate(self.rows):
            k = len(d)
            self.delay[i, :k] = d
            self.amp[i, :k] = a
            self.phase[i, :k] = p
            self.rate[i, :k] = r
        del self.rows

    @property
    def npts(self):
        return len(self.time)


def getnum(word, lineno):
    """Equivalent of rcwlib GETNUM: bad numbers are reported and read as 0."""
    try:
        v = float(word)
    except ValueError:
        warn(' GETNUM: cannot decode "%s" on line %d.  Using 0.' % (word, lineno))
        return 0.0
    if math.isnan(v):
        warn(' GETNUM: NaN found on line %d.  Return 0.' % lineno)
        return 0.0
    return v


def read_apd(path, antennas, tmin_req, tmax_req):
    """Read the whole .apd file.

    Returns (expname, jday1, nchan, tmin, tmax, baselines) where baselines is
    a list in order of first appearance (as BASNUM numbered them).
    TMIN/TMAX are the data time limits over all requested-antenna baselines,
    before time-range selection, as in the Fortran.
    """
    want_all = any(a.upper() == 'ALL' for a in antennas)
    want = {a.upper() for a in antennas}

    with open(path, 'r', errors='replace') as f:
        header = f.readline()
        if header == '':
            raise SystemExit('Input file %s is empty - nothing to plot.' % path)
        hw = header.split()
        expname = hw[1][:8] if len(hw) > 1 else ''

        baselines = {}
        jday1 = None
        tmin = tmax = None
        nchan = 0
        nlines = 0
        any_data = False

        for line in f:
            nlines += 1
            words = line.split()
            if len(words) == 0 or len(words) == 2:
                continue            # blank line, or header of a concatenated file
            if len(words) < 9:
                warn(' Too few words (%d) on line %d - skipped' % (len(words), nlines))
                continue
            any_data = True

            ant_a = words[6][:8]
            ant_b = words[7][:8]
            if not (want_all or ant_a.upper() in want or ant_b.upper() in want):
                continue

            nwchan = (len(words) - 6) // 4
            if nwchan <= 0:
                warn(' Bad number of channels ( %d  on line %d' % (nwchan, nlines))
                continue

            jday = int(getnum(words[0], nlines))
            # Fortran holds times as REAL; mimic that so the printed hh mm ss
            # in apd.delays truncate the same way.
            rtime = np.float32(getnum(words[1], nlines) * 3600.0)
            ndchan = int(getnum(words[8], nlines))

            key = (ant_a, ant_b)
            if key not in baselines:
                baselines[key] = Baseline(ant_a, ant_b)
            bl = baselines[key]

            if jday1 is None:
                jday1 = jday
                tmin = tmax = rtime
            rtime = np.float32(np.float32((jday - jday1) * 24.0 * 3600.0) + rtime)
            tmax = max(tmax, rtime)
            tmin = min(tmin, rtime)

            if nwchan != ndchan:
                warn(' Number of data points does not match claimed number of '
                     'channels (Day, sec, ndat, nch) %d %.1f %d %d'
                     % (jday, rtime, nwchan, ndchan))
            nchan = max(nchan, nwchan)

            if tmin_req <= rtime < tmax_req:
                d, a, p, r = [], [], [], []
                for i in range(nwchan):
                    nd = 4 * i + 9
                    d.append(getnum(words[nd], nlines))
                    a.append(getnum(words[nd + 1], nlines) * 1.0e5)  # corr units
                    p.append(getnum(words[nd + 2], nlines))
                    r.append(getnum(words[nd + 3], nlines) * 1.0e3)  # mHz
                bl.time.append(rtime)
                bl.source.append(words[3][:12])
                bl.rows.append((d, a, p, r))

    if jday1 is None:
        if nlines == 0 or not any_data:
            raise SystemExit('Input file %s contains no data (only a header) - '
                             'nothing to plot.' % path)
        raise SystemExit(' No data for the requested antennas. ')

    bls = list(baselines.values())
    for bl in bls:
        bl.finish(nchan)
    return expname, jday1, nchan, float(tmin), float(tmax), bls


# ----------------------------------------------------------------------------
# Plot limits (end of GETAPD)
# ----------------------------------------------------------------------------
def fixlim(low, high, lowfr, highfr):
    """rcwlib FIXLIM: pad plot limits to leave an edge around the data."""
    rng = high - low
    if rng == 0.0:
        rng = 1.0
    return low - rng * lowfr, high + rng * highfr


def baseline_limits(bl, chans):
    """Amp/phase/delay/rate plot limits for one baseline using the requested
    channels and only points with amp > 0."""
    cols = [c - 1 for c in chans if c - 1 < bl.amp.shape[1]]
    if bl.npts > 0 and cols:
        a = bl.amp[:, cols]
        good = a > 0.0
    else:
        good = np.zeros((0, 0), dtype=bool)

    if good.any():
        amax = float(a[good].max())
        pmin = float(bl.phase[:, cols][good].min())
        pmax = float(bl.phase[:, cols][good].max())
        dmin = float(bl.delay[:, cols][good].min())
        dmax = float(bl.delay[:, cols][good].max())
        rmin = float(bl.rate[:, cols][good].min())
        rmax = float(bl.rate[:, cols][good].max())
    else:
        # No usable points: the Fortran would produce absurd axes here, so
        # use sensible empty-plot ranges instead.
        amax, pmin, pmax, dmin, dmax, rmin, rmax = 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0

    lim = {}
    lim['amp'] = fixlim(0.0, amax, 0.0, 0.1)
    lim['phase'] = (min(pmin, -185.0), max(pmax, 185.0))
    for name, lo, hi, minr in (('delay', dmin, dmax, DMINR), ('rate', rmin, rmax, RMINR)):
        lo, hi = fixlim(lo, hi, 0.1, 0.1)
        if hi - lo < minr:
            avg = (hi + lo) / 2.0
            lo, hi = avg - minr / 2.0, avg + minr / 2.0
        lim[name] = (lo, hi)
    return lim, bool(good.any())


# ----------------------------------------------------------------------------
# Plotting (PLTAPD)
# ----------------------------------------------------------------------------
def mjd_to_jdate(mjd):
    """rcwlib JDATE: MJD -> e.g. 1994oct25."""
    d = datetime.date(1858, 11, 17) + datetime.timedelta(days=int(mjd))
    return '%04d%s%02d' % (d.year, d.strftime('%b').lower(), d.day)


def time_locator_and_formatter(tmin, tmax):
    from matplotlib.ticker import FixedLocator, FuncFormatter, AutoMinorLocator
    span = max(tmax - tmin, 1.0)
    steps = [1, 2, 5, 10, 15, 20, 30, 60, 120, 300, 600, 900, 1200, 1800,
             3600, 7200, 10800, 14400, 21600, 43200, 86400, 172800, 432000]
    step = steps[-1]
    for s in steps:
        if span / s <= 7:
            step = s
            break
    first = math.ceil(tmin / step) * step
    ticks = np.arange(first, tmax + 0.5 * step, step)
    show_days = tmax >= 86400.0

    def fmt(x, pos=None):
        sign = '-' if x < 0 else ''
        x = int(round(abs(x)))
        d, r = divmod(x, 86400)
        h, r = divmod(r, 3600)
        m, s = divmod(r, 60)
        txt = '%02d:%02d' % (h, m)
        if step < 60:
            txt += ':%02d' % s
        if show_days:
            txt = '%d/%s' % (d, txt)
        return sign + txt

    return FixedLocator(ticks), FuncFormatter(fmt), AutoMinorLocator()


def channel_styles(nchan, first_chan, cht):
    """Symbol and size per channel, as in PLTAPD: the first requested channel
    is drawn twice as large; channels above 4 are dots."""
    cirht = 0.8 * cht
    styles = {}
    for ich in range(1, nchan + 1):
        if ich == first_chan:
            sym, size = DSYM.get(ich, 0), 2.0 * cirht
        elif ich > 4:
            sym, size = 1, cirht
        else:
            sym, size = DSYM[ich], cirht
        styles[ich] = (sym, size)
    return styles


def scatter_kwargs(sym, size, ptsize):
    marker, filled = PGSYM[sym]
    # PGPLOT markers are drawn at roughly half the character height.
    ms = size * ptsize * (0.8 if sym == 1 else 0.5)
    kw = dict(linestyle='none', marker=marker, markersize=ms, color='black',
              markeredgewidth=0.6)
    if not filled:
        kw['markerfacecolor'] = 'none'
    return kw


def plot_baseline(fig, slot, nslot, bl, lim, has_data, chans, nchan, styles,
                  tmin, tmax, expname, jdate, cht, base_pt):
    """Draw the 4 attached panels for one baseline into sub-page `slot`
    (0 = top) of a figure divided into `nslot` horizontal sub-pages."""
    from matplotlib.ticker import FixedLocator, MaxNLocator, AutoMinorLocator

    sub_h = 1.0 / nslot
    sub_y0 = 1.0 - (slot + 1) * sub_h
    loc, fmt, _ = time_locator_and_formatter(tmin, tmax)

    def axes(i):
        return fig.add_axes([LEFT, sub_y0 + BOTTOM[i] * sub_h,
                             RIGHT - LEFT, (TOP[i] - BOTTOM[i]) * sub_h])

    ax_amp, ax_pha, ax_del, ax_rat = axes(0), axes(1), axes(2), axes(3)
    panels = ((ax_rat, bl.rate if has_data else None, 'rate', 'Rate (mHz)'),
              (ax_del, bl.delay if has_data else None, 'delay', 'Delay (ns)'),
              (ax_pha, bl.phase if has_data else None, 'phase', 'Phase (deg)'),
              (ax_amp, bl.amp if has_data else None, 'amp', r'Amplitude $\times\,10^5$'))

    label_pt = 1.2 * base_pt
    # The phase panel is short, so the y labels are kept small enough not to
    # run into each other.
    ylabel_pt = 0.95 * base_pt
    for ax, data, key, ylabel in panels:
        ax.set_xlim(tmin, tmax)
        ax.set_ylim(*lim[key])
        ax.xaxis.set_major_locator(loc)
        ax.xaxis.set_major_formatter(fmt)
        ax.xaxis.set_minor_locator(AutoMinorLocator())
        ax.tick_params(which='both', direction='in', top=True, right=True,
                       labelsize=base_pt)
        ax.set_ylabel(ylabel, fontsize=ylabel_pt)
        ax.yaxis.set_label_coords(-0.065, 0.5)
        if key == 'phase':
            ax.yaxis.set_major_locator(FixedLocator(np.arange(-360, 361, 120)))
            ax.yaxis.set_minor_locator(AutoMinorLocator(3))
        else:
            ax.yaxis.set_major_locator(MaxNLocator(nbins=4))
            ax.yaxis.set_minor_locator(AutoMinorLocator())
        if key != 'amp':
            ax.tick_params(labelbottom=False)
        if data is not None:
            for ich in chans:
                if ich > nchan:
                    continue
                c = ich - 1
                good = bl.amp[:, c] > 0.0
                if good.any():
                    sym, size = styles[ich]
                    ax.plot(bl.time[good], data[good, c],
                            **scatter_kwargs(sym, size, base_pt))

    # Header above the rate plot.
    head = '%s-%s   Amplitude, Phase, Delay and Rate for %s   %s' % (
        bl.ant_a.strip(), bl.ant_b.strip(), expname.strip(), jdate)
    ax_rat.set_title(head, fontsize=1.5 * base_pt, pad=0.4 * base_pt)

    ax_amp.set_xlabel('Time (from beginning of %s)' % jdate, fontsize=label_pt)

    if not has_data:
        ax_del.text(0.5, 0.5, 'No data in selected channels / time range',
                    transform=ax_del.transAxes, ha='center', va='center',
                    fontsize=label_pt)
        return

    # Source labels on the rate plot, if there are not too many.
    starts = []
    last_t, last_src = -np.inf, None
    for t, src in zip(bl.time, bl.source):
        if t - last_t > MTLAB or src != last_src:
            starts.append((t, src))
        last_t, last_src = t, src
    if len(starts) <= MLABEL:
        rlo, rhi = lim['rate']
        yt = rlo + 0.05 * (rhi - rlo)
        for t, src in starts:
            ax_rat.text(t, yt, src, rotation=90, ha='center', va='bottom',
                        fontsize=0.7 * base_pt, clip_on=True)

    # Symbol key along the bottom of the amplitude plot.
    from matplotlib.lines import Line2D
    handles = [Line2D([], [], linestyle='none')]
    labels = ['Plot symbols by baseband channel:']
    other = None
    for ich in chans:
        if ich > nchan:
            continue
        sym, size = styles[ich]
        if sym != 1:
            handles.append(Line2D([], [], **scatter_kwargs(sym, size, base_pt)))
            labels.append('%d:' % ich)
        elif other is None:
            other = Line2D([], [], **scatter_kwargs(sym, size, base_pt))
    if other is not None:
        handles.append(other)
        labels.append('Others:')
    ax_amp.legend(handles, labels, loc='lower left', bbox_to_anchor=(0.04, 0.04),
                  ncol=len(handles), frameon=False, markerfirst=False,
                  handlelength=0.8, handletextpad=0.3, columnspacing=1.0,
                  borderaxespad=0.0, fontsize=1.1 * base_pt)


def page_iden(fig, base_pt):
    """Equivalent of PGIDEN: user name and date at the bottom right."""
    try:
        user = getpass.getuser()
    except Exception:
        user = ''
    stamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')
    fig.text(0.99, 0.003, ('%s %s' % (user, stamp)).strip(), ha='right',
             va='bottom', fontsize=0.8 * base_pt)


# ----------------------------------------------------------------------------
# Highest-weight delay/rate table (HIWT)
# ----------------------------------------------------------------------------
def fA(s, w):
    return s[:w].ljust(w)


def fI(v, w):
    s = '%d' % v
    return s.rjust(w) if len(s) <= w else '*' * w


def fF(v, w, d):
    s = '%.*f' % (d, v)
    return s.rjust(w) if len(s) <= w else '*' * w


def hiwt(bl, nchan, chans, out):
    """For each channel find the point with the most near neighbours in both
    delay and rate (gaussian weights of width DELSIG ns and RATSIG mHz) and
    write its delay and rate, then the weighted average over the channels
    selected for plotting."""
    if bl.npts == 0:
        return
    out.write('\n\n')
    out.write(' Delay and Rate results for the point with the most close '
              'neighbors in value.\n')
    out.write(' The closer the Quality is to the Npts, the better the data set.\n')
    out.write('\n')
    out.write(' Stations Chan  Npts     Quality     Source          Time     '
              'Delay    Rate  In\n')
    out.write(' ' + ' ' * 61 + '(ns)    (mHz) Avg\n')

    selected = set(chans)
    nd = 0
    wd = wr = accdel = accrat = np.float32(0.0)
    two_d2 = np.float32(2.0 * DELSIG ** 2)
    two_r2 = np.float32(2.0 * RATSIG ** 2)
    a4, b4 = fA(bl.ant_a, 4), fA(bl.ant_b, 4)

    for ich in range(1, nchan + 1):
        c = ich - 1
        amp, dly, rat = bl.amp[:, c], bl.delay[:, c], bl.rate[:, c]
        good = amp > 0.0
        maxwt, nmax, itmax = np.float32(0.0), 0, -1
        with np.errstate(over='ignore'):
            for it in range(bl.npts):
                if not good[it]:
                    continue        # ACCWT stays 0 for a flagged point
                others = good.copy()
                others[it] = False
                nacc = int(others.sum())
                if nacc == 0:
                    continue
                dd = dly[it] - dly[others]
                dr = rat[it] - rat[others]
                w = np.float32(1.0) / (np.exp(dd * dd / two_d2) + np.exp(dr * dr / two_r2))
                accwt = np.float32(w.sum(dtype=np.float32))
                if accwt > maxwt:
                    maxwt, nmax, itmax = accwt, nacc, it

        if itmax >= 0:
            t = np.float32(bl.time[itmax])
            hrf = int(np.float32(t / np.float32(3600.0)))
            hr = hrf % 24
            mn = int(np.float32((t - np.float32(hrf * 3600.0)) / np.float32(60.0)))
            sec = int(np.float32(t - np.float32(hrf * 3600.0) - np.float32(mn * 60.0)))
            inavg = '*' if ich in selected else ' '
            out.write(' ' + a4 + ' ' + b4 + fI(ich, 3) + fI(nmax, 6) +
                      fF(maxwt, 12, 3) + '    ' + fA(bl.source[itmax], 12) +
                      fI(hr, 5) + fI(mn, 3) + fI(sec, 3) +
                      fF(dly[itmax], 9, 1) + fF(rat[itmax], 8, 2) + '  ' +
                      inavg + '\n')
            if inavg == '*':
                nd += 1
                wd += maxwt
                accdel += dly[itmax] * maxwt
                wr += maxwt
                accrat += rat[itmax] * maxwt
        else:
            out.write(' ' + a4 + ' ' + b4 + fI(ich, 3) + '    No result\n')

    if wd > 0.0 and wr > 0.0:
        text = ('Weighted Averages of ' + fI(nd, 2) + ' channels for ' +
                bl.ant_a.strip() + ' - ' + bl.ant_b.strip())
        nums = fF(accdel / wd, 9, 1) + fF(accrat / wr, 8, 2)
        # Fortran T59: tab to column 59 (overwriting if the text is longer).
        text = text.ljust(58)
        out.write((text[:58] + nums + text[58 + len(nums):]).rstrip() + '\n')


# ----------------------------------------------------------------------------
# Output handling
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
    one multi-page DSC document.  matplotlib can only write one page per PS
    file, and converting a PDF to PS (pdftops/pdf2ps) inflates these plots
    3-10x.  Each page keeps its own prolog (fonts are subsetted per page)
    inside save/restore, so pages cannot interfere with each other."""
    header = []
    for line in pages[0].splitlines():
        if line.startswith('%%EndComments'):
            break
        if line.startswith('%%Pages:'):
            line = '%%%%Pages: %d' % len(pages)
        elif line.startswith('%%Title:'):
            line = '%%Title: plotapd'
        header.append(line)
    out.write('\n'.join(header) + '\n%%EndComments\n')
    for n, page in enumerate(pages, 1):
        lines = page.splitlines()
        start = next(i for i, l in enumerate(lines) if l.startswith('%%EndComments')) + 1
        # Drop the inner document's DSC structure comments; the outer
        # document supplies its own.
        body = [l for l in lines[start:] if not l.startswith(('%%', '%!'))]
        # matplotlib's page code leaves values on the operand stack, so keep
        # the save object in a name and clear the stacks before restoring.
        out.write('%%%%Page: %d %d\n/plotapd_save save def\n' % (n, n))
        out.write('\n'.join(body))
        out.write('\nclear cleardictstack\nplotapd_save restore\n')
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
            # A single page needs no page number in its name.
            if len(self.written) == 1:
                os.replace(self.written[0], self.filename)
                self.written = [self.filename]
            return ', '.join(self.written)
        return self.filename


# ----------------------------------------------------------------------------
# Command line
# ----------------------------------------------------------------------------
def parse_channels(values):
    """ALL, END (first and last), or a list of channel numbers."""
    if not values:
        return 'ALL'
    joined = ' '.join(values).replace(',', ' ').split()
    if len(joined) == 1 and joined[0].upper().startswith('ALL'):
        return 'ALL'
    if len(joined) == 1 and joined[0].upper().startswith('END'):
        return 'END'
    try:
        chans = [int(v) for v in joined]
    except ValueError:
        raise SystemExit('Bad --channels value: %s' % ' '.join(values))
    if any(c < 1 for c in chans):
        raise SystemExit('Channel numbers start at 1')
    return chans


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
    if tmax == 0:
        tmax = TMAX_ALL
    return float(tmin), float(tmax)


def main(argv=None):
    p = argparse.ArgumentParser(
        description='Plot sniffer amplitude, phase, delay and rate versus time '
                    'for each baseline, and write a table of the most '
                    'consistent delay and rate per channel (Python port of '
                    'PLOTAPD).',
        epilog='Examples:\n'
               '  plotapd.py datafile.lis bm14_apd.ps/vps\n'
               '  plotapd.py bm14.apd bm14_apd.pdf -c 1 3 8 -a LA PT\n'
               '  plotapd.py bm14.apd /xs -t 0 10 0 0 0 12 30 0\n',
        formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('datafile', help='input .apd file (or difxsniff datafile.lis)')
    p.add_argument('plotfile', nargs='?',
                   help='output plot file.  The extension sets the format '
                        '(.pdf, .ps, .png, .svg, ...; multi-page for .pdf/.ps, '
                        'one file per page otherwise).  A PGPLOT style device '
                        'suffix is also accepted, e.g. name.ps/vps (portrait), '
                        'name.ps/ps (landscape), or /xs to display on screen '
                        'one baseline at a time.  Default: <datafile>_apd.pdf')
    p.add_argument('legacy', nargs='*', help=argparse.SUPPRESS)
    p.add_argument('-c', '--channels', nargs='+', metavar='CH',
                   help='baseband channels to plot: ALL (default), END (first '
                        'and last), or a list of numbers, e.g. -c 1 3 8.  The '
                        'first one listed is drawn larger.  The weighted '
                        'averages in the delays file use these channels.')
    p.add_argument('-a', '--antennas', nargs='+', metavar='ANT',
                   help='plot any baseline that includes one of these '
                        'antennas (default ALL)')
    p.add_argument('-t', '--timerange', nargs='+', metavar='N',
                   help='time range as up to 8 integers: start dd hh mm ss '
                        'stop dd hh mm ss, where the first day is day 0 '
                        '(default: everything)')
    p.add_argument('-d', '--delays-file', default='apd.delays',
                   help='output file for the delay/rate table (default '
                        'apd.delays)')
    args = p.parse_args(argv)

    # difxsniff calls "plotapd datafile.lis name/vps ALL ALL ''".  The Fortran
    # ignored everything after the plot file when run with arguments, so do
    # the same rather than failing.
    if args.legacy and any(v.strip() and v.strip().upper() != 'ALL' for v in args.legacy):
        warn('Ignoring extra arguments %s (use --channels/--antennas/--timerange)'
             % args.legacy)

    chan_req = parse_channels(args.channels)
    antennas = args.antennas or ['ALL']
    tmin_req, tmax_req = parse_timerange(args.timerange)

    if args.plotfile is None:
        args.plotfile = os.path.splitext(os.path.basename(args.datafile))[0] + '_apd.pdf'
    plotname, interactive, landscape = parse_plot_target(args.plotfile)

    if not os.path.isfile(args.datafile):
        raise SystemExit(' File not found: %s' % args.datafile)
    with open(args.datafile, 'r', errors='replace') as f:
        if not any(line.strip() for line in f):
            raise SystemExit('Input file %s is empty - nothing to plot.' % args.datafile)
    for path in ([] if interactive else [plotname]) + [args.delays_file]:
        outdir = os.path.dirname(path)
        if outdir and not os.path.isdir(outdir):
            raise SystemExit('Output directory does not exist: %s' % outdir)

    if not interactive:
        matplotlib.use('Agg')
    import matplotlib.pyplot as plt

    expname, jday1, nchan, tmin, tmax, bls = read_apd(
        args.datafile, antennas, tmin_req, tmax_req)

    if chan_req == 'ALL':
        chans = list(range(1, nchan + 1))
    elif chan_req == 'END':
        chans = [1, nchan] if nchan > 1 else [1]
    else:
        chans = chan_req
        bad = [c for c in chans if c > nchan]
        if bad:
            warn('Channels %s are not in the data (max %d)' % (bad, nchan))

    # Time axis limits are common to all baselines.
    if tmin_req != 0.0:
        tmin = tmin_req
    if tmax_req != TMAX_ALL:
        tmax = tmax_req
    tmin, tmax = fixlim(tmin, tmax, 0.04, 0.04)

    jdate = mjd_to_jdate(jday1)
    if interactive:
        nper, cht = 1, 0.8
        figsize = (11.0, 8.5)
    else:
        # About the size of a PGPLOT /VPS (/PS) page, so the plots fit on
        # both Letter and A4 paper.
        nper, cht = 3, 1.0
        figsize = (10.5, 8.0) if landscape else (8.0, 10.5)
    # PGPLOT's default character height is 1/40 of the (sub-)page height.
    base_pt = figsize[1] / nper / 40.0 * 72.0 * cht
    styles = channel_styles(nchan, chans[0] if chans else 0, cht)

    writer = None if interactive else PageWriter(plotname, landscape)
    with open(args.delays_file, 'w') as delays:
        fig = None
        for i, bl in enumerate(bls):
            slot = i % nper
            if slot == 0:
                fig = plt.figure(figsize=figsize)
            lim, has_data = baseline_limits(bl, chans)
            plot_baseline(fig, slot, nper, bl, lim, has_data, chans, nchan,
                          styles, tmin, tmax, expname, jdate, cht, base_pt)
            hiwt(bl, nchan, chans, delays)
            if slot == nper - 1 or i == len(bls) - 1:
                page_iden(fig, base_pt)
                if interactive:
                    fig.canvas.manager.set_window_title(
                        '%s-%s' % (bl.ant_a.strip(), bl.ant_b.strip()))
                    plt.show()      # close the window to see the next baseline
                else:
                    writer.save(fig)
                plt.close(fig)

    if writer is not None:
        where = writer.close()
        print('Plotted %d baseline%s on %d page%s: %s'
              % (len(bls), '' if len(bls) == 1 else 's',
                 writer.npages, '' if writer.npages == 1 else 's', where))
    print('Delay/rate table written to %s' % args.delays_file)
    return 0


if __name__ == '__main__':
    sys.exit(main())
