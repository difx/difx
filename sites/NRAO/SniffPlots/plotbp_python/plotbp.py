#!/usr/bin/env python3
"""
plotbp.py - Python port of the NRAO SniffPlots program PLOTBP.

Plots sniffer bandpasses: every autocorrelation (.acb) or cross correlation
(.xcb) spectrum in the input file, 10 panels per page (5 for screen
display), and then two summary plots:

  acbandsum.ps  for autocorrelations: for each station and frequency setup,
                the average spectrum with the range of all spectra shaded.
  xcbandmax.ps  for cross correlations: for each baseline and frequency
                setup, the spectrum with the smoothest phases.

Autocorrelation spectra dominated by a few very high channels (RFI) are also
redrawn on an expanded scale.

Input format (as written by difx2fits sniffer.c), repeated for each spectrum:
    timerange: MJD hhmmss MJD hhmmss obscode: EXP chans: NCHAN x NBBC
    source: NAME bandw: BW MHz
    bandfreq: FREQ GHz polar: PP side: S bbchan: 0      (one per BBC)
    then one line per channel, either
      ant antName chan amp                              (autocorrelation)
      ant1 ant2 ant1Name ant2Name chan amp phase        (cross correlation)

Unlike the Fortran original, this version never prompts: everything is set
with command line options.  See "plotbp.py --help".
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
    sys.exit('plotbp.py needs numpy and matplotlib (%s).  Install them with, '
             'for example:  python3 -m pip install --user numpy matplotlib' % exc)

F32 = np.float32

# Panel geometry (normalised panel coordinates), from pltbp.f / pltsum.f.
LEFT, RIGHT = 0.1, 0.95
BOTTOM, ATOP, PTOP = 0.15, 0.6, 0.85

INTERACTIVE_DEVICES = {'x', 'xs', 'xw', 'xwin', 'xwindow', 'xserve'}
PS_DEVICES = {'ps', 'vps', 'cps', 'vcps'}
LANDSCAPE_DEVICES = {'ps', 'cps'}
RASTER_DEVICES = {'png', 'tpng', 'gif', 'vgif'}
KNOWN_DEVICES = INTERACTIVE_DEVICES | PS_DEVICES | RASTER_DEVICES | {'pdf'}
HEADER_WORDS = ('timerange:', 'source:', 'bandfreq:')


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


def fF(v, w, d):
    s = '%.*f' % (d, v)
    return s.rjust(w) if len(s) <= w else '*' * w


def mjd_to_jdate(mjd):
    """rcwlib JDATE: MJD -> e.g. 1994oct25."""
    d = datetime.date(1858, 11, 17) + datetime.timedelta(days=int(mjd))
    return '%04d%s%02d' % (d.year, d.strftime('%b').lower(), d.day)


# ----------------------------------------------------------------------------
# Reading (GETBP / GETHDR)
# ----------------------------------------------------------------------------
class Header:
    """Scan information from the timerange:/source:/bandfreq: lines."""

    def __init__(self):
        self.ctime = ('', '')
        self.expname = ''
        self.jday1 = 0
        self.nchif = 0
        self.nif = 0
        self.newfmt = True
        self.source = ''
        self.bw = F32(0.0)
        self.freq = []
        self.stokes = []
        self.sbd = []


class Spectrum:
    """One spectrum, with the header that applied to it."""

    def __init__(self, hdr):
        self.hdr = hdr
        self.type = ''
        self.name1 = ''
        self.name2 = ''
        self.amp = {}
        self.phase = {}
        self.ampavg = F32(0.0)
        self.navg = 0
        self.nchan = 0
        self.amax = F32(-1.0e9)
        self.pmin = F32(200.0)
        self.pmax = F32(-200.0)

    def arrays(self):
        """AMP and PHASE as float32 arrays indexed by channel-1, zero where
        no data was given (as the Fortran arrays are)."""
        n = max(self.nchan, self.hdr.nif * self.hdr.nchif)
        amp = np.zeros(n, dtype=F32)
        phase = np.zeros(n, dtype=F32)
        for ch, v in self.amp.items():
            amp[ch - 1] = v
        for ch, v in self.phase.items():
            phase[ch - 1] = v
        return amp, phase


class Reader:
    """Line reader with the RDLINE behaviour (whitespace separated words)."""

    def __init__(self, f):
        self.f = f
        self.lineno = 0

    def next(self):
        line = self.f.readline()
        if line == '':
            return None
        self.lineno += 1
        return line.split()


def read_header(words, rd, hdr_prev):
    """GETHDR.  `words` is the timerange: line.  Returns (Header or None)."""
    h = Header()
    w = words + [''] * 12
    h.ctime = (w[2][:11], w[4][:11])
    h.expname = w[6][:8]
    h.jday1 = int(getnum(w[1], rd.lineno)) if w[1] else 0
    h.nchif = int(getnum(w[8], rd.lineno)) if w[8] else 0
    h.nif = int(getnum(w[10], rd.lineno)) if w[10] else 0
    words = rd.next()
    if words is None:
        print(' No data ')
        return None
    h.newfmt = len(words) != 10
    w = words + [''] * 12
    if not h.newfmt:
        h.source = w[1][:11]
        h.stokes = [w[9][:2]]
        h.freq = [F32(getnum(w[3], rd.lineno) * 1000.0)]
        h.bw = F32(getnum(w[6], rd.lineno))
        h.sbd = [' ']
    else:
        h.source = w[1][:11]
        h.bw = F32(getnum(w[3], rd.lineno)) if w[3] else F32(0.0)
        for _ in range(h.nif):
            words = rd.next()
            if words is None:
                print(' GETHDR: Missing channel info. ')
                return None
            w = words + [''] * 8
            h.freq.append(F32(getnum(w[1], rd.lineno) * 1000.0))
            h.stokes.append(w[4][:2])
            h.sbd.append(w[6][:1])
    return h


def read_spectra(path):
    """GETBP: yield each finished Spectrum in file order."""
    with open(path, 'r', errors='replace') as f:
        rd = Reader(f)
        hdr = Header()
        spec = None
        gotdat = False
        first = True
        linchan = 0
        while True:
            words = rd.next()
            eof = words is None
            if eof:
                words = []
            inchan = 0
            if words and words[0] in HEADER_WORDS:
                pass
            elif len(words) == 4:
                inchan = int(getnum(words[2], rd.lineno))
            elif len(words) >= 7:
                inchan = int(getnum(words[4], rd.lineno))

            # A drop in channel number (or a header) ends the spectrum.
            if gotdat and inchan < linchan:
                yield spec
                gotdat = False

            if eof:
                return

            if words[0] == 'timerange:':
                hdr = read_header(words, rd, hdr)
                if hdr is None:
                    return
                continue

            if first or inchan < linchan:
                spec = Spectrum(hdr)
            linchan = inchan

            if len(words) == 4:
                spec.type = 'AC'
                spec.name1 = words[1][:8]
                a = F32(getnum(words[3], rd.lineno))
                ph = spec.phase.get(inchan, F32(0.0))
                spec.ampavg = F32(spec.ampavg + a)
                spec.navg += 1
            elif len(words) in (7, 8):
                spec.type = 'XC'
                spec.name1 = words[2][:8]
                spec.name2 = words[3][:8]
                a = F32(getnum(words[5], rd.lineno) * 1000.0)
                ph = F32(getnum(words[6], rd.lineno))
                spec.phase[inchan] = ph
            else:
                print(' Skipping unrecognized record. %d' % len(words))
                continue
            spec.amp[inchan] = a
            if spec.hdr is not hdr:
                spec.hdr = hdr
            gotdat = True
            first = False
            spec.nchan = max(spec.nchan, inchan)
            spec.amax = max(spec.amax, a)
            spec.pmin = min(spec.pmin, ph)
            spec.pmax = max(spec.pmax, ph)


# ----------------------------------------------------------------------------
# Summary accumulation (ACCSUM)
# ----------------------------------------------------------------------------
class Summary:
    def __init__(self):
        self.pairs = []          # [(name1, name2)]
        self.setups = []         # [dict(nif, nchan, nchif, bw, freq, stokes, sbd)]
        self.normac = {}         # (setup, pair) -> count
        self.sumac = {}          # -> (sum, min, max) float32 arrays
        self.lowdp = {}          # -> lowest average phase difference
        self.hi = {}             # -> (amp, phase)
        self.head = {}           # -> dict(source, jday, ctime1, ctime2, expname)

    def add(self, sp):
        h = sp.hdr
        pair = (sp.name1, sp.name2)
        if pair not in self.pairs:
            self.pairs.append(pair)
        ista = self.pairs.index(pair)
        key = (h.nif, sp.nchan, h.nchif, float(h.bw))
        ifr = None
        for i, s in enumerate(self.setups):
            if s['key'] == key and s['freq'] == list(h.freq[:h.nif]) and \
               s['stokes'] == list(h.stokes[:h.nif]) and s['sbd'] == list(h.sbd[:h.nif]):
                ifr = i
                break
        if ifr is None:
            self.setups.append(dict(key=key, nif=h.nif, nchan=sp.nchan, nchif=h.nchif,
                                    bw=h.bw, freq=list(h.freq[:h.nif]),
                                    stokes=list(h.stokes[:h.nif]), sbd=list(h.sbd[:h.nif])))
            ifr = len(self.setups) - 1
            print(' Found setup %d' % (ifr + 1))
        k = (ifr, ista)
        amp, phase = sp.arrays()
        n = sp.nchan
        gethead = False
        if sp.type == 'AC':
            self.normac[k] = self.normac.get(k, 0) + 1
            if k not in self.sumac:
                self.sumac[k] = (np.zeros(n, F32), np.full(n, 1.0e10, F32), np.zeros(n, F32))
            s, lo, hi = self.sumac[k]
            s += amp[:n]
            np.minimum(lo, amp[:n], out=lo)
            np.maximum(hi, amp[:n], out=hi)
            gethead = True
        elif sp.type == 'XC':
            # Choose the spectrum with the smoothest phases: smallest average
            # adjacent-channel phase difference (every second channel when
            # there are more than 32 channels per IF).
            if n >= 2:
                incr = 2 if n > h.nif * 32 else 1
                d = np.abs(phase[incr:n] - phase[:n - incr]).astype(F32)
                d = np.where(d > F32(180.0), F32(360.0) - d, d).astype(F32)
                aphase = F32(np.add.accumulate(d, dtype=F32)[-1] / F32(n - incr))
            else:
                aphase = F32(360.0)
            if aphase < self.lowdp.get(k, F32(360.0)):
                self.hi[k] = (amp[:n].copy(), phase[:n].copy())
                self.lowdp[k] = aphase
                gethead = True
        if gethead:
            old = self.head.get(k)
            ct1 = h.ctime[0] if (sp.type == 'XC' or old is None) else old['ctime1']
            self.head[k] = dict(source=h.source, jday=h.jday1, ctime1=ct1,
                                ctime2=h.ctime[1], expname=h.expname)


# ----------------------------------------------------------------------------
# Plotting helpers
# ----------------------------------------------------------------------------
class PanelPages:
    """PGSUBP-like grid of nx x ny panels per page, filled row by row."""

    def __init__(self, plt, writer, nx, ny, figsize, chsize):
        self.plt = plt
        self.writer = writer
        self.nx, self.ny = nx, ny
        self.figsize = figsize
        self.fig = None
        self.slot = 0
        cell_h = figsize[1] / ny
        # PGPLOT character height is 1/40 of the panel; matplotlib's font is
        # wider, so scale it down a little.
        self.base_pt = cell_h / 40.0 * 72.0 * chsize * 0.75

    def next(self):
        """Return (fig, (x0, y0, w, h)) for the next panel."""
        if self.fig is None or self.slot == self.nx * self.ny:
            self.flush()
            self.fig = self.plt.figure(figsize=self.figsize)
            self.slot = 0
        col = self.slot % self.nx
        row = self.slot // self.nx
        self.slot += 1
        w = 1.0 / self.nx
        h = (1.0 - 0.02) / self.ny       # leave room for the page stamp
        return self.fig, (col * w, 1.0 - (row + 1) * h, w, h)

    def flush(self):
        if self.fig is not None:
            page_iden(self.fig, self.base_pt)
            if self.writer is None:
                self.plt.show()
            else:
                self.writer.save(self.fig)
            self.plt.close(self.fig)
            self.fig = None


def add_axes(fig, cell, x0, x1, y0, y1):
    cx, cy, cw, ch = cell
    return fig.add_axes([cx + x0 * cw, cy + y0 * ch, (x1 - x0) * cw, (y1 - y0) * ch])


def style_axes(ax, base_pt, xlabels=True):
    from matplotlib.ticker import AutoMinorLocator, MaxNLocator
    ax.tick_params(which='both', direction='in', top=True, right=True,
                   labelsize=base_pt, pad=2)
    ax.xaxis.set_major_locator(MaxNLocator(nbins=8, integer=True))
    ax.yaxis.set_major_locator(MaxNLocator(nbins=4))
    ax.xaxis.set_minor_locator(AutoMinorLocator())
    ax.yaxis.set_minor_locator(AutoMinorLocator())
    if not xlabels:
        ax.tick_params(labelbottom=False)


def phase_axis(ax):
    from matplotlib.ticker import FixedLocator, AutoMinorLocator
    ax.yaxis.set_major_locator(FixedLocator(np.arange(-360, 361, 120)))
    ax.yaxis.set_minor_locator(AutoMinorLocator(3))


def label_ifs(ax, nif, nchif, ymax, freqs, stokes, sbds, newfmt, nx, base_pt,
              small, stagger, sep_from=0.0, sep_to=None, dashed=True):
    """Separator lines between baseband channels plus the frequency and
    polarization/sideband of each, as in PLTBP/PLTSUM/PACSUM."""
    if sep_to is None:
        sep_to = ymax
    ls = '--' if dashed else '-'
    for iif in range(1, nif + 1):
        ich1 = (iif - 1) * nchif
        if iif != 1:
            ax.plot([ich1 + 0.5, ich1 + 0.5], [sep_from, sep_to], color='black',
                    linestyle=ls, linewidth=0.5)
        if newfmt or iif == 1:
            yt = ymax * 0.9
            if stagger:
                yt += ymax * (-0.04 + 0.07 * (iif % 2))
            fs = base_pt * (small if nif >= 8 else 1.0)
            ax.text(ich1 + 0.05 * nchif, yt, fF(freqs[iif - 1], 9, 2),
                    fontsize=fs, clip_on=True)
            ax.text(ich1 + 0.15 * nchif, ymax * 0.03,
                    '%s %s' % (stokes[iif - 1], sbds[iif - 1]), fontsize=fs, clip_on=True)


def btline(bw):
    return 'Channel (' + fF(bw, 7, 3) + ' MHz/BBCh. )'


def plot_spectrum(pages, sp, nx):
    """PLTBP for one spectrum."""
    h = sp.hdr
    amp, phase = sp.arrays()
    nchan = sp.nchan
    nif, nchif = h.nif, h.nchif
    amax = sp.amax
    pmin, pmax = sp.pmin, sp.pmax
    ampavg = F32(sp.ampavg / F32(sp.navg)) if sp.navg else F32(np.nan)
    almax = min(amax, F32(ampavg * F32(3.0))) if sp.navg else amax
    alim = np.minimum(amp, almax).astype(F32)

    dozoom = bool(amax > almax)
    amax = F32(amax * F32(1.1))
    if amax == 0.0:
        amax = F32(0.1)
    pmin = min(pmin, F32(-185.0))
    pmax = max(pmax, F32(185.0))
    xmax = nchan + 1
    if nchif == 0 or nif != nchan // nchif:
        print('PLTBP: Number of IFs %d not consistent with total channels and '
              'chan/IF %d %d' % (nif, nchan, nchif))

    hd = (' ' + h.source.strip() + '  ' + h.expname.strip() + ' ' +
          mjd_to_jdate(h.jday1) + ' ' + h.ctime[0].ljust(11) + '-' + h.ctime[1].ljust(11))
    bt = btline(h.bw)
    base_pt = pages.base_pt
    stagger = nif > 8 and nx > 1

    def if_lines(ax, y, **kw):
        for iif in range(1, nif + 1):
            i0 = (iif - 1) * nchif
            x = np.arange(i0 + 1, i0 + nchif + 1)
            ax.plot(x, y[i0:i0 + nchif], color='black', **kw)

    if sp.type == 'AC':
        title = (sp.name1.strip() + hd)[:80]
        for zoom in ((False, True) if dozoom else (False,)):
            fig, cell = pages.next()
            ax = add_axes(fig, cell, LEFT, RIGHT, BOTTOM, PTOP)
            ytop = almax if zoom else amax
            ax.set_xlim(0.0, xmax)
            ax.set_ylim(0.0, float(ytop))
            style_axes(ax, base_pt)
            ax.set_title(title, fontsize=base_pt, pad=0.6 * base_pt)
            ax.set_xlabel(bt, fontsize=base_pt, labelpad=1)
            ax.set_ylabel('Amp * 1000 (Expanded Scale)' if zoom else 'Amp * 1000',
                          fontsize=base_pt)
            if_lines(ax, alim if zoom else amp, linewidth=0.6)
            label_ifs(ax, nif, nchif, float(ytop), h.freq, h.stokes, h.sbd, h.newfmt,
                      nx, base_pt, 0.6, stagger)
    elif sp.type == 'XC':
        title = (sp.name1.strip() + '-' + sp.name2.strip() + hd)[:80]
        fig, cell = pages.next()
        ax = add_axes(fig, cell, LEFT, RIGHT, BOTTOM, ATOP)
        ax.set_xlim(0.0, xmax)
        ax.set_ylim(0.0, float(amax))
        style_axes(ax, base_pt)
        ax.set_xlabel(bt, fontsize=base_pt, labelpad=1)
        ax.set_ylabel('Amp * 1000', fontsize=base_pt)
        if_lines(ax, amp, linewidth=0.6)
        label_ifs(ax, nif, nchif, float(amax), h.freq, h.stokes, h.sbd, h.newfmt,
                  nx, base_pt, 0.7, stagger)
        ax2 = add_axes(fig, cell, LEFT, RIGHT, ATOP, PTOP)
        ax2.set_xlim(0.0, xmax)
        ax2.set_ylim(float(pmin), float(pmax))
        style_axes(ax2, base_pt, xlabels=False)
        phase_axis(ax2)
        ax2.set_title(title, fontsize=base_pt, pad=0.6 * base_pt)
        ax2.set_ylabel('Phase', fontsize=base_pt)
        for iif in range(1, nif + 1):
            i0 = (iif - 1) * nchif
            ax2.plot(np.arange(i0 + 1, i0 + nchif + 1), phase[i0:i0 + nchif],
                     linestyle='none', marker='o', markersize=1.0,
                     markerfacecolor='none', markeredgecolor='black', markeredgewidth=0.4)
        for iif in range(2, nif + 1):
            x = (iif - 1) * nchif + 0.5
            ax2.plot([x, x], [float(pmin), float(pmax)], color='black',
                     linestyle='--', linewidth=0.5)


def plot_ac_summary(pages, summ, nx):
    """PLTSUM/PACSUM: average autocorrelation spectrum per station and setup
    with the min-max range shaded; redrawn on an expanded scale if RFI
    makes the maximum more than 3 times the average of the upper envelope."""
    base_pt = pages.base_pt
    for ista, pair in enumerate(summ.pairs):
        for ifr, s in enumerate(summ.setups):
            k = (ifr, ista)
            if summ.normac.get(k, 0) == 0:
                continue
            tot, lo, hi = summ.sumac[k]
            n = s['nchan']
            avg = (tot[:n] / F32(summ.normac[k])).astype(F32)
            amax = F32(hi[:n].max() if n else 0.0)
            ampavg = F32(np.add.accumulate(hi[:n], dtype=F32)[-1] / F32(n)) if n else F32(0.0)
            amax = F32(amax * F32(1.1))
            if n == 0:
                ampavg = F32(amax / F32(3.0))
            if amax == 0.0:
                amax = F32(1.1)
            hd = summ.head[k]
            title = (pair[0].strip() + ' ' + hd['expname'].strip() + ' ' +
                     mjd_to_jdate(hd['jday']) + ' ' + hd['ctime1'].ljust(11) + '-' +
                     hd['ctime2'].ljust(11))[:80]
            scales = [(amax, 'Amp')]
            if amax > F32(3.0) * ampavg:
                scales.append((F32(F32(3.0) * ampavg), 'Amp (Expanded Scale)'))
            for ytop, ylabel in scales:
                fig, cell = pages.next()
                ax = add_axes(fig, cell, LEFT, RIGHT, BOTTOM, PTOP)
                ax.set_xlim(0.0, n + 1)
                ax.set_ylim(0.0, float(ytop))
                style_axes(ax, base_pt)
                ax.set_title(title, fontsize=base_pt, pad=0.6 * base_pt)
                ax.set_xlabel(btline(s['bw']), fontsize=base_pt, labelpad=1)
                ax.set_ylabel(ylabel, fontsize=base_pt)
                nchif = s['nchif']
                for iif in range(1, s['nif'] + 1):
                    i0 = (iif - 1) * nchif
                    x = np.arange(i0 + 1, i0 + nchif + 1, dtype=float)
                    ylo = np.minimum(lo[i0:i0 + nchif], ytop)
                    yhi = np.minimum(hi[i0:i0 + nchif], ytop)
                    ax.fill(np.concatenate([x, x[::-1]]), np.concatenate([ylo, yhi[::-1]]),
                            color='0.8', linewidth=0)
                    ax.plot(x, np.minimum(avg[i0:i0 + nchif], ytop), color='black', linewidth=1.2)
                    ax.plot(x, ylo, color='black', linewidth=0.4)
                    ax.plot(x, yhi, color='black', linewidth=0.4)
                label_ifs(ax, s['nif'], nchif, float(ytop), s['freq'], s['stokes'], s['sbd'],
                          True, nx, base_pt, 0.75, s['nif'] > 8, dashed=False)


def plot_xc_summary(pages, summ, nx):
    """PLTSUM: for each baseline and setup, the cross correlation spectrum
    with the smoothest phases."""
    base_pt = pages.base_pt
    for ista, pair in enumerate(summ.pairs):
        for ifr, s in enumerate(summ.setups):
            k = (ifr, ista)
            if k not in summ.lowdp:
                continue
            hamp, hph = summ.hi[k]
            n = s['nchan']
            amax = F32(F32(hamp.max() if n else 0.0) * F32(1.1))
            if amax == 0.0:
                amax = F32(1.1)
            nchif = s['nchif']
            fig, cell = pages.next()
            ax = add_axes(fig, cell, LEFT, RIGHT, BOTTOM, ATOP)
            ax.set_xlim(0.0, n + 1)
            ax.set_ylim(0.0, float(amax))
            style_axes(ax, base_pt)
            ax.set_xlabel(btline(s['bw']), fontsize=base_pt, labelpad=1)
            ax.set_ylabel('Amp', fontsize=base_pt)
            for iif in range(1, s['nif'] + 1):
                i0 = (iif - 1) * nchif
                ax.plot(np.arange(i0 + 1, i0 + nchif + 1), hamp[i0:i0 + nchif],
                        color='black', linewidth=1.2)
            label_ifs(ax, s['nif'], nchif, float(amax), s['freq'], s['stokes'], s['sbd'],
                      True, nx, base_pt, 0.75, s['nif'] > 8, dashed=False)
            hd = summ.head[k]
            title = (pair[0].strip() + ' ' + pair[1].strip() + ' ' + hd['source'].strip() +
                     ' ' + hd['expname'].strip() + ' ' + mjd_to_jdate(hd['jday']) + ' ' +
                     hd['ctime1'].ljust(11) + '-' + hd['ctime2'].ljust(11))[:80]
            ax2 = add_axes(fig, cell, LEFT, RIGHT, ATOP, PTOP)
            ax2.set_xlim(0.0, n + 1)
            ax2.set_ylim(-185.0, 185.0)
            style_axes(ax2, base_pt, xlabels=False)
            phase_axis(ax2)
            ax2.set_title(title, fontsize=base_pt, pad=0.6 * base_pt)
            ax2.set_ylabel('Phase', fontsize=base_pt)
            ax2.plot(np.arange(1, n + 1), hph, linestyle='none', marker='o',
                     markersize=1.2, markerfacecolor='none', markeredgecolor='black',
                     markeredgewidth=0.6)
            for iif in range(2, s['nif'] + 1):
                x = (iif - 1) * nchif + 0.5
                ax2.plot([x, x], [-185.0, 185.0], color='black', linewidth=0.5)


def page_iden(fig, base_pt):
    """Equivalent of PGIDEN: user name and date at the bottom right."""
    try:
        user = getpass.getuser()
    except Exception:
        user = ''
    stamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M')
    fig.text(0.99, 0.003, ('%s %s' % (user, stamp)).strip(), ha='right',
             va='bottom', fontsize=max(5.0, base_pt))


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
            line = '%%Title: plotbp'
        header.append(line)
    out.write('\n'.join(header) + '\n%%EndComments\n')
    for n, page in enumerate(pages, 1):
        lines = page.splitlines()
        start = next(i for i, l in enumerate(lines) if l.startswith('%%EndComments')) + 1
        body = [l for l in lines[start:] if not l.startswith(('%%', '%!'))]
        # matplotlib's page code leaves values on the operand stack, so keep
        # the save object in a name and clear the stacks before restoring.
        out.write('%%%%Page: %d %d\n/plotbp_save save def\n' % (n, n))
        out.write('\n'.join(body))
        out.write('\nclear cleardictstack\nplotbp_save restore\n')
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
            if not self.pages:
                return None
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
def main(argv=None):
    p = argparse.ArgumentParser(
        description='Plot sniffer autocorrelation or cross correlation '
                    'bandpasses, plus summary plots (Python port of PLOTBP).',
        epilog='Examples:\n'
               '  plotbp.py acbandfile.lis acbandfile.ps/vps\n'
               '  plotbp.py xcbandfile.lis xcb.pdf -s PT:LA\n'
               '\n'
               'The summary plots acbandsum.ps (autocorrelation input) and\n'
               'xcbandmax.ps (cross correlation input) are also written to the\n'
               'current directory unless --no-summary is given.\n',
        formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('bpfile', help='bandpass file (.acb or .xcb from difx2fits, or '
                                  'acbandfile.lis / xcbandfile.lis from difxsniff)')
    p.add_argument('plotfile', nargs='?',
                   help='output plot file.  The extension sets the format '
                        '(.pdf, .ps, .png, .svg, ...; multi-page for .pdf/.ps, '
                        'one file per page otherwise).  A PGPLOT style device '
                        'suffix is also accepted, e.g. name.ps/vps (portrait), '
                        'name.ps/ps (landscape), or /xs to display on screen.  '
                        'Default: <bpfile>_bp.pdf')
    p.add_argument('station_pos', nargs='?', metavar='STATION', default=None,
                   help='same as --station (kept for compatibility with difxsniff)')
    p.add_argument('-s', '--station', default=None,
                   help='only plot spectra involving this station, or this '
                        'baseline when given as e.g. PT:LA (default: all).  '
                        'The summary plots always include everything.')
    p.add_argument('--ac-summary', default='acbandsum.ps',
                   help='autocorrelation summary plot file (default acbandsum.ps)')
    p.add_argument('--xc-summary', default='xcbandmax.ps',
                   help='cross correlation summary plot file (default xcbandmax.ps)')
    p.add_argument('--no-summary', action='store_true',
                   help='do not make the summary plots')
    args = p.parse_args(argv)

    dosta = (args.station if args.station is not None else (args.station_pos or '')).strip().upper()
    dosta2 = ''
    if ':' in dosta[1:]:
        dosta, dosta2 = dosta.split(':', 1)
        print(' Will plot baseline %s %s' % (dosta, dosta2))

    if args.plotfile is None:
        args.plotfile = os.path.splitext(os.path.basename(args.bpfile))[0] + '_bp.pdf'
    plotname, interactive, landscape = parse_plot_target(args.plotfile)
    summaries = []
    if not interactive and not args.no_summary:
        summaries = [parse_plot_target(args.ac_summary)[0], parse_plot_target(args.xc_summary)[0]]

    if not os.path.isfile(args.bpfile):
        raise SystemExit(' File not found: %s' % args.bpfile)
    with open(args.bpfile, 'r', errors='replace') as f:
        if not any(line.strip() for line in f):
            raise SystemExit('Input file %s is empty - nothing to plot.' % args.bpfile)
    for path in ([] if interactive else [plotname]) + summaries:
        outdir = os.path.dirname(path)
        if outdir and not os.path.isdir(outdir):
            raise SystemExit('Output directory does not exist: %s' % outdir)

    if not interactive:
        matplotlib.use('Agg')
    import matplotlib.pyplot as plt

    if interactive:
        ny, chsize, figsize = 2, 1.5, (11.0, 8.5)
    else:
        ny, chsize = 5, 2.0
        figsize = (10.5, 8.0) if landscape else (8.0, 10.5)

    summ = Summary()
    pages = None
    writer = None
    nx = 2
    nspec = nplot = 0
    last_nif = 0
    for sp in read_spectra(args.bpfile):
        nspec += 1
        last_nif = sp.hdr.nif
        want = ((dosta == '' or dosta in (sp.name1, sp.name2)) and
                (dosta2 == '' or dosta2 in (sp.name1, sp.name2)))
        if want:
            if pages is None:
                # Two panels across for up to 8 basebands, else full width.
                nx = 2 if sp.hdr.nif <= 8 else 1
                writer = None if interactive else PageWriter(plotname, landscape)
                pages = PanelPages(plt, writer, nx, ny, figsize, chsize)
            plot_spectrum(pages, sp, nx)
            nplot += 1
        summ.add(sp)

    if nspec == 0:
        raise SystemExit('Input file %s contains no spectra - nothing to plot.' % args.bpfile)
    if pages is None:
        print('No spectra matched station selection "%s%s%s"'
              % (dosta, ':' if dosta2 else '', dosta2))
    else:
        pages.flush()
        if writer is not None:
            where = writer.close()
            print('Plotted %d spectr%s on %d page%s: %s'
                  % (nplot, 'um' if nplot == 1 else 'a', writer.npages,
                     '' if writer.npages == 1 else 's', where))

    # Summary plots (not for screen display).
    if summaries:
        snx = 2 if last_nif <= 8 else 1
        for fname, have, func in ((summaries[0], bool(summ.normac), plot_ac_summary),
                                  (summaries[1], bool(summ.lowdp), plot_xc_summary)):
            if not have:
                continue
            w = PageWriter(fname, parse_plot_target(fname)[2])
            sp_pages = PanelPages(plt, w, snx, 5, (8.0, 10.5), 2.0)
            func(sp_pages, summ, snx)
            sp_pages.flush()
            w.close()
            print('Summary plot written to %s' % fname)
    return 0


if __name__ == '__main__':
    sys.exit(main())
