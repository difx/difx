#!/usr/bin/env python3
"""
plotgps.py - Python port of the NRAO SniffPlots program PLOTGPS.

Reads the gps-fmout (or fmout-gps) clock measurements from a Mark IV / SNAP
station log, fits a straight line (offset and rate) to them, prints the
result as clock table lines for a job script, and plots the measurements
with the fit.

Station clock offsets ("traditional" maser-minus-GPS offsets) are read from
an offsets file with lines of the form
    STATION_CODE   OFFSET(s)   SIGN
('#' starts a comment line).  The station code is taken from the log file
name, which should end in log.xx (xx = station code), or from --station.

Unlike the Fortran original, this version never prompts: everything is set
with command line options, and the plot range is chosen with --xrange and
--yrange instead of being re-entered interactively.  See "plotgps.py --help".
"""

import argparse
import datetime
import getpass
import math
import os
import re
import sys

try:
    import numpy as np
    import matplotlib
except ImportError as exc:
    sys.exit('plotgps.py needs numpy and matplotlib (%s).  Install them with, '
             'for example:  python3 -m pip install --user numpy matplotlib' % exc)

F32 = np.float32
DEFAULT_OFFSETS = '/users/cwalker/bin/gpsoffsets.dat'
MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

INTERACTIVE_DEVICES = {'x', 'xs', 'xw', 'xwin', 'xwindow', 'xserve'}
PS_DEVICES = {'ps', 'vps', 'cps', 'vcps'}
LANDSCAPE_DEVICES = {'ps', 'cps'}
RASTER_DEVICES = {'png', 'tpng', 'gif', 'vgif'}
KNOWN_DEVICES = INTERACTIVE_DEVICES | PS_DEVICES | RASTER_DEVICES | {'pdf'}


# ----------------------------------------------------------------------------
# Fortran-style output, so the printed results look as they always have.
# ----------------------------------------------------------------------------
def _fixed_sig(x, sig, width):
    ax = abs(x)
    ndig = int(math.floor(math.log10(ax))) + 1 if ax >= 1 else 0
    s = '%.*f' % (max(sig - ndig, 0), x)
    if '.' not in s:
        s += '.'
    return s.rjust(width)


def _ld_real(x, double):
    """gfortran list-directed REAL (G16.9E2) / DOUBLE PRECISION (G25.17E3)."""
    sig, width, ew = (17, 20, 3) if double else (9, 12, 2)
    x = float(x) if double else float(F32(x))
    ax = abs(x)
    if x == 0.0:
        return ('%.*f' % (sig - 1, 0.0)).rjust(width) + ' ' * (ew + 2)
    rounded = float('%.*e' % (sig - 1, ax))
    if 0.1 <= rounded < 10.0 ** sig:
        return _fixed_sig(x, sig, width) + ' ' * (ew + 2)
    mant, exp = ('%.*e' % (sig - 1, x)).split('e')
    e = int(exp)
    return ('%sE%s%0*d' % (mant, '-' if e < 0 else '+', ew, abs(e))).rjust(width + ew + 2)


class R4(float):
    """Marks a value to be printed as a single precision REAL."""


def ldwrite(*items):
    """WRITE(*,*) items: list-directed output as gfortran formats it."""
    out = ''
    prev_char = None
    for it in items:
        is_char = isinstance(it, str)
        if prev_char is not None and not (is_char and prev_char):
            out += ' '
        if is_char:
            out += it
        elif isinstance(it, (bool, np.bool_)):
            out += 'T' if it else 'F'
        elif isinstance(it, (int, np.integer)):
            out += '%11d' % it
        elif isinstance(it, (R4, np.float32)):
            out += _ld_real(it, False)
        else:
            out += _ld_real(it, True)
        prev_char = is_char
    print(' ' + out)


def fE(v, w, d):
    """Fortran 1P,Ew.d: one digit before the point and d after it."""
    s = '%.*E' % (d, float(v))
    mant, exp = s.split('E')
    e = int(exp)
    s = '%sE%s%02d' % (mant, '-' if e < 0 else '+', abs(e)) if abs(e) < 100 else \
        '%s%s%03d' % (mant, '-' if e < 0 else '+', abs(e))
    return s.rjust(w) if len(s) <= w else '*' * w


def fF(v, w, d):
    s = '%.*f' % (d, v)
    return s.rjust(w) if len(s) <= w else '*' * w


# ----------------------------------------------------------------------------
# Reading
# ----------------------------------------------------------------------------
def len1(s):
    return len(s.rstrip(' '))


def fortran_read_real(text):
    """List-directed READ of one DOUBLE PRECISION value from `text`.  Returns
    None where the Fortran READ would take its ERR= branch."""
    s = text.lstrip(' ')
    if s == '':
        return None
    tok = re.split(r'[ ,/]', s, maxsplit=1)[0]
    if tok == '':
        return None
    m = re.fullmatch(r'([+-]?)(\d+\.?\d*|\.\d+)(?:([EeDdQq])([+-]?\d+)|([+-]\d+))?', tok)
    if not m:
        return None
    mant = m.group(1) + m.group(2)
    exp = m.group(4) or m.group(5)
    try:
        return float(mant + ('e' + exp if exp else ''))
    except ValueError:
        return None


def int_field(s):
    """Fortran Iw input: blanks are ignored, an all-blank field is 0."""
    s = s.replace(' ', '')
    if s == '':
        return 0
    return int(s)


def read_offsets(path):
    """Read the station offsets file (code, offset, sign)."""
    offs = []
    with open(path, 'r', errors='replace') as f:
        for line in f:
            line = line.rstrip('\n')[:128]
            if line[:1] == '#' or line.strip() == '':
                continue
            w = re.split(r'[ ,]+', line.strip())
            try:
                code, off, sign = w[0].strip('\'"')[:2].ljust(2), float(w[1]), float(w[2])
            except (IndexError, ValueError):
                print(' Bad line in clock offset file: %s' % line.strip())
                continue
            offs.append((code, F32(off), F32(sign)))
            ldwrite(len(offs), '  ', code, R4(off), R4(sign))
    return offs


def read_log(path):
    """Extract the GPS clock measurements from a SNAP log.  Returns
    (year, times (DOY, float32), offsets (s, float32))."""
    times, gps = [], []
    liy = 0
    nprt = 1
    iy = 0
    print(' Some sample modified input lines:')
    with open(path, 'r', errors='replace') as f:
        for raw in f:
            inline = raw.rstrip('\n').rstrip('\r')[:128].ljust(128)
            ich1 = ich2 = 0
            if 'clock' not in inline:
                ich1 = inline.find('/gps-fmout/') + 1
                ich2 = inline.find('/fmout-gps/') + 1
            if ich1 == 0 and ich2 == 0:
                continue
            newfmt = inline[4:5] == '.'
            ich = max(ich1, ich2) + 11            # 1 based start of the value
            nch = len1(inline)
            if nprt <= 5:
                ldwrite('----Original: ', inline[:nch])
            try:
                if newfmt:
                    iy, idoy, ih, im, isec = (int_field(inline[0:4]), int_field(inline[5:8]),
                                              int_field(inline[9:11]), int_field(inline[12:14]),
                                              int_field(inline[15:17]))
                else:
                    iy, idoy, ih, im, isec = (int_field(inline[0:2]), int_field(inline[2:5]),
                                              int_field(inline[5:7]), int_field(inline[7:9]),
                                              int_field(inline[9:11]))
                    iy += 1900
            except ValueError:
                ldwrite('Bad gps line: ', inline[:nch])
                continue
            if liy != 0 and iy != liy:
                print(' ****** PROBLEM ********* ')
                print('  Crossed year boundary. ')
                print('  This program can\'t handle that. ')
                print('  Break log into single year segments ')
                sys.exit(1)
            liy = iy

            line = list(inline)

            def seg(a, b):              # 1 based inclusive substring
                return ''.join(line[a - 1:b])

            def put(pos, ch):
                line[pos - 1] = ch

            # Units: a "usec" means microseconds; blank it and the rest.
            ie = seg(ich, nch).find('usec') + 1
            if ie:
                mult = 1.0e-6
                for k in range(ich + ie - 1, nch + 1):
                    put(k, ' ')
                nch = len1(''.join(line))
            else:
                mult = 1.0
            # nEm -> nDm
            ie = seg(ich, nch).find('E') + 1
            if ie == 0:
                ie = seg(ich, nch).find('e') + 1
            if ie:
                put(ich + ie - 1, 'D')
            # Remove a + sign, "S", "T" or "RQIR" before the exponent.
            ie = seg(ich, nch).find('D') + 1
            jch = nch if ie == 0 else ie + ich - 1
            for tok in ('+', 'S', 'T'):
                ie = seg(ich, jch).find(tok) + 1
                if ie:
                    put(ich + ie - 1, ' ')
            ie = seg(ich, jch).find('RQIR') + 1
            if ie:
                for k in range(4):
                    put(ich + ie - 1 + k, ' ')
            # Squeeze out blanks (there can be blanks between a minus sign
            # and the number).
            i = ich
            while i < nch:
                if line[i - 1] == ' ':
                    del line[i - 1]
                    line.insert(nch - 1, ' ')
                    nch -= 1
                else:
                    i += 1
            text = ''.join(line)
            # Two decimal points (eg "+5.18.0E-6") - don't guess.
            ipch = seg(ich, nch).find('.') + 1
            rgps = None
            if not (ipch and '.' in seg(ich + ipch, nch)):
                rgps = fortran_read_real(text[ich - 1:nch])
            if rgps is None:
                ldwrite('Bad gps line: ', text[:nch])
                continue
            if nprt <= 5:
                ldwrite('Fixed Format: ', text[:nch])
                ldwrite('Derived offset, mult, offset: ', rgps, mult, rgps * mult)
                nprt += 1
            rgps = mult * rgps
            if rgps > 0.5:
                rgps -= 1.0
            if rgps < -0.5:
                rgps += 1.0
            if ich2 != 0:
                rgps = -rgps
            t = F32(F32(F32(F32(idoy) + F32(ih) / F32(24.0)) + F32(im) / F32(1440.0)) +
                    F32(isec) / F32(86400.0))
            times.append(t)
            gps.append(F32(rgps))
    return iy, np.array(times, dtype=F32), np.array(gps, dtype=F32)


# ----------------------------------------------------------------------------
# Fitting (Numerical Recipes FIT, unweighted, in single precision)
# ----------------------------------------------------------------------------
def fit_line(x, y):
    n = len(x)
    sx = F32(0.0)
    sy = F32(0.0)
    for i in range(n):
        sx = F32(sx + x[i])
        sy = F32(sy + y[i])
    ss = F32(n)
    sxoss = F32(sx / ss)
    st2 = F32(0.0)
    b = F32(0.0)
    for i in range(n):
        t = F32(x[i] - sxoss)
        st2 = F32(st2 + t * t)
        b = F32(b + t * y[i])
    with np.errstate(all='ignore'):
        b = F32(b / st2)
        a = F32((sy - sx * b) / ss)
        siga = F32(np.sqrt(F32((F32(1.0) + sx * sx / (ss * st2)) / ss)))
        sigb = F32(np.sqrt(F32(F32(1.0) / st2)))
        chi2 = F32(0.0)
        for i in range(n):
            chi2 = F32(chi2 + F32(y[i] - a - b * x[i]) ** 2)
        sigdat = F32(np.sqrt(F32(chi2 / F32(n - 2)))) if n > 2 else F32(np.nan)
        siga = F32(siga * sigdat)
        sigb = F32(sigb * sigdat)
    return a, b, siga, sigb


def doy_to_date(year, doy):
    """TDATECW with month 0: day of year -> (month name, day)."""
    d = datetime.date(year, 1, 1) + datetime.timedelta(days=doy - 1)
    return MONTHS[d.month - 1], d.day, d.year


# ----------------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------------
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


def pgrnge(x1, x2):
    return F32(x1 - F32(0.1) * F32(x2 - x1)), F32(x2 + F32(0.1) * F32(x2 - x1))


def main(argv=None):
    p = argparse.ArgumentParser(
        description='Fit and plot GPS minus station clock offsets from a SNAP '
                    'log (Python port of PLOTGPS).',
        epilog='Examples:\n'
               '  plotgps.py bm14kk.log.kk gps_kk.ps/ps\n'
               '  plotgps.py bm14kk.log --station KK -o gpsoffsets.dat gps.pdf\n'
               '  plotgps.py n23x1.log.pt gps.pdf --xrange 78 79 --yrange 1e-6 2e-6\n',
        formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('logfile', help='SNAP (Mark IV) log file, named ...log.xx where '
                                   'xx is the station code')
    p.add_argument('plotfile', nargs='?',
                   help='output plot file (.pdf, .ps, .png, ...; a PGPLOT style '
                        'name.ps/ps or /xs is also accepted).  Default: '
                        '<logfile>_gps.pdf')
    p.add_argument('-o', '--offsets', default=None,
                   help='station clock offsets file (default %s if it exists)'
                        % DEFAULT_OFFSETS)
    p.add_argument('-s', '--station', help='station code (default: the xx in '
                                           'the log file name ...log.xx)')
    p.add_argument('--xrange', nargs=2, type=float, metavar=('XMIN', 'XMAX'),
                   help='plot range in day of year (default: the data range)')
    p.add_argument('--yrange', nargs=2, type=float, metavar=('YMIN', 'YMAX'),
                   help='plot range in seconds (default: the data range)')
    args = p.parse_args(argv)

    if args.plotfile is None:
        args.plotfile = os.path.basename(args.logfile) + '_gps.pdf'
    plotname, interactive, landscape = parse_plot_target(args.plotfile)

    # Station offsets.
    offs = []
    offfile = args.offsets or DEFAULT_OFFSETS
    if os.path.isfile(offfile):
        offs = read_offsets(offfile)
    elif args.offsets:
        raise SystemExit('Clock offset file not found: %s' % args.offsets)
    else:
        print(' No clock offset file (%s) - no station offsets applied.' % offfile)

    if not os.path.isfile(args.logfile):
        raise SystemExit(' File not found: %s' % args.logfile)
    with open(args.logfile, 'r', errors='replace') as f:
        if not any(line.strip() for line in f):
            raise SystemExit('Input file %s is empty - nothing to plot.' % args.logfile)
    if plotname and os.path.dirname(plotname) and not os.path.isdir(os.path.dirname(plotname)):
        raise SystemExit('Output directory does not exist: %s' % os.path.dirname(plotname))

    if args.station:
        stacode = args.station[:2].upper().ljust(2)
    else:
        k = args.logfile.find('log.')
        if k >= 0:
            stacode = args.logfile[k + 4:k + 6].upper().ljust(2)
        else:
            stacode = 'XX'
            print(' *** No station code.  File name must be of form ...log.xx '
                  'where xx is the station code (or use --station).')

    iy, time, gps = read_log(args.logfile)
    npt = len(time)
    if npt == 0:
        raise SystemExit('No gps-fmout or fmout-gps measurements found in %s - '
                         'nothing to plot.' % args.logfile)
    if npt < 3:
        print(' Warning: only %d measurement%s - the fit is not meaningful.'
              % (npt, '' if npt == 1 else 's'))

    xmin, xmax = F32(time.min()), F32(time.max())
    ymin, ymax = F32(gps.min()), F32(gps.max())

    # Refer the time to the middle, so the fitted constant is about the average.
    timemid = F32(F32(xmax + xmin) / F32(2.0))
    ftime = (time - timemid).astype(F32)
    iday = int(timemid)
    dtime = F32(timemid - F32(iday))
    rh = F32(dtime * F32(24.0))
    ih = int(rh)
    rm = F32(F32(rh - F32(ih)) * F32(60.0))
    im = int(rm)
    isec = int(F32(F32(rm - F32(im)) * F32(60.0)))
    month, mday, year = doy_to_date(iy, iday)
    date = '%4d%s%02d_%02dh%02dm%02ds' % (year, month.upper(), mday, ih, im, isec)

    avg, rate, savg, srate = fit_line(ftime, gps)
    ratess = F32(rate / F32(24.0 * 3600.0))
    sratess = F32(srate / F32(24.0 * 3600.0))

    print(' ----------------------------------------------')
    ldwrite('Mid time: ', timemid)
    print('Average: ' + fE(avg, 15, 5) + ' +- ' + fE(savg, 15, 5))
    print('Rate:    ' + fE(ratess, 15, 5) + ' +- ' + fE(sratess, 15, 5))

    if stacode == 'MC':
        print(' *** WARNING:  For MC, the sign is traditionally reversed ')
        print('               from that of other stations.')
        print('   This program used to reverse the sign.')
        print('   But MC is changing its sign as of August 2005')
        print('   Be very careful that the sign is correct.')

    jobavg = float(avg)
    jobrate = ratess
    joboff = F32(0.0)
    for code, off, sign in offs:
        if stacode == code:
            jobavg = float(F32(sign * avg + off))
            jobrate = F32(sign * ratess)
            joboff = off
            print('')
            print('The job script value below is adjusted by the traditional ' +
                  code + ' offset of ' + fE(off, 13, 5) + 'us')
            print('    The traditional offset is the typical  value of the measured '
                  'clock minus the GPS.')
            print('    Note the sign convention for the traditional offset was '
                  'reversed in July 2005.')
            if sign < 0.0:
                print(' Also the sign of the gps average and rate are inverted '
                      'for this station.')
            print('  ')
            print('    This version of plotgps uses clock offsets adjusted for '
                  'zero at Kokee.')
    print('')
    print(' ------------- For jobs: ----------------------')
    if jobavg > 0.01:
        print(' Average offset large enough that there are likely to be '
              'precision problems.')
        print(' Does the raw data have enough precision?')
        print(' Think hard about what you are doing.')
    else:
        print('!* ' + stacode + ' clock from plotgps fit to log data with ' +
              fF(F32(joboff * F32(1.0e6)), 8, 3) + ' us added. *!')
        print('name = ' + stacode + '   time = ' + date + '   offset = ' + fE(jobavg, 13, 5))
        print('        rate = ' + fE(jobrate, 13, 5) + '   !row!')
    print(' ----------------------------------------------')
    print('')

    # Fit line across the data.
    xl = [xmin, xmax]
    yl = [F32(avg + F32(x - timemid) * rate) for x in xl]

    # Pad the edges.
    xrng = F32(xmax - xmin)
    if xrng == 0.0:
        xrng = F32(F32(0.1) * F32(xmin + xmax))
    if xrng == 0.0:
        xrng = F32(1.0)
    xmin, xmax = F32(xmin - F32(0.03) * xrng), F32(xmax + F32(0.03) * xrng)
    yrng = F32(ymax - ymin)
    if yrng == 0.0:
        yrng = F32(F32(0.1) * F32(ymin + ymax))
    if yrng == 0.0:
        yrng = F32(1.0)
    ymin, ymax = F32(ymin - F32(0.03) * yrng), F32(ymax + F32(0.03) * yrng)
    xlo, xhi = pgrnge(xmin, xmax)
    ylo, yhi = pgrnge(ymin, ymax)
    if args.xrange:
        xlo, xhi = args.xrange
    if args.yrange:
        ylo, yhi = args.yrange

    if not interactive:
        matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    fig, ax = plt.subplots(figsize=(10.5, 8.0) if not interactive else (11, 8.5))
    ax.set_xlim(float(xlo), float(xhi))
    ax.set_ylim(float(ylo), float(yhi))
    ax.axhline(0.0, color='black', linewidth=0.5)
    ax.axvline(0.0, color='black', linewidth=0.5)
    ax.plot(time, gps, linestyle='none', marker='+', markersize=5, color='black')
    ax.plot(xl, yl, color='black', linewidth=1.0)
    ax.set_xlabel('Day (DOY)')
    ax.set_ylabel('gps-fmt (sec)')
    ax.set_title('GPS Offset in ' + args.logfile)
    ax.tick_params(which='both', direction='in', top=True, right=True)
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
        print('Plot written to %s' % plotname)
    return 0


if __name__ == '__main__':
    sys.exit(main())
