#!/usr/bin/python
#
# Script to manufacture various VEX artifacts for use with ALMA data
#

from __future__ import absolute_import
from __future__ import print_function
import optparse
import sys

def deal_with_opts():
    '''
    This script generates some VEX fragments useful for correlating
    experiments involving ALMA.  The arguments supply information,
    and the normal response is to provide shell variable assignments
    with some appropriate VEX or V2D information.
    The report option --report <item> (-r<item>) is one of:

    help        which provides this list
    vars        which outputs everything as shell variables
    alma        which outputs the ALMA versions of things
    ptwo        which outputs the non-ALMA compatible versions
    zoom        which outputs just the zoom channel data
    chan        which outputs chan_ids for HOPS config files
    f1mm        outputs the commands to generate the standard
                (band6/1mm) setup used by the EHT in 2017, 2018...

    This script is currently only useful for some APP experiments.
    '''
    cmd='%prog [options]'
    desc='ALMA VEX helper tool'
    epi='Try -rhelp for more on usage'
    vers='%prog $Id$'
    parser = optparse.OptionParser(usage=cmd, description=desc,
        version=vers, epilog=epi)
    parser.add_option('-a', '--assn', dest='assn', metavar='STRING',
        default='eg', help='the variable prefix (eg)')
    parser.add_option('-b', '--bbcn', dest='bbcn', metavar='DIGITS',
        default='01', help='01 or similar for the BBC name')
    parser.add_option('-c', '--nchn', dest='nchn', metavar='INT', type=int,
        default=32, help='Number of channel definitions (32)')
    parser.add_option('-d', '--ffmt', dest='ffmt', metavar='%.Xf',
        default='%.6f', help='Format for frequencies to print (%.6f MHz)')
    parser.add_option('-f', '--freq', dest='freq', metavar='MHz',
        default='90000.0', help='ALMA band center frequency in MHz')
    parser.add_option('-i', '--idnt', dest='idnt', metavar='STRING',
        default='      ', help='Provide whitespace indent')
    parser.add_option('-m', '--move', dest='move', metavar='INT', type=float,
        default=0, help='Move frequencies by this many resolution channels')
    parser.add_option('-n', '--band', dest='band', metavar='CHAR',
        default='B', help='VEX band name')
    parser.add_option('-o', '--chof', dest='chof', metavar='INT', type=int,
        default=0, help='Channel offset to be added to channel numbers')
    parser.add_option('-p', '--poln', dest='poln', metavar='CHAR',
        default='R', help='L or R or X or Y')
    parser.add_option('-q', '--rndq', dest='rndq', metavar='FLOAT',
        type=float, default=0.0,
        help='if nonzero, round variables to this frequency resolution')
    parser.add_option('-r', '--report', dest='report', metavar='STRING',
        default='help', help='report type, use "-rhelp" for options')
    parser.add_option('-s', '--side', dest='side', metavar='CHAR',
        default='U', help='L or U for the sideband')
    parser.add_option('-v', '--verb', dest='verb',
        action="store_true", default = False, help='provide some commentary')
    parser.add_option('-w', '--wdth', dest='wdth', metavar='MHz',
        default='51.2', help='Zoom Channel bandwidth')
    parser.add_option('-x', '--chpa', dest='chpa', metavar='INT', type=int,
        default=20000, help='freq channels per ALMA band')
    parser.add_option('-z', '--zoom', dest='zoom',
        metavar='MIN-F,ALMA-CF,MAX-F,COUNT',
        default='', help='Min, ALMA Center, and Max freqs and zoom band count')
    (o, a) = parser.parse_args()
    return(o)

def provide_bbc(assn, idnt, poln, bbcn):
    bbc  = '%s* boilerplate\n' % idnt
    bbc += '%sBBC_assign = &BBC%s : 01 : &IF_X%s;' % (idnt, bbcn, poln)
    return assn + "_bbc_assign='" + bbc + "'"

def dcfreq(freq, side):
    '''
    ALMA bands are specified by midpoint; only the central 15/16
    of each sub-band (TFB) is used, and they overlap.
    '''
    if     side == 'U':
        dc = float(freq) - 1875.00000 / 2.0 - 62.5 / 32
    else: #side == 'L'
        dc = float(freq) + 1875.00000 / 2.0 + 62.5 / 32
    return dc

def provide_ifa(assn, idnt, poln, freq, side, ffmt):
    ifd  = ('%s* ALMA band center freq is ' + ffmt + ' MHz\n') % (
        idnt, float(freq))
    ifd += ('%sif_def = &IF_X%s : XX : %s : ' + ffmt +
        ' MHz : %s : 0 MHz : 0 Hz;') % (
        idnt, poln, poln, dcfreq(freq, side), side)
    return assn + "_if_assign='" + ifd + "'"

def provide_ifd(assn, idnt, poln, freq, side, ffmt):
    ifd  = ('%s* DC edge freq is ' + ffmt + ' MHz\n') % (idnt, float(freq))
    ifd += ('%sif_def = &IF_X%s : XX : %s : ' + ffmt +
        ' MHz : %s : 0 MHz : 0 Hz;') % (idnt, poln, poln, float(freq), side)
    return assn + "_if_assign='" + ifd + "'"

def provide_cda(assn, idnt, band, nchn, freq, side, ffmt, bbcn, chof):
    chd  = '%s* %d chans\n' % (idnt, nchn)
    chd += ('%s* ALMA center freq is ' + ffmt + ' MHz\n') % (idnt, float(freq))
    dcedge = dcfreq(freq, side)
    chd += ('%s* DC edge freq is at  ' + ffmt + ' MHz\n') % (idnt, dcedge)
    chd += ('%ssample_rate = ' + ffmt + ' Ms/sec;\n') % (idnt, 125.0)
    for ch in range(0, nchn):
        name = 'CH%02d' % (ch + 1 + chof)
        if     side == 'U':
            cfrq = dcedge + float(ch) * 62.5 * 15.0 / 16.0
        else: #side == 'L'
            cfrq = dcedge - float(ch) * 62.5 * 15.0 / 16.0
        chd += ('%schan_def = &%s : ' + ffmt +
            ' MHz : %s : 62.5 MHz : &%s : &BBC%s : &cp;\n') % (
            idnt, band, cfrq, side, name, bbcn)
    return assn + "_freq_assign='" + chd + "'"

def provide_cdf(assn, idnt, band, nchn, freq, side, ffmt, bbcn, wdth, chof):
    chd  = '%s* %d chans\n' % (idnt, nchn)
    dcedge = float(freq)
    srate = float(wdth) * 2.0
    chd += ('%s* DC edge freq is at  ' + ffmt + ' MHz\n') % (idnt, dcedge)
    chd += ('%ssample_rate = ' + ffmt + ' Ms/sec;\n') % (idnt, srate)
    for ch in range(0, nchn):
        name = 'CH%02d' % (ch + 1 + chof)
        if     side == 'U':
            cfrq = dcedge + float(ch) * float(wdth)
        else: #side == 'L'
            cfrq = dcedge - float(ch) * float(wdth)
        chd += ('%schan_def = &%s : ' + ffmt +
            ' MHz : %s : ' + ffmt + ' MHz : &%s : &BBC%s : &cp;\n') % (
            idnt, band, cfrq, side, float(wdth), name, bbcn)
    return assn + "_freq_assign='" + chd + "'"

def provide_zms(assn,idnt,freq,side,ffmt,zoom,wdth,chpa,move,rndq,verb,
    doch=False):
    '''
    Generate zoom bands by pairs around central frequency, discarding
    those exceeding min/max frequency limits.  The original code assumed
    0.0125 MHz spectral resolution (5000 pts / sub-band, keeping 4096 on
    output).  Each zoom band then has a bandwidth of 51.2 MHz, however,
    an extra factor of 4 (20000 pts/sub-band) was needed to make v2d happy.

    This version allows some freedom to experiment....

    The doch option provides an alternate return with the HOPS channel
    assigmnents suitable for the chan_ids directive.
    '''
    (minf,cenf,maxf,cnt) = map(float, zoom.split(','))
    if minf == 0 or minf < cenf - 1875.0/2.0: minf = cenf - 1875.0/2.0
    if maxf == 0 or maxf > cenf + 1875.0/2.0: maxf = cenf + 1875.0/2.0
    sep = 62.5 * 15.0 / 16.0
    hops = int(float(chpa + 0.5)*float(wdth) / 62.5)  # 4096
    zbw = 62.5 * hops / chpa    # 51.2 = 62.5 * 4096 / 5000
    fres = 62.5 / float(chpa)   # 0.01250 = 62.5 / 5000
    # number of channels that fit within a non-overlapped ALMA channel
    zrn = int(sep / zbw)
    # hif and lof are DC freq edges of the zoom channels
    hif = cenf # - 62.5 / 32.0 + fres * (chpa - zrn*hops)/2
    free = (fres * (chpa - zrn*hops) - 62.5 / 16.0)
    off = fres * int(free / (zrn*2) / fres + 0.5)
    runt = free - off * (zrn*2)
    lof = hif - sep
    zlist = []
    for p in range(int((cnt + 0.2)/2.0)):
        an = cenf + p * sep
        ax = an + sep
        for z in range(zrn):
            hl = hif + p * sep + (
                 z*zbw + (2*z+1)*off + (z+1)*runt/zrn + move*fres)
            if rndq != 0.0: hl = rndq*int(hl/rndq + 0.5)
            hu = hif + p * sep + (
                 (z+1)*zbw + (2*z+1)*off + (z+1)*runt/zrn + move*fres)
            if rndq != 0.0: hu = rndq*int(hu/rndq + 0.5)
            if verb: print(("# %f %f %f %f %f %f" % (minf,an,hl,hu,ax,maxf)))
            if hu <= ax and hl >= an and hu <= maxf: zlist.append(hl)
    for p in range(int((cnt + 0.2)/2.0)):
        an = cenf - p * sep - sep
        ax = an + sep
        for z in range(zrn):
            ll = lof - p * sep + (
                 z*zbw + (2*z+1)*off + (z+1)*runt/zrn + move*fres)
            if rndq != 0.0: ll = rndq*int(ll/rndq + 0.5)
            lu = lof - p * sep + (
                 (z+1)*zbw + (2*z+1)*off + (z+1)*runt/zrn + move*fres)
            if rndq != 0.0: lu = rndq*int(lu/rndq + 0.5)
            if verb: print(("# %f %f %f %f %f %f" % (minf,an,ll,lu,ax,maxf)))
            if lu <= ax and ll >= an and ll >= minf: zlist.append(ll)
    zlist.sort()
    if side == 'L': zlist.reverse()
    if verb: zout = (idnt + "# provide_zms(" +
        str(freq) + ';' + str(side) + ';' + str(ffmt) + ';' + str(zoom) +
        ';' + str(wdth) + ';' + str(chpa) + ';' + str(move) +")\n")
    else: zout = ''
    zout += (
        '%s# %d zoom bands in [' + ffmt + ',' + ffmt +
            '] %d zm/ch mv %g res %g\n') % (
        idnt, len(zlist), minf, maxf, zrn, move, rndq)
    r2dbe = int(2048.0 / fres + 0.5)
    alman = int(62.5 /fres + 0.5)
    sampa = float(alman) * (1.0/125000.0/(8000.0/32.0)/4.0)
    sampr = float(r2dbe) * (1.0/125000.0/8192.0/4.0)
    zout += ('%s# center ' + ffmt + ', nInt x %.15f s %.15f s\n') % (
        idnt, cenf, sampa, sampr)
    zout += ('%s# %d ch/HOPS %d ch/ALMA %d ch/R2DBE Res %.15f MHz\n') % (
        idnt, hops, chpa, r2dbe, fres)
    for z in zlist:
        zout += ('%saddZoomFreq = freq@' + ffmt + '/bw@%s/noparent@true' +
            '  #< ' + ffmt + '\n') % (idnt, z, wdth, z + float(wdth))
    zout += '%s#' % idnt
    if doch:
        chan_freqs = '    '
        for freq,item in list(zip(zlist,range(32))):
            if item % 4 == 3: sep = '\n    '
            else:             sep = ' '
            chan_freqs += (ffmt % freq) + sep
        return 'chan_ids abcdefghijklmnopqrstuvwxyzABCDEF\n' + chan_freqs
    return assn + "_zoom='\n" + zout + "\n'"

def provide_alma(o):
    '''
    This function prints out all variable assignments needed by ALMA.
    '''
    print(provide_bbc(o.assn, o.idnt, o.poln, o.bbcn))
    print(provide_ifa(o.assn, o.idnt, o.poln, o.freq, o.side, o.ffmt))
    print(provide_cda(o.assn, o.idnt, o.band, o.nchn, o.freq, o.side, o.ffmt,
        o.bbcn, o.chof))

def provide_ptwo(o):
    '''
    This function prints out all variable assignments needed by traditional
    power-of-two backends.
    '''
    print(provide_bbc(o.assn, o.idnt, o.poln, o.bbcn))
    print(provide_ifd(o.assn, o.idnt, o.poln, o.freq, o.side, o.ffmt))
    print(provide_cdf(o.assn, o.idnt, o.band, o.nchn, o.freq, o.side, o.ffmt,
        o.bbcn, o.wdth, o.chof))

def provide_vars(o):
    '''
    This function prints out all variable assignments defined.
    '''
    print(provide_bbc(o.assn, o.idnt, o.poln, o.bbcn))
    print(provide_ifa(o.assn, o.idnt, o.poln, o.freq, o.side, o.ffmt))
    print(provide_ifd(o.assn, o.idnt, o.poln, o.freq, o.side, o.ffmt))
    print(provide_cda(o.assn, o.idnt, o.band, o.nchn, o.freq, o.side, o.ffmt,
        o.bbcn, o.chof))
    print(provide_cdf(o.assn, o.idnt, o.band, o.nchn, o.freq, o.side, o.ffmt,
        o.bbcn, o.wdth, o.chof))

def provide_zoom(o):
    '''
    This function prints out just the zoom assignments
    '''
    print(provide_zms(o.assn, o.idnt, o.freq, o.side, o.ffmt, o.zoom,
        o.wdth, o.chpa, o.move, o.rndq, o.verb))

def provide_chan(o):
    '''
    This function prints out the chan_ids assignment for HOPS config files
    '''
    print(provide_zms(o.assn, o.idnt, o.freq, o.side, o.ffmt, o.zoom,
        o.wdth, o.chpa, o.move, o.rndq, o.verb, doch=True))

f1mm_help = '''
# Set this appropriately
ehtc=path-to-the-ehtc-scripts-directory

# For chan_def assignments in VEX file for ALMA:
$ehtc/alma-vex-defs.py -ralma -f213100.0 -sL -w58.0
$ehtc/alma-vex-defs.py -ralma -f215100.0 -sL -w58.0
$ehtc/alma-vex-defs.py -ralma -f227100.0 -sU -w58.0
$ehtc/alma-vex-defs.py -ralma -f229100.0 -sU -w58.0

# For zoom bands in V2D file:
$ehtc/alma-vex-defs.py -rzoom -f213100.0 -sL -w58.0
$ehtc/alma-vex-defs.py -rzoom -f215100.0 -sL -w58.0
$ehtc/alma-vex-defs.py -rzoom -f227100.0 -sU -w58.0
$ehtc/alma-vex-defs.py -rzoom -f229100.0 -sU -w58.0

# For chan_ids in HOPS config file:
$ehtc/alma-vex-defs.py -rchan -f213100.0 -sL -w58.0
$ehtc/alma-vex-defs.py -rchan -f215100.0 -sL -w58.0
$ehtc/alma-vex-defs.py -rchan -f227100.0 -sU -w58.0
$ehtc/alma-vex-defs.py -rchan -f229100.0 -sU -w58.0
'''


if __name__ == '__main__':
    '''
    Main entry point: parse command line options and call appropriate method
    '''
    o = deal_with_opts()
    if o.side != 'U' and o.side != 'L':
        print('sideband must be U or L')
        sys.exit(1)
    if o.zoom == '':
        o.zoom = '0,' + str(o.freq) + ',0,' + str(o.nchn)
    if o.report == 'help':
        print(deal_with_opts.__doc__)
    elif o.report == 'alma':
        provide_alma(o)
    elif o.report == 'ptwo':
        provide_ptwo(o)
    elif o.report == 'zoom':
        provide_zoom(o)
    elif o.report == 'vars':
        provide_vars(o)
    elif o.report == 'chan':
        provide_chan(o)
    elif o.report == 'f1mm':
        print(f1mm_help)
    sys.exit(0)

#
# eof
#
