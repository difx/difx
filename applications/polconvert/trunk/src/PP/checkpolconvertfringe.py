#!/usr/bin/python
#
# Copyright (c) Ivan Marti-Vidal 2015-2023, University of Valencia (Spain)
#       and Geoffrey Crew 2015-2023, Massachusetts Institute of Technology
#
# Script to open and assess the POLCONVERT.FRINGE_* binary files.
# Code cribbed from TOP/task_polconvert.py around line 2550 or so.
# As usual with stupid python-numpy-pylab crap...things get rather out of
# control rather quickly.  However this ends up as a bit of a mini-fourfit.
#
'''
checkpolconvertfringe.py -- a program to check POLCONVERT.FRINGE binary files
'''

import argparse
import glob
import numpy as np
import os
import re
import struct as stk
import subprocess
import sys
import warnings

# pylab is "deprecated" so we've converted to matplotlib.* and played with
# it a bit further to try for a make for a more useful postmortem tool.
try:
    import matplotlib.pyplot as pl
except:
    try:
        import pylab as pl
    except Exception as ex:
        print('at least one of matplotlib or pylab must be available')
        raise(ex)

def formatDescription(o):
    '''
    This generates a legend below the main title.  Things to say are
    captured in o.description, and we reformat that to fit a header
    and a footer.  If the publish option is set, then we create a
    text file with the information as well
    '''
    header =  'Start:  ' + o.description['time0'] + '\n'
    header += 'Finish: ' + o.description['timex']
    footer = o.description['antennas']
    footer += "AMPs: LL %8.2f LR %8.2f RL %8.2f RR %8.2f" % (
        o.description['amps'])
    o.description['snrs'].reverse()
    footer += "\nSNRs: LL %8.2f LR %8.2f RL %8.2f RR %8.2f" % tuple(
        o.description['snrs'])
    saved = '%s.%s' % (o.name, o.ext)
    pname = '%s.%s' % (o.name, 'txt')
    return header, footer, saved, pname

def antennaBlock(legend, antprow, antdict):
    '''
    Simply doing str(antdict) is ugly; try for two equal lines
    if we have more than 8 antennas
    '''
    if antprow == 0:
        if len(antdict) > 10: antprow = (len(antdict) + 1) // 2
        else:                 antprow = len(antdict)
    text = [" %d:%s" % (ky,antdict[ky]) for ky in antdict]
    for runt in range((antprow - len(text) % antprow) % antprow):
        text.append(' '*5)
    for endr in range(1+len(text)//antprow):
        try:
            text[endr*antprow + 0] = legend + text[endr*antprow]
            text[endr*antprow + antprow-1] += '\n'
        except:
            pass
    return ''.join(text)

def findAntennaNames(o):
    '''
    Assuming we can locate the PolConvert log, the antennas show up
    in lines such as these:
    TELESCOPE AA AT X: 2225061.285 ; Y: -5440061.738 ; Z: -2481681.151
    TELESCOPE BR AT X: -2112065.351 ; Y: -3705356.500 ; Z: 4726813.606
    TELESCOPE FD AT X: -1324009.452 ; Y: -5332181.950 ; Z: 3231962.351
    ...
    and a simple grep should suffice to complete the mapping.  Apparently
    subprocess.run() is recommended if it suffices, now.
    '''
    pclog = "%s/PolConvert.log" % o.dir
    if not os.path.exists(pclog): return '??','??'
    if o.verb: print('  found',pclog)
    cmd = 'grep ^TELESCOPE....AT.X: %s' % pclog
    if o.verb: print('  running',cmd.split(' ')[0:2],'...\n')
    antennas = dict()
    try:    # CompletedProcess tells the tale
        cpro = subprocess.run(cmd.split(' '), capture_output=True)
        if cpro.returncode == 0:
            for aa,liner in enumerate(cpro.stdout.decode().split('\n')):
                if len(liner) > 10: antennas[aa+1] = liner[10:12]
    except Exception as ex:
        if o.verb: print('Unable to dig out TELESCOPE names',str(ex)) 
        antennas[o.ant1] = antennas[o.ant2] = '??'
    o.description['antennas'] = antennaBlock(o.antlegend, o.antprow, antennas)
    if o.verb: print(' ',o.description['antennas'])
    return antennas[o.ant1],antennas[o.ant2]

def getAntennaNames(o):
    '''
    Do this at the outset and save it for later use.
    '''
    try:  o.ant1,o.ant2 = map(int,o.ants.split(','))
    except: raise Exception('This is not an antenna-index pair: ' + o.ants)
    o.antenna1, o.antenna2 = findAntennaNames(o)

def dtype0(fringedata,frfile,quiet):
    '''
    The version with PANG? but not UVDIST..DiFX 2.6 through 2.8.1
    (through PolConvert version 2.0.3).  Note that PANG? is junk.
    '''
    if not quiet: print('Reading',os.path.basename(fringedata),'...',end=' ')
    alldats = frfile.read(4)
    nchPlot = stk.unpack("i", alldats[:4])[0]
    if not quiet: print('no UVDIST')
    dtype = np.dtype(
        [
            ("JDT", np.float64),
            ("ANT1", np.int32),
            ("ANT2", np.int32),
            ("PANG1", np.float64),
            ("PANG2", np.float64),
            ("MATRICES", np.complex64, 12 * nchPlot),
        ]
    )
    return dtype,nchPlot

def dtype1(fringedata,frfile,quiet):
    '''
    The version with PANG? and UVDIST..DiFX 2.8.2 (after 2.0.4)
    '''
    if not quiet: print('Reading',os.path.basename(fringedata),'...',end=' ')
    alldats = frfile.read(5)
    nchPlot,isParang = stk.unpack("i?", alldats)
    if not quiet: print('with Parang?',isParang,'w/UVDIST')
    dtype = np.dtype(
        [
            ("FILE", np.int32),
            ("JDT", np.float64),
            ("ANT1", np.int32),
            ("ANT2", np.int32),
            ("PANG1", np.float64),
            ("PANG2", np.float64),
            ("UVDIST", np.float64),
            ("MATRICES", np.complex64, 12 * nchPlot),
        ]
    )
    return dtype,nchPlot

def deducePCvers(pcdir, verb):
    '''
    Look for VERSION and make a choice....
    '''
    pclog = pcdir + '/PolConvert.log'
    if not os.path.exists(pclog): raise Exception('No file ' + pclog + 'to examine.')
    cmd = 'grep VERSION %s' % pclog
    if verb: print('  running',cmd.split(' ')[0:2],'... PolConvert.log')
    try:    # CompletedProcess tells the tale
        cpro = subprocess.run(cmd.split(' '), capture_output=True)
        if cpro.returncode == 0:
            versio = re.sub(r'.*VERSION ','', cpro.stdout.decode().split('\n')[0])
            if versio >= '2.0.5': vers = '1'
            else:                 vers = '0'
            if verb: print('  version string from log:', versio,'using "-V ',vers,'"\n')
            return vers
    except Exception as ex:
        print("Unable to work out a good choice for -V argument; try -V help")
        raise(ex)

def examineFRINGE_IF(pli, o):
    '''
    pli is the index of the file, so .../POLCONVERT.FRINGE_IF??
    is expected to hold some binary data this task will try to
    unpack it and report on what it holds.  Options in o affect
    what it does with the data.  The FRINGE data file holds records
    by time for antenna pairs (including only baselines to the
    "plotAnt") and for that a matrix of unconverted, converted
    and the conversion matrices.
    '''
    if o.withIF: ifs = 'IF'
    else:        ifs = ''
    fringedata = "%s/POLCONVERT.FRINGE/POLCONVERT.FRINGE_%s%i" % (
        o.dir,ifs,pli)
    o.thisIF = pli
    frfile = open(fringedata,"rb")
    if o.pcvers == '': o.pcvers = deducePCvers(o.dir, o.verb)
    if o.pcvers == '0': dtype,nchPlot = dtype0(fringedata,frfile,o.quiet)
    elif o.pcvers == '1': dtype,nchPlot = dtype1(fringedata,frfile,o.quiet)
    else: raise Exception('Unsupported fringe version ' + o.pcvers)
    o.nchPlot = int(nchPlot)
    try:
        fringe = np.fromfile(frfile,dtype=dtype)
        frfile.close()
    except Exception as ex:
        raise Exception('Unable to read fringe',str(ex))
    if o.verb and not o.quiet: print(' ',os.path.basename(fringedata),
        'has ',len(fringe),'time-baseline samples and',o.nchPlot,'channels')
    x = len(fringe)-1
    if o.pcvers == '1':
        file0 = fringe[0]['FILE']
        fileX = fringe[x]['FILE']
    else:
        file0 = fileX = '--'
    o.description['time0'] = 'JDT %f s = %s'%jdt(fringe[0]['JDT'])
    o.description['timex'] = 'JDT %f s = %s'%jdt(fringe[x]['JDT'])
    if not o.quiet:
        print('  [%04d] File:'%0,file0, o.description['time0'])
        print('  [%04d] File:'%x,fileX, o.description['timex'])
    ant1set = set(list(fringe[:]["ANT1"]))
    ant2set = set(list(fringe[:]["ANT2"]))
    if o.verb: print('  ANT1: ', ant1set, ', ANT2: ',ant2set)
    maxUVDIST = ''
    if o.pcvers == '1' and o.verb and not o.quiet:
        maxUVDIST = (
            ' max UVDIST %f'%np.max(fringe[:]["UVDIST"]) + '(units unknown)')
        print('  PANG1: %.2f'%np.rad2deg(np.min(fringe[:]["PANG1"])),
            '.. %.2f'%np.rad2deg(np.max(fringe[:]["PANG1"])),
            '  PANG2: %.2f'%np.rad2deg(np.min(fringe[:]["PANG2"])),
            '.. %.2f'%np.rad2deg(np.max(fringe[:]["PANG2"])),
            ' (deg);\n', maxUVDIST)
    if o.ant1 in ant1set and o.ant2 in ant2set:
        if o.verb and not o.quiet:
            print('  Prepping data on baseline', o.ant1, '(', o.antenna1, ')',
            'to', o.ant2, '(', o.antenna2, ') for plot')
        AntEntry1 = np.logical_and(
            fringe[:]["ANT1"] == o.ant1,fringe[:]["ANT2"] == o.ant2)
        AntEntry2 = np.logical_and(
            fringe[:]["ANT2"] == o.ant1,fringe[:]["ANT1"] == o.ant2)
        AntEntry = np.logical_or(AntEntry1,AntEntry2)
        if np.sum(AntEntry)>0:
            # this is the polconverted data
            cal12 = [ (fringe[AntEntry]["MATRICES"])[:,i::12]
                for i in range(4,8)]
            # this is the number of delay rate channels for the baseline:
            # typically this is (time-baseline samples) / (number antennas)
            o.rchan = np.shape(cal12[0])[0]
            return prepPlot(cal12, pli, o)
        else:
            raise Exception("No data on %d--%d baseline" % (ant1,ant2))
    else:
        print(ant1 in ant1set,ant2 in ant2set)
    raise Exception("The antenna pair %s has no data?" % o.ants)

def jdt(jdts):
    '''
    Apparently the unit is (Modified) Julian Date in seconds.
    The time origin for that is 11 Nov 1858.  It is not clear
    what way to code this is least likely to lose precision.
    '''
    import datetime
    dt = datetime.timedelta(seconds=jdts)
    d0 = datetime.datetime(1858,11,17)
    iso = (d0+dt).isoformat()
    return(jdts, iso)

def prepPlot(cal, plif, o):
    '''
    Ok, now replicate the steps of task_polconvert.py for fringe
    plotting.  Ideally, we want to head towards a combined fringe
    across IFs such as fourfit does so that weaker fringes gain
    in significance.  The np array cal holds the polconveted
    data and other things of interest are in o.
    '''
    # Fringes in delay-rate space: double fft of the "cal" data
    # fftshift swaps half-spaces so that the 0 freq at the center and
    # by default it does this for both axes.  fft2 does a 2dim FFT.
    RRVis = np.fft.fftshift(np.fft.fft2(cal[0]))
    RLVis = np.fft.fftshift(np.fft.fft2(cal[1]))
    LRVis = np.fft.fftshift(np.fft.fft2(cal[2]))
    LLVis = np.fft.fftshift(np.fft.fft2(cal[3]))
    # amplitudes
    RR = np.abs(RRVis)
    RL = np.abs(RLVis)
    LR = np.abs(LRVis)
    LL = np.abs(LLVis)
    # locate max peak
    RMAX = np.unravel_index(np.argmax(RR+LL),np.shape(RRVis))
    MAXVis = np.array([RRVis[RMAX],RLVis[RMAX],LRVis[RMAX],LLVis[RMAX]])
    MAXl = np.array([RR[RMAX],RL[RMAX],LR[RMAX],LL[RMAX]])
    MAX = max(MAXl)
    o.description["amps"] = (LL[RMAX],LR[RMAX],RL[RMAX],RR[RMAX])
    print("  IF%d peaks at %s < +/-[%d,%d] with (RR,RL,LR,LL) Vis:" %
        (o.thisIF, repr(RMAX), int(o.rchan), int(o.nchPlot)))
    if o.verb: print('  ', MAXVis)
    print('  ', MAXl, '; overall max |Vis|: %f\n'%float(MAX))
    # provide the data actually needed for a combined plot
    return [ RR, RL, LR, LL, float(MAX), RMAX, plif, MAXl ]

def scaleAlias(scale):
    if scale == 'loge': scale = 'elog'
    return scale

def setScaling(scale):
    '''
    In theory one can be quite creative here...;
    return a function and a sensible min for it.
    '''
    scale = scaleAlias(scale)
    if   scale == 'elog': scalor = np.log
    elif scale == 'log10': scalor = np.log10
    elif scale == 'linear': scalor = lambda x:x
    elif scale == 'sqrt': scalor = np.sqrt
    else: raise Exception("scale option %s not defined (set)" % (scale))
    return scalor

def invScaling(scale):
    '''
    And then provide an inverse so that cb range can be known
    '''
    scale = scaleAlias(scale)
    if   scale == 'elog': scalor = np.exp
    elif scale == 'log10': scalor = lambda x:np.power(10.0, x)
    elif scale == 'linear': scalor = lambda x:x
    elif scale == 'sqrt': scalor = np.square
    else: raise Exception("scale option %s not defined (inv)" % (scale))
    return scalor

def plotMinimum(scale, samdev, count, sigma):
    '''
    Choose a sensible minimum; log are the only tricky case since
    log (0) is not a good thing.
    '''
    scale = scaleAlias(scale)
    if   scale == 'elog': minimum=float(np.log(samdev/np.sqrt(count))*sigma)
    elif scale == 'log10': minimum=float(np.log10(samdev/np.sqrt(count))*sigma)
    elif scale == 'linear': minimum = 0.0
    elif scale == 'sqrt': minimum = 0.0
    else: raise Exception("scale option %s not defined (min)" % (scale))
    return minimum

def avePeakPositions(plotdata):
    count = 0
    for pd in plotdata:
        if count == 0: peaks = pd[5]
        else: peaks = np.add(peaks, pd[5])
        count += 1
    peaks = np.divide(peaks, count)        
    return "(delay %.1f, delay rate %.1f)"%(float(peaks[1]),float(peaks[0]))

def sampleDevFromPlotdata(plotdata, ylim, xlim):
    '''
    Estimate the sample deviation from parts of the images away from
    the peaks.  If we grab some samples from the 4 corners of every
    plot, make a list and pick the median, we are very likely ok.
    '''
    samples = list()
    for pd in plotdata:
        for vi in range(4):
           samples.append(np.std(pd[vi][1:ylim,1:xlim].flatten()))
           samples.append(np.std(pd[vi][1:ylim,xlim:-1].flatten()))
           samples.append(np.std(pd[vi][ylim-1,xlim:-1].flatten()))
           samples.append(np.std(pd[vi][ylim-1,1:xlim].flatten()))
    samedian = np.median(np.array(samples))
    return samedian

def padSlice(mn, cen, mx, pnd, xtra):
    '''
    Some stupid index games: min, max and pad used below...
    '''
    before = after = 0
    pnd += xtra
    if   pnd > cen: after  = pnd - cen
    elif pnd < cen: before = cen - pnd
    thismin = mn + after
    thismax = mx + after
    padding = (before+xtra, after+xtra)
    return thismin, thismax, padding

def computeSNRs(vizzy, maximum, count, samdev, nsigma, scale, fwdscalor):
    '''
    Return a list of the estimated SNRs for the 4 product images in vizzy
    and the scaled maximum and minimum for in the image arrays.  The maximum
    should be the total max/count, the mimimum is zero or sigma-scaled stdev.
    Note however we are starting with abs(vis), which is perhaps Raleigh
    distributed, so the sample deviation computed and passed to us will
    underestimate the true std dev by sqrt(2-pi/2) or 0.655136377562
    '''
    maximum = fwdscalor(maximum/count)
    minimum = plotMinimum(scale, samdev, count, nsigma)
    invscalor = invScaling(scale)
    SNRs = np.array(range(4))
    ocmpmax = cmpmax = invscalor(minimum)
    for ii,vis in enumerate(vizzy):
        # recover unscaled max on this visibility
        npmaxvis = np.max(vis)
        if npmaxvis > cmpmax: cmpmax = npmaxvis
        maxvis = float(invscalor(npmaxvis))
        # generate SNRs of the combined data -- attempting to correct...
        SNRs[ii] = ((maxvis / samdev) *
            float(np.sqrt(count)) * 0.655136377562)
    return SNRs, minimum, maximum

def parseFringeRequest(fringe, verb):
    '''
    The o.fringe var is npix[,xtra[,sigma]] but split gets upset.
    The plots look nicer when npix is odd when there is a fringe.
    If npix is larger than the amount of data, we need xtra nonzero
    '''
    npix,xtra,sigma,junk = (o.fringe + ',,,').split(',',maxsplit=3)
    npix = 2*int(int(npix)/2.0) + 1
    if xtra == '':
        xtra = npix//2
    if sigma == '': sigma = 1.0
    xtra = int(xtra)
    sigma = float(sigma)
    if o.verb: print('  npix,xtra,sigma: ',npix,xtra,sigma)
    return npix, xtra, sigma

def combinePlotdata(plotdata, o):
    '''
    Should have been given list of plotdata tuples (per IF).  Combine
    them and make a 2x2 image plot centered around the peaks +/- npix,
    which we do by padding with np.pad and then slicing out npix around
    the new center.  We also add xtra padding so that if there is not
    much data, we still get some approximation of the original npix.
    Returns the things to be plotted.
    '''
    npix,xtra,sigma = parseFringeRequest(o.fringe, o.verb)
    xcen = int((o.nchPlot+2*xtra)/2)
    ycen = int((o.rchan+2*xtra)/2)
    wind = min(npix, xcen, ycen)
    xmin, xmax = (xcen - wind, xcen + wind + 1)
    ymin, ymax = (ycen - wind, ycen + wind + 1)
    # these should all be the same if it is a real fringe
    truecenter = avePeakPositions(plotdata)
    # sample median of the original np.abs(visibilities)
    samdev = sampleDevFromPlotdata(plotdata,
        min(npix,ycen)//3, min(npix,xcen)//3)
    if o.verb: print(('  %s plot %dx%d on %d peaks at %s') % (
        o.scale, 2*wind+1,2*wind+1, len(plotdata), truecenter))
    count = 0
    AMPs = np.zeros(4)
    for pd in plotdata: # RR,RL,LR,LL,  4:MX, 5:RMAX, 6:IF,  7:MAXl
        # note that y indices precede x indices
        pndy,pndx = pd[5]
        thismax = pd[4]
        AMPs = np.add(AMPs, pd[7])
        # if are multiple peaks, this is definitely not a droid we want
        if not (type(pndx) is np.int64 and type(pndy) is np.int64 and
            thismax > 0.0):     # there better be a peak somewhere
            print(' No single max from',pd[6],'so we shall ignore it')
            continue
        # accumulate a maximum value
        if count == 0: maximum = thismax
        else:          maximum += thismax
        # pad the sides so that a slice window puts the peak at the center
        thisxmin,thisxmax,xpadding = padSlice(xmin,xcen,xmax,int(pndx),xtra)
        thisymin,thisymax,ypadding = padSlice(ymin,ycen,ymax,int(pndy),xtra)
        window = np.s_[thisymin:thisymax,thisxmin:thisxmax]
        pad_width = ( ypadding, xpadding )
        vis = list()
        # finally generate the centered, sliced images
        for vi in range(4):
            vis.append(np.pad(pd[vi], pad_width, mode='constant',
                constant_values=samdev/10.0)[window])
            if count > 0:
                vizzy[vi] = np.add(vizzy[vi], vis[vi])
        if count == 0: vizzy = vis
        count += 1
    if count == 0:
        raise Exception("Nothing to plot?")
    # average and scale
    AMPs = np.divide(AMPs, float(count))
    scalor = setScaling(o.scale)
    for vi in range(4): vizzy[vi] = scalor(np.divide(vizzy[vi], float(count)))
    # compute SNRs and scaled image max,min
    SNRs, minimum, maximum = computeSNRs(
        vizzy, maximum, count, samdev, sigma, o.scale, scalor)
    #   vizzy, scalor(maximum/count), count, samdev, sigma, o.scale, scalor)
    o.description['snrs'] = list(SNRs)
    invscalor = invScaling(o.scale)
    print('  SNRs on',o.ants,'(%s && %s)'%(o.antenna1,o.antenna2),
        SNRs,'\n  %s|Vis| data e [%.2f..%.2f] +/- %.3f (std.dev)'%(
        o.scale, invscalor(minimum), invscalor(maximum), samdev))
    # return plot products; all should have same ratio, so use first
    ratio = vizzy[0].shape[1] / vizzy[0].shape[0]
    return vizzy, [minimum, maximum], ratio, SNRs, AMPs

def plotProcessing(plotdata, o):
    '''
    Combine the plotdata tuples into abs(visibility), the mx val.
    Note that we have reversed the order of visibilities to be
    the canonical alphabetical one.  The 'constrained' layout will
    prevent the axis labels from getting buried, but it then doesn't
    leave much control over placement of other things.
    '''
    vis, vxn, ratio, SNRs, AMPs = combinePlotdata(plotdata, o)
    lab = [ 'RR','RL','LR','LL' ]
    scalor = invScaling(o.scale)
    cbrange = '..'.join(list(map(lambda x:"%.2f"%x, scalor(vxn))))
    pl.ioff()
    # lengendary--blank lines in tile opens space for header block
    # space for footer is created by padding (above) the colorbar
    fig, axs = pl.subplots(2, 2, figsize=(8.5,11), layout='constrained',
        subplot_kw={'xticks':[], 'yticks':[]})
    fig.suptitle(('Averaged Fringes (IFs: %s)' % ','.join(o.ifused)) +
        '\nJob: ' + o.job + '   Vis(' + o.antenna1 + ' && ' + o.antenna2 + ')'
        + "\n\n\n", fontsize=14)   # open up space for header
    props = dict(boxstyle='round', facecolor='snow', alpha=1.0)
    header,footer,saved,pname = formatDescription(o)
    fig.text(0.5, 0.930, header, fontsize=10, fontfamily='monospace',
        ha='center', va='center', wrap=True, bbox=props)
    fig.text(0.51, 0.130, footer, fontsize=10, fontfamily='monospace',
        ha='center', va='center', wrap=True, bbox=props)
    # subplots
    for row in range(2):
        for col in range(2):
            ndx = 2*(1-row)+(1-col)
            if o.verb:
                print('  Vis[',ndx, lab[ndx],'] range',cbrange,
                '% 7.2f Amp %.1f'% (SNRs[ndx], AMPs[ndx]))
            ax = axs[row, col]
            ax.set_title(lab[ndx] + ' Vis., SNR %5.1f Amp %.1f' % 
                (SNRs[ndx], AMPs[ndx]))
            ax.set_xlabel('delay\n')
            ax.set_ylabel('delay rate')
            im = ax.imshow(vis[ndx], vmin=vxn[0], vmax=vxn[1],
                interpolation='nearest', cmap=pl.cm.viridis, origin='lower')
    # common colorbar, with updated labels for scaling: replace('−','-')
    cbar = fig.colorbar(im, ax=axs,
        label='('+o.scale+'-scaled) |Vis(LL,LR,RL,RR)|',
        location='bottom', shrink=0.60, pad=0.12)
    ttt = ['%.0f'%scalor(float(text.get_text().replace('\u2212','\u002d')))
        for text in cbar.ax.get_xticklabels()]
    warnings.filterwarnings(action='ignore', category=UserWarning)
    cbar.ax.set_xticklabels(ttt)
    fig.savefig(saved)
    plotCoda(header, footer, saved, pname, o)
    return 0

def plotCoda(header, footer, saved, pname, o):
    '''
    Tell the human about what is now available
    '''
    if o.publish:
        fp = open(pname, 'w')
        fp.write('Job: ' + o.job + '\n');
        fp.write('Stamp: ' + o.stamp + '\n');
        fp.write('Ants: ' + str(o.ant1) + ' v ' + str(o.ant2) + '\n');
        fp.write(header + '\n')
        fp.write(footer + '\n')
        fp.close()
        print("  text: '%s'" % pname)
    print("  plot: '%s'" % saved)
    if o.viewer != '':
        cmd = '%s %s.%s &' % (o.viewer, o.name, o.ext)
        print('  ' + o.viewer + ' ....' + o.ext + ' launched')
        os.system(cmd)

def parseJobStamp(o):
    '''
    It is somewhat convenient to parse the dirname for correlation
    job number as well as timestamp (for plot labels). Do that now.
    And this is a good place to check stupid stuff.
    '''
    if not os.path.exists(o.dir):
        raise Exception("Directory %s does not exist" % o.dir)
    if not os.path.exists(o.dir + '/POLCONVERT.FRINGE'):
        raise Exception("No POLCONVERT.FRINGE subdir to %s" % o.dir)
    o.description = {}
    getAntennaNames(o)
    if o.name == '':
        if o.antenna1+o.antenna2 == '????':
            o.name = 'checkFringe-%d-%d' % (o.ant1,o.ant2)
        else:
            o.name = 'checkFringe-%s-%s' % (o.antenna1,o.antenna2)
    if o.publish:
        o.name = o.dir + '/' + o.name
    if o.verb: print('plotting to', o.name)
    try:
        parts = o.dir.split('.polconvert-')
        o.job = parts[0]
        o.stamp = parts[1]
    except Exception as ex:
        print(str(ex))
        o.job = ''
        o.stamp = ''

def parseIFarg(o):
    '''
    Convert the IF input option to a list of IFs to examine.
    We also make sure that the named IFs have data to plot.
    '''
    iflist = list()
    targetdir = "%s/POLCONVERT.FRINGE" % o.dir
    dirdir = os.path.dirname(o.dir)
    if len(dirdir) > 0: dirdir = '\n ' + dirdir + '/'
    if o.verb: print('Locating fringes in:%s\n  %s' %
        (dirdir, os.path.basename(o.dir)))
    # POLCONVERT.FRINGE_* initially, later POLCONVERT.FRINGE__IF*
    o.withIF = None
    for frng in sorted(glob.glob("%s/*FRINGE_IF*" % targetdir)):
        o.withIF = True
        iflist.append(frng[-2:])
        if o.verb: print('   ',os.path.basename(frng),'as IF',iflist[-1])
    if o.withIF is None:
      for frng in sorted(glob.glob("%s/*FRINGE_*" % targetdir)):
        iflist.append(frng[-2:])
        if o.verb: print('   ',os.path.basename(frng),'as IF',iflist[-1])
        o.withIF = False
    # if no selection provide full list
    if o.IF == '': return iflist
    # else make a cut to those requested
    ifcull = list()
    for iffy in o.IF.split(','):
        if iffy in iflist: ifcull.append(iffy)
    if o.verb: print(' limiting actions to these IFs:', ','.join(ifcull),'\n')
    if len(ifcull) == 0: print('No IFs match: -I',o.IF,'choose wisely.')
    return ifcull

def getVersion():
    '''
    There has to be a better solution than editing all the files.
    '''
    try:
        import pcvers
        return pcvers
    except:
        return 'not available'
    return 'total failure'

def parseOptions():
    '''
    While converting data, PolConvert writes out binary data
    which it uses to either support solving for the XY phase
    or merely to plot fringes.  This program examines those
    binary files and reports on what it finds.
    '''
    des = parseOptions.__doc__
    epi =  'In the typical case, you may have run PolConvert, '
    epi += 'something did not work, and you wish to verify that '
    epi += 'binary fringe files written by PolConvert, are ok (or not). '
    epi += 'For this you need at least the -d *polconvert* argument '
    epi += 'which will located the PolConvert.log and the binary '
    epi += 'fringe files.  The remaining arguments controls what '
    epi += '(exactly) is done with those files. '
    epi += 'Use -f "example" for sample invocations.'
    use = '%(prog)s [options]\n\nVersion ' + getVersion()
    parser = argparse.ArgumentParser(epilog=epi, description=des, usage=use)
    major = parser.add_argument_group('Major Options')
    minor = parser.add_argument_group('Minor Options')
    picky = parser.add_argument_group('Picky Options')
    major.add_argument('-d', '--dir', dest='dir',
        default='.', metavar='DIR', help='(Mandatory) Path to '
        'the polconvert output directory.  In production processing, '
        'that is $job.polconvert-$timestamp')
    major.add_argument('-I', '--IF', dest='IF',
        default='', metavar="IF", help='This controls the IFs '
        'that will be considered.  If unset, all IFs in the '
        'directory are examined.  You may also supply a comma-sep '
        'list of IF numbers to process.')
    major.add_argument('-a', '--antennas', dest='ants',
        default='1,2', metavar='ANT1,ANT2', help='Indicies for the'
        ' pair of antennas to use for subsequent checking.  Ideally,'
        ' the first one is a linear station (ALMA) and the second'
        ' is a short baseline to a good station.')
    major.add_argument('-f', '--fringe', dest='fringe',
        default='', help='String to configure fringing checks.'
        ' Use "help" as an argument for more information; reminder:'
        ' npix,pads,sigma')
    major.add_argument('-P', '--publish', dest='publish',
        default=False, action='store_true', help='place results in'
        ' the -d directory (a graphic and a text file).')
    #
    minor.add_argument('-v', '--verbose', dest='verb',
        default=False, action='store_true',
        help='be chatty about the work')
    minor.add_argument('-q', '--quiet', dest='quiet',
        default=False, action='store_true',
        help='this is useful if you are playing with the plots'
            ' and no longer need to see the fringe file details.')
    minor.add_argument('-n', '--name', dest='name',
        default='', help='Basename for any plot generated.  If no name'
        ' is supplied, one will be created for you based on the baseline.')
    minor.add_argument('-V', '--pcvers', dest='pcvers',
        default='', help='Fringe file version: 1 = 2.0.5 and later'
        ' (with UVDIST), 0 = 2.0.3 and earlier (without UVDIST); or "help"'
        ' to print out a more complete explanation.')
    minor.add_argument('-s', '--scale', dest='scale',
        default='log10', help='One of "elog" (or "loge"),'
        ' "log10" (the default), "linear", "sqrt".  Use "help" '
        ' for more information.')
    minor.add_argument('-g', '--viewer', dest='viewer',
        default='', help='Name of graphic display tool, e.g.'
        ' eog, okular.... The default is "" to just make a PNG'
        ' file (see -n) and to not display it.')
    minor.add_argument('-e', '--extension', dest='ext',
        default='png', metavar='EXT', help='Graphics extension for'
        ' the file produced: png (default), pdf, ... (Cf. Matplotlib).')
    #
    picky.add_argument('-p', '--precision', dest='prec', type=int,
        default=3, metavar=int,
        help='Precision for numpy printing if verbosity active')
    picky.add_argument('-t', '--threshold', dest='thres', type=int,
        default=20, metavar=int,
        help='Threshold for numpy printing if verbosity active')
    picky.add_argument('-w', '--linewidth', dest='width', type=int,
        default=78, metavar=int,
        help='Linewidth for numpy printing if verbosity active')
    picky.add_argument('-L', '--antenna-legend', dest='antlegend',
        default='Ant. Map:', help='Number of lines in antenna map legend.')
    picky.add_argument('-M', '--antenna-per-row', dest='antprow',
        default='0', type=int,
        help='Number of antennas per row in map legend.')
    return parser.parse_args()

def pcvershelp():
    return '''
    The fringe file is binary packed for numpy to read it
    The early versions had parallactic angles (not implemented)
    and as of 2.0.5 (targetted for DiFX 2.8.2), UVDIST was added.
    Use -V 0 for the earlier format and -V 1 for the later one.
    The default is to examine the PolConvert.log and make a choice.
    '''
def fringehelp():
    return '''
    Normally polconvert generates plots of before and after the
    polconversion...with a zoom into "npix" around the peak.  An
    issue is that if fringes are weak, the peak is not enough to
    work with.  If the 'fringe' argument is not empty, it is parsed
    first to supply npix.  Then ALL the IFs mentioned in the -I
    argument are combined into an average image, and the result is
    plotted for a window around npix.  The image may need to be
    padded and you can also scale it.  Use -f "more" to find out
    about that.
    '''
def fringemore():
    return '''
    After npix, a comma and a second argument indicates the amount
    of padding you want.  If the delay or delay-rate is at the edge,
    you will need to pad in order to combine the images.  (If you
    do not supply a pad, the code will try and may fail...)  A
    third argument affects the image range...use -s "help" for more
    about that.
    '''
def scalehelp():
    return '''
    For some of the scalings, a zero minimum is safe to use.  For
    log-scaled plots, however, you'll need to set the minimum.  The
    final fringe argument specifies the number of sigma to multiply
    the noise floor by for the minimum.  The default is 1.0, but
    you can do as you like.
    '''
def fringeexam():
    return '''
    To generate a plot from the fringe data in pdir for the 1,8 antenna
    baseline pair, put the result in pdir and display the result with eog:

    checkpolconvertfringe.py -d pdir -a 1,8 -g eog -f 50 -P

    A more verbose version with padding of 25 pixels and a 0.1-sigma
    noise floor:

    checkpolconvertfringe.py -d pdir -a 1,8 -g eog -f 50,25,0.1 -P
    '''
def somehelp(o):
    if o.pcvers == 'help':
        print(pcvershelp())
        return True
    if o.fringe == 'help':
        print(fringehelp())
        return True
    if o.fringe == 'more':
        print(fringehelp())
        return True
    if o.fringe == 'example':
        print(fringeexam())
        return True
    if o.scale == 'help':
        print(scalehelp())
        return True
    return False

#
# enter here to do the work
#
if __name__ == '__main__':
    if sys.version_info.major < 3:
        print('Sorry, this code works in Python3 only.')
        sys.exit(1)
    o = parseOptions()
    if somehelp(o): sys.exit(0)
    if o.verb:
        print('\nprinting with %d precision, %d elements, %d width' % (
            o.prec, o.thres, o.width))
    np.set_printoptions(
        precision=o.prec, threshold=o.thres, linewidth=o.width)
    errors = 0
    plotdata = list()
    parseJobStamp(o)
    o.ifused = parseIFarg(o)
    print("\nOpening POLCONVERT.FRINGE files\n")
    for pli in o.ifused:
        try:
            plotdata.append(examineFRINGE_IF(int(pli), o))
        except Exception as ex:
            print("Unable to read IF %d successfully"%int(pli))
            print("Exception was:\n",str(ex))
            errors += 1
    print("Have plotting data for %d fringes"%len(plotdata))
    if (o.fringe != ''):
        try:
            errors += plotProcessing(plotdata, o);
        except Exception as ex:
            print("Unable to make a plot")
            print("Exception was:\n",str(ex))
            errors += 1
    if errors > 0:
        print('\nall done with',errors,'errors')
        sys.exit(errors)
    else:
        print('\nall done with no errors')
    sys.exit(0)

#
# eof vim: set nospell:
#
