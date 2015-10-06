#!/usr/bin/python
"""
plotDiFXPCal.py version 1.2  Jan Wagner  20151006

Usage: plotDiFXPCal.py [--pdf] [--txt] 
           [--dly=<band>,<tone>,<band>,<tone>,...]
           <output_1.difx> <station> 
           [<band>,<tone>[,<tone>,...]] [<band>,<tone>[...]] 

Plots the contents of the PCAL file of the given station,
showing amplitude and phase against time for all tones.
Currently supports the DiFX 2.4 format of PCAL files.

Options:
  --pdf      to generate PDF file of plot
  --txt      to store phases and amplitudes into a text file,
             discarding details about frequency and polarization
  --dly=...  to combine specific tones (at least two) of arbitrary
             bands in a calculation of a best-fit delay, given by 
                'delay[s] = - delta phi[rad] / 2pi delta nu[Hz]'
             and later plotted in a separate window

Arguments:
  <output_1.difx>  the DiFX output to read
  <station>        the two-letter station name

Optional arguments:
  band,tone  to select specific rather than all tone(s) of all bands,
             1,1 is the first tone in the first band

Has some similarity to 'plotpcal' from vex2difx: plotDiFXPCal.py
has no automatic tone selection, no x/y plots, but is faster (x25),
and produces optional PDF and ASCII output files. The delay
calculation allows tones from multiple subbands to be combined,
useful for subbands produced by a wideband digital filterbank.

"""

import sys, os, glob, math, cmath
import numpy, pylab

difxVersion = 240

def parsepcalfile(infile,band_tone_sel=()):
    """Loads selected (or all) PCal tones from a DiFX 2.4.x PCAL file"""

    pcalvalues = {}
    pcalids = []
    times = numpy.zeros(0)

    for line in infile:
        line = line.split()

        if (difxVersion == 240) and line[0]=='#':
            continue

        # Decode the information in the current line
        if (difxVersion == 240):
            # line = ['KY', '57092.6388948', '0.0000119', '1', '8', '16', <pcal data>]
            station = line[0]
            mjd = float(line[1])
            tint = float(line[2])*86400.0 
            dstream = max(int(line[3]), 1)
            nsubband = max(int(line[4]), 1)
            ntones = int(line[5])

            # line = ...,  '21997' 'R' '-2.03274e-05'  '9.69250e-05', ...]
            tone = line[6:]
            vals_per_tone = 4 # freq pol re im

            times = numpy.append(times, [mjd])
            if len(band_tone_sel)==0:
                selected = [(b,t) for b in range(nsubband) for t in range(ntones)]
            else:
                selected = band_tone_sel

        # Pick selected tones from current PCal line
        npol = 1 # in DiFX post-2.4.0 trunk, dstream# replaces npol in the text file... 
        for pol in range(npol):
            for (band,tonenr) in selected:
                if (tonenr >= ntones) or (band >= nsubband):
                   continue
                i = vals_per_tone * (pol*(nsubband/npol)*ntones + band*ntones + tonenr)
                pc = tone[i:(i+vals_per_tone)]
                if (pc[0] == '-1') or (pc[0] == '0'):
                   continue

                id = pc[0] + pc[1] # + ' tone ' + str(tonenr)
                if not(id in pcalvalues):
                   pcalvalues[id] = numpy.zeros(0)
                   pcalids.append(id)
                   print ('New band added: %s' % (id))
                pcalvalues[id] = numpy.append(pcalvalues[id], [float(pc[2]) + 1j*float(pc[3])])

    pcaldata = (pcalvalues,pcalids,times,tint)
    return pcaldata


def plotpcal(pcaldata,infile,band_tone_sel=(),delay_band_tone_sel=(),doPDF=False,doTxt=True):
    """
    Produces plots of amplitude and phase of the selected tones.
    If delay_band_tone_sel is non-empty, determines the group delay using these tones.
    Writes plots into PDF files if doPDF=True. 
    Writes plot data into text files if doTxt=True.
    """

    pcalvalues = pcaldata[0]
    pcalids = pcaldata[1]
    times = pcaldata[2]
    tint = pcaldata[3]

    # Settings for plot
    colormp = pylab.cm.jet(numpy.linspace(0,1,len(pcalvalues.keys())))
    markers = ['o','x','+','s','p','*','h','H','D','d']
    Nrep    = 1 + len(pcalvalues.keys()) / len(markers)
    markers = iter(markers * Nrep)
    colors  = iter(colormp)
    handles = []
    ids     = pcalids
    T       = (times - min(times)) * 86400.0  # MJD into seconds
    if False:
         # Follow alpahnumerical order, instead of user-specified order?
         ids = sorted(pcalvalues.keys())
    phstep  = float(30)

    # Actual plot
    pylab.figure(figsize=(16,6))
    pylab.gcf().set_facecolor('white')
    for id in ids:
        A = abs(pcalvalues[id])
        p = numpy.angle(pcalvalues[id])*(180.0/math.pi)
        c = next(colors)
        m = next(markers)

        pylab.subplot(211)
        h = pylab.plot(T,A, m,c=c)
        handles.append(h)

        pylab.subplot(212)
        pylab.plot(T,p,m, c=c)

    ax1 = pylab.subplot(211)
    ax1.set_xticklabels([])
    pylab.ylabel('Amplitude')
    pylab.title('PCAL data in %s' % (infile.name))

    ax2 = pylab.subplot(212)
    pylab.axis('tight')
    pylab.ylabel('Phase (deg)')
    pylab.xlabel('Time in Seconds since MJD %.6f' % min(times))

    ax1.set_xlim([min(T)-tint/2,max(T)+tint/2])
    ax2.set_xlim([min(T)-tint/2,max(T)+tint/2])

    ax1.set_ylim([0, 1.1])

    # Adjust phase axis limits to a 'phstep' granularity
    ylims2 = ax2.get_ylim()
    ylims2 = [phstep*math.floor(ylims2[0]/phstep), phstep*math.ceil(ylims2[1]/phstep)] 
    ax2.set_ylim(ylims2)

    # Cram the legend box into the figure
    pylab.subplots_adjust(left=0.05,right=0.95,bottom=0.2,top=0.90)
    box1 = ax1.get_position()
    box2 = ax2.get_position()
    ax2.set_position([box1.x0, box1.y0 - box1.height*1.05, box1.width, box2.height])
    h_leg = pylab.legend(handles,ids,loc='upper center', shadow=True,
                        bbox_to_anchor=(0.5,-0.25),ncol=4,prop={'size':12},numpoints=1)
    print ('Plotted %u averaging periods x %u tones.' % (len(T),len(ids)))

    # Optional output of plot as a PDF
    if doPDF:
        outfile = os.path.basename(infile.name) + '.pdf'
        pylab.savefig(outfile, bbox_extra_artist=[h_leg])
        print ('Saved plot to %s' % outfile)

    # Optional output of data into a text file
    if doTxt:
        outfile = os.path.basename(infile.name) + '.txt'
        f = open(outfile,'w')
        f.write('# MJD  %s\n' % (str(ids)))
        for ii in range(len(times)):
            f.write('%.7f ' % (times[ii]))
            for jj in ids:
                A = abs(pcalvalues[jj][ii])
                p = numpy.angle(pcalvalues[jj][ii])*(180.0/math.pi)
                f.write('%.3f %.1f ' % (A,p))
            f.write('\n')
        f.close()
        print ('Saved PCAL data without polarization and frequency infos into %s' % (outfile))

    # Optionally determine and plot a single fitted group delay
    if len(delay_band_tone_sel)>0:
        # Make matrix of 'phases = freq x phase[t,freq]'
        ids    = pcalids
        freqs  = numpy.zeros(0)
        phases = numpy.zeros(0) # shape later will be (Ntones,Ntimes)
        dlyids = []
        for bt in delay_band_tone_sel:
            id     = ids[band_tone_sel.index(bt)]
            f      = float(id[:-1]) * 1e6 # in Hz
            ph     = numpy.angle(pcalvalues[id])
            freqs  = numpy.append(freqs, f)
            dlyids.append(id)
            if len(phases)==0:
                phases = numpy.array(ph)
            else:
                phases = numpy.vstack([phases, ph])

        # Unwrap the phase (risky if only few tones!)
        phases = numpy.unwrap(phases,axis=0)

        # Fit slope ("delay") through freq x phase[f] at each time t
        omegas = 2*numpy.pi*freqs
        A = numpy.array([omegas, numpy.ones_like(omegas)])
        (m,b) = numpy.linalg.lstsq(A.T, phases)[0]
        dly_est = -m[:]

        # Plot the delay against time
        pylab.figure(figsize=(16,6))
        pylab.gcf().set_facecolor('white')
        pylab.plot(T, 1e9*dly_est, 'kx')
        pylab.axis('tight')
        pylab.title('Delay over %s' % (str(dlyids)))
        pylab.xlabel('Time in Seconds since MJD %.6f' % min(times))
        pylab.ylabel('Delay (nanoseconds)')
        pylab.draw()
        if doPDF:
            outfile = os.path.basename(infile.name) + '.delay.pdf'
            pylab.savefig(outfile, bbox_extra_artist=[h_leg])
            print ('Saved delay plot to %s' % outfile)
        if doTxt:
            outfile = os.path.basename(infile.name) + '.delay.txt'
            f = open(outfile,'w')
            f.write('# MJD | delay (nanosec) based on tones %s\n' % (str(dlyids)))
            for ii in range(len(times)):
                f.write('%.7f %.6f\n' % (times[ii],1e9*dly_est[ii]))
            f.write('\n')
            f.close()
            print ('Saved delay data into %s' % (outfile))

        # Plot an example of the fit at some time t0
        t0   = 0
        line = -dly_est[t0]*omegas + b[t0]
        legs = dlyids
        legs.append('LSQ fit')
        pylab.figure(figsize=(16,6))
        pylab.gcf().set_facecolor('white')
        for i in xrange(len(freqs)):
            pylab.plot(freqs[i]*1e-6, phases[i][t0]*(180/numpy.pi), 'x')
        pylab.plot(freqs*1e-6, line*(180/numpy.pi),'r-')
        pylab.gca().set_xlim([numpy.min(freqs)*0.9e-6, numpy.max(freqs)*1.1e-6])
        pylab.legend(legs)
        pylab.title('Example of fit at %d:th time sample' % (t0))
        pylab.xlabel('Frequency (MHz)')
        pylab.ylabel('Phase (deg)')
        pylab.draw()

    pylab.show()
    return


def main(argv=sys.argv):
    args = argv[1:]
    doTxt = False
    doPDF = False
    toneSel = []
    dlySel = []

    # Optional args
    while (len(args) > 0) and (args[0][0:2] == '--'):
        if args[0] == '--pdf':
            doPDF = True
        if args[0] == '--txt':
            doTxt = True
        if args[0][:6] == '--dly=':
            dlySel = [int(x)-1 for x in args[0][6:].split(',')]
            dlySel = zip(dlySel[::2], dlySel[1::2])
        args = args[1:]

    if len(args)<2:
       print __doc__
       sys.exit(1)

    # List the PCAL files (named like pcal-3_1.difx/PCAL_57092_055200_KT)
    pattern = "%s/PCAL_*_%s" % (args[0],args[1]) 
    antennafiles = glob.glob(pattern)
    if len(antennafiles) < 1:
        print "Error: no PCAL files found (pattern: %s)" % pattern
        sys.exit(1)

    # Prepare the selection of bands and tones ([] means all tones)
    if len(args)>2:
        for bt in args[2:]:
           bt = bt.split(',')
           for tt in bt[1:]:
              toneSel.append( (int(bt[0])-1, int(tt)-1) )

    # If tones were picked for delay calcs, make sure they are also in toneSel
    if len(dlySel)>0:
        for dd in dlySel:
           if dd not in toneSel:
              toneSel.append(dd)

    # Plot each file
    for af in antennafiles:
        infile = open(af, 'r')
        pc = parsepcalfile(infile,toneSel)
        plotpcal(pc,infile,toneSel,dlySel,doPDF,doTxt)
        infile.close()

if __name__ == '__main__':
    sys.exit(main())
