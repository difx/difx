#!/usr/bin/python
"""
plotDiFXPCal.py version 1.0  Jan Wagner  20150508

Usage: plotDiFXPCal.py <output_1.difx> <station> [<band>,<tone>]
                       [<band>,<tone>] [...]

Currently supports the DiFX 2.3.0 format of PCAL files.

Plots the contents of the PCAL file of the given station,
showing amplitude and phase against time for all tones.

Optionally, one or several pairs of band number (1..n) and
tone number (1..t) can be specified to limit plotting to
a particular subset of PCal tones.
"""

import sys, os, glob, math, cmath
import numpy, pylab

pcalVersion = 230

def parsepcalfile(infile,band_tone_sel=()):

    pcalvalues = {}
    times = numpy.zeros(0)

    for line in infile:
        line = line.split()

        if (pcalVersion == 230) and line[0]=='#':
            # Skip comments in PCal Version 1 file
            continue

        if (pcalVersion == 230):
            # line = ['KY', '57092.6388948', '0.0000119', '1', '8', '16', <pcal data>]
            station = line[0]
            mjd = float(line[1])
            tint = float(line[2])*86400.0 
            npol = int(line[3])
            nsubband = int(line[4])
            ntones = int(line[5])
            # line = ...,  '21997' 'R' '-2.03274e-05'  '9.69250e-05', ...]
            tone = line[6:]

            vals_per_tone = 4
            times = numpy.append(times, [mjd])
            if len(band_tone_sel)==0:
                selected = [(b,t) for b in range(nsubband/npol) for t in range(ntones)]
            else:
                selected = band_tone_sel

        for pol in range(npol):
            for (band,tonenr) in selected:
                i = vals_per_tone * (pol*(nsubband/npol)*ntones + band*ntones + tonenr)
                pc = tone[i:(i+vals_per_tone)]

                id = pc[0] + pc[1] # + ' tone ' + str(tonenr)
                if not(id in pcalvalues):
                   pcalvalues[id] = numpy.zeros(0)
                pcalvalues[id] = numpy.append(pcalvalues[id], [float(pc[2]) + 1j*float(pc[3])])

                #phase = math.atan2(float(pc[3]),float(pc[2])) * (180/math.pi)
                #print 'pol=%u band=%u tone=%u ' % (pol,band,tonenr),
                #print ': f=%s %s-pol ph=%f' % (pc[0],pc[1],phase)

    # Settings for plot
    colors  = iter(pylab.cm.jet(numpy.linspace(0,1,len(pcalvalues.keys()))))
    markers = ['o','x','+','s','p','*','h','H','D','d']
    Nrep    = 1 + len(pcalvalues.keys()) / len(markers)
    markers = iter(markers * Nrep)
    handles = []
    ids     = sorted(pcalvalues.keys())
    T       = (times - min(times)) * 86400.0  # MJD into seconds
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

        # print '%s : %s' % (id, str(p))

    ax1 = pylab.subplot(211)
    ax1.set_xticklabels([])
    pylab.axis('tight')
    pylab.ylabel('Amplitude')
    pylab.title('PCAL data in %s' % (infile.name))

    ax2 = pylab.subplot(212)
    pylab.axis('tight')
    pylab.ylabel('Phase (deg)')
    pylab.xlabel('Time in Seconds since MJD %.6f' % min(times))

    ax1.set_xlim([min(T)-tint/2,max(T)+tint/2])
    ax2.set_xlim([min(T)-tint/2,max(T)+tint/2])

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

    outfile = os.path.basename(infile.name) + '.pdf'
    pylab.savefig(outfile, bbox_extra_artist=[h_leg])
    print ('Saved plot to %s' % outfile)

    pylab.show()
    return


def main(argv=sys.argv):
    if len(argv)<3:
       print __doc__
       sys.exit(1)

    # List the PCAL files (named like pcal-3_1.difx/PCAL_57092_055200_KT)
    pattern = "%s/PCAL_*_%s" % (argv[1],argv[2]) 
    antennafiles = glob.glob(pattern)
    if len(antennafiles) < 1:
        print "Error: no PCAL files found (pattern: %s)" % pattern
        sys.exit(1)

    sel = []
    if len(argv)>3:
        for bt in argv[3:]:
           bt = bt.split(',')
           sel.append( (int(bt[0])-1, int(bt[1])-1) )

    for af in antennafiles:
        infile = open(af, 'r')
        parsepcalfile(infile,sel)
        infile.close()

if __name__ == '__main__':
    sys.exit(main())
