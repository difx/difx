#!/usr/bin/python
"""
plotDiFXPCal.py version 1.0  Jan Wagner  20150508

Usage: plotDiFXPCal.py [--pdf] [--txt] <output_1.difx> <station> 
                       [<band>,<tone>[,<tone>,...]] [<band>,<tone>[...]] 

Currently supports the DiFX 2.4 format of PCAL files.

Plots the contents of the PCAL file of the given station,
showing amplitude and phase against time for all tones.

Arguments:
  <output_1.difx>  the DiFX output to read
  <station>        the two-letter station name

Optional arguments;
  --pdf      to generate PDF file of plot
  --txt      to store phases and amplitudes into a text file,
             discarding details about frequency and polarization
  band,tone  to select specific tone(s) of a band rather than all
             the first tone in the first band is 1,1

Has some similarity to 'plotpcal' from vex2difx: plotDiFXPCal.py
has no automatic tone selection, and lacks x/y and delay plots,
but is much faster, and produces optional PDF and ASCII output files.

"""

import sys, os, glob, math, cmath
import numpy, pylab

difxVersion = 240

def parsepcalfile(infile,band_tone_sel=(),doPDF=False,doTxt=True):
    """Loads selected (or all) PCal tones from a DiFX 2.4.x PCAL file"""

    pcalvalues = {}
    times = numpy.zeros(0)

    for line in infile:
        line = line.split()

        if (difxVersion == 240) and line[0]=='#':
            continue

        if (difxVersion == 240):
            # line = ['KY', '57092.6388948', '0.0000119', '1', '8', '16', <pcal data>]
            station = line[0]
            mjd = float(line[1])
            tint = float(line[2])*86400.0 
            npol = max(int(line[3]), 1)
            nsubband = max(int(line[4]), 1)
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
                if (tonenr >= ntones) or (band >= nsubband):
                   continue
                i = vals_per_tone * (pol*(nsubband/npol)*ntones + band*ntones + tonenr)
                pc = tone[i:(i+vals_per_tone)]

                id = pc[0] + pc[1] # + ' tone ' + str(tonenr)
                if not(id in pcalvalues):
                   pcalvalues[id] = numpy.zeros(0)
                   print ('New band added: %s' % (id))
                pcalvalues[id] = numpy.append(pcalvalues[id], [float(pc[2]) + 1j*float(pc[3])])

    pcaldata = (pcalvalues,times,tint)
    return pcaldata


def plotpcal(pcaldata,infile,band_tone_sel=(),doPDF=False,doTxt=True):

    pcalvalues = pcaldata[0]
    times = pcaldata[1]
    tint = pcaldata[2]

    # Settings for plot
    colormp = pylab.cm.jet(numpy.linspace(0,1,len(pcalvalues.keys())))
    markers = ['o','x','+','s','p','*','h','H','D','d']
    Nrep    = 1 + len(pcalvalues.keys()) / len(markers)
    markers = iter(markers * Nrep)
    colors  = iter(colormp)
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

    if doPDF:
        outfile = os.path.basename(infile.name) + '.pdf'
        pylab.savefig(outfile, bbox_extra_artist=[h_leg])
        print ('Saved plot to %s' % outfile)
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

    pylab.show()
    return


def main(argv=sys.argv):
    args = argv[1:]
    doTxt = False
    doPDF = False

    # Optional args
    while (len(args) > 0) and (args[0][0:2] == '--'):
        if args[0] == '--pdf':
            doPDF = True
        if args[0] == '--txt':
            doTxt = True
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
    sel = []
    if len(args)>2:
        for bt in args[2:]:
           bt = bt.split(',')
           for tt in bt[1:]:
              sel.append( (int(bt[0])-1, int(tt)-1) )

    # Plot each file
    for af in antennafiles:
        infile = open(af, 'r')
        pc = parsepcalfile(infile,sel)
        plotpcal(pc,infile,sel,doPDF,doTxt)
        infile.close()

if __name__ == '__main__':
    sys.exit(main())
