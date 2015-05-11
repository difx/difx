#!/usr/bin/python
"""
plotDiFXPCal.py version 1.0  Jan Wagner  20150508

Usage: plotDiFXPCal.py [--pdf] [--txt] <output_1.difx> <station> 
                       [<band>,<tone>] [<band>,<tone>] [...]

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
  band,tone  to select specific tones instead of all tones
"""

import sys, os, glob, math, cmath
import numpy, pylab

difxVersion = 240

def parsepcalfile(infile,band_tone_sel=(),doPDF=False,doTxt=True):

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
                   print ('New band added: %s' % (id))
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

    sel = []
    if len(args)>2:
        for bt in args[2:]:
           bt = bt.split(',')
           sel.append( (int(bt[0])-1, int(bt[1])-1) )

    for af in antennafiles:
        infile = open(af, 'r')
        parsepcalfile(infile,sel,doPDF,doTxt)
        infile.close()

if __name__ == '__main__':
    sys.exit(main())
