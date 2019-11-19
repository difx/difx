#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import os, sys,argparse

parser = argparse.ArgumentParser()
parser.add_argument("-n", "--nbins", type=int, default=None, help="Number of images files from which to extract the spectrum; note: zero-indexed, so nbins=11 means bin00 to bin10")
parser.add_argument("-c", "--nchan", type=int, default=None, help="Number of channel slices in each cube image; note: zero-indexed")
parser.add_argument("-s", "--src", type=str, default=None, help="Source name to be used for the spectra text file prefix")
parser.add_argument("-r", "--res", type=float, default=None, help="Temporal resolution of data in ms")
parser.add_argument("-z", "--zero", default=False, help="Set zeroth bin equal to zero; use for nbins>1 when the zeroth bin contains little to no signal but mostly noise", action="store_true")
parser.add_argument("-f", "--basefreq", type=float, default=None, help="The lowest frequency in the observation in MHz")
parser.add_argument("-F", "--fscrunch", default=False, help="Make fscrunched plot", action="store_true")
parser.add_argument("--pols", type=str, default="XX,YY,I,Q,U,V", help='The polarisations to be imaged if --imagecube is set. Defaulted to all. Input as a list of strings: e.g., "XX,YY"')
parser.add_argument("-a", "--avg", type=int, default=24, help="Number of channels to average together per image cube slice")

args = parser.parse_args()

print args

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

if args.nbins is None:
    parser.error("You must specify the number of images you're processing")

if args.nchan is None:
    parser.error("You must specify the number of slices in the cube image")

if args.src is None:
    parser.error("You must specify an output spectra file name prefix")

if args.res is None:
    parser.error("You must specify the data's temporal resolution")

if args.basefreq is None:
    parser.error("You must specify the data's lowest frequency")

nbins = args.nbins
nchan = args.nchan
src = args.src
res = args.res

# Define dynamic spectra parameters
basefreq = args.basefreq
bandwidth = args.avg/6 * nchan # MHz
startchan = 0
endchan=nchan-3

# Define plotting parameters
startfreq = basefreq + (startchan*bandwidth)/nchan
endfreq = basefreq + (endchan*bandwidth)/nchan
starttime = 0
endtime = nbins*res
dynspec = {}
fscrunch = {}

# Change global font size
matplotlib.rcParams.update({'font.size': 12})

for stokes in args.pols.split(','):

    # Set figure size
    fig, ax = plt.subplots(figsize=(6,7))
    plt.title("Stokes "+ stokes)
    
    dynspec[stokes] = np.loadtxt("{0}-imageplane-dynspectrum.stokes{1}.txt".format(src, stokes))

    print dynspec[stokes].shape

    if nbins == 1:
        f = np.linspace(startfreq, endfreq, endchan)

        ax.plot(f,dynspec[stokes][startchan:endchan])
        ax.set_xlabel("Frequency (MHz)")
        ax.set_ylabel("Amp")
        plt.savefig('{0}-imageplane-dynspectrum.stokes{1}.png'.format(src, stokes))
        plt.clf()

    else:
        if args.zero:
            dynspec[stokes][0] = 0
#            dynspec[stokes][1] = 0
            print "Setting zeroth input bin equal to zero"
        else: print "Plotting all bins"

        if args.fscrunch:
            fscrunch[stokes] = np.sum(dynspec[stokes], 1)
            np.savetxt("{0}-imageplane-fscrunch-spectrum.stokes{1}.txt".format(src, stokes), fscrunch[stokes])

        ax.imshow(dynspec[stokes][:,startchan:endchan].transpose(), cmap=plt.cm.plasma, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
        ax.set_aspect(0.003) # you may also use am.imshow(..., aspect="auto") to restore the aspect ratio
        ax.set_xlabel("Time (ms)")
        ax.set_ylabel("Frequency (MHz)")
        plt.savefig('{0}-imageplane-dynspectrum.stokes{1}.png'.format(src, stokes), bbox_inches = 'tight')
        plt.clf()

if args.fscrunch:

    # Set figure size
    fig, ax = plt.subplots(figsize=(8,7))
    plt.title("Frequency averaged")

    print "fscrunching..."
    centretimes = np.arange(starttime+res, endtime+res, res)
    for stokes in args.pols.split(','):

        if stokes=="I": 
            col='k'
            plotlinestyle=':'
        elif stokes=="Q": 
            col='cyan'
            plotlinestyle='-'
        elif stokes=="U": 
            col='magenta'
            plotlinestyle='-.'
        elif stokes=="V": 
            col='r'
            plotlinestyle='--'
        elif stokes=="YY": 
            col='orange'
            plotlinestyle='-'
        else: 
            col='yellow'
            plotlinestyle='--'
    
        amp_jy = fscrunch[stokes][:] * 0.01/res
        ax.plot(centretimes,amp_jy, label=stokes, color=col, linestyle=plotlinestyle)
        ax.legend()
        ax.set_xlabel("Time (ms)")
        ax.set_ylabel("Amplitude (Jy)")
        plt.savefig("{0}-fscrunch.png".format(src), bbox_inches = 'tight')
else: print "Done!"
