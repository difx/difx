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
parser.add_argument("--rms", default=False, action="store_true", help="Use the off-source rms estimate")
parser.add_argument("--frbtitletext", type=str, default="", help="The name of the FRB (or source) to be used as the title of the plots")

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
frbtitletext = args.frbtitletext

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
dynrms = {}
fscrunch = {}
fscrunchrms = {}

# Change global font size
matplotlib.rcParams.update({'font.size': 16})

combinedfig, combinedaxs = plt.subplots(4, 1, sharex=True, sharey=True, figsize=(4.5,15))

for stokes in args.pols.split(','):

    # Set figure size
    fig, ax = plt.subplots(figsize=(7,7))
    plt.title("Stokes "+ stokes)
    
    dynspec[stokes] = np.loadtxt("{0}-imageplane-dynspectrum.stokes{1}.txt".format(src, stokes))
    if args.rms:
        dynrms[stokes] = np.loadtxt("{0}-imageplane-rms.stokes{1}.txt".format(src, stokes))
        print "{0}-imageplane-rms.stokes{1}.txt".format(src, stokes)

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
            if args.rms:
                dynrms[stokes][0] = 0
            print "Setting zeroth input bin equal to zero"
        else: print "Plotting all bins"

        if args.fscrunch:
            fscrunch[stokes] = np.mean(dynspec[stokes], 1)
            np.savetxt("{0}-imageplane-fscrunch-spectrum.stokes{1}.txt".format(src, stokes), fscrunch[stokes])
            if args.rms:
                fscrunchrms[stokes] = np.divide(np.mean(dynrms[stokes], 1),np.sqrt(nchan))

        ax.imshow(dynspec[stokes][:,startchan:endchan].transpose(), cmap=plt.cm.plasma, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
        #ax.set_aspect(0.03) # you may also use am.imshow(..., aspect="auto") to restore the aspect ratio
        ax.set_xlabel("Time (ms)")
        ax.set_ylabel("Frequency (MHz)")
        plt.tight_layout()
        plt.savefig('{0}-imageplane-dynspectrum.stokes{1}.png'.format(src, stokes))

        if args.rms:
            dynrms_jy = dynrms[stokes]*10000/res
            print dynrms_jy
            ax.imshow(dynrms_jy[:,startchan:endchan].transpose(), cmap=plt.cm.plasma, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
            #ax.set_aspect(0.03) # you may also use am.imshow(..., aspect="auto") to restore the aspect ratio
            ax.set_xlabel("Time (ms)")
            ax.set_ylabel("Frequency (MHz)")
            plt.savefig('{0}-imageplane-rms.stokes{1}.png'.format(src, stokes))
        plt.clf()

        # Also plot onto the multipanel plot
        if stokes in ["I","Q","U","V"]:
            combinedaxs[["I","Q","U","V"].index(stokes)].imshow(dynspec[stokes][:,startchan:endchan].transpose(), cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')

# Save the multipanel plot
combinedfig.suptitle(frbtitletext, x=0.25, y=0.99)
combinedaxs[3].set_xlabel("Time (ms)")
for i in range(4):
    combinedaxs[i].set_ylabel("Frequency (MHz)")
    if i==0:
        combinedaxs[i].title.set_text("\n Stokes I")
    else:
        combinedaxs[i].title.set_text("Stokes "+["I","Q","U","V"][i])
combinedaxs[0].title.set_text("\n Stokes I")
plt.figure(combinedfig.number)
plt.tight_layout()
plt.savefig("{0}-multipanel-dynspectrum.png".format(src))
plt.clf()
plt.figure(fig.number)

# Plot the fscrunched time series if asked
if args.fscrunch:
    print "fscrunching..."
    centretimes = np.arange(starttime+res/2.0, endtime, res)
    for stokes in args.pols.split(','):

        if stokes=="I": 
            col='k'
            plotlinestyle='-'
        elif stokes=="Q": 
            col='#8c510a'
            plotlinestyle='--'
        elif stokes=="U": 
            col='#d8b365'
            plotlinestyle='-.'
        elif stokes=="V": 
            col='#01665e'
            plotlinestyle=':'
        elif stokes=="YY": 
            col='#dfc27d'
            plotlinestyle='-'
        else: 
            col='#35978f'
            plotlinestyle=':'

        print stokes
        amp_jy = fscrunch[stokes][:] * 10000/res
        rms_jy = fscrunchrms[stokes][:] * 10000/res

        if args.rms:
            plt.title('   '+frbtitletext, loc='left', pad=-20)
            plt.errorbar(centretimes, amp_jy, yerr=rms_jy, label=stokes, color=col, linestyle=plotlinestyle, linewidth=1.5, capsize=2)
        else:
            plt.title('   '+frbtitletext, loc='left', pad=-20)
            plt.plot(centretimes, amp_jy, label=stokes, color=col, linestyle=plotlinestyle)
    plt.legend()
    plt.xlabel("Time (ms)")
    plt.ylabel("Amplitude (Jy)")
    plt.savefig("{0}-fscrunch.png".format(src), bbox_inches = 'tight')
    plt.clf()
    col='k'
    plotlinestyle=':'
    amp_jy = fscrunch["I"][:] * 10000/res
    rms_jy = fscrunchrms["I"][:] * 10000/res
    if args.rms:
        plt.errorbar(centretimes,amp_jy,yerr=rms_jy, label="I")
    else:
        plt.plot(centretimes,amp_jy,label="I")
    plt.xlabel("Time (ms)")
    plt.ylabel("Amplitude (Jy)")
    plt.savefig("{0}-fscrunch.stokesI.png".format(src), bbox_inches = 'tight')
    plt.clf()

else: print "Done!"
