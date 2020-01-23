#!/usr/bin/env python2

from __future__ import absolute_import
from __future__ import print_function
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from astropy import constants as const
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
parser.add_argument("-a", "--avg", type=int, default=16, help="Number of channels to average together per image cube slice")
parser.add_argument("--rms", default=False, action="store_true", help="Use the off-source rms estimate")
parser.add_argument("--flagchans", default="", help="comma-separated list of channels to zero in the plot")
parser.add_argument("--frbtitletext", type=str, default="", help="The name of the FRB (or source) to be used as the title of the plots")
parser.add_argument("--diagnostic", default=False, action="store_true", help="Set if you wish to make diagnostic plots")
parser.add_argument("--rotmeas", type=int, default=None, help="Number of channels to average together per image cube slice")
parser.add_argument("-t", "--threshold_factor", type=float, default=2.5, help="Factor to use in thresholding for masking the polarisation position angle for plotting")

args = parser.parse_args()

print(args)

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

if args.rotmeas is None:
    parser.error("You must specify the data's RM in order to derotate Stokes Q and U!")

nbins = args.nbins
nchan = args.nchan
flagchans = [int(x) for x in args.flagchans.split(',') if x.strip().isdigit()]
src = args.src
res = args.res
frbtitletext = args.frbtitletext

# Define dynamic spectra parameters
basefreq = args.basefreq
bandwidth = args.avg/4 * nchan # MHz
print("Bandwidth: {0}".format(bandwidth))
startchan = 0
endchan=nchan - 1

# Define plotting parameters
startfreq = basefreq + (startchan*bandwidth)/nchan
endfreq = basefreq + (endchan*bandwidth)/nchan
freqs = np.linspace(startfreq, endfreq, nchan)
starttime = 0
endtime = nbins*res
dynspec = {}
dynrms = {}
fscrunch = {}
fscrunchrms = {}
dynspec_rmcorrected = {}
fscrunch_rmcorrected = {}

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
        print("{0}-imageplane-rms.stokes{1}.txt".format(src, stokes))

    print(dynspec[stokes].shape)

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
            print("Setting zeroth input bin equal to zero")
        else:
            print("Plotting all bins")
        for f in flagchans:
            print("Flagging channel", f)
            dynspec[stokes][:,f] = 0

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
            print(dynrms_jy)
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
plt.close('all')


#####################################################################################

# POLARISATION CORRECTIONS

#####################################################################################

# TO ADD: Polarisaion calibration

# Correcting for Faraday rotation

# The RM measured using the data
rotmeas = args.rotmeas

# Calculate the total linear polarisation
p_qu = np.sqrt(dynspec["Q"]**2 + dynspec["U"]**2)

# Calculate the polarisation position angle
pa = 0.5*np.arctan2(dynspec["U"],dynspec["Q"])

# Wavelength squared
c = const.c.value # speed of light in m/s
lambda_sq = (c/(freqs*1e6))**2

# Derotating using the measured RM 
delta_pa = rotmeas * lambda_sq
pa_corrected = pa + delta_pa

# Apply corrections to Stokes Q and U
dynspec_rmcorrected["Q"] = p_qu * np.cos(2*pa_corrected)
dynspec_rmcorrected["U"] = p_qu * np.sin(2*pa_corrected)
dynspec_rmcorrected["I"] = np.copy(dynspec["I"])
dynspec_rmcorrected["V"] = np.copy(dynspec["V"])

# Plot to confirm that the sign of the RM is correct:
# Set figure size
pol_fig, pol_ax = plt.subplots(figsize=(7,7))

pol_ax.imshow(dynspec_rmcorrected["Q"][:,startchan:endchan].transpose(), cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
pol_ax.set_xlabel("Time (ms)")
pol_ax.set_ylabel("Frequency (MHz)")
plt.tight_layout()
plt.savefig('{0}-RMcorrected.stokesQ.png'.format(src, stokes))

pol_ax.imshow(dynspec_rmcorrected["U"][:,startchan:endchan].transpose(), cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
pol_ax.set_xlabel("Time (ms)")
pol_ax.set_ylabel("Frequency (MHz)")
plt.tight_layout()
plt.savefig('{0}-RMcorrected.stokesU.png'.format(src, stokes))


# TO ADD: Polarisation fractions
# Total polarisation relative to I
# TO ADD: P/I, L/I, V/I


#####################################################################################

# FSCRUNCH DATA WORK

#####################################################################################

# Plot the fscrunched time series if asked
if args.fscrunch:
    print("fscrunching...")
    centretimes = np.arange(starttime+res/2.0, endtime, res)
    # Setup figure and axes for diagnostic plot
    scrunch_fig_diag = plt.figure(figsize=(7,11))
    scrunch_ax0_diag = plt.subplot2grid((10,3), (0,0), rowspan=2, colspan=3)
    scrunch_ax1_diag = plt.subplot2grid((10,3), (3,0), rowspan=2, colspan=3, sharex=scrunch_ax0_diag)
    scrunch_ax2_diag = plt.subplot2grid((10,3), (5,0), rowspan=5, colspan=3, sharex=scrunch_ax0_diag)
    plt.setp(scrunch_ax1_diag.get_xticklabels(), visible=False)
    scrunch_fig_diag.subplots_adjust(hspace=0)
    scrunch_ax1_diag.tick_params(axis='x', direction='in')

    # Set up figure and axes for publication plot
    scrunch_fig = plt.figure(figsize=(7,9))
    scrunch_ax0 = plt.subplot2grid((7,3), (0,0), rowspan=2, colspan=3)
    scrunch_ax1 = plt.subplot2grid((7,3), (2,0), rowspan=5, colspan=3, sharex=scrunch_ax0)
    plt.setp(scrunch_ax0.get_xticklabels(), visible=False)
    scrunch_fig.subplots_adjust(hspace=0)
    scrunch_fig.subplots_adjust(hspace=0)
    scrunch_ax0.tick_params(axis='x', direction='in')

    for stokes in args.pols.split(','):

        fscrunch_rmcorrected[stokes] = np.mean(dynspec_rmcorrected[stokes], 1)
#        np.savetxt("{0}-imageplane-fscrunch-spectrum.RMcorrected.stokes{1}.txt".format(src, stokes), fscrunch_rmcorrected[stokes])

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

        print(stokes)
        amp_jy = fscrunch_rmcorrected[stokes][:] * 10000/res

        if args.rms:
            rms_jy = fscrunchrms[stokes][:] * 10000/res
            # Diagnostic plot
            scrunch_ax2_diag.errorbar(centretimes, amp_jy, yerr=rms_jy, label=stokes, color=col, linestyle=plotlinestyle, linewidth=1.5, capsize=2, elinewidth=2)
            # Publication plot
            scrunch_ax1.errorbar(centretimes, amp_jy, yerr=rms_jy, label=stokes, color=col, linestyle=plotlinestyle, linewidth=1.5, capsize=2, elinewidth=2)
        else:
            plt.title('   '+frbtitletext, loc='left', pad=-20)
            # Diagnostic plot
            scrunch_ax2_diag.plot(centretimes, amp_jy, label=stokes, color=col, linestyle=plotlinestyle)
            # Publication plot
            scrunch_ax1.plot(centretimes, amp_jy, label=stokes, color=col, linestyle=plotlinestyle)

    # Get new, corrected polarisation position angle for fscrunched data
    pol_pa = 0.5*np.arctan2(fscrunch_rmcorrected["U"], fscrunch_rmcorrected["Q"])*180/np.pi
    total_linear_pol_flux_meas = np.sqrt(fscrunch_rmcorrected["Q"]**2 + fscrunch_rmcorrected["U"]**2)
    # Calculate a noise threshold for masking the polarisation position angle plot
    sigma_I = np.sqrt((fscrunchrms["Q"]**2 + fscrunchrms["U"]**2)/2)
    threshold_factor=args.threshold_factor

    # Check that sigma_I above and fscrunchrms["I"] are roughly equivalent
    sigma_I_fig = plt.figure(figsize=(7,7))
    sigma_I_ax0 = plt.subplot2grid((9,3), (0,0), rowspan=5, colspan=3)
    sigma_I_ax1 = plt.subplot2grid((9,3), (5,0), rowspan=4, colspan=3, sharex=sigma_I_ax0)
    sigma_I_ax0.plot(centretimes, sigma_I*10000/res, label="sigma_I_Q,U")
    sigma_I_ax0.plot(centretimes, (fscrunchrms["I"]*10000/res), label="sigma_I_image")
    sigma_I_ax1.plot(centretimes, (fscrunchrms["I"]*10000/res)-(sigma_I*10000/res), label="sigma_I_image - sigma_I_Q,U")
    sigma_I_ax0.legend()
    sigma_I_ax0.set_xlabel("Time (ms)")
    sigma_I_ax0.set_ylabel("Jy")
    sigma_I_ax1.legend()
    sigma_I_ax1.set_xlabel("Time (ms)")
    sigma_I_ax1.set_ylabel("Jy")
    sigma_I_fig.savefig("sigma_I_image_vs_QU_diagnostic.png".format(src), bbox_inches = 'tight')

    # Removing bias from L_measured (i.e., the total linear polarisation flux):
    # Set L_true = sqrt( (L_meas / sigma_I)^2 - sigma_I ) for L_meas/sigma_I > 1.57 and 0 otherwise,
    # following Everett, J. E.; Weisberg, J. M. 2001 (Sec. 3.2)
    total_linear_pol_flux_true = np.zeros(len(total_linear_pol_flux_meas))
    total_linear_pol_flux_true[total_linear_pol_flux_meas / sigma_I > 1.57] = (np.sqrt( (total_linear_pol_flux_meas/sigma_I)**2 - 1)*sigma_I)[total_linear_pol_flux_meas / sigma_I > 1.57]
    print("sigma: {0}".format(sigma_I))
    print("L_meas / sigma: {0}".format(total_linear_pol_flux_meas/sigma_I))
    print("L_meas: {0}".format(total_linear_pol_flux_meas))
    print("L_true: {0}".format(total_linear_pol_flux_true))
    # Prepare the arrays for masking
    pol_pa_to_mask = np.ma.array(pol_pa)
    # Mask values below threshold_factor x sigma_I
    pol_pa_masked = np.ma.masked_where(total_linear_pol_flux_true < threshold_factor*sigma_I, pol_pa_to_mask)
    pol_pa_masked = np.ma.masked_where(total_linear_pol_flux_true == 0, pol_pa_masked)

    pa_rms = (180/np.pi) * (np.sqrt( ((fscrunch_rmcorrected["Q"]**2 * fscrunchrms["U"]**2) + (fscrunch_rmcorrected["U"]**2 *fscrunchrms["Q"]**2))/(4*(fscrunch_rmcorrected["Q"]**2 + fscrunch_rmcorrected["U"]**2)**2) ))
    print("PA rms: {0}".format(pa_rms))
    pol_pa_rms_to_mask = np.ma.array(pa_rms)
    pol_pa_rms_masked = np.ma.masked_where(total_linear_pol_flux_true < threshold_factor*sigma_I, pol_pa_rms_to_mask)
    pol_pa_rms_masked = np.ma.masked_where(total_linear_pol_flux_true == 0, pol_pa_rms_masked)
#    print(pol_pa_to_mask)
#    print(fscrunch_rmcorrected["U"])
#    print(fscrunch_rmcorrected["Q"])
#    print(fscrunch["Q"])
#    print(fscrunch["U"])

    # Diagnostic plots
    scrunch_ax1_diag.set_title('   '+frbtitletext)
    scrunch_ax0_diag.errorbar(centretimes, pol_pa, yerr=pa_rms, fmt='ko', markersize=2)
    scrunch_ax0.set_ylabel("Position Angle (deg)")
    scrunch_ax1_diag.errorbar(centretimes, pol_pa_masked, yerr=pol_pa_rms_masked, fmt='ko', markersize=2, capsize=2)
    scrunch_ax1_diag.set_ylabel("Position Angle (deg)")
    scrunch_ax2_diag.legend()
    scrunch_ax2_diag.set_xlabel("Time (ms)")
    scrunch_ax2_diag.set_ylabel("Flux Density (Jy)")

    # Publication plots
    scrunch_ax0.set_title('   '+frbtitletext)
    scrunch_ax0.errorbar(centretimes, pol_pa_masked, yerr=pol_pa_rms_masked, fmt='ko', markersize=2, capsize=2)
    scrunch_ax0.set_ylabel("Position Angle (deg)")
    scrunch_ax1.legend()
    scrunch_ax1.set_xlabel("Time (ms)")
    scrunch_ax1.set_ylabel("Flux Density (Jy)")
    if args.diagnostic:
        plt.show()
    else: print("Just saving plots")
    scrunch_fig_diag.savefig("{0}-fscrunch.RMcorrected_diagnostic.png".format(src), bbox_inches = 'tight')
    scrunch_fig.savefig("{0}-fscrunch.RMcorrected.png".format(src), bbox_inches = 'tight')
    scrunch_fig.clf()

    scrunch_i_fig, scrunch_i_ax = plt.subplots(figsize=(7,7))
    col='k'
    plotlinestyle=':'
    amp_jy = fscrunch_rmcorrected["I"][:] * 10000/res
    if args.rms:
        rms_jy = fscrunchrms["I"][:] * 10000/res
        scrunch_i_ax.errorbar(centretimes,amp_jy,yerr=rms_jy, label="I", capsize=2, elinewidth=2)
    else:
        scrunch_i_ax.plot(centretimes,amp_jy,label="I")
    scrunch_i_ax.set_xlabel("Time (ms)")
    scrunch_i_ax.set_ylabel("Flux Density (Jy)")
    scrunch_i_ax.legend()
    scrunch_i_fig.savefig("{0}-fscrunch.RMcorrected.stokesI.png".format(src), bbox_inches = 'tight')
    scrunch_i_fig.clf()

else: print("Done!")
