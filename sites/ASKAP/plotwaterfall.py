#!/usr/bin/env python

from __future__ import absolute_import
from __future__ import print_function
import numpy as np
from numpy.polynomial import polynomial as P
import matplotlib
import matplotlib.pyplot as plt
from astropy import constants as const
import os, sys,argparse

parser = argparse.ArgumentParser()
parser.add_argument("-n", "--nbins", type=int, default=None, help="Number of images files from which to extract the spectrum; note: zero-indexed, so nbins=11 means bin00 to bin10")
parser.add_argument("-c", "--nchan", type=int, default=None, help="Number of channel slices in each cube image; note: zero-indexed")
parser.add_argument("-w", "--chanwidth", type=int, default=None, help="Width of frequency channel in MHz")
parser.add_argument("-s", "--src", type=str, default=None, help="Source name to be used for the spectra text file prefix")
parser.add_argument("-r", "--res", type=float, default=None, help="Temporal resolution of data in ms")
parser.add_argument("-z", "--zero", default=False, help="Set zeroth bin equal to zero; use for nbins>1 when the zeroth bin contains little to no signal but mostly noise", action="store_true")
parser.add_argument("-f", "--basefreq", type=float, default=None, help="The lowest frequency in the observation in MHz")
parser.add_argument("-F", "--fscrunch", default=False, help="Make fscrunched plot", action="store_true")
parser.add_argument("--pols", type=str, default="XX,YY,I,Q,U,V", help='The polarisations to be imaged if --imagecube is set. Defaulted to all. Input as a list of strings: e.g., "XX,YY"')
parser.add_argument("--rms", default=False, action="store_true", help="Use the off-source rms estimate")
parser.add_argument("--flagchans", default="", help="comma-separated list of channels to zero in the plot")
parser.add_argument("--frbtitletext", type=str, default="", help="The name of the FRB (or source) to be used as the title of the plots")
parser.add_argument("--rotmeas", type=float, help="The rotation measure for the pulse; used to derotate the data prior to frequency scrunching")
parser.add_argument("-t", "--threshold_factor", type=float, default=2.5, help="Factor to use in thresholding for masking the polarisation position angle for plotting")
parser.add_argument("--isolate", default=False, action="store_true", help="Set if you want to calculate the polarisation fractions or to isolate specific sub-pulses")
parser.add_argument("--pulse_number", type=int, default=None, help="Number of the pulse of interest for determining the polarisation fraction values")
parser.add_argument("--binstartstop", type=str, default=None, help="Start and end bins for integrating over the pulse to get the polarisation fraction totals; input as a comma separated string")
parser.add_argument("--logstartstop", type=str, default=None, help="Start and end bins for fitting the log of the frequency-averaged spectra for a given number of components; input as a comma separated string: e.g., start,stop,start2,stop2")
parser.add_argument("--multi_rotmeas", type=str, default=None, help="To be used for cases requiring multiple RMs; input is a list of strings: e.g., RM1,RM2")
parser.add_argument("--rm_bins_starts", type=str, default=None, help="The start bins used for de-rotating at a given RM; to be used with --multi_rotmeas set; input is a list of strings: e.g., RM1_start,RM2_start,RM3_start")
parser.add_argument("--dm_offset", type=float, default=0.0, help="A roughly estimate of the residual DM to be corrected in a pulse")
parser.add_argument("--delta_psi", type=float, default=0.0, help="The polarisation position angle change used in the polarisation calibration; in radians")
parser.add_argument("--delta_t", type=float, default=0.0, help="The delay derived between the linearly polarised feeds to be used in the polarisation calibration; in nanosec")
parser.add_argument("--phi", type=float, default=0.0, help="The phase offset derived between the linearly polarised feeds to be used in the polarisation calibration; in radians")
parser.add_argument("--unwrap", default=False, action="store_true", help="Set if there is clear phase wrapping in the FRB PA")
parser.add_argument("--subband", default=False, action="store_true", help="Set to subband the data into numsub subbands")
parser.add_argument("--numsub", type=int, default=None, help="Number of subbands to divide the data into")
parser.add_argument("--firstlook", default=False, action="store_true", help="Set if only doing a first look run (i.e., no calibration, etc.)")

args = parser.parse_args()

print(args)

if len(sys.argv) < 2:
    parser.print_usage()
    sys.exit()

if args.nbins is None:
    parser.error("You must specify the number of images you're processing")

if args.nchan is None:
    parser.error("You must specify the number of slices in the cube image")

if args.chanwidth is None:
    parser.error("You must specify the frequency channel width in MHz")

if args.src is None:
    parser.error("You must specify an output spectra file name prefix")

if args.res is None:
    parser.error("You must specify the data's temporal resolution")

if args.basefreq is None:
    parser.error("You must specify the data's lowest frequency")

if args.isolate:
    if args.pulse_number is None:
        parser.error("You must specify the pulse number for which you want to obtain the polarisation fractions")

    if args.binstartstop is None:
        parser.error("You must specify the start and stop bins for the pulse for which you want to obtain the polarisation fractions")

if args.logstartstop is None:
    parser.error("You must specify the start and stop bins for the range over which you want to obtain the log fit and Stokes I plot")

if args.multi_rotmeas:
    if args.rm_bins_starts is None:
        parser.error("You must specify the start bins for the range of the spectrum you wish to de-rotate")

if args.subband:
    if args.numsub is None:
        parser.error("You must specify the number of subbands if you wish to subband the data")

# Define general parameters
nbins = args.nbins
nchan = args.nchan
chanwidth = args.chanwidth
flagchans = [int(x) for x in args.flagchans.split(',') if x.strip().isdigit()]
src = args.src
res = args.res
frbtitletext = args.frbtitletext
if args.isolate:
    pulse_number = args.pulse_number
    binstart = int(args.binstartstop.split(',')[0])
    binstop = int(args.binstartstop.split(',')[1])
else:
    binstart = 0
    binstop = -1

# Define dynamic and fscrunched spectra parameters
basefreq = args.basefreq
bandwidth = nchan * chanwidth
print("Bandwidth: {0}".format(bandwidth))
startchan = 0
endchan=nchan - 1
dynspec = {}
dynrms = {}
fscrunch = {}
fscrunchrms = {}

# Define plotting parameters
startfreq = basefreq + (startchan*bandwidth)/nchan
endfreq = basefreq + (endchan*bandwidth)/nchan
freqs = np.linspace(startfreq, endfreq, nchan) # MHz
freqs_ghz = freqs * 1e-3
if args.isolate:
    starttime_isolate = binstart * res
    endtime_isolate = binstop * res
starttime = 0
endtime = nbins*res

if not args.firstlook:
    # Define RM correction parameters
    if hasattr(args, "rotmeas"):
        rotmeas = args.rotmeas # The RM measured using the data; for single RM data
    else: rotmeas = 0
    print("RM = {0}".format(rotmeas))
    label_rotmeas = str(rotmeas) + "rad/s^2"
    label_rotmeas_save = str(rotmeas)
    if args.multi_rotmeas: # The RMs measured using the data; for multiple RM data
        # Get the RMs and convert to floats
        multi_rotmeas_str = args.multi_rotmeas.split(',')
        num_RMs = len(multi_rotmeas_str)
        RM_indx = np.arange(num_RMs)
        multi_rotmeas = [0.0] + [np.float(multi_rotmeas_str[i]) for i in RM_indx] + [0.0]
        # Get RM for plot label and figure saving
        label_rotmeas = ''
        label_rotmeas_save = ''
        for i in np.arange(len(multi_rotmeas_str)):
            if i > 0:
                label_rotmeas += ','
                label_rotmeas_save += '_'
                label_rotmeas += multi_rotmeas_str[i]
                label_rotmeas_save += multi_rotmeas_str[i]
                print("RM plot label: {0}".format(label_rotmeas))
                # Get the starts for each chunk of data to be de-rotated and make floats in list
                rm_bins_str = args.rm_bins_starts.split(',')
                num_rm_bins = len(rm_bins_str)
                rm_bins_indx = np.arange(num_rm_bins)
                rm_bins_starts = [np.float(rm_bins_str[i]) for i in rm_bins_indx]
                # Pad the end with the number of time bins + 1 in order to have a correct index range for later
                rm_bins_starts += [nbins+1.0]
                print("RMs chosen for each pulse: {0}".format(multi_rotmeas))
                print("Start bins for each RM: {0}".format(rm_bins_starts))
                dynspec_rmcorrected = {}
                fscrunch_rmcorrected = {}

    # Polarisation calibration parameters
    delta_psi = args.delta_psi # radians
    delta_t = args.delta_t # nanoseconds
    phi = args.phi # radians

# Define number of subbands if requested
if args.subband:
    numsub = args.numsub
    basename_sub = "fscrunch_subband"
    fscrunch_subband = {}

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

    print("Shape of Stokes {0} dynamic spectrum: {1}".format(stokes, dynspec[stokes].shape))

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

        ax.imshow(dynspec[stokes][:,startchan:endchan].transpose(), cmap=plt.cm.plasma, interpolation='none', origin='lower', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
        #ax.set_aspect(0.03) # you may also use am.imshow(..., aspect="auto") to restore the aspect ratio
        ax.set_xlabel("Time (ms)")
        ax.set_ylabel("Frequency (MHz)")
        plt.tight_layout()
        plt.savefig('{0}-imageplane-dynspectrum.stokes{1}.png'.format(src, stokes))

        if args.rms:
            dynrms_jy = dynrms[stokes]*10000/res
            print(dynrms_jy)
            ax.imshow(dynrms_jy[:,startchan:endchan].transpose(), cmap=plt.cm.plasma, interpolation='none', origin='lower', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            #ax.set_aspect(0.03) # you may also use am.imshow(..., aspect="auto") to restore the aspect ratio
            ax.set_xlabel("Time (ms)")
            ax.set_ylabel("Frequency (MHz)")
            plt.savefig('{0}-imageplane-rms.stokes{1}.png'.format(src, stokes))
        plt.clf()

        # Also plot onto the multipanel plot
        if stokes in ["I","Q","U","V"]:
            combinedaxs[["I","Q","U","V"].index(stokes)].imshow(dynspec[stokes][:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')

        # If desired, save dynamic spectra for specific pulse
        if args.isolate:
            isolate_fig, isolate_ax = plt.subplots(figsize=(7,7))
            isolate_ax.imshow(dynspec[stokes][binstart:binstop,startchan:endchan].transpose(), cmap=plt.cm.plasma, interpolation='none', origin='lower', extent=[starttime_isolate,endtime_isolate,startfreq,endfreq], aspect='auto')
            isolate_ax.set_xlabel("Time (ms)")
            isolate_ax.set_ylabel("Frequency (MHz)")
            plt.tight_layout()
            isolate_fig.savefig('{0}-imageplane-dynspectrum.stokes{1}.pulse{2}.png'.format(src, stokes, pulse_number))

            if args.rms:
                dynrms_jy = dynrms[stokes]*10000/res
                print("Dynamic spectrum RMS for Stokes {0}, pulse {1}: {2}".format(stokes, pulse_number, dynrms_jy))
                isolate_ax.imshow(dynrms_jy[binstart:binstop,startchan:endchan].transpose(), cmap=plt.cm.plasma, interpolation='none', origin='lower', extent=[starttime_isolate,endtime_isolate,startfreq,endfreq], aspect='auto')
                isolate_ax.set_xlabel("Time (ms)")
                isolate_ax.set_ylabel("Frequency (MHz)")
                isolate_fig.savefig('{0}-imageplane-rms.stokes{1}.pulse{2}.png'.format(src, stokes, pulse_number))
            plt.clf()

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


if not args.firstlook:
    #####################################################################################

    # POLARISATION CORRECTIONS

    #####################################################################################

    # POLARISATION CALIBRATION

    q_calibrated = -dynspec["Q"]*np.cos(delta_psi) - ( (dynspec["U"]*np.cos(phi + 2*np.pi*freqs_ghz*delta_t) - dynspec["V"]*np.sin(phi + 2*np.pi*freqs_ghz*delta_t)) * np.sin(delta_psi) )
    u_calibrated = -dynspec["Q"]*np.sin(delta_psi) + ( (dynspec["U"]*np.cos(phi + 2*np.pi*freqs_ghz*delta_t) - dynspec["V"]*np.sin(phi + 2*np.pi*freqs_ghz*delta_t)) * np.cos(delta_psi) )
    v_calibrated = dynspec["U"]*np.sin(phi + 2*np.pi*freqs_ghz*delta_t) + dynspec["V"]*np.cos(phi + 2*np.pi*freqs_ghz*delta_t)

    # Save the calibrated dynamic spectra
    np.savetxt("{0}-imageplane-dynspectrum-calibrated.stokesI.txt".format(src), dynspec["I"])
    np.savetxt("{0}-imageplane-dynspectrum-calibrated.stokesQ.txt".format(src), q_calibrated)
    np.savetxt("{0}-imageplane-dynspectrum-calibrated.stokesU.txt".format(src), u_calibrated)
    np.savetxt("{0}-imageplane-dynspectrum-calibrated.stokesV.txt".format(src), v_calibrated)

    # Check the output of the delay and phase offset corrections
    del_phase_offset_arg = phi + 2*np.pi*freqs_ghz*delta_t
    del_ph_fig, del_ph_ax = plt.subplots(figsize=(7,7))
    del_ph_ax.plot(freqs_ghz,del_phase_offset_arg)
    del_ph_ax.set_ylabel("delay and phase offset (radians)")
    del_ph_ax.set_xlabel("frequency (GHz)")
    plt.tight_layout()
    plt.savefig('{0}-delay_phase_offset_delta-psi{1}_delta-t{2}_phi{3}.png'.format(src, delta_psi, delta_t, phi))

    # Plotting the calibrated dynamic spectra
    combinedfig_cal, combinedaxs_cal = plt.subplots(4, 1, sharex=True, sharey=True, figsize=(4.5,15))
    # Save the multipanel plot
    combinedfig_cal.suptitle(frbtitletext, x=0.25, y=0.99)
    combinedaxs_cal[3].set_xlabel("Time (ms)")
    for i in range(4):
        combinedaxs_cal[i].set_ylabel("Frequency (MHz)")
        if i==0:
            combinedaxs_cal[i].title.set_text("\n Stokes I")
        else:
            combinedaxs_cal[i].title.set_text("Stokes "+["I","Q","U","V"][i])
            combinedaxs_cal[0].title.set_text("\n Stokes I")
            combinedaxs_cal[0].imshow(dynspec["I"][:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            combinedaxs_cal[1].imshow(q_calibrated[:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            combinedaxs_cal[2].imshow(u_calibrated[:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            combinedaxs_cal[3].imshow(v_calibrated[:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            plt.figure(combinedfig_cal.number)
            plt.tight_layout()
            plt.savefig("{0}-multipanel-dynspectrum_calibrated_delta-psi{1}_delta-t{2}_phi{3}.png".format(src, delta_psi, delta_t, phi))
            combinedfig_cal.clf()
            plt.figure(fig.number)
            plt.close('all')

    # CORRECTING FOR FARADAY ROTATION

    # Calculate the total linear polarisation
    p_qu = np.sqrt(q_calibrated**2 + u_calibrated**2)

    # Calculate the polarisation position angle
    pa = 0.5*np.arctan2(u_calibrated,q_calibrated)

    # Wavelength squared
    c = const.c.value # speed of light in m/s
    lambda_sq = (c/(freqs*1e6))**2
    lambda_cen_sq = lambda_sq[int(np.floor(len(lambda_sq)/2))]
    print("lambda^2: {0}".format(lambda_sq))
    print("lambda^2 centre: {0}".format(lambda_cen_sq))
    # If multiple RMs are supplied
    if args.multi_rotmeas:
        # Make array of RM values for each slice of data
        k = 0
        rot_measures = np.zeros(nbins)
        for num_bin in np.arange(nbins):
            if num_bin >= rm_bins_starts[k]-1:
                k += 1
                rot_measures[num_bin] = multi_rotmeas[k]
                # Calculate the change required for PA correction and perform correction (referenced to the centre of the band)
                rm_angle = (rot_measures[:, None] * lambda_sq) - (rot_measures[:, None] * lambda_cen_sq)
                # If only one RM is provided
            else:
                # Derotating using the measured RM (referenced to the centre of the band)
                rm_angle = (rotmeas * lambda_sq) - (rotmeas * lambda_cen_sq)

    # Apply corrections to Stokes Q and U
    dynspec_rmcorrected["Q"] = q_calibrated*np.cos(2*rm_angle) + u_calibrated*np.sin(2*rm_angle) 
    dynspec_rmcorrected["U"] = u_calibrated*np.cos(2*rm_angle) - q_calibrated*np.sin(2*rm_angle)
    dynspec_rmcorrected["I"] = np.copy(dynspec["I"])
    dynspec_rmcorrected["V"] = np.copy(v_calibrated)
    #dynspec_rmcorrected["XX"] = np.copy(dynspec["XX"])
    #dynspec_rmcorrected["YY"] = np.copy(dynspec["YY"])

    # Plot to confirm that the sign of the RM is correct:
    # Set figure size
    pol_fig, pol_ax = plt.subplots(figsize=(7,7))

    print("U: {0}".format(dynspec["U"]))
    print("RM corrected U: {0}".format(dynspec_rmcorrected["U"]))
    print("RM corrected Q: {0}".format(dynspec_rmcorrected["Q"]))
    print("I: {0}".format(dynspec_rmcorrected["I"]))

    # Calculate U/I and Q/I ratios to determine the correct RM sign
    if args.isolate:
        if args.rms:
            U_on_I = dynspec_rmcorrected["U"][1:] / dynspec_rmcorrected["I"][1:]
            Q_on_I = dynspec_rmcorrected["Q"][1:] / dynspec_rmcorrected["I"][1:]
            print("U/I: {0}".format(U_on_I))
            print("Q/I: {0}".format(Q_on_I))
            print("U/I from {0} to {1}: {2}".format(binstart, binstop, U_on_I[binstart:binstop]))
            print("Q/I from {0} to {1}: {2}".format(binstart, binstop, Q_on_I[binstart:binstop]))
            print("U/I shape: {0}".format(U_on_I.shape))
            print("Q/I shape: {0}".format(Q_on_I.shape))
            U_on_I[dynspec_rmcorrected["I"][1:] < 3*dynrms["I"][1:]] = 0
            Q_on_I[dynspec_rmcorrected["I"][1:] < 3*dynrms["I"][1:]] = 0
            print("U/I post threshold: {0}".format(U_on_I[binstart:binstop]))
            print("Q/I post threshold: {0}".format(Q_on_I[binstart:binstop]))

    # Time scrunch the U/I and Q/I ratios to plot the ratios vs. frequency
    tscrunch_I = np.mean(dynspec_rmcorrected["I"][1:], 0)
    tscrunch_U = np.mean(dynspec_rmcorrected["U"][1:], 0)
    tscrunch_Q = np.mean(dynspec_rmcorrected["Q"][1:], 0)
    tscrunch_U_on_I = tscrunch_U / tscrunch_I
    tscrunch_Q_on_I = tscrunch_Q / tscrunch_I

    pol_ax.imshow(dynspec_rmcorrected["Q"][:,startchan:endchan].transpose(), cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
    pol_ax.set_xlabel("Time (ms)")
    pol_ax.set_ylabel("Frequency (MHz)")
    plt.tight_layout()
    plt.savefig('{0}-RMcorrected.stokesQ.RM{2}.png'.format(src, stokes, label_rotmeas_save))

    pol_ax.imshow(dynspec_rmcorrected["U"][:,startchan:endchan].transpose(), cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
    pol_ax.set_xlabel("Time (ms)")
    pol_ax.set_ylabel("Frequency (MHz)")
    plt.tight_layout()
    plt.savefig('{0}-RMcorrected.stokesU.RM{2}.png'.format(src, stokes, label_rotmeas_save))

    if args.pulse_number is None:
        pulse_number = '_all'

    if args.isolate:
        pol_slice_fig, pol_slice_ax = plt.subplots(figsize=(7,7))
        pol_slice_ax.imshow(U_on_I[:,startchan:endchan].transpose(), cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
        pol_slice_ax.set_xlabel("Time (ms)")
        pol_slice_ax.set_ylabel("Frequency (MHz)")
        plt.tight_layout()
        pol_slice_fig.savefig('{0}-RMcorrected.stokesQ_on_I.RM{2}.pulse{3}.png'.format(src, stokes, label_rotmeas_save, pulse_number))

        pol_slice_ax.imshow(U_on_I[:,startchan:endchan].transpose(), cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,endfreq,startfreq], aspect='auto')
        pol_slice_ax.set_xlabel("Time (ms)")
        pol_slice_ax.set_ylabel("Frequency (MHz)")
        plt.tight_layout()
        pol_slice_fig.savefig('{0}-RMcorrected.stokesU_on_I.RM{2}.pulse{3}.png'.format(src, stokes, label_rotmeas_save, pulse_number))

    # Plot the full calibrated dynamic spectra for the RM corrected data
    combinedfig_calrm, combinedaxs_calrm = plt.subplots(4, 1, sharex=True, sharey=True, figsize=(4.5,15))
    # Save the multipanel plot
    combinedfig_calrm.suptitle(frbtitletext, x=0.25, y=0.99)
    combinedaxs_calrm[3].set_xlabel("Time (ms)")
    for i in range(4):
        combinedaxs_calrm[i].set_ylabel("Frequency (MHz)")
        if i==0:
            combinedaxs_calrm[i].title.set_text("\n Stokes I")
        else:
            combinedaxs_calrm[i].title.set_text("Stokes "+["I","Q","U","V"][i])
            combinedaxs_calrm[0].title.set_text("\n Stokes I")
            combinedaxs_calrm[0].imshow(dynspec_rmcorrected["I"][:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            combinedaxs_calrm[1].imshow(dynspec_rmcorrected["Q"][:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            combinedaxs_calrm[2].imshow(dynspec_rmcorrected["U"][:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            combinedaxs_calrm[3].imshow(dynspec_rmcorrected["V"][:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            plt.figure(combinedfig_calrm.number)
            plt.tight_layout()
            plt.savefig("{0}-multipanel-dynspectrum_calibrated_RMcorrected{1}delta-psi{2}_delta-t{3}_phi{4}.png".format(src, label_rotmeas_save, delta_psi, delta_t, phi))
            combinedfig_calrm.clf()
            plt.figure(fig.number)
            plt.close('all')

    if args.isolate:
        # Time scrunch the U/I and Q/I ratios to plot the ratios vs. frequency
        threshold_time_average_indx = [np.where(np.mean(dynspec_rmcorrected["I"][binstart:binstop], 0) > args.threshold_factor*np.mean(dynrms["I"][binstart:binstop], 0))]
        print("Time average threshold indices: {0}".format(threshold_time_average_indx))
        print("Stokes I at said indices: {0}".format([dynspec_rmcorrected["I"][binstart:binstop][i][tuple(threshold_time_average_indx)] for i in np.arange(binstop-binstart)]))
        tscrunch_I_isolated = np.mean([dynspec_rmcorrected["I"][binstart:binstop][i][tuple(threshold_time_average_indx)] for i in np.arange(binstop-binstart)], 0)[0]
        print("Stokes I time scrunched (above 2.5sigma threshold): {0}; length of array: {1}".format(tscrunch_I_isolated, len(tscrunch_I_isolated)))
        tscrunch_U_isolated = np.mean([dynspec_rmcorrected["U"][binstart:binstop][i][tuple(threshold_time_average_indx)] for i in np.arange(binstop-binstart)], 0)[0]
        print("Stokes U time scrunched (above 2.5sigma threshold): {0}; length of array: {1}".format(tscrunch_U_isolated, len(tscrunch_U_isolated)))
        tscrunch_Q_isolated = np.mean([dynspec_rmcorrected["Q"][binstart:binstop][i][tuple(threshold_time_average_indx)] for i in np.arange(binstop-binstart)], 0)[0]
        print("Stokes Q time scrunched (above 2.5sigma threshold): {0}; length of array: {1}".format(tscrunch_Q_isolated, len(tscrunch_Q_isolated)))
        tscrunch_U_on_I_isolated = tscrunch_U_isolated / tscrunch_I_isolated
        print("tscrunched U/I isolated: {0}".format(tscrunch_U_on_I_isolated))
        tscrunch_Q_on_I_isolated = tscrunch_Q_isolated / tscrunch_I_isolated
        print("tscrunched Q/I isolated: {0}".format(tscrunch_Q_on_I_isolated))

        # Define good frequencies and lambda squared for thresholded time-averaged data
        freqs_threshold = freqs[tuple(threshold_time_average_indx)][0]
        print("Thresholded frequencies: {0}".format(freqs_threshold))
        lambda_sq_threshold = lambda_sq[tuple(threshold_time_average_indx)][0]
        print("Thresholded wavelengths: {0}".format(lambda_sq_threshold))

        # Fit the line to determine the flattest U/I and Q/I
        best_UonI_fit_coefs, best_UonI_fit_stats = P.polyfit(freqs_threshold, tscrunch_U_on_I_isolated, 1, full=True)
        print("U/I coefs: {0}".format(best_UonI_fit_coefs))
        print("U/I stats: {0}".format(best_UonI_fit_stats))
        UonI_fit_line = best_UonI_fit_coefs[0] + best_UonI_fit_coefs[1]*freqs_threshold

        best_QonI_fit_coefs, best_QonI_fit_stats = P.polyfit(freqs_threshold, tscrunch_Q_on_I_isolated, 1, full=True)
        print("Q/I coefs: {0}".format(best_QonI_fit_coefs))
        print("Q/I stats: {0}".format(best_QonI_fit_stats))
        QonI_fit_line = best_QonI_fit_coefs[0] + best_QonI_fit_coefs[1]*freqs_threshold

    # FIXME: add weighting to fit using rms
        # Get corrected polarisation position angle for tscrunched data
        pol_pa_tscrunched = 0.5*np.arctan2(tscrunch_U_isolated, tscrunch_Q_isolated)*180/np.pi
        pa_pre_rm_correction = 0.5*np.arctan2(np.mean(u_calibrated[binstart:binstop], 0), np.mean(q_calibrated[binstart:binstop], 0))
        pol_pa_coefs, pol_pa_stats = P.polyfit(lambda_sq_threshold, pol_pa_tscrunched, 1, full=True)
        print("PA coefs: {0}".format(pol_pa_coefs))
        print("PA stats: {0}".format(pol_pa_stats))
        pol_pa_fit_line = pol_pa_coefs[0] + pol_pa_coefs[1]*lambda_sq_threshold

        pa_fig, pa_ax = plt.subplots(figsize=(7,7))
        pa_ax.plot(lambda_sq,pa_pre_rm_correction, '.')
        pa_ax.legend(["pulse {0}".format(pulse_number)])
        pa_ax.set_ylabel("PA (rad)")
        pa_ax.set_xlabel("lambda^2")
        pa_fig.savefig('{0}-PA_pre-RMcorrection_pulse{1}.png'.format(src, pulse_number), bbox_inches = 'tight')

        uq_on_i_fig = plt.figure(figsize=(7,9))
        uq_on_i_ax0 = plt.subplot2grid((12,3), (0,0), rowspan=4, colspan=3)
        uq_on_i_ax1 = plt.subplot2grid((12,3), (4,0), rowspan=4, colspan=3, sharex=uq_on_i_ax0)
        uq_on_i_ax2 = plt.subplot2grid((12,3), (8,0), rowspan=4, colspan=3)
        plt.setp(uq_on_i_ax0.get_xticklabels(), visible=False)
        uq_on_i_fig.subplots_adjust(hspace=0)
        uq_on_i_ax0.tick_params(axis='x', direction='in')

        uq_on_i_ax0.plot(freqs_threshold, tscrunch_U_on_I_isolated, '.')
        uq_on_i_ax0.plot(freqs_threshold, UonI_fit_line, '-')
        uq_on_i_ax1.plot(freqs_threshold, tscrunch_Q_on_I_isolated, '.')
        uq_on_i_ax1.plot(freqs_threshold, QonI_fit_line, '-')
        uq_on_i_ax2.plot(lambda_sq_threshold, pol_pa_tscrunched, '.')
        uq_on_i_ax2.plot(lambda_sq_threshold, pol_pa_fit_line, '-')
        uq_on_i_ax0.legend(["RM={0} pulse {1}".format(label_rotmeas, pulse_number)])
        uq_on_i_ax1.legend(["RM={0} pulse {1}".format(label_rotmeas, pulse_number)])
        uq_on_i_ax2.legend(["RM={0} pulse {1}".format(label_rotmeas, pulse_number)])
        uq_on_i_ax1.set_xlabel("Frequency")
        uq_on_i_ax0.set_ylim(-1.5, 1.5)
        uq_on_i_ax1.set_ylim(-1.5, 1.5)
        uq_on_i_ax0.set_ylabel("U/I")
        uq_on_i_ax1.set_ylabel("Q/I")
        uq_on_i_ax2.set_xlabel("lambda^2")
        uq_on_i_ax2.set_ylabel("PA (deg)")
        uq_on_i_fig.savefig('{0}-RMcorrected.stokesUQonI_RMeq{1}_pulse{2}.png'.format(src, label_rotmeas_save, pulse_number), bbox_inches = 'tight')

        # Calculate and plot L pre-cal and post-cal for time-averaged pulse slice
        l_precal = np.sqrt(np.mean(dynspec["Q"][binstart:binstop],0)**2 + np.mean(dynspec["U"][binstart:binstop],0)**2)
        l_postcal = np.sqrt(np.mean(q_calibrated[binstart:binstop],0)**2 + np.mean(u_calibrated[binstart:binstop],0)**2)

        l_fig = plt.figure(figsize=(7,9))
        l_ax0 = plt.subplot2grid((8,3), (0,0), rowspan=4, colspan=3)
        l_ax1 = plt.subplot2grid((8,3), (4,0), rowspan=4, colspan=3, sharex=uq_on_i_ax0)
        plt.setp(l_ax0.get_xticklabels(), visible=False)
        l_fig.subplots_adjust(hspace=0)
        l_ax0.tick_params(axis='x', direction='in')

        l_ax0.plot(freqs, l_precal, '.')
        l_ax1.plot(freqs, l_postcal, '.')
        l_ax0.legend(["pulse {1} pre-cal".format(label_rotmeas, pulse_number)])
        l_ax1.legend(["pulse {1} post-cal".format(label_rotmeas, pulse_number)])
        l_ax1.set_xlabel("Frequency")
        l_ax0.set_ylabel("L amplitude")
        l_ax1.set_ylabel("L amplitude")
        l_fig.savefig('{0}-L_pre-post-cal_pulse{1}.png'.format(src, pulse_number), bbox_inches = 'tight')

    l_precal_dynspec = np.sqrt(dynspec["Q"]**2 + dynspec["U"]**2)
    l_postcal_dynspec = np.sqrt(q_calibrated**2 + u_calibrated**2)

    l_dynspec_fig, l_dynspec_ax = plt.subplots(2, 1, sharex=True, sharey=True, figsize=(4.5,15))
    l_dynspec_fig.suptitle(frbtitletext, x=0.25, y=0.99)
    l_dynspec_ax[1].set_xlabel("Time (ms)")
    for i in range(2):
        l_dynspec_ax[i].set_ylabel("Frequency (MHz)")
        if i==0:
            l_dynspec_ax[i].title.set_text("\n Pre-cal")
        else:
            l_dynspec_ax[i].title.set_text("\n Post-cal")
            l_dynspec_ax[0].imshow(l_precal_dynspec[:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            l_dynspec_ax[1].imshow(l_postcal_dynspec[:,startchan:endchan].transpose(), origin='lower', cmap=plt.cm.inferno, interpolation='none', extent=[starttime,endtime,startfreq,endfreq], aspect='auto')
            plt.figure(l_dynspec_fig.number)
            plt.tight_layout()
            plt.savefig("{0}-L_pre-post-cal_dynspec.png".format(src), bbox_inches = 'tight')
            l_dynspec_fig.clf()
            plt.figure(fig.number)
            plt.close('all')


#####################################################################################

# FSCRUNCH DATA WORK

#####################################################################################

# Plot the fscrunched time series if asked
if args.fscrunch:
    print("fscrunching...")
    centretimes = np.arange(starttime+res/2.0, endtime, res)
    print("Number of time bins: {0}".format(len(centretimes)))
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
    scrunch_ax0.tick_params(axis='x', direction='in')

    if args.firstlook:
        # Set up figure and axes for first look plot
        scrunch_fig_firstlook,scrunch_ax_firstlook = plt.subplots(figsize=(7,9))

    # Set up figure and axes for isolated pulse plot
    isolate_scrunch_fig = plt.figure(figsize=(7,9))
    isolate_scrunch_ax0 = plt.subplot2grid((7,3), (0,0), rowspan=2, colspan=3)
    isolate_scrunch_ax1 = plt.subplot2grid((7,3), (2,0), rowspan=5, colspan=3, sharex=isolate_scrunch_ax0)
    plt.setp(isolate_scrunch_ax0.get_xticklabels(), visible=False)
    isolate_scrunch_fig.subplots_adjust(hspace=0)
    isolate_scrunch_ax0.tick_params(axis='x', direction='in')

    # Set up figure and axes for log scale plots for Stokes I
    scrunch_log_fig, scrunch_log_ax = plt.subplots(figsize=(9,9))
    scrunch_log_ax.set_yscale('log', nonposy='clip')

    # Set up figure and axes if subbanding has been requested for PA and fscrunched spectra
    if args.subband:
        sub_scrunch_fig = plt.figure(figsize=(7,9))
        sub_scrunch_ax0 = plt.subplot2grid((7,3), (0,0), rowspan=2, colspan=3)
        sub_scrunch_ax1 = plt.subplot2grid((7,3), (2,0), rowspan=5, colspan=3, sharex=scrunch_ax0)
        plt.setp(sub_scrunch_ax0.get_xticklabels(), visible=False)
        sub_scrunch_fig.subplots_adjust(hspace=0)
        sub_scrunch_ax0.tick_params(axis='x', direction='in')

    for stokes in args.pols.split(','):

        if not args.firstlook:
            fscrunch_rmcorrected[stokes] = np.mean(dynspec_rmcorrected[stokes], 1)
            np.savetxt("{0}-imageplane-fscrunch-spectrum.calibrated.RMcorrected.stokes{1}.txt".format(src, stokes), fscrunch_rmcorrected[stokes])
            print("Frequency scrunched data size: {0}".format(fscrunch_rmcorrected[stokes].shape))

        # Subband the data and scrunch if requested
        if args.subband:
            subband_size = np.floor(nchan/numsub) # number of whole channels to be placed within a given subband
#            fscrunch_subband
            

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

        if args.firstlook:
            amp_jy = fscrunch[stokes][:] * 10000/res
        else:
            amp_jy = fscrunch_rmcorrected[stokes][:] * 10000/res

        # Log scale fit of Stokes I over specified range
        if stokes == "I":
            numlogfits = int(len(args.logstartstop.split(','))/2)
            logstarts = [int(start) for start in args.logstartstop.split(',')][::2] # Selects all even numbered indices (starts)
            logstops = [int(stop) for stop in args.logstartstop.split(',')][1::2]  # Selects all odd numbered indices (stops)
            poly_coeffs = {}
            amp_fit = {}
            max_value_log_plots = {}
            if numlogfits > 1:
                print("Multiple log fit ranges selected")
            for comp in np.arange(numlogfits):
                print("Log Fit Component {0}: bins {1} - {2}".format(comp+1, logstarts[comp], logstops[comp]))
                print(len(centretimes[logstarts[comp]:logstops[comp]]),len(np.log(amp_jy[logstarts[comp]:logstops[comp]])))
                poly_coeffs[comp] = np.polyfit(centretimes[logstarts[comp]:logstops[comp]], np.log(amp_jy[logstarts[comp]:logstops[comp]]), deg=1)
                print("Stokes I log fit coefficients: {}".format(poly_coeffs[comp]))
                amp_fit[comp] = centretimes*poly_coeffs[comp][0] + poly_coeffs[comp][1]
                print("Stokes I log fit line: {}".format(amp_fit[comp]))
                max_value_log_plots[comp] = 10 * np.round((np.max(amp_jy)/10)) + 10 # Round maximum amplitude value to the nearest factor of 10
                print("Nearest factor of ten for amplitude: {}".format(max_value_log_plots[comp]))
            max_value_log_plot = max(max_value_log_plots.values())
            print("Nearest factor of ten for amplitude to be used for plotting: {}".format(max_value_log_plot))

        if args.rms:
            rms_jy = fscrunchrms[stokes][:] * 10000/res
            # Diagnostic plot
            print("Amplitude array shape: {0} \n Time shape: {1}".format(amp_jy.shape, centretimes.shape))
            print("Amplitude rms (Jy): {}".format(rms_jy))
            scrunch_ax2_diag.errorbar(centretimes, amp_jy, yerr=rms_jy, label=stokes, color=col, linestyle=plotlinestyle, linewidth=1.5, capsize=2, elinewidth=2)
            # Publication plot
            scrunch_ax1.errorbar(centretimes, amp_jy, yerr=rms_jy, label=stokes, color=col, linestyle=plotlinestyle, linewidth=1.5, capsize=2, elinewidth=2)

            if args.firstlook:
                scrunch_ax_firstlook.errorbar(centretimes, amp_jy, yerr=rms_jy, label=stokes, color=col, linestyle=plotlinestyle, linewidth=1.5, capsize=2, elinewidth=2)
                print("Making first look averaged plot with error bars....")

            # If isolated pulse plot is requested
            if args.isolate:
                isolate_scrunch_ax1.errorbar(centretimes[binstart:binstop], amp_jy[binstart:binstop], yerr=rms_jy[binstart:binstop], label=stokes, color=col, linestyle=plotlinestyle, linewidth=1.5, capsize=2, elinewidth=2)

            # Log scale plot of Stokes I and fit
            if stokes == "I":
                # Create array of zeros and ones for upper limits on clipped point error bars
                upperlims = np.array(amp_jy < rms_jy)
                print("Amplitude (Jy): {}".format(amp_jy))
                print("Upper limits for log plot error bars: {0}; length of array {1}".format(upperlims, len(upperlims)))
                amp_jy_modified = np.where(upperlims==True, 2*rms_jy, amp_jy)
                scrunch_log_ax.errorbar(centretimes, amp_jy_modified, yerr=rms_jy, uplims=upperlims, lolims=False, label="Stokes "+stokes, color=col, linestyle='None', linewidth=1.5, capsize=2, elinewidth=2)
                for comp,colfitrange,colfit in zip(np.arange(numlogfits),['#80cdc1','#35978f','#8c510a'],['#dfc27d','#bf812d','#01665e']):
                    scrunch_log_ax.errorbar(centretimes[logstarts[comp]:logstops[comp]], amp_jy[logstarts[comp]:logstops[comp]], yerr=rms_jy[logstarts[comp]:logstops[comp]], label="Stokes "+stokes+" (used for fit of comp {})".format(comp+1), color=colfitrange, linestyle=plotlinestyle, linewidth=1.5, capsize=2, elinewidth=2)
                    scrunch_log_ax.plot(centretimes, np.e**amp_fit[comp], label='fit: comp {}'.format(comp+1), color=colfit, linestyle=plotlinestyle, linewidth=1.5)

        else:
            plt.title('   '+frbtitletext, loc='left', pad=-20)
            # Diagnostic plot
            scrunch_ax2_diag.plot(centretimes, amp_jy, label=stokes, color=col, linestyle=plotlinestyle)
            # Publication plot
            scrunch_ax1.plot(centretimes, amp_jy, label=stokes, color=col, linestyle=plotlinestyle)
            # If isolated pulse plot is requested
            if args.isolate:
                isolate_scrunch_ax1.plot(centretimes[binstart:binstop], amp_jy[binstart:binstop], label=stokes, color=col, linestyle=plotlinestyle)
                # Log scale plot of Stokes I
            if stokes == "I":
                for comp,colfitrange,colfit in zip(np.arange(numlogfits),['#80cdc1','#35978f'],['#dfc27d','#bf812d']):
                    scrunch_log_ax.plot(centretimes, amp_jy, label="Stokes "+stokes, color=col, linestyle=plotlinestyle)
                    scrunch_log_ax.plot(centretimes[logstarts[comp]:logstops[comp]], amp_jy[logstarts[comp]:logstops[comp]], label="Stokes "+stokes+" (used for fit of comp {})".format(comp+1), color=colfitrange, linestyle=plotlinestyle)
                    scrunch_log_ax.plot(centretimes, np.e**amp_fit[comp], label='fit: comp {}'.format(comp+1), color=colfit, linestyle=plotlinestyle, linewidth=1.5)

            if args.firstlook:
                scrunch_ax_firstlook.plot(centretimes, amp_jy, label=stokes, color=col, linestyle=plotlinestyle, linewidth=1.5)
                print("Making first look averaged plot sans error bars....")

    if not args.firstlook:
        if args.rms:
            # Get new, corrected polarisation position angle for fscrunched data
            pol_pa = 0.5*np.arctan2(fscrunch_rmcorrected["U"][1:], fscrunch_rmcorrected["Q"][1:])*180/np.pi
            total_linear_pol_flux_meas = np.sqrt(fscrunch_rmcorrected["Q"][1:]**2 + fscrunch_rmcorrected["U"][1:]**2)
            # Calculate a noise threshold for masking the polarisation position angle plot
            sigma_I = np.sqrt((fscrunchrms["Q"][1:]**2 + fscrunchrms["U"][1:]**2)/2)
            threshold_factor=args.threshold_factor

            # Correct for phase wrapping:
            if args.unwrap:
                for i in np.arange(len(pol_pa)):
                    if pol_pa[i] < 0: pol_pa[i]+=180

            # Check that sigma_I above and fscrunchrms["I"] are roughly equivalent
            sigma_I_fig = plt.figure(figsize=(7,7))
            sigma_I_ax0 = plt.subplot2grid((9,3), (0,0), rowspan=5, colspan=3)
            sigma_I_ax1 = plt.subplot2grid((9,3), (5,0), rowspan=4, colspan=3, sharex=sigma_I_ax0)
            sigma_I_ax0.plot(centretimes[1:], sigma_I*10000/res, label="sigma_I_Q,U")
            sigma_I_ax0.plot(centretimes[1:], (fscrunchrms["I"][1:]*10000/res), label="sigma_I_image")
            sigma_I_ax1.plot(centretimes[1:], (fscrunchrms["I"][1:]*10000/res)-(sigma_I*10000/res), label="sigma_I_image - sigma_I_Q,U")
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
            print("PA masked: {0}".format(pol_pa_masked))

            pa_rms = (180/np.pi) * (np.sqrt( ((fscrunch_rmcorrected["Q"][1:]**2 * fscrunchrms["U"][1:]**2) + (fscrunch_rmcorrected["U"][1:]**2 *fscrunchrms["Q"][1:]**2))/(4*(fscrunch_rmcorrected["Q"][1:]**2 + fscrunch_rmcorrected["U"][1:]**2)**2) ))
            print("PA rms: {0}".format(pa_rms))
            pol_pa_rms_to_mask = np.ma.array(pa_rms)
            pol_pa_rms_masked = np.ma.masked_where(total_linear_pol_flux_true < threshold_factor*sigma_I, pol_pa_rms_to_mask)
            pol_pa_rms_masked = np.ma.masked_where(total_linear_pol_flux_true == 0, pol_pa_rms_masked)

            # Save PA values and uncertainties
            #FIXME np.savetxt("{0}-PAs.txt".format(src), (pol_pa_masked,pol_pa_rms_masked))

            # Diagnostic plots
            scrunch_ax1_diag.set_title('   '+frbtitletext)
            scrunch_ax0_diag.errorbar(centretimes[1:], pol_pa, yerr=pa_rms, fmt='ko', markersize=2)
            if args.unwrap:
                scrunch_ax0_diag.set_ylim(0,180)
                scrunch_ax1_diag.set_ylim(0,180)
                scrunch_ax1_diag.errorbar(centretimes[1:], pol_pa_masked, yerr=pol_pa_rms_masked, fmt='ko', markersize=2, capsize=2)
                scrunch_ax1_diag.set_ylabel("Position Angle (deg)")
                scrunch_ax2_diag.legend()
                scrunch_ax2_diag.set_xlabel("Time (ms)")
                scrunch_ax2_diag.set_ylabel("Flux Density (Jy)")

            # Publication plots
            scrunch_ax0.set_title('   '+frbtitletext)
            scrunch_ax0.errorbar(centretimes[1:], pol_pa_masked, yerr=pol_pa_rms_masked, fmt='ko', markersize=2, capsize=2)
            scrunch_ax0.set_ylabel("Position Angle (deg)")
            if args.unwrap:
                scrunch_ax0.set_ylim(0,180)
                scrunch_ax1.legend()
                scrunch_ax1.set_xlabel("Time (ms)")
                scrunch_ax1.set_ylabel("Flux Density (Jy)")
                scrunch_fig_diag.savefig("{0}-fscrunch.RMcorrected_diagnostic_RM{1}.png".format(src, label_rotmeas_save), bbox_inches = 'tight')
                scrunch_fig.savefig("{0}-fscrunch.RMcorrected_RM{1}_delta-psi{2}_delta-t{3}_phi{4}.png".format(src, label_rotmeas_save, delta_psi, delta_t, phi), bbox_inches = 'tight')
                scrunch_fig.clf()

            # Plot isolated pulses if requested
            if args.isolate:
                isolate_scrunch_ax0.set_title('   '+frbtitletext)
                isolate_scrunch_ax0.errorbar(centretimes[binstart:binstop], pol_pa_masked[binstart:binstop], yerr=pol_pa_rms_masked[binstart:binstop], fmt='ko', markersize=2, capsize=2)
                isolate_scrunch_ax0.set_ylabel("Position Angle (deg)")
                isolate_scrunch_ax1.legend()
                isolate_scrunch_ax1.set_xlabel("Time (ms)")
                isolate_scrunch_ax1.set_ylabel("Flux Density (Jy)")
                isolate_scrunch_fig.savefig("{0}-fscrunch.RMcorrected_RM{1}_pulse{2}_delta-psi{3}_delta-t{4}_phi{5}.png".format(src, label_rotmeas_save, pulse_number, delta_psi, delta_t, phi), bbox_inches = 'tight')
                isolate_scrunch_fig.clf()

        else:
            # Publication plots
            scrunch_ax0.set_title('   '+frbtitletext)
            if args.unwrap:
                scrunch_ax0.set_ylim(0,180)
                scrunch_ax1.legend()
                scrunch_ax1.set_xlabel("Time (ms)")
                scrunch_ax1.set_ylabel("Flux Density (Jy)")
                scrunch_fig_diag.savefig("{0}-fscrunch.RMcorrected_diagnostic_RM{1}.png".format(src, label_rotmeas_save), bbox_inches = 'tight')
                scrunch_fig.savefig("{0}-fscrunch.RMcorrected_RM{1}_delta-psi{2}_delta-t{3}_phi{4}.png".format(src, label_rotmeas_save, delta_psi, delta_t, phi), bbox_inches = 'tight')
                scrunch_fig.clf()

            # Plot isolated pulses if requested
            if args.isolate:
                isolate_scrunch_ax0.set_title('   '+frbtitletext)
                isolate_scrunch_ax1.legend()
                isolate_scrunch_ax1.set_xlabel("Time (ms)")
                isolate_scrunch_ax1.set_ylabel("Flux Density (Jy)")
                isolate_scrunch_fig.savefig("{0}-fscrunch.RMcorrected_RM{1}_pulse{2}_delta-psi{3}_delta-t{4}_phi{5}.png".format(src, label_rotmeas_save, pulse_number, delta_psi, delta_t, phi), bbox_inches = 'tight')
                isolate_scrunch_fig.clf()

        # Log plots
        scrunch_log_ax.set_title('   '+frbtitletext)
        scrunch_log_ax.legend()
        scrunch_log_ax.set_xlabel("Time (ms)")
        scrunch_log_ax.set_ylabel("Flux Density (Jy)")
        scrunch_log_ax.set_ylim(top=max_value_log_plot)
        scrunch_log_fig.savefig("{0}-fscrunch.RMcorrected_logscale_pulse{1}_RM{2}.png".format(src, pulse_number, label_rotmeas_save), bbox_inches = 'tight')
        scrunch_log_fig.clf()

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
            scrunch_i_fig.savefig("{0}-fscrunch.RMcorrected.stokesI_delta-psi{1}_delta-t{2}_phi{3}.png".format(src, delta_psi, delta_t, phi), bbox_inches = 'tight')
            scrunch_i_fig.clf()

        # POLARISATION FRACTIONS:

        if args.rms:
            # Total intensity
            I = fscrunch_rmcorrected["I"][1:] * 10000/res
            I_rms = fscrunchrms["I"][1:] * 10000/res

            # Total linearly polarised flux
            L = total_linear_pol_flux_true * 10000/res
            print("L true (Jy): {0}".format(L))
            # Prepare the arrays for masking
            L_to_mask = np.ma.array(L)
            # Mask inf values
            L_masked = np.ma.masked_where(L == 0, L_to_mask)
            print("L true masked (Jy): {0}".format(L_masked))
            L_rms = np.sqrt(( ((fscrunch_rmcorrected["Q"][1:]*10000/res)**2 * (fscrunchrms["Q"][1:]*10000/res)**2) + ((fscrunch_rmcorrected["U"][1:]*10000/res)**2 * (fscrunchrms["U"][1:]*10000/res)**2)) / L_masked**2)
            print("L_rms: {0}".format(L_rms))
            # Prepare the arrays for masking
            I_to_mask = np.ma.array(I)
            I_rms_to_mask = np.ma.array(I_rms)
            # Mask inf values
            I_masked = np.ma.masked_where(L == 0, I_to_mask)
            I_rms_masked = np.ma.masked_where(L == 0, I_rms_to_mask)
            print("I: {0}".format(I_masked))
            print("I_rms: {0}".format(I_rms_masked))
            print("Q_rms {0}: ".format(fscrunchrms["Q"]*10000/res))

            # Total circularly polarised flux
            V = fscrunch_rmcorrected["V"][1:] * 10000/res
            V_rms = fscrunchrms["V"][1:] * 10000/res
            # Prepare the arrays for masking
            V_to_mask = np.ma.array(V)
            V_rms_to_mask = np.ma.array(V_rms)
            # Mask zero values
            V_masked = np.ma.masked_where(L == 0, V_to_mask)
            V_rms_masked = np.ma.masked_where(L == 0, V_rms_to_mask)

            # Total polarised flux
            P = np.sqrt(L_masked**2 + V_masked**2)
            P_rms = np.sqrt( ((L_masked**2 * L_rms**2) + (V_masked**2 * V_rms_masked**2)) / (L_masked**2 + V_masked**2) )

            # Plot the polarisations
            if args.isolate:
                pulse_num = ".pulse{0}".format(args.pulse_number)
                pol_fig, pol_ax = plt.subplots(figsize=(7,7))
                pol_ax.set_title('   '+frbtitletext)
                pol_ax.errorbar(centretimes[binstart:binstop], I[binstart:binstop], label="I", yerr=I_rms[binstart:binstop], linestyle='-', color='k', linewidth=1.5, elinewidth=2, capsize=2)
                pol_ax.errorbar(centretimes[binstart:binstop], V[binstart:binstop], label="V", yerr=V_rms[binstart:binstop], linestyle=':', color='#01665e', linewidth=1.5, elinewidth=2, capsize=2)
                pol_ax.errorbar(centretimes[binstart:binstop], L[binstart:binstop], label="L", yerr=L_rms[binstart:binstop], linestyle='--', color='#8c510a', linewidth=1.5, elinewidth=2, capsize=2)
                pol_ax.errorbar(centretimes[binstart:binstop], P[binstart:binstop], label="P", yerr=P_rms[binstart:binstop], linestyle='-.', color='#d8b365', linewidth=1.5, elinewidth=2, capsize=2)
                pol_ax.set_xlabel("Time (ms)")
                pol_ax.set_ylabel("Flux density (Jy)")
                pol_ax.legend(loc='upper right')
                pol_fig.savefig("{0}-polarisation.fluxes.RM{1}{2}.png".format(src, label_rotmeas_save, pulse_num), bbox_inches = 'tight')
                pol_fig.clf()

                # Weighted average total intensity and rms
                I_weighted_avg = np.sum(I_masked[binstart:binstop]*I_masked[binstart:binstop]) / np.sum(I_masked[binstart:binstop])
                I_weighted_avg_rms = np.sqrt( np.sum( I_rms_masked[binstart:binstop]**2 * I_masked[binstart:binstop]**2 )) / np.sum(I_masked[binstart:binstop])

                # Calculate the weighted average signal and noise for total linear polarisation
                L_weighted_avg = np.sum(L_masked[binstart:binstop]*I_masked[binstart:binstop]) / np.sum(I_masked[binstart:binstop])
                L_weighted_avg_rms = np.sqrt( np.sum(L_rms[binstart:binstop]**2 * I_masked[binstart:binstop]**2 )) / np.sum(I_masked[binstart:binstop])

                # Calculate the weighted average signal and noise for total circular polarisation
                V_weighted_avg = np.sum(V_masked[binstart:binstop]*I_masked[binstart:binstop]) / np.sum(I_masked[binstart:binstop])
                V_weighted_avg_rms = np.sqrt( np.sum(V_rms_masked[binstart:binstop]**2 * I_masked[binstart:binstop]**2 )) / np.sum(I_masked[binstart:binstop])

                # Calculate the weighted average signal and noise for total polarisation
                P_weighted_avg = np.sum(P[binstart:binstop]*I_masked[binstart:binstop]) / np.sum(I_masked[binstart:binstop])
                P_weighted_avg_rms = np.sqrt( np.sum(P_rms[binstart:binstop]**2 * I_masked[binstart:binstop]**2 )) / np.sum(I_masked[binstart:binstop])

                # Total fraction of polarised flux using the de-biased L_true; the full array for plotting and the weighted averages
                P_on_I = P / I_masked
                P_on_I_rms = np.sqrt(P_rms**2 + ((P_on_I)**2 * I_rms_masked**2)) / I_masked
                print("P/I rms: {0}".format(P_on_I_rms))
                P_on_I_weighted_avg = P_weighted_avg / I_weighted_avg
                P_on_I_weighted_avg_rms = np.sqrt(P_weighted_avg_rms**2 + ((P_weighted_avg/I_weighted_avg)**2 * I_weighted_avg_rms**2)) / I_weighted_avg
                # Total fraction of linearly polarised flux using the de-biased L_true and the weighted averages
                L_on_I = L_masked / I_masked
                print("L/I: {0}".format(L_on_I))
                L_on_I_rms = np.sqrt((L_rms**2) + (((L_masked/I_masked)**2) * (I_rms_masked**2))) / I_masked
                print("L/I rms: {0}".format(L_on_I_rms))
                L_on_I_weighted_avg = L_weighted_avg / I_weighted_avg
                L_on_I_weighted_avg_rms = np.sqrt(L_weighted_avg_rms**2 + ((L_weighted_avg/I_weighted_avg)**2 * I_weighted_avg_rms**2)) / I_weighted_avg
                # Total fraction of circularly polarised flux and the weighted averages
                V_on_I = V / I
                print("V/I: {0}".format(V_on_I))
                V_on_I_rms = np.sqrt(V_rms**2 + ((V/I)**2 * I_rms**2)) / I
                V_on_I_masked = V / I
                V_on_I_rms_masked = np.sqrt(V_rms_masked**2 + ((V_masked/I_masked)**2 * I_rms_masked**2)) / I_masked
                print("V/I rms: {0}".format(V_on_I_rms))
                V_on_I_weighted_avg = V_weighted_avg / I_weighted_avg
                V_on_I_weighted_avg_rms = np.sqrt(V_weighted_avg_rms**2 + ((V_weighted_avg/I_weighted_avg)**2 * I_weighted_avg_rms**2)) / I_weighted_avg

                # Print out polarisations and fractions for sanity check
                print("P weighted average: {0}".format(P_weighted_avg))
                print("L weighted average: {0}".format(L_weighted_avg))
                print("V weighted average: {0}".format(V_weighted_avg))
                print("I weighted average: {0}".format(I_weighted_avg))
                print("P/I weighted average: {0} +/- {1}".format(P_on_I_weighted_avg, P_on_I_weighted_avg_rms))
                print("L/I weighted average: {0} +/- {1}".format(L_on_I_weighted_avg, L_on_I_weighted_avg_rms))
                print("V/I weighted average: {0} +/- {1}".format(V_on_I_weighted_avg, V_on_I_weighted_avg_rms))


                # Plot the polarisation fractions
                polfrac_fig, polfrac_ax = plt.subplots(figsize=(7,7))
                polfrac_ax.set_title('   '+frbtitletext)
                polfrac_ax.errorbar(centretimes[binstart:binstop], V_on_I[binstart:binstop]*100, yerr=V_on_I_rms[binstart:binstop]*100, label="V/I", linestyle=':', color='#01665e', linewidth=1.5, elinewidth=2, capsize=2)
                polfrac_ax.errorbar(centretimes[binstart:binstop], P_on_I[binstart:binstop]*100, yerr=P_on_I_rms[binstart:binstop]*100, label="P/I", linestyle='-.', color='#d8b365', linewidth=1.5, elinewidth=2, capsize=2)
                polfrac_ax.errorbar(centretimes[binstart:binstop], L_on_I[binstart:binstop]*100, yerr=L_on_I_rms[binstart:binstop]*100, label="L/I", linestyle='--', color='#8c510a', linewidth=1.5, elinewidth=2, capsize=2)
                polfrac_ax.set_xlabel("Time (ms)")
                polfrac_ax.set_ylabel("Degree of polarisation")
                polfrac_ax.legend()
                polfrac_ax.set_ylim(-50,150)
                polfrac_ax.set_xlim(centretimes[binstart],centretimes[binstop])
                polfrac_fig.savefig("{0}-polarisation.fractions_diagnostic.pulse{1}_delta-psi{2}_delta-t{3}_phi{4}.png".format(src, pulse_number, delta_psi, delta_t, phi), bbox_inches = 'tight')
                polfrac_fig.clf()

                # Get total fractional polarisations (integrated across the sub pulses using the weighted sum;
                # weight each time bin by the total intensity and then sum the result; these quantities are used
                # to calculate a pulse-averaged polarisation fractions)
                print("Bin start and stop for pulse {0}: {1},{2}".format(pulse_number, binstart, binstop))
                print("Pulse {0} integrated from {1} ms to {2} ms".format(pulse_number, binstart*res, binstop*res))
                P_on_I_weighted_avg_total = P_on_I_weighted_avg * 100
                P_on_I_weighted_avg_total_rms = P_on_I_weighted_avg_rms * 100
                print("Total weighted average P/I over pulse {0}: {1} +/- {2}".format(pulse_number, P_on_I_weighted_avg_total, P_on_I_weighted_avg_total_rms))
                L_on_I_weighted_avg_total = L_on_I_weighted_avg * 100
                L_on_I_weighted_avg_total_rms = L_on_I_weighted_avg_rms * 100
                print("Total weighted average L/I over pulse {0}: {1} +/- {2}".format(pulse_number, L_on_I_weighted_avg_total, L_on_I_weighted_avg_total_rms))
                V_on_I_weighted_avg_total = V_on_I_weighted_avg * 100
                V_on_I_weighted_avg_total_rms = V_on_I_weighted_avg_rms * 100
                print("Total weighted average V/I over pulse {0}: {1} +/- {2}".format(pulse_number, V_on_I_weighted_avg_total, V_on_I_weighted_avg_total_rms))

    else:
    # Will run if args.firstlook is set

        # First look plots
        scrunch_ax_firstlook.set_title('   '+frbtitletext)
        scrunch_ax_firstlook.legend()
        scrunch_ax_firstlook.set_xlabel("Time (ms)")
        scrunch_ax_firstlook.set_ylabel("Flux Density (Jy)")
        scrunch_fig_firstlook.savefig("{0}-fscrunch.first.look.png".format(src), bbox_inches = 'tight')
        scrunch_fig_firstlook.clf()


else: print("Done!")


# ADD IN:
# Do subband fscrunching to determine the P/I, L/I, and V/I fractions as a function of frequency
