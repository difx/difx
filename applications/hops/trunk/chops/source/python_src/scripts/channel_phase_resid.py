#!/usr/bin/python

#core imports
from __future__ import print_function
from __future__ import division
from builtins import str
from builtins import range
from past.utils import old_div
import argparse
import sys
import os
import math
import re
from datetime import datetime

#non-core imports
import numpy as np
import scipy.stats

import matplotlib
matplotlib.use("Agg", warn=False)

import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
matplotlib.rcParams.update({'savefig.dpi':300})
import pylab

#HOPS module imports
import vpal.processing
import vpal.utility
import vpal.fringe_file_manipulation


################################################################################

def main():
    # usage_text = '\n channel_phase_resid.py [options] <control-file> <ref_station> <stations> <pol-product> <experiment-directory>' \
    #              '\n e.g.: channel_phase_resid.py ./cf_GHEVY_ff G HEV I ./'
    # parser = optparse.OptionParser(usage=usage_text)

    parser = argparse.ArgumentParser(
        prog='channel_phase_resid.py', \
        description='''utility for plotting channel-by-channel phase residuals''' \
        )

    parser.add_argument('control_file', help='the control file to be applied to all scans')
    parser.add_argument('ref_station', help='single character code of station for channel-by-channel residual analysis')
    parser.add_argument('stations', help='concatenated string of single character codes for all stations to be fringe fit')
    parser.add_argument('pol_product', help='the polarization-product to be fringe fit')
    parser.add_argument('experiment_directory', help='relative path to directory containing experiment data')

    parser.add_argument('-n', '--numproc', type=int, dest='num_proc', help='number of concurrent fourfit jobs to run, default=1', default=1)
    parser.add_argument('-c', '--channels', dest='channels', help='specify the channels to be used, default=abcdefghijklmnopqrstuvwxyzABCDEF.', default='abcdefghijklmnopqrstuvwxyzABCDEF')
    parser.add_argument('-s', '--snr-min', type=float, dest='snr_min', help='set minimum allowed snr threshold, default=15.', default=15.)
    #parser.add_argument('-d', '--dtec-threshold', type=float, dest='dtec_thresh', help='set maximum allowed difference in dTEC, default=1.', default=1.0)
    parser.add_argument('-q', '--quality-limit', type=int, dest='quality_lower_limit', help='set the lower limit on fringe quality (inclusive), default=3.', default=3)
    parser.add_argument('-p', '--progress', action='store_true', dest='use_progress_ticker', help='monitor process with progress indicator', default=False)
    parser.add_argument('-b', '--begin-scan', dest='begin_scan_limit', help='limit the earliest scan to be used e.g 244-1719', default="000-0000")
    parser.add_argument('-e', '--end-scan', dest='end_scan_limit', help='limit the latest scan to be used, e.g. 244-2345', default="999-9999")
    #parser.add_argument('-r', '--remove-outliers', dest='remove_outlier_nsigma', help='remove scans which are n*sigma away from the mean, default=0 (off)', default=0.0 )


    args = parser.parse_args()

    #print('args: ', args)

    control_file = args.control_file
    ref_station = args.ref_station
    stations = args.stations
    polprod = args.pol_product
    exp_dir = args.experiment_directory

    abs_exp_dir = os.path.abspath(exp_dir)
    exp_name = os.path.split(os.path.abspath(exp_dir))[1]
    #rnsigma = float(args.remove_outlier_nsigma)

    if not os.path.isfile(os.path.abspath(control_file)):
        print("could not find control file: ", control_file)
        sys.exit(1)

    #pol product:
    if polprod not in ['XX', 'YY', 'XY', 'YX', 'I']:
        print("polarization product must be one of: XX, YY, XY, YX, or I")
        sys.exit(1)

    
    #determine all possible baselines
    print('Calculating baselines')

    baseline_list = vpal.processing.construct_valid_baseline_list(abs_exp_dir, ref_station, stations, network_reference_baselines_only=True)

    print('Baselines:', baseline_list)
    
    qcode_list = []
    for q in list(range(args.quality_lower_limit, 10)):
        qcode_list.append( str(q) )

    #needed for plot-naming
    control_file_stripped = re.sub('[/\.]', '', control_file)

    #default output filename
    plot_name = "./channel_phaseresid_" + ref_station + "_" + stations + '_' + exp_name

    # initialize a dictionary to hold lists of channel phase residuals; this stores the nominal 32 VGOS channels
    channel_phase = dict()
    for ch in args.channels:
        channel_phase[ch] = list()

    freqs = dict()
    for ch in args.channels:
        freqs[ch] = -1.0
        
        
    for bline in baseline_list:

        print('Collecting fringe files for baseline',bline)

        # baselines may have different channel lists (eg a station may ignore some noisy channels),
        # but the frequencies should all be the same
        channel_freqs = list()#ff.get_channel_frequency_tuples()

        
        #need to:
        #(1) collect all of the type_210 phase residuals,
        #(2) apply the snr, and quality code cuts
        #(3) for each channel, insert phase residual values and time stamps into array
        #(4) compute mean phase residual for each channel and remove it

        ################################################################################
        #collect/compute fringe files, and apply cuts
        set_commands = "set gen_cf_record true"
        ff_list_pre = vpal.processing.load_and_batch_fourfit( \
            os.path.abspath(exp_dir), bline[0], bline[1], os.path.abspath(control_file), set_commands, \
            num_processes=args.num_proc, start_scan_limit=args.begin_scan_limit, \
            stop_scan_limit=args.end_scan_limit, pol_products=[polprod], use_progress_ticker=args.use_progress_ticker \
        )

        print("n fringe files  =", str(len(ff_list_pre)))
        
        #apply cuts
        ff_list = []

        for ff in ff_list_pre:
            if ff.snr >= args.snr_min and ff.quality >= args.quality_lower_limit:
                ff_list.append(ff)
        
        if len(ff_list) == 0:
            print("Error: no fringe files available after cuts, skipping baseline: ", bline)

        else:

            #loop over fringe files and collect the phase residuals
            phase_residuals = list()

            #invert, unwrap, remove mean phase, and clamp to [-180, 180)
            for ff in ff_list:

                # make sure the number of channels is consistent for each scan in this baseline
                chfreqs = ff.get_channel_frequency_tuples()
                if len(channel_freqs)>0:
                    if len(chfreqs) != len(channel_freqs):
                        print('Scan '+ff.scan_id+' has a different channel setup!')
                        sys.exit()
                else:
                    channel_freqs = chfreqs

                phresid = vpal.fringe_file_manipulation.PhaseResidualData()
                phresid.extract(ff.filename)
                if phresid.is_valid is True:
                    phase_index = []
                    phase_list_proxy = []
                    for ch,ph in list(phresid.phase_residuals.items()):
                        phase_index.append(ch)
                        phase_list_proxy.append(ph)
                    phase_list_proxy = [-1.0*(old_div(math.pi,180.0))*x for x in phase_list_proxy] #negate and convert to radians
                    phase_list_proxy = np.unwrap(phase_list_proxy) #arguments must be in radians
                    phase_list_proxy = [(old_div(180.0,math.pi))*x for x in phase_list_proxy] #convert back to degrees
                    mean_phase = scipy.stats.circmean( np.asarray(phase_list_proxy), high=180.0, low=-180.0) #compute circular mean phase
                    phase_list_proxy = [ (x - mean_phase) for x in phase_list_proxy] #subtract off the mean
                    for i in list(range(0,len(phase_index))):
                        ch = phase_index[i]
                        limited_phase = vpal.utility.limit_periodic_quantity_to_range(phase_list_proxy[i], low_value=-180.0, high_value=180.0)
                        phresid.phase_residuals[ch] = limited_phase
                    phase_residuals.append(phresid)


            if len(phase_residuals) == 0:
                print("Error: could not find phase residuals")
                sys.exit(1)


            print("Processing phase residuals for baseline: ", bline )
            for ch in args.channels:
                for phr in phase_residuals:
                    if ch in phr.phase_residuals:
                        channel_phase[ch].append(phr.phase_residuals[ch])
                        if abs(phr.phase_residuals[ch]) > 180:
                            print("Scan: ", phr.scan_name, "has channel phase of: ", phr.phase_residuals[ch] )



        # store dict of frequencies in GHz
        # check that the frequency is the same as previous baselines
        for chan in channel_freqs:
            if freqs[chan[0]] == chan[2]/1e9:
                continue
            elif freqs[chan[0]] < 0.0:
                freqs[chan[0]] = chan[2]/1e9
            else:
                print('Channel '+chan[0]+' has frequency '+str(chan[2]/1e9)+' but it should be '+str(freqs[chan[0]]))
                sys.exit(1)


                            
    #print("Number of scans:", len(channel_phase[args.channels[0]]))
    
    channel_mean_phase = dict()
    channel_stddev = dict()
    for ch in args.channels:
        channel_mean_phase[ch] = scipy.stats.circmean( np.asarray(channel_phase[ch]), high=180.0, low=-180.0) #compute circular mean phase
        channel_stddev[ch] = scipy.stats.circstd( np.asarray(channel_phase[ch]), high=180, low=-180) #compute circular std dev.
        print( "(mean, std. dev) phase for channel: ", ch, " = ", channel_mean_phase[ch], channel_stddev[ch])



    # store the frequencies and data in lists for plotting
    # I think we can assume the channel labels are in order of frequency
    channel_resids = []
    channel_f = []
    channel_names = []
    for ch in args.channels:
        if freqs[ch]>0:
            channel_names.append(ch)
            channel_resids.append(channel_phase[ch])
            channel_f.append(freqs[ch])
        else:
            channel_resids.append(0.0)
            channel_f.append(0.0)
    
        
    fig_width_pt = 600  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    #golden_mean = (2.236-1.0)/2.0         # Aesthetic ratio
    golden_mean = 0.5
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width*golden_mean      # height in inches
    fig_size = [fig_width,fig_height]
                                                                        

    matplotlib.rcParams.update({'savefig.dpi':350,
                                'text.usetex':True,
                                'figure.figsize':fig_size,
                                'font.family':"serif",
                                'font.serif':["Times"]})
    
    
    fig = pylab.figure(np.random.randint(0,1000))

    ax0 = plt.subplot(111)
    pylab.title('Channel-by-channel phase residuals for station '+ref_station+' in experiment '+exp_name, fontsize=10, y=1.06)
    
    ax0.spines['top'].set_color('none')
    ax0.spines['bottom'].set_color('none')
    ax0.spines['left'].set_color('none')
    ax0.spines['top'].set_color('none')
    plt.tick_params(labelcolor='none', which='both', top=False, bottom=False, left=False, right=False)
    plt.xlabel('Channel Frequency [GHz]', labelpad=2, fontsize=8)
    
    for ii in list(range(4)):

        ax = fig.add_subplot(1,4,ii+1)

        # first, plot the per-channel mean and stdev as errobars, store the freq of each channel for the top tickmarks
        xtick_locs = list()
        xtick_labels = list()
        band_freqs = list()
        bidx = list()
        for jj in list(range(8*ii,8*ii+8)):
            ch = args.channels[jj]
            if ch in channel_names:
                pylab.errorbar(freqs[ch], channel_mean_phase[ch], yerr=channel_stddev[ch], fmt='ks', markersize=3, ecolor='k', elinewidth=0.8, capsize=2, capthick=1)
                xtick_locs.append(freqs[ch])
                xtick_labels.append(ch)
                band_freqs.append(freqs[ch]) # keep track of the frequencies, for setting the plot range
                bidx.append(jj) # keep track of the indices, for the violin plots
            else:
                print('Channel '+ch+' is missing!')

        if len(band_freqs)>0:
            xmin = min(band_freqs)-0.03
            xmax = max(band_freqs)+0.03
            pylab.xlim(xmin,xmax)

            # add the violin plots
            parts = ax.violinplot([channel_resids[i] for i in bidx], [channel_f[i] for i in bidx], points=100, widths=0.04, showmeans=False, showextrema=False, showmedians=False)
            for pc in parts['bodies']:
                pc.set_facecolor('orangered')
                pc.set_alpha(0.5)

        else:
            xmin = 1.0
            xmax = 2.0

                
        pylab.grid(True, which='both', linestyle=':', alpha=0.6)
        pylab.xticks(fontsize=7)

        pylab.ylim(-180,180)        
        if ii==0:
            pylab.yticks(fontsize=7)
            pylab.ylabel('Phase Residual [deg]', fontsize=8)
        else:
            pylab.yticks(visible=False)


        # twin in the y-direction and label the channels
        axn = ax.twiny()

        pylab.xlim(xmin,xmax)
        pylab.xticks(xtick_locs, xtick_labels, fontsize=7)
        pylab.yticks(visible=False)

    
    pylab.savefig(plot_name + '.png', bbox_inches='tight')
    pylab.close()




if __name__ == '__main__':          # official entry point
    main()
    sys.exit(0)
