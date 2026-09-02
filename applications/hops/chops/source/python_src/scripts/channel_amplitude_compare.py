#!/usr/bin/python

#core imports
#-3.13++#from __future__ import print_function
#-3.13++#from __future__ import division
from builtins import str
from builtins import range
from mk4b.mk4b import old_div #from past.utils ...
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
matplotlib.use("Agg")

import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
matplotlib.rcParams.update({'savefig.dpi':300})
import pylab
import matplotlib.patches as mpatches

#HOPS module imports
import vpal.processing
import vpal.utility
import vpal.fringe_file_manipulation

import mk4b
import hopstestb as ht

################################################################################

def main():
    # usage_text = '\n channel_amplitude_compare.py [options] <control-file> <ref_station> <stations> <pol-product> <experiment-directory>' \
    #              '\n e.g.: channel_amplitude_compare.py ./cf_GHEVY_ff G HEV I ./'
    # parser = optparse.OptionParser(usage=usage_text)

    parser = argparse.ArgumentParser(
        prog='channel_phase_resid.py', \
        description='''utility for comparing XX and YY channel amplitudes (relative to mean amplitude)''' \
        )

    parser.add_argument('control_file', help='the control file to be applied to all scans')
    parser.add_argument('ref_station', help='single character code of station for channel-by-channel residual analysis')
    parser.add_argument('stations', help='concatenated string of single character codes for remote stations to use')
    #parser.add_argument('pol_product', help='the polarization-product to be fringe fit')
    parser.add_argument('experiment_directory', help='relative path to directory containing experiment data')

    parser.add_argument('-c', '--channels', dest='channels', help='specify the channels to be used, default=abcdefghijklmnopqrstuvwxyzABCDEF.', default='abcdefghijklmnopqrstuvwxyzABCDEF')
    parser.add_argument('-s', '--snr-min', type=float, dest='snr_min', help='set minimum allowed snr threshold, default=15.', default=20.)
    parser.add_argument('-q', '--quality-limit', type=int, dest='quality_lower_limit', help='set the lower limit on fringe quality (inclusive), default=3.', default=6)
    parser.add_argument('-b', '--begin-scan', dest='begin_scan_limit', help='limit the earliest scan to be used e.g 244-1719', default="000-0000")
    parser.add_argument('-e', '--end-scan', dest='end_scan_limit', help='limit the latest scan to be used, e.g. 244-2345', default="999-9999")

    args = parser.parse_args()

    #print('args: ', args)

    control_file = args.control_file
    ref_station = args.ref_station
    stations = args.stations
    #polprod = args.pol_product
    exp_dir = args.experiment_directory

    abs_exp_dir = os.path.abspath(exp_dir)
    exp_name = os.path.split(os.path.abspath(exp_dir))[1]

    if not os.path.isfile(os.path.abspath(control_file)):
        print("could not find control file: ", control_file)
        sys.exit(1)

    #pol product:
    #if polprod not in ['XX', 'YY', 'XY', 'YX', 'I']:
    #    print("polarization product must be one of: XX, YY, XY, YX, or I")
    #    sys.exit(1)

    polprod = ['XX','YY']
    
    #determine all possible baselines
    print('Calculating baselines')

    baseline_list = vpal.processing.construct_valid_baseline_list(abs_exp_dir, ref_station, stations, network_reference_baselines_only=True)
    #baseline_list = ['EV', 'SV', 'TV']
    #baseline_list = ['SV', 'TV']
    print('Baselines:', baseline_list)
    
    #qcode_list = []
    #for q in list(range(args.quality_lower_limit, 10)):
    #    qcode_list.append( str(q) )

    #needed for plot-naming
    #control_file_stripped = re.sub('[/\.]', '', control_file)

    #default output filename
    plot_name = "./channel_amplitude_compare_" + ref_station + "_" + stations + '_' + exp_name

    # initialize a dictionary to hold lists of channel phase residuals; this stores the nominal 32 VGOS channels
    #channel_amp = dict()
    #for ch in args.channels:
    #    channel_amp[ch] = list()

    freqs = dict()
    YY_amplitudes = dict()
    XX_amplitudes = dict()
    for ch in args.channels:
        freqs[ch] = -1.0
        XX_amplitudes[ch] = list()
        YY_amplitudes[ch] = list()
    
    for bline in baseline_list:

        print('Collecting fringe files for baseline',bline)

        # baselines may have different channel lists (eg a station may ignore some noisy channels),
        # but the frequencies should all be the same
        channel_freqs = list()

        
        #need to:
        #(1) collect all of the type_210 phase residuals,
        #(2) apply the snr, and quality code cuts
        #(3) for each channel, insert phase residual values and time stamps into array
        #(4) compute mean phase residual for each channel and remove it

        ################################################################################
        #collect/compute fringe files, and apply cuts
        #set_commands = "set gen_cf_record true"
        #ff_list_pre = vpal.processing.load_and_batch_fourfit( \
        #    os.path.abspath(exp_dir), bline[0], bline[1], os.path.abspath(control_file), set_commands, \
        #    num_processes=args.num_proc, start_scan_limit=args.begin_scan_limit, \
        #    stop_scan_limit=args.end_scan_limit, pol_products=[polprod], use_progress_ticker=args.use_progress_ticker \
        #)


        ff_list_pre = vpal.processing.gather_fringe_files(exp_dir, control_file, [bline], pol_products=polprod, max_depth=2)
        
        print("n fringe files  =", str(len(ff_list_pre)))
        
        #apply cuts
        ff_list = []

        for ff in ff_list_pre:
            if ff.snr >= args.snr_min and ff.quality >= args.quality_lower_limit and ff.scan_name>=args.begin_scan_limit and ff.scan_name<=args.end_scan_limit:
                ff_list.append(ff)
        
        if len(ff_list) == 0:
            print("Error: no fringe files available after cuts, skipping baseline: ", bline)

        else:


            #invert, unwrap, remove mean phase, and clamp to [-180, 180)
            for ff in ff_list:

                #if ff.snr < args.snr_min or ff.quality < args.quality_lower_limit:
                #    continue
                
                # make sure the number of channels is consistent for each scan in this baseline
                # if this is the first scan for this baseline (channel_freqs hasn't been initialized), we'll assign it to this set of channels
                chfreqs = ff.get_channel_frequency_tuples()
                if len(channel_freqs)>0:
                    if len(chfreqs) != len(channel_freqs):
                        print('Scan '+ff.scan_id+' has a different channel setup!')
                        sys.exit()
                else:
                    channel_freqs = chfreqs
                    #for ii in range(len(channel_freqs)):
                    #    XX_amplitudes[channel_freqs[ii][0]] = list()
                    #    YY_amplitudes[channel_freqs[ii][0]] = list()

                
                mf = mk4b.mk4fringe(ff.filename)

                for ii in range(len(channel_freqs)):

                    ff_pp_list = ht.get_file_polarization_product_provisional(ff.filename)
                    if len(ff_pp_list)>1:
                        continue
                    if ff_pp_list[0]=='XX':
                        XX_amplitudes[channel_freqs[ii][0]].append(mf.t210.contents.amp_phas[ii].ampl*10000 / ff.amp)
                    elif ff_pp_list[0]=='YY':
                        YY_amplitudes[channel_freqs[ii][0]].append(mf.t210.contents.amp_phas[ii].ampl*10000 / ff.amp)
                    else:
                        continue
                        
                    #print(ff.scan_name, ff.baseline, channel_freqs[ii][0], np.round(mf.t210.contents.amp_phas[ii].ampl*10000,3), np.round(ff.amp,3))


            if len(XX_amplitudes)==0 or len(YY_amplitudes)==0:
                print("Error: could not find channel amplitudes.")
                sys.exit(1)


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

        
    print("Number of scans:", len(XX_amplitudes[args.channels[0]]))
    
    XX_channel_mean_amp = dict()
    XX_channel_stddev = dict()
    YY_channel_mean_amp = dict()
    YY_channel_stddev = dict()
    for ch in args.channels:
        XX_channel_mean_amp[ch] = np.mean(XX_amplitudes[ch])
        XX_channel_stddev[ch] = np.std(XX_amplitudes[ch])
        #print( "(mean, std. dev) XX amplitude for channel: ", ch, " = ", XX_channel_mean_amp[ch], XX_channel_stddev[ch])
        print( "(mean, std. dev) phase for channel: ", ch, " =", '{:5.2f}'.format(XX_channel_mean_amp[ch]).rjust(5), '{:5.2f}'.format(XX_channel_stddev[ch]).ljust(5))

        YY_channel_mean_amp[ch] = np.mean(YY_amplitudes[ch])
        YY_channel_stddev[ch] = np.std(YY_amplitudes[ch])
        #print( "(mean, std. dev) YY amplitude for channel: ", ch, " = ", YY_channel_mean_amp[ch], YY_channel_stddev[ch])
        print( "(mean, std. dev) phase for channel: ", ch, " =", '{:5.2f}'.format(YY_channel_mean_amp[ch]).rjust(5), '{:5.2f}'.format(YY_channel_stddev[ch]).ljust(5))


    # store the frequencies and data in lists for plotting
    # I think we can assume the channel labels are in order of frequency
    YY_channel_amps = []
    XX_channel_amps = []
    channel_f = []
    channel_names = []
    for ch in args.channels:
        if freqs[ch]>0:
            channel_names.append(ch)
            YY_channel_amps.append(YY_amplitudes[ch])
            XX_channel_amps.append(XX_amplitudes[ch])
            channel_f.append(freqs[ch])
        else:
            YY_channel_amps.append(0.0)
            XX_channel_amps.append(0.0)
            channel_f.append(0.0)
    
        
    fig_width_pt = 600  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    #golden_mean = (2.236-1.0)/2.0         # Aesthetic ratio
    golden_mean = 0.5
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width*golden_mean      # height in inches
    fig_size = [fig_width,fig_height]
                                                                        

    matplotlib.rcParams.update({'savefig.dpi':350,
    #                            'text.usetex':True,
                                'figure.figsize':fig_size})#,
    #                            'font.family':"serif",
    #                            'font.serif':["Times"]})
    
    
    fig = pylab.figure(np.random.randint(0,1000))

    ax0 = plt.subplot(111)
    pylab.title(r'Channel-by-channel normalized amplitudes ($\div$ by total amp) for station '+ref_station+' in experiment '+exp_name, fontsize=10, y=1.06)
    
    ax0.spines['top'].set_color('none')
    ax0.spines['bottom'].set_color('none')
    ax0.spines['left'].set_color('none')
    ax0.spines['right'].set_color('none')
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
                pylab.errorbar(freqs[ch], YY_channel_mean_amp[ch], yerr=YY_channel_stddev[ch], fmt='ks', markersize=3, ecolor='k', elinewidth=0.8, capsize=2, capthick=1)
                pylab.errorbar(freqs[ch], XX_channel_mean_amp[ch], yerr=XX_channel_stddev[ch], fmt='ks', markersize=3, ecolor='k', elinewidth=0.8, capsize=2, capthick=1)
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
            parts_XX = ax.violinplot([XX_channel_amps[i] for i in bidx], [channel_f[i] for i in bidx], points=100, widths=0.04, showmeans=False, showextrema=False, showmedians=False)
            parts_YY = ax.violinplot([YY_channel_amps[i] for i in bidx], [channel_f[i] for i in bidx], points=100, widths=0.04, showmeans=False, showextrema=False, showmedians=False)
            for pc in parts_XX['bodies']:
                pc.set_facecolor('orangered')
                pc.set_alpha(0.5)
            for pc in parts_YY['bodies']:
                pc.set_facecolor('teal')
                pc.set_alpha(0.5)

            if ii==0:
                labels = []
                color = parts_XX["bodies"][0].get_facecolor().flatten()
                labels.append((mpatches.Patch(color=color), 'XX'))
                color = parts_YY["bodies"][0].get_facecolor().flatten()
                labels.append((mpatches.Patch(color=color), 'YY'))
                pylab.legend(*zip(*labels), loc='upper left',fontsize=8)
            
                
        else:
            xmin = 1.0
            xmax = 2.0

                
        pylab.grid(True, which='both', linestyle=':', alpha=0.6)
        pylab.xticks(fontsize=7)

        pylab.ylim(0,3.0)
        if ii==0:
            pylab.yticks(fontsize=7)
            pylab.ylabel('normalized amplitude', fontsize=8)
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
