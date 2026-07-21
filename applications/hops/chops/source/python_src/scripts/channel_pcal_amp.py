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
#import scipy.stats

import matplotlib
matplotlib.use("Agg")

import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
matplotlib.rcParams.update({'savefig.dpi':300})
import pylab
import matplotlib.patches as mpatches

#HOPS module imports
import vpal.processing
#import vpal.utility
import vpal.fringe_file_manipulation

import mk4b
import hopstestb as ht

################################################################################

def main():
    # usage_text = '\n channel_pcal_amp.py [options] <control-file> <ref_station> <stations> <pol-product> <experiment-directory>' \
    #              '\n e.g.: channel_pcal_amp.py ./cf_GHEVY_ff G HEV I ./'
    # parser = optparse.OptionParser(usage=usage_text)

    parser = argparse.ArgumentParser(
        prog='channel_pcal_amp.py', \
        description='''utility for plotting channel-by-channel phasecal amplitudes''' \
        )

    parser.add_argument('control_file', help='the control file used for the fringe fitting')
    parser.add_argument('ref_station', help='single character code of station for channel-by-channel residual analysis')
    parser.add_argument('stations', help='concatenated string of single character codes for remote stations to use')
    #parser.add_argument('pol_product', help='the polarization-product to be used')
    parser.add_argument('experiment_directory', help='relative path to directory containing experiment data')

    #parser.add_argument('-n', '--numproc', type=int, dest='num_proc', help='number of concurrent fourfit jobs to run, default=1', default=1)
    parser.add_argument('-c', '--channels', dest='channels', help='specify the channels to be used, default=abcdefghijklmnopqrstuvwxyzABCDEF.', default='abcdefghijklmnopqrstuvwxyzABCDEF')
    parser.add_argument('-s', '--snr-min', type=float, dest='snr_min', help='set minimum allowed snr threshold, default=15.', default=15.)
    #parser.add_argument('-d', '--dtec-threshold', type=float, dest='dtec_thresh', help='set maximum allowed difference in dTEC, default=1.', default=1.0)
    parser.add_argument('-q', '--quality-limit', type=int, dest='quality_lower_limit', help='set the lower limit on fringe quality (inclusive), default=3.', default=3)
    parser.add_argument('-g', '--errcode', type=str, dest='errcode', help='fringe error code to filter, default=None', default=None)
    #parser.add_argument('-p', '--progress', action='store_true', dest='use_progress_ticker', help='monitor process with progress indicator', default=False)
    parser.add_argument('-b', '--begin-scan', dest='begin_scan_limit', help='limit the earliest scan to be used e.g 244-1719', default="000-0000")
    parser.add_argument('-e', '--end-scan', dest='end_scan_limit', help='limit the latest scan to be used, e.g. 244-2345', default="999-9999")
    #parser.add_argument('-r', '--remove-outliers', dest='remove_outlier_nsigma', help='remove scans which are n*sigma away from the mean, default=0 (off)', default=0.0 )


    args = parser.parse_args()

    #print('args: ', args)

    control_file = args.control_file
    ref_station = args.ref_station
    stations = args.stations
    polprod = ['XX','YY']
    exp_dir = args.experiment_directory
    errcode = args.errcode

    abs_exp_dir = os.path.abspath(exp_dir)
    exp_name = os.path.split(os.path.abspath(exp_dir))[1]
    #rnsigma = float(args.remove_outlier_nsigma)

    if not os.path.isfile(os.path.abspath(control_file)):
        print("could not find control file: ", control_file)
        sys.exit(1)

    #pol product:
    #if polprod not in ['XX', 'YY', 'XY', 'YX', 'I']:
    #    print("polarization product must be one of: XX, YY, XY, YX, or I")
    #    sys.exit(1)

    
    #determine all possible baselines
    print('Calculating baselines')

    baseline_list = vpal.processing.construct_valid_baseline_list(abs_exp_dir, ref_station, stations, network_reference_baselines_only=True)

    print('Baselines:', baseline_list)
    
    #qcode_list = []
    #for q in list(range(args.quality_lower_limit, 10)):
    #    qcode_list.append( str(q) )

    #needed for plot-naming
    #control_file_stripped = re.sub('[/\.]', '', control_file)

    #default output filename
    if errcode is not None:
        plot_name = "./channel_pcal_amp_" + ref_station + "_" + stations + '_' + errcode + '_' + exp_name
    else:
        plot_name = "./channel_pcal_amp_" + ref_station + "_" + stations + '_' + exp_name
        
    # initialize a dictionary to hold lists of channel phase residuals; this stores the nominal 32 VGOS channels
    channel_pcal_amp = dict()
    channel_XX_pcal_amp = dict()
    channel_YY_pcal_amp = dict()
    for ch in args.channels:
        channel_pcal_amp[ch] = list()
        channel_XX_pcal_amp[ch] = list()
        channel_YY_pcal_amp[ch] = list()

    freqs = dict()
    for ch in args.channels:
        freqs[ch] = -1.0
        
        
    for bline in baseline_list:

        print('Collecting fringe files for baseline',bline)
        
        # baselines may have different channel lists (eg a station may ignore some noisy channels),
        # but the frequencies should all be the same
        channel_freqs = list()
        
        ff_list_pre = vpal.processing.gather_fringe_files(exp_dir, control_file, [bline], pol_products=polprod, max_depth=2)
        
        print("n fringe files =", str(len(ff_list_pre)))
        
        #apply cuts
        ff_list = []

        for ff in ff_list_pre:
            if ff.snr >= args.snr_min and ff.quality >= args.quality_lower_limit and ff.scan_name >= args.begin_scan_limit and ff.scan_name<=args.end_scan_limit:
                if errcode is not None:
                    if ff.errcode==errcode:
                        ff_list.append(ff)
                else:
                    ff_list.append(ff)


                    
        if len(ff_list) == 0:
            print("Error: no fringe files available after cuts, skipping baseline: ", bline)

        else:

            print("n fringe files passing cuts =", str(len(ff_list)))

            
            ref_flag=True
            if bline[0]==args.ref_station:
                print('Collecting data from ',bline,'reference station')
            elif bline[1]==args.ref_station:
                ref_flag=False
                print('Collecting data from ',bline,'remote station')
                
            #loop over fringe files and collect the phase residuals
            pcal_amps = dict()

            for ff in ff_list:

                # make sure the number of channels is consistent for each scan in this baseline
                # if this is the first scan for this baseline (channel_freqs hasn't been initialized), we'll assign it to this set of channels
                chfreqs = ff.get_channel_frequency_tuples()
                if len(channel_freqs)>0:
                    if len(chfreqs) != len(channel_freqs):
                        print('Scan '+ff.scan_id+' has a different channel setup!')
                        sys.exit()
                else:
                    channel_freqs = chfreqs
                    for ii in range(len(channel_freqs)):
                        pcal_amps[channel_freqs[ii][0]] = list()

                
                mf = mk4b.mk4fringe(ff.filename)

                ff_pp_list = ht.get_file_polarization_product_provisional(ff.filename)
                if len(ff_pp_list)>1:
                    continue
                
                for ii in range(len(channel_freqs)):
                    # choose reference or remote station pcals
                    if ref_flag:
                        if ff_pp_list[0]=='XX':
                            channel_XX_pcal_amp[channel_freqs[ii][0]].append(mf.t207.contents.ref_pcamp[ii].usb*1000)
                        elif ff_pp_list[0]=='YY':
                            channel_YY_pcal_amp[channel_freqs[ii][0]].append(mf.t207.contents.ref_pcamp[ii].usb*1000)
                    else:
                        if ff_pp_list[0]=='XX':
                            channel_XX_pcal_amp[channel_freqs[ii][0]].append(mf.t207.contents.rem_pcamp[ii].usb*1000)
                        elif ff_pp_list[0]=='YY':
                            channel_YY_pcal_amp[channel_freqs[ii][0]].append(mf.t207.contents.rem_pcamp[ii].usb*1000)


                    #pcal_amps[channel_freqs[ii][0]].append(mf.t207.contents.ref_pcamp[ii].usb*1000)
                    #print(ff.scan_name, ff.baseline, channel_freqs[ii][0], np.round(mf.t210.contents.amp_phas[ii].ampl*10000,3), np.round(ff.amp,3))

            # will this fail if we're missing band A?
            #if len(channel_XX_pcal_amp[args.channels[0]])==0 or len(channel_YY_pcal_amp[args.channels[0]])==0:
            #    print("Error: could not find pcal amplitudes for .")
            #    #sys.exit(1)


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

                            
    print("Number of scans:", len(channel_XX_pcal_amp[args.channels[0]]))
    
    channel_XX_mean_pcal_amp = dict()
    channel_XX_stddev = dict()
    channel_YY_mean_pcal_amp = dict()
    channel_YY_stddev = dict()
    for ch in args.channels:
        if len(channel_XX_pcal_amp[ch])==0 or len(channel_YY_pcal_amp[ch])==0:
            channel_XX_mean_pcal_amp[ch] = 0.0
            channel_XX_stddev[ch] = 0.0
            channel_YY_mean_pcal_amp[ch] = 0.0
            channel_YY_stddev[ch] = 0.0
        else:
            channel_XX_mean_pcal_amp[ch] = np.mean(channel_XX_pcal_amp[ch])
            channel_XX_stddev[ch] = np.std(channel_XX_pcal_amp[ch])
            channel_YY_mean_pcal_amp[ch] = np.mean(channel_YY_pcal_amp[ch])
            channel_YY_stddev[ch] = np.std(channel_YY_pcal_amp[ch])
        #print( "(mean, std. dev) X phasecal amplitude for channel: ", ch, " = ", channel_XX_mean_pcal_amp[ch], channel_XX_stddev[ch])
        #print( "(mean, std. dev) Y phasecal amplitude for channel: ", ch, " = ", channel_YY_mean_pcal_amp[ch], channel_YY_stddev[ch])
        
        print( "(mean, std. dev) phase for channel: ", ch, " =", '{:5.2f}'.format(channel_XX_mean_pcal_amp[ch]).rjust(5), '{:5.2f}'.format(channel_XX_stddev[ch]).ljust(5))
        print( "(mean, std. dev) phase for channel: ", ch, " =", '{:5.2f}'.format(channel_YY_mean_pcal_amp[ch]).rjust(5), '{:5.2f}'.format(channel_YY_stddev[ch]).ljust(5))

        

    # store the frequencies and data in lists for plotting
    # I think we can assume the channel labels are in order of frequency
    channel_XX_pc_amps = []
    channel_YY_pc_amps = []
    channel_f = []
    channel_names = []
    for ch in args.channels:
        if freqs[ch]>0:
            channel_names.append(ch)
            if len(channel_XX_pcal_amp[ch])==0 or len(channel_YY_pcal_amp[ch])==0:
                channel_XX_pc_amps.append([0.0]) # this is a list of lists, instead of a dict of lists
                channel_YY_pc_amps.append([0.0])
            else:
                channel_XX_pc_amps.append(channel_XX_pcal_amp[ch]) # this is a list of lists, instead of a dict of lists
                channel_YY_pc_amps.append(channel_YY_pcal_amp[ch])
            channel_f.append(freqs[ch])
        else:
            channel_names.append(ch)
            channel_XX_pc_amps.append(channel_XX_pcal_amp[ch])
            channel_YY_pc_amps.append(channel_YY_pcal_amp[ch])
            channel_f.append(0.0)

        
    fig_width_pt = 600  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    #golden_mean = (2.236-1.0)/2.0         # Aesthetic ratio
    golden_mean = 0.5
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width*golden_mean      # height in inches
    fig_size = [fig_width,fig_height]
                                                                        

    matplotlib.rcParams.update({'savefig.dpi':350,
                                'figure.figsize':fig_size})#,
    #                            'text.usetex':True,
    #                            'font.family':"serif",
    #                            'font.serif':["Times"]})
    
    
    fig = pylab.figure(np.random.randint(0,1000))

    ax0 = plt.subplot(111)
    pylab.title(r'Channel-by-channel phasecal amplitudes for station '+ref_station+' in experiment '+exp_name, fontsize=10, y=1.06)
    
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
                pylab.errorbar(freqs[ch], channel_XX_mean_pcal_amp[ch], yerr=channel_XX_stddev[ch], fmt='ks', markersize=3,
                               ecolor='k', elinewidth=0.8, capsize=2, capthick=1)

                pylab.errorbar(freqs[ch], channel_YY_mean_pcal_amp[ch], yerr=channel_YY_stddev[ch], fmt='ks', markersize=3,
                               ecolor='k', elinewidth=0.8, capsize=2, capthick=1)

                if len(channel_XX_pc_amps[jj])>0 and len(channel_YY_pc_amps[jj])>0:
                    xtick_locs.append(freqs[ch])
                    xtick_labels.append(ch)
                    band_freqs.append(freqs[ch]) # keep track of the frequencies, for setting the plot range
                    bidx.append(jj) # keep track of the indices, for the violin plots
            else:
                print('Channel '+ch+' is missing!')

        if len(band_freqs)>0 and len(bidx)>0:
            xmin = min(band_freqs)-0.03
            xmax = max(band_freqs)+0.03
            pylab.xlim(xmin,xmax)

            # add the violin plots
            partsX = ax.violinplot([channel_XX_pc_amps[i] for i in bidx], [channel_f[i] for i in bidx], points=100, widths=0.04,
                                  showmeans=False, showextrema=False, showmedians=False)
            partsY = ax.violinplot([channel_YY_pc_amps[i] for i in bidx], [channel_f[i] for i in bidx], points=100, widths=0.04,
                                  showmeans=False, showextrema=False, showmedians=False)
            for pc in partsX['bodies']:
                pc.set_facecolor('orangered')
                pc.set_alpha(0.5)
            for pc in partsY['bodies']:
                pc.set_facecolor('teal')
                pc.set_alpha(0.5)

            if ii==0:
                labels = []
                color = partsX["bodies"][0].get_facecolor().flatten()
                labels.append((mpatches.Patch(color=color), 'XX'))
                color = partsY["bodies"][0].get_facecolor().flatten()
                labels.append((mpatches.Patch(color=color), 'YY'))
                pylab.legend(*zip(*labels), loc='lower left',fontsize=8)

                
        else:
            xmin = 1.0
            xmax = 2.0

                
        pylab.grid(True, which='both', linestyle=':', alpha=0.6)
        pylab.xticks(fontsize=7)

        #pylab.ylim(0,150.0)
        pylab.ylim(0.1,250.0)
        pylab.yscale('log')
        if ii==0:
            pylab.yticks(fontsize=7)
            pylab.ylabel('amplitude [arb]', fontsize=8)
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
