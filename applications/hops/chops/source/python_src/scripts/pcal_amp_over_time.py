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
#import matplotlib.patches as mpatches
import matplotlib.colors as mcolors
import matplotlib.cm as cmx
import matplotlib.colorbar as mcbar

#HOPS module imports
import vpal.processing
import vpal.utility
import vpal.fringe_file_manipulation

import mk4b
import hopstestb as ht

################################################################################

def main():
    # usage_text = '\n pcal_amp_over_time.py [options] <control-file> <ref_station> <stations> <pol-product> <experiment-directory>' \
    #              '\n e.g.: pcal_amp_over_time.py ./cf_3857_HAEIKMNOSTVW_pstokes E AHM ./ -e vr2404'
    # parser = optparse.OptionParser(usage=usage_text)

    parser = argparse.ArgumentParser(
        prog='pcal_amp_over_time.py', \
        description='''utility for plotting per-scan pcal amplitude for each channel''' \
        )

    parser.add_argument('control_file', help='the control file to be applied to all scans')
    parser.add_argument('ref_station', help='single character code of target station')
    parser.add_argument('stations', help='concatenated string of single character codes for remote stations to use')
    parser.add_argument('pol_product', help='the polarization-product to be fringe fit, eg XX or YY')
    parser.add_argument('experiment_directory', help='relative path to directory containing experiment data')

    parser.add_argument('-c', '--channels', dest='channels', help='specify the channels to be used, default=abcdefghijklmnopqrstuvwxyzABCDEF.', default='abcdefghijklmnopqrstuvwxyzABCDEF')
    #parser.add_argument('-s', '--snr-min', type=float, dest='snr_min', help='set minimum allowed snr threshold, default=15.', default=15.)
    #parser.add_argument('-q', '--quality-limit', type=int, dest='quality_lower_limit', help='set the lower limit on fringe quality (inclusive), default=6', default=6)
    parser.add_argument('-p', '--plot_param', type=str, dest='plot_param', help='parameter for datapoint color, "elev" or "az" (default is elev)', default='elev')
    parser.add_argument('-e', '--exp_name', type=str, dest='exp_name', help='experiment name for plot label', default=None)


    args = parser.parse_args()

    #print('args: ', args)
    
    control_file = args.control_file
    ref_station = args.ref_station
    stations = args.stations
    polprod = args.pol_product
    exp_dir = args.experiment_directory
    plot_param = args.plot_param
    if plot_param=='elev':
        param_label = 'elevation'
    elif plot_param=='az':
        param_label = 'azimuth'
    else:
        print("Plot parameter not supported, must be 'elev' or 'az'.")
        sys.exit(1)
    
    abs_exp_dir = os.path.abspath(exp_dir)
    exp_name = os.path.split(os.path.abspath(exp_dir))[1]

    if args.exp_name is not None:
        exp_name = args.exp_name
    else:
        # get a label from the 4-digit folder name
        exp_name = os.path.split(os.path.abspath(exp_dir))[1]        

    if not os.path.isfile(os.path.abspath(control_file)):
        print("could not find control file: ", control_file)
        sys.exit(1)

    #pol product:
    if polprod not in ['XX', 'YY']:
        print("polarization product must be XX or YY")
        sys.exit(1)

    
    #determine available baselines
    print('Calculating baselines')

    baseline_list = vpal.processing.construct_valid_baseline_list(abs_exp_dir, ref_station, stations, network_reference_baselines_only=True)
    print('Baselines:', baseline_list)
    
    #needed for plot-naming
    #control_file_stripped = re.sub('[/\.]', '', control_file)

    #default output filename
    plot_name = "./pcal_amplitude_timeseries_" + ref_station + "_" + stations + '_' + polprod + '_' + exp_name

    # initialize a dictionary to hold lists of channel phase residuals; this stores the nominal 32 VGOS channels
    #channel_amp = dict()
    #for ch in args.channels:
    #    channel_amp[ch] = list()

    freqs = dict()
    amplitudes = dict()
    scans = list()
    scan_names = list()
    param = list()
    for ch in args.channels:
        freqs[ch] = -1.0
        amplitudes[ch] = list()
    
    for bline in baseline_list:

        print('Collecting fringe files for baseline',bline)

        # baselines may have different channel lists (eg a station may be missing channels),
        # but the frequencies should all be the same
        channel_freqs = list()


        ff_list = vpal.processing.gather_fringe_files(exp_dir, control_file, [bline], pol_products=polprod, max_depth=2)
        
        print("n fringe files  =", str(len(ff_list)))
        
        #apply cuts
        #ff_list = []

        #for ff in ff_list_pre:
        #    if ff.snr >= args.snr_min and ff.quality >= args.quality_lower_limit:
        #        ff_list.append(ff)
        
        if len(ff_list) == 0:
            print("Error: no fringe files available, skipping baseline: ", bline)

        else:

            ref_flag=True
            if bline[0]==args.ref_station:
                print('Collecting data from ',bline,'reference station')
            elif bline[1]==args.ref_station:
                ref_flag=False
                print('Collecting data from ',bline,'remote station')

            
            #invert, unwrap, remove mean phase, and clamp to [-180, 180)
            for ff in ff_list:

                # make sure the number of channels is consistent for each scan in this baseline
                # if this is the first scan for this baseline (channel_freqs hasn't been initialized), we'll assign it to this set of channels
                chfreqs = ff.get_channel_frequency_tuples()
                if len(channel_freqs)>0:
                    if len(chfreqs) != len(channel_freqs):
                        print('Error: scan '+ff.scan_id+' has a different channel setup! Try a different baseline.')
                        sys.exit()
                else:
                    channel_freqs = chfreqs

                
                mf = mk4b.mk4fringe(ff.filename)

                ff_pp_list = ht.get_file_polarization_product_provisional(ff.filename)

                if ff_pp_list[0]==polprod:

                    # convert the scan name into a float for plotting
                    scan_name = ff.scan_name
                    scan_day, scan_time = scan_name.split('-')
                    scan_hour = scan_time[0:2]
                    scan_minute = scan_time[2:4]
                    stime = int(scan_day) + int(scan_hour)/24. + int(scan_minute)/1440.
                        
                    scans.append(stime)
                    scan_names.append(ff.scan_name)

                    if ref_flag:
                        param.append(ff.__dict__['ref_'+plot_param])
                    else:
                        param.append(ff.__dict__['rem_'+plot_param])
                        
                    for ii in range(len(channel_freqs)):
                        
                        # choose reference or remote station pcals
                        # we assume USB...
                        if ref_flag:
                            amplitudes[channel_freqs[ii][0]].append(mf.t207.contents.ref_pcamp[ii].usb*1000)
                        else:
                            amplitudes[channel_freqs[ii][0]].append(mf.t207.contents.rem_pcamp[ii].usb*1000)

                else:
                    # This fringe file does not have the requested polarization
                    continue
                        

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

    #sys.exit()
                
        
    print("Number of scans:", len(amplitudes[args.channels[0]]))
    
    channel_mean_amp = dict()
    channel_stddev = dict()
    for ch in args.channels:
        if freqs[ch]>0:
            channel_mean_amp[ch] = np.mean(amplitudes[ch])
            channel_stddev[ch] = np.std(amplitudes[ch])
            #print( "(mean, std. dev) phasecal amplitude for channel: ", ch, " = ", channel_mean_amp[ch], channel_stddev[ch])
            print( "(mean, std. dev) phasecal amplitude for channel: ", ch, " =", '{:5.2f}'.format(channel_mean_amp[ch]).rjust(5), '{:5.2f}'.format(channel_stddev[ch]).ljust(5))
        else:
            print( "No data for channel",ch)
    
    # build a list of channel names for plot labels
    # I think we can assume the channel labels are in order of frequency
    #channel_f = []
    channel_names = []
    for ch in args.channels:
        if freqs[ch]>0:
            channel_names.append(ch)
            #channel_f.append(freqs[ch])
        #else:
        #    channel_f.append(0.0)
    

    
    scan_idx = np.argsort(scans)
    #for ii in scan_idx:
    #    print(scan_names[ii], scans[ii], amplitudes['l'][ii])
            
    #sys.exit()

            
    fig_width_pt = 600  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    #golden_mean = (2.236-1.0)/2.0         # Aesthetic ratio
    golden_mean = 1.5
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width*golden_mean      # height in inches
    fig_size = [fig_width,fig_height]
                                                                        

    matplotlib.rcParams.update({'savefig.dpi':350,
                                'figure.figsize':fig_size}),
    #                            'text.usetex':True,
    #                            'font.family':"serif",
    #                            'font.serif':["Times"]})
    
    
    fig = pylab.figure(np.random.randint(0,1000))
    
    gs1 = gridspec.GridSpec(16,2)
    gs1.update(wspace=0.15, hspace=0.1)
    
    cmap=cmx.get_cmap('viridis')
    normalize = mcolors.Normalize(vmin=min(param), vmax=max(param))

    ax0 = plt.subplot(111)
    
    ax0.spines['top'].set_color('none')
    ax0.spines['bottom'].set_color('none')
    ax0.spines['left'].set_color('none')
    ax0.spines['right'].set_color('none')
    plt.tick_params(labelcolor='none', which='both', top=False, bottom=False, left=False, right=False)

    
    num_chans = len(channel_names)
    for ii in list(range(len(channel_names))):

        ch = channel_names[ii]
        
        ax = fig.add_subplot(gs1[ii])

        #for s in range(len(np.asarray(scans)[scan_idx])):
        #    print(ch, np.asarray(scans)[scan_idx][s], np.asarray(amplitudes[ch])[scan_idx][s])
        
        plt.scatter(np.asarray(scans)[scan_idx], np.asarray(amplitudes[ch])[scan_idx],s=2,c=np.asarray(param)[scan_idx],cmap=cmap)

        pylab.ylim(0,1.2*np.max(np.asarray(amplitudes[ch])[scan_idx]))        
        pylab.grid(True, which='both', linestyle=':', alpha=0.6)

        if ii==num_chans-2 or ii==num_chans-1:
            pylab.xticks(fontsize=9)
            pylab.xlabel('Time, fractional day of year',fontsize=9)
        else:
            ax.tick_params(bottom=False)
            pylab.xticks(visible=False)
            
        ax.set_ylabel(ch, rotation=0, fontsize=9)#, labelpad=20)
        pylab.yticks(fontsize=7, visible=True)
    
    
    
    cax, _ = mcbar.make_axes(ax0, location='right', anchor=(1.5, 0.2), aspect=50)
    cbar = mcbar.ColorbarBase(cax, cmap=cmap, norm=normalize, orientation='vertical', label=param_label+' [deg]')
    #plt.tick_params(labelcolor='none', which='both', top=False, bottom=False, left=False, right=False)
    #cbar.ax0.set_xlabel('elevation')
    #cax.set_xlabel('elevation')
    
    
    pylab.suptitle('Phasecal amplitude for station '+ ref_station + ", polarization " + polprod + ' in session ' + exp_name, fontsize=14, x=0.5, y=0.9, fontweight='bold')
    pylab.savefig(plot_name + '.png', bbox_inches='tight')
    pylab.close()




if __name__ == '__main__':          # official entry point
    main()
    sys.exit(0)
