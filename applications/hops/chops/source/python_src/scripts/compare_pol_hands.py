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

#non-core imports
#set the plotting back-end to 'agg' to avoid display
import numpy as np

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

import mk4b


################################################################################

def main():
    # usage_text = '\n compare_pol_hands.py [options] <control-file> <stations> <experiment-directory>' \
    #              '\n e.g.: compare_pol_hands.py ./cf_GHEVY_ff GHEV ./'
    # parser = optparse.OptionParser(usage=usage_text)

    parser = argparse.ArgumentParser(
        prog='compare_pol_hands.py', \
        description='''utility for plotting ratio of XX/YY pol products across a VLBI network''' \
        )

    parser.add_argument('control_file', help='the control file to be applied to all scans')
    parser.add_argument('stations', help='concatenated string of single character codes for all stations to be fringe fit')
    #parser.add_argument('pol_product', help='the polarization-product to be fringe fit')
    parser.add_argument('experiment_directory', help='relative path to directory containing experiment data')

    parser.add_argument('-n', '--numproc', type=int, dest='num_proc', help='number of concurrent fourfit jobs to run, default=1', default=16)
    parser.add_argument('-c', '--channels', dest='channels', help='specify the channels to be used, default=abcdefghijklmnopqrstuvwxyzABCDEF.', default='abcdefghijklmnopqrstuvwxyzABCDEF')
    parser.add_argument('-s', '--snr-min', type=float, dest='snr_min', help='set minimum allowed snr threshold, default=30.', default=30)
    parser.add_argument('-d', '--dtec-threshold', type=float, dest='dtec_thresh', help='set maximum allowed difference in dTEC, default=1.', default=1.0)
    parser.add_argument('-q', '--quality-limit', type=int, dest='quality_lower_limit', help='set the lower limit on fringe quality (inclusive), default=3.', default=3)
    parser.add_argument('-p', '--progress', action='store_true', dest='use_progress_ticker', help='monitor process with progress indicator', default=True)
    parser.add_argument('-b', '--begin-scan', dest='begin_scan_limit', help='limit the earliest scan to be used e.g 244-1719', default="000-0000")
    parser.add_argument('-e', '--end-scan', dest='end_scan_limit', help='limit the latest scan to be used, e.g. 244-2345', default="999-9999")
    #parser.add_argument('-r', '--remove-outliers', dest='remove_outlier_nsigma', help='remove scans which are n*sigma away from the mean, default=0 (off)', default=0.0 )
    #parser.add_argument('-z', '--z-axis', dest='z_axis', help='select color axis, options are: amp, dtec, or none, default=none', default='none' )
    parser.add_argument('-L', '--label', dest='exper_label', help='experiment label, eg vr2201, default=none', default='none' )

    args = parser.parse_args()

    #print('args: ', args)

    control_file = args.control_file
    stations = args.stations
    polprod = ['XX','YY']
    exp_dir = args.experiment_directory

    abs_exp_dir = os.path.abspath(exp_dir)
    exp_name = os.path.split(os.path.abspath(exp_dir))[1]
    #rnsigma = float(args.remove_outlier_nsigma)

    exper_label = args.exper_label
    
    print('Calculating baselines')
    
    #determine all possible baselines
    blist = vpal.processing.construct_valid_baseline_list(abs_exp_dir, stations[0], stations[1:], network_reference_baselines_only=False)
    
    qcode_list = []
    for q in list(range(args.quality_lower_limit, 10)):
        qcode_list.append( str(q) )

    #needed for plot-naming
    control_file_stripped = re.sub('[/\.]', '', control_file)

    # get number of stations and baselines
    all_stations = []
    all_stations[:0] = ''.join(blist)
    unique_stations = np.unique(all_stations)
    print(unique_stations)

    # rebuild an ordered baseline list

    baseline_list = []
    for ii in range(len(unique_stations)):
        for jj in range(ii+1,len(unique_stations)):
            if unique_stations[ii]==unique_stations[jj]:
                continue
            baseline_list.append(unique_stations[ii]+unique_stations[jj])

    if 'ST' in baseline_list:
        baseline_list.remove('ST')        
    print(baseline_list)
    
    #sys.exit()

    # list to store arrays of polprod amps
    baseline_polprods = []
    
    # bline is a two-element string of [reference_station, other_station]
    for bline in baseline_list:

        print('Collecting fringe files for', bline)

        if not os.path.isfile(os.path.abspath(control_file)):
            print("could not find control file: ", control_file)
            sys.exit(1)

        #need to:
        #(1) collect the amplitudes for each pol-product from each scan
        #(2) apply the snr, and quality code cuts
        #(3) build array for each pol-product of parallactic angle and (normalized) amplitude
        #(5) create plot with pol-products for each possible baseline

        ################################################################################
        #collect/compute fringe files, and apply cuts
        set_commands = "set gen_cf_record true"
        ff_list = vpal.processing.load_and_batch_fourfit( \
            os.path.abspath(exp_dir), bline[0], bline[1], os.path.abspath(control_file), set_commands, \
            num_processes=args.num_proc, start_scan_limit=args.begin_scan_limit, \
            stop_scan_limit=args.end_scan_limit, pol_products=polprod, use_progress_ticker=args.use_progress_ticker \
        )

        print("n fringe files  =", str(len(ff_list)))

        
        #apply cuts
        filter_list = []
        filter_list.append( vpal.utility.DiscreteQuantityFilter("quality", qcode_list) )
        filter_list.append( vpal.utility.ContinuousQuantityFilter("snr", args.snr_min, 500) )
        vpal.utility.combined_filter(ff_list, filter_list)

        if len(ff_list) == 0:
            print("Error: no fringe files available after cuts, skipping baseline: ", bline)

        else:

            # how many unique, complete scans?
            scans = []
            #polarization = []

            if len(ff_list) % 2 !=0:
                print("Not an even number of fringe files!", len(ff_list))
                sys.exit()

                
            # get the number of unique scans for this baseline
            for ff in ff_list:
                scans.append(ff.scan_id)
            unique_scans = np.unique(scans)

            print("Found "+str(len(unique_scans))+" unique scans")
            
            # initialize and array to store the polprod amps for each scan on this baseline
            polprod_amps = -1*np.ones((len(unique_scans),2))

            # go through the fringe files for this baseline, store the amplitude in the array
            for ff in ff_list:
                if ff.polarization not in polprod:
                    continue
                #scans.append(ff.scan_id)
                polprod_amps[np.where(unique_scans == ff.scan_id),polprod.index(ff.polarization)] = ff.amp
                

            # check that each scan has both polprod_amps recorded
            for ii in range(len(unique_scans)):

                if any(polprod_amps[ii,:]==-1):
                    print('This scan has missing data!', unique_scans[ii], polproduct_amps[ii,:])
                    

            # append the array of polprod amps to the list of baseline results
            baseline_polprods.append(polprod_amps)


    #for ii in range(len(baseline_list)):
    #    print(baseline_list[ii], baseline_polprods[ii])

    
    fig_width_pt = 600  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    #golden_mean = (2.236-1.0)/2.0         # Aesthetic ratio
    golden_mean = 1.0
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width*golden_mean      # height in inches
    fig_size = [fig_width,fig_height]
    

    matplotlib.rcParams.update({'savefig.dpi':350,
                                'text.usetex':True,
                                'figure.figsize':fig_size,
                                'font.family':"serif",
                                'font.serif':["Times"]})

    fig = pylab.figure(np.random.randint(0,1000))
    gs1 = gridspec.GridSpec(len(unique_stations),len(unique_stations))
    gs1.update(wspace=0.25, hspace=0.25)


    for ii in range(len(unique_stations)):

        new_row = True
        
        for jj in range(ii,len(unique_stations)):

            #print(unique_stations[ii], unique_stations[jj], ii, jj, ii*len(unique_stations) + jj)
            
            if unique_stations[ii]==unique_stations[jj]:
                continue

            if unique_stations[jj]+unique_stations[ii] in baseline_list:
                bb = unique_stations[jj]+unique_stations[ii]

            elif unique_stations[ii]+unique_stations[jj] in baseline_list:
                bb = unique_stations[ii]+unique_stations[jj]
            else:
                continue
                
            bline_idx = baseline_list.index(bb)
            #print(baseline_list[bline_idx], ii*len(unique_stations) + jj)
            
            ax = plt.subplot(gs1[ ii*len(unique_stations) + jj ])
            
            pylab.plot(baseline_polprods[bline_idx][:,0], baseline_polprods[bline_idx][:,1], 'ko', markersize=1)
            x = np.arange(0,np.max(baseline_polprods[bline_idx].flatten()))
            pylab.plot(x,x,'k--',linewidth=0.6)
            pylab.grid(True, which='both', linestyle=':', alpha=0.6)
            pylab.text(0.1,0.8,baseline_list[bline_idx],transform=ax.transAxes,fontsize=10)
            
            pylab.xticks(fontsize=7)
            pylab.yticks(fontsize=7)
            
            if new_row:
                pylab.ylabel(polprod[1]+' amp', fontsize=8)
                new_row=False

                pylab.xlabel(polprod[0]+' amp',fontsize=8)
                
                    
            #if (ii+jj) % len(unique_stations)-1 == 0:
            #    pylab.ylabel(polprods[1], fontsize=10)

            #if ii==jj:
            #    pylab.xlabel(polprods[0], fontsize=10)

            #pylab.ylabel('normalized amplitude', fontsize=10)
            #pylab.title(polprod[0],fontsize=10)
            #pylab.ylim(-0.05, 1.05)

            """
            pylab.plot(scan_dPar, polprod_amps_norm[:,3], 'ks', markersize=3)
            pylab.grid(True, which='both', linestyle=':', alpha=0.6)

            pylab.xlabel('dPar [deg]')
            pylab.ylabel('normalized amplitude', fontsize=10)
            pylab.title(polprod[3],fontsize=10)
            pylab.ylim(-0.05, 1.05)
            """

    pylab.suptitle('Comparison of amplitudes for parallel-hands', fontsize=14, x=0.6, y=0.93, fontweight='bold')
    pylab.savefig('polprod_amp_compare_' + ff.exp_name + '_' + ''.join(unique_stations) + '.png', bbox_inches='tight')
    pylab.close()
    
            
    """

            fig = pylab.figure(np.random.randint(0,1000))
            #gs1 = gridspec.GridSpec(2,2)
            #gs1.update(wspace=0.25, hspace=0.0)
            
            pylab.plot(scan_dPar, polprod_amps_norm[:,0], 'rs', markersize=2,label=polprod[0])
            pylab.plot(scan_dPar, polprod_amps_norm[:,1], 'bs', markersize=2,label=polprod[1])
            pylab.plot(scan_dPar, polprod_amps_norm[:,2], 'yo', markersize=2,label=polprod[2])
            pylab.plot(scan_dPar, polprod_amps_norm[:,3], 'ko', markersize=2,label=polprod[3])

            x_min = np.min(scan_dPar)
            x_max = np.max(scan_dPar)
            x = np.arange(x_min-15,x_max+15,1)
            y_cos = np.abs(np.cos(x*np.pi/180.))
            y_sin = np.abs(np.sin(x*np.pi/180.))
            pylab.plot(x,y_cos,'k--',linewidth=0.8,label=r'$|\cos(\Delta PA)|$')
            pylab.plot(x,y_sin,'r--',linewidth=0.8,label=r'$|\sin(\Delta PA)|$')

            pylab.grid(True, which='both', linestyle=':', alpha=0.6)
            #pylab.xticks(visible=False)
            pylab.xlabel(r'parallactic angle difference [deg], PA$_'+bline[1]+'$-PA$_'+bline[0]+'$')
            pylab.ylabel('normalized scan amplitude', fontsize=10)
            if exper_label is not None:
                pylab.title('Test for polarization swap, experiment '+ff.exp_name+' ('+exper_label+'), baseline '+bline+', '+str(len(scan_dPar))+' scans',fontsize=12)
            else:
                pylab.title('Test for polarization swap, experiment '+ff.exp_name+', baseline '+bline+', '+str(len(scan_dPar))+' scans',fontsize=12)
                
            pylab.ylim(-0.01, 1.01)
            pylab.xlim(x_min-15,x_max+15)

            #pylab.legend(loc='upper left',fancybox=True,prop={'size':9})
            pylab.legend(bbox_to_anchor=(0.43,0.35),fancybox=True,prop={'size':11},framealpha=0.8)
            
            pylab.savefig(ff.exp_name + '_' + bline + '_compare.png', bbox_inches='tight')
            pylab.close()

    """


if __name__ == '__main__':          # official entry point
    main()
    sys.exit(0)
