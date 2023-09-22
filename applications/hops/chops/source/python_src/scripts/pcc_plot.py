#!/usr/bin/python
#
# Script to take proxy-cable cal .dat files and plot the delay as a function of time
# 08/08/18 first version jpb

#core imports
from __future__ import print_function
from __future__ import division
from builtins import str
from builtins import zip
from past.utils import old_div
import datetime
import argparse
import re
import string
import sys
import os

#non-core imports
#set the plotting back-end to 'agg' to avoid display
import matplotlib as mpl
mpl.use('Agg')
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
import matplotlib.gridspec as gridspec
import matplotlib.mlab as mlab
import matplotlib.ticker as ticker

#HOPS module imports
import vpal

def plot_station_delay_data(station, station_band_data_objects, sigma_factor=0.0, use_median=False, cut_threshold=0.0, uniform=False, outfile_name="./delay_trend.png"):

    #for each data object, we want to look over the data-lines, and extract (time, delay) tuples
    #then we correct the time to be relative to the start time of the experiment
    #then we compute the mean,sigma of the delay trend and strip outliers beyond the cut factor (TODO: figure out what to do when we have a step-like jump)
    #then plot the delay vs time for all bands/pols, and add border markers for the scans we cut

    band_pol_data = dict()
    all_bands = set()
    all_pols = set()
    exp_start_time = vpal.proxy_cable_cal.PccDate()
    exp_name = ""
    for obj in station_band_data_objects:
        band = obj.band_name
        pol = obj.pol_name
        exp_name = obj.experiment_name
        all_bands.add(band)
        all_pols.add(pol)
        band_pol = band + ":" + pol
        #get the time of the first scan and refererence all scan times w.r.t midnight 00:00:00 the day of the first (reference scan)
        start_scan = obj.scan_pcc_line_list[0]
        exp_start_time.year = start_scan.year
        exp_start_time.day = start_scan.doy
        exp_start_time.hour = 0
        exp_start_time.minute = 0
        exp_start_time.second = 0
        time_delay_tup_list = list()
        for pcc_line in obj.scan_pcc_line_list:
            scan_start_time = vpal.proxy_cable_cal.PccDate()
            scan_start_time.year = pcc_line.year
            scan_start_time.day = pcc_line.doy
            scan_start_time.hour =  pcc_line.hour
            scan_start_time.minute = pcc_line.minute
            scan_start_time.second = pcc_line.second
            scn_reltime = scan_start_time.get_time_delta_seconds( exp_start_time )
            scn_reltime /= 3600 #convert to hours
            delay_ps = pcc_line.delay_model_ps
            td = (scn_reltime, delay_ps)
            time_delay_tup_list.append(td)
        band_pol_data[band_pol] = time_delay_tup_list

    all_bands = ''.join(sorted(all_bands))
    all_pols = ''.join(sorted(all_pols))

    #figure out delay trend mean (for each band-pol), and perform cuts if necessary
    band_pol_kept_data = dict()
    band_pol_cut_data = dict()
    for band_pol, data in list(band_pol_data.items()):
        if sigma_factor == 0.0 and cut_threshold == 0.0:
            #keep all data points, no cuts
            band_pol_cut_data[band_pol] = list()
            band_pol_kept_data[band_pol] = band_pol_data[band_pol]
        else:
            #some combination of cuts depending on sigma_factor/threshold
            delay_values = [ x[1] for x in data]
            if use_median == True:
                delay_estimator = np.median(delay_values)
            else:
                delay_estimator = np.mean(delay_values)
            sigma = np.std(delay_values)
            cut_data = list()
            kept_data = list()
            for pair in data:
                keep = False
                delta = abs(pair[1] - delay_estimator)
                if sigma_factor != 0.0 and cut_threshold != 0.0:
                    if delta <  abs( sigma_factor*sigma) and delta < abs(cut_threshold):
                        keep = True
                elif sigma_factor == 0.0 and cut_threshold != 0.0:
                    if delta < abs(cut_threshold):
                        keep = True
                elif sigma_factor != 0.0 and cut_threshold == 0.0:
                    if delta <  abs( sigma_factor*sigma):
                        keep = True
                if keep == True:
                    kept_data.append(pair)
                else:
                    cut_data.append(pair)

            band_pol_cut_data[band_pol] = cut_data
            band_pol_kept_data[band_pol] = kept_data

    #now create a plot for each band/pol we have data for (on a 2X4 grid)
    auto_fig = plt.figure(figsize=(8.5,11))
    auto_fig.suptitle("Exp. " + exp_name + ", station " + station + ": delay trend for bands (" + all_bands + ") and pols (" + all_pols + ")"  )
    pdict = {'X':0, 'Y':1}
    bdict = {'A':0,'B':1,'C':2,'D':3}

    xmin_global = 1e30
    xmax_global = -1e30
    ymin_global = 1e30
    ymax_global = -1e30

    if uniform:
        for band_pol in list(band_pol_data.keys()):
            kept_data = band_pol_kept_data[band_pol]
            time_arr = [val[0] for val in kept_data] #time
            delay_arr = [val[1] for val in kept_data] #delay (ps)
            xmin = min(time_arr)
            xmax = max(time_arr)
            ymin = min(delay_arr)
            ymax = max(delay_arr)
            if xmin < xmin_global:
                xmin_global = xmin
            if xmax > xmax_global:
                xmax_global = xmax
            if ymin < ymin_global:
                ymin_global = ymin
            if ymax > ymax_global:
                ymax_global = ymax

    for band_pol in list(band_pol_data.keys()):
        band = (band_pol.split(":") )[0]
        pol = (band_pol.split(":") )[1]
        pol_num = pdict[pol]
        band_num = bdict[band]
        plot_num = 2*(band_num) + pol_num + 1 #index starts from 1

        kept_data = band_pol_kept_data[band_pol]
        time_arr = [val[0] for val in kept_data] #time
        delay_arr = [val[1] for val in kept_data] #delay (ps)

        ax = plt.subplot(4, 2, plot_num)
        plt.plot(time_arr, delay_arr, 'bx', markersize=3)
        if uniform:
            ax.set_xlim(xmin_global, xmax_global)
            ax.set_ylim(ymin_global, ymax_global)
        plt.ylabel(band_pol + " delay (ps)")
        plt.grid(True)

        ymin, ymax = ax.get_ylim()
        #loop over the cut data and replace the delay values with the y-axis limits
        cut_data = band_pol_cut_data[band_pol]
        if len(cut_data) != 0:
            upper_cut_time_arr = []
            upper_cut_delay_arr = []
            lower_cut_time_arr = []
            lower_cut_delay_arr = []
            for val in cut_data:
                if val[1] >= ymax:
                    upper_cut_time_arr.append(val[0])
                    upper_cut_delay_arr.append(ymax)
                else:
                    lower_cut_time_arr.append(val[0])
                    lower_cut_delay_arr.append(ymin)
            plt.plot(upper_cut_time_arr, upper_cut_delay_arr, 'r^', markersize=3, clip_on=False)
            plt.plot(lower_cut_time_arr, lower_cut_delay_arr, 'rv', markersize=3, clip_on=False)

        if plot_num == 7 or plot_num == 8: #bottom two plots
            plt.xlabel("hours since: " + exp_start_time.as_string() )

    plt.tight_layout()
    auto_fig.subplots_adjust(top=0.94)
    auto_fig.savefig(outfile_name)
    plt.close(auto_fig)

################################################################################


def plot_station_ave_delay_data(station, bands, pols, station_ave_calc, sigma_factor=0.0, use_median=False, cut_threshold=0.0, outfile_name="./ave_delay_trend.png"):

    line_keys = ['year', 'month', 'day', 'hour', 'minute', 'second', 'mean_delay', 'source_name', 'scan_name']

    exp_name = station_ave_calc.experiment_name
    station_id = station_ave_calc.station_code

    #extract the starting scan and experiment start time
    start_scan =  { key:value for (key, value) in list(zip(line_keys, station_ave_calc.file_lines[0].split())) }
    exp_start_time = datetime.datetime(year=int(start_scan['year']), month=int(start_scan['month']), day=int(start_scan['day']), hour=0, minute=0, second=0)

    data = list()
    for fline in station_ave_calc.file_lines:
        scan_data = { key:value for (key, value) in list(zip(line_keys, fline.split()))}
        scan_time = datetime.datetime(year=int(scan_data['year']), month=int(scan_data['month']), \
            day=int(scan_data['day']), hour=int(scan_data['hour']), minute=int(scan_data['minute']), second=int(scan_data['second']) )
        scn_reltime = (scan_time - exp_start_time).total_seconds()
        scn_reltime /= 3600 #convert to hours
        delay_ps = old_div(float( scan_data['mean_delay'] ),1e-12) #conver to ps
        td = (scn_reltime, delay_ps)
        data.append(td)

    #figure out delay trend mean (for each band-pol), and perform cuts if necessary
    kept_data = []
    cut_data = []

    if sigma_factor == 0.0 and cut_threshold == 0.0:
        #keep all data points, no cuts
        cut_data = list()
        kept_data = data
    else:
        #some combination of cuts depending on sigma_factor/threshold
        delay_values = [ x[1] for x in data]
        if use_median == True:
            delay_estimator = np.median(delay_values)
        else:
            delay_estimator = np.mean(delay_values)
        sigma = np.std(delay_values)
        for pair in data:
            keep = False
            delta = abs(pair[1] - delay_estimator)
            if sigma_factor != 0.0 and cut_threshold != 0.0:
                if delta <  abs( sigma_factor*sigma) and delta < abs(cut_threshold):
                    keep = True
            elif sigma_factor == 0.0 and cut_threshold != 0.0:
                if delta < abs(cut_threshold):
                    keep = True
            elif sigma_factor != 0.0 and cut_threshold == 0.0:
                if delta <  abs( sigma_factor*sigma):
                    keep = True
            if keep == True:
                kept_data.append(pair)
            else:
                cut_data.append(pair)

    #now create a plot for each band/pol we have data for (on a 2X4 grid)
    auto_fig = plt.figure(figsize=(8.5,11))
    auto_fig.suptitle("Exp. " + exp_name + ", station " + station + ": average delay trend for bands (" + bands + ") and pols (" + pols + ")"  )

    time_arr = [val[0] for val in kept_data] #time
    delay_arr = [val[1] for val in kept_data] #delay (ps)

    ax = plt.subplot(1, 1, 1)
    plt.plot(time_arr, delay_arr, 'bx', markersize=3)
    plt.ylabel("mean delay (ps)")
    plt.grid(True)
    ymin, ymax = ax.get_ylim()
    #loop over the cut data and replace the delay values with the y-axis limits
    if len(cut_data) != 0:
        cut_time_arr = []
        cut_delay_arr = []
        for val in cut_data:
            cut_time_arr.append(val[0])
            if val[1] >= ymax:
                cut_delay_arr.append(ymax)
            else:
                cut_delay_arr.append(ymin)
        plt.plot(cut_time_arr, cut_delay_arr, 'r*', markersize=3)

    exp_start_time_string = start_scan['year'] + "-" +  start_scan['month'] + "-" + start_scan['day'] + ":00:00:00 UTC"
    plt.xlabel("hours since: " + exp_start_time_string )
    plt.tight_layout()
    auto_fig.subplots_adjust(top=0.94)
    auto_fig.savefig(outfile_name)
    plt.close(auto_fig)


################################################################################


def main():

    if len(sys.argv)==1:
        print("use -h option for help.")
        sys.exit(1)

    parser = argparse.ArgumentParser()
    parser._action_groups.pop()
    required = parser.add_argument_group('required arguments')
    optional = parser.add_argument_group('optional arguments')

    required.add_argument("-e", "--experiment", dest='experiment_name', help="Experiment name, for example: vt7226", required=True) #required argument
    required.add_argument("-d", "--dat-dir", dest='dat_directory', help="Directory containing per-band .dat files", required=True) #required argument
    optional.add_argument("-o", "--output-dir", dest='output_directory', default="DAT_DIRECTORY", help="Specify the plot output directory, default is DAT_DIRECTORY") #optional argument
    optional.add_argument("-c", "--cut-sigma-factor", dest='cut_sigma_factor', type=float, default=0.0, help="remove delay values which are greater than factor*sigma from the delay estimator (mean or median)") #optional argument
    optional.add_argument("-t", "--cut-threshold", dest='cut_threshold', type=float, default=0.0, help="remove delay values which are greater some absolute threshhold (ps) from the delay estimator (mean or median)") #optional argument
    optional.add_argument("-m", "--median", dest='use_median', action='store_true', default=False, help="use median as delay estimator (default is mean)") #optional argument
    optional.add_argument("-u", "--uniform", dest='uniform', action='store_true', default=False, help="force all graphs to share same axis limits") #optional
    optional.add_argument("-a", "--averaging", dest='averaging', action='store_true', default=False, help="also produce a single plot by averaging the delay trends of all bands:pols specified for each station") #optional

    selection_help_string = 'List of selected stations, bands, and polarizations. Stations must be specified with single character code. \n' \
    ' allowable values for bands: A, B, C, D ' \
    ' allowable values for polarizations: X, Y. ' \
    ' For example: G:BCD:XY E:BC:XY V:BC:X Y:BCD:Y '
    required.add_argument("-s", "--select", dest='station_bands_pols', nargs='*', help=selection_help_string, required=True) #require argument with variable number
    args = parser.parse_args()

    #construct default output directory
    default_output_directory = os.path.abspath(args.dat_directory)

    if args.output_directory != 'DAT_DIRECTORY':
        output_directory = os.path.abspath(args.output_directory)
    else:
        output_directory = default_output_directory

    if not os.path.exists(output_directory):
        os.makedirs(output_directory)

    print("experiment_name: {}".format(args.experiment_name))
    print("dat_directory: {}".format(args.dat_directory))
    print("station_bands_pols: {}".format(args.station_bands_pols))

    stations = []
    bands = dict()
    pols = dict()

    #loop over the station/band/pol arguments and determine the list of files we need
    for x in args.station_bands_pols:
        sbp_list = x.split(':')
        if len(sbp_list) != 3:
            print("Error: could not parse station:bands:polarizations from <", x, ">")
            sys.exit(1)
        elif len(sbp_list[0]) != 1:
            print("Error: could not parse station:bands:polarizations from <", x, ">")
            sys.exit(1)
        elif not all( b in 'ABCDabcd' for b in sbp_list[1]) or len(sbp_list[1]) == 0:
            print("Error: could not parse station:bands:polarizations from <", x, ">")
            sys.exit(1)
        elif not all( p in 'XYxy' for p in sbp_list[2]) or len(sbp_list[2]) == 0:
            print("Error: could not parse station:bands:polarizations from <", x, ">")
            sys.exit(1)
        else:
            #format and add to list
            st = (sbp_list[0].upper()).strip()
            bd = ''.join(sorted(set((sbp_list[1].upper()).strip())))
            pl = ''.join(sorted(set((sbp_list[2].upper()).strip())))
            stations.append(st)
            bands[st] = bd
            pols[st] = pl

    station_files = dict()
    for s in stations:
        bd = bands[s]
        pl = pols[s]
        print("Preparing to create delay trend plot for station: ", s, " using bands: ", str(bd), " and polarizations: ", str(pl))
        #now generate the names of the files we need to compute the band delay corrections (e.g bandmodel.G.C.X.dat)
        files_needed = []
        for b in bd:
            for p in pl:
                fname = 'bandmodel.' + args.experiment_name + '.' + s + '.' + b + '.' + p + '.dat'
                full_fname = os.path.join( os.path.abspath(args.dat_directory), fname )
                if os.path.exists(full_fname): #check that the file exists
                    files_needed.append(full_fname)
                else:
                    #now try looking for the same file with the experiment name capitalized
                    fname = 'bandmodel.' + args.experiment_name.upper() + '.' + s + '.' + b + '.' + p + '.dat'
                    full_fname_upper = os.path.join( os.path.abspath(args.dat_directory), fname )
                    if os.path.exists(full_fname_upper): #check that the file exists
                        files_needed.append(full_fname_upper)
                    else:
                        print("Error, could not find a data file matching either: ", full_fname, " or ", full_fname_upper, ".")
                        sys.exit(1)
        if len(files_needed) != 0:
            station_files[s] = files_needed

    #now loop over each station and read the files to collect the data
    station_band_data = dict()
    station_ave_calc = dict()
    for s in stations:
        files_needed = station_files[s]
        file_data_list = []
        station_ave_calc[s] = vpal.proxy_cable_cal.ExperimentMultibandDelayAverager()
        station_ave_calc[s].experiment_name = args.experiment_name
        for f in files_needed:
            #read the file into a experiment_pcc_band_delay object
            file_obj = vpal.proxy_cable_cal.ExperimentPccBandDelay()
            file_obj.read_file(f)
            if len(file_obj.scan_pcc_line_list) >= 1:
                file_data_list.append(file_obj)
                station_ave_calc[s].add_band_data(file_obj)
        station_band_data[s] = file_data_list
        station_ave_calc[s].average_band_delays()

        #now create a plot for each station displaying the delay vs. time trend for all the selected bands/pols
        #for now we don't plot any mean delay, but we do perform cuts on extreme values so they don't blow up the plot
        outfile_basename = "delay_trend_" + args.experiment_name + "_" + s + "_" + bands[s] + "_" + pols[s] + ".png"
        outfile_name= os.path.join( os.path.abspath(output_directory), outfile_basename )
        plot_station_delay_data(s, station_band_data[s], args.cut_sigma_factor, args.use_median, args.cut_threshold, args.uniform, outfile_name )

        if args.averaging is True:
            ave_outfile_basename = "ave_delay_trend_" + args.experiment_name + "_" + s + "_" + bands[s] + "_" + pols[s] + ".png"
            ave_outfile_name= os.path.join( os.path.abspath(output_directory), ave_outfile_basename )
            plot_station_ave_delay_data(s, bands[s], pols[s], station_ave_calc[s], args.cut_sigma_factor, args.use_median, args.cut_threshold, ave_outfile_name )

    print("Done.")

################################################################################

if __name__ == '__main__':          # official entry point
    main()
    sys.exit(0)
