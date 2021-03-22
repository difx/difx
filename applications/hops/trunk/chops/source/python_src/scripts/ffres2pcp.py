#!/usr/bin/python

#core imports
from __future__ import print_function
from builtins import str
import datetime
import argparse
import sys
import os
import logging

#HOPS imports
import hopstestb as ht
import vpal.ffres2pcp_lib

################################################################################

def main():
    parser = argparse.ArgumentParser(
        prog='ffres2pcp.py', \
        description='''utility for determining channel-by-channel pc_phases corrections for VGOS stations''' \
        )

    parser.add_argument('control_file', help='the control file to be applied to all scans')
    parser.add_argument('network_reference_station', help='single character code of station used as network reference')
    parser.add_argument('stations', help='concatenated string of single codes of non-network-reference stations of interest')
    parser.add_argument('data_directory', help='relative path to directory containing experiment or scan data')

    parser.add_argument('-v', '--verbosity', type=int, dest='verbosity', help='verbosity level: 0 (least verbose) to 3 (most verbose), default=2.', default=2)
    parser.add_argument('-n', '--num-proc', type=int, dest='num_proc', help='number of concurrent fourfit jobs to run, default=1', default=1)
    parser.add_argument('-s', '--snr-min', type=float, dest='snr_min', help='set minimum allowed snr threshold, default=30.', default=30)
    parser.add_argument('-q', '--quality-limit', type=int, dest='quality_lower_limit', help='set the lower limit on fringe quality (inclusive), default=6.', default=6)
    parser.add_argument('-d', '--dtec-threshold', type=float, dest='dtec_thresh', help='set maximum allowed difference in dTEC, default=1.0', default=1.0)
    parser.add_argument('-b', '--begin-scan', dest='begin_scan_limit', help='limit the earliest scan to be used in the calibration, e.g. 244-1719', default="000-0000")
    parser.add_argument('-e', '--end-scan', dest='end_scan_limit', help='limit the latest scan to be used in the calibration, e.g. 244-2345', default="999-9999")
    parser.add_argument('-c', '--cut-sigma-factor', dest='sigma_cut_factor', type=float, help='cut phase values which are more than cut_factor*sigma away from the mean value, default=3.0 (use 0.0 to disable).', default=3.0)
    parser.add_argument('-w', '--without-scratch', action='store_false', dest='use_scratch', help='disable use of scratch directory and work directly in experiment directory', default=True)
    parser.add_argument('-p', '--progress', action='store_true', dest='use_progress_ticker', help='monitor process with progress indicator', default=False)
    parser.add_argument('-o', '--output-filename', dest='output_filename', help='specify the name of the generated control file', default='')
    parser.add_argument('-a', '--averaging-scan-limit', type=int, dest='averaging_scan_limit', help='limit the number of scans used in averaging, use 0 to disable, default=10', default=10)
    parser.add_argument('-t', '--toggle-run-info', action='store_false', dest='toggle_dump_info', help='do not append control file with information about how this program was called', default=True)
    parser.add_argument('-x', '--nchannel-discard-threshold', type=int, dest='nchannel_discard_threshold', help='specify number of channels allowed to be flagged before discarding this entire scan', default=5)
    parser.add_argument('-y', '--channel-discard-tolerance', type=float, dest='channel_discard_tolerance', help='allowable channel phase error (deg) before it is flagged', default=15.0)

    args = parser.parse_args()

    #verbosity levels: 0 is silence (except for errors), 1 is warnings, 2 is info, 3 is debug
    args.verbosity = max(0, args.verbosity)
    args.verbosity = min(3, args.verbosity)
    loglevel_dict = { 0:logging.ERROR, 1:logging.WARNING, 2:logging.INFO, 3:logging.DEBUG }
    loglevel = loglevel_dict[args.verbosity]

    #make sure sigma cut is positive
    args.sigma_cut_factor = abs(args.sigma_cut_factor)

    #create a start-date string so we can name the log  file
    date_string = datetime.datetime.now().strftime('%b-%d-%I%M%p-%G')
    logging.basicConfig(filename='ffres2pcp-' + date_string + '.log', level=loglevel)

    if args.verbosity >= 1:
        print('args: ', args)

    logging.info( 'ffres2pcp program arguments:\n ' + str( args ) )

    initial_cf = args.control_file
    ref_station = args.network_reference_station
    rem_stations = args.stations
    data_path = args.data_directory

    #rely on simple naming conventions to determine if we were given a scan or
    #a full experiment directory (this may not work in the general case)
    single_scan_mode = False
    if '-' in os.path.split(data_path)[1]: #look for scan name with DDD-HHMM of DDD-HHMMx format
        single_scan_mode = True
    if single_scan_mode:
        scan_dir = os.path.abspath(data_path)
        scan_name = os.path.split(scan_dir)[1]
        exp_dir = os.path.dirname(scan_dir)
        exp_name = os.path.split(exp_dir)[1]
    else:
        exp_dir = os.path.abspath(data_path)
        exp_name = os.path.split(exp_dir)[1]

    #make sure we only got a single char for the ref stations
    if len(ref_station) != 1:
        print('Error: must specify a single reference station.')
        sys.exit(1)

    #make sure the ref station didn't sneak into the list of remote stations
    rem_stations = ''.join( sorted( list( set( list(rem_stations) ).difference( set( list(ref_station) ) ) ) ) )

    #get experiment number from dir name
    exp_name = os.path.split( os.path.abspath(exp_dir) )[1]
    work_dir = exp_dir

    #if this option is passed we need to mirror the experiment directory into some scratch space
    #so that we don't pollute it with extraneous fringe files
    if args.use_scratch is True:
        scratch_topdir = os.path.join(exp_dir, 'scratch')
        if not os.path.exists(scratch_topdir):
            os.makedirs(scratch_topdir)
        current_date = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
        scratch_dir = os.path.join(scratch_topdir, current_date)
        if not os.path.exists(scratch_dir):
            os.makedirs(scratch_dir)
        #mirror the experiment directory to the scratch space directory using symlinks
        exp_name = os.path.split( os.path.abspath(exp_dir) )[1]
        work_dir = os.path.join(scratch_dir, exp_name)
        ht.mirror_directory_with_symlinks(exp_dir, work_dir, exclude_list=['prepass', 'scratch'])

    #take care of start/stop scan limits
    start_scan_limit = "000-0000"
    stop_scan_limit = "999-9999"
    if single_scan_mode is True:
        start_scan_limit = scan_name
        stop_scan_limit = scan_name
    else:
        #check the format of the scan limit formats:
        if args.begin_scan_limit != "000-0000":
            if len( args.begin_scan_limit ) !=  len('DOY-HHMM'):
                print("error: begin-scan format not understood, please specify as DOY-HHMM.")
                sys.exit(1)
            doy = int(args.begin_scan_limit.split('-')[0])
            hour = int(args.begin_scan_limit.split('-')[1][:2])
            minute = int(args.begin_scan_limit.split('-')[1][2:4])
            if (doy < 0) or (doy > 366) or (hour < 0) or (hour > 23) or (minute < 0) or (minute > 59):
                print("error: could not decode end-scan format please specify as DOY-HHMM.")
                sys.exit(1)

        if args.end_scan_limit != "999-9999":
            if len( args.end_scan_limit ) !=  len('DOY-HHMM'):
                print("error: begin-scan format not understood, please specify as DOY-HHMM.")
                sys.exit(1)
            doy = int(args.end_scan_limit.split('-')[0])
            hour = int(args.end_scan_limit.split('-')[1][:2])
            minute = int(args.end_scan_limit.split('-')[1][2:4])
            if (doy < 0) or (doy > 366) or (hour < 0) or (hour > 23) or (minute < 0) or (minute > 59):
                print("error: could not decode end-scan format please specify as DOY-HHMM.")
                sys.exit(1)

        start_scan_limit = args.begin_scan_limit
        stop_scan_limit = args.end_scan_limit


    #now configure and run ffres2pcp
    ffres2pcp_conf = vpal.ffres2pcp_lib.Configuration()
    ffres2pcp_conf.exp_directory = os.path.abspath(work_dir)
    ffres2pcp_conf.network_reference_station = ref_station
    ffres2pcp_conf.target_stations = rem_stations
    ffres2pcp_conf.control_file = os.path.abspath(initial_cf)
    ffres2pcp_conf.num_proc = args.num_proc
    ffres2pcp_conf.verbosity = args.verbosity
    ffres2pcp_conf.min_snr = args.snr_min
    ffres2pcp_conf.min_qcode = args.quality_lower_limit
    ffres2pcp_conf.dtec_tolerance = args.dtec_thresh
    ffres2pcp_conf.max_number_to_select = args.averaging_scan_limit
    ffres2pcp_conf.sigma_cut_factor = args.sigma_cut_factor
    ffres2pcp_conf.start_scan_limit = start_scan_limit
    ffres2pcp_conf.stop_scan_limit = stop_scan_limit
    ffres2pcp_conf.use_progress_ticker = args.use_progress_ticker
    ffres2pcp_conf.nchannel_discard_threshold = args.nchannel_discard_threshold
    ffres2pcp_conf.channel_discard_tolerance = args.channel_discard_tolerance

    #now generate the new control file name
    ffres2pcp_output_control_filename = ''
    if args.output_filename != '':
        ffres2pcp_output_control_filename = os.path.abspath( args.output_filename )
    else:
        if single_scan_mode:
            outputfile_basename = "./cf_" + exp_name + "_" + scan_name + "_" + ref_station + rem_stations + "_pcphases"
        else:
            outputfile_basename = "./cf_" + exp_name + "_" + ref_station + rem_stations + "_pcphases"
        ffres2pcp_output_control_filename = os.path.join(os.path.abspath(work_dir), outputfile_basename)

    vpal.ffres2pcp_lib.generate_ffres2pcp_control_file(ffres2pcp_conf, ffres2pcp_output_control_filename)

    #append a log of the options/parameters used when generating this control file:
    if args.toggle_dump_info is True:
        with open(os.path.abspath(ffres2pcp_output_control_filename), "a") as cf_file:
            cf_file.write("\n")
            cf_file.write("*==========================================================\n")
            cf_file.write("* This control file was generated with the script ffres2pcp.py\n")
            cf_file.write("* on " + date_string + " using the following arguments: \n")
            cf_file.write('* args: ' + str(args) + "\n" )
            cf_file.write("*========================================================== \n")



if __name__ == '__main__':          # official entry point
    main()
    sys.exit(0)
