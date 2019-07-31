#!/usr/bin/python
#
# Python port of Brian Corey's supermongo scripts to process extracted pcal
# data for each station/band/pol and extract delays
# 11/21/2017 first version jpb

#core imports
from __future__ import print_function
from builtins import input
from builtins import str
import datetime
import argparse
import sys
import os
import glob
import logging

#HOPS module imports
import vpal.proxy_cable_cal as pcc
import vpal.pcc_plotting_utils as pcc_plot

def prompt_yes_no(prompt):
    while True:
        reply = str(input(prompt + ' (y/n):\n')).lower().strip()
        if reply[:1] == 'y':
            return True
        if reply[:1] == 'n':
            return False

################################################################################

def main():
    parser = argparse.ArgumentParser(
        prog='pcc_generate.py', \
        description='''utility for generating proxy-cable calibration delays from phase-cal tones''' \
        )

    parser.add_argument('stations', help='concatenated string of single codes of stations of interest')
    parser.add_argument('data_directory', help='relative path to directory containing experiment or scan data')

    parser.add_argument('-o', '--outputdir', dest='odir', help='set output directory name, overrides default location: <exp_dir>/pcc_datfiles.', default='INVALID')
    parser.add_argument('-v', '--verbosity', type=int, dest='verbosity', help='verbosity level: 0 (least verbose) to 3 (most verbose), default=0.', default=0)
    parser.add_argument('-f', '--figures', action='store_true', dest='figures', help='enable standard figures, default=False.', default=False)
    parser.add_argument('-t', '--trim-length', type=int, help='length of time to trim from the start of each scan (seconds), default=2.', dest='trim_length', default=pcc.DISCARD_OFFSET)
    parser.add_argument('-d', '--diagnostics', type=int, dest='diagnostics', help='enable p-cal (1) and fit (2) diagnostics plots, default=0 (disabled)', default=0)
    parser.add_argument('-r', '--reference-scan', help='specify phase reference scan (defaults to first scan)', dest='ref_scan', default='')
    parser.add_argument('-b', '--bands', help='list of frequency bands to process: e.g. --bands=A,B,C,D default is all.',dest='bands', default="")
    parser.add_argument('-p', '--pols', help='list of polarizations to include: e.g. --pols=X,Y, default=X,Y default is all', dest='pols', default="")
    parser.add_argument('-i', '--include-headers', action='store_true', dest='inc_headers', help='include a header line in the dat files, default=False.', default=False)
    parser.add_argument('-y', '--yes', action='store_true', dest='yes_prompt', help='reply yes to all prompts, default=False.', default=False)
    parser.add_argument('-n', '--no', action='store_true', dest='no_prompt', help='reply no to all prompts, default=False.', default=False)
    parser.add_argument('-e', '--estimate-progress', action='store_true', dest='est_prog', help='estimate progress, default=False.', default=False)
    parser.add_argument('-c', '--cut-sigma-factor', dest='sigma_cut_factor', type=float, help='cut tones with residual phase values which are more than cut_factor*sigma, default=2.5 (use 0.0 to disable).', default=2.5)

    #Eventually we will enable non-VGOS modes with this option, but these are disabled for the moment as they are experimental
    #parser.add_argument('-m', '--mode', help='proxy-cable cal mode: VGOS, MIXED, MIXED-RDBE, SX', dest='mode', default="VGOS")

    args = parser.parse_args()

    args.verbosity = max(0, args.verbosity)
    args.verbosity = min(3, args.verbosity)
    loglevel_dict = { 0:logging.ERROR, 1:logging.WARNING, 2:logging.INFO, 3:logging.DEBUG }
    loglevel = loglevel_dict[args.verbosity]

    #create a start-date string so we can name the log  file
    date_string = datetime.datetime.now().strftime('%b-%d-%I%M%p-%G')
    logging.basicConfig(filename='pcc-generate-' + date_string + '.log', level=loglevel)

    if args.verbosity >= 2:
        print( 'args: ', args)
    logging.info( 'pcc_generate program arguments:\n ' + str( args ) )

    stations = args.stations
    exp_dir = args.data_directory

    if len(stations) < 1:
        print( 'Error: must specify at least one station.')
        sys.exit(1)

    #get the experiment and output directories
    exp_directory = os.path.abspath(exp_dir)

    #construct default output directory
    default_output_directory = os.path.join( exp_directory, "pcc_datfiles")
    output_directory = default_output_directory

    if args.odir != 'INVALID':
        output_directory = os.path.abspath(args.odir)
    else:
        args.odir = output_directory

    file_count = 0;
    overwritten_stations = ''
    if not os.path.exists(output_directory):
        os.makedirs(output_directory)
    else:
        #need to check this directory for already existing dat-files
        file_count = 0
        for st in stations:
            #get a list of any station files in the output directory
            file_regex = os.path.join(output_directory, "bandmodel.*." + st + ".*.*.dat")
            st_list = glob.glob(file_regex)
            file_count += len(st_list)
            if len(st_list) != 0:
                overwritten_stations += st

    #if there are pre-existing files, warn the user they are about to be overwritten and give them a chance to bail out
    #this is all or nothing...we do not try to keep track of individual band-pol files
    keep_going = True
    if file_count != 0:
        if args.no_prompt == True:
            print("Warning: " + str(file_count) + " files for stations: " + overwritten_stations + " would have been overwritten, aborting." )
            keep_going = False
        elif args.yes_prompt == True:
            print("Warning: " + str(file_count) + " files for stations: " + overwritten_stations + " will be overwritten." )
            keep_going = True
        else:
            print("Warning: " + str(file_count) + " files for stations: " + overwritten_stations + " are about to be overwritten.")
            keep_going = prompt_yes_no("Do you wish to continue?")

    if keep_going == False:
        print("Aborting...")
        return;

    band_list = [ b.upper() for b in args.bands.split(',') if len(b) == 1 ]
    pol_list = [ p.upper() for p in args.pols.split(',') if len(p) == 1 ]
    #try to catch simple typos
    b_args_ok = True
    if len(band_list) != 0:
        if not all( len(x) == 1 for x in band_list):
            b_args_ok = False
    if b_args_ok is False:
        print("Error, could not parse bands specified: ", args.bands)
        sys.exit(1)

    p_args_ok = True
    if len(pol_list) != 0:
        if not all( len(x) == 1 for x in pol_list):
            p_args_ok = False
    if p_args_ok is False:
        print("Error, could not parse pols specified: ", args.pols)
        sys.exit(1)

    #split the pol and band strings into lists
    pcc_config = pcc.PccConfiguration()
    pcc_config.exp_dir = exp_directory
    pcc_config.stations = stations
    pcc_config.output_dir = output_directory
    pcc_config.band_list = band_list
    pcc_config.pol_list = pol_list
    pcc_config.scan_start_trim_length = args.trim_length
    pcc_config.reference_scan = args.ref_scan
    pcc_config.use_progress_ticker = args.est_prog
    pcc_config.verbosity = args.verbosity
    pcc_config.cut_threshold = args.sigma_cut_factor
    pcc_config.include_headers = args.inc_headers

    #run the pcc generation
    station_delay_data = pcc.process_experiment(pcc_config)

    pcc_plotter = pcc_plot.ProxyCableDelayFitPlotter()

    #now run results plotting
    if args.figures is True:
        for dat in station_delay_data.values():
            pcc_plotter.plot_individual_band_delays(dat, pcc_config)
            pcc_plotter.plot_mean_band_delay(dat, pcc_config)

    #run p-cal diagnostics plotting if needed
    if args.diagnostics >= 1:
        for dat in station_delay_data.values():
            pcc_plotter.plot_phasor_amplitude_surface(dat, pcc_config)

    #run fit diagnostics plotting if needed
    if args.diagnostics == 2:
        for dat in station_delay_data.values():
            pcc_plotter.plot_bandfits(dat, pcc_config)


################################################################################

if __name__ == '__main__':          # official entry point
    main()
    sys.exit(0)
