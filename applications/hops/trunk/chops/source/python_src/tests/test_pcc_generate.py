"""integration test for proxy-cable-cal lib"""
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

#non-core imports
#set the plotting back-end to 'agg' to avoid display
import matplotlib as mpl
mpl.use('Agg')

#HOPS module imports
import vpal.proxy_cable_cal as pcc

################################################################################

def main():

    parser = argparse.ArgumentParser(
        prog='test_pcc_generate.py', \
        description='''integration test for proxy_cable_cal lib''' \
        )

    parser.add_argument('data_directory', help='path to the test data directory')
    args = parser.parse_args()
    exp_dir = os.path.abspath(args.data_directory)
    if os.path.isdir(exp_dir) is False:
        return 1


    logging.getLogger('vpal.proxy_cable_cal').addHandler(logging.NullHandler())

    #construct default output directory
    default_output_directory = os.path.join( exp_dir, "pcc_datfiles")
    output_directory = default_output_directory

    #stations/band/pol lists
    stations = 'GEV'
    band_list = ['A','B','C','D']
    pol_list = ['X','Y']

    #split the pol and band strings into lists
    pcc_config = pcc.PccConfiguration()
    pcc_config.exp_dir = exp_dir
    pcc_config.stations = stations
    pcc_config.output_dir = output_directory
    pcc_config.generate_plots = False
    pcc_config.generate_fit_diagnostics = False
    pcc_config.mode = 'VGOS' #args.mode
    pcc_config.band_list = band_list
    pcc_config.pol_list = pol_list
    pcc_config.scan_start_trim_length = 2
    pcc_config.reference_scan = ''
    pcc_config.use_progress_ticker = False
    pcc_config.verbosity = 0
    pcc_config.cut_threshold = 2.5

    #run the pcc generation
    station_delay_data = pcc.process_experiment(pcc_config)

    #test return value
    ret_status = 0

    #now for each station/band/pol, read and compare the result files with the cached comparison data
    #construct a file list fo each station/band/pol
    prefix = "bandmodel.vt7058."
    postfix = ".dat"
    file_list = []
    for st in stations:
        for band in band_list:
            for pol in pol_list:
                stbpol = st + "." + band + '.' + pol
                file_list.append(prefix + stbpol + postfix)

    #now for each file read the recently generated data and the cached data and do the comparison
    gen_dir = output_directory
    cached_dir = os.path.join( exp_dir, "cached_pcc_datfiles")
    for f in file_list:
        gen_data = os.path.join(gen_dir,f)
        cached_data = os.path.join(cached_dir,f)
        if os.path.exists(gen_data) and os.path.exists(cached_data): #check that the files exists
            #read the files into a experiment_pcc_band_delay object and compare
            gen_file_obj = pcc.ExperimentPccBandDelay()
            gen_file_obj.read_file(gen_data)
            cached_file_obj = pcc.ExperimentPccBandDelay()
            cached_file_obj.read_file(cached_data)
            if len(gen_file_obj.scan_pcc_line_list) == len(cached_file_obj.scan_pcc_line_list):
                n_lines = len(gen_file_obj.scan_pcc_line_list)
                #loop through the scan-data lines and compare the band model data
                for n in list(range(0,n_lines)):
                    gen_line = gen_file_obj.scan_pcc_line_list[n]
                    cached_line = cached_file_obj.scan_pcc_line_list[n]
                    if gen_line.is_equal_within_tolerance(cached_line) is False:
                        print(gen_data, " : ")
                        gen_line.print_line()
                        print(cached_data, " : ")
                        cached_line.print_line()
                        ret_status += 1
        else:
            ret_status += 1

    print("pcc_generate test: ", ret_status)
    return ret_status


################################################################################

if __name__ == '__main__':          # official entry point
    return_value = main()
    sys.exit(return_value)
