"""integration test for fourphase_lib"""
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
import vpal.fourphase_lib

################################################################################

def main():

    parser = argparse.ArgumentParser(
        prog='test_fourphase.py', \
        description='''integration test for fourphase_lib''' \
        )

    parser.add_argument('data_directory', help='path to the test data directory')
    args = parser.parse_args()
    exp_dir = os.path.abspath(args.data_directory)
    if os.path.isdir(exp_dir) is False:
        return 1

    ref_station = 'G'
    rem_stations = 'EV'

    #now configure and run fourphase
    config_obj = vpal.fourphase_lib.Configuration()
    config_obj.exp_directory = exp_dir
    config_obj.stations = ref_station + rem_stations
    config_obj.control_file = os.path.join( exp_dir, 'cf_GEV_test_pcphases')
    config_obj.num_proc = 8
    config_obj.verbosity = 3
    config_obj.min_snr = 30
    config_obj.min_qcode = 3
    config_obj.dtec_tolerance = 1.0
    config_obj.max_number_to_select = 10
    config_obj.sigma_cut_factor = 3.0
    config_obj.start_scan_limit = "000-0000"
    config_obj.stop_scan_limit = "999-9999"
    config_obj.use_progress_ticker = False

    #check that the expected control file is there
    if os.path.isfile(config_obj.control_file) is False:
        return 1

    #verbosity levels: 0 is silence
    loglevel_dict = { 0:logging.ERROR, 1:logging.WARNING, 2:logging.INFO, 3:logging.DEBUG }
    loglevel = loglevel_dict[config_obj.verbosity]
    logging.basicConfig(level=loglevel)

    #generate the station phase/delay offsets
    report_data = vpal.fourphase_lib.ExperimentReportData()
    station_delay_phase_results = vpal.fourphase_lib.generate_station_phase_delay_corrections(config_obj, report_data)
    report_data.generated_station_offsets = station_delay_phase_results

    #now compare with captured data for test
    #captured phase offset (deg) data values for test
    cached_phase_offset_values = dict()
    cached_phase_offset_values['E'] = [114.9, 2.12] #Y-off, error
    cached_phase_offset_values['G'] = [38.7, 2.31] #Y-off, error
    cached_phase_offset_values['V'] = [110.1, 5.12] #Y-off, error
    #capture delay offset (ns) data values for test
    cached_delay_values = dict()
    cached_delay_values['E'] = [0.721, 0.0011] #Y-off, error
    cached_delay_values['G'] = [1.675, 0.0011] #Y-off, error
    cached_delay_values['V'] = [0.054, 0.0014] #Y-off, error

    #test return value
    ret_status = 0

    all_stations = sorted( list(set(station_delay_phase_results.keys()) ) )
    micro_to_nano = 1000.0
    for st in all_stations:
        result = station_delay_phase_results[st]
        if result.get_n_total_entries() >= 1 and result.get_n_used_entries() >= 1:
            delay_mean = result.get_delay_offset_mean()*micro_to_nano
            delay_err = result.get_delay_offset_error()*micro_to_nano
            phase_mean = result.get_phase_offset_mean()
            phase_err = result.get_phase_offset_error()
            print("station, delay, delay_err, phase, phase_err = ", st, delay_mean, delay_err, phase_mean, phase_err)
            delta_arr = [0.0, 0.0, 0.0, 0.0]
            tolerance_arr = [0.001, 0.1, 0.001, 0.1]
            delta_arr[0] = abs(delay_mean - cached_delay_values[st][0]) #delay
            delta_arr[1] = abs(phase_mean - cached_phase_offset_values[st][0]) #phase
            delta_arr[2] = abs(delay_err - cached_delay_values[st][1]) #delay error
            delta_arr[3] = abs(phase_err - cached_phase_offset_values[st][1]) #phase error

            for i in list(range(0,len(delta_arr))):
                if delta_arr[i] > tolerance_arr[i]:
                    ret_status += 1


    #generate a report file
    date_string = datetime.datetime.now().strftime('%b-%d-%I%M%p-%G')
    report_data.create_report( os.path.join(exp_dir, "fourphase-report-" + date_string + ".json") )

    print("fourphase_lib test: ", ret_status)

    return ret_status


if __name__ == '__main__':          # official entry point
    return_value = main()
    sys.exit(return_value)
