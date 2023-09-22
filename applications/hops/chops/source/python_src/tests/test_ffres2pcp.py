"""integration test for ffres2pcp_lib"""
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
        prog='test_ffres2pcp.py', \
        description='''integration test for ffres2pcp_lib''' \
        )

    parser.add_argument('data_directory', help='path to the test data directory')
    args = parser.parse_args()
    exp_dir = os.path.abspath(args.data_directory)
    if os.path.isdir(exp_dir) is False:
        return 1

    #configure test and run ffres2pcp
    config_obj = vpal.ffres2pcp_lib.Configuration()
    config_obj.exp_directory = exp_dir
    config_obj.network_reference_station = 'G'
    config_obj.target_stations = 'EV'
    config_obj.control_file = os.path.join( config_obj.exp_directory, 'cf_GEV_test')
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

    #report data object
    report_data = vpal.ffres2pcp_lib.ExperimentReportData()
    report_data.config_obj = config_obj

    #retrieve pre-existing pc_phases from control
    apriori_pcp = vpal.ffres2pcp_lib.get_apriori_pc_phases( os.path.abspath(config_obj.control_file), config_obj.target_stations, config_obj.network_reference_station, config_obj.network_reference_station_pol, config_obj.frequency_group)
    report_data.apriori_sspcp = apriori_pcp

    #generate station pc_phases for this experiment
    all_station_pcp = vpal.ffres2pcp_lib.generate_pc_phases(config_obj, report_data)
    report_data.generated_sspcp = all_station_pcp

    #captured data values for the test
    cached_pc_values = dict()
    cached_pc_values['E_X'] =[-11.3, -7.5, -3.7, -13.9, -8.4, 4.6, 9.9, 10.5, 2.0, 11.3, 8.4, 9.8, 2.9, 2.6, -1.3, 2.0, -19.3, -19.4, -20.9, -12.7, -8.0, -1.9, 0.7, 0.6, 10.7, 20.9, 27.0, 20.7, 5.8, -5.1, -27.0, 9.7]
    cached_pc_values['E_Y'] =[-17.2, -6.1, -4.8, -36.7, -16.0, -7.0, 35.2, 16.7, -2.3, -0.7, 10.0, 14.1, 6.9, 5.5, -1.4, 2.4, -17.7, -15.2, -17.9, -22.4, -8.4, -9.0, -1.1, 1.6, 42.9, 3.6, 32.9, 18.8, 26.9, -6.5, -27.7, 7.1]
    cached_pc_values['G_Y'] =[-2.5, 1.0, 4.4, -4.2, -7.1, -5.4, -2.5, -5.3, -10.3, -1.0, -5.5, -2.9, -5.7, -0.5, -4.0, -5.1, -0.2, 0.0, 0.6, 0.0, 0.5, 2.3, 3.1, 1.4, 0.0, 10.5, 22.8, 15.9, 0.0, -2.2, -25.6, 0.0]
    cached_pc_values['V_X'] =[-36.1, -35.7, -15.4, 10.4, 28.4, 25.5, 21.7, 4.8, -9.4, -9.8, -8.7, -16.4, -9.0, 1.1, 12.8, 13.6, -5.1, -2.4, 0.8, -16.4, -10.3, 78.1, -14.3, -33.3, -1.5, 11.3, 24.5, 17.7, 16.3, -25.8, -11.1, 11.4]
    cached_pc_values['V_Y'] =[-37.3, -33.6, -20.2, 10.8, 27.4, 32.6, 28.2, 10.4, -9.8, -5.7, -11.5, -8.8, -4.5, 2.4, 7.9, 4.3, 0.5, 16.5, 12.1, -3.3, 14.5, -16.2, -20.1, -22.3, -3.5, 7.6, 10.2, 0.1, 1.1, -6.7, 0.5, 14.8]

    #compute the final pc_phases
    all_keys, final_pc_values = vpal.ffres2pcp_lib.compute_final_pcphases(apriori_pcp, all_station_pcp)

    #test return value
    ret_status = 0 #0 indicates success, a non-zero value: failure

    tolerance = 0.1 #tolerance to detect changes is 0.1 degrees
    for stpol in cached_pc_values.keys():
        if stpol in final_pc_values:
            gen_ch_pcp = final_pc_values[stpol].items()
            gen_values = [ x[1] for x in ( sorted(gen_ch_pcp, key=vpal.ffres2pcp_lib.channel_sort_key) ) ]
            test_values = cached_pc_values[stpol] #this is a list
            if len(gen_values) != len(test_values):
                ret_status += 1
            else:
                for i in list(range(0,len(gen_values))):
                    delta = gen_values[i] - test_values[i]
                    if abs(delta) > tolerance:
                        ret_status += 1
        else:
            ret_status += 1
            break

    #generate a report file
    date_string = datetime.datetime.now().strftime('%b-%d-%I%M%p-%G')
    report_data.create_report( os.path.join(exp_dir, "ffres2pcp-report-" + date_string + ".json") )


    print("ffres2pcp_lib test: ", ret_status)
    return ret_status




if __name__ == '__main__':          # official entry point
    return_value = main()
    sys.exit(return_value)
