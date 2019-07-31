"""module to determine the appropriate manual phases/delay offsets between the
Y-X polarization signal chains for each station during an experiment"""

#core imports
from __future__ import print_function
from __future__ import absolute_import
from __future__ import division
from builtins import str
from past.utils import old_div
from builtins import object
from builtins import range
import datetime
import os
import math
import json
#set up module level logger
import logging
fourphase_logger = logging.getLogger(__name__)

#non-core imports
import numpy as np
import scipy.stats

#hops package python libs
import ffcontrol

#local imports
from . import processing
from . import utility
from . import report_lib

#progress bar
try:
    from progress.bar import Bar #non-core import
except:
    Bar = utility.Bar


class Configuration(report_lib.JsonSerializableObject ):
    """configuration object for fourphase"""
    #default values for init
    def __init__(self):
        self.exp_directory = './' #location of experiment data
        self.stations = '' #list of stations to generate pc_phases for
        self.control_file = '' #initial_control_file
        self.num_proc = 1 #number of processes to run in parallel
        self.verbosity = 0 #verbostiy level
        self.min_snr = 30 #min SNR allowed
        self.max_snr = 500 #max SNR allowed
        self.min_qcode = 3 #minimum quality code required
        self.dtec_tolerance = 1.0
        self.start_scan_limit = '000-0000' #earliest scan to use (DOY-HHMM)
        self.stop_scan_limit = '999-9999' #latest scan to use (DOY-HHMM)
        self.max_number_to_select = 0 #the max number of scans used for each station to compute the delay/phase offsets (zero = unlimited)
        self.dtec_nominal_error = 0.1 #empirically determined factor for search function
        self.sigma_cut_factor = 3.0 #cut delay/phase corrections which are further from the mean than sigma*sigma_cut_factor, use 0.0 to disable
        self.pol_products = ['XX', 'YY', 'XY', 'YX'] #default, vgos pol products
        self.use_progress_ticker = True

    def export_json(self):
        return self.__dict__

################################################################################
################################################################################
################################################################################

class VGOSFourphaseSingleScanProcessor(object):

    """ simplified fourphase processor, single baseline values (no least-squares fitting involved)
    we just average the results of this processor over multiple scans to obtain results for each station"""

    def __init__(self, root_file, control_file):
        self.root_file_path = os.path.abspath(root_file)
        self.scan_dir = os.path.dirname(self.root_file_path)
        self.exp_dir = os.path.dirname(self.scan_dir)
        self.scan_name = os.path.split(self.scan_dir)[1]
        self.exp_name = os.path.split(self.exp_dir)[1]
        self.control_file_path = os.path.abspath(control_file)
        self.ion_search_control_file_path = ''
        self.ion_fix_control_file_path = dict()
        self.stations = ''
        self.error_condition = 0
        self.pol_products = ['XX', 'XY', 'YY', 'YX']
        self.num_proc = 1
        self.dtec_tolerance = 1.0
        self.baseline_dtec = dict() #filled in 'single baseline' mode
        self.station_y_minus_x_delays = dict()
        self.station_y_minus_x_phases = dict()
        self.use_progress_ticker = True

    def set_stations(self, stations=''):
        self.stations = stations

    def construct_ionosphere_search_control_file(self):
        """create and ionosphere search control file for the first pass"""
        #make sure that the control enforces a exhaustive search for the dTEC for each baseline
        cf_ion_name = os.path.basename(self.control_file_path) + "-" + self.scan_name + "-ion-search"
        self.ion_search_control_file_path = os.path.join(self.scan_dir, cf_ion_name)
        cf_lines = 'ion_npts 45\n'
        cf_lines += 'ion_smooth true\n'
        cf_lines += 'ion_win -88.0 88.0\n'
        cf_lines += 'pc_delay_x 0.0' + ' \n'
        cf_lines += 'pc_delay_y 0.0' + ' \n'

        #need to make sure we remove pc_delay and pc_phase_offset lines, as well as ionosphere lines
        original_string_list = []
        replacement_string_list = []
        original_string_list.append('pc_delay_'); replacement_string_list.append('*removed by fourphase: pc_delay_')
        original_string_list.append('pc_phase_offset_'); replacement_string_list.append('*removed by fourphase: pc_phase_offset_')
        original_string_list.append('ion_win'); replacement_string_list.append('*removed by fourphase: ion_win')
        original_string_list.append('ion_npts'); replacement_string_list.append('*removed by fourphase: ion_npts')
        # original_string_list.append('ion_smooth'); replacement_string_list.append('*removed by fourphase: ion_smooth')
        original_string_list.append('ionosphere'); replacement_string_list.append('*removed by fourphase: ionosphere')
        ffcontrol.prepend_control_file_with_find_and_replace(self.control_file_path, self.ion_search_control_file_path, cf_lines, original_string_list, replacement_string_list)
        # ffcontrol.append_control_file_with_find_and_replace(self.control_file_path, self.ion_search_control_file_path, cf_lines, original_string_list, replacement_string_list)

    def determine_ionosphere(self):
        """determine the dTEC for each baseline"""
        self.construct_ionosphere_search_control_file()

        #fourfit all pol-products on each baseline to compute the SNR-weighted dTEC
        set_commands = "set gen_cf_record true"
        blpp_collections = processing.load_batch_cut_and_sort( \
            self.exp_dir, self.stations[0], self.stations[1:], self.ion_search_control_file_path, \
            set_commands, network_reference_baselines_only=False, num_processes=self.num_proc, \
            start_scan_limit=self.scan_name, stop_scan_limit=self.scan_name, \
            only_complete=True, pol_products=self.pol_products, use_progress_ticker=self.use_progress_ticker \
        )
        baseline_info = dict() # weighted-mean-dtec, net-snr
        for blc in blpp_collections:
            bl = blc.baseline
            if blc.get_dtec_max_deviation() < self.dtec_tolerance:
                dtec_list = blc.get_dtec_list()
                snr2_list = [ x*x for x in blc.get_snr_list() ]
                wmean_dtec = utility.compute_weighted_mean(dtec_list, snr2_list)
                net_weight = math.sqrt( math.fsum(snr2_list) )
                baseline_info[bl] = (wmean_dtec, net_weight)
            else:
                #error, this baseline collection does not have consistent dTEC solution for all pols
                self.error_condition = 1
                fourphase_logger.debug( '\n**** WARNING **** check ionosphere fit! dtec on baseline ' + bl + ' differed by '  + str( round(blc.get_dtec_max_deviation(),1) ) + ' TEC units' )

        #ionosphere determined separately for each baseline, over the 4 pol-products (no global combined fit)
        self.baseline_dtec = { bl:x[0] for bl, x in list(baseline_info.items()) }

    def construct_ionosphere_fix_control_file(self):
        """construct a control file with a fixed dTEC for each baseline """
        #make sure the list is always in the same order
        bl_sorted = sorted( set( list(self.baseline_dtec.keys()) ) )
        for bl in bl_sorted:
            cf_ion_name = os.path.basename(self.control_file_path) + "-" + self.scan_name + "-ion-fixed-" + bl
            self.ion_fix_control_file_path[bl] = os.path.join(self.scan_dir, cf_ion_name)
            cf_lines = '\n'
            dtec = round(self.baseline_dtec[bl], 6)
            cf_lines += 'if station ' + bl[0] + '\n'
            cf_lines += '  ion_npts 1 \n'
            cf_lines += '  ionosphere 0.0 \n'
            cf_lines += 'if station ' + bl[1] + '\n'
            cf_lines += '  ion_npts 1 \n'
            cf_lines += '  ionosphere ' + str(dtec) + ' \n'
            #need to make sure we remove pc_delay and pc_phase_offset lines, as well as ionosphere lines
            original_string_list = []
            replacement_string_list = []
            original_string_list.append('pc_delay_'); replacement_string_list.append('*removed by fourphase: pc_delay_')
            original_string_list.append('pc_phase_offset_'); replacement_string_list.append('*removed by fourphase: pc_phase_offset_')
            original_string_list.append('ion_win'); replacement_string_list.append('*removed by fourphase: ion_win')
            original_string_list.append('ion_npts'); replacement_string_list.append('*removed by fourphase: ion_npts')
            # original_string_list.append('ion_smooth'); replacement_string_list.append('*removed by fourphase: ion_smooth')
            original_string_list.append('ionosphere'); replacement_string_list.append('*removed by fourphase: ionosphere')
            ffcontrol.append_control_file_with_find_and_replace(self.control_file_path, self.ion_fix_control_file_path[bl], cf_lines, original_string_list, replacement_string_list)

    def calculate_delay_phase_offsets(self):
        """ compute the phase/delay offsets for Y-X at each station """
        self.construct_ionosphere_fix_control_file()

        bl_sorted = sorted( set( list(self.baseline_dtec.keys()) ) )

        #run fourfit again with the fixed ionosphere solutions
        set_commands = "set gen_cf_record true"
        blpp_collections = []
        for bl in bl_sorted:
            temp_blpp_collections = processing.load_batch_cut_and_sort( \
                self.exp_dir, bl[0], bl[1], self.ion_fix_control_file_path[bl], \
                set_commands, network_reference_baselines_only=False, num_processes=self.num_proc, \
                start_scan_limit=self.scan_name, stop_scan_limit=self.scan_name, \
                only_complete=True, pol_products=self.pol_products, use_progress_ticker=self.use_progress_ticker \
            )
            blpp_collections.extend(temp_blpp_collections)

        #loop over station-pol-product-baseline collections, extracting mbd and phase
        results_table = dict()
        valid_stations = set()
        valid_baselines = set()
        for blc in blpp_collections:
            bl = blc.baseline
            if blc.get_min_snr() > 15.0 and blc.get_mean_snr() > 25.0:
                #SNR ok, include this baseline
                valid_baselines.add(bl)
                valid_stations.add(bl[0])
                valid_stations.add(bl[1])
                for pp in self.pol_products:
                    ff = blc.get_fringe_object(pp)
                    results = ( ff.snr, ff.sbdelay, ff.mbdelay, ff.resid_phas, ff.dtec, ff.amp*10000 )
                    blpp_key  = (bl, pp)
                    results_table[blpp_key] = results
            else:
                #not enough cross power in this scan, so discard this baseline and issue warning
                self.error_condition = 2
                fourphase_logger.debug( '\n**** ERROR **** ' + bl + ' has min snr of only ' +  str(blc.get_min_snr() ) + ' and mean snr for all other pol-products is ' + str(blc.get_mean_snr()) )

        #if verbose:
        #    print_table(results_table)

        #loop over results and determine the Y-X phase and delay offsets for each station
        self.station_y_minus_x_delays = { st:list() for st in valid_stations }
        self.station_y_minus_x_phases = { st:list() for st in valid_stations }
        #loop over each baseline, and extract the station y-x phase/delays offsets
        for bl in valid_baselines:
            #compute delay offsets for ref and rem stations
            dyy_xy = results_table[(bl,'YY')][2] - results_table[(bl,'XY')][2]
            dyx_xx = results_table[(bl,'YX')][2] - results_table[(bl,'XX')][2]
            dxx_xy = results_table[(bl,'XX')][2] - results_table[(bl,'XY')][2]
            dyx_yy = results_table[(bl,'YX')][2] - results_table[(bl,'YY')][2]
            self.station_y_minus_x_delays[bl[0]].append( ( dyy_xy, bl[1] ) )
            self.station_y_minus_x_delays[bl[0]].append( ( dyx_xx, bl[1] ) )
            self.station_y_minus_x_delays[bl[1]].append( ( dxx_xy, bl[0] ) )
            self.station_y_minus_x_delays[bl[1]].append( ( dyx_yy, bl[0] ) )
            #compute phase offsets
            pyy_xy = utility.limit_periodic_quantity_to_range(results_table[(bl,'YY')][3] - results_table[(bl,'XY')][3], low_value=-180.0, high_value=180.0)
            pyx_xx = utility.limit_periodic_quantity_to_range(results_table[(bl,'YX')][3] - results_table[(bl,'XX')][3], low_value=-180.0, high_value=180.0)
            pxx_xy = utility.limit_periodic_quantity_to_range(results_table[(bl,'XX')][3] - results_table[(bl,'XY')][3], low_value=-180.0, high_value=180.0)
            pyx_yy = utility.limit_periodic_quantity_to_range(results_table[(bl,'YX')][3] - results_table[(bl,'YY')][3], low_value=-180.0, high_value=180.0)
            self.station_y_minus_x_phases[bl[0]].append( ( pyy_xy, bl[1] ) )
            self.station_y_minus_x_phases[bl[0]].append( ( pyx_xx, bl[1] ) )
            self.station_y_minus_x_phases[bl[1]].append( ( pxx_xy, bl[0] ) )
            self.station_y_minus_x_phases[bl[1]].append( ( pyx_yy, bl[0] ) )

    def get_raw_station_delay_offsets(self):
        return self.station_y_minus_x_delays

    def get_raw_station_phase_offsets(self):
        return self.station_y_minus_x_phases

    def get_mean_station_delay_offsets_and_error(self):
        st_delay_offsets = dict()
        st_delay_err = dict()
        for st, values in list(self.station_y_minus_x_delays.items()):
            tmp_val = [x[0] for x in values]
            if len(tmp_val) != 0:
                st_delay_offsets[st] = np.mean(tmp_val)
                st_delay_err[st] = np.std(tmp_val)
        return st_delay_offsets, st_delay_err

    def get_mean_station_phase_offsets_and_error(self):
        st_phase_offsets = dict()
        st_phase_err = dict()
        for st, values in list(self.station_y_minus_x_phases.items()):
            tmp_val = [x[0] for x in values]
            if len(tmp_val) != 0:
                st_phase_offsets[st] = scipy.stats.circmean( np.asarray(tmp_val), low=-180.0, high=180.0)
                st_phase_err[st] = scipy.stats.circstd( np.asarray(tmp_val), low=-180.0, high=180.0)
        return st_phase_offsets, st_phase_err

################################################################################
################################################################################
################################################################################

class SingleStationPhaseDelayOffsets( report_lib.JsonSerializableObject ):
    """container object for the y-x phase/delay offsets of a particular station"""

    def __init__(self, station_id):
        #raw data is indexed by scan
        self.station_id = station_id
        self.used_scans = []
        self.pc_delay_offsets = dict()
        self.pc_phase_offsets = dict()

    def add_phase_delay_offsets(self, scan_name, delay_offsets, phase_offsets):
        """add the phase/delay offsets derived from a particular scan"""
        self.used_scans.append(scan_name)
        self.pc_delay_offsets[scan_name] = delay_offsets
        self.pc_phase_offsets[scan_name] = phase_offsets

    def export_json(self):
        """export this object as a dict for json export"""
        json_dict = dict()
        json_dict["station_id"] = self.station_id
        json_dict["used_scans"] = self.used_scans
        json_dict["mean_pc_delay_offset"] = self.get_delay_offset_mean()
        json_dict["stddev_pc_delay_offset"] = self.get_delay_offset_error()
        json_dict["mean_pc_phase_offset"] = self.get_phase_offset_mean()
        json_dict["stddev_pc_phase_offset"] = self.get_phase_offset_error()
        json_dict["pc_delay_offsets"] = self.pc_delay_offsets
        json_dict["pc_phase_offsets"] = self.pc_phase_offsets
        return json_dict

    def use_data_point(self, data_point):
        """ quick check to see if this data point is use-able after cuts are applied """
        if len(data_point) == 2:
            return True
        if len(data_point) == 3:
            if data_point[2] is True:
                return True
            else:
                return False
        return False

    def get_n_total_entries(self):
        count = 0
        for val in list(self.pc_delay_offsets.values()):
            count += len(val)
        return count

    def get_n_used_entries(self):
        count = 0
        for val in list(self.pc_delay_offsets.values()):
            for x in val:
                if len(x) == 3:
                    if x[2] is True:
                        count += 1
                else:
                    count += 1
        return count

    def get_n_cut_entries(self):
        count = 0
        for val in list(self.pc_delay_offsets.values()):
            for x in val:
                if len(x) == 3:
                    if x[2] is False:
                        count += 1
                else:
                    count += 1
        return count

    def get_all_delay_offset_values(self):
        all_values = []
        for val in list(self.pc_delay_offsets.values()):
            tmp_val = [x[0] for x in val]
            all_values.extend(tmp_val)
        return all_values

    def get_all_phase_offset_values(self):
        all_values = []
        for val in list(self.pc_phase_offsets.values()):
            tmp_val = [x[0] for x in val]
            all_values.extend(tmp_val)
        return all_values

    def get_delay_offset_mean(self):
        all_values = []
        for val in list(self.pc_delay_offsets.values()):
            tmp_val = [x[0] for x in val if self.use_data_point(x)]
            all_values.extend(tmp_val)
        if len(all_values) != 0:
            return np.mean(all_values)
        else:
            fourphase_logger.error( 'Error: station ' + self.station_id + ' has no useable delay offset data to compute mean Y-X delay offset.' )
            return 0.0

    def get_delay_offset_median(self):
        all_values = []
        for val in list(self.pc_delay_offsets.values()):
            tmp_val = [x[0] for x in val if self.use_data_point(x)]
            all_values.extend(tmp_val)
        if len(all_values) != 0:
            return np.median(all_values)
        else:
            fourphase_logger.error( 'Error: station ' + self.station_id + ' has no useable delay offset data to compute median Y-X delay offset.' )
            return 0.0

    def get_delay_offset_mad(self):
        all_values = []
        for val in list(self.pc_delay_offsets.values()):
            tmp_val = [x[0] for x in val if self.use_data_point(x)]
            all_values.extend(tmp_val)
        if len(all_values) != 0:
            return utility.mad(all_values)
        else:
            fourphase_logger.error( 'Error: station ' + self.station_id + ' has no useable delay offset data to compute Y-X delay offset MAD.' )
            return np.nan

    def get_delay_offset_error(self):
        all_values = []
        for val in list(self.pc_delay_offsets.values()):
            tmp_val = [x[0] for x in val if self.use_data_point(x)]
            all_values.extend(tmp_val)
        if len(all_values) != 0:
            return np.std(all_values)
        else:
            fourphase_logger.error( 'Error: station ' + self.station_id + ' has no useable delay offset data to compute std. dev. of Y-X delay offset.' )
            return np.nan

    def get_phase_offset_mean(self):
        all_values = []
        for val in list(self.pc_phase_offsets.values()):
            tmp_val = [x[0] for x in val if self.use_data_point(x)]
            all_values.extend(tmp_val)
        if len(all_values) != 0:
            return scipy.stats.circmean( np.asarray(all_values), low=-180.0, high=180.0)
        else:
            fourphase_logger.error( 'Error: station ' + self.station_id + ' has no useable phase offset data to compute mean Y-X phase offset.' )
            return 0.0

    def get_phase_offset_error(self):
        all_values = []
        for val in list(self.pc_phase_offsets.values()):
            tmp_val = [x[0] for x in val if self.use_data_point(x)]
            all_values.extend(tmp_val)
        if len(all_values) != 0:
            return scipy.stats.circstd( np.asarray(all_values), low=-180.0, high=180.0)
        else:
            fourphase_logger.error( 'Error: station ' + self.station_id + ' has no useable phase offset data to compute std. dev of Y-X phase offset.' )
            return np.nan

    def apply_sigma_cut(self, sigma_cut_factor):
        """remove data points which are more then sigma_cut_factor*sigma away from the mean value """
        if sigma_cut_factor != 0.0:
            mean_delay = self.get_delay_offset_mean()
            stddev_delay = self.get_delay_offset_error()
            mean_phase = self.get_phase_offset_mean()
            stddev_phase = self.get_phase_offset_error()
            median_delay = self.get_delay_offset_median()
            mad_delay = self.get_delay_offset_mad()

            for scan in self.used_scans:
                #strip delay values which are too far from the mean
                delay_list = self.pc_delay_offsets[scan]
                new_delay_list = []
                for val in delay_list:
                    tmp_val = val[0]
                    if abs(tmp_val - median_delay) < abs(mad_delay*sigma_cut_factor):
                        new_delay_list.append( (val[0], val[1], True) )
                    else:
                        new_delay_list.append( (val[0], val[1], False) )
                self.pc_delay_offsets[scan] = new_delay_list

                #strip phase values which are too far from the mean
                phase_list = self.pc_phase_offsets[scan]
                new_phase_list = []
                for val in phase_list:
                    tmp_val = val[0]
                    if abs( utility.minimum_angular_difference(tmp_val, mean_phase) ) < abs(stddev_phase*sigma_cut_factor):
                        new_phase_list.append( (val[0], val[1], True) )
                    else:
                        new_phase_list.append( (val[0], val[1], False) )
                self.pc_phase_offsets[scan] = new_phase_list


################################################################################
################################################################################
################################################################################
class ExperimentReportData( report_lib.JsonSerializableObject ):
    """container class for report data """
    def __init__(self):
        self.config_obj = Configuration()
        self.generated_station_offsets = dict()

    def export_json(self):
        json_dict = dict()
        json_dict["configuration"] = self.config_obj.export_json()
        generated_offsets_dict = dict()
        for st, obj in list(self.generated_station_offsets.items()):
            generated_offsets_dict[st] = obj.export_json()
        json_dict['generated_station_phase_delay_offsets'] = generated_offsets_dict
        return json_dict

    def import_json(self, json_dict):
        if isinstance(json_dict, dict):
            self.config_obj.import_json(json_dict["configuration"])
            for key, val in list(json_dict['generated_station_phase_delay_offsets'].items()):
                obj = SingleStationPhaseDelayOffsets(key)
                obj.import_json(val)
                self.generated_station_offsets[key] = obj

    def create_report(self, report_filename):
        report_file = os.path.abspath(report_filename)
        report = open(report_filename, "w")
        report.write(json.dumps(self,default=report_lib.NestedObjectEncoder, indent=2))
        report.close()

    def load_report(self, report_filename):
        report_file = os.path.abspath(report_filename)
        with open(report_file, "r") as report:
            data = json.load(report)
            self.import_json(data)

################################################################################
################################################################################
################################################################################

def select_fourphase_scans_by_baseline(SingleScanBaselineCollection_list, exp_directory, stations, dtec_tolerance=1.0, min_snr=30, dtec_nom=0.1, max_number_to_select=0):
    """ given a list of SingleScanBaselineCollection's, try to select a list of scans which will provide good fourphase output stations on each baseline
        returns a dictionary (indexed by baseline) of scans collections to be processed"""

    #we want to go through the given list of scans, and try to come up with a subset of scans we expect to be useful for each baseline
    #using these per-baseline scan-lists, we construct a station-based list of scans, and then we eliminate duplicates to get
    #final set of scans to process

    blist = processing.construct_valid_baseline_list(exp_directory, stations[0], stations[1:])
    baseline_scan_list = { bl:[] for bl in blist }

    for bl in blist:
        sublist = [ ssbc for ssbc in SingleScanBaselineCollection_list if bl in ssbc.get_baselines_present() ]

        #remove all of the scans which do not meet our cut criteria on this baseline
        tmp_sublist = []
        for scan_collection in sublist:
            blc = scan_collection.get_collection_for_baseline(bl)
            if blc.get_dtec_max_deviation() <= dtec_tolerance and blc.get_min_snr() >= min_snr:
                tmp_sublist.append(scan_collection)
        sublist = tmp_sublist

        if max_number_to_select > 0:
            #If the user has limited the number of scans to be used by fourphase
            #then we need to go through and select the best ones we can find.
            #To do this we construct the pareto front for the current set of scans
            #then remove the 'optimal' scan on the front and put it into the list of scans we
            #want to process. These then repeats iteratively until we have enough scans.
            while len(baseline_scan_list[bl]) < max_number_to_select and len(sublist) > 0:
                #create list of baseline-collections
                blc_list = [ scan.get_collection_for_baseline(bl) for scan in sublist]
                #create look-up table from baseline-collection to scan
                blc_to_scan_dict = { scan.get_collection_for_baseline(bl):scan for scan in sublist }
                #find the pareto front for this list of scans
                fourphase_pareto = utility.compute_2d_pareto_front(blc_list, "min_snr", "dtec_mdev", True, False)
                fourphase_scan = None
                best_score = 0
                for blc in fourphase_pareto:
                    score = abs( old_div(blc.get_min_snr(),(blc.get_dtec_max_deviation() + dtec_nom)) )
                    if score > best_score:
                        best_score = score
                        fourphase_scan = blc_to_scan_dict[blc]
                if fourphase_scan != None:
                    baseline_scan_list[bl].append(fourphase_scan)
                    sublist.remove(fourphase_scan)
                else:
                    break
        else:
            #just use all the scans which pass the dtec/snr cuts
            baseline_scan_list[bl] = sublist

    return baseline_scan_list

################################################################################
################################################################################
################################################################################

def generate_station_phase_delay_corrections(config_obj, report_data_obj=None):
    """run the fourphase algorithm to generate Y-X delay/phase corrections for each VGOS station"""
    #basic process is to:
    #(1) fourfit the experiment with the initial control file
    #(2) collect the baseline-pol-products collections into scan collections
    #(3) use empirical search to locate good fourphase scans
    #(4) extract delay/phase offsets from each scan for each station
    #(5) return station delay/phase result objects

    exp_dir = config_obj.exp_directory
    stations = config_obj.stations
    control_file_path = os.path.abspath( config_obj.control_file )

    #force all fringe files generated to save the control file
    #information in the type_222 records
    set_commands = "set gen_cf_record true"
    min_snr = config_obj.min_snr
    max_snr = config_obj.max_snr
    verbosity = config_obj.verbosity
    valid_qcodes = [ x for x in list(range(config_obj.min_qcode, 10)) ]
    num_proc = config_obj.num_proc
    start = config_obj.start_scan_limit
    stop = config_obj.stop_scan_limit
    netref_baselines_only = False
    only_complete_blc = True
    max_num_scans = config_obj.max_number_to_select
    dtec_nom = config_obj.dtec_nominal_error
    dtec_tolerance = config_obj.dtec_tolerance
    pol_products = config_obj.pol_products


    fourphase_logger.info("generating Y-X delay/phase corrections from data in exp_dir: " + exp_dir + " for stations: " + config_obj.stations)
    fourphase_logger.info("initial control file is: " + control_file_path)

    log_fourfit_processes=False
    if config_obj.verbosity >=3:
        log_fourfit_processes = True

    #fourfit the exp_directory with the initial control file
    blc_list = processing.load_batch_cut_and_sort( \
        exp_dir, stations[0], stations[1:], control_file_path, set_commands, min_snr, max_snr, \
        valid_qcodes, netref_baselines_only, num_proc, start, stop, only_complete_blc, pol_products, config_obj.use_progress_ticker, log_fourfit_processes \
    )

    #join the baseline-pol-prod collections into scan collections
    scan_blc = processing.group_baseline_collections_by_scan(blc_list)

    #select scans to run fourphase on for each baseline
    baseline_indexed_scan_lists = select_fourphase_scans_by_baseline(scan_blc, exp_dir, stations, dtec_tolerance, min_snr, dtec_nom, max_num_scans)

    #since we are going to fourphase each scan and then average the results, we can just collect the set of unique scans/root files from all baselines
    root_file_set = set()
    for bl_list in list(baseline_indexed_scan_lists.values()):
        for scan in bl_list:
            root_file_set.add(scan.associated_root_file)
    root_file_set = list(root_file_set)

    #now we have a list of unique scans/root-files, for each one we will now run fourphase, and collect the results for each stations
    station_delay_phase_results = { st:SingleStationPhaseDelayOffsets(st) for st in stations }

    #if we are monitoring progress, tick off the scans we have fourphase'd
    if config_obj.use_progress_ticker:
        pbar = Bar('Processing: ', max=len(root_file_set))
    for root in root_file_set:
        scan_processor = VGOSFourphaseSingleScanProcessor( os.path.abspath(root), control_file_path)
        scan_processor.use_progress_ticker = False
        scan_processor.num_proc = num_proc
        scan_processor.set_stations(stations)
        scan_processor.determine_ionosphere()
        scan_processor.calculate_delay_phase_offsets()
        station_delays = scan_processor.get_raw_station_delay_offsets()
        station_phases = scan_processor.get_raw_station_phase_offsets()
        if scan_processor.error_condition == 0:
            for st in stations:
                if st in station_delays and st in station_phases:
                    station_delay_phase_results[st].add_phase_delay_offsets(scan_processor.scan_name, station_delays[st], station_phases[st])
        if config_obj.use_progress_ticker:
            pbar.next()

    if config_obj.use_progress_ticker:
        pbar.finish()

    #cut outliers if the cut factor is non-zero
    if config_obj.sigma_cut_factor != 0.0:
        for st, result in list(station_delay_phase_results.items()):
            result.apply_sigma_cut(config_obj.sigma_cut_factor)

    #look at the amount of data we have for each station
    for stdph in list(station_delay_phase_results.values()):
        fourphase_logger.info("station: " + stdph.station_id + " had " + str(stdph.get_n_total_entries()) + " total data points for determining delay/phase offsets of which " + str(stdph.get_n_cut_entries()) + " were cut"  )

    return station_delay_phase_results

################################################################################
################################################################################
################################################################################

def construct_control_file_lines(station_delay_phase_results):
    """given the a-priori and generated phase/delay offsets, sum and round their values
    to obtain the corrections necessary for the control file output, then output formatted lines """

    cf_lines = ''
    all_stations = sorted( list(set(station_delay_phase_results.keys()) ) )

    micro_to_nano = 1000.0
    for st in all_stations:
        result = station_delay_phase_results[st]
        if result.get_n_total_entries() >= 1 and result.get_n_used_entries() >= 1:
            delay_mean = result.get_delay_offset_mean()*micro_to_nano
            delay_err = result.get_delay_offset_error()*micro_to_nano
            phase_mean = result.get_phase_offset_mean()
            phase_err = result.get_phase_offset_error()
            cf_lines += '\nif station ' + st + '\n'
            cf_lines += '  pc_delay_x ' + ' 0.0' + '\n'
            cf_lines += '  pc_delay_y ' + str(round(delay_mean,3)) + ' * (ns) estimated error is +/- ' + str(round(delay_err,3)) + '\n'
            cf_lines += '  pc_phase_offset_x ' + ' 0.0 \n'
            cf_lines += '  pc_phase_offset_y ' + str(round(phase_mean,1)) + ' * (deg) estimated error is +/- ' + str(round(phase_err,1)) + '\n'
        else:
            cf_lines += '\n* Warning: this control file was generated expecting input for station: ' + st + ' but no good data for pc_delay_x/y or pc_phase_offset_x/y generation was found. \n'

    return cf_lines

################################################################################
################################################################################
################################################################################

def generate_fourphase_control_file(config_obj, output_control_filename):
    """write the new phase/delay offset lines to the final control file"""

    #report data object
    report_data = ExperimentReportData()

    #generate the station phase/delay offsets
    station_delay_phase_results = generate_station_phase_delay_corrections(config_obj, report_data)
    report_data.generated_station_offsets = station_delay_phase_results

    #generate the control file lines
    cf_lines = construct_control_file_lines(station_delay_phase_results)

    #create the new control file
    remove_string_list = ['pc_delay_']
    remove_string_list.append('pc_phase_offset_')
    ffcontrol.remove_lines_and_append_control_file(os.path.abspath(config_obj.control_file), os.path.abspath(output_control_filename), cf_lines, remove_string_list)

    #generate a report file
    date_string = datetime.datetime.now().strftime('%b-%d-%I%M%p-%G')
    report_data.create_report("./fourphase-report-" + date_string + ".json")


################################################################################
################################################################################
################################################################################
