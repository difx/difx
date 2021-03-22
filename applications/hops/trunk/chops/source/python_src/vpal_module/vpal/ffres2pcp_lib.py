"""module to determine the appropriate manual p-cal phases offsets for each
station during an experiment w.r.t to a network reference station"""

#core imports
from __future__ import division
from __future__ import absolute_import
from builtins import str
from builtins import range
from past.utils import old_div
from builtins import object
import json
import os
import datetime
#set up module level logger
import logging
ffres2pcp_logger = logging.getLogger(__name__)

#non-core imports
import numpy as np
import scipy.stats

#hops package python libs
import ffcontrol

#local imports
from . import utility
from . import report_lib
from . import processing
from . import fringe_file_manipulation


class DefaultChannelDefines(object):
    """declare fourfit channel codes associated with different observing configurations"""
    def __init__(self):
        self.all = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$%'
        self.vgos = 'abcdefghijklmnopqrstuvwxyzABCDEF'
        self.legacy_s = 'abcdef'
        self.legacy_x = 'ghijklmn'


class Configuration( report_lib.JsonSerializableObject ):
    """configuration object for ffres2pcp"""
    #default values for init
    def __init__(self):
        self.exp_directory = './' #location of experiment data
        self.network_reference_station = '' #station to be used as network phase reference
        self.network_reference_station_pol = 'X' #can be 'X' (vgos) or 'R' (mixed-mode)
        self.frequency_group = 'X' #TODO mixed-mode (S or X)...needs work!
        self.target_stations = '' #list of stations to generate pc_phases for
        self.control_file = '' #initial_control_file
        self.num_proc = 1 #number of processes to run in parallel
        self.verbosity = 0 #verbostiy level
        self.min_snr = 30 #min SNR allowed
        self.max_snr = 500 #max SNR allowed
        self.min_qcode = 3 #minimum quality code required
        self.dtec_tolerance = 1.0
        self.start_scan_limit = '000-0000' #earliest scan to use (DOY-HHMM)
        self.stop_scan_limit = '999-9999' #latest scan to use (DOY-HHMM)
        self.max_number_to_select = 0 #the max number of scans used for each station to compute the pc_phases (zero = unlimited)
        self.dtec_nominal_error = 0.1 #empirically determined factor for search function
        self.sigma_cut_factor = 3.0 #cut phase corrections which are further from the mean than sigma*sigma_cut_factor, use 0.0 to disable
        self.use_progress_ticker = True
        self.nchannel_discard_threshold = 5
        self.channel_discard_tolerance = 15.0

    def export_json(self):
        return self.__dict__

########### Basic process (for a single scan) is to:
#(1) fourfit all of the polarization products for each baseline connected to the network_reference_station (single pol, e.g. X)
#(2) group the individual polarization products for each baseline together into a baseline collection
#(3) form a 'SingleScanBaselineCollection' out of all the baselines-collections for a particular scan
#(4) For each SingleBaselinePolarizationProductCollection, compute the pc_phases phase corrections w.r.t to the appropriate pol of network_reference_station
# as part of this step, we need to make sure the pc_phases corrections (which are applied on a per-station basis) get the correct sign depending on whether
# or not the network_reference_station is the 'reference_station' of the baseline used to extract the pc_phases
#(5) For the network_reference_station (if it has 2 pols), we then also need to get the pc_phases for the secondary pol. w.r.t to the first.
# These pc_phases are computed by closure w/ the other stations, to reduce errors they are averaged (unweighted) together
#(6) Generate the appropriate control file lines

########### For multiple scans:
#(1) Compute the single scan corrections for each station (including the network_reference_station) for each scan/baseline
#(2) Average the resulting corrections together
#(3) Generate the control file lines


################################################################################
################################################################################
################################################################################

class SingleStationPCPhases( report_lib.JsonSerializableObject ):
    """container class for the pc_phases (corrections) of a single polarization of a single station
    the values stored in this class must be corrected for the baseline order of the involved stations,
    such that this station is treated as if it were always a remote station
    the value of scan_baselines_used, keeps track of which scans and baselines were used when
    computing the average values of the pc_phases """

    def __init__(self, station_id, pol):
        self.station_id = station_id #station mk4 id
        self.pol = pol #station polarization
        self.scan_baselines_used = set() #set of scan-baseline tuples inserted into this library of pc_phases
        self.pc_phases = dict() #scan-baseline-tuple indexed dict of pc-phases (which are channel indexed dicts of phase offsets)
        self.mean_pc_phases = dict() #a channel indexed dict of average values of the phase offsets
        self.stddev_pc_phases = dict() #a channel indexed dict of the std. dev. of the pc_phases


    def is_empty(self):
        """determine if this object has any pc phases data or not """
        if len(self.pc_phases) == 0:
            return True
        else:
            return False

    def add_pc_phases(self, scan_name, baseline, pol, pc_phases_dict):
        """ add a set of phases derived from a scan/baseline to the collection """
        scan_base_string = scan_name + "_" + baseline
        if self.pol == pol:
            if scan_base_string not in self.scan_baselines_used:
                self.scan_baselines_used.add( scan_base_string )
                self.pc_phases[scan_base_string] = pc_phases_dict

    def export_json(self):
        """ create a dict to be used as json output """
        json_dict = dict()
        json_dict["station_id"] = self.station_id
        json_dict['pol'] = self.pol
        json_dict['scan_baselines_used'] = list(self.scan_baselines_used)
        json_dict['pc_phases'] = self.pc_phases
        json_dict['mean_pc_phases'] = self.mean_pc_phases
        json_dict['stddev_pc_phases'] = self.stddev_pc_phases
        return json_dict

    def get_channel_n_datapoints(self):
        """ get number of data points associated with each channel """
        n_pts = dict()
        for scan_base_string in self.scan_baselines_used:
            pcph = self.pc_phases[scan_base_string]
            for ch_key, val in list(pcph.items()):
                if ch_key not in n_pts:
                    n_pts[ch_key] = 0
                if not np.isnan(val):
                    n_pts[ch_key] += 1
        return n_pts

    def get_mean_phases(self):
        """ get mean phases of each channel """
        self.compute_mean_phases()
        return self.mean_pc_phases

    def get_stddev_phases(self):
        """ get the std. dev. of the phases of each channel """
        self.compute_mean_phases()
        return self.stddev_pc_phases

    def compute_mean_phases(self):
        """ compute the mean phases of each channel """
        chan_pc_phases = dict()
        for scan_base_string in self.scan_baselines_used:
            pcph = self.pc_phases[scan_base_string]
            for ch_key, val in list(pcph.items()):
                if ch_key not in chan_pc_phases:
                    chan_pc_phases[ch_key] = []
                if not np.isnan(val):
                    chan_pc_phases[ch_key].append(val)
        for ch_key, val_list in list(chan_pc_phases.items()):
            if len(val_list) != 0:
                self.mean_pc_phases[ch_key] = scipy.stats.circmean( np.asarray(val_list), high=180.0, low=-180.0)
                self.stddev_pc_phases[ch_key] = scipy.stats.circstd( np.asarray(val_list), high=180.0, low=-180.0)

    def merge(self, obj):
        """ merge in the data from another object of the same time (assuming it matches the station and pol) """
        if self.station_id == obj.station_id and self.pol == obj.pol:
            #add the set of scan-baseline-pc_phases which are not yet present:
            for key, pc_ph in list(obj.pc_phases.items()):
                sc_bl = key.split("_")
                self.add_pc_phases( sc_bl[0], sc_bl[1], obj.pol, pc_ph)

    def apply_sigma_cut(self, sigma_cut_factor):
        """ we expect each channel to have a mean phase and standard deviation (sigma)
        this function eliminates outliers by iterating over each channel
        and removing all data points which lie beyond the range of sigma_cut_factor*sigma from the channel mean """
        self.compute_mean_phases()
        n_total_removed = 0
        n_total = 0
        for scan_base_string in self.scan_baselines_used:
            pcph = self.pc_phases[scan_base_string]
            chan_entries_to_remove = []
            for ch, val in list(pcph.items()):
                n_total += 1
                angular_diff = abs( utility.minimum_angular_difference(val, self.mean_pc_phases[ch]) )
                if angular_diff > abs( self.stddev_pc_phases[ch]*sigma_cut_factor):
                    chan_entries_to_remove.append(ch)
                    n_total_removed += 1
            for ch in chan_entries_to_remove:
                pcph.pop(ch)
            self.pc_phases[scan_base_string] = pcph
        ffres2pcp_logger.info("sigma cut removed: " + str(n_total_removed) + " outlier values out of a total of " + str(n_total) + " pc_phases estimates over all channels for station: " + self.station_id + ", pol: " + self.pol)
        #recompute the mean phases after removal of outliers
        self.compute_mean_phases()


################################################################################
################################################################################
################################################################################
class ExperimentReportData( report_lib.JsonSerializableObject ):
    """object to gather all station data for a particular experiment """
    def __init__(self):
        self.config_obj = Configuration()
        self.all_blc = []
        self.generated_sspcp = dict()
        self.apriori_sspcp = dict()
        self.channel_freqs = dict()

    def export_json(self):
        """ dump to a dict for json export """
        json_dict = dict()
        json_dict["channel_freqs"] = self.channel_freqs
        json_dict["configuration"] = self.config_obj.export_json()
        generated_sspcp_dict = dict()
        for st_pol_string, obj in list(self.generated_sspcp.items()):
            generated_sspcp_dict[st_pol_string] = obj.export_json()
        json_dict["generated_single_station_pc_phases"] = generated_sspcp_dict
        apriori_sspcp_dict = dict()
        for st_pol_string, obj in list(self.apriori_sspcp.items()):
            apriori_sspcp_dict[st_pol_string] = obj.export_json()
        json_dict["apriori_single_station_pc_phases"] = apriori_sspcp_dict
        blc_json_list = []
        for x in self.all_blc:
            blc_json_list.append(x.export_json())
        json_dict["all_baseline_collections_considered"] = blc_json_list
        return json_dict

    def import_json(self, json_dict):
        """ reconstruct from dict object """
        if isinstance(json_dict, dict):
            self.config_obj.import_json(json_dict["configuration"])
            self.channel_freqs = json_dict["channel_freqs"]
            for key, val in list(json_dict["generated_single_station_pc_phases"].items()):
                station, pol = key.split('_')
                obj = SingleStationPCPhases(station,pol)
                obj.import_json(val)
                self.generated_sspcp[key] = obj
            for key, val in list(json_dict["apriori_single_station_pc_phases"].items()):
                station, pol = key.split('_')
                obj = SingleStationPCPhases(station,pol)
                obj.import_json(val)
                self.apriori_sspcp[key] = obj
            blc_json_list = json_dict["all_baseline_collections_considered"]
            for x in blc_json_list:
                obj = fringe_file_manipulation.SingleBaselinePolarizationProductCollection()
                obj.import_json(x)
                self.all_blc.append(obj)

    def create_report(self, report_filename):
        """dump this object to a json report file"""
        report_file = os.path.abspath(report_filename)
        report = open(report_filename, "w")
        report.write(json.dumps(self,default=report_lib.NestedObjectEncoder, indent=2))
        report.close()

    def load_report(self, report_filename):
        """load data from a report file """
        report_file = os.path.abspath(report_filename)
        with open(report_file, "r") as report:
            data = json.load(report)
            self.import_json(data)



################################################################################
################################################################################
################################################################################

def compute_vgos_network_reference_station_pc_phases(single_baseline_pp_collection_obj, network_reference_station, discard_tolerance=15.0, complete_discard_count=5):
    """ compute the (Y-pol) pc_phases corrections for the station being used as the network reference station """
    sbpp = single_baseline_pp_collection_obj

    #for now, we only use baselines where the network_reference_station comes first (TODO, also use baselines where it appears as a 'remote' station)
    #by default we assume the network_reference_station is the 'X' pol and we are computing corrections for 'Y' pol. 'Y' as reference is not currently supported
    net_ref_pc = SingleStationPCPhases(network_reference_station, 'Y')

    #for now, only use baselines where the network_reference_station is also the 'reference' station (not remote)
    if sbpp.is_complete() and network_reference_station == sbpp.baseline[0]:
        pc_xx = sbpp.get_phase_residuals('XX').phase_corrections
        pc_xy = sbpp.get_phase_residuals('XY').phase_corrections
        pc_yx = sbpp.get_phase_residuals('YX').phase_corrections
        pc_yy = sbpp.get_phase_residuals('YY').phase_corrections


        ch_set = set()
        for ch in list(pc_xx.keys()): ch_set.add(ch)
        for ch in list(pc_xy.keys()): ch_set.add(ch)
        for ch in list(pc_yx.keys()): ch_set.add(ch)
        for ch in list(pc_yy.keys()): ch_set.add(ch)

        ref_pc_phase_y = dict()
        #compute Y-X offsets from (XX - YX) and (XY - YY)
        #result is the average of the two methods, unless the difference exceeds the discard tolerance
        #in which case that channel will not produce any results

        discard_count = 0
        for ch in ch_set:
            if ch in pc_xx and ch in pc_yx and ch in pc_yy and ch in pc_xy:
                res1 = pc_xx[ch] - pc_yx[ch]
                res2 = pc_xy[ch] - pc_yy[ch]
                if abs(res2 - res1) <= discard_tolerance:
                    ref_pc_phase_y[ch] = old_div((res1 + res2),2.0)
                else:
                    discard_count +=1
            elif ch in pc_xx and ch in pc_yx:
                res1 = pc_xx[ch] - pc_yx[ch]
                ref_pc_phase_y[ch] = res1
            elif ch in pc_yy and ch in pc_xy:
                res2 = pc_xy[ch] - pc_yy[ch]
                ref_pc_phase_y[ch] = res2

        #if we have more than a certain number of channels with large phase dis-agreements, we completely ignore this scan
        if discard_count < complete_discard_count:
            net_ref_pc.add_pc_phases(sbpp.scan_name, sbpp.baseline, 'Y', ref_pc_phase_y)

    return net_ref_pc


################################################################################
################################################################################
################################################################################


def generate_station_phase_corrections(SingleBaselinePolarizationProductCollection_list, target_station, \
    network_reference_station, dtec_tolerance=1.0, network_reference_station_pol='X', \
    discard_tolerance=15, nchannel_discard_threshold=5):

    """ function to generate pc_phases for station in an experiment
    from a list of selected scans (gathered into SingleScanBaselineCollection) """

    sbppcl = SingleBaselinePolarizationProductCollection_list
    station_pc_phases = dict() # indexed by (station, pol)
    st = target_station

    if network_reference_station_pol == 'X':
        #vgos mode, so add 'Y' pc_phases for network_reference_station
        stpy = network_reference_station + "_" + "Y"
        station_pc_phases[ stpy ] = SingleStationPCPhases(network_reference_station, 'Y')

    if network_reference_station_pol == 'X':
        #regular VGOS session
        for sbpp in sbppcl:
            scan_name = sbpp.scan_name
            bl = sbpp.baseline

            if st in bl and network_reference_station in bl and st != network_reference_station:
                if sbpp.is_complete() and sbpp.get_dtec_max_deviation() < dtec_tolerance:
                    #first take care of any network reference station pc corrections we can generate from this scan/baseline (only if VGOS mode)
                    if network_reference_station_pol == 'X' and bl[0] == network_reference_station:
                        ref_st_pc = compute_vgos_network_reference_station_pc_phases(sbpp, network_reference_station,)
                        stpy = network_reference_station + "_" + "Y"
                        station_pc_phases[stpy].merge(ref_st_pc)
                    #now take care of the remote station corrections for this scan/baseline
                    pc_x = dict()
                    pc_y = dict()
                    if bl[0] == network_reference_station and bl[1] == st:
                        #normal case
                        pc_x = sbpp.get_phase_residuals(network_reference_station_pol + 'X').phase_corrections #needed for X-pol pc_phases
                        pc_y = sbpp.get_phase_residuals(network_reference_station_pol + 'Y').phase_corrections #needed for Y-pol pc_phases
                    elif bl[1] == network_reference_station and bl[0] == st:
                        #reverse case (network reference station is remote station on this baseline)
                        pc_x = {key:val*-1.0 for key, val in list(sbpp.get_phase_residuals('X' + network_reference_station_pol ).phase_corrections.items())} #needed for X-pol pc_phases, but invert the sign b/c the baseline is flipped
                        pc_y = {key:val*-1.0 for key, val in list(sbpp.get_phase_residuals('Y' + network_reference_station_pol ).phase_corrections.items())} #needed for Y-pol pc_phases, but invert the sign b/c the baseline is flipped
                    if len(pc_x) != 0:
                        stpx = st + "_" + 'X'
                        if stpx not in station_pc_phases:
                            station_pc_phases[stpx] = SingleStationPCPhases(st,'X')
                        station_pc_phases[stpx].add_pc_phases(scan_name, bl, 'X', pc_x)
                    if len(pc_y) != 0:
                        stpy = st + "_" + 'Y'
                        if stpy not in station_pc_phases:
                            station_pc_phases[stpy] = SingleStationPCPhases(st,'Y')
                        station_pc_phases[stpy].add_pc_phases(scan_name, bl, 'Y', pc_y)

    #return a dict of pc_phases correction objects for the target_station + network_reference_station
    #for a vgos experiment, this contains 3 objects
    #for mix-mode, only 2 objects (no-ref station offsets)
    return station_pc_phases

################################################################################
################################################################################
################################################################################


def select_collection_subset(SingleBaselinePolarizationProductCollection_list, target_station, network_reference_station, dtec_nom=0.1, max_number_to_select=0):
    """this function selects single scan baseline pol-product collections for a particular target/reference station based on empirical method
    this assumes cuts in min_snr, dtec deviation, etc. have already been made"""

    #dtec_nom is empircally determined to be 0.1
    #if max_number_to_select is zero, number is unlimited
    sublist = []
    for sbpp in SingleBaselinePolarizationProductCollection_list:
        if target_station in sbpp.baseline and network_reference_station in sbpp.baseline:
            sublist.append(sbpp)

    utility.sort_objects_by_quantity(sublist, "dtec_mdev")
    n_scans_collected = 0
    selected_scans = []

    #if the number to select is not specified, we max it out to all of the scans
    if max_number_to_select == 0:
        max_number_to_select = len(sublist)

    while n_scans_collected < max_number_to_select and len(sublist) > 0:
        #now find the pareto front for this sublist
        ffres2pcp_pareto = utility.compute_2d_pareto_front(sublist, "min_snr", "dtec_mdev", True, False)
        #to do this we will pick the scan that maximizes the value of min_snr/(dtec_mdev + 0.1)
        best_scan = None
        best_score = 0
        for scan in ffres2pcp_pareto:
            score = abs( old_div(scan.min_snr,(scan.dtec_mdev + dtec_nom)) )
            if score > best_score:
                best_score = score
                best_scan = scan
        #collect the best scans to use
        if best_scan != None:
            n_scans_collected += 1
            selected_scans.append(best_scan)
            sublist.remove(best_scan)
        else:
            break

    return selected_scans


################################################################################
################################################################################
################################################################################


#
def generate_pc_phases(config_obj, report_data_obj=None):
    """ main function for ffres2pcp to generate the pc_phase corrections for each station-pol (not including the apriori pc_phases)
    this function does:
    #(1) batch fourfits all of the available scans/baselines
    #(2) combines the generated fringe files into collections
    #(3) applies some selection critera to the collections to generate a list of scan collections for each station
    #(4) extracts the pc_phases for each individual station from the selected scans
    #(5) merges the reference station pc_phases (from all baselines/stations)
    #(6) applies sigma cut to phase correction data
    #(7) returns a dictionary of SingleStationPCPhases indexed by (station,pol) tuples """

    exp_dir = config_obj.exp_directory
    netref_station = config_obj.network_reference_station
    netref_pol = config_obj.network_reference_station_pol
    target_stations = config_obj.target_stations
    control_file_path = os.path.abspath( config_obj.control_file )

    ffres2pcp_logger.info("generating pc_phases for exp_dir: " + exp_dir + " using network reference station: " + netref_station + " for target stations: " + target_stations)
    ffres2pcp_logger.info("initial control file is: " + control_file_path)

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
    netref_baselines_only = True
    only_complete_blc = True
    max_num_scans = config_obj.max_number_to_select
    dtec_nom = config_obj.dtec_nominal_error
    dtec_tolerance = config_obj.dtec_tolerance

    pol_products=['XX', 'YY', 'XY', 'YX'] #default, vgos pol products
    if netref_pol == 'R':
        pol_products=['RX', 'RY', 'XR', 'YR'] #all possible mixed-mode pol products

    log_fourfit_processes=False
    if config_obj.verbosity >=3:
        log_fourfit_processes = True

    blc_list = processing.load_batch_cut_and_sort( \
        exp_dir, netref_station, target_stations, control_file_path, set_commands, min_snr, max_snr, \
        valid_qcodes, netref_baselines_only, num_proc, start, stop, only_complete_blc, pol_products, config_obj.use_progress_ticker, \
        log_fourfit_processes \
    )

    if report_data_obj != None:
        report_data_obj.all_blc = blc_list
        #get the channel/frequency_tuples
        chan_freqs = dict()
        for n in list(range(0, 20)): #this is a crummy work around to make sure we get all the channels available (even if some stations have channels deleted)
            if len(blc_list) > n:
                for m in list(range(0, len(blc_list[n].fringe_objects))):
                    ch_freq_tups = (list(blc_list[n].fringe_objects.values())[m]).get_channel_frequency_tuples()
                    for x in ch_freq_tups:
                        chan_freqs[x[0]] = x[2]
        report_data_obj.channel_freqs = chan_freqs
        chan_def = DefaultChannelDefines()
        if len(report_data_obj.channel_freqs) < len(chan_def.vgos):
            #didn't locate the proper number of channels!
            ffres2pcp_logger.error("Error, did not locate the proper number (32) of channel defintions, only found: " + str( len(report_data_obj.channel_freqs) ) )

    #TODO FIXME: We need to check for dTEC closure when selecting scans to use...if there is a non-closing triangle, when we should eliminate that scan from consideration
    #sblc_list = processing.group_baseline_collections_by_scan(blc_list)
    #for sblc in sblc_list:

    #select useable baseline collections for each station and generate the pc_phases corrections from them, then merge all derived values
    all_station_pc_phases = dict()
    for target in target_stations:
        blc_subset = select_collection_subset(blc_list, target, netref_station, dtec_nom, max_num_scans)
        pc_phases = generate_station_phase_corrections(blc_subset, target, netref_station, dtec_tolerance, netref_pol, config_obj.channel_discard_tolerance, config_obj.nchannel_discard_threshold)
        for key, val in list(pc_phases.items()):
            if key in all_station_pc_phases:
                all_station_pc_phases[key].merge(val)
            else:
                all_station_pc_phases[key] = val

    #apply sigma cut factor if it is non-zero
    if config_obj.sigma_cut_factor != 0.0:
        for pc_phases in list(all_station_pc_phases.values()):
            pc_phases.apply_sigma_cut(config_obj.sigma_cut_factor)

    #look over all station pc_phases object and determine if any are empty, and issue warning/log message
    for stpcp in list(all_station_pc_phases.values()):
        if stpcp.is_empty() is True:
            ffres2pcp_logger.warning("No new pc_phases generated for station: " + stpcp.station_id + ", pol: " + stpcp.pol)

    return all_station_pc_phases

################################################################################
################################################################################
################################################################################

def get_apriori_pc_phases(control_filename, target_stations, network_reference_station, network_reference_station_pol='X', frequency_group='X'):
    """ parse the control file and get the (a-priori) pc_phases which are already present """

    #vgos default: network_reference_station_pol='X' and frequency_group='X'
    #for mixed mode: network_reference_station_pol='R' and frequency_group = 'X' or 'S'
    #returns a dictionary of SingleStationPCPhases indexed by station-pol pairs

    apriori_pcp_list = dict()

    #vgos and s/x channel names
    chan_def = DefaultChannelDefines()
    channels = ''
    if network_reference_station_pol == 'X' and frequency_group == 'X':
        channels = chan_def.vgos
    if network_reference_station_pol == 'R' and frequency_group == 'X':
        channels = chan_def.legacy_x
    if network_reference_station_pol == 'R' and frequency_group == 'S':
        channels = chan_def.legacy_s

    if network_reference_station_pol == 'X' and frequency_group == 'X':
        # extract the apriori pc_phases_x/y for the reference station (only if we are in vgos mode) from the original control file
        bl_string = network_reference_station + '?'
        cblock = ffcontrol.get_control_block(control_filename, bl_string, ' ', frequency_group, 0)
        ref_pc_phase_y = dict()
        ref_pc_phase_x = dict() #nominally the reference station x-pol shouldn't have any non-zero a priori pc_phases, but this can happen (see VGOS memo #51 for reason why)
        for ch in channels:
            i = ffcontrol.get_fcode_index(ch)
            ref_pc_phase_x[ch] = cblock.pc_phase[i][0].ref
            ref_pc_phase_y[ch] = cblock.pc_phase[i][1].ref
        ref_pc_y = SingleStationPCPhases(network_reference_station, 'Y')
        ref_pc_y.add_pc_phases("a-priori", bl_string, 'Y', ref_pc_phase_y)
        ref_pc_x = SingleStationPCPhases(network_reference_station, 'X')
        ref_pc_x.add_pc_phases("a-priori", bl_string, 'X', ref_pc_phase_x)
        refsty = network_reference_station + "_" + "Y"
        refstx = network_reference_station + "_" + "X"
        apriori_pcp_list[refsty] = ref_pc_y
        apriori_pcp_list[refstx] = ref_pc_x

    #now for other stations X/Y pol, extract the pc phases from the control file
    for st in target_stations:
        bl_string = network_reference_station + st
        cblock = ffcontrol.get_control_block(control_filename, bl_string, ' ', 'X', 0)
        rem_pc_phase_x = dict()
        rem_pc_phase_y = dict()
        for ch in channels:
            i = ffcontrol.get_fcode_index(ch)
            rem_pc_phase_x[ch] = cblock.pc_phase[i][0].rem
            rem_pc_phase_y[ch] = cblock.pc_phase[i][1].rem
        rem_pc_x = SingleStationPCPhases(st, 'X')
        rem_pc_x.add_pc_phases("a-priori", bl_string, 'X', rem_pc_phase_x)
        rem_pc_y = SingleStationPCPhases(st, 'Y')
        rem_pc_y.add_pc_phases("a-priori", bl_string, 'Y', rem_pc_phase_y)
        remstx = st + "_" + "X"
        remsty = st + "_" + "Y"
        apriori_pcp_list[remstx] = rem_pc_x
        apriori_pcp_list[remsty] = rem_pc_y

    return apriori_pcp_list


################################################################################
################################################################################
################################################################################
def sum_mean_pc_phases(pcp_obj_a, pcp_obj_b):
    """sums the mean phases of the two SingleStationPCPhases objects and
    returns a channel indexed dictionary of the summed values """

    ma = pcp_obj_a.get_mean_phases()
    mb = pcp_obj_b.get_mean_phases()
    ch_a_set = set( ma.keys() )
    ch_b_set = set( mb.keys() )
    channels = ch_a_set.union(ch_b_set)

    summed_pcp = dict()
    for ch in channels:
        a = 0.0
        b = 0.0
        if ch in ma:
            a = ma[ch]
        if ch in mb:
            b = mb[ch]
        summed_pcp[ch] = a+b

    return summed_pcp


################################################################################
################################################################################
################################################################################
def channel_sort_key(ch_val_pair):
    """sort channel value pairs in channel order"""
    chans = list('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$%')
    #locate index of a and b in all channel list
    a_index = chans.index( ch_val_pair[0] )
    return a_index

################################################################################
################################################################################
################################################################################

def compute_final_pcphases(apriori_pc_phases, generated_pc_phases):
    """given both the a-priori and generated SingleStationPCPhases, sum and round their values
    to obtain the corrections necessary for the control file output"""

    #first determine all of the unique station-pol pairs available
    ap_keys = list(apriori_pc_phases.keys())
    gen_keys = list(generated_pc_phases.keys())
    all_keys = set(ap_keys).union( set(gen_keys) )

    final_pc_values = dict()
    for key in all_keys:
        if key in apriori_pc_phases and key in generated_pc_phases:
            final_pc_values[key] = sum_mean_pc_phases(apriori_pc_phases[key], generated_pc_phases[key])
        elif key in apriori_pc_phases and key not in generated_pc_phases:
            final_pc_values[key] = apriori_pc_phases[key].get_mean_phases()
        elif key not in apriori_pc_phases and key in generated_pc_phases:
            final_pc_values[key] = generated_pc_phases[key].get_mean_phases()

    return all_keys, final_pc_values



################################################################################
################################################################################
################################################################################


def construct_control_file_lines(apriori_pc_phases, generated_pc_phases):
    """given both the a-priori and generated SingleStationPCPhases, sum and round their values
    to obtain the corrections necessary for the control file output, then output formatted lines """

    all_keys, final_pc_values = compute_final_pcphases(apriori_pc_phases, generated_pc_phases)

    #first determine all of the unique station-pol pairs available
    ap_keys = list(apriori_pc_phases.keys())
    gen_keys = list(generated_pc_phases.keys())
    all_keys = set(ap_keys).union( set(gen_keys) )

    all_pols = ['X', 'Y']
    pc_keyword = {'X': '  pc_phases_x ', 'Y': '  pc_phases_y '}

    all_stations = set()
    station_select = dict()
    for key in all_keys:
        st = key[0]
        all_stations.add(st)
        station_select[st] = "if station " + st + "\n"

    cf_lines = ''
    for st in sorted(list(all_stations)):
        station_lines = station_select[st]
        count = 0
        for p in all_pols:
            stpol = st + "_" + p
            if stpol in all_keys:
                count += 1
                pcp_line = pc_keyword[p]
                stpolpc = final_pc_values[stpol]
                ch_val_pair_list = list(stpolpc.items())
                ch_val_pair_list.sort(key=channel_sort_key)
                chans = ''
                pcp_values = ''
                for ch, val in ch_val_pair_list:
                    chans += ch
                    pcp_values += ' ' + str( round(val,1) ) + ' '
                pcp_line +=  chans + ' ' + pcp_values + '\n'
                station_lines += pcp_line
        if count != 0:
            cf_lines += station_lines
            cf_lines += '\n'

    return cf_lines


################################################################################
################################################################################
################################################################################

def generate_ffres2pcp_control_file(config_obj, output_control_filename):
    """create the control file with the necessary pc_phases correction lines """

    #report data object
    report_data = ExperimentReportData()
    report_data.config_obj = config_obj

    #retrieve pre-existing pc_phases from control
    apriori_pcp = get_apriori_pc_phases( os.path.abspath(config_obj.control_file), config_obj.target_stations, config_obj.network_reference_station, config_obj.network_reference_station_pol, config_obj.frequency_group)
    report_data.apriori_sspcp = apriori_pcp

    #generate station pc_phases for this experiment
    all_station_pcp = generate_pc_phases(config_obj, report_data)
    report_data.generated_sspcp = all_station_pcp

    #create the formatted control file lines
    cf_lines = construct_control_file_lines(apriori_pcp, all_station_pcp)

    #remove previous pc_phases lines from control file, and update with new lines
    string_list = ['pc_phases_']
    string_list.append('pc_delay_')
    string_list.append('pc_phase_offset_')
    ffcontrol.remove_lines_and_append_control_file(os.path.abspath(config_obj.control_file), output_control_filename, cf_lines, string_list)

    #generate a report file
    date_string = datetime.datetime.now().strftime('%b-%d-%I%M%p-%G')
    report_data.create_report("./ffres2pcp-report-" + date_string + ".json")
