""" some useful utility functions for data processing with fourfit """

#core imports
from __future__ import absolute_import
from future import standard_library
standard_library.install_aliases()
from builtins import filter
from builtins import next
from builtins import str
import os
#set up module level logger
import logging
processing_logger = logging.getLogger(__name__)

#other hops package python libs
import hopstestb as ht
import ffcontrol

try:
    from progress.bar import Bar #non-core import
except:
    from .utility import Bar #local import

from .utility import combined_filter
from .utility import DiscreteQuantityFilter
from .utility import ContinuousQuantityFilter
from . import fringe_file_manipulation as ffm


################################################################################
def load_directory_fringe_files(dir_name, baselines_list, include_autos=False):
    """find and load all fringe files in a directory """
    #find all fringe files
    ff_list = ht.recursive_find_fringe_files(dir_name, include_autos)

    #if we were give a list of baselines, restrict the fringe files to these baselines only
    ff_list2 = []
    if len(baselines_list) != 0:
        for x in ff_list:
            ffbasename = os.path.basename(x)
            bl = ( ffbasename.split('.') )[0]
            if bl in baselines_list:
                ff_list2.append(x)
    else:
        ff_list2 = ff_list #no list, just load them all

    #load fringes into memory
    fringe_objects = []
    for ff_name in ff_list2:
        f_obj = ffm.FringeFileHandle()
        f_obj.load(ff_name)
        if f_obj.is_valid:
            fringe_objects.append(f_obj)

    return fringe_objects

################################################################################
def join_fringes_into_baseline_collection(exp_directory, fringe_object_list, station_list, include_autos=False, required_polprod_list=None, only_complete=True):
    """this function takes individual fringe files (for each scan, baseline, and polarization product)
    and assembles the polarization product files together by baseline for each scan """

    baselines = construct_valid_baseline_list(exp_directory, station_list[0], station_list[1:], network_reference_baselines_only=False, include_autos=include_autos)

    if required_polprod_list == None:
        required_polprod_list = []

    bline_collection_set = set()
    categorized_objs = []
    for obj in fringe_object_list:
        if obj.baseline in baselines and obj not in categorized_objs:
            sflist = ffm.SingleBaselinePolarizationProductCollection()
            sflist.required_polprod_list = required_polprod_list
            sflist.add_fringe_object(obj) #this sets, root_id, scan_name, etc.
            is_present = False
            for sfl in bline_collection_set:
                if sfl == sflist:
                    sfl.add_fringe_object(obj)
                    is_present = True
                    break
            if is_present is False:
                bline_collection_set.add(sflist)
            categorized_objs.append(obj)

    ret_list = []
    if only_complete is True:
        ret_list = [blc for blc in bline_collection_set if blc.is_complete() ]
    else:
        ret_list = list(bline_collection_set)

    for x in ret_list:
        x.init_values()

    return ret_list


################################################################################
def sort_collections_by_baseline(baseline_collection_list):
    """ returns a dict of lists, each dict value contains a list of the collections corresponding to a single baseline """
    bline_set = set()
    for x in baseline_collection_list:
        bl = x.baseline
        bline_set.add(bl)

    sorted_collections = dict()
    for bl in bline_set:
        sorted_collections[bl]= []

    for y in bline_set:
        for x in baseline_collection_list:
            if y == x.baseline:
                sorted_collections[bl].append(x)

    return sorted_collections

################################################################################
def group_baseline_collections_by_scan(bl_collection_list):
    """ group the data of associated with each baseline by scan """
    scan_id_set = set()
    for x in bl_collection_list:
        scan_id_set.add(x.root_id)
    scan_blc = dict()
    for sc_root_id in scan_id_set:
        scan_blc[sc_root_id] = ffm.SingleScanBaselineCollection()
    for blc in bl_collection_list:
        scan_blc[blc.root_id].add_baseline_collection(blc)
    for sc in list(scan_blc.values()):
        sc.init_values()
    return list(scan_blc.values())

################################################################################
def get_max_nblines_in_all_collections(bl_collection_list):
    """ get the maximum number of baselines in any single observation/scan """
    scan_blc = group_baseline_collections_by_scan( bl_collection_list )
    max_n_blines = 0
    for sc in list(scan_blc.values()):
        if max_n_blines < sc.get_n_baseline_collections():
            max_n_blines = sc.get_n_baseline_collections()
    return max_n_blines


################################################################################
def launch_fourfit_processes_in_parallel(full_arg_list, max_num_processes=1, use_progress_ticker=True, log_processes=False):
    """ launch several fourfit processes in parallel to process all the jobs specified by the 'full_arg_list' """
    threads = []
    generated_fringe_files = []
    processed_args_list = []
    arg_list = full_arg_list
    #generate new threads, and monitor until all data is processed
    use_ticker = use_progress_ticker is True and len(arg_list) != 0

    if use_ticker:
        pbar = Bar('Processing: ', max=len(arg_list))
    while threads or arg_list:

        #spawn a new thread if we are not using the max number of processes and there are still files to process
        if (len(threads) < max_num_processes) and (len(arg_list) != 0 ):
            t_args = arg_list.pop()
            t = ht.FourFitThread(target=ht.fourfit_generate_fringe, args=t_args)
            t.setDaemon(True)
            t.start()
            threads.append(t)

        #we have already spawned the max number of process (or there is no data left to run on) so just
        #monitor the running processes and remove them once they are finished
        else:
            for thread in threads:
                if not thread.isAlive():
                    generated_files = thread.get_return_value()
                    processed_args= thread.get_input_args()
                    if log_processes == True:
                        processing_logger.debug( "completed: " + thread.as_string() + " and produced: " + str( thread.get_return_value() ) )
                    generated_fringe_files.extend( generated_files )
                    processed_args_list.append( processed_args )
                    threads.remove(thread)
                    if use_ticker is True:
                        pbar.next()

    if use_ticker is True:
        pbar.finish()
    return generated_fringe_files, processed_args_list

################################################################################
def construct_valid_baseline_list(directory, network_reference_station, remote_stations, network_reference_baselines_only=False, include_autos=False):
    """construct a list of all the valid baselines in this experiment """
    #make sure there are no duplicates in the station lists:
    remote_station_set = set(remote_stations)
    if len(remote_stations) > len(remote_station_set):
        remote_stations = ''.join(remote_station_set)
    if network_reference_station in remote_station_set:
        remote_station_set.remove(network_reference_station)
        remote_stations = ''.join(remote_station_set)

    #determine all possible baselines
    stations = network_reference_station + remote_stations
    all_blist = []
    for s1 in stations:
        for s2 in stations:
            if include_autos is True:
                all_blist.append(s1+s2)
            else:
                if s1 != s2:
                    all_blist.append(s1+s2)

    #just reference station blines
    ref_blist = []
    for s1 in remote_stations:
        if s1 != network_reference_station:
            ref_blist.append(network_reference_station+s1)
            ref_blist.append(s1+network_reference_station)

    if include_autos is True:
        ref_blist.append(network_reference_station+network_reference_station)

    #now locate all of the corel/type-1 files, so we can determine which baselines are valid
    type1_file_list = ht.recursive_find_corel_files(os.path.abspath(directory), include_autos=include_autos)
    unique_blines = set()
    for f in type1_file_list:
        filename_base = os.path.basename(f)
        bl = filename_base.split('.')[0]
        if len(bl) == 2:
            unique_blines.add(bl)

    #strip out non-existent baselines
    baseline_set = set()
    if network_reference_baselines_only is True:
        for bl in ref_blist:
            if bl in unique_blines:
                baseline_set.add(bl)
    else:
        for bl in all_blist:
            if bl in unique_blines:
                baseline_set.add(bl)

    return list(baseline_set)



def apply_fringe_file_cuts(ff_list, control_file_hash, min_snr=0.0, max_snr=1e30, \
                            valid_quality_code_list=None, \
                            start_scan_limit="000-0000", stop_scan_limit="999-9999"):

    """convenience function for a set of commonly used filters on fringe files,
    default values for this function, are such that it essentially only filters on the control file hash """

    if valid_quality_code_list == None:
        valid_quality_code_list=[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

    filter_list = []
    filter_list.append( DiscreteQuantityFilter("control_file_hash", [control_file_hash]) ) #filter on control file hash
    filter_list.append( DiscreteQuantityFilter("quality", valid_quality_code_list) )
    filter_list.append( ContinuousQuantityFilter("scan_name", start_scan_limit, stop_scan_limit) )
    filter_list.append( ContinuousQuantityFilter("snr", min_snr, max_snr) )

    return combined_filter(ff_list, filter_list)


################################################################################

def load_and_batch_fourfit(exp_directory, network_reference_station, remote_stations, \
                           control_file_path, set_commands, \
                           network_reference_baselines_only=True, num_processes=1, \
                           start_scan_limit="000-0000", stop_scan_limit="999-9999", pol_products=None, use_progress_ticker=True, log_fourfit_processes=False):

    """loads any pre-existing fringe files which match the criteria and batch fourfits
    any missing items, then returns a list of the fringe-files"""

    if pol_products == None:
        pol_products=['XX', 'YY', 'XY', 'YX']

    #first check if the stop/start scan limits are the same...if they are, this is a one-off
    #so we don't want to waste time searching/loading the entire experiment directory
    work_dir = os.path.abspath(exp_directory)
    if start_scan_limit == stop_scan_limit:
        if os.path.isdir(os.path.join(work_dir, start_scan_limit)) is True:
            work_dir = os.path.join(work_dir, start_scan_limit)

    #determine all possible (existing) baselines
    baseline_list = construct_valid_baseline_list( work_dir, network_reference_station, remote_stations, network_reference_baselines_only=network_reference_baselines_only, include_autos=False)

    #collect a list of all the root files that are present
    root_file_list = ht.recursive_find_root_files(work_dir, True)

    #now strip out the root files which are outside of the specified time-range
    tmp_root_file_list = []
    for rf in root_file_list:
        rf_scan_name = os.path.abspath(rf).split('/')[-2]
        if start_scan_limit <= rf_scan_name and rf_scan_name <= stop_scan_limit:
            tmp_root_file_list.append(rf)
    else:
        tmp_root_file_list = root_file_list
    root_file_list = tmp_root_file_list

    processing_logger.info("load_and_batch_fourfit: attempting to load cached fringe files from: " + work_dir)

    #now load the fringe files (no-auto-corrs) meta data for this directory
    ff_list = load_directory_fringe_files(work_dir, baseline_list)

    #strip out the ones which do not match the control file used
    #compute the control file hash so we know which fringe files to look for
    control_file_hash = ffcontrol.get_control_file_hash(control_file_path)

    hash_filter = DiscreteQuantityFilter("control_file_hash", [control_file_hash])
    ff_list = list(filter(hash_filter.does_object_pass_filter, ff_list))

    if len(ff_list) != 0:
        processing_logger.info("load_and_batch_fourfit: loaded " + str(len(ff_list)) + " previously generated fringe-files" )
    else:
        processing_logger.info("load_and_batch_fourfit: did not locate any previously generated fringe files" )

    #we need to figure out which scans have already been fringe-fitted
    #with the pol-products and baselines in which we are interested
    root_file_bl_pp_dict = dict()
    for rf in root_file_list:
        root_file_bl_pp_dict[os.path.abspath(rf)] = set()

    for ff in ff_list:
        ff_pp_list = ht.get_file_polarization_product_provisional(ff.filename)
        ff_bl = ff.baseline
        for pp in ff_pp_list:
            if ff.associated_root_file in root_file_bl_pp_dict:
                root_file_bl_pp_dict[ os.path.abspath(ff.associated_root_file) ].add( (ff_bl, pp) ) #add baseline, pol-product tuple

    #now construct a list of arg lists for each process we need to run
    #eliminating tasks corresponding to fringe files which already exist as we go along
    arg_list = []
    for root in tmp_root_file_list:
        for base in baseline_list:
            for pp in pol_products:
                if (base,pp) not in root_file_bl_pp_dict[root]:
                    #construct the expected corel file and check if it is present
                    root_code = ( os.path.basename(root).split('.') )[1]
                    needed_corel_file = os.path.join( os.path.dirname( os.path.abspath(root) ), base + ".." + root_code )
                    if os.path.isfile(needed_corel_file):
                        #now we check to make sure this type of pol-product is actually available in the corel file
                        #this is to avoid problems with mixed-mode
                        pp_present_list = ht.get_polarization_products_present(needed_corel_file)
                        required_pp = ht.get_required_pol_products(pp)
                        if set(required_pp).issubset( set(pp_present_list) ):
                            pol_opt = " -P " + pp + " "
                            arg_list.append( [ pol_opt, base, control_file_path, root, False, set_commands, False ] )

    if len(arg_list) != 0:
        processing_logger.info("load_and_batch_fourfit: will run a total of " + str(len(arg_list)) + " fourfit processes, with up to: " + str(num_processes) + " running simultaneously" )

    #run the fourfit processes
    processed_args_list = []
    generated_fringe_files = []
    [generated_fringe_files, processed_args_list] = launch_fourfit_processes_in_parallel(arg_list, num_processes, use_progress_ticker=use_progress_ticker, log_processes=log_fourfit_processes)

    #now reload the fringe files (no-auto-corrs) meta data for this directory, and filter by control file and time-range
    ff_list = load_directory_fringe_files(work_dir, baseline_list)
    ff_filtered = apply_fringe_file_cuts(ff_list, control_file_hash, start_scan_limit=start_scan_limit, stop_scan_limit=stop_scan_limit)

    return ff_filtered


################################################################################

def load_batch_cut_and_sort(exp_directory, network_reference_station, remote_stations, \
                            control_file_path, set_commands, min_snr=30, max_snr=500, \
                            valid_quality_code_list=None, \
                            network_reference_baselines_only=True, num_processes=1, \
                            start_scan_limit="000-0000", stop_scan_limit="999-9999", only_complete=True, pol_products=None, use_progress_ticker=True, log_fourfit_processes=False):

    """ convenience function to do a load-and-batch fourfit, followed by some filters, then join fringe files associated with a single scan-baseline into collections """

    if valid_quality_code_list == None:
        valid_quality_code_list=[3,4,5,6,7,8,9]

    if pol_products == None:
        pol_products=['XX', 'YY', 'XY', 'YX']

    processing_logger.info("executing load_batch_cut_and_sort" )

    #load all pre-existing fringe files and compute any missing fringe files
    ff_list = load_and_batch_fourfit(exp_directory, network_reference_station, remote_stations, control_file_path, \
                                     set_commands, network_reference_baselines_only, num_processes, \
                                     start_scan_limit, stop_scan_limit, pol_products, use_progress_ticker, log_fourfit_processes)

    #apply cuts to fringe file list
    control_file_hash = ffcontrol.get_control_file_hash(control_file_path)

    ff_list = apply_fringe_file_cuts(ff_list, control_file_hash, min_snr, max_snr, valid_quality_code_list, start_scan_limit, stop_scan_limit)

    processing_logger.info("load_batch_cut_and_sort: collected " + str(len(ff_list)) + " matching fringe files" )

    #collect all fringe files into baseline pol-product collections (if only_complete is true, those which are missing any of the 4 pol-products will be discarded)
    stations = network_reference_station + remote_stations

    bline_collection_list = []
    if only_complete == True:
        bline_collection_list = join_fringes_into_baseline_collection( os.path.abspath(exp_directory), ff_list, stations, include_autos=False, required_polprod_list=pol_products, only_complete=True)
        processing_logger.info("load_batch_cut_and_sort: returning a total of " + str(len(bline_collection_list)) + " complete baseline collections" )
    else:
        bline_collection_list = join_fringes_into_baseline_collection( os.path.abspath(exp_directory), ff_list, stations, include_autos=False, only_complete=False)
        processing_logger.info("load_batch_cut_and_sort: returning a total of " + str(len(bline_collection_list)) + " (possibly incomplete) baseline collections" )


    return bline_collection_list
