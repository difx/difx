import datetime
import optparse
import re
import string
import sys
import os
import math
import numpy as np
import shutil
import tempfile
from distutils.dir_util import copy_tree

import matplotlib as mpl
mpl.use('Agg')
import pylab

import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cmx
import matplotlib.gridspec as gridspec


#hops package python libs
import mk4
import afio
import hopstest as ht

#phorc libs
from .fringe_file_handle import *
from .fourphase_lib import *
from .ffres2pcp_lib import *
from .processing import *
from .baseline_fringe_product_list import *
from .mixedmode import *

def process_vgos_exp(exp_directory, reference_station, remote_stations, control_file, num_proc=1, general_verbosity=True, ffres2pcp_verbosity=False, fourphase_verbosity=False):

    #full path to control file
    control_file_path = os.path.abspath(control_file)
    
    #force all fringe files generated to save the control file 
    #information in the type_222 records
    set_commands = "set gen_cf_record true"

    #need these to be user configurable
    snr_thresh = 100
    dtec_thresh = 0.1
    dtec_nom = 0.2
    dtec_extremum_max = 1
    snr_extremum_min = 10
    snr_extremum_max = 500
    valid_quality_code_list = ["3", "4", "5", "6", "7", "8", "9", "10"]
    output_filename = ""

    #collect a list of all the root files that are present
    root_file_list = ht.recursive_find_root_files(exp_directory, True)

    [orig_control_hash, orig_setstring_hash] = get_control_hash(root_file_list[0], control_file_path, set_commands)
    if general_verbosity is True:
        print "Hash of control file: ", control_file, " = ", str(orig_control_hash)

    #determine all possible baselines
    stations = reference_station + remote_stations

    blist = []
    for s1 in stations:
        for s2 in stations:
            if s1 != s2:
                blist.append(s1+s2)

    #TODO: Need to find a way to strip out the baselines which are not valid
    #e.g. get rid of EG, and only use GE. This is ok to leave for now since these
    #non-valid baselines don't produce any output

    #set up for batch processing
    options_list = [" -P XX ", " -P YY ", " -P XY ", " -P YX "]
    baseline_list = blist

    #load the fringe files (no-auto-corrs) meta data for this directory
    ff_list_all = load_directory_fringe_files(exp_directory)
    
    #strip out the ones which do not match the control file used
    ff_list_tmp1 = filter_fringe_files_on_discrete_quantity(ff_list_all, "control_file_hash", [orig_control_hash])

    #cross off the root files which have already been processed: 
    #we assume that if we see any fringe file associated with a root file
    #the whole scan has been processed
    
    tmp_root_file_list = list(root_file_list)

    for ff in ff_list_tmp1:
        for rf in tmp_root_file_list:
            if ff.associated_root_file == rf:
                tmp_root_file_list.remove(rf)
                break

    #batch process all the scans, baselines and pol_prods in parallel with
    #original control file
    if len(tmp_root_file_list) != 0:
        if general_verbosity is True:
            print "Processing: ", len(tmp_root_file_list), " scans with control file: ", control_file
        tmp_list = ht.batch_fourfit_generate_fringe_parallel(options_list, baseline_list, control_file_path, tmp_root_file_list, set_commands, num_proc)
    else:
        if general_verbosity is True:
            print "Reloading", str(len(ff_list_tmp1)), " fringe files previously processed with: ", control_file

    #reload the fringe files (no-auto-corrs) meta data for this directory
    ff_list_all = load_directory_fringe_files(exp_directory)

    #get the experiment name
    exp_name = ff_list_all[0].exp_name

    #strip out the ones which do not match the control file used
    ff_list_tmp1 = filter_fringe_files_on_discrete_quantity(ff_list_all, "control_file_hash", [orig_control_hash])

    if general_verbosity is True:
        print "The total number of fringe files generated with control file: ", control_file, " = ", str(len(ff_list_tmp1))

    #apply data cuts to fringe files: on fringe quality, error codes, SNR range, etc.
    ff_list_tmp2 = filter_fringe_files_on_discrete_quantity(ff_list_tmp1, "quality", valid_quality_code_list)

    if general_verbosity is True:
        print "Remaining number of fringe files after quality code cuts = ", str(len(ff_list_tmp2))

    # ff_list_tmp3 = filter_fringe_files_on_discrete_quantity(ff_list_tmp2, "errcode", [' '])

    # print "number of fringe files after second filter = ", str(len(ff_list_tmp3))

    ff_list_cf1 = filter_fringe_files_on_value_range(ff_list_tmp2, "snr", snr_extremum_min, snr_extremum_max) #extremes in SNR are usually a sign of problems

    if general_verbosity is True:
        print "Remaining number of fringe files after SNR cuts = ", str(len(ff_list_cf1))

    #collect all fringe files into baseline collections (only if complete with 4x pols)
    bline_collection_list_cf1 = join_fringes_into_baseline_collection(ff_list_cf1, stations, include_autos=False, only_complete=True)
    
    if general_verbosity is True:
        print "Total number of complete baseline collections = ", str(len(bline_collection_list_cf1))

    for x in bline_collection_list_cf1:
        x.init_values()

    #split the complete collection by baseline
    split_blc_collection = sort_collections_by_baseline(bline_collection_list_cf1)
    if general_verbosity is True:
        print "Total number of baselines = ", str(len(split_blc_collection))

    #get the baselines we care about, discard sub lists of
    #all baselines which do not connect to the reference station
    bl_list = []
    split_blc_cf1 = []
    for sublist in split_blc_collection:
        bl = sublist[0].baseline
        if reference_station in bl:
            bl_list.append(bl)
            split_blc_cf1.append(sublist)

    #now sort baseline collections by max dtec deviation to find which scans are
    #optimal for each baseline so that we can run the ffres2pcp extraction on it
    for sublist in split_blc_cf1:
        if general_verbosity is True:
            print "Number of scans associated with baseline: ", sublist[0].baseline, " is ", str( len(sublist) )
        sort_objects_by_quantity(sublist, "dtec_mdev")

    #locate optimal scans for ffres2pcp
    bl_scan_list = []
    snr_thresh_list = []
    dtec_thresh_list = []
    for sublist in split_blc_cf1:
        #find the pareto front for this sublist
        sublist_pareto = compute_2d_pareto_front(sublist, "min_snr", "dtec_mdev", True, False)
        if general_verbosity is True:
            print "Baseline: ", sublist[0].baseline, " has ", str(len(sublist_pareto)), " scans in the Pareto front."
        #now we make a some-what ad-hoc choice of the 'best' scan in the pareto front
        #to do this we will pick the scan that maximizes the value of min_snr/(dtec_mdev + 0.1)
        best_scan = None
        best_score = 0
        for scan in sublist_pareto:
            #print "examining (", scan.min_snr, ", ", scan.dtec_mdev, ") ", scan.root_id, " ", scan.scan_name
            score = scan.min_snr/(scan.dtec_mdev + dtec_nom)
            if score > best_score:
                best_score = score
                best_scan = scan
        if best_scan is not None:
            if general_verbosity is True:
                print "Best scan for baseline: ", sublist[0].baseline, " is: ", best_scan.root_id, " : ", best_scan.scan_name, " with (min_snr, dtec_mdev) = (", best_scan.min_snr, ", ", best_scan.dtec_mdev, ") " 
            bl_scan_list.append(best_scan)
            snr_thresh_list.append(best_scan.min_snr + 1)
            dtec_thresh_list.append(best_scan.dtec_mdev + 0.1)
        else:
            print "Error: failed to find any suitable scan for ffres2pcp on ", str(sublist[0].baseline), " baseline"
            sys.exit()
            
        #generate a figure of the scan search space
        [min_snr, dtec_mdev] = collect_object_value_pairs(sublist, "min_snr", "dtec_mdev", sort_items=True)
        [min_snr_p, dtec_mdev_p] = collect_object_value_pairs(sublist_pareto, "min_snr", "dtec_mdev", sort_items=True)
        #add limiting values to bring the exclustion line to the borders
        min_snr_p.insert(0,0.1); dtec_mdev_p.insert(0,min(dtec_mdev_p))
        min_snr_p.append(max(min_snr_p)); dtec_mdev_p.append(1000)

        [best_scan_snr, best_scan_dtec_mdev] = collect_object_value_pairs([best_scan], "min_snr", "dtec_mdev", sort_items=True)
        auto_fig = pylab.figure(figsize=(11,8))
        pylab.xlim(1, 1000)
        pylab.ylim(0.001, 100)
        pylab.plot(min_snr, dtec_mdev, 'bo')
        pylab.plot(min_snr_p, dtec_mdev_p, '.r-')
        pylab.plot(best_scan_snr, best_scan_dtec_mdev, '*g', markersize=12)
        pylab.yscale('log')
        pylab.xscale('log')
        pylab.title('Maximum dTEC deviation vs. Minimum SNR by scan for baseline: ' + sublist[0].baseline )
        pylab.ylabel('Maximum deviation from mean dTEC')
        pylab.xlabel('Minimum SNR')
        auto_fig_name = "./ffres2pcp_scan_search_" + sublist[0].baseline + ".png"
        auto_fig.savefig(auto_fig_name)
        pylab.close(auto_fig)

    if len(bl_scan_list) == 0:
        print "Error could not find any scans suitible for ffres2pcp."
        sys.exit()

    #run ffres2pcp, collect the control file lines to append
    rem_cfl_accum = ""
    ref_cfl_accum = ""
    tmp_max_snr = 0
    for n in range(0, len(bl_scan_list)):
        #determine the remote station
        #TODO FIXME, currently, first character in bl is reference, second is remote, but this is not always true, need to deal with this
        bl = bl_scan_list[n].baseline
        [ref_cfl, rem_cfl, cfname] = compute_ffres2pcp_lines( bl_scan_list[n].associated_root_file, bl[0], bl[1], control_file_path, output_filename, ffres2pcp_verbosity, dtec_thresh_list[n], num_proc)
        rem_cfl_accum += rem_cfl
        #TODO FIXME, overly simplistic assumption, assume GE present, and use GE
        if bl == "GE":
            ref_cfl_accum = ref_cfl
            tmp_max_snr = bl_scan_list[n].get_max_snr()
        # #TODO FIXME, overly simplistic assumption, just use scan with highest snr (on all pol-prod) to generate the reference stations pc_phases_y corrections
        # #probably want to use Arthur's suggestion of a 1/SNR^2 weighted mean
        # if bl_scan_list[n].get_max_snr() > tmp_max_snr:
        #     ref_cfl_accum = ref_cfl
        #     tmp_max_snr = bl_scan_list[n].get_max_snr()

    if ffres2pcp_verbosity is True:
        print "ffres2pcp generated control file lines are: "
        print rem_cfl_accum
        print ref_cfl_accum

    #create the new control file
    ffres2pcp_cfilename = "./cf_" + exp_name + "_" +  reference_station + remote_stations + "_pcphases"
    append_control_file(control_file, ffres2pcp_cfilename, ref_cfl_accum + rem_cfl_accum)
    control_file_path = os.path.abspath(ffres2pcp_cfilename)

    #compute the hash of the control file for later use
    [ffres2pcp_control_hash, ffres2pcp_setstring_hash] = get_control_hash(root_file_list[0], control_file_path, set_commands)
    if general_verbosity:
        print "Hash of ffres2pcp modified control file = ", str(ffres2pcp_control_hash)

    if orig_control_hash == ffres2pcp_control_hash:
        print "Error: ffres2pcp produced control file is the same as original."
        sys.exit()

    #load the fringe files (no-auto-corrs) meta data for this directory
    ff_list_all = load_directory_fringe_files(exp_directory)
    #strip out the ones which do not match the control file used
    ff_list_tmp1 = filter_fringe_files_on_discrete_quantity(ff_list_all, "control_file_hash", [ffres2pcp_control_hash])

    #cross off the root files which have already been processed: 
    #we assume that if we see any fringe file associated with a root file
    #the whole scan has been processed
    tmp_root_file_list = list(root_file_list)
    for ff in ff_list_tmp1:
        for rf in tmp_root_file_list:
            if ff.associated_root_file == rf:
                tmp_root_file_list.remove(rf)
                break

    #TODO Determine if this step is necessary, or if we can just reprocess the handful
    #of scans that are candidates for the fourphase calibration...
    #batch process all of the scans, baselines and pol-prods in parallel
    #with the updated pc_phases control file
    if len(tmp_root_file_list) != 0:
        if general_verbosity is True:
            print "Processing: ", len(tmp_root_file_list), " scans with control file: ", ffres2pcp_cfilename
        tmp_list = ht.batch_fourfit_generate_fringe_parallel(options_list, baseline_list, control_file_path, tmp_root_file_list, set_commands, num_proc)
    else:
        if general_verbosity is True:
            print "Reloading", str(len(ff_list_tmp1)), " fringe files previously processed with: ", ffres2pcp_cfilename

    #get list of all the fringe files which match the updated pc_phases control file
    #again load the fringe files (no-auto-corrs) meta data for this directory
    ff_list_all = load_directory_fringe_files(exp_directory)
    #strip out the ones which do not match the ffres2pcp control file used
    ff_list_tmp1 = filter_fringe_files_on_discrete_quantity(ff_list_all, "control_file_hash", [ffres2pcp_control_hash])
    if general_verbosity is True:
        print "Total number of fringe files generated from ffres2pcp modified control file is: ", str(len(ff_list_tmp1))

    #apply data cuts to fringe files: on fringe quality, error codes, SNR range, etc.
    ff_list_tmp2 = filter_fringe_files_on_discrete_quantity(ff_list_tmp1, "quality", valid_quality_code_list)
    
    if general_verbosity is True:
        print "Remaining number of fringe files after quality code cuts = ", str(len(ff_list_tmp2))

    ff_list_cf2 = filter_fringe_files_on_value_range(ff_list_tmp2, "snr", snr_extremum_min, snr_extremum_max) #extremes in SNR are usually a sign of problems

    if general_verbosity is True:
        print "Remaining number of fringe files after SNR cuts = ", str(len(ff_list_cf2))

    #collect all fringe files into baseline collections (only if complete with 4x pols)
    bline_collection_list_cf2 = join_fringes_into_baseline_collection(ff_list_cf2, stations, include_autos=False, only_complete=True)
    if general_verbosity:
        print "Total number of complete baseline collections after ffres2pcp reprocessing = ", str(len(bline_collection_list_cf2))

    # for blc in bline_collection_list_cf2:
    #     print "have blc from scan ", blc.scan_name, " and baseline: ", blc.baseline

    #find optimal scan from baseline collections to use to run fourphase_mod on,
    #this scan needs to to contain all of the stations in the experiment
    #and we would like it to have the maximal SNR and minimal dtec deviation on all baselines

    #first create a set of scan ids, count how many baselines are associated with each
    scan_id_set = set()
    for x in bline_collection_list_cf2:
        scan_id_set.add(x.root_id)

    scan_id_list = []
    for y in scan_id_set:
        scan_id_list.append(y)

    # print "scan set = ", scan_id_list

    list_of_scan_instances = [0]*len(scan_id_list)
    for n in range(0, len( scan_id_list ) ):
        for x in bline_collection_list_cf2:
            if x.root_id == scan_id_list[n]:
                list_of_scan_instances[n] += 1

    max_n_blines = max(list_of_scan_instances)

    if general_verbosity is True:
        print "Maximum number of baselines in a single scan = ", str(max_n_blines)

    # #use only those scans which have the maximal number of baselines
    # for n in range(0, len(list_of_scan_instances)):
    #     if list_of_scan_instances[n] != max_n_blines:
    #         print "n blines associated with this scan = ", list_of_scan_instances[n]
    #         print "removing scan ", scan_id_list[n]
    #         scan_id_list.remove(scan_id_list[n])

    #collect all baselines associated with a scan
    scan_list = []
    for n in range(0, len(scan_id_list) ):
        scan_list.append( scan_baseline_list() )
        scan_list[n].scan_name = scan_id_list[n]
        for x in bline_collection_list_cf2:
            if x.root_id == scan_id_list[n]:
                # print "adding a blc of scan ", x.scan_name, "bl: ", x.baseline, " to collection: ", scan_id_list[n]
                scan_list[n].add_baseline_collection(x)

    # print "scan list = "
    # for n in range(0,len(scan_list)):
    #     print "n = ", n
    #     print scan_list[n].scan_name, " has : ", str(len(scan_list[n].baseline_list)), " baselines"

    fourphase_scan_list = []
    for n in range(0,len(scan_list)):
        if len(scan_list[n].baseline_list) == max_n_blines:
            scan_list[n].init_values()
            fourphase_scan_list.append(scan_list[n])
        #     print "including scan: ", scan_list[n].scan_name, " w/ root id: ", scan_list[n].root_id
        # else:
        #     print "not including scan: ", scan_list[n].scan_name, " w/ root id: ", scan_list[n].root_id

    #sort them on the mean dtec_mdev over all baselines and pols
    sort_objects_by_quantity(fourphase_scan_list, "mean_dtec_mdev")

    if general_verbosity is True:
        print "Total number of fourphase candidate scans = ", str(len(fourphase_scan_list))
        for x in fourphase_scan_list:
            print "Scan in fourphase list: ", x.scan_name, " root id: ", x.root_id

    fit_status_bad = True
    while fit_status_bad is True and len(fourphase_scan_list) != 0:
        #take the best one which also satisfies dtec constraint
        fourphase_scan = fourphase_scan_list[0]
        found_scan = False
        snr_lim = snr_thresh
        dtec_lim = dtec_thresh

        #find the pareto front for this list of scans
        fourphase_pareto = compute_2d_pareto_front(fourphase_scan_list, "min_snr", "mean_dtec_mdev", True, False)
        if general_verbosity:
            print "Number of scans in fourphase pareto front = ", str(len(fourphase_pareto))
        fourphase_scan = None
        best_score = 0
        for scan in fourphase_pareto:
            # print "examining for fourphase (", scan.min_snr, ", ", scan.mean_dtec_mdev, ") ", scan.root_id, " ", scan.scan_name
            score = scan.min_snr/(scan.mean_dtec_mdev + dtec_nom)
            if score > best_score:
                best_score = score
                fourphase_scan = scan
                snr_lim = scan.min_snr
                dtec_lim = scan.mean_dtec_mdev
        if fourphase_scan is None:
            print "Error: failed to find suitable scan for fourphase in Pareto front."
            sys.exit()
        else:
            if general_verbosity is True:
                print "Using scan: ", best_scan.root_id, " : ", best_scan.scan_name, " with (min_snr, dtec_mdev) = (", best_scan.min_snr, ", ", best_scan.dtec_mdev, ") for fourphase." 
            
        #generate a figure of the scan search space
        [min_snr, dtec_mdev] = collect_object_value_pairs(fourphase_scan_list, "min_snr", "mean_dtec_mdev", sort_items=True)
        [min_snr_p, dtec_mdev_p] = collect_object_value_pairs(fourphase_pareto, "min_snr", "mean_dtec_mdev", sort_items=True)
        #add limiting values to bring the exclustion line to the borders
        min_snr_p.insert(0,0.1); dtec_mdev_p.insert(0,min(dtec_mdev_p))
        min_snr_p.append(max(min_snr_p)); dtec_mdev_p.append(1000)
        [best_scan_snr, best_scan_dtec_mdev] = collect_object_value_pairs([fourphase_scan], "min_snr", "mean_dtec_mdev", sort_items=True)
        auto_fig = pylab.figure(figsize=(11,8))
        pylab.xlim(1, 1000)
        pylab.ylim(0.001, 100)
        pylab.plot(min_snr, dtec_mdev, 'bo')
        pylab.plot(min_snr_p, dtec_mdev_p, '.r-')
        pylab.plot(best_scan_snr, best_scan_dtec_mdev, '*g', markersize=12)
        pylab.yscale('log')
        pylab.xscale('log')
        pylab.title('Mean of maximum dTEC deviation vs. Minimum SNR by scan')
        pylab.ylabel('Mean (over all baselines) of maximum dTEC deviation')
        pylab.xlabel('Minimum SNR')
        auto_fig_name = "./fourphase_scan_search_" + exp_name + "_" + stations +".png"
        auto_fig.savefig(auto_fig_name)
        pylab.close(auto_fig)

        #run fourphase on this scan
        fourphase_cfilename = "./cf_" + exp_name + "_" + fourphase_scan.scan_name  + "_" + stations + "I" 
        fourphase_cfilename = os.path.abspath(fourphase_cfilename)
        fit_status_bad = compute_fourphase(fourphase_scan.associated_root_file, stations, control_file_path, fourphase_cfilename, fourphase_verbosity, ion_original=False, num_processes=num_proc)
        
        if fit_status_bad is True:
            if general_verbosity:
                print "Fourphase fit failed for scan: ", fourphase_scan.scan_name, " trying next scan."
            #remove the scan, since it failed the fourphase checks
            fourphase_scan_list.remove(fourphase_scan)
            #remove the generated control file and plot
            os.remove(fourphase_cfilename)
            os.remove(auto_fig_name)

    #end of while loop
    
    if fit_status_bad is True:
        print "Error: failed to find suitable scan for fourphase."
        sys.exit()

    #batch process all of the scans, baselines using the combined pol -IXY option
    #in parallel using the fourphase generated control file

    #set up for batch processing
    # options_list = [" -P I "]
    #batch process all the scans, baselines and pol_prods in parallel with
    #original control file
    # tmp_list = ht.batch_fourfit_generate_fringe_parallel(options_list, baseline_list, fourphase_cfilename, root_file_list, set_commands, num_proc)

    #collect all of the -IXY fringe files generated with the final control file

    #generate an alist from these fringe files


################################################################################
################################################################################
################################################################################
################################################################################




















































def examine_vgos_phase_corrections(exp_directory, reference_station, remote_stations, control_file, num_proc=1, general_verbosity=True, ffres2pcp_verbosity=False):

    #full path to control file
    control_file_path = os.path.abspath(control_file)
    
    #force all fringe files generated to save the control file 
    #information in the type_222 records
    set_commands = "set gen_cf_record true"

    #need these to be user configurable
    snr_thresh = 100
    dtec_thresh = 0.1
    dtec_nom = 0.2
    dtec_extremum_max = 1
    snr_extremum_min = 10
    snr_extremum_max = 500
    valid_quality_code_list = ["3", "4", "5", "6", "7", "8", "9", "10"]
    output_filename = ""

    #collect a list of all the root files that are present
    root_file_list = ht.recursive_find_root_files(exp_directory, True)
    
    #figure out the experiment #
    exp_dir = os.path.dirname( os.path.dirname(  os.path.abspath(root_file_list[0])  ) )
    exp_name = os.path.split(exp_dir)[1]

    [orig_control_hash, orig_setstring_hash] = get_control_hash(root_file_list[0], control_file_path, set_commands)
    if general_verbosity is True:
        print "Hash of control file: ", control_file, " = ", str(orig_control_hash)

    #determine all possible baselines
    stations = reference_station + remote_stations

    blist = []
    for s1 in stations:
        for s2 in stations:
            if s1 != s2:
                blist.append(s1+s2)

    #TODO: Need to find a way to strip out the baselines which are not valid
    #e.g. get rid of EG, and only use GE. This is ok to leave for now since these
    #non-valid baselines don't produce any output

    #set up for batch processing
    options_list = [" -P XX ", " -P YY ", " -P XY ", " -P YX "]
    baseline_list = blist

    #load the fringe files (no-auto-corrs) meta data for this directory
    ff_list_all = load_directory_fringe_files(exp_directory)
    
    #strip out the ones which do not match the control file used
    ff_list_tmp1 = filter_fringe_files_on_discrete_quantity(ff_list_all, "control_file_hash", [orig_control_hash])

    #cross off the root files which have already been processed: 
    #we assume that if we see any fringe file associated with a root file
    #the whole scan has been processed
    
    tmp_root_file_list = list(root_file_list)

    for ff in ff_list_tmp1:
        for rf in tmp_root_file_list:
            if ff.associated_root_file == rf:
                tmp_root_file_list.remove(rf)
                break

    #batch process all the scans, baselines and pol_prods in parallel with
    #original control file
    if len(tmp_root_file_list) != 0:
        if general_verbosity is True:
            print "Processing: ", len(tmp_root_file_list), " scans with control file: ", control_file
        tmp_list = ht.batch_fourfit_generate_fringe_parallel(options_list, baseline_list, control_file_path, tmp_root_file_list, set_commands, num_proc)
    else:
        if general_verbosity is True:
            print "Reloading", str(len(ff_list_tmp1)), " fringe files previously processed with: ", control_file

    #reload the fringe files (no-auto-corrs) meta data for this directory
    ff_list_all = load_directory_fringe_files(exp_directory)

    #get the experiment name
    exp_name = ff_list_all[0].exp_name

    #strip out the ones which do not match the control file used
    ff_list_tmp1 = filter_fringe_files_on_discrete_quantity(ff_list_all, "control_file_hash", [orig_control_hash])

    if general_verbosity is True:
        print "The total number of fringe files generated with control file: ", control_file, " = ", str(len(ff_list_tmp1))

    #apply data cuts to fringe files: on fringe quality, error codes, SNR range, etc.
    ff_list_tmp2 = filter_fringe_files_on_discrete_quantity(ff_list_tmp1, "quality", valid_quality_code_list)

    if general_verbosity is True:
        print "Remaining number of fringe files after quality code cuts = ", str(len(ff_list_tmp2))

    # ff_list_tmp3 = filter_fringe_files_on_discrete_quantity(ff_list_tmp2, "errcode", [' '])

    # print "number of fringe files after second filter = ", str(len(ff_list_tmp3))

    ff_list_cf1 = filter_fringe_files_on_value_range(ff_list_tmp2, "snr", snr_extremum_min, snr_extremum_max) #extremes in SNR are usually a sign of problems

    if general_verbosity is True:
        print "Remaining number of fringe files after SNR cuts = ", str(len(ff_list_cf1))

    #collect all fringe files into baseline collections (only if complete with 4x pols)
    bline_collection_list_cf1 = join_fringes_into_baseline_collection(ff_list_cf1, stations, include_autos=False, only_complete=True)
    
    if general_verbosity is True:
        print "Total number of complete baseline collections = ", str(len(bline_collection_list_cf1))

    for x in bline_collection_list_cf1:
        x.init_values()

    #split the complete collection by baseline
    split_blc_collection = sort_collections_by_baseline(bline_collection_list_cf1)
    if general_verbosity is True:
        print "Total number of baselines = ", str(len(split_blc_collection))

    #get the baselines we care about, discard sub lists of
    #all baselines which do not connect to the reference station
    bl_list = []
    split_blc_cf1 = []
    for sublist in split_blc_collection:
        bl = sublist[0].baseline
        if reference_station in bl:
            bl_list.append(bl)
            split_blc_cf1.append(sublist)

    #now sort baseline collections by max dtec deviation to find which scans are
    #optimal for each baseline so that we can run the ffres2pcp extraction on it
    for sublist in split_blc_cf1:
        if general_verbosity is True:
            print "Number of scans associated with baseline: ", sublist[0].baseline, " is ", str( len(sublist) )
        sort_objects_by_quantity(sublist, "dtec_mdev")

    #first locate the optimal scans for ffres2pcp, to serve as our reference implementation
    bl_scan_list = []
    snr_thresh_list = []
    dtec_thresh_list = []
    for sublist in split_blc_cf1:
        #find the pareto front for this sublist
        sublist_pareto = compute_2d_pareto_front(sublist, "min_snr", "dtec_mdev", True, False)
        if general_verbosity is True:
            print "Baseline: ", sublist[0].baseline, " has ", str(len(sublist_pareto)), " scans in the Pareto front."
        #now we make a some-what ad-hoc choice of the 'best' scan in the pareto front
        #to do this we will pick the scan that maximizes the value of min_snr/(dtec_mdev + 0.1)
        best_scan = None
        best_score = 0
        for scan in sublist_pareto:
            #print "examining (", scan.min_snr, ", ", scan.dtec_mdev, ") ", scan.root_id, " ", scan.scan_name
            score = scan.min_snr/(scan.dtec_mdev + dtec_nom)
            if score > best_score:
                best_score = score
                best_scan = scan
        if best_scan is not None:
            if general_verbosity is True:
                print "Best scan for baseline: ", sublist[0].baseline, " is: ", best_scan.root_id, " : ", best_scan.scan_name, " with (min_snr, dtec_mdev) = (", best_scan.min_snr, ", ", best_scan.dtec_mdev, ") " 
            bl_scan_list.append(best_scan)
            snr_thresh_list.append(best_scan.min_snr + 1)
            dtec_thresh_list.append(best_scan.dtec_mdev + 0.1)
        else:
            print "Error: failed to find any suitable scan for ffres2pcp on ", str(sublist[0].baseline), " baseline"
            sys.exit()
            
        #generate a figure of the scan search space
        [min_snr, dtec_mdev] = collect_object_value_pairs(sublist, "min_snr", "dtec_mdev", sort_items=True)
        [min_snr_p, dtec_mdev_p] = collect_object_value_pairs(sublist_pareto, "min_snr", "dtec_mdev", sort_items=True)
        #add limiting values to bring the exclustion line to the borders
        min_snr_p.insert(0,0.1); dtec_mdev_p.insert(0,min(dtec_mdev_p))
        min_snr_p.append(max(min_snr_p)); dtec_mdev_p.append(1000)

        [best_scan_snr, best_scan_dtec_mdev] = collect_object_value_pairs([best_scan], "min_snr", "dtec_mdev", sort_items=True)

    if len(bl_scan_list) == 0:
        print "Error could not find any scans suitible for ffres2pcp."
        sys.exit()

    #run ffres2pcp on the optimal scans
    opt_ref_corr = []
    opt_rem_corr = []
    
    for n in range(0, len(bl_scan_list)):
        #determine the remote station
        #TODO FIXME, currently, first character in bl is reference, second is remote, but this is not always true, need to deal with this
        bl = bl_scan_list[n].baseline
        [ref_corr, rem_corr] =  compute_ffres2pcp_phase_correction_objects( bl_scan_list[n].associated_root_file, bl[0], bl[1], control_file_path, output_filename, ffres2pcp_verbosity, dtec_thresh_list[n], num_proc)
        opt_ref_corr.append(ref_corr)
        for x in rem_corr:
            x.scan_name = bl_scan_list[n].scan_name
            opt_rem_corr.append(x)

    #now for each baseline we want to extract the phase corrections
    #so we collect a set of candidate scans using relaxed thresholds 
    #and compare them to the optimal selection
    candidate_snr_min = 60
    candidate_snr_max = 200
    candidate_dtec_mdev_threshold = 0.8
    unopt_ref_corr = []
    unopt_rem_corr = []    
    for i in range(0, len(split_blc_cf1)):
        sublist = split_blc_cf1[i]
        for scan in sublist:
            bl = scan.baseline
            if scan.min_snr > candidate_snr_min and scan.dtec_mdev < candidate_dtec_mdev_threshold:
                [ref_corr, rem_corr] = compute_ffres2pcp_phase_correction_objects( scan.associated_root_file, bl[0], bl[1], control_file_path, output_filename, ffres2pcp_verbosity, candidate_dtec_mdev_threshold, num_proc)
                ref_corr.scan_name = scan.scan_name
                unopt_ref_corr.append(ref_corr)
                for x in rem_corr:
                    x.scan_name = scan.scan_name
                    unopt_rem_corr.append(x)

    print "number of unopt scans = ", str(len(unopt_rem_corr))
    
    deg_ticks = [0]*19
    for i in range(0,19):
        deg_ticks[i] = -90 + i*10
    
    chan_indexes = list(range(0, len(opt_ref_corr[0].phase_corrections) ) )
    #figure out the channel frequencies (we are assuming that these are the same for all scans/baselines, OK for vgos)
    print "first filename = ", ff_list_all[0].filename
    chan_info_tuples = get_channel_frequency_tuples(ff_list_all[0].filename)
    chan_freqs = []
    hz_to_ghz = 1e-9
    for n in range(0,len(chan_info_tuples)):
        chan_freqs.append(chan_info_tuples[n][2]*hz_to_ghz)

    chan_lables = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F' ]

    #this is simplistic (since its a cyclic variable), but probably ok as long as the phase variance isn't large
    station_phase_variance_by_channel_x = []
    station_scan_count = []
    for m in range(0,len(stations)):
        st = stations[m]
        station_phase_variance_by_channel_x.append([0]*len(chan_indexes))
        station_scan_count.append(0)
        for opt_corr in opt_rem_corr:
            if opt_corr.rem_station == st and opt_corr.ref_station == reference_station and opt_corr.rem_station_pol == "X":
                for unopt_corr in unopt_rem_corr:
                    if unopt_corr.rem_station == st and unopt_corr.ref_station == reference_station and unopt_corr.rem_station_pol == "X":
                        diff2 = []
                        for n in range(0, len(unopt_corr.phase_corrections)):
                            delta = opt_corr.phase_corrections[n] - unopt_corr.phase_corrections[n]
                            diff2.append(delta*delta)
                        for y in range(0, len(station_phase_variance_by_channel_x[m])):
                            station_phase_variance_by_channel_x[m][y] += diff2[y]
                        station_scan_count[m] += 1
        for y in range(0, len(station_phase_variance_by_channel_x[m])):
            if (station_scan_count[m]-1 == 0):
                print "Warning: cannot compute variance with only one scan"
                station_phase_variance_by_channel_x[m][y] = 1.0
            else:
                station_phase_variance_by_channel_x[m][y] = math.sqrt( (1.0/(station_scan_count[m]-1.0))*station_phase_variance_by_channel_x[m][y]) 
            
    for a in range(0, len(station_phase_variance_by_channel_x) ):
        print "station: ", stations[a], " x pol channel phase variance" 
        print station_phase_variance_by_channel_x[a]
        
    station_phase_variance_by_channel_y = []
    station_scan_count = []
    for m in range(0,len(stations)):
        st = stations[m]
        station_phase_variance_by_channel_y.append([0]*len(chan_indexes))
        station_scan_count.append(0)
        for opt_corr in opt_rem_corr:
            if opt_corr.rem_station == st and opt_corr.ref_station == reference_station and opt_corr.rem_station_pol == "Y":
                for unopt_corr in unopt_rem_corr:
                    if unopt_corr.rem_station == st and unopt_corr.ref_station == reference_station and unopt_corr.rem_station_pol == "Y":
                        diff2 = []
                        for n in range(0, len(unopt_corr.phase_corrections)):
                            delta = opt_corr.phase_corrections[n] - unopt_corr.phase_corrections[n]
                            diff2.append(delta*delta)
                        for y in range(0, len(station_phase_variance_by_channel_y[m])):
                            station_phase_variance_by_channel_y[m][y] += diff2[y]
                        station_scan_count[m] += 1
        for y in range(0, len(station_phase_variance_by_channel_y[m])):
            if (station_scan_count[m]-1 == 0):
                print "Warning: cannot compute variance with only one scan"
                station_phase_variance_by_channel_y[m][y] = 1.0
            else:
                station_phase_variance_by_channel_y[m][y] = math.sqrt( (1.0/(station_scan_count[m]-1.0))*station_phase_variance_by_channel_y[m][y]) 
            
    for a in range(0, len(station_phase_variance_by_channel_y) ):
        print "station: ", stations[a], " y pol channel phase variance" 
        print station_phase_variance_by_channel_y[a]
    
    plt.xticks(np.arange(min(chan_indexes), max(chan_indexes), 1.0))
    colmap = cmx.get_cmap(name="jet") 
    cNorm  = colors.Normalize(vmin=candidate_snr_min, vmax=candidate_snr_max)
    scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=colmap)
    print scalarMap.get_clim()
    
    coltickvals = []
    colticks = []
    snr_tickstep = 20
    for i in range(0, (candidate_snr_max-candidate_snr_min)/snr_tickstep):
        coltickvals.append(candidate_snr_min + i*snr_tickstep)
        colticks.append(str(candidate_snr_min + i*snr_tickstep))

    freq_buffer = 0.01
    
    #use contourf to generate a color bar gradient
    Z = [[0,0],[0,0]]
    levels = range(80,200+1,1)
    collevel = plt.contourf(Z, levels, cmap=colmap)
    plt.clf()


################################################################################
#X POL

    for m in range(0,len(stations)):
        st = stations[m]
        for opt_corr in opt_rem_corr:
            if opt_corr.rem_station == st and opt_corr.ref_station == reference_station and opt_corr.rem_station_pol == "X":
                opt_corr_phases = opt_corr
                auto_fig = plt.figure(figsize=(14,8))
                bigax = auto_fig.add_subplot(111)
                bigax.spines['top'].set_color('none')
                bigax.spines['bottom'].set_color('none')
                bigax.spines['left'].set_color('none')
                bigax.spines['right'].set_color('none')
                bigax.tick_params(top='off', bottom='off', left='on', right='off')
                bigax.set_ylabel('Phase Correction (deg)')
                bigax.set_xlabel('Channel Frequency (GHz)', labelpad=20)
                plt.title('Estimated X-pol phase corrections for station (' + st + ') by channel for exp #' + exp_name, y=1.11 )
                bigax.set_yticks(deg_ticks)
                bigax.set_xticks([])
                bigax.xaxis.set_ticks_position('none')
                bigax2 = bigax.twiny()
                bigax2.set_xlim(bigax.get_xlim())
                bigax2.set_xlabel('Channel Index', labelpad=20)
                bigax2.set_xticklabels([])
                bigax2.spines['top'].set_color('none')
                bigax2.spines['bottom'].set_color('none')
                bigax2.spines['left'].set_color('none')
                bigax2.spines['right'].set_color('none')
                print "------------------------- looking at X-pol station: ", st, " --------------------------"
                for unopt_corr in unopt_rem_corr:
                    if unopt_corr.rem_station == st and unopt_corr.ref_station == reference_station and unopt_corr.rem_station_pol == "X":
                        colorVal = scalarMap.to_rgba(unopt_corr.snr)
                        chi2 = calc_chi_squared(opt_corr_phases.phase_corrections, unopt_corr.phase_corrections, station_phase_variance_by_channel_x[m])
                        print unopt_corr.scan_name, " & ", str( round(chi2/len(chan_indexes),2) ) , " & ", str(round(unopt_corr.snr,1)), " & ", str(round(unopt_corr.dtec,1))
                        for band in [1,2,3,4]:
                            ax = auto_fig.add_subplot(1,4,band)
                            ax.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                            ax.set_ylim(-90, 90)
                            ax.tick_params(top='off', bottom='on', left='on', right='off')
                            ax.plot(chan_freqs[(band-1)*8:band*8], unopt_corr.phase_corrections[(band-1)*8:band*8], '-', color=colorVal)
                            ax.grid(True)
                            ax.set_yticklabels([])
                colorVal = scalarMap.to_rgba(opt_corr.snr)
                for band in [1,2,3,4]:
                    ax = auto_fig.add_subplot(1,4,band)
                    ax.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                    ax.set_ylim(-90, 90)
                    ax.tick_params(top='off', bottom='on', left='on', right='off')
                    ax.plot(chan_freqs[(band-1)*8:band*8], opt_corr_phases.phase_corrections[(band-1)*8:band*8], 'ro')
                    #ax.errorbar(chan_freqs[(band-1)*8:band*8], opt_corr_phases.phase_corrections[(band-1)*8:band*8],  fmt='ro', yerr=station_phase_variance_by_channel_x[m][(band-1)*8:band*8])
                    ax.grid(True)
                    ax.set_yticklabels([])
                    ax2 = ax.twiny()
                    ax2.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                    ax2.set_xticks(chan_freqs[(band-1)*8:band*8])
                    ax2.set_xticklabels(chan_lables[(band-1)*8:band*8])
                print "reference scan = ", opt_corr_phases.scan_name
                cbax = auto_fig.add_subplot(111)
                cbar_position=auto_fig.add_axes([0.91,0.1,0.02,0.78])
                cbar = plt.colorbar(collevel, cmap=colmap, ticks=coltickvals, cax=cbar_position)
                cbar.ax.set_yticklabels(colticks)
                cbar.ax.set_ylabel('Scan SNR')
                auto_fig_name = "./phase_variation_" + st +"_X_" + exp_name + ".png"
                plt.subplots_adjust(left=0.08, bottom=0.1, right=0.9, top=0.88, wspace=0.05, hspace=0.05)
                auto_fig.savefig(auto_fig_name)
                plt.close(auto_fig)

################################################################################
#Y POL

    for m in range(0,len(stations)):
        st = stations[m]
        for opt_corr in opt_rem_corr:
            if opt_corr.rem_station == st and opt_corr.ref_station == reference_station and opt_corr.rem_station_pol == "Y":
                opt_corr_phases = opt_corr
                auto_fig = plt.figure(figsize=(14,8))
                plt.xlim(chan_freqs[0], chan_freqs[31])
                plt.ylim(-90, 90)
                bigax = auto_fig.add_subplot(111)
                bigax.spines['top'].set_color('none')
                bigax.spines['bottom'].set_color('none')
                bigax.spines['left'].set_color('none')
                bigax.spines['right'].set_color('none')
                bigax.tick_params(top='off', bottom='off', left='on', right='off')
                bigax.set_ylabel('Phase Correction (deg)')
                bigax.set_xlabel('Channel Frequency (GHz)', labelpad=20)
                plt.title('Estimated Y-pol phase corrections for station (' + st + ') by channel for exp #' + exp_name, y=1.11 )
                bigax.set_yticks(deg_ticks)
                bigax.set_xticks([])
                bigax.xaxis.set_ticks_position('none')
                bigax2 = bigax.twiny()
                bigax2.set_xlim(bigax.get_xlim())
                bigax2.set_xlabel('Channel Index', labelpad=20)
                bigax2.set_xticklabels([])
                bigax2.spines['top'].set_color('none')
                bigax2.spines['bottom'].set_color('none')
                bigax2.spines['left'].set_color('none')
                bigax2.spines['right'].set_color('none')
                print "------------------------- looking at Y-pol station: ", st, " --------------------------"
                for unopt_corr in unopt_rem_corr:
                    if unopt_corr.rem_station == st and unopt_corr.ref_station == reference_station and unopt_corr.rem_station_pol == "Y":
                        colorVal = scalarMap.to_rgba(unopt_corr.snr)
                        chi2 = calc_chi_squared(opt_corr_phases.phase_corrections, unopt_corr.phase_corrections, station_phase_variance_by_channel_y[m])
                        print unopt_corr.scan_name, " & ", str( round(chi2/len(chan_indexes),2) ) , " & ", str(round(unopt_corr.snr,1)), " & ", str(round(unopt_corr.dtec,1))
                        for band in [1,2,3,4]:
                            ax = auto_fig.add_subplot(1,4,band)
                            ax.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                            ax.set_ylim(-90, 90)
                            ax.tick_params(top='off', bottom='on', left='on', right='off')
                            ax.plot(chan_freqs[(band-1)*8:band*8], unopt_corr.phase_corrections[(band-1)*8:band*8], '-', color=colorVal)
                            ax.grid(True)
                            ax.set_yticklabels([])
                colorVal = scalarMap.to_rgba(opt_corr.snr)
                for band in [1,2,3,4]:
                    ax = auto_fig.add_subplot(1,4,band)
                    ax.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                    ax.set_ylim(-90, 90)
                    ax.tick_params(top='off', bottom='on', left='on', right='off')
                    ax.plot(chan_freqs[(band-1)*8:band*8], opt_corr_phases.phase_corrections[(band-1)*8:band*8], 'ro')
                    #ax.errorbar(chan_freqs[(band-1)*8:band*8], opt_corr_phases.phase_corrections[(band-1)*8:band*8],  fmt='ro', yerr=station_phase_variance_by_channel_y[m][(band-1)*8:band*8])
                    ax.grid(True)
                    ax.set_yticklabels([])
                    ax2 = ax.twiny()
                    ax2.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                    ax2.set_xticks(chan_freqs[(band-1)*8:band*8])
                    ax2.set_xticklabels(chan_lables[(band-1)*8:band*8])
                print "reference scan = ", opt_corr_phases.scan_name
                cbax = auto_fig.add_subplot(111)
                cbar_position=auto_fig.add_axes([0.91,0.1,0.02,0.8])
                cbar = plt.colorbar(collevel, cmap=colmap, ticks=coltickvals, cax=cbar_position)
                cbar.ax.set_yticklabels(colticks)
                cbar.ax.set_ylabel('Scan SNR')
                axes = auto_fig.get_axes()
                auto_fig_name = "./phase_variation_" + st +"_Y_" + exp_name + ".png"
                plt.subplots_adjust(left=0.08, bottom=0.1, right=0.9, top=0.88, wspace=0.05, hspace=0.05)
                auto_fig.savefig(auto_fig_name)
                plt.close(auto_fig)

################################################################################
#Y-POL reference station

    st = reference_station
    for opt_corr in opt_ref_corr:
        if opt_corr.rem_station == st and opt_corr.ref_station == reference_station and opt_corr.rem_station_pol == "Y":
            opt_corr_phases = opt_corr
            auto_fig = plt.figure(figsize=(14,8))
            plt.xlim(chan_freqs[0], chan_freqs[31])
            plt.ylim(-90, 90)
            bigax = auto_fig.add_subplot(111)
            bigax.spines['top'].set_color('none')
            bigax.spines['bottom'].set_color('none')
            bigax.spines['left'].set_color('none')
            bigax.spines['right'].set_color('none')
            bigax.tick_params(top='off', bottom='off', left='on', right='off')
            bigax.set_ylabel('Phase Correction (deg)')
            bigax.set_xlabel('Channel Frequency (GHz)', labelpad=20)
            plt.title('Estimated Y-pol phase corrections for station (' + st + ') by channel for exp #' + exp_name, y=1.11 )
            bigax.set_yticks(deg_ticks)
            bigax.set_xticks([])
            bigax.xaxis.set_ticks_position('none')
            bigax2 = bigax.twiny()
            bigax2.set_xlim(bigax.get_xlim())
            bigax2.set_xlabel('Channel Index', labelpad=20)
            bigax2.set_xticklabels([])
            bigax2.spines['top'].set_color('none')
            bigax2.spines['bottom'].set_color('none')
            bigax2.spines['left'].set_color('none')
            bigax2.spines['right'].set_color('none')
            print "------------------------- looking at Y-pol station: ", st, " --------------------------"
            for unopt_corr in unopt_ref_corr:
                if unopt_corr.rem_station == st and unopt_corr.ref_station == reference_station and unopt_corr.rem_station_pol == "Y":
                    colorVal = scalarMap.to_rgba(unopt_corr.snr)
                    #chi2 = calc_chi_squared(opt_corr_phases.phase_corrections, unopt_corr.phase_corrections, station_phase_variance_by_channel_y[m])
                    #print unopt_corr.scan_name, " & ", str( round(chi2/len(chan_indexes),2) ) , " & ", str(round(unopt_corr.snr,1)), " & ", str(round(unopt_corr.dtec,1))
                    for band in [1,2,3,4]:
                        ax = auto_fig.add_subplot(1,4,band)
                        ax.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                        ax.set_ylim(-90, 90)
                        ax.tick_params(top='off', bottom='on', left='on', right='off')
                        ax.plot(chan_freqs[(band-1)*8:band*8], unopt_corr.phase_corrections[(band-1)*8:band*8], '-', color=colorVal)
                        ax.grid(True)
                        ax.set_yticklabels([])
            colorVal = scalarMap.to_rgba(opt_corr.snr)
            for band in [1,2,3,4]:
                ax = auto_fig.add_subplot(1,4,band)
                ax.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                ax.set_ylim(-90, 90)
                ax.tick_params(top='off', bottom='on', left='on', right='off')
                ax.plot(chan_freqs[(band-1)*8:band*8], opt_corr_phases.phase_corrections[(band-1)*8:band*8], 'ro')
                #ax.errorbar(chan_freqs[(band-1)*8:band*8], opt_corr_phases.phase_corrections[(band-1)*8:band*8],  fmt='ro', yerr=station_phase_variance_by_channel_y[m][(band-1)*8:band*8])
                ax.grid(True)
                ax.set_yticklabels([])
                ax2 = ax.twiny()
                ax2.set_xlim(chan_freqs[(band-1)*8]-freq_buffer, chan_freqs[band*8-1]+freq_buffer)
                ax2.set_xticks(chan_freqs[(band-1)*8:band*8])
                ax2.set_xticklabels(chan_lables[(band-1)*8:band*8])
            print "reference scan = ", opt_corr_phases.scan_name
            cbax = auto_fig.add_subplot(111)
            cbar_position=auto_fig.add_axes([0.91,0.1,0.02,0.8])
            cbar = plt.colorbar(collevel, cmap=colmap, ticks=coltickvals, cax=cbar_position)
            cbar.ax.set_yticklabels(colticks)
            cbar.ax.set_ylabel('Scan SNR')
            axes = auto_fig.get_axes()
            auto_fig_name = "./phase_variation_" + st +"_Y_" + exp_name + ".png"
            plt.subplots_adjust(left=0.08, bottom=0.1, right=0.9, top=0.88, wspace=0.05, hspace=0.05)
            auto_fig.savefig(auto_fig_name)
            plt.close(auto_fig)

################################################################################
################################################################################
################################################################################
################################################################################















































#reference_station must be circular, at the moment
def process_mixedmode_exp(exp_directory, circular_reference_station, linear_station, ref_baseline, control_file, output_cf_filename, num_proc=1, general_verbosity=True):

    #full path to control file
    control_file_path = os.path.abspath(control_file)
    
    #force all fringe files generated to save the control file 
    #information in the type_222 records
    set_commands = "set gen_cf_record true"

    #need these to be user configurable
    snr_thresh = 100
    dtec_thresh = 0.1
    dtec_nom = 0.2
    dtec_extremum_max = 1
    snr_extremum_min = 10
    snr_extremum_max = 500
    valid_quality_code_list = ["3", "4", "5", "6", "7", "8", "9", "10"]
    output_filename = ""

    #collect a list of all the root files that are present
    root_file_list = ht.recursive_find_root_files(exp_directory, True)

    [orig_control_hash, orig_setstring_hash] = get_control_hash(root_file_list[0], control_file_path, set_commands)
    if general_verbosity is True:
        print "Hash of control file: ", control_file, " = ", str(orig_control_hash)

    #only need to do one baseline at the moment, but do freq bands separately
    #set up for batch processing
    options_list = []
    baseline_list = [ref_baseline+":X", ref_baseline+":S"]
    if circular_reference_station == ref_baseline[0]:
        options_list = ['-P RX', '-P RY']
    if circular_reference_station == ref_baseline[1]:
        options_list = ['-P XR', '-P YR']

    #load the fringe files (no-auto-corrs) meta data for this directory
    ff_list_all = load_directory_fringe_files(exp_directory)
    
    #strip out the ones which do not match the control file used
    ff_list_tmp1 = filter_fringe_files_on_discrete_quantity(ff_list_all, "control_file_hash", [orig_control_hash])

    #cross off the root files which have already been processed: 
    #we assume that if we see any fringe file associated with a root file
    #the whole scan has been processed
    tmp_root_file_list = list(root_file_list)
    for ff in ff_list_tmp1:
        for rf in tmp_root_file_list:
            if ff.associated_root_file == rf:
                tmp_root_file_list.remove(rf)
                break

    #batch process all the scans, baselines and pol_prods in parallel with
    #original control file
    if len(tmp_root_file_list) != 0:
        if general_verbosity is True:
            print "Processing: ", len(tmp_root_file_list), " scans with control file: ", control_file
        tmp_list = ht.batch_fourfit_generate_fringe_parallel(options_list, baseline_list, control_file_path, tmp_root_file_list, set_commands, num_proc)
    else:
        if general_verbosity is True:
            print "Reloading", str(len(ff_list_tmp1)), " fringe files previously processed with: ", control_file

    #reload the fringe files (no-auto-corrs) meta data for this directory
    ff_list_all = load_directory_fringe_files(exp_directory)

    #get the experiment name
    exp_name = ff_list_all[0].exp_name

    #strip out the ones which do not match the control file used
    ff_list_tmp1 = filter_fringe_files_on_discrete_quantity(ff_list_all, "control_file_hash", [orig_control_hash])

    if general_verbosity is True:
        print "The total number of fringe files generated with control file: ", control_file, " = ", str(len(ff_list_tmp1))

    #apply data cuts to fringe files: on fringe quality, error codes, SNR range, etc.
    ff_list_tmp2 = filter_fringe_files_on_discrete_quantity(ff_list_tmp1, "quality", valid_quality_code_list)

    if general_verbosity is True:
        print "Remaining number of fringe files after quality code cuts = ", str(len(ff_list_tmp2))

    # ff_list_tmp3 = filter_fringe_files_on_discrete_quantity(ff_list_tmp2, "errcode", [' '])

    # print "number of fringe files after second filter = ", str(len(ff_list_tmp3))

    ff_list_cf1 = filter_fringe_files_on_value_range(ff_list_tmp2, "snr", snr_extremum_min, snr_extremum_max) #extremes in SNR are usually a sign of problems

    if general_verbosity is True:
        print "Remaining number of fringe files after SNR cuts = ", str(len(ff_list_cf1))


    #now split the fringe files based on frequency group
    x_band_ff = []
    s_band_ff = []
    for ff in ff_list_cf1:
        ff_path = ff.filename
        ff_name = ff_path.split('/')[-1]
        ff_freq = ff_name.split('.')[1]
        if ff_freq == 'S':
            s_band_ff.append(ff)
        if ff_freq == 'X':
            x_band_ff.append(ff)


    #collect all fringe files into baseline collections (only if complete with 2x pol-prods)
    stations = circular_reference_station + linear_station
    sband_bline_collection_list = join_fringes_into_baseline_collection(s_band_ff, stations, include_autos=False, only_complete=True, mixedmode=True)
    xband_bline_collection_list = join_fringes_into_baseline_collection(x_band_ff, stations, include_autos=False, only_complete=True, mixedmode=True)

    
    if general_verbosity is True:
        print "Total number of complete baseline collections for s band = ", str(len(sband_bline_collection_list))
        print "Total number of complete baseline collections for x band = ", str(len(xband_bline_collection_list))

    for x in sband_bline_collection_list:
        x.init_values()

    for x in xband_bline_collection_list:
        x.init_values()

    #sort in order to find maximumal min-snr
    sort_objects_by_quantity(sband_bline_collection_list, "min_snr", reverse_boolean=True)
    sort_objects_by_quantity(xband_bline_collection_list, "min_snr", reverse_boolean=True)
        
    #simplest thing possible...just use scan with highest min snr
    #will need to get better with this...use multiple scans, baselines etc.
    xband_cflines = generate_mixed_mode_control_file_lines(xband_bline_collection_list[0].associated_root_file, circular_reference_station, linear_station, 'X', control_file_path, general_verbosity, num_proc)
    sband_cflines = generate_mixed_mode_control_file_lines(sband_bline_collection_list[0].associated_root_file, circular_reference_station, linear_station, 'S', control_file_path, general_verbosity, num_proc)

    append_control_file(control_file_path, output_cf_filename, xband_cflines+sband_cflines) 

################################################################################
################################################################################
################################################################################
################################################################################
