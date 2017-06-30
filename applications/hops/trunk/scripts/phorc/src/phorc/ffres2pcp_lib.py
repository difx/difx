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

#hops package python libs
import mk4
import afio
import hopstest as ht

from .processing import launch_processes_in_parallel
from .processing import append_control_file
from .fringe_file_handle import *

def generate_ffres2pcp_control_file(root_filename, reference_station, remote_stations, control_filename, output_filename, verbose=False, dtec_tol=0.1, num_processes=1):
    
    control_file = os.path.abspath(control_filename)
    
    [ref_cf_lines, rem_cf_lines, cfilename] = compute_ffres2pcp_lines(root_filename, reference_station, remote_stations, control_file, output_filename, verbose, dtec_tol, num_processes)

    append_control_file(output_filename, cfilename, ref_cf_lines + rem_cf_lines)

def compute_ffres2pcp_lines(root_filename, reference_station, remote_stations, control_filename, output_filename, verbose=False, dtec_tol=0.1, num_processes=1):
    ref_station = reference_station
    stations = remote_stations
    control_file = control_filename
    root = root_filename
    
    #dtec tolerance
    dtec_tolerance = dtec_tol

    # generate list of all baselines of interest
    blist = []
    for s2 in stations:
        blist.append (ref_station+s2)
    
    #remove auto-corrs (shouldn't happen for good user input, but just in case)
    for bl in blist:
        if bl[0] == bl[1]:
            blist.remove (bl)

    global pol_prods, chans
    pol_prods = ['XX', 'YY', 'XY', 'YX']
    chans = 'abcdefghijklmnopqrstuvwxyzABCDEF'

    #first figure out the root directory, scan and the experiment number
    abs_root_path = os.path.abspath(root)
    scan_dir = os.path.dirname(abs_root_path)
    exp_dir = os.path.dirname(scan_dir)
    root_filename = os.path.split(abs_root_path)[1]
    scan_name = os.path.split(scan_dir)[1]
    exp_name = os.path.split(exp_dir)[1]

    #make sure we have the files we need for the baselines specified
    suffix = root[-6:]
    flist = os.listdir(scan_dir);
    blpresent = list(blist)
    for fname in flist:
        for bl in blist:
            blname = bl + ".." + suffix
            if blname == fname:
                blpresent.remove(bl)

    if len(blpresent) != 0:
        print "Error: not all specified baselines are present: ", blpresent[0], " is missing." 
        sys.exit()
    
    #copy the root directories contents over to a temp directory mirroring
    #the file tree of the original scan
    temp_dir = tempfile.mkdtemp()
    path = os.path.join(temp_dir, exp_name, scan_name)
    copy_tree(scan_dir, path)
    #set root to point to the new temp directory
    root = os.path.join(path, root_filename)

    #now we are going to fourfit the scan of interest once again
    #with the specified control file for all blines from reference station
    data_objects = all_blines_ff_parallel(control_file, blist, root, num_processes)
    
    if verbose is True:
        print "Generating control file lines."
    [ref_cf_lines, rem_cf_lines] = generate_control_lines(root, data_objects, temp_dir, ref_station, remote_stations, dtec_tolerance, verbose)
    
    #clean up
    shutil.rmtree(temp_dir, ignore_errors=False, onerror=None)
    
    #now generate the control file name
    cfilename = ""
    if output_filename != "":
        cfilename =  output_filename
    else:
        cfilename = "./cf" + exp_name + "_" + scan_name + "_" + ref_station + stations
    
    #return
    return [ref_cf_lines, rem_cf_lines, cfilename]

################################################################################

def compute_ffres2pcp_phase_correction_objects(root_filename, reference_station, remote_stations, control_filename, output_filename, verbose=False, dtec_tol=0.1, num_processes=1):
    ref_station = reference_station
    stations = remote_stations
    control_file = control_filename
    root = root_filename
    
    #dtec tolerance
    dtec_tolerance = dtec_tol

    # generate list of all baselines of interest
    blist = []
    for s2 in stations:
        blist.append (ref_station+s2)
    
    #remove auto-corrs (shouldn't happen for good user input, but just in case)
    for bl in blist:
        if bl[0] == bl[1]:
            blist.remove (bl)

    global pol_prods, chans
    pol_prods = ['XX', 'YY', 'XY', 'YX']
    chans = 'abcdefghijklmnopqrstuvwxyzABCDEF'

    #first figure out the root directory, scan and the experiment number
    abs_root_path = os.path.abspath(root)
    scan_dir = os.path.dirname(abs_root_path)
    exp_dir = os.path.dirname(scan_dir)
    root_filename = os.path.split(abs_root_path)[1]
    scan_name = os.path.split(scan_dir)[1]
    exp_name = os.path.split(exp_dir)[1]

    #make sure we have the files we need for the baselines specified
    suffix = root[-6:]
    flist = os.listdir(scan_dir);
    blpresent = list(blist)
    for fname in flist:
        for bl in blist:
            blname = bl + ".." + suffix
            if blname == fname:
                blpresent.remove(bl)

    if len(blpresent) != 0:
        print "Error: not all specified baselines are present: ", blpresent[0], " is missing." 
        sys.exit()
    
    #copy the root directories contents over to a temp directory mirroring
    #the file tree of the original scan
    temp_dir = tempfile.mkdtemp()
    path = os.path.join(temp_dir, exp_name, scan_name)
    copy_tree(scan_dir, path)
    #set root to point to the new temp directory
    root = os.path.join(path, root_filename)

    #now we are going to fourfit the scan of interest once again
    #with the specified control file for all blines from reference station
    data_objects = all_blines_ff_parallel(control_file, blist, root, num_processes)
    
    if verbose is True:
        print "Generating phase corrections."
    [ref_corr, rem_corr] = generate_phase_corrections(root_filename, data_objects, temp_dir, ref_station, stations, dtec_tolerance, verbose)

    #clean up
    shutil.rmtree(temp_dir, ignore_errors=False, onerror=None)

    #return
    return [ref_corr, rem_corr]

################################################################################

def all_blines_ff_parallel(control_file, blines, root, max_num_processes=1):
    
    pol_opts = []
    for n in range(0,len(pol_prods)):
        pol_opts.append('-P'+pol_prods[n])

    #determine number of jobs we need to run
    n_pols = len(pol_opts)
    n_baselines = len(blines)
    n_total = n_pols*n_baselines

    #construct a list of arg lists for each process we need to run
    arg_list = []
    for nbase in range(0,n_baselines):
        for npol in range(0,n_pols):
            arg_list.append( [ pol_opts[npol], blines[nbase], control_file, root, False, "", False ] )

    #run the fourfit processes
    processed_args_list = []
    generated_fringe_files = []
    [generated_fringe_files, processed_args_list] = launch_processes_in_parallel(arg_list, max_num_processes)

    #loop over generated fringe files and retrieve the needed data
    data_objects = []
    for ffile in generated_fringe_files:
        data_objects.append(extract_data(ffile))

    return data_objects


########################################################################

def generate_control_lines(root_file, data_objects, temp_dir, ref_station, stations, dtec_tolerance, verbose=False):
    
    #first figure out the root directory, scan and the experiment number
    abs_root_path = os.path.abspath(root_file)
    scan_dir = os.path.dirname(abs_root_path)
    exp_dir = os.path.dirname(scan_dir)
    root_filename = os.path.split(abs_root_path)[1]
    scan_name = os.path.split(scan_dir)[1]
    exp_name = os.path.split(exp_dir)[1]
    
    [ref_pcp_obj, rem_pcp_obj_list] = generate_phase_corrections(root_file, data_objects, temp_dir, ref_station, stations, dtec_tolerance, verbose)
    
    ref_control_lines = ""
    rem_control_lines = ""

    info_lines = "*==========================================================\n"
    info_lines += "* the following pc_phase_y entry for (reference) station: " + str(ref_station) + " was generated\n"
    info_lines += "* using data from baselines:"
    for station in stations:
        tmpbline = ref_station + station
        info_lines += " " + tmpbline
    info_lines += "\n"
    info_lines += "* with ffres2pcp using: experiment # " + exp_name + " and scan " + scan_name + "\n"
    info_lines += "*==========================================================\n"
    ref_control_lines += info_lines

    station_line = "if station " + ref_station + "\n"
    ref_control_lines += station_line

    pcphase_y_line = "  pc_phases_y " + chans + " "
    for i in range(0,len(chans)):
        pcphase_y_line += "\t" + str( ref_pcp_obj.phase_corrections[i] )
    pcphase_y_line += "\n"
    ref_control_lines += pcphase_y_line
    ref_control_lines += "\n"
    
    #now do remote stations X/Y relative to reference X-pol
    for station in stations:
        info_lines = "*==========================================================\n"
        info_lines += "* the following pc_phase_x/y entries for station: " + str(station) + " were generated\n"
        info_lines += "* with ffres2pcp using: experiment # " + exp_name + " and scan " + scan_name + "\n"
        info_lines += "*==========================================================\n"
        rem_control_lines += info_lines

        station_line = "if station " + station + "\n"
        rem_control_lines += station_line

        pcphase_x_line = "  pc_phases_x " + chans + " "
        pcphase_y_line = "  pc_phases_y " + chans + " "
        for obj in rem_pcp_obj_list:
            if obj.ref_station == ref_station and obj.rem_station == station: 
                if obj.ref_station_pol == "X" and obj.rem_station_pol == "X":
                    phase_corr = obj.phase_corrections
                    for x in phase_corr:
                        pcphase_x_line += "\t" + str(x)
                    pcphase_x_line += "\n"
                if obj.ref_station_pol == "X" and obj.rem_station_pol == "Y":
                    phase_corr = obj.phase_corrections
                    for y in phase_corr:
                        pcphase_y_line += "\t" + str(y)
                    pcphase_y_line += "\n"
        rem_control_lines += pcphase_x_line
        rem_control_lines += pcphase_y_line
        rem_control_lines += "\n"

    return [ref_control_lines, rem_control_lines]

    
def generate_phase_corrections(root_file, data_objects, temp_dir, ref_station, stations, dtec_tolerance, verbose=False):

    correction_list = []

    #first figure out the root directory, scan and the experiment number
    abs_root_path = os.path.abspath(root_file)
    scan_dir = os.path.dirname(abs_root_path)
    exp_dir = os.path.dirname(scan_dir)
    root_filename = os.path.split(abs_root_path)[1]
    scan_name = os.path.split(scan_dir)[1]
    exp_name = os.path.split(exp_dir)[1]
    
    #run check that the dTEC is within tolerance for all polarization products
    #along a particular baseline
    dtec_ok = True
    for station in stations:
        tmpbline = ref_station + station
        bline_dtec = []
        bline_pp = []
        for obj in data_objects:
            if obj.is_valid == True and obj.baseline == tmpbline:
                bline_dtec.append(obj.dtec)
                bline_pp.append(obj.polprod)
        mean_dtec = np.mean(bline_dtec)
        for x in range(0, len(bline_dtec)):
            delta = bline_dtec[x] - mean_dtec
            if abs(delta) > dtec_tolerance:
                if verbose is True:
                    print "Warning: dtec variation of: ", str(abs(delta)), " exceeds tolerance of: ", dtec_tolerance, " on baseline: ", tmpbline , "and polarization product: ", bline_pp[x] 
                    print "array of baseline dtec values = ", str(bline_dtec)
                    print "mean dtec value = ", str(mean_dtec)
                dtec_ok = False

    if dtec_ok is False:
        #clean up
        shutil.rmtree(temp_dir, ignore_errors=False, onerror=None)
        print "Exiting."
        sys.exit()
    
    #compute the phase corrections for all cross-product objects by subtracting the mean phase and unwrapping
    for obj in data_objects:
        if obj.is_valid == True:
            if obj.polprod == 'XX' or obj.polprod == 'XY' or obj.polprod == 'YY' or obj.polprod == 'YX':
                #unwrap and subtract the mean of the phases
                phase_arr = obj.phase_residuals
                for i in range(0,len(phase_arr)):
                    phase_arr[i] *= -1.0*(math.pi/180.0)
                phase_arr = np.unwrap(phase_arr)
                for i in range(0,len(phase_arr)):
                    phase_arr[i] = phase_arr[i]*(180.0/math.pi)
                mean_phase = np.mean(phase_arr)
                for i in range(0,len(phase_arr)):
                    phase_arr[i] -= mean_phase
                    phase_arr[i] = round(phase_arr[i],1) #round to 1 decimal place
                #save the phase correction in object
                obj.phase_corrections = phase_arr
    
    #use the reference station X-pol as reference for all,
    #now determine the reference station Y-pol phases relative to X-pol
    #through closure relations, can do this calculation for each station
    ref_pc_phase_y = [ [] for x in xrange(len(chans)) ]
    mean_snr = 0
    ms_count = 0
    for station in stations:
        tmpbline = ref_station + station
        phase_corr_xx = []
        phase_corr_xy = []
        phase_corr_yy = []
        phase_corr_yx = []
        for obj in data_objects:
            if obj.is_valid == True:
                if obj.baseline == tmpbline:
                    if obj.polprod == "XX":
                        phase_corr_xx = obj.phase_corrections
                        mean_snr += obj.snr
                        ms_count += 1.0
                    if obj.polprod == "XY":
                        phase_corr_xy = obj.phase_corrections
                        mean_snr += obj.snr
                        ms_count += 1.0
                    if obj.polprod == "YY":
                        phase_corr_yy = obj.phase_corrections
                        mean_snr += obj.snr
                        ms_count += 1.0
                    if obj.polprod == "YX":
                        phase_corr_yx = obj.phase_corrections
                        mean_snr += obj.snr
                        ms_count += 1.0
        loop_xx_yx = []
        loop_yy_xy = []
        for i in range(0,len(chans)):
            loop_xx_yx.append( phase_corr_xx[i] - phase_corr_yx[i] )
            loop_yy_xy.append( phase_corr_xy[i] - phase_corr_yy[i] )
            ref_pc_phase_y[i].append(phase_corr_xx[i] - phase_corr_yx[i])
            ref_pc_phase_y[i].append(phase_corr_xy[i] - phase_corr_yy[i])

    #warn about too large a spread in the pc_phase_y corrections
    if verbose is True:
        print "Reference station pc_phase_y corrections are:  "
    for i in range(0,len(chans)):
        if verbose is True:
            print "channel: ", chans[i], " pc_phase_y = ",  str( round( np.mean(ref_pc_phase_y[i]), 1) ), " and std. dev. = " , str( round( np.std(ref_pc_phase_y[i]), 1) )
            if np.std(ref_pc_phase_y[i]) > 15.0:
                print "Warning, standard deviation in reference station pc_phase_y correction larger than 15 degrees in channel: " + chans[i]

    #entry for the reference station Y-pol corrections
    #here we are performing an unweighted average over the phase values
    #TODO determine what we should use for the SNR value
    ref_pcp_obj = pcphase_correction_data()
    ref_pcp_obj.snr = mean_snr/ms_count #FIXME
    ref_pcp_obj.filename = "" #FIXME
    ref_pcp_obj.dtec = 0 #FIXME
    ref_pcp_obj.ref_station = ref_station
    ref_pcp_obj.rem_station = ref_station
    ref_pcp_obj.ref_station_pol = "X"
    ref_pcp_obj.rem_station_pol = "Y"
    ref_pcp_obj.phase_corrections = []
    for i in range(0,len(chans)):
        ref_pcp_obj.phase_corrections.append( round( np.mean(ref_pc_phase_y[i]), 1) )
        
    #now do remote stations X/Y relative to reference X-pol
    for station in stations:
        tmpbline = ref_station + station
        for obj in data_objects:
            if obj.is_valid == True:
                if obj.baseline == tmpbline:
                    if obj.polprod == "XX":
                        rem_pcp_obj_x = pcphase_correction_data()
                        rem_pcp_obj_x.snr = obj.snr
                        rem_pcp_obj_x.dtec = obj.dtec
                        rem_pcp_obj_x.filename = obj.filename
                        rem_pcp_obj_x.ref_station = ref_station
                        rem_pcp_obj_x.rem_station = station
                        rem_pcp_obj_x.ref_station_pol = "X"
                        rem_pcp_obj_x.rem_station_pol = "X"
                        rem_pcp_obj_x.phase_corrections = []
                        for val in obj.phase_corrections:
                            rem_pcp_obj_x.phase_corrections.append(val)
                        correction_list.append(rem_pcp_obj_x)
                    if obj.polprod == "XY":
                        rem_pcp_obj_y = pcphase_correction_data()
                        rem_pcp_obj_y.snr = obj.snr
                        rem_pcp_obj_y.dtec = obj.dtec
                        rem_pcp_obj_y.filename = obj.filename
                        rem_pcp_obj_y.ref_station = ref_station
                        rem_pcp_obj_y.rem_station = station
                        rem_pcp_obj_y.ref_station_pol = "X"
                        rem_pcp_obj_y.rem_station_pol = "Y"
                        rem_pcp_obj_y.phase_corrections = []
                        for val in obj.phase_corrections:
                            rem_pcp_obj_y.phase_corrections.append(val)
                        correction_list.append(rem_pcp_obj_y)
                        phase_corr = obj.phase_corrections

    return [ref_pcp_obj, correction_list]

########################################################################
def extract_data(filename):
    ret_obj = residual_data()
    if os.path.isfile(filename):
        pol_product = ht.get_file_polarization_product(filename)
        ff = mk4.mk4fringe(filename)
        bline = ff.t202.contents.baseline
        dtec = ff.t201.contents.dispersion
        
        phases = []
        for n in range(0,len(chans)):
            #append the phase residuals
            phases.append(ff.t210.contents.amp_phas[n].phase)
        
        ret_obj.snr = ff.t208.contents.snr
        ret_obj.filename = filename
        ret_obj.scan_name = ff.t200.contents.scan_name
        ret_obj.is_valid = True
        ret_obj.baseline = bline
        ret_obj.polprod = pol_product
        ret_obj.dtec = dtec
        ret_obj.phase_residuals = phases
    return ret_obj
