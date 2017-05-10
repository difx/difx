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

error_condition = False

def compute_fourphase(root_filename, station_ids, control_filename, output_filename, verbose=False, ion_original=False, num_processes=1):
    
    global error_condition
    error_condition = False
    
    global pol_prods, chans
    pol_prods = ['XX', 'YY', 'XY', 'YX']
    chans = 'abcdefghijklmnopqrstuvwxyzABCDEF '

    stations = station_ids
    root = root_filename
    suffix = root[-6:]
                                    # generate list of all possible baselines
    blist = []
    for s1 in stations:
        for s2 in stations:
            blist.append (s1+s2+'..'+suffix)

    for bl in blist:
        if bl[0] == bl[1]:
            blist.remove (bl)
            
    #create a temp directory for workspace
    temp_dir = tempfile.mkdtemp()

    #first figure out the root directory, scan and the experiment number
    abs_root_path = os.path.abspath(root)
    scan_dir = os.path.dirname(abs_root_path)
    exp_dir = os.path.dirname(scan_dir)
    root_filename = os.path.split(abs_root_path)[1]
    scan_name = os.path.split(scan_dir)[1]
    exp_name = os.path.split(exp_dir)[1]

    #copy the root directories contents over to a temp directory mirroring
    #the file tree of the original scan
    path = os.path.join(temp_dir, exp_name, scan_name)
    copy_tree(scan_dir, path)
    #set root to point to the new temp directory
    root = os.path.join(path, root_filename)

    filz = os.listdir (path)
    blines = []
    for bl in blist:                # winnow exhaustive bl list based on actual files present
        if bl in filz:
            blines.append (bl[0]+bl[1])
    if verbose:
        print 'baselines:'
        for bl in blines:
            print bl

    sbd = []
    mbd = []
    phase = []
    tec = []

    control_file = os.path.abspath(control_filename)
    of_name = output_filename

                                # find best-fit ionosphere tec
    st_tec = all_blines_ion_parallel (control_file, blines, stations, root, chans, ion_original, verbose, max_num_processes=num_processes)

    if verbose:
        print 'dTEC by station for', stations, ':\n', st_tec

                                    # delay and phase fits
    bproducts = all_blines_phd_parallel (control_file, blines, stations, st_tec, root, chans, verbose, max_num_processes=num_processes)
                                # do statistical analysis to catch errors
    bproducts = bp_analyze (blines, bproducts, verbose)
                                # now fit ph & del params to data
    ydelays, yphases = fit_phd (blines, stations, bproducts)
    if verbose:
        print 'ydelay (ns)  by station:', '%7.3f ' * len(ydelays) % tuple (ydelays) 
        print 'yphase (deg) by station:', '%7.1f ' * len(yphases) % tuple (yphases) 

                                # create output file with new params
    write_out (of_name, root, stations, st_tec, ydelays, yphases, control_file)
                                    # see if new file works well
    test_out_parallel (of_name, of_name, blines, stations, root, st_tec, verbose, max_num_processes=num_processes)

    #clean up
    shutil.rmtree(temp_dir, ignore_errors=False, onerror=None)

    return error_condition

# function to find best-fit ionosphere for all baselines
def all_blines_ion_parallel (control_file, blines, stations, root, chans, ion_original, verbose, max_num_processes=1):
    if verbose:
        print '\nfitting ionosphere tec...'
        
    bl_tec = []
    bl_combo_snr = []
    
    pol_opts = []
    for n in range(0,len(pol_prods)):
        pol_opts.append('-P'+pol_prods[n])

    bl_setstrings = []
    
    for bl in blines:
        setstring = zero_string (bl)
        # tack on ion. search stmts.
        if not ion_original:
            setstring += 'if '
            setstring += 'ion_npts 41 ion_win -80.0 80.0 '
        bl_setstrings.append(setstring)
    
    #determine number of jobs we need to run
    n_opts = len(pol_opts)
    n_baselines = len(blines)
    n_total = n_opts*n_baselines

    #easiest to just construct a list of arg lists for each process we need to run
    arg_list = []
    for nbase in range(0,n_baselines):
        for npol in range(0,n_opts):
            arg_list.append( [ pol_opts[npol], blines[nbase], control_file, root, False, bl_setstrings[nbase], False ] )
            #arg_list.append( [ pol_opts[npol], blines[nbase], control_file, root, False, bl_setstrings[nbase], True ] ) #for plots

    processed_args_list = []
    generated_fringe_files = []
    [generated_fringe_files, processed_args_list] = launch_processes_in_parallel(arg_list, max_num_processes)

    #now we read out the fringe files and determine best-fit tec for each baseline
    for bl in blines:               # determine best-fit tec for each baseline
        [dtec, combo_snr] = bline_ion_from_file(bl, processed_args_list, generated_fringe_files, verbose) 
        bl_tec.append (dtec)
        bl_combo_snr.append (combo_snr)

    wgt = []                        # form weight array
    snr_min = min (bl_combo_snr)
    for bcs in bl_combo_snr:
        wgt.append (bcs / snr_min)
    wgt.append (1.0)                # weight of sum pseudo-obs.
    wgt = np.array (wgt)

                                    # build normal equations
    A = []
    B = []

    for bl in blines:
        arow = np.zeros (len (stations))
        s = 0
        for st in stations:
            if bl[0] == st:
                arow[s] = -1
            if bl[1] == st:
                arow[s] = 1
            s += 1
        A.append (arow)
    A.append (np.ones (len (stations)))

    A = np.array (A)

    Aw = A * wgt[:, None]

    bl_tec.append (0.0)            # append sum pseudo-obs.
    Bw = np.multiply (np.array (bl_tec), wgt)

    rvals = np.linalg.lstsq (Aw, Bw)

    st_tec = rvals[0]
    return st_tec


# function to find best ionosphere for 1 baseline from collection of files
def bline_ion_from_file (bl, processed_args_list, generated_file_list, verbose):
    
    global error_condition
    
    pol_opts = []
    for n in range(0,len(pol_prods)):
        pol_opts.append('-P'+pol_prods[n])

        
    refstn = bl[0]
    remstn = bl[1]
    bline = bl

    sum_dtec_weighted = 0.0
    sum_weights = 0.0

    results_table = []
    results_table.append(['bl', 'pol', 'snr', 'amp', 'phase', 'sbd', 'mbd', 'dtec'])

    dtec_min = 1e6
    dtec_max = -1e6
    for j in range(0, len(pol_opts) ):
        pp = pol_opts[j] 
        for i in range( 0, len(processed_args_list) ):
            p_args = processed_args_list[i]
            if p_args[1] == bline and p_args[0] == pp:
                file_to_read = generated_file_list[i]
                [snr, sb, mb, phi, dtec, amp] = pol_prod_read_fringe( file_to_read )
                results_table.append([bl, pol_prods[j], round(snr,2), round(10000*amp,3), round(phi,2), round(sb,6) , round(mb,6), round(dtec,6)])
                sum_dtec_weighted += dtec * snr * snr
                sum_weights += snr * snr
                if dtec > dtec_max:
                    dtec_max = dtec
                if dtec < dtec_min:
                    dtec_min = dtec

    dtec = sum_dtec_weighted / sum_weights
    weight = math.sqrt (sum_weights)
                        # detect overly large ion discrepancy
    dtec_ptop = dtec_max - dtec_min
    if dtec_ptop > 3.0:
        error_condition = True
        print '\n**** ERROR **** bad ionosphere fit! dtec on baseline', bl, \
            'differed by %6.1f ' % dtec_ptop, 'TEC units'
    elif dtec_ptop > 1.0:
        error_condition = True
        print '\n**** WARNING **** check ionosphere fit! dtec on baseline', bl, \
            'differed by %6.1f ' % dtec_ptop, 'TEC units'

    if verbose:
        print_table(results_table)

    return dtec, weight

# function to generate setstring for 0 a priori case

def zero_string (bl):
                                    # force a priori phases and delays to zero
    setstring = 'set if station ' + bl[0] + ' '
    setstring += 'pc_delay_x 0.0' + ' '
    setstring += 'pc_delay_y 0.0' + ' '

    setstring += 'if station ' + bl[1] + ' '
    setstring += 'pc_delay_x 0.0' + ' '
    setstring += 'pc_delay_y 0.0' + ' '

    return setstring

#read data from fringe file
def pol_prod_read_fringe(filename):
    ffile = mk4.mk4fringe(filename)
    #get the fringe solution data from the type_208 
    t208 = ffile.t208.contents
    snr = t208.snr
    amp = t208.amplitude
    phase = t208.resphase
    sbd = t208.resid_sbd
    mbd = t208.resid_mbd
    #ion_diff (TEC) is stored in the type_201.dispersion
    ion_dtec = ffile.t201.contents.dispersion
    results = [snr, sbd, mbd, phase, ion_dtec, amp]
    return results

# function to find delay and phase parameters on all baselines
def all_blines_phd_parallel (control_file, blines, stations, st_tec, root, chans, verbose, max_num_processes=1):
    if verbose:
        print '\ngenerating baseline & pol-prod data using dTECs...'

    bproducts = []
    
    pol_opts = []
    for n in range(0,len(pol_prods)):
        pol_opts.append('-P'+pol_prods[n])

    bl_setstrings = []
    
    for bl in blines:
        dtec = 0.0
        setstring = zero_string (bl) + ' if ion_npts 1 '
        s = 0
        for st in stations:         # form dtec = rem - ref tec
            if st == bl[0] or st == bl[1]:
                setstring += ' if station ' + st + ' ionosphere ' + str (st_tec[s]) + ' '
            s += 1
        bl_setstrings.append(setstring)
        
    #determine number of jobs we need to run
    n_opts = len(pol_opts)
    n_baselines = len(blines)

    #easiest to just construct a list of arg lists for each process we need to run
    arg_list = []
    for nbase in range(0,n_baselines):
        for npol in range(0,n_opts):
            arg_list.append( [ pol_opts[npol], blines[nbase], control_file, root, False, bl_setstrings[nbase], False ] )
            #arg_list.append( [ pol_opts[npol], blines[nbase], control_file, root, False, bl_setstrings[nbase], True ] ) #for plots

    
    processed_args_list = []
    generated_fringe_files = []
    [generated_fringe_files, processed_args_list] = launch_processes_in_parallel(arg_list, max_num_processes)
    
    #now we read out the fringe files and determine best-fit tec for each baseline
    for bl in blines:               # determine best-fit tec for each baseline
        products = bline_phd_from_file(bl, processed_args_list, generated_fringe_files, verbose) 
        bproducts.append (products)
    
    return bproducts

# function to find delay and phase parameters on 1 baseline & 4 polarization products from fringe files
def bline_phd_from_file (bl, processed_args_list, generated_file_list, verbose):

    global error_condition

    pol_opts = []
    for n in range(0,len(pol_prods)):
        pol_opts.append('-P'+pol_prods[n])

    bline = bl
    products = []
    
    results_table = []
    results_table.append(['bl', 'pol', 'snr', 'amp', 'phase', 'sbd', 'mbd', 'dtec'])

    for j in range(0, len(pol_opts) ):
        pp = pol_opts[j] 
        for i in range( 0, len(processed_args_list) ):
            p_args = processed_args_list[i]
            if p_args[1] == bline and p_args[0] == pp:
                file_to_read = generated_file_list[i]
                [snr, sb, mb, phi, dtec, amp] = pol_prod_read_fringe( file_to_read )
                results_table.append([bl, pol_prods[j], round(snr,2), round(10000*amp,3), round(phi,2), round(sb,6) , round(mb,6), round(dtec,6)])
                # print 'baseline', bl, 'pol_prod', pp, 'mbd', mb, 'phase %6.1f' % phi, 'snr', snr
                if snr < 15:
                    error_condition = True
                    print '\n**** ERROR ****', pp, 'on', bl, 'has snr of only', snr
                elif snr < 25:
                    error_condition = True
                    print '\n**** WARNING ****', pp, 'on', bl, 'has snr of only', snr
                products.append ([mb, phi, snr])


    if verbose:
        print_table(results_table)

    return products

# function to fit station y phases and delays to baseline data
# bproducts is triply-nested list, most-significantly indexed 
# by baseline, then pol-prod within baseline, and [mbd, phase, snr]

def fit_phd (blines, stations, bproducts):

                                    # fit all x&y delays first
    mbd = [bproducts[u][v][0] for u in range(len(bproducts)) for v in range(4)]
    phz = [bproducts[u][v][1] for u in range(len(bproducts)) for v in range(4)]
    wgt = [bproducts[u][v][2] for u in range(len(bproducts)) for v in range(4)]
    wgt.append (1)                  # append nominal weight for X0 pseudo-obs

    wgt = np.array (wgt)
                                    # append x of 1st stn. pseudo-obs.
    mbd.append (0.0)
                                    # fit station y delays to mbd data
    delays = fit_data (blines, stations, mbd, wgt)
    delays *= 1000                  # convert units to ns

                                    # append x of 1st stn. pseudo-obs.
    phz.append (0.0)
                                    # fit station y phases to phase data
    phases = fit_data (blines, stations, phz, wgt)
    
    return delays, phases


# function to determine best-fit delays or phases using least-squares

def fit_data (blines, stations, data, wgt):
                                    # build normal equations
    A = []
    B = []
    ns = len (stations)
    b = 0
    for bl in blines:
        p = 0
        for pp in pol_prods:
                                    # each row has both x&y per station
            arow = 2 * ns * [0] 
            sref = stations.index (bl[0])
            srem = stations.index (bl[1])

            xy = 0
            for pol in ['X', 'Y']:
                if pp[0] == pol:
                    arow[sref + xy] = -1
                if pp[1] == pol:
                    arow[srem + xy] = +1
                xy += ns
            A.append (arow)
            p += 1
        b += 1
                                    # constrain first station's X pol. to be 0
    A.append ([1] + (2 * ns - 1) * [0])    

    Aw = np.array (A) * wgt[:, None]

    Bw = np.multiply (np.array (data), wgt)

    rvals = np.linalg.lstsq (Aw, Bw)
    params = rvals[0]
                                    # make all stn params relative to xpol = 0
    for n in range (ns):            # and flip sign
        params[n+ns] = params[n] - params[n+ns]
                                    # ret y values, which are in 2nd half of the array
    return params[ns:2*ns]



# examine closure quantities to see if fits look OK
def bp_analyze (blines, bproducts, verbose):
    
    global error_condition
    
    b = 0
    dir = [1, 1, -1,-1]             # xx + yy - xy - yx

    for bl in blines:
        dsum = 0
        phsum = 0
        p = 0
        for pp in pol_prods:
            dsum  += bproducts[b][p][0] * dir[p] * 1e6
            phsum += bproducts[b][p][1] * dir[p]
            p += 1
            
        if verbose:
            print 'baseline', bl, 'closure delay %8.1f ps' % dsum, 'closure phase %7.1f deg' % phsum
        if abs (dsum) > 25:
            error_condition = True
            print '\n**** ERROR **** delay misclosure on', bl, \
                  'pol_prods is %6.0f' % dsum, 'ps'
        elif abs (dsum) > 10:
            error_condition = True
            print '\n**** WARNING **** delay misclosure on', bl, \
                  'pol_prods is %6.0f' % dsum, 'ps'

                                    # find ambiguity combo with minimum rms
        phases = [0] * 4
        for i in range(4):
            phases[i] = bproducts[b][i][1]
        orig_phases = list (phases)
        orig_rms = rms (phases)
        min_rms = orig_rms
        best_phases = list (orig_phases)
                                    # search over all possible ambiguities
        for k in range(1, 15):
            phases = list (orig_phases)
            for i in range(4):
                phases[i] += 360 * ((k>>i) & 1)
            r = rms (phases)
            if r < min_rms:
                min_rms = r         # remember new minimum
                best_phases = phases

        if min_rms < orig_rms:
            for i in range(4):      # update original phases
                bproducts[b][i][1] = best_phases[i]
            if verbose:
                print '\nnew phase ambiguities selected for baseline', bl
                print 'old phases: %7.1f %7.1f %7.1f %7.1f' % tuple(orig_phases), \
                      'rms %7.1f' % orig_rms
                print 'new phases: %7.1f %7.1f %7.1f %7.1f' % tuple(best_phases), \
                      'rms %7.1f' % min_rms

        # print 'reference:'
        # print 'original %s YX-XX' % bl, orig_phases[3] - orig_phases[0]
        # print 'original %s YY-XY' % bl, orig_phases[1] - orig_phases[2]
        # print 'modified %s YX-XX' % bl, best_phases[3] - best_phases[0]
        # print 'modified %s YY-XY' % bl, best_phases[1] - best_phases[2]
        # print 'remote:'
        # print 'original %s XY-XX' % bl, orig_phases[2] - orig_phases[0]
        # print 'original %s YY-YX' % bl, orig_phases[1] - orig_phases[3]
        # print 'modified %s XY-XX' % bl, best_phases[2] - best_phases[0]
        # print 'modified %s YY-YX' % bl, best_phases[1] - best_phases[3]

        if verbose:
            print 'baseline', bl, 'closure delay %8.1f ps' % dsum, 'closure phase %7.1f deg' % phsum
                                    # recalculate phase sum after modifications
        phsum = 0
        p = 0
        for pp in pol_prods:
            phsum += bproducts[b][p][1] * dir[p]
            p += 1

        if abs (phsum) > 25:
            error_condition = True
            print '\n**** ERROR **** phase misclosure on', bl, \
                  'pol_prods is %6.0f' % phsum, 'deg'
        elif abs (phsum) > 10:
            error_condition = True
            print '\n**** WARNING **** phase misclosure on', bl, \
                  'pol_prods is %6.0f' % phsum, 'deg'

        b += 1
    return bproducts





# create output file and write new delay & phase params to it
def write_out (of_name, root, stations, st_tec, ydelays, yphases, control_file):
                                    # open control file & new output
    ifile = open (control_file, 'r')
    orig = ifile.read ()
    ifile.close ()

    ofile = open (of_name, 'w')
    ofile.write (orig)              # copy contents of input cf)

    now = datetime.datetime.now ()
    ofile.write ('* following lines added by fourphase on ' + str (now))
    ofile.write ('\n* by analysis of root file ' + root)

                                    # add lines for every station
    n = 0
    for stn in stations:
        ofile.write ('\nif station ' + stn + '\n')
        ofile.write ('  * following ionosphere was used to derive phase and delay\n')
        ofile.write ('  * ionosphere ' + '%8.3f' % (st_tec[n]) + '\n')
        ofile.write ('  pc_delay_x ' + ' 0.0' + '\n')
        ofile.write ('  pc_delay_y ' + '%8.3f' % (ydelays[n]) + '\n')

        pcpx = '  pc_phase_offset_x ' + ' 0.0'
        pcpy = '  pc_phase_offset_y ' + '%7.1f' % (yphases[n])

        ofile.write (pcpx + '\n')
        ofile.write (pcpy + '\n')
        n += 1
    ofile.close ()
    return

# do consistency tests on 
def test_out_parallel (cf_name, control_file, blines, stations, root, st_tec, verbose=False, max_num_processes=1):
    
    global error_condition
    
    if verbose:
        print '\nchecking derived offset parameters...'

    pol_opts = []
    for n in range(0,len(pol_prods)):
        pol_opts.append('-P'+pol_prods[n])

    bl_setstrings = []
    for bl in blines:
        bline = bl
        sref = stations.index (bl[0])
        srem = stations.index (bl[1])
        setstring = 'set ion_npts 1'
        setstring += ' if station ' + stations[sref] + ' ionosphere ' + str(st_tec[sref])
        setstring += ' if station ' + stations[srem] + ' ionosphere ' + str(st_tec[srem])
        bl_setstrings.append(setstring)
    
    #determine number of jobs we need to run
    n_opts = len(pol_opts)
    n_baselines = len(blines)

    #easiest to just construct a list of arg lists for each process we need to run
    arg_list = []
    for nbase in range(0,n_baselines):
        for npol in range(0,n_opts):
                arg_list.append( [ pol_opts[npol], blines[nbase], control_file, root, False, bl_setstrings[nbase] ] )
                
    #run the threads
    processed_args_list = []
    generated_fringe_files = []
    [generated_fringe_files, processed_args_list] = launch_processes_in_parallel(arg_list, max_num_processes)

    #now go ahead and extract the data from the fringe files
    for bl in blines:               # test baselines one at a time
        mbds = []
        phis = []
        for pp in pol_prods:        # gather data on each pol-prod per bl
            #figure out which file we need to look at
            file_to_read = ""
            for i in range( 0, len(processed_args_list) ):
                p_args = processed_args_list[i]
                pol_prod_opt = '-P'+pp
                if p_args[1] == bl and p_args[0] == pol_prod_opt:
                    file_to_read = generated_fringe_files[i]
            [snr, sb, mb, phi, dtec, amp] = pol_prod_read_fringe( file_to_read )
            mbds.append (mb)
            phis.append (phi)
            if verbose is True:
                print pp, 'mbd', mb, 'phase %6.1f' % phi
                                    # adjust phases if wrap-around is detected
        if abs(phis[0]) + abs(phis[1]) + abs(phis[2]) + abs(phis[3]) > 360:
            for i in range(4):
                if phis[i] < 0:     # modify negative phases only
                    phis[i] += 360
                                    # do rms calculations, maybe issue warnings
        mb_rms = 1e6 * rms (mbds)
        phi_rms = rms (phis)

        if verbose is True:
            print bl, 'rms mbd %7.1f' % mb_rms, 'ps   rms phase %6.1f' % phi_rms, 'deg\n'

        if mb_rms > 25:
            error_condition = True
            print '\n**** ERROR ****', bl, 'mb delay rms of %7.1f' % mb_rms, ' ps is too large'
        elif mb_rms > 10:
            error_condition = True
            print '\n**** WARNING ****', bl, 'mb delay rms of %7.1f' % mb_rms, ' ps is large'

        if phi_rms > 40:
            error_condition = True
            print '\n**** ERROR ****', bl, 'phase rms of %6.1f' % phi_rms, ' deg is too large'
        elif phi_rms > 10:
            error_condition = True
            print '\n**** WARNING ****', bl, 'phase rms of %6.1f' % phi_rms, ' deg is large'
    return


# calculate rms of a numeric list
def rms (x_array):
    sum = 0.0
    for x in x_array:               # find mean of x
        sum += x

    xbar = sum / len (x_array)

    sum = 0.0
    for x in x_array:
        sum += (x-xbar)**2

    return math.sqrt (sum / len(x_array))



def print_table(table):
    # Split input data by row and then on spaces

    max_width = 0
    for row in table:
        for col in row:
            if len(str(col)) > max_width:
                max_width = len(str(col))

    max_width += 2;

    for row in table:
        print "".join(str(word).ljust(max_width) for word in row)
