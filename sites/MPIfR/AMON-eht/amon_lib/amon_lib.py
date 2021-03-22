from __future__ import division # division behaves like in Python 3
import sys
import numpy as np
# local lib
import vex_parser_lib as vex

CONFIGFILE = "amon_config/amon_config.txt"

# a very general function to load config parameters from a file
def load_params_from_file(configfilename, number_of_title_lines = 2, number_of_params = 1):
  with open(configfilename, 'r') as f:
    params = list(f) # all file lines
    # first nnn lines are the title and general comments, they are ignored
    param_list=[]
    for index in range(number_of_title_lines, number_of_title_lines+number_of_params):
      if not params[index]:
        param_list.append("")
      elif params[index][0]== " " or params[index][0]== "\n" or params[index][0]== "\r" or params[index][0]== "\t" or params[index].split()[0]=='#':
        param_list.append("") # also empty
      else:
        prm = []
        for el in params[index].split():
          if el[0]=='#': # that's it, comments starting
            break
          else:
            prm.append(el)
        if len(prm) == 1:
          param_list.append(prm[0])
        else:
          param_list.append(tuple(prm)) 
    del params
  return param_list

# just getting and checking the filename
def get_filenames():
  set_params = load_params_from_command_line(2)
  filename1 = set_params[0] # filename of the .alist to process (or for .vex, if it is the only one)
  filename2 = set_params[1] # filename of the .vex to process
  # checking if valid filename
  help_info = """
####################################################################################################
                                      RUNNING THE SCRIPT:

python amon.py <.alist file name>  <.vex file name>

python amon.py <.vex file name> (will display only inactive elements based on .vex)  

python amon.py (displays only this part of the help)   

Python 2.7 or later is assumed, this version WON'T run with Python 3. 
Standard Python libraries required for this script are: sys, os, shutil, subprocess, 
                                                        numpy, matplotlib, Tkinter.
They are typically included in most Python distributions.  

Many important features of this program will work only if hops fplot and gs are available.

To run, this version of AMON must include the following files (all files except amon.py
are placed in two subdirectories, amon_lib and amon_config):

amon.py                                                                  main script
amon_lib: amon_lib.py, vex_parser_lib.py, plt_lbls_lib.py, __init__.py   libraries
amon_config/amon_config.txt                                              the configuration file
amon_config/amon_help.txt (this name can be changed in the config file)  this help file
amon_config/gmva_codes (this name can be changed in the config file)     one letter - two letter
                                                                         antenna code
                                                                         correspondence file        
      
####################################################################################################
  """
  if not filename1 and not filename2: # this won't happen since the main script already checks if there's
                                      # at least one argument, but keep this here for possible future use
    print(help_info)
    sys.exit()
  elif filename1 and filename2:
    try:
      f=open(filename1, 'r')
      f.close()
    #except FileNotFoundError:
    except IOError:
      print '\nCannot open the file you requested:', filename1
      print(help_info)
      sys.exit()  
    try:
      f=open(filename2, 'r')
      f.close()
    #except FileNotFoundError:
    except IOError:
      print '\nCannot open the file you requested:', filename2
      print(help_info)
      sys.exit()
  else: # only one filename was provided, assume it is .vex
    filename2 = filename1
    filename1 = ''
    print '\nOnly one argument was provided, assume this is a .vex file:', filename2
    try:
      f=open(filename2, 'r')
      f.close()
    #except FileNotFoundError:
    except IOError:
      print '\nCannot open the file you requested:', filename2
      print(help_info)
      sys.exit()  
  return filename1, filename2
  
# a very general function to load config parameters from the command line
# first actual entered command line parameters are taken
# if not enough, default tuple is used
# finally, the same replacement value is added, until all number_of_parameters are created
def load_params_from_command_line(number_of_parameters = 1, defaults = (), to_replace_with=''):
  param_list=[]
  for index in range(1,number_of_parameters+1):
    try:
      param_list.append(sys.argv[index])
    except IndexError: # no more command line parameters
      try:
        param_list.append(defaults[index-1])
      except IndexError: # no more supplied defaults in the tuple
        param_list.append(to_replace_with)
  return param_list
  

def keep_only_some_rows(start_row, no_to_keep, labels, fringes, errors):
  """
      keeps only rows from start_row to start_row+no_to_keep in labels, fringes and errors
  """
  labels_new = labels[start_row:start_row+no_to_keep]
  fringes_new = fringes[start_row:start_row+no_to_keep, :, :, :]
  errors_new = errors[start_row:start_row+no_to_keep, :, :]
  return labels_new, fringes_new, errors_new

def test_an_array(arr, num, maxnum):
  """
      tests a numpy array assumed to be of numbers
      on how many of its elements are NOT equal to num
      If this number does not exceed maxnum, True is returned
      otherwise -- False.
      This ensures that it won't crash tkinter with too many widgets.
  """
  if np.count_nonzero(arr-num)+arr.shape[0]*2+arr.shape[1]*3 <= maxnum: return True # taking labels into account
  else: return False
  
def keep_only_some_elements(ref_ant, source, start_row, labels, baselines, fringes, errors, empty_val = -10, max_elements = 9500):
  """
     This function first keeps only those rows and columns of labels, baselines, fringes, errors
     that correspond to the given ref_ant and source values. If ref_ant and/or source are '', all corresponding rows/columns are retained
     Then the fringes array is tested. If the number of non-trivial (not equal to empty_val) elements are more than max_elements,
     the number of rows is diminished until the number of non-trivial elements is small enough
  """
  labels_new = labels[:]
  baselines_new =  baselines[:]
  fringes_new = fringes[:]
  errors_new = errors[:]
  ### basic reshape, just removing the unnecessary without regard of how big this gets
  if source:
    labels_new = []
    row_counter = 0
    rows_to_keep = []
    for label in labels:
      if label[5:] == source or (label[4]=='#' and label[7:] == source): # with dupl case
        labels_new.append(label)
        rows_to_keep.append(row_counter) 
      row_counter += 1
    fringes_new = np.take(fringes_new, rows_to_keep, axis = 0)
    errors_new = np.take(errors_new, rows_to_keep, axis = 0)
  if ref_ant:
    baselines_new = []
    col_counter = 0
    cols_to_keep = []
    for baseline in baselines:
      if ref_ant in baseline:
        baselines_new.append(baseline)
        cols_to_keep.append(col_counter*2) # for both pols
        cols_to_keep.append(col_counter*2+1)
      col_counter += 1
    fringes_new = np.take(fringes_new, cols_to_keep, axis = 1)
    errors_new = np.take(errors_new, cols_to_keep, axis = 1)
    ##### removing the rows where ref. antenna is not present
    rows_to_keep = []
    labels_newnew = []
    for rw in range(fringes_new.shape[0]):
      if np.count_nonzero(fringes_new[rw,:,0,0] - empty_val): 
        rows_to_keep.append(rw)
        labels_newnew.append(labels_new[rw])
    fringes_new = np.take(fringes_new, rows_to_keep, axis = 0)
    errors_new = np.take(errors_new, rows_to_keep, axis = 0)
    labels_new = labels_newnew[:]
  ### end basic reshape
  ### now let's consern ourselves with the size
  if not test_an_array(fringes_new[:, :, 0, 0], empty_val, max_elements): # need to scale down
    entry = True
    no_to_keep = len(labels_new) # first very rough guess, luckily Python ignores overshooting in array indices 
    labels_newnew, fringes_newnew, errors_newnew = keep_only_some_rows(start_row, no_to_keep, labels_new, fringes_new, errors_new)
    no_to_keep = len(labels_newnew)
    while not test_an_array(fringes_newnew[:, :, 0, 0], empty_val, max_elements):
      no_to_keep -= 1
      labels_newnew, fringes_newnew, errors_newnew = keep_only_some_rows(start_row, no_to_keep, labels_new, fringes_new, errors_new)
    return entry, start_row, no_to_keep, labels_newnew, baselines_new, fringes_newnew, errors_newnew
  else: # no need to scale down, ignore start_row, no_to_keep
    entry = False
    start_row = 0
    no_to_keep = len(labels_new)
    return entry, start_row, no_to_keep, labels_new, baselines_new, fringes_new, errors_new

def keep_only_some_elements_PRINT(ref_ant, source, labels, baselines, fringes, errors, empty_val = -10):
  """
     This function first keeps only those rows and columns of labels, baselines, fringes, errors
     that correspond to the given ref_ant and source values. If ref_ant and/or source are '', all corresponding rows/columns are retained
     Print version of keep_only_some_elements, here we do not care about the size and keep ALL the rows always
  """
  labels_new = labels[:]
  baselines_new =  baselines[:]
  fringes_new = fringes[:]
  errors_new = errors[:]
  if source:
    labels_new = []
    row_counter = 0
    rows_to_keep = []
    for label in labels:
      if label[5:] == source or (label[4]=='#' and label[7:] == source): # with dupl case
        labels_new.append(label)
        rows_to_keep.append(row_counter) 
      row_counter += 1
    fringes_new = np.take(fringes_new, rows_to_keep, axis = 0)
    errors_new = np.take(errors_new, rows_to_keep, axis = 0)
  if ref_ant:
    baselines_new = []
    col_counter = 0
    cols_to_keep = []
    for baseline in baselines:
      if ref_ant in baseline:
        baselines_new.append(baseline)
        cols_to_keep.append(col_counter*2) # for both pols
        cols_to_keep.append(col_counter*2+1)
      col_counter += 1
    fringes_new = np.take(fringes_new, cols_to_keep, axis = 1)
    errors_new = np.take(errors_new, cols_to_keep, axis = 1) 
    ##### removing the rows where ref. antenna is not present
    rows_to_keep = []
    labels_newnew = []
    for rw in range(fringes_new.shape[0]):
      if np.count_nonzero(fringes_new[rw,:,0,0] - empty_val): 
        rows_to_keep.append(rw)
        labels_newnew.append(labels_new[rw])
    fringes_new = np.take(fringes_new, rows_to_keep, axis = 0)
    errors_new = np.take(errors_new, rows_to_keep, axis = 0)
    labels_new = labels_newnew[:]
  return labels_new, baselines_new, fringes_new, errors_new
    
def translate_to_0_9(value, min, max, backwards = False, log_map = False):
  """
      maps any number onto a discrete range of 0-9
      based on simple rules.
      if min is smaller than max, then 0 corresponds to min and less,
      9 to max and more, and the rest is spread in between either
      proportionally or logarithmically (if flag log_map is True).
      If flag backwards is True, then 9 corresponds to min and 0 to max.
  """
  ## if min and max mixed up, make sure they are in order
  if min > max: # exchange their places
    max_ = max
    max = min
    min = max_
  if not np.isfinite(min) or not np.isfinite(max) or not np.isfinite(value) or min == max: # one of them is nan or inf, or there is no interval between
    return np.nan
  if log_map and (min <= 0 or max <= 0 or value <= 0): # logarithmic scale works only with positive values
    return np.nan
  # processing the value
  if value <= min:
    if backwards: return 9
    else: return 0
  elif value >= max:
    if backwards: return 0
    else: return 9
  else:
    if log_map:
      if backwards:
        return 9 - int(8.*(np.log(value) - np.log(min))/(np.log(max) - np.log(min)))
      else:
        return int(8.*(np.log(value) - np.log(min))/(np.log(max) - np.log(min))) + 1
    else:
      if backwards:
        return 9 - int(8.*(value - min)/(max - min))
      else:
        return int(8.*(value - min)/(max - min)) + 1
    
def fill_XPOL(fr, er):
  for row in range(fr.shape[0]):
    for bsl in range(fr.shape[1]//2):
      if fr[row, bsl*2, 0, 0] >= 0 or fr[row, bsl*2+1, 0, 0] >= 0: #at least one LL or RR correlation exists
        try:
          SNR_LL = float(er[row, bsl*2, 0].split()[1])
        except IndexError: #not full pol
          SNR_LL = np.nan
        try:
          SNR_RR = float(er[row, bsl*2+1, 0].split()[1])
        except IndexError: #not full pol
          SNR_RR = np.nan
        try:
          SNR_LR = float(er[row, bsl*2, 1].split()[1])
        except IndexError: #not full pol
          SNR_LR = np.nan
        try:
          SNR_RL = float(er[row, bsl*2+1, 1].split()[1])
        except IndexError: #not full pol
          SNR_RL = np.nan
        ### keep all 4 SNR values in the error string for each cell
        stval = " %s %s %s %s" % (SNR_LL, SNR_RR, SNR_LR, SNR_RL)
        er[row, bsl*2, 0] += stval
        er[row, bsl*2+1, 0] += stval
        er[row, bsl*2, 1] += stval
        er[row, bsl*2+1, 1] += stval
        ### general case
        fr[row, bsl*2, 0, 1] = translate_to_0_9(2*SNR_LL/(SNR_LR + SNR_RL), 1.15, 5, backwards = False, log_map = True)
        fr[row, bsl*2+1, 0, 1] = translate_to_0_9(2*SNR_RR/(SNR_LR + SNR_RL), 1.15, 5, backwards = False, log_map = True) 
        fr[row, bsl*2, 1, 1] = translate_to_0_9(2*SNR_LR/(SNR_LL + SNR_RR), 1.15, 5, backwards = False, log_map = True) 
        fr[row, bsl*2+1, 1, 1] = translate_to_0_9(2*SNR_RL/(SNR_LL + SNR_RR), 1.15, 5, backwards = False, log_map = True)
        ### special cases
        ### only one cross fringe, we care only about it
        ### case LR
        if (fr[row, bsl*2, 1, 0] > 0 and fr[row, bsl*2+1, 1, 0] <= 0) or (fr[row, bsl*2, 1, 0] == 0 and fr[row, bsl*2+1, 1, 0] < 0):
          fr[row, bsl*2, 0, 1] = translate_to_0_9(SNR_LL/SNR_LR, 1.15, 5, backwards = False, log_map = True) 
          fr[row, bsl*2+1, 0, 1] = translate_to_0_9(SNR_RR/SNR_LR, 1.15, 5, backwards = False, log_map = True) 
        ### case RL
        if (fr[row, bsl*2, 1, 0] <= 0 and fr[row, bsl*2+1, 1, 0] > 0) or (fr[row, bsl*2, 1, 0] < 0 and fr[row, bsl*2+1, 1, 0] == 0):
          fr[row, bsl*2, 0, 1] = translate_to_0_9(SNR_LL/SNR_RL, 1.15, 5, backwards = False, log_map = True) 
          fr[row, bsl*2+1, 0, 1] = translate_to_0_9(SNR_RR/SNR_RL, 1.15, 5, backwards = False, log_map = True) 
        ##### cases with very different SNRs
        if np.isfinite(SNR_LR) and np.isfinite(SNR_RL):
          if (fr[row, bsl*2, 1, 0] > 0 and fr[row, bsl*2+1, 1, 0] > 0) and SNR_LR > 3*SNR_RL: # both fringes exist, but one SNR is much larger
            fr[row, bsl*2, 0, 1] = translate_to_0_9(SNR_LL/SNR_LR, 1.15, 5, backwards = False, log_map = True) 
            fr[row, bsl*2+1, 0, 1] = translate_to_0_9(SNR_RR/SNR_LR, 1.15, 5, backwards = False, log_map = True) 
          if (fr[row, bsl*2, 1, 0] > 0 and fr[row, bsl*2+1, 1, 0] > 0) and SNR_RL > 3*SNR_LR: # both fringes exist, but one SNR is much larger
            fr[row, bsl*2, 0, 1] = translate_to_0_9(SNR_LL/SNR_RL, 1.15, 5, backwards = False, log_map = True) 
            fr[row, bsl*2+1, 0, 1] = translate_to_0_9(SNR_RR/SNR_RL, 1.15, 5, backwards = False, log_map = True) 
        ### only one parallel fringe, we care only about it
        ### case LL
        if (fr[row, bsl*2, 0, 0] > 0 and fr[row, bsl*2+1, 0, 0] <= 0) or (fr[row, bsl*2, 0, 0] == 0 and fr[row, bsl*2+1, 0, 0] < 0) :
          fr[row, bsl*2, 1, 1] = translate_to_0_9(SNR_LR/SNR_LL, 1.15, 5, backwards = False, log_map = True) 
          fr[row, bsl*2+1, 1, 1] = translate_to_0_9(SNR_RL/SNR_LL, 1.15, 5, backwards = False, log_map = True) 
        ### case RR
        if (fr[row, bsl*2, 0, 0] <= 0 and fr[row, bsl*2+1, 0, 0] > 0) or (fr[row, bsl*2, 0, 0] < 0 and fr[row, bsl*2+1, 0, 0] == 0):
          fr[row, bsl*2, 1, 1] = translate_to_0_9(SNR_LR/SNR_RR, 1.15, 5, backwards = False, log_map = True) 
          fr[row, bsl*2+1, 1, 1] = translate_to_0_9(SNR_RL/SNR_RR, 1.15, 5, backwards = False, log_map = True)
        ##### cases with very different SNRs
        if np.isfinite(SNR_LL) and np.isfinite(SNR_RR):
          if (fr[row, bsl*2, 0, 0] > 0 and fr[row, bsl*2+1, 0, 0] > 0) and SNR_LL > 3*SNR_RR: # both fringes exist, but one SNR is much larger
            fr[row, bsl*2, 1, 1] = translate_to_0_9(SNR_LR/SNR_LL, 1.15, 5, backwards = False, log_map = True) 
            fr[row, bsl*2+1, 1, 1] = translate_to_0_9(SNR_RL/SNR_LL, 1.15, 5, backwards = False, log_map = True) 
          if (fr[row, bsl*2, 0, 0] > 0 and fr[row, bsl*2+1, 0, 0] > 0) and SNR_RR > 3*SNR_LL: # both fringes exist, but one SNR is much larger
            fr[row, bsl*2, 1, 1] = translate_to_0_9(SNR_LR/SNR_RR, 1.15, 5, backwards = False, log_map = True) 
            fr[row, bsl*2+1, 1, 1] = translate_to_0_9(SNR_RL/SNR_RR, 1.15, 5, backwards = False, log_map = True) 
        ### no crosses, but at least one parallel
        if fr[row, bsl*2, 1, 0] <= 0 and fr[row, bsl*2+1, 1, 0] <= 0:
          if fr[row, bsl*2, 0, 0] > 0:
            fr[row, bsl*2, 0, 1] = 9
            fr[row, bsl*2, 1, 1] = 0
            fr[row, bsl*2+1, 1, 1] = 0
          if fr[row, bsl*2+1, 0, 0] > 0:
            fr[row, bsl*2+1, 0, 1] = 9
            fr[row, bsl*2, 1, 1] = 0
            fr[row, bsl*2+1, 1, 1] = 0
        ### no parallels, but at least one cross
        if fr[row, bsl*2, 0, 0] <= 0 and fr[row, bsl*2+1, 0, 0] <= 0:
          if fr[row, bsl*2, 1, 0] > 0:
            fr[row, bsl*2, 1, 1] = 9
            fr[row, bsl*2, 0, 1] = 0
            fr[row, bsl*2+1, 0, 1] = 0
          if fr[row, bsl*2+1, 1, 0] > 0:
            fr[row, bsl*2+1, 1, 1] = 9
            fr[row, bsl*2, 0, 1] = 0
            fr[row, bsl*2+1, 0, 1] = 0 
  return fr, er


def handle_duplicates(labels, fringes, errors, dupl): # handling duplicate entries from the same scan
  rows = []
  correction = 0 # what to add to all array row indices
  for ind in sorted(dupl.keys()):
    if ind[0] not in rows: # new row, need to create a gap in all the arrays
      rows.append(ind[0])
      old_label = labels[ind[0]+correction]
      labels[ind[0]+correction] = old_label+'#1'+old_label
      labels.insert(ind[0]+correction+1, old_label+'#2'+old_label)
      fringes = np.insert(fringes, ind[0]+correction+1, -10, axis = 0)
      errors = np.insert(errors, ind[0]+correction+1, '', axis = 0)
      correction+=1
    #else: # gap already created
    fringes[ind[0]+correction, ind[1], ind[2], 0] = dupl[ind][0]
    fringes[ind[0]+correction, ind[1], ind[2], 2] = dupl[ind][1]
    fringes[ind[0]+correction, ind[1], ind[2], 3] = dupl[ind][2]
    errors[ind[0]+correction, ind[1], ind[2]] = dupl[ind][3]
  return labels, fringes, errors
            

def load_data_alist_vex(alistfilename, vexfilename, launch_path): # alistfilename may be empty "" in vex-only mode
  print("\nInitializing... Please wait a few seconds...\n")

  # load config parameters from file
  set_params = load_params_from_file(configfilename = launch_path +'/'+ CONFIGFILE, number_of_params = 22)
  
  #no_title_lines=int(set_params[0]) # number of title lines in the alist file
  # starting from version 1.4b determine no_title_lines automatically
  
  ### COLUMN NUMBERING STARTS WITH 0 !!! ###
  scan_num_col = int(set_params[1])  # scan num column #
  source_col   = int(set_params[2])  # source name column #
  baseline_col = int(set_params[3])  # baseline column #
  fringe_col   = int(set_params[4])  # fringe quality and error column #
  pol_col      = int(set_params[5])  # polarization column #
  SNR_col      = int(set_params[6])  # SNR column #
  SBD_col      = int(set_params[7])  # SBD column #
  DRATE_col    = int(set_params[8])  # DRATE column #
  filecode_col = int(set_params[9])  # filecode (zymnxc)
  filenum_col  = int(set_params[10]) # filenumb (in XP.W.18.zymnxc)
  bandchan_col = int(set_params[11]) # band letter code and number of channels (W08)
  root_size    = (int(set_params[12][0]), int(set_params[12][1])) # root win size horizontal and vertical
  #### smaller canvas size:
  canvas_size = (root_size[0] - 26, root_size[1] - 138)
  
  SBDthreshold = float(set_params[13]) # SBD threshold for "bad"
  DRATEthreshold = float(set_params[14]) # DRATE threshold for "bad"
  codesfilename = set_params[15]     # one-letter to two-letter antenna code translation file
  codesfilename =  launch_path +'/'+ codesfilename
  helpfilename  = set_params[16]      # help file
  helpfilename =  launch_path +'/'+ helpfilename
  help_size     = (int(set_params[17][0]), int(set_params[17][1])) # help win size horizontal and vertical
  pathadd       = set_params[18]           # additional path to the No0001 etc. directories with the fourfit output
  if pathadd[-1] != "/": pathadd += "/" # must end with /
  hopspath = set_params[19]          # path to where the hops scripts are located
  if hopspath[-1] != "/": hopspath += "/" # must end with /
  ants_to_ignore = set_params[20]
  if not ants_to_ignore:
    ants_to_ignore = []
  elif isinstance(ants_to_ignore, basestring): # if it is a string, not a list, happens when only one antenna selected
    _ = ants_to_ignore
    ants_to_ignore = []
    ants_to_ignore.append(_)
  
  # new since v1.4b
  ref_freq_col = int(set_params[21]) # ref freq col

  # RUN 0: collecting info on scans and baselines from the .vex file
  #print ("\nLooking at the .vex scans...\n")
  
  vex_antennas = vex.two2one_stations(vexfilename, codesfilename) # separate vex station list for convenience
  vex_antennas.sort()
  vex_sources = sorted(vex.sources(vexfilename))
  vex_exp_name = vex.exper(vexfilename)
  codesdict = vex.one2two_code(codesfilename) # dict to translate one-letter codes into the two-letter ones
  codesdict_back = vex.two2one_code(codesfilename) # dict to translate two-letter codes into the one-letter ones
  vextimedict = vex.timedict(vexfilename)
  
  #### deal with ants to ignore
  ants_to_ignore_new = []
  for an in ants_to_ignore:
    if len(an) == 1 and an in vex_antennas:
      ants_to_ignore_new.append(an) # legitimate antenna, present in the vex file
    elif len(an) == 2:
      try:
        ann = codesdict_back[an]
        if ann in vex_antennas:
          ants_to_ignore_new.append(ann) # legitimate antenna, present in the vex file
      except KeyError:
        pass # not known, ignore
    # longer than 2 symb antenna designations -- ignore
  ants_to_ignore = ants_to_ignore_new
  vex_dict = vex.bsl_two2one_in_vex(vexfilename, codesfilename) # dictionary with keys -- labels in format like '0001:BLLAC'
                                                                # and values -- lists of all the baselines for that scan/source
                                                                # all according to the vex file
  if ants_to_ignore:
    vex_antennas = [_ for _ in vex_antennas if _ not in ants_to_ignore] # remove all ignored ants
    vex_dict_new = {k:[_ for _ in v if _[0] not in ants_to_ignore and _[1] not in ants_to_ignore] \
                    for k,v in vex_dict.items()} # remove all ignored ants
    vex_dict_newnew = {k:v for k,v in vex_dict_new.items() if v !=[]} # if removing ignored ants made whole scans empty, we ignore them too
    vex_dict = vex_dict_newnew

  vex_baselines = vex.ant2bsl(vex_antennas) # all baselines according to vex
  vex_labels = sorted(vex_dict.keys()) # labels only
  ##### finished processing ignored ants and vex stuff
  
  # RUN 1: collecting info on scans and baselines from the .alist file
  
  alist_labels = [] # init
  alist_baselines = [] # init
  alist_antennas = [] # init
  #print ("\nLooking at the .alist scans...\n")
  if alistfilename: # normal mode, alist present
    with open(alistfilename, 'r') as mainf:
      #linecounter = 0
      line = '*'
      #for _ in range(no_title_lines+1): #skip the title, +1 sets the line variable to be the first data line to be read in the next cycle
      #  line = mainf.readline()
      #  linecounter += 1
      while (line[0] == '*'): # alist title lines are marked by * in the first position
        line = mainf.readline()
        #linecounter += 1
      while (line != "\n" and line != ""):
        try:
          if (line[0] == '*'):
            # print('Unexpected comment line encountered after header lines, ignoring it: %s' % (line.strip()))
            line = mainf.readline()
            continue
          baseline = line.split()[baseline_col]
        except:
          print('Could not split for col %d of line: %s' % (baseline_col,line))
        if baseline[0] != baseline[1] and baseline[0] not in ants_to_ignore and baseline[1] not in ants_to_ignore: # ignore autocorr or ignoret ant if present
          scan_name = line.split()[scan_num_col]
          source = line.split()[source_col]
          label = "%s:%s" % (scan_name, source)
          if label not in alist_labels: # collect all the unique labels in a list
            alist_labels.append(label)
          antenna1 = baseline[0]
          antenna2 = baseline[1]
          if baseline not in alist_baselines and (antenna2+antenna1) not in alist_baselines: # collect all the unique baselines in a list
            alist_baselines.append(baseline)                                           # remembering that XY and YX is the same baseline
          if antenna1 not in alist_antennas:
            alist_antennas.append(antenna1)  # same for antennas
          if antenna2 not in alist_antennas:
            alist_antennas.append(antenna2)
        #if linecounter % 10000 == 0:
          #pass
          #print("at line", linecounter)
        line = mainf.readline()
        #linecounter += 1
    # comparing the .alist and .vex baselines
    baselines = vex.compare_bslns(vex_baselines, alist_baselines) # this checks if the vex and alist are compatible
                                                                  # and makes sure that vex baselines conform to the existing alist baselines 
  else: # vex-only mode
    baselines = vex_baselines
    labels = vex_labels
  
  ### PREPARING THE DATABASE, arrays fringes and errors
  no_rows = len(vex_labels)
  no_columns = len(baselines)
  # array to accumulate fringe quality numbers and other number data
  fringes = np.empty((no_rows, no_columns*2, 2, 4)) # rows -- scans, cols -- baselines, 3d position -- pol, 4th -- mode: fringe, xpol, sbd or drate
  fringes[:] = -10 #init
  # array to accumulate error codes and other info
  errors = np.array(np.empty((no_rows, no_columns*2, 2)),dtype=object) # weird type declaration necessary
  errors[:] = '' # init
  # fill the fringes array in accordance with the vex file, -5 will mark those positions that should have existed based on vex
  for i in range(no_rows):
    for baseline in vex_dict[vex_labels[i]]:
      # left pol position
      try: # should not be necessary, but safer
        deduced_column = baselines.index(baseline)*2    
      except ValueError: # look for a baseline with the reversed order of antennas   
        deduced_column = baselines.index(baseline[1]+baseline[0])*2 
      fringes[i, deduced_column] = -5 # all pols and modes
      scanname = vex_labels[i].split(':')[0]
      errors[i, deduced_column] = vextimedict[scanname] #timestamp
      # and right pol position
      fringes[i, deduced_column+1] = -5
      errors[i, deduced_column+1] = vextimedict[scanname] #timestamp
    
  fplot_files = [] # to collect all the fplot files
  ###### END OF PREPARATION
  
  # RUN 2: filling the database  
  
  dupl = {} # to collect duplicate entries
  warnedaboutduplscan = []

  flag_linear_pol = False
  translate_lin_pol_dict = {'XX':'LL', 'XY':'LR', 'YX':'RL', 'YY':'RR',
                            'XL':'LL', 'XR':'LR', 'LX':'LL', 'RX':'RL',
                            'YL':'RL', 'YR':'RR', 'LY':'LR', 'RY':'RR' }
          
  if alistfilename:
    with open(alistfilename, 'r') as mainf:
      line = '*'
      #for _ in range(no_title_lines+1): #skip the title, +1 sets the line variable to be the first data line to be read in the next cycle
      #  line = mainf.readline()
      #  linecounter += 1
      while (line[0] == '*'): # alist title lines are marked by * in the first position
        line = mainf.readline()
        #linecounter += 1
      while (line != "\n" and line != ""):
        if (line[0] == '*'):
          # print('Unexpected comment line encountered after header lines, ignoring it: %s' % (line.strip()))
          line = mainf.readline()
          continue
        baseline = line.split()[baseline_col]
        if baseline[0] != baseline[1] and baseline[0] not in ants_to_ignore and baseline[1] not in ants_to_ignore: # ignore autocorr or ignored ant if present
          scan = line.split()[scan_num_col]
          scan_name = line.split()[scan_num_col]
          source  = line.split()[source_col]
          label = "%s:%s" % (scan_name, source)
          fringe_quality = int(line.split()[fringe_col][0])
          try:
            error_code = line.split()[fringe_col][1]  
          except IndexError: # no error code
            error_code = '0'
          pol      = line.split()[pol_col]
          if 'X' in pol or 'Y' in pol: # an antenna with linear pol is in the set!
            if not flag_linear_pol:
              flag_linear_pol = True
              print "\nA linear-polarization antenna is detected, hope you know what you're doing!"
              print "Arbitrarily reassigning X as L and Y as R, be careful with interpreting these results!\n"
            pol = translate_lin_pol_dict[pol]
          if pol == 'LL' or pol == 'LR': 
            try: # should not be necessary, but safer
              deduced_column = baselines.index(baseline)*2    
            except ValueError: # look for a baseline with the reversed order of antennas   
              deduced_column = baselines.index(baseline[1]+baseline[0])*2 
          else:
            try:
              deduced_column = baselines.index(baseline)*2+1    
            except ValueError: # look for a baseline with the reversed order of antennas   
              deduced_column = baselines.index(baseline[1]+baseline[0])*2+1 
          deduced_row = vex_labels.index(label)
          if pol == 'LL' or pol == 'RR':
            pol_choice = 0 # main choice
          else:
            pol_choice = 1 # cross pols
          #various properties of the current element
          filecode = line.split()[filecode_col]
          filenum = line.split()[filenum_col]
          bandletter = line.split()[bandchan_col][0]
          secret_filename = scan+'/'+baseline + '.' + bandletter +'.' + filenum + '.' + filecode 
          fplot_files.append(secret_filename)
          SNR = line.split()[SNR_col]
          SBD = line.split()[SBD_col]
          DRATE = line.split()[DRATE_col]
          RefFreq = line.split()[ref_freq_col]
          #################################
          ### recording the main arrays ###
          #################################
          if fringes[deduced_row, deduced_column, pol_choice, 0] >= 0: # already recorded, duplicate scan!
            if scan not in warnedaboutduplscan:
              print "WARNING: scan", scan, "was broken into at least two correlation jobs!"
              warnedaboutduplscan.append(scan)
            if (deduced_row, deduced_column, pol_choice, 0) in dupl.keys(): # more than two correlator jobs for one scan? can this even happen?
              print "!!! WARNING !!!"
              print "!!! Actually, scan", scan, "was broken into MORE than two correlation jobs !!!"
              print "!!! Cannot handle this, some data won't be displayed !!!"
              print "!!! You'll have to manually check fplot file !!!",  secret_filename
            else:
              timecode = vextimedict[vex_labels[deduced_row]]
              dupl[(deduced_row, deduced_column, pol_choice)] = (fringe_quality,\
                                                                 translate_to_0_9(np.abs(float(SBD)), 0.01, SBDthreshold, backwards = True, log_map = False),\
                                                                 translate_to_0_9(np.abs(float(DRATE)), 0.01, DRATEthreshold, backwards = True, log_map = False),\
                                                                 error_code+secret_filename+' '+SNR+' '+SBD+' ' + DRATE + ' ' + RefFreq + ' ' + timecode)
          else:
            fringes[deduced_row, deduced_column, pol_choice, 0] = fringe_quality
            # SBD mode data
            fringes[deduced_row, deduced_column, pol_choice, 2] = translate_to_0_9(np.abs(float(SBD)), 0.01, SBDthreshold, backwards = True, log_map = False)
            # DRATE mode data
            fringes[deduced_row, deduced_column, pol_choice, 3] = translate_to_0_9(np.abs(float(DRATE)), 0.01, DRATEthreshold, backwards = True, log_map = False)
            timecode = errors[deduced_row, deduced_column, pol_choice] # presaved timecode
            errors[deduced_row, deduced_column, pol_choice] = error_code+secret_filename+' '+SNR+' '+SBD+' ' + DRATE + ' ' + RefFreq + ' ' + timecode
          #if linecounter % 10000 == 0:
          #  print("at line", linecounter)
        line = mainf.readline()
        #linecounter += 1
    labels = vex_labels # in case nothing changes, will use vex ones as the final labels
    if dupl: # some duplicates were detected
      labels, fringes, errors = handle_duplicates(labels, fringes, errors, dupl)
  fringes, errors = fill_XPOL(fringes, errors) # take care of XPOL
  print "\n"
  return vex_exp_name, labels, vex_sources, baselines, vex_antennas, fringes, errors,\
         root_size, canvas_size, help_size, launch_path +'/'+ CONFIGFILE,\
         codesfilename, codesdict, helpfilename, pathadd, hopspath, fplot_files
