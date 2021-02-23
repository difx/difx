from __future__ import division # division behaves like in Python 3

# a collection of functions useful for paring vex-files
def ant2bsl(ant):
  """
     for a given list of antennas, assumed to be a list of strings,
     not necessarily unique, find all the baselines
     (assuming AB = BA, of course, i. e. ['A', 'B', 'C'] => ['AB', 'AC', 'BC']).
     If the list includes '' (empty string), it is ignored.  
     The list is sorted beforehand.
  """
  ant_set = list(set(ant)) # to ensure uniqueness
  ant_set.sort()
  try: 
    ant_set.remove('') # we cannot use empty strings!
  except ValueError:
    pass # no empty strings
  if len(ant_set) < 2: # min 2 antennas for a baseline
    return []
  else:
    to_pair = ant_set[:]
    bsl = []
    for a in ant_set:
      to_pair.remove(a)
      for b in to_pair:
        bsl.append(a+b)
    return bsl

def filename_check(filename):
  """
     checks the filename to be a non-empty string
     and then tries to open it.
     Returns true if everything works and false otherwise.
  """
  if not filename or filename != str(filename): # not string
    return False
  else:
    try:
      f=open(filename, 'r')
    except FileNotFoundError:
      return False
    f.close()
    return True
    
def one2two_code(file = 'gmva_codes'):
  """
     Creates a dictionary translating list of one-letter station codes,
     (the list is not assumed to contain unique items, duplicates are excluded,
     and all the strings are checked to be exactly one symbol long),
     into two-letter codes, using the correspondence in the file provided.
     Each file line must be one letter, space(s), two letters, spase(s)/endline, anything
     e. g.: "a Aa" or "a Aa ALMA". Empty lines are ignored.
  """
  # checking if valid filename
  if not filename_check(file):
    raise Exception('Invalid filename for the GMVA letter code file!')
  else:
    code_dict = {}
    with open(file, 'r') as codes:
      line = codes.readline() # to enter the cycle
      while line and line != '\n' and line != '\r' and line != '\r\n': # handle possible OS newline differences just in case
        one_letter = line.split()[0]
        if len(one_letter) != 1: raise Exception('Invalid format for the GMVA letter code file! Lines like "a Aa" or "a Aa ALMA" expected.')
        try:
          two_letter = line.split()[1]
        except IndexError:
          raise Exception('Invalid format for the GMVA letter code file! Lines like "a Aa" or "a Aa ALMA" expected.')
        if len(two_letter) != 2: raise Exception('Invalid format for the GMVA letter code file! Lines like "a Aa" or "a Aa ALMA" expected.')
        code_dict[one_letter] = two_letter
        line = codes.readline()
    return code_dict

def two2one_code(file = 'gmva_codes'):
  """
     Creates a dictionary translating list of two-letter station codes,
     (the list is not assumed to contain unique items, duplicates are excluded,
     and all the strings are checked to be exactly one symbol long),
     into one-letter codes, using the correspondence in the file provided.
     Each file line must be one letter, space(s), two letters, spase(s)/endline, anything
     e. g.: "a Aa" or "a Aa ALMA". Empty lines are ignored.
  """
  # checking if valid filename
  if not filename_check(file):
    raise Exception('Invalid filename for the GMVA letter code file!')
  else:
    code_dict = {}
    with open(file, 'r') as codes:
      line = codes.readline() # to enter the cycle
      while line and line != '\n' and line != '\r' and line != '\r\n': # handle possible OS newline differences just in case
        one_letter = line.split()[0]
        if len(one_letter) != 1: raise Exception('Invalid format for the GMVA letter code file! Lines like "a Aa" or "a Aa ALMA" expected.')
        try:
          two_letter = line.split()[1]
        except IndexError:
          raise Exception('Invalid format for the GMVA letter code file! Lines like "a Aa" or "a Aa ALMA" expected.')
        if len(two_letter) != 2: raise Exception('Invalid format for the GMVA letter code file! Lines like "a Aa" or "a Aa ALMA" expected.')
        code_dict[two_letter] = one_letter
        line = codes.readline()
    return code_dict

def conv_list_with_dict(var_list, var_dict):
  """
      given a list and a dictionary with values related to that list 
      (like two-letter codes and one-letter dict)
      converts list using that dictionary
  """
  try:
    return [var_dict[_] for _ in var_list]
  except KeyError:
    raise Exception('The list and the dictionary are incompatible!\n\nPerhaps the vex-file contains a new antenna, which is not mentioned in the single-letter code file!')

def timedict(vexfile):
  """
     creates a dictionary with scans numbers in the form "00XX" as keys
     and timestamps "2017y273d19h00m00s" as values
  """
  if not filename_check(vexfile):
    raise Exception('Invalid vex filename!')
  else:
    dct = {}
    with open(vexfile, 'r') as vex:
      line = vex.readline() # to enter the cycle
      sched_section_found = False
      while line and line != '\n' and line != '\r' and line != '\r\n': # handle possible OS newline differences just in case
        if line[:6] != '$SCHED': line = vex.readline()
        else:
          sched_section_found = True
          break
      if not sched_section_found: raise Exception('Invalid vex-file format! $SCHED section not found in %s.' % (vexfile))
      else:
        line = vex.readline() # to enter the cycle
        try:
          while line[0] != '$' and line and line != '\n' and line != '\r' and line != '\r\n': # before the next section starts or file ends
            if line[:4] == 'scan': # we are inside a scan now
              scan_no = line[7:11] # e. g. '0052'
              while line.split()[0][:5] != 'start': line = vex.readline() # .split() to remove spaces at the beginning
              timestamp = line.split()[0][6:24] # gets the time stamp, like 2017y273d19h00m00s
              dct[scan_no] = timestamp
              line = vex.readline()
              while line[:7] != 'endscan':
                line = vex.readline() # scrolling through the station lines
            line = vex.readline()
        except IndexError:
          raise Exception('Invalid vex-file format! $SCHED section is not formatted correctly.')
        if not dct: raise Exception('Invalid vex-file format! No valid scan definitions in $STATION section.')
    return dct     

def stations(vexfile):
  """
     returns a list with two-letter codes of all the stations
     mentioned in the $STATION section of the vex-file
  """
  if not filename_check(vexfile):
    raise Exception('Invalid vex filename!')
  else:
    ants = []
    with open(vexfile, 'r') as vex:
      line = vex.readline() # to enter the cycle
      station_section_found = False
      while line and line != '\n' and line != '\r' and line != '\r\n': # handle possible OS newline differences just in case
        if line[:8] != '$STATION': line = vex.readline()
        else:
          station_section_found = True
          break
      if not station_section_found: raise Exception('Invalid vex-file format! $STATION section not found.')
      else:
        line = vex.readline() # to enter the cycle
        try:
          while line[0] != '$' and line and line != '\n' and line != '\r' and line != '\r\n': # before the next section starts or file ends
            if line[:3] == 'def':
              ants.append(line.split()[1][:2])
            line = vex.readline()
        except IndexError:
          raise Exception('Invalid vex-file format! $STATION section is not formatted correctly.')
        if not ants: raise Exception('Invalid vex-file format! No valid station definitions in $STATION section.')
    return ants

def sources(vexfile):
  """
     returns a list with source names of all the stations
     mentioned in the $STATION section of the vex-file
  """
  if not filename_check(vexfile):
    raise Exception('Invalid vex filename!')
  else:
    srcs = []
    with open(vexfile, 'r') as vex:
      line = vex.readline() # to enter the cycle
      source_section_found = False
      while line and line != '\n' and line != '\r' and line != '\r\n': # handle possible OS newline differences just in case
        if line[:7] != '$SOURCE': line = vex.readline()
        else:
          source_section_found = True
          break
      if not source_section_found: raise Exception('Invalid vex-file format! $SOURCE section not found.')
      else:
        line = vex.readline() # to enter the cycle
        try:
          while line[0] != '$' and line and line != '\n' and line != '\r' and line != '\r\n': # before the next section starts or file ends
            if line[:3] == 'def':
              srcs.append(line.split()[1][:-1])
            line = vex.readline()
        except IndexError:
          raise Exception('Invalid vex-file format! $SOURCE section is not formatted correctly.')
        if not srcs: raise Exception('Invalid vex-file format! No valid station definitions in $SOURCE section.')
    return srcs    
    
def exper(vexfile):
  """
     returns a string with the experiment name
     defined in the $EXPER section of the vex-file
  """
  if not filename_check(vexfile):
    raise Exception('Invalid vex filename!')
  else:
    with open(vexfile, 'r') as vex:
      line = vex.readline() # to enter the cycle
      exper_section_found = False
      while line and line != '\n' and line != '\r' and line != '\r\n': # handle possible OS newline differences just in case
        if line[:6] != '$EXPER': line = vex.readline()
        else:
          exper_section_found = True
          break
      if not exper_section_found: raise Exception('Invalid vex-file format! $EXPER section not found.')
      else:
        line = vex.readline() # to enter the cycle
        exper = ''
        try:
          while line[0] != '$' and line and line != '\n' and line != '\r' and line != '\r\n': # before the next section starts or file ends
            if line[:3] == 'def':
              exper = line.split()[1][:-1]
              break
            line = vex.readline()
        except IndexError:
          raise Exception('Invalid vex-file format! $EXPER section is not formatted correctly.')
        if not exper: raise Exception('Invalid vex-file format! No valid station definitions in $EXPER section.')
    return exper    
    
def two2one_stations(vexfile, file = 'gmva_codes'):
  """
     returns a list with one-letter codes of all the stations
     mentioned in the $STATION section of the vex-file
  """
  return conv_list_with_dict(stations(vexfile), two2one_code(file))
    
def scans(vexfile):
  """
     returns a dictionary of string keys in form scan number : source
     i. e. "No0015:BLLAC" and a list of all the stations in that scan
     i. e. ["Ef", "Mh", "Ys"] based on the $SCHED section of the vex-file
  """
  if not filename_check(vexfile):
    raise Exception('Invalid vex filename!')
  else:
    sched_dict = {}
    with open(vexfile, 'r') as vex:
      line = vex.readline() # to enter the cycle
      sched_section_found = False
      while line and line != '\n' and line != '\r' and line != '\r\n': # handle possible OS newline differences just in case
        if line[:6] != '$SCHED': line = vex.readline()
        else:
          sched_section_found = True
          break
      if not sched_section_found: raise Exception('Invalid vex-file format! $SCHED section not found.')
      else:
        line = vex.readline() # to enter the cycle
        try:
          while line[0] != '$' and line and line != '\n' and line != '\r' and line != '\r\n': # before the next section starts or file ends
            if line[:4] == 'scan': # we are inside a scan now
              scan_no = line[7:11] # e. g. '0052'
              while line.split()[0][:5] != 'start': line = vex.readline() # .split() to remove spaces at the beginning
              source = line[line.find('source=')+7:] # gets the source name, like '1156+295' with trailing ;\n or something
              while source[-1] == ';' or source[-1] == '\n' or source[-1] == '\r' : source = source[:-1] # get rid of trailing junk
              label = scan_no + ':' + source # getting the combined scan label, like '0052:1156+295'
              line = vex.readline() 
              ants = []                
              while line[:7] != 'endscan':
                if line.split()[0][:7] == 'station': ants.append(line.split()[0][8:10])
                line = vex.readline() 
              if ants: sched_dict[label] = ants[:]
              else: raise Exception('Invalid vex-file format! A scan with no antennas found.')             
            line = vex.readline()
        except IndexError:
          raise Exception('Invalid vex-file format! $SCHED section is not formatted correctly.')
        if not sched_dict: raise Exception('Invalid vex-file format! No valid scan definitions in $STATION section.')
    return sched_dict     
    
    
def clean_one_ant_scans(sched_dict):
  """
      removes scans with just one antenna (pointing) from the dictionary
      since they are useless for determining baselines
  """
  return {k:v for k, v in sched_dict.items() if len(v) > 1}

def cleaned_scans(vexfile):
  """
     returns a dictionary of string keys in form scan number : source
     i. e. "No0015:BLLAC" and a list of all the stations in that scan
     i. e. ["Ef", "Mh", "Ys"] based on the $SCHED section of the vex-file,
     but with single antenna (pointing) scans removed 
  """
  return clean_one_ant_scans(scans(vexfile))
  
def two2one_in_dict(sched_dict, file = 'gmva_codes'):
  """
      changes two letter codes into one letter codes inside a sched dictionary
  """
  return {k : conv_list_with_dict(v, two2one_code(file)) for k, v in sched_dict.items()}

def ant2bsl_in_dict(sched_dict):
  """
      converts a dictionary with lists of antennas in values
      into a dictionary with lists of baselines in values      
  """
  return {k : ant2bsl(v) for k, v in sched_dict.items()}

def ordered_labels(vexfile):
  """
      gets a sorted list of 'labels', i. e. strings like '0052:BLLAC'
      for a vex-file, including one antenna scans.
  """
  return sorted(scans(vexfile).keys())
  
def ordered_cleaned_labels(vexfile):
  """
      gets a sorted list of 'labels', i. e. strings like '0052:BLLAC'
      for a vex-file, excluding one antenna scans.
  """
  return sorted(cleaned_scans(vexfile).keys())
  
  
def two2one_in_vex(vexfile, file = 'gmva_codes'):
  """
     returns a dictionary of string keys in form scan number : source
     i. e. "No0015:BLLAC" and a list of all the stations in that scan,
     based on the $SCHED section of the vex-file,
     with name replaced to one-letter code using the gmva code file
     i. e. ["B", "X", "Y"].
     including single-antenna (pointing) scans 
  """
  return two2one_in_dict(scans(vexfile), file)

def cleaned_two2one_in_vex(vexfile, file = 'gmva_codes'):
  """
     returns a dictionary of string keys in form scan number : source
     i. e. "No0015:BLLAC" and a list of all the stations in that scan,
     based on the $SCHED section of the vex-file,
     with name replaced to one-letter code using the gmva code file
     i. e. ["B", "X", "Y"].
     excluding single-antenna (pointing) scans 
  """
  return two2one_in_dict(cleaned_scans(vexfile), file)
  
def bsl_in_vex(vexfile):
  """
     returns a dictionary of string keys in form scan number : source
     i. e. "No0015:BLLAC" and a list of all the BASELINES derived from
     the stations in that scan, which are based on the $SCHED section
     of the vex-file.
     i. e. ["EfOn", "EfYs", "OnYs"]. 
  """
  return ant2bsl_in_dict(cleaned_scans(vexfile))  

def bsl_two2one_in_vex(vexfile, file = 'gmva_codes'):
  """
     returns a dictionary of string keys in form scan number : source
     i. e. "No0015:BLLAC" and a list of all the BASELINES derived from
     the stations in that scan, which are based on the $SCHED section
     of the vex-file, with names replaced to one-letter code using
     the gmva code file.
     i. e. ["BX", "BY", "XY"]. 
  """
  return ant2bsl_in_dict(cleaned_two2one_in_vex(vexfile, file))
  
  
def compare_bslns(vex_bslns, alist_bslns):
  """
      compares two list of baselines (assumed to be in one-letter antenna format, i. e. "XY" etc.)
      one from vex_file, one from alist. Checks if vex baselines includes all the alist baselines
      (returns exception otherwise). Replaces the antenna order in the vex baseline set to correspond
      to alist order. Returns a sorted list, which is identical to the vex_bslns except some baselines
      maybe translated from "XY" to "YX"
  """
  if sorted(vex_bslns) != sorted(list(set(vex_bslns))): # check if there are repeating elements in each fo the lists
    print('\n',vex_bslns,'\n')
    while vex_bslns:
      bsl = vex_bslns.pop()
      if bsl in vex_bslns: print(bsl, 'occurs more than once in the .vex baselines list!')
    raise Exception('Invalid .vex baseline list format! All baselines must be unique.')
  if sorted(alist_bslns) != sorted(list(set(alist_bslns))): # check if there are repeating elements in each fo the lists
    print('\n',alist_bslns,'\n')
    while vex_bslns:
      bsl = alist_bslns.pop()
      if bsl in alist_bslns: print(bsl, 'occurs more than once in the .alist baselines list!')
    raise Exception('Invalid .alist baseline list format! All baselines must be unique.')
  for bsl in alist_bslns:
    if len(bsl) != 2:
      raise Exception('Invalid baseline format! Antenna designations must be in one-letter format, so that baselines appear as e. g. "XY".')
    else:
      if bsl not in vex_bslns and bsl[1]+bsl[0] not in vex_bslns:
        print (bsl, "exists in alist, but not in vex!")
        raise Exception('Incompatible baseline sets! The first set MUST include all the baselines from the second one.')
  new_vex_bslns = vex_bslns[:] # make a copy
  for bsl in vex_bslns:
    if len(bsl) != 2:
      raise Exception('Invalid baseline format! Antenna designations must be in one-letter format, so that baselines appear as e. g. "XY".')
    else:
      if bsl not in alist_bslns and bsl[1]+bsl[0] in alist_bslns:
        new_vex_bslns.remove(bsl)
        new_vex_bslns.append(bsl[1]+bsl[0])
    
  return sorted(new_vex_bslns)
      
  
  
