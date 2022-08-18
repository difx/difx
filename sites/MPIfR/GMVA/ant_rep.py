#!/usr/bin/env python3
'''
GMVA Antenna Report template script v5

(C) 2022 Yurii Pidopryhora

The scripts reads the *.vex file (assuming only one is present),
the alist file, and all the .difxlog files. It then produces a
bunch of .antrep files, a general one *_ALL and additional ones
for each antenna. The files are tab formatted txt and have detailed
legends in all tables. The idea is that the detailed statistics are
near the top and the most general cumulative tables are at the bottom.

This script must be copied and run in the experiment correlation directory.
A few lines must be modified, cf. code block "need to modify these" in the script.

The script can then be run in the root directory of the production correlation.
'''

from __future__ import division # division behaves like in Python 3

#### need to modify these #####################################################
project_name = 'c212c'          # project or observing block name
# ignore = ['Sc', 'Hn']         # 2-letter codes of antennas to ignore
alistfile = '3007.alist_v6'     # A-list file to compare against *.vex file
mode_name = '43ghz'             # name of VEX mode, or None or just comment out
###############################################################################
# TODO: relocate the above hard-coded vars into argparse cmd line args

import pyvexfile
import os, sys
import argparse
import re

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
    #except FileNotFoundError:
    except (OSError, IOError):
      return False
    f.close()
    return True
    
def clean_one_ant_scans(sched_dict):
  """
      removes scans with just one antenna (pointing) from the dictionary
      since they are useless for determining baselines
  """
  return {k:v for k, v in sched_dict.items() if len(v) > 1}

def cleaned_scans(vexfile,modename=None):
  """
     returns a dictionary of string keys in form scan number : source
     i. e. "No0015:BLLAC" and a list of all the stations in that scan
     i. e. ["Ef", "Mh", "Ys"] based on the $SCHED section of the vex-file,
     but with single antenna (pointing) scans removed 
  """
  return clean_one_ant_scans(scans(vexfile,modename))

def scans(vexfile,modename=None):
    """
     returns a dictionary of string keys in form scan number : source
     i. e. "No0015:BLLAC" and a list of all the stations in that scan
     i. e. ["Ef", "Mh", "Ys"] based on the $SCHED section of the vex-file
    """
    #print (vexfile)
    v = pyvexfile.Vex(vexfile, vexfile)
    #print(v["SCHED"])

    if not filename_check(vexfile):
        raise Exception('Invalid vex filename!')

    sched_dict = {}

    for key,value in v['SCHED'].items():
        
        source = ""
        # skip anything that is not a scan definition (e.g. comments)
        if type(v['SCHED'][key]) != pyvexfile.Scan:
            continue
            
    #    print (key)
     #   print (v['SCHED'][key]['station'])
        #print (v['SCHED'][key]['station'])
        #print (v['SCHED'][scan])
        ants = []
        #print (type(v['SCHED'][key]['start'].value))

        try:
            for ant in v['SCHED'][key]['station']:
                    ants.append(ant.value[0])
        except:
      #      print ("Exception")
       #     print (v['SCHED'][key]['station'].value)
            ants.append(v['SCHED'][key]['station'].value[0])

        #print (ants)
        start = v['SCHED'][key]['start']
        #rint (start)
#start = 2021y275d04h52m30s; mode=43ghz; source=3C454.3;
        m = re.search(".*source\s*=\s*([a-zA-Z0-9+-.]*)\s*;.*", str(start))
        if m:
            source = m.group(1).strip()
        else:
            print('Scan %s source lookup failed. Full line: %s' % (str(key),str(start)))

        if modename:
            m = re.search(".*mode\s*=\s*([a-zA-Z0-9_]*)\s*;.*", str(start))
            if m:
                scanmode = m.group(1).strip()
                if len(scanmode)>0 and scanmode != modename:
                    continue
                # print('Scan on ', source, ' in mode ', scanmode)
            else:
                print('Scan %s mode lookup failed. Full line: %s' % (str(key),str(start)))

        label = key + ":" + source
        if ants:
             sched_dict[label] = ants[:]

        #print (v['SCHED'][scan]['station'])
        #print (scan)
        #print ( v['SCHED']['No0398']['station'])
        #print ( v['SCHED'][scan].keys())
        

    #with open(vexfile, 'r') as vex:
    #  line = vex.readline() # to enter the cycle
    #  sched_section_found = False
    #  while line and line != '\n' and line != '\r' and line != '\r\n': # handle possible OS newline differences just in case
    #    print (line)
    #    if line[:6] != '$SCHED': line = vex.readline()
    #    else:
    #      sched_section_found = True
    #      break
    #  if not sched_section_found: raise Exception('Invalid vex-file format! $SCHED section not found.')
    #  else:
    #    line = vex.readline() # to enter the cycle
    #    try:
    #      while line[0] != '$' and line and line != '\n' and line != '\r' and line != '\r\n': # before the next section starts or file ends
    #        if line[:4] == 'scan': # we are inside a scan now
    #          scan_no = line[7:11] # e. g. '0052'
    #          while line.split()[0][:5] != 'start': line = vex.readline() # .split() to remove spaces at the beginning
    #          source = line[line.find('source=')+7:] # gets the source name, like '1156+295' with trailing ;\n or something
    #          while source[-1] == ';' or source[-1] == '\n' or source[-1] == '\r' : source = source[:-1] # get rid of trailing junk
    #          label = scan_no + ':' + source # getting the combined scan label, like '0052:1156+295'
    #          line = vex.readline() 
    #          ants = []                
    #          while line[:7] != 'endscan':
    #            if line.split()[0][:7] == 'station': ants.append(line.split()[0][8:10])
    #            line = vex.readline() 
    #          if ants: sched_dict[label] = ants[:]
    #          else: raise Exception('Invalid vex-file format! A scan with no antennas found.')             
    #        line = vex.readline()
    #    except IndexError:
    #      raise Exception('Invalid vex-file format! $SCHED section is not formatted correctly.')
    #    if not sched_dict: raise Exception('Invalid vex-file format! No valid scan definitions in $STATION section.')
    #print (sched_dict)
    return sched_dict     
    
def ordered_cleaned_labels(vexfile,modename=None):
  """
      gets a sorted list of 'labels', i. e. strings like '0052:BLLAC'
      for a vex-file, excluding one antenna scans.
  """
  return sorted(cleaned_scans(vexfile,modename).keys())

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

  
def calc_files_dict(excllist):
  """
      creates a dict based on the info in all the calc files
      in the launch directory. For each vex-style label
      '0067:BLLAC' it contains a tuple:
      (job ID, num_telescopes, list of telescope names in order)
  """
  d = {}
  for file in FileListWithExt('calc'):
    job_ID = 0
    Ntel = 0
    scan_name=''
    source_name=''
    tel_list = []
    list_tel_des = []
    with open(file, 'r') as readf:
      line = readf.readline()
      ### looking for job ID
      while line[:7] != 'JOB ID:' or not line: # not line is safety for bad file format
        line = readf.readline()
      job_ID = int(line[7:])
      if not str(job_ID) in file:
        print('ERROR: Job ID in the .calc file ' + file + ' does not correspond to its file name!')
        sys.exit(1)
      ### looking for number of telescopes
      line = readf.readline()
      while line[:15] != 'NUM TELESCOPES:' or not line:
        line = readf.readline()      
      Ntel = int(line[15:])
      ### populating the telescope list
      for _ in range(Ntel):
        list_tel_des.append('TELESCOPE ' + str(_) + ' NAME:')  
      for des in list_tel_des:
        line = readf.readline()
        while line[:len(des)] != des or not line:
          line = readf.readline()
        tel_list.append(line[len(des):].strip()[-2].upper() + line[len(des):].strip()[-1].lower()) # make sure that case is right, e. g. 'Ef'
      if not len(tel_list) == Ntel:
        print('ERROR: Number of ants in ' + file + ' is inconsistent!')
        sys.exit(1)
      ### looking for the source name
      line = readf.readline()
      while line[:14] != 'SOURCE 0 NAME:' or not line:
        line = readf.readline()
      source_name = line[14:].strip() # remove extra spaces or CRs
      ### looking for the scan number
      line = readf.readline()
      while line[:18] != 'SCAN 0 IDENTIFIER:' or not line:
        line = readf.readline()
      # print(line)
      # scan_name = line[18:].strip()[-4:] # we cut 'No'
      scan_name = line.split(":")[1].strip()
    ### finished with .calc file
    ### now go to difxlog:
    difxlogname = file[:-len('.calc')]+'.difxlog'
    weights = []
    weights_cur = []
    numb = 0
    # determine the number of double stream telescopes
    # known double stream telescopes
    known_double_streams = ['Aa', 'Gl', 'Nn', 'P3', 'Pv']
    no_double = 0 # number of double streams in this job
    for tt in tel_list:
      if tt in known_double_streams:
        no_double = no_double + 1
    if filename_check(difxlogname):
      with open(difxlogname, 'r') as readf:
        line = readf.readline()
        while line:
          if 'WEIGHTS' in line:
            weights_cur = [float(_) for _ in line[line.find('WEIGHTS')+7:].strip().split()]
            if weights:
              w = []
              for i in range(len(weights)):
                w.append(weights[i] + weights_cur[i])
              weights = w[:]
            else:
              weights = weights_cur[:]
            numb = numb + 1
          line = readf.readline() 
          if 'is about to start processing' in line: # difx was restarted, ignore previous log
            weights = []
            numb = 0
      w = []          
      for i in range(len(weights)):
        w.append(weights[i]/numb) # average
      weights = w[:]
    else: # no difxlog
      weights = [0.0]*(Ntel + no_double)
    # correction for double data streams
    if no_double > 0:
      #print(tel_list)
      #print(weights)
      ind = 0
      weights_new = []
      for tt in tel_list:
        if tt in known_double_streams:
          weights_new.append(0.5*(weights[ind] + weights[ind+1]))
          ind = ind + 2
        else:
          weights_new.append(weights[ind])
          ind = ind + 1
      weights = weights_new[:]
      #print(weights)
    #else: no correction needed
    # correcting for excllist
    for ant in excllist:
      if ant in tel_list:
        Ntel = Ntel - 1
        ind = tel_list.index(ant)
        tel_list.remove(ant)
        weights.pop(ind)
    d[scan_name + ':' + source_name] = (job_ID, Ntel, tel_list, weights)
    if Ntel != len(weights): # in case another telescope with double data streams appears
      print('ERROR: Number of ants and weights in ' + file + ' is different from each other!')
      print('Check all telescopes with double data streams (like Aa, Gl, Nn, P3, Pv etc.)')
      print('')
      print('This version only corrects for known double data streams: Aa, Gl, Nn, P3, Pv')
      print('If any of these double-stream-telescopes became single-stream')
      print('or another double-data-stream antenna appeared')
      print('correct the known_double_streams list accordingly.')
      sys.exit(1)
  return d

def FileListWithExt(file_ext):
  """
      Generates a list of all files with the given extension
      in the launch directory.
  """
  file_ext = '.' + file_ext
  # isinstance(file_ext, str) -- Python 3 way
  # isinstance(file_ext, basestring) -- Python 2 way
  if not file_ext or not isinstance(file_ext, str) or ' ' in file_ext:
    raise Exception("ERROR: File extension must be a non-empty string with no spaces!")
  else:
    return [f for f in os.listdir('.') if os.path.isfile(f) and f[-len(file_ext):] == file_ext]

def alist_dict(alistfilenames, antlist):
  """
      Creates a dictionary with alist data for every scan
      Note that the argument, alistfilenames MUST be a list
  """
  one2two_dict = one2two_code()
  adict = {}
  for filename in alistfilenames:
    with open(filename, 'r') as f:
      line = '*'
      while line[0] == '*': # skip the header
        line = f.readline()
      while (line != "\n" and line != ""):
        pol = line.split()[17] # polarization, e. g. RR
        bs = line.split()[14] # baseline, e. g. BX
        if (pol[0] == pol[1] or 'X' in pol or 'Y' in pol) and bs[0] != bs[1]: # either parallel hands or lin pol present, not an autocorr
          ant1 = one2two_dict[bs[0]]
          ant2 = one2two_dict[bs[1]]
          if (ant1 in antlist) and (ant2 in antlist):
            #scan = line.split()[8][2:] # scan name, cut 'No'
            scan = line.split()[8]
            src = line.split()[13] # source name, e. g. BLLAC
            label = scan + ':' + src
            qual = int(line.split()[15][0]) # fringe code, e. g. 9 for 9D
            SNR = float(line.split()[20]) # SNR
            #print label, pol, bs, ant1, ant2, qual, SNR 
            if label not in adict.keys():
              adict[label] = ([],[],[],[])
            ants = adict[label][0]
            no_corr = adict[label][1]
            no_fring = adict[label][2]
            snrs = adict[label][3]
            if ant1 not in ants:
              ants.append(ant1)
              no_corr.append(1)
              if qual > 0:
                no_fring.append(1)
                snrs.append(SNR)
              else:
                no_fring.append(0)
                snrs.append(0.0)
            else:
              ind = ants.index(ant1)
              no_corr[ind] = no_corr[ind] + 1
              if qual > 0: # otherwise no need to touch the other two lists
                full = snrs[ind]*no_fring[ind] + SNR
                no_fring[ind] = no_fring[ind] + 1
                snrs[ind] = full/no_fring[ind]
            if ant2 not in ants:
              ants.append(ant2)
              no_corr.append(1)
              if qual > 0:
                no_fring.append(1)
                snrs.append(SNR)
              else:
                no_fring.append(0)
                snrs.append(0.0)
            else:
              ind = ants.index(ant2)
              no_corr[ind] = no_corr[ind] + 1
              if qual > 0: # otherwise no need to touch the other two lists
                full = snrs[ind]*no_fring[ind] + SNR
                no_fring[ind] = no_fring[ind] + 1
                snrs[ind] = full/no_fring[ind]
            adict[label] = (ants, no_corr, no_fring, snrs)
            #print label, ants, no_corr
        line = f.readline()
  return adict
  
def main():
  #### basic setup ####
  ign_str = ''
  if args.ignore:
      for _ in args.ignore:
        ign_str = ign_str + _ + ', '
      ign_str = ign_str[:-2]
  else:
      args.ignore = []
  if ign_str:
    print('\nIgnoring antennas:')
    print(ign_str)
    print('\n\n')
  filename_main = project_name + '_ALL.antrep'
###################################################################################
  LEGEND = '''
This file was generated by ant_rep version 5.
---------------------------------------------------------------------------------------------------------------
           Column description:

1 (scan: src) - scan number based on the vex file with excluded pointing scans : source name
2 (vex) - indicates if this antenna was included in the vex file for this scan
3 (calc) - indicates if this antenna was included in the correlation of this scan based on precorrelation .calc
4 (wgt) - average weight for this antenna in the scan based on the difx log
         (for multiple correlations of the same scan only the last correlation is included in the averaging)
5 (ants) - number of antennas correlated for this scan (excluding the given one)
    / number of antennas in the vex-file for this scan (excluding the given one)
6 (fr/bs) - number of baselines with fringes found by fourfit
  / number of expected baselines in the correlation (taking into account only parallel hands,
i. e. the first number of the previous column is multiplied by two)
7 (snr) - average snr for the found fringes
8 (missed) - explanation of column 5, noting which antennas are actually missing
---------------------------------------------------------------------------------------------------------------
'''
####################################################################################
  with open(filename_main, 'w') as mainf:
    mainf.write(LEGEND)
    #######################
    if len(FileListWithExt('vex')) > 1:
      print('ERROR: more than one vex file found!') ### only one vex handled so far
      sys.exit(4)
    else:
      vexfile=FileListWithExt('vex')[0]
    ######## preparing the databases #############
    myants = [_ for _ in stations(vexfile) if _ not in args.ignore]
    dct = cleaned_scans(vexfile,mode_name)
    scns = ordered_cleaned_labels(vexfile,mode_name)
    calcfilesdict = calc_files_dict(args.ignore)
    alistdict = alist_dict([args.alistfile], myants)
    maindict = {}
    antfile_list = []
    for ant in myants:
      newantfilename = project_name + '_' + ant + '.antrep'
      antfile_list.append(newantfilename)
      antf=open(newantfilename, 'w')
      antf.write(LEGEND)
      maindict[ant] = ([],[],[],[],[],[],[],[]) # 0sources, 1scheduled, 2participated, 3weight, 4exp baselines, 5corr baselines, 6fringes, 7snr
      mainf.write('\nAntenna: ' + ant + '\n')
      antf.write('\nAntenna: ' + ant + '\n')
      mainf.write('scan:src \tvex \tcalc \twgt \tants \tfr/bs \tsnr \tmissed\n')
      antf.write('scan:src \tvex \tcalc \twgt \tants \tfr/bs \tsnr \tmissed\n')
      for s in scns:
        src = maindict[ant][0]
        sch = maindict[ant][1]
        part = maindict[ant][2]
        wgt = maindict[ant][3]
        ebsl = maindict[ant][4]
        cbsl = maindict[ant][5]
        fr = maindict[ant][6]
        snr7 = maindict[ant][7]
        print (s)
        source = s.split(":")[1]
        num_ants_vex = 0
        num_ants_calc = 0
        rep_lin = s
        missstr = ' '
        ants = [_ for _ in dct[s] if _ not in args.ignore] 
        if ant in ants: 
          rep_lin = rep_lin + '\t+'
          num_ants_vex = len(ants) - 1
          if source not in src:
            src.append(source)
            ind = src.index(source)
            sch.append(1)
            part.append(0)
            wgt.append(0.0)
            ebsl.append(0)
            cbsl.append(0)
            fr.append(0)
            snr7.append(0.0)
          else:
            ind = src.index(source)
            sch[ind] = sch[ind] + 1
        else:
          rep_lin = rep_lin + '\t.'
        if s in calcfilesdict.keys():
          if ant in calcfilesdict[s][2]:
            part[ind] = part[ind] + 1
            num_ants_calc = len(calcfilesdict[s][2])-1
            #print ebsl
            #print num_ants_calc
            if 'Aa' not in calcfilesdict[s][2] and ant != 'Aa':
              ebsl[ind] = ebsl[ind] + num_ants_calc*2 # only two pols
            elif ant == 'Aa':
              ebsl[ind] = ebsl[ind] + num_ants_calc*4 # take all 4 pols -- XR, XL, YR, YL
            else: # Aa in other antennas
              ebsl[ind] = ebsl[ind] + num_ants_calc*2 + 2 # 4 pols in Aa
            #print ebsl
            if num_ants_calc < num_ants_vex:
              miss = [_ for _ in ants if _ != ant and _ not in calcfilesdict[s][2]]
              for _ in miss:
                missstr = missstr + _ +','
              missstr = missstr[:-1]
            if calcfilesdict[s][3]: # weights exist
              if len(calcfilesdict[s][3]) != len(calcfilesdict[s][2]):
                print('ERROR: In scan ' + s + ' number of antennas in .calc and .difxlog is different!!!')
                sys.exit(3)
              rep_lin = rep_lin + '\t+\t'+'{:1.3f}'.format(calcfilesdict[s][3][calcfilesdict[s][2].index(ant)])
              wgt[ind] = (wgt[ind]*(part[ind]-1) + calcfilesdict[s][3][calcfilesdict[s][2].index(ant)])/part[ind]
            else: # no weights exist
              wgt[ind] = wgt[ind]*(part[ind]-1)/part[ind]
              print('WARNING: Scan ' + s + ' was not correlated? No weights or no difxlog found.')
              rep_lin = rep_lin + '\t.\t?'
          else:
            rep_lin = rep_lin + '\t.\t.'
        else:
          print('WARNING: Scan ' + s + ' is not in the .calc files!')
          rep_lin = rep_lin + '\t?\t?'
        rep_lin = rep_lin + '\t' + str(num_ants_calc) + '/' + str(num_ants_vex)
        if s in alistdict.keys():
          if ant in alistdict[s][0]:
            inde = alistdict[s][0].index(ant)
            rep_lin = rep_lin + '\t{}/{}\t{}'.format(alistdict[s][2][inde], alistdict[s][1][inde], int(round(alistdict[s][3][inde])))
            #print cbsl
            cbsl[ind] = cbsl[ind] + alistdict[s][1][inde]
            #print  alistdict[s][1][inde]
            #print cbsl
            if alistdict[s][2][inde] > 0:
              fr[ind] = fr[ind] + alistdict[s][2][inde]
              snr7[ind] = (snr7[ind]*(fr[ind]-alistdict[s][2][inde]) + alistdict[s][3][inde]*alistdict[s][2][inde])/fr[ind]
          else:
            rep_lin = rep_lin + '\t.\t.'
        else:
          print('WARNING: Scan ' + s + ' is not in the alist file(s)!')
          # print('All A-list scans:\n', str(alistdict.keys()))
          rep_lin = rep_lin + '\t?\t?'
        rep_lin = rep_lin + '\t' + missstr
        maindict[ant] = (src[:],sch[:],part[:],wgt[:],ebsl[:],cbsl[:],fr[:],snr7[:])
        #print ant, src[:], ebsl[:], cbsl[:]
        del src,sch,part,wgt,ebsl,cbsl,fr,snr7
        mainf.write(rep_lin+'\n')
        antf.write(rep_lin+'\n')
      antf.close()
###################################################################################
    FINLEGEND = '''
-------------------------------------------------------------------------------------------------------------
           Final statistics per source, column description:

1 (ant) - which antenna
2 (source) - which source
3 (sched) - scheduled, how many scans were scheduled in the vex
4 (prtsp) - participated, how many scans were actually observed based on precorrelation .calc files  
5 (wgt) - average weight for this antenna in the correlated scans based on the difx logs
         (for multiple correlations of the same scan only the last correlation is included in the averaging)
6 (expect) - expected, how many baselines*pols should be found based on participation, only LL and RR
             are included (unless a linear pol antenna is present), so in most cases should
             be all possible baselines based on participating antennas times 2 
7 (fnd) - found, how many baselines*pols (with fringes or not) are found in the alist file,
          again, unless a linear pol antenna is present, only LL and RR are included.
          This is a santity check, must be fnd = expect.
8 (frng) - fringes, how many of fnd have a nonzero fringe code in the alist file 
9 (snr) - average snr of frng
-------------------------------------------------------------------------------------------------------------
'''
####################################################################################
    mainf.write('\n\nFinal:\n')
    mainf.write(FINLEGEND)
    mainf.write('ant \tsource   \tsched \tprtsp \twgt \texpect \tfnd \tfrng \tsnr\n')
    for antfilename in antfile_list:
      antf=open(antfilename, 'a')
      antf.write('\n\nFinal:\n')
      antf.write(FINLEGEND)
      antf.write('ant \tsource   \tsched \tprtsp \twgt \texpect \tfnd \tfrng \tsnr\n')
      antf.close()
    fin_dict = {}
    for ant in sorted(maindict.keys()):
      accum_sched = 0
      accum_prtsp = 0
      accum_weight = 0.0
      accum_expect = 0
      accum_fnd = 0
      accum_frng = 0
      accum_snr = 0.0
      antfilename = project_name + '_' + ant + '.antrep'
      antf=open(antfilename, 'a')
      for i in range(len(maindict[ant][0])):
        accum_sched = accum_sched + maindict[ant][1][i]
        accum_prtsp = accum_prtsp + maindict[ant][2][i]
        accum_weight = accum_weight + maindict[ant][3][i]*maindict[ant][2][i]
        accum_expect = accum_expect + maindict[ant][4][i]
        accum_fnd = accum_fnd + maindict[ant][5][i]
        accum_frng = accum_frng + maindict[ant][6][i] 
        accum_snr = accum_snr + maindict[ant][7][i]*maindict[ant][6][i]
        mainf.write('{}\t{:14}\t{}\t{}\t{:1.3f}\t{}\t{}\t{}\t{}\n'.format(ant, maindict[ant][0][i], maindict[ant][1][i], maindict[ant][2][i], \
                                                                       maindict[ant][3][i], maindict[ant][4][i], maindict[ant][5][i], \
                                                                       maindict[ant][6][i], int(round(maindict[ant][7][i]))))
        antf.write('{}\t{:14}\t{}\t{}\t{:1.3f}\t{}\t{}\t{}\t{}\n'.format(ant, maindict[ant][0][i], maindict[ant][1][i], maindict[ant][2][i], \
                                                                       maindict[ant][3][i], maindict[ant][4][i], maindict[ant][5][i], \
                                                                       maindict[ant][6][i], int(round(maindict[ant][7][i]))))
      if accum_prtsp > 0:
        accum_weight = accum_weight/accum_prtsp
      else:
        accum_weight = 0.0
      if accum_frng > 0:
        accum_snr = accum_snr/accum_frng
      else:
        accum_snr = 0.0
      fin_dict[ant] = (accum_sched, accum_prtsp, accum_weight, accum_expect, accum_fnd, accum_frng, accum_snr) 
      antf.close()
###################################################################################
    ACCLEGEND = '''
-------------------------------------------------------------------------------------------------------------
           Accumulated statistics per antenna, all sources averaged, column description:

1 (ant) - which antenna
2 (sched) - scheduled, how many scans were scheduled in the vex
3 (prtsp) - participated, how many scans were actually observed based on precorrelation .calc files  
4 (wgt) - average weight for this antenna in the correlated scans based on the difx logs
         (for multiple correlations of the same scan only the last correlation is included in the averaging)
5 (%) - participation ratio: prtsp*wgt/sched
        should be close to 100% in most cases
6 (expect) - expected, how many baselines*pols should be found based on participation, only LL and RR
             are included (unless a linear pol antenna is present), so in most cases should
             be all possible baselines based on participating antennas times 2 
7 (fnd) - found, how many baselines*pols (with fringes or not) are found in the alist file,
          again, unless a linear pol antenna is present, only LL and RR are included.
          This is a santity check, must be fnd = expect.
8 (frng) - fringes, how many of fnd have a nonzero fringe code in the alist file 
9 (%) - fringe ratio, frng/fnd 
10 (snr) - average snr of frng
-------------------------------------------------------------------------------------------------------------
'''
####################################################################################
    mainf.write('\n\nAccumulated:\n')
    mainf.write(ACCLEGEND)
    mainf.write('ant \tsched \tprtsp \twgt \t% \texpect \tfnd \tfrng \t% \tsnr\n')
    for antfilename in antfile_list:
      antf=open(antfilename, 'a')
      antf.write('\n\nAccumulated:\n')
      antf.write(ACCLEGEND)
      antf.write('ant \tsched \tprtsp \twgt \t% \texpect \tfnd \tfrng \t% \tsnr\n')
      antf.close()
    for ant in sorted(fin_dict.keys()):
      antfilename = project_name + '_' + ant + '.antrep'
      antf=open(antfilename, 'a')
      if fin_dict[ant][0] > 0:
        prc_prtcp = fin_dict[ant][1]*fin_dict[ant][2]*100./fin_dict[ant][0]
      else:
        prc_prtcp = 0.0
      if fin_dict[ant][4] > 0:
        prc_frn = fin_dict[ant][5]*100./fin_dict[ant][4]
      else:
        prc_frn = 0.0
      mainf.write('{}\t{}\t{}\t{:1.3f}\t{}\t{}\t{}\t{}\t{}\t{}\n'.format(ant, fin_dict[ant][0], fin_dict[ant][1], fin_dict[ant][2], int(round(prc_prtcp)), \
                                                                       fin_dict[ant][3], fin_dict[ant][4], fin_dict[ant][5], int(round(prc_frn)), \
                                                                       int(round(fin_dict[ant][6]))))
      antf.write('{}\t{}\t{}\t{:1.3f}\t{}\t{}\t{}\t{}\t{}\t{}\n'.format(ant, fin_dict[ant][0], fin_dict[ant][1], fin_dict[ant][2], int(round(prc_prtcp)), \
                                                                       fin_dict[ant][3], fin_dict[ant][4], fin_dict[ant][5], int(round(prc_frn)), \
                                                                       int(round(fin_dict[ant][6]))))
      antf.close()

if __name__ == '__main__':

    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('-e', '--exclude', dest="ignore", action="append", required=False, help='Exclude this station code from the report')
    parser.add_argument('alistfile', help='The alist file for the project.')
    args = parser.parse_args()
    # print (args)

    main()
