#! /Library/Frameworks/Python.framework/Versions/3.12/bin/python3

# Written by: Solomon Clark summer intern @USNO

import shutil
import os
import glob
import sys
import struct
import parseDiFX
import numpy as np 
from collections import defaultdict





# --------- ADD CHECK TO SEE IF THE SCALED FILE EXISTS, IF SO EXIT PROGRAM -------



 
# ---------- METHODS ----------


# Method to check if header is valid
def headerIsValid(header):
    try: 
        # Checking to see if there is a valid baseline value
        if header[0] > 256:
            return True

    # Notify the user once we've reached the end of the file
    except IndexError:
        pass # Silently ignoring the error        

    return False

# Method that determines if the given header is for an autocorrelation
def isAutocorr(header_dict):
    if(header_dict["baselineNum"] % 257 == 0):
      return True

# Method to calculate the  maximum freqIndex for a set of headers in a timestamp
def getMaxFreqInd(timestamp_data):
    max_index = 0
    for h in timestamp_data:
      print(h["freqIndex"])
      if(h["freqIndex"] > max_index):
        max_index = h["freqIndex"]

    return max_index

# Method for calculating the scaling factor for two given ACs
def scalingFactor(AC1, AC2): 
    # S formula
    # Assuming AC1 and AC2 are both autocorrelations, "visData" should be the average of their respective visibilities
    return np.power((AC1["visData"] * AC2["visData"]), 0.5)


# Method for grouping headers with matching freqInex values 
def freqGroups(list_of_headers):
    grouped_by_freq = defaultdict(list)
    for header in list_of_headers: 
      grouped_by_freq[header["freqIndex"]].append(header)
   

    # In this method, if you pass a list of header dictionaries, you will get a dictionary of lists, where each key is the freqIndex and each list contains all of the headers with that freqIndex 
    return grouped_by_freq


# ---------- METHODS ----------


# Taking in the directory basename from the command line
cmd_args = sys.argv
#print(cmd_args)
cmd_args.pop(0)
basename = cmd_args.pop(0)

# Putting together the full .difx directory
difx_file_path = (os.getcwd() + "/" + basename + ".difx")
#print(difx_file_path)

# Getting the name of the SWIN file saved in the .difx directory
swin_file_path = glob.glob(difx_file_path + "/DIFX_*")
#print(swin_file_name)

# Creating a difx object 
difx = parseDiFX.DiFXFile()

# Initializing numfreqs, freqs
(numfreqs, freqs) = parseDiFX.get_freqtable_info(basename + ".input")

# Opening the information stored in the SWIN file 
fh = open(basename + ".difx/" + os.path.basename(swin_file_path[0]), 'rb')

# Creating new visibility record object
vr = difx.nextVisibilityRecord()

# Initializing nChan
nChan = 0

# Creating list to hold complex visibilty data associated with each header
vis         = []

# Creating an outline for how the header is organized to aid in building a dict object for each header. Header srtucture reference: https://difxdoc.readthedocs.io/en/latest/sources/difx_files.html
header_structure = ["baselineNum", "dayNum", "seconds", "configIndex", "sourceIndex", "freqIndex", "ant1/ant2Pol", "pulsarBinNUm", "visWeight", "u", "v", "w", "binaryData", "visData"]


# ------------ PARSING FILE AND DECODING BINARY ----------


# Initializing timestamp container
header_list = []

# Parsing into first header
header = parseDiFX.parse_output_header(fh)

while(headerIsValid(header)):

  try: 
    # Creating header dictionary object to make it easier to access header elements
    header_dict = {}
    
    for i in range(len(header)):
      header_dict[header_structure[i]] = header[i]
    

    # Initializing freq_index for nChan calculations
    freq_index = header_dict["freqIndex"]
    
    nChan = freqs[freq_index].numchan // freqs[freq_index].specavg

    # Creating binary buffer  visibility data
    buffer = fh.read(8*nChan)

    vis = [] # Clearing visibility list for this iteration
    visavg = []
 
    for j in range(nChan):
        try: 
          # Translating the data packaged in buffer and adding it to vis
          cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
          vis.append(complex(cvis[0], cvis[1]))
          #print(vis[-1])
        except struct.error: 
          pass # Silently bypass the error

   if(isAutocorr(header_dict)):
      # Adding visibility average to the end of AC header
      visavg = np.mean(vis)
      header_dict["visData"] = visavg
     
      # Adding header to list of headers: in this portion, we are organizing to calculate the scaling factor, which only requires using the autocorrelations
      header_list.append(header_dict)
    # Parsing to next header
    header = parseDiFX.parse_output_header(fh)
  except IndexError: 
    print("\n\n--------- REACHED END OF FILE ----------\n\n")

# ----- REMOVE AFTER DONE CHECKING ------   

print("\n\n\n--------------- END OF ORIGINAL FILE ----------------\n\n\n")

print("\n\n\n--------------- START OF SCALED FILE ---------------- \n\n\n")

# ----- REMOVE AFTER DONE CHECKING ------   

# ---------- CONSTRUCTING ORGANIZATIONAL DATA STRUCTURES ----------


# Creating a timestamp dictionary that holds each timestamp as a key containing the list of all headers under that timestamp
timestamp_dict = defaultdict(list)

for header in header_list: 
  timestamp_dict[header["seconds"]].append(header)


# Optional: Printing pre-scaled timestamp groups: 
"""
for timestamp, headers in timestamp_dict.items(): 
  print(f"\n\n---------- TIMESTAMP: {timestamp} ----------\n\n")
  for h in headers: 
    print(" ", h)
"""


# Extracting the time groups as separate lists. Now each time group can be accessed by index or a for-each loop
time_groups = list(timestamp_dict.values()) # Each element in time_groups is a list of header dictionaries that share the same timestamp

# Making a list of dictionaries that are sorted by freqIndex

# freq_sorted_list[i] is going to be a dictionary sorted by freqIndex for all headers within the ith timestamp
freq_sorted_list = []

# Adding sorted frequency groups to container
for groups in time_groups: 
  freq_sorted_list.append(freqGroups(groups))



# Creating a list to hold all of the scaling factors
factor_dict_container = []

# Structure for building scaling factor list
factor_dict_structure = ["timestamp", "freqIndex", "scalingfactor", "canFactor"]

# Creating a dictionary to hold the scalingFactor data
factor_dict = {}

# Filling the container with the correct scaling factors
for timestamps in freq_sorted_list:
  for freq_groups in timestamps.values():
    if(len(freq_groups) > 1):
     
      # In each freq_index grouping for each timestamp, the elements at index 0 and 1 are the autocorrelations
      sFactor = scalingFactor(freq_groups[0], freq_groups[1])

      # Declaring the values of the scaling facor dictionary depending on the number of baselines in the frequency group. The scaling factor will only be applied if the correct number of baselines is present
      factor_dict = {"timestamp": freq_groups[0]["seconds"],"freqIndex": freq_groups[0]["freqIndex"], "scalingFactor": sFactor, "canFactor": True}

    else: 
      # If there is an incorrect number of baselines in a frequency group, then the scaling factor is defaulted to zero
      factor_dict = {"timestamp": freq_groups[0]["seconds"],"freqIndex": freq_groups[0]["freqIndex"], "scalingFactor": 0, "canFactor":  False}

    factor_dict_container.append(factor_dict)

# Converting list of factor dictionaries into a lookup table
factor_lookup = {}

for entry in factor_dict_container: 
  ts = entry["timestamp"]
  fi = entry["freqIndex"]
  sf = entry["scalingFactor"]

  if ts not in factor_lookup: 
    factor_lookup[ts] = {}

  factor_lookup[ts][fi] = sf

print(factor_lookup)

exit(0)
# ---------- MULTIPLYING SCALING FACTOR ----------


# Going back to the top of the SWIN file
fh.seek(0)     

second_header = parseDiFX.parse_output_header(fh)

# Creating a container to store all of the changes visibility sets
vis_container = []

while(headerIsValid(second_header)):

  try:
    # Creating header dictionary object to make it easier to access header elements
    second_header_dict = {}

    for i in range(len(second_header)):
      second_header_dict[header_structure[i]] = second_header[i]

    # Initializing freq_index for nChan calculations
    freq_index = second_header_dict["freqIndex"]

    nChan = freqs[freq_index].numchan // freqs[freq_index].specavg
    
    buffer = fh.read(8*nChan)

    vis = [] # Clearing visibility list for this iteration
    visavg = []

    for j in range(nChan):
      try:  # Translating the data packaged in buffer and adding it to vis
        cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
        vis.append(complex(cvis[0], cvis[1]))
        #print(vis[-1])
      except struct.error: 
        pass # Silently bypass error 

    # Initializing header data for later search in lookup table 
    timestamp = 0
    freq_index = 0
    scaling_factor = 0
    baseline = 0

    timestamp = second_header_dict["seconds"]
    freq_index = second_header_dict["freqIndex"]
    baseline = second_header_dict["baselineNum"]      


    # Inserting the timestamp and freqIndex of the CC into the fast lookup table to apply correct scaling factor
    scaling_factor = factor_lookup[timestamp][freq_index]
    #print(f"\n\nBaseline: {baseline} ,Timestamp: {timestamp}, Frequency Index: {freq_index}, Scaling Factor: {scaling_factor}")

    #print()
  
    #print(f"Vis Before: {vis}")
  
    # Initializing container for scaled visibilities
    new_vis = []
      
    # Applying scaling factor
    for v in vis: 
      v*=scaling_factor
      new_vis.append(v)


    # Repacking new visibility values back into binary
    format_string = 'f' * 2 * nChan

    vis_just_floats = []
    for i in range(len(new_vis)):
      vis_just_floats.append(vis[i].real)
      vis_just_floats.append(vis[i].imag)
      

      
    packed_vis = struct.pack(format_string, *vis_just_floats)
    second_header_dict["visData"] = packed_vis

    vis_container.append(second_header_dict) 
    # Printing vis after for comparison
    #print(new_vis)
      
    #print()

    #print(f"Vis After: {new_vis}")
      
    # Parsing to next header
    second_header = parseDiFX.parse_output_header(fh)
  except IndexError:
    print("\n\n--------- REACHED END OF FILE ----------\n\n")



# Creating fast lookup for visibilities
vis_lookup = {}

for entry in vis_container: 
  ts = entry["seconds"]
  fi = entry["freqIndex"]
  vis = entry["visData"]
  
  if ts not in vis_lookup: 
    vis_lookup[ts] = {}
  
  vis_lookup[ts][fi] = vis

# ---------- COPYING AND REWRITING INTO FILE ----------


# Copying the original file to a new destination
first_swin_file_path = swin_file_path[0]

shutil.copy(first_swin_file_path, first_swin_file_path + "_scaled")

# Creating a second file handler to parse the copied file
fh2 = open(basename + ".difx/" + os.path.basename(first_swin_file_path + "_scaled"), 'r+b')

vis_data_size= 8*nChan


# Parsing into the first header on the file. This lands the pointer at position 74
copy_header = parseDiFX.parse_output_header(fh2)


while(headerIsValid(copy_header)): 
 
  #print(copy_header) 
  copy_header_dict = {}
  scaled_vis_data = []  

  for i in range(len(copy_header)):
    copy_header_dict[header_structure[i]] = copy_header[i]
  
  timestamp = copy_header_dict["seconds"]
  freq_index = copy_header_dict["freqIndex"]
  
  # Using fast lookup to rewrite the correct dataset
  scaled_vis_data = vis_lookup[timestamp][freq_index]
  pointer_pos = int(fh2.tell())

  try:

    # Scenario for when we are just past the first header: we don't want to seek backwards the length of the vis data becuase it is longer than the header, meaning that will give us a parsing error
    if(pointer_pos <  vis_data_size):
      fh2.write(scaled_vis_data)
    # When we have gone enough distance away from the top of the document, we can use the original double seek to write down the rest of the file 
    else:
      fh2.seek(-vis_data_size, os.SEEK_CUR)
      fh2.write(scaled_vis_data)
      fh2.seek(vis_data_size, os.SEEK_CUR)

    #x = fh2.read(512)
    #format_string = format_string
    #z = struct.unpack(format_string,x)
    #print(z)
    #exit(0)

  # Error handling
  except TypeError:
    print("New vis data must be of type bytes")
    exit(0)
  except ValueError: 
    print("File is closed or not writeable")
    exit(0)
  except OSError as e: 
    print(f"OS Error while writing file: {e}")
    exit(0)
 
  # Parsing into next header
  copy_header = parseDiFX.parse_output_header(fh2)
  































# ---------- PAST CODE ----------


"""
# Constructing While loop that reads to the end of each SWIN file 

i = 1 # Iterator

# Parsing to the first header of the page
header = parseDiFX.parse_output_header(fh) 

header_dict = {}

for i in range(len(header)):
	header_dict[header_structure[i]] = header[i]	


#print(header_dict)
#time = header[2]

#print(time in header)

# Creating a list that will hold the timestamp data
timestamp_list = []


# Running the loop only as long as there is a valid header
while(headerIsValid(header)):

  # Assigning values based on the positions of the data in the header
  baseline = header_dict["baselineNum"]
  freqindex = header_dict["freqIndex"]
  mjd = header_dict["dayNum"]
  time = header_dict["seconds"]
    
  		       	
  # Printing header and baseline info
  #print(f"\n\nHeader {i}:\n")
  #print(header) 
  #print(f"Baseline: {baseline}\n\n")
  
  
  timestamp_list.append(header_dict)   

  # Calculating the number of channels per spectral window
  nChan = freqs[freqindex].numchan // freqs[freqindex].specavg
  #print(f"# Channels : {nchan}")

  
  # Packing the binary data to be translated in coming for loop
  buffer = fh.read(8*nChan)
  
  #print(f"Visual Data {i}:\n\n")
    
  vis = [] # Clearing visibility list for this iteration
  visavg = []
 
  for j in range(nChan):
    # Translating the data packaged in buffer and adding it to vis
    cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
    vis.append(complex(cvis[0], cvis[1]))
    #print(vis[-1])

  # Calculating the average of vis
  visavg = np.mean(vis)
  #print(visavg)
  
  if(isAutocorr(header)):
    # Adding visibility average to the end of AC header
    header_dict["visAvg"] = visavg
  else: 
    # Adding cross-ciorrelation  signifier at the end of CC header
    header_dict["visAvg"] = "CC"

  #print(header_dict)
  # Next header // Can check to see if timestamp is different 
  header = parseDiFX.parse_output_header(fh)
  
  # Loading information in new header into a new header dict object
  for i in range(len(header)):
    header_dict[header_structure[i]] = header[i]
  

  #header_dict["visAvg"] = 0 
  #print(header_dict)
  #print(header)
  #print(header[2])

  

  try :   
    # Checking next timestamp
    if(time != header_dict["seconds"]): # This means we are at a new timestamp
      print(f"\n\n {getMaxFreqInd(timestamp_list)}")
      print(f"\n\n Timestamp Grouping {header_dict["seconds"]}: \n\n")
     
      is_autocorr = True
      for h in timestamp_list:
        if(not isAutocorr(h)):
          is_autocorr = False 
        # print(h)
      
      timestamp_list = [] # Resetting timestamp list
      time = header_dict["seconds"]  # Setting new timestamp
  except IndexError: 
    print()
  
  # Tracking # of headers/datasets
  i += 1

# Printing which datasets are autocorrelations
#print("\n\n--AUTOCORRELATED DATASETS--\n\n")

#for data in autocorrs:
  #print(data)
"""






























"""
# Start Loop


for i in range(128): 
	
  print()
  print()

  print(f"Header {i+1}: ")

  print()
  print()
  header = parseDiFX.parse_output_header(fh)

  print(header)

  print()
  print()

  baseline = header[0]
  freqindex = header[5]
  nchan = freqs[freqindex].numchan // freqs[freqindex].specavg
  print(f"# Channels : {nchan}") 
  mjd = header[1]
  seconds = header[2]
  buffer = fh.read(8*nchan)

  print(f"Visual Data {i+1}: ")

  print()
  print()

  for j in range(nchan):
    cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
    vis.append(complex(cvis[0], cvis[1]))
    print(vis[j])

  vis = []


# End Loop

"""


"""
print()
print()

print("Header: ")

print()
print()
header = parseDiFX.parse_output_header(fh)

print(header)

print()
print()

baseline = header[0]
freqindex = header[5]
nchan = freqs[freqindex].numchan // freqs[freqindex].specavg
mjd = header[1]
seconds = header[2]
buffer = fh.read(8*nchan)

print("Visual Data: ")

print()
print()

for j in range(nchan):
  cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
  vis.append(complex(cvis[0], cvis[1]))
  print(vis[j])

vis = []
"""



#print("# bytes in last element of header: " + str(len(header[-1]))) 
# Finding the length of the binary at the end of the header

#data = header[-1]





"""
Trying to decipher the binary all at once: 

formatting_string = "<HHIIIQd8sQd16s"

unpacked = struct.unpack(formatting_string, data)

for i, val in enumarate(unpacked):
		print(f"Field {i}: {val}")
"""
"""
# Deciphering each byte one section at a time, guessing what they are (ChatGPT)

print(struct.unpack_from('<HH', data, 0))  # uint16 × 2
print(struct.unpack_from('<I', data, 4))   # uint32

print(struct.unpack_from('<I', data, 8))   # 4 bytes
print(struct.unpack_from('<I', data, 12))  # 4 bytes

print(struct.unpack_from('<Q', data, 16))  # unsigned long long (8 bytes)

print(struct.unpack_from('<d', data, 24))  # double (8 bytes)

print(struct.unpack_from('<8s', data, 32))

print(struct.unpack_from('<Q', data, 40))

print(struct.unpack_from('<d', data, 48))

print(struct.unpack_from('<16s', data, 56))
"""

# Using the iter_unpack cmd to try and decipher the binary

#unpacked_floats = struct.iter_unpack('f', data)
#print(int(unpacked_floats))


"""
header2 = parseDiFX.parse_output_header(fh)

print()
print()

print(header2)


print()
print()
vis = []
buffer = fh.read(8*nchan)
for j in range(nchan):
  cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
  vis.append(complex(cvis[0], cvis[1]))
  print(vis[j])


quit()
vr = difx.nextHeader()
"""
"""
cvis = struct.unpack("ff", buffer[8*j:8*(j+1)])
vis[i][j] = complex(cvis[0], cvis[1])
"""
"""
while vr.isvalid():
	h = vr.header
	info = f"MJD {h.mjd}/{h.seconds} : ant {h.antenna1} : freqindex = {h.freqindex} : polpair = {h.polpair} : {len(vr.vis)}"
	print(info)
	vr = difx.nextVisibilityRecord
"""












3
