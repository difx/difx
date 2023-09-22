#########################################################################
# <AutoZoom: calculate ZoomFreqs automatically>                         #             
# Copyright (C) <2015> <Zheng Meyer-Zhao>                               #
#                                                                       #
# AutoZoom is free software: you can redistribute it and/or modify      #
# it under the terms of the GNU General Public License as published by  #
# the Free Software Foundation, either version 3 of the License, or     #
# (at your option) any later version.                                   #
#                                                                       #
# AutoZoom is distributed in the hope that it will be useful,           #
# but WITHOUT ANY WARRANTY; without even the implied warranty of        #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
# GNU General Public License for more details.                          #
#                                                                       #
# You should have received a copy of the GNU General Public License     #
# along with this program.  If not, see <http://www.gnu.org/licenses/>. #
#########################################################################

ALMABW = 62.5
ALMAZOOMBW = 58.59375
EPSILON = 0.00001
MAXBW = 2048.0

# check if a number is 2^N
def is_pow2(num):
  return num > 0 and ((num & (num - 1) == 0))

# Case 1: zoom band <-> recorded band
# ALL stations have the same frequency coverage (max. 2048 MHz).
# Start and end frequencies of all stations are the same. Bandwidth is 2^N.
def allvlbi(zoom, freqs):
  # add case-specific code here
  # zoom.refbw, zoom.reffreqs, and zoom.refnchans should be set
  zoom.refbw = MAXBW
  pow2 = True
  minfreqs = set()
  maxfreqs = set()
  # get the smallest bandwidth of all frequency setup
  # use it as reference for zoom frequency
  for f in freqs.keys():
    pow2 &= is_pow2(int(freqs[f]['bandwidth']))
    minfreqs.add(freqs[f]['min_freq'])
    maxfreqs.add(freqs[f]['max_freq'])
    if int(zoom.refbw) >= int(freqs[f]['bandwidth']):
      zoom.refbw = int(freqs[f]['bandwidth'])
      zoom.reffreqs = freqs[f]['band_freqs']
      zoom.refnchans = freqs[f]['num_channels']
  
  if not pow2:
    raise Exception("Not all bandwidth is 2^N!!!")
  if len(minfreqs) != 1 or len(maxfreqs) != 1:
    raise Exception("Start frequency or end frequency of antennas are not the same!!!")

# Case 2: ALMA <-> 2048 MHz VLBI
# ALMA uses the normal configuration, i.e. ALMA's channels are overlapped,
# the band centers are 62.5 * 15/16 = 58.59375 MHz apart.
# Use ALMA frequency setup as reference, center-align ALMA bands.
# User can define zoom bandwidth, default zoom bandwidth is ALMAZOOMBW=58.59375 MHz.
def almavlbi(zoom, freqs, zoombw):
  # add case-specific code here
  # zoom.refbw, zoom.reffreqs, and zoom.refnchans should be set
  if zoombw == None:
    zoombw = ALMAZOOMBW
  zoom.refbw = zoombw

  for f in freqs.keys():
    if abs(freqs[f]['bandwidth'] - ALMABW) < EPSILON:
      zoom.refnchans = freqs[f]['num_channels']
      if freqs[f]['side_band'] == 'U':
        for freq in freqs[f]['band_freqs']:
          zoom.reffreqs.append(freq + ALMABW/2 - zoombw/2)
      else:
        for freq in freqs[f]['band_freqs']:
          zoom.reffreqs.append(freq - ALMABW + ALMABW/2 - zoombw/2)

# Case 3: ALMA <-> VLBA DDC
# ALMA uses the normal configuration, i.e. ALMA's channels are overlapped,
# the band centers are 62.5 * 15/16 = 58.59375 MHz apart.
# Use ALMA frequency setup as reference, center-align ALMA bands.
# User can define zoom bandwidth, default zoom bandwidth is ALMAZOOMBW=58.59375 MHz.
# In case ALMA frequency coverage is larger than VLBA DDC,
# only select the ALMA bands within the VLBA DDC frequency coverage
def almavlbaddc(zoom, freqs, zoombw):
  # add case-specific code here
  # zoom.refbw, zoom.reffreqs, and zoom.refnchans should be set
  if zoombw == None:
    zoombw = ALMAZOOMBW
  zoom.refbw = zoombw

  for f in freqs.keys():
    if abs(freqs[f]['bandwidth'] - ALMABW) < EPSILON:
      zoom.refnchans = freqs[f]['num_channels']
      if freqs[f]['side_band'] == 'U':
        for freq in freqs[f]['band_freqs']:
          zoom.reffreqs.append(freq + ALMABW/2 - zoombw/2)
      else:
        for freq in freqs[f]['band_freqs']:
          zoom.reffreqs.append(freq - ALMABW + ALMABW/2 - zoombw/2)

# To add new cases, comment out the following template,
# and add case-specific code

#def newcase(freqs):
#  # add case-specific code here
#  # zoom.refbw, zoom.reffreqs, and zoom.refnchans should be set


# Add newcase into zoom_options as hown below
def zoom_options():

  zoom_options = {1 : allvlbi,
                  2 : almavlbi,
                  3 : almavlbaddc
                 #N : newcase
                  }
  return zoom_options

class Zoom:
  def __init__(self):
    self.z = {}
    self.reffreqs = []
    self.refnchans = 0
    self.refbw = 0.0

  def addzoomfreqs(self, freqs):
    if not self.reffreqs or self.refnchans == 0:
      raise Exception("No reference frequency setup found!!!")
    for f in freqs.keys():
      self.z[f] = []
      # if bandwidth of the frequency setup is not the same as reference bandwidth
      if abs(freqs[f]['bandwidth'] - self.refbw) >= EPSILON:
        for ch in range(self.refnchans):
          self.z[f].append("addZoomFreq = freq@%f/bw@%f/noparent@true" % (self.reffreqs[ch], self.refbw))
    return self.z

  def setreference(self, freqs, opts, zoombw):
    options = zoom_options()
    if opts == 1:
      if zoombw != None:
        raise Exception("Zoom bandwidth cannot be specified for Case 1!!!")
      options[opts](self, freqs)
    else:
      options[opts](self, freqs, zoombw)
  