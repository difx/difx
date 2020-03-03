"""library to handle the calculation of proxy-cable-calibration delays from phase-cal data """

#core imports
from __future__ import print_function
from __future__ import absolute_import
from __future__ import division
from builtins import next
from builtins import str
from past.utils import old_div
from builtins import object
from builtins import range
import datetime
import sys
import os
import math
import cmath
import copy

import logging
pcc_logger = logging.getLogger(__name__)

#non-core imports
import numpy as np
import scipy.stats

try:
    from progress.bar import Bar
except:
    from .utility import Bar

from .utility import limit_periodic_quantity_to_range
from .utility import minimum_angular_difference

#hops package python libs
import mk4b
import hopstestb as ht
import vexpy
from . import pcc_delay_fitting

#hard-coded values from HOPS
MAX_CHAN = 128
#define number of channels and accumulators (hard-coded in type_309_v1)
T309_NCHAN = 64
T309_NACC = 64

#taken from fourfit pcal_interp.c
TWO31 = 2147483648.0
TWO32 = 4294967296.0


#number of seconds offset from start of scan
#that we discard
DISCARD_OFFSET = 2



"""
proxy-cable-cal procedure should operate in an object oriented way :

- go through the whole experiment for each station, and do:
- for each scan we should:
    - parse the ovex file, get a list of stations (and list of channels for each station):
    - for each station read in the station data file type_3XX
    - then extract the type_309 for each channels
    - convert the type_309 data to raw complex double-arrays by tone-ap(time), this needs to handle upper/lower-sidebands and give tones in sky-frequency
    - time average the raw-pcal data over the whole scan
    - present a set of objects which are labelled by scan-station containing the (raw) mean phasor for each p-cal tone
    - then we should apply a reference scan(s) and cut bad tones
    - the for each scan-station object we should group channels by each particular band-pol. How do we group channels into bands? Give a standard list for VGOS/mixed-mode?
    - order the tones for each band-pol and perform a delay fit
    - output a collection of objects labelled by scan-station which contain delays for each available band-pol
    - ouput a time series of delays for each station-band-pol (with meta data)
"""

class PccDate(object):
    """simple date class """
    def __init__(self):
        self.year = 0
        self.day = 0
        self.hour = 0
        self.minute = 0
        self.second = 0

    def __lt__(self, other):
        if self.year < other.year:
            return True
        if self.year == other.year:
            if self.day < other.day:
                return True
            if self.day == other.day:
                if self.hour < other.hour:
                    return True
                if self.hour == other.hour:
                    if self.minute < other.minute:
                        return True
                    if self.minute == other.minute:
                        if self.second < other.second:
                            return True
        return False

    def initialize_from_date(self,mk4_date):
        self.year = mk4_date.year
        self.day = mk4_date.day
        self.hour = mk4_date.hour
        self.minute = mk4_date.minute
        self.second = mk4_date.second

    def get_time_delta_seconds(self,other):
        """compute difference in time """
        if self.year == other.year:
            deltaday = self.day - other.day
            deltahour = self.hour - other.hour
            deltamin = self.minute - other.minute
            deltasec = self.second - other.second
            delta_seconds = datetime.timedelta(days=deltaday, hours=deltahour, minutes=deltamin, seconds=deltasec).total_seconds()
            return delta_seconds
        else:
            #TODO FIXME, better hope no-one runs an experiment over new year's eve
            pcc_logger.error("Error: PccData.get_time_delta_seconds() is not yet implemented for scans in different years.")
            return 0

    def as_string(self):
        day_str = str(self.day)
        hour_str = str(self.hour)
        minute_str = str(self.minute)
        second_str = str(int(self.second))
        return day_str.zfill(3) + "-" + hour_str.zfill(2) + ":" + minute_str.zfill(2) + ":" + second_str.zfill(2) + " UTC"



################################################################################



class NumericalInterval(object):
    """simple class for a numerical interval """
    #default values for init
    def __init__(self, center, width):
        self.center = center
        self.width = width

    def get_center(self):
        return self.center

    def get_width(self):
        return self.width

    def get_lower_limit(self):
        return self.center - old_div(self.width,2.0)

    def get_upper_limit(self):
        return self.center + old_div(self.width,2.0)

    def __str__(self):
        return "(" + str(self.get_lower_limit() ) + ", " + str(self.get_upper_limit() ) + ")"


class ComplexReImCovarianceMatrix(object):
    """See the paper:
        @inproceedings{williams2006phase,
          title={In-phase/quadrature covariance-matrix representation of the uncertainty of vectors and complex numbers},
          author={Williams, Dylan F and Wang, CM and Arz, Uwe},
          booktitle={2006 68th ARFTG Conference: Microwave Measurement},
          pages={1--4},
          year={2006},
          organization={IEEE}
        }"""

    def __init__(self, complex_data_vector=None):
        self.is_valid = False
        self.mx = np.array([[0., 0.],[0., 0.]]) #this representation is ReIm covar.
        self.iq_mx = np.array([[0., 0.],[0., 0.]])  #this representation is In-phase/Quad covar.
        self.data = []
        self.real_mean = 0.0
        self.imag_mean = 0.0
        self.complex_mean = 0.0
        self.mean_phase = 0.0
        self.mean_magnitude = 0.0
        if complex_data_vector != None:
            self.set_data_vector(complex_data_vector)
            self.compute_mx()

    def set_data_vector(self, complex_data_vector):
        self.data = complex_data_vector

    def multiply_data_by_complex_scalar(self, complex_scalar):
        for n in list(range(0, len(self.data) ) ):
            self.data[n] *= complex_scalar
        self.compute_mx()

    def compute_mx(self):
        real_arr = np.array( [x.real for x in self.data] )
        imag_arr = np.array( [x.imag for x in self.data] )
        #first compute the Re/Im mean/variance from the data
        self.real_mean = np.mean(real_arr)
        self.imag_mean = np.mean(imag_arr)
        self.complex_mean = complex(self.real_mean, self.imag_mean)
        self.mean_phase = cmath.phase(self.complex_mean)
        self.mean_magnitude = abs(self.complex_mean)
        #then compute the mx elements
        self.mx = np.cov(real_arr, imag_arr)
        theta = self.mean_phase
        rot_mx = np.array([ [ math.cos(theta), -1.0*math.sin(theta)], [ math.sin(theta), math.cos(theta)] ] )
        self.iq_mx = np.dot( np.transpose(rot_mx), np.dot(self.mx, rot_mx) )
        self.is_valid = True

    def get_RI_representation(self):
        return self.mx

    def get_IQ_representation(self):
        return self.iq_mx

    def get_magnitude_variance(self):
        return self.iq_mx[0,0]

    def get_phase_variance(self):
        #geometrically this makes sense,
        #but the paper only gives the results in the small angle case: tan\theta ~ \theta
        sigma_q = math.sqrt(self.iq_mx[1,1])
        mag = abs(self.complex_mean)
        sigma_theta = 0.0
        if abs(mag) > 1e-15:
            sigma_theta = math.atan( sigma_q/mag)
        return sigma_theta*sigma_theta

        #return scipy.stats.circvar( np.array([ cmath.phase(val) for val in self.data ]), high=math.pi, low=-1*math.pi )
        #return (math.atan( math.sqrt(self.iq_mx[1,1])/abs(self.complex_mean) ))**2
        #m2 = abs(self.complex_mean)*abs(self.complex_mean)
        #return math.asin(self.iq_mx[1,1]/m2)


################################################################################



class ChannelToBandMap(object):

    def __init__(self, map_type, filename=None):
        #map type should be a string specifying: "vgos", "mixed" (vgos against sx), bbmixed (vgos against vgos in an sx sessions),  "sx", or "file"
        #file is available to define custom channel to band mappings, whereas the others
        #use 'standard' channel assignments
        self.map_type = map_type.upper()
        self.band_list = []
        self.pol_list = []
        self.channel_to_band_pol = dict()
        self.band_pol_to_channel = dict()

        if self.map_type == 'VGOS':
            #setup and use vgos standard channel <-> band mapping, this set up is typical for 4x RDBE's
            self.band_list = ['A','B','C','D']
            self.pol_list = ['X','Y']
            self.band_pol_to_channel['A:X'] = ['X00LX', 'X01LX','X02LX','X03LX','X04LX', 'X05LX', 'X06LX', 'X07LX']
            self.band_pol_to_channel['B:X'] = ['X08LX', 'X09LX','X10LX','X11LX','X12LX', 'X13LX', 'X14LX', 'X15LX']
            self.band_pol_to_channel['C:X'] = ['X16LX', 'X17LX','X18LX','X19LX','X20LX', 'X21LX', 'X22LX', 'X23LX']
            self.band_pol_to_channel['D:X'] = ['X24LX', 'X25LX','X26LX','X27LX','X28LX', 'X29LX', 'X30LX', 'X31LX']
            self.band_pol_to_channel['A:Y'] = ['X00LY', 'X01LY','X02LY','X03LY','X04LY', 'X05LY', 'X06LY', 'X07LY']
            self.band_pol_to_channel['B:Y'] = ['X08LY', 'X09LY','X10LY','X11LY','X12LY', 'X13LY', 'X14LY', 'X15LY']
            self.band_pol_to_channel['C:Y'] = ['X16LY', 'X17LY','X18LY','X19LY','X20LY', 'X21LY', 'X22LY', 'X23LY']
            self.band_pol_to_channel['D:Y'] = ['X24LY', 'X25LY','X26LY','X27LY','X28LY', 'X29LY', 'X30LY', 'X31LY']

        if self.map_type == 'MIXED':
            #vgos-as-sx channel to band mapping for stations with RDBE's (in this case band-X is split between two different samplers B and C that have different LO's)
            self.band_list = ['A','B','C']
            self.pol_list = ['X','Y']
            self.band_pol_to_channel['A:X'] = ['S00UX', 'S01UX', 'S02UX', 'S03UX', 'S04UX']
            self.band_pol_to_channel['A:Y'] = ['S00UY', 'S01UY', 'S02UY', 'S03UY', 'S04UY']
            self.band_pol_to_channel['B:X'] = ['X05UX', 'X06UX', 'X07UX', 'X08UX']
            self.band_pol_to_channel['B:Y'] = ['X05UY', 'X06UY', 'X07UY', 'X08UY']
            self.band_pol_to_channel['C:X'] = ['X09UX', 'X10UX', 'X11UX', 'X12UX', 'X13UX']
            self.band_pol_to_channel['C:Y'] = ['X09UY', 'X10UY', 'X11UY', 'X12UY', 'X13UY']

        if self.map_type == 'BBMIXED':
            #use vgos-as-sx standard channel to band mapping
            self.band_list = ['A','B', 'C', 'D']
            self.pol_list = ['X','Y']
            self.band_pol_to_channel['A:X'] = ['S00UX', 'S01UX', 'S02UX', 'S03UX', 'S04UX', 'S05UX', 'S06UX', 'S07UX']
            self.band_pol_to_channel['A:Y'] = ['S00UY', 'S01UY', 'S02UY', 'S03UY', 'S04UY', 'S05UY', 'S06UY', 'S07UY']
            self.band_pol_to_channel['B:X'] = ['C08UX', 'C09UX', 'C10UX', 'C11UX', 'C12UX', 'C13UX', 'C14UX', 'C15UX']
            self.band_pol_to_channel['B:Y'] = ['C08UY', 'C09UY', 'C10UY', 'C11UY', 'C12UY', 'C13UY', 'C14UY', 'C15UY']
            self.band_pol_to_channel['C:X'] = ['X16UX', 'X17UX', 'X18UX', 'X19UX', 'X20UX', 'X21UX', 'X22UX', 'X23UX']
            self.band_pol_to_channel['C:Y'] = ['X16UY', 'X17UY', 'X18UY', 'X19UY', 'X20UY', 'X21UY', 'X22UY', 'X23UY']
            self.band_pol_to_channel['D:X'] = ['X24UX', 'X25UX', 'X26UX', 'X27UX', 'X28UX', 'X29UX', 'X30UX', 'X31UX']
            self.band_pol_to_channel['D:Y'] = ['X24UY', 'X25UY', 'X26UY', 'X27UY', 'X28UY', 'X29UY', 'X30UY', 'X31UY']

        if self.map_type == 'SX':
            #use SX standard channel to band mapping
            self.band_list = ['S','X']
            self.pol_list = ['R']
            self.band_pol_to_channel['S:R'] = ['S00UR','S01UR','S02UR','S03UR','S04UR']
            self.band_pol_to_channel['X:R'] = ['X05UR','X06UR','X07UR','X08UR','X09UR','X10UR','X11UR','X12UR','X13UR']

        if self.map_type == 'FILE' and filename != None:
            #use a custom band <-> channel mapping defined in a file
            pcc_logger.error("Error: file band-channel map not yet implemented!")
            sys.exit()

        #set up inverse map
        for b in self.band_list:
            for p in self.pol_list:
                bp = b + ':' + p
                for ch in self.band_pol_to_channel[bp]:
                    self.channel_to_band_pol[ch] = bp

    def list_types(self):
        return ["VGOS", "MIXED", "BBMIXED", "SX"]

    def get_band_pol_from_channel_name(self, channel_name):
        band = ''
        pol = ''
        if channel_name in self.channel_to_band_pol:
            band, pol = (self.channel_to_band_pol[channel_name]).split(':')
        return band, pol


    def get_channels_from_band_pol(self, band, pol):
        bp  = band + ':' + pol
        if bp in self.band_pol_to_channel:
            return self.band_pol_to_channel
        else:
            return []

    def does_ovex_contain_current_channel_setup(self, ovex_filename, mk4_site_id):
        """check ovex file for the current channel setup for a specified station"""
        success = True
        if os.path.isfile(ovex_filename):
            #get the ovex data from the root file
            ovex = vexpy.get_ovex(ovex_filename)
            #locate the station data
            st_index = 0
            for n in list(range(0, ovex.nst)):
                if str( ovex.st[n].mk4_site_id.decode() ) == mk4_site_id:
                    st_index = n
                    break
            #fill it in and run through and populate the channel entries
            channel_list = []
            for ch in list(range(0, MAX_CHAN)):
                channel_name = str( ovex.st[st_index].channels[ch].chan_name.decode() )
                if len(channel_name) != 0:
                    if channel_name not in self.channel_to_band_pol.keys():
                        pcc_logger.error("Error a channel: " + channel_name + " that is not in the " + self.map_type + " band <-> channel map keys in: " + ovex_filename)
                        success = False #This disables proxy-cable-cal delay fitting for the scan associated with this root-file
            return success
        else:
            return False

    def construct_band_pol_keys(self, bands_to_use, pols_to_use):
        blist = self.intersecting_band_subset(bands_to_use)
        plist = self.intersecting_pol_subset(pols_to_use)
        bp_list = []
        for b in blist:
            for p in plist:
                bp_list.append(b + ':' + p)
        return bp_list

    def intersecting_band_subset(self, bands_to_use):
        #if bands_to_use_list is empty, or doesn't intersect with the current modes band
        #list, then we default to using the entire band list of the current mode
        blist = [b for b in bands_to_use if b in self.band_list]
        if len(blist) == 0:
            blist = self.band_list
        return blist

    def intersecting_pol_subset(self, pols_to_use):
        #if bands_to_use_list is empty, or doesn't intersect with the current modes band
        #list, then we default to using the entire band list of the current mode
        plist = [p for p in pols_to_use if p in self.pol_list]
        if len(plist) == 0:
            plist = self.pol_list
        return plist

###############################################################################



class SingleChannelPhasorCollection(object):
    """ object to store the phase-cal phasors for a single channel over a complete scan """

    def __init__(self):
        #default values for init, these must be set externally
        self.channel_name = ""
        self.sky_frequency = 0
        self.bandwidth = 0
        self.net_sideband = ""
        self.sideband_sign = -1.0
        self.polarization = ""
        self.pcal_spacing = 0
        self.pcal_base_freq = 0
        self.sample_rate = 0

        #these quantities are set up when initialize is called and computed from the ovex info
        self.ntones = 0
        self.tone_low = 0.0
        self.tone_high = 0.0
        self.tone_offset_freq_array = []
        self.tone_physical_freq_array = []

        #these quantities are set up when the typ309 data is read in
        #phasors are stored in 2d array index by [tone_index, ap_index]
        self.phasor = np.zeros((1,1),dtype=complex)
        self.phasor_integer = np.zeros((1,1), dtype='2uint32')
        #frequency/time intervals allow look-up of the tone/time value associated
        #with each phasor by its index
        self.frequency_interval = []
        self.time_interval = []

        #the results we are after (time averaged phasors for each tone in this channel)
        self.rotated_by_reference=False
        self.freq_phasor_pairs = []

    def initialize(self):
        """construct the expected number of physical tones and their locations"""
        if self.net_sideband == "U":
            self.sideband_sign = 1.0
            self.tone_low = self.pcal_spacing*math.ceil( old_div( self.sky_frequency, self.pcal_spacing) )
            self.tone_high =  self.pcal_spacing*math.floor( old_div( (self.sky_frequency + self.sideband_sign*self.bandwidth), self.pcal_spacing) )
        elif self.net_sideband == "L":
            self.sideband_sign = -1.0
            self.tone_high = self.pcal_spacing*math.floor( old_div( self.sky_frequency, self.pcal_spacing) )
            self.tone_low =  self.pcal_spacing*math.ceil( old_div( (self.sky_frequency + self.sideband_sign*self.bandwidth), self.pcal_spacing) )
        else:
            pcc_logger.error("Error, channel with net-sideband not L or U present.")
            sys.exit()

        self.ntones = int( 1 + old_div( abs(self.tone_high - self.tone_low), self.pcal_spacing) )
        self.tone_physical_freq_array = []
        self.tone_offset_freq_array = []
        for n in list(range(0, self.ntones)):
            phys_tone = self.tone_low + n*self.pcal_spacing
            self.tone_physical_freq_array.append(phys_tone)
            self.tone_offset_freq_array.append(phys_tone - self.sky_frequency)

    def compute_time_averaged_phasors(self, trim_length=DISCARD_OFFSET):
        """ compute time averaged complex phasor """
        self.rotated_by_reference=False
        self.freq_phasor_pairs = [ (0.0, complex(0.0, 0.0), ComplexReImCovarianceMatrix(), True ) ]*self.ntones
        self.rotated_freq_phasor_pairs = [ (0.0, complex(0.0, 0.0), ComplexReImCovarianceMatrix(), True ) ]*self.ntones
        for tone_index in list(range(0, self.ntones)):
            ave = complex(0,0)
            freq = self.frequency_interval[tone_index].get_center()
            time_accum = 0.0
            start_index = DISCARD_OFFSET
            data_vec = []
            if trim_length !=0 :
                start_index = trim_length
            if tone_index < len(self.phasor):
                for n in list(range(start_index, len(self.time_interval))):
                    delta = self.time_interval[n].get_width()
                    time_accum += delta
                    sample = delta*( self.phasor[tone_index][n] )
                    ave += sample
                    data_vec.append(sample)
            if time_accum != 0.0: #accumulated time must be non-zero!
                ave *= 1.0/time_accum
            else:
                ave = complex(0.0,0.0)
            self.freq_phasor_pairs[tone_index] = [freq, ave, ComplexReImCovarianceMatrix(data_vec), True]

    def apply_phase_reference_collection(self, reference_scpc):
        """Correct this single channel phasor collection by a set of reference phasors for the same channel.
        This rotates the phasor so that the reference phasor collection has a phase of zero for each tone"""
        #check that this is the same channel
        if self.channel_name == reference_scpc.channel_name and self.ntones == reference_scpc.ntones:
            #we use the time averaged values of the reference_phasor_collection
            for n in list(range(0,self.ntones)):
                if n < len(self.freq_phasor_pairs):
                    [freq, time_ave_ph, covar_mx, validity_flag] = reference_scpc.freq_phasor_pairs[n]
                    rotation = time_ave_ph.conjugate()/abs(time_ave_ph)
                    self.freq_phasor_pairs[n][1] *= rotation
                    #self.freq_phasor_pairs[n][2].multiply_data_by_complex_scalar(rotation)
            self.rotated_by_reference=True
        else:
            #error
            pcc_logger.error("Error: channel names or number of tones do not match: ", self.channel_name, " : ", self.ntones,  " != ", reference_scpc.channel_name, " : ", reference_scpc.ntones)

################################################################################



class StationScanPhaseCalibrationData(object):

    def __init__(self, mk4_site_id, ovex_data_file):
        self.ovex_data_file = os.path.abspath(ovex_data_file) #the 'root' file
        self.root_suffix = ovex_data_file[-6:]
        self.station_data_file = os.path.join( os.path.dirname(self.ovex_data_file), mk4_site_id + ".." + self.root_suffix)
        self.mk4_site_id = mk4_site_id #single character station Mk4-id
        self.scan_name = ""
        self.source_name = ""
        self.exp_name = ""
        self.station_code = "" #two character station code
        self.site_name = ""
        self.sample_rate = 0
        self.azimuth = 0
        self.elevation = 0
        self.start_time = PccDate()
        self.fourfit_ref_time = PccDate()
        self.channel_name_list = []
        self.max_channel_bandwidth = 0
        #these contain the type_309 data (converted to complex floats)
        self.single_channel_phasor_collections = dict()
        self.fit_results = dict() #contains delay-fit results indexed by band:pol
        self.valid = False
        #run the extraction routines
        self.extract_ovex()
        self.extract_pcal()


    def extract_ovex(self):
        """read in the station channel set up data from the ovex file"""
        if os.path.isfile(self.ovex_data_file):
            #get the ovex data from the root file
            ovex = vexpy.get_ovex(self.ovex_data_file)
            #get the scan information
            self.exp_name = str( ovex.exper_name.decode() )
            self.scan_name = str( ovex.scan_name.decode() )
            self.source_name = str( ovex.src.source_name.decode() )
            self.start_time.initialize_from_date( ovex.start_time)
            self.fourfit_ref_time.initialize_from_date( ovex.ffit_reftime )
            #locate the station data
            st_index = 0
            for n in list(range(0, ovex.nst)):
                if str( ovex.st[n].mk4_site_id.decode() ) == self.mk4_site_id:
                    st_index = n
                    break
            #fill it in and run through and populate the channel entries
            self.site_name = str( ovex.st[st_index].site_name.decode() )
            self.station_code = str( ovex.st[st_index].site_id.decode() )
            self.sample_rate = ovex.st[st_index].samplerate
            for ch in list(range(0, MAX_CHAN)):
                channel_name = str( ovex.st[st_index].channels[ch].chan_name.decode() )
                if len(channel_name) != 0:
                    self.channel_name_list.append(channel_name)
                    pol = str( ovex.st[st_index].channels[ch].polarization.decode() )
                    sky_freq = ovex.st[st_index].channels[ch].sky_frequency
                    bandwidth_Hz =  ovex.st[st_index].channels[ch].bandwidth
                    if bandwidth_Hz > self.max_channel_bandwidth:
                        self.max_channel_bandwidth = bandwidth_Hz
                    net_sb = str( ovex.st[st_index].channels[n].net_sideband.decode() )
                    pcal_space_Hz = ovex.st[st_index].channels[ch].pcal_spacing
                    pcal_base_freq_Hz = ovex.st[st_index].channels[ch].pcal_base_freq
                    self.single_channel_phasor_collections[channel_name] = SingleChannelPhasorCollection()
                    self.single_channel_phasor_collections[channel_name].channel_name = channel_name
                    self.single_channel_phasor_collections[channel_name].sky_frequency = sky_freq
                    self.single_channel_phasor_collections[channel_name].bandwidth = bandwidth_Hz
                    self.single_channel_phasor_collections[channel_name].net_sideband = net_sb
                    self.single_channel_phasor_collections[channel_name].polarization = pol
                    self.single_channel_phasor_collections[channel_name].pcal_spacing = pcal_space_Hz
                    self.single_channel_phasor_collections[channel_name].pcal_base_freq = pcal_base_freq_Hz
                    self.single_channel_phasor_collections[channel_name].sample_rate = self.sample_rate
                    self.single_channel_phasor_collections[channel_name].initialize()


    def extract_pcal(self):
        """read in the station p-cal phasor data for this scan and convert to usable format"""
        #read the station data file
        if os.path.isfile(self.station_data_file):
            station_data = mk4b.mk4sdata(self.station_data_file)

            #construct a channel name and tone lookup-table
            tone_freq_to_index = dict()
            channel_name_to_index = dict()

            #first have to loop over all channels to extract the tone frequencies
            #they are stored sequentially in the freq parameter of each channel
            #their position in this list then indicates the position of the corresponding phasor in the 'acc' array
            for ch in list(range(0,T309_NCHAN)):
                channel_name = str( station_data.t309[0].contents.chan[ch].chan_name.decode() )
                if len(channel_name) != 0:
                    current_freq = station_data.t309[0].contents.chan[ch].freq
                    tone_freq_to_index[int( round(current_freq,0) )] = ch
                    channel_name_to_index[channel_name] = ch

            #we are assuming the tone-lookup and channel order do not change from one AP to another
            num_ap = station_data.n309

            #time since LSB pcal sign flip
            after2012d124 = True
            if self.start_time.year < 2012 or (self.start_time.year == 2012 and self.start_time.day < 124):
                after2012d124 = False

            for channel_name in self.single_channel_phasor_collections.keys():
                scpc = self.single_channel_phasor_collections[channel_name]
                #extract the channel name, sky_frequency, bandwidth and polarization from the vex
                scpc.phasor = np.zeros( (scpc.ntones, num_ap), dtype=complex )
                scpc.phasor_integer = np.zeros( (scpc.ntones, num_ap), dtype='2uint32' )
                ch = channel_name_to_index[channel_name]
                #look up the tone phasors:
                for ti in list(range(0, scpc.ntones)):
                    tone_freq = int( round(scpc.tone_offset_freq_array[ti],0) )
                    if tone_freq in tone_freq_to_index:
                        tone_phasor_index = tone_freq_to_index[tone_freq]
                        scpc.frequency_interval.append( NumericalInterval( scpc.tone_physical_freq_array[ti], 0.0 ) )
                        scpc.time_interval = [None]*num_ap
                        for ap in list(range(0,num_ap)):
                            #assume DIFX correlator (which gives centroid of time interval)
                            pc_time = old_div(station_data.t309[ap].contents.rot, scpc.bandwidth)
                            scpc.time_interval[ap] = NumericalInterval(pc_time, station_data.t309[ap].contents.acc_period)
                            u = station_data.t309[ap].contents.chan[ch].acc[tone_phasor_index][0]
                            v = station_data.t309[ap].contents.chan[ch].acc[tone_phasor_index][1]
                            if  u != 0 or v != 0 :
                                sample_period = 1.0/scpc.sample_rate
                                pcp = self.convert_integer_phasor_to_float(u, v, station_data.t309[ap].contents.acc_period, sample_period)
                                if scpc.net_sideband == 'L':
                                    if after2012d124: #correct for LSB pcal sign flip after 2012y124d
                                        pcp = pcp.conjugate()
                                        #pcal_interp.c says LSB needs sign flip and 180-deg offset to match Mk3 convention, done by flipping sign of real part
                                        #however this gives the wrong sign on LSB generated delays, so don't do this
                                        #pcp = complex( -1.0*pcp.real, pcp.imag)
                                scpc.phasor[ti][ap] = pcp
                            scpc.phasor_integer[ti][ap] = (u,v)
                scpc.compute_time_averaged_phasors()
            #also extract the information about the azimuth and elevation at the site of interest
            #the value returned is the (az, el) model evaluated at the beginning of the scan
            #(i.e. the constant term in the model polynomial)
            self.azimuth = station_data.model[0].t303[0].contents.azimuth[0]
            self.elevation = station_data.model[0].t303[0].contents.elevation[0]
            self.valid = True
        else:
            self.valid = False
            pcc_logger.warning("Warning: failed to locate a station file: " + self.station_data_file)


    def convert_integer_phasor_to_float(self, u, v, acc_period, sample_period=1e-6):
        """convert integer pcal data to double (taken from pcal_interp.c) """

        #correct for 2's complement arithmetic
        if u < TWO31:
            u = u
        else:
            u = u - TWO32

        if v < TWO31:
            v = v
        else:
            v = v - TWO32

        #scale such that 1000 = 100% correlation
        #and match SU phase by shifting 180 degrees
        #what is the origin of the hard-coded value 128?
        pc_real = ( float( u ) * sample_period )/(-128.0 * acc_period)
        pc_imag = ( float( v ) * sample_period )/(-128.0 * acc_period)
        cp = complex(pc_real, pc_imag)
        return cp

    def apply_phase_reference_collection(self, ref_station_spc):
        """Correct this single channel phasor collection by a set of reference phasors for the same channel.
        This rotates the phasor so that the reference phasor collection has a phase of zero for each tone"""
        for ref_channel_name, reference_scpc in ref_station_spc.single_channel_phasor_collections.items():
            if ref_channel_name in self.single_channel_phasor_collections:
                self.single_channel_phasor_collections[ref_channel_name].apply_phase_reference_collection(reference_scpc)

    def get_tone_phasors_grouped_by_band(self, channel_to_band_map):
        """ use the channel to band map to select tones/phasor which should be fit together
        to determine a delay, returns a dictionary contains lists of frequency-phasor pairs
        dictionary keys are the band:pol """
        grouped_phasors = dict()
        for bp in channel_to_band_map.band_pol_to_channel.keys():
            grouped_phasors[bp] = []
            freq_phasor_pair_list = []
            for chan in channel_to_band_map.band_pol_to_channel[bp]:
                if chan in self.single_channel_phasor_collections:
                    scpc = self.single_channel_phasor_collections[chan]
                    for freq_phasor in scpc.freq_phasor_pairs:
                        freq_phasor_pair_list.append(freq_phasor)
            #now we have all the channel-frequency phasors, sort them by frequency, low to high
            freq_phasor_pair_list.sort(key=lambda fq_ph: fq_ph[0])
            grouped_phasors[bp] = freq_phasor_pair_list
        return grouped_phasors

    def get_band_reference_frequencies(self, channel_to_band_map):
        """use the frequencies of the channels which span a band to
        determine an appropriate reference frequency"""
        band_reference_frequencies = dict()
        for bp in channel_to_band_map.band_pol_to_channel.keys():
            max_frequencies = []
            min_frequencies = []
            for chan in channel_to_band_map.band_pol_to_channel[bp]:
                if chan in self.single_channel_phasor_collections:
                    scpc = self.single_channel_phasor_collections[chan]
                    channel_edge1 = scpc.sky_frequency
                    channel_edge2 = scpc.sky_frequency + scpc.sideband_sign*scpc.bandwidth
                    min_frequencies.append( min(channel_edge1, channel_edge2) )
                    max_frequencies.append( max(channel_edge1, channel_edge2) )
            if len(max_frequencies) !=0 and len(min_frequencies) != 0 :
                max_freq = max(max_frequencies)
                min_freq = min(min_frequencies)
                #use the middle of the band as the reference frequency, maybe we should use the DC edge instead?
                ref_freq = (min_freq + max_freq)/2.0
                band_reference_frequencies[bp] = ref_freq
        return band_reference_frequencies

    def fit_band_delay(self, channel_to_band_map, band_pol, delay_fitter, cut_threshold, verbosity=0):
        # #collect and sort the phasor tones by band
        band_tone_phasors = self.get_tone_phasors_grouped_by_band(channel_to_band_map)
        band_reference_frequencies = self.get_band_reference_frequencies(channel_to_band_map)

        #now perform the fit for this band-pol
        if band_pol in band_tone_phasors.keys() and band_pol in band_reference_frequencies.keys():
            band_delay_results = delay_fitter.fit_band_delay(band_tone_phasors[band_pol], band_reference_frequencies[band_pol], cut_threshold, verbosity)
            pcc_logger.debug(self.scan_name + band_pol + " delay = " + str(band_delay_results.delay) + " offset = " + str(band_delay_results.phase_offset) )
            self.fit_results[band_pol] = band_delay_results


################################################################################


class StationExperimentPhasorCollection(object):

    """container class for all station-scan phasor data for the whole experiment """
    def __init__(self, experiment_directory, channel_to_band_map, mk4_site_id, verbosity=0):
        self.exp_dir = os.path.abspath(experiment_directory)
        self.exp_num = os.path.split(self.exp_dir)[1]
        self.channel_to_band_map = channel_to_band_map
        self.mk4_site_id = mk4_site_id
        self.exp_name = ""
        self.station_code = ""
        self.reference_scan_name = ""
        self.reference_scan_object = None
        #list of scan channel phasor collections
        self.verbosity = verbosity
        self.active_band_pol_list = []
        self.root_files = []
        self.sspc_list = []
        self.datfile_list = []

    def initialize(self):
        self.locate_root_files()
        if len(self.root_files) == 0:
            pcc_logger.error("Error: No matching root-files located.")
            sys.exit(1)
        self.read_in_data()

    def locate_root_files(self):
        tmp_root_file_list = ht.recursive_find_root_files(self.exp_dir, sort_list=True, exclude_list=None, max_depth=1)
        pcc_logger.debug("Number of root files found in " + self.exp_dir + " is: " + str(len(tmp_root_file_list)) )
        #now we go through the root_file_list and ensure that there is only one root-file per scan
        root_file_dict = dict()
        for rf in tmp_root_file_list:
            if self.channel_to_band_map.does_ovex_contain_current_channel_setup(rf, self.mk4_site_id):
                scan_dir = os.path.dirname(rf)
                if scan_dir in root_file_dict:
                    pcc_logger.warning("Warning: duplicate root file present! Replacing " + root_file_dict[scan_dir] + " with " + rf)
                root_file_dict[scan_dir] = rf
        self.root_files = list(root_file_dict.values())

    def read_in_data(self):
        self.sspc_list = []
        for rf in self.root_files:
            sspc = StationScanPhaseCalibrationData(self.mk4_site_id, rf)
            if sspc.valid is True:
                self.sspc_list.append(sspc)
        #now sort the list in time order using the scan start time
        self.sspc_list.sort(key = lambda x: x.start_time)
        if len(self.sspc_list) > 0:
            self.station_code = self.sspc_list[0].station_code
            self.exp_name = self.sspc_list[0].exp_name

    def apply_start_as_phase_reference(self):
        """apply the first scan as the phase reference to all other scan-phasor collections """
        #rotates all phasor collections such that
        #the time average of the earliest (reference) phasor collection has
        #a phase of zero for all tones
        pcc_logger.debug("Number of scans = " + str(len(self.sspc_list) ) + " for station: " + self.station_code )
        if len(self.sspc_list) > 0:
            self.reference_scan_object = copy.deepcopy(self.sspc_list[0])
            self.reference_scan_name = self.reference_scan_object.scan_name
            for sspc in self.sspc_list:
                sspc.apply_phase_reference_collection(self.reference_scan_object)

    def apply_arbitrary_scan_as_phase_reference(self,scan_name):
        """apply a selected scan as the phase reference to all other scan-phasor collections """
        #search for the scan we want to use:
        found_scan = False
        for sspc in self.sspc_list:
            if sspc.scan_name == scan_name:
                found_scan = True
                self.reference_scan_object = copy.deepcopy(sspc)
                self.reference_scan_name = self.reference_scan_object.scan_name

        if found_scan is False:
            pcc_logger.warning("Warning, could not find specified phase reference scan: " +  scan_name + " falling back to first scan.")
            self.apply_start_as_phase_reference()
        else:
            for sspc in self.sspc_list:
                sspc.apply_phase_reference_collection(self.reference_scan_object)

    def get_refererence_scan(self):
        return self.reference_scan_object


    def fit_scan_band_delays(self, active_band_pol_list, method='basinhopping',  trim_length=DISCARD_OFFSET, \
        cut_threshold=2.5, outputdir="./", verbosity=0, use_progress_ticker=False):
        """fit each scan-band phasor data for a delay"""

        #first construct the fitter object and
        delay_fitter = pcc_delay_fitting.BandDelayFitter()
        self.active_band_pol_list = active_band_pol_list

        #now for each station-scan phasor data collection, do the fit for the active band-pols
        if use_progress_ticker is True:
            prog_bar = Bar('progress: ', max=len(self.sspc_list))
        for sspc in self.sspc_list:
            for bp in self.active_band_pol_list:
                delay_fitter.channel_bandwidth = sspc.max_channel_bandwidth
                sspc.fit_band_delay(self.channel_to_band_map, bp, delay_fitter, cut_threshold, verbosity)
            if use_progress_ticker is True:
                next(prog_bar)
        if use_progress_ticker is True:
            prog_bar.finish()

    def generate_dat_files(self, pcc_config):
        """create a .dat file containg the proxy-cable-cal delay information """

        if not os.path.exists(pcc_config.output_dir):
            os.makedirs(pcc_config.output_dir)

        station_code = self.station_code
        for bp in self.active_band_pol_list:
            band, pol = bp.split(':')
            datfile_name = "bandmodel." + self.exp_name + "." + self.mk4_site_id + "." + band + "." + pol + ".dat"
            with open(os.path.join(pcc_config.output_dir, datfile_name) ,'w') as datfile:
                if pcc_config.include_headers is True:
                    header_line = "#year doy hour minute second phase_model_midband phase_model_dc delay_model_ps phase_rmse scan_name source_name station_code azimuth elevation\n"
                    datfile.write(header_line)
                for scan in self.sspc_list:
                    year = scan.start_time.year
                    doy = scan.start_time.day
                    hour = scan.start_time.hour
                    minute = scan.start_time.minute
                    second = scan.start_time.second
                    source_name = scan.source_name
                    scan_name = scan.scan_name
                    azi = scan.azimuth
                    ele = scan.elevation
                    if bp in scan.fit_results.keys():
                        band_delay = (scan.fit_results[bp].delay)/pcc_delay_fitting.PICOSECOND
                        phase_model_midband = (scan.fit_results[bp].phase_offset)*(180.0/math.pi)
                        phase_model_dc = (scan.fit_results[bp].model_dc_phase)*(180.0/math.pi)
                        phase_rmse = (scan.fit_results[bp].phase_rmse)*(180.0/math.pi)
                        #print the data line out to file
                        output_line = "  "
                        output_line += str(year) + "  "
                        output_line += str(doy) + "  "
                        output_line += str(hour) + "  "
                        output_line += str(minute) + "  "
                        output_line += str( int(round(second,0) ) ) + "   "
                        output_line += str( round(phase_model_midband,2)) + "   "
                        output_line += str(round(phase_model_dc,2)) + "   "
                        output_line += str(round(band_delay,2)) + "    "
                        output_line += str(round(phase_rmse,2)) + "    "
                        output_line += source_name + "    "
                        output_line += scan_name + "    "
                        output_line += station_code + "    "
                        output_line += str(round(azi,2)) + "    "
                        output_line += str(round(ele,2)) + "    "
                        output_line += "\n"
                        datfile.write(output_line)
                self.datfile_list.append( os.path.join(pcc_config.output_dir, datfile_name) )


################################################################################
################################################################################



class ScanPccBandDelay(object):
    """class to store a single line from a pcc band-pol delay file (e.g. bandmodel.E.A.X.dat) """
    def __init__(self):
        self.year = 0
        self.doy = 0
        self.hour = 0
        self.minute = 0
        self.second = 0
        self.phase_model_midband = 0
        self.phase_model_dc = 0
        self.delay_model_ps = 0
        self.phase_rmse = 0
        self.scan_name = ''
        self.source_name = ''
        self.station_code = ''
        self.azimuth = 0
        self.elevation = 0

    def init_from_string(self,line_string):
        line_items = line_string.split()
        if len(line_items) == 14:
            self.year = int(line_items[0])
            self.doy = int(line_items[1])
            self.hour = int(line_items[2])
            self.minute = int(line_items[3])
            self.second = int(line_items[4])
            self.phase_model_midband = float(line_items[5])
            self.phase_model_dc = float(line_items[6])
            self.delay_model_ps = float(line_items[7])
            self.phase_rmse = float(line_items[8])
            self.source_name = line_items[9]
            self.scan_name = line_items[10]
            self.station_code = line_items[11]
            self.azimuth = float(line_items[12])
            self.elevation = float(line_items[13])

    def print_line(self):
        print( self.year, self.doy, self.hour, self.minute, self.second, self.phase_model_midband, self.phase_model_dc, self.delay_model_ps, self.phase_rmse, self.scan_name, self.source_name, self.station_code, self.azimuth, self.elevation )

    def is_equal_within_tolerance(self,scan_pcc_delay_object, relative_tolerance=1e-3, absolute_tolerance=1e-6):
        if self.year != scan_pcc_delay_object.year:
            return False
        if self.doy != scan_pcc_delay_object.doy:
            return False
        if self.hour != scan_pcc_delay_object.hour:
            return False
        if self.minute != scan_pcc_delay_object.minute:
            return False
        if self.second != scan_pcc_delay_object.second:
            return False
        if self.scan_name != scan_pcc_delay_object.scan_name:
            return False
        if self.source_name != scan_pcc_delay_object.source_name:
            return False
        if self.station_code != scan_pcc_delay_object.station_code:
            return False
        if mk4b.mk4fp_approximately_equal(self.phase_model_midband, scan_pcc_delay_object.phase_model_midband, abs_tol=absolute_tolerance, rel_tol=relative_tolerance) is False:
            return False
        if mk4b.mk4fp_approximately_equal(self.phase_model_dc, scan_pcc_delay_object.phase_model_dc, abs_tol=absolute_tolerance, rel_tol=relative_tolerance) is False:
            return False
        if mk4b.mk4fp_approximately_equal(self.delay_model_ps, scan_pcc_delay_object.delay_model_ps, abs_tol=absolute_tolerance, rel_tol=relative_tolerance) is False:
            return False
        if mk4b.mk4fp_approximately_equal(self.phase_rmse, scan_pcc_delay_object.phase_rmse, abs_tol=absolute_tolerance, rel_tol=relative_tolerance) is False:
            return False
        if mk4b.mk4fp_approximately_equal(self.azimuth, scan_pcc_delay_object.azimuth, abs_tol=absolute_tolerance, rel_tol=relative_tolerance) is False:
            return False
        if mk4b.mk4fp_approximately_equal(self.elevation, scan_pcc_delay_object.elevation, abs_tol=absolute_tolerance, rel_tol=relative_tolerance) is False:
            return False
        return True


################################################################################
################################################################################


class ExperimentPccBandDelay(object):
    """" container class for the proxy-cable-cal delays for a single station over the course of a whole experiment """
    def __init__(self):
        self.experiment_name = ''
        self.station_id = ''
        self.band_name = ''
        self.pol_name = ''
        self.format_flag = 0
        self.scan_pcc_line_list = []

    def read_file(self,filename):
        if os.path.exists( os.path.abspath(filename) ):
            #parse the file name and assign the station, band, pol names
            elem = (os.path.basename( os.path.abspath(filename ) ) ).split('.')
            if len(elem) != 6 or elem[-1] != 'dat':
                pcc_logger.error('Error: file: ' + filename + " does not have a correctly formatted filename")
                sys.exit(1)
            self.experiment_name = str(elem[1])
            self.station_id = str(elem[2])
            self.band_name = str(elem[3])
            self.pol_name = str(elem[4])
            #now open the file and read the data line by line
            invalid_lines = 0
            with open( os.path.abspath(filename) ) as f:
                for line in f:
                    if '#' not in line and len(line.split()) == 14 :
                        scan_pcc_obj = ScanPccBandDelay()
                        scan_pcc_obj.init_from_string(line)
                        self.scan_pcc_line_list.append(scan_pcc_obj)
                    elif '#' not in line:
                        invalid_lines += 1

            if invalid_lines != 0:
                pcc_logger.warning("Warning: could not read " + invalid_lines + " invalid data lines in file: " + filename)


################################################################################
################################################################################


class ExperimentMultibandDelayAverager(object):
    """simple class to average together the proxy-cable-cal delays of multple band-pols """
    def __init__(self):
        self.band_data_list = []
        self.bands = ''
        self.pols = ''
        self.experiment_name = ''
        self.station_code = ''
        self.scan_year = []
        self.scan_doy = []
        self.scan_hour = []
        self.scan_min = []
        self.scan_sec = []
        self.mean_scan_delay = []
        self.scan_name = []
        self.src_name = []
        self.scan_id = []
        self.file_lines = []
        self.scan_abs_time = []
        self.scan_rel_time = []
        self.scan_az = []
        self.scan_el = []
        self.mean_delay = []
        self.band_diff = []
        self.dual_pol_mean_delay = []

    def add_band_data(self, exp_pcc_band_delay_dat):
        """append the data for a particular band-pol """
        self.station_code = exp_pcc_band_delay_dat.scan_pcc_line_list[0].station_code
        self.band_data_list.append(exp_pcc_band_delay_dat)
        self.bands += exp_pcc_band_delay_dat.band_name
        self.pols += exp_pcc_band_delay_dat.pol_name
        #sorted/capitalized/unique
        self.bands = "".join( sorted(set((self.bands.upper()).strip())) )
        self.pols = "".join( sorted(set((self.pols.upper()).strip())) )

    def average_band_delays(self):
        """average the delays"""
        n_bands = len(self.band_data_list)

        #first construct a set of scan names, so we know what data is available (and can deal with missing scans in one or more band/pols)
        scan_name_set = set()
        for n in list(range(0,n_bands)):
            for scan_line in self.band_data_list[n].scan_pcc_line_list:
                scan_name_set.add( scan_line.scan_name )

        scan_name_list = list(scan_name_set)
        n_scans = len(scan_name_list)

        #collect the data from each band
        for s in list(range(0,n_scans)):
            missing_bands = []
            scan_line_list = []
            scan_name = scan_name_list[s]
            #find the data for this scan, brute force search with no finess!
            for n in list(range(0,n_bands)):
                found = False
                for scan_line in self.band_data_list[n].scan_pcc_line_list:
                    if scan_line.scan_name == scan_name:
                        scan_line_list.append( scan_line )
                        found = True
                        break
                if found == False:
                    missing_bands.append( self.band_data_list[n].band_name + self.band_data_list[n].pol_name )
            #we check that the time tags match before averaging
            year = scan_line_list[0].year
            doy = scan_line_list[0].doy
            hour = scan_line_list[0].hour
            minute = scan_line_list[0].minute
            second = scan_line_list[0].second
            #scan_name = scan_line_list[0].scan_name  #if this is not assigned, it will be missing
            source_name = scan_line_list[0].source_name
            valid = True
            delay_list = []
            mean_delay = 0.0
            if len(scan_line_list) == 0:
                valid = False
                pcc_logger.warning("Warning, no data found for scan: " + scan_name + " skipping.")
            #warn if a band is missing from the data
            if len(scan_line_list) != n_bands:
                pcc_logger.warning("Warning, data missing for scan: " + scan_name + "from: " + str(missing_bands))
            for sc in scan_line_list:
                if doy != sc.doy or hour != sc.hour or minute != sc.minute or second != sc.second or source_name != sc.source_name:
                    pcc_logger.warning("Warning, scan time stamps do match, skipping: " + scan_name)
                    valid = False
                #convert ps to sec
                delay_list.append(sc.delay_model_ps)
            if valid is True:
                mean_delay =  round(np.mean(delay_list),2)
                #delay_stddev = np.std(delay_list)
                self.scan_year.append(year)
                self.scan_doy.append(doy)
                self.scan_hour.append(hour)
                self.scan_min.append(minute)
                self.scan_sec.append(second)
                self.mean_scan_delay.append(mean_delay)
                self.src_name.append(source_name)
                self.scan_id.append(scan_name)

                #we need to convert doy to month day
                scan_datetime = datetime.datetime(year,1,1) + datetime.timedelta(days=doy-1, hours=hour, minutes=minute, seconds=second)
                scan_date = scan_datetime.date()
                #create a file line for this delay
                #	yyyy	mo	dd	hh	mm    delay(sec)  scan_id
                f_line = "  " + str(scan_date.year) + "    " + str(scan_date.month).zfill(2)  \
                + "    " + str(scan_date.day).zfill(2) + "    " + str(hour).zfill(2) + "    " + str(minute).zfill(2) \
                + "    " + str(second).zfill(2)  + "    " + str( mean_delay*1e-12) + "    " + str(source_name) + "    " + str(scan_name) #convert delay from picosec to sec
                self.file_lines.append(f_line)
        self.file_lines.sort()

################################################################################
################################################################################


class PccConfiguration(object):
    """container class of configuration parameters controlling the proxy-cable-cal process"""
    def __init__(self):
        self.stations = ''
        self.exp_dir = './'
        self.mode = 'VGOS' #default is VGOS, other types (e.g. MIXE-MODE) are experimental for the time being, so this is not allowed to be changed
        self.band_list = [] #empty defaults to doing all bands available in the channel-band map
        self.pol_list = [] #empty defaults to doing all bands available in the channel-band map
        self.fit_method = 'basinhopping' #For the time being this is fixed...there are not other methods in use, but we may change this in the future
        self.cut_threshold = 2.5 #strip tones which have a phase residual larger than cut_threshold*sigma
        self.scan_start_trim_length = DISCARD_OFFSET  #length of time to trim from  the start each scan when computing the time-weighted average phasors (default: 2 sec)
        self.reference_scan = '' #typically the first scan of the session is the reference scan, but we can use any scan we like
        self.include_headers = False
        self.output_dir = './'
        self.use_progress_ticker = False
        self.verbosity = 0


################################################################################
################################################################################

def process_experiment(pcc_config):

    chan_map = ChannelToBandMap(pcc_config.mode)
    all_band_pol_list = chan_map.band_pol_to_channel.keys()
    active_band_pol_list = chan_map.construct_band_pol_keys(pcc_config.band_list, pcc_config.pol_list)

    #if unset, default to doing all band-pols
    if len(active_band_pol_list) == 0:
        active_band_pol_list = all_band_pol_list

    station_data = dict()
    for st in pcc_config.stations:
        sepc = StationExperimentPhasorCollection(pcc_config.exp_dir, chan_map, st)
        sepc.initialize()
        if pcc_config.reference_scan == '':
            sepc.apply_start_as_phase_reference()
        else:
            sepc.apply_arbitrary_scan_as_phase_reference(pcc_config.reference_scan)
        #fit band delays
        sepc.fit_scan_band_delays(active_band_pol_list, method=pcc_config.fit_method, trim_length=pcc_config.scan_start_trim_length, \
            cut_threshold=pcc_config.cut_threshold, outputdir=pcc_config.output_dir, verbosity=pcc_config.verbosity, use_progress_ticker=pcc_config.use_progress_ticker)
        sepc.generate_dat_files(pcc_config)
        station_data[st] = sepc

    return station_data
