import os
import sys

#hops package python libs
import mk4
import afio
import hopstest as ht
from math import fmod
from math import floor

#ported from time_to_int.c in hops/sub/util
def time_to_int(year, day, hour, min, sec):
    if year == 0:
        year80 = 0
    else:
        year80 = year % 100 - 80

    if year80 < 0:
        year80 += 100;

    nleaps = (year80 + 3) /4
    secs_since_80 = year80*31536000 + (day+nleaps-1)*86400 + hour*3600 + min*60 + sec
    return secs_since_80

class fringe_file_handle(object):

    #default values for init
    def __init__(self):
        self.is_valid = False #has the file been loaded
        self.fringe_data = None

        #while the whole data file is available in memory
        #these are common and/or useful data quatities
        self.filename = "" #disk location of mk4_fringe object
        self.root_id = ""
        self.scan_name = ""
        self.exp_name = ""
        self.pol_product = ""
        self.dtec = 0
        self.nchans = 0
        self.associated_root_file = ""

        self.control_filename = ""

        #these quantities mostly mirror the alist fringe sum
        #TODO: MAKE SURE THESE ALL WORK
        self.version =  0
        self.fname =  ""
        self.expt_no =  0
        self.extent_no =  0
        self.length =  0
        self.corel_vers = ""
        self.procdate =  0
        self.time_tag =  0
        self.ssec =  0
        self.source =  ""
        self.baseline =  ""
        self.quality =  ""
        # self.freq_code =  ""
        self.mode =  ""
        self.no_freq =  0
        self.archiv =  0
        self.reftape =  ""
        self.remtape =  ""
        self.amp =  0.0
        self.snr =  0.0
        self.resid_phas =  0.0
        self.sbdelay =  0.0
        self.mbdelay =  0.0
        self.delay_rate =  0.0
        # self.esdesp =  0
        # self.epoch = 0
        self.total_phas =  0.0
        self.total_rate =  0.0
        self.total_mbdelay =  0.0
        self.total_sbresid =  0.0
        self.ambiguity =  0.0
        self.pcals = 0
        self.root_id =  ""
        self.ref_freq =  0.0
        # self.datatype =  ""
        self.ref_elev =  0.0
        self.rem_elev =  0.0
        self.ref_az =  0.0
        self.rem_az =  0.0
        self.u =  0.0
        self.v =  0.0
        self.parents = 0
        self.duration =  0
        # self.offset =  0
        self.scan_offset = 0
        self.lags = 0
        self.phase_snr = 0.0
        self.srch_cotime =  0
        self.noloss_cotime =  0
        self.scan_id =  ""
        self.polarization =  ""
        self.errcode =  ""
        # self.ra_hrs =  0.0
        # self.dec_deg =  0.0
        self.resid_delay = 0.0

        self.control_file_hash = 0
        self.set_string_hash = 0

        #these are residual quantities
        self.phase = 0
        self.sbdelay = 0
        self.mbdelay = 0
        self.delay_rate = 0

    def load(self,filename):
        self.is_valid = False
        if os.path.isfile(filename):
            self.is_valid = True
            self.filename = os.path.abspath(filename)
            abs_path = self.filename
            scan_dir = os.path.dirname(abs_path)
            exp_dir = os.path.dirname(scan_dir)
            root_filename = os.path.split(abs_path)[1]
            self.scan_name = os.path.split(scan_dir)[1]
            self.exp_name = os.path.split(exp_dir)[1]
            self.root_id = self.filename[-6:]
            self.fringe_data = mk4.mk4fringe(filename)
            self.init_data_members()
            self.associated_root_file = scan_dir + "/" + self.source + "." + self.root_id
            mk4.clear_mk4fringe(self.fringe_data)
            self.fringe_data = None

    def init_data_members(self):
        self.dtec = self.fringe_data.t201.contents.dispersion
        self.control_filename = self.fringe_data.t204.contents.control_file

        self.nchans = 16
        if self.fringe_data.t205.contents.version_no != "00":
            self.nchans = 64
        pol_error = False
        #this logic comes from alist, in summarize_mk4fringe.c, line 86
        refpol = ' '
        rempol = ' '
        for n in range(0,self.nchans):
            if self.fringe_data.t205.contents.ffit_chan[n].ffit_chan_id != ' ':
                for j in range(0,4):
                    channel_index = self.fringe_data.t205.contents.ffit_chan[n].channels[j]
                    if not (channel_index < 0):
                        if refpol == ' ':
                            refpol = self.fringe_data.t203.contents.channels[channel_index].refpol
                        elif self.fringe_data.t203.contents.channels[channel_index].refpol != refpol:
                            pol_error = True
                        if rempol == ' ':
                            rempol = self.fringe_data.t203.contents.channels[channel_index].rempol
                        elif self.fringe_data.t203.contents.channels[channel_index].rempol != rempol:
                            pol_error = True
        if pol_error:
            print "Error, inconsistent polarization products in file: ", self.filename
            refpol = '*'
            rempol = '*'
            self.is_valid = False
        self.pol_product = refpol + rempol
        self.polarization = self.pol_product

        #most of the a-list fringe sum data
        self.version = 0
        self.expt_no = self.fringe_data.t200.contents.expt_no
        isec = self.fringe_data.t200.contents.scantime.second + 0.5
        self.time_tag = time_to_int (self.fringe_data.t200.contents.scantime.year,
                                       self.fringe_data.t200.contents.scantime.day,
                                       self.fringe_data.t200.contents.scantime.hour,
                                       self.fringe_data.t200.contents.scantime.minute,
                                       isec)

        self.scan_offset = 0
        self.scan_id = self.fringe_data.t200.contents.scan_name
        self.duration = self.fringe_data.t200.contents.stop_offset - self.fringe_data.t200.contents.start_offset
        self.archiv = self.expt_no
        self.baseline = self.fringe_data.t202.contents.baseline

        #count frequencies from type 205
        for i in range(0,self.nchans):
            if (self.fringe_data.t205.contents.ffit_chan[i].ffit_chan_id == ' '):
                break
        self.no_freq = i

        self.length = self.fringe_data.t206.contents.intg_time + 0.5
        isec = self.fringe_data.t200.contents.fourfit_date.second + 0.5
        self.procdate = time_to_int (self.fringe_data.t200.contents.fourfit_date.year,
                                                self.fringe_data.t200.contents.fourfit_date.day,
                                                self.fringe_data.t200.contents.fourfit_date.hour,
                                                self.fringe_data.t200.contents.fourfit_date.minute,
                                                isec)
        self.source = self.fringe_data.t201.contents.source
        self.quality = self.fringe_data.t208.contents.quality
        self.errcode = self.fringe_data.t208.contents.errcode
        self.amp = self.fringe_data.t208.contents.amplitude * 10000.0
        self.snr = self.fringe_data.t208.contents.snr
        self.lags = self.fringe_data.t202.contents.nlags
        # All phases are 0-360
        self.resid_phas = fmod ( (self.fringe_data.t208.contents.resphase + 360.0), 360.0)
        self.sbdelay = self.fringe_data.t208.contents.resid_sbd
        self.mbdelay = self.fringe_data.t208.contents.resid_mbd
        self.delay_rate = self.fringe_data.t208.contents.resid_rate * 1.0E6

        self.ref_freq = self.fringe_data.t205.contents.ref_freq
        self.total_phas = fmod( (self.fringe_data.t208.contents.totphase + 360.0), 360.0)
        self.total_rate = self.fringe_data.t208.contents.tot_rate
        self.total_mbdelay = self.fringe_data.t208.contents.tot_mbd
        self.total_sbresid = self.fringe_data.t208.contents.tot_sbd - self.total_mbdelay
        self.ambiguity = self.fringe_data.t208.contents.ambiguity
        self.ref_elev = self.fringe_data.t202.contents.ref_elev
        self.rem_elev = self.fringe_data.t202.contents.rem_elev
        self.ref_az = self.fringe_data.t202.contents.ref_az
        self.rem_az = self.fringe_data.t202.contents.rem_az
        self.u = self.fringe_data.t202.contents.u * 0.2062648
        self.v = self.fringe_data.t202.contents.v * 0.2062648
        self.resid_delay = self.mbdelay + self.ambiguity * floor((self.sbdelay - self.mbdelay) / self.ambiguity + 0.5)
        
        if self.fringe_data.t222:
            self.control_file_hash = self.fringe_data.t222.contents.control_hash
            self.set_string_hash = self.fringe_data.t222.contents.setstring_hash

    def load_file_data(self):
        if os.path.isfile(filename):
            self.fringe_data = mk4.mk4fringe(filename)

    def relase_file_data(self):
        mk4.clear_mk4fringe(self.fringe_data)
        self.fringe_data = None
        
class residual_data(object):
    
    #default values for init
    def __init__(self):
        self.is_valid = False
        self.filename = ""
        self.baseline = ""
        self.polprod = ""
        self.dtec = 0
        self.phase_residuals = []
        self.phase_corrections = []
