""" module for manipulating, accessing and grouping fringe files"""

#core imports
from __future__ import print_function
from __future__ import absolute_import
from __future__ import division
from builtins import str
from builtins import object
from past.utils import old_div
from builtins import range
import os
import sys
import math

#non-core imports
import numpy as np
import scipy.stats

#hops package python libs
import mk4b
import hopstestb as ht

#local imports
from . import utility
from . import report_lib


class FringeFileHandle(object):
    """container class for data associated with a fringe file """
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
        self.quality = 0
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
            self.scan_name = os.path.split(scan_dir)[1]
            self.exp_name = os.path.split(exp_dir)[1]
            self.root_id = self.filename[-6:]
            self.fringe_data = mk4b.mk4fringe(filename)
            self.init_data_members()
            self.associated_root_file = scan_dir + "/" + self.source + "." + self.root_id
            mk4b.clear_mk4fringe(self.fringe_data)
            self.fringe_data = None

    def init_data_members(self):
        """ initialize the data members from the fringe file """
        self.dtec = self.fringe_data.t201.contents.dispersion
        self.control_filename = str( self.fringe_data.t204.contents.control_file.decode() )

        self.nchans = 16
        if str( self.fringe_data.t205.contents.version_no.decode() ) != "00":
            self.nchans = 64
        pol_error = False
        #this logic comes from alist, in summarize_mk4fringe.c, line 86
        refpol = ' '
        rempol = ' '
        for n in list(range(0,self.nchans)):
            if str( self.fringe_data.t205.contents.ffit_chan[n].ffit_chan_id.decode() ) != ' ':
                for j in list(range(0,4)):
                    channel_index = self.fringe_data.t205.contents.ffit_chan[n].channels[j]
                    if not channel_index < 0:
                        ch_refpol_value = str( self.fringe_data.t203.contents.channels[channel_index].refpol.decode() )
                        ch_rempol_value = str( self.fringe_data.t203.contents.channels[channel_index].rempol.decode() )
                        if refpol == ' ':
                            refpol = ch_refpol_value
                        elif ch_refpol_value != refpol:
                            pol_error = True
                        if rempol == ' ':
                            rempol = ch_rempol_value
                        elif ch_rempol_value != rempol:
                            pol_error = True
        if pol_error:
            print( "Error, inconsistent polarization products in file: ", self.filename)
            refpol = '*'
            rempol = '*'
            self.is_valid = False
        self.pol_product = refpol + rempol
        self.polarization = self.pol_product

        #most of the a-list fringe sum data
        self.version = 0
        self.expt_no = self.fringe_data.t200.contents.expt_no
        isec = self.fringe_data.t200.contents.scantime.second + 0.5
        self.time_tag = utility.time_to_int( \
            self.fringe_data.t200.contents.scantime.year, \
            self.fringe_data.t200.contents.scantime.day, \
            self.fringe_data.t200.contents.scantime.hour, \
            self.fringe_data.t200.contents.scantime.minute, isec \
        )

        self.scan_offset = 0
        self.scan_id = str( self.fringe_data.t200.contents.scan_name.decode() )
        self.duration = self.fringe_data.t200.contents.stop_offset - self.fringe_data.t200.contents.start_offset
        self.archiv = self.expt_no
        self.baseline = str( self.fringe_data.t202.contents.baseline.decode() )

        #count frequencies from type 205
        i = 0
        for j in list(range(0,self.nchans)):
            if self.fringe_data.t205.contents.ffit_chan[j].ffit_chan_id == ' ':
                i = j
                break
        self.no_freq = i

        self.length = self.fringe_data.t206.contents.intg_time + 0.5
        isec = self.fringe_data.t200.contents.fourfit_date.second + 0.5
        self.procdate = utility.time_to_int( \
            self.fringe_data.t200.contents.fourfit_date.year, \
            self.fringe_data.t200.contents.fourfit_date.day, \
            self.fringe_data.t200.contents.fourfit_date.hour, \
            self.fringe_data.t200.contents.fourfit_date.minute,isec \
        )
        self.source =  str( self.fringe_data.t201.contents.source.decode() )
        qcode = str( self.fringe_data.t208.contents.quality.decode() )
        self.quality = int( qcode )
        self.errcode = str( self.fringe_data.t208.contents.errcode.decode() )
        self.amp = self.fringe_data.t208.contents.amplitude * 10000.0
        self.snr = self.fringe_data.t208.contents.snr
        self.lags = self.fringe_data.t202.contents.nlags
        # All phases are 0-360
        self.resid_phas = utility.limit_periodic_quantity_to_range( self.fringe_data.t208.contents.resphase, 0 , 360.0)
        self.sbdelay = self.fringe_data.t208.contents.resid_sbd
        self.mbdelay = self.fringe_data.t208.contents.resid_mbd
        self.delay_rate = self.fringe_data.t208.contents.resid_rate * 1.0E6

        self.ref_freq = self.fringe_data.t205.contents.ref_freq
        self.total_phas =  utility.limit_periodic_quantity_to_range( self.fringe_data.t208.contents.totphase, 0 , 360.0)
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
        self.resid_delay = self.mbdelay + self.ambiguity * math.floor(old_div((self.sbdelay - self.mbdelay), self.ambiguity) + 0.5)

        if self.fringe_data.t222:
            self.control_file_hash = self.fringe_data.t222.contents.control_hash
            self.set_string_hash = self.fringe_data.t222.contents.setstring_hash

    def load_file_data(self):
        """ read the data of mk4 type-2 file """
        if os.path.isfile(self.filename):
            self.fringe_data = mk4b.mk4fringe(self.filename)

    def relase_file_data(self):
        """ release the fringe file data object """
        mk4b.clear_mk4fringe(self.fringe_data)
        self.fringe_data = None

    def get_channel_frequency_tuples(self):
        """ get the frequency associated with each channel id"""
        self.load_file_data() #make sure data is loaded
        #get the 205 object so we can look up the channels we have
        t205 = self.fringe_data.t205.contents
        #get the 203 object so we can look of the frequencies of the channels
        t203 = self.fringe_data.t203.contents
        channel_tuples = []
        for n in list(range(0,len(t205.ffit_chan))):
            ch_id = str( t205.ffit_chan[n].ffit_chan_id.decode() )
            ch_index = t205.ffit_chan[n].channels[0]
            ch_freq = 0
            if ch_index != -1:
                ch_entry = t203.channels[ch_index]
                ch_freq = ch_entry.ref_freq
                if ch_entry.ref_freq != ch_entry.rem_freq:
                    print( "error frequency mismatch in type_203" )
                    sys.exit()
                ch_tup = [ch_id, ch_index, ch_freq]
                channel_tuples.append(ch_tup)
        return channel_tuples

################################################################################

class PhaseResidualData(object):
    """ container class to store the phase residuals for each channel """
    #default values for init
    def __init__(self):
        self.is_valid = False
        self.filename = ""
        self.scan_name = ""
        self.baseline = ""
        self.polprod = ""
        self.dtec = 0
        self.snr = 0
        self.phase_residuals = dict()
        self.channel_phasor_amplitudes = dict()
        self.phase_corrections = dict()

    def extract(self, filename):
        self.is_valid = False
        if os.path.isfile(filename):
            pol_product = ht.get_file_polarization_product(filename)
            ff_pp_list = ht.get_file_polarization_product_provisional(filename)
            #this is a pseudo-stokes file
            if 'I' in ff_pp_list:
                pol_product = 'I'
            #nominal pol-product (this will be correct provided the polarization-product is not pseudo-stokes)
            ff = mk4b.mk4fringe(filename)
            bline = str( ff.t202.contents.baseline.decode() )
            dtec = ff.t201.contents.dispersion
            self.snr = ff.t208.contents.snr
            self.filename = filename
            self.scan_name = str( ff.t200.contents.scan_name.decode() )
            self.is_valid = True
            self.baseline = bline
            self.polprod = pol_product
            self.dtec = dtec

            #loop over the type_205 entries, to determine which channels are
            #actually stored in the type 210's and what their fourfit names are
            for i in list(range(0, 64)): #64 is the max number of supported channels in fourfit
                chan_id = str( ff.t205.contents.ffit_chan[i].ffit_chan_id.decode() )
                if chan_id != ' ' and len(chan_id) != 0:
                    #this channel is valid, add it to the phase_residuals dictionary
                    self.phase_residuals[chan_id] = (ff.t210.contents.amp_phas[i].phase)
                    self.channel_phasor_amplitudes[chan_id] = (ff.t210.contents.amp_phas[i].ampl)

            #now compute the phase corrections
            self.phase_corrections = self.phase_residuals.copy()
            phase_list_proxy = []
            channel_list = []
            for ch, ch_phase in list(self.phase_corrections.items()):
                channel_list.append(ch)
                phase_list_proxy.append( ch_phase )

            #invert, unwrap and remove mean phase
            phase_list_proxy = [(-1.0*(old_div(math.pi,180.0)))*x for x in phase_list_proxy] #negate and convert to radians
            phase_list_proxy = np.unwrap(phase_list_proxy) #arguments must be in radians
            phase_list_proxy = [(old_div(180.0,math.pi))*x for x in phase_list_proxy] #convert back to degrees
            mean_phase = scipy.stats.circmean( np.asarray(phase_list_proxy), high=180.0, low=-180.0) #compute circular mean phase
            phase_list_proxy = [ utility.limit_periodic_quantity_to_range( (x - mean_phase), -180.0, 180.0 ) for x in phase_list_proxy] #subtract off the mean and limit to [-180,180)

            #assign the corrections
            for i in list(range(0, len(phase_list_proxy))):
                self.phase_corrections[ channel_list[i] ] = phase_list_proxy[i]




################################################################################
class SingleBaselinePolarizationProductCollection(report_lib.JsonSerializableObject):
    """ container class to store the fringe data associated with each polarization product for a particular scan/baseline """
    #default values for init
    def __init__(self):
        self.root_id = ""
        self.scan_name = ""
        self.baseline = ""
        self.associated_root_file = ""
        self.control_filename = "" #use hash of control file contents to distinguish fringe files processed in different ways
        self.control_file_hash = 0
        self.fringe_objects = dict()
        self.required_polprod_list = [] #this normally empty, but can be used to over-ride the concept of a 'complete' collection if it is explicity set
        self.cached_required_polprod_list = []
        self.dtec_mean = 0.0
        self.dtec_stddev = 0.0
        self.dtec_mdev = 0.0
        self.mean_snr = 0.0
        self.min_snr = 0.0
        self.max_snr = 0.0

    def __eq__(self, other):
        if other != None:
            if (self.root_id == other.root_id) and (self.baseline == other.baseline) and (self.control_file_hash == other.control_file_hash):
                return True
        return False

    def __hash__(self):
        return hash(self.root_id + self.baseline + str(self.control_file_hash) )

    def export_json(self):
        """ dump this object to a dictionary for inspection """
        #create a dict to be used as json output, here we explicity do not include the fringe objects!
        json_dict = dict()
        json_dict['root_id'] = self.root_id
        json_dict['associated_root_file'] = self.associated_root_file
        json_dict['scan_name'] = self.scan_name
        json_dict['baseline'] = self.baseline
        json_dict['control_filename'] = self.control_filename
        json_dict['control_file_hash'] = self.control_file_hash
        json_dict['dtec_mean'] = self.get_dtec_mean()
        json_dict['dtec_stddev'] = self.get_dtec_stddev()
        json_dict['dtec_mdev'] = self.get_dtec_max_deviation()
        json_dict['mean_snr'] = self.get_mean_snr()
        json_dict['min_snr'] = self.get_min_snr()
        json_dict['max_snr'] = self.get_max_snr()
        return json_dict

    def is_complete(self):
        """determine if we have all of the available polarization products """
        #locate the corresponding corel file, and determine which pol-products are present in it
        #if we have a fringe file associated with each one of these pol-products then we have a complete baseline collection
        if len(self.required_polprod_list) == 0:
            for pp in self.cached_required_polprod_list:
                if pp.upper() not in self.fringe_objects:
                    return False
            return True
        else:
            for pp in self.required_polprod_list:
                if pp.upper() not in self.fringe_objects:
                    return False
            return True

    def polarization_product_is_required(self, pp_check):
        """ indicate if a polarization producted is needed to make this a complete collection """
        if len(self.required_polprod_list) == 0:
            if pp_check.upper() in self.cached_required_polprod_list:
                return True
        else:
            if pp_check.upper() in self.required_polprod_list:
                return True
        return False

    def init_values(self):
        """ compute any internal data members after pol prod fringes are set """
        self.dtec_mean = self.get_dtec_mean()
        self.dtec_stddev = self.get_dtec_stddev()
        self.dtec_mdev = self.get_dtec_max_deviation()
        self.mean_snr = self.get_mean_snr()
        self.min_snr = self.get_min_snr()
        self.max_snr = self.get_max_snr()

    def add_fringe_object(self, obj):
        """add a finge file object that should be associated with this collection """
        obj_pp = obj.pol_product.upper()
        #if this is the first fringe file object, set-up the associated root file, control, etc
        if len(self.fringe_objects) == 0:
            self.root_id = obj.root_id
            self.scan_name = obj.scan_name
            self.baseline = obj.baseline
            self.associated_root_file = obj.associated_root_file
            self.control_filename = obj.control_filename
            self.control_file_hash = obj.control_file_hash
            self.fringe_objects[obj_pp] = obj
            #look up the pol-products available in the corel file, and cache them
            path_to_root_file = os.path.abspath( self.associated_root_file )
            root_file_dir = os.path.dirname(path_to_root_file)
            corel_filename = self.baseline + ".." + self.root_id
            path_to_corel_file = os.path.join(root_file_dir, corel_filename)
            if os.path.isfile(path_to_corel_file):
                self.cached_required_polprod_list = ht.get_polarization_products_present(path_to_corel_file)
        else:
            #check that this fringe file object matches the others
            if obj.root_id == self.root_id and obj.scan_name == self.scan_name \
            and obj.baseline == self.baseline and obj.associated_root_file == self.associated_root_file \
            and obj.control_file_hash == self.control_file_hash and obj.control_filename == self.control_filename:
                self.fringe_objects[obj_pp] = obj

    def get_fringe_object(self, polprod):
        """return the fringe data associated with a particular polarization product"""
        if polprod.upper() in self.fringe_objects:
            return self.fringe_objects[polprod.upper()]
        else:
            return None

    def get_dtec_list(self):
        """ get the baseline dTEC associated with fringe of each polarization product """
        dtec_list = []
        for val in list(self.fringe_objects.values()):
            dtec_list.append(val.dtec)
        return dtec_list

    def get_dtec_stddev(self):
        """ get the std. dev. of the dTEC values """
        dtec_list = self.get_dtec_list()
        return np.std(dtec_list)

    def get_dtec_mean(self):
        """ get the mean dTEC value """
        dtec_list = self.get_dtec_list()
        return np.mean(dtec_list)

    def get_dtec_max_deviation(self):
        """ get the larged difference between a dTEC value and the mean """
        dtec_list = self.get_dtec_list()
        dtecmean = np.mean(dtec_list)
        mdev = 0.0
        for x in dtec_list:
            if abs(x - dtecmean) >= mdev:
                mdev = abs(x-dtecmean)
        return mdev

    def get_snr_list(self):
        """ get the SNRs of each polarization product fringe """
        snr_list = []
        for val in list(self.fringe_objects.values()):
            snr_list.append(val.snr)
        return snr_list

    def get_mean_snr(self):
        snr_list = self.get_snr_list()
        return np.mean(snr_list)

    def get_min_snr(self):
        snr_list = self.get_snr_list()
        return min(snr_list)

    def get_max_snr(self):
        snr_list = self.get_snr_list()
        return max(snr_list)

    def get_phase_residuals(self, pol_product):
        """ get the channel-by-channel phase residual associated with the fringe-fit of each polarization product """
        phresid = PhaseResidualData()
        if pol_product in self.fringe_objects:
            ff_obj = self.fringe_objects[pol_product]
            phresid.extract(ff_obj.filename)
        return phresid

################################################################################
class SingleScanBaselineCollection(object):
    """ a collection of all the fringe-collections (over polarization-products) of each baseline associated with a single scan """
    #default values for init
    def __init__(self):
        self.root_id = ""
        self.scan_name = ""
        self.associated_root_file = ""
        self.control_filename = ""
        self.control_file_hash = 0
        self.mean_snr = 0
        self.max_snr = 0
        self.min_snr = 0
        self.mean_dtec_mdev = 0
        self.max_dtec_mdev = 0
        self.min_dtec_mdev = 0
        self.single_baseline_pp_collection_dict = dict()

    def init_values(self):
        if len(self.single_baseline_pp_collection_dict) > 0:
            self.mean_snr = self.get_mean_snr()
            self.mean_dtec_mdev = self.get_mean_dtec_mdev()
            self.max_snr = self.get_max_snr()
            self.min_snr = self.get_min_snr()
            self.max_dtec_mdev = self.get_max_dtec_mdev()
            self.min_dtec_mdev = self.get_min_dtec_mdev()

    def add_baseline_collection(self, baseline_collection):
        """ add the polarization-product collection of a single baseline """
        if len(self.single_baseline_pp_collection_dict) == 0:
            self.root_id = baseline_collection.root_id
            self.scan_name = baseline_collection.scan_name
            self.associated_root_file = baseline_collection.associated_root_file
            self.control_filename = baseline_collection.control_filename
            self.control_file_hash = baseline_collection.control_file_hash
        self.single_baseline_pp_collection_dict[baseline_collection.baseline] = baseline_collection

    def get_list_of_baselines(self):
        """ a list of baselines with data """
        bl_list = list(self.single_baseline_pp_collection_dict.keys())
        return bl_list

    def get_n_baseline_collections(self):
        return len(self.single_baseline_pp_collection_dict)

    def get_mean_snr(self):
        msnr = []
        for x in list(self.single_baseline_pp_collection_dict.values()):
            msnr.append(x.get_mean_snr())
        return np.mean(msnr)

    def get_max_snr(self):
        msnr = []
        for x in list(self.single_baseline_pp_collection_dict.values()):
            msnr.append(x.get_max_snr())
        return max(msnr)

    def get_min_snr(self):
        msnr = []
        for x in list(self.single_baseline_pp_collection_dict.values()):
            msnr.append(x.get_min_snr())
        return min(msnr)

    def get_mean_dtec_mdev(self):
        """ the mean value of the maximum-dTEC deviation over all baselines """
        dtec_mdev = []
        for x in list(self.single_baseline_pp_collection_dict.values()):
            dtec_mdev.append(x.get_dtec_max_deviation())
        return np.mean(dtec_mdev)

    def get_max_dtec_mdev(self):
        """ the max value of the maximum-dTEC deviation over all baselines """
        dtec_mdev = []
        for x in list(self.single_baseline_pp_collection_dict.values()):
            dtec_mdev.append(x.get_dtec_max_deviation())
        return max(dtec_mdev)

    def get_min_dtec_mdev(self):
        """ the min value of the maximum-dTEC deviation over all baselines """
        dtec_mdev = []
        for x in list(self.single_baseline_pp_collection_dict.values()):
            dtec_mdev.append(x.get_dtec_max_deviation())
        return min(dtec_mdev)

    def get_baselines_present(self):
        """ return list of unique baselines present """
        bl_set = set()
        for blc in list(self.single_baseline_pp_collection_dict.keys()):
            bl_set.add(blc)
        return list(bl_set)

    def get_stations_present(self):
        """ return list of unique station present """
        bl_list = self.get_baselines_present()
        station_set = set()
        for bl in bl_list:
            station_set.add(bl[0])
            station_set.add(bl[1])
        return list(station_set)

    def get_collection_for_baseline(self, bl):
        """ retrieve the single-baseline polarization product collection associated with a particular baseline """
        if bl in self.single_baseline_pp_collection_dict:
            return self.single_baseline_pp_collection_dict[bl]
        else:
            return None
