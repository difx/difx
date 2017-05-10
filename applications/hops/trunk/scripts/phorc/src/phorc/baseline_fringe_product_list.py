import numpy as np

class baseline_fringe_product_list(object):

    #default values for init
    def __init__(self):
        self.root_id = ""
        self.scan_name = ""
        self.baseline = ""
        self.associated_root_file = ""
        self.control_filename = "" #eventually we should use hash of control file contents to differentiate
        self.xx_obj = None
        self.xy_obj = None
        self.yy_obj = None
        self.yx_obj = None
        self.dtec_mean = 0;
        self.dtec_stddev = 0;
        self.dtec_mdev = 0;
        self.mean_snr = 0;
        self.min_snr = 0;
        self.max_snr = 0;

    def __eq__(self, other):
        if (self.root_id == other.root_id) and (self.baseline == other.baseline) and (self.control_filename == other.control_filename):
            return True
        else:
            return False

    def __hash__(self):
        return hash(self.root_id + self.baseline + self.control_filename)

    def is_complete(self):
        if self.xx_obj != None and self.xy_obj != None and self.yy_obj != None and self.yx_obj != None:
            return True
        else:
            return False

    def init_values(self):
        #compute any internal data members after pol prod fringes are set
        self.dtec_mean = self.get_dtec_mean()
        self.dtec_stddev = self.get_dtec_stddev()
        self.dtec_mdev = self.get_dtec_max_deviation()
        self.mean_snr = self.get_mean_snr()
        self.min_snr = self.get_min_snr()
        self.max_snr = self.get_max_snr()

    def add_fringe_object(self, obj):
        if obj.pol_product == "XX":
            self.xx_obj = obj
            if self.associated_root_file == "":
                self.associated_root_file = obj.associated_root_file
            elif self.associated_root_file != obj.associated_root_file:
                print "Non-matching root file detected in baseline_fringe_product_list!"
            return
        if obj.pol_product == "XY":
            self.xy_obj = obj
            if self.associated_root_file == "":
                self.associated_root_file = obj.associated_root_file
            elif self.associated_root_file != obj.associated_root_file:
                print "Non-matching root file detected in baseline_fringe_product_list!"
            return
        if obj.pol_product == "YY":
            self.yy_obj = obj
            if self.associated_root_file == "":
                self.associated_root_file = obj.associated_root_file
            elif self.associated_root_file != obj.associated_root_file:
                print "Non-matching root file detected in baseline_fringe_product_list!"
            return
        if obj.pol_product == "YX":
            self.yx_obj = obj
            if self.associated_root_file == "":
                self.associated_root_file = obj.associated_root_file
            elif self.associated_root_file != obj.associated_root_file:
                print "Non-matching root file detected in baseline_fringe_product_list!"
            return
            
    def get_dtec_list(self):
        dtec_list = [self.xx_obj.dtec, self.xy_obj.dtec, self.yy_obj.dtec, self.yx_obj.dtec]
        return dtec_list

    def get_dtec_stddev(self):
        dtec_list = [self.xx_obj.dtec, self.xy_obj.dtec, self.yy_obj.dtec, self.yx_obj.dtec]
        return np.std(dtec_list)

    def get_dtec_mean(self):
        dtec_list = [self.xx_obj.dtec, self.xy_obj.dtec, self.yy_obj.dtec, self.yx_obj.dtec]
        return np.mean(dtec_list)

    def get_dtec_max_deviation(self):
        dtec_list = [self.xx_obj.dtec, self.xy_obj.dtec, self.yy_obj.dtec, self.yx_obj.dtec]
        dtecmean = np.mean(dtec_list)
        mdev = 0
        for x in dtec_list:
            if abs(x - dtecmean) >= mdev:
                mdev = abs(x-dtecmean)
        return mdev

    def get_mean_snr(self):
        snr_list = [self.xx_obj.snr, self.xy_obj.snr, self.yy_obj.snr, self.yx_obj.snr]
        return np.mean(snr_list)

    def get_min_snr(self):
        snr_list = [self.xx_obj.snr, self.xy_obj.snr, self.yy_obj.snr, self.yx_obj.snr]
        return min(snr_list)

    def get_max_snr(self):
        snr_list = [self.xx_obj.snr, self.xy_obj.snr, self.yy_obj.snr, self.yx_obj.snr]
        return max(snr_list)


class scan_baseline_list(object):
    
        #default values for init
        def __init__(self):
            self.root_id = ""
            self.scan_name = ""
            self.associated_root_file = ""
            self.control_filename = "" #eventually we should use hash of control file contents to differentiate
            self.mean_snr = 0
            self.max_snr = 0
            self.min_snr = 0
            self.mean_dtec_mdev = 0
            self.max_dtec_mdev = 0
            self.min_dtec_mdev = 0
            self.baseline_list = []
            
        def init_values(self):
            if len(self.baseline_list) > 0:
                self.mean_snr = self.get_mean_snr()
                self.mean_dtec_mdev = self.get_mean_dtec_mdev()
                self.root_id = self.baseline_list[0].root_id
                self.scan_name = self.baseline_list[0].scan_name
                self.associated_root_file = self.baseline_list[0].associated_root_file
                self.max_snr = self.get_max_snr()
                self.min_snr = self.get_min_snr()
                self.max_dtec_mdev = self.get_max_dtec_mdev()
                self.min_dtec_mdev = self.get_min_dtec_mdev()
        
        def add_baseline_collection(self, baseline_collection):
            self.baseline_list.append(baseline_collection)
            
        def get_mean_snr(self):
            msnr = []
            for x in self.baseline_list:
                msnr.append(x.get_mean_snr())
            return np.mean(msnr)
            
        def get_max_snr(self):
            msnr = []
            for x in self.baseline_list:
                msnr.append(x.get_max_snr())
            return max(msnr)
        
        def get_min_snr(self):
            msnr = []
            for x in self.baseline_list:
                msnr.append(x.get_min_snr())
            return min(msnr)

        def get_mean_dtec_mdev(self):
            dtec_mdev = []
            for x in self.baseline_list:
                dtec_mdev.append(x.get_dtec_max_deviation())
            return np.mean(dtec_mdev)
            
        def get_max_dtec_mdev(self):
            dtec_mdev = []
            for x in self.baseline_list:
                dtec_mdev.append(x.get_dtec_max_deviation())
            return max(dtec_mdev)
            
        def get_min_dtec_mdev(self):
            dtec_mdev = []
            for x in self.baseline_list:
                dtec_mdev.append(x.get_dtec_max_deviation())
            return min(dtec_mdev)
