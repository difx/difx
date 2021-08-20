"""Holds the classes that deal with Vis data"""
from copy import deepcopy
import pyqtgraph as pg
import numpy
import parseDiFX

class AverageVis:
    """handle averaging and storing vis list, also calls plot"""

    def __init__(self, exp_info, aver_amount, plot):
        self.exp_info = exp_info
        self.plot = plot
        self.setup_list(exp_info)
        self.aver_amount = aver_amount
        self.current_vis = 1

    def setup_list(self, exp_info):
        """setup the array to hold all vis'"""
        # gonna add all vis to a list for averaging
        all_vis = {}
        for bl in [b[0].name + "-" + b[1].name for b in exp_info.baselines]:
            all_vis[bl] = []
            for _ in range(exp_info.numfreqs):
                all_vis[bl].append([[], []])
                # array is [bl][freq][pol] where RR = 0
        self.vis_arr = all_vis

    def add(self, vis):
        """Add a vis to the list, return True if need to update the plot"""
        vis_list = self.vis_arr[vis.baseline_name][vis.freq_index][vis.pol]
        vis_list.append(vis)
        # if the lengths is now longer than last time, we are at a new vis for everybody so update graph
        if len(vis_list) > self.current_vis:
            self.current_vis += 1
            return True
        return False

    def handle(self, vis):
        """Deal with a new vis"""
        if not vis.baseline_name in [
            b[0].name + "-" + b[1].name for b in self.plot.baselines
        ]:
            return
        update_plot = self.add(vis)
        if self.aver_amount != 1:
            vis = self.aver(vis)
#        print(vis)
        self.plot.plot(vis)
        if update_plot:
            pg.QtGui.QApplication.processEvents()

    def aver(self, vis_in):
        """handle averaging. vis_in has all the right metadata so we copy it"""
        averaged_vis = deepcopy(vis_in)
        vis_list = self.vis_arr[vis_in.baseline_name][vis_in.freq_index][vis_in.pol]
        av_vis_data = []
        if len(vis_list) >= self.aver_amount:
            for vis in vis_list[-self.aver_amount :]:
                av_vis_data.append(vis.vis)
            av_vis_data = numpy.average(av_vis_data, axis=0)

        else:  # just average all data to now
            for vis in vis_list:
                av_vis_data.append(vis.vis)
            av_vis_data = numpy.average(av_vis_data, axis=0)

        averaged_vis.vis = av_vis_data
        averaged_vis.update()
        return averaged_vis

    def write_aver_all(self):
        """For each baseline write out the total average vis"""
        try:
            with open(self.exp_info.input_file_base+'.fringe', 'wt') as fout:
                fout.write(f"Source: {self.exp_info.source_name}\n")
                baseline_names = [b[0].name + "-" + b[1].name for b in self.plot.baselines]
                for bl in baseline_names:
                    for freq in range(self.exp_info.numfreqs):
                        for pol in range(2):
                            av_vis_data = []
                            vis_list = self.vis_arr[bl][freq][pol]
                            for vis in vis_list:
                                av_vis_data.append(vis.vis)
                            av_vis_data = numpy.average(av_vis_data, axis=0)
                            if len(vis_list) > 0:
                                vis_out = deepcopy(vis_list[0])
                                vis_out.vis = av_vis_data
                                vis_out.update()
                                fout.write(str(vis_out))
        except PermissionError:
            print("Can't write .fringe file due to permissions")
            return

class Vis:
    """Initializes a vis storing all correlated data and describing products"""
    def __init__(self, seconds, baseline, polpair, nchan, vis, freq_index, exp_info):
        """
        Inputs
        ------
        - seconds: seconds from the ref epoch
        - baseline: the baseline number
        - freqindex: the frequency index...
        - polpair: eg rr,ll,lr,rl
        - nchan: (lag) channels
        - vis: list of complex data
        """
        self.seconds = int(seconds)
        self.baseline = int(baseline)
        self.polpair = polpair
        self.pol = ['RR', 'LL', 'LR', 'RL'].index(self.polpair)
        self.nchan = nchan
        self.vis = vis
        self.ant1_index = int(self.baseline / 256) - 1
        self.ant2_index = int(self.baseline % 256) - 1
        self.ant1_name  = exp_info.telescopes[self.ant1_index].name
        self.ant2_name  = exp_info.telescopes[self.ant2_index].name
        self.baseline_name = self.ant1_name + '-' + self.ant2_name
        self.freq_index = freq_index
        self.freq_low   = exp_info.freqs[freq_index].freq
        self.bandwidth  = exp_info.freqs[freq_index].bandwidth
        self.freq_high  = self.freq_low + self.bandwidth
        self.lags = self.get_lags()
        self.snr = self.get_snr()
        self.offset = self.get_offset()
        self.fringe_amp = max(self.lags)

    def __str__(self):
        return f"""BL: {self.baseline}, {self.ant1_name}-{self.ant2_name}, pol: {self.polpair}, freq {self.freq_low}-{self.freq_high} second:{self.seconds}
        Fringe at offset {self.offset: .6f} us with SNR {self.snr[0]:3.0f} corrected SNR {self.snr[1]:3.0f}
"""

    def is_auto_corr(self):
        """return true if vis is an auto (ant1_name == ant2_name)"""
        return self.ant1_name == self.ant2_name

    def amps(self):
        """computes and returns the amplitudes in the vis. May need to call this for averaging hence a method"""
        return numpy.abs(self.vis)

    def phases(self):
        """computes and returns the phases in the vis. May need to call this for averaging hence a method"""
        return numpy.angle(self.vis)

    def update(self):
        """recalculates lags, snr, offset, and fringe_amp. This is needed if a new averaged vis was added. The order is important as snr, offset, fringe_amp all depend on self.lags"""
        self.lags = self.get_lags()
        self.snr = self.get_snr()
        self.offset = self.get_offset()
        self.fringe_amp = max(self.lags)

    def get_lags(self):
        """computes the lag space"""
        lag = numpy.fft.ifft(self.vis, self.nchan)
        lagamp=[]
        for j in range(self.nchan):
            lagamp.append(0)
        for j in range(int(self.nchan/2)):
            lagamp[int(j+self.nchan/2)] = abs(lag[j])
        for j in range(int(self.nchan/2)):
            lagamp[j] = abs(lag[int(j+self.nchan/2)])

        return lagamp

    def get_snr(self):
        """Computes snr and corrected snr"""
        snrcalc = self.lags.copy()
        maxlag = max(snrcalc)
        maxindex = snrcalc.index(maxlag)

        #blank out surrounding freq points for snr calc
        #Maybe worth thinking about a nicer way
        snrcalc[maxindex] = 0

        if maxindex > 0:
            snrcalc[maxindex-1] = 0
        if maxindex > 1:
            snrcalc[maxindex-2] = 0
        if maxindex > 2:
            snrcalc[maxindex-3] = 0
        if maxindex < len(snrcalc)-1:
            snrcalc[maxindex+1] = 0
        if maxindex < len(snrcalc)-2:
            snrcalc[maxindex+2] = 0
        if maxindex < len(snrcalc)-3:
            snrcalc[maxindex+3] = 0

        rms = numpy.std(snrcalc)
        snr = maxlag/rms
        corrected_snr = (snr-3)/2
        return (snr, corrected_snr)

    def get_offset(self):
        """returns the offset in us of the highest peak"""
        lagamp = self.lags.copy()
        maxlag = max(lagamp)
        maxindex = lagamp.index(maxlag)
        delay_offset_us = (maxindex - self.nchan/2) * 1.0/(self.bandwidth)
        return delay_offset_us


def read_vis(difxin, exp_info):
    """Read in a chunk of a difx file"""
    header = parseDiFX.parse_output_header(difxin)
    if len(header) == 0:
        return False

    seconds = header[2]
    baseline = header[0]
#    mjd = header[1]
    freqindex = header[5]
    polpair = header[6]
    nchan = int(exp_info.freqs[freqindex].numchan/exp_info.freqs[freqindex].specavg)
    vis = numpy.frombuffer(difxin.read(8*nchan), dtype='complex64')
    vis_obj = Vis(seconds, baseline, polpair, nchan, vis, freqindex, exp_info)
    return vis_obj


def plot_vis(vis, axis):
    """Plots the visibilies:"""
    xs = numpy.arange(vis.freqLow,vis.freqHigh,(vis.freqHigh-vis.freqLow)/vis.nchan)
    if vis.polpair == 'RR':
        axis['Phases'][vis.freqIndex*2].setData(xs, vis.getPhases())
        axis['Amps'][vis.freqIndex*2].setData(xs, vis.getAmps())
        axis['Lags'][vis.freqIndex*2].setData(xs, vis.getLags())
    elif vis.polpair == 'LL':
        axis['Phases'][vis.freqIndex*2+1].setData(xs, vis.getPhases())
        axis['Amps'][vis.freqIndex*2+1].setData(xs, vis.getAmps())
        axis['Lags'][vis.freqIndex*2+1].setData(xs, vis.getLags())
