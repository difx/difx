"""Contains the plotting classes used for autocorrelations by difxPlot.py"""
import os
import itertools
from time import sleep
from copy import deepcopy
from pyqtgraph.Qt import QtGui
import numpy
import pyqtgraph as pg
from difxplot.vis import read_vis, AverageVis
from difxplot.util import InputDetail, check_for_difx_file

class RefantNotFound(Exception):
    """refant not found"""

class Plot:
    """Base plotting class"""
    def __init__(self, exp_info):
        self.exp_info = exp_info
        self.bl_plots = {}
        self.nrows = 0
        self.ncols = 0
        # need to use openGL for pen width > 1
        pg.setConfigOptions(foreground="k", background="w", useOpenGL=True)
        # Enable antialiasing for prettier plots - doesn't work for openGL
        pg.setConfigOptions(antialias=True)

        self.app = QtGui.QApplication([])
        self.view = pg.GraphicsView()
        self.win = pg.GraphicsLayout()
        self.app.setApplicationDisplayName(f"Autocorrelations for {self.exp_info.source_name}")
        self.view.setCentralItem(self.win)
        self.view.resize(1900, 1000)
        self.win.layout.setSpacing(0.0)

        # make list of all baselines (telescope pairs) we still want this even though only plotting autos so that the averages doesn't care
        # though we only need to make the scope-scope 'baselines'
#        self.baselines = list(itertools.combinations(exp_info.telescopes, 2))
        self.baselines=[]
        for scope in exp_info.telescopes:
            self.baselines.append((scope,deepcopy(scope)))
        exp_info.baselines = self.baselines

    def calc_ncols(self, ncols=False):
        """returns the number of columns desired for graph setup"""
        if ncols:
            self.ncols = ncols
        elif len(self.baselines) in [6, 9]:
            self.ncols = 3
        elif len(self.baselines) > 30:
            self.ncols = 5
        else:
            self.ncols = 4

        if len(self.baselines) < self.ncols:
            self.ncols = len(self.baselines)

    def setup_plots(self):
        """sets up the plots"""
        red_pen = pg.mkPen(color=(255, 0, 0), width=2)
        blue_pen = pg.mkPen(color=(0, 0, 255), width=2)
        scope = 0
            
        for row in range(self.nrows):
            for col in range(self.ncols):
                # dictionary containing one plot per pol, per frequency (subband) 
                plots = {"Amps": []}
                if scope < len(self.baselines):
                    title_string = (
                        self.baselines[scope][0].name
                        + "-"
                        + self.baselines[scope][1].name
                    )
                else:
                    title_string = ""

                amps = self.win.addPlot(row=row, col=col)
                for _ in range(self.exp_info.numfreqs):
                    a1 = amps.plot(pen=red_pen)
                    plots["Amps"].append(a1)

                    a2 = amps.plot(pen=blue_pen)
                    plots["Amps"].append(a2)

                self.bl_plots[title_string] = plots
                amps.setTitle(
                    title_string
                )  
                scope += 1

    def plot(self, vis):
        """plots the vis given to the correct plot..."""
        xs = numpy.arange(
            vis.freq_low, vis.freq_high, (vis.freq_high - vis.freq_low) / vis.nchan
        )
        axis = self.bl_plots[vis.baseline_name]
        if vis.polpair == "RR":
            axis["Amps"][vis.freq_index * 2].setData(xs, vis.amps())
        elif vis.polpair == "LL":
            axis["Amps"][vis.freq_index * 2 + 1].setData(xs, vis.amps())


class PlotAll(Plot):
    """subclass of Plot, handle plotting all baseline pairs, without splitting frequencies"""

    def __init__(self, exp_info, ncols=False):
        super().__init__(exp_info)
        self.calc_ncols(ncols)
        self.nrows = int(numpy.ceil(exp_info.numtelescopes / self.ncols))
        self.setup_plots()


class PlotRef(Plot):
    """subclass of Plot, handle plotting to a reference station, without splitting frequencies"""

    def __init__(self, exp_info, refant, ncols=False):
        super().__init__(exp_info)
        self.refant = refant.upper()
        if self.refant not in [t.name for t in exp_info.telescopes]:
            raise RefantNotFound
        self.exp_info.numbaselines = exp_info.numtelescopes - 1
        self.baselines = [
            x
            for x in self.baselines
            if (x[0].name == self.refant) or (x[1].name == self.refant)
        ]
        self.exp_info.baselines = self.baselines
        self.ncols = 1
        self.nrows = 1
        self.setup_plots()

class PlotAllSplit(Plot):
    """subclass of Plot, handle plotting all baseline pairs, but split frequencies"""

    def __init__(self, exp_info, ncols=False):
        super().__init__(exp_info)
        self.exp_info.numbaselines*=2
        new_bl = []
        for bl in self.baselines:
            new_bl.append(bl)
            bl_edit = deepcopy(bl)
            bl_edit[1].name=bl[1].name+'-upper'
            new_bl.append(bl_edit)
        self.baselines = new_bl
        self.exp_info.baselines = self.baselines
        self.calc_ncols(ncols)
        self.nrows = int(numpy.ceil(exp_info.numtelescopes*2 / self.ncols))
        self.setup_plots()

    def plot(self, vis):
        """plots the vis given to the correct plot..."""
        xs = numpy.arange(
            vis.freq_low, vis.freq_high, (vis.freq_high - vis.freq_low) / vis.nchan
        )
        split_freq = self.exp_info.freq_range[0] + 0.5*(self.exp_info.freq_range[1]-self.exp_info.freq_range[0])
        if self.exp_info.freqs[vis.freq_index].freq > split_freq:
            axis = self.bl_plots[vis.baseline_name+'-upper']
        else:
            axis = self.bl_plots[vis.baseline_name]
        if vis.polpair == "RR":
            axis["Amps"][vis.freq_index * 2].setData(xs, vis.amps())
        elif vis.polpair == "LL":
            axis["Amps"][vis.freq_index * 2 + 1].setData(xs, vis.amps())

class PlotRefSplit(Plot):
    """subclass of Plot, handle plotting baseline pairs, to a refant but split frequencies"""

    def __init__(self, exp_info, refant, ncols=False):
        super().__init__(exp_info)
        self.refant = refant.upper()
        if self.refant not in [t.name for t in exp_info.telescopes]:
            raise RefantNotFound
        self.exp_info.numbaselines = exp_info.numtelescopes - 1
        self.baselines = [
            x
            for x in self.baselines
            if (x[0].name == self.refant) or (x[1].name == self.refant)
        ]
        self.exp_info.baselines = self.baselines
        self.exp_info.numbaselines*=2
        new_bl = []
        for bl in self.baselines:
            new_bl.append(bl)
            bl_edit = deepcopy(bl)
            bl_edit[1].name=bl[1].name+'-upper'
            new_bl.append(bl_edit)
        self.baselines = new_bl
        self.exp_info.baselines = self.baselines
        self.ncols=1
        self.nrows = 2
        self.setup_plots()

    def plot(self, vis):
        """plots the vis given to the correct plot..."""
        xs = numpy.arange(
            vis.freq_low, vis.freq_high, (vis.freq_high - vis.freq_low) / vis.nchan
        )
        split_freq = self.exp_info.freq_range[0] + 0.5*(self.exp_info.freq_range[1]-self.exp_info.freq_range[0])
        if self.exp_info.freqs[vis.freq_index].freq > split_freq:
            axis = self.bl_plots[vis.baseline_name+'-upper']
        else:
            axis = self.bl_plots[vis.baseline_name]
        if vis.polpair == "RR":
            axis["Amps"][vis.freq_index * 2].setData(xs, vis.amps())
        elif vis.polpair == "LL":
            axis["Amps"][vis.freq_index * 2 + 1].setData(xs, vis.amps())

class PlotAuto:
    """Plots autocorrs from a difx file."""
    def __init__(self, input_base, wait_for_file=False, refant=False, aver=1, live=False, ncols=4, write_fringes = False):
        self.fin = None
        self.input_base = input_base
        self.refant = refant
        self.aver = aver
        self.averager = None
        self.live = live
        self.ncols = ncols
        self.write_fringes = write_fringes
        self.exp_info = InputDetail(input_base)
        self.difx_in = check_for_difx_file(self.input_base)
        if not self.difx_in:
            if wait_for_file:
                while not self.difx_in:
                    sleep(0.2)
                    self.difx_in = check_for_difx_file(self.input_base)
            else:
                print(f"DiFX file {self.input_base}.difx does not exist?")
                return
        self.plot_instance = self.choose_plot()
        self.potential_new_file = "{}_{:0{pad}}".format(input_base.split("_")[0], (int(input_base.split("_")[1])+1), pad=len(input_base.split("_")[1]))
        self.plot()

    @staticmethod
    def max_diff(list_in):
        """returns the maximum difference between elements in a list"""
        max_diff = 0
        last_val = list_in[0]
        for item in list_in:
            if abs(item - last_val) > max_diff:
                max_diff = abs(item - last_val)
            last_val = item
        return max_diff
            
    def choose_plot(self):
        """Chooses the plot type depending on if a refant is set and if S/X mode or similar is used"""
        split_plot = self.max_diff([f.freq for f in self.exp_info.freqs if f.freq != 0.999]) > 300 #MHz so split if subbands have a split of more than 300 MHz, ignore dummy zoom band freq
        if self.refant:
            if split_plot:
                plot = PlotRefSplit(self.exp_info, self.refant)
            else:
                plot = PlotRef(self.exp_info, self.refant)
        else:
            if split_plot:
                plot = PlotAllSplit(self.exp_info)
            else:
                plot = PlotAll(self.exp_info)
        return plot

    def plot(self):
        """Starts plotting"""
        print(f"Opening {self.difx_in}")
        self.averager = AverageVis(self.exp_info, self.aver, self.plot_instance)
        self.plot_instance.view.show()  # move this?
        with open(self.difx_in, "rb") as self.fin:
            plotter_outp = self.run_plotter()
        if self.write_fringes:
            self.averager.write_aver_all()
        if plotter_outp:
            #open a new file (and close old)
            print("Opening next file, {}".format(self.potential_new_file))
            self.plot_instance.app.exit(0)
            self.plot_instance.win.close()
            self.plot_instance.view.close()
            del self.plot_instance
            self.__init__(self.potential_new_file, True, self.refant, self.aver, self.live, self.ncols, self.write_fringes)


    def run_plotter(self):
        """Main loop for file read/plot"""
        first_eof = True
        while True:
            if not self.plot_instance.view.isVisible():
                print("Window closed - exiting")
                return False
            current_vis = read_vis(self.fin, self.exp_info)
            if not current_vis:
                if self.live:
                    if first_eof:
                        print("Waiting for more data")
                        first_eof=False
                        print("EOF")
                    sleep(0.1)
                    if self.check_for_new_file():
                        return True
                    pg.QtGui.QApplication.processEvents()
                    continue
                print("EOF")
                break
            if (not current_vis.is_auto_corr()) or current_vis.polpair in ["RL", "LR"]:
                continue
            self.averager.handle(current_vis)
        QtGui.QApplication.instance().exec_()
        return False

    def check_for_new_file(self):
        """check if difx has opened a new file in series _01 _02 etc"""
        if os.path.exists(self.potential_new_file+".difx"):
            return True
        return False
