"""Contains the plotting classes for difxPlot.py"""
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
        # make list of all baselines (telescope pairs)
        self.baselines = list(itertools.combinations(exp_info.telescopes, 2))
        exp_info.baselines = self.baselines
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
        self.app.setApplicationDisplayName(f"Baseline Fringes for {self.exp_info.source_name}")
        self.view.setCentralItem(self.win)
        self.view.resize(1900, 1000)
        self.win.layout.setSpacing(0.0)

    def calc_ncols(self, ncols=False):
        """returns the number of columns desired for graph setup"""
        if ncols:
            self.ncols = ncols
        elif self.exp_info.numbaselines in [6, 9]:
            self.ncols = 3
        elif self.exp_info.numbaselines > 30:
            self.ncols = 5
        else:
            self.ncols = 4

        if self.exp_info.numbaselines < self.ncols:
            self.ncols = self.exp_info.numbaselines

    def setup_plots(self):
        """sets up the plots"""
        red_pen = pg.mkPen(color=(255, 0, 0), width=2)
        blue_pen = pg.mkPen(color=(0, 0, 255), width=2)
        baseline = 0
        for row in range(self.nrows):
            for col in range(self.ncols):
                # dictionary containing one plot per pol, per frequency (subband) for each of phases amps lags
                plots = {"Phases": [], "Amps": [], "Lags": []}
                if baseline < self.exp_info.numbaselines:
                    title_string = (
                        self.baselines[baseline][0].name
                        + "-"
                        + self.baselines[baseline][1].name
                    )
                else:
                    title_string = ""

                phases = self.win.addPlot(row=row * 5, col=col, rowspan=1)
                amps = self.win.addPlot(row=row * 5 + 1, col=col, rowspan=2)
                lags = self.win.addPlot(row=row * 5 + 3, col=col, rowspan=2)
                phases.setTitle(
                    title_string
                )  # We set a title for the phases which also separates the plots
                for _ in range(self.exp_info.numfreqs):
                    p1 = phases.plot(
                        pen=None,
                        symbolBrush=(255, 0, 0),
                        symbol="o",
                        symbolSize=2,
                        symbolPen=(255, 0, 0),
                    )
                    plots["Phases"].append(p1)

                    a1 = amps.plot(pen=red_pen)
                    plots["Amps"].append(a1)

                    l1 = lags.plot(pen=red_pen)
                    plots["Lags"].append(l1)

                    p2 = phases.plot(
                        pen=None,
                        symbolBrush=(0, 0, 255),
                        symbol="o",
                        symbolSize=2,
                        symbolPen=(0, 0, 255),
                    )
                    plots["Phases"].append(p2)

                    a2 = amps.plot(pen=blue_pen)
                    plots["Amps"].append(a2)

                    l2 = lags.plot(pen=blue_pen)
                    plots["Lags"].append(l2)
                self.bl_plots[title_string] = plots
                # link the axes but we also need to define a width for the labels (seWidth) or y axis will shift
                amps.setXLink(phases)
                lags.setXLink(phases)
                phases.getAxis("left").setWidth(w=40)
                amps.getAxis("left").setWidth(w=40)
                lags.getAxis("left").setWidth(w=40)
                # Don't show labels for the freqaxis except for the lags (bottom most plot)
                phases.getAxis("bottom").setStyle(showValues=False)
                amps.getAxis("bottom").setStyle(showValues=False)
                # don't scale the y limits for phases...
                phases.setYRange(-3.5, 3.5, update=False)
                # Make amp/lag plot twice as big as phases
                self.win.layout.setRowStretchFactor(row * 5, 1)
                self.win.layout.setRowStretchFactor(row * 5 + 1, 2)
                self.win.layout.setRowStretchFactor(row * 5 + 3, 2)
                baseline += 1

    def plot(self, vis):
        """plots the vis given to the correct plot..."""
        xs = numpy.arange(
            vis.freq_low, vis.freq_high, (vis.freq_high - vis.freq_low) / vis.nchan
        )
        axis = self.bl_plots[vis.baseline_name]
        if vis.polpair == "RR":
            axis["Phases"][vis.freq_index * 2].setData(xs, vis.phases())
            axis["Amps"][vis.freq_index * 2].setData(xs, vis.amps())
            axis["Lags"][vis.freq_index * 2].setData(xs, vis.lags)
        elif vis.polpair == "LL":
            axis["Phases"][vis.freq_index * 2 + 1].setData(xs, vis.phases())
            axis["Amps"][vis.freq_index * 2 + 1].setData(xs, vis.amps())
            axis["Lags"][vis.freq_index * 2 + 1].setData(xs, vis.lags)


class PlotAll(Plot):
    """subclass of Plot, handle plotting all baseline pairs, without splitting frequencies"""

    def __init__(self, exp_info, ncols=False):
        super().__init__(exp_info)
        self.calc_ncols(ncols)
        self.nrows = int(numpy.ceil(exp_info.numbaselines / self.ncols))
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
        self.calc_ncols(ncols)
        self.nrows = int(numpy.ceil(exp_info.numbaselines / self.ncols))
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
        self.nrows = int(numpy.ceil(exp_info.numbaselines / self.ncols))
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
            axis["Phases"][vis.freq_index * 2].setData(xs, vis.phases())
            axis["Amps"][vis.freq_index * 2].setData(xs, vis.amps())
            axis["Lags"][vis.freq_index * 2].setData(xs, vis.lags)
        elif vis.polpair == "LL":
            axis["Phases"][vis.freq_index * 2 + 1].setData(xs, vis.phases())
            axis["Amps"][vis.freq_index * 2 + 1].setData(xs, vis.amps())
            axis["Lags"][vis.freq_index * 2 + 1].setData(xs, vis.lags)

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
        self.calc_ncols(ncols)
        self.nrows = int(numpy.ceil(exp_info.numbaselines / self.ncols))
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
            axis["Phases"][vis.freq_index * 2].setData(xs, vis.phases())
            axis["Amps"][vis.freq_index * 2].setData(xs, vis.amps())
            axis["Lags"][vis.freq_index * 2].setData(xs, vis.lags)
        elif vis.polpair == "LL":
            axis["Phases"][vis.freq_index * 2 + 1].setData(xs, vis.phases())
            axis["Amps"][vis.freq_index * 2 + 1].setData(xs, vis.amps())
            axis["Lags"][vis.freq_index * 2 + 1].setData(xs, vis.lags)

class PlotDifx:
    """Plots a difx file."""
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

    def choose_plot(self):
        """Chooses the plot type depending on if a refant is set and if S/X mode or similar is used"""
        if self.refant:
            #if there's more than a 50% difference between min and max F we split the band into two plots:
            if self.exp_info.freq_range[0]/self.exp_info.freq_range[1] > 0.5:
                plot = PlotRefSplit(self.exp_info, self.refant)
            else:
                plot = PlotRef(self.exp_info, self.refant)
        else:
            if self.exp_info.freq_range[0]/self.exp_info.freq_range[1] > 0.5:
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
            if current_vis.is_auto_corr() or current_vis.polpair in ["RL", "LR"]:
                continue
            self.averager.handle(current_vis)
        QtGui.QApplication.instance().exec_()
        return False

    def check_for_new_file(self):
        """check if difx has opened a new file in series _01 _02 etc"""
        if os.path.exists(self.potential_new_file+".difx"):
            return True
        return False
