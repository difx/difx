#!/bin/env python3
"""Plots Phase/Amp vs Freq and Amp vs Lag for a .difx + .input file. Requires pyqtgraph and numpy"""
import os
import argparse
from time import sleep
from pyqtgraph.Qt import QtGui
import pyqtgraph as pg
from difxplot.vis import read_vis, AverageVis
from difxplot.plot import PlotAll, PlotAllSplit, PlotRef, PlotRefSplit
from difxplot.util import InputDetail, check_for_difx_file


class PlotDifx:
    """Plots a difx file."""
    def __init__(self, input_base, refant=False, aver=1, live=False, ncols=4, write_fringes = False):
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
        self.plot_instance = self.choose_plot()
        self.potential_new_file = "{}_{:0{pad}}".format(input_base.split("_")[0], (int(input_base.split("_")[1])+1), pad=len(input_base.split("_")[1]))
        self.plot()

    def choose_plot(self):
        """Chooses the plot type depending on if a refant is set and if S/X mode or similar is used"""
        if self.refant:
            #if there's more than a 50% difference between min and max F we split the band into two plots:
            if self.exp_info.freq_range[0]/self.exp_info.freq_range[1] < 0.5:
                plot = PlotRefSplit(self.exp_info, self.refant)
            else:
                plot = PlotRef(self.exp_info, self.refant)
        else:
            if self.exp_info.freq_range[0]/self.exp_info.freq_range[1] < 0.5:
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
            self.__init__(self.potential_new_file, self.refant, self.aver, self.live, self.ncols, self.write_fringes)


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



if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="plots a difx output file, based on plotDiFX")
    parser.add_argument("input_base", help="Base filename for both the <base>.input file and <base>.difx dir")
    parser.add_argument("-a", "--aver", help="Average n integrations", type=int, default=1)
    parser.add_argument("-l", "--live", help="Will expect more data to be written to the file. Useful for eVLBI", action="store_true")
    parser.add_argument("-f", "--write_fringes", help="Writes the fringe offsets and amps to file", action="store_true")
#    parser.add_argument("-p", "--pause", help="Pause for <float> seconds before updating plot (useful if reading a file back and it's reading too quick", type=float, default=0.0)
    parser.add_argument("-r", "--refant", help="Plot only baselines to a refant", default=False)
    parser.add_argument("-c", "--ncols", help="number of Columns in the plot", type=int, default=False)

    args = parser.parse_args()
    plot_difx = PlotDifx(args.input_base, args.refant, args.aver, args.live, args.ncols, args.write_fringes)
