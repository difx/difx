#!/bin/env python3
"""Plots Phase/Amp vs Freq and Amp vs Lag for a .difx + .input file. Requires pyqtgraph and numpy"""
import argparse
import pyqtgraph as pg
from difxplot.plot import PlotDifx

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
    wait = False
    if args.live:
        wait = True
    
    plot_difx = PlotDifx(args.input_base, wait, args.refant, args.aver, args.live, args.ncols, args.write_fringes)
