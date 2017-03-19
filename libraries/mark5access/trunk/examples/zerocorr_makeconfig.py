#!/usr/bin/env python
# coding: latin-1

#===========================================================================
# Copyright (C) 2017  Max-Planck-Institut für Radioastronomie, Bonn, Germany
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================
import argparse

description = "A script for creating a configuration file for the zerocorr program. For details see the help of zerocorr. If options for the second input file are ommited the seetings of the first file are duplicated."

parser = argparse.ArgumentParser(description=description)
parser.add_argument('-i2','--infile2', default="", help='The second data file to process. [default: the first data file]')
parser.add_argument('-f2','--format2', default="", help='The format specifier for the second data file. [default: format of the first data file]')
parser.add_argument('-b2', '--band2', default=0, type=int, help='The number of the band to process of the second data file(starts at 0) [default: the band selected for the first file]')
parser.add_argument('-o', '--offset', default=0, type=int, help='Offset into the first file (bytes). [default: %(default)s]")')
parser.add_argument('-o2', '--offset2', default=0, type=int, help='Offset into the second file (bytes). [default: %(default)s]")')
parser.add_argument('-fs', '--fftsize', default=512, type=int, help='Size of the FFT to perform over the original bandwidth of first data file. [default: %(default)s]")')
parser.add_argument('-fs2','--fftsize2', default=512, type=int, help='Size of the FFT to perform over the original bandwidth of second data file. [default: %(default)s]")')
parser.add_argument('-fc','--firstchan', default=0, type=int, help='First channel to correlate for the first data file [default: %(default)s]")')
parser.add_argument('-fc2','--firstchan2', default=0, type=int, help='First channel to correlate for the second data file. [default: %(default)s]")')
parser.add_argument('-n', '--numchan', default=256, type=int, help='Number of channels to correlate (negative for LSB) of first data file. [default: %(default)s]")')
parser.add_argument('-n2', '--numchan2', default=256, type=int, help='Number of channels to correlate (negative for LSB) of second data file. [default: %(default)s]")')
parser.add_argument('--numFFT', type=int, default=-1, help='number of FFTs to process [default = all]')

parser.add_argument('infile', help='the data file. Use --datafile2 option to supply a second data file.')
parser.add_argument('format', help='The DiFX format descriptor for the data file (e.g. VDIF_5032-2048-16-2)')
parser.add_argument('band', type=int, help='The number of the band to process (starts at 0)')
parser.add_argument('basename', help='the base name of the visibility (.vis) and lag (.lag) output files.')
parser.add_argument('outfile', help='name of the output .conf file')
args = parser.parse_args()

if args.infile2 == "":
	args.infile2 = args.infile
if args.format2 == "":
	args.format2 = args.format
if args.band2 == "":
	args.band2 = args.band
if args.offset2 == "":
	args.offset2 = args.offset
if args.fftsize2 == "":
	args.fftsize2 = args.fftsize
if args.firstchan2 == "":
	args.firstchan2 = args.firstchan
if args.numchan2 == "":
	args.numchan2 = args.numchan

visOut = open (args.outfile, "w")

visOut.write(args.infile + "\n")
visOut.write(args.format + "\n")
visOut.write(str(args.band) + "\n")
visOut.write(str(args.offset) + "\n")
visOut.write(str(args.fftsize) + "\n")
visOut.write(str(args.firstchan) + "\n")
visOut.write(str(args.numchan) + "\n")
visOut.write(args.infile2 + "\n")
visOut.write(args.format2 + "\n")
visOut.write(str(args.band2) + "\n")
visOut.write(str(args.offset2) + "\n")
visOut.write(str(args.fftsize2) + "\n")
visOut.write(str(args.firstchan2) + "\n")
visOut.write(str(args.numchan2) + "\n")
visOut.write(args.basename + ".vis\n")
visOut.write(args.basename + ".lag\n")
visOut.write(str(args.numFFT) + "\n")

visOut.close()
