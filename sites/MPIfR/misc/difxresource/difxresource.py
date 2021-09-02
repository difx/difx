#!/usr/bin/env python3
# coding: latin-1

#===========================================================================
# Copyright (C) 2021  Max-Planck-Institut für Radioastronomie, Bonn, Germany
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

import parseDiFX
import argparse
import os
from math import ceil

# max. rates for the various data source types
rate={}
rate["MODULE"]=1400.0
rate["MARK6"]=8000.0
rate["FILE"]=16000.0

maxNodes = 68
maxThreads = 39

parser = argparse.ArgumentParser(description='Estimate DiFX processing requirements.')
#parser.add_argument('-s', '--speedup', required=False, type=float, help='desired speedup-factor (scan duration/processing duration)')
parser.add_argument('-r', '--rate', required=True, type=float, help='processing rate of the cluster nodes [Mbps per thread]')

parser.add_argument('inputfile', help='the DiFX input file.')

args = parser.parse_args()
baseName = os.path.splitext(args.inputfile)[0]

difx = parseDiFX.DiFXFile()
difx.open(args.inputfile)

numDS = difx.metainfo.numdatastreams
scanDuration=difx.metainfo.common["exectime"]

freqs = difx.metainfo.freqs

maxRate = 1E9
maxSpeedup = 1E9
totalDataRate = 0
numDsFile = 0
numDsM5 = 0
numDsM6 = 0

for ds in difx.metainfo.datastreams:
    totalBW = 0
    
    if ds.datasource == "FILE":
      numDsFile +=1
    elif ds.datasource == "MODULE":
      numDsM5 +=1
    elif ds.datasource == "MARK6":
      numDsM6 += 1
      
    if rate[ds.datasource] < maxRate:
        maxRate = rate[ds.datasource]
    dataRate = 0
    for f in range(ds.nrecfreq):
        totalBW += freqs[ds.recfreqindex[f]].bandwidth
        dataRate += freqs[ds.recfreqindex[f]].bandwidth * freqs[ds.recfreqindex[f]].decimfac * ds.recfreqpols[f] * ds.quantbits  * 2
    #    print f, freqs[ds.recfreqindex[f]].bandwidth, freqs[ds.recfreqindex[f]].decimfac, ds.recfreqpols[f], ds.quantbits
    speedup =  rate[ds.datasource]/ dataRate
    totalDataRate += dataRate
#    print rate[ds.datasource], dataRate, speedup

    if speedup < maxSpeedup:
        maxSpeedup = speedup

# in case all datastreams are FILE-based reduce the speedup due to IO limitations
if numDS == numDsFile:
  maxSpeedup /= numDS

#print maxSpeedup, totalDataRate, totalDataRate * maxSpeedup / args.rate
numProcs = int(ceil(totalDataRate * maxSpeedup / args.rate))

if numProcs > maxNodes*maxThreads:
  numProcs = maxNodes*maxThreads

#print ("{} {}/{}/{} {} {}".format(numDS, numDsFile, numDsM5, numDsM6, totalDataRate, maxSpeedup))
print(numProcs)
    
