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

import parseDiFX
import argparse
import os
import re
from math import ceil

# max. rates for the various data source types
rate={}
rate["MODULE"]=1400.0
rate["MARK6"]=16000.0
rate["MARK6MODULE"]=4000.0
rate["FILE"]=16000.0
rate["FAKE"]=16000.0 # needed for source=fake test-correlation v2d's

minNodes = 6
maxNodes = 60

description = "A program to estimate the number of openmpi processes required to process a DiFX job at the fastest possible speed.\n"
description += "The estimate is done by relating the maximum possible processing datarate based on the\n"
description += "types of datastreams of the job to the maximum processing rate of a single thread.\n"
description += "Note that these are based on empirical determination and must be adapted to the individual cluster specifications.\n"
description += "Returns the number of compute processes."

parser = argparse.ArgumentParser(description=description)
#parser.add_argument('-s', '--speedup', required=False, type=float, help='desired speedup-factor (scan duration/processing duration)')
parser.add_argument('-r', '--rate', required=True, type=float, help='processing rate of the cluster nodes [Mbps per thread]')
parser.add_argument('-n', '--nodes',  type=int, help='Number of nodes to use. Overrides the estimates normally done by the script.')
parser.add_argument('-t', '--threads',  default=19, type=int, help='Number if threads to use on each node (must match cluster definition file). Default: %(default)s')
parser.add_argument('-v', "--verbose", default=False, action='store_true', help='print additional information (debugging only)')
parser.add_argument('--all', default=False, action='store_true', help='return all processes head/datastream/compute.')

parser.add_argument('inputfile', help='the DiFX input file.')

args = parser.parse_args()
baseName = os.path.splitext(args.inputfile)[0]
maxThreads = args.threads

difx = parseDiFX.DiFXFile()
difx.open(args.inputfile)

numDS = difx.metainfo.numdatastreams
scanDuration=difx.metainfo.common["exectime"]

freqs = difx.metainfo.freqs

#maxRate = 1E9
maxSpeedup = 1E9
totalDataRate = 0
numDsFile = {}
numDsM5 = 0
numDsM6 = 0
source = "" 

# first determine number and nature of datastreams
dsIdx = 0
for ds in difx.metainfo.datastreams:
    
    if args.verbose:
        print ("DS={} type={} telescope={} media={}".format(dsIdx, ds.datasource, ds.telescopeindex, difx.metainfo.data[dsIdx].media))
    if ds.datasource == "FILE":
        if len(difx.metainfo.data[dsIdx].media) == 0:
            # empty entry, happens e.g. when multi-datastream station lacks a recording on one of the streams
            pass
        elif difx.metainfo.data[dsIdx].media[0].startswith("/"):
            # get first part of path (assuming path starts with /)
            source = difx.metainfo.data[dsIdx].media[0].split("/")[1]

            if source in numDsFile.keys():
                numDsFile[source] += 1
            else:
                numDsFile[source] = 1
        else:
            # mark6 native mode
            numDsM6 += 1

    elif ds.datasource == "MODULE":
        numDsM5 +=1
    elif ds.datasource == "MARK6":
        numDsM6 += 1
    dsIdx += 1
      
dsIdx = 0
pFuseMark5 = re.compile('mark5fx.._fuse')
pFuseMark6 = re.compile('mark6-.._fuse')
for ds in difx.metainfo.datastreams:
    totalBW = 0
    
    rateDiv = rate[ds.datasource]
    
    # if several file-based datastreams play from the same source the rate must be reduced accordingly
    if ds.datasource == "FILE":
      if len(difx.metainfo.data[dsIdx].media) == 0:
          # empty entry, happens e.g. when multi-datastream station lacks a recording on one of the streams
          dsIdx += 1
          continue
      elif difx.metainfo.data[dsIdx].media[0].startswith("/"):
          source = difx.metainfo.data[dsIdx].media[0].split("/")[1]
          if pFuseMark5.match(source):
              rateDiv = rate["MODULE"]
          elif pFuseMark6.match(source):
              rateDiv = rate["MARK6"] / numDsFile[source]
          else:
              rateDiv = rate[ds.datasource] / numDsFile[source]
      else:
          rateDiv = rate["MARK6MODULE"]


    dataRate = 0
    for f in range(ds.nrecfreq):
        totalBW += freqs[ds.recfreqindex[f]].bandwidth
        dataRate += freqs[ds.recfreqindex[f]].bandwidth * freqs[ds.recfreqindex[f]].decimfac * ds.recfreqpols[f] * ds.quantbits  * 2
    #    print f, freqs[ds.recfreqindex[f]].bandwidth, freqs[ds.recfreqindex[f]].decimfac, ds.recfreqpols[f], ds.quantbits
    speedup =  rateDiv / dataRate
    totalDataRate += dataRate
    if args.verbose:
        print ("DS={} BW={} dsRate={} playRate={} speedup={}, total_DR={}".format(dsIdx, totalBW, dataRate, rateDiv, speedup, totalDataRate))

    if speedup < maxSpeedup:
        maxSpeedup = speedup
    
    dsIdx += 1

#print maxSpeedup, totalDataRate, totalDataRate * maxSpeedup / args.rate
numProcs = int(ceil(totalDataRate * maxSpeedup / args.rate))

if numProcs < minNodes*maxThreads:
  numProcs = minNodes*maxThreads

if numProcs > maxNodes*maxThreads:
  numProcs = maxNodes*maxThreads

# override the esitmation
if (args.nodes):
  numProcs = args.nodes * maxThreads;

if args.all:
    print("{}/{}/{}".format(1,numDS,numProcs))
else:
    print(numProcs)

