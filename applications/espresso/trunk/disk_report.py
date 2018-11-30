#!/usr/bin/env python2
# =======================================================================
# Copyright (C) 2016 Cormac Reynolds
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
# =======================================================================

# Simple program to show disk usage on the main CUPPA data storage areas
# Cormac Reynolds: 2010 June 2


from __future__ import print_function, division
import os
import subprocess
import sys
import optparse
import multiprocessing
from multiprocessing import Process, Queue
from Queue import Empty
import json
import espressolib


def remote_command(inputq, outputq):
    """Form command to run remotely based on contents of inputq, and place the
    commands in the output queue

    Keep going until the inputq is exhausted"""

    while True:
        try:
            disk_query, machine, data_area = inputq.get(block=False)
            command = str()
            if disk_query == "du":
                command = "ssh MACHINE 'du -c -B 1G DATA_AREA'"
                #command = "du -c -B 1G DATA_AREA"
            elif disk_query == "df":
                command = "ssh MACHINE 'df -P -B 1G DATA_AREA'"
                #command = "df -P -B 1G DATA_AREA"

            command = command.replace("MACHINE", machine)
            command = command.replace("DATA_AREA", data_area)
            if options.verbose:
                sys.stderr.write(command + "\n")

            proc = subprocess.Popen(
                    command, shell=True, stdout=subprocess.PIPE)
            output = proc.communicate()[0]
            #print output
            outputq.put([machine, data_area, output])
        except Empty:
            break
    #return output


def sizesort(x):
    try:
        sortval = int(x.split()[0])
    except:
        sortval = -1

    try:
        # the 'total' line is a special case - always put at bottom.
        if x.split()[1] == "total":
            sortval = -1
    except:
        pass

    return sortval

#def data_summary():
    #print 'here'

usage = """%prog will produce a summary of data areas in $DIFX_MACHINES"""
parser = optparse.OptionParser(usage=usage, version="%prog " + "1.0")
parser.add_option(
        "--verbose", "-v", dest="verbose", action="store_true", default=False,
        help="Echo commands to stderr")

(options, args) = parser.parse_args()

difx_machines = os.environ.get("DIFX_MACHINES")
if not difx_machines:
    difx_machines = os.environ.get("CORR_HOSTS")
    if difx_machines:
        sys.stderr.write(
                "Warning: use of the $CORR_HOSTS variable is deprecated."
                " Please define $DIFX_MACHINES instead\n")
    else:
        raise Exception("$DIFX_MACHINES not set!")

try:
    data_areas = espressolib.get_corrhosts(difx_machines)
except:
    sys.stderr.write("Problem with file: {:s}\n".format(difx_machines))
    raise 

# run two processes per available cpu (the work is all being done remotely)
nproc = multiprocessing.cpu_count()*2
diskreport = dict()
disk_queries = []
disk_queries = ["du", "df"]
for disk_query in disk_queries:
    # do the disk queries in parallel
    inputq = Queue()
    outputq = Queue()
    for machine in data_areas:
        if data_areas[machine][1]:
            #sys.stderr.write(machine + '\n')

            for data_area in data_areas[machine][1]:
                # form a queue of data areas
                inputq.put([disk_query, machine, data_area])

    # nproc is the number of parallel processes to initiate (each process will
    # take jobs from the inputq until it is empty)
    processes = [Process(
            target=remote_command, args=(inputq, outputq)) for i in
            range(nproc)]
    for p in processes:
        p.start()
    for p in processes:
        #sys.stderr.write( str(p.pid) + "\n" )
        p.join()

    while not outputq.empty():
        machine, data_area, output = outputq.get()
        if machine not in diskreport.keys():
            diskreport[machine] = dict()
        if data_area not in diskreport[machine].keys():
            diskreport[machine][data_area] = dict()
        if disk_query == "du":
            #diskreport[machine][data_area]["du"] = (output.split("\n"))
            diskreport[machine][data_area]["du"] = []
            for directory in output.split("\n")[0:-1]:
                du_output = directory.split()
                du_output[0] = int(du_output[0])
                diskreport[machine][data_area]["du"].append(du_output)
        elif disk_query == "df":
            # just keep the disk name, size, used, avail and % columns
            keep_index = [8, 9, 10, 11, 12]
            df_output = [output.split()[i] for i in keep_index]
            for i in [0, 1, 2]:
                df_output[i] = int(df_output[i])
            diskreport[machine][data_area]["df"] = df_output


#JSONOUT = open ("disk.json", 'w')
print (json.dumps(diskreport, indent=4, sort_keys=True))
