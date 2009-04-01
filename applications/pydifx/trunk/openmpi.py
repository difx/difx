#!/usr/bin/python
"""
Includes various functions for spawning mpifxcorr.

TODO: Add more checks on input files?
"""
import re
import sys
import os.path
import getopt
from time import time
from shutil import rmtree
import difxlog as log
from astro import df2hhms


import observation

from spawn import spawn, EOF, spawnClass
from pfile import get_parameter

defreg = ["\r\n", EOF]
retimestep = re.compile("""FXMANAGER telling visbuffer\[(\d+)\] to write out - this refers to time (\d+\.\d+|\d+) - the previous buffer has time (\d+\.\d+|\d+), and the next one has (\d+\.\d+|\d+)""")

def check_threads(machine_file, input_file):
    log.info('checking how processes are assigned')
    #find out the number of datastreams and add this to nprocesses
    active_datastreams = int(get_parameter('ACTIVE DATASTREAMS', input_file))

    #open machine file and thread file
    mf = open(machine_file, 'r')
    thread_file = get_parameter('CORE CONF FILENAME', input_file)
    tf = open(thread_file, 'r')

    # parse machine file
    mftable = []
    for line in mf:
        if '#' in line:
            line = line.split('#')[0]
            if line == '':
                continue
        line = line.split()
        mfline = {}
        mfline['host'] = line[0]
        for entry in line[1:]:
            entry = entry.split('=')
            mfline[entry[0]] = int(entry[1])
        mftable.append(mfline)

    #remove unused nodes and add up total number of processes

    np = 0
    nodes = []
    for host in mftable:
        node = {}
        node['host'] = host['host']
        node['processes'] = []
        if host.has_key('max_slots'):
            node['np'] = host['max_slots']
            node['ncores'] = node['np']
            np += node['np']
        elif host.has_key('slots'):
            node['np'] = host['slots']
            node['ncores'] = node['np']
            np += node['np']
        else:
            node['np'] = 1
            node['ncores'] = node['np']
            np += node['np']
        if node['np'] > 0:
            nodes.append(node)
        print node['host'], str(node['np']), str(np)

    print nodes
    # note which machine will be the manager
    # note which machines will be the datastreams
    if not np > active_datastreams:
        raise RuntimeError, "Not enough available slots to run FXManager and datastreams"
    manager = None
    datastreams = []
    datastreams_assigned = 0
    while(datastreams_assigned < active_datastreams):
        for node in nodes:
            #will only run once
            if not manager:
                manager = node['host']
                node['ncores'] -= 1
                node['processes'].append('FXManager')
            else:
                if datastreams_assigned < active_datastreams:
                    datastreams_assigned += 1
                    datastreams.append(node['host'])
                    node['ncores'] -= 1
                    node['processes'].append('datastream ' + str(datastreams_assigned))

    log.info('Machine file    ' + machine_file)
    log.info('Thread file     ' + thread_file)
    log.info('np              ' + str(np))
    
    log.info('Hosts:')
    for node in nodes:
        log.info("    " + node['host'] + ':')
        for process in node['processes']:
            log.info("        " + process)
        if node['ncores'] > 0:
            log.info("         + " + str(node['ncores']) + " core(s)")
        log.info('')
    return np

def check_outfile(input_file):
    output_file = get_parameter('OUTPUT FILENAME', input_file)
    if os.path.exists(output_file):
        log.error('Output file ' + output_file + ' exists.')
        raise RuntimeError, "Output file exists."

class mpifxcorrSpawnClass(spawnClass):
    def __init__(self, classobj, child):
        self.child = child
        self.start_time, self.exec_time, self.int_time = classobj
        self.start_time = float(self.start_time)
        self.exec_time = float(self.exec_time)
        self.last_time = self.start_time
        self.last_timestep = -1
    def run(self, i):
        if i == 0:
            log.debug(self.child.before)
            a = retimestep.match(self.child.before)
            if a:
                this_time = time()
                this_timestep = float(a.group(2))
                if self.last_timestep == -1:
                    realtime = 0
                    time_elapsed = 0
                    current_realtime = 0
                    time_remaining = 0
                else:
                    realtime = (this_timestep / (this_time - self.start_time))
                    if realtime == 0:
                        realtime = 1
                    time_elapsed = this_time - self.start_time
                    current_realtime = self.int_time / (this_time - self.last_time)
                    time_remaining = (self.exec_time - this_timestep) / realtime
		time_elapsed_str = "%3d:%02d:%02.0f" % df2hhms((time_elapsed + 0.5)/ 86400. )
		time_remaining_str = "%3d:%02d:%02.0f" % df2hhms((time_remaining + 0.5) / 86400. )
		fullstring = "Completed %9.2f/%9.2fs(%2.0f%%)|Elapsed %s|Remaining %s|%3.0f%%" %\
			     (this_timestep,
			      self.exec_time,
			      100.0 * this_timestep / self.exec_time,
			      time_elapsed_str,
			      time_remaining_str,
			      100.0 * current_realtime + 0.5)
		log.info(fullstring)
		self.last_time = this_time
		self.last_timestep = this_timestep
            return 0
        # Could add some other regexps for detecting errors etc.
        # returning 2 or something like that
        if i == 1:
            return 1

def run_mpifxcorr(rootname, np, mpi = None,  machine_file = None, mpifxcorr = None, input_file = None, timeout = None):
    # machine_file = os.path.abspath(rootname + '.machine')
    # input_file = os.path.abspath(rootname + '.input')

    if mpi == None:
        mpi = observation.mpi
    if mpifxcorr == None:
        mpifxcorr = observation.mpifxcorr
    if timeout == None:
        timeout = observation.mpifxcorr_timeout

    check_outfile(input_file)

    import difxlog as log
    #        ' -x LD_LIBRARY_PATH' +\
    command = mpi + ' -np ' + str(np) +\
              ' -machinefile ' + machine_file + ' ' +\
              ' -byslot ' +\
              mpifxcorr + ' ' + input_file
    exec_time = get_parameter('EXECUTE TIME (SEC)', input_file)
    int_time = float(get_parameter('INT TIME (SEC)', input_file))
    log.info('Int time           = ' + str(int_time))
    log.info('num channels       = ' + get_parameter('NUM CHANNELS', input_file))
    log.info('Vis Buffer Length  = ' + get_parameter('VIS BUFFER LENGTH', input_file))
    log.info('Blocks Per Send    = ' + get_parameter('BLOCKS PER SEND', input_file))
    log.info('Data Buffer Factor = ' + get_parameter('DATA BUFFER FACTOR', input_file))
    log.info('num Data Segments  = ' + get_parameter('NUM DATA SEGMENTS', input_file))
    spawn(command, defreg, mpifxcorrSpawnClass, (time(), exec_time, int_time), timeout = timeout)
    log.info('Correlator Finished')

def main():
    """ 
    Run the mpifxcorr correlator

    Usage:
        mpifxcorr.py rootname
    Options:
    -t --timeout     timeout in seconds
       --mpi         path/to/mpirun
       --mpifxcorr   path/to/mpifxcorr
       --machinefile path/to/machinefile
       --inputfile   path/to/inputfile
       --np          number of processes
    -d --delete      delete the previous .difx file as defined in the .input file 
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "t:d", ["timeout=", "mpi=", "mpifxcorr=", "np=", "delete", "machinefile=", "inputfile="])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if not len(args) == 1:
        print "Error: Wrong number of Arguments"
        print main.__doc__
        sys.exit(2)


    # read arguments
    rootname = args[0]

    # set defaults
    timeout = None
    machine_file = rootname + '.machine'
    input_file = rootname + '.input'
    np = check_threads(machine_file, input_file)
    mpipath = observation.mpi
    mpifxcorr = observation.mpifxcorr

    for o, a in opts:
        if o in ("-t", "--timeout"):
            timeout = int(a)
        elif o == "--mpi":
            mpipath = a
        elif o == "--mpifxcorr":
            mpifxcorr = a
        elif o == "--machinefile":
            machine_file = a
        elif o == "--inputfile":
            input_file = a
        elif o == "--np":
            np = int(a)
        elif o in ("-d", "--delete"):
            output_file = get_parameter('OUTPUT FILENAME', input_file)
            if os.path.exists(output_file):
                log.warning('removing ' + output_file)
                rmtree(output_file)

    run_mpifxcorr(rootname, np, mpipath, machine_file, mpifxcorr, input_file, timeout)

if __name__ == "__main__":
    main()
