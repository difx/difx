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


import observation

from spawn import spawn, EOF
from pfile import get_parameter

defreg = ["\r\n", EOF]
retimestep = re.compile("""FXMANAGER telling visbuffer\[(\d+)\] to write out - this refers to time (\d+) - the previous buffer has time (\d+), and the next one has (\d+)""")

def check_threads(machine_file, input_file):
    # for our datastream there is simply one process per line in the machine file
    # so we could simply use that to work out the number of processes.
    # however this is more thorough.

    log.info('checking how processes are assigned')
    #set nprocesses to 1 to account for master
    n_processes = 1
    #find out the number of datastreams and add this to nprocesses
    active_datastreams = int(get_parameter('ACTIVE DATASTREAMS', input_file))
    n_processes += active_datastreams

    #open machine file and thread file
    mf = open(machine_file, 'r')
    thread_file = get_parameter('CORE CONF FILENAME', input_file)
    tf = open(thread_file, 'r')
    #note which machine will be the manager
    manager = mf.next().strip()
    #note which machines will be the datastreams
    datastreams = []
    for i in range(active_datastreams):
        datastreams.append(mf.next().strip())
    #find out number of cores from first line of the threads file and work out 
    #and work out which machines will be cores and how many threads will run on
    #each
    cores = []
    threads = []
    ncores = int(tf.next().split(':')[1].strip())
    for i in range(ncores):
        try:
            threads.append(int(tf.next()))
        except StopIteration:
            raise RuntimeError, 'faulty .threads file'
        try:
            cores.append(mf.next().strip())
        except StopIteration:
            log.warning('warning, mismatch in number of cores and number of machines in machine file')
    n_processes += sum(threads)
    log.info('machine file    ' + machine_file)
    log.info('thread file     ' + thread_file)
    log.info('n_processes     ' + str(n_processes))
    log.info('manager         ' + manager)
    for i in range(active_datastreams):
        log.info('datastream ' + str(i + 1).rjust(4) + ' ' + datastreams[i])
    for i in range(ncores):
        log.info('core ' + str(i + 1).rjust(4) + '       ' + cores[i].rjust(2) + ' ' + str(threads[i]).rjust(2) + ' thread(s)')
    return {'n_processes' : n_processes, 'manager': manager, 'datastreams':datastreams, 'cores':cores, 'threads':threads}

def check_outfile(input_file):
    output_file = get_parameter('OUTPUT FILENAME', input_file)
    if os.path.exists(output_file):
        log.error('Output file ' + output_file + ' exists.')
        raise RuntimeError, "Output file exists."

def spawn_func(i, child, funcobj):
    if i == 0:
        start_time, exec_time = funcobj
        log.debug(child.before)
        a = retimestep.match(child.before)
        if a:
            timestep = int(a.group(2))
            realtime = int(100. * timestep / (time() - start_time))
            log.info(''.join(('Timestep ',  a.group(2), ' / ', exec_time, '.  ',
                             str(timestep * 100 / int(exec_time)), '% completed in ',
                             str(realtime), '% realtime.')))
        return 0
    # Could add some other regexps for detecting errors etc.
    # returning 2 or something like that
    if i == 1:
        return 1

def run_mpifxcorr(rootname, np, mpi = None,  machine_file = None, mpifxcorr = None, input_file = None, timeout = None):

    machine_file = os.path.abspath(rootname + '.machine')
    input_file = os.path.abspath(rootname + '.input')

    if mpi == None:
        mpi = observation.mpi
    if mpifxcorr == None:
        correlator = observation.mpifxcorr
    if timeout == None:
        timeout = observation.mpifxcorr_timeout

    check_outfile(input_file)

    import difxlog as log
    command = mpi + ' -nolocal -np ' + str(np) + ' -machinefile ' +\
              machine_file + ' ' + mpifxcorr + ' ' + input_file
    exec_time = get_parameter('EXECUTE TIME (SEC)', input_file)
    spawn(command, defreg, spawn_func, (time(), exec_time), timeout = 120)
    log.info('Correlator Finished')

def main():
    """ 
    Run the mpifxcorr correlator

    Usage:
        mpifxcorr.py rootname
    Options:
    -t --timeout   timeout in seconds
       --mpi       path/to/mpirun
       --mpifxcorr path/to/mpifxcorr
       --np        number of processes
    -d --delete    delete the previous .difx file as defined in the .input file 
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "t:d", ["timeout=", "mpi=", "mpifxcorr=", "np=", "delete"])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if not len(args) == 1:
        print "Error: Wrong number of Arguments"
        print main.__doc__
        sys.exit(2)

    # set defaults
    timeout = None

    # read arguments
    rootname = args[0]
    machine_file = rootname + '.machine'
    input_file = rootname + '.input'
    np = check_threads(machine_file, input_file)['n_processes']
    mpipath = observation.mpi
    mpifxcorr = observation.mpifxcorr

    for o, a in opts:
        if o in ("-t", "--timeout"):
            timeout = int(a)
        elif o == "--mpi":
            mpipath = a
        elif o == "--mpifxcorr":
            mpifxcorrpath = a
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
