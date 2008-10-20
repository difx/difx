#!/usr/bin/python
"""
Generate machine and threads files from cluster file
TODO change to be compatible also with difx machines file
"""

import sys

from pfile import get_parameter
from observation import cluster_path

def parse_clusterfile(cluster_file):
    """
    Parse clusterfile to return a list of lists
    """
    cf = open(cluster_file, 'r')
    machines = []
    for i, line in enumerate(cf):
        #skip comments
        if line.startswith('#'):
            continue
        line = line.split('#')
        line = line[0]

        #add all other lines as lines in machine
        if not line == '' or line.isspace():
            line = line.split()
            try:
                machines.append([line[0], int(line[1]), int(line[2]), int(line[3])])
            except:
                raise(RuntimeError, 'Error in line ' + str(i + 1) + 'of .cluster file:')
        #next sort by datastream, but zero comes after all other numbers

    return machines

def machine_gen(machines, machine_file, input_file):
    """
    Generate a machine file.

    It is necessary to run parse_clusterfile first
    """
    nstreams = int(get_parameter('ACTIVE DATASTREAMS', input_file))
    try:
        machinefile = open(machine_file, 'w')
    except:
        raise(RuntimeError, "Can't open machine file" + machine_file)
    #Manager:
    print machines 
    machinefile.write(machines[0][0] + '\n')
    #Datastreams
    for i in range(nstreams):
        if machines[i][3] == 0:
            raise(RuntimeError, "not enough datastreams in cluster file")
        else:
            machinefile.write(machines[i][0] + '\n')
    #Cores
    #work out the number of cores
    ncores = 0
    for i in machines:
        ncores += i[1]
        for j in range(i[1]):
            machinefile.write(i[0] + '\n')
    return nstreams, ncores

def thread_gen(input_file, ncores, machines):
    """
    Generate a threads file.

    input_file is the input file
    ncores is the total number of cores
    machines is an array defining the cluster as returned by parse_clusterfile()
    """
    threadfilename = get_parameter('CORE CONF FILENAME', input_file)
    try:
        threadfile = open(threadfilename, 'w')
    except:
        raise(RuntimeError, "Can't open machine file" + machine_file)
    threadfile.write('NUMBER OF CORES:   ' + str(ncores) + '\n')
    for i in machines:
        for j in range(i[1]):
            threadfile.write(str(i[2]) + '\n')

def thread_machine_gen(root, clusterfile):
    machines = parse_clusterfile(clusterfile)
    ncores = machine_gen(machines, root + '.machine', root + '.input')[1]
    thread_gen(root + '.input', ncores, machines)

def main():
    """
    Generate a .machine file and .threads file from a .input file and
    cluster file.

    Usage:
        machinegen.py root [clusterfile]

    Arguments:
    clusterfile is the path to the clusterfile including the extension.    
    root is the root of the input file (not including the .input extension)

    The .machine will then be generated in the same directory with
    the same root as the input file.

    the .threads file will be generated and placed in the location specified 
    in the input file.

    The manager will be put on the same computer as datastream 1.
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    root = sys.argv[1]
    if len(sys.argv) == 3:
        clusterfile = sys.argv[2]
    else:
        clusterfile = cluster_path
    if len(sys.argv) > 3:
        print "Error: Too many arguments"
        print main.__doc__
        sys.exit(2)
    thread_machine_gen(root, clusterfile)

if __name__ == "__main__":
    main()
