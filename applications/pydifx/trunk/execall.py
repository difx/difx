#!/usr/bin/python
"""
Functions for running a command on all machines in a machine file using 
os.system

TODO change to using pexpect rather than os.system?
"""
import getopt, sys, os
from sets import Set

import observation

def execall(command, machine_path, rsh = None):
    """
    Execute command on all machines in machine_file using rsh
    """
    if rsh == None:
        rsh = observation.rsh
    try:
        machine_file = open(machine_path, 'r')
    except IOError:
        print 'Error opening machine file'
        sys.exit(2)
    machines = Set()
    for line in machine_file:
        machines.add(line.split()[0])
    machines = list(machines)
    machines.sort()
    machine_file.close()
    command = "'" + command + "'"
    for machine in machines:
        print 'Executing command ' + command + ' on ' + machine
        os.system(' '.join((rsh, machine, command)))

def main():
    """
Run a command on all machines in a machine file

Usage:
    execall.py command, machine.file [--rsh]

Options:
    --rsh           default taken from observation.py

Commands with more than one argument should be surrounded with
"double quotes"
    """
    if len(sys.argv) < 3:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "", ["rsh="])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)

    # set defaults
    rsh = None

    # read arguments
    command = args[0]
    machine_file = args[1]

    for o, a in opts:
        if o == "--rsh":
            rsh = a

    execall(command, machine_file, rsh)

if __name__ == "__main__":
    main()
