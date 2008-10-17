#!/usr/bin/python
"""
Functions for running a command on all machines in a machine file using 
spawn
"""
import getopt, sys
from sets import Set

import observation
from spawn import deffunc, defreg, funcobj, pspawn

def spawnall(command, machines, rsh = None, reg = defreg, refunc = deffunc, funcobj = funcobj, timeout = None):
    """
    command is the command to spawn
    reg is the regexp or list of regexp to pass to pexpect
    reffunc is a function which is run after the expect
    it  takes i and child as arguments and should return 
    a nonzero value when the child returns EOF and 0 otherwise
    the nonzero value is returned.
    """
    if rsh == None:
        rsh = observation.rsh
    if timeout == None:
        timeout = observation.spawn_timeout
    command = "'" + command + "'"
    n = len(machines)
    child = []
    running = []
    output = []
    for i in range(n):
        child.append(pspawn(' '.join((rsh, machines[i], command))))
        running.append(True)
        output.append('')
    while True in running:
        for i in range(n):
            if running[i]:
                j = child[i].expect(reg, timeout)
                if j == 0:
                    output[i] += child[i].before + '\n'
                else:
                    running[i] = False
    return output

def execall(command, machine_path, rsh = None):
    """
    Execute command on all machines in machine_file using rsh
    """
    try:
        machine_file = open(machine_path, 'r')
    except IOError:
        print 'Error opening machine file'
        sys.exit(2)
    machines = Set()
    for line in machine_file:
        machines.add(line.split()[0])
    machine_file.close()
    machines = tuple(machines)
    output = spawnall(command, machines, rsh)
    for i in range(len(machines)):
        print "output on " + machines[i]
        print output[i]

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
