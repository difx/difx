#!/usr/bin/python
"""
Function to ssh into all machines in machine file and kill all mpifxcorr jobs.
"""
import getopt, sys

import observation
import execall

def run_killall(machine_file, executable = None, options = None, rsh = None):
    if executable == None:
        executable = observation.mpifxcorr
    if options == None:
        options = observation.killdifx_options
    command = ' '.join(('killall', options, executable))
    execall.execall(command, machine_file, rsh)

def main():
    """
Run killall on all machines in a machine file

Usage:
    killdifx.py

killdifx.py options:
-x, --executable    by default mpifxcorr
    --rsh           default taken from observation.py

killall options supported (see killall man page)
-Z  --context pattern 
-e  --exact
-g  --process-group
-i  --interactive
-q  --quiet
-r  --regexp
-s  --signal signal
-u  --user user
-v  --verbose
-w  --wait
-I  --ignore-case
-V  --version
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(0)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "x:Z:egiqrs:u:vwIVl",
                     ["executable=", "rsh=",
                      "context=", "exact", "process-group", "interactive",
                      "quiet", "regexp", "signal", "user=", "verbose", "wait",
                      "ignore-case", "version"])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if len(args) < 1:
        print "Error: Not enough arguments"
        print main.__doc__
        sys.exit(2)
    if len(args) > 1:
        print "Error: Too many arguments"
        print main.__doc__
        sys.exit(2)

    #read arguments
    machine_file = args[0]

    #set defaults
    executable = None
    rsh = None
    for o, a in opts:
        if o in ("-x", "--executable"):
            executable = a
            opts.remove((o, a))
        elif o == "--rsh":
            rsh = a
            opts.remove((o, a))

    if len(opts) > 1:
        options = ''
        for a in opts:
            options += ' '.join(a)
    else:
        options = None

    run_killall(machine_file, executable, options, rsh)

if __name__ == "__main__":
    main()
