#!/usr/bin/python
"""
run the calcif program
some of these functions assume that:
    - the calc server is running on the current machine
    - that it is not supposed to run all the time but
      is started and stopped when needed.

It would be relatively simple to modify for the cases where 
 - the calc server is running all the time
 - the calc server is running on another host

TODO make passing default command line options a bit cleverer
"""
import sys
import getopt

import observation
from spawn import spawn, EOF

checkreg = "CALCServer is running normally."
defreg = ["\r\n", EOF]

def checkcalcfunc(i, child, funcobj):
    """
    Wait for last line of checkCalcserver
    """
    if i == 0:
        if child.before == checkreg:
            return True
        return 0
    if i == 1:
        return -1

def start_calc():
    """
    Run startCalcserver and check it's running
    """
    return spawn(observation.startCalc, defreg, checkcalcfunc)

def kill_calc():
    """
    Kill calcserver on current host
    """
    spawn('killall -w CalcServer')

def run_calcif(calcfile, options, timeout):
    """
    run calcif
    """
    import difxlog as log
    spawn(' '.join(('calcif', options, calcfile)), timeout = timeout)

def run_calc(calcfile, options = None, timeout = None):
    """
    start calcserver, check it's running and run calcif
    """
    if options == None:
        options = observation.calcif_options
    if timeout == None:
        timeout = observation.calcif_timeout

    while 1:
        if start_calc() == True:
            break
        print 'killing calc'
        kill_calc()
        print 'starting calc'
        start_calc()
    print 'running calcif'
    run_calcif(calcfile, options, timeout)

def main():
    """ 
    Start calcserver, run calcif, log its output and shut down calcserver.

    Usage:
        calcif.py root

    calcif options:
    -v --verbose                    Be more verbose in operation

    -s --server  <servername>       Use <servername> as calcserver
    """

    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    try:
        opts, args = getopt.gnu_getopt(sys.argv[1:], "s:v", ["server=", "verbose"])
    except getopt.GetoptError, err:
        print err
        print main.__doc__
        sys.exit(2)
    if not 1 <= len(args) == 1:
        print "Error: Wrong number of Arguments"
        print main.__doc__
        sys.exit(2)


    if len(opts) > 0:
        options = ''
        for o, a in opts:
            options += ' ' + ' '.join((o, a))
    else:
        options = None

    calcfile = args[0] + '.calc'

    run_calc(calcfile, options)

if __name__ == "__main__":
    main()
