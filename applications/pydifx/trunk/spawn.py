#!/usr/bin/python
"""
Opens process and parses any error code. Records all output.

TODO make deffunc stateful - maybe it could be a class?
"""
import sys

import observation
from pexpect import spawn as pspawn
from pexpect import EOF
import observation

#match 1 line

defreg = ["\r\n", EOF]
funcobj = None

def deffunc(i, child, funcobj):
    """
    Default function

    writes each line to log as debug

    must 0 unless terminating 
    """
    import difxlog as log
    if i == 0:
        log.debug(child.before)
        return 0
    if i == 1:
        return 1

def spawn(command, reg = defreg, refunc = deffunc, funcobj = funcobj, timeout = None):
    """
    command is the command to spawn
    reg is the regexp or list of regexp to pass to pexpect
    reffunc is a function which is run after the expect
    it  takes i and child as arguments and should return 
    a nonzero value when the child returns EOF and 0 otherwise
    the nonzero value is returned.
    """
    import difxlog as log
    if timeout == None:
        timeout = observation.spawn_timeout
    log.info('spawning ' + command)
    child = pspawn(command)
    log.debug(command + ' output:')
    while 1:
        i = child.expect(reg, timeout)
        a = refunc(i, child, funcobj)
        if not a == 0:
            break
    return a

def main():
    """
    spawn a command using pexpect

    Usage: 
        spawn.py command [timeout]
    
    timeout is in seconds.
    """
    if len(sys.argv) < 2:
        print main.__doc__
        sys.exit(2)
    if len(sys.argv) == 3:
        timeout = int(sys.argv[2])
    else:
        timeout = None
    if len(sys.argv) > 3:
        print "Error: Wrong number of Arguments"
        print main.__doc__
        sys.exit(2)
    command = sys.argv[1]
    spawn(command, timeout = timeout)

if __name__ == '__main__':
    main()
