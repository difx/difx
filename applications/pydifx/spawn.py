#!/usr/bin/python
"""
Opens process and parses any error code. Records all output.
"""
import sys

import observation
from pexpect.pexpect import spawn as pspawn
from pexpect.pexpect import EOF
import observation

#match 1 line

defreg = ["\r\n", EOF]
spawn_class_obj = None

class spawnClass():
    """
    Default function

    writes each line to log as debug

    must 0 unless terminating 
    """
    def __init__(self, classobj, child):
        import difxlog as log
        self.obj = classobj
        self.child = child

    def run(self, i):
        if i == 0:
            log.debug(self.child.before)
            return 0
        if i == 1:
            return 1

def spawn(command, reg = defreg, reclass = spawnClass, classobj = spawn_class_obj, timeout = None):
    """
    command is the command to spawn
    reg is the regexp or list of regexp to pass to pexpect
    reclass is a class
      __init__ should take self, classobj and child
      run should take self and i
    it  takes i and child as arguments and should return 
    a nonzero value when the child returns EOF and 0 otherwise
    the nonzero value is returned.
    """
    import difxlog as log
    if timeout == None:
        timeout = observation.spawn_timeout
    log.info('spawning ' + command)
    log.info('timeout  ' + str(timeout) + 's')
    child = pspawn(command)
    cl = reclass(classobj, child)
    while 1:
        i = child.expect(reg, timeout)
        a = cl.run(i)
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
