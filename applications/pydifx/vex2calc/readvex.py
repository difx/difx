#!/usr/bin/python
"""
Functions for parseing a block of a vex file, 
The block is represented as a class originally inherited from the
dictionary class.

Each statement is converted to an appropriate datatype using
vexvalue.py.

TODO Work out how to support links in vexfiles
"""

from odict import Odict, VexDict, VexList
from vexvalue import reblock, recomment, classify
from astro import datetime2mjd
from datetime import timedelta

class VexBlock(VexDict):
    """
    This class represents a block of a vex file.
    """

    def __init__(self, vexpath, blockname):
        """
        Constructor.
        
        vexpath -- path to vex file
        blockname -- name of block within vexfile
        opens the vex file with pointer self.vexfile
        finds the start of the block

        inherited function must do the rest
        """
        self._vexpath = vexpath
        self._blockname = blockname
        VexDict.__init__(self)
        self.statements = []
        self.extract_block()

    def extract_block(self):
        """
        read vexfile and write statements to self.statements
        endlines should be left in the string
        
        #TODO *Assumes *NO* string literals in relevant block
        #TODO *Assumes *NO* strings containing " or * in relevant block
        """
        vexfile = open(self._vexpath, 'r')

        #find correct block
        for line in vexfile:
            m = reblock.match(line.strip())
            if m:
                if m.group(1) == self._blockname:
                    break
        else:
            raise RuntimeError, "couldn't find block " + self._blockname
        for line in vexfile:
            m = reblock.match(line)
            if m:
                break
            #remove comment from line
            line = recomment.split(line)[0]
            self.statements.append(line.strip())
        self.statements = ''.join(self.statements)
        self.statements = self.statements.split(';')

    def parse_block(self, start = 'def', end = 'enddef'):
        """
        Parse a block or sched block
        
        statements -- list of statements
        start -- definition start
        end -- definition end
        """
        s = len(self.statements)
        i = 0
        for i in range(s):
            if self.statements[i].startswith(start + ' '):
                subblock = self.statements[i].split()[1]
                self.__setitem__(subblock, VexDict({}))
                i += 1
                while not self.statements[i] == end:
                    statement = self.statements[i].split('=', 1)
                    parameter = statement[0].strip()
                    value = classify(statement[1].strip())
                    try:
                        self.__getitem__(subblock).__setitem__(parameter, value)
                    except:
                        print 'Error reading vex file in',
                        print '$' + self._blockname,
                        print subblock, parameter,
                        print ', '.join(str(value))
                    i += 1

class VexSite(VexBlock):
    """
    This class represents the $SITE block of a vex file
    """
    def __init__(self, vexpath):
        """
        Constructor.
        
        vexpath -- path to vex file
        blockname -- name of block within vex file
        """
        VexBlock.__init__(self, vexpath, 'SITE')
        self.parse_block()
    
    def id_dict(self):
        """
        Return a dictionary linking site_ID to site_name
        e.g.
        {'Ma': ['MATERA'], 'Ef': ['EFLSBERG'], 'Nt': ['NOTO'], 'Wz': ['WETTZELL'], 'Mc': ['MEDICINA']}
        """
        # would be good to make this an Odict so that it keeps it's order, but doesn't look like you can
        # construct an odict from tuples like this
        return dict(zip([self[i]['site_ID'][0] for i in self.keys()], [self[i]['site_name'][0] for i in self.keys()]))

    def name_dict(self):
        """
        Return a dictionary linking site_name to site_ID
        e.g.
        """
        return dict(zip([self[i]['site_name'][0] for i in self.keys()], [self[i]['site_ID'][0] for i in self.keys()]))

class VexAntenna(VexBlock):
    """
    This class represents the $ANTENNA block of a vex file
    """
    def __init__(self, vexpath):
        """
        Constructor.
        
        vexpath -- path to vex file
        blockname -- name of block within vex file
        """
        VexBlock.__init__(self, vexpath, 'ANTENNA')
        self.parse_block()

class VexSource(VexBlock):
    """
    This class represents the $SOURCE block of a vex file
    """
    def __init__(self, vexpath):
        """
        Constructor.
        
        vexpath -- path to vex file
        blockname -- name of block within vex file
        """
        VexBlock.__init__(self, vexpath, 'SOURCE')
        self.parse_block()

class VexSched(VexBlock):
    """
    This class represents the $SCHED block of a vex file

    TODO:
    According to the vex standard, these should be in 
    it time order. However this class does *not* assume
    time order. When it is initialised, a list self.order
    is created of all of the scan names in time order.
    """
    def __init__(self, vexpath):
        """
        Constructor.
        
        vexpath -- path to vex file
        blockname -- name of block within vex file
        """
        VexBlock.__init__(self, vexpath, 'SCHED')
        self.parse_block('scan', 'endscan')

    def start(self):
        """
    Return start time of first scan in vex file
        """
        return self.__getitem__(self.keys()[0])['start'][0]

    def startmjd(self):
        """
    Return integer mjd nearest to start time of first scan in vex file
        """
        return datetime2mjd(self.start())

    def end(self):
        """
    Return end time of last scan in vex file
        """
        lstart = self.__getitem__(self.keys()[-1])['start'][0]
        duration = 0
        durations = self.__getitem__(self.keys()[-1])['station']
        for i in range(len(durations)):
            if duration < durations[i][2]:
                duration = durations[i][2]
        return lstart + timedelta(0, duration, 0)

    def endmjd(self):
        """
    Unimplemented
    Return integer mjd nearest to start time of last scan in vex file
        """
        return datetime2mjd(self.end())
