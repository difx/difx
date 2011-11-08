
__author__="Helge Rottmann"
__date__ ="$07.11.2011 12:47:19$"

import os
import os.path
from collections import deque
from string import upper

class DifxDir(object):
    
    def __init__(self, dirPath, vsn):
        
        self.dirPath = dirPath
        self.vsn = vsn
        self.scanCount = 0
        self.scans = deque()
        self.experiments = deque()
        
        if (not os.path.isdir(self.dirPath)):
            raise IOError("DiFX directory path: %s does not exist. " % self.dirPath)
        
        self.filename = dirPath + "/" + vsn + ".dir"
        
        self._parse()
      
    def _parse(self):
        
        scanCount = 0
        lineCount = 0
        
        if (not self.exists()):
            return
        
        file = open(self.filename, "r")
        
        for line in file:
            fields = line.split()
            
            #extract header fields
            if (lineCount == 0):
                vsn = fields[0]
                scanCount = int(fields[1])
                
                if (vsn != self.vsn):
                    raise Exception("VSN mismatch. File %s contains VSN: %s" % (self.filename, vsn))
                    
            else: 
                if (len(fields) == 12):
                    
                    scan = self.DirLine()
                    scan.startByte = fields[0]
                    scan.length = fields[1]
                    scan.startMJD = fields[2]
                    scan.startSec = fields[3]
                    scan.frameNum = fields[4]
                    scan.frameRate = fields[5]
                    scan.scanDuration = fields[6]
                    scan.frameSize = fields[7]
                    scan.frameOffset = fields[8]
                    scan.tracks = fields[9]
                    scan.format = fields[10]
                    scan.scanName = fields[11]
                    
                    nameSplit = scan.scanName.split("_")
                    if (len(nameSplit) > 0):
                        expName = upper(nameSplit[0])
                        
                    if (expName not in self.experiments):
                        self.experiments.append(expName)
                    

                    self.scans.append(scan)
                    
            lineCount += 1
        
        if (lineCount == 0):
            raise Exception("Empty .dir file: %s" % self.filename)
        
        if (scanCount != (lineCount -1)):
            raise Exception("Scan mismatch. File %s contains %s scans, but header claims it should be: %s" % (self.filename, lineCount-1, scanCount))
     
        
    def exists(self):
        '''
        Checks whether the .dir file exists for the given vsn
        '''
        return(os.path.isfile(self.filename))
    
    def getScanCount(self):
        
        return(len(self.scans))
        
    def getExperiments(self):
        
        return(self.experiments)
    
    class DirLine(object):
        '''
        Inner class for storing contents of .dir file scan entries
        '''
        def __init__(self):
            
            self.startByte = 0
            self.length = 0
            self.startMJD = 0
            self.startSec = 0
            self.frameNum = 0
            self.frameRate = 0
            self.scanDuration = 0
            self.frameSize = 0
            self.frameOffset = 0
            self.tracks = 0
            self.format = 0
            self.scanName = ""
            
    
    
    

