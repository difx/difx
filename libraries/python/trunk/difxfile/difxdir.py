# coding: latin-1
#===========================================================================
# Copyright (C) 2016  Max-Planck-Institut für Radioastronomie, Bonn, Germany
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

__author__="Helge Rottmann"

import os
import os.path
from collections import deque
from string import upper

def buildDirFilename(dirPath, vsn):
    
    return(dirPath + "/" + vsn + ".dir")
    
class DifxDir(object):
    
    def __init__(self, dirPath, vsn):
        
    
        self.dirPath = dirPath
        self.vsn = vsn
        self.scanCount = 0
        self.stationCode = ""
        self.scans = deque()
        self.experiments = deque()
        self.fileDate = 0.0
        self.parseErrors = 0
        
        if (not os.path.isdir(self.dirPath)):
            raise IOError("DiFX directory path: %s does not exist. " % self.dirPath)
        
        self.filename = buildDirFilename(dirPath, vsn)
        
        if (not os.path.isfile(self.filename)):
            return
        
        self.fileDate = os.path.getmtime(self.filename)
        
        self._parse()
      
    def _parse(self):
        '''
        Parses the module directory file
        '''
        
        scanCount = 0
        lineCount = 0
      
        
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
                if (len(fields) >= 12):
                    
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
                    #scan.scanName = fields[11]
                    
                    # try to separate the scan name into experiment stationcode and scanname
                    nameSplit = fields[11].split("_")
                    if (len(nameSplit) == 3):
                        scan.expName = upper(nameSplit[0])
                        scan.stationCode = upper(nameSplit[1])
                        scan.scanName = nameSplit[2]
                    elif (len(nameSplit) == 4):
                        scan.expName = upper(nameSplit[0])
                        scan.stationCode = upper(nameSplit[1])
                        scan.scanName = nameSplit[2] + "_" + nameSplit[3]
                    else:
                        self.parseErrors += 1
                   
                  
                    if (scan.expName != ""):
                        if (scan.expName not in self.experiments):
                            self.experiments.append(scan.expName)
                    
                    if (len(scan.stationCode) == 2) and (len(self.stationCode) == 0):
                        self.stationCode = scan.stationCode
    
                                    
                    self.scans.append(scan)
                    
            lineCount += 1
        
        if (lineCount == 0):
            raise Exception("Empty .dir file: %s" % self.filename)
        
        # check that .dir file contains as many entries as claimed in the header line
        if (scanCount != (lineCount -1)):
            raise Exception("Scan mismatch. File %s contains %s scans, but header claims it should be: %s" % (self.filename, lineCount-1, scanCount))
        
    def getFileDate(self):      
        return(self.fileDate)
    
    def getFilename(self):
        '''
        Return the full path name of the module directory file
        '''
        return(self.filename)
    
    def exists(self):
        '''
        Checks whether the .dir file exists for the given vsn
        '''
        return(os.path.isfile(self.filename))
    
    def getScanCount(self):
        '''
        Returns the total number of scans found in the module directory
        '''
        
        return(len(self.scans))
    
    def getStationCode(self):
        '''
        Returns the station code that was obtained
        by parsing the scan names found on this
        module directory.
        '''
        return(self.stationCode)
        
    def getExperiments(self):
        '''
        Returns the list of experiment codes obtained
        by parsing the scan names found on this
        module directory.
        '''
        return(self.experiments)
    
    def getParseErrorCount(self):
        '''
        Returns the number of lines in the module directory that
        could not be parsed to obtain experiment, stationcode, scanname
        '''
        return(self.parseErrors)
    
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
            self.expName = ""
            self.stationCode = ""
            self.scanName = ""
            
    
    
    

