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
# $Id: difxdir.py 7604 2016-12-22 13:18:43Z HelgeRottmann $
# $HeadURL: https://svn.atnf.csiro.au/difx/libraries/python/trunk/difxfile/difxdir.py $
# $LastChangedRevision: 7604 $
# $Author: HelgeRottmann $
# $LastChangedDate: 2016-12-22 14:18:43 +0100 (Thu, 22 Dec 2016) $
#
#============================================================================

__author__="Helge Rottmann"

import os
import math
import os.path
from collections import deque
from datetime import datetime, timedelta
from difxutil.dateutil import mjdToDate


def buildFilename(dirPath, vsn):
    
    return(dirPath + "/" + vsn + ".filelist")
    
class DifxFilelist(object):

    '''
    Class for handling contents of .filelist files 
    as created by mk6dir, vdifsum etc.

    contents are expected to be of the form:
    e18a24_Mm_114-0942.vdif 58232.40416667 58232.40625000
    '''
    
    def __init__(self, dirPath, vsn):
        
    
        self.dirPath = dirPath
        self.vsn = vsn
        self.scanCount = 0
        self.stationCode = ""
        self.scans = deque()
        self.experiments = deque()
        self.expStart = {}
        self.expStop = {}
        self.fileDate = 0.0
        self.parseErrors = []
        
        if (not os.path.isdir(self.dirPath)):
            raise IOError("DiFX directory path: %s does not exist. " % self.dirPath)
        
        self.filename = buildFilename(dirPath, vsn)
        
        if (not os.path.isfile(self.filename)):
            raise IOError("File %s does not exist. " % self.filename)
        
        self.fileDate = os.path.getmtime(self.filename)
        
        self._parse()
      
    def _parse(self):
        '''
        Parses the filelist
        '''
        
        lineCount = 0
        errorCount = 0
      
        
        file = open(self.filename, "r")
        
        for line in file:

            line = line.strip()
            if line.startswith("#"):
                continue
            fields = line.split()
            
            # e18a24_Mm_114-0942.vdif 58232.40416667 58232.40625000 216411093408
            if (len(fields) < 3):
                errorCount += 1
                self.parseErrors.append("Line %d: illegal format %s" % (lineCount, fields[0]))
            else:
                    scan = self.FileListLine()

                    scan.startMJD = float(fields[1])
                    scan.stopMJD = float(fields[2])
                    if (len(fields) == 4):
                        scan.bytes = float(fields[3])
                                 
                    # try to separate the scan name into experiment stationcode and scanname
                    nameSplit = fields[0].strip('_').split("_")
                    if (len(nameSplit) == 3):
                        scan.expName = nameSplit[0].upper()
                        scan.stationCode = nameSplit[1].upper()
                        scanName = nameSplit[2]      
                        # check for file extensions in scan name
                        scanName = scanName.split(".")[0]
                    else:
                        errorCount += 1
                        self.parseErrors.append("Line %d: illegal format %s" % (lineCount, fields[0]))
                        continue
                        
                    # determine start seconds
                    
                    fracSeconds = scan.startMJD - int(scan.startMJD)                
                    scan.startSec = int(fracSeconds * 86400.0)
                    scan.scanDuration = int(round((scan.stopMJD - scan.startMJD) * 86400))
                    
                    # construct datetime from MJD and seconds within a day
                    y,M,d = mjdToDate(scan.startMJD)
                    m, s = divmod(int(scan.startSec), 60)
                    h, m = divmod(m, 60)
                    date = datetime(int(y),int(M),int(d),h,m,s)
       #             print date
                  
                    if (scan.expName != ""):
                        # new experiment found
                        if (scan.expName not in self.experiments):
                            self.experiments.append(scan.expName)
                            self.expStart[scan.expName] = date
                            self.expStop[scan.expName] = date
                        else:
                            if date < self.expStart[scan.expName]:
                                self.expStart[scan.expName] = date
                            elif date > self.expStop[scan.expName]:
                                self.expStop[scan.expName] = date + timedelta(0,float(scan.scanDuration))
                            
                    if (len(scan.stationCode) == 2) and (len(self.stationCode) == 0):
                        self.stationCode = scan.stationCode
    
                                    
                    self.scans.append(scan)
                    
            lineCount += 1
        
        self.scanCount = lineCount
        if (lineCount == 0):
            raise Exception("Empty .filelist file: %s" % self.filename)
        
        
    def getFileDate(self):      
        return(self.fileDate)
    
    def getFilename(self):
        '''
        Return the full path name of the module directory file
        '''
        return(self.filename)
    
    def exists(self):
        '''
        Checks whether the .filelist file exists for the given vsn
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
    
    def getExperimentStartDatetime(self, expCode):
        ''' Returns the datetime of the first scan of the given experiment
        found in the module .dir listing.
        '''
        if expCode in self.expStart:
                return(self.expStart[expCode])
        else:
                return(None)

    def getExperimentStopDatetime(self, expCode):
        ''' Returns the datetime (including the scan duration) of the last scan of the given experiment
        found in the module .dir listing.
        '''
        if expCode in self.expStop:
                return(self.expStop[expCode])
        else:
                return(None)

    def getParseErrorCount(self):
        '''
        Returns the number of lines in the module directory that
        could not be parsed to obtain experiment, stationcode, scanname
        '''
        return(len(self.parseErrors))
    
    def getParseErrors(self):
        '''
        Returns the list of parse errors
        '''
        return (self.parseErrors)
        
    class FileListLine(object):
        '''
        Inner class for storing contents of .dir file scan entries
        '''
        
        # e18a24_Mm_114-0942.vdif 58232.40416667 58232.40625000
        def __init__(self):
            self.expName = ""
            self.stationCode = ""
            self.scanName = ""       
            self.startMJD = 0
            self.stopMJD = 0
            self.startSec = 0
            self.scanDuration = 0
            self.bytes = 0

if __name__ == "__main__":
    
    import sys
    
    # usage: python difxfilelist.py {path_to_filelist} {VSN}
    
    #flist= DifxFilelist(".", ")
    flist= DifxFilelist(sys.argv[1], sys.argv[2])
    print ("Found scans:")
    for scan in flist.scans:
            print((scan.expName, scan.stationCode, scan.scanName, scan.startMJD, scan.stopMJD, scan.startSec, scan.scanDuration))
            
    print(("Number of scans: %d" % flist.scanCount))
    print(("Station code: %s" % flist.stationCode))
    print(("Experiments: %s" % flist.experiments))
    print(("Filelist date: %s " % datetime.fromtimestamp(flist.fileDate).strftime('%d/%m/%Y')))
    print(("Parse errros: %d" % flist.getParseErrorCount()))
    
    if (flist.getParseErrorCount() > 0):
        for error in flist.getParseErrors():
            print (error)
    print ("Experiment summary:")
    for exp in flist.experiments:
        print((exp, flist.expStart[exp], flist.expStop[exp]))
        
    
        
    
    

