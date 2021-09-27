#!/usr/bin/env python3

#**************************************************************************
#   Copyright (C) 2008-2020 by Walter Brisken & Helge Rottmann            *
#                                                                         *
#   This program is free software; you can redistribute it and/or modify  *
#   it under the terms of the GNU General Public License as published by  *
#   the Free Software Foundation; either version 3 of the License, or     *
#   (at your option) any later version.                                   *
#                                                                         *
#   This program is distributed in the hope that it will be useful,       *
#   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
#   GNU General Public License for more details.                          *
#                                                                         *
#   You should have received a copy of the GNU General Public License     *
#   along with this program; if not, write to the                         *
#   Free Software Foundation, Inc.,                                       *
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
#**************************************************************************
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL: $
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

#from string import   
from sys import exit
from os import getenv, umask, environ
from os.path import isfile
import re
import socket
import struct
import subprocess
import signal
import sys
#from optparse import OptionParser
import argparse
from xml.parsers import expat
from copy import deepcopy
from ast import literal_eval
from datetime import datetime

try:
    from difxfile.difxmachines import *
except ImportError:
    print("ERROR: Cannot find difxmachines library. Please include $DIFXROOT/lib/python in your $PYTHONPATH environment")
    sys.exit(1)

author  = 'Walter Brisken and Helge Rottmann'
version = '2.5.0'
verdate = '20200822'
minMachinefileVersion = "1.0"   # cluster definition file must have at least this version

defaultDifxMessagePort = 50200
defaultDifxMessageGroup = '224.2.2.1'
#ignoreIncompleteModules = True

MARK6_VSN_IDS = ['%']

def getmonthdate(daynumber, yearnumber):
    md = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
    mdl = [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366]
    if yearnumber % 4 == 0:
        dl = mdl
    else:
        dl = md
    for i in range(len(dl)):
        if daynumber > dl[i] and daynumber <= dl[i+1]:
            return [i + 1, daynumber - dl[i]]

def convertDateString2Mjd(dateStr):
    mjd0 = datetime(1858, 11, 17, 0, 0)
    if len(dateStr) == 18 and dateStr[4] == 'y' and dateStr[8] == 'd' and dateStr[11] == 'h' and dateStr[14] == 'm' and dateStr[17] == 's':
        startyear = int(dateStr[0:4])
        startday = int(dateStr[5:8])
        mondate = getmonthdate(startday, startyear)
        startmonth = mondate[0]
        startdate = mondate[1]
        starthour = int(dateStr[9:11])
        startminute = int(dateStr[12:14])
        startsecond = int(dateStr[15:17])
        startdt = datetime(startyear, startmonth, startdate, starthour, startminute, startsecond)
        startmjd = startdt - mjd0
        return startmjd
    else:
        print('invalid date string', dateStr)
        return datetime(1900, 1, 1, 0, 0)

class MessageParser:
    """
    Parses Mark5StatusMessage and Mark6StatusMessage
    """

    def __init__(self):
        self._parser = expat.ParserCreate()
        self._parser.StartElementHandler = self.start
        self._parser.EndElementHandler = self.end
        self._parser.CharacterDataHandler = self.data
        self.fields = {}
        self.type = 'unknown'
        self.vsnA = 'none'
        self.vsnB = 'none'
        self.state = 'Unknown'
        self.unit = 'unknown'
        self.sender = 'unknown'
        self.tmp = ''
        self.ok = False

    def feed(self, sender, data):
        self._parser.Parse(data, 0)
        self.sender = sender

    def close(self):
        self._parser.Parse("", 1) # end of data
        del self._parser # get rid of circular references

    def start(self, tag, attrs):
        if tag == 'mark5Status' or tag=='mark6Status':
                self.type = tag
                self.ok = True
    def parseMark6(self,tag):

        self.fields[tag] = self.tmp

    def parseMark5(self,tag):

        if tag == 'bankAVSN' and self.ok:
                if len(self.tmp) != 8:
                        self.vsnA = 'none'
                else:
                        self.vsnA = self.tmp.upper()
        if tag == 'bankBVSN' and self.ok:
                if len(self.tmp) != 8:
                        self.vsnB = 'none'
                else:
                        self.vsnB = self.tmp.upper()

    def end(self, tag):
        if tag == 'from':
                self.unit = self.tmp.lower()
        if tag == 'state' and self.ok:
                self.state = self.tmp
        if self.type == 'mark5Status':
                self.parseMark5(tag)
        if self.type == 'mark6Status':
                self.parseMark6(tag)

    def data(self, data):
        self.tmp = data

    def getinfo(self):
        if self.ok:
                if self.type == 'mark5Status':
                        return [self.unit, self.type, self.vsnA, self.vsnB, self.state, self.sender]
                elif self.type == 'mark6Status':
                        return [self.unit, self.type, self.fields, self.state, self.sender]
        else:
                return ['unknown', 'none', 'none', 'Unknown', 'unknown']

def sendRequest(destination, command):

        src = socket.gethostname()
        dest = '<to>%s</to>' %(destination)

        message = \
          '<?xml version="1.0" encoding="UTF-8"?>\n' \
          '<difxMessage>' \
            '<header>' \
              '<from>%s</from>' \
              '%s' \
              '<mpiProcessId>-1</mpiProcessId>' \
              '<identifier>genmachines</identifier>' \
              '<type>DifxCommand</type>' \
            '</header>' \
            '<body>' \
              '<seqNumber>0</seqNumber>' \
              '<difxCommand>' \
                '<command>%s</command>' \
              '</difxCommand>' \
            '</body>' \
          '</difxMessage>' % (src, dest, command)

        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
        sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 2)
        sock.sendto(message.encode('utf-8'), (group, port))

        return message

def isModuleComplete(slot, message):
        """
        Checks whether a Mark6 module has the expected number of disks
        """
        key = slot + "Disks"    
        if key in list(message.keys()):
                disks = int(message[key])
        key = slot + "MissingDisks"
        if key in list(message.keys()):
                missingDisks = int(message[key])
                
        if disks ==0: 
                return False
        if missingDisks > 0:
                return False
        
        return True
        
def getVsnsByMulticast(maxtime, datastreams, verbose):
        dt = 0.2
        t = 0.0
        vsnlist = []

        count = 0
        for stream in datastreams:
            count += 1
            if len(stream.vsn) > 0:
                vsnlist.append(stream.vsn)
            if len(stream.msn) > 0:
                for m in stream.msn:
                    if isinstance(m,list):
                        vsnlist += m
                    else:
                        vsnlist.append(m)

        missingVsns = deepcopy(vsnlist)

        # First send out a call for VSNs
        sendRequest("mark5","getvsn")
        sendRequest("mark6","getvsn")

        # Now listen for responses, until either time runs out or we get all we need
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind(('', port))
        mreq = struct.pack("4sl", socket.inet_aton(group), socket.INADDR_ANY)
        s.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
        s.settimeout(dt)
        conflicts = []
        results = []
        machines = []
        notidle = []
        incomplete = []
        while t < maxtime and len(missingVsns) > 0:
                try:
                        message, address = s.recvfrom(8000)
                        sender = socket.gethostbyaddr(address[0])[0].split('.')[0]
                        if verbose > 1:
                                print(message)
                        p = MessageParser()
                        p.feed(sender, message)
                        info = p.getinfo()
                        p.close()
                        if info[0] == 'unknown':
                                continue
                        if info[0] in machines:
                                continue
                        machines.append(info[0])
                        results.append(info)

                        if info[1] == "mark5Status":
                                if info[2] in missingVsns and info[3] in missingVsns:
                                        conflicts.append(info)
                                if info[2] in missingVsns:
                                        missingVsns.remove(info[2])
                                        if info[4] != 'Idle' and info[4] != 'Close':
                                                notidle.append(info[2])
                                if info[3] in missingVsns:
                                        missingVsns.remove(info[3])
                                        if info[4] != 'Idle' and info[4] != 'Close':
                                                notidle.append(info[3])
                        elif info[1] == 'mark6Status':
                                for key in ['slot1MSN', 'slot2MSN', 'slot3MSN', 'slot4MSN']:
                                        if key in list(info[2].keys()):
                                                if info[2][key] in missingVsns:
                                                        missingVsns.remove(info[2][key])
                                                        print('found',info[2][key],'on',info[0])
                                                        if not isModuleComplete(key[:5], info[2]):
                                                                incomplete.append(info[2][key])
                                        
                except socket.timeout:
                        t += dt
                except socket.herror:
                        print('Weird: cannot gethostbyaddr for %s' % address[0])

        results.sort()
        conflicts.sort()
        missingVsns.sort()
        notidle.sort()
        incomplete.sort()

        return results, conflicts, missingVsns, notidle, incomplete

def startTimeInPackTimeRange(startTime, packStartStr, packEndStr):
    packStartMjd = convertDateString2Mjd(packStartStr)
    packEndMjd = convertDateString2Mjd(packEndStr)
    if startTime.days > packStartMjd.days or (startTime.days == packStartMjd.days and startTime.seconds >= packStartMjd.seconds):
        if startTime.days < packEndMjd.days or (startTime.days == packEndMjd.days and startTime.seconds <= packEndMjd.seconds):
            return True
    return False

def getvexantennamodlists(vexobsfile, startTime):
    '''
    Returns tuple (experName,antennaModuleList,modList) for Mark6 antennas and VSNs found
    in the given file with path vexobsfile.

    On errors this function exits the script.
    '''
    experNameFound = False
    tapelogObsFound = False
    experName = ''
    ant = ''
    antennaModuleList = []
    modList = []

    # Regexps to flexibly detect VEX entries and fields; can test with https://regex101.com/
    reComment = re.compile("^\s*\*")
    reSection = re.compile("^\s*\$\s*(.*?)\s*;")
    reDef = re.compile("^\s*def\s+(.*?)\s*;")
    reVSN = re.compile("\W*VSN\s*=\s*(.*?)\s*:\s*(.*?)\s*:\s*(.*?)\s*:\s*(.*?)\s*;")
    reEnddef = re.compile("\W*enddef\s*;")

    # Look up expt name and module info from VEX
    vexobs = open(vexobsfile, "r")
    for line in vexobs:
        if reComment.match(line):
            continue
        # detect EXPER info
        i = line.find('exper_name')
        if i != -1:
            experNameFound = True
            lineend = line[i+10:len(line)-1]
            for i in range(len(lineend)):
                if lineend[i] not in [' ', '=', ';']:
                    experName += lineend[i].upper()
            continue
        # detect change of VEX section
        section = reSection.search(line)
        if section and tapelogObsFound:
            # encountered section after TAPELOG_OBS and if expt name also present by now, finished!
            if experNameFound:
               break
        if section and not tapelogObsFound:
            tapelogObsFound = section.group(1) == 'TAPELOG_OBS'
        # detect entries in TAPELOG_OBS section
        if tapelogObsFound:
            # note the possibility of one-liners e.g. 'def Pv; VSN=0:<vsn>:<time>:<time>; enddef'
            newdef = reDef.search(line)
            vsn = reVSN.search(line)
            enddef = reEnddef.search(line)
            if newdef:
                ant = newdef.group(1).upper()
                modList = []
            if vsn:
                vsnNr = int(vsn.group(1))
                mod = vsn.group(2)
                # add only Mark6 modules to modlist
                if any([key in mod for key in MARK6_VSN_IDS]):
                    # add only modules to modlist that contain start time for this job, next module might not be in playback unit yet
                    if startTimeInPackTimeRange(startTime, vsn.group(3), vsn.group(4)):
                        modList.append(mod)
            if enddef:
                antennaModuleList.append([ant,modList])

    # fatal lack of info?
    ok = experNameFound and tapelogObsFound
    if not experNameFound:
        print("could not find experiment name, exiting")
    if not tapelogObsFound:
        print("could not find TAPELOG_OBS data, exiting")
    if not ok:
        exit(1)
    # harmless lack of info
    if len(antennaModuleList) == 0:
        print("no antennas with mark6 modules found, nothing more to do, exiting")
        exit(0)
    return (experName,antennaModuleList,modList)

# replaces filename mark6 data source with msn, if necessary
def adjustMark6datastreams(datastreams, inputfile, verbose, startTime):
        # get .vex.obs filename from .calc file
        calcfilename = inputfile.replace('.input', '.calc')
        try:
            calcfile = open(calcfilename)
            for line in calcfile:
                if "VEX FILE" in line:
                    vexobsfile = (line.split(':')[1]).strip()
                    break

        except IOError:
            print(calcfilename, "could not be openened, returning")
            return

        # get antenna module mapping from .vex.obs
        try:
            (experName,antennaModuleList,modList) = getvexantennamodlists(vexobsfile, startTime)
        except IOError:
            print(vexobsfile, "could not be openened, returning")
            return

        if len(antennaModuleList) < 1:
            print("no antennas using mark6 found, returning")
            return

        # loop through datastreams, modifying mark6 streams if necessary
        for stream in datastreams:
            if stream.type == 'MARK6':
                msnlen = len(stream.msn)
                i = 0
                while i < msnlen:
                    # skip if valid mark6 msn
                    if len(stream.msn[i]) == 8 and '%' in stream.msn[i]:
                        i += 1
                        continue
                    # look for antenna in filename
                    if verbose > 0:
                        print('trying to replace', stream.msn[i], 'with a valid mark6 msn')
                    msnsplit = (stream.msn[i]).split('_')
                    if len(msnsplit) >= 2 and (len(msnsplit[1]) == 2 or len(msnsplit[1]) == 1):
                        ant = msnsplit[1].upper()
                        # if antenna found in antenna module list, replace filename with module
                        antmodfound = False
                        for antmod in antennaModuleList:
                            if ant == antmod[0] and i < msnlen:
                                antmodfound = True
                                if antmod[1] not in stream.msn:
                                    if verbose > 0:
                                        print('replacing', stream.msn[i], 'with', antmod[1])
                                    stream.msn[i] = antmod[1]
                                    i += 1
                                else:
                                    ### TODO: tidy this up, altering list (msn.pop(i)) in loop not so good!
                                    stream.msn.pop(i)
                                    msnlen -= 1
                        if antmodfound is False:
                            i += 1

class StartTime:
        def __init__(self):
                self.days = 0
                self.seconds = 0

def getDatastreamsFromInputFile(inputfile, verbose):
        """
        Parse the datastream section of the input file to
        obtain VSNs, MSNs and file paths
        """
        datastreams = []
        nds = 0
        dsindices = []
        dssources = []
        dscount = 0
        
        input = open(inputfile).readlines()
    
        mark6flag = False
        startTime = StartTime()
        for inputLine in input:
                s = inputLine.split(':')
                if len(s) < 2:
                        continue
                key = s[0].strip()
                keyparts = key.split()
                value = s[1].strip()
               
                # get start mjd
                if key == 'START MJD':
                        startMjd = int(value)
                        startTime.days = startMjd
                
                # get start seconds
                if key == 'START SECONDS':
                        startSeconds = int(value)
                        startTime.seconds = startSeconds

                # find number of datastreams
                if key == 'ACTIVE DATASTREAMS':
                        nds = int(value)
                        # create  empty Datastream objects
                        for i in range (0, nds):
                            stream = Datastream()
                            datastreams.append(stream)
                        
                
                # get datastream indices
                if len(keyparts) == 3 and keyparts[0] == 'DATASTREAM' and keyparts[2] == 'INDEX':
                        dsindices.append(int(value))
                
                # obtain types of all datastreams
                if key == 'DATA SOURCE':
                        if dscount in dsindices:
                                dssources.append(value)
                                datastreams[dscount].type = value
                        dscount += 1
                        
                # parse data table
                if len(keyparts) == 2 and keyparts[0] == 'FILE':
                        
                        # obtain datastream index
                        numDS,index = keyparts[1].split("/")
                        ds = int(numDS.strip())
                        idx = int(index.strip())

                        

                        if ds < nds:
                            if datastreams[ds].type == 'MODULE':
                                datastreams[ds].vsn = value
                            elif datastreams[ds].type == 'FILE':
                                if datastreams[ds].path == "":
                                    datastreams[ds].path = os.path.dirname(value)
                                    if not datastreams[ds].path.endswith("/"):
                                        datastreams[ds].path += "/"


                            elif datastreams[ds].type == 'MARK6':
                                mark6flag = True
                                datastreams[ds].msn.append(value)  
         
        if mark6flag is True:
            adjustMark6datastreams(datastreams, inputfile, verbose, startTime)
            
        return (datastreams)

def writethreads(basename, threads):
        """
        Write the threads file to be used by mpifxcor
        """
        o = open(basename+'threads', 'w')
        o.write('NUMBER OF CORES:    %d\n' % len(threads))
        for t in threads:
                o.write('%d\n' % t)
        o.close()

def writemachines(basename, hostname, results, datastreams, overheadcores, verbose, dorankfile=False):
        """
        Write machines file to be used by mpirun
        """

        dsnodes = []
        threads = []

        dsBookings = {} # keep track of the DS-node assignements (can assign multiple nodes to serve the same url)
        for stream in datastreams:
            if stream.type == "FILE":
                # check if path for this datastream matches storage area defined in the cluster definition file
                        
                matchNodes = []
                currentUrl = ""
                for node in difxmachines.getStorageNodes():
                    for url in node.fileUrls:
                        # add trailing /
                        if not url.endswith("/"):
                                url += "/"
                        #print ("Trying: ", stream.path, url)
                        if stream.path.startswith(url):
                            #print ("match: ", stream.path, url)
                            matchNodes.append(node.name)

                            if url not in dsBookings:
                                dsBookings[url] = {}
                                dsBookings[url][node.name] = 0
                            else:
                                if node.name not in dsBookings[url]:
                                    dsBookings[url][node.name] = 0

                            currentUrl = url
                            break

                if len(matchNodes) > 1:
                    # find eligible ds node with the least number of bookings
                    minBooking = min(dsBookings[currentUrl], key=dsBookings[currentUrl].get)
                    dsBookings[currentUrl][minBooking] += 1

                    dsnodes.append(minBooking)

                elif len(matchNodes) == 1:
                    dsnodes.append(matchNodes[0])
                else:
                    # use compute node             
                    nodecount = 0
                    for node in difxmachines.getComputeNodes():
                        # skip if already used as datastream node
                        if node.name in dsnodes and nodecount < len(difxmachines.getComputeNodes()) - 1: # only keep skipping if you are not at last node
                            nodecount += 1
                            continue

                        dsnodes.append(node.name)
                        print(nodecount, len(difxmachines.getComputeNodes()))
                        break
                
            elif stream.type == "MODULE":    
                matchNode = ""
                for r in results:
                    # find  module either in bank A or B
                    if r[2] == stream.vsn or r[3] == stream.vsn:
                            if r[0] in difxmachines.getMk5NodeNames():
                                matchNode = r[0]
                            else:
                                # use message sending host
                                matchNode = r[5]

                if matchNode in difxmachines.getMk5NodeNames():
                    dsnodes.append(matchNode)
                else:
                    print('stream type MODULE with VSN %s, matching node %s not listed as an active mark5 host in machines file' % (stream.vsn,matchNode))
                    return []

            elif stream.type == "MARK6":
                matchNode = ""  
                for r in results:
                    if type(r[2]) is dict:
                        r2msns = [r[2]['slot1MSN'], r[2]['slot2MSN'], r[2]['slot3MSN'], r[2]['slot4MSN']]
                        r2msns = set(r2msns)
                        missingStreamMsn = False
                        # all stream msns must be in unit msns
                        for msn in stream.msn:
                            contained = len( r2msns.intersection(set(msn)) ) > 0
                            if not contained:
                                missingStreamMsn = True
                                break
                        if missingStreamMsn == False:
                            if r[0] in difxmachines.getMk6NodeNames():
                                matchNode = r[0]
                            else:
                                # use message sending host
                                matchNode = r[4]

                if matchNode in difxmachines.getMk6NodeNames():
                    dsnodes.append(matchNode)
                else:
                    print('stream type MARK6 with MSN %s, matching node %s not listed as an active mark6 host in machines file' % (str(stream.msn),matchNode))
                    return []
                
        # write machine file
        o = open(basename+'machines', 'w')
        
        # head node
        o.write('%s\n' % (hostname))
        
        # datastream nodes
        for node in dsnodes:
            o.write('%s\n' % (node))
            
        # compute nodes
        for node in difxmachines.getComputeNodes():
            usedThreads = 0
            # if compute node is also used as datastream nodes reduce number of threads
            if node.name in dsnodes:
                usedThreads = dsnodes.count(node.name)
          
            # if head node is also used as compute nodes reduce number of threads by one
            if node.name in hostname:
                usedThreads = 1
                
            o.write('%s\n' % (node.name))
            threads.append(node.threads-usedThreads)

        o.close()

        # write optional OpenMPI rank file
        if dorankfile:

            allnodes = [hostname] + dsnodes + [node.name for node in difxmachines.getComputeNodes()]
            corecounts = {}

            o = open(basename+'rankfile', 'w')
        
            for rank in range(len(allnodes)):
                node = allnodes[rank]
                if node not in corecounts:
                    corecounts[node] = 0

                # Syntax of "physical" rankfiles; need MCA param rmaps_rank_file_physical=1 :
                o.write('rank %d=%s slot=%d\n' % (rank, node, corecounts[node]))

                # Syntax of "logical" rankfiles;
                # o.write('rank %d=%s slot=%d:%d\n' % (rank, node, cpusocket, cpucore]))

                corecounts[node] += 1

            o.close()
        
        return threads

def uniqueVsns(datastreams):
        """
        Check for duplicate datastreams VSNs. Returns 1 if duplicates are found, 0 otherwise
        """
        
        d = {}
        vsns = []
        for stream in datastreams:
            if len(stream.vsn) > 0: 
                vsns.append(stream.vsn)
             
        for v in vsns:
                d[v] = 1
        if len(d) != len(vsns):
                return 0
        else:
                return 1

def run(infile, machinesfile, overheadcores, verbose, dothreads, useDifxDb, dorankfile):
        ok = True

        # check if host is an allowed headnode
        hostname = socket.gethostname()
        if not hostname in difxmachines.getHeadNodeNames():
                print('ERROR: hostname (%s) is not an allowed headnode in the machines file : %s' % (hostname, machinesfile))
                exit(1)

        basename = infile[0:-5]
        if basename + 'input' != infile:
                print('expecting input file')
                exit(1)
                                
        datastreams =  getDatastreamsFromInputFile(infile, verbose)

        if verbose > 0:
                print('Datastreams:')
                for stream in datastreams:
                        print('type', stream.type, 'vsn', stream.vsn, 'msn', stream.msn, 'path', stream.path)

        if not uniqueVsns(datastreams):
                print('ERROR: at least one duplicate VSN exists in %s !' % infile)
                exit(1)
                
        results, conflicts, missing, notidle, incomplete = getVsnsByMulticast(5, datastreams, verbose)

        if verbose > 0:
                print('Found modules:')
                for r in results:
                        if 'mark6' in r[0]:
                                print('  %-10s : %10s %10s %10s %10s' % (r[0], r[2]['slot1MSN'], r[2]['slot2MSN'], r[2]['slot3MSN'], r[2]['slot4MSN']))
                        else:
                                print('  %-10s : %10s %10s   %s' % (r[0], r[2], r[3], r[4]))

        if len(conflicts) > 0:
                ok = False
                print('Module conflicts:')
                for c in conflicts:
                        print('  %-10s : %10s %10s' % (c[0], c[1], c[2]))
        
        if len(missing) > 0:
                ok = False
                print('Missing modules:')
                for m in missing:

                        slot = "unknown"
                        if useDifxDb:
                                child = subprocess.Popen(["getslot", m], stdout=subprocess.PIPE)
                                (slot, stderr) = child.communicate()

                        
                        print('  %s (slot = %s )' % (m, slot.strip()))

        if len(notidle) > 0:
                ok = False
                print('Modules not ready:')
                for n in notidle:
                        print('  %s' % n)

        if len(incomplete) > 0:
                if args.ignoreIncompleteModules == False:
                        ok = False
                print('Incomplete modules:')
                for i in incomplete:
                        print('  %s' % i)

        if not ok:
                return 1

        t = writemachines(basename, hostname, results, datastreams, overheadcores, verbose, dorankfile)
        
        if len(t) == 0:
                return 1

        if dothreads:
                writethreads(basename, t)

        return 0

def signalHandler(signal, frame):
        print('You pressed Ctrl+C!')
        sys.exit(8)
        
class Datastream:
        """
        Storage class containing datastream description read from the input file
        NETWORK datastreams not yet supported
        """
        def __init__(self):
                self.type = ""  # allowed types MODULE FILE MARK6
                self.vsn = ""   # module VSN in case of MODULE datasource
                self.msn = []   # module MSN in case of MARK6 datasource
                self.path = ""          # path in case of FILE datasource

class Mark6Datastream(Datastream):
        def __init__(self):
                self.msn = []
                self.group = []
                self.disks = []
                self.missingDisks = []
        
if __name__ == "__main__":

        test = 1
        # catch ctrl+c
        signal.signal(signal.SIGINT, signalHandler)

        epilog = '\n\nNote: %(prog)s respects the following environment variables:'
        epilog +=  '\nDIFX_MACHINES: required, unless -m option is given. -m overrides DIFX_MACHINES.'
        epilog +=  '\nDIFX_GROUP: if not defined a default of %s will be used.' % defaultDifxMessageGroup
        epilog +=  '\nDIFX_PORT: if not defined a default of %s will be used.' % defaultDifxMessagePort
        epilog +=  '\nSee https://www.atnf.csiro.au/vlbi/dokuwiki/doku.php/difx/clusterdef for documentation on the machines file format'

        description = 'A program to write the machines file appropriate for a particular DiFX job.'

        parser = argparse.ArgumentParser(epilog=epilog, formatter_class=argparse.RawDescriptionHelpFormatter,description=description)
  
        parser.add_argument("-v", "--verbose", action="count", dest="verbose", default=0, help="increase verbosity level");
        parser.add_argument("-m", "--machines", dest="machinesfile", default="", help="use MACHINESFILE instead of $DIFX_MACHINES")
        parser.add_argument("-n", "--nothreads", dest="dothreads", action="store_false", default=True, help="don't write a .threads file")
        parser.add_argument("-d", "--difxdb", dest="usedifxdb", action="store_true", default=False, help="use difxdb to obtain data location")
        parser.add_argument("-r", "--rankfile", dest="dorankfile", action="store_true", default=False, help="additionally write an OpenMPI rank file")
        parser.add_argument("--ignore-incomplete-module", dest="ignoreIncompleteModules", action="store_true", default=True, help="Proceed even when Mark6 modules are found to be incomplete.")
        parser.add_argument("input",  nargs='+', help= "DiFX inputs file(s)")
        
        args = parser.parse_args()

        overheadcores = 0
        verbose = args.verbose
        dothreads = args.dothreads
        useDifxDb = args.usedifxdb
        dorankfile = args.dorankfile

        # assign the cluster definition file
        if len(args.machinesfile) == 0:
                try:
                        machinesfile = environ['DIFX_MACHINES']
                except:
                        print ('DIFX_MACHINES environment has to be set. Use -m option instead')
                        sys.exit(1)
        else:
                machinesfile = args.machinesfile


        # check that cluster definition file exist
        if not isfile(machinesfile):
                sys.exit("Cluster definition file not found: %s" % machinesfile)

        if getenv('DIFX_GROUP_ID'):
                umask(2)

        # list of input files to process
        files = args.input

        quit = False
        for f in files:
                if not isfile(f):
                        print('File %s not found' % f)
                        quit = True
        if quit:
                print('genmachines quitting.')
                exit(1)

        if verbose > 0:
                print('DIFX_MACHINES -> %s' % machinesfile)

        port = getenv('DIFX_MESSAGE_PORT')
        if port == None:
                port = defaultDifxMessagePort
        else:
                port = int(port)
        group = getenv('DIFX_MESSAGE_GROUP')
        if group == None:
                group = defaultDifxMessageGroup

        
        # read machines file
        difxmachines = DifxMachines(machinesfile)
        
        # compare version
        fail = False
        major, minor = difxmachines.getVersion()
        reqMaj,reqMin = minMachinefileVersion.split(".")
        if (reqMaj < major):
            fail = False
        elif (reqMaj > major):
            fail = True
        else:
            if reqMin > minor:
                fail = True
            elif reqMin < minor:
                fail = False
            else:
                fail = False
        if fail:
            print("ERROR: This version of genmachines requires a cluster defintion file of version > %s. Found version is: %s.%s" % (minMachinefileVersion, major,minor))
            exit(1)
        
        for file in files:
                v = run(file, machinesfile, overheadcores, verbose, dothreads, useDifxDb, dorankfile)
                if v != 0:
                        exit(v)
