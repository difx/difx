#!/usr/bin/env python3

#**************************************************************************
#   Copyright (C) 2008-2023 by Walter Brisken and Helge Rottmann          *
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

PROGRAM = 'startdifx'
VERSION = '3.0.7'
VERDATE = '20221219'
AUTHOR  = 'Walter Brisken and Helge Rottmann'

defaultgroup = "224.2.2.1"
defaultport  = 50200
defaultttl   = 3
maxGenmachineFail = -1

genmachines              = 'genmachines -v'
defaultDelayModelProgram = 'calcif2'
difx2fits                = 'difx2fits -v'
mpiOptions               = '--mca mpi_yield_when_idle 1 --mca rmaps seq'
agent                    = ''

from sys import argv, exit, stdout
from os import popen, getcwd, system, getenv, getpid, environ
from os.path import isfile, isdir, isfile
from time import time, sleep, asctime
from glob import glob
from copy import deepcopy
from xml.parsers import expat
from optparse import OptionParser
import argparse
import socket
import struct
import signal
import subprocess

delayModelProgram = defaultDelayModelProgram
delayModelOptions = getenv("DIFX_CALC_OPTIONS")

verbose = 0

def getVersion():
        return '%s ver. %s  %s  %s' % (PROGRAM, VERSION, VERDATE, AUTHOR)

def epilog():
    epilog = '\nThis program responds to the following environment variables:\n'
    epilog += 'DIFX_MESSAGE_GROUP and DIFX_MESSAGE_PORT can be used to override the default group/port of %s/%d\n' % (defaultgroup, defaultport)
    epilog += 'DIFX_HEAD_NODE must name the correlation head node (only with the -m option).\n'
    epilog += 'DIFX_MPIRUNOPTIONS can be used to pass options to the mpirun command.\n'
    epilog += 'DIFX_CALC_PROGRAM can be used to change the delay model program (the default is %s, but difxcalc can be used).\n' % defaultDelayModelProgram
    epilog += 'DIFX_CALC_OPTIONS can be used to override options to the delay model program.\n'
    return (epilog)

class Parser:

        def __init__(self):
                self._parser = expat.ParserCreate()
                self._parser.StartElementHandler = self.start
                self._parser.EndElementHandler = self.end
                self._parser.CharacterDataHandler = self.data
                self.message = ''
                self.mjd = 0.0
                self.state = ''
                self.tmp = ''
                self.ok = False
                self.unit = ''
                self.mpiId = -1
                self.id = ''
                self.tag = ''

        def feed(self, data):
                self._parser.Parse(data, 0)

        def close(self):
                self._parser.Parse("", 1) # end of data
                del self._parser # get rid of circular references

        def start(self, tag, attrs):
                self.tag = tag
                self.tmp = ''
                if tag == 'difxStatus':
                        self.ok = True

        def end(self, tag):
                if tag == 'message' and self.ok:
                        self.message = self.tmp
                elif tag == 'state':
                        self.state = self.tmp
                elif tag == 'visibilityMJD':
                        self.mjd = float(self.tmp)
                elif tag == 'from':
                        self.unit = self.tmp.lower()
                elif tag == 'identifier':
                        self.id = self.tmp
                elif tag == 'mpiProcessId':
                        self.mpiId = int(self.tmp)

        def data(self, data):
                if self.tag == 'message':
                        self.tmp = self.tmp + data      
                else:
                        self.tmp = data

        def getInfo(self):
                if self.ok:
                        return 'MPI[%2d] %-9s %-12s %-7s %s' % (self.mpiId, self.unit, self.id, self.state, self.message)
                else:
                        return ''

def watchJob(jobId):
        
        if verbose > 1:
                print('Watching %s:' % jobId)

        port = getenv('DIFX_MESSAGE_PORT')
        if port == None:
                print('DIFX_MESSAGE_PORT needs to be defined')
                exit(0)
        else:
                port = int(port)
        group = getenv('DIFX_MESSAGE_GROUP')
        if group == None:
                print('DIFX_MESSAGE_GROUP needs to be defined')
                exit(0)

        # FIXME: eventually migrate to IPv6 compliant multicast receive
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind(('', port))
        mreq = struct.pack("4sl", socket.inet_aton(group), socket.INADDR_ANY)
        s.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)

        try:
                while 1:
                        try:
                                message = s.recv(8000).decode('utf-8')
                                if len(message) > 0 and message[0] == '<':
                                        p = Parser()
                                        p.feed(message)
                                        info = p.getInfo()
                                        p.close()
                                        now = time()
                                        # print(now)
                                        if p.ok and p.id == jobId:
                                                if p.state == 'Running':
                                                        if verbose > 0:
                                                                stdout.write('.')
                                                                stdout.flush()
                                                else:
                                                        if verbose > 0:
                                                                stdout.write("<%s>" % p.state)
                                                                stdout.flush()
                                                if p.state == 'MpiDone' or p.state == 'Crashed':
                                                        return
                        except socket.timeout:
                                pass
        except KeyboardInterrupt:
                pass
        
        if verbose > 1:
                print('Watching %s:' % jobId)

        port = getenv('DIFX_MESSAGE_PORT')
        if port == None:
                print('DIFX_MESSAGE_PORT needs to be defined')
                exit(0)
        else:
                port = int(port)
        group = getenv('DIFX_MESSAGE_GROUP')
        if group == None:
                print('DIFX_MESSAGE_GROUP needs to be defined')
                exit(0)

        # FIXME: eventually migrate to IPv6 compliant multicast receive
        s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        s.bind(('', port))
        mreq = struct.pack("4sl", socket.inet_aton(group), socket.INADDR_ANY)
        s.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)

        try:
                while 1:
                        try:
                                message = s.recv(8000).decode('utf-8')
                                if len(message) > 0 and message[0] == '<':
                                        p = Parser()
                                        p.feed(message)
                                        info = p.getInfo()
                                        p.close()
                                        now = time()
                                        # print(now)
                                        if p.ok and p.id == jobId:
                                                if p.state == 'Running':
                                                        if verbose > 0:
                                                                stdout.write('.')
                                                                stdout.flush()
                                                else:
                                                        if verbose > 0:
                                                                stdout.write("<%s>" % p.state)
                                                                stdout.flush()
                                                if p.state == 'MpiDone' or p.state == 'Crashed':
                                                        return
                        except socket.timeout:
                                pass
        except KeyboardInterrupt:
                pass


def updateMpiOptions():
        global mpiOptions
        opt = getenv('DIFX_MPIRUNOPTIONS')
        if opt is not None:
                mpiOptions = opt

        
def updateEnvironment(filename):
        
        savedEnvironment = []
        
        if isfile(filename):
                lines = open(filename).readlines()
                for l in lines:
                        if l[0] == '#':
                                continue
                        e = l.strip().split()
                        if len(e) == 2:
                                savedEnvironment.append([e[0], getenv(e[0])])
                                environ[e[0]] = e[1]

        return savedEnvironment


def restoreEnvironment(savedEnvironment):

        for e in savedEnvironment:
                if e[1] == None:
                        del environ[e[0]]
                else:
                        environ[e[0]] = e[1]


def getjobdifxversion(fileBase):
        version = ''
        label = ''

        files = glob(fileBase+'.calc')
        if len(files) < 1:
                print('Can\'t find file %s.calc.  Quitting.' % fileBase)
                exit(0);
        d = open(files[0]).readlines()
        for l in d:
                if l[:12] == 'DIFX VERSION':
                        version = l[20:].strip()
                if l[:12] == 'DIFX LABEL':
                        label = l[20:].strip()
        
        return version, label

def testDifxVersion(fileBase, override):
        jobversion, joblabel = getjobdifxversion(fileBase)
        version = getenv('DIFX_VERSION')
        label = getenv('DIFX_LABEL')
        if label == None:
                label = ''
        if version == None:
                print('Warning: env. var. DIFX_VERSION not defined!')
        if jobversion == None:
                print('Warning: %s.calc does not contain version info' % fileBase)
        if version != None and jobversion != None and version != jobversion:
                print('Warning: $DIFX_VERSION and job version (in .calc file) mismatch')
                print('    <%s> != <%s>' % (version, jobversion))
                if override:
                        print('Continuing anyway due to --override-version')
                else:
                        print('Quitting since --override-version not specified')
                        exit(0)

        if label != '' and joblabel != '' and label != joblabel:
                print('Warning: $DIFX_LABEL and job label (in .calc file) mismatch')
                print('    <%s> != <%s>' % (ver, jobver))

        if label == '':
                label = version
        
        return version, label
        

def sendMessage(fileBase, state, statusmessage):
        group = getenv('DIFX_MESSAGE_GROUP')
        if group == None:
                group = defaultgroup
        
        port = getenv('DIFX_MESSAGE_PORT')
        if port == None:
                port = defaultport
        else:
                port = int(port)
                
        identifier = fileBase.split('/')[-1]
        
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
        sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, defaultttl)

        message = \
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" \
                "<difxMessage>" \
                  "<header>" \
                        "<from>%s</from>" \
                        "<mpiProcessId>-1</mpiProcessId>" \
                        "<identifier>%s</identifier>" \
                        "<type>DifxStatusMessage</type>" \
                  "</header>" \
                  "<body>" \
                        "<seqNumber>-1</seqNumber>" \
                        "<difxStatus>" \
                          "<state>%s</state>" \
                          "<message>%s</message>" \
                          "<visibilityMJD>0</visibilityMJD>" \
                        "</difxStatus>" \
                  "</body>" \
                "</difxMessage>\n" % \
                (socket.gethostname(), identifier, state, statusmessage)
        
        sock.sendto(message.encode('utf-8'), (group, port) )


# getNodes depends on the particular assumptions made by genmachines
def getNodes(fileBase):
        threads = []
        machines = []
        datastreams = []
        processors = {}

        threadData = open(fileBase + '.threads').readlines()
        for t in threadData[1:]:
                threads.append(int(t))
        machineData = open(fileBase + '.machines').readlines()
        
        nDatastream = len(machineData) - len(threads) - 1

        for m in range(1, nDatastream+1):
                datastreams.append(machineData[m].strip().split()[0])

        for m in range(1+nDatastream, len(machineData)):
                i = m - 1 - nDatastream
                t = threads[i]
                if t in processors:
                        processors[t].append(machineData[m].strip().split()[0])
                else:
                        processors[t] = [machineData[m].strip().split()[0]]

        return datastreams, processors


def sendStartMessage(fileBase, difxVersion, localHeadNode, restartSeconds):
        inputFile = fileBase + '.input'

        group = getenv('DIFX_MESSAGE_GROUP')
        if group == None:
                group = defaultgroup
        
        port = getenv('DIFX_MESSAGE_PORT')
        if port == None:
                port = defaultport
        else:
                port = int(port)
                
        headNode = getenv('DIFX_HEAD_NODE')
        # -l option overrides DIFX_HEAD_NODE
        if localHeadNode:
                headNode = socket.gethostname()
        corrHeadNode = headNode

        identifier = fileBase.split('/')[-1]
        
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
        sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, defaultttl)

        dsList, pList = getNodes(fileBase)
        system("rm -f %s.machines" % fileBase)
        system("rm -f %s.threads" % fileBase)

        dataStreams = ""
        for d in dsList:
                dataStreams = dataStreams + d + " "
        dataStreams = dataStreams.strip()
        processors = ""
        pKeys = pList.keys()
        for p in pKeys:
                processors = processors + "<process threads=\"%s\" nodes=\"" % p
                for q in pList[p]:
                        processors = processors + q + " "
                processors = processors.strip() + "\"/>"
        
        if restartSeconds > 0:
                restartOption = '<restartSeconds>%f</restartSeconds>' % restartSeconds
        else:
                restartOption = ''

        message = \
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" \
                "<difxMessage>" \
                  "<header>" \
                        "<from>startdifx</from>" \
                        "<to>%s</to>" \
                        "<mpiProcessId>-1</mpiProcessId>" \
                        "<identifier>%s</identifier>" \
                        "<type>DifxStart</type>" \
                  "</header>" \
                  "<body>" \
                        "<difxStart>" \
                          "<input>%s</input>" \
                          "<manager node=\"%s\"/>" \
                          "<datastream nodes=\"%s\"/>" \
                          "%s" \
                          "<difxVersion>%s</difxVersion>" \
                          "%s" \
                        "</difxStart>" \
                  "</body>" \
                "</difxMessage>\n" % \
                (headNode, identifier, inputFile, corrHeadNode, dataStreams, processors, difxVersion, restartOption)

        if verbose > 0:
                print("Sending: ", message)

        sock.sendto(message.encode('utf-8'), (group, port) )

        watchJob(fileBase.split('/')[-1])

        print("\n")


# verify presence of .machines file
def doMachines(fileBase, machinesPolicy, cache):

        machinesFile = fileBase + '.machines'

        cmd = 'grep "FILE " %s.input' % fileBase
        files = popen(cmd, 'r').readlines()
        
        cmd = 'grep "CORE CONF FILENAME" %s.input' % fileBase
        f = popen(cmd, 'r').readlines()
        if len(f) == 1:
                threadsFile = f[0].split(':')[-1].strip()
        else:
                threadsFile = fileBase + '.threads'

        if cache[2] == files:
                if verbose > 0:
                        print('Using cached machines/threads info')
                cmd = 'cp %s %s' % (cache[0], machinesFile);
                if verbose > 1:
                        print('Executing: %s' % cmd)
                system(cmd)
                cmd = 'cp %s %s' % (cache[1], threadsFile);
                if verbose > 1:
                        print('Executing: %s' % cmd)
                system(cmd)
        else:
                if machinesPolicy == 2:
                        system('rm -f %s' % machinesFile)

                nFail = 0
                while not isfile(machinesFile):
                        if machinesPolicy > 0:
                                localHeadString = ""

                                extraOpts = ""
                                if args.difxdb:
                                        extraOpts += " -d "
                                if args.machinesFile != "":
                                        extraOpts += " -m %s " % (args.machinesFile)

                                cmd = '%s %s %s.input' % (genmachines, extraOpts, fileBase)
                                if verbose > 1:
                                        print('Executing: %s' % cmd)
                                v = -1
                                while v !=0 :
                                        v = subprocess.call(cmd, shell=True)
                                        # CTRL+C
                                        if v==8:
                                                exit(0)
                                if not isfile(machinesFile):
                                        nFail += 1
                                        if nFail > maxGenmachineFail and maxGenmachineFail > 0:
                                                return 0
                        else:
                                return 0

        cache[0] = machinesFile
        cache[1] = threadsFile
        cache[2] = files

        return len(open(machinesFile).readlines())

def handler(signum, frame):
        print('Signal handler called with signal', signum)
        exit(0)

# Start difx with DifxStartMessage
def runMessage(fileBase, machinesCache, restartSeconds):

        difxVersion, difxLabel = testDifxVersion(fileBase, args.override)

        identifier = fileBase.split('/')[-1]

        if not isfile(fileBase+'.input'):
                return 'Error: input file %s.input not found' % fileBase
        
        if not isfile(fileBase+'.im'):
                if args.makeModel and isfile(fileBase+'.calc'):
                        if delayModelOptions != None and len(delayModelOptions) > 0:
                                calcOptions = delayModelOptions
                        else:
                                calcOptions = ""
                                if args.override:
                                        calcOptions += "--override-version "
                                for i in range(verbose):
                                        calcOptions += "-v "
                        system('%s %s %s.calc' % (delayModelProgram, calcOptions, fileBase))
                else:
                        return 'Error: model not available for %s' % fileBase
        
        if restartSeconds > 0:
                if not isdir(fileBase+'.difx'):
                        print('Warning: restartSeconds = %d and no existing output!' % restartSeconds)
        elif isdir(fileBase+'.difx') or isfile(fileBase+'.difx'):
                if args.force :
                        if verbose > 0:
                                print('Removing %s.difx' % fileBase)
                        sendMessage(fileBase, 'Info', 'Deleting %s.difx' % fileBase)
                        system('rm -rf %s.difx' % fileBase)
                else:
                        print('Warning: output file %s.difx exists' % fileBase)
                        return 
        
        np = doMachines(fileBase, args.machinesPolicy, machinesCache)
        if np <= 0:
                return 'Error: %s.machines not found' % fileBase

        if args.commentStart != None:
                print(args.commentStart.replace('%B', fileBase).replace('%b', fileBase.split('/')[-1]))
        sendStartMessage(fileBase, difxLabel, args.useLocalHead, restartSeconds)
        if args.commentEnd != None:
                print(args.commentEnd.replace('%B', fileBase).replace('%b', fileBase.split('/')[-1]))

        return None

# Start difx directly with mpirun
def runDirect(fileBase, machinesCache, restartSeconds):

        if verbose > 1:
                print(args)
        difxVersion, difxLabel = testDifxVersion(fileBase, args.override)

        identifier = fileBase.split('/')[-1]

        out = popen('which mpifxcorr').readlines()
        if len(out) != 1:
                return 'Error: mpifxcorr not found'
        pgm = out[0].strip()

        if not isfile(fileBase+'.input'):
                return 'Error: input file %s.input not found' % fileBase
        
        if not isfile(fileBase+'.im'):
                if args.makeModel and isfile(fileBase+'.calc'):
                        if delayModelOptions != None and len(delayModelOptions) > 0:
                            calcOptions = delayModelOptions
                        else:
                            calcOptions = ""
                            if args.override:
                                    calcOptions += "--override-version "
                            for i in range(verbose):
                                    calcOptions += "-v "
                        system('%s %s %s.calc' % (delayModelProgram, calcOptions, fileBase))
                else:
                        return 'Error: model not available for %s' % fileBase
        
        if restartSeconds > 0:
                if not isdir(fileBase+'.difx'):
                        print('Warning: restartSeconds = %d and no existing output!' % restartSeconds)
        elif isdir(fileBase+'.difx') or isfile(fileBase+'.difx'):
                if args.force:
                        if verbose > 0:
                                print('Removing %s.difx' % fileBase)
                        sendMessage(fileBase, 'Info', 'Deleting %s.difx' % fileBase)
                        system('rm -rf %s.difx' % fileBase)
                else:
                        print('Warning: output file %s.difx exists' % fileBase)
                        return
        
        # generate machine file
        np = doMachines(fileBase, args.machinesPolicy, machinesCache)
        if np <= 0:
                return 'Error: %s.machines not found' % fileBase

        # check before proceeding
        if verbose == 0:
                cmd = 'checkmpifxcorr -q %s.input' % fileBase
        else:
                cmd = 'checkmpifxcorr %s.input' % fileBase
        if verbose > 1:
                print('Executing: %s' % cmd)
        if system(cmd):
                return 'Error: checkmpifxcorr has issues with this job'

        # spawn a logger process that will quit once this script ends
        # this will quietly fail on systems without difxlog installed
        cmd = ['difxlog', str(identifier), fileBase+".difxlog", "4", str(getpid())]
        if verbose > 1:
                print('Executing: %s' % ' '.join(cmd))
        logProcess = subprocess.Popen(cmd)

        if difxLabel == None or difxLabel == '':
                difxProgram = 'mpifxcorr'
        else:
                difxProgram = 'runmpifxcorr.' + difxLabel
        if verbose > 1:	
                print("difxProgram: %s" % (difxProgram))

        if restartSeconds > 0.0:
                restartOption = ' -r%f' % restartSeconds
        else:
                restartOption = ''

        if len(agent) > 0:      # hand off to agent
                cmd = '%s %s' % (agent, fileBase)
        else:                   # hand off to mpirun
                cmd = 'mpirun -np %d --hostfile %s.machines %s  %s %s.input%s' % (np, fileBase, mpiOptions, difxProgram, fileBase, restartOption)

        sendMessage(fileBase, 'Spawning', 'Spawning %d processes' % np)
        if verbose > 0:
                print('Executing: ', cmd)
        if args.commentStart != None:
                print(args.commentStart.replace('%B', fileBase).replace('%b', fileBase.split('/')[-1]))
        t0 = time()
        if args.logFile != None:
                system(cmd + " >> " + args.logFile.replace('%b', fileBase.split('/')[-1]) + " 2>&1")
        else:
                system(cmd)
        t1 = time()
        if verbose > 0:
                print('Elapsed time (s) =', t1-t0)
        if args.commentEnd != None:
                print(args.commentEnd.replace('%B', fileBase).replace('%b', fileBase.split('/')[-1]))
        groupId = getenv('DIFX_GROUP_ID')
        if groupId != None:
                cmd = 'chown :%s %s.difx/*' % (groupId, fileBase)
                if verbose > 1:
                        print('Executing: %s' % cmd)
                system(cmd)
                cmd = 'chmod g+w %s.difx/*' % fileBase
                if verbose > 1:
                        print('Executing: %s' % cmd)
                system(cmd)
        sendMessage(fileBase, 'MpiDone', '')

        # end difxlog (necessary when running startdifx with joblist)
        if runJobList:
                logProcess.kill()

        return None

def waitMJD(mjd):
        mjd_unix0 = 40587.0     # MJD at timestamp = 0
        target = (mjd - mjd_unix0)*86400.0
        now = time()
        totalWait = target - now
        if totalWait < 0:
                return totalWait
        print('Wait time is %d seconds.' % int(totalWait))
        while True:
                now = time()
                if now >= target:
                        break
                delta = target - now
                print('Waiting %d more seconds...' % int(delta), end='\r')
                sleep(1)
        print('The wait is over.          ')
        return totalWait

def checkFiles(fileBase, level):
        nBad = 0
        cmd = 'difxfilelist %s' % fileBase
        filedata = popen(cmd).readlines()
        for f in filedata:
                s = f.split()
                if 'VDIF' in s[3].upper():
                        levels = { 1: '--short', 2: '', 3: '--full' }
                        cmd = 'checkVDIF %s --bits %s --framesize %s %s' % (levels[level], s[4], s[5], s[2])
                        print('\nChecking file: %s' % cmd)
                        rv = system(cmd)
                        if rv != 0:
                                print('\nFile %s appears to have a problem.  Correlation will likely fail.' % s[2])
                                nBad += 1
                # FIXME: add other file check options here as they are implemented
        
        if nBad > 0:
                exit(0)

def run(fileBase, machinesCache, restartSeconds):
        
        if args.checkfiles != None:
                checkFiles(fileBase, args.checkfiles)

        if args.wait > 0:
                intmjd = 0
                sec = 0.0
                dur = 0.0
                data = open(fileBase + '.input').readlines()
                for d in data:
                        s = d.strip().split(':')
                        if len(s) > 1:
                                k = s[0].strip()
                                v = s[1].strip()
                                if k == 'START MJD':
                                        intmjd = int(v)
                                elif k == 'START SECONDS':
                                        sec = float(v)
                                elif k == 'EXECUTE TIME (SEC)':
                                        dur = float(v)
                mjd = intmjd + sec/86400.0
                end = mjd + dur/86400.0
                now = time()
                curmjd = now/86400.0 + 40587.0

                if curmjd > end:
                        print('Skipping job %s because its end time has passed.' % fileBase)
                        return None 

                waitMJD(mjd + args.wait/86400.0)

        savedEnvironment = updateEnvironment(fileBase+'.input.env')
        if args.useStartMessage:
                rv = runMessage(fileBase, machinesCache, restartSeconds)
        else:
                rv = runDirect(fileBase, machinesCache, restartSeconds)
        restoreEnvironment(savedEnvironment)

        return rv

#------------------
# main starts here
#------------------

restartSeconds = 0.0
fileBaseList = []
cwd = getcwd()
machinesCache = ['','',['Nothing to see here']]
updateMpiOptions()

signal.signal(signal.SIGINT, handler)

parser = argparse.ArgumentParser(description="A program to simplify the launching of mpifxcorr.", epilog=epilog(), formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("-A", "--agent", type=str, default="", help="call mpirun through this agent with filebase as only argument")
group = parser.add_mutually_exclusive_group()
group.add_argument("-g", "--genmachines", dest="machinesPolicy", action="store_const", const=2, default=2, help="will run genmachines even if not needed [default]")
group.add_argument("-a", "--automachines", dest="machinesPolicy", action="store_const", const=1, help="will run genmachines if needed")
group.add_argument("-n", "--nomachines", dest="machinesPolicy", action="store_const", const=0, help="will not run genmachines, even if needed")
parser.add_argument("-c", "--checkfiles", action="count", help="do a sanity check on the files to correlate (only for some formats).")
parser.add_argument("-M", "--machines-file", dest="machinesFile", action="store", type=str, default="", help="use supplied machines file instead of one based on job name")
parser.add_argument("-L", "--log-file", dest="logFile", action="store", type=str, help="capture stderr and stdout and write to specified file")
parser.add_argument("-m", "--message", dest="useStartMessage", action="store_true", help="start difx via DifxStartMessage")
parser.add_argument("-f", "--force", action="store_true", help="force running even if output file exists")
parser.add_argument("-d", "--dont-calc", dest="makeModel", action="store_false", help="will not calculate delay model, even if needed")
parser.add_argument("-D", "--difxdb", dest="difxdb", action="store_true", help="make use of difxdb to obtain module location")
parser.add_argument("-F", "--fits", dest="makeFits", action="store_true", help="generate 1 fits file per job at end of each job")
parser.add_argument("-v", "--verbose", action="count", default=0, help="send more output to the screen and difxlog file (use -v -v for extra info)")
parser.add_argument("-q", "--quiet", action="count", default=0, help="be quieter")
parser.add_argument("-w", "--wait",  action="count", default=0, help="wait until job start time before launching")
parser.add_argument("-l", "--localhead", dest="useLocalHead", action="store_true", help="use the current host as the head node. Overrides DIFX_HEAD_NODE.")
parser.add_argument("--override-version", dest="override", action="store_true", help="ignore difx version differences")
parser.add_argument("--comment-start", dest="commentStart", action="store", help="a string to print just before starting each job")
parser.add_argument("--comment-end", dest="commentEnd", action="store", help="a string to print just after each job ends")
parser.add_argument('--version', action='version', version=getVersion())
parser.add_argument('delay', type=str, nargs='?', help='an optional delay (seconds) to add to the job start time')
parser.add_argument('input', type=str, nargs='+', help='the list of DiFX .input files to process. Alternatively a DiFX .joblist file can be given.')

args = parser.parse_args()

verbose = args.verbose - args.quiet

# parse arguments
joblist = args.input
try:
        # check if start delay parameter was given
        if args.delay:
          restartSeconds = float(args.delay)
except:
        joblist = args.delay.split(" ")  + args.input
        print (joblist)

if args.agent: agent = args.agent

# loop over all arguments
fb = None
runJobList = False
for a in joblist:
        # check for extension
        if a[-6:] == '.input':
                fb = a[:-6]
        elif a[-8:] == '.joblist':
                runJobList = True
                d = open(a).readlines()
                for l in d:
                        parts = l.split()
                        if len(parts) == 0:
                                job = l.strip()
                        else:
                                job = parts[0].strip()

                        if job.startswith("#") or job.startswith("exper"):
                                continue
                        if parts[0] == '/':
                                fileBaseList.append(parts[0])
                        else:
                                fileBaseList.append(cwd + '/' + parts[0])
        else:
                fb = a

        if fb != None:
                if fb[0] == '/':
                        fileBaseList.append(fb)
                else:
                        fileBaseList.append(cwd + '/' + fb)
        
# verify that joblist is not empty
if len(fileBaseList) < 1:
        optParser.error("ERROR: At least one .input or .joblist file must be given.")

# if more than one job: terminate difxlog after each job completes, rather than terminating N open difxlog's after N jobs
if len(fileBaseList) > 1:
        runJobList = True

# verify that machinesfile exists
if args.machinesFile != "" and  isfile(args.machinesFile) == False:
        print("ERROR: The machines file (%s) does not exist" % args.machinesFile)
        exit(1)

headNode = getenv('DIFX_HEAD_NODE')
if args.useStartMessage and args.useLocalHead == False and headNode == None:
        print("ERROR: Environment variable DIFX_HEAD_NODE must be defined.")
        print("Alternatively  you can use the -l option in case mk5daemon -H is running locally on this host.")
        exit(1)
        
nBad = 0
for fileBase in fileBaseList:
        if not isfile(fileBase + '.input'):
                print('Error: %s.input not found (or not a regular file)' % fileBase)
                nBad += 1

if args.logFile != None:
        print('All command line output is being redirected to log file(s) %s' % args.logFile.replace('%b', '<filebase>'))

if nBad > 0:
        exit(1)

dcp = getenv("DIFX_CALC_PROGRAM")
if dcp != None:
        delayModelProgram = dcp
        if verbose > 0:
                print('Environment variable DIFX_CALC_PROGRAM was set, so using specified calc program: %s' % delayModelProgram)
                print('')

for fileBase in fileBaseList:
        a = fileBase.rfind("/")
        jobname = fileBase[a+1:]

        if len(jobname) > 31:
                sendMessage(fileBase, 'ABORTED', "Did not start job because jobname is too long!")
                print("Job name %s is too long and will be skipped!" % fileBase)
                exit(1)

        v = run(fileBase, machinesCache, restartSeconds)

        if v != None:
                sendMessage(fileBase, 'ABORTED', v)
                print(v)
                exit(1)
        elif args.makeFits == True:
                system('%s %s %s.FITS' % (difx2fits, fileBase, fileBase))
