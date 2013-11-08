#!/usr/bin/env python

#**************************************************************************
#   Copyright (C) 2008-2013 by Walter Brisken                             *
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

from string import split, strip, upper, lower
from sys import exit
from os import getenv, umask, environ
from os.path import isfile
import socket
import struct
import subprocess
import signal
import sys
from optparse import OptionParser
from xml.parsers import expat
from copy import deepcopy
try:
    from difxfile.difxmachines import *
except ImportError:
    print "ERROR: Cannot find difxmachines library. Please include $DIFXROOT/lib/python in your $PYTHONPATH environment"
    sys.exit(1)

author  = 'Walter Brisken and Helge Rottmann'
version = '2.3.0'
verdate = '20131108'
minMachinefileVersion = "1.0"	# cluster definition file must have at least this version

defaultDifxMessagePort = 50200
defaultDifxMessageGroup = '224.2.2.1'	

def getUsage():
        """
        Compile usage text for OptionParser
        """
	usage = "%prog [options] [<input1> [<input2>] ...]\n"
	usage += '\n<input> is a DiFX .input file.'
	usage += '\nA program to find required Mark5 modules and write the machines file'
	usage += '\nappropriate for a particular DiFX job.'
	usage += '\n\nNote: %prog respects the following environment variables:'
        usage +=  '\nDIFX_MACHINES: required, unless -m option is given. -m overrides DIFX_MACHINES.'
        usage +=  '\nDIFX_GROUP: if not defined a default of %s will be used.' % defaultDifxMessageGroup
        usage +=  '\nDIFX_PORT: if not defined a default of %s will be used.' % defaultDifxMessagePort
	
	return(usage)

class Parser:

    def __init__(self):
        self._parser = expat.ParserCreate()
        self._parser.StartElementHandler = self.start
        self._parser.EndElementHandler = self.end
        self._parser.CharacterDataHandler = self.data
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
        if tag == 'mark5Status':
		self.ok = True

    def end(self, tag):
        if tag == 'bankAVSN' and self.ok:
		if len(self.tmp) != 8:
			self.vsnA = 'none'
		else:
			self.vsnA = upper(self.tmp)
        if tag == 'bankBVSN' and self.ok:
		if len(self.tmp) != 8:
			self.vsnB = 'none'
		else:
			self.vsnB = upper(self.tmp)
	if tag == 'from':
		self.unit = lower(self.tmp)
	if tag == 'state' and self.ok:
		self.state = self.tmp

    def data(self, data):
        self.tmp = data

    def getinfo(self):
	if self.ok:
        	return [self.unit, self.vsnA, self.vsnB, self.state, self.sender]
	else:
		return ['unknown', 'none', 'none', 'Unknown', 'unknown']

def vsn_request():
	src = socket.gethostname()
	dest = '<to>mark5</to>'
	cmd = 'getvsn'

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
	  '</difxMessage>' % (src, dest, cmd)

	return message

def getVsnsByMulticast(maxtime, datastreams, verbose):
	dt = 0.2
	t = 0.0
        modlist = []

	port = getenv('DIFX_MESSAGE_PORT')
	if port == None:
		port = defaultDifxMessagePort
	else:
		port = int(port)
	group = getenv('DIFX_MESSAGE_GROUP')
	if group == None:
		group = defaultDifxMessageGroup

        for stream in datastreams:
            if len(stream.vsn) > 0:
                modlist.append(stream.vsn)
            
	missing = deepcopy(modlist)

	message = vsn_request()

	# First send out a call for VSNs
	sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
	sock.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 2)
	sock.sendto(message, (group, port))

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
	while t < maxtime and len(missing) > 0:
		try:
			message, address = s.recvfrom(2048)
			sender = split(socket.gethostbyaddr(address[0])[0], '.')[0]
			if verbose > 1:
				print message
			p = Parser()
			p.feed(sender, message)
			info = p.getinfo()
			p.close()
			if info[0] == 'unknown':
				continue
			if info[0] in machines:
				continue
			machines.append(info[0])
			results.append(info)
			if info[1] in missing and info[2] in missing:
				conflicts.append(info)
			if info[1] in missing:
				missing.remove(info[1])
				if info[3] != 'Idle' and info[3] != 'Close':
					notidle.append(info[1])
			if info[2] in missing:
				missing.remove(info[2])
				if info[3] != 'Idle' and info[3] != 'Close':
					notidle.append(info[2])

		except socket.timeout:
			t += dt
		except socket.herror:
			print 'Weird: cannot gethostbyaddr for %s' % address[0]

	results.sort()
	conflicts.sort()
	missing.sort()
	notidle.sort()

	return results, conflicts, missing, notidle

def getVsnsFromInputFile(inputfile):
        """
        Parse the datastream section of the input file to
        obtain VSNs file paths
        """
        datastreams = []
	nds = 0
        dsindices = []
	dssources = []
	dscount = 0
        
	input = open(inputfile).readlines()
    
	for inputLine in input:
		s = split(inputLine, ':')
		if len(s) < 2:
			continue;
		key = s[0].strip()
		keyparts = key.split()
		value = s[1].strip()
                
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
			numDS,index = split(keyparts[1], '/')
			ds = int(numDS.strip())

                        if ds < nds:
                            if datastreams[ds].type == 'MODULE':
                                datastreams[ds].vsn = value
                            elif datastreams[ds].type == 'FILE':
                                if datastreams[ds].path == "":
                                    datastreams[ds].path = os.path.dirname(value)
                             
            
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

def writemachines(basename, hostname, results, datastreams, overheadcores, verbose):
        """
        Write machines file to be used by mpirun
        """
        
	dsnodes = []
	threads = []
        
	for stream in datastreams:
            if stream.type == "FILE":
                # check if path for this datastream matches storage area defined in the cluster definition file
                # strip off last directory for matching
                #path = stream.path[:rfind(stream.path, "/")]
                        
                matchCount = 0
                matchNode = ""
                for node in difxmachines.getStorageNodes():
                    for url in node.fileUrls:
                        if stream.path.startswith(url):
                            matchCount += 1
                            matchNode = node.name
                            break
                if matchCount > 1:
                    print "ERROR: identical storage area is associated with different hosts: %s" % path
                    sys.exit(1)
                elif matchCount == 1:
                    dsnodes.append(matchNode)
                else:
                    # use compute node             
                    for node in difxmachines.getComputeNodes():
                        # skip if already used as datastream node
                        if node.name in dsnodes:
                            continue
                            
                        dsnodes.append(node.name)
                        break
                
            elif stream.type == "MODULE":    
                matchNode = ""
                for r in results:
                    # find  module either in bank A or B
                    if r[1] == stream.vsn or r[2] == stream.vsn:
                            if r[0] in difxmachines.getMk5NodeNames():
                                matchNode = r[0]
                            else:
                                # use message sending host
                                matchNode = r[4]

                if matchNode in difxmachines.getMk5NodeNames():
                    dsnodes.append(matchNode)
                else:
                    print '%s not listed as an active mark5 host in machines file' % matchNode
                    return []
                
	# write machine file
	o = open(basename+'machines', 'w')
        
        # head node
        o.write('%s slots=1 \n' % (hostname))
        
        # datastream nodes
        for node in dsnodes:
            o.write('%s slots=1 \n' % (node))
            
        # compute nodes
        for node in difxmachines.getComputeNodes():
            usedThreads = 0
            # if compute node is also used as datastream nodes reduce number of threads
            if node.name in dsnodes:
                usedThreads = dsnodes.count(node.name)
          
            # if head node is also used as compute nodes reduce number of threads by one
            if node.name in hostname:
                usedThreads = 1
                
            o.write('%s slots=1 \n' % (node.name))
            threads.append(node.threads-usedThreads)
        
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

def run(files, machinesfile, overheadcores, verbose, dothreads, useDifxDb):
	ok = True

        # check if host is an allowed headnode
	hostname = socket.gethostname()
	if not hostname in difxmachines.getHeadNodeNames():
		print 'ERROR: hostname is not an allowed headnode in the machines file : %s' % machinesfile
		exit(1)

	infile = files[0]

	basename = infile[0:-5]
	if basename + 'input' != infile:
		print 'expecting input file'
		exit(1)
				
        datastreams =  getVsnsFromInputFile(infile)

	if not uniqueVsns(datastreams):
		print 'ERROR: at least one duplicate VSN exists in %s !' % infile
		exit(1)
                
	results, conflicts, missing, notidle = getVsnsByMulticast(5, datastreams, verbose)

	if verbose > 0:
		print 'Found modules:'
		for r in results:
			print '  %-10s : %10s %10s   %s' % (r[0], r[1], r[2], r[3])

	if len(conflicts) > 0:
		ok = False
		print 'Module conflicts:'
		for c in conflicts:
			print '  %-10s : %10s %10s' % (c[0], c[1], c[2])
	
	if len(missing) > 0:
		ok = False
		print 'Missing modules:'
		for m in missing:

			slot = "unknown"
			if useDifxDb:
				child = subprocess.Popen(["getslot", m], stdout=subprocess.PIPE)
				(slot, stderr) = child.communicate()

			
			print '  %s (slot = %s )' % (m, strip(slot))

	if len(notidle) > 0:
		ok = False
		print 'Modules not ready:'
		for n in notidle:
			print '  %s' % n

	if not ok:
		return 1

        t = writemachines(basename, hostname, results, datastreams, overheadcores, verbose)
        
	if len(t) == 0:
		return 1

	if dothreads:
		writethreads(basename, t)

	return 0

def signalHandler(signal, frame):
        print 'You pressed Ctrl+C!'
        sys.exit(8)
        
class Datastream:
        """
        Storage class containing datastream description read from the input file
        NETWORK datastreams not yet supported
        """
        type = ""
        vsn = ""
        path = ""
        
if __name__ == "__main__":

	# catch ctrl+c
	signal.signal(signal.SIGINT, signalHandler)

	usage = getUsage()

	parser = OptionParser(version=version, usage=usage)
	parser.add_option("-v", "--verbose", action="count", dest="verbose", default=0, help="increase verbosity level");
	#parser.add_option("-o", "--overheadcores", dest="overheadcores", type="int", default=1, help="set overheadcores, default = 1");
	parser.add_option("-m", "--machines", dest="machinesfile", default="", help="use MACHINESFILE instead of $DIFX_MACHINES")
	parser.add_option("-n", "--nothreads", dest="dothreads", action="store_false", default=True, help="don't write a .threads file")
	parser.add_option("-d", "--difxdb", dest="usedifxdb", action="store_true", default=False, help="use difxdb to obtain data location")

	(options, args) = parser.parse_args()

	if len(args) == 0:
		parser.print_usage()
		sys.exit(1)

	#overheadcores = options.overheadcores
        overheadcores = 0
	verbose = options.verbose
	dothreads = options.dothreads
	useDifxDb = options.usedifxdb

	# assign the cluster definition file
	if len(options.machinesfile) == 0:
		try:
			machinesfile = environ['DIFX_MACHINES']
		except:
			print ('DIFX_MACHINES environment has to be set. Use -m option instead')
			sys.exit(1)
	else:
		machinesfile = options.machinesfile


	# check that cluster definition file exist
	if not isfile(machinesfile):
		sys.exit("Machines file not found: %s" % machinesfile)

	if getenv('DIFX_GROUP_ID'):
		umask(2)

	# list of input files to process
	files = args

	quit = False
	for f in files:
		if not isfile(f):
			print 'File %s not found' % f
			quit = True
	if quit:
		print 'genmachines quitting.'
		exit(1)

	if verbose > 0:
		print 'DIFX_MACHINES -> %s' % machinesfile
	
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
            print "ERROR: This version of genmachines requires a cluster defintion file of version > %s. Found version is: %s.%s" % (minMachinefileVersion, major,minor)
            exit(1)
        
	v = run(files, machinesfile, overheadcores, verbose, dothreads, useDifxDb)
	if v != 0:
		exit(v)
