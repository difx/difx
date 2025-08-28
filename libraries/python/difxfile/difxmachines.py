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
# $Id: difxmachines.py 10988 2023-06-19 06:30:36Z JanWagner $
# $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/libraries/python/difxfile/difxmachines.py $
# $LastChangedRevision: 10988 $
# $Author: JanWagner $
# $LastChangedDate: 2023-06-19 14:30:36 +0800 (一, 2023-06-19) $
#
#============================================================================

__author__="Helge Rottmann and Cormac Reynolds"

import os
import os.path
#from string import upper,strip
import re
import subprocess
import sys

class DifxMachines(object):
	"""
	class for parsing the DiFX cluster definition file
	"""

	def __init__(self, machinefile):
		""" 
		Constructor. Checks that machinefile exists. If not, an IOError is raised.
		"""

		self.machinefile = machinefile
		self.version = ""
		self.nodes = {}

		if (not os.path.isfile(self.machinefile)):
			raise IOError("DiFX machinefile: %s does not exist. " % self.machinefile)

		self._parse()

		self.slurmConf = self.SlurmMachines(self.slurm_config)

		if self.slurmConf.isValid():
			for nodename in self.nodes.keys():
				m = self.slurmConf.getNodeMaxProcesses(nodename)
				if m > 0:
					self.nodes[nodename].isInSlurm = True
					self.nodes[nodename].slurm_maxProc = m
					self.nodes[nodename].slurm_numProc = 0
					nstate = self.slurmConf.getNodeState(nodename)
					if 'CPUAlloc' in nstate:
						self.nodes[nodename].slurm_numProc = int(nstate['CPUAlloc'])/int(nstate['ThreadsPerCore'])


	def __str__(self):
		"""
		Executed when the DifxMachines instance is printed
		"""
		result = ""
		for name, node in self.nodes.items():
			result += node.__str__() + "\n"
		return result

	def _charRange(self, c1, c2):
		"""Generates the characters from `c1` to `c2`, inclusive."""
		if len(c1) > 1 or len(c2) > 1:
			raise Exception("Illegal letter range")
		for c in range(ord(c1), ord(c2)+1):
			yield chr(c)

	def getMk6Nodes(self):
		"""
		returns a list of all Mk6 nodes
		"""
		nodes = []
		for name, node in self.nodes.items():
			if node.isMk6 == 1:
				nodes.append(node)
		return nodes

	def getMk6NodeNames(self):
		"""
		returns a list of all Mk6 node names
		"""
		nodes = []
		for name, node in self.nodes.items():
			if node.isMk6 == 1:
				nodes.append(node.name)
		return nodes
	
	def getMk5Nodes(self):
		"""
		returns a list of all Mk5 nodes
		"""
		nodes = []
		for name, node in self.nodes.items():
			if node.isMk5 == 1:
				nodes.append(node)
		return nodes
	
	def getMk5NodeNames(self):
		"""
		returns a list of all Mk5 node names
		"""
		nodes = []
		for name, node in self.nodes.items():
			if node.isMk5 == 1:
				nodes.append(node.name)
		return nodes

	def getComputeNodes(self):
		"""
		Returns a list of nodes serving as compute nodes. 
		Compute nodes have a threads setting > 0
		"""
		nodes = []
		for name, node in self.nodes.items():
			if node.threads > 0:
				nodes.append(node)
		return nodes
		
	def getStorageNodes(self):
		"""
		returns a list of all nodes having storage areas
		"""
		nodes = []
		for name, node in self.nodes.items():
			if len(node.fileUrls) > 0:
				nodes.append(node)
		return nodes
	
	def getHeadNodes(self):
		"""
		returns a list of allowed headnodes
		"""
		nodes = []
		for name, node in self.nodes.items():
			if node.isHeadnode == 1:
				nodes.append(node)
		return nodes
	
	def getHeadNodeNames(self):
		"""
		returns a list of allowed headnode names
		"""
		nodes = []
		for name, node in self.nodes.items():
			if node.isHeadnode == 1:
				nodes.append(node.name)
		return nodes

	def getNetworkNodes(self):
		"""
		returns a list of all nodes serving as network nodes
		"""
		nodes = []
		for name, node in self.nodes.items():
			if len(node.networkUrls) > 0:
				nodes.append(node)
		return nodes

	def getVersion(self):
		"""
		returns a tuple containing the major and minor version of the parsed cluster definition file
		"""
		# Note: this code handles a 2-part version code despite the general consensus being that just 
		# a single integer would do and would be simpler.  Here the version is augmented with ".0" if
		# it is just an integer to allow the rest of the 2-part version to be handled with no change.
		versionParts = self.version.split(".")
		if len(versionParts) < 2:
			versionParts.append('0')
		return versionParts


	### Nested classes

	class SlurmMachines:
		"""
		nested class in DifxMachines that parses a SLURM config file and follows 'include' statements,
		with accessor funcs for by-name look up of details for a DifxMachines node
		"""

		def __init__(self, slurm_conf = '/etc/slurm/slurm.conf'):
			"""
			Constructor. Loads all node definitions from a SLURM config file, if it exists.
			"""
			txtNodeDefs = self._readSlurmConfNodes(slurm_conf)
			self.nodeDefs = [self._parseNodeDef(nodedef) for nodedef in txtNodeDefs]

		def _readSlurmConfNodes(self, slurm_conf):
			"""recursively reads NodeName lines from slurm.conf and any included slurm.d/* conf files"""
			node_lines = []
			if os.path.isfile(slurm_conf):
				fd = open(slurm_conf, 'r')
				for line in fd.readlines():
					line = line.strip()
					if len(line) < 1 or line[0] == '#':
						continue
					if line.startswith("NodeName"):
						node_lines.append(line)
					elif line.startswith("include "):
						inc_file = line[8:].strip()
						inc_nodes = self._readSlurmConfNodes(inc_file)
						node_lines += inc_nodes
				fd.close()
			return node_lines


		def _parseNodeDef(self, namedef):
			"""parse a SLURM NodeName line and split it into key-value pairs"""
			# Name def example: 'NodeName=mark6-01,mark6-02 CPUs=12 Sockets=1 CoresPerSocket=6 ThreadsPerCore=2 Feature=mark6 State=UNKNOWN'
			items = namedef.split()
			keys = [item.split('=')[0] for item in items]
			values = [item.split('=')[1] for item in items]
			data = dict(zip(keys,values))

			if 'NodeName' in data:
				if '[' in data['NodeName']:
					print("TODO: node name definition '%s' would apparently need expansion, currently not implemented in script." % (data['NodeName']))
				if ',' in data['NodeName']:
					data['NodeName'] = data['NodeName'].split(',')

			return data

		def getNodeState(self, difxnodename):
			"""Return some of 'scontrol show node <difxnodename>' fields as a dictionary"""
			values = {}
			try:
				out = subprocess.check_output(['scontrol', 'show', 'node', difxnodename])
				out = out.decode('utf8')
				for line in out.split('\n'):
					if 'CPUAlloc' in line or 'Threads' in line or 'AllocMem' in line or 'ThreadsPerCore' in line:
						d = dict(x.split('=') for x in line.strip().split(' '))
						values.update(d)
			except Exception as e:
				pass
			return values

		def isValid(self):
			return len(self.nodeDefs) > 0


		def getNodeMaxProcesses(self, difxnodename):
			"""return the maximum nr of processes the SLURM config permits to be launched on the given node"""
			m = -1
			for nodedef in self.nodeDefs:
				if difxnodename in nodedef['NodeName']:
					m = int(nodedef['CPUs']) / int(nodedef['ThreadsPerCore'])
			return m				



	def _parse(self):
		'''
		Parses the machinefile
		'''
	
		lineCount = 0

		reLine = re.compile("^\s*(.*?)\s*,\s*([0-2]?)\s*,\s*(\d{1,})\s*,?\s*(.*)")	
		reRange = re.compile("(.+)\[(.*)\-(.*)\]")
		reLetter = re.compile("[a-z]")		

		lines = open(self.machinefile).readlines()
		for line in lines:

			lineCount += 1	
			line = line.strip()

			# remove single line comments and trailing comments
			if line.find('#') >= 0:
				line = line[:line.find("#")]
				line = line.strip()

			# skip empty lines
			if len(line) == 0:
				continue

			# look for version string
			if line.startswith("version"):
				verToks= line.split("=")
				if len(verToks) == 2:
					self.version = verToks[1].strip()
				continue

			result = reLine.match(line)
			if result is None:
				raise Exception ("Misformed line in the machine file (line : %s : '%s')" % (lineCount,line))

			nodeNames = []
			# check if name field contains a range
			sub = reRange.match(result.group(1))
			if (sub):
				start = sub.group(2)
				stop = sub.group(3)

				if  not start.isdigit() or  not stop.isdigit():
					raise Exception("Illegal range specified in line %s of the machine file" % lineCount)

				if int(start) > int(stop):
					raise Exception("Illegal range specified in line %s of the machine file" % lineCount)

				try:
					for nodeNum in range(int(start), int(stop)+1):
						nodeNames.append(sub.group(1) + str(nodeNum).zfill(len(stop)))

				except:
					raise Exception("Illegal range specified in line %s of the machine file" % lineCount)
			else:
				nodeNames.append(result.group(1))	

			for nodeName in nodeNames:
				# check if node exists already
				if nodeName in self.nodes:
					node = self.nodes[nodeName]
				else:
					node = Node()
					node.name = nodeName
				node.threads = int(result.group(3).strip())

				fileUrls = []
				networkUrls = []

				# check for headnode
				if int(result.group(2).strip()) == 2:
					node.isHeadnode = 1

				if len(result.groups()) == 4:

					for url in result.group(4).split():
						if url.strip().startswith("mark5://"):
							node.isMk5 = 1
						elif url.strip().startswith("mark6://"):
							node.isMk6 = 1
						elif  url.strip().startswith("file://"):
							# add trailing "/" (if not present)
							if url.strip().endswith("/"):
								fileUrls.append(url.strip()[7:])
							else:
								fileUrls.append(url.strip()[7:] + "/")
						elif url.strip().startswith("network://"):
							networkUrls.append(url.strip()[10:])

				node.fileUrls = fileUrls
				node.networkUrls = networkUrls

				self.nodes[nodeName] = node

				# remove previous node entry if a later one is found
				if node.name in self.nodes:
					del self.nodes[node.name]
				# add node if enabled
				if int(result.group(2).strip()) > 0:
					self.nodes[node.name] = node

		# check that version string was properly set in the cluster definition file
		if len(self.version) == 0:
			sys.exit("ERROR: Missing or malformed version statement in the machines file: %s" % self.machinefile)

class Node:
	"""
	Storage class representing a node found in the cluster definition file
	"""
	name = ""
	threads = 0
	isMk5 = 0
	isMk6 = 0
	isHeadnode = 0
	fileUrls = []
	networkUrls = []

	def __str__(self):
		result = "name=%s threads=%s isHeadnode=%s isMk5=%s isMk6=%s fileUrls=%s networkUrls=%s" % (self.name, self.threads, self.isHeadnode, self.isMk5, self.isMk6, self.fileUrls, self.networkUrls)
		return(result)
		
if __name__ == "__main__":
	# run python difxmachines <machinefile> to execute this test code
	
	if len(sys.argv) < 2:
		print("Give full path to machinefile as first argument")
		sys.exit(1)
	difxmachines = DifxMachines(sys.argv[1])

	print(difxmachines)
		
	print("------------\nCompute nodes:\n------------")
	for node in difxmachines.getComputeNodes():
		print(node.name, node.threads)
				
	print("------------\nMark5 nodes:\n------------")
	for node in difxmachines.getMk5Nodes():
		print(node.name)

	print("------------\nMark6 nodes:\n------------")
	for node in difxmachines.getMk6Nodes():
		print(node.name)


	print("------------\nStorage nodes:\n------------")
	for node in difxmachines.getStorageNodes():
		print(node.name)

	print("------------\nHead nodes:\n------------")
	for node in difxmachines.getHeadNodes():
		print(node.name)
		
	print("------------\nNetwork nodes:\n------------")
	for node in difxmachines.getNetworkNodes():
		print(node.name)

	print("------------\nVersion:\n------------")
	print(difxmachines.getVersion())
	

