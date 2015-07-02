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

__author__="Helge Rottmann and Cormac Reynolds"

import os
import os.path
#from string import upper,strip
import re
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

	def __str__(self):
		"""
		Executed when the DifxMachines instance is printed
		"""
		result = ""
		for name, node in self.nodes.iteritems():
			result += node.__str__() + "\n"
		return result

	def _charRange(self, c1, c2):
		"""Generates the characters from `c1` to `c2`, inclusive."""
		if len(c1) > 1 or len(c2) > 1:
			raise Exception("Illegal letter range")
		for c in xrange(ord(c1), ord(c2)+1):
			yield chr(c)

	def getMk5Nodes(self):
		"""
		returns a list of all Mk5 nodes
		"""
		nodes = []
		for name, node in self.nodes.iteritems():
			if node.isMk5 == 1:
				nodes.append(node)
		return nodes
	
	def getMk5NodeNames(self):
		"""
		returns a list of all Mk5 node names
		"""
		nodes = []
		for name, node in self.nodes.iteritems():
			if node.isMk5 == 1:
				nodes.append(node.name)
		return nodes

	def getComputeNodes(self):
		"""
		Returns a list of nodes serving as compute nodes. 
		Compute nodes have a threads setting > 0
		"""
		nodes = []
		for name, node in self.nodes.iteritems():
			if node.threads > 0:
				nodes.append(node)
		return nodes
		
	def getStorageNodes(self):
		"""
		returns a list of all nodes having storage areas
		"""
		nodes = []
		for name, node in self.nodes.iteritems():
			if len(node.fileUrls) > 0:
				nodes.append(node)
		return nodes
	
	def getHeadNodes(self):
		"""
		returns a list of allowed headnodes
		"""
		nodes = []
		for name, node in self.nodes.iteritems():
			if node.isHeadnode == 1:
				nodes.append(node)
		return nodes
	
	def getHeadNodeNames(self):
		"""
		returns a list of allowed headnode names
		"""
		nodes = []
		for name, node in self.nodes.iteritems():
			if node.isHeadnode == 1:
				nodes.append(node.name)
		return nodes

	def getNetworkNodes(self):
		"""
		returns a list of all nodes serving as network nodes
		"""
		nodes = []
		for name, node in self.nodes.iteritems():
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

	def _parse(self):
		'''
		Parses the machinefile
		'''
	
		lineCount = 0

		reLine = re.compile("^\s*(.*?)\s*,\s*([0-2]?),\s*(\d{1,})\s*,?\s*(.*)")	
		reRange = re.compile("(.+)\[(.*)\-(.*)\]")
		reLetter = re.compile("[a-z]")		

		lines = open(self.machinefile).readlines()
		for line in lines:

			lineCount += 1	
			line = line.strip()

			# skip empty lines
			if len(line) == 0:
				continue

			# skip comments
			if line.strip().startswith('#'):
				continue


			#remove trailing comment
			#line = line[:line.find("#")]

			# look for version string
			if line.startswith("version"):
				verToks= line.split("=")
				if len(verToks) == 2:
					self.version = verToks[1].strip()
				continue

			result = reLine.match(line)
			if result is None:
				raise Exception ("Missformed line in the machine file (line : %s)" % lineCount)

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
				if self.nodes.has_key(node.name):
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
	isHeadnode = 0
	fileUrls = []
	networkUrls = []

	def __str__(self):
		result = "name=%s threads=%s isHeadnode=%s isMk5=%s fileUrls=%s networkUrls=%s" % (self.name, self.threads, self.isHeadnode, self.isMk5, self.fileUrls, self.networkUrls)
		return(result)
		
if __name__ == "__main__":
	# run python difxmachines <machinefile> to execute this test code
	
	if len(sys.argv) < 2:
		print "Give full path to machinefile as first argument"
		sys.exit(1)
	difxmachines = DifxMachines(sys.argv[1])

	print difxmachines
		
	print "------------\nCompute nodes:\n------------"
	for node in difxmachines.getComputeNodes():
		print node.name, node.threads
				
	print "------------\nMark5 nodes:\n------------"
	for node in difxmachines.getMk5Nodes():
		print node.name

	print "------------\nStorage nodes:\n------------"
	for node in difxmachines.getStorageNodes():
		print node.name

		print "------------\nHead nodes:\n------------"
	for node in difxmachines.getHeadNodes():
		print node.name
		
	print "------------\nNetwork nodes:\n------------"
	for node in difxmachines.getNetworkNodes():
		print node.name

	print "------------\nVersion:\n------------"
	print difxmachines.getVersion()
	

