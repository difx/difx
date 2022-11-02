#!/usr/bin/env python3
# Usage: mk5daemonproxy.py [-h]
'''
A utility that repeats the last slot statuses of Mark5/6 units that fell silent.

The main use case is SLURM host suspend/resume. When a playback unit is sent to
power saving mode by SLURM, this script automatically stands in for it, such
that 'genmachines' continues to be able to locate modules and SLURM wrappers
are able to wake the correct playback unit.

Note that the sending units i.e. their IP addresses must resolve to a host
name that contains "mark". Unresolved and non-"mark" hosts are ignored.
'''

import argparse
import datetime
import os
import re
import socket
import struct

author  = 'Jan Wagner'
version = '1.0.0'
verdate = '20200822'

defaultDifxMessagePort = 50200
defaultDifxMessageGroup = '224.2.2.1'
acceptedMessageTypes = ['mark5Status', 'mark6Status', 'mark6SlotStatus']

class MarkXMulticast:
	"""
	Send and receive Mark5/Mark6 multicast messages in DiFX multicast group
	"""

	def __init__(self, group, port):

		self.socktimeout_sec = 10
		self.localname = socket.gethostname()
		self.mcastaddr = str(group)
		self.mcastport = int(port)

		self.s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
		self.s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
		self.s.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 2)
		self.s.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_LOOP, 0)
		self.s.bind(('', self.mcastport))

		mreq = struct.pack("4sl", socket.inet_aton(self.mcastaddr), socket.INADDR_ANY)
		self.s.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
		self.s.settimeout(self.socktimeout_sec)


	def requestMSNs(self):

		cmdTemplate = '<?xml version="1.0" encoding="UTF-8"?>\n' \
			'<difxMessage>' \
			'<header>' \
			'<from>%s</from>' \
			'<to>%s</to>' \
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
			'</difxMessage>'

		for dst in ["mark5","mark6"]:
			cmd = cmdTemplate % (self.localname, dst, "getvsn")
			cmd = cmd.encode('utf-8')
			self.send(cmd)


	def send(self, message):

		self.s.sendto(message, (self.mcastaddr, self.mcastport))


	def receive(self):

		result = ()

		try:
			msg, addr = self.s.recvfrom(8000)
			sender = socket.gethostbyaddr(addr[0])[0].split('.')[0]
			result = (sender, msg)
		except socket.timeout:
			pass
		except socket.herror:
			print('Weird: cannot gethostbyaddr for %s' % addr[0])

		return result


class MessageStore:
	"""
	Internal storage of multicasted messages, with last-seen counter.
	proxydata{'hostname'} = { 'last_seen_counter':<int>, messages{'slot':'all|1|2|3|4|5|6'}=msg} }
	"""

	def __init__(self):

		# Arbitrary unit, depends on update interval used outside of this class
		self.inactivity_count_limit = 4

		self.proxydata = {} 
		self.regexpSlotStatus = re.compile("<slot>([0-9]+)<\/slot>")


	def _messageToSlotStr(self, msg):

		slotname = ''
		m = str(msg)

		if 'mark6SlotStatus' in m:
			slotname = self.regexpSlotStatus.search(m).group(1)
		elif 'mark6Status' in m:
			slotname = 'all'
		elif 'mark5Status' in m:
			slotname = 'all'
		else:
			print('MessageStore ERROR: unsupported type of message (%s)!' % (m))

		return slotname


	def update(self, hostname, message):
		'''Store a new message and reset the last-seen count of the associated host'''

		slotname = self._messageToSlotStr(message)

		if hostname not in self.proxydata:

			# register a new host
			self.proxydata[hostname] = { 'last_seen_counter':0, 'messages':{slotname:message} }

		else:

			# purge all old messages when an offline host comes back online
			if self.proxydata[hostname]['last_seen_counter'] >= self.inactivity_count_limit:
				self.proxydata[hostname]['messages'] = {}

			# refresh the host and message
			self.proxydata[hostname]['messages'][slotname] = message
			self.proxydata[hostname]['last_seen_counter'] = 0


	def increment(self):
		'''Increment the last-seen counts for all hosts'''

		for hostname in self.proxydata.keys():
			self.proxydata[hostname]['last_seen_counter'] += 1
		

	def getMessages(self, hostname):

		messages = []

		if hostname in self.proxydata:
			slots = self.proxydata[hostname]['messages'].keys()
			for slot in slots:
				messages += [self.proxydata[hostname]['messages'][slot]]

		return messages


	def getSlots(self, hostname):

		slots = []

		if hostname in self.proxydata:
			slots = self.proxydata[hostname]['messages'].keys()

		return slots


	def printHostStatuses(self):

		for host in self.proxydata.keys():
			hostmessages = self.getMessages(host)
			hostslots = self.getSlots(host)
			print("%s : last seen since %d ticks : %d, %s" % (host, self.proxydata[host]['last_seen_counter'], len(hostmessages), str(hostslots)))


	def getOnlineHosts(self):

		hosts = []

		for hostname in self.proxydata.keys():
			if self.proxydata[hostname]['last_seen_counter'] < self.inactivity_count_limit:
				hosts += [hostname]

		return hosts


	def getOfflineHosts(self):

		hosts = []

		for hostname in self.proxydata.keys():
			if self.proxydata[hostname]['last_seen_counter'] >= self.inactivity_count_limit:
				hosts += [hostname]

		return hosts



def mk5daemonproxy(mgroup, mport, update_interval_secs=5, verbose=0):

	tstart = datetime.datetime.utcnow()
	msgapi = MarkXMulticast(mgroup, mport)
	msgstore = MessageStore()

	msgapi.requestMSNs()

	tlast = tstart

	while True:

		# Grab a new multicast message and remember it
		(sender, msg) = msgapi.receive()	
		if ('mark' in sender) and any(msgtype in str(msg) for msgtype in acceptedMessageTypes):
			msgstore.update(sender, msg)

		# Periodically check for downed hosts and repeat their multicast messages
		tnow = datetime.datetime.utcnow()
		if (tnow - tlast).seconds >= update_interval_secs:

			tlast = tnow

			if verbose > 0:
				msgstore.printHostStatuses()

			msgstore.increment()
			online = msgstore.getOnlineHosts()
			offline = msgstore.getOfflineHosts()

			print('Time         : %d sec' % ( (tnow-tstart).seconds ))
			print('Online hosts : %s' % ( str(online) ))
			print('Offline hosts: %s' % ( str(offline) ))

			for host in offline:
				hostmessages = msgstore.getMessages(host)
				hostslots = msgstore.getSlots(host)
				print('   retransmitting on behalf of %s : %d, %s' % ( host, len(hostmessages), str(hostslots) ))
				for message in hostmessages:
					msgapi.send(message)

			print('-' * 60)


if __name__ == "__main__":

	epilog = '\n\nNote: %(prog)s respects the following environment variables:'
	epilog +=  '\nDIFX_GROUP: if not defined a default of %s will be used.' % defaultDifxMessageGroup
	epilog +=  '\nDIFX_PORT: if not defined a default of %s will be used.' % defaultDifxMessagePort

	parser = argparse.ArgumentParser(epilog=epilog, formatter_class=argparse.RawDescriptionHelpFormatter, description=__doc__)
	parser.add_argument("-v", "--verbose", action="count", dest="verbose", default=0, help="increase verbosity level");
        
	args = parser.parse_args()
	verbose = args.verbose

	group = os.getenv('DIFX_MESSAGE_GROUP')
	port = os.getenv('DIFX_MESSAGE_PORT')

	if not group:
		group = defaultDifxMessageGroup
	if not port:
		port = defaultDifxMessagePort

	mk5daemonproxy(group, port, verbose=verbose)
