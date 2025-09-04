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
import time

import systemd

author  = 'Jan Wagner'
version = '1.0.0'
verdate = '20221102'

defaultDifxMessagePort = 50201
defaultDifxMessageGroup = '224.2.2.1'
acceptedMessageTypes = ['mark5Status', 'mark6Status', 'mark6SlotStatus']


class MarkXMulticast:
	"""
	Send and receive Mark5/Mark6 multicast messages in DiFX multicast group
	"""

	def __init__(self, group, port):

		self.socktimeout_sec = 1
		self.localname = socket.gethostname()
		self.mcastaddr = str(group)
		self.mcastport = int(port)

		self.s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
		self.s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
		self.s.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, 2)
		self.s.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_LOOP, 1)
		self.s.bind(('', self.mcastport))

		mreq = struct.pack("4sl", socket.inet_aton(self.mcastaddr), socket.INADDR_ANY)
		self.s.setsockopt(socket.IPPROTO_IP, socket.IP_ADD_MEMBERSHIP, mreq)
		self.s.settimeout(self.socktimeout_sec)

		self.regexpMsgtype = re.compile("<type>(\w*)<\/type>")


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

		result = ('',b'')

		try:
			msg, addr = self.s.recvfrom(8000)
			sender = socket.gethostbyaddr(addr[0])[0].split('.')[0]
			result = (sender, msg)
		except socket.timeout:
			pass
		except socket.herror:
			print('Weird: cannot gethostbyaddr for %s' % addr[0])

		return result


	def getType(self, msg):

		type = ''
		mgrp = self.regexpMsgtype.search(str(msg))
		if mgrp:
			type = str(mgrp.group(1))

		return type



class MessageStore:
	"""
	Internal storage of multicasted messages, with last-seen timestamps.
	proxydata{'hostname'} = { 'last_seen_time':<int>, messages{'slot':'all|1|2|3|4|5|6'}=msg} }
	"""

	def __init__(self, timelimit_secs=30):

		# Arbitrary unit, depends on update interval used outside of this class
		self.inactivity_secs_limit = timelimit_secs

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

		tnow = int(time.time())
		slotname = self._messageToSlotStr(message)

		if hostname not in self.proxydata:

			# register a new host
			self.proxydata[hostname] = { 'last_seen_time':tnow, 'messages':{slotname:message} }

		else:

			# purge all old messages when an offline host comes back online
			if abs(tnow - self.proxydata[hostname]['last_seen_time']) >= self.inactivity_secs_limit:
				self.proxydata[hostname]['messages'] = {}

			# refresh the host and message
			self.proxydata[hostname]['messages'][slotname] = message
			self.proxydata[hostname]['last_seen_time'] = tnow


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

		tnow = int(time.time())

		for host in self.proxydata.keys():
			hostmessages = self.getMessages(host)
			hostslots = self.getSlots(host)
			print("%s : last seen %d sec ago : %d, %s" % (host, tnow - self.proxydata[host]['last_seen_time'], len(hostmessages), str(hostslots)))


	def getOnlineHosts(self):

		hosts = []
		tnow = int(time.time())

		for hostname in self.proxydata.keys():
			if abs(tnow - self.proxydata[hostname]['last_seen_time']) < self.inactivity_secs_limit:
				hosts += [hostname]

		return sorted(hosts)


	def getOfflineHosts(self):

		hosts = []
		tnow = int(time.time())

		for hostname in self.proxydata.keys():
			if abs(tnow - self.proxydata[hostname]['last_seen_time']) >= self.inactivity_secs_limit:
				hosts += [hostname]

		return sorted(hosts)


class Mark5DaemonProxy:

	def __init__(self, mgroup, mport, update_interval_secs=5, timelimit_secs=30, verbose=0, logfile=None, notify_systemd=False):

		self.msgapi = MarkXMulticast(mgroup, mport)
		self.msgstore = MessageStore(timelimit_secs)

		self.update_interval_secs = update_interval_secs
		self.verbose = verbose
		self.logfile = logfile

		if self.verbose:
			print ('Started repeater on %s port %d' % (str(mgroup), int(mport)))

		if notify_systemd:
			state = systemd.daemon.Notification.READY
			systemd.daemon.notify(state)



	def run(self):

		tstart = datetime.datetime.utcnow()

		self.msgapi.requestMSNs()

		tlast = tstart

		while True:

			# Grab a new multicast message (or time-out)
			(sender, msg) = self.msgapi.receive()
			msgtype = self.msgapi.getType(msg)
			msgstr = str(msg.decode('utf-8'))
			if ('mark' in sender) and any(msgtype in msgstr for msgtype in acceptedMessageTypes):
				self.msgstore.update(sender, msg)
				if self.verbose > 2:
					print ('RX: %s: %s' % (sender, msgtype))
			else:
				if self.verbose > 3:
					if sender:
						print ('DROP: %s: %s' % (sender, msgtype))
					else:
						print ('Rx TIMEOUT')

			# Time for a periodic re-check?
			tnow = datetime.datetime.utcnow()
			if (tnow - tlast).seconds < self.update_interval_secs:
				continue

			tlast = tnow
			online = self.msgstore.getOnlineHosts()
			offline = self.msgstore.getOfflineHosts()

			if verbose > 0:
				self.msgstore.printHostStatuses()

			print('Runtime  : %d sec' % ( (tnow-tstart).seconds ))
			print('Online   : %s' % ( str(online) ))
			print('Offline  : %s' % ( str(offline) ))

			# Repeat last multicast msgs on behalf of downed hosts
			for host in offline:
				hostmessages = self.msgstore.getMessages(host)
				hostslots = self.msgstore.getSlots(host)
				print('   retransmitting on behalf of %s : %d, %s' % ( host, len(hostmessages), str(hostslots) ))
				for message in hostmessages:
					if self.verbose > 2:
						print('TX: %s' % (message))
					self.msgapi.send(message)

			# Re-request MSNs from hosts that might have come back up
			self.msgapi.requestMSNs()

			print('-' * 60)


if __name__ == "__main__":

	epilog = '\n\nNote: %(prog)s respects the following environment variables:'
	epilog +=  '\nDIFX_GROUP: if not defined a default of %s will be used.' % defaultDifxMessageGroup
	epilog +=  '\nDIFX_PORT: if not defined a default of %s will be used.' % defaultDifxMessagePort

	parser = argparse.ArgumentParser(epilog=epilog, formatter_class=argparse.RawDescriptionHelpFormatter, description=__doc__)
	parser.add_argument("-d", "--systemd", action="store_true", help="notify Linux systemd upon start of the proxy");
	parser.add_argument("-v", "--verbose", action="count", dest="verbose", default=0, help="increase verbosity level");
        
	args = parser.parse_args()
	verbose = args.verbose

	group = os.getenv('DIFX_MESSAGE_GROUP')
	port = os.getenv('DIFX_MESSAGE_PORT')

	if not group:
		group = defaultDifxMessageGroup
	if not port:
		port = defaultDifxMessagePort

	m5proxy = Mark5DaemonProxy(group, port, verbose=verbose, notify_systemd=args.systemd)

	m5proxy.run()
