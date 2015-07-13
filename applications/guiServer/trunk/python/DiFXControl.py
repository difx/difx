import socket
import threading
import time
import select
import struct
import os
import xml.dom.minidom

RELAY_PACKET                   = 1
RELAY_COMMAND_PACKET           = 2
COMMAND_PACKET                 = 3
INFORMATION_PACKET             = 4
WARNING_PACKET                 = 5
ERROR_PACKET                   = 6
MULTICAST_SETTINGS_PACKET      = 7
GUISERVER_VERSION              = 8
GUISERVER_DIFX_VERSION         = 9
AVAILABLE_DIFX_VERSION         = 10
DIFX_BASE                      = 11
GUISERVER_ENVIRONMENT          = 12
DIFX_SETUP_PATH                = 13
START_DIFX_MONITOR             = 14
DIFX_RUN_LABEL                 = 15
GUISERVER_USER                 = 16
MESSAGE_SELECTION_PACKET       = 17
CHANNEL_ALL_DATA               = 18
CHANNEL_ALL_DATA_ON            = 19
CHANNEL_ALL_DATA_OFF           = 20
CHANNEL_CONNECTION             = 21
CHANNEL_DATA                   = 22
GENERATE_FILELIST              = 23

#===============================================================================
#  Thread to monitor a socket from the difxServer.  
#===============================================================================
class MonitorThread( threading.Thread ):
	
	def __init__( self, sock ):
		threading.Thread.__init__( self )
		self.sock = sock
		self.keepGoing = True
		self.i = struct.Struct( "I" )
		self.cb = None
		self.failCallback = None
		self.bytesReceived = 0
		self.ok = 1
		self.stopped = 2
		self.packetIDFail = 3
		self.packetLengthFail = 4
		self.packetDataFail = 5
		self.noConnection = 6
		self.status = self.ok;

	#---------------------------------------------------------------------------	
	#  Triggered by a "start()" function call.  This loop reads packets until
	#  something breaks.
	#---------------------------------------------------------------------------	
	def run( self ):
		self.status = self.ok
		while self.status == self.ok:
			self.newPacket()

	#---------------------------------------------------------------------------	
	#  Read a single data packet by interpreting the the low-level
	#  packet protocol of the difxServer - integer packetID, integer packet size,
	#  packet data following.  Some effort is made to detect broken sockets and
	#  identify at what stage of the process they were broken.
	#---------------------------------------------------------------------------	
	def newPacket( self ):
		iwtd, owtd, ewtd = select.select( [self.sock], [], [], .05 )
		if len( iwtd ) > 0:
			#  This should be a packet ID, or (possibly) an indication
			#  that the socket has crapped out.
			ind = self.sock.recv( 4 )
			if len( ind ) > 3:
				self.bytesReceived += 4
				packetId = socket.ntohl( self.i.unpack( ind )[0] )
				#  Then there is a data length
				ind = self.sock.recv( 4 )
				if len( ind ) > 3:
					self.bytesReceived += 4
					nBytes = socket.ntohl( self.i.unpack( ind )[0] )
					#  Then the data (if there are any)
					if nBytes > 0:
						data = ""
						while self.status == self.ok and len( data ) < nBytes:
							tmpBuf = self.sock.recv( nBytes - len( data ) )
							self.bytesReceived += len( tmpBuf )
							if not tmpBuf:
								self.status = self.packetDataFail
								print "Failure after reading", len( data ), "/", nBytes, "packet data"
								break
							data += tmpBuf
					else:
						data = None
					#  Trigger a callback with the packet ID and data.
					if self.cb != None:
						self.cb( packetId, data )
				else:
					self.status = self.packetLengthFail
					print "Failure to read data length"
			else:
				if self.bytesReceived > 0:
					self.status = self.packetIDFail
					print "Failure to read packetID"
				else:
					#  We failed on our first attempt to read data - assume the
					#  connection was no good to begin with.
					self.status = self.noConnection
			if self.status != self.ok:
				#  Socket failure.  Bail out, do the failure callback.
				if self.failCallback != None:
					self.failCallback()

	#---------------------------------------------------------------------------	
	#  Stops the thread.  The "sleep" is twice the timeout of the select call in
	#  the "run" portion of the thread.
	#---------------------------------------------------------------------------	
	def stop( self ):
		self.status = self.stopped
		time.sleep( 0.1 )
		
	#---------------------------------------------------------------------------	
	#  Set a callback for received packet data.
	#---------------------------------------------------------------------------	
	def setCallback( self, callbackFunction ):
		self.cb = callbackFunction
		
	#---------------------------------------------------------------------------	
	#  Set a callback for socket failures.  If you don't use this, the socket will
	#  simply stop on a failure.
	#---------------------------------------------------------------------------	
	def setFailCallback( self, callbackFunction ):
		self.failCallback = callbackFunction

#===============================================================================
#  The Client class makes a TCP connection to a difxServer on a specified address and
#  port.
#
#  difx = DiFXControl.Client()
#  difx.connect( ( hostname (string), port (integer) ) )
#  difx.monitor()        #  start a thread for incoming messages
#===============================================================================
class Client:
	
	#  Failure types - returned by the failure callback.
	BROKEN_SOCKET                       = 1
	FAILED_CONNECTION                   = 2
	 
	def __init__( self ):
		self.socketOK = True
		try:
			self.sock = socket.socket( socket.AF_INET, socket.SOCK_STREAM )
		except socket.error, ( value, message ):
			print "Could not open socket", message
			self.socketOK = False
		self.i = struct.Struct( "I" )
		self.monitorThread = None
		#  These are list of callback functions that can be set for each of the
		#  incoming packet types from the difxServer.
		self.allCallbacks = []
		self.relayCallbacks = []
		self.failCallbacks = []
		self.bytesReceived = 0
		
	#---------------------------------------------------------------------------	
	#  Functions for adding new callbacks for each packet type.  This is for
	#  all types.
	#---------------------------------------------------------------------------	
	def addAllCallback( self, newcb ):
		self.allCallbacks.append( newcb )
		
	#---------------------------------------------------------------------------
	#  Callback for relay messages - UDP broadcasts that are "relayed" by the
	#  difxServer.	
	#---------------------------------------------------------------------------	
	def addRelayCallback( self, newcb ):
		self.relayCallbacks.append( newcb )
		
	#---------------------------------------------------------------------------	
	#  Also add a callback for any sort of failure.
	#---------------------------------------------------------------------------	
	def addFailCallback( self, newcb ):
		self.failCallbacks.append( newcb )

	#---------------------------------------------------------------------------	
	#  Connect will attempt to connect to a host and port by constructing a list for the
	#  socket "connect()" function.  If not provided with a host and port, it will
	#  attempt to get them from environment variables, and, failing that, will use
	#  some defaults.
	#---------------------------------------------------------------------------	
	def connect( self, host = None, port = None ):
		if host == None:
			host = os.environ.get( "DIFX_CONTROL_HOST" )
		if port == None:
			port = os.environ.get( "DIFX_CONTROL_PORT" )
		if port == None:
			port = os.environ.get( "DIFX_MESSAGE_PORT" )
		if host == None:
			host = "localhost"
		if port == None:
			port = 50401
			connectInfo = ( host, port )
		try:
			self.sock.connect( connectInfo )
		except socket.error, ( value, message ):
			print "Could not connect socket:", message
			self.socketOK = False
			
	#---------------------------------------------------------------------------
	#  Create an instance of the monitor thread so we can use it to read
	#  packets, but don't start it - so its basically non-threaded.  This is
	#  useful if we have an external thread that does a select on the socket.
	#---------------------------------------------------------------------------
	def passiveMonitor( self ):
		self.monitorThread = MonitorThread( self.sock )
		self.monitorThread.setCallback( self.packetCallback )
		self.monitorThread.setFailCallback( self.failCallback )

	#---------------------------------------------------------------------------	
	#  Activate a thread that will monitor incoming data.
	#---------------------------------------------------------------------------	
	def monitor( self ):
		self.monitorThread = MonitorThread( self.sock )
		self.monitorThread.setCallback( self.packetCallback )
		self.monitorThread.setFailCallback( self.failCallback )
		self.monitorThread.start()

	#---------------------------------------------------------------------------	
	#  Close the socket.  If there is a monitoring thread, stop it first.
	#---------------------------------------------------------------------------	
	def close( self ):
		if self.monitorThread != None:
			self.monitorThread.stop()
			self.bytesReceived = self.monitorThread.bytesReceived
		if self.socketOK:
			self.sock.shutdown( socket.SHUT_RDWR )
		self.sock.close()

	#---------------------------------------------------------------------------	
	#  Send a packet along with associated data.  The difxServer packet protocol
	#  has an integer ID followed by an integer number of data bytes followed by
	#  data.  The packet IDs are listed all defined at the top of this file.
	#---------------------------------------------------------------------------	
	def sendPacket( self, packetId, data = "" ):
		self.sock.sendall( self.i.pack( socket.htonl( packetId ) ) )
		self.sock.sendall( self.i.pack( socket.htonl( len( data ) ) ) )
		self.sock.sendall( data )

	#---------------------------------------------------------------------------	
	#  Send a "relay packet" command.  This will trigger the difxServer to provide all
	#  DiFX UDP packet communications by relay.  This function can be used to turn
	#  relay on (by default) or off.
	#---------------------------------------------------------------------------	
	def relayPackets( self, on = True ):
		if on:
			self.sendPacket( RELAY_PACKET, self.i.pack( socket.htonl( 1 ) ) )
		else:
			self.sendPacket( RELAY_PACKET, self.i.pack( 0 ) )
			
	#---------------------------------------------------------------------------	
	#  Related to the above function - this function will specify a list of UDP
	#  packet types that should be relayed.  "messageTypes" is a list of legal
	#  type strings, which may include any of the following (each item has an
	#  integer alternative):
	#       1 : DifxLoadMessage
	#       2 : DifxAlertMessage
	#       3 : Mark5StatusMessage
	#       4 : DifxStatusMessage
	#       5 : DifxInfoMessage
	#       6 : DifxDatastreamMessage
	#       7 : DifxCommand
	#       8 : DifxParameter
	#       9 : DifxStart
	#      10 : DifxStop
	#      11 : Mark5VersionMessage
	#      12 : Mark5ConditionMessage
	#      13 : DifxTransientMessage
	#      14 : DifxSmartMessage
	#      15 : Mark5DriveStatsMessage
	#      16 : DifxDiagnosticMessage
	#      17 : DifxFileTransfer
	#      18 : DifxFileOperation
	#      19 : DifxVex2DifxRun
	#      20 : DifxMachinesDefinition
	#      21 : DifxGetDirectory
	#      22 : DifxMk5Control
	#      23 : DifxMark5Copy
	#---------------------------------------------------------------------------	
	def messageSelection( self, messageTypes = None ):
		packetData = ""
		if messageTypes == None:
			packetData = "ALL_MESSAGES"
		else:
			mTypes = [ "SELECTED_MESSAGES" ]
			for st in messageTypes:
				#  BLAT:  WE ARE CURRENTLY MISSING A BUNCH OF MESSAGE TYPES!
				#  Changes here are trivial, but they must be done in guiServer/difxServer
				#  as well.
				if st.strip().lower() in [ "0", "Unknown" ]:
					mTypes.append( "UNKNOWN_MESSAGES" )
				elif st.strip().lower() in [ "1", "DifxLoadMessage".lower() ]:
					mTypes.append( "LOAD_MESSAGES" )
				elif st.strip().lower() in [ "2", "DifxAlertMessage".lower() ]:
					mTypes.append( "ALERT_MESSAGES" )
				elif st.strip().lower() in [ "3", "Mark5StatusMessage".lower() ]:
					mTypes.append( "MARK5STATUS_MESSAGES" )
				elif st.strip().lower() in [ "4", "DifxStatusMessage".lower() ]:
					mTypes.append( "STATUS_MESSAGES" )
				elif st.strip().lower() in [ "5", "DifxInfoMessage".lower() ]:
					mTypes.append( "INFO_MESSAGES" )
				elif st.strip().lower() in [ "6", "DifxDatastreamMessage".lower() ]:
					#mTypes.append( "" )
					pass
				elif st.strip().lower() in [ "7", "DifxCommand".lower() ]:
					mTypes.append( "COMMAND_MESSAGES" )
				elif st.strip().lower() in [ "8", "DifxParameter".lower() ]:
					#mTypes.append( "" )
					pass
				elif st.strip().lower() in [ "9", "DifxStart".lower() ]:
					#mTypes.append( "" )
					pass
				elif st.strip().lower() in [ "10", "DifxStop".lower() ]:
					mTypes.append( "STOP_MESSAGES" )
				elif st.strip().lower() in [ "11", "Mark5VersionMessage".lower() ]:
					#mTypes.append( "" )
					pass
				elif st.strip().lower() in [ "12", "Mark5ConditionMessage".lower() ]:
					#mTypes.append( "" )
					pass
				elif st.strip().lower() in [ "13", "DifxTransientMessage".lower() ]:
					#mTypes.append( "" )
					pass
				elif st.strip().lower() in [ "14", "DifxSmartMessage".lower() ]:
					mTypes.append( "SMART_MESSAGES" )
				elif st.strip().lower() in [ "15", "Mark5DriveStatsMessage".lower() ]:
					#mTypes.append( "" )
					pass
				elif st.strip().lower() in [ "16", "DifxDiagnosticMessage".lower() ]:
					#mTypes.append( "" )
					pass
				elif st.strip().lower() in [ "17", "DifxFileTransfer".lower() ]:
					mTypes.append( "FILETRANSFER_MESSAGES" )
				elif st.strip().lower() in [ "18", "DifxFileOperation".lower() ]:
					mTypes.append( "FILEOPERATION_MESSAGES" )
				elif st.strip().lower() in [ "19", "DifxVex2DifxRun".lower() ]:
					mTypes.append( "VEX2DIFXRUN_MESSAGES" )
				elif st.strip().lower() in [ "20", "DifxMachinesDefinition".lower() ]:
					mTypes.append( "MACHINESDEFINITION_MESSAGES" )
				elif st.strip().lower() in [ "21", "DifxGetDirectory".lower() ]:
					mTypes.append( "GETDIRECTORY_MESSAGES" )
				elif st.strip().lower() in [ "22", "DifxMk5Control".lower() ]:
					mTypes.append( "MK5CONTROL_MESSAGES" )
				elif st.strip().lower() in [ "23", "DifxMark5Copy".lower() ]:
					mTypes.append( "MARK5COPY_MESSAGES" )
			#  Translaste the list into a string with newlines.
			packetData = "\n".join( mTypes )
		self.sendPacket( MESSAGE_SELECTION_PACKET, packetData )
		
	#---------------------------------------------------------------------------	
	#  Callback function for the socket monitor.  Triggered when a new packet is
	#  received.
	#---------------------------------------------------------------------------	
	def packetCallback( self, packetId, data ):
		if packetId == RELAY_PACKET:
			for cb in self.relayCallbacks:
				cb( data )
		for cb in self.allCallbacks:
			cb( data )

	#---------------------------------------------------------------------------	
	#  Callback function for failures in the socket monitor.
	#---------------------------------------------------------------------------	
	def failCallback( self ):
		for cb in self.failCallbacks:
			#  If no data were ever read, we assume this is caused by a socket
			#  that was bad from the start.
			if self.monitorThread.bytesReceived == 0:
				self.socketOK = False
				cb( self.FAILED_CONNECTION )
			else:
				cb( self.BROKEN_SOCKET )
				

#===============================================================================
#  Below are a series of classes for parsing and storing the information
#  contained in each DiFX Message type.
#
#  For all DiFX messages, items are stored as strings in variable who's names match
#  XML node names except where those names are Python reserved words ("from", "type",
#  etc.).  In those latter cases new names that are hopefully intuitive has been chosen.
#
#  Each class instance is created with a pointer to an XML string containing
#  the DiFX message.
#
#  First is the Generic DiFX XML message class.  This holds only the header 
#  information that is shared between all DiFX message types.
#===============================================================================
class XMLMessage:
	def __init__( self, data ):
		#  Send the data to the parser and collect the top-level node (there is
		#  only one).
		self.pdat = xml.dom.minidom.parseString( data )
		self.header = None
		self.body = None
		self.fromNode = None
		self.mpiProcessId = None
		self.identifier = None
		self.typeStr = None
		self.seqNumber = None
		if len( self.pdat.childNodes ) > 0:
			self.top = self.pdat.firstChild
			if len( self.top.childNodes ) > 1:
				#  Header information is constant among all DiFX messages.
				self.header = self.top.firstChild
				self.body = self.top.lastChild
				#  Get the known items from the header.
				for itm in self.header.childNodes:
					if itm.nodeName.lower() == "from" and itm.firstChild != None:
						self.fromNode = itm.firstChild.data
					elif itm.nodeName.lower() == "mpiprocessid" and itm.firstChild != None:
						self.mpiProcessId = itm.firstChild.data
					elif itm.nodeName.lower() == "identifier" and itm.firstChild != None:
						self.identifier = itm.firstChild.data
					elif itm.nodeName.lower() == "type" and itm.firstChild != None:
						self.typeStr = itm.firstChild.data
				for itm in self.body.childNodes:
					if itm.nodeName == "seqNumber" and itm.firstChild != None:
						self.seqNumber = itm.firstChild.data

#===============================================================================
#  The following are classes for each of the (known) DiFX message types.  The
#  class instance generates the generic XML message type (which contains the
#  header) and then parses out data specific to the message type.  Everything is
#  stored as strings.
#===============================================================================
class DifxLoadMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.cpuLoad = None
		self.totalMemory = None
		self.usedMemory = None
		self.netRXRate = None
		self.netTXRate = None
		self.nCore = None
		if self.body != None:
			#  Locate the "difxLoad" information (one child down)
			for block in self.body.childNodes:
				if block.nodeName == "difxLoad":
					#  Everything interesting is in it...
					for itm in block.childNodes:
						if itm.nodeName == "cpuLoad" and itm.firstChild != None:
							self.cpuLoad = itm.firstChild.data
						elif itm.nodeName == "totalMemory" and itm.firstChild != None:
							self.totalMemory = itm.firstChild.data
						elif itm.nodeName == "usedMemory" and itm.firstChild != None:
							self.usedMemory = itm.firstChild.data
						elif itm.nodeName == "netRXRate" and itm.firstChild != None:
							self.netRXRate = itm.firstChild.data
						elif itm.nodeName == "netTXRate" and itm.firstChild != None:
							self.netTXRate = itm.firstChild.data
						elif itm.nodeName == "nCore" and itm.firstChild != None:
							self.nCore = itm.firstChild.data
							
class Mark5StatusMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.inputFile = None
		self.bankAVSN = None
		self.bankBVSN = None
		self.statusWord = None
		self.activeBank = None
		self.state = None
		self.scanNumber = None
		self.scanName = None
		self.position = None
		self.playRate = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "mark5Status":
					for itm in block.childNodes:
						if itm.nodeName == "input" and itm.firstChild != None:
							self.inputFile = itm.firstChild.data
						elif itm.nodeName == "bankAVSN" and itm.firstChild != None:
							self.bankAVSN = itm.firstChild.data
						elif itm.nodeName == "bankBVSN" and itm.firstChild != None:
							self.bankBVSN = itm.firstChild.data
						elif itm.nodeName == "statusWord" and itm.firstChild != None:
							self.statusWord = itm.firstChild.data
						elif itm.nodeName == "activeBank" and itm.firstChild != None:
							self.activeBank = itm.firstChild.data
						elif itm.nodeName == "state" and itm.firstChild != None:
							self.state = itm.firstChild.data
						elif itm.nodeName == "scanNumber" and itm.firstChild != None:
							self.scanNumber = itm.firstChild.data
						elif itm.nodeName == "scanName" and itm.firstChild != None:
							self.scanName = itm.firstChild.data
						elif itm.nodeName == "position" and itm.firstChild != None:
							self.position = itm.firstChild.data
						elif itm.nodeName == "playRate" and itm.firstChild != None:
							self.playRate = itm.firstChild.data
							
class DifxAlertMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.inputFile = None
		self.alertMessage = None
		self.severity = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxAlert":
					for itm in block.childNodes:
						if itm.nodeName == "input" and itm.firstChild != None:
							self.inputFile = itm.firstChild.data
						elif itm.nodeName == "alertMessage" and itm.firstChild != None:
							self.alertMessage = itm.firstChild.data
						elif itm.nodeName == "severity" and itm.firstChild != None:
							self.severity = itm.firstChild.data

class DifxStatusMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.inputFile = None
		self.state = None
		self.message = None
		self.visibilityMJD = None
		self.jobstartMJD = None
		self.jobstopMJD = None
		self.weights = []
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxStatus":
					for itm in block.childNodes:
						if itm.nodeName == "input" and itm.firstChild != None:
							self.inputFile = itm.firstChild.data
						elif itm.nodeName == "state" and itm.firstChild != None:
							self.state = itm.firstChild.data
						elif itm.nodeName == "message" and itm.firstChild != None:
							self.message = itm.firstChild.data
						elif itm.nodeName == "visibilityMJD" and itm.firstChild != None:
							self.visibilityMJD = itm.firstChild.data
						elif itm.nodeName == "jobstartMJD" and itm.firstChild != None:
							self.jobstartMJD = itm.firstChild.data
						elif itm.nodeName == "jobstopMJD" and itm.firstChild != None:
							self.jobstopMJD = itm.firstChild.data
						elif itm.nodeName == "weight":
							self.weights.append( ( itm.getAttribute( "ant" ), itm.getAttribute( "wt" ) ) )

class DifxInfoMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.message = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxInfo":
					for itm in block.childNodes:
						if itm.nodeName == "message" and itm.firstChild != None:
							self.message = itm.firstChild.data

class DifxCommandMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.command = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxCommand":
					for itm in block.childNodes:
						if itm.nodeName == "command" and itm.firstChild != None:
							self.command = itm.firstChild.data

class DifxParameter( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.targetMpiId = None
		self.name = None
		self.value = None
		self.index = {}
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxParameter":
					for itm in block.childNodes:
						if itm.nodeName == "targetMpiId" and itm.firstChild != None:
							self.targetMpiId = itm.firstChild.data
						elif itm.nodeName == "name" and itm.firstChild != None:
							self.name = itm.firstChild.data
						elif itm.nodeName == "value" and itm.firstChild != None:
							self.value = itm.firstChild.data
						#  Parameters can have indices assigned to them...it looks like an arbitrary
						#  number of them, with arbitrary values.
						elif itm.nodeName[:5] == "index" and itm.firstChild != None:
							idx = int( itm.nodeName[5:] )
							self.index[idx] = itm.firstChild.data
							
class DifxStart( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.inputFile = None
		self.manager = None
		self.datastream = []
		self.process = []
		self.force = None
		self.difxVersion = None
		self.restartSeconds = None
		self.USNO = None
		self.address = None
		self.port = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxStart":
					for itm in block.childNodes:
						if itm.nodeName == "input" and itm.firstChild != None:
							self.inputFile = itm.firstChild.data
						elif itm.nodeName == "manager":
							self.manager = itm.getAttribute( "node" )
						elif itm.nodeName == "datastream":
							for node in itm.getAttribute( "nodes" ).split( " " ):
								self.datastream.append( node.strip() )
						elif itm.nodeName == "process" and itm.firstChild != None:
							self.process.append( ( itm.getAttribute( "nodes" ), itm.getAttribute( "threads" ) ) )
						elif itm.nodeName == "force" and itm.firstChild != None:
							self.force = itm.firstChild.data
						elif itm.nodeName == "difxVersion" and itm.firstChild != None:
							self.difxVersion = itm.firstChild.data
						elif itm.nodeName == "restartSeconds" and itm.firstChild != None:
							self.restartSeconds = itm.firstChild.data
						elif itm.nodeName == "USNO" and itm.firstChild != None:
							self.USNO = itm.firstChild.data
						elif itm.nodeName == "address" and itm.firstChild != None:
							self.address = itm.firstChild.data
						elif itm.nodeName == "port" and itm.firstChild != None:
							self.port = itm.firstChild.data

class DifxStop( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.inputFile = None
		self.mpiWrapper = None
		self.difxVersion = None
		self.difxProgram = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxStop":
					for itm in block.childNodes:
						if itm.nodeName == "input" and itm.firstChild != None:
							self.inputFile = itm.firstChild.data
						elif itm.nodeName == "mpiWrapper" and itm.firstChild != None:
							self.mpiWrapper = itm.firstChild.data
						elif itm.nodeName == "difxVersion" and itm.firstChild != None:
							self.difxVersion = itm.firstChild.data
						elif itm.nodeName == "difxProgram" and itm.firstChild != None:
							self.difxProgram = itm.firstChild.data

class DifxSmartMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.mjd = None
		self.vsn = None
		self.slot = None
		self.smarts = []
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxSmart":
					for itm in block.childNodes:
						if itm.nodeName == "mjd" and itm.firstChild != None:
							self.mjd = itm.firstChild.data
						elif itm.nodeName == "vsn" and itm.firstChild != None:
							self.vsn = itm.firstChild.data
						elif itm.nodeName == "slot" and itm.firstChild != None:
							self.slot = itm.firstChild.data
						elif itm.nodeName == "smart":
							self.smarts.append( ( itm.getAttribute( "id" ), itm.getAttribute( "value" ) ) )

class DifxDiagnosticMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.diagnosticType = None
		self.threadId = None
		self.numBufElements = None
		self.statBufElement = None
		self.activeBufElements = None
		self.microsec = None
		self.bytes = None
		self.bytespersec = None
		self.numSubintsLost = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxDiagnostic":
					for itm in block.childNodes:
						if itm.nodeName == "diagnosticType" and itm.firstChild != None:
							self.diagnosticType = itm.firstChild.data
						if self.diagnosticType == "MemoryUsage":
							if itm.nodeName == "bytes" and itm.firstChild != None:
								self.bytes = itm.firstChild.data
						elif self.diagnosticType == "BufferStatus":
							if itm.nodeName == "threadId" and itm.firstChild != None:
								self.threadId = itm.firstChild.data
							elif itm.nodeName == "numBufElements" and itm.firstChild != None:
								self.numBufElements = itm.firstChild.data
							elif itm.nodeName == "startBufElement" and itm.firstChild != None:
								self.startBufElement = itm.firstChild.data
							elif itm.nodeName == "activeBufElements" and itm.firstChild != None:
								self.activeBufElements = itm.firstChild.data
						elif self.diagnosticType == "ProcessingTime":
							if itm.nodeName == "threadId" and itm.firstChild != None:
								self.threadId = itm.firstChild.data
							elif itm.nodeName == "microsec" and itm.firstChild != None:
								self.microsec = itm.firstChild.data
						elif self.diagnosticType == "DataConsumed":
							if itm.nodeName == "bytes" and itm.firstChild != None:
								self.bytes = itm.firstChild.data
						elif self.diagnosticType == "InputDatarate":
							if itm.nodeName == "bytespersec" and itm.firstChild != None:
								self.bytespersec = itm.firstChild.data
						elif self.diagnosticType == "NumSubintsLost":
							if itm.nodeName == "numSubintsLost" and itm.firstChild != None:
								self.numSubintsLost = itm.firstChild.data

class DifxTransientMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.jobId = None
		self.startMJD = None
		self.stopMJD = None
		self.priority = None
		self.destDir = None
		self.comment = None
		self.dm = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxTransient":
					#  Everything interesting is in it...
					for itm in block.childNodes:
						if itm.nodeName == "jobId" and itm.firstChild != None:
							self.jobId = itm.firstChild.data
						elif itm.nodeName == "startMJD" and itm.firstChild != None:
							self.startMJD = itm.firstChild.data
						elif itm.nodeName == "stopMJD" and itm.firstChild != None:
							self.stopMJD = itm.firstChild.data
						elif itm.nodeName == "priority" and itm.firstChild != None:
							self.priority = itm.firstChild.data
						elif itm.nodeName == "destDir" and itm.firstChild != None:
							self.destDir = itm.firstChild.data
						elif itm.nodeName == "comment" and itm.firstChild != None:
							self.comment = itm.firstChild.data
						elif itm.nodeName == "dm" and itm.firstChild != None:
							self.dm = itm.firstChild.data

class Mark5VersionMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.daughterBoards = None
		self.PCBVer = None
		self.PCBType = None
		self.PCBSubType = None
		self.FPGAConfig = None
		self.FPGAConfigVer = None
		self.dbSerialNum = None
		self.NumChannels = None
		self.ApiVer = None
		self.ApiDate = None
		self.FirmVer = None
		self.MonVer = None
		self.XbarVer = None
		self.AtaVer = None
		self.UAtaVer = None
		self.DriverVer = None
		self.BoardType = None
		self.SerialNum = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "DaughterBoards":
					self.daughterBoards = True
					for itm in block.childNodes:
						if itm.nodeName == "PCBVer" and itm.firstChild != None:
							self.PCBVer = itm.firstChild.data
						elif itm.nodeName == "PCBType" and itm.firstChild != None:
							self.PCBType = itm.firstChild.data
						elif itm.nodeName == "PCBSubType" and itm.firstChild != None:
							self.PCBSubType = itm.firstChild.data
						elif itm.nodeName == "FPGAConfig" and itm.firstChild != None:
							self.FPGAConfig = itm.firstChild.data
						elif itm.nodeName == "FPGAConfigVer" and itm.firstChild != None:
							self.FPGAConfigVer = itm.firstChild.data
						elif itm.nodeName == "SerialNum" and itm.firstChild != None:
							self.dbSerialNum = itm.firstChild.data
						elif itm.nodeName == "NumChannels" and itm.firstChild != None:
							self.NumChannels = itm.firstChild.data
				if block.nodeName == "mark5Version":
					for itm in block.childNodes:
						if itm.nodeName == "ApiVer" and itm.firstChild != None:
							self.ApiVer = itm.firstChild.data
						elif itm.nodeName == "ApiDate" and itm.firstChild != None:
							self.ApiDate = itm.firstChild.data
						elif itm.nodeName == "FirmVer" and itm.firstChild != None:
							self.FirmVer = itm.firstChild.data
						elif itm.nodeName == "MonVer" and itm.firstChild != None:
							self.MonVer = itm.firstChild.data
						elif itm.nodeName == "XbarVer" and itm.firstChild != None:
							self.XbarVer = itm.firstChild.data
						elif itm.nodeName == "AtaVer" and itm.firstChild != None:
							self.AtaVer = itm.firstChild.data
						elif itm.nodeName == "UAtaVer" and itm.firstChild != None:
							self.UAtaVer = itm.firstChild.data
						elif itm.nodeName == "DriverVer" and itm.firstChild != None:
							self.DriverVer = itm.firstChild.data
						elif itm.nodeName == "BoardType" and itm.firstChild != None:
							self.BoardType = itm.firstChild.data
						elif itm.nodeName == "SerialNum" and itm.firstChild != None:
							self.SerialNum = itm.firstChild.data

class Mark5DriveStatsMessage( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.serialNumber = None
		self.modelNumber = None
		self.size = None
		self.moduleVSN = None
		self.moduleSlot = None
		self.startMJD = None
		self.stopMJD = None
		self.driveType = None
		self.startByte = None
		self.bins = {}
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxDriveStats":
					#  Everything interesting is in it...
					for itm in block.childNodes:
						if itm.nodeName == "serialNumber" and itm.firstChild != None:
							self.serialNumber = itm.firstChild.data
						elif itm.nodeName == "modelNumber" and itm.firstChild != None:
							self.modelNumber = itm.firstChild.data
						elif itm.nodeName == "size" and itm.firstChild != None:
							self.size = itm.firstChild.data
						elif itm.nodeName == "moduleVSN" and itm.firstChild != None:
							self.moduleVSN = itm.firstChild.data
						elif itm.nodeName == "moduleSlot" and itm.firstChild != None:
							self.moduleSlot = itm.firstChild.data
						elif itm.nodeName == "startMJD" and itm.firstChild != None:
							self.startMJD = itm.firstChild.data
						elif itm.nodeName == "stopMJD" and itm.firstChild != None:
							self.stopMJD = itm.firstChild.data
						elif itm.nodeName == "type" and itm.firstChild != None:
							self.driveType = itm.firstChild.data
						elif itm.nodeName == "startByte" and itm.firstChild != None:
							self.startByte = itm.firstChild.data
						elif itm.nodeName[:3] == "bin" and itm.firstChild != None:
							self.bins[int( itm.nodeName[3:] )] = itm.firstChild.data

class DifxFileTransfer( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.origin = None
		self.destination = None
		self.dataNode = None
		self.address = None
		self.direction = None
		self.port = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxFileTransfer":
					for itm in block.childNodes:
						if itm.nodeName == "origin" and itm.firstChild != None:
							self.origin = itm.firstChild.data
						elif itm.nodeName == "destination" and itm.firstChild != None:
							self.destination = itm.firstChild.data
						elif itm.nodeName == "dataNode" and itm.firstChild != None:
							self.dataNode = itm.firstChild.data
						elif itm.nodeName == "address" and itm.firstChild != None:
							self.address = itm.firstChild.data
						elif itm.nodeName == "direction" and itm.firstChild != None:
							self.direction = itm.firstChild.data
						elif itm.nodeName == "port" and itm.firstChild != None:
							self.port = itm.firstChild.data

class DifxFileOperation( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.path = None
		self.operation = None
		self.dataNode = None
		self.arg = None
		self.address = None
		self.port = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxFileOperation":
					for itm in block.childNodes:
						if itm.nodeName == "path" and itm.firstChild != None:
							self.path = itm.firstChild.data
						elif itm.nodeName == "operation" and itm.firstChild != None:
							self.operation = itm.firstChild.data
						elif itm.nodeName == "dataNode" and itm.firstChild != None:
							self.dataNode = itm.firstChild.data
						elif itm.nodeName == "arg" and itm.firstChild != None:
							self.arg = itm.firstChild.data
						elif itm.nodeName == "address" and itm.firstChild != None:
							self.address = itm.firstChild.data
						elif itm.nodeName == "port" and itm.firstChild != None:
							self.port = itm.firstChild.data

class DifxVex2DifxRun( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.user = None
		self.headNode = None
		self.difxVersion = None
		self.passPath = None
		self.v2dFile = None
		self.address = None
		self.port = None
		self.calcifOnly = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxVex2difxRun":
					for itm in block.childNodes:
						if itm.nodeName == "user" and itm.firstChild != None:
							self.user = itm.firstChild.data
						elif itm.nodeName == "headNode" and itm.firstChild != None:
							self.headNode = itm.firstChild.data
						elif itm.nodeName == "difxVersion" and itm.firstChild != None:
							self.difxVersion = itm.firstChild.data
						elif itm.nodeName == "passPath" and itm.firstChild != None:
							self.passPath = itm.firstChild.data
						elif itm.nodeName == "v2dFile" and itm.firstChild != None:
							self.v2dFile = itm.firstChild.data
						elif itm.nodeName == "address" and itm.firstChild != None:
							self.address = itm.firstChild.data
						elif itm.nodeName == "port" and itm.firstChild != None:
							self.port = itm.firstChild.data
						elif itm.nodeName == "calcifOnly" and itm.firstChild != None:
							self.calcifOnly = itm.firstChild.data

class DifxMachinesDefinition( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.inputFile = None
		self.manager = None
		self.datastream = []
		self.process = []
		self.difxVersion = None
		self.testProcessors = None
		self.machinesFile = None
		self.threadsFile = None
		self.address = None
		self.port = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxMachinesDefinition":
					for itm in block.childNodes:
						if itm.nodeName == "input" and itm.firstChild != None:
							self.inputFile = itm.firstChild.data
						elif itm.nodeName == "manager":
							self.manager = itm.getAttribute( "node" )
						elif itm.nodeName == "datastream":
							for node in itm.getAttribute( "nodes" ).split( " " ):
								self.datastream.append( node.strip() )
						elif itm.nodeName == "process" and itm.firstChild != None:
							self.process.append( ( itm.getAttribute( "nodes" ), itm.getAttribute( "threads" ) ) )
						elif itm.nodeName == "difxVersion" and itm.firstChild != None:
							self.difxVersion = itm.firstChild.data
						elif itm.nodeName == "testProcessors" and itm.firstChild != None:
							self.testProcessors = itm.firstChild.data
						elif itm.nodeName == "machinesFile" and itm.firstChild != None:
							self.machinesFile = itm.firstChild.data
						elif itm.nodeName == "threadsFile" and itm.firstChild != None:
							self.threadsFile = itm.firstChild.data
						elif itm.nodeName == "address" and itm.firstChild != None:
							self.address = itm.firstChild.data
						elif itm.nodeName == "port" and itm.firstChild != None:
							self.port = itm.firstChild.data

class DifxGetDirectory( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.difxVersion = None
		self.mark5 = None
		self.vsn = None
		self.address = None
		self.port = None
		self.generateNew = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxGetDirectory":
					for itm in block.childNodes:
						if itm.nodeName == "difxVersion" and itm.firstChild != None:
							self.difxVersion = itm.firstChild.data
						elif itm.nodeName == "mark5" and itm.firstChild != None:
							self.mark5 = itm.firstChild.data
						elif itm.nodeName == "vsn" and itm.firstChild != None:
							self.vsn = itm.firstChild.data
						elif itm.nodeName == "address" and itm.firstChild != None:
							self.address = itm.firstChild.data
						elif itm.nodeName == "port" and itm.firstChild != None:
							self.port = itm.firstChild.data
						elif itm.nodeName == "generateNew" and itm.firstChild != None:
							self.generateNew = itm.firstChild.data

class DifxMk5Control( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.command = None
		self.targetNode = None
		self.address = None
		self.port = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxMk5Control":
					for itm in block.childNodes:
						if itm.nodeName == "command" and itm.firstChild != None:
							self.command = itm.firstChild.data
						elif itm.nodeName == "targetNode" and itm.firstChild != None:
							self.targetNode = itm.firstChild.data
						elif itm.nodeName == "address" and itm.firstChild != None:
							self.address = itm.firstChild.data
						elif itm.nodeName == "port" and itm.firstChild != None:
							self.port = itm.firstChild.data

class DifxMark5Copy( XMLMessage ):
	def __init__( self, data ):
		XMLMessage.__init__( self, data )
		self.difxVersion = None
		self.mark5 = None
		self.vsn = None
		self.scans = None
		self.destination = None
		self.address = None
		self.port = None
		if self.body != None:
			for block in self.body.childNodes:
				if block.nodeName == "difxMark5Copy":
					for itm in block.childNodes:
						if itm.nodeName == "difxVersion" and itm.firstChild != None:
							self.difxVersion = itm.firstChild.data
						elif itm.nodeName == "mark5" and itm.firstChild != None:
							self.mark5 = itm.firstChild.data
						elif itm.nodeName == "vsn" and itm.firstChild != None:
							self.vsn = itm.firstChild.data
						elif itm.nodeName == "scans" and itm.firstChild != None:
							self.scans = itm.firstChild.data
						elif itm.nodeName == "destination" and itm.firstChild != None:
							self.destination = itm.firstChild.data
						elif itm.nodeName == "address" and itm.firstChild != None:
							self.address = itm.firstChild.data
						elif itm.nodeName == "port" and itm.firstChild != None:
							self.port = itm.firstChild.data

#===============================================================================
#  Function to parse a DiFX message given its XML data.  An instance of a
#  class appropriate to the message type is returned.
#===============================================================================
def parseXML( data ):
	#  Create a generic message type to extract the header information.
	xmlDat = XMLMessage( data )
	#  Create an instance that matches the message type.
	if xmlDat.typeStr == "DifxLoadMessage":
		return DifxLoadMessage( data )
	elif xmlDat.typeStr == "DifxAlertMessage":
		return DifxAlertMessage( data )
	elif xmlDat.typeStr == "Mark5StatusMessage":
		return Mark5StatusMessage( data )
	elif xmlDat.typeStr == "DifxStatusMessage":
		return DifxStatusMessage( data )
	elif xmlDat.typeStr == "DifxInfoMessage":
		return DifxInfoMessage( data )
	elif xmlDat.typeStr == "DifxDatastreamMessage":
		pass #  not defined currently (as far as I can tell)
	elif xmlDat.typeStr == "DifxCommand":
		return DifxCommand( data )
	elif xmlDat.typeStr == "DifxParameter":
		return DifxParameter( data )
	elif xmlDat.typeStr == "DifxStart":
		return DifxStart( data )
	elif xmlDat.typeStr == "DifxStop":
		return DifxStop( data )
	elif xmlDat.typeStr == "Mark5VersionMessage":
		return Mark5VersionMessage( data )
	elif xmlDat.typeStr == "Mark5ConditionMessage":
		pass #  deprecated message, replaced by Mark5DriveStatsMessage
	elif xmlDat.typeStr == "DifxTransientMessage":
		return DifxTransientMessage( data )
	elif xmlDat.typeStr == "DifxSmartMessage":
		return DifxSmartMessage( data )
	elif xmlDat.typeStr == "Mark5DriveStatsMessage":
		return Mark5DriveStatsMessage( data )
	elif xmlDat.typeStr == "DifxDiagnosticMessage":
		return DifxDiagnosticMessage( data )
	elif xmlDat.typeStr == "DifxFileTransfer":
		return DifxFileTransfer( data )
	elif xmlDat.typeStr == "DifxFileOperation":
		return DifxFileOperation( data )
	elif xmlDat.typeStr == "DifxVex2DifxRun":
		return DifxVex2DifxRun( data )
	elif xmlDat.typeStr == "DifxMachinesDefinition":
		return DifxMachinesDefinition( data )  
	elif xmlDat.typeStr == "DifxGetDirectory":
		return DifxGetDirectory( data )
	elif xmlDat.typeStr == "DifxMk5Control":
		return DifxMk5Control( data )
	elif xmlDat.typeStr == "DifxMark5Copy":
		return DifxMark5Copy( data )
	else:
		#  If we don't know what type of message this is, return the generic
		#  type.
		return xmlDat
		
		
		
		
		
		
		
			
