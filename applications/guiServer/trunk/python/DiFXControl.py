import socket
import threading
import time
import select
import struct
import os
import xml.dom.minidom
#  Forward declaration of the Client class so the following imports don't gag.  
class Client:
	pass
import DiFXls
import DiFXJobStatus

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
## Hello, this is an overview!
#
#  The Client class makes a TCP connection to a server at a specified address and
#  port and provides methods for controlling and receiving feedback from DiFX
#  processes.
#
#  difx = DiFXControl.Client()
#  difx.connect( ( hostname (string), port (integer) ) )
#  difx.monitor()        #  start a thread for incoming messages
#
#<!---======================================================================--->
class Client:
	
	#  Failure types - returned by the failure callback.
	BROKEN_SOCKET                       = 1
	FAILED_CONNECTION                   = 2
	
	#  Packet types used in communication with the server.
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
	GET_JOB_STATUS                 = 24

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
		self.versionInfoCallbacks = []
		self.bytesReceived = 0
		self.versionPreference = None
		self.serverVersion = None
		self.difxBase = None
		self.serverUser = None
		self.availableVersion = []
		self.serverEnvironment = {}
		self._runFile = None
		self._channelData = False
		self._waitTime = 5.0
		self.channelPool = 12345
		self.channelCallbacks = {}
		
	#---------------------------------------------------------------------------
	#  Global wait time (in seconds) that the client will wait for commands to
	#  respond.  Defaults is 5.0 seconds.
	#---------------------------------------------------------------------------
	def waitTime( self, newVal ):
		_waitTime = newVal
		
	#---------------------------------------------------------------------------
	#  Generate a unique "channel" number for channeled communication and assign
	#  a callback function to it ("None" is permitted).  The "channel" number is
	#  a unique identifier such that the incoming channeled data tagged with it
	#  goes to the correct callback.
	#  Don't worry about confusing channel numbers between client connections -
	#  they only need to be unique within the client connection.
	#---------------------------------------------------------------------------
	def newChannel( self, callback ):
		++self.channelPool
		self.channelCallbacks[self.channelPool] = callback
		return self.channelPool
		
	#<!------------------------------------------------------------------------>
	## Utility function for interpreting the server packet protocol.
	#
	#    @param data A new piece of data to add to the packet.
	#    @param packet A (possibly partial) packet in the form of a tuple.
	#
	#  This is a self-contained function that operates only on data it is given.
	#  The data are interpreted as part of the server packet protocol where
	#  an integer packet ID is followed by an integer data length followed by
	#  a bunch of data when that length is zero.  A tuple consisting of the
	#  packet ID, the length, the data, and a boolean indicating whether the
	#  packet is complete is returned.  This tuple is the same used as an
	#  argument.
	#
	#  The use of the tuple (instead of class variables) eliminates problems
	#  where multiple operations step on each other when interpreting the
	#  protocol simultaneously.
	#<!------------------------------------------------------------------------>
	def packetProtocol( self, data, packet ):
		if packet == None or packet[3] == True:
			#  No existing packet data or last packet was completely parsed.
			packetId = None
			packetLen = None
			packetData = None
			packetComplete = False
		else:
			packetId = packet[0]
			packetLen = packet[1]
			packetData = packet[2]
			packetComplete = False
		#  Fill in the packet data based on what we should be expecting next.
		if packetId == None:
			packetId = socket.ntohl( self.i.unpack( data[0:4] )[0] )
		elif packetLen == None:
			packetLen = socket.ntohl( self.i.unpack( data[0:4] )[0] )
			if packetLen == 0:
				packetComplete = True
			else:
				packetData = ""
		else:
			packetData += data
			if len( packetData ) >= packetLen:
				packetComplete = True
		return ( packetId, packetLen, packetData, packetComplete )
		
	#---------------------------------------------------------------------------
	#  Cleans up a channel for channeled connections when it is no longer needed.
	#---------------------------------------------------------------------------
	def closeChannel( self, channelNum ):
		try:
			del self.channelCallbacks[channelNum]
		except KeyError:
			print "no such channel key: " + str( channelNum )
		
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
		port = int( port )
		connectInfo = ( host, port )
		try:
			self.sock.connect( connectInfo )
		except socket.error, ( value, message ):
			print "Could not connect socket (" + str( host ) + ", " + str( port ) + "):", message
			self.socketOK = False
			
	#---------------------------------------------------------------------------
	#  Create an instance of the monitor thread so we can use it to read
	#  packets, but don't start it - so its basically non-threaded.  This is
	#  useful if we have an external thread that does a select on the socket
	#  (which I have to do when using the fltk GUI stuff).
	#---------------------------------------------------------------------------
	def passiveMonitor( self ):
		self.monitorThread = MonitorThread( self.sock )
		self.monitorThread.setCallback( self.packetCallback )
		self.monitorThread.setFailCallback( self.failCallback )
		self.channelData()

	#---------------------------------------------------------------------------	
	#  Activate a thread that will monitor incoming data.
	#---------------------------------------------------------------------------	
	def monitor( self ):
		self.monitorThread = MonitorThread( self.sock )
		self.monitorThread.setCallback( self.packetCallback )
		self.monitorThread.setFailCallback( self.failCallback )
		self.monitorThread.start()
		self.channelData()

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
	#  Send a "command" packet containing the included XML packet data.
	#  This function composes the header and includes the data.  Formatted for
	#  readability (at the expense of sending some extra bytes).
	#---------------------------------------------------------------------------
	def sendCommandPacket( self, command, packetData ):
		totalPacketData = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
		totalPacketData += "<difxMessage>\n"
		totalPacketData += "	<header>\n"
		totalPacketData += "		<type>" + command + "</type>\n"
		totalPacketData += "	</header>\n"
		totalPacketData += "	<body>\n"
		totalPacketData += packetData
		totalPacketData += "	</body>\n"
		totalPacketData += "</difxMessage>"
		self.sendPacket( self.COMMAND_PACKET, totalPacketData )

	#---------------------------------------------------------------------------	
	#  Send a "relay packet" command.  This will trigger the difxServer to provide all
	#  DiFX UDP packet communications by relay.  This function can be used to turn
	#  relay on (by default) or off.
	#---------------------------------------------------------------------------	
	def relayPackets( self, on = True ):
		if on:
			self.sendPacket( self.RELAY_PACKET, self.i.pack( socket.htonl( 1 ) ) )
		else:
			self.sendPacket( self.RELAY_PACKET, self.i.pack( 0 ) )
			
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
		self.sendPacket( self.MESSAGE_SELECTION_PACKET, packetData )
		
	#---------------------------------------------------------------------------
	#  Request all data be "channeled" - that is, instead of opening new sockets
	#  for operation-specific communication, the server will channel all through
	#  the primary connection socket.  This changes the way the data are parsed
	#  on the client end.  This function can be used to turn channeling on or
	#  off.
	#
	#  Channeling is recommended, and may become the only communications method
	#  in the future.
	#---------------------------------------------------------------------------
	def channelData( self, on = True ):
		if on:
			self.sendPacket( self.CHANNEL_ALL_DATA_ON )
			self._channelData = True
		else:
			self.sendPacket( self.CHANNEL_ALL_DATA_OFF )
			self._channelData = False
			
	#---------------------------------------------------------------------------
	#  Send a "guiServer Version" request - the server will respond with a
	#  bunch of version information messages, including the GUISERVER_VERSION,
	#  DIFX_BASE, GUISERVER_USER, AVAILABLE_DIFX_VERSION, GUISERVER_ENVIRONMENT,
	#  and CHANNEL_ALL_DATA packet IDs.  The results of all of these are parsed
	#  locally.
	#---------------------------------------------------------------------------
	def versionRequest( self ):
		self.sendPacket( self.GUISERVER_VERSION )
		
	#---------------------------------------------------------------------------
	#  Send a "file operation" command.  There are a bunch of different file
	#  operations.  XML is formatted to be pretty on the other end (for
	#  diagnostic purposes).
	#---------------------------------------------------------------------------
	def fileOperation( self, channel, command, args, path, dataNode ):
		#  Compose an XML string holding the command.
		#  And send it.
		packetData = "		<port>" + str( channel ) + "</port>\n"
		packetData += "		<operation>" + command + "</operation>\n"
		if args != None:
			packetData += "		<arg>" + args + "</arg>\n"
		packetData += "		<path>" + path + "</path>\n"
		if dataNode != None:
			packetData += "		<dataNode>" + path + "</dataNode>\n"
		self.sendCommandPacket( "DifxFileOperation", packetData )
		
	#---------------------------------------------------------------------------	
	#  Callback function for the socket monitor.  Triggered when a new packet is
	#  received.  With the exception of the "version" information, which is all
	#  stored locally, packet IDs trigger unique callback functions.
	#---------------------------------------------------------------------------	
	def packetCallback( self, packetId, data ):
		if packetId == self.RELAY_PACKET:
			for cb in self.relayCallbacks:
				cb( data )
		elif packetId == self.GUISERVER_VERSION:
			self.serverVersion = data
		elif packetId == self.GUISERVER_USER:
			self.serverUser = data
		elif packetId == self.AVAILABLE_DIFX_VERSION:
			self.availableVersion.append( data )
		elif packetId == self.GUISERVER_ENVIRONMENT:
			#  Break environment variable definitions into their "name" and "value"
			#  components.
			if data.find( "=" ) >= 0:
				name = data[0:data.find( "=" )]
				self.serverEnvironment[name] = data[data.find( "=" ) + 1:]
			else:
				print "something wrong with environment defintion: " + str( data )
		elif packetId == self.CHANNEL_ALL_DATA:
			#  This is received at the end of a response to a version request.  We
			#  use it to indicate that the request has been completed.
			self.channelResponse = True
		elif packetId == self.CHANNEL_DATA:
			#  This, on the other hand, is genuine data, directed to a specific
			#  "channel port".
			self.consumeChannelData( data )
		for cb in self.allCallbacks:
			cb( packetId, data )

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
				
	#---------------------------------------------------------------------------
	#  Query the server for DiFX Versions available and either set the version
	#  preferred by this client to one specified as an argument, or use the
	#  version of the server if not specified.  This function has to sleep while
	#  waiting for all responses from the server.
	#
	#  The client's preferred version is something that should be set before any
	#  DiFX operations are run.
	#---------------------------------------------------------------------------
	def version( self, preference = None ):
		self.channelResponse = False
		self.versionRequest()
		#  Wait for the channel reponse to be set to True, indicating all version
		#  information has been downloaded.  A five second limit is put on this
		#  wait.
		wait = self._waitTime
		while wait > 0.0 and not self.channelResponse:
			time.sleep( 0.01 )
			wait -= 0.01
		#  Set the version preference if the version request worked, either to
		#  our preference if requested or "DIFX_DEVEL" if not.
		if self.channelResponse:
			if preference == None:
				preference = "DIFX_DEVEL"
			#  Is this available?
			try:
				self.availableVersion.index( preference )
				self.versionPreference = preference
				#  Try also setting the "generic run file" appropriate to this
				#  version.
				self.setVersionRunFile( self.versionPreference )
			except:
				pass
		else:
			print "SERVER DID NOT RESPOND!"
				
	#---------------------------------------------------------------------------
	#  Set the generic run file based on a version name.
	#---------------------------------------------------------------------------
	def setVersionRunFile( self, version ):
		#  See if this version is available, bail out if not.
		try:
			self.availableVersion.index( version )
			#  Compose a name for the generic run file, if we can do that.  Bail out
			#  if we fail at any point.
			base = self.getenv( "DIFX_BASE" )
			if base != None:
				self.runFile( base + "/bin/rungeneric." + version )
			else:
				print "setVersionRunFile: no DIFX_BASE environment variable defined."
		except:
			print "setVersionRunFile: requested version \"" + str( version ) + "\" is not available."
		
	#---------------------------------------------------------------------------
	#  Set the generic run file for all DiFX processes.  The generic run file is
	#  a script that runs a given executable, often after setting proper environment
	#  variables and other items.  The "difxbuild" process creates generic run files
	#  for each DiFX version they create - these are in $DIFX_BASE/bin/rungenerc.[VERSION].
	#  The file defined here will be used to run all DiFX operations.
	#---------------------------------------------------------------------------
	def runFile( self, newVal ):
		self._runFile = newVal
			
	#---------------------------------------------------------------------------
	#  Return the value of an environment variable as seen by the server.  If the
	#  variable does not exist, "None" is returned.
	#---------------------------------------------------------------------------
	def getenv( self, varName ):
		try:
			return self.serverEnvironment[varName]
		except:
			return None
			
	#<!------------------------------------------------------------------------>
	## Run an "ls" command on the server with the specified path.
	#
	#    @param path Full path that should be listed (as one would pass to "ls").
	#    @param args Standard "ls" arguments - optional.
	#
	#  This function returns either after the ls is complete or the "waitTime"
	#  passes.
	#<!------------------------------------------------------------------------>
	def ls( self, path, args = None ):
		return DiFXls.DiFXls(self).ls( path, args )
			
	#<!------------------------------------------------------------------------>
	## Get the "status" of a job or jobs using .input file path.
	#
	#    @param path Full path that should be listed (as one would pass to "ls").
	#    @param shortStatus True/False whether a "short" status (including
	#                       only the final job state) is requested.  Faster
	#                       and easier to deal with for large numbers of jobs!
	#
	#  This function returns either after the requested status information is
	#  returned or the "waitTime" passes.
	#<!------------------------------------------------------------------------>
	def jobStatus( self, path, shortStatus ):
		return DiFXJobStatus.DiFXJobStatus(self).jobStatus( path, shortStatus )
			
	#<!------------------------------------------------------------------------>
	## Run a "mkdir" command on the server with the specified path.
	#
	#    @param path Full path of the directory that should be created.
	#
	#  The server does not currently accept any arguments to mkdir unfortunately.
	#  Nor does it produce any direct feedback.  Error messages will be produced,
	#  but they have to be collected using RELAY_PACKETs.
	#
	#<!------------------------------------------------------------------------>
	def mkdir( self, path ):
		channel = self.newChannel( None )
		self.fileOperation( channel, "mkdir", None, path, None )
		self.closeChannel( channel )
		
	#---------------------------------------------------------------------------
	#  Run a "rmdir" command on the server with the specified path.  Similar
	#  to mkdir - no arguments, no feedback.
	#---------------------------------------------------------------------------
	def rmdir( self, path ):
		channel = self.newChannel( None )
		self.fileOperation( channel, "rmdir", None, path, None )
		self.closeChannel( channel )
		
	#---------------------------------------------------------------------------
	#  Respond to a single packet containing channeled data.  We parse out the
	#  "port" number to find an appropriate callback for the data.
	#---------------------------------------------------------------------------
	def consumeChannelData( self, data ):
		#  Split off the first 4 bytes, which should identify the "channel".
		channel = socket.ntohl( self.i.unpack( data[0:4] )[0] )
		newData = data[4:]
		#  Use the channel number to do the proper callback.
		try:
			if self.channelCallbacks[channel] != None:
				self.channelCallbacks[channel]( newData )
		except KeyError:
			print "effort to direct channeled data to channel number " + channel + " failed - no defined callback"

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
		
		
		
		
		
		
		
			
