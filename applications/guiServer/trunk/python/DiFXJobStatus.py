import time
import socket
import DiFXControl

#===============================================================================
## Class used to obtain the status of a job or jobs on the DiFX server..
#
#  Given a list of input files, the server will parse associated
#  .difxlog files (if there are any) to obtain status and job run history
#  information about each.
#
#  The class inherits the DiFXControl.Client class.  A new instance will
#  create a new connection to the server.  If you already have
#  DiFXControl.Client instance it can be used in the instantiation method
#  (which should be more efficient).  This slightly odd structure also allows
#  the DiFXControl.Client class to have its own jobStatus() method with low overhead.
#
#<!---======================================================================--->
class DiFXJobStatus( DiFXControl.Client ):

	def __init__( self, client = None ):
		if client == None:
			DiFXControl.Client.__init__( self )
			self._client = self
		else:
			self._client = client
		
	#<!----------------------------------------------------------------------->
	## Obtain the "status" of a job or list of jobs by input file name.
	#
	#    @param difx DiFXControl.Client instance.
	#    @param path Full path of the input file(s).
	#    @param shortStatus True/False whether a "short" status (including
	#                       only the final job state) is requested.  Faster
	#                       and easier to deal with for large numbers of jobs!
	#
	#<!----------------------------------------------------------------------->
	def jobStatus( self, path, shortStatus ):
		self.jobStatusComplete = False
		self.jobStatusList = None
		self.jobStatusListTime = None
		self.currentInputFile = None
		self.packetData = None
		channel = self._client.newChannel( self.jobStatusCallback )
		#  Build the "GET_JOB_STATUS" command.
		#  The "short" status tells us whether a short form of the status
		#  (consisting of only the final job state) is desired.
		if shortStatus:
			data = self._client.i.pack( socket.htonl( 1 ) )
		else:
			data = self._client.i.pack( socket.htonl( 0 ) )
		#  The size of the path specification, then the path.
		data += self._client.i.pack( socket.htonl( len( path ) ) )
		data += path
		#  Our IP address (length of string, then string itself).
		ipAddress = self._client.sock.getsockname()[0]
		data += self._client.i.pack( socket.htonl( len( ipAddress ) ) )
		data += ipAddress
		#  The communications port.
		data += self._client.i.pack( socket.htonl( channel ) )
		#  Send the command.
		self._client.sendPacket( self._client.GET_JOB_STATUS, data )
		#  Now we wait for the responses to pile in.
		wait = self._client._waitTime
		while wait > 0.0 and not self.jobStatusComplete:
			try:
				time.sleep( 0.01 )
			except KeyboardInterrupt:
				self.jobStatusComplete = True			
			wait -= 0.01
		self._client.closeChannel( channel )
		return ( self.jobStatusListTime, self.jobStatusList )
		
	GET_JOB_STATUS_TASK_ENDED_GRACEFULLY               = 101
	GET_JOB_STATUS_TASK_STARTED                        = 102
	GET_JOB_STATUS_INPUT_FILE_NAME                     = 104
	GET_JOB_STATUS_NO_DIFXLOG                          = 105
	GET_JOB_STATUS_STATUS                              = 106
	GET_JOB_STATUS_OPEN_ERROR                          = 107
	GET_JOB_STATUS_NO_STATUS                           = 108
	GET_JOB_STATUS_TASK_TERMINATED                     = 109
	GET_JOB_STATUS_TIME_STRING                         = 110
	GET_JOB_STATUS_FORK_FAILED                         = 111

	#<!------------------------------------------------------------------------>
	## Callback function for a "GET_JOB_STATUS" command response.
	#
	#  The job status procedure on the server produces a bunch of different
	#  responses that are indexed by a leading integer type.  The types are
	#  listed as constants above.
	#<!------------------------------------------------------------------------>
	def jobStatusCallback( self, data ):
		#  The data from this callback can consist of one of three components
		#  as part of the server packet protocol - these are an ID, a data length,
		#  and data itself.  The packetProtocol function gets this for us.
		self.packetData = self._client.packetProtocol( data, self.packetData )
		if self.packetData[3]: #  Indicates whether packet is complete
			#  Figure out what this message is from the packet ID
			packetId = self.packetData[0]
			if packetId == self.GET_JOB_STATUS_TASK_TERMINATED:
				self.jobStatusComplete = True
			elif packetId == self.GET_JOB_STATUS_TASK_ENDED_GRACEFULLY:
				self.jobStatusComplete = True
			elif packetId == self.GET_JOB_STATUS_TASK_STARTED:
				self.jobStatusList = []
			elif packetId == self.GET_JOB_STATUS_INPUT_FILE_NAME:
				if self.currentInputFile != None and not self.statusFound:
					self.jobStatusList.append( ( self.currentInputFile, ( "", "Unknown" ) ) )
				self.currentInputFile = self.packetData[2]
				self.statusFound = False
			elif packetId == self.GET_JOB_STATUS_NO_DIFXLOG:
				self.jobStatusList.append( ( self.currentInputFile, ( "", "No .difxlog" ) ) )
				self.statusFound = True
			elif packetId == self.GET_JOB_STATUS_STATUS:
				#  The first 25 characters of the job status are the time stamp.
				print self.packetData[2][:25] + "     " + self.packetData[2][25:]
				self.jobStatusList.append( ( self.currentInputFile, ( self.packetData[2][:24], self.packetData[2][24:] ) ) )
				self.statusFound = True
			elif packetId == self.GET_JOB_STATUS_OPEN_ERROR:
				self.jobStatusList.append( ( self.currentInputFile, ( "", ".difxlog open error" ) ) )
				self.statusFound = True
			elif packetId == self.GET_JOB_STATUS_NO_STATUS:
				self.jobStatusList.append( ( self.currentInputFile, ( "", "No status in .difxlog" ) ) )
				self.statusFound = True
			elif packetId == self.GET_JOB_STATUS_FORK_FAILED:
				print "server fork failure"
			elif packetId == self.GET_JOB_STATUS_TIME_STRING:
				self.jobStatusListTime = self.packetData[2]

