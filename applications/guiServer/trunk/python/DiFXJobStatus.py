import time
import socket
import DiFXControl

#<!---======================================================================--->
## Class used to obtain the status of a job or jobs on the DiFX server..
#
#  Given a list of input files, the server will parse associated
#  .difxlog files (if there are any) to obtain status and job run history
#  information about each.  A sample use is below (a more complete implementation
#  can be found in the \ref difxjobstatus "DiFXJobStatus" application):
#
#  \code{.py}
#  #  Create an instance of the JobStatus class
#  difx = DiFXJobStatus.Client()
#
#  #  Employ (inherited) DiFXControl.Client class methods to connect to the server
#  difx.connect( ( "localhost", 50401 ) )
#  difx.monitor()
#
#  #  Get the job status you want
#  difx = jobStatus.jobStatus( "/full/path/to/input/files*", True )
#
#  #  Report your results
#  ...
#  \endcode
#
#  The class inherits the DiFXControl.Client class.  A new instance will
#  create a new connection to the server.  If you already have
#  DiFXControl.Client instance it can be used in the instantiation method
#  (eliminating an inefficient duplicate instantiation).  This slightly odd structure also allows
#  the DiFXControl.Client class to have its own jobStatus() method with low overhead.
#
#<!---======================================================================--->
class Client( DiFXControl.Client ):

	def __init__( self, client = None ):
		if client == None:
			DiFXControl.Client.__init__( self )
			self._client = self
		else:
			self._client = client
		
	#<!----------------------------------------------------------------------->
	## Obtain the "status" of a job or list of jobs by input file name.
	#
	#    @param path Full path of the input file(s).
	#    @param shortStatus True/False whether a "short" status (including
	#                       only the final job state) is requested.  Faster
	#                       and easier to deal with for large numbers of jobs,
	#                       and currently the only thing implemented!
	#    @param waitToFinish True/False tells the method to wait for the job status
	#                        to be collected and return it, or not wait and return
	#                        nothing.  True by default.
	#    @return A tuple containing the time stamp on a job status (representing
	#            the last-most status received) and the status itself (complex - see below).
	#            Or if "waitToFinish" is False, nothing is returned.
	#
	#  This function sends a GET_JOB_STATUS request to the server and awaits
	#  a response (it gives up after the "wait time" - see DiFXControl.Client.waitTime()).
	#  The job status request includes an input file path (which may have
	#  standard "ls-compatible" wildcards, and thus may really be a list of files)
	#  that represents the job or jobs for which the status is desired.
	#
	#  Status is obtained from the <code>.difxlog</code> file associated with each <code>.input</code> file.
	#  The <code>.difxlog</code> file may not exist, in with case the status will
	#  be "No .difxlog".  If the file does exist, the status will be the last-most
	#  found in it - presumably the last thing that happened to the job.
	#
	#  The return structure for this function is kind of complicated - it returns
	#  a tuple containing the last time stamp encountered in any <code>.difxlog</code> file -
	#  this is used as a book-keeping time stamp for applications that are
	#  repeatedly querying status - and
	#  a list of real status information.  The list is a bunch of tuples itself,
	#  each containing an <code>.input</code> file name (there should be one
	#  for each <code>.input</code> file requested by the path argument) paired
	#  with yet another tuple that contains a status string and a time stamp string.
	#
	#  The \ref difxjobstatus "DiFXJobStatus" application is a good example of how to extract
	#  data from the return.
	#<!----------------------------------------------------------------------->
	def jobStatus( self, path, shortStatus, waitToFinish = True ):
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
		#  Now we wait for the responses to pile in, unless we've been
		#  instructed to bail out.
		if waitToFinish:
			wait = self._client._waitTime
			while wait > 0.0 and not self.jobStatusComplete:
				try:
					time.sleep( 0.01 )
				except KeyboardInterrupt:
					self.jobStatusComplete = True			
				wait -= 0.01
				if wait == 0.0:
					self._client.doTimeoutCallback()
			self._client.closeChannel( channel )
			return ( self.jobStatusListTime, self.jobStatusList )
		else:
			return
			
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
				self._client.doFinalCallback()
			elif packetId == self.GET_JOB_STATUS_TASK_ENDED_GRACEFULLY:
				self.jobStatusComplete = True
				self._client.doFinalCallback()
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

