import time
import socket
import DiFXControl

#<!---======================================================================--->
## Class used to send and receive files to/from the DiFX Server
#
#  This class can be used to perform data transfers to and from files on the
#  DiFX server.  There are basically two things you can do - send data to files,
#  and grab data from files.  In both cases you need to provide full path
#  names.  A sample use of the class:
#
#  \code{.py}
#  #  Open a new instance of the file transfer class
#  difx = DiFXFileTransfer.Client()
#  difx.connect()
#  difx.monitor()
#
#  #  Create some data
#  theData = "this is some data"
#
#  #  Put it in a new file location on the server.  Always use the
#  #  full path!
#  difx.sendFile( "/tmp/newFile", theData )
#
#  #  Get the content of a file (in this case the same file)
#  junk = difx.getFile( "/tmp/newFile" )
#  print junk
#  \endcode
#
#  More complete code examples of the above procedures can be found in the
#  \ref difxgetfile "DiFXgetFile" and \ref difxsendfile "DiFXsendFile" example
#  applications.
#
#<!---======================================================================--->
class Client( DiFXControl.Client ):

	def __init__( self, client = None ):
		if client == None:
			DiFXControl.Client.__init__( self )
			self._client = self
		else:
			self._client = client
		
	#<!------------------------------------------------------------------------>
	## Construct an XML file transfer command and send it.
	#
	#  This is an internally-used method that builds the XML command out of
	#  the components necessary for a file transfer.  Many of these components
	#  are not used and are part of the XML for historical reasons.  All you
	#  really need are:
	#     <ul>
	#     <li><b>origin</b> - A full path of a file on the server (for "get" operations only)
	#     <li><b>destination</b> - A full path at which to write a file on the server (for "send" operations)
	#     <li><b>direction</b> - Either "from DiFX" or "to DiFX" for send or get operations respectively
	#     <li><b>port</b> - A "channel" number to route the return data.
	#     </ul>
	#
	#<!------------------------------------------------------------------------>
	def	sendFileTransfer( self, origin, destination, dataNode, address, direction, port ):
		packetData = ""
		if origin != None:
			packetData += "		<origin>" + origin + "</origin>\n"
		if destination != None:
			packetData += "		<destination>" + destination + "</destination>\n"
		if dataNode != None:
			packetData += "		<dataNode>" + dataNode + "</dataNone>\n"
		if address != None:
			packetData += "		<address>" + address + "</address>\n"
		packetData += "		<direction>" + direction + "</direction>\n"
		packetData += "		<port>" + str( port ) + "</port>\n"
		self._client.sendCommandPacket( "DifxFileTransfer", packetData )
		
	#<!------------------------------------------------------------------------>
	## Get the content of a file on the server.
	#
	#    @param path Full path of the file.
	#    @param waitToFinish True/False tells the method to wait for the operation
	#                        to complete and return the file content, or not wait and return
	#                        nothing.  True by default.
	#    @return The content of the file (as a string), or such fraction that has
	#            been collected before a timeout of a keyboard interrupt.
	#            Or if "waitToFinish" is False, nothing is returned.
	#
	#  This function triggers a file transfer from the DiFX server.  By default
	#  it will not return until the file transfer is complete, terminated by
	#  keyboard interrupt, or a timeout occurs.  In this case it will return the
	#  content of the file it has received.  It can also be used to simply
	#  trigger the file transfer and return immediately (in this case it will
	#  return nothing).
	#
	#  In either case, feedback to the calling program is provided by the
	#  getFileCallback() method.
	#<!----------------------------------------------------------------------->
	def getFile( self, path, waitToFinish = True ):
		self.fileSize = None
		self.fileData = ""
		self.fileComplete = False
		self.channel = self._client.newChannel( self.getFileCallback )
		#  Send the file transfer command with appropriate arguments.
		self.sendFileTransfer( path, None, None, None, "from DiFX", self.channel )
		#  Get out of here if waitToFinish is False.
		if waitToFinish == False:
			return
		self.wait = self._client._waitTime
		while self.wait > 0.0 and not self.fileComplete:
			try:
				time.sleep( 0.01 )
			except KeyboardInterrupt:
				self.fileComplete = True		
				self._client.closeChannel( self.channel )
			self.wait -= 0.01
			if self.wait == 0.0:
				self._client.doTimeoutCallback()
				self._client.closeChannel( self.channel )
		return self.fileData
			
	#<!------------------------------------------------------------------------>
	## Callback for a "from DiFX" file transfer request.
	#
	#  @param data String containing data, receipt of which triggered the
	#              callback.
	#
	#  This callback responds to received data from a file transfer request
	#  in the getFile() method.  It is triggered each time the server sends
	#  a data packet in response to this request.  These packets contain file
	#  data, which are tacked on to an accumulating "fileData" string.
	#
	#  The file size is known because it is sent as the first four bytes of
	#  data.  When data are received that add to the file content but do not
	#  complete it, the "interval" callback is called.  When the data are
	#  complete, the "final" callback is called.
	#
	#<!------------------------------------------------------------------------>
	def getFileCallback( self, data ):
		#  The first returned data is the file size
		if self.fileSize == None:
			self.fileSize = socket.ntohl( self._client.i.unpack( data[0:4] )[0] )
			#  Look for negative integers.
			if self.fileSize >= socket.ntohl( socket.htonl( ( -4 ) & 0xffffffff ) ):
				#  Some problem with the file...doesn't exist, can't be read...something
				self.fileData = None
				self.fileComplete = True
				self._client.closeChannel( self.channel )
			self._client.doIntervalCallback()
		#  Other returns are file data
		else:
			self.fileData += data
			#  When the file length has reached the size we expect, we are done.
			if len( self.fileData ) >= self.fileSize:
				self._client.doFinalCallback()
				self.fileComplete = True
				self._client.closeChannel( self.channel )
			else:
				self._client.doIntervalCallback()
			
	#<!------------------------------------------------------------------------>
	## Send data to a file destination on the server.
	#
	#    @param path Full path of the file.
	#    @param data String of data to put in the file.
	#    @param waitToFinish True/False tells the method to wait for the send
	#                        operation to complete and return the number of
	#                        bytes received, or not wait and return
	#                        nothing.  True by default.
	#    @return The number of bytes received by the server if "waitToFinish" is True,
	#            Or nothing if "waitToFinish" is False.
	#
	#  This function triggers a file transfer to the DiFX server.  By default
	#  it will not return until the file transfer is complete, terminated by
	#  keyboard interrupt, or a timeout occurs.  In these cases it will return
	#  the number of bytes received by the server (if known).  It can also be used to simply
	#  trigger the file transfer and return immediately (in this case it will
	#  return nothing).
	#
	#  The sendFileCallback() method is used to determine when the process is
	#  complete.
	#<!----------------------------------------------------------------------->
	def sendFile( self, path, data, waitToFinish = True ):
		self.fileSize = None
		self.fileComplete = False
		self.channel = self._client.newChannel( self.sendFileCallback )
		#  Send the file transfer command with appropriate arguments.
		self.sendFileTransfer( None, path, None, None, "to DiFX", self.channel )
		#  Send the size of the file and then the data.
		self._client.sendChannelData( self.channel, self._client.i.pack( socket.htonl( len( data ) ) ) )
		self._client.sendChannelData( self.channel, data )
		#  Get out of here if waitToFinish is False.
		if waitToFinish == False:
			return
		self.wait = self._client._waitTime
		while self.wait > 0.0 and not self.fileComplete:
			try:
				time.sleep( 0.01 )
			except KeyboardInterrupt:
				self.fileComplete = True		
				self._client.closeChannel( self.channel )
			self.wait -= 0.01
			if self.wait == 0.0:
				self._client.doTimeoutCallback()
				self._client.closeChannel( self.channel )
		return self.fileSize
			
	#<!------------------------------------------------------------------------>
	## Callback for a "to DiFX" file transfer request.
	#
	#  @param data String containing data, receipt of which triggered the
	#              callback.
	#
	#  This callback responds to received data from a file transfer request
	#  in the sendFile() method.  The only thing we get from the server in
	#  response to this instruction is the size of the received file.
	#
	#<!------------------------------------------------------------------------>
	def sendFileCallback( self, data ):
		#  The first returned data is the file size
		self.fileSize = socket.ntohl( self._client.i.unpack( data[0:4] )[0] )
		self._client.doFinalCallback()
		self.fileComplete = True
		self._client.closeChannel( self.channel )

