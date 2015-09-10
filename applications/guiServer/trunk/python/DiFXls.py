import time
import socket
import DiFXControl

#===============================================================================
## Class used to do an "ls" on a path on the DiFX server.
#
#  This class can be used to generate the results of an "ls" command on the
#  DiFX server.  Standard path names (with wildcards) are accepted, along with
#  all standard "ls" arguments.
#
#  The class inherits the DiFXControl.Client class.  A new instance with no
#  arguments will create a new connection to the server.  If you already have
#  DiFXControl.Client instance it can be used in the instantiation method
#  (which should be more efficient).  This slightly odd structure also allows
#  the DiFXControl.Client class to have its own ls() method with low overhead.
#
#<!---======================================================================--->
class DiFXls( DiFXControl.Client ):

	def __init__( self, client = None ):
		if client == None:
			DiFXControl.Client.__init__( self )
			self._client = self
		else:
			self._client = client
		
	#<!------------------------------------------------------------------------>
	## Run an "ls" command on the server with the specified path.
	#
	#    @param path Full path that should be listed (as one would pass to "ls").
	#    @param args Standard "ls" arguments - optional.
	#
	#  This function returns either after the ls is complete or the "waitTime"
	#  passes.  Results will be passed to the lsCallback() method below.
	#<!------------------------------------------------------------------------>
	def ls( self, path, args = None ):
		if args == None:
			argStr = ""
		else:
			argStr = args
		self._client.lsComplete = False
		self._client.lsGetSize = True
		self._client.lsList = None
		channel = self._client.newChannel( self.lsCallback )
		self._client.fileOperation( channel, "ls", argStr, path, None )
		wait = self._client._waitTime
		while wait > 0.0 and not self._client.lsComplete:
			time.sleep( 0.01 )
			wait -= 0.01
		self._client.closeChannel( channel )
		return self._client.lsList
		
	#<!------------------------------------------------------------------------>
	## Callback function for an "ls" command.
	#
	#  Each line of data should be either
	#  an integer telling us the size of the next line, or a line of file listing
	#  data.  The former we only use to determine when the listing is done (it
	#  will be zero).
	#<!------------------------------------------------------------------------>
	def lsCallback( self, data ):
		if self._client.lsGetSize:
			if socket.ntohl( self._client.i.unpack( data[0:4] )[0] ) == 0:
				self._client.lsComplete = True
			self._client.lsGetSize = False
		else:
			if self._client.lsList == None:
				self._client.lsList = []
			self._client.lsList.append( data.strip() )
			self._client.lsGetSize = True
			

