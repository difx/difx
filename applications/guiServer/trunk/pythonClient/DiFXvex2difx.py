#\if DOXYGEN_IGNORE ############################################################
#                                                                              #
#   Copyright (C) 2016 by John Spitzak                                         #
#                                                                              #
#   This program is free software; you can redistribute it and/or modify       #
#   it under the terms of the GNU General Public License as published by       #
#   the Free Software Foundation; either version 3 of the License, or          #
#   (at your option) any later version.                                        #
#                                                                              #
#   This program is distributed in the hope that it will be useful,            #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#   GNU General Public License for more details.                               #
#                                                                              #
#   You should have received a copy of the GNU General Public License          #
#   along with this program; if not, write to the                              #
#   Free Software Foundation, Inc.,                                            #
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  #
#                                                                              #
#\endif ########################################################################
import time
import socket
import DiFXControl
#<!---======================================================================--->
## \brief Class used to run vex2difx and associated actions
#
#  This class provides a list of functions that can be used to run <i>vex2difx</i>
#  to create DiFX jobs from a .v2d file.  As part of this process it runs the
#  <i>calc</i> operation, alternatively it can be used to run <i>calc</i> on existing jobs.
#
#  Valid job creation requires a directory, writeable by the DiFX server, in which
#  jobs will be created, and a valid .v2d file and .vex file, both located in the
#  directory.  The .v2d file must contain a line:
#<pre>
#     vex = NAME.vex
#</pre>
#  where NAME is the name of the .vex file.
#
#  <h3>Connection to the Server</h3>
#
#  This class inherits the DiFXControl.Client class, and uses its methods for making
#  client connections.
#
#  \code{.py}
#  import DiFXvex2difx
#
#  #  Open a new instance of the DiFXvex2difx.Client class and connect to the server
#  difx = DiFXJobControl.Client()
#  difx.connect()
#  difx.monitor()
#
#  #  Do job creation stuff.
#  ...
#  difx.close()
#  \endcode
#
#  <h3>Required: Define the Directory and .v2d File Name</h3>
#
#  The directory in which .v2d and .vex files reside, and in which jobs will be created,
#  is defined using the passPath() method.  This is a required definition, and it should
#  include a complete path.  Also required
#  is the "name" of the .v2d file, defined using the v2dFile() method.  The name is the
#  file name, without the .v2d extension and without path information.  The .v2d file
#  name is <i>not</i> required if you are running the calc process <i>only</i>.
#
#  \code{.py}
#  difx.passPath( "/this/is/the/directory" )
#  difx.v2dFile( "name" )  #  name.v2d should be in the directory
#  \endcode
#
#  <h3>Optional Things - Running Calc</h3>
#
#  In "normal" operation both <i>vex2difx</i> and the calc process are run to create
#  jobs and prepare them for DiFX.  There are circumstances where you may wish to run
#  the calc process only on a group of existing jobs, either to recompute the calc
#  parameters or because the calc process failed for some reason.  The DiFXvex2difx.Client
#  can be made to do this by passing "True" to the calcOnly() method.  You can then
#  set a list of jobs on which calc should be run using the jobName() method.  This
#  method accepts ls-style wild cards.  If you are running calc alone, you do not need
#  to set the .v2d file using the v2dFile() method.
#
#  It is also possible that you would wish to specify the calc process.  By default,
#  the <i>calcif2</i> application is used to run calc, but other applications are in
#  development that may ultimately be better tuned to DiFX.  You can set the calc
#  process using the calcCommand() method.  Whatever process you set must be accessible
#  to the DiFX server, and must accept the "-f FILE" argument, where FILE is the name
#  of a single .calc file (these files are created by <i>vex2difx</i>).  The calc process
#  can be set whether you are running <i>vex2difx</i> and calc, or just calc alone.
#	
#  \code{.py}
#  #  You always need to set the path
#  difx.passPath( "/this/is/the/directory" )
#  #  We only want to run calc
#  difx.calcOnly( True )
#  #  But we don't like calcif2
#  difx.calcCommand( "myCalcProcess" )
#  #  And we want it run on all files that look like "job_0*.calc"
#  difx.v2dFile( "job_0*.calc" )
#  \endcode
#
#  <h3>Running and Callbacks</h3>
#
#  The <i>vex2difx</i> and calc processes are run using the runVex2Difx() method.  This
#  method can be set to return when everything is complete (the default), or to return
#  immediately and let things run in the background (using "False" as an argument).
#
#  The runVex2Difx() method runs silently in either case.  Feedback is available in
#  the form of two callbacks.  One is called each time a .input or .im file is
#  created (indicating the creation of a job and the running of the calc process on
#  it, respectively).  The callback function, which is defined by the user, must
#  accept a single argument (the path of the created file).  The second callback,
#  which produces no argument, is called when all processing is complete.  The callbacks
#  are assigned using the newFileCallback() and processCompleteCallback() functions.
#
#	\code{.py}
#	import DiFXvex2difx
#
#	vex2difxRunning = False
#
#	#  Define some callback functions.
#	def myNewFileCallback( newFile ):
#		print newFile + " was created"
#
#	def myProcessCompleteCallback():
#		vex2difxRunning = False
#
#	#  New client instance, connecting to the DiFX server in the usual manner
#	difx = DiFXvex2difx.Client()
#	difx.connect( ( "localhost", 50401 ) )
#	difx.monitor()
#
#	#  Set path and .v2d file name
#	difx.passPath( "/data/correlator/newExperiment" )
#	difx.v2dFile( "jobName" )
#
#	#  Assign callbacks
#	difx.newFileCallback( myNewFileCallback )
#	difx.processCompleteCallback( myProcessCompleteCallback )
#
#	#  Run vex2difx and calc, return immediately
#	vex2difxRunning = True
#	difx.runVex2Difx( False )
#	print "vex2difx started"
#
#	#  Do some other stuff.
#	while vex2difxRunning:
#		print "still running"
#		...
#
#	print "vex2difx and calc complete!"
#
#	difx.close()
#	\endcode
#	
#
#<!---======================================================================--->
class Client( DiFXControl.Client ):

	def __init__( self, client = None ):
		if client == None:
			DiFXControl.Client.__init__( self )
			self._client = self
		else:
			self._client = client
		self._fileName = None
		self._passPath = None
		self._calcCommand = "calcif2"
		self._calcOnly = False
		self._vex2difxComplete = False
		self._fileSize = None
		self._newFileCallback = None
		self._processCompleteCallback = None
			
	#<!------------------------------------------------------------------------>
	## Define the file name for a vex2difx command.
	#
	#  @param name Name of the file used in a vex2difx command.  This is either
	#              a .v2d file (with extension) if running a full vex2difx 
	#              operation, or a job name (without extension, containing 
	#              optional wildcards) for a calc-only operation.
	#
	#  In all cases, files lack any path information (they are supposed to be
	#  within the pass on which the operation is being performed).
	#
	#<!------------------------------------------------------------------------>
	def fileName( self, path ):
		self._fileName = path
	
	#<!------------------------------------------------------------------------>
	## Define the .v2d file for a vex2difx command.
	#
	#  @param name Name of the .v2d file used in a vex2difx command (includes
	#              the .v2d extension, but no path information).
	#
	#  The .v2d file is assumed to already reside in the the "pass path" when
	#  vex2difx is run.
	#
	#<!------------------------------------------------------------------------>
	def v2dFile( self, name ):
		self._fileName = name
	
	#<!------------------------------------------------------------------------>
	## Define a job or list of jobs for a calc-only operation.
	#
	#  @param name Job name, which may contain wildcards to make it a list of
	#              jobs, on which a calc-only operation will be run.
	#
	#<!------------------------------------------------------------------------>
	def jobName( self, name ):
		self._fileName = name
	
	#<!------------------------------------------------------------------------>
	## Define the pass path.
	#
	#  @param path Full path of the pass in which operations are to take place.
	#
	#<!------------------------------------------------------------------------>
	def passPath( self, path ):
		self._passPath = path
	
	#<!------------------------------------------------------------------------>
	## Decide whether to run calc operations only
	#
	#  @param doIt True/False - False is full job-creation operation, True is
	#              calc-only.
	#
	#<!------------------------------------------------------------------------>
	def calcOnly( self, doIt ):
		self._calcOnly = doIt
	
	#<!------------------------------------------------------------------------>
	## Set the calc command
	#
	#  @param command New calc command.  By default the command is "calcif2".
	#
	#<!------------------------------------------------------------------------>
	def calcCommand( self, command ):
		self._calcCommand = command
	
	#<!------------------------------------------------------------------------>
	## Run a vex2difx command on the server.
	#
	#  @param waitToFinish True/False determines whether this function will
	#                      return immediately or wait for the operation
	#                      to be complete on the server.
	#
	#  Composes and sends an XML message that triggers either a complete
	#  job-creation seqeunce in a pass, or runs only calc operations on existing
	#  job files in that pass.  The pass directory must exists.  For the job
	#  creation, a .vex and .v2d file must exist in the directory.  For calc-only
	#  operations, .input and .calc files that match specified job names must
	#  exist.
	#
	#<!------------------------------------------------------------------------>
	def runVex2Difx( self, waitToFinish = True ):
		#  Open a new channel for this operation.
		self.channel = self._client.newChannel( self.vex2difxCallback )
		#  Compose the command in XML
		packetData = "		<difxVersion>" + str( self.versionPreference ) + "</difxVersion>\n"
		packetData += "		<file>" + str( self._fileName ) + "</file>\n"
		packetData += "		<passPath>" + str( self._passPath ) + "</passPath>\n"
		packetData += "		<address>" + self._client.sock.getsockname()[0] + "</address>\n"
		packetData += "		<port>" + str( self.channel ) + "</port>\n"
		packetData += "		<calcCommand>" + str( self._calcCommand ) + "</calcCommand>\n"
		if self._calcOnly:
			packetData += "		<calcOnly>1</calcOnly>\n"
		else:
			packetData += "		<calcOnly>0</calcOnly>\n"
		self._vex2difxComplete = False
		self._client.sendCommandPacket( "DifxVex2DifxRun", packetData )
		#  Now we wait for the responses to pile in, unless we've been
		#  instructed to bail out.
		if waitToFinish:
			wait = self._client._waitTime
			while wait > 0.0 and not self._vex2difxComplete:
				try:
					time.sleep( 0.01 )
				except KeyboardInterrupt:
					self._vex2difxComplete = True			
				wait -= 0.01
				if wait == 0.0:
					self._client.doTimeoutCallback()
			self._client.closeChannel( self.channel )
			return
		else:
			return
		
	#<!------------------------------------------------------------------------>
	## Callback function for a "vex2difx" command.
	#
	#  The returns from this command are all file names, either .input files
	#  created by vex2difx or .im files created by the calc process.
	#<!------------------------------------------------------------------------>
	def vex2difxCallback( self, data ):
		#  The return messages alternate between a size measurement telling
		#  us the length of subsequent data or the data itself.  A zero-length
		#  size measurement tells us that no more messages are to be expected.
		if self._fileSize == None:
			self._fileSize = socket.ntohl( self._client.i.unpack( data[0:4] )[0] )
			if self._fileSize == 0:
				self._client._vex2difxComplete = True
				if self._processCompleteCallback != None:
					self._processCompleteCallback()
		else:
			self._fileName = data
			self._fileSize = None
			if self._newFileCallback != None:
				self._newFileCallback( self._fileName )
				
	#<!------------------------------------------------------------------------>
	## Set a callback function that will be called when a new file is created.
	#
	#  @param function The name of the callback function.
	#
	#  The callback function should have a single argument - the name of the
	#  new file.
	#<!------------------------------------------------------------------------>
	def newFileCallback( self, function ):
		self._newFileCallback = function

	#<!------------------------------------------------------------------------>
	## Set a callback function that will be called when DiFX is done.
	#
	#  @param function The name of the callback function.
	#
	#  The callback function, which takes no arguments, will be called when the
	#  vex2difx command has been completed on the server side.
	#<!------------------------------------------------------------------------>
	def processCompleteCallback( self, function ):
		self._processCompleteCallback = function

			
