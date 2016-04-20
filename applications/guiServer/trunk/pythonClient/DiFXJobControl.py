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
## \brief Class used to start and stop jobs
#
#  This class provides a list of functions that can be used to start and stop
#  DiFX jobs, as well as control their run environment to some degree.  
#
#  <h3>The .input File</h3>
#
#  In all the controls contained in this class, jobs
#  are identified by the full path to their associated <code>.input</code>
#  files on the DiFX server (an <code>.input</code> file always ends with the extension
#  ".input").  Before you run any controls on a job, you first
#  need to tell the class this path using the inputFile() method:
#
#  \code{.py}
#  #  Open a new instance of the DiFXJobControl.Client class
#  difx = DiFXJobControl.Client()
#  difx.connect()
#  difx.monitor()
#  difx.inputFile( "/full/path/to/the/file.input" )
#  \endcode
#
#  The DiFXJobControl.Client class can only operate on a single job at a time.  Using it
#  you can do the following things:
#
#  <h3>Define Machines/Threads Used by <i>mpirun</i></h3>
#
#  When running a DiFX job, <i>mpirun</i> requires a description of the cluster
#  nodes and threads that will be devoted to the process.  This description is
#  contained in the <code>\<JOBNAME\>.machines</code> and <code>\<JOBNAME\>.threads</code>
#  files ("JOBNAME" is the full path of the <code>.input</code> file without the ".input"
#  extension).  The DiFXJobControl.Client class has methods that help you create these
#  files based on specifications you provide.  To create the files, you must
#  provide names of the head node, data sources (one for each antenna involved),
#  and processors, along with the number of threads that should be run in each
#  processor.
#
#  \code{.py}
#  #  Set the head node
#  difx.headNode( "head.node" )
#
#  #  Two data sources...
#  difx.clearDataSources()  #  not necessary for a new class instance
#  difx.addDataSource( "data1" )
#  difx.addDataSource( "data2" )
#
#  #  Two processors, each with 8 threads
#  difx.clearProcessors()   #  not necessary for a new class instance
#  difx.addProcessor( "node1" )
#  difx.addProcessor( "node2" )
#
#  #  Tell the server to create the .machines and .threads files
#  difx.defineMachines()
#  \endcode
#
#  <h3>Start a Job</h3>
#
#  A job is started through an instance of the JobRunner Class, which you generate
#  with the newJob() method.  The use of a unique instance of the JobRunner class
#  allows multiple jobs to be tracked simultaneously.  Once you have the JobRunner
#  instance, the job can be started with its start() method.  This is called with a "wait to finish"
#  boolean value that will tell the method whether to return immediately or to wait
#  until a job is complete (the default value is True).  In this example the "wait time"
#  is made very long to avoid having the start() method time out while the job is
#  starting - this may be necessary if your system is slow.
#
#  \code{.py}
#  #  Make the wait time huge to avoid a timeout while the job is starting
#  difx.waitTime( 300.0 )
#  thisJob = difx.newJob()
#  thisJob.start()
#  \endcode
#
#  DiFX jobs don't provide a lot of direct feedback while they are running, but
#  <i>guiServer</i> does collect what is available and transmits it as messages, warnings
#  and errors (as appropriate).  These can be collected by callback functions that you
#  define (do this before you start the job!).
#
#  \code{.py}
#  #  Define some functions to respond to DiFX feedback...
#  def messageCallback( argstr ):
#  	print str( argstr )
#  	
#  def warningCallback( argstr ):
#  	print "WARNING: " + str( argstr )
#  	
#  def errorCallback( argstr ):
#  	print "ERROR: " + str( argstr )
#
#  #  Set the callbacks (they won't be called unless you do this)
#  difx.messageCallback( messageCallback )
#  difx.warningCallback( warningCallback )
#  difx.errorCallback( errorCallback )
#
#  #  Start the job
#  thisJob = difx.newJob()
#  thisJob.start()
#  \endcode
#
#  A slightly more complex but possibly more useful approach would be to collect
#  DiFX message traffic that pertains to running jobs.  See \ref messages "Monitoring DiFX Messages".
#	
#  <h3>(Try to) Stop a Running Job</h3>
#
#  The stop() method can be used to <i>try</i> to stop a running job (it doesn't always
#  work).  By default it will attemp to stop the job specified by the current <code>.input</code>
#  file, but you can optionally give it any <code>.input</code> file path.
#
#  \code{.py}
#  #  Stop the job we just started
#  difx.stop()
#
#  #  Also stop some other job
#  difx.stop( "/full/path/to/another/job.input" )
#  \endcode
#
#  <h3>Collect Real-Time Data Products</h3>
#
#  The DiFXJobControl.Client class has a limited ability to collect the results of
#  DiFX processing in real time (or roughly real time).  A running job can
#  be monitored to see if its results are making any sense (and perhaps stopped
#  if they are not), and data can be used to create real time plots
#  that are more satisfying to look at than progress bars.  
#
#  Once an <code>.input</code> file has been selected, you can turn on the real
#  time monitor with the startMonitor() method.  The getMonitorProducts() method can then be
#  prompted to provide a complex structure of real time "product" descriptions
#  (see the getMonitorProducts() method for details) from which you can select what
#  you wish to monitor by providing "product numbers" to the requestProducts() method.
#  If satisfied, you then use the runWithMonitor() method to actually turn on
#  real time monitoring.
#
#  To examine the data
#  collected by real time monitoring, you need to provide callbacks for the
#  various products, then provide them to the DiFXJobControl.Client class using the monitorDataCallbacks() method.
#  The callbacks will be called as the data arrive (see the monitorDataCallbacks()
#  method for a full description of the products).
#
#  \code{.py}
#  #  Define a callback for monitor data
#  def myRTCallback():
#  	print "got some real time data!"
#
#  #  Start the real time monitor
#  difx.startMonitor()
#
#  #  Generate a list of available products
#  monProducts = difx.getMonitorProducts()
#
#  #  Produce a list of products from the above (messy) structure
#  #  and request these data
#  difx.requestProducts( productList )
#
#  #  Turn monitoring on
#  difx.runWithMonitor( True )
#
#  #  Select your callbacks for one or more of the six product types
#  difx.monitorDataCallbacks( None, None, None, None, None, myRTCallback )
#
#  #  Start the job - real time data will trigger the callback periodically
#  difx.start()
#  \endcode
#
#  Although DiFX is perfectly capable of running more than one job at a time,
#  real time monitoring can currently only monitor one job at a time.
#
#  Real time monitoring is something of a work in progress.
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
		self._inputFile = None
		self._headNode = None
		self._dataSources = []
		self._processors = []
		self._testProcessors = False
		self._forceJobStart = True
		self._restartSeconds = 0.0
		self._runWithMonitor = False
		self._stopOnConfigFail = True
		self._configOnly = False
		self._setMachinesDefCallback = None
		self.callbackExclusive = False
		self.amplitudeCB = None
		self.phaseCB = None
		self.lagCB = None
		self.meanAmplitudeCB = None
		self.meanPhaseCB = None
		self.meanLagCB = None
		self.jobFinalMessage = None
			
	#<!------------------------------------------------------------------------>
	## Create the .machines and .threads files associated with a job.
	#
	#  @param inputFile Full path of the .input file associated with the job.
	#  @param headNode Name of the head node 
	#  @param dataSources List of names of nodes to be used as data sources
	#  @param processors List of tuples containing processor names and the number
	#                    of threads each should run.
	#  @param waitToFinish True/False determines whether this function will
	#                      return immediately or wait for the machines definition
	#                      to be complete on the server.
	#
	#  This method accepts a full list of machine specifications to create
	#  the <code>.machines</code> and <code>.threads</code> files on the DiFX
	#  server.  These files are associated with a specific <code>.input</code>
	#  file, and are used by <i>mpirun</i> to distribute processing amongst
	#  the members of a DiFX cluster.  The complete process is performed here,
	#  including defining the various nodes and thread specifications and
	#  sending the appropriate commands to the DiFX server.
	#<!------------------------------------------------------------------------>
	def machinesDefinition( self, inputFile, headNode, dataSources, processors, waitToFinish = True ):
		self._inputFile = inputFile
		self._headNode = headNode
		self._dataSources = dataSources
		self._processors = processors
		self.defineMachines( waitToFinish )
		
	#<!------------------------------------------------------------------------>
	## Use existing .machines and .threads files to fill current structures.
	#
	#  @return success True/False whether the existing files were located and
	#                  successfully parsed.
	#
	#  If the <code>.input</code> path has an associated <code>.machines</code>
	#  and <code>.threads</code> file, load the appropriate class structures
	#  with the content.
	#
	#<!------------------------------------------------------------------------>
	def getMachines( self ):
		#  Need an input file...
		if self._inputFile == None:
			self.error( "No .input file defined" )
			return False
		#  Get the .machines and .threads files.
		machinesData = self._client.getFile( self._inputFile.replace( ".input", ".machines" ) )
		if machinesData == None:
			self.error( "No .machines file \"" + self._inputFile.replace( ".input", ".machines" ) + " was found" )
			return False
		threadsData = self.getFile( self._inputFile.replace( ".input", ".threads" ) )
		if threadsData == None:
			self.error( "No .threads file \"" + self._inputFile.replace( ".input", ".threads" ) + " was found" )
			return False
		#  Divide each file into lines of data.
		machinesData = machinesData.split( "\n" )
		threadsData = threadsData.split( "\n" )
		if len( machinesData ) == 0 or len( threadsData ) == 0 or len( machinesData ) <= len( threadsData ):
			self.error( ".machines and .threads data look incorrect" )
			return False
		#  The .machines file is led by the head node:
		self._headNode = machinesData.pop( 0 )
		#  The first line of the threads file is useless to us...
		threadsData.pop( 0 )
		#  The remaining lines of the threads file are the threads for each processor in
		#  the machines file...but the machines file first contains the data sources.  
		self._dataSources = machinesData[:len( machinesData ) - len( threadsData )]
		#  The remaining lines are processors.  The thread count for each is in
		#  the threads file, but should be incremented by one if the name matches the
		#  head node (this is something guiServer does which it probably shouldn't but
		#  it is not being fixed right now)
		self.clearProcessors()
		threadIndex = 0
		for proc in machinesData[len( machinesData ) - len( threadsData ):]:
			if proc != None and proc != "":
				threadCount = int( threadsData[threadIndex] )
				if proc == self._headNode:
					threadCount += 1
				++threadIndex
				self.addProcessor( proc, threadCount )
		return True
	
	#<!------------------------------------------------------------------------>
	## Define the input file for all subsequent DiFX operations.
	#
	#  @param path Full path of the .input file associated with the job.
	#
	#<!------------------------------------------------------------------------>
	def inputFile( self, path ):
		self._inputFile = path
	
	#<!------------------------------------------------------------------------>
	## Define the head node for all subsequent DiFX operations.
	#
	#  @param node Node name of the head node, as <i>guiServer</i> would address it.
	#
	#<!------------------------------------------------------------------------>
	def setHeadNode( self, node ):
		self._headNode = node
	
	#<!------------------------------------------------------------------------>
	## Remove all data source definitions
	#<!------------------------------------------------------------------------>
	def clearDataSources( self ):
		self._dataSources = []
	
	#<!------------------------------------------------------------------------>
	## Add a data source to the list of data sources.
	#
	#  @param node Node name of the data source, as <i>guiServer</i> would address it.
	#
	#  Data sources are added to the <code>.machines</code> file in the order
	#  in which they are entered using this method.  This order should match
	#  the location of the data outlined in the "DATA TABLE" section of the
	#  <code>.input</code> file.
	#<!------------------------------------------------------------------------>
	def addDataSource( self, node ):
		self._dataSources.append( node )
	
	#<!------------------------------------------------------------------------>
	## Remove all processor definitions
	#<!------------------------------------------------------------------------>
	def clearProcessors( self ):
		self._processors = []
	
	#<!------------------------------------------------------------------------>
	## Add a processor to the list of processors.
	#
	#  @param processor Node name of the processor, as <i>guiServer</i> would address it.
	#  @param threads Number of threads to use with this processor
	#
	#<!------------------------------------------------------------------------>
	def addProcessor( self, processor, threads ):
		self._processors.append( ( processor, threads ) )
		
	#<!------------------------------------------------------------------------>
	## Run a communications test with each processor.
	#
	#  @param runTest True/False turns on/off the processor test.
	#
	#  If a processor test is requested <i>guiServer</i> will run a quick
	#  <i>mpirun</i> job on each processor that is to be included in the
	#  <code>.machines</code> file.  The job is quite simple - it creates a
	#  zero-length file that is then deleted by <i>guiServer</i>.  It will
	#  fail if the processor name is incorrect, or <i>mpirun</i> doesn't exist
	#  or is installed improperly, or if the processor and the head node do
	#  not share file systems correctly.
	#
	#  The test is run when a call is made to the defineMachines() method.
	#  Processors that are tested and fail are not included in the final
	#  <code>.machines</code> and <code>.threads</code> files.
	#
	#  By default the test are not run.
	#
	#<!------------------------------------------------------------------------>
	def testProcessors( self, runTest ):
		self._testProcessors = runTest

	#<!------------------------------------------------------------------------>
	## Return the name of the head node.
	#
	#  @return headnode String containing the name of the head node.
	#
	#<!------------------------------------------------------------------------>
	def headNode( self ):
		return self._headNode

	#<!------------------------------------------------------------------------>
	## Return a list of the data source nodes.
	#
	#  @return nodelist List of strings containing data source nodes.
	#
	#<!------------------------------------------------------------------------>
	def dataSources( self ):
		return self._dataSources

	#<!------------------------------------------------------------------------>
	## Return a list of the processors and threads.
	#
	#  @return nodelist List of tuples containing the names of processing
	#                   nodes and the threads assigned to them.
	#
	#<!------------------------------------------------------------------------>
	def processors( self ):
		return self._processors

	#<!------------------------------------------------------------------------>
	## Send a machines definition to the server.
	#
	#  @param waitToFinish True/False determines whether this function will
	#                      return immediately or wait for the machines definition
	#                      to be complete on the server.
	#
	#  Composes and sends an XML message that triggers the creation of <code>.machines</code>
	#  and <code>.threads</code> files.  Variables containing the <code>.input</code>
	#  file name, head node, data sources, and processors and threads need to
	#  already be defined.
	#
	#<!------------------------------------------------------------------------>
	def defineMachines( self, waitToFinish = True ):
		#  Open a new channel for this operation.
		self.machinesFileName = None
		self.threadsFileName = None
		self.machinesDefComplete = False
		self.packetData = None
		self.channel = self._client.newChannel( self.machinesDefCallback )
		packetData = "		<input>" + self._inputFile + "</input>\n"
		packetData += "		<manager node=\"" + self._headNode + "\"/>\n"
		packetData += "		<datastream nodes=\""
		for node in self._dataSources:
			packetData += node + " "
		packetData += "\"/>\n"
		for node in self._processors:
			packetData += "		<process nodes=\"" + str( node[0] ) + "\" threads=\"" + str( node[1] ) + "\"/>\n"
		packetData += "		<difxVersion>" + str( self.versionPreference ) + "</difxVersion>\n"
		if self._testProcessors:
			packetData += "		<testProcessors>true</testProcessors>\n"
		else:
			packetData += "		<testProcessors>false</testProcessors>\n"
		packetData += "		<machinesFile>" + self._inputFile.replace( ".input", ".machines" ) + "</machinesFile>\n"
		packetData += "		<threadsFile>" + self._inputFile.replace( ".input", ".threads" ) + "</threadsFile>\n"
		packetData += "		<port>" + str( self.channel ) + "</port>\n"
		self._client.sendCommandPacket( "DifxMachinesDefinition", packetData )
		#  Now we wait for the responses to pile in, unless we've been
		#  instructed to bail out.
		if waitToFinish:
			wait = self._client._waitTime
			while wait > 0.0 and not self.machinesDefComplete:
				try:
					time.sleep( 0.01 )
				except KeyboardInterrupt:
					self.machinesDefComplete = True			
				wait -= 0.01
				if wait == 0.0:
					self._client.doTimeoutCallback()
			self._client.closeChannel( self.channel )
			return
		else:
			return
		
	#<!------------------------------------------------------------------------>
	## Set your own callback for responses to a machines definition.
	#
	#  @param callbackFunction User-defined function that takes an integer an
	#                          a string as arguments.
	#  @param exclusive True/False determines whether this function will
	#                   be called exclusively, or if the internal response
	#                   to packets will be employed as well.  False by default.
	#
	#  Set a user-defined function to serve as a callback to <i>guiServer</i>
	#  packets that are produced when creating <code>.machines</code> and
	#  <code>.threads</code> files.  The function must accept an integer "packet ID" as its first
	#  argument and a string "packet data" as its second.  This can be the only
	#  response to packet data if "exclusive" is True.  Otherwise the internal
	#  packet handling (which consists mostly of sending status messages) will
	#  be employed as well.
	#
	#  Portions of the startCallback() method can provide a model for writing your own
	#  callback.  The table below lists the packet IDs as defined in the
	#  DiFXJobControl.Client class, their integer values, and their associated
	#  packet data (an empty data entry means there is no associated data).
	#
	#  <table>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_TASK_TERMINATED                     <td>100  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_TASK_ENDED_GRACEFULLY               <td>101  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_TASK_STARTED                        <td>102  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_PARAMETER_CHECK_IN_PROGRESS         <td>103  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_PARAMETER_CHECK_SUCCESS             <td>104  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_NO_HEADNODE                 <td>105  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_NO_DATASOURCES              <td>106  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_NO_PROCESSORS               <td>107  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_WARNING_NO_MACHINES_FILE_SPECIFIED  <td>108  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_WARNING_NO_THREADS_FILE_SPECIFIED   <td>109  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_THREADS_FILE_NAME                   <td>110  <td>Name of the <code>.threads</code> file created
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_MACHINES_FILE_NAME                  <td>111  <td>Name of the <code>.machines</code> file created
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_NO_FILES_SPECIFIED          <td>112  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_OPEN_MACHINES_FILE          <td>113  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_OPEN_THREADS_FILE           <td>114  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_MACHINES_FILE_CREATED               <td>115  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_THREADS_FILE_CREATED                <td>116  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_FILE_REMOVAL                <td>117  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_POPEN                       <td>118  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_FAILURE_MPIRUN                      <td>119  <td>Node that failed <i>mpirun</i> test
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_SUCCESS_MPIRUN                      <td>120  <td>Node that passed <i>mpirun</i> test
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_LOW_THREAD_COUNT                    <td>121  <td>
	#  <tr><td>DiFXJobControl.Client.MACHINE_DEF_RUNNING_MPIRUN_TESTS                <td>122  <td>
	#  </table>
	#
	#<!------------------------------------------------------------------------>
	def setMachinesDefCallback( self, callbackFunction, exclusive = False ):
		self._setMachinesDefCallback = callbackFunction
		self.exclusiveCallback = exclusive
				
	MACHINE_DEF_TASK_TERMINATED                     = 100
	MACHINE_DEF_TASK_ENDED_GRACEFULLY               = 101
	MACHINE_DEF_TASK_STARTED                        = 102
	MACHINE_DEF_PARAMETER_CHECK_IN_PROGRESS         = 103
	MACHINE_DEF_PARAMETER_CHECK_SUCCESS             = 104
	MACHINE_DEF_FAILURE_NO_HEADNODE                 = 105
	MACHINE_DEF_FAILURE_NO_DATASOURCES              = 106
	MACHINE_DEF_FAILURE_NO_PROCESSORS               = 107
	MACHINE_DEF_WARNING_NO_MACHINES_FILE_SPECIFIED  = 108
	MACHINE_DEF_WARNING_NO_THREADS_FILE_SPECIFIED   = 109
	MACHINE_DEF_THREADS_FILE_NAME                   = 110
	MACHINE_DEF_MACHINES_FILE_NAME                  = 111
	MACHINE_DEF_FAILURE_NO_FILES_SPECIFIED          = 112
	MACHINE_DEF_FAILURE_OPEN_MACHINES_FILE          = 113
	MACHINE_DEF_FAILURE_OPEN_THREADS_FILE           = 114
	MACHINE_DEF_MACHINES_FILE_CREATED               = 115
	MACHINE_DEF_THREADS_FILE_CREATED                = 116
	MACHINE_DEF_FAILURE_FILE_REMOVAL                = 117
	MACHINE_DEF_FAILURE_POPEN                       = 118
	MACHINE_DEF_FAILURE_MPIRUN                      = 119
	MACHINE_DEF_SUCCESS_MPIRUN                      = 120
	MACHINE_DEF_LOW_THREAD_COUNT                    = 121
	MACHINE_DEF_RUNNING_MPIRUN_TESTS                = 122

	#<!------------------------------------------------------------------------>
	## Callback function for a "DifxMachinesDefinition" command response.
	#
	#  @param data Data transmitted by the server
	#
	#  This function is called when data are received over the socket "channel"
	#  opened for the machines definition procedure.  The data are fed to the
	#  DiFXControl.Client.packetProtocol() method to parse out a "packet ID"
	#  that matches one of the "MACHINE_DEF" values (above), and data associated
	#  with it.  The packet may not be complete at this time - the 
	#  DiFXControl.Client.packetProtocol() method deals with that.
	#
	#  If you wish to define your own callback to respond to packet data,
	#  use the setMachinesCallback() method.
	#<!------------------------------------------------------------------------>
	def machinesDefCallback( self, data ):
		#  The data from this callback can consist of one of three components
		#  as part of the server packet protocol - these are an ID, a data length,
		#  and data itself.  The packetProtocol function gets this for us.
		self.packetData = self._client.packetProtocol( data, self.packetData )
		if self.packetData[3]: #  Indicates whether packet is complete
			#  If the user has set their own callback, use it.  We send the packet
			#  ID and packet data (a string) as arguments.
			if self._setMachinesDefCallback != None:
				self._setMachinesDefCallback( self.packetData[0], self.packetData[2] )
				if self.exclusiveCallback:
					return
			#  Figure out what this message is from the packet ID
			packetId = self.packetData[0]
			if packetId == self.MACHINE_DEF_TASK_TERMINATED:
				#  This is a failure, but always occurs after another error message
				#  so we don't bother sending an error here.
				self.machinesDefComplete = True
				self.doFinalCallback()
			elif packetId == self.MACHINE_DEF_TASK_ENDED_GRACEFULLY:
				self.machinesDefComplete = True
				self.message( "Machines definition ended gracefully." )
				self.doFinalCallback()
			elif packetId == self.MACHINE_DEF_TASK_STARTED:
				self.message( "Machines definition process started on guiServer." )
			elif packetId == self.MACHINE_DEF_PARAMETER_CHECK_IN_PROGRESS:
				self.message( "Checking parameters." )
			elif packetId == self.MACHINE_DEF_PARAMETER_CHECK_SUCCESS:
				self.message( "Parameter check complete." )
			elif packetId == self.MACHINE_DEF_FAILURE_NO_HEADNODE:
				self.error( "Machines specification failed - no headnode specified." )
			elif packetId == self.MACHINE_DEF_FAILURE_NO_DATASOURCES:
				self.error( "Machines specification failed - no data sources specified." )
			elif packetId == self.MACHINE_DEF_FAILURE_NO_PROCESSORS:
				self.error( "Machines specification failed - no processing nodes  specified." )
			elif packetId == self.MACHINE_DEF_WARNING_NO_MACHINES_FILE_SPECIFIED:
				self.warning( "No machines file specified - generating path from input file." )
			elif packetId == self.MACHINE_DEF_WARNING_NO_THREADS_FILE_SPECIFIED:
				self.warning( "No threads file specified - generating path from input file." )
			elif packetId == self.MACHINE_DEF_THREADS_FILE_NAME:
				self.threadsFileName = self.packetData[2]
				self.message( "Will create .threads file \"" + self.packetData[2] + "\"." )
			elif packetId == self.MACHINE_DEF_MACHINES_FILE_NAME:
				self.machinesFileName = self.packetData[2]
				self.message( "Will create .machines file \"" + self.packetData[2] + "\"." )
			elif packetId == self.MACHINE_DEF_FAILURE_NO_FILES_SPECIFIED:
				self.error( "Machines specification failed - no input file or machines file specified" )
			elif packetId == self.MACHINE_DEF_FAILURE_OPEN_MACHINES_FILE:
				self.error( self.packetData[2] )
			elif packetId == self.MACHINE_DEF_FAILURE_OPEN_THREADS_FILE:
				self.error( self.packetData[2] )
			elif packetId == self.MACHINE_DEF_MACHINES_FILE_CREATED:
				#  Save the name of the created machines file.  If this wasn't created by the
				#  server, we assume it is based on the .input file.
				if self.machinesFileName == None:
					self.machinesFileName = self._inputFile.replace( ".input", ".machines" )
				self.message( "Machines file \"" + self.machinesFileName + "\" created." )
			elif packetId == self.MACHINE_DEF_THREADS_FILE_CREATED:
				#  Save the name of the created machines file.  If this wasn't created by the
				#  server, we assume it is based on the .input file.
				if self.machinesFileName == None:
					self.machinesFileName = self._inputFile.replace( ".input", ".threads" )
				self.message( "Threads file \"" + self.threadsFileName + "\" created." )
			elif packetId == self.MACHINE_DEF_FAILURE_FILE_REMOVAL:
				pass
			elif packetId == self.MACHINE_DEF_FAILURE_POPEN:
				pass
			elif packetId == self.MACHINE_DEF_FAILURE_MPIRUN:
				self.warning( "Node " + self.packetData[2] + " failed - will be removed from processor list." )
			elif packetId == self.MACHINE_DEF_SUCCESS_MPIRUN:
				self.message( "Node " + self.packetData[2] + " passed." )
			elif packetId == self.MACHINE_DEF_LOW_THREAD_COUNT:
				self.warning( "Total thread count in .threads file is low." )
			elif packetId == self.MACHINE_DEF_RUNNING_MPIRUN_TESTS:
				self.message( "Running mpirun tests on all processing nodes." )
	
	#<!------------------------------------------------------------------------>
	## Set up a new JobRunner class.
	#
	#  @return The new class instance.
	#
	#  The JobRunner class is used to run a single job.  It is passed the
	#  DiFXJobControl.Client class instance to gain access to its settings.
	#
	#  Creating a class instance allows multiple jobs to be run by the
	#  DiFXJobControl.Client class simultaneously.
	#<!------------------------------------------------------------------------>
	def newJob( self ):
		return self.JobRunner( self )

	#<!------------------------------------------------------------------------>
	## \brief Class used to run a job.
	#
	#  This class is used to start a job and monitor it while it is running.
	#  It depends on an instance of the DiFXJobControl.Client class for many of the
	#  settings it needs.
	#
	#  <h3>Why A New Class?</h3>
	#
	#  There are a couple of justifications for requiring a new class type to run
	#  jobs.  For one, the use of this class allows a single instance of the DiFXJobControl.Client class
	#  to start more than one job at a time.  Each DiFXJobControl.Client.JobRunner class
	#  instance will give some (limited) control and feedback from the running job.  Any number of instances
	#  can co-exist without the control/feedback interacting becoming confused.
	#
	#  In addition, JobRunner is relatively light-weight compared to a DiFXJobControl.Client
	#  instance, and does not require a new network connection as DiFXJobControl.Client
	#  does.
	#
	#  <h3>Creating an Instance</h3>
	#
	#  An instance of the class is created using the DiFXJobControl.Client.newJob()
	#  method.  Alternatively it can be created independently.  In either case,
	#  all job-related settings from DiFXJobControl.Client are copied so they will
	#  not change for the job even if they are altered in the DiFXJobControl.Client
	#  instance.
	#
	#  <h3>Running A Job</h3>
	#
	#  A job is not started until a call to the DiFXJobControl.Client.JobRunner.start()
	#  method.  
	#
	#  <h3>How To Find Out If A Job Is Done</h3>
	#
	#  <h3>Cleaning Up</h3>
	#
	#  When a job is started an independent "virtual communications channel" is
	#  created to uniquely route control and data transfer associated with the
	#  job between the client and <i>guiServer</i>.  A small amount of overhead
	#  is associated with this structure on both the client and server side.  
	#  When a job is complete and it is no longer needed, the channel should be
	#  closed using the DiFXJobControl.Client.JobRunner.closeChannel() method.
	#  
	#<!------------------------------------------------------------------------>
	class JobRunner:
		def __init__( self, jobControl ):
			self.jobControl = jobControl
			self._client = jobControl._client
			self._forceJobStart = True
			self._restartSeconds = 0.0
			self._runWithMonitor = False
			self._stopOnConfigFail = True
			self._configOnly = False
			self._setStartCallback = None
			self.wait = self._client._waitTime
			self.jobComplete = False
			self.message = self.jobControl.message
			self.warning = self.jobControl.warning
			self.error = self.jobControl.error
			self.inputFile = self.jobControl._inputFile
			self.headNode = self.jobControl._headNode
			self.dataSources = self.jobControl._dataSources
			self.processors = self.jobControl._processors
			self.versionPreference = self.jobControl.versionPreference
	
		#<!------------------------------------------------------------------------>
		## Determine whether a job should be started even if output exists from a prior run.
		#
		#  @param newVal True/False determines whether to "force" a job start.
		#
		#  If output exists from a previous run of a DiFX job (it will be in a in a 
		#  <code>\<JOBNAME\>.difx</code> directory), <i>guiServer</i> will not start
		#  unless the job unless it is "forced".  If forced, the old output will
		#  first be deleted.  By default this setting is True.
		#
		#<!------------------------------------------------------------------------>
		def forceJobStart( self, newVal ):
			self._forceJobStart = newVal

		#<!------------------------------------------------------------------------>
		## Set the time to advance into a job before restarting it.
		#
		#  @param startSeconds The restart time from the start of the job.  0.0 is the
		#                      default.
		#
		#  DiFX allows a job to be started at a specified time after the job's actual
		#  start.  This method lets you set that time (measured in seconds).  By
		#  default the value is 0.0 (start at the beginning).
		#
		#<!------------------------------------------------------------------------>
		def restartSeconds( self, startSeconds ):
			self._restartSeconds = startSeconds

		#<!------------------------------------------------------------------------>
		## Determine whether a job should be run with the "monitor" on.
		#
		#  @param newVal True/False determines whether to run with the monitor.
		#
		#  The monitor is a function of the core DiFX processing that allows real
		#  time data to be extracted from a running job.  In the past it has been
		#  (occasionally) problematic, but such problems appear to be fixed so by
		#  default this setting is True.  If you are having job start issues you
		#  may wish to try turning the monitor off (the monitor is only required
		#  if you are employing the monitor() method).
		#
		#<!------------------------------------------------------------------------>
		def runWithMonitor( self, newVal ):
			self._runWithMonitor = newVal

		#<!------------------------------------------------------------------------>
		## Determine whether a job should run only the configuration test.
		#
		#  @param newVal True/False determines whether to stop on a failed test.
		#
		#  The "configuration test" is performed on the .input file to determine
		#  if a job can be run successfully.  If this option is set ONLY the test
		#  will be run - the job will not be run whether the test passes or fails.
		#
		#<!------------------------------------------------------------------------>
		def configOnly( self, newVal ):
			self._configOnly = newVal

		#<!------------------------------------------------------------------------>
		## Determine whether a job should stop if the configuration test fails.
		#
		#  @param newVal True/False determines whether to stop on a failed test.
		#
		#  Before a job is run a "configuration test" is performed on the .input
		#  file.  The test will determine whether the job can be run successfully
		#  (with a few exceptions).  If the test fails and this option is set to
		#  True (the default), no effort will be made to actually run the job.
		#
		#<!------------------------------------------------------------------------>
		def stopOnConfigFail( self, newVal ):
			self._stopOnConfigFail = newVal

		#<!------------------------------------------------------------------------>
		## Send a command to start a job to the server.
		#
		#  @param waitToFinish True/False determines whether this function will
		#                      return immediately or wait for the job
		#                      to be complete on the server.
		#
		#  Composes and sends an XML message that triggers the start of a job.  Either
		#  wait for the job to finish or return immediately after starting it based
		#  on the value of waitToFinish.
		#
		#<!------------------------------------------------------------------------>
		def start( self, waitToFinish = True ):
			#  Open a new channel for this operation.
			self.jobComplete = False
			self.jobFinalMessage = None
			self.packetData = None
			self.channel = self._client.newChannel( self.startCallback )
			packetData = "		<input>" + self.inputFile + "</input>\n"
			#  The manager, datastream, and processor nodes are not used by the start process
			#  on guiServer.  However, they are checked, so we need to include something for
			#  them.  If we have legitimate values, we use those, but if we don't we can get
			#  away with "None".
			if self.headNode != None:
				packetData += "		<manager node=\"" + self.headNode + "\"/>\n"
			else:
				packetData += "		<manager node=\"None\"/>\n"
			if len( self.dataSources ) > 0:
				packetData += "		<datastream nodes=\""
				for node in self.dataSources:
					packetData += node + " "
				packetData += "\"/>\n"
			else:
				packetData += "		<datastream nodes=\"None\"/>\n"
			if len( self.processors ) > 0:
				for node in self.processors:
					packetData += "		<process nodes=\"" + str( node[0] ) + "\" threads=\"" + str( node[1] ) + "\"/>\n"
			else:
				packetData += "		<process nodes=\"None\" threads=\"None\"/>\n"
			packetData += "		<difxVersion>" + str( self.versionPreference ) + "</difxVersion>\n"
			if self._forceJobStart:
				packetData += "		<force>1</force>\n"
			else:
				packetData += "		<force>0</force>\n"
			packetData += "		<restartSeconds>" + str( self._restartSeconds ) + "</restartSeconds>\n"
			if self._runWithMonitor:
				packetData += "		<function>RUN_MONITOR</function>\n"
			elif self._configOnly:
				packetData += "     <function>CONFIG_ONLY</function>"
			elif self._stopOnConfigFail:
				packetData += "     <function>BAIL_ON_CONFIG_FAIL</function>"
			else:
				packetData += "		<function>USNO</function>\n"
			packetData += "		<port>" + str( self.channel ) + "</port>\n"
			self._client.sendCommandPacket( "DifxStart", packetData )
			#  Now we wait for the responses to pile in, unless we've been
			#  instructed to bail out.
			if waitToFinish:
				self.wait = self._client._waitTime
				while self.wait > 0.0 and not self.jobComplete:
					try:
						time.sleep( 0.01 )
					except KeyboardInterrupt:
						self.jobComplete = True			
					self.wait -= 0.01
					if self.wait == 0.0:
						print "timeout!"
						self._client.doTimeoutCallback()
				self._client.closeChannel( self.channel )
				return
			else:
				return

		#<!------------------------------------------------------------------------>
		## Clean up the channel connection associated with this job.
		#
		#<!------------------------------------------------------------------------>
		def closeChannel( self ):
			self._client.closeChannel( self.channel )
			
		#<!------------------------------------------------------------------------>
		## Set your own callback for responses to start activities.
		#
		#  @param callbackFunction User-defined function that takes an integer an
		#                          a string as arguments.
		#  @param exclusive True/False determines whether this function will
		#                   be called exclusively, or if the internal response
		#                   to packets will be employed as well.  False by default.
		#
		#  Set a user-defined function to serve as a callback to <i>guiServer</i>
		#  packets that are sent as a job runs.  The function must accept an integer "packet ID" as its first
		#  argument and a string "packet data" as its second.  This can be the only
		#  response to packet data if "exclusive" is True, otherwise the internal
		#  packet handling (which consists mostly of sending status messages) will
		#  be employed as well.
		#
		#  Portions of the JobRunner.startCallback() method can provide a model for writing your own
		#  callback.  The table below lists the packet IDs as defined in the
		#  DiFXJobControl.Client class, their integer values, and their associated
		#  packet data (an empty data entry means there is no associated data).
		#
		#  If you are going to use your own callback function, it makes sense to set
		#  it before you start a job so you don't miss anything.
		#
		#  <table>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_JOB_TERMINATED                   <td>100<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_JOB_ENDED_GRACEFULLY             <td>101<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_JOB_STARTED                      <td>102<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_PARAMETER_CHECK_IN_PROGRESS      <td>103<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_PARAMETER_CHECK_SUCCESS          <td>104<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_FAILURE_NO_HEADNODE              <td>105<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_FAILURE_NO_DATASOURCES           <td>106<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_FAILURE_NO_PROCESSORS            <td>107<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_FAILURE_NO_INPUTFILE_SPECIFIED   <td>108<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_FAILURE_INPUTFILE_NOT_FOUND      <td>109<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_FAILURE_INPUTFILE_NAME_TOO_LONG  <td>110<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_FAILURE_OUTPUT_EXISTS            <td>111<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DELETING_PREVIOUS_OUTPUT         <td>112<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_STARTING_DIFX                    <td>113<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DIFX_MESSAGE                     <td>114<td>Message text
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DIFX_WARNING                     <td>115<td>Warning text
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DIFX_ERROR                       <td>116<td>Error text
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DIFX_COMPLETE                    <td>117<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DATA_FILE_SIZE                   <td>118<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_JOB_FAILED                       <td>119<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_JOB_ENDED_WITH_ERRORS            <td>120<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DIFX_MONITOR_CONNECTION_ACTIVE   <td>121<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DIFX_MONITOR_CONNECTION_BROKEN   <td>122<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_DIFX_MONITOR_CONNECTION_FAILED   <td>123<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_FAILURE_INPUTFILE_BAD_CONFIG     <td>124<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_CONFIG_PASSED                    <td>125<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_CONFIG_COMMAND_NOT_FOUND         <td>126<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_RUN_CONFIG_CHECK                 <td>127<td>
		#  <tr><td>DiFXJobControl.Client.RUN_DIFX_STOPPED_AFTER_CONFIG             <td>128<td>
		#  </table>
		#
		#<!------------------------------------------------------------------------>
		def setStartCallback( self, callbackFunction, exclusive = False ):
			self._setStartCallback = callbackFunction
			self.exclusiveCallback = exclusive

		RUN_DIFX_JOB_TERMINATED                   = 100
		RUN_DIFX_JOB_ENDED_GRACEFULLY             = 101
		RUN_DIFX_JOB_STARTED                      = 102
		RUN_DIFX_PARAMETER_CHECK_IN_PROGRESS      = 103
		RUN_DIFX_PARAMETER_CHECK_SUCCESS          = 104
		RUN_DIFX_FAILURE_NO_HEADNODE              = 105
		RUN_DIFX_FAILURE_NO_DATASOURCES           = 106
		RUN_DIFX_FAILURE_NO_PROCESSORS            = 107
		RUN_DIFX_FAILURE_NO_INPUTFILE_SPECIFIED   = 108
		RUN_DIFX_FAILURE_INPUTFILE_NOT_FOUND      = 109
		RUN_DIFX_FAILURE_INPUTFILE_NAME_TOO_LONG  = 110
		RUN_DIFX_FAILURE_OUTPUT_EXISTS            = 111
		RUN_DIFX_DELETING_PREVIOUS_OUTPUT         = 112
		RUN_DIFX_STARTING_DIFX                    = 113
		RUN_DIFX_DIFX_MESSAGE                     = 114
		RUN_DIFX_DIFX_WARNING                     = 115
		RUN_DIFX_DIFX_ERROR                       = 116
		RUN_DIFX_DIFX_COMPLETE                    = 117
		RUN_DIFX_DATA_FILE_SIZE                   = 118
		RUN_DIFX_JOB_FAILED                       = 119
		RUN_DIFX_JOB_ENDED_WITH_ERRORS            = 120
		RUN_DIFX_DIFX_MONITOR_CONNECTION_ACTIVE   = 121
		RUN_DIFX_DIFX_MONITOR_CONNECTION_BROKEN   = 122
		RUN_DIFX_DIFX_MONITOR_CONNECTION_FAILED   = 123
		RUN_DIFX_FAILURE_INPUTFILE_BAD_CONFIG     = 124
		RUN_DIFX_CONFIG_PASSED                    = 125
		RUN_DIFX_CONFIG_COMMAND_NOT_FOUND         = 126
		RUN_DIFX_RUN_CONFIG_CHECK                 = 127
		RUN_DIFX_STOPPED_AFTER_CONFIG             = 128
		RUN_DIFX_NOT_RESPONDING                   = 129

		#<!------------------------------------------------------------------------>
		## Callback function for a "DifxStart" command response.
		#
		#  @param data Data transmitted by the server
		#
		#  This function is called when data are received over the socket "channel"
		#  opened for the job start procedure.  The data are fed to the
		#  DiFXControl.Client.packetProtocol() method to parse out a "packet ID"
		#  that matches one of the "RUN_DIFX" values (above), and data associated
		#  with it.  The packet may not be complete at this time - the 
		#  DiFXControl.Client.packetProtocol() method deals with that.
		#<!------------------------------------------------------------------------>
		def startCallback( self, data ):
			#  Reset the wait time.
			self.wait = self._client._waitTime
			#  The data from this callback can consist of one of three components
			#  as part of the server packet protocol - these are an ID, a data length,
			#  and data itself.  The packetProtocol function gets this for us.
			self.packetData = self._client.packetProtocol( data, self.packetData )
			if self.packetData[3]: #  Indicates whether packet is complete
				#  If the user has set their own callback, use it.  We send the packet
				#  ID and packet data (a string) as arguments.
				if self._setStartCallback != None:
					self._setStartCallback( self.packetData[0], self.packetData[2] )
					if self.exclusiveCallback:
						return
				#  Figure out what this message is from the packet ID
				packetId = self.packetData[0]
				if packetId == self.RUN_DIFX_JOB_TERMINATED:
					self.warning( "Job terminated" )
					self.jobFinalMessage = packetId
					self.jobComplete = True
				elif packetId == self.RUN_DIFX_JOB_ENDED_GRACEFULLY:
					self.message( "Job ended gracefully" )
					self.jobFinalMessage = packetId
					self.jobComplete = True
				elif packetId == self.RUN_DIFX_JOB_STARTED:
					self.jobFinalMessage = packetId
					pass
				elif packetId == self.RUN_DIFX_PARAMETER_CHECK_IN_PROGRESS:
					self.message( "Start parameter check" )
				elif packetId == self.RUN_DIFX_PARAMETER_CHECK_SUCCESS:
					self.message( "Parameter check was successful" )
				elif packetId == self.RUN_DIFX_FAILURE_NO_HEADNODE:
					self.error( "Start failed - no headnode specified" )
				elif packetId == self.RUN_DIFX_FAILURE_NO_DATASOURCES:
					self.error( "Start failed - no data sources specified" )
				elif packetId == self.RUN_DIFX_FAILURE_NO_PROCESSORS:
					self.error( "Start failed - no processing nodes  specified" )
				elif packetId == self.RUN_DIFX_FAILURE_NO_INPUTFILE_SPECIFIED:
					self.error( "Start failed - no input file specified" )
				elif packetId == self.RUN_DIFX_FAILURE_INPUTFILE_NOT_FOUND:
					self.error( "Start failed - input file was not found" )
				elif packetId == self.RUN_DIFX_FAILURE_INPUTFILE_NAME_TOO_LONG:
					self.error( "Start failed - input file name was too long" )	
				elif packetId == self.RUN_DIFX_FAILURE_OUTPUT_EXISTS:
					self.error( "Start failed - output directory exists" )
				elif packetId == self.RUN_DIFX_DELETING_PREVIOUS_OUTPUT:
					self.warning( "Deleting existing output directory" )
				elif packetId == self.RUN_DIFX_STARTING_DIFX:
					self.message( "Starting DiFX" )
				elif packetId == self.RUN_DIFX_DIFX_MESSAGE:
					self.message( self.packetData[2] )
				elif packetId == self.RUN_DIFX_DIFX_WARNING:
					self.warning( self.packetData[2] )
				elif packetId == self.RUN_DIFX_DIFX_ERROR:
					self.error( self.packetData[2] )
				elif packetId == self.RUN_DIFX_DIFX_COMPLETE:
					self.message( "Job complete" )
				elif packetId == self.RUN_DIFX_DATA_FILE_SIZE:
					pass
				elif packetId == self.RUN_DIFX_JOB_FAILED:
					self.error( "Job failed" )
					self.jobFinalMessage = packetId
					self.jobComplete = True
				elif packetId == self.RUN_DIFX_JOB_ENDED_WITH_ERRORS:
					self.warning( "Job ended with errors" )
					self.jobFinalMessage = packetId
					self.jobComplete = True
				elif packetId == self.RUN_DIFX_DIFX_MONITOR_CONNECTION_ACTIVE:
					pass
				elif packetId == self.RUN_DIFX_DIFX_MONITOR_CONNECTION_BROKEN:
					pass
				elif packetId == self.RUN_DIFX_DIFX_MONITOR_CONNECTION_FAILED:
					pass
				elif packetId == self.RUN_DIFX_FAILURE_INPUTFILE_BAD_CONFIG:
					self.error( "Start failed - input file did not pass configuation test." )
					self.jobFinalMessage = packetId
				elif packetId == self.RUN_DIFX_CONFIG_PASSED:
					self.message( "Configuration test passed." )
					self.jobFinalMessage = packetId
				elif packetId == self.RUN_DIFX_CONFIG_COMMAND_NOT_FOUND:
					self.error( "Configuration command was not found." )
				elif packetId == self.RUN_DIFX_RUN_CONFIG_CHECK:
					self.message( "Running configuration check." )
				elif packetId == self.RUN_DIFX_STOPPED_AFTER_CONFIG:
					self.jobComplete = True

		#<!------------------------------------------------------------------------>
		## Get the "final" state of a job.
		#
		#  Return the last message type received from a "complete" job.  The message
		#  will be one of:
		#  <ul>
		#     <li>self.RUN_DIFX_JOB_FAILED
		#     <li>self.RUN_DIFX_JOB_ENDED_WITH_ERRORS
		#  </ul>
		#  These all mean exactly what they sound like they mean.
		#<!------------------------------------------------------------------------>
		def finalMessage( self ):
			return self.jobFinalMessage

		#<!------------------------------------------------------------------------>
		## Set the "final" message of a job.
		#
		#  @param newMessage The new message.
		#
		#  Change the "final" message of a job.  The message may be over written
		#  by subsequent actualy messages.
		#<!------------------------------------------------------------------------>
		def setFinalMessage( self, newMessage ):
			self.jobFinalMessage = newMessage

	#<!------------------------------------------------------------------------>
	## Attempt to stop a running job.
	#
	#  This method sends a "stop" command to <i>guiServer</i> with the
	#  <code>.input</code> file associated with a job.  <i>GuiServer</i> will
	#  attempt, based on the <code>.machines</code> file, to instruct all
	#  processors that might be working on a job to stop.  This is a set-and-forget
	#  command - there is no feedback.
	#
	#  Stopping a job only works sometimes, but it is always worth a shot if you
	#  want to kill something.  Generally it
	#  is most effective when a job is running fine, which coincidentally is
	#  when you are least likely to want to stop it.
	#
	#  Attempting to stop a job that has already finished, been stopped, or
	#  does not otherwise exist is completely harmless.
	#<!------------------------------------------------------------------------>
	def stop( self, inputFile = None ):
		if inputFile == None:
			inputFile = self._inputFile
		#  Compose the stop command.  All we really need to convey is the .input
		#  file name.
		packetData = "		<input>" + inputFile + "</input>\n"
		self.sendCommandPacket( "DifxStop", packetData )
		return

		MESSAGE                            = 100
		WARNING                            = 101
		ERROR                              = 102
		INPUT_FILE_PATH                    = 103
		CLOSE_CONNECTION                   = 104
		NUM_BASELINES                      = 105
		NUM_FREQUENCIES                    = 106
		BASELINE                           = 107
		FREQUENCY                          = 108
		NUM_SCANS                          = 109
		SCAN                               = 110
		TELESCOPE_1                        = 111
		TELESCOPE_2                        = 112
		BEGIN_CORRELATION_PRODUCTS         = 113
		NUM_PHASE_CENTERS                  = 114
		PHASE_CENTER                       = 115
		NUM_PULSAR_BINS                    = 116
		PULSAR_BIN                         = 117
		NUM_POL_PRODUCTS                   = 118
		POL_PRODUCT                        = 119
		NEW_PRODUCT                        = 120
		AUTOCORRELATION                    = 121
		PRODUCT_REQUEST                    = 122
		START_PRODUCT_REQUESTS             = 123
		END_PRODUCT_REQUESTS               = 124
		VISIBILITY_DATA                    = 125
		AMPLITUDE_DATA                     = 126
		PHASE_DATA                         = 127
		LAG_DATA                           = 128
		END_VISIBILITY_BLOCK               = 129
		JOB_NAME                           = 130
		OBS_CODE                           = 131
		SCAN_IDENTIFIER                    = 132
		SCAN_START_TIME                    = 133
		SCAN_END_TIME                      = 134
		SOURCE                             = 135
		SOURCE_RA                          = 136
		SOURCE_DEC                         = 137
		VISIBILITY_SCAN                    = 138
		FFT_SIZE                           = 139
		MEAN_AMPLITUDE_DATA                = 140
		MEAN_PHASE_DATA                    = 141
		MEAN_LAG_DATA                      = 142
		END_CORRELATION_PRODUCTS           = 143

	#<!------------------------------------------------------------------------>
	## Send a command to start the real time monitor
	#
	#<!------------------------------------------------------------------------>
	def startMonitor( self ):
		self.monitorProductsDone = False
		self.monPacketData = None
		self.scanData = []
		self._productByIndex = {}
		#  Open a new channel for this operation.
		self.monitorChannel = self._client.newChannel( self.monitorCallback )
		#  Start the monitor associated with this channel
		self._client.sendPacket( self._client.START_DIFX_MONITOR, self._client.i.pack( socket.htonl( self.monitorChannel ) ) )
		#  Send the .input file so it can be identified
		self._client.sendChannelPacket( self.monitorChannel, self.INPUT_FILE_PATH, self._inputFile )
		
	#<!------------------------------------------------------------------------>
	## Send a command to stop the real time monitor
	#
	#  This method tells the server to close the monitor connection.  This
	#  should shut down all data collection threads.
	#<!------------------------------------------------------------------------>
	def stopMonitor( self ):
		self._client.sendChannelPacket( self.monitorChannel, self.CLOSE_CONNECTION, "" )
		
	#<!------------------------------------------------------------------------>
	## Set the FFT size used for processing real time data.
	#
	#  @param newSize FFT size used in real time processing (should be a
	#                 power of 2).
	#
	#  The real time monitoring system (currently, anyway) processes the raw
	#  DiFX visibility output to produce amplitude, phase, and lag values.  This
	#  parameter specifies the size of the FFT used to do those calculations (and
	#  whence the size of the output arrays).  Nominally the value is 64.  This
	#  value should be set before starting a job.  The value should be an
	#  integer power of 2.
	#<!------------------------------------------------------------------------>
	def fftSize( self, newSize ):
		self._client.sendChannelPacket( self.monitorChannel, self.FFT_SIZE, self._client.i.pack( socket.htonl( newSize ) ) )
		
	#<!------------------------------------------------------------------------>
	## Get data products available for real time monitoring.
	#
	#  @return Structure containing description of all monitor products (see below)
	#
	#  Return a complex structure containing the monitor products that are
	#  available with the current job, or None if a timeout occurred (which
	#  will only happen if the server or connection to the server is messed up).
	#  On the server side this information is obtained by parsing the <code>.input</code>
	#  file, so this may be a useful method call even when you are not interested
	#  in the output of the real time monitor.
	#
	#  The returned structure contains information about each scan in the job in a list
	#  of tuples - one tuple for each scan.  The following table lists the items
	#  included for each scan.
	#
	#  <table>
	#  <tr><td>Identifier			<td>String	<td>Identifier from the .input file
	#  <tr><td>Start Time			<td>Double	<td>MJD of the start of the scan
	#  <tr><td>End Time				<td>Double	<td>MJD of the end of the scan
	#  <tr><td>Source				<td>String	<td>Name of the observed source
	#  <tr><td>RA					<td>Double	<td>Source RA
	#  <tr><td>DEC					<td>Double	<td>Source DEC
	#  <tr><td>Baselines			<td>Integer	<td>Number of baselines in the observation
	#  <tr><td>Baseline Data		<td>List	<td>List of tuples containing baseline data (see below)
	#  </table>
	#
	#  Each scan includes a list of baseline data, one per baseline.  These are
	#  also tuples containing the following:
	#
	#  <table>
	#  <tr><td>Telescope 1			<td>String	<td>Two-character identifier for one of the baseline telescopes
	#  <tr><td>Telescope 2			<td>String	<td>Two-character identifier for the other baseline telescope
	#  <tr><td>Frequencies			<td>Integer	<td>Number of frequencies available
	#  <tr><td>Frequency Data		<td>List	<td>List of tuples containing frequency data (see below)
	#  <tr><td>Autocorrelation Data	<td>List	<td>List of tuples containing autocorrelation data (see below)
	#  </table>
	#
	#  For each frequency, there is a tuple of frequency data.  These data represent
	#  correlation results for both sets of telescope data.
	#
	#  <table>
	#  <tr><td>Product Number		<td>Integer	<td>Number used to uniquely identifying it when requesting real time data
	#  <tr><td>Frequency			<td>Double	<td>Frequency
	#  <tr><td>Phase Center			<td>Integer	<td>Phase center index in the event there are multiple phase centers
	#  <tr><td>Pulsar Bin			<td>Integer	<td>Pulsar bin index in the event there are multiple pulsar bins
	#  <tr><td>Polarization Product	<td>Integer	<td>Polarization index in the event there are multiple polarizations
	#  <tr><td>Offset				<td>Integer	<td>Data offset within the source data
	#  <tr><td>Frequency Channels	<td>Integer	<td>Number of frequency channels
	#  </table>
	#
	#  The autocorrelation data are available for each telescope, so there should
	#  be twice as many of these as items in the frequency data.
	#
	#  <table>
	#  <tr><td>Product Number		<td>Integer	<td>Number used to uniquely identifying it when requesting real time data
	#  <tr><td>Telescope			<td>String	<td>Telescope data used for the autocorrelation
	#  <tr><td>Frequency			<td>Double	<td>Frequency
	#  <tr><td>Offset				<td>Integer	<td>Data offset within the source data
	#  <tr><td>Frequency Channels	<td>Integer	<td>Number of frequency channels
	#  </table>
	#
	#  The "product number" of each product can be used to request a real time feed
	#  of the results for that product.  This is done using the requestMonitorProducts()
	#  method.
	#
	#  These data should be available shortly after the startMonitor() method
	#  is called.
	#
	#  A call to this method will also create a structure that can access
	#  product information using the product number.  See the productByIndex()
	#  method.
	#
	#<!------------------------------------------------------------------------>
	def getMonitorProducts( self ):
		wait = self._client._waitTime
		while wait > 0.0 and not self.monitorProductsDone:
			try:
				time.sleep( 0.01 )
			except KeyboardInterrupt:
				wait = 0.0
				return None
			wait -= 0.01
		return self.scanData

	#<!------------------------------------------------------------------------>
	## Return the current product information indexed by product number.
	#
	#  @return Dictionary of tuples containing product information (indexed by
	#          product number).
	#
	#  Return a dictionary of product information, where a "product number" key
	#  indexes a tuple of all information about a real time data product.  The
	#  tuple contains the following information (some fields may or may not
	#  apply):
	#
	#  <table>
	#  <tr><td>Autocorrelation		<td>boolean		<td>True if an autocorrelation, False if not
	#  <tr><td>Frequency			<td>double		<td>Product frequency
	#  <tr><td>Phase Center			<td>int			<td>Phase center index (not applicable to autocorrelation)
	#  <tr><td>Pulsar Bin			<td>int			<td>Pulsar bin index (not applicable to autocorrelation)
	#  <tr><td>Polarization Product	<td>int			<td>Polarization index (not applicable to autocorrelation)
	#  <tr><td>Offset				<td>int			<td>Data offset within the source data
	#  <tr><td>Frequency Channels	<td>int			<td>Number of channels (matches FFT size)
	#  <tr><td>Scan Identifier		<td>String		<td>Scan identifier from the <code>.input</code> file
	#  <tr><td>Scan Start Time		<td>double		<td>MJD of the start of the scan
	#  <tr><td>Scan End Time		<td>double		<td>MJD of the start of the scan
	#  <tr><td>Scan Source			<td>String		<td>Name of observed source
	#  <tr><td>Source RA			<td>double		<td>RA of observed source
	#  <tr><td>Source DEC			<td>double		<td>DEC of observed source	
	#  <tr><td>Telescope 1			<td>String		<td>Two character telescope code
	#  <tr><td>Telescope 2 			<td>String		<td>Two character telescope code
	#  </table>
	#<!------------------------------------------------------------------------>
	def productByIndex( self ):
		return self._productByIndex

	#<!------------------------------------------------------------------------>
	## Set callback functions for the different types of monitor data.
	#
	#  @param amplitudeCB Callback function for amplitude data.
	#  @param phaseCB Callback function for phase data.
	#  @param lagCB Callback function for lag data.
	#  @param meanAmplitudeCB Callback function for mean amplitude data.
	#  @param meanPhaseCB Callback function for mean phase data.
	#  @param meanLagCB Callback function for mean lag data.
	#
	#  Set callback functions that are triggered when different data types
	#  are received from the real time monitor.  These need not be different
	#  functions, and you don't need to set them all (just put None in for data
	#  types you don't care about).
	#
	#  When a callback occurs, access is available to data related to the
	#  callback.  These data are not guaranteed to exist after the callback
	#  function ends, so if they are needed they should be copied by the
	#  callback.  The data can be accessed by looking at the variables in this
	#  class.  The following data are available to each callback:
	#
	#  <table>
	#  <tr><td><b>Callback</b>	<td><b>Class Variable</b>		<td><b>Variable type</b>			<td><b>Description</b>
	#  <tr><td>amplitudeCB		<td>amplitudeIProduct			<td>int								<td>Product Number
	#  <tr><td>					<td>amplitudeNChannels			<td>int								<td>Number of amplitude values (matches FFT size)
	#  <tr><td>					<td>amplitudeTimeStamp			<td>int								<td>Visibilities time stamp
	#  <tr><td>					<td>amplitudeIntegrationTime	<td>int								<td>Integration time
	#  <tr><td>					<td>amplitude					<td>double[amplitudeNChannels]		<td>segment amplitude values
	#  <tr><td>phaseCB			<td>phaseIProduct				<td>int								<td>Product Number
	#  <tr><td>					<td>phaseNChannels				<td>int								<td>Number of phase values (matches FFT size)
	#  <tr><td>					<td>phaseTimeStamp				<td>int								<td>Visibilities time stamp
	#  <tr><td>					<td>phaseIntegrationTime		<td>int								<td>Integration time
	#  <tr><td>					<td>phase						<td>double[phaseNChannels]			<td>segment phase values
	#  <tr><td>lagCB			<td>lagIProduct					<td>int								<td>Product Number
	#  <tr><td>					<td>lagHalfSize					<td>int								<td>Half the number of lag values
	#  <tr><td>					<td>lagTimeStamp				<td>int								<td>Visibilities time stamp
	#  <tr><td>					<td>lagIntegrationTime			<td>int								<td>Integration time
	#  <tr><td>					<td>lagMaxChannel				<td>int								<td>Index of peak lag value
	#  <tr><td>					<td>lagDelay					<td>double							<td>Delay time represented by peak lag value
	#  <tr><td>					<td>lagSnr						<td>double							<td>Signal to noise of peak lag value
	#  <tr><td>					<td>lag							<td>double[2 * lagHalfTime]			<td>segment lag values
	#  <tr><td>meanAmplitudeCB	<td>meanAmplitudeIProduct		<td>int								<td>Product Number
	#  <tr><td>					<td>meanAmplitudeNChannels		<td>int								<td>Number of amplitude values (matches FFT size)
	#  <tr><td>					<td>meanAmplitudeTimeStamp		<td>int								<td>Most recent visibilities time stamp
	#  <tr><td>					<td>meanAmplitudeIntegrationTime<td>int								<td>Integration time
	#  <tr><td>					<td>meanAmplitude				<td>double[meanAmplitudeNChannels]	<td>scan cumulative amplitude values
	#  <tr><td>meanPhaseCB		<td>meanPhaseIProduct			<td>int								<td>Product Number
	#  <tr><td>					<td>meanPhaseNChannels			<td>int								<td>Number of phase values (matches FFT size)
	#  <tr><td>					<td>meanPhaseTimeStamp			<td>int								<td>Most recent visibilities time stamp
	#  <tr><td>					<td>meanPhaseIntegrationTime	<td>int								<td>Integration time
	#  <tr><td>					<td>meanPhase					<td>double[meanPhaseNChannels]		<td>scan cumulative phase values
	#  <tr><td>meanLagCB		<td>meanLagIProduct				<td>int								<td>Product Number
	#  <tr><td>					<td>meanLagHalfSize				<td>int								<td>Half the number of lag values
	#  <tr><td>					<td>meanLagTimeStamp			<td>int								<td>Most recent visibilities time stamp
	#  <tr><td>					<td>meanLagIntegrationTime		<td>int								<td>Integration time
	#  <tr><td>					<td>meanLagMaxChannel			<td>int								<td>Index of peak lag value
	#  <tr><td>					<td>meanLagDelay				<td>double							<td>Delay time represented by peak lag value
	#  <tr><td>					<td>meanLagSnr					<td>double							<td>Signal to noise of peak lag value
	#  <tr><td>					<td>meanLag						<td>double[2 * meanLagHalfTime]		<td>scan cumulative lag values
	#  </table>
	#
	#<!------------------------------------------------------------------------>
	def monitorDataCallbacks( self, amplitudeCB, phaseCB, lagCB, meanAmplitudeCB, meanPhaseCB, meanLagCB ):
		self.amplitudeCB = amplitudeCB
		self.phaseCB = phaseCB
		self.lagCB = lagCB
		self.meanAmplitudeCB = meanAmplitudeCB
		self.meanPhaseCB = meanPhaseCB
		self.meanLagCB = meanLagCB
		
	#<!------------------------------------------------------------------------>
	## Callback function for a "start monitor" command response.
	#
	#  @param data Data transmitted by the server
	#
	#  This method responds to data from the real time monitoring function.
	#  The process is triggered in the startMonitor() method.
	#
	#<!------------------------------------------------------------------------>
	def monitorCallback( self, data ):
		#  The data from this callback can consist of one of three components
		#  as part of the server packet protocol - these are an ID, a data length,
		#  and data itself.  The packetProtocol function gets this for us.
		self.monPacketData = self._client.packetProtocol( data, self.monPacketData )
		if self.monPacketData[3]: #  Indicates whether packet is complete
			packetId = self.monPacketData[0]
			if packetId == self.MESSAGE:
				self.message( self.monPacketData[2] )
			elif packetId == self.WARNING:
				self.warning( self.monPacketData[2] )
			elif packetId == self.ERROR:
				self.error( self.monPacketData[2] )
			elif packetId == self.NUM_BASELINES:
				self.scanBaselines = socket.ntohl( self._client.i.unpack( self.monPacketData[2] )[0] )
				#  Number of baselines is the last item in a scan.  Create a new scan
				#  entry.
				self.baselineData = []
				self.productRequestData = {}
				self.autoCorrelation = None
				self.scanData.append( ( self.scanIdentifier, self.scanStartTime, self.scanEndTime, self.scanSource, self.scanSourceRA, self.scanSourceDEC, self.scanBaselines, self.baselineData ) ) 
			elif packetId == self.NUM_FREQUENCIES:
				self.frequencies = socket.ntohl( self._client.i.unpack( self.monPacketData[2] )[0] )
				#  Create a new frequency entry
				self.frequencyData = []
				self.autoCorrelationData = []
				self.baselineData.append( ( self.telescope1, self.telescope2, self.frequencies, self.frequencyData, self.autoCorrelationData ) )
			elif packetId == self.BASELINE:
				self.baseline = socket.ntohl( self._client.i.unpack( self.monPacketData[2] )[0] )
			elif packetId == self.FREQUENCY:
				self.frequency = self._client.d.unpack( self.monPacketData[2] )[0]
			elif packetId == self.NUM_SCANS:
				self.numScans = socket.ntohl( self._client.i.unpack( self.monPacketData[2] )[0] )
			elif packetId == self.SCAN:
				self.scanNum = socket.ntohl( self._client.i.unpack( self.monPacketData[2] )[0] )
			elif packetId == self.TELESCOPE_1:
				self.telescope1 = self.monPacketData[2]
			elif packetId == self.TELESCOPE_2 :
				self.telescope2 = self.monPacketData[2]
			elif packetId == self.BEGIN_CORRELATION_PRODUCTS:
				pass
			elif packetId == self.NUM_PHASE_CENTERS:
				pass
			elif packetId == self.PHASE_CENTER:
				self.phaseCenter = socket.ntohl( self._client.i.unpack( self.monPacketData[2] )[0] )
			elif packetId == self.NUM_PULSAR_BINS:
				pass
			elif packetId == self.PULSAR_BIN:
				self.pulsarBin = socket.ntohl( self._client.i.unpack( self.monPacketData[2] )[0] )
			elif packetId == self.NUM_POL_PRODUCTS:
				pass
			elif packetId == self.POL_PRODUCT:
				self.polProduct = socket.ntohl( self._client.i.unpack( self.monPacketData[2] )[0] )
			elif packetId == self.NEW_PRODUCT:
				self.productN = socket.ntohl( self._client.i.unpack( self.monPacketData[2][:4] )[0] )
				self.resultIndex = socket.ntohl( self._client.i.unpack( self.monPacketData[2][4:8] )[0] )
				self.frequencyChannels = socket.ntohl( self._client.i.unpack( self.monPacketData[2][8:12] )[0] )
				self.productRequestData[self.productN] = ( self.resultIndex, self.frequencyChannels )
				if self.autoCorrelation == None:
					self.frequencyData.append( ( self.productN, self.frequency, self.phaseCenter, self.pulsarBin, self.polProduct, self.resultIndex, self.frequencyChannels ) )
					self._productByIndex[self.productN] = ( False, self.frequency, self.phaseCenter, self.pulsarBin, self.polProduct, self.resultIndex, self.frequencyChannels, self.scanIdentifier, self.scanStartTime, self.scanEndTime, self.scanSource, self.scanSourceRA, self.scanSourceDEC, self.telescope1, self.telescope2 )
				else:
					self.autoCorrelationData.append( ( self.productN, self.autoCorrelation, self.frequency, self.resultIndex, self.frequencyChannels ) )
					self._productByIndex[self.productN] = ( True, self.frequency, 0, 0, 0, self.resultIndex, self.frequencyChannels, self.scanIdentifier, self.scanStartTime, self.scanEndTime, self.scanSource, self.scanSourceRA, self.scanSourceDEC, self.telescope1, self.telescope2 )
				self.autoCorrelation = None
			elif packetId == self.AUTOCORRELATION:
				self.autoCorrelation = self.monPacketData[2]
			elif packetId == self.VISIBILITY_DATA:
				pass
			elif packetId == self.AMPLITUDE_DATA:
				#  Amplitude data consist of four integers (one of which is the number of
				#  channels) and then nChannels "doubles" in 14-character strings.
				self.amplitudeIProduct = socket.ntohl( self._client.i.unpack( self.monPacketData[2][0:4] )[0] )
				self.amplitudeNChannels = socket.ntohl( self._client.i.unpack( self.monPacketData[2][4:8] )[0] )
				self.amplitudeTimeStamp = socket.ntohl( self._client.i.unpack( self.monPacketData[2][8:12] )[0] )
				self.amplitudeIntegrationTime = socket.ntohl( self._client.i.unpack( self.monPacketData[2][12:16] )[0] )
				self.amplitude = []
				for index in range( self.amplitudeNChannels ):
					ampStr = self.monPacketData[2][16 + index * 14: 16 + index * 14 + 14]
					if len( ampStr.strip() ) > 0:
						self.amplitude.append( float( self.monPacketData[2][16 + index * 14: 16 + index * 14 + 14] ) )
					else:
						self.amplitude.append( 0.0 )
				if self.amplitudeCB != None:
					self.amplitudeCB()
			elif packetId == self.PHASE_DATA:
				#  Phase data are similar to amplitude data
				self.phaseIProduct = socket.ntohl( self._client.i.unpack( self.monPacketData[2][0:4] )[0] )
				self.phaseNChannels = socket.ntohl( self._client.i.unpack( self.monPacketData[2][4:8] )[0] )
				self.phaseTimeStamp = socket.ntohl( self._client.i.unpack( self.monPacketData[2][8:12] )[0] )
				self.phaseIntegrationTime = socket.ntohl( self._client.i.unpack( self.monPacketData[2][12:16] )[0] )
				self.phase = []
				for index in range( self.phaseNChannels ):
					phaseStr = self.monPacketData[2][16 + index * 14: 16 + index * 14 + 14]
					if len( phaseStr.strip() ) > 0:
						self.phase.append( float( self.monPacketData[2][16 + index * 14: 16 + index * 14 + 14] ) )
					else:
						self.phase.append( 0.0 )
				if self.phaseCB != None:
					self.phaseCB()
			elif packetId == self.LAG_DATA:
				#  Lag data have a few extra parameters, including the index of the maximum value,
				#  and the computed delay and snr.
				self.lagIProduct = socket.ntohl( self._client.i.unpack( self.monPacketData[2][0:4] )[0] )
				self.lagHalfSize = socket.ntohl( self._client.i.unpack( self.monPacketData[2][4:8] )[0] )
				self.lagTimeStamp = socket.ntohl( self._client.i.unpack( self.monPacketData[2][8:12] )[0] )
				self.lagIntegrationTime = socket.ntohl( self._client.i.unpack( self.monPacketData[2][12:16] )[0] )
				self.lagMaxChannel = socket.ntohl( self._client.i.unpack( self.monPacketData[2][16: 20] )[0] )
				self.lagDelay = float( self.monPacketData[2][20:34] )
				self.lagSnr = float( self.monPacketData[2][34:48] )
				self.lag = []
				for index in range( 2 * self.lagHalfSize ):
					lagStr = self.monPacketData[2][48 + index * 14: 48 + index * 14 + 14]
					if len( lagStr.strip() ) > 0:
						self.lag.append( float( self.monPacketData[2][48 + index * 14: 48 + index * 14 + 14] ) )
					else:
						self.lag.append( 0.0 )
				if self.lagCB != None:
					self.lagCB()
			elif packetId == self.END_VISIBILITY_BLOCK:
				pass
			elif packetId == self.JOB_NAME:
				self.jobName = self.monPacketData[2]
			elif packetId == self.OBS_CODE:
				self.obsCode = self.monPacketData[2]
			elif packetId == self.SCAN_IDENTIFIER:
				self.scanIdentifier = self.monPacketData[2]
			elif packetId == self.SCAN_START_TIME:
				self.scanStartTime = self.monPacketData[2]
			elif packetId == self.SCAN_END_TIME:
				self.scanEndTime = self.monPacketData[2]
			elif packetId == self.SOURCE:
				self.scanSource = self.monPacketData[2]
			elif packetId == self.SOURCE_RA:
				self.scanSourceRA = self.monPacketData[2]
			elif packetId == self.SOURCE_DEC:
				self.scanSourceDEC = self.monPacketData[2]
			elif packetId == self.VISIBILITY_SCAN:
				self.visibilityScan = self.monPacketData[2]
			elif packetId == self.MEAN_AMPLITUDE_DATA:
				#  Amplitude data consist of four integers (one of which is the number of
				#  channels) and then nChannels "doubles" in 14-character strings.
				self.meanAmplitudeIProduct = socket.ntohl( self._client.i.unpack( self.monPacketData[2][0:4] )[0] )
				self.meanAmplitudeNChannels = socket.ntohl( self._client.i.unpack( self.monPacketData[2][4:8] )[0] )
				self.meanAmplitudeTimeStamp = socket.ntohl( self._client.i.unpack( self.monPacketData[2][8:12] )[0] )
				self.meanAmplitudeIntegrationTime = socket.ntohl( self._client.i.unpack( self.monPacketData[2][12:16] )[0] )
				self.meanAmplitude = []
				for index in range( self.meanAmplitudeNChannels ):
					ampStr = self.monPacketData[2][16 + index * 14: 16 + index * 14 + 14]
					if len( ampStr.strip() ) > 0:
						self.meanAmplitude.append( float( self.monPacketData[2][16 + index * 14: 16 + index * 14 + 14] ) )
					else:
						self.meanAmplitude.append( 0.0 )
				if self.meanAmplitudeCB != None:
					self.meanAmplitudeCB()
			elif packetId == self.MEAN_PHASE_DATA:
				#  Phase data are similar to amplitude data
				self.meanPhaseIProduct = socket.ntohl( self._client.i.unpack( self.monPacketData[2][0:4] )[0] )
				self.meanPhaseNChannels = socket.ntohl( self._client.i.unpack( self.monPacketData[2][4:8] )[0] )
				self.meanPhaseTimeStamp = socket.ntohl( self._client.i.unpack( self.monPacketData[2][8:12] )[0] )
				self.meanPhaseIntegrationTime = socket.ntohl( self._client.i.unpack( self.monPacketData[2][12:16] )[0] )
				self.meanPhase = []
				for index in range( self.meanPhaseNChannels ):
					phaseStr = self.monPacketData[2][16 + index * 14: 16 + index * 14 + 14]
					if len( phaseStr.strip() ) > 0:
						self.meanPhase.append( float( self.monPacketData[2][16 + index * 14: 16 + index * 14 + 14] ) )
					else:
						self.meanPhase.append( 0.0 )
				if self.meanPhaseCB != None:
					self.meanPhaseCB()
			elif packetId == self.MEAN_LAG_DATA:
				#  Lag data have a few extra parameters, including the index of the maximum value,
				#  and the computed delay and snr.
				self.meanLagIProduct = socket.ntohl( self._client.i.unpack( self.monPacketData[2][0:4] )[0] )
				self.meanLagHalfSize = socket.ntohl( self._client.i.unpack( self.monPacketData[2][4:8] )[0] )
				self.meanLagTimeStamp = socket.ntohl( self._client.i.unpack( self.monPacketData[2][8:12] )[0] )
				self.meanLagIntegrationTime = socket.ntohl( self._client.i.unpack( self.monPacketData[2][12:16] )[0] )
				self.meanLagMaxChannel = socket.ntohl( self._client.i.unpack( self.monPacketData[2][16: 20] )[0] )
				self.meanLagDelay = float( self.monPacketData[2][20:34] )
				self.meanLagSnr = float( self.monPacketData[2][34:48] )
				self.meanLag = []
				for index in range( 2 * self.meanLagHalfSize ):
					lagStr = self.monPacketData[2][48 + index * 14: 48 + index * 14 + 14]
					if len( lagStr.strip() ) > 0:
						self.meanLag.append( float( self.monPacketData[2][48 + index * 14: 48 + index * 14 + 14] ) )
					else:
						self.meanLag.append( 0.0 )
				if self.meanLagCB != None:
					self.meanLagCB()
			elif packetId == self.END_CORRELATION_PRODUCTS:
				self.monitorProductsDone = True
		else:
			pass
	
	#<!------------------------------------------------------------------------>
	## Request a list of products from the real time monitor.
	#
	#  @param productList List of integer "product numbers"
	#
	#  Request a list of "product numbers" for real time monitoring data from
	#  the server.  The product numbers (and information about their associated
	#  products) are obtained with the getMonitorProducts() method.
	#
	#<!------------------------------------------------------------------------>
	def requestProducts( self, productList ):
		if len( productList ) > 0:
			self._client.sendChannelPacket( self.monitorChannel, self.START_PRODUCT_REQUESTS, "" )
			for product in productList:
				sendData = self._client.i.pack( socket.htonl( product ) )
				try:
					sendData += self._client.i.pack( socket.htonl( self.productRequestData[product][0] ) )
					sendData += self._client.i.pack( socket.htonl( self.productRequestData[product][1] ) )
					self._client.sendChannelPacket( self.monitorChannel, self.PRODUCT_REQUEST, sendData )
				except:
					print "product", product, "was not found"
			self._client.sendChannelPacket( self.monitorChannel, self.END_PRODUCT_REQUESTS, "" )
	
	
	
