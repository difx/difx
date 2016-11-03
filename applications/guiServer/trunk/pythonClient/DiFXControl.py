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
## @namespace DiFXControl
#  some detail
#
#  more detail
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
import DiFXFileTransfer

#<!---======================================================================--->
##  Thread to monitor a socket from the DiFX server.
#
#  The MonitorThread class provides a threaded receiver for the "packet protocol"
#  of the DiFX server.  Given a (connected) socket, it will consume complete
#  packets as they are received and alert callback functions (set by methods
#  setCallback() and setFailCallback()) when new data or failures are detected.
#
#  MonitorThread inherits the Thread class, so it needs to be started like a
#  normal thread.  The following code shows a typical use:
#
#  \code{.py}
#    #  Create an instance of MonitorThread with an existing socket.
#    monitorThread = MonitorThread( mySocket )
#
#    #  Set callbacks with defined functions/methods that are to be called when
#    #  new packets are received or socket failures occur.  See the method
#    #  documentation for what the callback functions should look like, and
#    #  the DiFXControl.Client class for examples.  If you don't set the callback
#    #  functions the thread will still run, but it won't do anything useful
#    #  and will consume and throw away incoming data.
#    monitorThread.setCallback( myPacketCallback )
#    monitorThread.setFailCallback( myFailCallback )
#
#    #  Start the thread.
#    monitorThread.start()
#  \endcode
#
#  MonitorThread was created to be used by the DiFXControl.Client class, but it
#  may have other uses.
#
#  <h3>Non-Threaded MonitorThread</h3>
#
#  MonitorThread is also, oddly enough, useful in a non-threaded mode.  If run as a thread
#  it will be continually waiting on the <i>select()</i> line in the newPacket()
#  method.  This might be annoying for other threaded functionality, or if you
#  wish to run your own <i>select()</i> call.  An example of such a use is in the
#  DiFXBusy application (when running with the Fltk GUI) - it has its own <i>select()</i>
#  function (part of the Fltk package) that triggers a callback whereupon the
#  newPacket() method of this class is called (including another <i>select()</i>, which
#  returns immediately).
#
#<!---======================================================================--->
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

    #<!------------------------------------------------------------------------>
    ## Main loop of the thread.
    #
    #  This is the main loop of the thread, triggered by a "start()" function call.
    #  This loop reads packets until something breaks (which might be caused by
    #  a user stop() call).  Beyond looking for such trouble, this method simply
    #  calls the newPacket() method repeatedly.
    #<!------------------------------------------------------------------------>
    def run( self ):
        self.status = self.ok
        while self.status == self.ok:
            self.newPacket()

    #<!------------------------------------------------------------------------>
    ## Packet protocol interpreter.
    #
    #  Read a single data packet by interpreting the the low-level
    #  packet protocol of the difxServer, which follows the pattern:
    #  \code{.py}
    #  int packetID      #  4-byte integer, network byte order
    #  int packetSize    #  4-byte integer, network byte order
    #  string packetData #  "packetSize" bytes in length
    #  \endcode
    #  See the DiFXControl.Client method for a list of packetIDs.
    #
    #  Some effort is made to detect broken sockets and
    #  identify at what stage of the process they were broken - a broken
    #  socket will trigger a call to the "fail callback" if the user has set
    #  one (see setFailCallback()).
    #
    #  When (proper) packet data are received, a call is made to the "callback"
    #  function, if the user has set one (see setCallback()).
    #
    #  This is designed as an internal function, called repeatedly by the endless loop in
    #  the main thread run() method.  However you can call it on your own if you
    #  wish to consume packet data.
    #<!------------------------------------------------------------------------>
    def newPacket( self ):
        iwtd, owtd, ewtd = select.select( [self.sock], [], [], .05 )
        if len( iwtd ) > 0:
            #  This should be a packet ID, or (possibly) an indication
            #  that the socket has crapped out.
            try:
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
            except:
                self.status = self.noConnection
            if self.status != self.ok:
                #  Socket failure.  Bail out, do the failure callback.
                if self.failCallback != None:
                    self.failCallback()

    #<!------------------------------------------------------------------------>
    ## Stops the thread.
    #
    #  This method should stop the main thread.  It has a "sleep" that is twice the timeout of the
    #  select call in the "run" portion of the thread to give that
    #  endlessly-looping thread a chance to play out.
    #<!------------------------------------------------------------------------>
    def stop( self ):
        self.status = self.stopped
        time.sleep( 0.1 )
        
    #<!------------------------------------------------------------------------>
    ## Set a callback for received packet data.
    #
    #     @param callbackFunction The function that will be called when a packet
    #                             of data is received.
    #
    #  Set the callback function for received packet data.  The function should
    #  take two arguments, an integer "packet ID", and the data associated with
    #  it (a string).  A sample use is below:
    #
    #  \code{.py}
    #   def myPacketCallback( self, packetId, data ):
    #       print "received packet ID", packetId
    #       print "its length in bytes is", len( data )
    #
    #   ...
    #   monitor.setCallback( myPacketCallback )
    #  \endcode
    #
    #<!------------------------------------------------------------------------>
    def setCallback( self, callbackFunction ):
        self.cb = callbackFunction
        
    #<!------------------------------------------------------------------------>
    ## Set a callback for socket failures.
    #
    #     @param callbackFunction The function that will be called when a socket
    #                             failure is detected.
    #
    #  Set the callback function for socket failures - the function should take
    #  no arguments.
    #  If you don't use this, the socket will
    #  simply stop on a failure.
    #<!------------------------------------------------------------------------>
    def setFailCallback( self, callbackFunction ):
        self.failCallback = callbackFunction

#<!---======================================================================--->
## Class for creating and manipulating a client connection to the DiFX server.
#
#  The Client class makes a TCP connection to a server at a specified address and
#  port and provides methods for controlling and receiving feedback from DiFX
#  processes.  It is the sole provider of connections between all DiFX Python
#  Interface classes and applications and the DiFX server.
#
#  The DiFX server provides constant status data, as well as feedback from commands
#  issued to it.  These are collected and parsed by an instance of the MonitorThread
#  class if the monitor() method is called.
#
#  A code snippet below shows how to use the Client class to make a connection
#  to a DiFX server:
#
#  \code{.py}
#  #  Create Client class instance
#  difx = DiFXControl.Client()
#
#  #  Connect to host "localhost" at port 50400.  See the connect() documentation
#  #  for default values for host and port.  Note these are passed as a tuple
#  #  (i.e. a single argument to connect())!
#  difx.connect( ( "localhost", 50401 ) )
#
#  #  Start a MonitorThread for incoming messages
#  difx.monitor()
#  \endcode
#
#  When you are done with the client, close it:
#
#  \code{.py}
#  difx.close()
#  \endcode
#
#  <h4>Other Details</h4>
#
#  This class has some structures that allow some of the inheriting classes to
#  accomplish their work when dealing with the server, in particular to handle
#  indeterminate response times.  
#
#  A "wait time" is included, which can be set by the
#  waitTime() method (it is 5 seconds by default).  Inheriting classes can use
#  this time to "time out" when waiting for the server to do something.
#
#  There are three callback structures, "interval", "final" and "timeout" for dealing with
#  server tasks that accomplish tasks in a bunch of steps, each of which the
#  calling class might be interested in.  The calling class sets up an interval callback using
#  the intervalCallback() method, after defining a function (that can have zero
#  or one arguments):
#
#  \code{.py}
#  #  Define a callback with an optional argument
#  def myIntervalCallback( self, myArg ):
#      print "still working, argument is", myArg
#
#  #  Set this to be the "interval" callback
#  client.intervalCallback( myIntervalCallback )
#  \endcode
#
#  Within the method this calling class sets up to respond to the server, it tells
#  the client to do that callback...
#
#  \code{.py}
#  #  Got something from the server...
#  
#  #  Send the interval callback
#  client.doIntervalCallback( "some % complete" )
#  \endcode
#
#  The "final" callback structure is identical, but it is meant to be used only
#  once when a task is complete.  
#
#  The "timeout" callback structure, also identical,
#  is meant to be called when the server fails to respond in the "wait time"
#  interval.
#
#  Use of these structures is completely optional - inheriting or calling classes
#  can set up their own ways of doing these things, or they can use the methods
#  in ways they were not intended.  The structures are included in this
#  class merely as a way to avoid repetitive typing.
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
        self.d = struct.Struct( "!d" )
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
        self._finalCallback = None
        self._intervalCallback = None
        self._timeoutCallback = None
        self._messageCallback = None
        self._warningCallback = None
        self._errorCallback = None
        
    #<!------------------------------------------------------------------------>
    ## Set the global wait time
    #
    #  @param newVal Time (in seconds) that the client will wait for commands to
    #                respond.
    #
    #  A number of methods in this class, as well as inheriting classes, require
    #  a pause to allow the server to respond to an instruction.  The global
    #  wait time provides an amount of time that these methods can wait before
    #  they give up and assume the server is wedged.  Of course there is no
    #  requirement that this wait time be used - it is merely a useful
    #  utility.  Defaults is 5.0 seconds.
    #<!------------------------------------------------------------------------>
    def waitTime( self, newVal ):
        self._waitTime = newVal
        
    #<!------------------------------------------------------------------------>
    ## Set the final callback
    #
    #  @param callback A callback function that can take one or no arguments.
    #
    #  Set the "final" callback function - this will be called when something
    #  final happens - a task completes, or fails.  The doFinalCallback() method
    #  is used to make the callback - it can be called with an argument if the 
    #  callback function expects one.
    #<!------------------------------------------------------------------------>
    def finalCallback( self, callback ):
        self._finalCallback = callback

    #<!------------------------------------------------------------------------>
    ## Set the interval callback
    #
    #  @param callback A callback function that can take one or no arguments.
    #
    #  Set the "interval" callback function - this will be called when a task
    #  reaches an interval stage that the calling program wants to recognize.
    #  The doIntervalCallback() method is used to make the callback - it can
    #  be called with an argument if the callback function expects one.
    #<!------------------------------------------------------------------------>
    def intervalCallback( self, callback ):
        self._intervalCallback = callback

    #<!------------------------------------------------------------------------>
    ## Set the timeout callback
    #
    #  @param callback A callback function that can take one or no arguments.
    #
    #  Set the "timeout" callback function - this will be called when a task
    #  decides that the server has not responded in sufficient time (possibly
    #  using the "wait time").
    #  The doTimeoutCallback() method is used to make the callback - it can
    #  be called with an argument if the callback function expects one.
    #<!------------------------------------------------------------------------>
    def timeoutCallback( self, callback ):
        self._timeoutCallback = callback

    #<!------------------------------------------------------------------------>
    ## Set the message callback
    #
    #  @param callback A callback function that expects a single string argument.
    #
    #  Set the "message" callback function - this will be called when a task
    #  has processing information messages.  The doMessageCallback() method
    #  can be used to trigger the callback.
    #<!------------------------------------------------------------------------>
    def messageCallback( self, callback ):
        self._messageCallback = callback

    #<!------------------------------------------------------------------------>
    ## Set the warning callback
    #
    #  @param callback A callback function that expects a single string argument.
    #
    #  Set the "warning" callback function - this will be called when a task
    #  bumps into some problem that does not cause the task to fail but that
    #  that might indicate an important issue.  The doWarningCallback() method
    #  can be used to trigger the callback.
    #<!------------------------------------------------------------------------>
    def warningCallback( self, callback ):
        self._warningCallback = callback

    #<!------------------------------------------------------------------------>
    ## Set the error callback
    #
    #  @param callback A callback function that expects a single string argument.
    #
    #  Set the "error" callback function - this will be called when a task
    #  encounteres a problem that is probably fatal.  The doErrorCallback() method
    #  can be used to trigger the callback.
    #<!------------------------------------------------------------------------>
    def errorCallback( self, callback ):
        self._errorCallback = callback

    #<!------------------------------------------------------------------------>
    ## Execute the final callback function
    #
    #  @param finalArg An argument that will be passed to the callback function.
    #                  Optional - only use it if the callback accepts it!
    #
    #  If the user has set a final callback function execute it here.  This
    #  method is (usually) called when a server task is complete.
    #<!------------------------------------------------------------------------>
    def doFinalCallback( self, finalArg = None ):
        if self._finalCallback != None:
            if finalArg != None:
                self._finalCallback( finalArg )
            else:
                self._finalCallback()       
        
    #<!------------------------------------------------------------------------>
    ## Execute the interval callback function
    #
    #  @param intervalArg An argument that will be passed to the callback function.
    #                     Optional - only use it if the callback accepts it!
    #
    #  If the user has set an interval callback function execute it here.  This
    #  method is meant to be called when a task completes some stage that the
    #  initiating program wants to know about.  The argument is optional, but
    #  should be used if the callback function expects it.
    #<!------------------------------------------------------------------------>
    def doIntervalCallback( self, intervalArg = None ):
        if self._intervalCallback != None:
            if intervalArg != None:
                self._intervalCallback( intervalArg )
            else:
                self._intervalCallback()        
        
    #<!------------------------------------------------------------------------>
    ## Execute the timeout callback function
    #
    #  @param timeoutArg An argument that will be passed to the callback function.
    #                     Optional - only use it if the callback accepts it!
    #
    #  If the user has set an interval callback function execute it here.  This
    #  method is meant to be called when a task completes some stage that the
    #  initiating program wants to know about.  The argument is optional, but
    #  should be used if the callback function expects it.
    #<!------------------------------------------------------------------------>
    def doTimeoutCallback( self, timeoutArg = None ):
        if self._timeoutCallback != None:
            if timeoutArg != None:
                self._timeoutCallback( timeoutArg )
            else:
                self._timeoutCallback()     
        
    #<!------------------------------------------------------------------------>
    ## Execute the message callback function
    #
    #  @param arg A string argument containing the message that will be passed
    #             to the callback function.
    #
    #  If the user has set a message callback function execute it here.
    #<!------------------------------------------------------------------------>
    def message( self, arg ):
        if self._messageCallback != None:
            self._messageCallback( arg )        
        
    #<!------------------------------------------------------------------------>
    ## Execute the warning callback function
    #
    #  @param arg A string argument containing the warning that will be passed
    #             to the callback function.
    #
    #  If the user has set a warning callback function execute it here.
    #<!------------------------------------------------------------------------>
    def warning( self, arg ):
        if self._warningCallback != None:
            self._warningCallback( arg )        
        
    #<!------------------------------------------------------------------------>
    ## Execute the error callback function
    #
    #  @param arg A string argument containing the error that will be passed
    #             to the callback function.
    #
    #  If the user has set an error callback function execute it here.
    #<!------------------------------------------------------------------------>
    def error( self, arg ):
        if self._errorCallback != None:
            self._errorCallback( arg )      
        
    #<!------------------------------------------------------------------------>
    ##  Generate a unique "channel" number for channeled communication and assign
    #  a callback function to it.
    #
    #  @param callback A callback function used when channel data associated with
    #                  the new channel are encountered (<i>None</i> is permitted).
    #  @return Integer channel number
    #
    #  This function assigns a callback function to a specific "channel number",
    #  and returns the channel number.  The channel number is
    #  a unique identifier that allows the incoming channeled data tagged with it
    #  by <i>guiServer</i> to go to the correct callback.  This channel number
    #  is passed to <i>guiServer</i> by various commands that require data
    #  feedback.
    #
    #  Don't worry about confusing channel numbers between client connections -
    #  they only need to be unique within the client connection.
    #<!------------------------------------------------------------------------>
    def newChannel( self, callback ):
        self.channelPool += 1
        self.channelCallbacks[self.channelPool] = callback
        return self.channelPool
        
    #<!------------------------------------------------------------------------>
    ## Utility function for interpreting the server packet protocol.
    #
    #    @param data A new piece of data to add to the packet.
    #    @param packet A (possibly partial) packet in the form of a tuple.
    #    @return Tuple containing packet ID, packet length, packet data, and
    #            (True/False) whether or not the packet is complete.
    #
    #  This is an internally-used function that interprets given data
    #  as part of the server packet protocol.  In this protocol 
    #  an integer packet ID is followed by an integer data length followed by
    #  a bunch of data when that length is non-zero.  A tuple consisting of the
    #  packet ID, the length, the data, and a boolean indicating whether the
    #  packet is complete is returned.  This tuple can be used as an
    #  argument to a subsequent call to this method if the packet is not complete.
    #
    #  The use of the tuple (instead of class variables) eliminates problems
    #  where multiple operations step on each other when interpreting the
    #  protocol simultaneously (trust me, this happens).
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
        
    #<!------------------------------------------------------------------------>
    ##  Cleans up a "channel" connection when it is no longer needed.
    #
    #  @param channelNum The channel that is to be closed.
    #
    #  This function should be called after all operations on a channel connection
    #  have been completed and it is no longer needed.  See the DiFXControl.Client.newChannel()
    #  method for more information about channels.
    #<!------------------------------------------------------------------------>
    def closeChannel( self, channelNum ):
        try:
            del self.channelCallbacks[channelNum]
        except KeyError:
            #  Making this error silent allows us to redundantly close channels
            #  "just to make sure" they are closed.
            #print "no such channel key: " + str( channelNum )
            pass
        
    #<!------------------------------------------------------------------------>
    ##  Add a new callback for regular packet data.
    #
    #  @param newcb Function called when packet data are detected.
    #
    #  Add a callback function for packet data.  The function should expect
    #  and integer packet ID and string packet data as arguments.
    #<!------------------------------------------------------------------------>
    def addAllCallback( self, newcb ):
        self.allCallbacks.append( newcb )
        
    #<!------------------------------------------------------------------------>
    ## Add a callback for relay messages
    #
    #   @param newcb Function to be called when a relay data are detected
    #
    #  Add a function to those called when a UDP broadcast message is "relayed" by 
    #  <i>guiServer</i>.  The function should accept a string argument - which will
    #  contain the relayed data.
    #<!------------------------------------------------------------------------>
    def addRelayCallback( self, newcb ):
        self.relayCallbacks.append( newcb )
        
    #<!------------------------------------------------------------------------>
    ##  Add a callback for any sort of failure.
    #
    #   @param newcb Function to be called when a failure occurs.
    #
    #   The Client class maintains a list of functions that will be called
    #   when a failure (probably a connection problem) occurs.  These will be
    #   called in the order they are added.  The function should not expect any
    #   arguments.
    #<!------------------------------------------------------------------------>
    def addFailCallback( self, newcb ):
        self.failCallbacks.append( newcb )

    #<!------------------------------------------------------------------------>
    ##Make a client connection to the DiFX server.
    #
    #  @param host String containing the hostname of the server (optional)
    #  @param port Integer containing the TCP port for connections on the server (optional).
    #
    #  Connect will attempt to connect to a host and port by constructing a list for the
    #  socket "connect()" function.  If not provided with a host and port, it will
    #  attempt to get them from environment variables, and, failing that, will use
    #  some defaults.
    #<!------------------------------------------------------------------------>
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
            
    #<!------------------------------------------------------------------------>
    ##Create an instance of the monitor thread but don't start it.
    #
    #  Create an instance of the monitor thread so we can use it to read
    #  packets, but don't start it - so its basically non-threaded.  This is
    #  useful if we have an external thread that does a select on the socket
    #  (which I have to do when using the fltk GUI stuff).
    #
    #<!------------------------------------------------------------------------>
    def passiveMonitor( self ):
        self.monitorThread = MonitorThread( self.sock )
        self.monitorThread.setCallback( self.packetCallback )
        self.monitorThread.setFailCallback( self.failCallback )
        self.channelData()

    #<!------------------------------------------------------------------------>
    ##Activate a thread that will monitor incoming data.
    #
    #  Set up a MonitorThread class instance to collect data from the server.
    #  Callback functions for new data (packetCallback()) and failures (failCallback())
    #  are also set up.
    #
    #  This method starts the thread.  If you don't wish to do that, the
    #  passiveMonitor() method can be used.
    #
    #<!------------------------------------------------------------------------>
    def monitor( self ):
        self.monitorThread = MonitorThread( self.sock )
        self.monitorThread.setCallback( self.packetCallback )
        self.monitorThread.setFailCallback( self.failCallback )
        self.monitorThread.start()
        self.channelData()

    #<!------------------------------------------------------------------------>
    ## Close the socket client socket connection.  
    #
    #  If there is a monitoring thread, stop it first.
    #<!------------------------------------------------------------------------>
    def close( self ):
        if self.monitorThread != None:
            self.monitorThread.stop()
            self.bytesReceived = self.monitorThread.bytesReceived
        if self.socketOK:
            try:
                self.sock.shutdown( socket.SHUT_RDWR )
            except:
                #  Ignore errors
                pass
        self.sock.close()

    #<!------------------------------------------------------------------------>
    ## Send a packet along with associated data.  
    #
    #  @param packetId Integer indentifier
    #  @param data Optional string containing packet data
    #
    #  The difxServer packet protocol
    #  has an integer ID followed by an integer number of data bytes followed by
    #  data.  The packet IDs are all defined as global variables to this class,
    #  and include:
    #  <table>
    #  <tr><td>RELAY_PACKET                   <td> 1
    #  <tr><td>RELAY_COMMAND_PACKET           <td> 2
    #  <tr><td>COMMAND_PACKET                 <td> 3
    #  <tr><td>INFORMATION_PACKET             <td> 4
    #  <tr><td>WARNING_PACKET                 <td> 5
    #  <tr><td>ERROR_PACKET                   <td> 6
    #  <tr><td>MULTICAST_SETTINGS_PACKET      <td> 7
    #  <tr><td>GUISERVER_VERSION              <td> 8
    #  <tr><td>GUISERVER_DIFX_VERSION         <td> 9
    #  <tr><td>AVAILABLE_DIFX_VERSION         <td> 10
    #  <tr><td>DIFX_BASE                      <td> 11
    #  <tr><td>GUISERVER_ENVIRONMENT          <td> 12
    #  <tr><td>DIFX_SETUP_PATH                <td> 13
    #  <tr><td>START_DIFX_MONITOR             <td> 14
    #  <tr><td>DIFX_RUN_LABEL                 <td> 15
    #  <tr><td>GUISERVER_USER                 <td> 16
    #  <tr><td>MESSAGE_SELECTION_PACKET       <td> 17
    #  <tr><td>CHANNEL_ALL_DATA               <td> 18
    #  <tr><td>CHANNEL_ALL_DATA_ON            <td> 19
    #  <tr><td>CHANNEL_ALL_DATA_OFF           <td> 20
    #  <tr><td>CHANNEL_CONNECTION             <td> 21
    #  <tr><td>CHANNEL_DATA                   <td> 22
    #  <tr><td>GENERATE_FILELIST              <td> 23
    #  <tr><td>GET_JOB_STATUS                 <td> 24
    #  </table>
    #   
    #<!------------------------------------------------------------------------>
    def sendPacket( self, packetId, data = "" ):
        #  A socket error of any sort is interpreted as a broken socket, which is
        #  usually a pretty good guess.
        try:
            self.sock.sendall( self.i.pack( socket.htonl( packetId ) ) )
            self.sock.sendall( self.i.pack( socket.htonl( len( data ) ) ) )
            self.sock.sendall( data )
        except:
            self.socketOK = False
        
    #<!------------------------------------------------------------------------>
    ## Send a "command" packet containing the included XML packet data.
    #
    #  @param command String containing the command
    #  @param packetData String containing XML-formatted data associated with the 
    #                    command.
    #
    #  This function composes an XML header and includes the preformatted XML
    #  data as the "body" of a command packet.  Formatted for
    #  readability (at the expense of sending some extra bytes).
    #
    #  It is not expected that users would make direct use of this function.
    #<!------------------------------------------------------------------------>
    def sendCommandPacket( self, command, packetData ):
        totalPacketData = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>\n"
        totalPacketData += "<difxMessage>\n"
        totalPacketData += "    <header>\n"
        totalPacketData += "        <type>" + command + "</type>\n"
        totalPacketData += "    </header>\n"
        totalPacketData += "    <body>\n"
        totalPacketData += packetData
        totalPacketData += "    </body>\n"
        totalPacketData += "</difxMessage>"
        self.sendPacket( self.COMMAND_PACKET, totalPacketData )

    #<!------------------------------------------------------------------------>
    ## Send a "relay packet" command
    #
    #  @param on True or False
    #
    #  Turning on packet relay will trigger <i>guiServer</i> to provide all
    #  DiFX UDP packet communications by relay.  This function can be used to turn
    #  relay on (it is on by default) or off.
    #<!------------------------------------------------------------------------>
    def relayPackets( self, on = True ):
        if on:
            self.sendPacket( self.RELAY_PACKET, self.i.pack( socket.htonl( 1 ) ) )
        else:
            self.sendPacket( self.RELAY_PACKET, self.i.pack( 0 ) )
            
    #<!------------------------------------------------------------------------>
    ## Select a list of messages that the server should relay.
    #
    #  @param messageTypes List of legal message type strings or integer codes.
    #   
    #  Related to the the relayPackets() function - this function will specify a list of UDP
    #  packet types that should be relayed.  The ist list of legal
    #  type strings includes the following (each item has an
    #  integer alternative):
    #  <table>
    #  <tr><td>     1 <td> DifxLoadMessage
    #  <tr><td>     2 <td> DifxAlertMessage
    #  <tr><td>     3 <td> Mark5StatusMessage
    #  <tr><td>     4 <td> DifxStatusMessage
    #  <tr><td>     5 <td> DifxInfoMessage
    #  <tr><td>     6 <td> DifxDatastreamMessage
    #  <tr><td>     7 <td> DifxCommand
    #  <tr><td>     8 <td> DifxParameter
    #  <tr><td>     9 <td> DifxStart
    #  <tr><td>    10 <td> DifxStop
    #  <tr><td>    11 <td> Mark5VersionMessage
    #  <tr><td>    12 <td> Mark5ConditionMessage
    #  <tr><td>    13 <td> DifxTransientMessage
    #  <tr><td>    14 <td> DifxSmartMessage
    #  <tr><td>    15 <td> Mark5DriveStatsMessage
    #  <tr><td>    16 <td> DifxDiagnosticMessage
    #  <tr><td>    17 <td> DifxFileTransfer
    #  <tr><td>    18 <td> DifxFileOperation
    #  <tr><td>    19 <td> DifxVex2DifxRun
    #  <tr><td>    20 <td> DifxMachinesDefinition
    #  <tr><td>    21 <td> DifxGetDirectory
    #  <tr><td>    22 <td> DifxMk5Control
    #  <tr><td>    23 <td> DifxMark5Copy
    #  </table>
    #<!------------------------------------------------------------------------>
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
        
    #<!------------------------------------------------------------------------>
    ## Request all data be "channeled"
    #
    #  @param on True/False
    #
    #  When data are "channeled" the server will channel all through
    #  the primary connection socket instead of opening new sockets
    #  for operation-specific communication.  This changes the way the data are parsed
    #  on the client end.  This function can be used to turn channeling on or
    #  off.
    #
    #  Much of the Python Interface depends on channeled data.  By default
    #  it is on, and probably it should not be turned off.
    #<!------------------------------------------------------------------------>
    def channelData( self, on = True ):
        if on:
            self.sendPacket( self.CHANNEL_ALL_DATA_ON )
            self._channelData = True
        else:
            self.sendPacket( self.CHANNEL_ALL_DATA_OFF )
            self._channelData = False
            
    #<!------------------------------------------------------------------------>
    ##  Send a "guiServer Version" request
    #
    #  Send a "version" request to <i>guiServer</i>.  The server will respond with a
    #  bunch of version information messages, including the GUISERVER_VERSION,
    #  DIFX_BASE, GUISERVER_USER, AVAILABLE_DIFX_VERSION, GUISERVER_ENVIRONMENT,
    #  and CHANNEL_ALL_DATA packet IDs.  The results of all of these are parsed
    #  locally.
    #<!------------------------------------------------------------------------>
    def versionRequest( self ):
        self.sendPacket( self.GUISERVER_VERSION )
        
    #<!------------------------------------------------------------------------>
    ##  Send channeled packet data.
    #
    #  @param channel The channel number
    #  @param data The data
    #
    #  Send "channeled" data to the server.  Channeled data are preceded by
    #  the integer channel number.
    #<!------------------------------------------------------------------------>
    def sendChannelData( self, channel, data ):
        self.sendPacket( self.CHANNEL_DATA, self.i.pack( socket.htonl( channel ) ) + data )
        
    #<!------------------------------------------------------------------------>
    ##  Send a complete packet to the specified channel.
    #
    #  @param channel The channel number
    #  @param data The packet number
    #  @param data The data
    #
    #  Send a "channeled" packet to the server.  The packet contains ID, size of
    #  data, and data.  This takes three calls to sendChannelData().
    #<!------------------------------------------------------------------------>
    def sendChannelPacket( self, channel, packetId, data ):
        self.sendChannelData( channel, self.i.pack( socket.htonl( packetId ) ) )
        self.sendChannelData( channel, self.i.pack( socket.htonl( len( data ) ) ) )
        self.sendChannelData( channel, data )
        
    #<!------------------------------------------------------------------------>
    ##  Send a "file operation" command.  
    #
    #  @param channel Channel for data responses, if there are any
    #  @param command File operation to perform
    #  @param args Command line arguments for the file operation
    #  @param path Full path used in the file operation
    #  @param dataNode Currently not used for anything, but part of the XML
    #                  message
    #
    #  Compose and send an XML message to <i>guiServer</i> that contains a
    #  file operation.  Possible file operations include <i>ls, rm, mv, mkdir</i>
    #  and <i>rmdir</i>.  This method is usually called by other methods that
    #  form part of the DiFX Interface.
    #
    #  XML is formatted to be pretty on the other end (for
    #  diagnostic purposes).
    #<!------------------------------------------------------------------------>
    def fileOperation( self, channel, command, args, path, dataNode ):
        #  Compose an XML string holding the command.
        #  And send it.
        packetData = "      <port>" + str( channel ) + "</port>\n"
        packetData += "     <operation>" + command + "</operation>\n"
        if args != None:
            packetData += "     <arg>" + args + "</arg>\n"
        packetData += "     <path>" + path + "</path>\n"
        if dataNode != None:
            packetData += "     <dataNode>" + dataNode + "</dataNode>\n"
        self.sendCommandPacket( "DifxFileOperation", packetData )
        
    #<!------------------------------------------------------------------------>
    ## Callback function for the socket monitor.  
    #
    #  @param packetId Integer packet identifier
    #  @param data String containing the packet data (stripped of the ID)
    #
    #  This method is triggered when a new packet is
    #  received by the socket monitor thread.  With the exception of the "version" information, which is all
    #  stored locally, packet IDs trigger callback functions that are assigned
    #  to them.  This is not a method external users are expected to employ.
    #<!------------------------------------------------------------------------>
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
                print "something wrong with environment definition: " + str( data )
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

    #<!------------------------------------------------------------------------>
    ##  Callback function for failures in the socket monitor.
    #
    #  This method is passed by the monitor() and passiveMonitor() methods to
    #  the MonitorThread instance.  It will be called when that thread detects
    #  a failure.  This method tries to determine the cause of the failure (more
    #  work required!) and then calls each function in a list of "registered" 
    #  functions that are to be called when a failure occurs (you "register" a 
    #  function using the addFailCallback() method).
    #<!------------------------------------------------------------------------>
    def failCallback( self ):
        for cb in self.failCallbacks:
            #  If no data were ever read, we assume this is caused by a socket
            #  that was bad from the start.
            if self.monitorThread.bytesReceived == 0:
                self.socketOK = False
                cb( self.FAILED_CONNECTION )
            else:
                cb( self.BROKEN_SOCKET )
                
    #<!------------------------------------------------------------------------>
    ## Find what versions are available on the server.
    #
    #  @param preference An optional version name that you wish to use.
    #
    #  Query the server for DiFX Versions available and either set the version
    #  preferred by this client to one specified as an argument, or use the
    #  version of the server if not specified.  This function has to sleep while
    #  waiting for all responses from the server.
    #
    #  The client's preferred version is something that should be set before any
    #  DiFX operations are run.
    #<!------------------------------------------------------------------------>
    def version( self, preference = None ):
        self.channelResponse = False
        self.versionRequest()
        #  Wait for the channel reponse to be set to True, indicating all version
        #  information has been downloaded.  A five second limit is put on this
        #  wait.
        wait = self._waitTime
        while self.socketOK and wait > 0.0 and not self.channelResponse:
            #  If the initial socket connection wasn't good, we'll hang here for
            #  the wait time, which might cause the user to issue a keyboard
            #  interrupt.  Capture that.
            try:
                time.sleep( 0.01 )
                wait -= 0.01
            except:
                self.socketOK = False
                pass
        #  Set the version preference if the version request worked, either to
        #  our preference if requested or "DIFX-DEVEL" if not.
        if self.channelResponse:
            if preference == None:
                preference = "DIFX-DEVEL"
            #  Is this available?
            try:
                self.availableVersion.index( preference )
                self.versionPreference = preference
                #  Try also setting the "generic run file" appropriate to this
                #  version.
                self.setVersionRunFile( self.versionPreference )
            except:
                print "Version \"" + preference + "\" is not available."
        else:
            self.socketOK = False
            print "SERVER DID NOT RESPOND!"
                
    #<!------------------------------------------------------------------------>
    ## Set the generic run file based on a version name.
    #
    #  @param version The version name
    #
    #  See the runFile() method for more information about generic run files.
    #<!------------------------------------------------------------------------>
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
        
    #<!------------------------------------------------------------------------>
    ##  Set the generic run file for all DiFX processes.  
    #
    #  @param newVal generic run file name
    #
    #  The generic run file is
    #  a script that runs a given executable, often after setting proper environment
    #  variables and other items.  The "difxbuild" process creates generic run files
    #  for each DiFX version they create - these are in $DIFX_BASE/bin/rungeneric.[VERSION].
    #  The file defined here will be used to run all DiFX operations.
    #
    #  If you do not have any such "rungeneric" file, set the run file to an empty
    #  string or <i>None</i>.  This <i>should</i> still work - <i>guiServer</i>
    #  will run DiFX software using the same environment it was started under.
    #<!------------------------------------------------------------------------>
    def runFile( self, newVal ):
        self._runFile = newVal
            
    #<!------------------------------------------------------------------------>
    ## Return the value of an environment variable as seen by the server.  
    #
    #  @param varName environment variable name
    #  @return String containing the variable value
    #
    #  If the
    #  variable does not exist, "None" is returned.
    #<!------------------------------------------------------------------------>
    def getenv( self, varName ):
        try:
            return self.serverEnvironment[varName]
        except:
            return None
            
    #<!------------------------------------------------------------------------>
    ## Run an "ls" command on the server with the specified path.
    #
    #    @param path Full path that should be listed (as one would pass to "ls").
    #                An empty string is permitted, as well as <i>None</i>.
    #    @param args Standard <i>ls</i> arguments - optional.
    #    @return List of strings containing the results of the <i>ls</i> command
    #            on the server.  See the DiFXls.Client.ls() method for detail.
    #
    #  This function returns either after the <i>ls</i> operation is complete or the "waitTime"
    #  passes.
    #<!------------------------------------------------------------------------>
    def ls( self, path, args = None ):
        return DiFXls.Client(self).ls( path, args )
            
    #<!------------------------------------------------------------------------>
    ## Get the "status" of a job or jobs using .input file path.
    #
    #    @param path Full path that should be listed (as one would pass to "ls").
    #    @param shortStatus True/False whether a "short" status (including
    #                       only the final job state) is requested.  Faster
    #                       and easier to deal with for large numbers of jobs!
    #    @return A tuple contain the time stamp of a status and the status
    #            structure - see the DiFXJobStatus.Client.jobStatus() method for a
    #            description of this structure.
    #
    #  This function returns either after the requested status information is
    #  returned or the "waitTime" passes.  See the DiFXJobStatus.Client.jobStatus() method
    #  for details on the return values.
    #<!------------------------------------------------------------------------>
    def jobStatus( self, path, shortStatus, waitToFinish = True ):
        return DiFXJobStatus.Client( self ).jobStatus( path, shortStatus, waitToFinish )
            
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
        
    #<!------------------------------------------------------------------------>
    ## Run a "rmdir" command on the server with the specified path.
    #
    #    @param path Full path of the directory that should be removed.
    #
    #  Similar to mkdir - no arguments (other than the path), no feedback.
    #
    #<!------------------------------------------------------------------------>
    def rmdir( self, path ):
        channel = self.newChannel( None )
        self.fileOperation( channel, "rmdir", None, path, None )
        self.closeChannel( channel )
        
    #<!------------------------------------------------------------------------>
    ## Run a "rm" command on the server with the specified arguments and path.
    #
    #    @param path Full path of the file that should be removed.
    #    @param args Arguments that are tacked onto the rm command.
    #
    #  Remove operations are limited to those that can be accomplished by the
    #  user running the server.
    #
    #<!------------------------------------------------------------------------>
    def rm( self, path, args = None ):
        channel = self.newChannel( None )
        self.fileOperation( channel, "rm", args, path, None )
        self.closeChannel( channel )
        
    #<!------------------------------------------------------------------------>
    ## Run a "mv" command on the server to move a path to another location.
    #
    #    @param pathFrom Full path of the file that should be moved.
    #    @param pathTo Full path of the destination of the file.
    #
    #  Move operations are limited to those that can be accomplished by the
    #  user running the server (i.e. it must have appropriate permissions).
    #  No arguments are permitted in a move operation, only the source and
    #  destination path names.
    #
    #<!------------------------------------------------------------------------>
    def mv( self, pathFrom, pathTo ):
        channel = self.newChannel( None )
        self.fileOperation( channel, "mv", pathTo, pathFrom, None )
        self.closeChannel( channel )
        
    #<!------------------------------------------------------------------------>
    ## Get the content of a file on the DiFX server.
    #
    #    @param path Full path of the file from which we want content.
    #    @param waitToFinish True/False wait for the instruction to complete.
    #                        True by default.
    #    @return String containing the file content, or nothing if waitToFinish
    #            is False.
    #
    #  If the value of "waitToFinish" is True (default) this function will return the
    #  content of the requested file, or whatever fraction of the file was obtained
    #  before a timeout of a keyboard interrupt.  The function will not return
    #  until one of these states is reached (file complete, timeout, or keyboard
    #  interrupt).
    #
    #  If "waitToFinish" is false the function will return immediately, and will
    #  return nothing.  The user needs to set progress callbacks using the
    #  intervalCallback() and finalCallback() functions.
    #<!------------------------------------------------------------------------>
    def getFile( self, path, waitToFinish = True ):
        return DiFXFileTransfer.Client( self ).getFile( path, waitToFinish )
            
    #<!------------------------------------------------------------------------>
    ## Send data to a file on the DiFX server.
    #
    #    @param path Full path of the destination file.
    #    @param data String data to put in the file.
    #    @param waitToFinish True/False wait for the instruction to complete.
    #                        True by default.
    #    @return Number of bytes received by the server, or nothing if waitToFinish
    #            is False.
    #
    #  If the value of "waitToFinish" is True (default) this function will wait
    #  until the file is fully transfered then return the number of bytes received
    #  by the server.  If "waitToFinish" is false the function will return immediately, and will
    #  return nothing.  The user needs to set a callback using the
    #  finalCallback() function to do something when the transfer is complete.
    #<!------------------------------------------------------------------------>
    def sendFile( self, path, data, waitToFinish = True ):
        return DiFXFileTransfer.Client( self ).sendFile( path, data, waitToFinish )
            
    #<!------------------------------------------------------------------------>
    ##  Respond to a single packet containing channeled data.  
    #
    #   @param data channel data returned
    #
    #   This function is called by the packetCallback() method when it detects
    #   channeled data.  Channeled data is uniquely associated with an integer
    #   channel ID.  This method strips off the channel ID (the first four
    #   bytes of the data) and passes the remaining data to a callback assigned
    #   to the chanel ID (sometimes, for historical reasons, called the "port").
    #
    #   Channel callbacks are assigned using the newChannel() method.
    #
    #   It is not expected that an external user would employ this function.
    #<!------------------------------------------------------------------------>
    def consumeChannelData( self, data ):
        #  Make sure the data are long enough to contain a channel identifier
        if len( data ) >= 4:
            #  Split off the first 4 bytes, which should identify the "channel".
            channel = socket.ntohl( self.i.unpack( data[0:4] )[0] )
            newData = data[4:]
            #  Use the channel number to do the proper callback.
            try:
                if self.channelCallbacks[channel] != None:
                    self.channelCallbacks[channel]( newData )
            except KeyError:
                print "effort to direct channeled data to channel number " + str( channel ) + " failed - no defined callback"

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
#<!---======================================================================--->
## Base class of DiFX Message classes.
#
#  This class is the base class for all DiFX Message classes.  It contains the
#  information in the header that is common to all DiFX Messages.  
#
#  Among the items in the DiFX Message header
#  is the Message "type", which is stored in this class in the
#  string <i>typeStr</i> (name changed so as not to be confused with the Python reserved
#  word "type").  This variable can be used to determine what sort of data the
#  rest of the message contains.
#
#  Initialization of this class is done with a string value "data" that is parsed
#  for the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
class XMLMessage:
    def __init__( self, data ):
        self.parseOK = True
        #  Send the data to the parser and collect the top-level node (there is
        #  only one).
        try:
            self.pdat = xml.dom.minidom.parseString( data )
            self.header = None
            self.body = None
            self.fromNode = None
            self.mpiProcessId = None
            self.identifier = None
            # \var this is important
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
        except:
            #  This is to accommodate the occasional broken message.  Not a graceful
            #  way of doing this obviously.
            self.parseOK = False
            pass

#===============================================================================
#  The following are classes for each of the (known) DiFX message types.  The
#  class instance generates the generic XML message type (which contains the
#  header) and then parses out data specific to the message type.  Everything is
#  stored as strings.
#===============================================================================
#<!---======================================================================--->
## Class to contain a DifxLoadMessage
#
#  This class is used to contain the contents of a "DifxLoadMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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
                            
#<!---======================================================================--->
## Class to contain a Mark5StatusMessage
#
#  This class is used to contain the contents of a "Mark5StatusMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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
                            
#<!---======================================================================--->
## Class to contain a DifxAlertMessage
#
#  This class is used to contain the contents of a "DifxAlertMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxStatusMessage
#
#  This class is used to contain the contents of a "DifxStatusMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxInfoMessage
#
#  This class is used to contain the contents of a "DifxInfoMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxCommandMessage
#
#  This class is used to contain the contents of a "DifxCommandMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxParameter Message
#
#  This class is used to contain the contents of a "DifxParameter" message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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
                            
#<!---======================================================================--->
## Class to contain a DifxStart Message
#
#  This class is used to contain the contents of a "DifxStart" message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxStop Message
#
#  This class is used to contain the contents of a "DifxStop" message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxSmartMessage
#
#  This class is used to contain the contents of a "DifxSmartMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxDiagnosticMessage
#
#  This class is used to contain the contents of a "DifxDiagnosticMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxTransientMessage
#
#  This class is used to contain the contents of a "DifxTransientMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a Mark5VersionMessage
#
#  This class is used to contain the contents of a "Mark5VersionMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a Mark5DriveStatsMessage
#
#  This class is used to contain the contents of a "Mark5DriveStatsMessage".
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxFileTransfer Message
#
#  This class is used to contain the contents of a "DifxFileTransfer" Message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxFileOperation Message
#
#  This class is used to contain the contents of a "DifxFileOperation" Message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxVex2DifxRun
#
#  This class is used to contain the contents of a "DifxVex2DifxRun" Message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxMachinesDefinition Message
#
#  This class is used to contain the contents of a "DifxMachinesDefinition" Message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxGetDirectory Message
#
#  This class is used to contain the contents of a "DifxGetDirectory" Message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxMk5Control Message
#
#  This class is used to contain the contents of a "DifxMk5Control" Message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Class to contain a DifxMark5Copy Message
#
#  This class is used to contain the contents of a "DifxMark5Copy" Message.
#
#  The class is initialized with a string value "data" that is parsed
#  for the class-specific information.  The inherited XMLMessage class is
#  used to fill in the header information.
#
#  See \ref messages "Monitoring DiFX Messages" for more information.
#
#<!---======================================================================--->
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

#<!---======================================================================--->
## Function to parse a DiFX message given its XML data.  
#
#  @param data String of data returned from the server that contains a complete
#              DiFX message in XML format.
#  @return Class containing the data appropriate to the "type" of DiFX Message
#
#  The <i>parseXML()</i> function can be used to convert an XML string form of
#  a DiFX UDP message as received from the DiFX server into a structure containing
#  its individual elements.  
#  An instance of a
#  class appropriate to the message type is returned - each of these classes
#  inherits the generic DiFXControl.XMLMessage class, which contains a string
#  variable "typeStr" that can be used to figure out what message type, and thus
#  what class, the return represents.  These message type-specific classes
#  include:
#  <ul>
#     <li>DiFXControl.DifxLoadMessage
#     <li>DiFXControl.Mark5StatusMessage
#     <li>DiFXControl.DifxAlertMessage
#     <li>DiFXControl.DifxStatusMessage
#     <li>DiFXControl.DifxInfoMessage
#     <li>DiFXControl.DifxCommandMessage
#     <li>DiFXControl.DifxParameter
#     <li>DiFXControl.DifxStart
#     <li>DiFXControl.DifxStop
#     <li>DiFXControl.DifxSmartMessage
#     <li>DiFXControl.DifxDiagnosticMessage
#     <li>DiFXControl.DifxTransientMessage
#     <li>DiFXControl.Mark5VersionMessage
#     <li>DiFXControl.Mark5DriveStatsMessage
#     <li>DiFXControl.DifxFileTransfer
#     <li>DiFXControl.DifxFileOperation
#     <li>DiFXControl.DifxVex2DifxRun
#     <li>DiFXControl.DifxMachinesDefinition
#     <li>DiFXControl.DifxGetDirectory
#     <li>DiFXControl.DifxMk5Control
#     <li>DiFXControl.DifxMark5Copy
#  </ul>
#
#<!---======================================================================--->
def parseXML( data ):
    #  Create a generic message type to extract the header information.
    xmlDat = XMLMessage( data )
    if not xmlDat.parseOK:
        #  If the parse didn't work, set the type string to None so calling
        #  functions know not to use it.
        xmlDat.typeStr = None
        return xmlDat
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
        
            
