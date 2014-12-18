/*
 * This class is used to form a connection to the guiServer application that
 * should be running on the DiFX host.  This is a bi-directional TCP connection.
 * Outgoing traffic (TO the guiServer) takes the form of commands, while incoming
 * traffic (FROM guiServer) is limited to a few informational packets (mostly 
 * transmitted upon connection) and relay of multicast traffic on the DiFX cluster (much
 * of which is mk5server traffic).  The design tries to keep this connection
 * fairly simple (and thus hopefully robust) - any additional data relays (for 
 * instance if a command generates return data, or a file needs to be transfered)
 * are done using purpose-formed TCP connections with threads on the guiServer.
 * 
 * This class generates three types of callbacks: "connect" events accompanied by
 * an explanatory String (either the connection status in the case of a change or
 * an exception string in the case of an error); "send" events accompanied
 * by an integer number of bytes sent; and  "receive" events accompanied by an
 * integer number of bytes received.  
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;

import java.net.Socket;
import java.net.SocketTimeoutException;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import java.util.ArrayDeque;
import java.util.HashMap;

import javax.swing.JOptionPane;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.EventListenerList;

/**
 *
 * @author johnspitzak
 */
public class GuiServerConnection {
    
    public final int RELAY_PACKET                   = 1;
    public final int RELAY_COMMAND_PACKET           = 2;
    public final int COMMAND_PACKET                 = 3;
    public final int INFORMATION_PACKET             = 4;
    public final int WARNING_PACKET                 = 5;
    public final int ERROR_PACKET                   = 6;
    public final int MULTICAST_SETTINGS_PACKET      = 7;
    public final int GUISERVER_VERSION              = 8;
    public final int GUISERVER_DIFX_VERSION         = 9;
    public final int AVAILABLE_DIFX_VERSION         = 10;
    public final int DIFX_BASE                      = 11;
    public final int GUISERVER_ENVIRONMENT          = 12;
    public final int DIFX_SETUP_PATH                = 13;
    public final int START_DIFX_MONITOR             = 14;
    public final int DIFX_RUN_LABEL                 = 15;
    public final int GUISERVER_USER                 = 16;
    public final int MESSAGE_SELECTION_PACKET       = 17;
    public final int CHANNEL_ALL_DATA               = 18;
    public final int CHANNEL_ALL_DATA_ON            = 19;
    public final int CHANNEL_ALL_DATA_OFF           = 20;
    public final int CHANNEL_CONNECTION             = 21;
    public final int CHANNEL_DATA                   = 22;
    public final int GENERATE_FILELIST              = 23;

    public GuiServerConnection( SystemSettings settings, String IP, int port ) {
        _settings = settings;
        _connectListeners = new EventListenerList();
        _sendListeners = new EventListenerList();
        _receiveListeners = new EventListenerList();
    }
    
    public boolean connect() {
        boolean ret = true;
        try {
            _socket = new Socket( _settings.difxControlAddress(), _settings.difxControlPort() );
            _socket.setSoTimeout( _settings.timeout() );
            _in = new DataInputStream( _socket.getInputStream() );
            _out = new DataOutputStream( _socket.getOutputStream() );
            _connected = true;
            connectEvent( "connected" );
            _receiveThread = new ReceiveThread();
            _settings.channelAllDataAvailable( true );
            _receiveThread.start();
            //  Request guiServer version information...and anything else it wants
            //  to tell us at startup.
            sendPacket( GUISERVER_VERSION, 0, null );
            ret = true;
        } catch ( java.net.UnknownHostException e ) {
            _connected = false;
            ret = false;
        } catch ( java.io.IOException e ) {
            _connected = false;
            ret = false;
        }
        return ret;
    }
    
    public String myIPAddress() {
        return _socket.getLocalAddress().getHostAddress();
    }
    
    public void close() {
        if ( connected() ) {
            try {
                _socket.close();
            } catch ( java.io.IOException e ) {
                //  Not being able to close the socket probably indicates something out of
                //  the user's ability to fix is wrong, so we won't trouble them by reporting
                //  the problem.
            }
            _connected = false;
            connectEvent( "connection closed" );
        }
    }
    
    /*
     * Send a string with the associated packet ID.  This converts the string to
     * data.
     */
    public void sendPacketWithString( int packetId, String str ) {
        byte [] data = str.getBytes();
        sendPacket( packetId, data.length, data );
        data = null;
    }
    
    /*
     * Send a packet of the given type.  The type and number of bytes in
     * the packet are sent as integers - and thus are swapped (if necessary) to
     * network byte order.  The data are not.
     */
    synchronized public void sendPacket( int packetId, int nBytes, byte[] data ) {
        if ( _connected ) {
            try {
                _out.writeInt( packetId );
                _out.writeInt( nBytes );
                if ( nBytes > 0 ) {
                    _out.write( data );
                }
                sendEvent( nBytes );
            } catch ( Exception e ) {
                java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null, 
                    e.toString() );
                connectEvent( e.toString() );
            }
        } else {
            sendEvent( -nBytes );
        }
    }
        
    /*
     * This is kind of gross and kludgy....it is meant to simulate waiting for
     * a socket message from difx of a specific type - left over from when the ONLY
     * type of message difx sent was a relay.
     */
    public byte[] getRelay( int timeout ) throws SocketTimeoutException {
        int counter = 0;
        while ( counter < timeout ) {
            if ( _difxRelayStack != null ) {
                synchronized( _difxRelayStack ) {
                    if ( _difxRelayStack.size() > 0 ) {
                        byte [] returnData = _difxRelayStack.pollFirst();
                        return returnData;
                    }
                }
            }
            counter += 1;
            try { Thread.sleep( 1 ); } catch ( Exception e ) {}
        }
        throw new SocketTimeoutException();
    }
    
    protected static int WARNING_SIZE = 10 * 1024 * 1024;

    /*
     * This thread recieves data packets of different types from the guiServer.
     */
    protected class ReceiveThread extends Thread {
        
        public void run() {
            int lastNBytes = 0;
            int lastID = 0;
            byte pad;
            byte[] inNum = new byte[8];
            while ( _connected ) {
                byte[] data = null;
                try {
                    int packetId = _in.readInt();
                    int nBytes = _in.readInt();
                    if ( nBytes > WARNING_SIZE || nBytes < 0 ) {
                        //  Only report this error if there have been packets received
                        //  already - otherwise it may just indicate a connection/handshaking
                        //  issue.
                        if ( lastID != 0 ) {
                            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.WARNING, 
                                    "trying to read (" + nBytes + " of data) - packetID is " + packetId + " last is " + lastID + "(" + lastNBytes + ")" );
                            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.WARNING, 
                                    "Message has NOT BEEN READ" );
                        }
                    }
                    else {
                        lastID = packetId;
                        lastNBytes = nBytes;
                        data = new byte[nBytes];
                        try {
                        _in.readFully( data );
                        } catch ( java.io.EOFException e ) {
                            System.out.println( "EOFException!" );
                        }
                        //  Sort out what to do with this packet.
                        if ( packetId == RELAY_PACKET && data != null ) {
                            if ( _difxRelayStack == null )
                                _difxRelayStack = new ArrayDeque<byte[]>();
                            synchronized ( _difxRelayStack ) {
                                _difxRelayStack.addLast( data );
                            }
                        }
                        else if ( packetId == GUISERVER_VERSION ) {
                            //  This is a report of the version of guiServer that is running.
                            _settings.guiServerVersion( new String( data ) );
                            //  Assuming the above message indicates a new connection, clear the
                            //  list of guiServer environment variables - we will get new ones.
                            _settings.clearGuiServerEnvironment();
                        }
                        else if ( packetId == GUISERVER_DIFX_VERSION ) {
                            //  This is the difx version for which the guiServer was compiled.  Not
                            //  currently used.
                            _settings.guiServerDifxVersion( new String( data ) );
                        }
                        else if ( packetId == GUISERVER_USER ) {
                            //  This is the username used to start guiServer.
                            _settings.difxControlUser( new String( data ) );
                        }
                        else if ( packetId == AVAILABLE_DIFX_VERSION ) {
                            //  Add an available DiFX version to the list in settings.
                            _settings.addDifxVersion( new String( data ) );
                        }
                        else if ( packetId == INFORMATION_PACKET ) {
                            _settings.messageCenter().message( 0, "guiServer", new String( data ) );
                        }
                        else if ( packetId == WARNING_PACKET ) {
                            _settings.messageCenter().warning( 0, "guiServer", new String( data ) );
                        }
                        else if ( packetId == ERROR_PACKET ) {
                            _settings.messageCenter().error( 0, "guiServer", new String( data ) );
                        }
                        else if ( packetId == DIFX_BASE ) {
                            //  The DiFX base is the path below which all "setup" files
                            //  exist.
                            _settings.clearDifxVersion();
                            _settings.difxBase( new String( data ) );
                        }
                        else if ( packetId == GUISERVER_ENVIRONMENT ) {
                            //  These are environment variables from the guiServer
                            _settings.addGuiServerEnvironment( new String( data ) );
                        }
                        else if ( packetId == CHANNEL_ALL_DATA ) {
                            //  Indicates the guiServer has the ability to "channel" all data
                            //  through a single TCP port.
                            _settings.channelAllDataAvailable( true );
                            //  Tell guiServer whether we want to do this based on the current
                            //  setting.
                            if ( _settings.channelAllData() )
                                sendPacket( CHANNEL_ALL_DATA_ON, 0, null );
                            else
                                sendPacket( CHANNEL_ALL_DATA_OFF, 0, null );
                        }
                        else if ( packetId == CHANNEL_CONNECTION ) {
                            //  A "channelled" TCP connection is requested.  
                            ByteBuffer b = ByteBuffer.wrap( data, 0, 4 );
                            b.order( ByteOrder.BIG_ENDIAN );
                            int port = b.getInt();
                            synchronized ( _channelConnected ) {
                                if ( _channelConnected.containsKey( port ) ) {
                                    _channelConnected.put( port, true );
                                    //  Create a "map" for data on this channel.
                                    _channelMap.put( port, new ArrayDeque<ByteBuffer>() );
                                }
                                else
                                    java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.WARNING, 
                                            "guiServer trying to connect to port " + port + " for a channel TCP connection, but this port doesn't exist" );
                            }
                        }
                        else if ( packetId == CHANNEL_DATA ) {
                            //  Data over a "channelled" TCP connection.
                            ByteBuffer b = ByteBuffer.wrap( data, 0, nBytes );
                            b.order( ByteOrder.BIG_ENDIAN );
                            int port = b.getInt();
                            synchronized ( _channelConnected ) {
                                _channelMap.get( port ).add( b );  
                            }
                        }
                        receiveEvent( data.length );
                    }
                } catch ( SocketTimeoutException e ) {
                    //  Timeouts are actually expected and should not cause alarm.
                } catch ( java.io.IOException e ) {
                    _connected = false;
                    connectEvent( e.toString() );
                }
            }
        }
        
    }
        
    
    /*
     * This turns on (or off) the broadcast relay capability.
     */
    public void relayBroadcast( boolean on ) {
        ByteBuffer b = ByteBuffer.allocate( 4 );
        b.order( ByteOrder.BIG_ENDIAN );
        if ( on )
            b.putInt( 1 );
        else
            b.putInt( 0 );
        sendPacket( RELAY_PACKET, 4, b.array() );
    }
    
    public boolean connected() { return _connected; }
    
    public void addConnectionListener( ActionListener a ) {
        _connectListeners.add( ActionListener.class, a );
    }

    public void addSendListener( ActionListener a ) {
        _sendListeners.add( ActionListener.class, a );
    }

    public void addReceiveListener( ActionListener a ) {
        _receiveListeners.add( ActionListener.class, a );
    }
    
    protected void connectEvent( String mess ) {
        Object[] listeners = _connectListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, mess ) );
        }
    }

    protected void sendEvent( Integer nBytes ) {
        Object[] listeners = _sendListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, nBytes.toString() ) );
        }
    }

    protected void receiveEvent( Integer nBytes ) {
        Object[] listeners = _receiveListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, nBytes.toString() ) );
        }
    }


    protected Socket _socket;
    protected DataInputStream _in;
    protected DataOutputStream _out;
    protected boolean _connected;
    protected EventListenerList _connectListeners;
    protected EventListenerList _sendListeners;
    protected EventListenerList _receiveListeners;
    protected ArrayDeque<byte[]> _difxRelayStack;
    protected ReceiveThread _receiveThread;
    protected SystemSettings _settings;
    
    //  Stuff used for channeling data.
    protected HashMap<Integer,ArrayDeque<ByteBuffer>> _channelMap;
    protected HashMap<Integer,Boolean> _channelConnected;
    protected HashMap<Integer,ByteBuffer> _lastBuffer;
    
    //  "Open" a new channeling port.  This is supposed to look (externally) like
    //  an independent TCP port, but the data for it are channelled through the 
    //  primary TCP connection.  The port number is used to decide where data are
    //  to go.
    public boolean openChannelPort( int port ) {
        //  Create maps if they don't already exist.
        if ( _channelMap == null ) {
            _channelMap = new HashMap<Integer,ArrayDeque<ByteBuffer>>();
            _channelConnected = new HashMap<Integer,Boolean>();
            _lastBuffer = new HashMap<Integer,ByteBuffer>();
        }
        boolean ret = true;
        synchronized ( _channelConnected ) {
            //  Bail out if this port is already in the map.
            if ( _channelConnected.get( port ) != null )
                ret = false;
            else
                //  Add a new map item to await a connection at the given port.
                _channelConnected.put( port, false );
        }
        return ret;
    }
    
    //  Determine whether a port has been connected to by the guiServer.
    public boolean portConnected( int port ) {
        boolean ret = false;
        synchronized ( _channelConnected ) {
            if ( _channelConnected.containsKey( port ) && _channelConnected.get( port ) == true )
                ret = true;
        }
        return ret;
    }
    
    //  Is there data on this port?
    public boolean portData( int port ) {
        boolean ret = false;
        if ( _channelConnected != null ) {
            synchronized( _channelConnected ) {
                ret = _channelMap.get( port ).size() > 0;
            }
        }
        return ret;
    }
    
    //  How many bytes are available in the next data item on the port?
    public int portDataNum( int port ) {
        int ret = 0;
        synchronized( _channelConnected ) {
            if ( portData( port ) )
                ret = _channelMap.get( port ).peekFirst().remaining();
        }
        return ret;
    }
    
    //  Return the oldest data associated with this channel port.
    public ByteBuffer portBuffer( int port ) {
        ByteBuffer ret = null;
        synchronized( _channelConnected ) {
            //  See if we have an existing buffer associated with this port.
            ret = _lastBuffer.get( port );
            //  Get a new buffer if one doesn't exist or the last one is empty.
            if ( ret == null || ret.remaining() <= 0 ) {
                ret = _channelMap.get( port ).pollFirst();
                _lastBuffer.put( port, ret );
            }
        }
        return ret;
    }
    
    //  Close anything associated with this port.
    public void portClose( int port ) {
        synchronized( _channelConnected ) {
            _channelMap.remove( port );
            _channelConnected.remove( port );
        }
    }
    
}
