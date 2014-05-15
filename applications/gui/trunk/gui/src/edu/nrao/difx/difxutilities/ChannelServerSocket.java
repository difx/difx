/*
 * The purpose of this class is to mimic a server socket in all the instances where
 * the DiFX GUI opens a socket for communication with the guiServer EXCEPT for the
 * one, initial connection.  This allows us to funnel all data through the initial
 * connection instead of using additional sockets.  This is useful when operating
 * remotely through an SSH tunnel where each additional socket would normally need
 * its own tunnel - a complex and probably almost impossible thing to set up properly.
 * 
 * Making this think look so much like a normal socket is admittedly a spectacularly
 * kludgey thing to do.  It is possible that a single socket connection will work
 * well enough that it may be the way to go for all operations in the future.
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;
import java.io.IOException;
import java.net.Socket;
import java.net.ServerSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public class ChannelServerSocket {
    
    public ChannelServerSocket( int port, SystemSettings settings ) throws IOException {
        _settings = settings;
        //  If we are working with a data "channel" (all TCP traffic through one socket),
        //  fake a server socket.
        if ( _settings.channelAllData() ) {
            _channelOn = true;
            _port = port;
            if ( !_settings.guiServerConnection().openChannelPort( port ) )
                throw new IOException();
        }
        //  Otherwise create a normal ServerSocket.
        else
            _serverSock = new ServerSocket( port );
    }
    
    public void setSoTimeout( int msec ) throws SocketException {
        if ( _channelOn )
            _timeout = msec;
        else
            _serverSock.setSoTimeout( msec );
    }
    
    public void close() throws IOException {
        if ( _channelOn ) {
            _settings.guiServerConnection().portClose( _port );
        }
        else {
            if ( _sock != null )
                _sock.close();
            if ( _serverSock != null )
                _serverSock.close();
        }
    }

    /*
     * If this is a normal TCP server socket, wait for a client connection on the server
     * socket and turn it into a data input stream.  If it is a "channelled" socket,
     * wait for a connection message from the main socket.
     */
    public void accept() throws IOException {
        if ( _channelOn ) {
            int sofar = 0;
            while ( sofar < _timeout ) {
                if ( _settings.guiServerConnection().portConnected( _port ) ) {
                    break;
                }
                ++sofar;
                try { Thread.sleep( 1 ); } catch ( Exception e ) {}
            }
            if ( sofar >= _timeout ) {
                System.out.println( "ChannelServerSocket::looks like a timeout waiting for accept()" );
                throw new SocketTimeoutException();
            }
        }
        else {
            _sock = _serverSock.accept();
            //  Turn the socket into a "data stream", which has useful
            //  functions.
            _inStream = new DataInputStream( _sock.getInputStream() );
            _outStream = new DataOutputStream( _sock.getOutputStream() );
        }
    }
    
    /*
     * Read an integer size from the input stream.
     */
    public int readInt() throws IOException {
        if ( _channelOn ) {
            boolean found = false;
            while ( !found ) {
                if ( _settings.guiServerConnection().portData( _port ) )
                    found = true;
                try { Thread.sleep( 1 ); } catch ( Exception e ) {}
            }
            return _settings.guiServerConnection().portBuffer( _port ).getInt();
        }
        else
            return _inStream.readInt();
    }
    
    /*
     * Read a bunch of bytes from the input stream.
     */
    public void readFully( byte[] bytes, int off, int len ) throws IOException {
        if ( _channelOn ) {
            boolean allDone = false;
            while ( !allDone ) {
                boolean found = false;
                while ( !found ) {
                    if ( _settings.guiServerConnection().portData( _port ) )
                        found = true;
                    try { Thread.sleep( 1 ); } catch ( Exception e ) {}
                }
                ByteBuffer b = _settings.guiServerConnection().portBuffer( _port );
                try {
                    b.get( bytes, off, len );
                    allDone = true;
                } catch ( java.nio.BufferUnderflowException e ) {
                }
            }
        }
        else
            _inStream.readFully( bytes, off, len );
    }

    /*
     * Write an integer.
     */
    public void writeInt( Integer val ) throws IOException {
        if ( _channelOn ) {
            ByteBuffer b = ByteBuffer.allocate( 8 );
            b.order( ByteOrder.BIG_ENDIAN );
            b.putInt( _port );
            b.putInt( val );
            _settings.guiServerConnection().sendPacket( _settings.guiServerConnection().CHANNEL_DATA, 8, b.array() );
        }
        else
            _outStream.writeInt( val );
    }
    
    /*
     * Write a string.
     */
    public void writeBytes( String content ) throws IOException {
        if ( _channelOn ) {
            ByteBuffer b = ByteBuffer.allocate( 4 + content.length() );
            b.order( ByteOrder.BIG_ENDIAN );
            b.putInt( _port );
            b.put( content.getBytes() );
            _settings.guiServerConnection().sendPacket( _settings.guiServerConnection().CHANNEL_DATA, 4 + content.length(), b.array() );
        }
        else
            _outStream.writeBytes( content );
    }
                    
    protected SystemSettings _settings;
    protected boolean _channelOn;
    protected ServerSocket _serverSock;
    protected Socket _sock;
    protected int _timeout;
    protected DataInputStream _inStream;
    protected DataOutputStream _outStream;
    protected int _port;
}
