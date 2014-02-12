/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;

import edu.nrao.difx.xmllib.difxmessage.DifxFileTransfer;
import java.net.UnknownHostException;
import java.net.Socket;
import java.net.ServerSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.io.DataOutputStream;
import java.io.DataInputStream;

import javax.swing.event.EventListenerList;

/**
 *
 * @author jspitzak
 */
public class DiFXCommand_sendFile extends DiFXCommand {
    
    public DiFXCommand_sendFile( String filename, SystemSettings settings ) {
        super( settings );
        this.header().setType( "DifxFileTransfer" );
        DifxFileTransfer xfer = this.factory().createDifxFileTransfer();
        xfer.setAddress( _settings.guiServerConnection().myIPAddress() );
        _port = _settings.newDifxTransferPort( 0, true, true );
        xfer.setPort( _port );
        xfer.setDirection( "to DiFX" );
        xfer.setDestination( filename );
        //  The "data" node is assumed to be the same as the DiFX "control" node
        //  (at least for now).
        xfer.setDataNode( settings.difxControlAddress() );
        this.body().setDifxFileTransfer( xfer );
        //  These lists contain "listeners" for callbacks when things occur...incremental
        //  read progress and the end of reading.
        _incrementalListeners = new EventListenerList();
        _endListeners = new EventListenerList();
        _acceptListeners = new EventListenerList();
    }
    
    /*
     * Send a string of text.  
     */
    public void sendString( String content ) throws java.net.UnknownHostException {
        StringWriter writer = new StringWriter();
        _content = content;
        writer.start();
        super.send();
    }
    
    public void addIncrementalListener( ActionListener a ) {
        _incrementalListeners.add( ActionListener.class, a );
    }

    protected void incrementalCallback() {
        Object[] listeners = _incrementalListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
    
    public void addEndListener( ActionListener a ) {
        _endListeners.add( ActionListener.class, a );
    }

    protected void endCallback() {
        Object[] listeners = _endListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
    
    public void addAcceptListener( ActionListener a ) {
        _acceptListeners.add( ActionListener.class, a );
    }

    protected void acceptCallback() {
        Object[] listeners = _acceptListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
    
    //  This class reads a text file that can contain UTF characters.  It produces
    //  a string.
    protected class StringWriter extends Thread {
        
        @Override
        public void run() {
            //  Open a new server socket and await a connection.  The connection
            //  will timeout after a given number of seconds (nominally 10).
            try {
                ServerSocket ssock = new ServerSocket( _port );
                ssock.setSoTimeout( 10000 );  //  timeout is in millisec
                try {
                    Socket sock = ssock.accept();
                    acceptCallback();
                    //  Turn the socket into a "data stream", which has useful
                    //  functions.
                    DataOutputStream out = new DataOutputStream( sock.getOutputStream() );
                    out.writeInt( _content.length() );
                    out.writeBytes( _content );
//                    out.flush();
                    //  Read how many characters were received.
                    DataInputStream in = new DataInputStream( sock.getInputStream() );
                    _fileSize = in.readInt();
                    sock.close();
                } catch ( SocketTimeoutException e ) {
                    _fileSize = -10;
                }
                ssock.close();
            } catch ( Exception e ) {
                _error = e.toString();
                _fileSize = -11;
            }
            _settings.releaseTransferPort( _port );
            endCallback();
        }
        
    }
    
    public int fileSize() { return _fileSize; }
    public String error() { return _error; }
    
    protected int _fileSize;
    protected String _content;
    protected EventListenerList _incrementalListeners;
    protected EventListenerList _endListeners;
    protected EventListenerList _acceptListeners;
    protected String _error;
    protected int _port;

}
