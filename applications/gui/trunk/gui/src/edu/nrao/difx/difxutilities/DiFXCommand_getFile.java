/*
 * This command reads a file on the DiFX host.  Currently it is meant to read
 * text files, but should be easy to expand to more complex files if need be.
 * 
 * To read a text file, the class is created with the filename and a pointer to
 * SystemSettings, where the DiFX host information is stored.  To actually trigger
 * a file read, the "readString()" function is called.  This starts a thread that
 * runs a TCP server, which the DiFX host can write the file contents to.  Callbacks
 * can be set to monitor incremental progress and know when the whole process is
 * complete.
 * 
 * The "fileSize" integer tells you how big the read file is.  It has some
 * possible negative values, all of which indicate errors.  Some of these are set
 * by the DiFX host, others locally.  They are:
 *      -10:   (local) socket connection timed out before the DiFX host connected
 *      -11:   (local) socket failure of some sort - look at the "error()" function
 *      -1:    (DiFX host) bad file name (probably the path was not complete)
 *      -2:    (DiFX host) requested file does not exist
 *      -3:    (DiFX host) no read permission on named file for DiFX user
 *      -4:    (DiFX host) bad DiFX user name (not in password file)
 *       0:    (DiFX host) not an error, but indicates a zero-length file, which might be bad
 * Even if the fileSize() is rational, you might want to check the length of the
 * resulting string to make sure it matches.
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

import java.io.DataInputStream;
import java.util.Arrays;

import javax.swing.event.EventListenerList;

import java.net.InetAddress;

/**
 *
 * @author jspitzak
 */
public class DiFXCommand_getFile extends DiFXCommand {
    
    public DiFXCommand_getFile( String filename, SystemSettings settings ) {
        super( settings );
        this.header().setType( "DifxFileTransfer" );
        //  Make sure the guiServer connection is working...bail out if not.
        if ( !_settings.guiServerConnection().connected() ) {
            _error = "No connection to guiServer";
            return;
        }
        DifxFileTransfer xfer = this.factory().createDifxFileTransfer();
        try {
            InetAddress[] foo = java.net.InetAddress.getAllByName( java.net.InetAddress.getLocalHost().getHostName() );
            xfer.setAddress( _settings.guiServerConnection().myIPAddress() );
            _port = _settings.newDifxTransferPort();
            xfer.setPort( _port );
            xfer.setDirection( "from DiFX" );
            xfer.setOrigin( filename );
            xfer.setDestination( "none" );
            //  The "data" node is assumed to be the same as the DiFX "control" node
            //  (at least for now).
            xfer.setDataNode( settings.difxControlAddress() );
            this.body().setDifxFileTransfer( xfer );
            //  These lists contain "listeners" for callbacks when things occur...incremental
            //  read progress and the end of reading.
            _incrementalListeners = new EventListenerList();
            _endListeners = new EventListenerList();
            _acceptListeners = new EventListenerList();
        } catch ( java.net.UnknownHostException e ) {
            _error = e.getMessage();
        }
    }
    
    /*
     * Read a string of text.  
     */
    public void readString() throws java.net.UnknownHostException {
        StringReader reader = new StringReader();
        reader.start();
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
    protected class StringReader extends Thread {
        
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
                    DataInputStream in = new DataInputStream( sock.getInputStream() );
                    //  Read the size of the incoming data (bytes).  This is the
                    //  total size - it will be broken into blocks.
                    _fileSize = in.readInt();
                    _inString = "";
                    incrementalCallback();
                    while ( _inString.length() < _fileSize ) {
                        int sz = _fileSize - _inString.length();
                        if ( sz > 1024 )
                            sz = 1024;
                        byte [] data = new byte[sz];
                        int n = in.read( data, 0, sz );
                        //_inString += in.readUTF();
                        _inString += new String( Arrays.copyOfRange( data, 0, n ) );
                        incrementalCallback();
                    }
                    sock.close();
                } catch ( SocketTimeoutException e ) {
                    _fileSize = -10;
                }
                ssock.close();
            } catch ( java.io.IOException e ) {
                e.printStackTrace();
                _error = "IOException : " + e.toString();
                _fileSize = -11;
            }
            _settings.releaseTransferPort( _port );
            endCallback();
        }
        
    }
    
    public int fileSize() { return _fileSize; }
    public String inString() { return _inString; }
    public String error() { return _error; }
    
    protected int _fileSize;
    protected String _inString;
    protected EventListenerList _incrementalListeners;
    protected EventListenerList _endListeners;
    protected EventListenerList _acceptListeners;
    protected String _error;
    protected int _port;

    
}
