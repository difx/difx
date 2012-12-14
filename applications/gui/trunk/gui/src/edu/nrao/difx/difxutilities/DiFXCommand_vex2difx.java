/*
 * Run the "vex2difx" command on the DiFX host.  The mk5daemon process on the DiFX
 * host will create a bunch of files.  The full path to each file created is
 * transmitted via a TCP socket connection back to this process, which receives
 * the data in a thread.  Call backs (incremental and end) tell us when each new
 * line of data appears and when all are done.
 * 
 * The "vex2difx" command can be set to run "calcif" alone (i.e. not run vex2difx
 * at all).  This may be required if the user messes with the .calc file.
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;

import edu.nrao.difx.xmllib.difxmessage.DifxVex2DifxRun;

import java.net.Socket;
import java.net.ServerSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.io.DataInputStream;

import java.net.UnknownHostException;
import javax.swing.event.EventListenerList;

/**
 *
 * @author jspitzak
 */
public class DiFXCommand_vex2difx extends DiFXCommand {
    
    public DiFXCommand_vex2difx( String passPath, String file, SystemSettings settings, boolean calcifOnly ) {
        super( settings );
        this.header().setType( "DifxVex2DifxRun" );
        DifxVex2DifxRun v2d = this.factory().createDifxVex2DifxRun();
        v2d.setUser( settings.difxControlUser() );
        v2d.setNode( settings.difxControlAddress() );
        v2d.setDifxVersion( settings.difxVersion() );
        v2d.setPassPath( passPath );
        v2d.setFile( file );
        if ( calcifOnly )
            v2d.setCalcifOnly( 1 );
        else
            v2d.setCalcifOnly( 0 );
        v2d.setAddress( _settings.guiServerConnection().myIPAddress() );
        _port = _settings.newDifxTransferPort();
        v2d.setPort( _port );
        this.body().setDifxVex2DifxRun( v2d );
        //  These lists contain "listeners" for callbacks when things occur...incremental
        //  read progress and the end of reading.
        _incrementalListeners = new EventListenerList();
        _endListeners = new EventListenerList();
    }
    
    public void addIncrementalListener( ActionListener a ) {
        _incrementalListeners.add( ActionListener.class, a );
    }

    protected void incrementalCallback( String filename ) {
        Object[] listeners = _incrementalListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, filename ) );
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
    
    @Override
    public void send() throws java.net.UnknownHostException {
        ResultReader reader = new ResultReader();
        reader.start();
        super.send();
    }
        
    //  This thread reads the results of the vex2difx operation as reported by mk5daemon
    //  (basically a list of files created by the process).  It produces two callbacks -
    //  the incremental callback for each filename read; and the end callback when
    //  all reading is complete.
    protected class ResultReader extends Thread {
        
        @Override
        public void run() {
            //  Open a new server socket and await a connection.  The connection
            //  will timeout after a given number of seconds (nominally 10).
            try {
                ServerSocket ssock = new ServerSocket( _port );
                ssock.setSoTimeout( 10000 );  //  timeout is in millisec
                try {
                    Socket sock = ssock.accept();
                    //  Turn the socket into a "data stream", which has useful
                    //  functions.
                    DataInputStream in = new DataInputStream( sock.getInputStream() );
                    //  Read each line of incoming data until there are no more.
                    boolean finished = false;
                    while ( !finished ) {
                        //  Read the size of the next file name....
                        int sz = in.readInt();
                        if ( sz == 0 )
                            finished = true;
                        else {
                            byte[] foo = new byte[sz + 1];
                            in.readFully( foo, 0, sz );
                            String inLine = new String( foo );
                            incrementalCallback( inLine );
                        }
                    }
                    sock.close();
                } catch ( SocketTimeoutException e ) {
                    
                }
                ssock.close();
            } catch ( java.io.IOException e ) {
                
            }
            endCallback();
            _settings.releaseTransferPort( _port );
        }
        
    }

    protected EventListenerList _incrementalListeners;
    protected EventListenerList _endListeners;
    protected int _port;

}
