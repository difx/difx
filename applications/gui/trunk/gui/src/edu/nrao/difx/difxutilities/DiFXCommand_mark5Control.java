/*
 * This class composes a "mark5control" message and sends it to a specific host.
 * The XML syntax allows for multiple hosts, but we don't do that currently.
 */
package edu.nrao.difx.difxutilities;

import javax.swing.event.EventListenerList;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import edu.nrao.difx.difxview.SystemSettings;

import java.net.Socket;
import java.net.ServerSocket;
import java.net.SocketTimeoutException;

import java.io.DataInputStream;

import edu.nrao.difx.xmllib.difxmessage.DifxCommand;
import edu.nrao.difx.xmllib.difxmessage.DifxMk5Control;

/**
 *
 * @author difx
 */
public class DiFXCommand_mark5Control extends DiFXCommand {
    
    public DiFXCommand_mark5Control( String cmd, String node, SystemSettings settings, boolean direct ) {
        super( settings );
        _direct = direct;
        //  This sends the command directly (i.e. over the local network using a composed XML
        //  message).  This can be used to issue these commands if you are on the LAN with
        //  your target nodes and guiServer isn't running.s
        if ( _direct ) {
            this.header().setType( "DifxCommand" );
            this.header().setTo( node );
            DifxCommand mark5Command = this.factory().createDifxCommand();
            this.body().setDifxCommand( mark5Command );

            mark5Command.setCommand( cmd );
            this.body().setDifxCommand( mark5Command );
        }
        //  This sends the command via guiServer, which is probably better.
        else {
            this.header().setType( "DifxMk5Control" );
            DifxMk5Control mk5Control = this.factory().createDifxMk5Control();
            mk5Control.setCommand( cmd );
            mk5Control.setTargetNode( node );
            mk5Control.setAddress( _settings.guiServerConnection().myIPAddress() );
            _port = _settings.newDifxTransferPort();
            mk5Control.setPort( _port );
            this.body().setDifxMk5Control( mk5Control );
        }
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
        if ( !_direct ) {
            ResultReader reader = new ResultReader();
            reader.start();
        }
        super.send();
    }
        
    //  This thread reads the results of the mk5control operation from the guiServer.
    //  It can include diagnostic messages as well as data.
    protected class ResultReader extends Thread {
        
        protected final int TASK_TERMINATED                     = 100;
        protected final int TASK_ENDED_GRACEFULLY               = 101;
        protected final int TASK_STARTED                        = 102;
        protected final int PARAMETER_CHECK_IN_PROGRESS         = 103;
        protected final int PARAMETER_CHECK_SUCCESS             = 104;
        protected final int FAILURE_BAD_TARGETNODE              = 105;
        protected final int FAILURE_BAD_COMMAND                 = 106;
        protected final int CANCEL_COMMAND                      = 107;
        protected final int INFORMATION                         = 108;
        protected final int WARNING                             = 109;
        protected final int ERROR                               = 110;

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
                        //  Read the next packet type and size of the packet.
                        int packetType = in.readInt();
                        int packetSize = in.readInt();
                        //  At the moment all of our data is string data...will this
                        //  continue?
                        if ( packetSize > 0 ) {
                            byte[] foo = new byte[packetSize + 1];
                            in.readFully( foo, 0, packetSize );
                            _inLine = new String( foo );
                        }
                        //  Respond to the packet based on its type.
                        switch ( packetType ) {
                            case TASK_STARTED:
                                System.out.println( "task has started" );
                                break;
                            case INFORMATION:
                                System.out.println( "mk5control info: " + _inLine );
                                break;
                            case ERROR:
                                System.out.println( "mk5control info: " + _inLine );
                                break;
                            case TASK_ENDED_GRACEFULLY:
                                System.out.println( "task completed" );
                                break;
                            default:
                                break;
                        }
                    }
                    sock.close();
                } catch ( SocketTimeoutException e ) {
                    
                }
                ssock.close();
            } catch ( Exception e ) {
                
            }
            endCallback();
            _settings.releaseTransferPort( _port );
        }
        
    }

    protected EventListenerList _incrementalListeners;
    protected EventListenerList _endListeners;
    protected int _port;    
    protected boolean _direct;
    protected String _inLine;
    
}
