/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
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

import java.net.SocketTimeoutException;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

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
        _port = _settings.newDifxTransferPort( 0, 100, true, true );
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
            //  will timeout after a given number of seconds (10 minutes to allow
            //  calcif2 the time it needs to run on big jobs).
            try {
                ChannelServerSocket ssock = new ChannelServerSocket( _port, _settings );
                ssock.setSoTimeout( 600000 );  //  timeout is in millisec
                try {
                    ssock.accept();
                    //  Read each line of incoming data until there are no more.
                    boolean finished = false;
                    while ( !finished ) {
                        //  Read the size of the next file name....
                        int sz = ssock.readInt();
                        if ( sz == 0 )
                            finished = true;
                        else {
                            byte[] foo = new byte[sz + 1];
                            ssock.readFully( foo, 0, sz );
                            String inLine = new String( foo );
                            incrementalCallback( inLine );
                        }
                    }
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
