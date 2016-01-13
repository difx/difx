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
 * This thread is used to monitor the network for DiFX multicast messages and
 * queue them for the DiFX message processing thread.  The queueing is necessary
 * to avoid missing a multicast broadcast - messages are collected and queued as
 * fast as possible - the processing thread can take its time figuring out what
 * to do with them.
 * 
 * The slightly strange structure of the "run()" function allows multicast messages
 * to be optionally "relayed" from the guiServer (via TCP).  This permits us to collect
 * the DiFX multicast broadcast even when we are beyond the reach of the
 * messages themselves (guiServer inevitibly runs somewhere close to the DiFX message
 * sources and thus can receive them).
 */
package edu.nrao.difx.difxcontroller;

import edu.nrao.difx.difxview.SystemSettings;
import java.net.*;
import java.io.*;

import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.Logger;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

public class MulticastMonitor extends Thread {

    private boolean _done = false;
    private boolean _settingsChange = true;
    // -- always start the process message thread before this thread.
    private DiFXMessageProcessor _difxMessageProcessor;
    SystemSettings _settings;

    // Constructor, give the thread a name and a link to the system settings.
    public MulticastMonitor( SystemSettings systemSettings ) {
        _settings = systemSettings;
        //  Set up a callback for changes to broadcast items in the system settings.
        _settings.broadcastChangeListener( new ActionListener() {

            public void actionPerformed( ActionEvent e ) {
                updateBroadcastSettings();
            }
        });

    }

    protected void updateBroadcastSettings() {
        _settingsChange = true;
    }

    public void shutDown() {
        _done = true;
    }

    // Methods specific to the message queue
    public void difxMessageProcessor( DiFXMessageProcessor newProcessor ) {
        _difxMessageProcessor = newProcessor;
    }

    /*
     * Loop forever collecting multicast packets either directly (using a UDP connection)
     * or via "relay" from guiServer (TCP connection).  Which we do depends on user
     * settings (and the presence of a connection to guiServer).  The structure here
     * makes the relayed packet collection look like the UDP packet collection.  Switching
     * between the two methods (a single checkbox in the Settings window) will appear
     * seamless.  
     * 
     * Both methods timeout and both report back the number of bytes they receive (a
     * timeout generates 0 bytes, not an error).
     */
    @Override
    public void run() {

        synchronized ( this ) {

            try {
                // start time stamp
                String startDate = Calendar.getInstance().getTime().toString();

                MulticastSocket socket = null;
                
                //  Loop forever, reading multicast packets.
                while ( !_done ) {
                    
                    try {
                        
                        //  We can get packets from either UDP or TCP.  
                        //  This is the TCP relay...
                        if ( _settings.useTCPRelay() ) {
                            try {
                                byte [] buffer = _settings.guiServerConnection().getRelay( _settings.timeout() );
                                if ( buffer != null ) {
                                    //  Feedback for the plot in the settings window
                                    _settings.gotPacket( buffer.length );
                                    //  Add the packet to the processing queue.
                                    if ( !_difxMessageProcessor.add( new ByteArrayInputStream( buffer, 0, buffer.length ) ) ) {
                                        System.out.printf("******** Read message thread packet FAILED to add into queue. \n");
                                    }
                                    buffer = null;
                                }
                            } catch ( SocketTimeoutException e ) {
                                _settings.gotPacket( 0 );
                            }
                        }
                        
                        //  UDP multicast receive (the "original" way)...
                        else {

                            //  Check for changes to the broadcast settings on each cycle.
                            if ( _settingsChange || socket == null ) {
                                socket = new MulticastSocket( _settings.port() );
                                socket.setSoTimeout( _settings.timeout() );              // timeout 100ms
                                socket.setReceiveBufferSize( 512000 );   // max buffer size 512k Bytes
                                socket.joinGroup( InetAddress.getByName( _settings.ipAddress() ) );
                                _settingsChange = false;
                            }

                            // create buffer and datagram packet
                            byte[] buffer = new byte[_settings.bufferSize()];
                            DatagramPacket packet = new DatagramPacket(buffer, buffer.length);

                            //  Do not process empty packets.
                            if ( packet != null ) {
                                try {
                                    socket.receive(packet);
                                    //  Feedback for the plot in the settings window.
                                    _settings.gotPacket( packet.getLength() );
                                    //  Add the packet to the processing queue.
                                    if ( !_difxMessageProcessor.add( new ByteArrayInputStream( packet.getData(), 0, packet.getLength() ) ) ) {
                                        System.out.printf("******** Read message thread packet FAILED to add into queue. \n");
                                    }
                                } catch ( SocketTimeoutException exception ) {
                                    _settings.gotPacket( 0 );
                                    // socket did not receive message within 100ms, clean up
                                    buffer = null;
                                    packet = null;
                                    Thread.yield();  //  BLAT This should not be necessary
                                }

                            } else {
                                System.out.println("******** Read message empty null packet - continue." );
                            }

                            // clean up  BLAT - recreating these each time is really strange...I guess the buffer
                            //  size can change, but enough to justify this constant memory allocation??
                            buffer = null;
                            packet = null;
                        
                        }

                        // catch an interrupt, stop thread
                        if (Thread.currentThread().isInterrupted() == true) {
                            System.out.println("******** Read message thread interrupted. \n" );
                            _done = true;
                        }
                        
                    } catch (OutOfMemoryError exception) {
                        System.out.printf("******** Read message thread caught OutOfMemoryError(%s  %s) - done.\n",
                                startDate, Calendar.getInstance().getTime().toString());
                        _done = true;
                        exception.printStackTrace();
                    }

                } // -- while (!mDone)

                System.out.println( "******** Read message thread done." );
            } catch ( IOException ex ) {
                Logger.getLogger(MulticastMonitor.class.getName()).log(Level.SEVERE, null, ex);
            }

        }

    }

}
