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
package mil.navy.usno.widgetlib;

import java.lang.Thread;
import java.lang.Runtime;

import java.io.IOException;
import java.io.OutputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import javax.swing.event.EventListenerList;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class PingTest extends Thread {
    
    public PingTest( String hostname ) {
        //  Produce 10 pings by default.
        _pings = 10;
        _hostname = hostname;
    }
    
    /*
     * Set the number of pings in the test.
     */
    public void pings( int newVal ) {
        _pings = newVal;
    }
    public int pings() { return _pings; }
    
    /*
     * Return the most recent ping message.
     */
    public synchronized String lastMessage() {
        return _lastMessage;
    }
    
    /*
     * Return the most recent ping error.
     */
    public synchronized String lastError() {
        return _lastError;
    }
    
    /*
     * Add a listener for "success" events - it will be called when a ping returns
     * successfully.
     */
    public void addSuccessListener( ActionListener a ) {
        if ( _successListeners == null )
            _successListeners = new EventListenerList();
        _successListeners.add( ActionListener.class, a );
    }

    /*
     * Add a listener for "failure" events - it will be called when a ping fails.
     */
    public void addFailureListener( ActionListener a ) {
        if ( _failureListeners == null )
            _failureListeners = new EventListenerList();
        _failureListeners.add( ActionListener.class, a );
    }

    /*
     * Send "success" events to all listeners in the success list.
     */
    protected void dispatchSuccessEvent() {
        if ( _successListeners == null )
            return;
        Object[] listeners = _successListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    /*
     * Send "failure" events to all listeners in the failure list.
     */
    protected void dispatchFailureEvent() {
        if ( _failureListeners == null )
            return;
        Object[] listeners = _failureListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    protected class SuccessThread extends Thread {
        SuccessThread( BufferedReader s ) {
            _in = s;
        }
        BufferedReader _in;
        public void run() {
            try {
                while( ( _lastMessage = _in.readLine()) != null )
                    dispatchSuccessEvent();
            } catch( IOException e ) {
            }
        }
    }
    
    protected class FailureThread extends Thread {
        FailureThread( BufferedReader s ) {
            _in = s;
        }
        BufferedReader _in;
        public void run() {
            try {
                while( ( _lastError = _in.readLine()) != null )
                    dispatchFailureEvent();
            } catch( IOException e ) {
            }
        }
    }
    
    /*
     * The actual thread.  Instead of pinging, say, 10 times, we ping once.
     */
    public void run() {
        try {
            String os = System.getProperty( "os.name" );
            Process p = null;
            if ( os.contentEquals( "Mac OS X" ) )
                p = Runtime.getRuntime().exec( "/sbin/ping -c " + _pings.toString() + " " + _hostname );
            else
                p = Runtime.getRuntime().exec( "/bin/ping -c " + _pings.toString() + " " + _hostname );

            SuccessThread successThread = new SuccessThread( new BufferedReader( new InputStreamReader( p.getInputStream() ) ) );
            FailureThread failureThread = new FailureThread( new BufferedReader( new InputStreamReader( p.getErrorStream() ) ) );
            successThread.start();
            failureThread.start();

        } catch ( IOException e ) {
        }
    }
    
    protected Integer _pings;
    protected EventListenerList _successListeners;
    protected EventListenerList _failureListeners;
    String _lastMessage;
    String _lastError;
    String _hostname;
    
}
