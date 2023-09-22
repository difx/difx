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
 * This is a fairly silly class that draws a "light" for monitoring activity
 * (such as network traffic - which is what it was originally designed for).
 * The light is normally "off", indicating no acitivity.  When activity occurs,
 * a call to the "data()" function will turn the light on for a short period
 * of time (set using the "onDuration()" function).  If no activity occurs for
 * a period of time (i.e. there are no calls to "data()") the light will display
 * a warning color.  After a further period with no activity it will display an
 * "alert" color.  All periods of time and colors can be changed, if desired.
 * Time periods are measured in 10ths of seconds.  A zero time period effectively
 * means "ignore this".  For instance if you set the "alertTime" to zero, the
 * light will never switch to the alert color.
 */
package mil.navy.usno.widgetlib;

import javax.swing.JPanel;
import javax.swing.Action;
import javax.swing.AbstractAction;
import javax.swing.Timer;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Dimension;

import javax.swing.event.EventListenerList;

public class ActivityMonitorLight extends JPanel {
    
    public ActivityMonitorLight() {
        super();
        //  Set default colors and time intervals.  All times are measured in
        //  10ths of seconds.
        onDuration( 2 );
        warningTime( 200 );
        alertTime( 600 );
        onColor( Color.GREEN );
        offColor( Color.WHITE );
        warningColor( Color.YELLOW );
        alertColor( Color.RED );
        _currentColor = _offColor;
        //  Set up a repeating timeout that occurs every 10th of a second.
        addTimeoutListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                timeoutIntervalEvent();
            }
        });
    }
    
    @Override
    public void paintComponent( Graphics g ) {
        Dimension d = getSize();
        g.setColor( _currentColor );
        g.fillRect( 0, 0, d.width, d.height );
        g.setColor( Color.GRAY );
        g.draw3DRect( 0, 0, d.width - 1, d.height - 1, false );
        //g.draw3DRect( 1, 1, d.width - 3, d.height - 3, false );
    }
    
    /*
     * This function responds to the repeated timeout.
     */
    public void timeoutIntervalEvent() {
        _timer += 1;
        if ( _alertTime > 0 && _timer > _alertTime )
            _currentColor = _alertColor;
        else if ( _warningTime > 0 && _timer > _warningTime )
            _currentColor = _warningColor;
        else if ( _timer > _onDuration )
            _currentColor = _offColor;
        this.updateUI();
    }
    
    /*
     * This function is called whenever there is new data.
     */
    public void data() {
        _currentColor = _onColor;
        _timer = 0;
    }
    
    /*
     * This function causes the light to go on (or off) permanently.  Or sort of.  
     * Depends how long you are willing to wait.
     */
    public void on( boolean newVal ) {
        if ( newVal ) {
            _currentColor = _onColor;
            onDuration( Long.MAX_VALUE );
            alertTime( Long.MAX_VALUE );
            warningTime( Long.MAX_VALUE );
        }
        else {
            _currentColor = _offColor;
            onDuration( 0 );
            alertTime( Long.MAX_VALUE );
            warningTime( Long.MAX_VALUE );
        }
        updateUI();
    }
    
    public boolean on() { return ( _currentColor == _onColor ); }
    
    public void onDuration( long newVal ) {
        _onDuration = newVal;
    }
    public long onDuration() {
        return _onDuration;
    }
    public void warningTime( long newVal ) {
        _warningTime = newVal;
    }
    public long warningTime() {
        return _warningTime;
    }
    public void alertTime( long newVal ) {
        _alertTime = newVal;
    }
    public long alertTime() {
        return _alertTime;
    }
    public void onColor( Color newVal ) {
        _onColor = newVal;
        updateUI();
    }
    public Color onColor() {
        return _onColor;
    }
    public void offColor( Color newVal ) {
        _offColor = newVal;
        updateUI();
    }
    public Color offColor() {
        return _offColor;
    }
    public void warningColor( Color newVal ) {
        _warningColor = newVal;
        updateUI();
    }
    public Color warningColor() {
        return _warningColor;
    }
    public void alertColor( Color newVal ) {
        _alertColor = newVal;
        updateUI();
    }
    public Color alertColor() {
        return _alertColor;
    }
    
    public void warning() {
        _currentColor = _warningColor;
        onDuration( Long.MAX_VALUE );
        alertTime( Long.MAX_VALUE );
        warningTime( Long.MAX_VALUE );
    }
    
    public void alert() {
        _currentColor = _alertColor;
        onDuration( Long.MAX_VALUE );
        alertTime( Long.MAX_VALUE );
        warningTime( Long.MAX_VALUE );
    }
    
    protected long _onDuration;
    protected long _warningTime;
    protected long _alertTime;
    protected long _timer;
    protected Color _onColor;
    protected Color _offColor;
    protected Color _warningColor;
    protected Color _alertColor;
    protected Color _currentColor;


    static EventListenerList _staticTimeoutListeners;
    
    public class TimeoutThread extends Thread {
        protected int _interval;
        protected boolean _keepGoing;
        public TimeoutThread() {
            _keepGoing = true;
        }
        public void keepGoing( boolean newVal ) {
            _keepGoing = newVal;
        }
        @Override
        public void run() {
            while ( _keepGoing ) {
                if ( _staticTimeoutListeners == null )
                    return;
                Object[] listeners = _staticTimeoutListeners.getListenerList();
                int numListeners = listeners.length;
                for ( int i = 0; i < numListeners; i+=2 ) {
                    if ( listeners[i] == ActionListener.class )
                        ((ActionListener)listeners[i+1]).actionPerformed( null );
                }
                try {
                    Thread.sleep( 100 );
                } catch ( Exception e ) {
                    _keepGoing = false;
                }
            }
        }
    }
    
    static TimeoutThread _staticTimeoutThread;
    
    public void addTimeoutListener( ActionListener a ) {
        if ( _staticTimeoutThread == null ) {
            _staticTimeoutThread = new TimeoutThread();
            _staticTimeoutThread.start();
        }
        if ( _staticTimeoutListeners == null )
            _staticTimeoutListeners = new EventListenerList();
        _staticTimeoutListeners.add( ActionListener.class, a );
    }
    
    static public void initializeStatics() {
        _staticTimeoutThread = null;
        _staticTimeoutListeners = null;
    }
    
}
