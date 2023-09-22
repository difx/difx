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
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.ButtonGrid;

import javax.swing.JPanel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.Font;
import java.awt.RenderingHints;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.event.EventListenerList;

public class TimeLimitPanel extends JPanel implements MouseMotionListener, 
        MouseWheelListener {
    
    public TimeLimitPanel() {
        super();
        this.setLayout( null );
        addMouseMotionListener( this );
        addMouseWheelListener( this );
        _minTime = new GregorianCalendar();
        _maxTime = new GregorianCalendar();
        _minView = new GregorianCalendar();
        _maxView = new GregorianCalendar();
        _minUser = new GregorianCalendar();
        _maxUser = new GregorianCalendar();
        this.setFont( new Font( "Monospaced", Font.BOLD, 10 ) );
        //  Set ourselves up to respond to a repeating timeout roughly 50 times
        //  a second.  This is used for animation of drag events.
        _timerThread = new TimerThread( 20 );
        _timerThread.start();
    }
    
    public class TimerThread extends Thread {
        protected int _interval;
        protected boolean _keepGoing;
        public TimerThread( int interval ) {
            _interval = interval;
            _keepGoing = true;
        }
        public void keepGoing( boolean newVal ) {
            _keepGoing = newVal;
        }
        @Override
        public void run() {
            _keepGoing = true;
            while ( _keepGoing ) {
                timeoutIntervalEvent();
                try {
                    Thread.sleep( _interval );
                } catch ( Exception e ) {
                    _keepGoing = false;
                }
            }
        }
    }
    
    /*
     * Recompute what should be drawn, and then trigger a draw.  This function is
     * probably called exclusively from other internal functions due to changes
     * in limits or whatever.
     */
    public void redraw() {
        //  The view minimum and maximum determine the scale we label.  The smallest
        //  scale is seconds, the largest is 10's of days.  Each scale (except for
        //  the smallest) will have tic marks showing the next scale down.
        //  The duration is the size of the scale beween the low and high view points
        //  measured in milliseconds.
        long duration = _maxView.getTimeInMillis() - _minView.getTimeInMillis();
        int year = _minView.get( Calendar.YEAR );
        int day = _minView.get( Calendar.DAY_OF_YEAR );
        int hour = _minView.get( Calendar.HOUR_OF_DAY );
        int min = _minView.get( Calendar.MINUTE );
        int sec = _minView.get( Calendar.SECOND );
        Calendar firstLittleStep = new GregorianCalendar();
        Calendar firstBigStep = new GregorianCalendar();
        firstBigStep.clear();
        firstLittleStep.clear();
        firstBigStep.set( Calendar.YEAR, year );
        firstLittleStep.set( Calendar.YEAR, year );
        long adj = 3;
        //  Duration > 10 days?
        if ( duration > ( adj * 864000000 ) ) {
            //  Large step in 10's of days, small step in days.
            _stepType = STEP10DAY;
            _bigStep = 864000000;
            _littleStep = 86400000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, 10 * ( day / 10 + 1 ) );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day + 1 );
        }
        //  Greater than a day?
        else if ( duration > ( adj * 86400000 ) ) {
            //  Large step in days, small step in 6-hour intervals.
            _stepType = STEPDAY;
            _bigStep = 86400000;
            _littleStep = 21600000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day + 1 );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, 6 * ( hour / 6 + 1 ) );
        }
        //  Greater than 6 hours?
        else if ( duration > ( adj * 21600000 ) ) {
            //  Large step in 6 hours, small step in hours.
            _stepType = STEP6HOUR;
            _bigStep = 21600000;
            _littleStep = 3600000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, 6 * ( hour / 6 + 1 ) );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour + 1 );
        }
        //  Greater than an hour?
        else if ( duration > ( adj * 3600000 ) ) {
            //  Large step in hours, small step in 10's of minutes.
            _stepType = STEPHOUR;
            _bigStep = 3600000;
            _littleStep = 600000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour + 1 );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, 10 * ( min / 10 + 1 ) );
        }
        //  Greater than 10 minutes?
        else if ( duration > ( adj * 600000 ) ) {
            //  Large step in 10's of minutes, small step in minutes.
            _stepType = STEP10MIN;
            _bigStep = 600000;
            _littleStep = 60000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour );
            firstBigStep.set( Calendar.MINUTE, 10 * ( min / 10 + 1 ) );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, min + 1 );
        }
        //  Greater than 5 minutes?
        else if ( duration > ( adj * 300000 ) ) {
            //  Large step in 5's of minutes, small step in minutes.
            _stepType = STEP10MIN;
            _bigStep = 300000;
            _littleStep = 60000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour );
            firstBigStep.set( Calendar.MINUTE, 5 * ( min / 5 + 1 ) );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, min + 1 );
        }
        //  Greater than 2 minutes?
        else if ( duration > ( adj * 120000 ) ) {
            //  Large step in 2's of minutes, small step in 1/2 minutes.
            _stepType = STEP10MIN;
            _bigStep = 120000;
            _littleStep = 30000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour );
            firstBigStep.set( Calendar.MINUTE, 2 * ( min / 2 + 1 ) );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, min );
            firstLittleStep.set( Calendar.SECOND, 30 * ( sec / 30 + 1 ) );
        }
        //  Greater than a minute?
        else if ( duration > ( adj * 60000 ) ) {
            //  Large step in minutes, small step in 10's of seconds.
            _stepType = STEPMIN;
            _bigStep = 60000;
            _littleStep = 10000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour );
            firstBigStep.set( Calendar.MINUTE, min + 1 );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, min );
            firstLittleStep.set( Calendar.SECOND, 10 * ( sec / 10 + 1 ) );
        }
        //  Greater than 30 seconds?
        else if ( duration > ( adj * 30000 ) ) {
            //  Large step in 30's of seconds, small step in 5 seconds.
            _stepType = STEP10SEC;
            _bigStep = 30000;
            _littleStep = 5000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour );
            firstBigStep.set( Calendar.MINUTE, min );
            firstBigStep.set( Calendar.SECOND, 30 * ( sec / 30 + 1 ) );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, min );
            firstLittleStep.set( Calendar.SECOND, 5 * ( sec / 5 + 1 ) );
        }
        //  Greater than 10 seconds?
        else if ( duration > ( adj * 10000 ) ) {
            //  Large step in 10's of seconds, small step in seconds.
            _stepType = STEP10SEC;
            _bigStep = 10000;
            _littleStep = 1000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour );
            firstBigStep.set( Calendar.MINUTE, min );
            firstBigStep.set( Calendar.SECOND, 10 * ( sec / 10 + 1 ) );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, min );
            firstLittleStep.set( Calendar.SECOND, sec + 1 );
        }
        //  Greater than 10 seconds total?
        else if ( duration > 10000 ) {
            //  Large step in 5's of seconds, small step in seconds.
            _stepType = STEP10SEC;
            _bigStep = 5000;
            _littleStep = 1000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour );
            firstBigStep.set( Calendar.MINUTE, min );
            firstBigStep.set( Calendar.SECOND, 10 * ( sec / 10 + 1 ) );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, min );
            firstLittleStep.set( Calendar.SECOND, sec + 1 );
        }
        else {
            //  Large step in seconds.  No small step.
            _stepType = STEPSEC;
            _bigStep = 1000;
            _littleStep = 1000;
            firstBigStep.set( Calendar.DAY_OF_YEAR, day );
            firstBigStep.set( Calendar.HOUR_OF_DAY, hour );
            firstBigStep.set( Calendar.MINUTE, min );
            firstBigStep.set( Calendar.SECOND, sec + 1 );
            firstLittleStep.set( Calendar.DAY_OF_YEAR, day );
            firstLittleStep.set( Calendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( Calendar.MINUTE, min );
            firstLittleStep.set( Calendar.SECOND, sec + 1 );
        }
        _viewStart = _minView.getTimeInMillis();
        _viewEnd = _maxView.getTimeInMillis();
        _viewSize = _viewEnd - _viewStart;
        _firstLittleStep = firstLittleStep.getTimeInMillis();
        _firstBigStep = firstBigStep.getTimeInMillis();
        this.updateUI();
    }
    
    /*
     * Set the minimum and maximum limits on this time panel.  We only do this
     * if both the min and max make sense (i.e. are not null).  This is expected
     * to be an "initialization" step, and in that, all limits (the view and the
     * user settings) are set to match the min and max.
     */
    public void limits( Calendar min, Calendar max ) {
        if ( min != null && max != null ) {
            long minVal = min.getTimeInMillis();
            long maxVal = max.getTimeInMillis();
            _minTime.setTimeInMillis( minVal );
            _maxTime.setTimeInMillis( maxVal );
            _minView.setTimeInMillis( minVal );
            _maxView.setTimeInMillis( maxVal );
            _minUser.setTimeInMillis( minVal );
            _maxUser.setTimeInMillis( maxVal );
            redraw();
        }
    }
    
    /*
     * Set user limits to the min and max of the timeline.
     */
    public void maxLimits() {
        _minUser.setTimeInMillis( _minTime.getTimeInMillis() );
        _maxUser.setTimeInMillis( _maxTime.getTimeInMillis() );
    }
    
    /*
     * Class that holds the information about a job.
     */
    protected class ButtonInfo {
        ButtonGrid.GridButton button;
        VexFileParser.Scan scan;
        long start;
        long end;
    }
    
    /*
     * Clear all button information.
     */
    public void clearButtons() {
        if ( _buttonList != null )
            _buttonList.clear();
    }
    
    /*
     * Add a button to the button list.
     */
    public void addButton( ButtonGrid.GridButton button, VexFileParser.Scan scan ) {
        if ( _buttonList == null )
            _buttonList = new ArrayList<ButtonInfo>();
        ButtonInfo newInfo = new ButtonInfo();
        newInfo.button = button;
        newInfo.scan = scan;
        Integer delay = null;
        Integer duration = null;
        for ( Iterator jter = scan.station.iterator(); jter.hasNext(); ) {
            VexFileParser.ScanStation station = (VexFileParser.ScanStation)jter.next();
            if ( delay == null ) {
                delay = station.delay;
                duration = station.duration + station.delay;
            }
            else {
                if ( station.delay < delay )
                    delay = station.delay;
                if ( station.duration + station.delay > duration )
                    duration = station.duration + station.delay;
            }
        }
        if ( delay != null ) {
            Calendar calc = new GregorianCalendar();
            calc.setTimeInMillis( scan.start.getTimeInMillis() );
            calc.add( Calendar.SECOND, delay );
            newInfo.start = calc.getTimeInMillis();
            calc.setTimeInMillis( scan.start.getTimeInMillis() );
            calc.add( Calendar.SECOND, duration );
            newInfo.end = calc.getTimeInMillis();
        }
        _buttonList.add( newInfo );
    }
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
    }
    
    @Override
    public void paintComponent( Graphics g ) {
        super.paintComponent( g );
        Graphics2D g2 = (Graphics2D)g;
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        Dimension d = getSize();
        //  The timeline is drawn to fill the widget area starting at 10 pixels
        //  below the top.  The widget is most ideally sized with a height of 30.
        g2.setColor( Color.WHITE );
        g2.fillRect( 0, 10, d.width  - 1, d.height - 11 );
        //  This scale is used for everything...
        _viewMul = (double)d.width / (double)_viewSize;
        //  Draw the buttons.  Green for "on", gray for "off".
        if ( _buttonList != null ) {
            for ( Iterator<ButtonInfo> iter = _buttonList.iterator(); iter.hasNext(); ) {
                ButtonInfo button = iter.next();
                if ( button.button.on() )
                    g2.setColor( Color.GREEN );
                else
                    g2.setColor( Color.GRAY );
                int x = (int)( (double)( button.start - _minView.getTimeInMillis() ) * _viewMul );
                int y = d.height - 10;
                int w = (int)( (double)( button.end - button.start) * _viewMul );
                int h = 10;
                for ( int i = 0; i < 2; ++i ) {
                    w -= 1;
                    h -= 1;
                    g2.fill3DRect( x++, y++, w, h, true );
                }
            }
        }
        //  Draw the small tic marks from the view start to the end.
        g2.setColor( Color.BLACK );
        long calc = _firstLittleStep;
        while ( calc < _viewEnd ) {
            int xpos = (int)( (double)( calc - _viewStart ) * _viewMul );
            calc += _littleStep;
            g2.drawLine( xpos, d.height - 10, xpos, d.height );
        }
        //  Then the large tic marks.  These are labeled.
        calc = _firstBigStep;
        Calendar calcCal = new GregorianCalendar();
        calcCal.setTimeInMillis( calc );
        int startYear = calcCal.get( Calendar.YEAR );
        int startDay = calcCal.get( Calendar.DAY_OF_YEAR );
        while ( calc < _viewEnd ) {
            int xpos = (int)( (double)( calc - _viewStart ) * _viewMul );
            //  Decide what to include in the label string.
            calcCal.setTimeInMillis( calc );
            String newLabel = "";
            if ( calc == _firstBigStep || calcCal.get( Calendar.YEAR ) != startYear )
                newLabel += calcCal.get( Calendar.YEAR ) + "/";
            if ( calc == _firstBigStep || calcCal.get( Calendar.DAY_OF_YEAR ) != startDay )
                newLabel += calcCal.get( Calendar.DAY_OF_YEAR ) + " ";
            newLabel += String.format( "%02d", calcCal.get( Calendar.HOUR_OF_DAY ) ) + ":" +
                    String.format( "%02d", calcCal.get( Calendar.MINUTE ) ) + ":" +
                    String.format( "%02d", calcCal.get( Calendar.SECOND ) );
            g2.drawLine( xpos, d.height - 13, xpos, d.height );
            g2.drawLine( xpos + 1, d.height - 13, xpos + 1, d.height );
            int w = g2.getFontMetrics().stringWidth( newLabel );
            g2.drawString( newLabel, xpos - w/2, d.height - 15 );
            calc += _bigStep;
        }
        //  Draw the "user limit" buttons.  These are transparent, as they can be put
        //  on top of one another.
        _minPos = (int)( (double)( _minUser.getTimeInMillis() - _minView.getTimeInMillis() ) * _viewMul );
        _maxPos = (int)( (double)( _maxUser.getTimeInMillis() - _minView.getTimeInMillis() ) * _viewMul );
        //  At the moment, I'm making these things red...
        g2.setColor( Color.RED );
        g2.drawLine( _minPos, 10, _minPos, d.height );
        int[] xpts = { _minPos - 5, _minPos, _minPos + 5, _minPos - 5 };
        int[] ypts = { 1, 9, 1, 1 };
        g2.drawPolygon( xpts, ypts, 4 );
        g2.setColor( Color.BLUE );
        g2.drawLine( _maxPos, 10, _maxPos, d.height );
        int[] xpts2 = { _maxPos - 5, _maxPos, _maxPos + 5, _maxPos - 5 };
        int[] ypts2 = { 1, 9, 1, 1 };
        g2.drawPolygon( xpts2, ypts2, 4 );
        g2.setColor( Color.BLACK );
        g2.drawRect( 0, 10, d.width  - 1, d.height - 11 );
    }
    
    /*
     * The timeout interval is used to animate the browser, giving scroll operations
     * "momentum".  The tricky bit is getting the momentum to decay with time in
     * a way that is visually pleasing.  Using the actual scrollbar does none of
     * this - it behaves conventionally (no momentum).
     */
    protected void timeoutIntervalEvent() {
        if ( _offsetMotion != 0 ) {
            //  Change the position of the timeline and "decay" the offset motion.
            _minView.add( Calendar.MILLISECOND, -(int)( (double)_offsetMotion / _viewMul ) );
            _maxView.add( Calendar.MILLISECOND, -(int)( (double)_offsetMotion / _viewMul ) );
            --_decayCount;
            if ( _decayCount < 1 ) {
                if ( _offsetMotion > 0 ) {
                    _decayCount = _decayStartCount - _offsetMotion;
                    --_offsetMotion;
                }
                else if ( _offsetMotion < 0 ) {
                    _decayCount = _decayStartCount + _offsetMotion;
                    ++_offsetMotion;
                }
            }
            //  Make sure we haven't moved too far in either direction.
            if ( _minView.before( _minTime ) ) {
                _maxView.add( Calendar.MILLISECOND, (int)( _minTime.getTimeInMillis() - _minView.getTimeInMillis() ) );
                _minView.setTimeInMillis( _minTime.getTimeInMillis() );
                _offsetMotion = 0;
            }
            else if ( _maxView.after( _maxTime ) ) {
                _minView.add( Calendar.MILLISECOND, (int)( _maxTime.getTimeInMillis() - _maxView.getTimeInMillis() ) );
                _maxView.setTimeInMillis( _maxTime.getTimeInMillis() );
                _offsetMotion = 0;
            }
            redraw();
        }
        else
            _timerThread.keepGoing( false );
    }

    @Override
    public void mouseMoved( MouseEvent e ) {
        _lastX = e.getX();
        _lastY = e.getY();
        //  Track whether we are over the "user maximum" button.
        if ( _lastX > _maxPos - 5 && _lastX < _maxPos + 5 ) {
            _inMaxButtonLimits = true;
            _inMaxTime = _maxUser.getTimeInMillis();
        }
        //  Also, if the user maximum is off the right edge of the screen, we will
        //  allow the user to grab it as if it is right on the edge.
        else if ( _maxUser.after( _maxView ) && _lastX > this.getWidth() - 5 ) {
            _inMaxButtonLimits = true;
            _inMaxTime = _maxView.getTimeInMillis();
        }
        else
            _inMaxButtonLimits = false;
        //  Same for the user minimum.
        if ( _lastX > _minPos - 5 && _lastX < _minPos + 5 ) {
            _inMinButtonLimits = true;
            _inMinTime = _minUser.getTimeInMillis();
        }
        else if ( _minUser.before( _minView ) && _lastX < 5 ) {
            _inMinButtonLimits = true;
            _inMinTime = _minView.getTimeInMillis();
        }
        else
            _inMinButtonLimits = false;
    }
    
    @Override
    public void mouseDragged( MouseEvent e ) {
        //  Don't do this unless the mouse in IN the timeline.
        if ( _lastY > 10 ) {
            _offsetMotion = e.getX() - _lastX;
            _decayCount = 10;
            _decayStartCount = 10;
            _lastX = e.getX();
            //  This try/catch is a bit lame...but I believe it catches complaints
            //  triggered when the thread is already started so the exception can be
            //  safely ignored.  Hopefully there aren't other good reasons to pay 
            //  attention to it.
            try { _timerThread.start(); } catch ( java.lang.IllegalThreadStateException ex ) {}
        }
        //  Otherwise, see if we are dragging the user limit buttons.
        else {
            boolean moveMin = false;
            boolean moveMax = false;
            _offsetMotion = e.getX() - _lastX;
            if ( _inMinButtonLimits && _inMaxButtonLimits ) {
                if ( _offsetMotion < 0 )
                    moveMin = true;
                else
                    moveMax = true;
            }
            else if ( _inMinButtonLimits )
                moveMin = true;
            else if ( _inMaxButtonLimits )
                moveMax = true;
            if ( moveMin ) {
                _minUser.setTimeInMillis( _inMinTime + (long)( _offsetMotion / _viewMul ) );
                if ( _minUser.before( _minTime ) )
                    _minUser.setTimeInMillis( _minTime.getTimeInMillis() );
                if ( _minUser.after( _maxTime ) )
                    _minUser.setTimeInMillis( _maxTime.getTimeInMillis() );
                if ( _minUser.after( _maxUser ) )
                    _maxUser.setTimeInMillis( _minUser.getTimeInMillis() );
                if ( _minUser.before( _minView ) ) {
                    _maxView.add( Calendar.MILLISECOND, (int)( _minUser.getTimeInMillis() - _minView.getTimeInMillis() ) );
                    _minView.setTimeInMillis( _minUser.getTimeInMillis() );
                }
                redraw();
            }
            else if ( moveMax ) {
                _maxUser.setTimeInMillis( _inMaxTime + (long)( _offsetMotion / _viewMul ) );
                if ( _maxUser.before( _minTime ) )
                    _maxUser.setTimeInMillis( _minTime.getTimeInMillis() );
                if ( _maxUser.after( _maxTime ) )
                    _maxUser.setTimeInMillis( _maxTime.getTimeInMillis() );
                if ( _maxUser.before( _minUser ) )
                    _minUser.setTimeInMillis( _maxUser.getTimeInMillis() );
                if ( _maxUser.after( _maxView ) ) {
                    _minView.add( Calendar.MILLISECOND, (int)( _maxUser.getTimeInMillis() - _maxView.getTimeInMillis() ) );
                    _maxView.setTimeInMillis( _maxUser.getTimeInMillis() );
                }
                else
                redraw();
            }
            if ( moveMin || moveMax ) {
                //  Check all buttons against the new user time limits.  Turn off any buttons
                //  outside of them, and turn on any inside.  There may be other limitations
                //  that ultimately turn these buttons off again.
                for ( Iterator<ButtonInfo> iter = _buttonList.iterator(); iter.hasNext(); ) {
                    ButtonInfo button = iter.next();
                    if ( button.end > _maxUser.getTimeInMillis() || button.start < _minUser.getTimeInMillis() )
                        button.button.on( false );
                    else
                        button.button.on( true );
                }
            }
        }   
    }
    
    @Override
    public void mouseWheelMoved( MouseWheelEvent e ) {
        double lowFraction = (double)( e.getX() ) / (double)( getWidth() );
        int scaler = e.getScrollAmount() * e.getWheelRotation();
        //  Each scroll click zooms in or out by 10%...
        double span = (double)( _maxView.getTimeInMillis() - _minView.getTimeInMillis() );
        double ypos = lowFraction * span;
        if ( scaler < 0 ) { //  Zoom in
            while ( scaler < 0 ) {
                span *= 0.9;
                scaler += 1;
            }
        }
        else {
            while ( scaler > 0 ) {
                span *= 1.05;
                scaler -= 1;
            }
        }
        //  Don't zoom in with more the 3 seconds resolution...
        if ( span < 3000.0 )
            span = 3000.01; //  make sure the integer comes out as 3000!
        //  Now we try to keep the value under the mouse the same.  This will break
        //  down as we zoom out to limits.
        long newMin = _minView.getTimeInMillis() + (long)( ypos - lowFraction * span );
        long newMax = newMin + (long)span;
        if ( newMin < _minTime.getTimeInMillis() )
            newMin = _minTime.getTimeInMillis();
        if ( newMax > _maxTime.getTimeInMillis() )
            newMax = _maxTime.getTimeInMillis();
        _minView.setTimeInMillis( newMin );
        _maxView.setTimeInMillis( newMax );
        redraw();
    }
    
    public void addChangeListener( ActionListener a ) {
        if ( _changeListeners == null )
            _changeListeners = new EventListenerList();
        _changeListeners.add( ActionListener.class, a );
    }

    protected void dispatchAction( ActionEvent actionEvent ) {
        Object[] listeners = _changeListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }

    protected EventListenerList _changeListeners;
    protected Calendar _minTime;
    protected Calendar _maxTime;
    protected Calendar _minView;
    protected Calendar _maxView;
    protected Calendar _minUser;
    protected Calendar _maxUser;
    protected long _firstBigStep;
    protected long _firstLittleStep;
    protected int _bigStep;
    protected int _littleStep;
    protected static int STEP10DAY = 1;
    protected static int STEPDAY = 2;
    protected static int STEP6HOUR = 3;
    protected static int STEPHOUR = 4;
    protected static int STEP10MIN = 5;
    protected static int STEPMIN = 6;
    protected static int STEP10SEC = 7;
    protected static int STEPSEC = 8;
    protected int _stepType;
    protected long _viewStart;
    protected long _viewEnd;
    protected long _viewSize;
    protected int _lastX;
    protected int _lastY;
    protected int _offsetMotion;
    protected int _decayCount;
    protected int _decayStartCount;
    protected double _viewMul;
    protected int _minPos;
    protected int _maxPos;
    protected boolean _inMinButtonLimits;
    protected boolean _inMaxButtonLimits;
    protected long _inMinTime;
    protected long _inMaxTime;
    protected ArrayList<ButtonInfo> _buttonList;
    protected TimerThread _timerThread;
    
}
