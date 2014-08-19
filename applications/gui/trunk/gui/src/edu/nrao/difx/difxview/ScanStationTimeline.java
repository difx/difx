/*
 * Shows a timeline outlining which stations are utilized in each scan.  Stations can
 * be turned on or off individually or based on time.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.IndexedPanel;
import mil.navy.usno.widgetlib.ComplexToolTip;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseListener;

import javax.swing.JPanel;
import javax.swing.JToolTip;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.Font;
import java.awt.RenderingHints;
import java.awt.Point;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Iterator;

import javax.swing.event.EventListenerList;

public class ScanStationTimeline extends IndexedPanel implements MouseMotionListener, 
        MouseWheelListener, MouseListener {
    
    static private final int TOP_MARGIN    = 50;
    static private final int BOTTOM_MARGIN = 15;
    static private final int LEFT_MARGIN    = 80;
    static private final int RIGHT_MARGIN = 80;
    static private final int STATION_HEIGHT = 30;
    static private final int FONT_SIZE = 10;
    static private final int BUTTON_SIZE = 8;
    
    public ScanStationTimeline( String title ) {
        super( title );
        this.setLayout( null );
        addMouseMotionListener( this );
        addMouseWheelListener( this );
        _minTime = new GregorianCalendar();
        _maxTime = new GregorianCalendar();
        _minView = new GregorianCalendar();
        _maxView = new GregorianCalendar();
        _minUser = new GregorianCalendar();
        _maxUser = new GregorianCalendar();
        this.setFont( new Font( "Monospaced", Font.BOLD, FONT_SIZE ) );
        _timeline = this;
    }
    
    public void vexData( VexFileParser newData ) {
        _vexData = newData;
        _scanList = new ArrayList<ScanContainer>();
        for ( Iterator<VexFileParser.Scan> iter = _vexData.scanList().iterator(); iter.hasNext(); )// {
            _scanList.add( new ScanContainer( iter.next() ) );
        //  Create the lists of buttons used to set time limits.  This is a list of lists...one list
        //  for each station.
        _buttons = new ArrayList<ArrayDeque<TimeLimitButton>>();
        for ( Iterator<VexFileParser.Station> iter = _vexData.stationList().iterator(); iter.hasNext(); ) {
            iter.next();
            ArrayDeque<TimeLimitButton> newList = new ArrayDeque<TimeLimitButton>();
            newList.add( new TimeLimitButton( null, true ) );
            newList.add( new TimeLimitButton( null, false ) );
            _buttons.add( newList );
        }
        redrawVexData();
    }
    
    @Override
    public void closedHeight( int h ) {
        super.closedHeight( h );
        this.openHeight( closedHeight() );
    }
    
    public void redrawVexData() {
        if ( _vexData == null )
            return;
        if ( _vexData.antennaList().size() <= 0 )
            return;
        //  Set the size of the panel based on the number of stations.
        this.openHeight( _vexData.antennaList().size() * STATION_HEIGHT + TOP_MARGIN + BOTTOM_MARGIN );
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
        int year = _minView.get( GregorianCalendar.YEAR );
        int day = _minView.get( GregorianCalendar.DAY_OF_YEAR );
        int hour = _minView.get( GregorianCalendar.HOUR_OF_DAY );
        int min = _minView.get( GregorianCalendar.MINUTE );
        int sec = _minView.get( GregorianCalendar.SECOND );
        GregorianCalendar firstLittleStep = new GregorianCalendar();
        GregorianCalendar firstBigStep = new GregorianCalendar();
        firstBigStep.clear();
        firstLittleStep.clear();
        firstBigStep.set( GregorianCalendar.YEAR, year );
        firstLittleStep.set( GregorianCalendar.YEAR, year );
        long adj = 3;
        //  Duration > 10 days?
        if ( duration > ( adj * 864000000 ) ) {
            //  Large step in 10's of days, small step in days.
            _stepType = STEP10DAY;
            _bigStep = 864000000;
            _littleStep = 86400000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, 10 * ( day / 10 + 1 ) );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day + 1 );
        }
        //  Greater than a day?
        else if ( duration > ( adj * 86400000 ) ) {
            //  Large step in days, small step in 6-hour intervals.
            _stepType = STEPDAY;
            _bigStep = 86400000;
            _littleStep = 21600000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day + 1 );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, 6 * ( hour / 6 + 1 ) );
        }
        //  Greater than 6 hours?
        else if ( duration > ( adj * 21600000 ) ) {
            //  Large step in 6 hours, small step in hours.
            _stepType = STEP6HOUR;
            _bigStep = 21600000;
            _littleStep = 3600000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, 6 * ( hour / 6 + 1 ) );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour + 1 );
        }
        //  Greater than an hour?
        else if ( duration > ( adj * 3600000 ) ) {
            //  Large step in hours, small step in 10's of minutes.
            _stepType = STEPHOUR;
            _bigStep = 3600000;
            _littleStep = 600000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour + 1 );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, 10 * ( min / 10 + 1 ) );
        }
        //  Greater than 10 minutes?
        else if ( duration > ( adj * 600000 ) ) {
            //  Large step in 10's of minutes, small step in minutes.
            _stepType = STEP10MIN;
            _bigStep = 600000;
            _littleStep = 60000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstBigStep.set( GregorianCalendar.MINUTE, 10 * ( min / 10 + 1 ) );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, min + 1 );
        }
        //  Greater than 5 minutes?
        else if ( duration > ( adj * 300000 ) ) {
            //  Large step in 5's of minutes, small step in minutes.
            _stepType = STEP10MIN;
            _bigStep = 300000;
            _littleStep = 60000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstBigStep.set( GregorianCalendar.MINUTE, 5 * ( min / 5 + 1 ) );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, min + 1 );
        }
        //  Greater than 2 minutes?
        else if ( duration > ( adj * 120000 ) ) {
            //  Large step in 2's of minutes, small step in 1/2 minutes.
            _stepType = STEP10MIN;
            _bigStep = 120000;
            _littleStep = 30000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstBigStep.set( GregorianCalendar.MINUTE, 2 * ( min / 2 + 1 ) );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, min );
            firstLittleStep.set( GregorianCalendar.SECOND, 30 * ( sec / 30 + 1 ) );
        }
        //  Greater than a minute?
        else if ( duration > ( adj * 60000 ) ) {
            //  Large step in minutes, small step in 10's of seconds.
            _stepType = STEPMIN;
            _bigStep = 60000;
            _littleStep = 10000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstBigStep.set( GregorianCalendar.MINUTE, min + 1 );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, min );
            firstLittleStep.set( GregorianCalendar.SECOND, 10 * ( sec / 10 + 1 ) );
        }
        //  Greater than 30 seconds?
        else if ( duration > ( adj * 30000 ) ) {
            //  Large step in 30's of seconds, small step in 5 seconds.
            _stepType = STEP10SEC;
            _bigStep = 30000;
            _littleStep = 5000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstBigStep.set( GregorianCalendar.MINUTE, min );
            firstBigStep.set( GregorianCalendar.SECOND, 30 * ( sec / 30 + 1 ) );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, min );
            firstLittleStep.set( GregorianCalendar.SECOND, 5 * ( sec / 5 + 1 ) );
        }
        //  Greater than 10 seconds?
        else if ( duration > ( adj * 10000 ) ) {
            //  Large step in 10's of seconds, small step in seconds.
            _stepType = STEP10SEC;
            _bigStep = 10000;
            _littleStep = 1000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstBigStep.set( GregorianCalendar.MINUTE, min );
            firstBigStep.set( GregorianCalendar.SECOND, 10 * ( sec / 10 + 1 ) );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, min );
            firstLittleStep.set( GregorianCalendar.SECOND, sec + 1 );
        }
        //  Greater than 10 seconds total?
        else if ( duration > 10000 ) {
            //  Large step in 5's of seconds, small step in seconds.
            _stepType = STEP10SEC;
            _bigStep = 5000;
            _littleStep = 1000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstBigStep.set( GregorianCalendar.MINUTE, min );
            firstBigStep.set( GregorianCalendar.SECOND, 10 * ( sec / 10 + 1 ) );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, min );
            firstLittleStep.set( GregorianCalendar.SECOND, sec + 1 );
        }
        else {
            //  Large step in seconds.  No small step.
            _stepType = STEPSEC;
            _bigStep = 1000;
            _littleStep = 1000;
            firstBigStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstBigStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstBigStep.set( GregorianCalendar.MINUTE, min );
            firstBigStep.set( GregorianCalendar.SECOND, sec + 1 );
            firstLittleStep.set( GregorianCalendar.DAY_OF_YEAR, day );
            firstLittleStep.set( GregorianCalendar.HOUR_OF_DAY, hour );
            firstLittleStep.set( GregorianCalendar.MINUTE, min );
            firstLittleStep.set( GregorianCalendar.SECOND, sec + 1 );
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
    
    @Override
    public void paintComponent( Graphics g ) {
        super.paintComponent( g );
        Graphics2D g2 = (Graphics2D)g;
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        Dimension d = getSize();
        _bottomEdge = d.height - BOTTOM_MARGIN;
        _rightEdge = d.width - RIGHT_MARGIN;
        //  Don't draw if we have no antennas.
        if ( _vexData == null || _vexData.antennaList() == null )
            return;
        //  Don't try drawing this crazy thing if there isn't space.
        if ( d.height < TOP_MARGIN + BOTTOM_MARGIN + STATION_HEIGHT * _vexData.antennaList().size() )
            return;
        if ( d.width < LEFT_MARGIN + RIGHT_MARGIN + 2 )
            return;
        //  The timeline is drawn as a grid with some borders around it.  The bottom border
        //  is purely visual.  Station names appear in the left border, buttons for time limits
        //  appear in the right, and the top border is used for time labels.
        g2.setColor( Color.WHITE );
        g2.fillRect( LEFT_MARGIN, TOP_MARGIN, d.width  - 1 - LEFT_MARGIN - RIGHT_MARGIN, d.height - 1 - TOP_MARGIN - BOTTOM_MARGIN );
        //  This scale is used for everything...
        _viewMul = (double)( d.width - LEFT_MARGIN - RIGHT_MARGIN ) / (double)_viewSize;
        //  Draw the small tic marks from the view start to the end.
        long calc = _firstLittleStep;
        while ( calc < _viewEnd ) {
            int xpos = LEFT_MARGIN + (int)( (double)( calc - _viewStart ) * _viewMul );
            calc += _littleStep;
            g2.setColor( Color.BLACK );
            g2.drawLine( xpos, TOP_MARGIN - 10, xpos, TOP_MARGIN );
            g2.setColor( Color.LIGHT_GRAY );
            g2.drawLine( xpos, TOP_MARGIN, xpos, d.height - BOTTOM_MARGIN );
        }
        //  Then the large tic marks.  These are labeled.
        calc = _firstBigStep;
        GregorianCalendar calcCal = new GregorianCalendar();
        calcCal.setTimeInMillis( calc );
        int startYear = calcCal.get( GregorianCalendar.YEAR );
        int startDay = calcCal.get( GregorianCalendar.DAY_OF_YEAR );
        while ( calc < _viewEnd ) {
            int xpos = LEFT_MARGIN + (int)( (double)( calc - _viewStart ) * _viewMul );
            //  Decide what to include in the label string.
            calcCal.setTimeInMillis( calc );
            String newLabel = "";
            if ( calc == _firstBigStep || calcCal.get( GregorianCalendar.YEAR ) != startYear )
                newLabel += calcCal.get( GregorianCalendar.YEAR ) + "/";
            if ( calc == _firstBigStep || calcCal.get( GregorianCalendar.DAY_OF_YEAR ) != startDay )
                newLabel += calcCal.get( GregorianCalendar.DAY_OF_YEAR ) + " ";
            newLabel += String.format( "%02d", calcCal.get( GregorianCalendar.HOUR_OF_DAY ) ) + ":" +
                    String.format( "%02d", calcCal.get( GregorianCalendar.MINUTE ) ) + ":" +
                    String.format( "%02d", calcCal.get( GregorianCalendar.SECOND ) );
            g2.setColor( Color.BLACK );
            g2.drawLine( xpos, TOP_MARGIN - 13, xpos, TOP_MARGIN );
            g2.drawLine( xpos + 1, TOP_MARGIN - 13, xpos + 1, TOP_MARGIN );
            g2.setColor( Color.LIGHT_GRAY );
            g2.drawLine( xpos, TOP_MARGIN, xpos, d.height - BOTTOM_MARGIN );
            g2.drawLine( xpos + 1, TOP_MARGIN, xpos + 1, d.height - BOTTOM_MARGIN );
            int w = g2.getFontMetrics().stringWidth( newLabel );
            g2.setColor( Color.BLACK );
            g2.drawString( newLabel, xpos - w/2, TOP_MARGIN - 15 );
            calc += _bigStep;
        }
        //  Lines and labels for the stations.
        int stationCount = 0;
        for ( Iterator<VexFileParser.Station> iter = _vexData.stationList().iterator(); iter.hasNext(); ) {
            VexFileParser.Station station = iter.next();
            g2.setColor( Color.BLACK );
            g2.drawString( station.name, LEFT_MARGIN - 15 - g2.getFontMetrics().stringWidth( station.name ), 
                    TOP_MARGIN + stationCount * STATION_HEIGHT + STATION_HEIGHT / 2 + FONT_SIZE / 2 );
            ++stationCount;
            if ( stationCount < _vexData.stationList().size() ) {
                g2.setColor( Color.LIGHT_GRAY );
                g2.drawLine( LEFT_MARGIN, TOP_MARGIN + STATION_HEIGHT * stationCount, d.width - RIGHT_MARGIN,
                        TOP_MARGIN + STATION_HEIGHT * stationCount );
            }
        }
        //  Clip the button field to avoid drawing buttons off the edge.
        g2.clipRect( LEFT_MARGIN, TOP_MARGIN, d.width  - LEFT_MARGIN - RIGHT_MARGIN, d.height - TOP_MARGIN - BOTTOM_MARGIN );
        //  Draw the scans.  We run through the entire list, discarding those that don't
        //  fall within the displayed time limits.
        if ( _scanList != null ) {
            for ( Iterator<ScanContainer> iter = _scanList.iterator(); iter.hasNext(); ) {
                ScanContainer scan = iter.next();
                if ( scan.insideLimits( _viewStart, _viewEnd ) )
                    scan.draw( g2 );
                else
                    scan.dontDraw();
            }
        }
        //  Undo the clipping.
        g2.setClip( null ); 
        //  A frame around everything.
        g2.setColor( Color.BLACK );
        g2.drawRect( LEFT_MARGIN, TOP_MARGIN, d.width - LEFT_MARGIN - RIGHT_MARGIN, d.height - TOP_MARGIN - BOTTOM_MARGIN );
        //  The buttons used to set time limits.  There is (at least) one list of these for each station.
        //  There are also "global" buttons that move a whole series of the others.
        if ( _buttons != null ) {
            stationCount = 0;
            for ( Iterator<ArrayDeque<TimeLimitButton>> iter = _buttons.iterator(); iter.hasNext(); ) {
                ArrayDeque<TimeLimitButton> buttonList = iter.next();
                for ( Iterator<TimeLimitButton> iter2 = buttonList.iterator(); iter2.hasNext(); ) {
                    TimeLimitButton button = iter2.next();
                    int xpos = 0;
                    int ypos = TOP_MARGIN + STATION_HEIGHT * stationCount + STATION_HEIGHT / 2 - BUTTON_SIZE;
                    boolean alphaButton = false;
                    boolean fixedButton = false;
                    //  If the time associated with the button is null, its position is "fixed"
                    //  off the right side of the plot.
                    if ( button.time() == null ) {
                        if ( button.go() )
                            xpos = d.width - RIGHT_MARGIN + 35;
                        else
                            xpos = d.width - RIGHT_MARGIN + 10;
                        fixedButton = true;
                    }
                    //  If there is a drag button, it should know its own xposition.
                    else if ( _dragButton == button ) {
                        xpos = _dragButton.xpos;
                        //  Convert the position to a time.
                        if ( _dragButton.xpos + BUTTON_SIZE > LEFT_MARGIN && _dragButton.xpos + BUTTON_SIZE < d.width - RIGHT_MARGIN ) {
                            _dragButton.time( (long)((double)(_dragButton.xpos + BUTTON_SIZE - LEFT_MARGIN) / _viewMul) + _viewStart );
                            //  Add a moving time label to the timeline along the top.
                            calcCal.setTimeInMillis( _dragButton.time() );
                            String newLabel = "";
                            newLabel += calcCal.get( GregorianCalendar.YEAR ) + "/";
                            newLabel += calcCal.get( GregorianCalendar.DAY_OF_YEAR ) + " ";
                            newLabel += String.format( "%02d", calcCal.get( GregorianCalendar.HOUR_OF_DAY ) ) + ":" +
                                    String.format( "%02d", calcCal.get( GregorianCalendar.MINUTE ) ) + ":" +
                                    String.format( "%02d", calcCal.get( GregorianCalendar.SECOND ) );
                            if ( _dragButton.go() )
                                g2.setColor( Color.GREEN.darker() );
                            else
                                g2.setColor( Color.RED );
                            g2.drawLine( _dragButton.xpos + BUTTON_SIZE, TOP_MARGIN - 13, _dragButton.xpos + BUTTON_SIZE, TOP_MARGIN );
                            int w = g2.getFontMetrics().stringWidth( newLabel );
                            g2.drawString( newLabel, _dragButton.xpos + BUTTON_SIZE - w/2, TOP_MARGIN - 15 );
                        }
                        else {
                            //  The button is out of bounds, and thus time should be zero.
                            _dragButton.time( (long)0 );
                        }
                        alphaButton = true;
                    }
                    //  Otherwise, this is a stationary button - convert its time to a position.
                    else {
                        xpos = LEFT_MARGIN + (int)( (double)( button.time() - _viewStart ) * _viewMul ) - BUTTON_SIZE + 1;
                        button.xpos = xpos;
                    }
                    //  Apply clipping to "field" buttons - those not being dragged and not
                    //  "fixed".
                    if ( !alphaButton && !fixedButton )
                        g2.clipRect( LEFT_MARGIN, TOP_MARGIN, d.width  - LEFT_MARGIN - RIGHT_MARGIN, d.height - TOP_MARGIN - BOTTOM_MARGIN );
                    drawButton( g2, button.go(), xpos, ypos, alphaButton );
                    if ( !alphaButton && !fixedButton ) {
                        g2.setClip( null ); 
                        g2.setColor( Color.BLACK );
                        g2.drawRect( LEFT_MARGIN, TOP_MARGIN, d.width  - LEFT_MARGIN - RIGHT_MARGIN, d.height - TOP_MARGIN - BOTTOM_MARGIN );
                    }
                }
                ++stationCount;
            }
        }
    }

    /*
     * Function to draw the little "stop/go" signs.
     */
    protected void drawButton( Graphics2D g2, boolean go, int xpos, int ypos, boolean alphaButton ) {
        //  Color and shape depends on the button "type" - go or not.
        if ( go ) {
            if ( alphaButton )
                g2.setColor( new Color( 0, 255, 0, 100 ) );
            else
                g2.setColor( Color.GREEN );
            g2.fillOval( xpos, ypos, 16, 16 );
            g2.setColor( Color.BLACK );
            g2.drawOval( xpos, ypos, 16, 16 );
            g2.drawLine( xpos + 8, ypos + 4, xpos + 8, ypos + 12 );
            g2.drawLine( xpos + 4, ypos + 8, xpos + 12, ypos + 8 );
        }
        else {
            if ( alphaButton )
                g2.setColor( new Color( 255, 0, 0, 100 ) );
            else
                g2.setColor( Color.RED );
            int[] xs = new int[8];
            int[] ys = new int[8];
            xs[0] = xpos + 5;
            xs[1] = xpos + 11;
            xs[2] = xpos + 16;
            xs[3] = xs[2];
            xs[4] = xs[1];
            xs[5] = xs[0];
            xs[6] = xpos;
            xs[7] = xs[6];
            ys[0] = ypos;
            ys[1] = ys[0];
            ys[2] = ypos + 5;
            ys[3] = ypos + 11;
            ys[4] = ypos + 16;
            ys[5] = ys[4];
            ys[6] = ys[3];
            ys[7] = ys[2];
            g2.fillPolygon( xs, ys, 8 );
            g2.setColor( Color.BLACK );
            g2.drawPolygon( xs, ys, 8 );
            g2.drawLine( xpos + 4, ypos + 8, xpos + 12, ypos + 8 );
        }
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
            _minView.add( GregorianCalendar.MILLISECOND, -(int)( (double)_offsetMotion / _viewMul ) );
            _maxView.add( GregorianCalendar.MILLISECOND, -(int)( (double)_offsetMotion / _viewMul ) );
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
                _maxView.add( GregorianCalendar.MILLISECOND, (int)( _minTime.getTimeInMillis() - _minView.getTimeInMillis() ) );
                _minView.setTimeInMillis( _minTime.getTimeInMillis() );
                _offsetMotion = 0;
            }
            else if ( _maxView.after( _maxTime ) ) {
                _minView.add( GregorianCalendar.MILLISECOND, (int)( _maxTime.getTimeInMillis() - _maxView.getTimeInMillis() ) );
                _maxView.setTimeInMillis( _maxTime.getTimeInMillis() );
                _offsetMotion = 0;
            }
            redraw();
        }
        else
            _timerThread.keepGoing( false );
    }

    @Override
    public void mouseExited( MouseEvent e ) {
    }
    
    @Override
    public void mouseEntered( MouseEvent e ) {
    }
    
    @Override
    public void mousePressed( MouseEvent e ) {
        mousePress( e.getX(), e.getY() );
    }
    
    public void mousePress( int x, int y ) {
        //  Grab a mouse location for a drag event.  The little start and stop buttons
        //  can be dragged, as can the whole timeline.
        _dragButton = null;
        _pushInTimeline = false;
        Dimension d = getSize();
        //  Check outselves against all of the start and stop buttons first.  
        if ( _buttons != null ) {
            int stationCount = 0;
            for ( Iterator<ArrayDeque<TimeLimitButton>> iter = _buttons.iterator(); iter.hasNext() && _dragButton == null; ) {
                ArrayDeque<TimeLimitButton> buttonList = iter.next();
                int ypos = TOP_MARGIN + STATION_HEIGHT * stationCount + STATION_HEIGHT / 2 - BUTTON_SIZE;
                //  Look at the buttons in reverse order so we get the most recent one first (that way
                //  it will appear to be "on top").
                for ( Iterator<TimeLimitButton> iter2 = buttonList.descendingIterator(); iter2.hasNext() && _dragButton == null; ) {
                    TimeLimitButton button = iter2.next();
                    int xpos = 0;
                    //  If the time associated with the button is null, its position is "fixed"
                    //  off the right side of the plot.
                    if ( button.time() == null ) {
                        if ( button.go() )
                            xpos = d.width - RIGHT_MARGIN + 35;
                        else
                            xpos = d.width - RIGHT_MARGIN + 10;
                    }
                    //  Otherwise, the button's xposition will have been set in the last draw. 
                    else {
                        xpos = button.xpos;
                    }
                    //  See if we are in this button.
                    if ( x > xpos && x < xpos + 16 && y > ypos && y < ypos + 16 ) {
                        //  Okay!  If this is one of the "fixed" buttons, it means we should now
                        //  create a new button for dragging to a new location.  The time
                        //  associated with this button is zero.
                        if ( button.time() == null ) {
                            _dragButton = new TimeLimitButton( (long)0, button.go() );
                            buttonList.add( _dragButton );
                        }
                        //  This is a pre-existing button that the user wants to move.
                        else
                            _dragButton = button;
                    }
                }
                ++stationCount;
            }
        }
        //  If we weren't pushing a button, see if we are in the timeline.
        if ( _dragButton == null ) {
            if ( x > LEFT_MARGIN && x < d.width + LEFT_MARGIN &&
                 y > closedHeight() && y < TOP_MARGIN + d.height ) {
                _pushInTimeline = true;
                _pushX = x;
                _pushY = y;
                _lastX = x;
                _lastY = y;
            }
        }
    }
    
    @Override
    public void mouseReleased( MouseEvent e ) {
        mouseRelease( e.getX(), e.getY() );
    }
    
    public void mouseRelease( int x, int y ) {
        //  See if we are dragging a button.  If so, delete it if we are outside the
        //  timeline.
        if ( _dragButton != null ) {
            if ( _dragButton.time() == (long)0 ) {
                //  Find the button and delete it.
                for ( Iterator<ArrayDeque<TimeLimitButton>> iter = _buttons.iterator(); iter.hasNext(); ) {
                    ArrayDeque<TimeLimitButton> buttonList = iter.next();
                    buttonList.remove( _dragButton );
                }
            }
            _dragButton = null;
            redraw();
        }
        //  Otherwise, this is a timeline event.
        if ( _pushInTimeline ) {
            if ( _timerThread != null )
                _timerThread.keepGoing( false );
            try { Thread.sleep( 40 ); } catch ( Exception ex ) {} 
            //  Set ourselves up to respond to a repeating timeout roughly 50 times
            //  a second.  This is used for animation of drag events.
            _decayCount = 10;
            _decayStartCount = 10;
            _timerThread = new TimerThread( 20 );
            _timerThread.start();
        }
    }
    
    @Override
    public void mouseClicked(MouseEvent e) {
        super.mouseClicked( e );
        mouseClick( e.getX(), e.getY() );
    }
    
    public void mouseClick( int x, int y ) {
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
    
    public void highlightOff() {
        if ( _scanList != null ) {
            for ( Iterator<ScanContainer> iter = _scanList.iterator(); iter.hasNext(); ) {
                ScanContainer scan = iter.next();
                scan.highlight( false );
            }
        }
    }
    
    @Override
    public void mouseMoved( MouseEvent e ) {
        //  If we have this event, it means we aren't moving over one of the station areas.
        //  Any highlights should thus be off.
        highlightOff();
    }
    
    @Override
    public void mouseDragged( MouseEvent e ) {
        dragEvent( e.getX(), e.getY() );
    }
    
    public void dragEvent( int x, int y ) {
        //  See if we are dragging one of the start/stop buttons.
        if ( _dragButton != null ) {
            //  All we need do here is save the position of the drag button...
            _dragButton.xpos = x - BUTTON_SIZE;
            //  ...and change the state of all scans that might have been affected.
            implementButtonDrag();
            vexChangeEvent();
        }
        //  Don't do this unless the mouse was pushed in the timeline.
        else if ( _pushInTimeline && x - _lastX != 0 ) {
            _offsetMotion = x - _lastX;
            _lastX = x;
            _minView.add( GregorianCalendar.MILLISECOND, -(int)( (double)_offsetMotion / _viewMul ) );
            _maxView.add( GregorianCalendar.MILLISECOND, -(int)( (double)_offsetMotion / _viewMul ) );
            if ( _minView.before( _minTime ) ) {
                _maxView.add( GregorianCalendar.MILLISECOND, (int)( _minTime.getTimeInMillis() - _minView.getTimeInMillis() ) );
                _minView.setTimeInMillis( _minTime.getTimeInMillis() );
                _offsetMotion = 0;
            }
            else if ( _maxView.after( _maxTime ) ) {
                _minView.add( GregorianCalendar.MILLISECOND, (int)( _maxTime.getTimeInMillis() - _maxView.getTimeInMillis() ) );
                _maxView.setTimeInMillis( _maxTime.getTimeInMillis() );
                _offsetMotion = 0;
            }
            redraw();
        }
    }
    
    /*
     * This function is called as a result of a drag event in one of the "start" or "stop"
     * buttons in the timeline.  It requires us to check the state of all scans associated
     * with the station to which the dragged button applies, and turn the station on or
     * off within the scan based on the drag.  We can ignore the other stations as only one
     * drag is done at a time.
     */
    protected void implementButtonDrag() {
        //  Don't bother if there is no drag button (probably won't be calling this
        //  function in that case, but it doesn't hurt us).
        if ( _dragButton != null ) {
            //  Locate the list of buttons for the station in the list of those lists.
            ArrayDeque<TimeLimitButton> stationButtonList = null;
            int stationCount = 0;
            if ( _buttons != null ) {
                for ( Iterator<ArrayDeque<TimeLimitButton>> iter = _buttons.iterator(); iter.hasNext() && stationButtonList == null; ) {
                    ArrayDeque<TimeLimitButton> buttonList = iter.next();
                    for ( Iterator<TimeLimitButton> iter2 = buttonList.iterator(); iter2.hasNext() && stationButtonList == null; ) {
                        TimeLimitButton button = iter2.next();
                        if ( button == _dragButton )
                            stationButtonList = buttonList.clone();
                    }
                    if ( stationButtonList == null )
                        ++stationCount;
                }
            }
            //  If for some reason that didn't work, bail out of here.
            if ( stationButtonList == null )
                return;
            //  We now should know the number of the station - get the station
            //  associated with it (in the vex data).
            if ( _vexData == null )
                return;
            VexFileParser.Station station = null;
            for ( Iterator<VexFileParser.Station> iter = _vexData.stationList().iterator(); iter.hasNext() && stationCount > -1; ) {
                station = iter.next();
                --stationCount;
            }
            //  Create a time-ordered list of the buttons associated with this station.
            ArrayDeque<TimeLimitButton> buttonList = new ArrayDeque<TimeLimitButton>();
            //  Remove any buttons from the station button list that have null or zero times.
            for ( Iterator<TimeLimitButton> iter = stationButtonList.iterator(); iter.hasNext(); ) {
                TimeLimitButton button = iter.next();
                if ( button.time() == null || button.time() == (long)0 )
                    stationButtonList.remove( button );
            }
            //  Now use the station button list as a source to produce a time-ordered list of
            //  the remaining buttons.
            while ( stationButtonList.size() > 0 ) {
                //  Find the button in the station button list that has the smallest time.
                TimeLimitButton lowButton = null;
                for ( Iterator<TimeLimitButton> iter = stationButtonList.iterator(); iter.hasNext(); ) {
                    TimeLimitButton button = iter.next();
                    if ( lowButton == null )
                        lowButton = button;
                    else if ( button.time() < lowButton.time() )
                        lowButton = button;
                }
                //  Add it to our new button list and remove it from the station button list.
                if ( lowButton != null ) {
                    buttonList.add( lowButton );
                    stationButtonList.remove( lowButton );
                }
            }
            //  We have to look at all of the scans and see where their station uses fall in the
            //  timing of the buttons.  We use the drawn scans, not the ones in the vex data
            //  (as this is all visual).
            if ( _scanList != null ) {
                for ( Iterator<ScanContainer> iter = _scanList.iterator(); iter.hasNext(); ) {
                    ScanContainer scan = iter.next();
                    //  See if the station we are interested in is used in this scan.
                    ScanContainer.StationContainer scanStation = null;
                    for ( Iterator<ScanContainer.StationContainer> iter2 = scan.stationList().iterator(); iter2.hasNext() && scanStation == null; ) {
                        ScanContainer.StationContainer testStation = iter2.next();
                        if ( testStation.station().name.equalsIgnoreCase( station.name ) )
                            scanStation = testStation;
                    }
                    //  If we use the station, compare the positions at which it is drawn to
                    //  the locations of buttons.
                    if ( scanStation != null ) {
                        //  Find the buttons that are before and after the start and stop
                        //  of the station.
                        TimeLimitButton beforeStart = null;
                        TimeLimitButton afterStart = null;
                        TimeLimitButton beforeEnd = null;
                        TimeLimitButton afterEnd = null;
                        for ( Iterator<TimeLimitButton> iter2 = buttonList.iterator(); iter2.hasNext(); ) {
                            TimeLimitButton button = iter2.next();
                            int contains = scanStation.contains( button.time() );
                            if ( contains == -1 )
                                beforeStart = button;
                            if ( contains == -1 || contains == 0 )
                                beforeEnd = button;
                            if ( afterStart == null && contains == 0 || contains == 1 )
                                afterStart = button;
                            if ( afterEnd == null && contains == 1 )
                                afterEnd = button;
                        }
                        //  Turn the station on or off within the scan based on the types of
                        //  buttons that surround its start and end.
                        if ( beforeStart == null || beforeStart.go() ) {
                            //  Button starts in a "go" regime.  See if it stays there.
                            if ( ( afterStart == null || afterStart.go() || afterStart == afterEnd ) && ( beforeEnd == null || beforeEnd.go() ) )
                                scanStation.station().omitFlag = false;
                            else
                                scanStation.station().omitFlag = true;
                        }
                        else {
                            //  Button starts in a "stop" regime, and thus the station should
                            //  be shut off.
                            scanStation.station().omitFlag = true;
                        }
                    }
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
    
    public void addVexChangeListener( ActionListener a ) {
        if ( _changeListeners == null )
            _changeListeners = new EventListenerList();
        _changeListeners.add( ActionListener.class, a );
    }

    protected void vexChangeEvent() {
        if ( _changeListeners == null )
            return;
        Object[] listeners = _changeListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    /*
     * The "ScanContainer" class draws a blue outline around a scan that includes all
     * of its stations.  Its total width shows the duration of the scan.  ScanContainers
     * have highly informative tooltips that describe their current state and the state
     * of their stations (switched on/off, etc).
     */
    protected class ScanContainer extends Object {
        public ScanContainer( VexFileParser.Scan scan ) {
            _scan = scan;
            _drawn = false;
            //  Figure out the start and stop time of this scan.  At the same time,
            //  make a list of the stations.
            _stationList = new ArrayList<StationContainer>();
            _firstStation = _vexData.stationList().size();
            _lastStation = -1;
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
                StationContainer stationContainer = new StationContainer( station, scan.start.getTimeInMillis() );
                _stationList.add( stationContainer );
                _timeline.add( stationContainer );
                if ( stationContainer.listLocation() > _lastStation )
                    _lastStation = stationContainer.listLocation();
                if ( stationContainer.listLocation() < _firstStation )
                    _firstStation = stationContainer.listLocation();
            }
            GregorianCalendar cal = new GregorianCalendar();
            cal.setTimeInMillis( scan.start.getTimeInMillis() );
            if ( delay != null )
                cal.add( GregorianCalendar.SECOND, delay );
            _scanStart = cal.getTimeInMillis();
            cal.setTimeInMillis( scan.start.getTimeInMillis() );
            if ( duration != null )
                cal.add( GregorianCalendar.SECOND, duration );
            _scanEnd = cal.getTimeInMillis();
        }
        public boolean insideLimits( long start, long end ) {
            if ( _scanStart < end && _scanEnd > start )
                return true;
            else
                return false;
        }
        //  Use the view limits to figure out where to draw this component.
        public void draw( Graphics2D g2 ) {
            _xpos = LEFT_MARGIN + (int)( (double)( _scanStart - _viewStart ) * _viewMul );
            _width = LEFT_MARGIN + (int)( (double)( _scanEnd - _viewStart ) * _viewMul ) - _xpos;            
            _ypos = TOP_MARGIN + STATION_HEIGHT * _firstStation + 1;
            _height = TOP_MARGIN + STATION_HEIGHT * ( _lastStation + 1 ) - 1 - _ypos;
            _drawn = true;
            if ( _highlight )
                g2.setColor( Color.BLUE );
            else {
                float[] hsb = Color.RGBtoHSB( 100, 150, 255, null );
                g2.setColor( Color.getHSBColor( hsb[0], hsb[1], hsb[2] ) );
            }
            g2.drawRect( _xpos, _ypos, _width, _height );
            g2.drawRect( _xpos - 1, _ypos - 1, _width + 2, _height + 2 );
            //  Now draw the stations.
            for ( Iterator<StationContainer> iter = _stationList.iterator(); iter.hasNext();) {
                StationContainer station = iter.next();
                station.xpos = LEFT_MARGIN + (int)( (double)( station.start() - _viewStart ) * _viewMul ) + 2;
                station.width = LEFT_MARGIN + (int)( (double)( station.end() - _viewStart ) * _viewMul ) - station.xpos - 1;            
                station.ypos = TOP_MARGIN + STATION_HEIGHT * station.listLocation() + 3;
                station.height = TOP_MARGIN + STATION_HEIGHT * ( station.listLocation() + 1 ) - 2 - station.ypos;
                station.setBounds( station.xpos, station.ypos, station.width, station.height );
                g2.setColor( Color.ORANGE );
                if ( station.station().omitFlag ) {
                    g2.drawRect( station.xpos, station.ypos, station.width, station.height );
                    g2.drawRect( station.xpos + 1, station.ypos + 1, station.width - 3, station.height - 3 );
                }
                else
                    g2.fillRect( station.xpos, station.ypos, station.width, station.height );
                if ( _highlight ) {
                    g2.setColor( Color.ORANGE.darker().darker() );
                    g2.drawRect( station.xpos, station.ypos, station.width - 1, station.height - 1 );
                    g2.drawRect( station.xpos + 1, station.ypos + 1, station.width - 3, station.height - 3 );
                    g2.drawString( station.station().name.toUpperCase(), station.xpos + 5, station.ypos + ( station.height + FONT_SIZE ) / 2 );
                }
            }
            //  Draw the name of this scan in the upper right corner.
            if ( _highlight )
                g2.setColor( Color.BLUE );
            else {
                float[] hsb = Color.RGBtoHSB( 100, 150, 255, null );
                g2.setColor( Color.getHSBColor( hsb[0], hsb[1], hsb[2] ) );
            }
            g2.drawString( _scan.name, _xpos + _width - 5 - g2.getFontMetrics().stringWidth( _scan.name ), 
                    _ypos + FONT_SIZE + 5 );
        }
        public void dontDraw() {
            _drawn = false;
        }
        /*
         * Test whether this scan should be highlighted.  To highlight a scan, the mouse
         * should be over one of the station observations associated with it, as these are
         * unique (and scans can overlap).
         */
        public void highlightTest( int x, int y ) {
            _highlight = false;
            for ( Iterator<StationContainer> iter = _stationList.iterator(); iter.hasNext();) {
                StationContainer station = iter.next();
                if ( x >= station.xpos && x <= station.xpos + station.width &&
                     y >= station.ypos && y <= station.ypos + station.height )
                    _highlight = true;
            }            
        }
        /*
         * Test if this click occurs inside one of the stations - if so, its "omit" flag
         * should be toggled.
         */
        public void clickTest( int x, int y ) {
            for ( Iterator<StationContainer> iter = _stationList.iterator(); iter.hasNext();) {
                StationContainer station = iter.next();
                if ( x >= station.xpos && x <= station.xpos + station.width &&
                     y >= station.ypos && y <= station.ypos + station.height )
                    station.station().omitFlag = !station.station().omitFlag;
            }            
        }
        public void highlight( boolean newVal ) {
            _highlight = newVal;
        }
        public void buildTooltip() {
            _tooltip = _scan.name;
            _tooltip += "\n" + "<<bold>>Mode: <</bold>>" + _scan.mode;
            _tooltip += "\n" + "<<bold>>Source: <</bold>>" + _scan.source;
            _tooltip += "\n" + "<<bold>>Start: <</bold>>" + _scanStart + " (";
            GregorianCalendar calcCal = new GregorianCalendar();
            calcCal.setTimeInMillis( _scanStart );
            _tooltip += calcCal.get( GregorianCalendar.YEAR ) + "/";
            _tooltip += calcCal.get( GregorianCalendar.DAY_OF_YEAR ) + " ";
            _tooltip += String.format( "%02d", calcCal.get( GregorianCalendar.HOUR_OF_DAY ) ) + ":" +
                    String.format( "%02d", calcCal.get( GregorianCalendar.MINUTE ) ) + ":" +
                    String.format( "%02d", calcCal.get( GregorianCalendar.SECOND ) );
            _tooltip += ")";
            _tooltip += "\n";
            if ( _scan.omitFlag )
                _tooltip += "<<red>>SCAN OMITTED<</color>>\n";
            _tooltip += "<<FIXED>>";
            for ( Iterator<StationContainer> iter = _stationList.iterator(); iter.hasNext(); ) {
                StationContainer station = iter.next();
                _tooltip += "\n<<bold>>" + station.station().name.toUpperCase() + ":<</bold>>   ";
                _tooltip += "delay: " + String.format( "%02d", station.station().delay ) + "   ";
                _tooltip += "duration: " + String.format( "%04d", station.station().duration );
                if ( station.station().omitFlag )
                    _tooltip += "<<red>>  STATION OMITTED<</color>>";
            }            
        }
        protected VexFileParser.Scan _scan;
        protected ArrayList<StationContainer> _stationList;
        public ArrayList<StationContainer> stationList() { return _stationList; }
        protected boolean _drawn;
        protected long _scanStart;
        protected long _scanEnd;
        protected int _firstStation;
        protected int _lastStation;
        protected int _xpos;
        protected int _ypos;
        protected int _width;
        protected int _height;
        protected boolean _highlight;
        protected String _tooltip;
        public class StationContainer extends JPanel implements MouseMotionListener, MouseListener {
            public StationContainer( VexFileParser.ScanStation station, long scanStart ) {
                _station = station;
                //  Where is this in the vex file?
                _listLocation = 0;
                boolean found = false;
                for ( Iterator<VexFileParser.Station> iter = _vexData.stationList().iterator(); iter.hasNext() && !found; ) {
                    VexFileParser.Station testStation = iter.next();
                    if ( testStation.name.equalsIgnoreCase( _station.name ) )
                        found = true;
                    else
                        ++_listLocation;
                }
                //  Compute the start and stop time for this station (in the scan).
                int delay = station.delay;
                int duration = station.duration + station.delay;
                GregorianCalendar cal = new GregorianCalendar();
                cal.setTimeInMillis( scanStart );
                cal.add( GregorianCalendar.SECOND, delay );
                _start = cal.getTimeInMillis();
                cal.setTimeInMillis( scanStart );
                cal.add( GregorianCalendar.SECOND, duration );
                _end = cal.getTimeInMillis();
                addMouseMotionListener( this );
                addMouseListener( this );
            }
            /*
             * Return -1 if the given time is before this station's time
             * limits, 1 if after, and 0 if contained within.
             */
            public int contains( long testTime ) {
                if ( _scanStart > testTime )
                    return -1;
                if ( _scanEnd < testTime )
                    return 1;
                return 0;
            }
            public void paintComponent( Graphics g ) {
            }
            @Override
            public void mouseMoved( MouseEvent e ) {
            }
            @Override
            public void mouseDragged( MouseEvent e ) {
                _timeline.dragEvent( e.getX() + xpos, e.getY() + ypos );
            }
            @Override
            public void mouseEntered( MouseEvent e ) {
                if ( xpos + e.getX() > LEFT_MARGIN && xpos + e.getX() < _rightEdge ) {
                    buildTooltip();
                    this.setToolTipText( _tooltip );
                    _highlight = true;
                    redraw();
                }
            }
            @Override
            public void mouseExited( MouseEvent e ) {
                if ( xpos + e.getX() > LEFT_MARGIN && xpos + e.getX() < _rightEdge ) {
                    _highlight = false;
                    redraw();
                }
            }
            @Override
            public void mouseReleased( MouseEvent e ) {
                _timeline.mouseRelease( e.getX() + xpos, e.getY() + ypos );
            }
            @Override
            public void mousePressed( MouseEvent e ) {
                _timeline.mousePress( e.getX() + xpos, e.getY() + ypos );
            }
            @Override
            public void mouseClicked( MouseEvent e ) {
                if ( xpos + e.getX() > LEFT_MARGIN && xpos + e.getX() < _rightEdge ) {
                    _station.omitFlag = !_station.omitFlag;
                    buildTooltip();
                    this.setToolTipText( _tooltip );
                    vexChangeEvent();
                }
                else
                    _timeline.mouseClick( e.getX() + xpos, e.getY() + ypos );
            }
            @Override
            public JToolTip createToolTip() {
                _tip = new ComplexToolTip();
                _tip.setComponent( this );
//                _tip.dynamicLinkPath( _dynamicLinkPath );
                return _tip;
            }
            @Override
            public Point getToolTipLocation( MouseEvent e) {
                return new Point( 10, getHeight() );
            }
            protected VexFileParser.ScanStation _station;
            protected int _listLocation;
            protected long _start;
            protected long _end;
            public int xpos;
            public int ypos;
            public int width;
            public int height;
            public VexFileParser.ScanStation station() { return _station; }
            public int listLocation() { return _listLocation; }
            public long start() { return _start; }
            public long end() { return _end; }
            ComplexToolTip _tip;
        }
    }
    /*
     * The TimeLimitButton class draws a little stop or start button along the
     * timeline at a specific time (and associated with a specific station).  There
     * are also "fixed" buttons on the right side of the plot that are used to
     * create new buttons.
     */
    class TimeLimitButton extends Object {
        public TimeLimitButton( Long time, boolean go ) {
            _time = time;
            _go = go;
        }
        protected Long _time;
        protected boolean _go;
        public int xpos;
        public void time( long newTime ) { _time = newTime; }
        public Long time() { return _time; }
        public boolean go() { return _go; }
    }
    protected EventListenerList _changeListeners;
    protected GregorianCalendar _minTime;
    protected GregorianCalendar _maxTime;
    protected GregorianCalendar _minView;
    protected GregorianCalendar _maxView;
    protected GregorianCalendar _minUser;
    protected GregorianCalendar _maxUser;
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
    protected ArrayList<ScanContainer> _scanList;
    protected ArrayList<ArrayDeque<TimeLimitButton>> _buttons;
    protected TimerThread _timerThread;
    protected boolean _pushInTimeline;
    protected int _pushX;
    protected int _pushY;
    protected int _bottomEdge;
    protected int _rightEdge;
    protected ScanStationTimeline _timeline;
    protected TimeLimitButton _dragButton;
    
    protected VexFileParser _vexData;
    
}
