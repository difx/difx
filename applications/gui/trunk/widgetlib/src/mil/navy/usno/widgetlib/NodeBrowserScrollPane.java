/*
 * This panel contains a NodeBrowserPane object.  It measures the size of the
 * NodeBrowserPane and adds a scrollbar if necessary.  The reason it doesn't simply
 * inherit the NodeBrowserPane is simple, if silly - the NodeBrowserPane does a lot of
 * drawing that might fall out of bounds and obscure things like the frame and
 * scrollbars.  By putting it in a sub-panel, anything drawn in it is effectively
 * clipped.
 */
package mil.navy.usno.widgetlib;

import javax.swing.JPanel;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseListener;
import java.awt.event.AdjustmentListener;
import java.awt.event.AdjustmentEvent;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Color;

import java.lang.Thread;

import javax.swing.JScrollBar;

import javax.swing.event.EventListenerList;

/**
 *
 * @author jspitzak
 */
public class NodeBrowserScrollPane extends JPanel implements MouseMotionListener, 
        MouseWheelListener, MouseListener, AdjustmentListener {
    
    public NodeBrowserScrollPane() {
        initialize( 20 );
    }
    
    public NodeBrowserScrollPane( int timeInterval ) {
        initialize( timeInterval );
    }
    
    protected void initialize( int timeInterval ) {
        this.setLayout( null );
        browserPane = new NodeBrowserPane();
        this.add( browserPane );
        _scrollBar = new JScrollBar( JScrollBar.VERTICAL );
        this.add( _scrollBar );
        _scrollBar.addAdjustmentListener( this );
        _scrollBar.setUnitIncrement( 10 );
        _initDecayCount = 10;
        
        //  Capture mouse motion and wheel events.  We don't really care about
        //  clicks.
        addMouseMotionListener( this );
        addMouseWheelListener( this );
        addMouseListener( this );
        
        //  These are counters used for "momentum" in scrolling.
        _offsetMotion = 0;
        
        //  This turns momentum on or off.
        momentumOn( true );
        
        //  This determines the direction stuff moves when you spin the mouse
        //  wheel.  -1 is conventional, but not entirely intuitive.  1 is the
        //  opposite direction.  You can also change the magnitude of scrolling
        //  by making this number bigger.
        _scrollSense = -1;
        
        //  Set ourselves up to respond to a repeating timeout roughly 50 times
        //  a second.  This is used for animation of the browser content.  The
        //  time interval is set to match the timing of drag and mouse wheel
        //  events.  Previously this was actually done with timeouts, but this was
        //  found to bog down the event loop.  The "scroll thread" handles it now.
        _scrollThread = new ScrollThread( timeInterval );
        _scrollThread.start();
        
        //  The yOffset tracks where the browser data are located vertically.
        //  It is measured in pixels.
        _yOffset = 0;
        
        //  The scrolledToEnd flag tells us when we have scrolled to the
        //  bottom of the screen.
        _scrolledToEnd = true;
        
        //  Draw a black frame around the outside of the panel.  Sometimes this
        //  is nice, sometimes not.
        _drawFrame = true;
    
    }
    
    /*
     * Adjust the browser to a change in the list.  This needs to be done when
     * the list is edited in any way.
     */
    public void listChange() {
        boolean scrolledToEnd = scrolledToEnd();
        Dimension d = getSize();
        browserPane.measureDataBounds();
        _scrollBar.setValues( -_yOffset, d.height, 0, browserPane.dataHeight() );
        if ( -_yOffset > browserPane.dataHeight() - d.height ) {
            _yOffset = - ( browserPane.dataHeight() - d.height );
            _scrollBar.setValues( -_yOffset, d.height, 0, browserPane.dataHeight() );
        }
        testScrollBar( d.height );
        if ( scrolledToEnd )
            scrollToEnd();
        _scrollBar.updateUI();
        browserPane.yOffset( _yOffset );
        browserPane.updateUI();
    }
    
    /*
     * This is a similar function that responds to changes in the size of the list -
     * i.e. things already in the list alter their size.  It does not mess with the
     * scroll bar position.
     */
    public void sizeChange() {
        //boolean scrolledToEnd = scrolledToEnd();
        Dimension d = getSize();
        browserPane.measureDataBounds();
        _scrollBar.setValues( -_yOffset, d.height, 0, browserPane.dataHeight() );
        if ( -_yOffset > browserPane.dataHeight() - d.height ) {
            _yOffset = - ( browserPane.dataHeight() - d.height );
            _scrollBar.setValues( -_yOffset, d.height, 0, browserPane.dataHeight() );
        }
        testScrollBar( d.height );
        //if ( scrolledToEnd )
        //    scrollToEnd();   // commented out 10/24/2012 - how's it working?
        //_scrollBar.updateUI();
        browserPane.yOffset( _yOffset );
        //browserPane.updateUI();
    }
    
    public void setYOffset( int newOffset ) {
        Dimension d = getSize();
        int offset;
        if ( newOffset > browserPane.dataHeight() - d.height )
            offset = -( browserPane.dataHeight() - d.height );
        else
            offset = -newOffset;
        _scrollBar.setValues( offset, d.height, 0, browserPane.dataHeight() );
        browserPane.yOffset( -offset );
        browserPane.updateUI();
    }
    
    public int getYOffset() {
        return _yOffset;
    }
    
    public void clear() {
        browserPane.clear();
        Dimension d = getSize();
        browserPane.measureDataBounds();
        _scrollBar.setValues( -_yOffset, d.height, 0, browserPane.dataHeight() ); 
        testScrollBar( d.height );
        dispatchResizeEvent();
    }
    
    /*
     * Used to add a "top level" node to the browser panel.
     */
    public void addNode( BrowserNode newNode ) {
        browserPane.addChild( newNode );
        Dimension d = getSize();
        browserPane.measureDataBounds();
        _scrollBar.setValues( -_yOffset, d.height, 0, browserPane.dataHeight() ); 
        testScrollBar( d.height );
        dispatchResizeEvent();
    }
    
    /*
     * Return the height of the browser pane.
     */
    public int browserHeight() {
        browserPane.measureDataBounds();
        return browserPane.dataHeight();
    }
    
    /*
     * This can be used to cause browser children to indent.  There is a step of
     * indentation for each level above 1, so make the browser 1 if you want it
     * not indented at all, while children (i.e. nodes added to this panel)
     * have indentations of 1.
     */
    public void setLevel( int newVal ) {
        browserPane.setLevel( newVal );
    }
    
    public boolean scrolledToEnd() {
        return _scrolledToEnd;
    }
    
    public void scrollToEnd() {
        if ( _scrollable ) {
            _scrollBar.setValue( _scrollBar.getMaximum() ); 
        }
        _scrolledToEnd = true;
    }
    
    public void scrollToTop() {
        if ( _scrollable ) {
            _scrollBar.setValue( 0 );
            _scrolledToEnd = false;
        }
    }
    
    public void drawFrame( boolean newVal ) { _drawFrame = newVal; }
    
    @Override
    public void setBackground( Color newColor ) {
        if ( browserPane != null )
            browserPane.setBackground( newColor );
        super.setBackground( newColor );
    }
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h ); 
//        browserPane.setBounds( 1, 1, w - 2, h - 2 );
        testScrollBar( h );
        if ( _scrollable && !_noScrollbar ) {
            //  We need to resize the browserPane now to avoid the scrollbar.
            browserPane.setBounds( 1, 1, w - SCROLLBAR_WIDTH - 2, h - 2 );
            _scrollBar.setBounds( w - SCROLLBAR_WIDTH - 1, 1, SCROLLBAR_WIDTH, h - 2);
            _scrollBar.setVisible( true );
        }
        else {
            browserPane.setBounds( 1, 1, w - 2, h - 2 );
            _yOffset = 0;
            browserPane.yOffset( _yOffset );
            _scrollBar.setVisible( false );
        }
    }
    
    public int dataHeight() {
        return browserPane.dataHeight();
    }
    
    public void testScrollBar( int h ) {
        //  See if there are enough data in the browser window to require a
        //  scrollbar.
        _minYOffset = h - browserPane.dataHeight();
        if ( _minYOffset >= 0 ) {
            _scrollable = false;
        }
        else {
            _scrollable = true;
            testScrollBar();
        }
    }
    
    @Override
    public void paintComponent( Graphics g ) {
        super.paintComponent( g );
        Dimension d = getSize();
        //  Draw the scrollbar.
        if ( _scrollable && !_noScrollbar ) {
            g.setColor( Color.LIGHT_GRAY );
            g.fillRect( d.width - SCROLLBAR_WIDTH, 0, SCROLLBAR_WIDTH, d.height );
            g.setColor( Color.BLACK );
            g.drawRect( d.width - SCROLLBAR_WIDTH, 0, SCROLLBAR_WIDTH, d.height );
            _scrollBar.setValues( -_yOffset, d.height, 0, browserPane.dataHeight() ); 
        }
        g.setColor( Color.BLACK );
        if ( _drawFrame )
            g.drawRect( 0, 0, d.width  - 1, d.height - 1 );
    }

    public class ScrollThread extends Thread {
        protected int _interval;
        protected boolean _keepGoing;
        public ScrollThread( int interval ) {
            _interval = interval;
            _keepGoing = true;
        }
        public void keepGoing( boolean newVal ) {
            _keepGoing = newVal;
        }
        @Override
        public void run() {
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
     * Add a "listener" to the timeout events that occur in this class (they are
     * used to redraw the browser at a rapid enough pace that it animates nicely).
     */
    public void addTimeoutEventListener( ActionListener a ) {
        if ( _timeoutEventListeners == null )
            _timeoutEventListeners = new EventListenerList();
        _timeoutEventListeners.add( ActionListener.class, a );
    }

    /*
     * The timeout interval is used to animate the browser, giving scroll operations
     * "momentum".  The tricky bit is getting the momentum to decay with time in
     * a way that is visually pleasing.  Using the actual scrollbar does none of
     * this - it behaves conventionally (no momentum).
     */
    protected void timeoutIntervalEvent() {
        //  It is occasionally desireable for classes to be aware of timeout events
        //  so they can resize things as necessary.  This is kludgey, and probably
        //  causes lots of overhead, so should be avoided.
        //  This capability was added to solve a problem!  It might cause others,
        //  although I hope not...
        if ( _timeoutEventListeners != null ) {
            Object[] listeners = _timeoutEventListeners.getListenerList();
            // loop through each listener and pass on the event if needed
            int numListeners = listeners.length;
            for ( int i = 0; i < numListeners; i+=2 ) {
                if ( listeners[i] == ActionListener.class )
                    ((ActionListener)listeners[i+1]).actionPerformed( null );
            }
        }
        //  This is where the function used to start.
        if ( _scrollBar.getValue() > _scrollBar.getMaximum() - _scrollBar.getVisibleAmount() )
            scrollToEnd();
        if ( _offsetMotion != 0 ) {
            //  Make sure we haven't scrolled too far in either direction.  The
            //  _yOffset measures how far from the top we start drawing browser
            //  items, so it should always be zero or negative.
            if ( _yOffset > 0 ) {
                _yOffset = 0;
                _offsetMotion = 0;
            }
            //  The _yOffset must also not be so large that we are displaying
            //  useless white space at the bottom of the browser.
            else if ( _yOffset < _minYOffset ) {
                _yOffset = _minYOffset;
                _offsetMotion = 0;
            }
            //  Otherwise, adjust the _yOffset for "momentum" motion, and decay 
            //  the amount we adjust.
            else if ( _momentumOn && !_scrolling ) {
                _yOffset += _offsetMotion;
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
            }
            else {
                _yOffset += _offsetMotion;
                if ( !_scrolling )
                    _offsetMotion = 0;
            }
            browserPane.yOffset( _yOffset );
            if ( this.isVisible() )
                this.updateUI();
        }
        //_scrolling = false;
    }
    
    @Override
    public void mouseExited( MouseEvent e ) {
        _scrolling = false;
    }
    
    @Override
    public void mouseEntered( MouseEvent e ) {
    }
    
    @Override
    public void mousePressed( MouseEvent e ) {
    }
    
    @Override
    public void mouseReleased( MouseEvent e ) {
        _scrolling = false;
    }
    
    @Override
    public void mouseClicked(MouseEvent me) {
    }
    
    @Override
    public void mouseMoved( MouseEvent e ) {
        _lastY = e.getY();
        _lastX = e.getX();
        //_scrolling = false;
    }
    
    @Override
    public void mouseDragged( MouseEvent e ) {
        _scrolling = true;
        if ( _scrollable && !_noScrollbar ) {
            _offsetMotion = e.getY() - _lastY;
            _decayCount = _initDecayCount;
            _decayStartCount = _initDecayCount;
            //_lastY = e.getY();
            testScrollBar();
        }
    }
    
    @Override
    public void mouseWheelMoved( MouseWheelEvent e ) {
        if ( _scrollable && !_noScrollbar ) {
            _offsetMotion = _scrollSense * 2 * e.getScrollAmount() * e.getWheelRotation();
            _decayCount = _initDecayCount;
            _decayStartCount = _initDecayCount;
            testScrollBar();
        }
    }
    
    /*
     * Set the direction and magnitude of the scrolling triggered by the mouse
     * wheel.  A value of -1 is conventional.
     */
    public void scrollSense( int newVal ) {
        _scrollSense = newVal;
    }
    public int scrollSense() {
        return _scrollSense;
    }
    
    @Override
    public void adjustmentValueChanged( AdjustmentEvent e ) {
        //  If the value has changed, this is an actual scroll event requiring
        //  a redraw and momentum operations.
        if ( _yOffset != -e.getValue() ) {
            //  Momentum just doesn't look very good...so I'm making the
            //  adjustment directly.
            _yOffset = -e.getValue();
            browserPane.yOffset( _yOffset );
            this.updateUI();
            testScrollBar();
        }
    }
    
    /*
     * This function is used internally to determine if the scrollbar has been
     * positioned at the bottom by the user.  It might be useful to something
     * outside the class, thus it is made public.
     */
    public void testScrollBar() {
        if ( _scrollable ) {
            if ( _scrollBar.getValue() >= _scrollBar.getMaximum() - _scrollBar.getVisibleAmount() )
                _scrolledToEnd = true;
            else
                _scrolledToEnd = false;
        }
    }
    
    public void momentumOn( boolean newVal ) {
        _momentumOn = newVal;
    }
    
    /*
     * Set the callback for changes in sizes of the stuff in the browser pane.
     */
    public void respondToResizeEvents( boolean newVal ) {
        if ( newVal ) {
            browserPane.addResizeEventListener(new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    sizeChange();
                    dispatchResizeEvent();
                }
            });
        }
    }
    
    /*
     * Stop the redraw timer.  This is only needed for scrolling...so if you resize
     * to accommodate expanded sizes, you don't need it.
     */
    public void noTimer() {
        if ( _scrollThread != null )
            _scrollThread.keepGoing( false );
    }

    public void addResizeEventListener( ActionListener a ) {
        if ( _resizeEventListeners == null )
            _resizeEventListeners = new EventListenerList();
        _resizeEventListeners.add( ActionListener.class, a );
    }

    protected void dispatchResizeEvent() {
        if ( _resizeEventListeners == null )
            return;
        Object[] listeners = _resizeEventListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
        
    /*
     * Give access to the browser pane, which is actually the top level node to
     * the tree of nodes that are displayed.
     */
    public BrowserNode browserTopNode() {
        return browserPane;
    }
    
    /*
     * Force the browser to not draw the scrollbar.
     */
    public void noScrollbar( boolean newVal ) { 
        _noScrollbar = newVal;
        if ( _noScrollbar ) {
            _scrollBar.setVisible( false );
        }
        else {
            _scrollBar.setVisible( true );
        }
    }
    
    public void decayCount( int newVal ) { _initDecayCount = newVal; }    
    protected int _initDecayCount;
    
    protected NodeBrowserPane browserPane;
    protected JScrollBar _scrollBar;

    protected EventListenerList _resizeEventListeners;
    protected int _yOffset;
    protected int _offsetMotion;
    protected int _decayCount;
    protected int _decayStartCount;
    protected int _minYOffset;
    protected boolean _scrollable;
    protected int _lastY;
    protected int _lastX;
    protected boolean _scrolling;
    protected boolean _momentumOn;
    protected int _scrollSense;
    protected boolean _scrolledToEnd;
    protected boolean _drawFrame;
    protected boolean _noScrollbar;
    
    protected ScrollThread _scrollThread;
    
    static protected int SCROLLBAR_WIDTH = 16;
    
    protected EventListenerList _timeoutEventListeners;
    
}
