/*
 * This panel shows a browse-able list of messages.
 */
package mil.navy.usno.widgetlib;

import mil.navy.usno.widgetlib.MessageNode;
import javax.swing.JPanel;

import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.AdjustmentEvent;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Font;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.StringSelection;
import java.awt.Toolkit;

import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.Calendar;

import javax.swing.JScrollBar;

/**
 *
 * @author jspitzak
 */
public class MessageScrollPane extends JPanel implements MouseMotionListener, 
        MouseWheelListener, AdjustmentListener, ClipboardOwner {
    
    public MessageScrollPane() {
        this.setLayout( null );
        _messageList = new ArrayDeque<MessageNode>();
        this.setBackground( Color.BLACK );
        _highlightColor = Color.GRAY;
        this.messageFont( new Font( Font.MONOSPACED, Font.BOLD, 12 ) );
        _scrollBar = new JScrollBar( JScrollBar.VERTICAL );
        this.add( _scrollBar );
        _scrollBar.addAdjustmentListener( this );
        _scrollBar.setUnitIncrement( 10 );
        _maxMessages = new Integer( 1000 );
        
        //  Capture mouse motion and wheel events.  We don't really care about
        //  clicks.
        addMouseMotionListener( this );
        addMouseWheelListener( this );
        
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
        _scrollThread = new ScrollThread( 20 );
        _scrollThread.start();
        
        //  The yOffset tracks where the browser data are located vertically.
        //  It is measured in pixels.
        _yOffset = 0;
        
        //  The scrolledToEnd flag tells us when we have scrolled to the
        //  bottom of the screen.
        _scrolledToEnd = true;
            
    }
    
    
    void close() {
        _scrollThread.stopNow();
        //  Makes sure the thread has actually stopped by waiting twice its interval.
        try { Thread.sleep( 40 ); } catch ( Exception e ) {}
        _messageList.clear();
        _messageList = null;
    }
    
    /*
     * Measure the height of all (visible) messages in the list.
     */
    public int measureDataHeight() {
        int height = 0;
        synchronized( _messageList ) {
            for ( Iterator<MessageNode> iter = _messageList.iterator(); iter.hasNext(); ) {
                MessageNode thisMessage = iter.next();
                if ( thisMessage.showThis() )
                    height += messageHeight();
            }
        }
        Dimension d = getSize();
//        height += d.height - ( d.height / messageHeight() ) * messageHeight();
        height += messageHeight();
        return height;
    }
    
    public int messageHeight() {
        if ( _messageFont != null )
            return _messageFont.getSize();
        else
            return 15;
    }
    /*
     * Adjust the browser to a change in the list.  This needs to be done when
     * the list is edited in any way.
     */
    public void listChange() {
        boolean scrolledToEnd = scrolledToEnd();
        Dimension d = getSize();
        int dataHeight = measureDataHeight();
        _scrollBar.setValues( -_yOffset, d.height, 0, dataHeight );
        if ( -_yOffset > dataHeight - d.height ) {
            _yOffset = - ( dataHeight - d.height );
            _scrollBar.setValues( -_yOffset, d.height, 0, dataHeight );
        }
        testScrollBar( d.height );
        if ( scrolledToEnd )
            scrollToEnd();
        this.updateUI();
    }
    
    public void setYOffset( int newOffset ) {
        Dimension d = getSize();
        int offset;
        int dataHeight = measureDataHeight();
        if ( newOffset > dataHeight - d.height )
            offset = -( dataHeight - d.height );
        else
            offset = -newOffset;
        _scrollBar.setValues( offset, d.height, 0, dataHeight );
        this.updateUI();
    }
    
    public int getYOffset() {
        return _yOffset;
    }
    
    public void clear() {
        System.out.println( "there are " + _messageList.size() + " messages" );
        synchronized( _messageList ) {
            _messageList.clear();
        }
        System.out.println( "now there are " + _messageList.size() );
        Dimension d = getSize();
        int dataHeight = measureDataHeight();
        _scrollBar.setValues( -_yOffset, d.height, 0, dataHeight ); 
        testScrollBar( d.height );
    }
    
    public void maxMessages( int newVal ) {
        _maxMessages = new Integer( newVal );
    }
    
    public void maxMessagesOff() {
        _maxMessages = null;
    }
    
    public void addMessage( MessageNode newNode ) {
        boolean atEnd = scrolledToEnd();
        //  In case messages are sent here after the "close()" operation.
        if ( _messageList == null ) return;
        synchronized( _messageList ) {
            _messageList.add( newNode );
            if ( _maxMessages != null ) {
                while ( _messageList.size() > _maxMessages )
                    _messageList.remove();
            }
        }
        Dimension d = getSize();
        int dataHeight = measureDataHeight();
        _scrollBar.setValues( -_yOffset, d.height, 0, dataHeight );
        if ( atEnd )
            scrollToEnd();
        listChange();
        //  Eliminate messages older than a given time, measured in milliseconds.
        if ( _diffTime != null ) {
            long refTime = Calendar.getInstance().getTimeInMillis() - _diffTime;
            synchronized ( _messageList ) {
                while ( _messageList.peek() != null && _messageList.peek().isOlderThan( refTime ) ) {
                    _messageList.remove();
                }
            }
        }
        if ( _maxMessages != null ) {
            synchronized( _messageList ) {
                while ( _messageList.size() > _maxMessages )
                    _messageList.remove();
            }
        }
        //testScrollBar( d.height );
        //this.updateUI();
    }
    
    /*
     * Get rid of messages that are a set number of days, hours, minutes, and
     * seconds old.
     */
    public void clearOlderThan( int days, int hours, int minutes, int seconds ) {
        _diffTime = new Long( 1000 * ( seconds + 60 * ( minutes + 60 * ( hours + 24 * days ) ) ) );
    }
    
    /*
     * Turn off the above behavior.
     */
    public void clearOlderThanOff() {
        _diffTime = null;
    }
    
    public void removeMessage( MessageNode newNode ) {
        synchronized( _messageList ) {
            _messageList.remove( newNode );
        }
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
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h ); 
        testScrollBar( h );
        if ( _scrollable ) {
            //  We need to resize the browserPane now to avoid the scrollbar.
            _scrollBar.setBounds( w - SCROLLBAR_WIDTH - 1, 1, SCROLLBAR_WIDTH, h - 2);
            _scrollBar.setVisible( true );
        }
        else {
            _yOffset = 0;
            _scrollBar.setVisible( false );
            this.updateUI();
        }
    }
    
    public void testScrollBar( int h ) {
        //  See if there are enough data in the browser window to require a
        //  scrollbar.
        _minYOffset = h - measureDataHeight();
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
        //  Use anti-aliasing on the text (looks much better)
        Graphics2D g2 = (Graphics2D)( g.create() );
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        //  Draw the text in a clipped area.
        g2.setFont( _messageFont );
        Dimension d = getSize();
        if ( _scrollable )
            g2.setClip( 0, 0, d.width - SCROLLBAR_WIDTH, d.height );
        g2.setColor( this.getBackground() );
        g2.fillRect( 0, 0, d.width  - 1, d.height - 1 );
        int y = _yOffset;
        synchronized( _messageList ) {
            for ( Iterator<MessageNode> iter = _messageList.iterator(); iter.hasNext(); ) {
                MessageNode thisMessage = iter.next();
                if ( thisMessage.showThis() ) {
                    y += messageHeight();
                    if ( y > 0 && y < d.height + messageHeight() )
                        thisMessage.draw( g2, 0, y );
                }
            }
        }
        //  Draw the scrollbar.
        if ( _scrollable ) {
            g.setColor( Color.LIGHT_GRAY );
            g.fillRect( d.width - SCROLLBAR_WIDTH, 0, SCROLLBAR_WIDTH, d.height );
            g.setColor( Color.BLACK );
            g.drawRect( d.width - SCROLLBAR_WIDTH, 0, SCROLLBAR_WIDTH, d.height );
            _scrollBar.setValues( -_yOffset, d.height, 0, measureDataHeight() );        //  SHOULD THIS BE HERE??
        }
        g.setColor( Color.BLACK );
        g.drawRect( 0, 0, d.width  - 1, d.height - 1 );
    }
    
    /*
     * The timeout interval is used to animate the browser, giving scroll operations
     * "momentum".  The tricky bit is getting the momentum to decay with time in
     * a way that is visually pleasing.  Using the actual scrollbar does none of
     * this - it behaves conventionally (no momentum).
     */
    protected void timeoutIntervalEvent() {
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
            else if ( _momentumOn ) {
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
                _offsetMotion = 0;
            }
            this.updateUI();
        }
        //  This is needed in case closing a browser node makes the data smaller
        //  than the screen.
        //if ( _yOffset == 0 )
        //    this.updateUI();
    }
    
    @Override
    public void mouseMoved( MouseEvent e ) {
        _lastY = e.getY();
        _lastX = e.getX();
    }
    
    @Override
    public void mouseDragged( MouseEvent e ) {
//  This stuff made a drag event move a scrollable display.
//        if ( _scrollable ) {
//            _offsetMotion = e.getY() - _lastY;
//            _decayCount = 10;
//            _decayStartCount = 10;
//            _lastY = e.getY();
//            testScrollBar();
//        }
        //  Figure out the lines in the text message pane that should be highlighted
        //  by this drag event.
        int startY = _lastY;
        int endY = e.getY();
        int startX = _lastX;
        int endX = e.getX();
        if ( e.getY() < startY ) {
            startY = e.getY();
            endY = _lastY;
            startX = e.getX();
            endX = _lastX;
        }
        String newCut = "";
        synchronized( _messageList ) {
            int y = _yOffset;
            for ( Iterator<MessageNode> iter = _messageList.iterator(); iter.hasNext(); ) {
                MessageNode thisMessage = iter.next();
                thisMessage.highlight( false, false, false, 0, 0, 0, _highlightColor );
                if ( thisMessage.showThis() ) {
                    int height = messageHeight();
                    y += height;
                    //  See if anything should be highlighted
                    if ( y > startY && y - height < endY ) {
                        //  See if we should be highlighting the whole line
                        if ( startY < y - height && endY > y )
                            newCut += thisMessage.highlight( true, true, true, 0, this.getWidth() + 100, height, _highlightColor );
                        //  Or maybe the whole line after the start
                        else if ( startY < y && endY > y)
                            newCut += thisMessage.highlight( true, false, true, startX, this.getWidth() + 100, height, _highlightColor );
                        //  Or, perhaps, the whole line up to the end
                        else if ( startY < y - height )
                            newCut += thisMessage.highlight( true, true, false, 0, endX, height, _highlightColor );
                        //  Final possibility is that this is all on one line.  Put the start
                        //  and end in the proper order.
                        else {
                            if ( startX < endX )
                                newCut += thisMessage.highlight( true, false, false, startX, endX, height, _highlightColor );
                            else
                                newCut += thisMessage.highlight( true, false, false, endX, startX, height, _highlightColor );
                        }
                    }
                }
            }
        }
        if ( newCut.length() > 0 )
            this.setClipboardContents( newCut );
        this.updateUI();
    }
    
    @Override
    public void mouseWheelMoved( MouseWheelEvent e ) {
        if ( _scrollable ) {
            _offsetMotion = _scrollSense * 2 * e.getScrollAmount() * e.getWheelRotation();
            _decayCount = 10;
            _decayStartCount = 10;
            //testScrollBar();
            listChange();
        }
    }
    
    /*
    * Empty implementation of the ClipboardOwner interface.
    */
    @Override
    public void lostOwnership( Clipboard aClipboard, Transferable aContents) {
    }

    /*
     * Place a String on the clipboard, and make this class the
     * owner of the Clipboard's contents.
     */
    public void setClipboardContents( String aString ){
        StringSelection stringSelection = new StringSelection( aString );
        Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
        clipboard.setContents( stringSelection, this );
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
            this.updateUI();
            testScrollBar();
            //  This stuff could be used instead if momentum was desired.
            //_offsetMotion = -e.getValue() - _yOffset;
            //_decayCount = 10;
            //_decayStartCount = 10;
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
        else
            _scrolledToEnd = true;
    }
    
    void momentumOn( boolean newVal ) {
        _momentumOn = newVal;
    }
    
    void messageFont( Font newFont ) {
        _messageFont = newFont;
    }
    
    public class ScrollThread extends Thread {
        protected int _interval;
        public ScrollThread( int interval ) {
            _interval = interval;
        }
        Boolean _keepGoing;
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
        void stopNow() {
            _keepGoing = false;
        }
    }
    
    /*
     * Give access to the message list.
     */
    public ArrayDeque<MessageNode> messageList() { return _messageList; }
    
    //protected NodeBrowserPane browserPane;
    protected ArrayDeque<MessageNode> _messageList;
    protected JScrollBar _scrollBar;

    protected int _yOffset;
    protected int _offsetMotion;
    protected int _decayCount;
    protected int _decayStartCount;
    protected int _minYOffset;
    protected boolean _scrollable;
    protected int _lastY;
    protected int _lastX;
    protected boolean _momentumOn;
    protected int _scrollSense;
    protected boolean _scrolledToEnd;
    protected Font _messageFont;
    protected Color _highlightColor;
    protected Integer _maxMessages;
    protected Long _diffTime;
    
    protected ScrollThread _scrollThread;
    
    static protected int SCROLLBAR_WIDTH = 16;
    
}
