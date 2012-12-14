/*
 * This window allows adjustment and display of a single double precision number.
 * Any internal change to the number triggers an ActionEvent callback.  The number
 * may be changed by typing it (hitting the return key registers the actual change
 * in value), using the up/down arrow or +/- keys to change specific digits (these
 * changes are registered immediately) or by clicking the mouse and dragging
 * (these changes also are registered immediately).  Values are checked against
 * maximum and minimum values, if those are set.
 * 
 * The mouse dragging behavior can be very annoying if you aren't expecting it,
 * so I'm making it an option that is OFF.  To turn it on, use mouseActive().
 */
package mil.navy.usno.widgetlib;

import java.beans.PropertyChangeListener;
import javax.swing.JFormattedTextField;
import javax.swing.JTextField;

import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.CaretListener;
import javax.swing.event.CaretEvent;

import java.awt.Graphics;
import java.awt.Container;
import java.awt.Dimension;

import java.lang.String;

import javax.swing.text.Keymap;
import javax.swing.KeyStroke;
import javax.swing.JToolTip;
import java.awt.event.KeyEvent;
import javax.swing.text.TextAction;

import javax.swing.event.EventListenerList;

public class NumberBox extends JFormattedTextField implements MouseListener, 
        MouseMotionListener, MouseWheelListener {
    
    public NumberBox() {
        super();
        this.setFocusLostBehavior( JFormattedTextField.COMMIT );
        this.setHorizontalAlignment( JTextField.CENTER );
        _actionListeners = new EventListenerList();
        //  Capture changes in the field (as entered by users).
        super.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                checkNewText();
            }
        } );
        //  This helps us keep track of where the "caret" (i.e. the cursor) is
        //  located in the text field.
        this.addCaretListener( new CaretListener() {
            public void caretUpdate( CaretEvent e ) {
                cursorPosition( e.getDot() );
            }
        } );
        addMouseListener( this );
        addMouseMotionListener( this );
        addMouseWheelListener( this );
        //  This crap adds listeners for the up and down keys.  This is a real mess...it seems
        //  that doing this keymap junk changes the keymap for the entire class.  Thus we have
        //  to do this "getTextComponent()" call inside the actionPerformed() function.  I can't
        //  fathom why this would work this way, but I'm sure there are legions of Java cultists
        //  who would defend the design with great enthusiasm.
        Keymap km = this.getKeymap();
        km.addActionForKeyStroke( KeyStroke.getKeyStroke( KeyEvent.VK_UP, 0 ), new TextAction( "move up" ) {
            public void actionPerformed( ActionEvent evt ) {
                ((NumberBox)(getTextComponent( evt ))).upKey();
            }
        } );
        km.addActionForKeyStroke( KeyStroke.getKeyStroke( KeyEvent.VK_DOWN, 0 ), new TextAction( "move down" ) {
            public void actionPerformed( ActionEvent evt ) {
                ((NumberBox)(getTextComponent( evt ))).downKey();
            }
        } );
        this.setKeymap( km );
    }

    public JToolTip createToolTip() {
        ComplexToolTip tip = new ComplexToolTip();
        tip.setComponent( this );
        return tip;
    }

    /*
     * Set the value.  This will change the display.
     */
    public void value( double newVal ) {
        _value = newVal;
        changeDisplay();
    }
    
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
    }
    
    /*
     * Get the current value.
     */
    public double value() { 
        return _value;
    }
    
    /*
     * The integer version of get value.
     */
    public int intValue() { 
        if ( _value < 0 )
            return (int)( _value - 0.5 );
        else
            return (int)( _value + 0.5 );
    }
    
    /*
     * If you store an integer as the value, it will be stored internally as a
     * double, but precision will be set to 0.
     */
    public void intValue( int newVal ) {
        value( (double)newVal );
        precision( 0 );
    }
    
    /*
     * Redundant version of the intValue function.  Unfortunately can't do this for
     * the return value.
     */
    public void value( int newVal ) { intValue( newVal ); }
    
    /*
     * Set the "precision" of the display.  This is an integer describing how
     * many digits appear to the right of the decimal place.  For 0 precision,
     * no decimal point will appear.
     */
    public void precision( int newVal ) { 
        _precision = newVal;
        changeDisplay();
    }
    public int precision() { return _precision; }
    
    /*
     * The minimum and maximum values the number can take.  If these are not
     * defined, there are no limits.  There are a number of ways of setting them.
     */
    public void minimum( double newVal ) { _minimum = new Double( newVal ); }
    public void maximum( double newVal ) { _maximum = new Double( newVal ); }
    public void limits( double min, double max ) {
        _minimum = new Double( min );
        _maximum = new Double( max );
    }
    public Double minimum() { return _minimum; }
    public Double maximum() { return _maximum; }
    
    /*
     * Change the display to show the current value.  How this number is displayed
     * depends on the precision specified.  We have to save the caret (cursor)
     * position before changing the text because a change will cause it to be
     * moved to the right.  We also save the length so we can position the cursor
     * by the same digit (in case the number of digits displayed changes).
     */
    protected void changeDisplay() {
        int saveCaret = this.getCaretPosition();
        int saveLength = this.getText().length();
        if ( _precision == 0 )
            this.setText( String.format( "%d", (int)(_value) ) );
        else
            this.setText( String.format( "%." + _precision + "f", _value ) );
        int newCaret = saveCaret + this.getText().length() - saveLength;
        if ( newCaret < 0 )
            newCaret = 0;
        this.setCaretPosition( newCaret );
        _clickPos += newCaret - saveCaret;
    }
    
    /*
     * Check the current string in the text field and determine if it is a legal
     * number.  If so, keep it.  If not, change back to the current value.  This
     * function is used internally - numbers entered using "value()" are not
     * checked.
     */
    protected void checkNewText() {
        //  Convert the string to a number (if it can be done).
        try {
            Double newNum = new Double( this.getText() );
            //  See if the number is inside our limits.
            if ( _minimum == null || newNum > _minimum ) {
                if ( _maximum == null || newNum < _maximum ) {
                    _value = newNum;
                    //  Tell each action listener that the value has changed.
                    Object[] listeners = _actionListeners.getListenerList();
                    int numListeners = listeners.length;
                    for ( int i = 0; i < numListeners; i+=2 ) {
                        if ( listeners[i] == ActionListener.class )
                            ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
                    }
                }
            }
        }
        catch ( java.lang.NumberFormatException e ) {}
        changeDisplay();
    }
    
    /*
     * Check a double precision number to see if it is a valid value.  If so,
     * set the value and do a callback.
     */
    protected void checkNewValue( double newNum ) {
        //  See if the number is inside our limits.
        if ( _minimum != null && newNum < _minimum )
            _value = _minimum;
        else if ( _maximum != null && newNum > _maximum )
            _value = _maximum;
        else
            _value = newNum;
        //  Tell each action listener that the value has changed.
        Object[] listeners = _actionListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
        changeDisplay();
    }
    
    @Override
    public void addActionListener( ActionListener a ) {
        _actionListeners.add( ActionListener.class, a );
    }

    @Override
    public void mouseClicked( MouseEvent e ) {
    }
    
    @Override
    public void mouseEntered( MouseEvent e ) {
    }
    
    @Override
    public void mouseExited( MouseEvent e ) {
    }
    
    @Override
    public void mousePressed( MouseEvent e ) {
        //  Record the location of a mouse press event.
        _clickX = e.getX();
        _clickY = e.getY();
        _clickValue = _value;
        _clickPos = _cursorPos;
    }
    
    @Override
    public void mouseReleased( MouseEvent e ) {
    }
    
    @Override
    public void mouseMoved( MouseEvent e ) {
    }
    
    @Override
    public void mouseDragged( MouseEvent e ) {
            if ( _mouseActive ) {
            //  If the mouse is being dragged outside the box, change the value in
            //  the box by increasing/decreasing the digit corresponding to the caret
            //  position.  Dragging the mouse to the northeast increases the value,
            //  to the southwest decreases it.  The amount of increase/decrease scales
            //  with the distance from the start point in units of the box height.
            //
            //  Okay, for the moment we are ignoring the "out of the box" thing.
            Dimension d = this.getSize();
            //  Make sure the drag event is outside of the box.
            //if ( e.getX() > d.width || e.getX() < 0 || e.getY() > d.height || e.getY() < 0 ) {
                int x = e.getX() - _clickX;
                int y = e.getY() - _clickY;
                if ( x < y )
                    checkNewValue( _clickValue - currentMultiple( _clickPos ) * java.lang.Math.sqrt( x * x + y * y ) / (double)d.height ); 
                else
                    checkNewValue( _clickValue + currentMultiple( _clickPos ) * java.lang.Math.sqrt( x * x + y * y ) / (double)d.height );
            //}
        }
    }
    
    @Override
    public void mouseWheelMoved( MouseWheelEvent e ) {
        checkNewValue( _value + (double)( -e.getWheelRotation() ) * currentMultiple() );
    }
    
    protected void upKey() {
        checkNewValue( _value + currentMultiple() );
    }
    
    protected void downKey() {
        checkNewValue( _value - currentMultiple() );        
    }
    
    protected double currentMultiple() {
        //  If there is no decimal place ...
        if ( _precision == 0 )
            return java.lang.Math.pow( 10.0, (double)( this.getText().length() - _cursorPos ) );
        //  If the cursor is to the right of the decimal...
        else if ( this.getText().length() - _cursorPos <= _precision )
            return java.lang.Math.pow( 10.0, -(double)( 1 + _precision - this.getText().length() + _cursorPos ) );
        //  Otherwise it is to the left....
        else
            return java.lang.Math.pow( 10.0, (double)( this.getText().length() - _cursorPos - _precision - 1 ) );
    }
        
    protected double currentMultiple( int savedCursorPos ) {
        //  If there is no decimal place ...
        if ( _precision == 0 )
            return java.lang.Math.pow( 10.0, (double)( this.getText().length() - savedCursorPos ) );
        //  If the cursor is to the right of the decimal...
        else if ( this.getText().length() - savedCursorPos <= _precision )
            return java.lang.Math.pow( 10.0, -(double)( 1 + _precision - this.getText().length() + savedCursorPos ) );
        //  Otherwise it is to the left....
        else
            return java.lang.Math.pow( 10.0, (double)( this.getText().length() - savedCursorPos - _precision - 1 ) );
    }
        
    public void cursorPosition( int newPos ) { 
        _cursorPos = newPos; 
    }
    
    public void mouseActive( boolean newVal ) { _mouseActive = newVal; }
    
    double _value;
    int _precision;
    Double _minimum;
    Double _maximum;
    EventListenerList _actionListeners;
    public int _cursorPos;
    
    protected int _clickX;
    protected int _clickY;
    protected double _clickValue;
    protected int _clickPos;
    protected boolean _mouseActive;
    
}
