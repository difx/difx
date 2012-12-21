package mil.navy.usno.widgetlib;

/*
 * This is a Text Field that behaves the way I like - you don't have to hit
 * "enter" to set the text to what you see in the field (you just move the mouse
 * so the field doesn't have focus) and it allows you to limit the size of the
 * text, something I've found useful.
 */

import javax.swing.JFormattedTextField;
import javax.swing.Timer;
import javax.swing.Action;
import javax.swing.AbstractAction;

import javax.swing.event.CaretListener;
import javax.swing.event.CaretEvent;

import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.Point;
import javax.swing.JToolTip;

public class SaneTextField extends FormattedTextField {
    
    public SaneTextField() {
        super();
        this.setFocusLostBehavior( JFormattedTextField.COMMIT );
    }
    
//    ComplexToolTip _tip;
//    public ComplexToolTip toolTip() { return _tip; }
//    public JToolTip createToolTip() {
//        _tip = new ComplexToolTip();
//        _tip.setComponent( this );
//        return _tip;
//    }
//    public Point getToolTipLocation( MouseEvent e) {
//        return new Point( 10, getHeight() );
//    }
    

    /*
     * Set the allowed text width to a given number of characters.
     */
    public void textWidthLimit( int newLimit ) {
        _sizeLimit = newLimit;
        this.addCaretListener( new CaretListener() {
            public void caretUpdate( CaretEvent e ) {
                checkTextSize();
            }
        } );
        Action timeoutAction = new AbstractAction() {
            @Override
            public void actionPerformed( ActionEvent e ) {
                changeTextSize();
            }
        };
        _timeoutTimer = new Timer( 1, timeoutAction );
        _timeoutTimer.setRepeats( false );
    }    
    
    /*
     * Any change in the cursor position triggers this call.
     */
    protected void checkTextSize() {
        if ( this.getText().length() > _sizeLimit ) {
            this.setCaretPosition( _sizeLimit - 1 );
            //  We can't change the text here - it causes an error.  Trigger a
            //  timeout to do it in a msec.
            _timeoutTimer.start();
        }
    }
    
    /*
     * Called by the timeout to change the text.
     */
    protected void changeTextSize() {
        this.setText( this.getText().substring( 0, _sizeLimit ) );
    }
        
    protected int _sizeLimit;
    protected Timer _timeoutTimer;
}
