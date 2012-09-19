/*
 * This is a simple widget that allows a user to choose a color.  It consists
 * of a single button (that fills the widget) which is colored with a given color
 * setting.
 */
package mil.navy.usno.widgetlib;

import javax.swing.JButton;
import javax.swing.JColorChooser;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.Color;

import javax.swing.event.EventListenerList;

/**
 *
 * @author jspitzak
 */
public class ColorButton extends JButton {
    
    public ColorButton() {
        super( "" );
        _actionListeners = new EventListenerList();
        super.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                setColor();
                dispatchAction( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
            }
        } );
    }
    
    public void color( Color newColor ) {
        this.setBackground( newColor );
    }
    public Color color() { return this.getBackground(); }
    
    protected void setColor() {
        Color newColor = JColorChooser.showDialog( this, "Background Color", this.getBackground() );
        if ( newColor != null ) {
            this.setBackground( newColor );
        }
    }

    public void addActionListener( ActionListener a ) {
        _actionListeners.add( ActionListener.class, a );
    }

    protected void dispatchAction( ActionEvent actionEvent ) {
        Object[] listeners = _actionListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( actionEvent );
        }
    }
    
    EventListenerList _actionListeners;

}
