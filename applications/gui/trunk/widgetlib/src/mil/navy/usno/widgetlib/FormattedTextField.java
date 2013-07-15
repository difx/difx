/*
 * This is an extension of the formatted text field that implements the "ComplexToolTip"
 * class as the tooltip.  It also allows control of the "callback" behavior when text is
 * entered.  Three types of text are interesting: by default the "enter" key triggers the
 * "action listener" following JFormattedTextField behavior; the "tab" callback occurs 
 * when the user hits a tab key while typing (his allows "tab completion" behavior to be
 * implemented); and any other text changes trigger the "text" callback.  Text changes also
 * can be set to change the background color of the widget (the "textChangeColor") - by 
 * default this is yellow.  This provides a visual warning to the user that the text is 
 * changed but not committed by an "enter".  This behavior can be switched off.
 */
package mil.navy.usno.widgetlib;

import java.awt.Point;
import java.awt.Color;
import java.awt.event.MouseEvent;
import javax.swing.JFormattedTextField;
import javax.swing.text.JTextComponent;
import javax.swing.JToolTip;

import javax.swing.event.EventListenerList;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

public class FormattedTextField extends JFormattedTextField {
    
    public FormattedTextField() {
        super();
        _this = this;
        _tabListeners = new EventListenerList();
        _textListeners = new EventListenerList();
        _nominalBackground = this.getBackground();
        _changeBackground = Color.YELLOW;
        _employChangeBackground = true;
        this.addKeyListener( new KeyAdapter() {
            public void keyPressed( KeyEvent e ) {
                int keyCode = e.getKeyCode();
                //  Respond to enter commands.
                if ( keyCode == 10 ) {
                    //  Enter key - this will trigger the "action event" for this
                    //  object.  Set the background back to "unchanged" as the color
                    //  only indicates that the enter key has not been hit yet.
                    _this.superSetBackground( _nominalBackground );
                }
                else if ( keyCode == 9 ) {
                    //  Tab key - do the tab callback.
                    tabCallback();
                }
                else {
                    //  Any other text key.  Change the background to "changed"
                    //  color and do the callback.
                    if ( _employChangeBackground )
                        _this.superSetBackground( _changeBackground );
                    textCallback();
                }
            }
        });
    }
    
    //  The tab callback sends an "action event" to all listeners in the tab listener
    //  list.  The current object text is sent with the event (as it is assumed this
    //  is probably useful).
    public void tabCallback() {
        Object[] listeners = _tabListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, this.getText() ) );
        }
    }

    //  Called when text is changed.
    public void textCallback() {
        Object[] listeners = _textListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, this.getText() ) );
        }
    }

    public JToolTip createToolTip() {
        _tip = new ComplexToolTip();
        _tip.setComponent( this );
        _tip.dynamicLinkPath( _dynamicLinkPath );
        return _tip;
    }
    public Point getToolTipLocation( MouseEvent e) {
        return new Point( 10, getHeight() );
    }
    
    public ComplexToolTip toolTip() { return _tip; }
    
    /*
     * Allows the user of this widget to set a "dynamic path" for links in the
     * tooltip string.  See the ComplexToolTip class for details.
     */
    public void toolTipDynamicLinkPath( JTextComponent textField ) {
        _dynamicLinkPath = textField;
    }
    
    /*
     * A tooltip-setting function that includes the above.
     */
    public void toolTip( String str, JTextComponent textField ) {
        this.setToolTipText( str );
        this.toolTipDynamicLinkPath( textField );
    }

    public void addTabListener( ActionListener a ) {
        //  This keeps tab keys from sending focus to the "next" widget.  We want the
        //  tab keys!
        this.setFocusTraversalKeysEnabled( false );
        _tabListeners.add( ActionListener.class, a );
    }
    public void addTextListener( ActionListener a ) {
        _textListeners.add( ActionListener.class, a );
    }
    
    public void employChangeBackground( boolean newVal ) {
        _employChangeBackground = newVal;
    }
    
    public void changeBackground( Color newVal ) {
        _changeBackground = newVal;
    }
    
    @Override
    public void setBackground( Color newVal ) {
        super.setBackground( newVal );
        _nominalBackground = newVal;
    }
    
    public void superSetBackground( Color newVal ) {
        super.setBackground( newVal );
    }

    ComplexToolTip _tip;
    JTextComponent _dynamicLinkPath;
    protected EventListenerList _tabListeners;
    protected EventListenerList _textListeners;
    protected boolean _employChangeBackground;
    protected Color _changeBackground;
    protected Color _nominalBackground;
    protected FormattedTextField _this;
    
}
