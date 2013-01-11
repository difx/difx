/*
 * This widget draws a grid of buttons that can have on/off states.
 */
package mil.navy.usno.widgetlib;

import java.awt.Color;
import javax.swing.JScrollPane;
import javax.swing.JPanel;
import javax.swing.JButton;

import javax.swing.JToolTip;

import java.util.ArrayList;
import java.util.Iterator;

import java.awt.Color;
import java.awt.Graphics;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.event.EventListenerList;

/**
 *
 * @author jspitzak
 */
public class ButtonGrid  extends JScrollPane {
    
    protected ArrayList<GridButton> _buttonList = new ArrayList<GridButton>();
    protected int _buttonsPerLine = 5;
    protected Color _onColor = Color.GREEN;
    protected Color _offColor = this.getBackground();
    protected int _buttonHeight = 25;

    public ButtonGrid() {
        super();
        _panel = new GridPanel();
        this.setViewportView( _panel );
        this.horizontalScrollBar.setVisible( false );
        _changeListeners = new EventListenerList();
    }
    
    public class GridButton extends JButton {
        
        public GridButton( String label ) {
            super( label );
            this.on( false );
        }
        
        public void on( boolean newVal ) {
            if ( newVal )
                this.setBackground( _onColor );
            else
                this.setBackground( _offColor );
        }
        public boolean on() {
            return ( this.getBackground() == _onColor );
        }
        
        public void data( Object newData ) {
            _data = newData;
        }
        public Object data() {
            return _data;
        }
        
        protected Object _data;
    }
    
    /*
     * Resizing this thing causes the buttons sizes to change...possibly.  The
     * panel also needs to resize itself vertically to contain all of the buttons.
     */
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        _panelWidth = w - 3;
        super.setBounds( x, y, w, h );
        //  Find the height of all of our buttons.
        int height = _buttonHeight * ( _buttonList.size() / _buttonsPerLine );
        if ( _buttonList.size() % _buttonsPerLine > 0 )
            height += _buttonHeight;
        //  If the height indicates there will be a scrollbar, set the width to
        //  give it space.  Otherwise, use the width of the available area.
        if ( height > h )
            _panelWidth -= this.verticalScrollBar.getWidth();
        else
            height = h;
        _panel.setBounds( 0, 0, _panelWidth, height );
    }
    
    protected class GridPanel extends JPanel {
        
        @Override
        public void setBounds( int x, int y, int w, int h ) {
            super.setBounds( x, y, _panelWidth, h );
        }
        
        @Override
        public void paintComponent( Graphics g ) {
            //  Find the width of buttons that will fit in the current width.
            int buttonW = this.getWidth() / _buttonsPerLine;
            //  Then run through the list of buttons and set their positions.
            int column = 0;
            int row = 0;
            try {
                for ( Iterator iter = _buttonList.iterator(); iter.hasNext(); ) {
                    GridButton button = (GridButton)iter.next();
                    button.setBounds( column * buttonW, row * _buttonHeight, buttonW, _buttonHeight );
                    column += 1;
                    if ( column == _buttonsPerLine ) {
                        column = 0;
                        row += 1;
                    }
                }
            } catch ( java.util.ConcurrentModificationException e ) {
            }
        }
        
    }
    
    /*
     * Add a "listener" to callbacks when any changes to button states occur.
     */
    public void addChangeListener( ActionListener a ) {
        _changeListeners.add( ActionListener.class, a );
    }

    protected void dispatchChangeCallback() {
        Object[] listeners = _changeListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
    
    /*
     * Add a button to the panel.  The button is given a label, tooltip string
     * (which can include newline characters) and an indication whether or not
     * it is "on" - as pushing buttons turns them on and off.
     * 
     * The newly created button is returned.  This seems like something that
     * might be useful.
     */
    public GridButton addButton( String label, String tooltip, boolean on ) {
        final GridButton button = new GridButton( label ) {
            public JToolTip createToolTip() {
                ComplexToolTip tip = new ComplexToolTip();
                tip.setComponent(this);
                return tip;
            }
        };
        button.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                button.on( !button.on() );
                dispatchChangeCallback();
            }
        } );
        button.setToolTipText( tooltip );
        button.on( on );
        _buttonList.add( button );
        _panel.add( button );
        return button;
    }
    
    /*
     * Get rid of all buttons.
     */
    public void clearButtons() {
        for ( Iterator iter = _buttonList.iterator(); iter.hasNext(); ) {
            GridButton button = (GridButton)iter.next();
            _panel.remove( button );
        }
        _buttonList.clear();
    }
        
    
    /*
     * Create a list of "on" items, identified by their names.
     */
    public ArrayList<String> onItems() {
        ArrayList<String> newList = new ArrayList<String>();
        for ( Iterator iter = _buttonList.iterator(); iter.hasNext(); ) {
            GridButton button = (GridButton)iter.next();
            if ( button.getBackground() == _onColor )
                newList.add( button.getText() );
        }
        return newList;
    }
    
    /*
     * Turn on or off a button based on its name.
     */
    public void setButton( String label, boolean on  ) {
        ArrayList<String> newList = new ArrayList<String>();
        for ( Iterator iter = _buttonList.iterator(); iter.hasNext(); ) {
            GridButton button = (GridButton)iter.next();
            if ( button.getText().contentEquals( label ) )
                button.on( on );
        }
    }
    
    /*
     * Return a list of all buttons.
     */
    public ArrayList<GridButton> buttonList() {
        return _buttonList;
    }
    
    /*
     * Turn all of the buttons "on".
     */
    public void allOn() {
        for ( Iterator iter = _buttonList.iterator(); iter.hasNext(); ) {
            GridButton button = (GridButton)iter.next();
            button.on( true );
        }
    }
        
    /*
     * Turn all of the buttons "off".
     */
    public void allOff() {
        for ( Iterator iter = _buttonList.iterator(); iter.hasNext(); ) {
            GridButton button = (GridButton)iter.next();
            button.on( false );
        }
    }
    
    public int buttonsPerLine() { return _buttonsPerLine; }
    public void buttonsPerLine( int newVal ) { _buttonsPerLine = newVal; }
    public void onColor( Color newVal ) { _onColor = newVal; }
    public void offColor( Color newVal ) { _offColor = newVal; }
    public void buttonHeight( int newVal ) { _buttonHeight = newVal; }
    
    protected GridPanel _panel;
    protected int _panelWidth;
    
    protected EventListenerList _changeListeners;
    
}

