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
 * This is a toggle button that makes it more obvious that it has been pressed
 * than the JToggleButton.
 */
package mil.navy.usno.widgetlib;

import javax.swing.JPanel;
import javax.swing.JLabel;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Dimension;
import java.awt.RenderingHints;

import java.awt.event.MouseListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.EventListenerList;

public class ToggleButton extends JPanel {
    
    public ToggleButton( String name ) {
        this.setLayout( null );
        _pressed = false;
        _label = new JLabel( name );
        _label.setHorizontalAlignment( JLabel.CENTER );
        this.add( _label );
        _this = this;
        _pressedBackground = Color.GRAY;
        _background = super.getBackground();
        _pressedForeground = Color.WHITE;
        _foreground = _label.getForeground();
        _actionListeners = new EventListenerList();
        addMouseListener( new MouseEventHandler() );
    }
    
    /*
     * Bunch o' functions to set colors of things.
     */
    public void setBackgroundColor( Color newColor ) { _background = newColor; }
    public Color getBackgroundColor() { return _background; }
    public void setLabelColor( Color newColor ) { _foreground = newColor; }
    public Color getLabelColor() { return _foreground; }
    public void setPressedBackgroundColor( Color newColor ) { _pressedBackground = newColor; }
    public Color getPressedBackgroundColor() { return _pressedBackground; }
    public void setPressedLabelColor( Color newColor ) { _pressedForeground = newColor; }
    public Color getPressedLabelColor() { return _pressedForeground; }
    
    public void setLabelFont( Font newFont ) { 
        if ( _label != null )
            _label.setFont( newFont );
    }
    public Font getLabelFont() { 
        if ( _label == null )
            return null;
        else
            return _label.getFont();
    }
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        _label.setBounds( 2, 2, w - 4, h - 4 );
    }
    
    @Override
    public void paintComponent( Graphics g ) {
        Graphics2D g2 = (Graphics2D)g;  // Graphics2 for antialiasing.
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        Dimension d = this.getSize();
        super.paintComponent( g );
        if ( _pressed )
            g.setColor( _pressedBackground );
        else
            g.setColor( _background );
        //  Find the width and height of a button.  The height is the height of
        //  this object, the width is 1/3 the width of the object.
        int w = d.width;
        int h = d.height;
        int x = 0;
        int y = 0;
        for ( int i = 0; i <2; ++i ) {
            g.draw3DRect( x++, y++, w, h, !_pressed );
            w -= 2;
            h -= 2;
        }
    }
    
    class MouseEventHandler extends MouseAdapter {
        public void mousePressed( MouseEvent evt ) {
            _this._pressed = !_this._pressed;
            _this.colorChanges();
            _this.updateUI();
            _this.dispatchAction( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
    
    public void colorChanges() {
        if ( _pressed ) {
            super.setBackground( _pressedBackground );
            _label.setForeground( _pressedForeground );
        }
        else {
            super.setBackground( _background );
            _label.setForeground( _foreground );
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
    
    public void pressed( boolean newVal ) {
        _pressed = newVal;
        this.updateUI();
    }
    public boolean pressed() { return _pressed; }

    boolean _pressed;
    Color _pressedBackground;
    Color _pressedForeground;
    Color _background;
    Color _foreground;
    String _name;
    ToggleButton _this;
    EventListenerList _actionListeners;
    JLabel _label;
    JPanel _super;

}
