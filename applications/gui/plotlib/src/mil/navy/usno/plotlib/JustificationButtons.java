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
 * This panel contains a group of three "buttons" that show text justification
 * possibilities (left, center, and right) using little pictures.  Currently it
 * is best displayed with a size of 60 x 20 pixels.
 */
package mil.navy.usno.plotlib;

import javax.swing.JPanel;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Dimension;

import java.awt.event.MouseListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.EventListenerList;

import java.awt.Color;

public class JustificationButtons extends JPanel {
    
    public JustificationButtons() {
        _this = this;
        _justification = Plot2DObject.LEFT_JUSTIFY;
        _actionListeners = new EventListenerList();
        addMouseListener( new MouseEventHandler() );
    }
    
    @Override
    public void paintComponent( Graphics g ) {
        Graphics2D g2 = (Graphics2D)g;  // Graphics2 for antialiasing.
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        Dimension d = this.getSize();
        //  Find the width and height of a button.  The height is the height of
        //  this object, the width is 1/3 the width of the object.
        _h = d.height;
        _w = d.width / 3;
        boolean raised = _justification != Plot2DObject.LEFT_JUSTIFY;
        int w = _w - 1;
        int h = _h - 1;
        int x = 0;
        int y = 0;
        g.setColor( getBackground() );
        for ( int i = 0; i <2; ++i ) {
            g.draw3DRect( x++, y++, w, h, raised);
            w -= 2;
            h -= 2;
        }
        g.fillRect( x, y, w+1, h + 1 );
        g.setColor( Color.BLACK );
        g.drawLine( x + 2, y + 2, x + 12, y + 2 );
        g.drawLine( x + 2, y + 5, x + 8, y + 5 );
        g.drawLine( x + 2, y + 8, x + 12, y + 8 );
        g.drawLine( x + 2, y + 11, x + 6, y + 11 );
        g.drawLine( x + 2, y + 14, x + 10, y + 14 );
        raised = _justification != Plot2DObject.CENTER_JUSTIFY;
        w = _w - 1;
        h = _h - 1;
        x = _w;
        y = 0;
        g.setColor( getBackground() );
        for ( int i = 0; i <2; ++i ) {
            g.draw3DRect( x++, y++, w, h, raised);
            w -= 2;
            h -= 2;
        }
        g.fillRect( x, y, w+1, h + 1 );
        g.setColor( Color.BLACK );
        g.drawLine( x + 2, y + 2, x + 12, y + 2 );
        g.drawLine( x + 4, y + 5, x + 10, y + 5 );
        g.drawLine( x + 2, y + 8, x + 12, y + 8 );
        g.drawLine( x + 5, y + 11, x + 9, y + 11 );
        g.drawLine( x + 3, y + 14, x + 11, y + 14 );
        raised = _justification != Plot2DObject.RIGHT_JUSTIFY;
        w = _w - 1;
        h = _h - 1;
        x = 2 * _w;
        y = 0;
        g.setColor( getBackground() );
        for ( int i = 0; i <2; ++i ) {
            g.draw3DRect( x++, y++, w, h, raised);
            w -= 2;
            h -= 2;
        }
        g.fillRect( x, y, w+1, h + 1 );
        g.setColor( Color.BLACK );
        g.drawLine( x + 2, y + 2, x + 12, y + 2 );
        g.drawLine( x + 6, y + 5, x + 12, y + 5 );
        g.drawLine( x + 2, y + 8, x + 12, y + 8 );
        g.drawLine( x + 8, y + 11, x + 12, y + 11 );
        g.drawLine( x + 4, y + 14, x + 12, y + 14 );
    }
    
    public void justification( int newJustification ) {
        _justification = newJustification;
        this.updateUI();
    }
    
    public int justification() {
        return _justification;
    }
    
    class MouseEventHandler extends MouseAdapter {
        public void mousePressed( MouseEvent evt ) {
            if ( evt.getX() < _w )
                _justification = Plot2DObject.LEFT_JUSTIFY;
            else if ( evt.getX() < 2 * _w )
                _justification = Plot2DObject.CENTER_JUSTIFY;
            else
                _justification = Plot2DObject.RIGHT_JUSTIFY;
            _this.updateUI();
            _this.dispatchAction( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
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

int _justification;
    int _h;
    int _w;
    JustificationButtons _this;
    EventListenerList _actionListeners;
    
}
