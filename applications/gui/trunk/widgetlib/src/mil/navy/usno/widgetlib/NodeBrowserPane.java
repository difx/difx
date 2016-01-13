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
 * This is a special case of the BrowserNode that contains a hierarchical list
 * of BrowserNode objects.
 */
package mil.navy.usno.widgetlib;

import mil.navy.usno.widgetlib.BrowserNode;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Dimension;

import java.util.Iterator;

/**
 *
 * @author jspitzak
 */
public class NodeBrowserPane extends BrowserNode {
    
    public NodeBrowserPane() {
        super( "" );
    }
    
    public int dataHeight() {
        return _dataHeight;
    }
    
    public void clear() {
        this.clearChildren();
    }
    
    @Override
    public void dispatchResizeEvent() {
        super.dispatchResizeEvent();
        measureDataBounds();
    }
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        //  Propogate the new width to all children.  Watch for the modification exception
        //  in the event the child list is being edited.
        try {
            for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
                iter.next().setWidth( w );
            }
        } catch ( java.util.ConcurrentModificationException e ) {
        }
        measureDataBounds();
    }
    
    public void measureDataBounds() {
        //  Offset the top of the browser data by an amount determined above this
        //  class.
        int yOffset = _yOffset;
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            //  Don't bother setting the draw conditions for items that fall off the
            //  bottom of the browser window.
            try {
                BrowserNode node = iter.next();
                yOffset += node.setDrawConditions( yOffset, true );
            } catch ( java.util.ConcurrentModificationException e ) {  //  removed 7/11/2014, put back 7/28/2015
                //  Ignore these - a redraw will occur soon enough
            }
        }
        //  Measure the total height of the data currently displayed in the
        //  browser.
        _dataHeight = yOffset - _yOffset;
        //  Pad the height slightly - this leaves a gap at the bottom of the
        //  browser when it fills with data, which helps a user know when they
        //  are at the end.
        _dataHeight += 5;
    }
    
    @Override
    public void paintComponent( Graphics g ) {
        super.paintComponent( g );
        Dimension d = getSize();
        g.setColor( this.getBackground() );
        g.fillRect( 0, 0, d.width, d.height );
    }
    
    public void yOffset( int newOffset ) {
        _yOffset = newOffset;
        measureDataBounds();
    }
    
    int _dataHeight;
    int _yOffset;

}
