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
            //try {
                yOffset += iter.next().setDrawConditions( yOffset, true );
            //} catch ( java.util.ConcurrentModificationException e ) {  //  I just removed this 7/11/2014
                //  Ignore these - a redraw will occur soon enough BLAT
            //}
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
