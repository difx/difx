/*
 * This class inherits the generic DrawWindow drawing area class to draw simple
 * data plots.  They are supposed to look pretty with minimal effort.
 */
package mil.navy.usno.plotlib;

import java.awt.Graphics;
import java.awt.Dimension;
import java.awt.Color;
import java.awt.Font;
import java.util.ArrayDeque;
import java.util.Iterator;

public class PlotWindow extends DrawWindow {
    
    public PlotWindow() {
        //  Generate a generic DrawObject for the top level of all drawing.
        _topLevel = new DrawObject();
        drawObject( _topLevel );
        backgroundColor( new Color( 180, 180, 180 ) );
        foregroundColor( Color.BLACK );
    }

    /*
     * The background color is used to fill this entire window with a background
     * color before any drawing.  This is entirely outside the DrawObject
     * list.  We use the JPanel background color to do this (as it serves no
     * other purpose).
     */
    public void backgroundColor( Color newColor ) {
        this.setBackground( newColor );
    }
    public Color backgroundColor() {
        return this.getBackground();
    }
    
    /*
     * The foreground color is the default color of stuff that is drawn on top
     * of the background.
     */
    public void foregroundColor( Color newColor ) {
        _topLevel.color( newColor );
    }
    public Color foregroundColor() {
        return _topLevel.color();
    }
    
    /*
     * Add a new 2D plot to this frame.
     */
    public void add2DPlot( Plot2DObject thisPlot ) {
        _topLevel.add( thisPlot );
    }
    
    /*
     * Remove a 2D Plot.
     */
    public void remove2DPlot( Plot2DObject thisPlot ) {
        _topLevel.remove( thisPlot );
    }
    
    /*
     * Obtain a "pointer" to a plot object using a name.
     */
    public Plot2DObject getPlot( String plotName ) {
        for ( Iterator<DrawObject> iter = _topLevel.iterator(); iter.hasNext(); ) {
            Plot2DObject thisPlot = (Plot2DObject)( iter.next() );
            if ( thisPlot.name().equals( plotName ) )
                return thisPlot;
        }
        return null;
    }
    
    /*
     * Provide the "top level" of the drawing list.  This can probably be used
     * for a bunch of things, but for this purpose it gives us the list of plots.
     */
    public ArrayDeque<DrawObject> plotList() {
        return (ArrayDeque<DrawObject>)_topLevel;
    }
        
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        //  Recompute the bounds of each individual subplot, if necessary.
        try {
            for ( Iterator<DrawObject> iter = _topLevel.iterator(); iter.hasNext(); ) {
                Plot2DObject thisPlot = (Plot2DObject)( iter.next() );
                thisPlot.resizeBasedOnWindow( w, h );
            }
        } catch ( java.util.ConcurrentModificationException e ) {}
        if ( _plotEditors != null ) {
            for ( Iterator<PlotEditor> iter = _plotEditors.iterator(); iter.hasNext(); )
                iter.next().dataChange();
        }
        super.setBounds( x, y, w, h );
    }
    
    /*
     * Set the list of plot editors that are associated with the plots in this
     * window.  The plot editors are created by the UI above, which is probably
     * kludgey - they should be created by this class.
     */
    public void plotEditors( ArrayDeque<PlotEditor> e ) { _plotEditors = e; }
       
    DrawObject _topLevel;
    protected ArrayDeque<PlotEditor> _plotEditors;
        
}
