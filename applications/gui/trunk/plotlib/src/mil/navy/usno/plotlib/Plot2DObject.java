/*
 * This class contains a single 2-D data plot as a DrawObject.  Controls for all
 * aspects of the plot are included.  Note that any changes to this object
 * DO NOT trigger a redraw - that is the responsibility of the entity making the
 * change.
 */
package mil.navy.usno.plotlib;

import java.awt.Color;
import java.util.Iterator;
import java.util.ArrayDeque;
import java.awt.Font;

/**
 *
 * @author jspitzak
 */
public class Plot2DObject extends DrawObject {
    
    /*
     * Set up the basic plot structure, including DrawObjects for different
     * components.
     */
    public Plot2DObject() {
        //  The background is a solid color field behind the plot.  By default
        //  it is white.
        _background = new DrawObject();
        this.add( _background );
        _background.color( new Color( 255, 255, 255 ) );
        //  The grid list contains grids, drawn on the background of the plot.
        //  These are stored as a series of lines.  Creating the lines is the
        //  job of the "addGrid()" function.
        _gridHolder = new DrawObject();
        //  This is a list that holds grid specifications (from which grid lines
        //  can be redrawn after limit changes).
        _gridInformation = new ArrayDeque<GridStructure>();
        this.add( _gridHolder );
        //  This object holds any data plotting we do.
        _dataHolder = new DrawObject();
        //  Set clipping on by default.  The actual size of the clipped area 
        //  doesn't matter - it will be changed by the next resize event.
        _dataHolder.setClip( 0.0, 0.0, 1.0, 1.0 );
        this.add( _dataHolder ); 
        //  This is another grid that can be drawn on top of data.
        _topGridHolder = new DrawObject();
        _topGridInformation = new ArrayDeque<GridStructure>();
        this.add( _topGridHolder );
        //  This object contains labels for axis.
        _labelHolder = new DrawObject();
        //  This is a list of label specifications from which labels can be 
        //  drawn (and redrawn) as limits change.
        _labelInformation = new ArrayDeque<LabelStructure>();
        this.add( _labelHolder );
        //  This object is used to draw the frame around the plot.  It is drawn
        //  after the plot data.
        _frame = new DrawObject();
        _frame.color( new Color( 0, 0, 255 ) );
        _frame.lineWidth( 1.0 );
        this.add( _frame );
        //  This is the x-axis title
        _xTitle = new DrawObject();
        this.add( _xTitle );
        //  The y-axis title
        _yTitle = new DrawObject();
        this.add( _yTitle );
        //  The title of the plot
        _title = new DrawObject();
        this.add( _title );
        //  This object can hold extra items.
        _extraItems = new DrawObject();
        this.add( _extraItems );
        //  Default plot limits.
        limits( 0.0, 1.0, 0.0, 1.0 );
        //  Default title position.
        titlePosition( 0.5, 10.0 );
    }

    /*
     * Set the size and location of this plot.
     */
    public void resize( double x, double y, double w, double h ) {
        _x = x;
        _y = y;
        _w = w;
        _h = h;
        _background.fillrect( x, y, w, h );
        _frame.drawrect( x, y, w, h );
        //  We want the lower left corner of the frame rectangle to correspond
        //  to 0,0 in the data world initially.
        _dataHolder.translate( x, y + h );
        //  The same applies to grid lines.
        _gridHolder.translate( x, y + h );
        _topGridHolder.translate( x, y + h );
        //  And to labels.
        _labelHolder.translate( x, y + h );
        //  Set clipping if we are doing that.
        if ( _dataHolder._clipSet ) {
            _dataHolder.setClip( 0, -h, w, h );
            _gridHolder.setClip( 0, -h, w, h );
            _topGridHolder.setClip( 0, -h, w, h );
        }
        //  Change the scale of the data plots.
        //rescale();
        //  Change labels - some of these are based on the plot size.  This also
        //  calls rescale().
        relabel();
        //  Change where we put titles and labels
        repositionTitle();
        //  Also do the same for "extra" items.
        repositionExtraItems();
    }
    
    /*
     * Rescale any data component so that the limits fall on the existing frame.
     * Note - Y values are upsidedown to switch from top-left corner
     * corresponding to 0,0 to lower-left.
     */
    protected void rescale() {
        synchronized( _dataHolder ) {
            for ( Iterator<DrawObject> iter = _dataHolder.iterator(); iter.hasNext(); ) {
                DrawObject dataComponent = iter.next();
                dataComponent.translate( -_xLow * ( _w / (_xHigh - _xLow ) ), 
                        -_yLow * ( _h / (_yLow - _yHigh ) ) );
                dataComponent.scale( _w / (_xHigh - _xLow ), _h / (_yLow - _yHigh ) );
            }
        }
        synchronized( _gridHolder ) {
            for ( Iterator<DrawObject> iter = _gridHolder.iterator(); iter.hasNext(); ) {
                DrawObject dataComponent = iter.next();
                dataComponent.translate( -_xLow * ( _w / (_xHigh - _xLow ) ), 
                        -_yLow * ( _h / (_yLow - _yHigh ) ) );
                dataComponent.scale( _w / (_xHigh - _xLow ), _h / (_yLow - _yHigh ) );
            }
        }
        synchronized( _topGridHolder ) {
            for ( Iterator<DrawObject> iter = _topGridHolder.iterator(); iter.hasNext(); ) {
                DrawObject dataComponent = iter.next();
                dataComponent.translate( -_xLow * ( _w / (_xHigh - _xLow ) ), 
                        -_yLow * ( _h / (_yLow - _yHigh ) ) );
                dataComponent.scale( _w / (_xHigh - _xLow ), _h / (_yLow - _yHigh ) );
            }
        }
        synchronized( _labelHolder ) {
            for ( Iterator<DrawObject> iter = _labelHolder.iterator(); iter.hasNext(); ) {
                DrawObject dataComponent = iter.next();
                dataComponent.translate( -_xLow * ( _w / (_xHigh - _xLow ) ), 
                        -_yLow * ( _h / (_yLow - _yHigh ) ) );
                dataComponent.scale( _w / (_xHigh - _xLow ), _h / (_yLow - _yHigh ) );
            }
        }
    }
    
    /*
     * Adjust grid lines to new limits.  These are based on the current grid
     * list.
     */
    protected void regrid() {
        _gridHolder.clear();
        for ( Iterator<GridStructure> iter = _gridInformation.iterator(); iter.hasNext(); ) {
            GridStructure grid = iter.next();
            //  Create new objects representing the grid lines along the proper
            //  axis with the proper spacing and color.
            if ( grid.axis == X_AXIS ) {
                double low = _xLow;
                double high = _xHigh;
                if ( _xLow > _xHigh ) {
                    low = _xHigh;
                    high = _xLow;
                }
                for ( double val = grid.step * (double)((int)(low/grid.step)); val <= high; val += grid.step ) {
                    double x[] = new double[2];
                    double y[] = new double[2];
                    x[0] = val;
                    x[1] = val;
                    y[0] = _yLow;
                    y[1] = _yHigh;
                    Curve2D newObject = new Curve2D( x, y );
                    newObject.color( grid.color );
                    _gridHolder.add( newObject );
                }
            }
            else if ( grid.axis == Y_AXIS ) {
                double low = _yLow;
                double high = _yHigh;
                if ( _yLow > _yHigh ) {
                    low = _yHigh;
                    high = _yLow;
                }
                for ( double val = grid.step * (double)((int)(low/grid.step)); val <= high; val += grid.step ) {
                    double x[] = new double[2];
                    double y[] = new double[2];
                    x[0] = _xLow;
                    x[1] = _xHigh;
                    y[0] = val;
                    y[1] = val;
                    Curve2D newObject = new Curve2D( x, y );
                    newObject.color( grid.color );
                    _gridHolder.add( newObject );
                }
            }
        }
        rescale();
    }
    
    /*
     * Regridding function for grids that lie on top of plot data.
     */
    protected void topRegrid() {
        _topGridHolder.clear();
        for ( Iterator<GridStructure> iter = _topGridInformation.iterator(); iter.hasNext(); ) {
            GridStructure grid = iter.next();
            //  Create new objects representing the grid lines along the proper
            //  axis with the proper spacing and color.
            if ( grid.axis == X_AXIS ) {
                double low = _xLow;
                double high = _xHigh;
                if ( _xLow > _xHigh ) {
                    low = _xHigh;
                    high = _xLow;
                }
                for ( double val = grid.step * (double)((int)(low/grid.step)); val <= high; val += grid.step ) {
                    double x[] = new double[2];
                    double y[] = new double[2];
                    x[0] = val;
                    x[1] = val;
                    y[0] = _yLow;
                    y[1] = _yHigh;
                    Curve2D newObject = new Curve2D( x, y );
                    newObject.color( grid.color );
                    _topGridHolder.add( newObject );
                }
            }
            else if ( grid.axis == Y_AXIS ) {
                double low = _yLow;
                double high = _yHigh;
                if ( _yLow > _yHigh ) {
                    low = _yHigh;
                    high = _yLow;
                }
                for ( double val = grid.step * (double)((int)(low/grid.step)); val <= high; val += grid.step ) {
                    double x[] = new double[2];
                    double y[] = new double[2];
                    x[0] = _xLow;
                    x[1] = _xHigh;
                    y[0] = val;
                    y[1] = val;
                    Curve2D newObject = new Curve2D( x, y );
                    newObject.color( grid.color );
                    _topGridHolder.add( newObject );
                }
            }
        }
        rescale();
    }
    
    /*
     * Adjust labels to new limits.  The labels are stored in the _labelStructure.
        public Double start;
        public Double stop;
        public String format;
        public double ticSize;
        public double gapSize;
        public int justification;
        public boolean plotAlways;
        public boolean drawScale;
     */
    public void relabel() {
        _labelHolder.clear();
        for ( Iterator<LabelStructure> iter = _labelInformation.iterator(); iter.hasNext(); ) {
            LabelStructure label = iter.next();
            //  Create new objects representing tic marks and labels along the proper
            //  axis accounting for all specifications.
            if ( label.axis == X_AXIS ) {
                //  Where on the Y-axis do we draw these tic marks and labels?
                double crossValue = _yLow;
                if ( label.crossValue != null )
                    crossValue = label.crossValue.doubleValue();
                //  Only draw labels if the "crossValue" is on the plot or if we are supposed to
                //  "always draw".
                if ( label.plotAlways || ( crossValue >= _yLow && crossValue <= _yHigh ) ) {
                    //  Draw a line parallel to the X-axis at the "crossValue" if this
                    //  has been requested.
                    if ( label.drawScale ) {
                        double x[] = new double[2];
                        double y[] = new double[2];
                        x[0] = _xLow;
                        x[1] = _xHigh;
                        y[0] = crossValue;
                        y[1] = crossValue;
                        Curve2D newObject = new Curve2D( x, y );
                        newObject.color( label.color );
                        _labelHolder.add( newObject );
                    }
                    //  Find the low and high limits on where these labels will be drawn.
                    //  If the user has not specified, use defaults.
                    double low = _xLow;
                    double high = _xHigh;
                    if ( low > high ) {
                        double tmp = low;
                        low = high;
                        high = tmp;
                    }
                    //  This is the "default" start value for labels.  It depends
                    //  on the label step, if available.
                    double startVal = low;
                    if ( label.step != null )
                        startVal = label.step * (double)((int)(low/label.step));
                    //  Set what the user wants if specified.
                    if ( label.start != null )
                        startVal = label.start;
                    //  This is the "default" end value for labels.
                    double stopVal = high;
                    if ( label.stop != null )
                        stopVal = label.stop;
                    //  Try to set the (default) step value based on the start and stop value.  This will get
                    //  all messed up if all three (start, stop, step) are null.  FIX THIS!!!
                    double stepVal = stepSize( stopVal - startVal, 3.0, 10.0 );
                    if ( label.step != null )
                        stepVal = label.step;
                    //  Prevent endless loop - shouldn't happen unless the user does something odd
                    if ( stepVal == 0.0 )
                        stepVal = 2 * ( stopVal - startVal );
                    for ( double val = startVal; val <= stopVal; val += stepVal ) {
                        //  This draws the tic mark, assuming there is one.
                        if ( label.ticSize != null ) {
                            Track2D newObject = new Track2D();
                            newObject.add( val, crossValue );
                            if ( label.ticSize < 1.0 && label.ticSize > -1.0 )
                                newObject.add( 0.0, label.ticSize * _h, Track2D.RELATIVE_POINT );
                            else
                                newObject.add( 0.0, label.ticSize, Track2D.RELATIVE_POINT );
                            newObject.color( label.color );
                            _labelHolder.add( newObject );
                        }
                        //  This draws the label, assuming there is one.  Note that a "null"
                        //  label actually means use default formatting, not that the label
                        //  doesn't exist (unlike the tic specification above).
                        Track2D newObject = new Track2D();
                        newObject.add( val, crossValue );
                        if ( label.gapSize < 1.0 && label.gapSize > -1.0 )
                            newObject.add( 0.0, label.gapSize * _h, Track2D.RELATIVE_POINT );
                        else
                            newObject.add( 0.0, label.gapSize, Track2D.RELATIVE_POINT );
                        newObject.color( label.color );
                        newObject.draw( false );
                        DrawObject textObject = new DrawObject();
                        //  Form the label based on the format.
                        String theText = null;
                        if ( label.format == null )
                            theText = Double.toString( val );
                        else {
                            theText = String.format( label.format, val );
                        }
                        textObject.complexText( label.justification, "<y=1.0>" + theText );
                        textObject.unscaled();
                        newObject.add( textObject );
                        _labelHolder.add( newObject );
                    }
                }
            }
            //  Do the same stuff on the Y_AXIS
            else if ( label.axis == Y_AXIS ) {
                //  Where on the X-axis do we draw these tic marks and labels?
                double crossValue = _xLow;
                if ( label.crossValue != null )
                    crossValue = label.crossValue.doubleValue();
                //  Only draw labels if the "crossValue" is on the plot or if we are supposed to
                //  "always draw".
                if ( label.plotAlways || ( crossValue >= _xLow && crossValue <= _xHigh ) ) {
                    //  Draw a line parallel to the X-axis at the "crossValue" if this
                    //  has been requested.
                    if ( label.drawScale ) {
                        double x[] = new double[2];
                        double y[] = new double[2];
                        x[0] = crossValue;
                        x[1] = crossValue;
                        y[0] = _yLow;
                        y[1] = _yHigh;
                        Curve2D newObject = new Curve2D( x, y );
                        newObject.color( label.color );
                        _labelHolder.add( newObject );
                    }
                    //  Find the low and high limits on where these labels will be drawn.
                    //  If the user has not specified, use defaults.
                    double low = _yLow;
                    double high = _yHigh;
                    if ( low > high ) {
                        double tmp = low;
                        low = high;
                        high = tmp;
                    }
                    //  This is the "default" start value for labels.  It depends
                    //  on the label step, if available.
                    double startVal = low;
                    if ( label.step != null )
                        startVal = label.step * (double)((int)(low/label.step));
                    //  Set what the user wants if specified.
                    if ( label.start != null )
                        startVal = label.start;
                    //  This is the "default" end value for labels.
                    double stopVal = high;
                    if ( label.stop != null )
                        stopVal = label.stop;
                    //  Try to set the (default) step value based on the start and stop value.  This will get
                    //  all messed up if all three (start, stop, step) are null.  FIX THIS!!!
                    double stepVal = stepSize( stopVal - startVal, 3.0, 10.0 );
                    ////  The default value for steps is to have none (this occurs when step size is null).
                    //double stepVal = 2 * ( stopVal - startVal );
                    if ( label.step != null )
                        stepVal = label.step;
                    //  Prevent endless loop - shouldn't happen unless the user does something odd
                    if ( stepVal == 0.0 )
                        stepVal = 2 * ( stopVal - startVal );
                    for ( double val = startVal; val <= stopVal; val += stepVal ) {
                        //  This draws the tic mark, assuming there is one.
                        if ( label.ticSize != null ) {
                            Track2D newObject = new Track2D();
                            newObject.add( crossValue, val );
                            if ( label.ticSize < 1.0 && label.ticSize > -1.0 )
                                newObject.add( label.ticSize * _w, 0.0, Track2D.RELATIVE_POINT );
                            else
                                newObject.add( label.ticSize, 0.0, Track2D.RELATIVE_POINT );
                            newObject.color( label.color );
                            _labelHolder.add( newObject );
                        }
                        //  This draws the label, assuming there is one.  Note that a "null"
                        //  label actually means use default formatting, not that the label
                        //  doesn't exist (unlike the tic specification above).
                        Track2D newObject = new Track2D();
                        newObject.add( crossValue, val );
                        if ( label.gapSize < 1.0 && label.gapSize > -1.0 )
                            newObject.add( label.gapSize * _h, 0.0, Track2D.RELATIVE_POINT );
                        else
                            newObject.add( label.gapSize, 0.0, Track2D.RELATIVE_POINT );
                        newObject.color( label.color );
                        newObject.draw( false );
                        DrawObject textObject = new DrawObject();
                        //  Form the label based on the format.
                        String theText = null;
                        if ( label.format == null )
                            theText = Double.toString( val );
                        else {
                            theText = String.format( label.format, val );
                        }
                        textObject.complexText( label.justification, "<y=0.35>" + theText );
                        textObject.unscaled();
                        newObject.add( textObject );
                        _labelHolder.add( newObject );
                    }
                }
            }
        }
        rescale();
    }
    
    /*
     * Set the size of this plot based on the size of the window holding it.
     * These can either be "absolute" or "relative", based on their initial
     * values.  Absolute bounds don't change based on the frame size of the
     * containing window.  Relative bounds do - they scale with it.  Relative
     * bounds are specified as a fraction - so anything less than 1.0 is taken 
     * to be a relative bound.
     */
    public void resizeBasedOnWindow( int w, int h ) {
        double nx = _xFrame;
        double ny = _yFrame;
        double nw = _wFrame;
        double nh = _hFrame;
        if ( _xFrameUseFraction )
            nx = nx * (double)w;
        if ( _yFrameUseFraction )
            ny = ny * (double)h;
        if ( _wFrameUseFraction )
            nw = nw * (double)w;
        if ( _hFrameUseFraction )
            nh = nh * (double)h;
        resize( nx, ny, nw, nh );
    }
    
    /*
     * The plot area color is used to fill the background to the plotting box.
     */
    public void backgroundColor( Color newColor ) {
        _background.color( newColor );
    }
    public Color backgroundColor() {
        return _background.color();
    }
    
    /*
     * Set the bounds of the frame for this plot.  These numbers are specified
     * as doubles, either representing pixels if they are greater than 1.0, or
     * fractions of the size of the window in which they are drawn if they are
     * smaller than 1.0.  The former sizes are absolute - they won't change with
     * window size.  The latter are locked to the window size, so they will
     * scale with changes.
     */
    public void frame( double x, double y, double w, double h ) {
        _xFrame = x;
        _yFrame = y;
        _wFrame = w;
        _hFrame = h;
        if ( x <= 1.0 && x >= -1.0 )
            _xFrameUseFraction = true;
        else
            _xFrameUseFraction = false;
        if ( y <= 1.0 && y >= -1.0 )
            _yFrameUseFraction = true;
        else
            _yFrameUseFraction = false;
        if ( w <= 1.0 && w >= -1.0 )
            _wFrameUseFraction = true;
        else
            _wFrameUseFraction = false;
        if ( h <= 1.0 && h >= -1.0 )
            _hFrameUseFraction = true;
        else
            _hFrameUseFraction = false;
    }
    public double frameX() { return _xFrame; }
    public double frameY() { return _yFrame; }
    public double frameW() { return _wFrame; }
    public double frameH() { return _hFrame; }
    public boolean frameXUseFraction() { return _xFrameUseFraction; }
    public boolean frameYUseFraction() { return _yFrameUseFraction; }
    public boolean frameWUseFraction() { return _wFrameUseFraction; }
    public boolean frameHUseFraction() { return _hFrameUseFraction; }
    
    /*
     * Set frame dimensions individually, along with whether the numbers given
     * represent fractions.
     */
    public void frameX( double x, boolean isFraction ) {
        _xFrame = x;
        _xFrameUseFraction = isFraction;
    }
    public void frameY( double y, boolean isFraction ) {
        _yFrame = y;
        _yFrameUseFraction = isFraction;
    }
    public void frameW( double w, boolean isFraction ) {
        _wFrame = w;
        _wFrameUseFraction = isFraction;
    }
    public void frameH( double h, boolean isFraction ) {
        _hFrame = h;
        _hFrameUseFraction = isFraction;
    }
    
    /*
     * These return the "current" dimensions of the frame in pixels.
     */
    public double x() { return _x; }
    public double y() { return _y; }
    public double w() { return _w; }
    public double h() { return _h; }
    
    /*
     * The frame color is the color of the box around a plot.
     */
    public void frameColor( Color newColor ) {
        _frame.color( newColor );
    }
    public Color frameColor() {
        return _frame.color();
    }
    
    /*
     * The width of the line used to draw the frame.
     */
    public void frameWidth( double newWidth ) {
        _frame.lineWidth( newWidth );
    }
    public double frameWidth() {
        return _frame.lineWidth();
    }
    
    /*
     * Draw the frame, or don't.
     */
    public void drawFrame( boolean newVal ) {
        _frame.visible( newVal );
    }
    public boolean drawFrame() {
        return _frame.visible();
    }
    
    /*
     * Fill the plot background with color, or don't.
     */
    public void drawBackground( boolean newVal ) {
        _background.visible( newVal );
    }
    public boolean drawBackground() {
        return _background.visible();
    }
    
    /*
     * Turn clipping on or off for this plot.  Clipping prevents data from being
     * drawn outside the frame.
     */
    public void clip( boolean newVal ) {
        if ( newVal )
            _dataHolder.setClip( 0.0, 0.0, 1.0, 1.0 );
        else
            _dataHolder.setClipOff();
    }
    public boolean clip() {
        return _dataHolder._clipSet;
    }
    
    /*
     * Set the limits on the plot.  These determine where data are drawn by
     * using the DrawObject scale feature.
     */
    public void limits( double xLow, double xHigh, double yLow, double yHigh ) {
        _xLow = xLow;
        _xHigh = xHigh;
        _yLow = yLow;
        _yHigh = yHigh;
        regrid();
        topRegrid();
        relabel();
        rescale();
    }
    
    /*
     * Return the current limits.
     */
    public double xLow() { return _xLow; }
    public double xHigh() { return _xHigh; }
    public double yLow() { return _yLow; }
    public double yHigh() { return _yHigh; }
    
    /*
     * Add a data curve to the plot.
     */
    public void addCurve( double x[], double y[] ) {
        Curve2D newObject = new Curve2D( x, y );
        _dataHolder.add( newObject );
    }
    
    /*
     * Add an existing curve to the plot.
     */
    public void addCurve( Curve2D newCurve ) {
        _dataHolder.add( newCurve );
    }
    
    /*
     * Add a track to this plot.
     */
    public void addTrack( Track2D newTrack ) {
        _dataHolder.add( newTrack );
    }
    
    /*
     * Compute a reasonable step size, given a delta, a minimum number of steps
     * and a maximum number of steps.
     */
    static public double stepSize( double delta, double minSteps, double maxSteps ) {
        //  Bail out if the user has done something stupid.
        if ( delta == 0.0 )
            return 0.0;
        if ( minSteps >= maxSteps )
            return 0.0;
        //  Find the order of the delta.
        double delt = Math.abs( delta );
        double logDelt = Math.log10( delt );
        int order = (int)logDelt;
        if ( logDelt < 0.0 )
            order -= 1;
        double modelStep = Math.pow( 10.0, (double)order );
        //  Mess around with the modelStep until it is within range.
        while ( delt / modelStep < minSteps )
            modelStep /= 10.0;
        while ( delt / modelStep > maxSteps )
            modelStep *= 2.0;
        if ( delta < 0.0 )
            return -modelStep;
        else
            return modelStep;
    }

    /*
     * This class holds information we need to track about user-requested grids.
     */
    protected class GridStructure {
        public GridStructure( int newAxis, Double newStep, Color newColor ) {
            axis = newAxis;
            color = newColor;
            step = newStep;
        }
        public int axis;
        public Double step;
        public Color color;
    };
    public static final int X_AXIS = 0;
    public static final int Y_AXIS = 1;
    
    /*
     * Remove all grid lines.
     */
    public void deleteGrids() {
        _gridInformation.clear();
        regrid();
    }
    
    /*
     * Add a grid specification.  This includes axis the grid is perpendicular
     * to (i.e. which axis it steps along), the size of the spacing of grid
     * lines, and their color.  The axis is defined as 0 for X, 1 for Y.
     */
    public void addGrid( int axis, Double step, Color color ) {
        _gridInformation.add( new GridStructure( axis, step, color ) );
        regrid();
    }
    
    /*
     * These are similar functions for the grid that lies on top of plots.
     */
    public void deleteTopGrids() {
        _topGridInformation.clear();
        topRegrid();
    }
    public void addTopGrid( int axis, Double step, Color color ) {
        _topGridInformation.add( new GridStructure( axis, step, color ) );
        topRegrid();
    }
    
    /*
     * This class holds information we need to know about drawing labels.  It
     * inherits but greatly expands the grid structure.
     */
    protected class LabelStructure extends GridStructure {
        public LabelStructure( int newAxis, Double newStart, Double newStop, 
                Double newStep, Color newColor, String newFormat,
                Double newTicSize, double newGapSize, int newJustification,
                Double newCrossValue, boolean newDrawScale, boolean newPlotAlways ) {
            super( newAxis, newStep, newColor );
            start = newStart;
            stop = newStop;
            format = newFormat;
            ticSize = newTicSize;
            gapSize = newGapSize;
            justification = newJustification;
            crossValue = newCrossValue;
            drawScale = newDrawScale;
            plotAlways = newPlotAlways;
        }
        public Double start;
        public Double stop;
        public String format;
        public Double ticSize;
        public double gapSize;
        public int justification;
        public Double crossValue;
        public boolean plotAlways;
        public boolean drawScale;
    };
    
    /*
     * Delete all labels.
     */
    public void deleteLabels() {
        _labelInformation.clear();
        relabel();
    }
    
    /*
     * Add some labels to a plot.  There are (too) many arguments here to give
     * maximum flexibility to the labels.  A brief explanation of each:
     * 
     *    axis          - Either X_AXIS or Y_AXIS.
     *    start         - The value at which labels should start.  If null labels
     *                    will start inside the plot.
     *    stop          - The last value at which a label will be drawn.  If null
     *                    labels will stop at the limits of the plot.
     *    step          - The distance between labels in the units of the given
     *                    axis.  A 0.0 here indicates that only one label should
     *                    be drawn.
     *    color         - The color of labels.  If not included, the global
     *                    color applied to labels will be used.
     *    format        - Format for converting numbers into label strings.  This
     *                    follows the convention of the Java String.format()
     *                    function (which is where it is used) with ONE EXCEPTION.
     *                    If "%#.#EE" formatting is used, superscripts (as opposed
     *                    to "e" characters) will be used.  A null format will
     *                    cause the numbers to be converted conventionally.  Note
     *                    that the format can just be a bunch of text - it doesn't
     *                    need to include the value it is labeling.
     *    ticSize       - The size of assoicated tic marks either in pixels or
     *                    fractions of the plot dimensions.  5.0 by default, can
     *                    be negative.  0.0 indicates no tic marks.  Tic marks
     *                    are drawn with the label color.
     *    gapSize       - The gap between the start of the label text and the 
     *                    position of the "crossValue" (see below).  10.0 by
     *                    default, can be negative.
     *    justification - Label justification, can be DrawObject.LEFT_JUSTIFY,
     *                    DrawObject.RIGHT_JUSTIFY, or DrawObject.CENTER_JUSTIFY.
     *    crossValue    - The value on the opposite axis at which the labels will
     *                    be drawn.  For instance, if "axis" is X_AXIS, this is
     *                    the value in Y at which the numbers will be drawn.  If
     *                    null, the plot minimum will be used.
     *    plotAlways    - Even if this label is outside the plot limits, draw it.
     *    drawScale     - Draw a line along the axis of the labels.  This will be
     *                    done using the label color.
     */
    public void addLabels( int axis, Double start, Double stop, Double step, 
            Color color, String format, double ticSize, double gapSize, 
            int justification, Double crossValue, boolean drawScale, 
            boolean plotAlways ) {
        _labelInformation.add( new LabelStructure( axis, start, stop, step, color,
                format, ticSize, gapSize, justification, crossValue, drawScale,
                plotAlways ) );
        relabel();
    }
    
    /*
     * These are some simplified versions of the above function using common
     * defaults.
     */
    public void addLabels( int axis, Double step, String format, double ticSize,
            double gapSize, Color color ) {
        int justification = DrawObject.RIGHT_JUSTIFY;
        if ( axis == X_AXIS )
            justification = DrawObject.CENTER_JUSTIFY;
        this.addLabels( axis, null, null, step, color, format, ticSize, gapSize,
                justification, null, false, false );
    }
    public void addLabels( int axis, Double step, String format, Color color ) {
        double gapSize = -10.0;
        double ticSize = -5.0;
        if ( axis == X_AXIS ) {
            gapSize = 10.0;
            ticSize = 5.0;
        }
        this.addLabels( axis, step, format, ticSize, gapSize, color );
    }
    public void addLabels( int axis, Double step, String format ) {
        this.addLabels( axis, step, format, null );
    }
    public void addLabels( int axis, Double step, Color color ) {
        this.addLabels( axis, step, null, color );
    }
    public void addLabels( int axis, Double step ) {
        this.addLabels( axis, step, null, null );
    }
    
    /*
     * Set the color of labels.  This can be changed for individual labels.
     */
    public void labelColor( Color newColor ) {
        _labelHolder.color( newColor );
    }
    public Color labelColor() {
        return _labelHolder.color();
    }
    
    /*
     * Set the font of the labels.
     */
    public void labelFont( Font newFont ) {
        _labelHolder.font( newFont );
    }
    public Font labelFont() {
        return _labelHolder.font();
    }
    
    /*
     * Give the plot a title.  By default the title is centered on the plot and
     * uses the current font (whatever that is), but those things can be changed.
     */
    public void title( String newTitle ) {
        this.title( newTitle, DrawObject.CENTER_JUSTIFY );
    }
    
    /*
     * Give the plot an X title.
     */
    public void xTitle( String newTitle ) {
        this.xTitle( newTitle, DrawObject.CENTER_JUSTIFY );
    }
    
    /*
     * Give the plot an Y title.
     */
    public void yTitle( String newTitle ) {
        this.yTitle( newTitle, DrawObject.CENTER_JUSTIFY );
    }
    
    /*
     * This is the more generic version of the title function, allowing different
     * justification settings.  The position can be set using titlePos().
     */
    public void title( String newTitle, int justification ) {
        _title.clear();
        DrawObject newObject = new DrawObject();
        newObject.complexText( justification, 0.0, 0.0, newTitle );
        _titleString = new String( newTitle );
        _titleJustification = justification;
        _title.add( newObject );
    }
    
    /*
     * This is the more generic version of the xTitle function, allowing different
     * justification settings.  The position can be set using xTitlePos().
     */
    public void xTitle( String newTitle, int justification ) {
        _xTitle.clear();
        DrawObject newObject = new DrawObject();
        newObject.complexText( justification, 0.0, 0.0, newTitle );
        _xTitleString = new String( newTitle );
        _xTitleJustification = justification;
        _xTitle.add( newObject );
    }
    
    /*
     * This is the more generic version of the yTitle function, allowing different
     * justification settings.  The position can be set using yTitlePos().
     */
    public void yTitle( String newTitle, int justification ) {
        _yTitle.clear();
        DrawObject newObject = new DrawObject();
        newObject.complexText( justification, 0.0, 0.0, newTitle );
        _yTitleString = new String( newTitle );
        _yTitleJustification = justification;
        _yTitle.add( newObject );
    }
    
    /*
     * Change the title fonts.
     */
    public void titleFont( Font newFont ) {
        if ( newFont == null )
            _title.fontOff();
        else
            _title.font( newFont );
    }
    public Font titleFont() { return _title.font(); }
    public void xTitleFont( Font newFont ) {
        if ( newFont == null )
            _xTitle.fontOff();
        else
            _xTitle.font( newFont );
    }
    public Font xTitleFont() { return _xTitle.font(); }
    public void yTitleFont( Font newFont ) {
        if ( newFont == null )
            _yTitle.fontOff();
        else
            _yTitle.font( newFont );
    }
    public Font yTitleFont() { return _yTitle.font(); }
    
    /*
     * Set or get the color assigned to the titles.
     */
    public void titleColor( Color newColor, boolean apply ) {
        if ( apply )
            _title.color( newColor );
        else
            _title.colorOff();
    }
    public Color titleColor() { return _title.color(); }
    public boolean titleColorSet() { return _title.colorSet(); }
    public void xTitleColor( Color newColor, boolean apply ) {
        if ( apply )
            _xTitle.color( newColor );
        else
            _xTitle.colorOff();
    }
    public Color xTitleColor() { return _xTitle.color(); }
    public boolean xTitleColorSet() { return _xTitle.colorSet(); }
    public void yTitleColor( Color newColor, boolean apply ) {
        if ( apply )
            _yTitle.color( newColor );
        else
            _yTitle.colorOff();
    }
    public Color yTitleColor() { return _yTitle.color(); }
    public boolean yTitleColorSet() { return _yTitle.colorSet(); }
    
    /*
     * Set the position of the plot title relative to the top left corner of 
     * the plot (given that putting the title on top of a plot is conventional).  
     * Fractional numbers are fractions of the frame sizes, numbers larger than
     * 1.0 are in pixels.
     */
    public void titlePosition( double x, double y ) {
        if ( x <= 1.0 && x >= -1.0 )
            _titleXUseFraction = true;
        else
            _titleXUseFraction = false;
        _titleXPos = x;
        if ( y <= 1.0 && y >= -1.0 )
            _titleYUseFraction = true;
        else
            _titleYUseFraction = false;
        _titleYPos = y;
        repositionTitle();
    }
    
    /*
     * Set the position of the X title relative to the bottom left corner of 
     * the plot (given that putting the X title below the plot is conventional).  
     * Fractional numbers are fractions of the frame sizes, numbers larger than
     * 1.0 are in pixels.
     */
    public void xTitlePosition( double x, double y ) {
        if ( x <= 1.0 && x >= -1.0 )
            _xTitleXUseFraction = true;
        else
            _xTitleXUseFraction = false;
        _xTitleXPos = x;
        if ( y <= 1.0 && y >= -1.0 )
            _xTitleYUseFraction = true;
        else
            _xTitleYUseFraction = false;
        _xTitleYPos = y;
        repositionTitle();
    }
    
    /*
     * Set the position of the Y title relative to the lower left corner of 
     * the plot (given that putting the title on the left of a plot is conventional).  
     * Fractional numbers are fractions of the frame sizes, numbers larger than
     * 1.0 are in pixels.
     */
    public void yTitlePosition( double x, double y ) {
        if ( x <= 1.0 && x >= -1.0 )
            _yTitleXUseFraction = true;
        else
            _yTitleXUseFraction = false;
        _yTitleXPos = x;
        if ( y <= 1.0 && y >= -1.0 )
            _yTitleYUseFraction = true;
        else
            _yTitleYUseFraction = false;
        _yTitleYPos = y;
        repositionTitle();
    }
    
    /*
     * Set the title positions and whether numbers are meant to be fractions of
     * the plot size.  This allows "fractions" greater than 1.0, among other things.
     * There are three sets of functions for the three title types.
     */
    public void titleXPos( double x, boolean useFraction ) {
        _titleXUseFraction = useFraction;
        _titleXPos = x;
        repositionTitle();
    }
    public void titleYPos( double y, boolean useFraction ) {
        _titleYUseFraction = useFraction;
        _titleYPos = y;
        repositionTitle();
    }
    public void xTitleXPos( double x, boolean useFraction ) {
        _xTitleXUseFraction = useFraction;
        _xTitleXPos = x;
        repositionTitle();
    }
    public void xTitleYPos( double y, boolean useFraction ) {
        _xTitleYUseFraction = useFraction;
        _xTitleYPos = y;
        repositionTitle();
    }
    public void yTitleXPos( double x, boolean useFraction ) {
        _yTitleXUseFraction = useFraction;
        _yTitleXPos = x;
        repositionTitle();
    }
    public void yTitleYPos( double y, boolean useFraction ) {
        _yTitleYUseFraction = useFraction;
        _yTitleYPos = y;
        repositionTitle();
    }
    
    /*
     * Return the current title positions.  Three sets of functions for the three
     * title types.
     */
    public double titleXPos() {
        return _titleXPos;
    }
    public double titleYPos() {
        return _titleYPos;
    }
    public double xTitleXPos() {
        return _xTitleXPos;
    }
    public double xTitleYPos() {
        return _xTitleYPos;
    }
    public double yTitleXPos() {
        return _yTitleXPos;
    }
    public double yTitleYPos() {
        return _yTitleYPos;
    }
    
    /*
     * Rotate titles.  Shut off rotations by passing null.
     */
    public void titleRotate( Double newVal ) {
        if ( newVal == null )
            _title.rotateOff();
        else
            _title.rotate( newVal );
    }
    public void xTitleRotate( Double newVal ) {
        if ( newVal == null )
            _xTitle.rotateOff();
        else
            _xTitle.rotate( newVal );
    }
    public void yTitleRotate( Double newVal ) {
        if ( newVal == null )
            _yTitle.rotateOff();
        else
            _yTitle.rotate( newVal );
    }
    
    /*
     * Return whether the title positions are considered fractions of the plot
     * size or pixel values.  Three sets of functions for the three title types.
     */
    public boolean titleXUseFraction() { return _titleXUseFraction; }
    public boolean titleYUseFraction() { return _titleYUseFraction; }
    public boolean xTitleXUseFraction() { return _xTitleXUseFraction; }
    public boolean xTitleYUseFraction() { return _xTitleYUseFraction; }
    public boolean yTitleXUseFraction() { return _yTitleXUseFraction; }
    public boolean yTitleYUseFraction() { return _yTitleYUseFraction; }
    
    /*
     * Return the string associated with the title.  There are three functions for each
     * of the title types.
     */
    public String titleString() {
        return _titleString;
    }
    public String xTitleString() {
        return _xTitleString;
    }
    public String yTitleString() {
        return _yTitleString;
    }
    
    /*
     * Return the justification of the titles.  There are three functions for each
     * of the title types.
     */
    public int titleJustification() {
        return _titleJustification;
    }
    public int xTitleJustification() {
        return _xTitleJustification;
    }
    public int yTitleJustification() {
        return _yTitleJustification;
    }
    
    /*
     * This function is called to reposition titles when the frame of the plot is
     * changed.  It is applied to the title of the plot and the X and Y axis titles.
     */
    public void repositionTitle() {
        double x = _titleXPos;
        if ( _titleXUseFraction )
            x *= _w;
        double y = _titleYPos;
        if ( _titleYUseFraction )
            y *= _h;
        _title.translate( _x + x, _y - y );
        x = _xTitleXPos;
        if ( _xTitleXUseFraction )
            x *= _w;
        y = _xTitleYPos;
        if ( _xTitleYUseFraction )
            y *= _h;
        _xTitle.translate( _x + x, _y + _h - y );
        x = _yTitleXPos;
        if ( _yTitleXUseFraction )
            x *= _w;
        y = _yTitleYPos;
        if ( _yTitleYUseFraction )
            y *= _h;
        _yTitle.translate( _x + x, _y + _h + y );
    }
    
    /*
     * Get rid of all extra items.
     */
    public void clearExtraItems() {
        _extraItems.clear();
    }
    
    /*
     * Create a new "extra" item object with position specifications.  The DrawObject
     * created is returned.
     */
    public DrawObject newExtraItem( double xPos, int xPosType, double yPos, int yPosType ) {
        ExtraItem newItem = new ExtraItem( xPos, xPosType, yPos, yPosType );
        addExtraItem( newItem );
        return newItem;
    }
    
    /*
     * Add an extra item.  This must be a DrawObject of the ExtraItem class.
     */
    public void addExtraItem( ExtraItem newItem ) {
        _extraItems.add( newItem );
        repositionExtraItems();
    }
    
    /*
     * This resets all of the positions of the extra items based on current criteria.
     */
    public void repositionExtraItems() {
        for ( Iterator<DrawObject> iter = _extraItems.iterator(); iter.hasNext(); ) {
            ExtraItem thisItem = (ExtraItem)iter.next();
            //  Translate based on the item description.
            double xt = thisItem.xPos;
            double yt = thisItem.yPos;
            switch ( thisItem.xPosType ) {
                case ExtraItem.BY_PIXEL:
                    //  No change required.
                    break;
                case ExtraItem.BY_FRAME:
                    //  Scale the x to the frame size.
                    xt *= _w;
                    break;
                case ExtraItem.BY_XY:
                    xt = ( xt - _xLow ) * _w / ( _xHigh - _xLow ); // UNTESTED
                    break;
            }
            switch ( thisItem.yPosType ) {
                case ExtraItem.BY_PIXEL:
                    //  No change required.
                    break;
                case ExtraItem.BY_FRAME:
                    //  Scale the y to the frame size.
                    yt *= _h;
                    break;
                case ExtraItem.BY_XY:
                    yt = ( yt - _yLow ) * _w / ( _xLow - _xHigh ); // UNTESTED
                    break;
            }
            thisItem.translate( _x + xt, _y - yt );
        }
    }
    
    //  These objects contain various components of the plot.
    DrawObject _background;
    DrawObject _frame;
    DrawObject _gridHolder;
    DrawObject _dataHolder;
    DrawObject _topGridHolder;
    DrawObject _data;
    DrawObject _labelHolder;
    DrawObject _xTitle;
    DrawObject _yTitle;
    DrawObject _title;

    //  These are nominal sizes for this plot frame.  They are not used in this
    //  class, but are useful outside of it (they were specifically designed for
    //  the PlotWindow class).
    protected double _xFrame;
    protected double _wFrame;
    protected double _yFrame;
    protected double _hFrame;
    protected boolean _xFrameUseFraction;
    protected boolean _yFrameUseFraction;
    protected boolean _wFrameUseFraction;
    protected boolean _hFrameUseFraction;
    
    //  These are the true size of the frame.  These values are used internally,
    //  and there should be no direct access to them.
    protected double _x;
    protected double _y;
    protected double _w;
    protected double _h;
    
    //  These are the limits applied to the plotted area.  They determine the
    //  scale at which data will be plotted.
    protected double _xLow;
    protected double _xHigh;
    protected double _yLow;
    protected double _yHigh;
    
    //  These are numbers determine the position of the title relative to the
    //  top left corner of the plot (given that putting the title on top of a
    //  plot is conventional).  Fractional numbers are fractions of the frame
    //  sizes, numbers larger than 1.0 are in pixels.
    protected double _titleXPos;
    protected double _titleYPos;
    protected boolean _titleXUseFraction;
    protected boolean _titleYUseFraction;
    
    //  These are items identical to those above, used for the x and y titles.
    protected double _xTitleXPos;
    protected double _xTitleYPos;
    protected boolean _xTitleXUseFraction;
    protected boolean _xTitleYUseFraction;
    protected double _yTitleXPos;
    protected double _yTitleYPos;
    protected boolean _yTitleXUseFraction;
    protected boolean _yTitleYUseFraction;
    
    //  These variables are used to store information about the title settings,
    //  but are not actually used in drawing.  They were created so an editor
    //  could see what these values were.
    protected String _titleString;
    protected int _titleJustification;
    
    //  X and Y title items identical to those above.
    protected String _xTitleString;
    protected int _xTitleJustification;
    protected String _yTitleString;
    protected int _yTitleJustification;
    
    //  This ArrayDeque holds information about grid lines that should be drawn.
    //  It is required so that we can track this information and regrid with it
    //  whenever the limits change.
    protected ArrayDeque<GridStructure> _gridInformation;
    protected ArrayDeque<GridStructure> _topGridInformation;
    
    //  This is a similar structure for plot axis labels.
    protected ArrayDeque<LabelStructure> _labelInformation;

    //  "Extra" items are positioned automatically based on the plot size, or limits,
    //  relative to the upper left corner of the plot.  Positions can be specified
    //  independently for x and y.  These positions govern the translations of the
    //  objects - overriding any other translations!  If you need to translate relative
    //  to the specified position, make a child object and translate that.
    protected DrawObject _extraItems;
    public class ExtraItem extends DrawObject {
        public static final int BY_PIXEL = 1;  //  position in pixels from the upper left of the plot
        public static final int BY_FRAME = 2;  //  position as a fraction of the frame size from the upper left of the plot
        public static final int BY_XY    = 3;  //  position based on the plot limits.
        public double xPos;
        public int xPosType;
        public double yPos;
        public int yPosType;
        public ExtraItem( double newXPos, int newXPosType, double newYPos, int newYPosType ) {
            super();
            xPos = newXPos;
            xPosType = newXPosType;
            yPos = newYPos;
            yPosType = newYPosType;
        }
        public void position( double newXPos, int newXPosType, double newYPos, int newYPosType ) {
            xPos = newXPos;
            xPosType = newXPosType;
            yPos = newYPos;
            yPosType = newYPosType;
        }
    }
}
