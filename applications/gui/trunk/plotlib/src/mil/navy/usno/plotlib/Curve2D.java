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
 * This is a DataObject that draws a curve through a series of data points.  It
 * overrides the "scale" method to change the actual data when plots are resized
 * instead of applying a scale (the reason for this is to avoid changing the
 * thickness of plot lines).  This is a somewhat inefficient was to draw curves
 * repeatedly, as the data have to be completely reconstructed at the DrawObject
 * level whenever a change is made.
 */
package mil.navy.usno.plotlib;

/**
 *
 * @author jspitzak
 */
public class Curve2D extends DrawObject {
    
    /*
     * Construct duplicate arrays to hold plotted data.  These data are scaled
     * as necessary.  We also maintain pointers to the initial data so we can
     * adjust as they change.
     */
    public Curve2D( double x[], double y[] ) {
        _n = x.length;
        if ( y.length < _n )
            _n = y.length;
        _xPlot = new double[_n];
        _yPlot = new double[_n];
        _saveXScale = 1.0;
        _saveYScale = 1.0;
        _x = x;
        _y = y;
    }
    
    @Override
    public void scale( double newX, double newY ) {
        _saveXScale = newX;
        _saveYScale = newY;
        dataChange();
    }
    
    public void dataChange() {
        for ( int i = 0; i < _n; ++i ) {
            _xPlot[i] = _saveXScale * _x[i];
            _yPlot[i] = _saveYScale * _y[i];
        }
        this.drawpoly( _xPlot, _yPlot, _n );
    }
    
    protected double _saveXScale;
    protected double _saveYScale;
    protected double _x[];
    protected double _y[];
    protected double _xPlot[];
    protected double _yPlot[];
    protected int _n;
    
}
