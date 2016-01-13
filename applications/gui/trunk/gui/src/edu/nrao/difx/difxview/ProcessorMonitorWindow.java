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
 * This is a popup window that monitors all of the data for a processor node.
 * Plots are included where appropriate.
 */
package edu.nrao.difx.difxview;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import java.awt.Color;

import mil.navy.usno.plotlib.PlotWindow;
import mil.navy.usno.plotlib.Plot2DObject;
import mil.navy.usno.plotlib.Track2D;

public class ProcessorMonitorWindow extends JFrame {
    
    public ProcessorMonitorWindow() {
        super( "" );
        _plotWindow = new PlotWindow();
        this.add( _plotWindow );
        this.setBounds( 500, 100, 500, 400 );
        _cpuPlot = new Plot2DObject();
        _cpuPlot.name( "CPU Plot" );
        _cpuTrack = new Track2D();
        _cpuTrack.fillCurve( true );
        _cpuPlot.addTrack( _cpuTrack );
        _cpuTrack.color( Color.GREEN );
        _cpuTrack.sizeLimit( 200 );
        _cpuPlot.frame( 0.1, 0.1, 0.375, 0.375 );
        _cpuPlot.backgroundColor( Color.BLACK );
        _cpuPlot.addLabels( Plot2DObject.Y_AXIS, 25.0 ); 
        //  Grids are added here (as opposed to where we set the limits) because
        //  they never actually change.
        _cpuPlot.addTopGrid( Plot2DObject.X_AXIS, 10.0, Color.GRAY );
        _cpuPlot.addTopGrid( Plot2DObject.Y_AXIS, 10.0, Color.GRAY );
        _cpuPlot.title( "CPU Usage (%)" );
        _plotWindow.add2DPlot( _cpuPlot );
        _memPlot = new Plot2DObject();
        _memPlot.name( "Memory Plot" );
        _memTrack = new Track2D();
        _memTrack.fillCurve( true );
        _memPlot.addTrack( _memTrack );
        _memTrack.color( Color.GREEN );
        _memTrack.sizeLimit( 200 );
        _memPlot.frame( 0.1, 0.6, 0.375, 0.375 );
        _memPlot.addLabels( Plot2DObject.Y_AXIS, 25.0 ); 
        //  Again, unchanging grids (see the comment for _cpuPlot).
        _memPlot.addTopGrid( Plot2DObject.X_AXIS, 10.0, Color.GRAY );
        _memPlot.addTopGrid( Plot2DObject.Y_AXIS, 10.0, Color.GRAY );
        _memPlot.backgroundColor( Color.BLACK );
        _memPlot.title( "Memory Usage (%)" );
        _plotWindow.add2DPlot( _memPlot );
        _transmitPlot = new Plot2DObject();
        _transmitPlot.name( "Transmit Plot" );
        _transmitTrack = new Track2D();
        _transmitTrack.fillCurve( true );
        _transmitPlot.addTrack( _transmitTrack );
        _transmitTrack.color( Color.GREEN );
        _transmitTrack.sizeLimit( 200 );
        _transmitPlot.frame( 0.6, 0.1, 0.375, 0.375 );
        _transmitPlot.backgroundColor( Color.BLACK );
        _transmitPlot.title( "Transmit Rate (Mb/s)" );
        _transmitMax = 0.01;
        _transmitPrecision = 4;
        _plotWindow.add2DPlot( _transmitPlot );
        _receivePlot = new Plot2DObject();
        _receivePlot.name( "Receive Plot" );
        _receiveTrack = new Track2D();
        _receiveTrack.fillCurve( true );
        _receivePlot.addTrack( _receiveTrack );
        _receiveTrack.color( Color.GREEN );
        _receiveTrack.sizeLimit( 200 );
        _receivePlot.frame( 0.6, 0.6, 0.375, 0.375 );
        _receivePlot.backgroundColor( Color.BLACK );
        _receivePlot.title( "Receive Rate (Mb/s)" );
        _receiveMax = 0.01;
        _receivePrecision = 4;
        _plotWindow.add2DPlot( _receivePlot );
    }
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        if ( _plotWindow != null ) {
            _plotWindow.setBounds( 0, 0, w, h );
            _plotWindow.updateUI();
        }
    }
    
    public void setNumCPUs( int newVal ) {
    }
    public void setNumCores( int newVal ) {
    }
    public void setBogusGHz( float newVal ) {
    }
    public void setType( int newVal ) {
    }
    public void setTypeString( String newVal ) {
    }
    public void setState( String newVal ) {
    }
    public void setCpuLoad( float newVal ) {
        _cpuPlot.limits( (double)(_cpuTrackSize - 100), (double)(_cpuTrackSize), 0.0, 100.0 );
        _cpuTrack.add( (double)(_cpuTrackSize), (double)(newVal) );
        _cpuTrackSize += 1;
        _plotWindow.updateUI();
    }
    public void setSysEnabled( boolean newVal ) {
    }
    public void setMemLoad( float newVal ) {
        _memPlot.limits( (double)(_memTrackSize - 100), (double)(_memTrackSize), 0.0, 100.0 );
        _memTrack.add( (double)(_memTrackSize), (double)(newVal) );
        _memTrackSize += 1;
        _plotWindow.updateUI();
    }
    public void setTotalMem( long newVal ) {
    }
    public void setUsedMem( long newVal ) {
    }
    public void setNetRxRate( double newVal ) {
        if ( newVal > 0.9 * _receiveMax ) {
            if ( _receivePrecision > 0 )
                --_receivePrecision;
            _receiveMax *= 10.0;
        }
        _receivePlot.deleteTopGrids();
        _receivePlot.addTopGrid( Plot2DObject.X_AXIS, 10.0, Color.GRAY );
        _receivePlot.addTopGrid( Plot2DObject.Y_AXIS, _receiveMax / 10.0, Color.GRAY );
        _receivePlot.deleteLabels();
        //  This precision crap (and the mirror of it for the transmit plot) is
        //  due to an annoying habit of Java of maitaining itty-bitty rounding errors
        //  in some numbers - which would be drawn in all their glory on the plot.
        _receivePlot.addLabels( Plot2DObject.Y_AXIS, _receiveMax / 4.0, ( "%." + _receivePrecision + "f" ) );
        _receivePlot.limits( (double)(_receiveTrackSize - 100), (double)(_receiveTrackSize), 0.0, _receiveMax );
        _receiveTrack.add( (double)(_receiveTrackSize), (double)(newVal) );
        _receiveTrackSize += 1;
        _plotWindow.updateUI();
    }
    public void setNetTxRate( double newVal ) {
        if ( newVal > 0.9 * _transmitMax ) {
            if ( _transmitPrecision > 0 )
                --_transmitPrecision;
            _transmitMax *= 10.0;
        }
        _transmitPlot.deleteTopGrids();
        _transmitPlot.addTopGrid( Plot2DObject.X_AXIS, 10.0, Color.GRAY );
        _transmitPlot.addTopGrid( Plot2DObject.Y_AXIS, _transmitMax / 10.0, Color.GRAY );
        _transmitPlot.deleteLabels();
        _transmitPlot.addLabels( Plot2DObject.Y_AXIS, _transmitMax / 4.0, ( "%." + _transmitPrecision + "f" ) );
        _transmitPlot.limits( (double)(_transmitTrackSize - 100), (double)(_transmitTrackSize), 0.0, _transmitMax );
        _transmitTrack.add( (double)(_transmitTrackSize), (double)(newVal) );
        _transmitTrackSize += 1;
        _plotWindow.updateUI();
    }
    
    protected Plot2DObject _cpuPlot;
    protected Track2D _cpuTrack;
    protected int _cpuTrackSize;
    protected Plot2DObject _memPlot;
    protected Track2D _memTrack;
    protected int _memTrackSize;
    protected Plot2DObject _transmitPlot;
    protected Track2D _transmitTrack;
    protected int _transmitTrackSize;
    protected double _transmitMax;
    protected int _transmitPrecision;
    protected Plot2DObject _receivePlot;
    protected Track2D _receiveTrack;
    protected int _receiveTrackSize;
    protected double _receiveMax;
    protected int _receivePrecision;
    protected PlotWindow _plotWindow;
}
