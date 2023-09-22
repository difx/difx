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
 * This widget is used to change a position in a window.  It expresses the position
 * both as an absolute number of pixels and as a fraction of a larger number 
 * (presumably the size of the window).  The "pixels" value is first.  Check boxes
 * accompany each value to allow a user to determine which applies (i.e. which
 * of the two values will be returned by the "value()" function).
 * 
 * The translation of the two numbers is made using a number set via the
 * "windowSize()" function.
 */
package mil.navy.usno.plotlib;

import mil.navy.usno.widgetlib.NumberBox;
import javax.swing.JPanel;
import javax.swing.JCheckBox;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.EventListenerList;

public class PixelLocation extends JPanel {
    
    public PixelLocation() {
        this.setLayout( null );
        _actionListeners = new EventListenerList();
        _pixels = new NumberBox();
        _pixels.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                pixelsChange();
            }
        });
        this.add( _pixels );
        _fraction = new NumberBox();
        _fraction.precision( 3 );
        _fraction.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                fractionChange();
            }
        });
        this.add( _fraction );
        _usePixels = new JCheckBox();
        _usePixels.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                checkBoxChange( evt.getSource() );
                sendEvent();
            }
        });
        this.add( _usePixels );
        _useFraction = new JCheckBox();
        _useFraction.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                checkBoxChange( evt.getSource() );
                sendEvent();
            }
        });
        this.add( _useFraction );
        _bufferSize = 2;
        useFraction( true );
        _pixels.setToolTipText( "Value in pixels." );
        _usePixels.setToolTipText( "Check to use the pixel value." );
        _fraction.setToolTipText( "Value as fraction." );
        _useFraction.setToolTipText( "Check to use the fracitonal value." );
    }
    
    public void setPixelsToolTip( String newTip ) { _pixels.setToolTipText( newTip ); }
    public void setFractionToolTip( String newTip ) { _fraction.setToolTipText( newTip ); }
    public void setPixelsCheckToolTip( String newTip ) { _usePixels.setToolTipText( newTip ); }
    public void setFractionCheckToolTip( String newTip ) { _useFraction.setToolTipText( newTip ); }
    
    /*
     * The height of any boundary change is used to as a square size for the
     * check boxes.  The remaining width after these are taken into account is
     * split between the pixel and fraction number boxes with buffer between
     * them.
     */
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        if ( _pixels != null ) {
            int boxSize = ( w - 2 * h - 4 * _bufferSize ) / 2;
            _pixels.setBounds( 0, 0, boxSize, h );
            _usePixels.setBounds( boxSize + _bufferSize, 0, h, h );
            _fraction.setBounds( boxSize + 3 * _bufferSize  + h, 0, boxSize, h );
            _useFraction.setBounds( 2 * boxSize + 4 * _bufferSize + h, 0, h, h );
        }
    }
    
    public void usePixels( boolean newVal ) {
        _usePixels.setSelected( newVal );
        checkBoxChange( _usePixels );
    }
    public boolean usePixels() { return _usePixels.isSelected(); }
    public void useFraction( boolean newVal ) {
        _useFraction.setSelected( newVal );
        checkBoxChange( _useFraction );
    }
    
    /*
     * A check box was changed.  This causes the other box to become the reverse
     * of the changed box.
     */
    protected void checkBoxChange( Object o ) {
        if ( o == _usePixels ) {
            if ( _usePixels.isSelected() )
                _useFraction.setSelected( false );
            else
                _useFraction.setSelected( true );
        }
        else {
            if ( _useFraction.isSelected() )
                _usePixels.setSelected( false );
            else
                _usePixels.setSelected( true );
        }
        if ( _usePixels.isSelected() ) {
            _pixels.setEnabled( true );
            _fraction.setEnabled( false );
        }
        else {
            _pixels.setEnabled( false );
            _fraction.setEnabled( true );
        }
    }
    
    public void windowSize( int newSize ) { 
        _windowSize = (double)newSize;
    }
    public void windowSize( double newSize ) { 
        _windowSize = newSize;
    }
    
    /*
     * Use a double precision number to set the value for this number and a boolean
     * value to indicate whether it is considered a "fraction" of the window size;
     */
    public void value( double newVal, boolean isFraction ) {
        if ( isFraction ) {
            _fraction.value( newVal );
            _pixels.value( newVal * _windowSize );
            useFraction( true );  //  this will change the check boxes appropriately
        }
        else {
            _pixels.value( newVal );
            _fraction.value( newVal / _windowSize );
            usePixels( true );
        }
    }
    
    /*
     * This version of the function returns the current value.
     */
    public double value() {
        if ( _useFraction.isSelected() )
            return _fraction.value();
        else
            return _pixels.value();
    }
    
    /*
     * This returns whether this value is considered a fraction.
     */
    public boolean isFraction() { return _useFraction.isSelected(); }
    
    /*
     * A change has occured in the fractions value box.  Use this change to
     * alter the value in the pixels box and notify parent components.
     */
    protected void fractionChange() {
        _pixels.value( _fraction.value() * _windowSize );
        sendEvent();
    }
    
    /*
     * Same function for the pixels box.
     */
    protected void pixelsChange() {
        _fraction.value( _pixels.value() / _windowSize );
        sendEvent();
    }
        
    protected void sendEvent() {
        Object[] listeners = _actionListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
    
    public void addActionListener( ActionListener a ) {
        _actionListeners.add( ActionListener.class, a );
    }

    NumberBox _pixels;
    NumberBox _fraction;
    JCheckBox _usePixels;
    JCheckBox _useFraction;
    int _bufferSize;
    double _windowSize;
    EventListenerList _actionListeners;
    
}
