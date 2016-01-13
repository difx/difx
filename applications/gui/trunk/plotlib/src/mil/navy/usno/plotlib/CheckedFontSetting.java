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
 * This widget implements a FontSetting class with an accompanying checkbox
 * meant to indicate whether the font is "applied" or not.  The checkbox doesn't
 * actually do anything, so it could be used however one saw fit.
 */
package mil.navy.usno.plotlib;

import mil.navy.usno.widgetlib.FontSetting;
import javax.swing.JPanel;
import javax.swing.JCheckBox;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.Font;

import javax.swing.event.EventListenerList;

public class CheckedFontSetting extends JPanel {
    
    public CheckedFontSetting() {
        super.setLayout( null );
        _actionListeners = new EventListenerList();
        _fontSetting = new FontSetting();
        this.add( _fontSetting );
        _check = new JCheckBox();
        this.add( _check );
        _fontSetting.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                dispatchAction( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
            }
        } );
        _check.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                dispatchAction( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
            }
        } );
        _check.setToolTipText( "Apply the font settings." );
    }
    
    public void checkToolTip( String newTip ) { _check.setToolTipText( newTip ); }
    public void fontToolTipName( String newName ) { _fontSetting.fontToolTipName( newName ); }
    
    public void font( Font newFont ) { _fontSetting.font( newFont ); }
    public Font font() { return _fontSetting.font(); } 
    public void apply( boolean newVal ) { _check.setSelected( newVal ); }
    public boolean apply() { return _check.isSelected(); }
    
    /*
     * The checkbox is made the a square on the right edge of the widget (its
     * size is determined from the height).  The color button fills the rest.
     */
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        _check.setBounds( w - h, 0, h, h );
        _fontSetting.setBounds( 0, 0, w - h, h );
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
    
    EventListenerList _actionListeners;
    FontSetting _fontSetting;
    JCheckBox _check;
    
}
