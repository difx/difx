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
 * This is a panel with a "tear off" button in the upper right.  Pushing the button
 * triggers a tear off event, which a parent panel can listen to and actually do
 * the tearing off.  
 */
package mil.navy.usno.widgetlib;

import java.awt.Font;
import java.awt.Insets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.event.EventListenerList;

/**
 *
 * @author jspitzak
 */
public class TearOffPanel extends JPanel {
    
    public TearOffPanel() {
        super();
        _tearOffButton = new JButton( "\u27a0" );//2398" );
        _tearOffButton.setFont( new Font( "Dialog", Font.BOLD, 16 ) );
        _tearOffButton.setMargin( new Insets( 0, 0, 2, 0 ) );
        _tearOffButton.setToolTipText( "Tear off this panel into its own window." );
        _tearOffButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                swapTearOffState();
                tearOffEvent();
            }
        } );
        _tearOffState = false;
        this.add( _tearOffButton );
        _tearOffListeners = new EventListenerList();
    }
    
    /*
     * Position the tear off button in the upper right.
     */
    @Override
    public void setBounds(int x, int y, int width, int height) {
        _tearOffButton.setBounds( width - 22, 2, 20, 20 );
        super.setBounds( x, y, width, height );
    }
    
    /*
     * This function is called to swap the state of the tear off button.
     */
    protected void swapTearOffState() {
        tearOffState( !_tearOffState );
    }
    
    /*
     * Set the tear off state.  This sets the button correctly.
     */
    public void tearOffState( boolean newVal ) {
        _tearOffState = newVal;
        if ( _tearOffState ) {
            _tearOffButton.setText( "\u27b2" );//2397" );
            _tearOffButton.setToolTipText( "Re-attach this panel to the main GUI." );
        }
        else {
            _tearOffButton.setText( "\u27a0" );//2398" );
            _tearOffButton.setToolTipText( "Tear off this panel into its own window." );
        }
    }
    
    /*
     * Return the tear off state.
     */
    public boolean tearOffState() { return _tearOffState; }
    
    /*
     * Trigger an event when the tear off button is pushed.  Parent windows actually
     * have to move this panel.
     */
    public void tearOffEvent() {
        Object[] listeners = _tearOffListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    /*
     * Add a listener to tear off events.
     */
    public void addTearOffListener( ActionListener a ) {
        _tearOffListeners.add( ActionListener.class, a );
    }

    JButton _tearOffButton;
    boolean _tearOffState;
    EventListenerList _tearOffListeners;

}
