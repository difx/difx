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
 * This widget provides the buttons necessary to produce a complete font
 * setting, including name, italic and bold settings, and size.  This class
 * dispatches different events for different setting changes (see the
 * FontSettingEvent class, which is internal).  These different event types
 * can be treated differently, or they can simply be intepreted as a "general"
 * change in the font.
 */
package mil.navy.usno.widgetlib;

import java.awt.Font;
import java.awt.Insets;

import javax.swing.JPanel;
import javax.swing.JComboBox;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.event.EventListenerList;

public class FontSetting extends JPanel {
    
    public FontSetting() {
        this.setLayout( null );
        _name = new JComboBox();
        this.add( _name );
        _name.addItem( new String( "SansSerif" ) );
        _name.addItem( new String( "Serif" ) );
        _name.addItem( new String( "Monospaced" ) );
        _name.addItem( new String( "Dialog" ) );
        _name.addItem( new String( "DialogInput" ) );
        _bold = new ToggleButton( "B" );
        //_bold.setMargin( new Insets( 0, 0, 2, 0 ) );
        this.add( _bold );
        _italic = new ToggleButton( "I" );
        _italic.setFont( new Font( "Dialog", Font.BOLD | Font.ITALIC, 12 ) );
        this.add( _italic );
        _scale = new NumberBox();
        _scale.precision( 0 );
        _scale.value( 12 );
        _scale.minimum( 0.0 );
        _actionListeners = new EventListenerList();
        //  Callbacks for the various components.
        _name.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                nameChange();
            }
        } );
        _bold.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                boldChange();
            }
        } );
        _italic.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                italicChange();
            }
        } );
        _scale.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                scaleChange();
            }
        } );
        this.add( _scale );
        fontToolTipName( new String( "Font" ) );
    }

    /*
     * There are four widgets we have to cram in whatever space we are given.
     * The bold and italic buttons are assumed to be squares, so their widths
     * are set by the height.  The remaining space is given 3/4 to the name and
     * 1/4 to the scale.
     */
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        int scaleSize = ( w - 2 * h ) / 4;
        _name.setBounds( 0, 0, w - 2 * h - scaleSize, h );
        _bold.setBounds( w - 2 * h - scaleSize, 0, h, h );
        _italic.setBounds( w - h - scaleSize, 0, h, h );
        _scale.setBounds( w - scaleSize, 0, scaleSize, h );
    }

    /*
     * Return the font represented by this object.
     */
    public Font font() {
        int fontStyle = 0;
        if ( _bold.pressed() )
            fontStyle |= Font.BOLD;
        if ( _italic.pressed() )
            fontStyle |= Font.ITALIC;
        return ( new Font( (String)(_name.getSelectedItem()), fontStyle, (int)(_scale.value()) ) );
    }
    
    /*
     * Set the font.  The incoming font can be null.
     */
    public void font( Font newFont ) {
        if ( newFont == null )
            return;
        _name.setSelectedItem( newFont.getName() );
        _scale.value( (double)newFont.getSize() );
        _bold.pressed( newFont.isBold() );
        _italic.pressed( newFont.isItalic() );           
    }
        
    static public int GENERAL_CHANGE = 0;
    static public int NAME_CHANGE = 1;
    static public int BOLD_CHANGE = 2;
    static public int ITALIC_CHANGE = 3;
    static public int SCALE_CHANGE = 4;
    
    public class FontSettingEvent extends ActionEvent {
        public FontSettingEvent( FontSetting instigator, int type ) {
            super( instigator, ActionEvent.ACTION_PERFORMED, "" );
            _type = type;
        }
        public int _type;
    }
    
    protected void nameChange() {
        this.dispatchAction( new FontSettingEvent( this, NAME_CHANGE ) );
    }
    
    protected void boldChange() {
        this.dispatchAction( new FontSettingEvent( this, BOLD_CHANGE ) );
    }
    
    protected void italicChange() {
        this.dispatchAction( new FontSettingEvent( this, ITALIC_CHANGE ) );
    }
    
    protected void scaleChange() {
        this.dispatchAction( new FontSettingEvent( this, SCALE_CHANGE ) );
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

    public void fontToolTipName( String newName ) { 
        _fontToolTipName = newName;
        _name.setToolTipText( _fontToolTipName + " name or \"family\"." );
        _bold.setToolTipText( "Toggle bold on or off." );
        _italic.setToolTipText( "Toggle italic on or off." );
        _scale.setToolTipText( "The " + _fontToolTipName + " scale, measured in pixels.  This is the approximate vertical size of the largest characters." );
    }

    protected JComboBox _name;
    protected NumberBox _scale;
    protected ToggleButton _bold;
    protected ToggleButton _italic;
    
    protected String _fontToolTipName;
    
    EventListenerList _actionListeners;
    
}
