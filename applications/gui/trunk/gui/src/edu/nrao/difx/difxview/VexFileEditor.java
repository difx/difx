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
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.SaneTextField;

import javax.swing.JLabel;
import javax.swing.JDialog;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import java.awt.Frame;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import mil.navy.usno.widgetlib.NodeBrowserScrollPane;

/**
 *
 * @author jspitzak
 */
public class VexFileEditor extends JDialog {
        
    public VexFileEditor( Frame frame, int x, int y, SystemSettings settings ) {
        super( frame, "Vex File Generator/Editor", true );
        _settings = settings;
        this.setBounds( x, y, 320, 280 );
        this.setResizable( true );
        this.getContentPane().setLayout( null );
        _this = this;
        _menuBar = new JMenuBar();
        JMenu helpMenu = new JMenu( "  Help  " );
        _menuBar.add( helpMenu );
        JMenuItem settingsHelpItem = new JMenuItem( "Vex File Editor Help" );
        settingsHelpItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.launchGUIHelp( "vexFileEditor.html" );
            }
        } );
        helpMenu.add( settingsHelpItem );
        JMenuItem helpIndexItem = new JMenuItem( "Help Index" );
        helpIndexItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.launchGUIHelp( "index.html" );
            }
        } );
        helpMenu.add( helpIndexItem );
        this.getContentPane().add( _menuBar );
        _scrollPane = new NodeBrowserScrollPane();
        _scrollPane.addTimeoutEventListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _this.newSize();
            }
        } );
        this.getContentPane().add( _scrollPane );
        /*
        IndexedPanel settingsFilePanel = new IndexedPanel( "Settings File" );
        JLabel idLabel = new JLabel( "Data Base ID:" );
        idLabel.setBounds( 140, 110, 85, 25 );
        idLabel.setHorizontalAlignment( JLabel.RIGHT );
        this.getContentPane().add( idLabel );
        _id = new JLabel( "" );
        _id.setBounds( 230, 110, 70, 25 );
        this.getContentPane().add( _id );
        _name = new SaneTextField();
        _name.setBounds( 100, 20, 210, 25 );
        _name.textWidthLimit( 30 );
        _name.setToolTipText( "Name assigned to the pass (up to 30 characters)." );
        this.getContentPane().add( _name );
        JLabel nameLabel = new JLabel( "Name:" );
        nameLabel.setBounds( 10, 20, 85, 25 );
        nameLabel.setHorizontalAlignment( JLabel.RIGHT );
        this.getContentPane().add( nameLabel );
        _type = new JLabel( "unknown" );
        _type.setBounds( 100, 80, 210, 25 );
        _type.setToolTipText( "Current status of this experiment." );
        this.getContentPane().add( _type );
        _type.setVisible( true );
        JLabel statusLabel = new JLabel( "Status:" );
        statusLabel.setBounds( 10, 80, 85, 25 );
        statusLabel.setHorizontalAlignment( JLabel.RIGHT );
        this.getContentPane().add( statusLabel );
        _typeList = new JComboBox();
        _typeList.setBounds( 100, 80, 210, 25 );
        _typeList.setToolTipText( "List of possible status settings for this experiment." );
        _typeList.setBackground( Color.WHITE );
        _typeList.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _type.setText( (String)_typeList.getSelectedItem() );
            }
        });
        this.getContentPane().add( _typeList );
        _typeList.setVisible( false );
        _cancelButton = new JButton( "Cancel" );
        _cancelButton.setBounds( 100, 170, 100, 25 );
        _cancelButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                ok( false );
            }
        });
        this.getContentPane().add( _cancelButton );
        _okButton = new JButton( "Apply" );
        _okButton.setBounds( 210, 170, 100, 25 );
        _okButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                ok( true );
            }
        });
        this.getContentPane().add( _okButton );
         * 
         */
        
        _allObjectsBuilt = true;
    }        

    public void newSize() {
        int w = this.getWidth();
        int h = this.getHeight();
        if ( _menuBar != null )
            _menuBar.setBounds( 0, 0, w, 25 );
        if ( _allObjectsBuilt ) {
            _scrollPane.setBounds( 0, 25, w, h - 47 );
        }
    }

            
    public String name() { return _name.getText(); }
    public void name( String newVal ) { 
        _name.setText( newVal );
    }
    protected void ok( boolean newVal ) {
        _ok = newVal;
        this.setVisible( false );
    }
    public boolean ok() { return _ok; }
    public void visible() {
        _ok = false;
        this.setVisible( true );
    }
    public void id( Integer newVal ) { 
        if ( newVal == null )
            _id.setText( "" );
        else
            _id.setText( newVal.toString() );
    }
    public void type( String newVal ) { 
        _type.setText( newVal );
        for ( int i = 0; i < _typeList.getItemCount(); ++i ) {
            if ( ((String)_typeList.getItemAt( i )).contentEquals( newVal ) ) {
                _typeList.setSelectedIndex( i );
            }
        }
    }
    public String type() { return _type.getText(); }

    protected SaneTextField _name;
    protected boolean _ok;
    protected JCheckBox _inDataBase;
    protected JLabel _id;
    protected JButton _okButton;
    protected JButton _cancelButton;
    protected JLabel _type;
    protected JComboBox _typeList;
    protected SystemSettings _settings;

    protected VexFileEditor _this;
    protected JMenuBar _menuBar;
    protected NodeBrowserScrollPane _scrollPane;
    protected boolean _allObjectsBuilt;
}
