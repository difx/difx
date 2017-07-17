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
 * This class produces a modal pop-up window for adjusting the properties
 * specific to an experiment.  This window is modal.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.SaneTextField;
import mil.navy.usno.widgetlib.NumberBox;
import mil.navy.usno.widgetlib.SimpleTextEditor;
import mil.navy.usno.widgetlib.AePlayWave;
import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.JulianCalendar;
import mil.navy.usno.widgetlib.ComplexToolTip;
import mil.navy.usno.widgetlib.ZCheckBox;
import mil.navy.usno.widgetlib.ZButton;
import mil.navy.usno.widgetlib.Power2NumberBox;
import mil.navy.usno.widgetlib.PopupMonitor;

import edu.nrao.difx.difxutilities.DiFXCommand_getFile;
import edu.nrao.difx.difxutilities.DiFXCommand_sendFile;
import edu.nrao.difx.difxutilities.DiFXCommand_mkdir;
import edu.nrao.difx.difxutilities.DiFXCommand_vex2difx;
import edu.nrao.difx.difxutilities.DiFXCommand_ls;
import edu.nrao.difx.difxutilities.V2dFileParser;
import edu.nrao.difx.difxutilities.TabCompletedTextField;

import edu.nrao.difx.difxdatabase.QueueDBConnection;

import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.SwingConstants;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JFrame;
import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.JToolTip;

import java.awt.Frame;
import java.awt.Color;
import java.awt.Point;
import java.awt.Component;
import java.awt.Cursor;

import java.util.Map;
import java.util.Iterator;
import java.util.Calendar;
import java.util.ArrayList;
import java.util.Vector;
import java.util.Date;
import java.util.HashMap;
import java.util.ArrayDeque;
import java.util.GregorianCalendar;

import java.text.SimpleDateFormat;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseEvent;

import java.net.URL;
import java.net.MalformedURLException;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

import java.net.UnknownHostException;
import java.util.Locale;
import mil.navy.usno.widgetlib.NodeBrowserScrollPane;
import mil.navy.usno.widgetlib.IndexedPanel;
import mil.navy.usno.widgetlib.ButtonGrid;

import javax.swing.event.EventListenerList;

import java.awt.event.ComponentEvent;

import java.sql.ResultSet;

public class ExperimentEditor extends JFrame {
        
    public ExperimentEditor( int x, int y, SystemSettings settings ) {
        _settings = settings;
        _settings.setLookAndFeel();
        this.setLayout( null );
        this.setBounds( x, y, _settings.windowConfiguration().experimentEditorW,
                _settings.windowConfiguration().experimentEditorH );
        _this = this;
        this.getContentPane().setLayout( null );
    	this.addComponentListener( new java.awt.event.ComponentAdapter() {
            public void componentResized( ComponentEvent e ) {
                _settings.windowConfiguration().experimentEditorW = _this.getWidth();
                _settings.windowConfiguration().experimentEditorH = _this.getHeight();
                newSize();
            }
        });
        _menuBar = new JMenuBar();
        JMenu helpMenu = new JMenu( "  Help  " );
        _menuBar.add( helpMenu );
        JMenuItem settingsHelpItem = new JMenuItem( "Experiment Editor Help" );
        settingsHelpItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.launchGUIHelp( "experimentEditor.html" );
            }
        } );
        helpMenu.add( settingsHelpItem );
        JMenuItem helpIndexItem = new JMenuItem( "GUI Documentation" );
        helpIndexItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.launchGUIHelp( "intro.html" );
            }
        } );
        helpMenu.add( helpIndexItem );
        this.getContentPane().add( _menuBar );
        _scrollPane = new NodeBrowserScrollPane();
        _scrollPane.respondToResizeEvents( true );
        _scrollPane.addTimeoutEventListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _this.newSize();
            }
        } );
        _scrollPane.addResizeEventListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _this.newSize();
            }
        } );
        this.getContentPane().add( _scrollPane );
        
        //  The "namePanel" holds all of the stuff that ALWAYS is shown.
        IndexedPanel namePanel = new IndexedPanel( "" );
        namePanel.openHeight( 210 );
        namePanel.alwaysOpen( true );
        namePanel.noArrow( true );
        _scrollPane.addNode( namePanel );
        _name = new SaneTextField();
        _name.setBounds( 100, 20, 210, 25 );
        _name.textWidthLimit( 20 );
        _name.setToolTipText( "Name assigned to the experiment (up to 20 characters)." );
        _name.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                nameChangeAction();
            }
        });
        namePanel.add( _name );
        _name.setVisible( false );
        _nameAsLabel = new JLabel( "" );
        _nameAsLabel.setBounds( 100, 20, 210, 25 );
        _nameAsLabel.setToolTipText( "Name assigned to the experiment." );
        namePanel.add( _nameAsLabel );
        JLabel nameLabel = new JLabel( "Name:" );
        nameLabel.setBounds( 10, 20, 85, 25 );
        nameLabel.setHorizontalAlignment( JLabel.RIGHT );
        namePanel.add( nameLabel );
        //  We aren't using the number right now - it was originally put here because it
        //  is in the database, but it isn't clear what, if anything, anyone needs it for.
        _number = new NumberBox();
        _number.setBounds( 100, 50, 80, 25 );
        _number.limits( 0.0, 9999.0 );
        _number.setToolTipText( "Number (up to four digits) used to associate experiments with the same name." );
//        namePanel.add( _number );
        _number.setVisible( false );
        _numberAsLabel = new JLabel( "" );
        _numberAsLabel.setBounds( 100, 50, 80, 25 );
        _numberAsLabel.setToolTipText( "Number used to associate experiments with the same name." );
//        namePanel.add( _numberAsLabel );
        _numberAsLabel.setVisible( false );
//        JLabel numberLabel = new JLabel( "Number:" );
//        numberLabel.setBounds( 10, 50, 85, 25 );
//        numberLabel.setHorizontalAlignment( JLabel.RIGHT );
//        namePanel.add( numberLabel );
        _status = new JLabel( "unknown" );
        _status.setBounds( 100, 50, 210, 25 );
        _status.setToolTipText( "Current status of this experiment." );
        namePanel.add( _status );
        _status.setVisible( false );
        JLabel statusLabel = new JLabel( "Status:" );
        statusLabel.setBounds( 10, 50, 85, 25 );
        statusLabel.setHorizontalAlignment( JLabel.RIGHT );
        namePanel.add( statusLabel );
        _statusList = new JComboBox<String>();
        _statusList.setBounds( 100, 50, 210, 25 );
        _statusList.setToolTipText( "List of possible status settings for this experiment." );
        _statusList.setBackground( Color.WHITE );
        _statusList.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _status.setText( (String)_statusList.getSelectedItem() );
            }
        });
        namePanel.add( _statusList );
        _inDataBase = new JCheckBox( "" );
        _inDataBase.setBounds( 100, 80, 25, 25 );
        _inDataBase.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                inDataBaseAction();
            }
        });
        namePanel.add( _inDataBase );
        JLabel inDataBaseLabel = new JLabel( "In Database:" );
        inDataBaseLabel.setBounds( 10, 80, 85, 25 );
        inDataBaseLabel.setHorizontalAlignment( JLabel.RIGHT );
        namePanel.add( inDataBaseLabel );
        JLabel idLabel = new JLabel( "Database ID:" );
        idLabel.setBounds( 140, 80, 85, 25 );
        idLabel.setHorizontalAlignment( JLabel.RIGHT );
        namePanel.add( idLabel );
        _id = new JLabel( "" );
        _id.setBounds( 230, 80, 70, 25 );
        namePanel.add( _id );
        _created = new JLabel( "" );
        _created.setBounds( 100, 110, 210, 25 );
        _created.setToolTipText( "Date this experiment was created (assigned by database if available)." );
        namePanel.add( _created );
        JLabel createdLabel = new JLabel( "Created:" );
        createdLabel.setBounds( 10, 110, 85, 25 );
        createdLabel.setHorizontalAlignment( JLabel.RIGHT );
        namePanel.add( createdLabel );
        _directory = new TabCompletedTextField( _settings );
        _directory.setBounds( 100, 140, 310, 25 );
        _directory.setToolTipText( "This is the directory that contains all files specific to this experiment.\n"
                + "Subdirectories associated with passes reside in this directory.\n"
                + "The <i>guiServer</i> process must be running such that it has permission to write in this\n"
                + "directory, and/or create it if necessary." );
        _directory.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                directoryChangeAction();
            }
        });
        namePanel.add( _directory );
        _directory.setVisible( false );
        _directoryAsLabel = new JLabel( "" );
        _directoryAsLabel.setBounds( 100, 140, 310, 25 );
        namePanel.add( _directoryAsLabel );
        JLabel directoryLabel = new JLabel( "Working Dir:" );
        directoryLabel.setBounds( 10, 140, 85, 25 );
        directoryLabel.setHorizontalAlignment( JLabel.RIGHT );
        namePanel.add( directoryLabel );
        _vexFileName = new SaneTextField();
        _vexFileName.setBounds( 100, 170, 205, 25 );
        _vexFileName.setToolTipText( "Name of the .vex file associated with this experiment." );
        namePanel.add( _vexFileName );
        _vexFileNameAsLabel = new JLabel( "" );
        _vexFileNameAsLabel.setBounds( 100, 170, 200, 25 );
        _vexFileNameAsLabel.setToolTipText( "Name of the .vex file associated with this experiment." );
        namePanel.add( _vexFileNameAsLabel );
        JLabel vexFileLabel = new JLabel( ".vex File:" );
        vexFileLabel.setBounds( 10, 170, 85, 25 );
        vexFileLabel.setHorizontalAlignment( JLabel.RIGHT );
        namePanel.add( vexFileLabel );
        _previousVexFileMenu = new JPopupMenu( "" );
        _previousVexFileButton = new JButton( "Previous Files" );
        _previousVexFileButton.setToolTipText( "Choose from the list of all .vex files associated with this Experiment." );
        _previousVexFileButton.setEnabled( false );
        _previousVexFileButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _previousVexFileMenu.show( _previousVexFileButton, 0, 0 );
            }
        });
        namePanel.add( _previousVexFileButton );
        
        //  This panel is used to grab the content of an existing .v2d file to use as
        //  a "template" for any setting changes in subsequent menus.  The choices
        //  for sources of these files mirror those used for the .vex file.
        IndexedPanel startingV2dPanel = new IndexedPanel( "Starting .v2d File" );
        startingV2dPanel.openHeight( 185 );
        startingV2dPanel.closedHeight( 20 );
        startingV2dPanel.open( false );
        //_scrollPane.addNode( startingV2dPanel );
        _v2dFromHost = new JCheckBox( "from DiFX Host" );
        _v2dFromHost.setSelected( _settings.defaultNames().v2dFromHost );
        _v2dFromHost.setToolTipText( "Copy the .v2d file data from a named file on the DiFX Host." );
        _v2dFromHost.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                v2dSourceChoice( _v2dFromHost );
            }
        });
        startingV2dPanel.add( _v2dFromHost );
        _v2dFromHostLocation = new TabCompletedTextField( _settings );
        _v2dFromHostLocation.setToolTipText( "Full path to the file on the DiFX Host." );
        if ( !_v2dFromHost.isSelected() )
            _v2dFromHostLocation.setEnabled( false );
        _v2dFromHostLocation.setText( _settings.defaultNames().v2dFileSource );
        startingV2dPanel.add( _v2dFromHostLocation );
        _v2dViaHttp = new JCheckBox( "via HTTP" );
        _v2dViaHttp.setSelected( _settings.defaultNames().v2dViaHttp );
        _v2dViaHttp.setToolTipText( "Copy the .v2d file data from a given location using HTTP." );
        _v2dViaHttp.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                v2dSourceChoice( _v2dViaHttp );
            }
        });
        startingV2dPanel.add( _v2dViaHttp );
        _v2dViaHttpLocation = new SaneTextField();
        _v2dViaHttpLocation.setToolTipText( "HTTP location of .v2d file." );
        if ( !_v2dViaHttp.isSelected() )
            _v2dViaHttpLocation.setEnabled( false );
        _v2dViaHttpLocation.setText( _settings.defaultNames().v2dViaHttpLocation );
        startingV2dPanel.add( _v2dViaHttpLocation );
        _v2dViaFtp = new JCheckBox( "via FTP" );
        _v2dViaFtp.setSelected( _settings.defaultNames().v2dViaFtp );
        _v2dViaFtp.setToolTipText( "Copy the .v2d file data from a given address via FTP." );
        _v2dViaFtp.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                v2dSourceChoice( _v2dViaFtp );
            }
        });
        startingV2dPanel.add( _v2dViaFtp );
        _v2dViaFtpLocation = new SaneTextField();
        _v2dViaFtpLocation.setToolTipText( "FTP address of .v2d file." );
        if ( !_v2dViaFtp.isSelected() )
            _v2dViaFtpLocation.setEnabled( false );
        _v2dViaFtpLocation.setText( _settings.defaultNames().v2dViaFtpLocation );
        startingV2dPanel.add( _v2dViaFtpLocation );
        _localV2dFile = new JCheckBox( "from Local File" );
        _localV2dFile.setSelected( _settings.defaultNames().v2dFromLocal );
        _localV2dFile.setToolTipText( "Copy the .v2d file data from a file on the local host." );
        _localV2dFile.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                v2dSourceChoice( _localV2dFile );
            }
        });
        startingV2dPanel.add( _localV2dFile );
        _localV2dFileLocation = new SaneTextField();
        _localV2dFileLocation.setToolTipText( "Location of the .v2d file on the local machine." );
        if ( !_localV2dFile.isSelected() )
            _localV2dFileLocation.setEnabled( false );
        _localV2dFileLocation.setText( _settings.defaultNames().localV2dFileLocation );
        startingV2dPanel.add( _localV2dFileLocation );
        _noV2dFile = new JCheckBox( "None" );
        _noV2dFile.setSelected( _settings.defaultNames().noV2dFile );
        _noV2dFile.setToolTipText( "Copy the .v2d file data from a file on the local host." );
        _noV2dFile.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                v2dSourceChoice( _noV2dFile );
            }
        });
        startingV2dPanel.add( _noV2dFile );
        _goV2dButton = new JButton( "GO!" );
        _goV2dButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                getV2dFromSource();
            }
        });
        startingV2dPanel.add( _goV2dButton );
        
        //  This panel is used to find a new vex file.
        IndexedPanel findVexPanel = new IndexedPanel( "Get .vex File Content" );
        findVexPanel.openHeight( 210 );
        findVexPanel.closedHeight( 20 );
        findVexPanel.open( false );
        _scrollPane.addNode( findVexPanel );
        _fromHost = new JCheckBox( "from DiFX Host" );
        _fromHost.setSelected( _settings.defaultNames().vexFromHost );
        _fromHost.setToolTipText( "Copy the .vex file data from a named file on the DiFX Host." );
        _fromHost.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                vexSourceChoice( _fromHost );
            }
        });
        findVexPanel.add( _fromHost );
        _fromHostLocation = new TabCompletedTextField( _settings );
        _fromHostLocation.setToolTipText( "Full path to the file on the DiFX Host." );
        if ( !_fromHost.isSelected() )
            _fromHostLocation.setEnabled( false );
        _fromHostLocation.setText( _settings.defaultNames().vexFileSource );
        findVexPanel.add( _fromHostLocation );
        _viaHttp = new JCheckBox( "via HTTP" );
        _viaHttp.setSelected( _settings.defaultNames().vexViaHttp );
        _viaHttp.setToolTipText( "Copy the .vex file data from a given location using HTTP." );
        _viaHttp.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                vexSourceChoice( _viaHttp );
            }
        });
        findVexPanel.add( _viaHttp );
        _viaHttpLocation = new SaneTextField();
        _viaHttpLocation.setToolTipText( "HTTP location of .vex file." );
        if ( !_viaHttp.isSelected() )
            _viaHttpLocation.setEnabled( false );
        _viaHttpLocation.setText( _settings.defaultNames().viaHttpLocation );
        findVexPanel.add( _viaHttpLocation );
        _viaFtp = new JCheckBox( "via FTP" );
        _viaFtp.setSelected( _settings.defaultNames().vexViaFtp );
        _viaFtp.setToolTipText( "Copy the .vex file data from a given address via FTP." );
        _viaFtp.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                vexSourceChoice( _viaFtp );
            }
        });
        findVexPanel.add( _viaFtp );
        _viaFtpLocation = new SaneTextField();
        _viaFtpLocation.setToolTipText( "FTP address of .vex file." );
        if ( !_viaFtp.isSelected() )
            _viaFtpLocation.setEnabled( false );
        _viaFtpLocation.setText( _settings.defaultNames().viaFtpLocation );
        findVexPanel.add( _viaFtpLocation );
        _localFile = new JCheckBox( "from Local File" );
        _localFile.setSelected( _settings.defaultNames().vexFromLocal );
        _localFile.setToolTipText( "Copy the .vex file data from a file on the local host." );
        _localFile.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                vexSourceChoice( _localFile );
            }
        });
        findVexPanel.add( _localFile );
        _localFileLocation = new SaneTextField();
        _localFileLocation.setToolTipText( "Location of the .vex file on the local machine." );
        if ( !_localFile.isSelected() )
            _localFileLocation.setEnabled( false );
        _localFileLocation.setText( _settings.defaultNames().localFileLocation );
        findVexPanel.add( _localFileLocation );
        _fromExperiment = new JCheckBox( "from Experiment" );
        _fromExperiment.setSelected( _settings.defaultNames().vexFromExperiment );
        _fromExperiment.setToolTipText( "Copy the .vex file from another experiment." );
        _fromExperiment.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                vexSourceChoice( _fromExperiment );
            }
        });
        findVexPanel.add( _fromExperiment );
        _vexBrowseButton = new JButton( "Browse" );
        _vexBrowseButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                getVexFromSource();
            }
        });
        _goButton = new JButton( "GO!" );
        findVexPanel.add( _vexBrowseButton );
        _goButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _statusLabel.setText( "Processing .vex data..." );
                _statusLabel.updateUI();
                getVexFromSource();
                _statusLabel.setText( "Ready!" );
                _statusLabel.updateUI();
            }
        });
        findVexPanel.add( _goButton );
        
        //  This panel contains a text editor so the .vex file can be edited by
        //  hand.
        IndexedPanel editorPanel = new IndexedPanel( ".vex File Editor" );
        editorPanel.openHeight( 500 );
        editorPanel.closedHeight( 20 );
        editorPanel.open( false );
        _scrollPane.addNode( editorPanel );
        _editor = new SimpleTextEditor();
        editorPanel.add( _editor );
        _parseEditorContent = new ZButton( "Parse Content" );
        _parseEditorContent.setToolTipText( "Use the editor content in the .vex file parser for this experiment.\n"
                + "The changed .vex data will become the new .vex file for the experiment\n"
                + "(the source of the original .vex data will not be changed)." );
        _parseEditorContent.setBounds( 20, 30, 140, 25 );
        _parseEditorContent.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                parseNewVexFile( true );
            }
        });
        editorPanel.add( _parseEditorContent );
        
        //  This panel contains a few often-changed parameters that govern the
        //  correlation.  These (mostly) end up in the "setup" section of the v2d file.
        IndexedPanel correlationPanel = new IndexedPanel( "Correlation Tuning Parameters" );
        correlationPanel.openHeight( 155 );
        correlationPanel.closedHeight( 20 );
        correlationPanel.open( false );
        _scrollPane.addNode( correlationPanel );
        _tInt = new NumberBox();
        _tInt.setToolTipText( "Integration Time in seconds." );
        _tInt.setBounds( 205, 30, 100, 25 );
        _tInt.limits( 0.0, 100.0 );
        _tInt.precision( 1 );
        _tInt.value( _settings.defaultNames().correlationTInt );
        _tInt.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().correlationTInt = _tInt.value();
                produceV2dFile();
            }
        });
        correlationPanel.add( _tInt );
        _tIntApply = new ZCheckBox( "" );
        _tIntApply.setBounds( 180, 32, 20, 20 );
        _tIntApply.setSelected( _settings.defaultNames().applyTInt );
        _tIntApply.setToolTipText( "Apply the current value of Integration Time to\n"
                + "the correlation." );
        _tIntApply.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().applyTInt = _tIntApply.isSelected();
                if ( _tIntApply.isSelected() )
                    _tInt.setBackground( Color.GREEN );
                else
                    _tInt.setBackground( Color.WHITE );
                produceV2dFile();
            }
        });
        if ( _tIntApply.isSelected() )
            _tInt.setBackground( Color.GREEN );
        else
            _tInt.setBackground( Color.WHITE );
        correlationPanel.add( _tIntApply );
        JLabel tIntLabel = new JLabel( "Integration Time (sec):" );
        tIntLabel.setBounds( 20, 30, 155, 25 );
        tIntLabel.setHorizontalAlignment( JLabel.RIGHT );
        correlationPanel.add( tIntLabel );
        _doPolar = new ZCheckBox( "Do Polar" );
        _doPolar.setBounds( 700, 30, 100, 25 );
        _doPolar.setToolTipText( "Correlate cross hands when possible." );
        _doPolar.setSelected( _settings.defaultNames().correlationDoPolar );
        _doPolar.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().correlationDoPolar = _doPolar.isSelected();
                produceV2dFile();
            }
        });
        correlationPanel.add( _doPolar );
        _doPolarApply = new ZCheckBox( "" );
        _doPolarApply.setBounds( 675, 32, 20, 20 );
        _doPolarApply.setSelected( _settings.defaultNames().applyDoPolar );
        _doPolarApply.setToolTipText( "Apply the \"Do Polar\" setting to\n"
                + "the correlation." );
        _doPolarApply.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().applyDoPolar = _doPolarApply.isSelected();
                if ( _doPolarApply.isSelected() )
                    _doPolar.setBackground( Color.GREEN );
                else
                    _doPolar.setBackground( _doPolarApply.getBackground() );
                produceV2dFile();
            }
        });
        if ( _doPolarApply.isSelected() )
            _doPolar.setBackground( Color.GREEN );
        else
            _doPolar.setBackground( _doPolarApply.getBackground() );
        correlationPanel.add( _doPolarApply );
        _specRes = new NumberBox();
        _specRes.setBounds( 205, 60, 100, 25 );
        _specRes.precision( 5 );
        _specRes.value( _settings.defaultNames().correlationSpecRes );
        _specRes.setToolTipText( "Spectral resolution of visibilities produced." );
        _specRes.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().correlationSpecRes = _specRes.value();
                produceV2dFile();
            }
        });
        correlationPanel.add( _specRes );
        _specResApply = new ZCheckBox( "" );
        _specResApply.setBounds( 180, 62, 20, 20 );
        _specResApply.setSelected( _settings.defaultNames().applySpecRes );
        _specResApply.setToolTipText( "Apply the current value of Spectral Resolution to\n"
                + "the correlation." );
        _specResApply.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().applySpecRes = _specResApply.isSelected();
                if ( _specResApply.isSelected() )
                    _specRes.setBackground( Color.GREEN );
                else
                    _specRes.setBackground( Color.WHITE );
                produceV2dFile();
            }
        });
        if ( _specResApply.isSelected() )
            _specRes.setBackground( Color.GREEN );
        else
            _specRes.setBackground( Color.WHITE );
        correlationPanel.add( _specResApply );
        JLabel specResLabel = new JLabel( "Spectral Resolution:" );
        specResLabel.setHorizontalAlignment( JLabel.RIGHT );
        specResLabel.setBounds( 20, 60, 155, 25 );
        correlationPanel.add( specResLabel );
        _nChan = new Power2NumberBox();
        _nChan.setBounds( 375, 60, 100, 25 );
        _nChan.setToolTipText( "Number of channels to divide the current bandwidth into.\n"
                + "This number can be used to set the Spectral Resolution (but\n"
                + "the reverse is not true - changing the Spectral Resolution\n"
                + "will not change this number!)." );
        _nChan.value( _settings.defaultNames().correlationNChan );
        _nChan.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().correlationNChan = _nChan.intValue();
                _specRes.value( _bandwidth / _nChan.value() );
                _settings.defaultNames().correlationSpecRes = _specRes.value();
                produceV2dFile();
            }
        });
        correlationPanel.add( _nChan );
        JLabel nChanLabel = new JLabel( "Channels:" );
        nChanLabel.setBounds( 305, 60, 65, 25 );
        nChanLabel.setHorizontalAlignment( JLabel.RIGHT );
        correlationPanel.add( nChanLabel );
        _fftSpecRes = new NumberBox();
        _fftSpecRes.setBounds( 205, 90, 100, 25 );
        _fftSpecRes.precision( 5 );
        _fftSpecRes.value( _settings.defaultNames().correlationFFTSpecRes );
        _fftSpecRes.setToolTipText( "Spectral resolution of first stage FFTs." );
        _fftSpecRes.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().correlationFFTSpecRes = _fftSpecRes.value();
                produceV2dFile();
            }
        });
        correlationPanel.add( _fftSpecRes );
        JLabel fftSpecResLabel = new JLabel( "FFT Spectral Resolution:" );
        fftSpecResLabel.setHorizontalAlignment( JLabel.RIGHT );
        fftSpecResLabel.setBounds( 20, 90, 155, 25 );
        correlationPanel.add( fftSpecResLabel );
        _fftSpecResApply = new ZCheckBox( "" );
        _fftSpecResApply.setBounds( 180, 92, 20, 20 );
        _fftSpecResApply.setSelected( _settings.defaultNames().applyFFTSpecRes );
        _fftSpecResApply.setToolTipText( "Apply the current value of FFT Spectral Resolution to\n"
                + "the correlation." );
        _fftSpecResApply.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().applyFFTSpecRes = _fftSpecResApply.isSelected();
                if ( _fftSpecResApply.isSelected() )
                    _fftSpecRes.setBackground( Color.GREEN );
                else
                    _fftSpecRes.setBackground( Color.WHITE );
                produceV2dFile();
            }
        });
        if ( _fftSpecResApply.isSelected() )
            _fftSpecRes.setBackground( Color.GREEN );
        else
            _fftSpecRes.setBackground( Color.WHITE );
        correlationPanel.add( _fftSpecResApply );
        _fftNChan = new Power2NumberBox();
        _fftNChan.setToolTipText( "Number of FFT channels to divide the current bandwidth into.\n"
                + "This number can be used to set the FFT Spectral Resolution (but\n"
                + "the reverse is not true - changing the FFT Spectral Resolution\n"
                + "will not change this number!)." );
        _fftNChan.setBounds( 375, 90, 100, 25 );
        _fftNChan.value( _settings.defaultNames().correlationNFFTChan );
        _fftNChan.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().correlationNFFTChan = _fftNChan.intValue();
                _fftSpecRes.value( _bandwidth / _fftNChan.value() );
                _settings.defaultNames().correlationFFTSpecRes = _fftSpecRes.value();
                produceV2dFile();
            }
        });
        correlationPanel.add( _fftNChan );
        JLabel fftNChanLabel = new JLabel( "Channels:" );
        fftNChanLabel.setBounds( 305, 90, 65, 25 );
        fftNChanLabel.setHorizontalAlignment( JLabel.RIGHT );
        correlationPanel.add( fftNChanLabel );
        _subintNS = new NumberBox();
        _subintNS.setBounds( 205, 120, 100, 25 );
        _subintNS.precision( 0 );
        _subintNS.intValue( _settings.defaultNames().correlationSubintNS );
        _subintNS.setToolTipText( "The sub integration time (in nanoseconds)" );
        _subintNS.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().correlationSubintNS = _subintNS.intValue();
                produceV2dFile();
            }
        });
        correlationPanel.add( _subintNS );
        JLabel subintNSLabel = new JLabel( "Subintegration (nsec):" );
        subintNSLabel.setHorizontalAlignment( JLabel.RIGHT );
        subintNSLabel.setBounds( 20, 120, 155, 25 );
        correlationPanel.add( subintNSLabel );
        _subintNSApply = new ZCheckBox( "" );
        _subintNSApply.setBounds( 180, 122, 20, 20 );
        _subintNSApply.setSelected( _settings.defaultNames().applySubIntNS );
        _subintNSApply.setToolTipText( "Apply the current value of Subintegration Time to\n"
                + "the correlation." );
        _subintNSApply.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().applySubIntNS = _subintNSApply.isSelected();
                if ( _subintNSApply.isSelected() )
                    _subintNS.setBackground( Color.GREEN );
                else
                    _subintNS.setBackground( Color.WHITE );
                produceV2dFile();
            }
        });
        if ( _subintNSApply.isSelected() )
            _subintNS.setBackground( Color.GREEN );
        else
            _subintNS.setBackground( Color.WHITE );
        correlationPanel.add( _subintNSApply );
        _strideLength = new NumberBox();
        _strideLength.setBounds( 700, 60, 100, 25 );
        _strideLength.precision( 0 );
        _strideLength.minimum( 1.0 );
        _strideLength.intValue( _settings.defaultNames().strideLength );
        _strideLength.setToolTipText( "The number of channels to “stride” for fringe rotation, fractional\n"
                + "sample correction etc." );
        _strideLength.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().strideLength = _strideLength.intValue();
                produceV2dFile();
            }
        });
        correlationPanel.add( _strideLength );
        JLabel strideLengthLabel = new JLabel( "Stride Length:" );
        strideLengthLabel.setHorizontalAlignment( JLabel.RIGHT );
        strideLengthLabel.setBounds( 515, 60, 155, 25 );
        correlationPanel.add( strideLengthLabel );
        _strideLengthApply = new ZCheckBox( "" );
        _strideLengthApply.setBounds( 675, 62, 20, 20 );
        _strideLengthApply.setSelected( _settings.defaultNames().applyStrideLength );
        _strideLengthApply.setToolTipText( "Apply the Stride Length setting to\n"
                + "the correlation." );
        _strideLengthApply.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().applyStrideLength = _strideLengthApply.isSelected();
                if ( _strideLengthApply.isSelected() )
                    _strideLength.setBackground( Color.GREEN );
                else
                    _strideLength.setBackground( Color.WHITE );
                produceV2dFile();
            }
        });
        if ( _strideLengthApply.isSelected() )
            _strideLength.setBackground( Color.GREEN );
        else
            _strideLength.setBackground( Color.WHITE );
        correlationPanel.add( _strideLengthApply );
        _xmacLength = new NumberBox();
        _xmacLength.setBounds( 700, 90, 100, 25 );
        _xmacLength.minimum( 1.0 );
        _xmacLength.precision( 0 );
        _xmacLength.intValue( _settings.defaultNames().xmacLength );
        _xmacLength.setToolTipText( "The number of channels to \"stride\" for cross-multiply accumulations." );
        _xmacLength.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().xmacLength = _xmacLength.intValue();
                produceV2dFile();
            }
        });
        correlationPanel.add( _xmacLength );
        JLabel xmacLengthLabel = new JLabel( "XMAC Length:" );
        xmacLengthLabel.setHorizontalAlignment( JLabel.RIGHT );
        xmacLengthLabel.setBounds( 515, 90, 155, 25 );
        correlationPanel.add( xmacLengthLabel );
        _xmacLengthApply = new ZCheckBox( "" );
        _xmacLengthApply.setBounds( 675, 92, 20, 20 );
        _xmacLengthApply.setSelected( _settings.defaultNames().applyXmacLength );
        _xmacLengthApply.setToolTipText( "Apply the XMAC Length setting to\n"
                + "the correlation." );
        _xmacLengthApply.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().applyXmacLength = _xmacLengthApply.isSelected();
                if ( _xmacLengthApply.isSelected() )
                    _xmacLength.setBackground( Color.GREEN );
                else
                    _xmacLength.setBackground( Color.WHITE );
                produceV2dFile();
            }
        });
        if ( _xmacLengthApply.isSelected() )
            _xmacLength.setBackground( Color.GREEN );
        else
            _xmacLength.setBackground( Color.WHITE );
        correlationPanel.add( _xmacLengthApply );
        _bufferedFFTs = new NumberBox();
        _bufferedFFTs.setBounds( 700, 120, 100, 25 );
        _bufferedFFTs.precision( 0 );
        _bufferedFFTs.minimum( 1.0 );
        _bufferedFFTs.intValue( _settings.defaultNames().numBufferedFFTs );
        _bufferedFFTs.setToolTipText( "The number of channels to “stride” for fringe rotation, fractional\n"
                + "sample correction etc." );
        _bufferedFFTs.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().numBufferedFFTs = _bufferedFFTs.intValue();
                produceV2dFile();
            }
        });
        correlationPanel.add( _bufferedFFTs );
        JLabel bufferedFFTsLabel = new JLabel( "# Buffered FFTs:" );
        bufferedFFTsLabel.setHorizontalAlignment( JLabel.RIGHT );
        bufferedFFTsLabel.setBounds( 515, 120, 155, 25 );
        correlationPanel.add( bufferedFFTsLabel );
        _bufferedFFTsApply = new ZCheckBox( "" );
        _bufferedFFTsApply.setBounds( 675, 122, 20, 20 );
        _bufferedFFTsApply.setSelected( _settings.defaultNames().applyNumBufferedFFTs );
        _bufferedFFTsApply.setToolTipText( "Apply the Number of Buffered FFTs setting to\n"
                + "the correlation." );
        _bufferedFFTsApply.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().applyNumBufferedFFTs = _bufferedFFTsApply.isSelected();
                if ( _bufferedFFTsApply.isSelected() )
                    _bufferedFFTs.setBackground( Color.GREEN );
                else
                    _bufferedFFTs.setBackground( Color.WHITE );
                produceV2dFile();
            }
        });
        if ( _bufferedFFTsApply.isSelected() )
            _bufferedFFTs.setBackground( Color.GREEN );
        else
            _bufferedFFTs.setBackground( Color.WHITE );
        correlationPanel.add( _bufferedFFTsApply );

        
        //  This panel is used to display and adjust antenna information.
        IndexedPanel antennaPanel = new IndexedPanel( "Stations" );
        antennaPanel.openHeight( 220 );
        antennaPanel.closedHeight( 20 );
        antennaPanel.open( false );
        antennaPanel.resizeOnTopBar( true );
        _scrollPane.addNode( antennaPanel );
        _antennaPane = new NodeBrowserScrollPane( false );
        _antennaPane.drawFrame( false );
        _antennaPane.setLevel( 1 );
        _antennaPane.respondToResizeEvents( true );
        antennaPanel.addScrollPane( _antennaPane );
        
        //  This panel allows the user to select time specific stations for each
        //  scan.
        _scanStationTimeline = new ScanStationTimeline( "Scan/Station Timeline" );
        _scanStationTimeline.open( false );
        _scanStationTimeline.closedHeight( 20 );
        _scanStationTimeline.drawFrame( false );
        _scanStationTimeline.resizeOnTopBar( true );
        _scanStationTimeline.addVexChangeListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                vexDataChange();
            }
        });
        _scrollPane.addNode( _scanStationTimeline );
        
        //  This panel contains the list of sources.
        IndexedPanel sourcePanel = new IndexedPanel( "Sources" );
        sourcePanel.open( false );
        sourcePanel.closedHeight( 20 );
        sourcePanel.resizeOnTopBar( true );
        _scrollPane.addNode( sourcePanel );
        _sourcePane = new NodeBrowserScrollPane( false );
        _sourcePane.drawFrame( false );
        _sourcePane.setLevel( 1 );
        _sourcePane.respondToResizeEvents( true );
        sourcePanel.addScrollPane( _sourcePane );
        
        //  This panel is used to select scans, which turn into jobs.
        IndexedPanel scanPanel = new IndexedPanel( "Scan Selection" );
        scanPanel.openHeight( 400 );
        scanPanel.closedHeight( 20 );
        scanPanel.open( false );
        _scrollPane.addNode( scanPanel );
        _scanGrid = new ButtonGrid();
        _scanGrid.addChangeListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  Find out which button in the grid was pushed.
                ButtonGrid.GridButton lastButton = _scanGrid.lastEventButton();
                //  Hunt through the vex data for the scan that matches this button.
                //  Turn everything in the scan on or off based on the new button setting.
                if ( _vexData != null ) {
                    VexFileParser.Scan scan = (VexFileParser.Scan)lastButton.data();
                    scan.omitFlag = !lastButton.on();
                    for ( Iterator<VexFileParser.ScanStation> iter2 = scan.station.iterator(); iter2.hasNext(); ) {
                        iter2.next().omitFlag = !lastButton.on();
                    }
                }
                vexDataChange();
            }
        });
        scanPanel.add( _scanGrid );
        _selectAllScansButton = new JButton( "Select All" );
        _selectAllScansButton.setBounds( 10, 40, 110, 25 );
        _selectAllScansButton.setToolTipText( "Select all scans for which all restrictions apply." );
        _selectAllScansButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _timeLimits.maxLimits();
                //  Look at all scans in the scan grid, find them in the .vex data, and
                //  turn them on.
                for ( Iterator<ButtonGrid.GridButton> iter = _scanGrid.buttonList().iterator(); iter.hasNext(); ) {
                    ButtonGrid.GridButton button = iter.next();
                    VexFileParser.Scan scan = (VexFileParser.Scan)button.data();
                    scan.omitFlag = false;
                    for ( Iterator<VexFileParser.ScanStation> iter2 = scan.station.iterator(); iter2.hasNext(); ) {
                        iter2.next().omitFlag = false;
                    }
                }
                vexDataChange();
            }
        });
        scanPanel.add( _selectAllScansButton );
        _selectNoScansButton = new JButton( "Deselect All" );
        _selectNoScansButton.setBounds( 125, 40, 110, 25 );
        _selectNoScansButton.setToolTipText( "De-select all scans." );
        _selectNoScansButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  Look at all scans in the scan grid, find them in the .vex data, and
                //  turn them off.
                for ( Iterator<ButtonGrid.GridButton> iter = _scanGrid.buttonList().iterator(); iter.hasNext(); ) {
                    ButtonGrid.GridButton button = iter.next();
                    VexFileParser.Scan scan = (VexFileParser.Scan)button.data();
                    scan.omitFlag = true;
                }
                vexDataChange();
            }
        });
        scanPanel.add( _selectNoScansButton );
        _timeLimits = new TimeLimitPanel();
        scanPanel.add( _timeLimits );
        JLabel timeLimitsLabel = new JLabel( "Time Limits:" );
        timeLimitsLabel.setBounds( 180, 40, 145, 25 );
        timeLimitsLabel.setHorizontalAlignment( JLabel.RIGHT );
        scanPanel.add( timeLimitsLabel );
        
        //  This panel contains the list of sources.
        IndexedPanel eopPanel = new IndexedPanel( "EOP Data" );
        eopPanel.open( false );
        eopPanel.closedHeight( 20 );
        eopPanel.resizeOnTopBar( true );
        _scrollPane.addNode( eopPanel );
        _eopPane = new NodeBrowserScrollPane( false );
        _eopPane.drawFrame( false );
        _eopPane.setLevel( 1 );
        _eopPane.respondToResizeEvents( true );
        eopPanel.addScrollPane( _eopPane );
        
        //  This panel is used to determine file names and related matters.
        IndexedPanel namesPanel = new IndexedPanel( "Names, Etc." );
        namesPanel.openHeight( 100 );
        namesPanel.closedHeight( 20 );
        namesPanel.open( false );
        _scrollPane.addNode( namesPanel );
        _inputFileBaseName = new SaneTextField();
        _inputFileBaseName.setBounds( 170, 30, 200, 25 );
        _inputFileBaseName.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                produceV2dFile();
            }
        });
        _inputFileBaseName.setToolTipText( "Base name for input files created by vex2difx on the DiFX host (leave blank for default)." );
        namesPanel.add( _inputFileBaseName );
        JLabel inputFileLabel = new JLabel( "Input File Base Name:" );
        inputFileLabel.setBounds( 10, 30, 155, 25 );
        inputFileLabel.setHorizontalAlignment( JLabel.RIGHT );
        namesPanel.add( inputFileLabel );
        _inputFileSequenceStart = new NumberBox();
        _inputFileSequenceStart.setBounds( 170, 60, 100, 25 );
        _inputFileSequenceStart.minimum( 1 );
        _inputFileSequenceStart.maximum( 900 );
        _inputFileSequenceStart.intValue( 1 );
        _inputFileSequenceStart.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                produceV2dFile();
            }
        });
        namesPanel.add( _inputFileSequenceStart );
        _inputFileSequenceStart.setToolTipText( "Start of numbers attached to the base name to create unique input file names." );
        JLabel sequenceStartLabel = new JLabel( "Sequence Start:" );
        sequenceStartLabel.setBounds( 10, 60, 155, 25 );
        sequenceStartLabel.setHorizontalAlignment( JLabel.RIGHT );
        namesPanel.add( sequenceStartLabel );
//        _inputJobNames = new JCheckBox( "Based on Input Files" );
//        _inputJobNames.setBounds( 170, 90, 325, 25 );
//        _inputJobNames.addActionListener( new ActionListener() {
//            public void actionPerformed(  ActionEvent e ) {
//                _inputJobNames.setSelected( true );
//                _scanJobNames.setSelected( false );
//            }
//        });
//        namesPanel.add( _inputJobNames );
//        _scanJobNames = new JCheckBox( "Use Scan Name" );
//        _scanJobNames.setBounds( 170, 120, 325, 25 );
//        _scanJobNames.addActionListener( new ActionListener() {
//            public void actionPerformed(  ActionEvent e ) {
//                _inputJobNames.setSelected( false );
//                _scanJobNames.setSelected( true );
//            }
//        });
//        namesPanel.add( _scanJobNames );
//        if ( _settings.defaultNames().scanBasedJobNames ) {
//            _scanJobNames.setSelected( true );
//            _inputJobNames.setSelected( false );
//        }
//        else {
//            _scanJobNames.setSelected( false );
//            _inputJobNames.setSelected( true );
//        }
//        JLabel jobNameLabel = new JLabel( "Job Names:" );
//        jobNameLabel.setBounds( 10, 90, 155, 25 );
//        jobNameLabel.setHorizontalAlignment( JLabel.RIGHT );
//        namesPanel.add( jobNameLabel );
        
        //  The v2d editor allows the .v2d file to be edited by hand.  At the
        //  moment it is visible, but I plan to make it not so!
        IndexedPanel v2dEditorPanel = new IndexedPanel( "Final .v2d File" );
        v2dEditorPanel.openHeight( 500 );
        v2dEditorPanel.closedHeight( 20 );
        v2dEditorPanel.open( false );
        _scrollPane.addNode( v2dEditorPanel );
        _v2dEditor = new SimpleTextEditor();
        v2dEditorPanel.add( _v2dEditor );
        _v2dFileName = new SaneTextField();
        v2dEditorPanel.add( _v2dFileName );
        JLabel v2dFileNameLabel = new JLabel( "File Name:" );
        v2dFileNameLabel.setBounds( 10, 30, 85, 25 );
        v2dFileNameLabel.setHorizontalAlignment( JLabel.RIGHT );
        v2dEditorPanel.add( v2dFileNameLabel );
        
        //  The "button" panel contains buttons and other items that always appear
        //  at the bottom of the frame.  It can not be closed.
        IndexedPanel buttonPanel = new IndexedPanel( "" );
        buttonPanel.openHeight( 180 );
        buttonPanel.alwaysOpen( true );
        buttonPanel.noArrow( true );
        _scrollPane.addNode( buttonPanel );
        _passLabel = new JLabel( "Create Pass:" );
        _passLabel.setBounds( 10, 20, 85, 25 );
        _passLabel.setHorizontalAlignment( JLabel.RIGHT );
        buttonPanel.add( _passLabel );
        _createPass = new JCheckBox( "" );
        _createPass.setBounds( 100, 20, 25, 25 );
        _createPass.setToolTipText( "Create a pass for this experiment." );
        _createPass.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                createPassSetting();
            }
        });
        buttonPanel.add( _createPass );
        _passName = new SaneTextField();
        _passName.setBounds( 285, 20, 280, 25 );
        _passName.setToolTipText( "Name of the new pass." );
        _passName.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                autoSetPassDirectory();
            }
        });
        buttonPanel.add( _passName );
        _passDirectory = new SaneTextField();
        _passDirectory.setBounds( 285, 50, 280, 25 );
        _passDirectory.setToolTipText( "Directory for all input files and data associated with the new pass." );
        _passDirectory.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                keepPassDirectory( true );
            }
        });
        buttonPanel.add( _passDirectory );
        JLabel passDirectoryLabel = new JLabel( "Pass Sub-Directory:" );
        passDirectoryLabel.setBounds( 10, 50, 270, 25 );
        passDirectoryLabel.setHorizontalAlignment( JLabel.RIGHT );
        buttonPanel.add( passDirectoryLabel );
//        _stagingArea = new TabCompletedTextField( _settings );
//        _stagingArea.setBounds( 285, 80, 280, 25 );
//        _stagingArea.setToolTipText( "Directory into which to copy all input files for processing." );
//        _stagingArea.setText( _settings.stagingArea() );
//        buttonPanel.add( _stagingArea );
//        JLabel stagingAreaLabel = new JLabel( "Use Staging Area:" );
//        stagingAreaLabel.setBounds( 10, 80, 240, 25 );
//        stagingAreaLabel.setHorizontalAlignment( JLabel.RIGHT );
//        buttonPanel.add( stagingAreaLabel );
//        _useStagingArea = new JCheckBox( "" );
//        _useStagingArea.setBounds( 255, 80, 25, 25 );
//        _useStagingArea.setSelected( _settings.useStagingArea() );
//        if ( _useStagingArea.isSelected() ) {
//            _stagingArea.setEnabled( true );
//        }
//        else {
//            _stagingArea.setEnabled( false );
//        }
//        _useStagingArea.addActionListener( new ActionListener() {
//            public void actionPerformed( ActionEvent e ) {
//                if ( _useStagingArea.isSelected() ) {
//                    _stagingArea.setEnabled( true );
//                }
//                else {
//                    _stagingArea.setEnabled( false );
//                }
//            }
//        });
//        buttonPanel.add( _useStagingArea );
        _passTypeList = new JComboBox<String>();
        _passTypeList.setBounds( 130, 20, 150, 25 );
        _passTypeList.setToolTipText( "List of possible pass types." );
        _passTypeList.setBackground( Color.WHITE );
        _passTypeList.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  See if the name of the pass looks like the default name for
                //  the previous (called "current") pass type.  If so, make a new
                //  default name out of this new pass type.
                if ( _passName.getText().equalsIgnoreCase( _currentPassType + " Pass" ) ) {
                    String newType = (String)_passTypeList.getSelectedItem();
                    _passName.setText( newType.substring( 0, 1 ).toUpperCase() + newType.substring( 1 ) + " Pass" );
                    autoSetPassDirectory();
                }
                _currentPassType = (String)_passTypeList.getSelectedItem();
                
            }
        });
        _passTypeList.setEditable( true );
        //  This little bit causes a typed-in item to be treated as a module name.
        _passTypeList.getEditor().addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  If not already in the list of pass types, add this name.
                boolean found = false;
                for ( int i = 0; i < _passTypeList.getItemCount() && !found; ++i ) {
                    if ( _passTypeList.getItemAt( i ).equals( _passTypeList.getEditor().getItem() ) ) {
                        found = true;
                        _passTypeList.setSelectedIndex( i );
                    }
                }
                if ( !found ) {
                    _passTypeList.addItem( (String)_passTypeList.getEditor().getItem() );
                    _passTypeList.setSelectedIndex( _passTypeList.getItemCount() - 1 );
                }
                _currentPassType = (String)_passTypeList.getSelectedItem();
            }
        });
        buttonPanel.add( _passTypeList );
        _currentPassType = (String)_passTypeList.getSelectedItem();
        Iterator iter = _settings.passTypeList().entrySet().iterator();
        for ( ; iter.hasNext(); )
            _passTypeList.addItem( (String)((Map.Entry)iter.next()).getValue() );
        _dataLabel = new JLabel( "Containing:" );
        _dataLabel.setBounds( 10, 110, 85, 25 );
        _dataLabel.setHorizontalAlignment( JLabel.RIGHT );
        buttonPanel.add( _dataLabel );
        _currentDataCheck = new JCheckBox( "Current Data" );
        _currentDataCheck.setBounds( 100, 110, 125, 25 );
        _currentDataCheck.setToolTipText( "Dump all existing jobs and data for this experiment (that are not part of a pass) into the new pass." );
        _currentDataCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _newJobsCheck.setSelected( false );
                _currentDataCheck.setSelected( true );
                _singleInputFileCheck.setEnabled( false );
            }
        });
        buttonPanel.add( _currentDataCheck );
        _newJobsCheck = new JCheckBox( "New Jobs" );
        _newJobsCheck.setBounds( 230, 110, 100, 25 );
        _newJobsCheck.setToolTipText( "New pass will contain new jobs specified below." );
        _newJobsCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _newJobsCheck.setSelected( true );
                _currentDataCheck.setSelected( false );
                _singleInputFileCheck.setEnabled( true );
            }
        });
        buttonPanel.add( _newJobsCheck );
        //  By default, "new jobs" will be checked.  Perhaps we can be smarter about
        //  this - base the setting on the existence of jobs outside a pass, or
        //  something else?
        _newJobsCheck.setSelected( true );
        _singleInputFileCheck = new JCheckBox( "Combine Scans in One Job" );
        _singleInputFileCheck.setBounds( 330, 110, 200, 25 );
        _singleInputFileCheck.setToolTipText( "Combine all specified scans in a single job (as opposed to one scan per job)." );
        _singleInputFileCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.defaultNames().singleInputFile = _singleInputFileCheck.isSelected();
                produceV2dFile();
            }
        });
        buttonPanel.add( _singleInputFileCheck );
        _singleInputFileCheck.setSelected( _settings.defaultNames().singleInputFile );
        _doSanityCheck = new JCheckBox( "" );
        _doSanityCheck.setToolTipText( "Check the settings for stations, files, etc, and warn of any problems." );
        _doSanityCheck.setSelected( _settings.defaultNames().jobCreationSanityCheck );
        _doSanityCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.defaultNames().jobCreationSanityCheck = _doSanityCheck.isSelected();
            }
        });
        buttonPanel.add( _doSanityCheck );
        _doSanityLabel = new JLabel( "Do Sanity Check:" );
        _doSanityLabel.setHorizontalAlignment( JLabel.RIGHT );
        buttonPanel.add( _doSanityLabel );
        _okButton = new JButton( "Apply" );
        _okButton.setBounds( 210, 140, 100, 25 );
        _okButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                applyButtonAction();
            }
        });
        buttonPanel.add( _okButton );
        
        //  The "status" panel shows information about the progress of different
        //  activities.
        IndexedPanel statusPanel = new IndexedPanel( "" );
        statusPanel.openHeight( 20 );
        statusPanel.alwaysOpen( true );
        statusPanel.noArrow( true );
        _statusLabel = new JLabel( "" );
        _statusLabel.setHorizontalAlignment( JLabel.RIGHT );
        statusPanel.add( _statusLabel );
        _scrollPane.addNode( statusPanel );
        
        _allObjectsBuilt = true;
        
    }        

    /*
     * Resize all components to fit properly anytime the window is resized or 
     * panes are opened and closed.
     */
    public void newSize() {
        int w = this.getWidth();
        int h = this.getHeight();
        if ( _menuBar != null )
            _menuBar.setBounds( 0, 0, w, 25 );
        if ( _allObjectsBuilt ) {
            _scrollPane.setBounds( 0, 25, w, h - 47 );
            _createPass.setBounds( 100, 20, 25, 25 );
            _passName.setBounds( 285, 20, w - 310, 25 );
            _okButton.setBounds( w - 125, 140, 100, 25 );
            _directory.setBounds( 100, 140, w - 125, 25 );
            _vexFileName.setBounds( 100, 170, w - 255, 25 );
            _previousVexFileButton.setBounds( w - 150, 170, 125, 25 );
            _directoryAsLabel.setBounds( 100, 140, w - 125, 25 );
            _vexFileNameAsLabel.setBounds( 100, 170, w - 125, 25 );
            _editor.setBounds( 10, 60, w - 35, 430 );
            _fromHost.setBounds( 20, 30, 150, 25 );
            _fromHostLocation.setBounds( 175, 30, w - 200, 25 );
            _viaHttp.setBounds( 20, 60, 150, 25 );
            _viaHttpLocation.setBounds( 175, 60, w - 200, 25 );
            _viaFtp.setBounds( 20, 90, 150, 25 );
            _viaFtpLocation.setBounds( 175, 90, w - 200, 25 );
            _localFile.setBounds( 20, 120, 150, 25 );
            _localFileLocation.setBounds( 175, 120, w - 200, 25 );
            _fromExperiment.setBounds( 20, 150, 150, 25 );
            _vexBrowseButton.setBounds( 175, 150, 100, 25 );
            _goButton.setBounds( w - 125, 175, 100, 25 );
            _v2dFromHost.setBounds( 20, 30, 150, 25 );
            _v2dFromHostLocation.setBounds( 175, 30, w - 200, 25 );
            _v2dViaHttp.setBounds( 20, 60, 150, 25 );
            _v2dViaHttpLocation.setBounds( 175, 60, w - 200, 25 );
            _v2dViaFtp.setBounds( 20, 90, 150, 25 );
            _v2dViaFtpLocation.setBounds( 175, 90, w - 200, 25 );
            _localV2dFile.setBounds( 20, 120, 150, 25 );
            _localV2dFileLocation.setBounds( 175, 120, w - 200, 25 );
            _noV2dFile.setBounds( 20, 150, 150, 25 );
            _goV2dButton.setBounds( w - 125, 150, 100, 25 );    
            _doSanityCheck.setBounds( w - 50, 110, 25, 25 );
            _doSanityLabel.setBounds( w - 250, 110, 195, 25 );
            _scanGrid.setBounds( 10, 70, w - 35, 320 );
            _timeLimits.setBounds( 330, 30, w - 355, 35 );
            _v2dFileName.setBounds( 100, 30, w - 125, 25 );
            _v2dEditor.setBounds( 10, 60, w - 35, 430 );
            _passDirectory.setBounds( 285, 50, w - 310, 25 );
//            _stagingArea.setBounds( 285, 80, w - 310, 25 );
            //  These things set the panel sizes based on their content.
            _antennaPane.setBounds( 0, 20, w, _antennaPane.dataHeight() );
            try {
                for ( Iterator<BrowserNode> iter = _antennaPane.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                    StationPanel thisPanel = (StationPanel)iter.next();
                    thisPanel.newWidth( w );//- 25 );
                }
                _sourcePane.setBounds( 0, 20, w, _sourcePane.dataHeight() );
                for ( Iterator<BrowserNode> iter = _sourcePane.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                    //  Use the "ClassCastException" to eliminate the button panel, which leads the
                    //  list of sources.
                    try {
                        SourcePanel thisPanel = (SourcePanel)iter.next();
                        thisPanel.newWidth( w - 25 );
                    } catch ( java.lang.ClassCastException e ) {
                    }
                }
            } catch ( java.util.ConcurrentModificationException e ) {
            }
            _eopPane.setBounds( 0, 20, w, _eopPane.dataHeight() );
            if ( _vexEOPPane != null )
                _vexEOPPane.setBounds( 0, 20, w, _vexEOPPane.dataHeight() );
            if ( _newEOPPane != null )
                _newEOPPane.setBounds( 0, 20, w, _newEOPPane.dataHeight() );
            _statusLabel.setBounds( 10, 0, w - 35, 25 );
        }
    }
    
    /*
     * One of the checkboxes for vex file sources was checked.
     */
    public void vexSourceChoice( JCheckBox selection ) {
        _fromHost.setSelected( false );
        _fromHostLocation.setEnabled( false );
        _viaHttp.setSelected( false );
        _viaHttpLocation.setEnabled( false );
        _viaFtp.setSelected( false );
        _viaFtpLocation.setEnabled( false );
        _localFile.setSelected( false );
        _localFileLocation.setEnabled( false );
        _fromExperiment.setSelected( false );
        _vexBrowseButton.setEnabled( false );
        if ( _fromHost == selection ) {
            _fromHost.setSelected( true );
            _fromHostLocation.setEnabled( true );
        }
        else if ( _viaHttp == selection ) {
            _viaHttp.setSelected( true );
            _viaHttpLocation.setEnabled( true );
        }
        else if ( _viaFtp == selection ) {
            _viaFtp.setSelected( true );
            _viaFtpLocation.setEnabled( true );
        }
        else if ( _localFile == selection ) {
            _localFile.setSelected( true );
            _localFileLocation.setEnabled( true );
        }
        else if ( _fromExperiment == selection ) {
            _fromExperiment.setSelected( true );
            _vexBrowseButton.setEnabled( true );
        }
        _settings.defaultNames().vexFromHost = _fromHost.isSelected();
        _settings.defaultNames().vexViaHttp = _viaHttp.isSelected();
        _settings.defaultNames().vexViaFtp = _viaFtp.isSelected();
        _settings.defaultNames().vexFromLocal = _localFile.isSelected();
        _settings.defaultNames().vexFromExperiment = _fromExperiment.isSelected();
    }
    
    /*
     * One of the checkboxes for .v2d file sources was checked.
     */
    public void v2dSourceChoice( JCheckBox selection ) {
        _v2dFromHost.setSelected( false );
        _v2dFromHostLocation.setEnabled( false );
        _v2dViaHttp.setSelected( false );
        _v2dViaHttpLocation.setEnabled( false );
        _v2dViaFtp.setSelected( false );
        _v2dViaFtpLocation.setEnabled( false );
        _localV2dFile.setSelected( false );
        _localV2dFileLocation.setEnabled( false );
        _noV2dFile.setSelected( false );
        if ( _v2dFromHost == selection ) {
            _v2dFromHost.setSelected( true );
            _v2dFromHostLocation.setEnabled( true );
        }
        else if ( _v2dViaHttp == selection ) {
            _v2dViaHttp.setSelected( true );
            _v2dViaHttpLocation.setEnabled( true );
        }
        else if ( _v2dViaFtp == selection ) {
            _v2dViaFtp.setSelected( true );
            _v2dViaFtpLocation.setEnabled( true );
        }
        else if ( _localV2dFile == selection ) {
            _localV2dFile.setSelected( true );
            _localV2dFileLocation.setEnabled( true );
        }
        else if ( _noV2dFile == selection ) {
            _noV2dFile.setSelected( true );
        }
        _settings.defaultNames().v2dFromHost = _v2dFromHost.isSelected();
        _settings.defaultNames().v2dViaHttp = _v2dViaHttp.isSelected();
        _settings.defaultNames().v2dViaFtp = _v2dViaFtp.isSelected();
        _settings.defaultNames().v2dFromLocal = _localV2dFile.isSelected();
        _settings.defaultNames().noV2dFile = _noV2dFile.isSelected();
    }
    
    /*
     * Set the name of the pass directory based on the pass name.
     */
    public void autoSetPassDirectory() {
        if ( !_keepPassDirectory ) {
            String str = _passName.getText();
            //  Replace any spaces with underscores.
            str = str.replace( ' ', '_' );
            str = str.replace( '\t', '_' );
            _passDirectory.setText( str );
        }
    }
    
    /*
     * Obtain .vex file from the user-specified source.
     */
    public void getVexFromSource() {
        if ( _fromHost.isSelected() ) {
            Component comp = this;
            while ( comp.getParent() != null )
                comp = comp.getParent();
            Point pt = _fromHost.getLocationOnScreen();
            GetFileMonitor fileGet = new GetFileMonitor( (Frame)comp, pt.x + 25, pt.y + 25, _fromHostLocation.getText(), _settings, false );
            //fileGet.setVisible( true );
            //  We only use the content of the file if it was read successfully.
            if ( fileGet.success() ) {
                _settings.defaultNames().vexFileSource = _fromHostLocation.getText();
                _editor.text( fileGet.inString() );
                _editor.top();
                parseNewVexFile( true );
            }
            else {
                JOptionPane.showMessageDialog( (Frame)comp, fileGet.error(), ".vex File Read Error", JOptionPane.WARNING_MESSAGE );
            }
        }
        else if ( _viaHttp.isSelected() ) {
            _settings.defaultNames().viaHttpLocation = _viaHttpLocation.getText();
            try {
                URL url = new URL( "http://" + _viaHttpLocation.getText() );
                url.openConnection();
                InputStream reader = url.openStream();
                byte[] buffer = new byte[153600];
                int bytesRead = 0;
                _editor.text( "" );
                while ( ( bytesRead = reader.read( buffer, 0, 153600 ) ) > 0 ) {
                    _editor.addText( new String( buffer ).substring( 0, bytesRead ) );
                }
                parseNewVexFile( true );
            } catch ( MalformedURLException e ) {
                JOptionPane.showMessageDialog( _this, "Malformed URL: \"http://" + _viaHttpLocation.getText() + "\"",
                        "Bad URL",
                        JOptionPane.ERROR_MESSAGE );
            } catch ( IOException e ) {
                JOptionPane.showMessageDialog( _this, e.toString(),
                        "IO Error",
                        JOptionPane.ERROR_MESSAGE );
            }
        }
        else if ( _viaFtp.isSelected() ) {
            _settings.defaultNames().viaFtpLocation = _viaFtpLocation.getText();
            try {
                URL url = new URL( "ftp://" + _viaHttpLocation.getText() );
                url.openConnection();
                InputStream reader = url.openStream();
                byte[] buffer = new byte[153600];
                int bytesRead = 0;
                _editor.text( "" );
                while ( ( bytesRead = reader.read( buffer, 0, 153600 ) ) > 0 ) {
                    _editor.addText( new String( buffer ).substring( 0, bytesRead ) );
                }
                parseNewVexFile( true );
            } catch ( MalformedURLException e ) {
                JOptionPane.showMessageDialog( _this, "Malformed URL: \"ftp://" + _viaFtpLocation.getText() + "\"",
                        "Bad URL",
                        JOptionPane.ERROR_MESSAGE );
            } catch ( IOException e ) {
                JOptionPane.showMessageDialog( _this, e.toString(),
                        "IO Error",
                        JOptionPane.ERROR_MESSAGE );
            }
        }
        else if ( _localFile.isSelected() ) {
            _settings.defaultNames().localFileLocation = _localFileLocation.getText();
            try {
                FileInputStream reader = new FileInputStream( _localFileLocation.getText() );
                byte[] buffer = new byte[153600];
                int bytesRead = 0;
                _editor.text( "" );
                while ( ( bytesRead = reader.read( buffer, 0, 153600 ) ) > 0 ) {
                    _editor.addText( new String( buffer ).substring( 0, bytesRead ) );
                }
                parseNewVexFile( true );
            } catch ( FileNotFoundException e ) {
                JOptionPane.showMessageDialog( _this, "Local File \"" + _localFileLocation.getText() + "\" was not found.",
                        "File Not Found",
                        JOptionPane.ERROR_MESSAGE );
            } catch ( IOException e ) {
                JOptionPane.showMessageDialog( _this, e.toString(),
                        "IO Error",
                        JOptionPane.ERROR_MESSAGE );
            }
        }
        else {
            new AePlayWave( _settings.guiDocPath() + "/cantdo.wav" ).start();
            JOptionPane.showMessageDialog( this, "That feature has not been implemented." );
        }
    }
    
    /*
     * Obtain .v2d file from the user-specified source.
     */
    public void getV2dFromSource() {
        if ( _v2dFromHost.isSelected() ) {
            Component comp = this;
            while ( comp.getParent() != null )
                comp = comp.getParent();
            Point pt = _v2dFromHost.getLocationOnScreen();
            GetFileMonitor fileGet = new GetFileMonitor( (Frame)comp, pt.x + 25, pt.y + 25, _v2dFromHostLocation.getText(), _settings, false );
            //  We only use the content of the file if it was read successfully.
            if ( fileGet.success() ) {
                _settings.defaultNames().v2dFileSource = _v2dFromHostLocation.getText();
                _startingV2dFileContent = fileGet.inString();
                parseStartingV2dFile( null );
            }
        }
        else if ( _v2dViaHttp.isSelected() ) {
            _settings.defaultNames().v2dViaHttpLocation = _v2dViaHttpLocation.getText();
            try {
                URL url = new URL( "http://" + _v2dViaHttpLocation.getText() );
                url.openConnection();
                InputStream reader = url.openStream();
                byte[] buffer = new byte[153600];
                int bytesRead = 0;
                _startingV2dFileContent = "";
                while ( ( bytesRead = reader.read( buffer, 0, 153600 ) ) > 0 ) {
                    _startingV2dFileContent += new String( buffer ).substring( 0, bytesRead );
                }
                parseStartingV2dFile( null );
            } catch ( MalformedURLException e ) {
                JOptionPane.showMessageDialog( _this, "Malformed URL: \"http://" + _v2dViaHttpLocation.getText() + "\"",
                        "Bad URL",
                        JOptionPane.ERROR_MESSAGE );
            } catch ( IOException e ) {
                JOptionPane.showMessageDialog( _this, e.toString(),
                        "IO Error",
                        JOptionPane.ERROR_MESSAGE );
            }
        }
        else if ( _v2dViaFtp.isSelected() ) {
            _settings.defaultNames().v2dViaFtpLocation = _v2dViaFtpLocation.getText();
            try {
                URL url = new URL( "ftp://" + _v2dViaFtpLocation.getText() );
                url.openConnection();
                InputStream reader = url.openStream();
                byte[] buffer = new byte[153600];
                int bytesRead = 0;
                _startingV2dFileContent = "";
                while ( ( bytesRead = reader.read( buffer, 0, 153600 ) ) > 0 ) {
                    _startingV2dFileContent += new String( buffer ).substring( 0, bytesRead );
                }
                parseStartingV2dFile( null );
            } catch ( MalformedURLException e ) {
                JOptionPane.showMessageDialog( _this, "Malformed URL: \"ftp://" + _v2dViaFtpLocation.getText() + "\"",
                        "Bad URL",
                        JOptionPane.ERROR_MESSAGE );
            } catch ( IOException e ) {
                JOptionPane.showMessageDialog( _this, e.toString(),
                        "IO Error",
                        JOptionPane.ERROR_MESSAGE );
            }
        }
        else if ( _localV2dFile.isSelected() ) {
            _settings.defaultNames().localV2dFileLocation = _localV2dFileLocation.getText();
            try {
                FileInputStream reader = new FileInputStream( _localV2dFileLocation.getText() );
                byte[] buffer = new byte[153600];
                int bytesRead = 0;
                _startingV2dFileContent = "";
                while ( ( bytesRead = reader.read( buffer, 0, 153600 ) ) > 0 ) {
                    _startingV2dFileContent += new String( buffer ).substring( 0, bytesRead );
                }
                parseStartingV2dFile( null );
            } catch ( FileNotFoundException e ) {
                JOptionPane.showMessageDialog( _this, "Local File \"" + _localV2dFileLocation.getText() + "\" was not found.",
                        "File Not Found",
                        JOptionPane.ERROR_MESSAGE );
            } catch ( IOException e ) {
                JOptionPane.showMessageDialog( _this, e.toString(),
                        "IO Error",
                        JOptionPane.ERROR_MESSAGE );
            }
        }
        else if ( _noV2dFile.isSelected() ) {
            _startingV2dFileContent = null;
        }
    }
    
    /*
     * Parse the content of the "starting" .v2d file and change all settings to
     * match it.  The .v2d file content has already been stored as the String
     * "_startingV2dFileContent".
     */
    public void parseStartingV2dFile( String basePath ) {
        //  The parser eats the content and stores it in structures.
        _v2dFileParser = new V2dFileParser( _startingV2dFileContent );
        
        //  Grab the .vex file name, if it exists.
        if ( _v2dFileParser.vexFile() != null ) {
            this.vexFileName( _v2dFileParser.vexFile() );
            //  Try to put the content of this file into the .vex file editor.  This
            //  requires a complete path, which needs to be constructed.  We don't do
            //  this if we don't have a "basePath".
            if ( basePath != null ) {
                Component comp = _this;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = new Point( 100, 100 );
                if ( _thisExperiment != null )
                    pt = _thisExperiment.getLocationOnScreen();
                GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        basePath + _v2dFileParser.vexFile(), _settings, false );
                if ( getFile.success() && getFile.inString() != null && getFile.inString().length() > 0 ) {
                    _editor.text( getFile.inString() );
                    //  This should initialize all of the settings properly...we hope.
                    //  Specific .v2d file settings will then change them.
                    parseNewVexFile( false );
                }  
            }
        }
        
        //  Global parameters...
        if ( _v2dFileParser.singleScan() != null )
            _singleInputFileCheck.setSelected( _v2dFileParser.singleScan() );
        if ( _v2dFileParser.jobSeries() != null )
            _inputFileBaseName.setText( _v2dFileParser.jobSeries() );
        if (  _v2dFileParser.startSeries() != null )
            _inputFileSequenceStart.intValue( _v2dFileParser.startSeries() );
        
        //  the "Normal" Setup section contains things from the "correlation parameters" settings.
        if ( _v2dFileParser.setupTInt( "normalSetup" ) != null )
            _tInt.value( _v2dFileParser.setupTInt( "normalSetup" ) );
        if ( _v2dFileParser.setupFFTSpecRes( "normalSetup" ) != null )
            _fftSpecRes.value( _v2dFileParser.setupFFTSpecRes( "normalSetup" ) );
        if ( _v2dFileParser.setupSpecRes( "normalSetup" ) != null )
            _specRes.value( _v2dFileParser.setupSpecRes( "normalSetup" ) );
        if ( _v2dFileParser.setupSubintNS( "normalSetup" ) != null )
            _subintNS.intValue( _v2dFileParser.setupSubintNS( "normalSetup" ) );
        if ( _v2dFileParser.setupDoPolar( "normalSetup" ) != null )
            _doPolar.setSelected( _v2dFileParser.setupDoPolar( "normalSetup" ) );
        
        //  Which scans are on/off.
        if ( _v2dFileParser.ruleScan( "scansubset" ) != null ) {
            //  Turn them all off.
            _scanGrid.allOff();
            //  Then turn individual ones back on.
            String[] scan = _v2dFileParser.ruleScan( "scansubset" ).split( "," );
            for ( int i = 0; i < scan.length; ++i ) {
                _scanGrid.setButton( scan[i], true );
            }
        }
            
        //  Which antennas to use, including parameters associated with them.  Check
        //  all of the antennas we know about (they should have been listed in the .vex
        //  file).
        if ( _antennaList != null && !_antennaList.useList().isEmpty() ) {
            synchronized ( _antennaList ) {
                for ( Iterator<StationPanel> iter = _antennaList.iterator(); iter.hasNext(); ) {
                    StationPanel antenna = iter.next();
                    //  If the antenna is not in the .v2d file, it is not being used.
                    if ( _v2dFileParser.antennaSection( antenna.name() ) == null )
                        antenna.use( false );
                    else {
                        if ( _v2dFileParser.antennaPhaseCalInt( antenna.name() ) != null )
                            antenna.phaseCalInt( _v2dFileParser.antennaPhaseCalInt( antenna.name() ) );
                        if ( _v2dFileParser.antennaToneSelection( antenna.name() ) != null )
                            antenna.toneSelection( _v2dFileParser.antennaToneSelection( antenna.name() ) );
                        if ( _v2dFileParser.antennaVsn( antenna.name() ) != null ) {
                            antenna.useVsn( true );
                            antenna.vsnSource( _v2dFileParser.antennaVsn( antenna.name() ) );
                        }
                        else if ( _v2dFileParser.antennaFile( antenna.name() ) != null ) {
                            antenna.useFile( true );
                            Vector<String> fileList = _v2dFileParser.antennaFile( antenna.name() );
                            for ( Iterator<String> iter1 = fileList.iterator(); iter1.hasNext(); )
                                antenna.useFile( iter1.next() );
                        }
                        else if ( _v2dFileParser.antennaFileList( antenna.name() ) != null ) {
                            antenna.useFile( _v2dFileParser.antennaFileList( antenna.name() ) );
                            antenna.fileListName( _v2dFileParser.antennaFileList( antenna.name() ) );
                            antenna.useFileList( true );
                        }
                        else if ( _v2dFileParser.antennaNetworkPort( antenna.name() ) != null ) {
                            antenna.useEVLBI( true );
                            antenna.networkPort( _v2dFileParser.antennaNetworkPort( antenna.name() ) );
                        }
                        else if ( _v2dFileParser.antennaFake( antenna.name() ) != null ) {
                            antenna.useFake( _v2dFileParser.antennaFake( antenna.name() ) );
                        }
                        //  Position changes.
                        if ( _v2dFileParser.antennaX( antenna.name() ) != null )
                            antenna.positionX( _v2dFileParser.antennaX( antenna.name() ) );
                        if ( _v2dFileParser.antennaY( antenna.name() ) != null )
                            antenna.positionY( _v2dFileParser.antennaY( antenna.name() ) );
                        if ( _v2dFileParser.antennaZ( antenna.name() ) != null )
                            antenna.positionZ( _v2dFileParser.antennaZ( antenna.name() ) );
                        //  Clock settings.
                        if ( _v2dFileParser.antennaDeltaClock( antenna.name() ) != null )
                            antenna.deltaClock( _v2dFileParser.antennaDeltaClock( antenna.name() ) );
                    }
                }
            }
        }
        
    }
    
    /*
     * This function is used when an experiment has already been created and contains
     * a .v2d file.  This file is parsed, and all settings are changed to reflect its
     * contents.  This is NOT the same as a "starting" .v2d file, which is used as
     * a template.
     * 
     * This function is given a file "base" - the directory containing .v2d file(s)
     * if they exist.  A remote "ls" command is used to generate their actual names.
     * We can only handle one .v2d file, so the LAST of these files that appears in
     * response to the "ls" is used (if all is well, there should be only one anyway).
     */
    protected String _lastV2dPath;
    protected String _lastV2dBase;
    synchronized public void findOldV2dFile( String fileBase ) {
        _lastV2dPath = null;
        _lastV2dBase = fileBase;
        DiFXCommand_ls ls = new DiFXCommand_ls( fileBase + "*.v2d", _settings );
        //  Set the callback for when the list is complete.  
        ls.addEndListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  Found anything at all?
                if ( _lastV2dPath != null ) {
                    final String thePath = _lastV2dPath;
                    final String theBase = _lastV2dBase;
                    Thread doThisInAThread = new Thread() {
                        public void run() {
                            readV2dFile( thePath, theBase );
                        }
                    };
                    doThisInAThread.start();       
                }
            }
        });
        //  Set the callback for when a new item is added to the list.
        ls.addIncrementalListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _lastV2dPath = e.getActionCommand().trim();
            }
        });
        try {
            ls.send();
        } catch ( java.net.UnknownHostException e ) {
            //  BLAT handle this
        }
    }
    
    /*
     * Load a given .v2d file by name.  This function is called by the "findOldV2dFile()"
     * function but it may have other uses.
     */
    public void readV2dFile( String fileName, String basePath ) {
        Component comp = _this;
        while ( comp.getParent() != null )
            comp = comp.getParent();
        Point pt = new Point( 100, 100 );
        if ( _thisExperiment != null )
            pt = _thisExperiment.getLocationOnScreen();
        GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                fileName, _settings, false );
        if ( getFile.inString() != null && getFile.inString().length() > 0 ) {
            _startingV2dFileContent = getFile.inString();
            parseStartingV2dFile( basePath );
        }
    }
    
    ArrayList<ArrayList<ArrayList<String>>> _scansInJobs;
    //--------------------------------------------------------------------------
    //  Create a list of all jobs and what scans they contain for this experiment.
    //  The scans contain scan names and sources.  Doing all of this requires a
    //  valid .vex and .v2d file.
    //--------------------------------------------------------------------------
    public void findScansInJobs() {
        VexFileParser vexFile = vexFileParser();
        V2dFileParser v2dFile = v2dFileParser();
        //  Carry on if we have access to the .v2d and .vex data.
        if ( vexFile != null && v2dFile != null ) {
            int startSeries = v2dFile.startSeries();
            Vector<String> scanList = v2dFile.scanList();
            _scansInJobs = new ArrayList<ArrayList<ArrayList<String>>>();
            ArrayList<ArrayList<String>> currentJob = new ArrayList<ArrayList<String>>();
            _scansInJobs.add( currentJob );
            for ( Iterator<String> iter = scanList.iterator(); iter.hasNext(); ) {
                String item = iter.next();
                //  Each string is either a single scan name or a "*", which indicates
                //  ALL scan names.
                if ( item.contentEquals( "*" ) ) {
                    //  Get every scan in the list (contained in the .vex file).  We get
                    //  the scan name and source from the same location.
                    for ( Iterator<VexFileParser.Scan> iter1 = vexFile.scanList().iterator(); iter1.hasNext(); ) {
                        VexFileParser.Scan scan = iter1.next();
                        ArrayList<String> scanData = new ArrayList<String>();
                        //System.out.println( "adding " + scan.name + " and " + scan.source );
                        scanData.add( scan.name );
                        scanData.add( scan.source );
                        if ( v2dFile.singleScan() == null || v2dFile.singleScan() ) {
                            currentJob.add( scanData );
                            currentJob = new ArrayList<ArrayList<String>>();
                            _scansInJobs.add( currentJob );
                        }
                    }
                }
                //  Otherwise is should just be a single scan.  The scan is named, but
                //  the source we have to look up.
                else {
                    ArrayList<String> scanData = new ArrayList<String>();
                    scanData.add( item );
                    //  Locate the source in the .vex file...
                    boolean foundIt = false;
                    for ( Iterator<VexFileParser.Scan> iter1 = vexFile.scanList().iterator(); iter1.hasNext() && !foundIt; ) {
                        VexFileParser.Scan scan = iter1.next();
                        if ( item.trim().contentEquals( scan.name.trim() ) ) {
                            foundIt = true;
                            scanData.add( scan.source );
                        }
                    }
                    System.out.println( "  " );
                    if ( !foundIt )
                        scanData.add( "unknown" );
                    if ( v2dFile.singleScan() == null || v2dFile.singleScan() ) {
                        currentJob.add( scanData );
                        currentJob = new ArrayList<ArrayList<String>>();
                        _scansInJobs.add( currentJob );
                    }
                }
            }
        }
    }
    
    //--------------------------------------------------------------------------
    //  Return a string containing the source of a given scan number.  If there
    //  are multiple sources, translate this to "multiple".  If we don't know
    //  what sources correspond to what jobs (for whatever reason) return
    //  "unknown".
    //--------------------------------------------------------------------------
    public String sourceOfJob( int num ) {
        ArrayList<ArrayList<String>> scanList = scansOfJob( num );
        if ( scanList == null )
            return "unknown";
        if ( scanList.size() == 0 )
            return "Unknown";
        if ( scanList.size() > 1 )
            return "multiple";
        return scanList.get( 0 ).get( 1 );
    }
    
    //--------------------------------------------------------------------------
    //  Return a string containing the scan name of a given scan number.  If there
    //  are multiple sources, translate this to "multiple".  If we don't know
    //  what scans correspond to what jobs (for whatever reason) return
    //  "unknown".
    //--------------------------------------------------------------------------
    public String scanOfJob( int num ) {
        //System.out.println( "looking for " + num );
        ArrayList<ArrayList<String>> scanList = scansOfJob( num );
        if ( scanList == null )
            return "unknown";
        if ( scanList.size() == 0 )
            return "Unknown";
        if ( scanList.size() > 1 )
            return "multiple";
        return scanList.get( 0 ).get( 0 );
    }
    
    //--------------------------------------------------------------------------
    //  Return an array list containing all sources contained in a job.
    //--------------------------------------------------------------------------
    public ArrayList<ArrayList<String>> scansOfJob( int num ) {
        if ( _scansInJobs == null )
            return null;
        int offset = 1;
        if ( v2dFileParser() != null && v2dFileParser().startSeries() != null )
            offset = v2dFileParser().startSeries();
        int index = num - offset;
        if ( index >= _scansInJobs.size() )
            return null;
        return _scansInJobs.get( index );
    }
    
    /*
     * This class is used to display information about a source.  It is an IndexedPanel
     * with a few controls.  May need to be put in its own file (like StationPanel) if
     * it gets much more complex.
     */
    public class SourcePanel extends IndexedPanel {

        public SourcePanel( VexFileParser.Source source, VexFileParser vexData, SystemSettings settings ) {
            super( source.name );
            _source = source;
            _vexData = vexData;
            _settings = settings;
            _this = this;
            _changeListeners = new EventListenerList();
            this.closedHeight( 20 );
            this.openHeight( 20 );
            this.open( false );
            this.darkTitleBar( false );
            this.drawFrame( false );
            this.resizeOnTopBar( true );
            //  These two things may be temporary - we might ultimately make these
            //  items expandable.
            this.alwaysOpen( true );
            this.noArrow( true );
            _useCheck = new JCheckBox( "" );
            _useCheck.setBounds( 200, 2, 18, 16 );
            _useCheck.setSelected( true );
            _useCheck.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    if ( _buttons != null ) {
                        for ( Iterator<StationButton> iter = _buttons.iterator(); iter.hasNext(); ) {
                            StationButton button = iter.next();
                            button.selected = _useCheck.isSelected();
                            button.selectionChange();
                        }
                    }
                    dispatchChangeCallback();
                }
            } );
            this.add( _useCheck );
            //  Add a button for each station involved in this experiment, or gaps for those stations
            //  that are not used.
            _buttons = new ArrayList<StationButton>();
            int xOffset = 300;
            for ( Iterator<VexFileParser.Station> iter = vexData.stationList().iterator(); iter.hasNext(); ) {
                VexFileParser.Station station = iter.next();
                ArrayList<VexFileParser.Scan> scanList = new ArrayList<VexFileParser.Scan>();
                //  Find all of the scans that use this station.
                for ( Iterator<VexFileParser.Scan> iter2 = vexData.scanList().iterator(); iter2.hasNext(); ) {
                    VexFileParser.Scan scan = iter2.next();
                    if ( scan.source.equalsIgnoreCase( _source.name ) ) {
                        boolean found = false;
                        for ( Iterator<VexFileParser.ScanStation> iter3 = scan.station.iterator(); iter3.hasNext() && !found; ) {
                            VexFileParser.ScanStation scanStation = iter3.next();
                            if ( scanStation.name.equalsIgnoreCase( station.name ) )
                                found = true;
                        }
                        if ( found )
                            scanList.add( scan );
                    }
                }
                //  Create a button if it has been used.
                if ( scanList.size() > 0 ) {
                    final StationButton thisLabel = new StationButton();
                    thisLabel.scanList = scanList;
                    thisLabel.setBounds( xOffset, 1, 75, 18 );
                    thisLabel.setBorder( BorderFactory.createLineBorder( Color.BLACK ) ); 
                    thisLabel.selected = true;
                    thisLabel.setBackground( Color.ORANGE );
                    thisLabel.stationName = station.name;
                    thisLabel.setTooltip();
                    this.add( thisLabel );
                    _buttons.add( thisLabel );
                }
                xOffset += 80;
            }
            newTooltipText();
        }
        
        /*
         * Class to hold one of the buttons.
         */
        class StationButton extends JButton {
            public StationButton() {
                //  Pushing the button either turns on or off all instances of this station in the
                //  scans associated with this source.  This causes changes to other things, so
                //  triggers a callback that causes a redraw.
                addActionListener( new ActionListener() {
                    public void actionPerformed( ActionEvent evt ) {
                        selected = !selected;
                        selectionChange();
                        if ( selected )
                            setBackground( Color.ORANGE );
                        else
                            setBackground( Color.WHITE );
                        newTooltipText();
                        dispatchChangeCallback();
                    }
                } );      
            }

            //  The button's tooltip lists all of the scans used by the current station.
            public void setTooltip() {
                String tooltip = "<<bold>>" + _name + " uses " + stationName + " in scans:<</bold>>\n";
                int len = 0;
                for ( Iterator<VexFileParser.Scan> iter3 = scanList.iterator(); iter3.hasNext(); ) {
                    VexFileParser.Scan scan = iter3.next();
                    if ( scan.omitFlag )
                        tooltip += "<<red>>";
                    tooltip += scan.name;
                    if ( scan.omitFlag )
                        tooltip += "<</color>>";
                    len += getFontMetrics( getFont() ).stringWidth( scan.name );
                    if ( iter3.hasNext() ) {
                        tooltip += ", ";
                        if ( len > 400 ) {
                            len = 0;
                            tooltip += "\n";
                        }
                    }
                }
                setToolTipText( tooltip );
            }
                    
            public JToolTip createToolTip() {
                _tip = new ComplexToolTip();
                _tip.setComponent( this );
                return _tip;
            }
            public Point getToolTipLocation( MouseEvent e) {
                return new Point( 10, getHeight() );
            }
            public ArrayList<VexFileParser.Scan> scanList;
            public void selectionChange() {
                for ( Iterator<VexFileParser.Scan> iter4 = scanList.iterator(); iter4.hasNext(); ) {
                    VexFileParser.Scan scan = iter4.next();
                    for ( Iterator<VexFileParser.ScanStation> iter5 = scan.station.iterator(); iter5.hasNext(); ) {
                        VexFileParser.ScanStation scanStation = iter5.next();
                        if ( scanStation.name.equalsIgnoreCase( stationName ) )
                            scanStation.omitFlag = !selected;
                    }
                }
            }
            public boolean selected;
            public String stationName;
        };
        
        /*
         * A change has been made in the vex data.  Look through the station buttons
         * and change their colors based on whether they are now used or not.
         */
        public void redraw() {
            //  Search through all of the station buttons associated with this source.  For each button,
            //  see if any of the scans are currently using the station.  We need only one to turn this
            //  button "on".
            for ( Iterator<StationButton> iter = _buttons.iterator(); iter.hasNext(); ) {
                StationButton button = iter.next();
                boolean selected = false;
                for ( Iterator<VexFileParser.Scan> iter2 = button.scanList.iterator(); iter2.hasNext() && !selected; ) {
                    VexFileParser.Scan scan = iter2.next();
                    for ( Iterator<VexFileParser.ScanStation> iter3 = scan.station.iterator(); iter3.hasNext() && !selected; ) {
                        VexFileParser.ScanStation scanStation = iter3.next();
                        if ( scanStation.name.equalsIgnoreCase( button.stationName ) && !scanStation.omitFlag )
                            selected = true;
                    }
                }
                if ( selected )
                    button.setBackground( Color.ORANGE );
                else
                    button.setBackground( Color.WHITE );
                button.setTooltip();
            }
        }

        public void newWidth( int w ) {
        }

        public void addChangeListener( ActionListener a ) {
            _changeListeners.add( ActionListener.class, a );
        }

        protected void dispatchChangeCallback() {
            Object[] listeners = _changeListeners.getListenerList();
            // loop through each listener and pass on the event if needed
            int numListeners = listeners.length;
            for ( int i = 0; i < numListeners; i+=2 ) {
                if ( listeners[i] == ActionListener.class )
                    ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
            }
        }
        
        public boolean use() { return _useCheck.isSelected(); }
        void use( boolean newVal ) {
            _useCheck.setSelected( newVal );
            if ( _buttons != null ) {
                for ( Iterator<StationButton> iter = _buttons.iterator(); iter.hasNext(); ) {
                    StationButton button = iter.next();
                    button.selected = _useCheck.isSelected();
                    newTooltipText();
                    button.selectionChange();
                }
            }
        }
        public VexFileParser.Source source() { return _source; }
        
        /*
         * Create a new tooltip showing information about this source and which scans
         * are used to observe it.
         */
        protected void newTooltipText() {
            String tooltip = "<<bold>>" + _source.name + "<</bold>>\n";
            tooltip += "<<fixed>><<bold>>RA:<</bold>>  " + _source.ra + "\n";
            tooltip += "<<bold>>DEC:<</bold>> " + _source.dec + "\n";
            tooltip += "\n<<bold>>Scans/Stations Using This Source:\n";
            for ( int i = 0; i < 30; ++i )
                tooltip += " ";
            for ( Iterator<VexFileParser.Station> iter = _vexData.stationList().iterator(); iter.hasNext(); )
                tooltip += " " + iter.next().name.toUpperCase() + " ";
            tooltip += "<</bold>>\n";
            //  This next bit is a little messy, but it works.  We make a list of scan names
            //  by looking at all of the buttons, then at which stations are used in each.
            ArrayList<String> scanNames = new ArrayList<String>();
            for ( Iterator<StationButton> iter = _buttons.iterator(); iter.hasNext(); ) {
                ArrayList<VexFileParser.Scan> scanList = iter.next().scanList;
                for ( Iterator<VexFileParser.Scan> iter4 = scanList.iterator(); iter4.hasNext(); ) {
                    VexFileParser.Scan scan = iter4.next();
                    tooltip += "<<bold>>" + scan.name + "<</bold>>";
                    for ( int i = scan.name.length(); i < 30; ++i )
                        tooltip += " ";
                    //  Look for each station.  If it is used, add an appropriate marking.
                    for ( Iterator<VexFileParser.Station> iter2 = _vexData.stationList().iterator(); iter2.hasNext(); ) {
                        VexFileParser.Station station = iter2.next();
                        boolean found = false;
                        for ( Iterator<VexFileParser.ScanStation> iter5 = scan.station.iterator(); iter5.hasNext() && !found; ) {
                            VexFileParser.ScanStation scanStation = iter5.next();
                            if ( scanStation.name.equalsIgnoreCase( station.name ) ) {
                                found = true;
                                if ( scanStation.omitFlag )
                                    tooltip += " <<red>>X<</color>>  ";
                                else
                                    tooltip += " X  ";
                            }
                        }
                        if ( !found )
                            tooltip += "    ";
                    }
                    tooltip += "\n";
                }
            }
            _this.setToolTipText( tooltip );
        }

        @Override
        public JToolTip createToolTip() {
            _tip = new ComplexToolTip();
            _tip.setComponent( this );
            return _tip;
        }
        @Override
        public Point getToolTipLocation( MouseEvent e) {
            return new Point( 10, getHeight() );
        }

        protected SourcePanel _this;
        protected JCheckBox _useCheck;
        protected EventListenerList _changeListeners;
        protected VexFileParser.Source _source;
        protected VexFileParser _vexData;
        protected ComplexToolTip _tip;
        protected ArrayList<StationButton> _buttons;
    }
    
    /*
     * This is a list of antenna panels that provides some useful information about the
     * data in the list.
     */
    public class AntennaList extends ArrayList<StationPanel> {
        
        public ArrayList<StationPanel> useList() {
            ArrayList<StationPanel> list = new ArrayList<StationPanel>();
            for ( Iterator<StationPanel> iter = this.iterator(); iter.hasNext(); ) {
                StationPanel panel = iter.next();
                if ( panel.use() )
                    list.add( panel );
            }
            return list;
        }
        
    }
    
    protected boolean _getDefaultSources;
    //--------------------------------------------------------------------------
    //  Read the current vex file, which is stored in the editor, and parse out items
    //  that we can use in the .v2d file.  The boolean argument determines whether
    //  default source locations are generated - this is a time consuming process
    //  and shouldn't be done if it isn't necessary (say, if you were going to
    //  replace the source locations anyway).
    //--------------------------------------------------------------------------
    public void parseNewVexFile( boolean getDefaultSources ) {
        _getDefaultSources = getDefaultSources;
        Thread runit = new Thread() {            
            public void run() {
            VexFileParser vexData = new VexFileParser();
            vexData.data( _editor.text(), _settings.eliminateCodeStationsCheck() );
            _vexData = vexData;
    //        //  See if the .vex file is making use of multiple format types.  
    //        if ( _vexData.usedModes().size() > 1 )
    //            JOptionPane.showMessageDialog( _this, "Scan data in this .vex file require mulitple modes.\n"
    //                    + "The GUI may or may not handle this situation well.", "Multiple Modes", JOptionPane.WARNING_MESSAGE );            
            _scanStationTimeline.vexData( vexData );
            if ( _eopLock == null )
                _eopLock = new Object();
            synchronized ( _eopLock ) {
                _eopMinTime = null;
                _eopMaxTime = null;
            }
            //  The GUI is assuming here that only one bandwidth exists.
            _bandwidth = vexData.bandwidth();
            //  Build a grid out of the scans found
            _scanGrid.clearButtons();
            _timeLimits.clearButtons();
            if ( vexData.scanList() != null ) {
                for ( Iterator iter = vexData.scanList().iterator(); iter.hasNext(); ) {
                    VexFileParser.Scan scan = (VexFileParser.Scan)iter.next();
                    String tooltip = scan.name + "\n" + scan.source + "\n" +
                            scan.start.get( Calendar.YEAR ) + "-" + scan.start.get( Calendar.DAY_OF_YEAR ) + " (" +
                            scan.start.get( Calendar.MONTH ) + "/" + scan.start.get( Calendar.DAY_OF_MONTH ) + ")  " +
                            String.format( "%02d", scan.start.get( Calendar.HOUR_OF_DAY ) ) + ":" +
                            String.format( "%02d", scan.start.get( Calendar.MINUTE ) ) + ":" + 
                            String.format( "%02d", scan.start.get( Calendar.SECOND ) ) + "\n";
                    for ( Iterator jter = scan.station.iterator(); jter.hasNext(); ) {
                        VexFileParser.ScanStation station = (VexFileParser.ScanStation)jter.next();
                        tooltip += station.wholeString + "\n";
                        //  Find the start and end time of this observation.
                        Calendar startTime = scan.start;
                        startTime.add( Calendar.SECOND, station.delay );
                        Calendar endTime = scan.start;
                        endTime.add( Calendar.SECOND, station.delay + station.duration );
                        //  Check these against the minimum and maximum times for the whole set of scans
                        //  described by this .vex file.
                        synchronized ( _eopLock ) {
                            if ( _eopMinTime == null ) {
                                _eopMinTime = startTime;
                                _eopMaxTime = endTime;
                            }
                            else {
                                if ( startTime.before( _eopMinTime ) ) {
                                    _eopMinTime = startTime;
                                }
                                if ( endTime.after( _eopMaxTime ) ) {
                                    _eopMaxTime = endTime;
                                }
                            }
                        }
                    }
                    //  Add a new button to the grid, then attach the associated data to it so they
                    //  can be consulted later.
                    ButtonGrid.GridButton newButton = _scanGrid.addButton( scan.name, tooltip, true );
                    newButton.data( scan );
                    _timeLimits.addButton( newButton, scan );
                }
            }
            //  Set the limits on the time limit panel.  We expand this a few percent beyond the
            //  actual limits to make sure everything is contained in the original timeline.
            synchronized ( _eopLock ) {
                int adj = (int)( ( _eopMaxTime.getTimeInMillis() - _eopMinTime.getTimeInMillis() ) / 50 );
                _eopMinTime.add( Calendar.MILLISECOND, -adj );
                _eopMaxTime.add( Calendar.MILLISECOND, adj );
                _timeLimits.limits( _eopMinTime, _eopMaxTime );
                _scanStationTimeline.limits( _eopMinTime, _eopMaxTime );
            }
            //  Add panels of information about each antenna.  First we clear the existing
            //  lists of antennas (these might have been formed the last time the .vex file
            //  was parsed).
            _antennaPane.clear();
            if ( _antennaList != null ) {
                synchronized ( _antennaList ) {
                    _antennaList.clear();
                }
            }
            if ( vexData.stationList() != null ) {
                for ( Iterator<VexFileParser.Station> iter = vexData.stationList().iterator(); iter.hasNext(); ) {
                    VexFileParser.Station station = iter.next();
                    //  Make a new panel to hold this station.
                    if ( _antennaList == null )
                        _antennaList = new AntennaList();
                    StationPanel panel = new StationPanel( station, _name.getText(), _settings );
                    if ( _getDefaultSources )
                        panel.defaultDataSources();
                    panel.vexData( _vexData );
                    panel.addChangeListener( new ActionListener() {
                        public void actionPerformed( ActionEvent evt ) {
                            vexDataChange();
                        }
                    } );
                    _antennaPane.addNode( panel );
                    synchronized ( _antennaList ) {
                        _antennaList.add( panel );
                    }
                    //  Add the appropriate site information for this station.
                    for ( Iterator<VexFileParser.Site> jter = vexData.siteList().iterator(); jter.hasNext(); ) {
                        VexFileParser.Site site = jter.next();
                        if ( site.name.equalsIgnoreCase( station.site ) )
                            panel.addSiteInformation( site );
                    }
                    //  Same for antenna information.
                    for ( Iterator<VexFileParser.Antenna> jter = vexData.antennaList().iterator(); jter.hasNext(); ) {
                        VexFileParser.Antenna antenna = jter.next();
                        if ( antenna.name.equalsIgnoreCase( station.antenna ) )
                            panel.addAntennaInformation( antenna );
                    }
                }
            }
            //  Add panels of information about each source.
            _sourcePane.clear();
            if ( _sourcePanelList != null ) {
                synchronized( _sourcePanelList ) {
                    _sourcePanelList.clear();
                }
            }
            if ( vexData.sourceList() != null ) {
                //  First add a kind of "header" panel that includes select and deselect
                //  buttons.
                IndexedPanel sourceButtonPanel = new IndexedPanel( "" );
                sourceButtonPanel.closedHeight( 35 );
                sourceButtonPanel.openHeight( 35 );
                sourceButtonPanel.open( true );
                sourceButtonPanel.darkTitleBar( false );
                sourceButtonPanel.drawFrame( false );
                sourceButtonPanel.resizeOnTopBar( false );
                sourceButtonPanel.alwaysOpen( true );
                sourceButtonPanel.noArrow( true );
                JButton selectAllSources = new JButton( "Select All" );
                selectAllSources.setBounds( 10, 5, 110, 25 );
                selectAllSources.addActionListener( new ActionListener() {
                    public void actionPerformed( ActionEvent e ) {
                        selectAllSources( true );
                    }
                });
                sourceButtonPanel.add( selectAllSources );
                JButton deselectAllSources = new JButton( "Deselect All" );
                deselectAllSources.setBounds( 125, 5, 110, 25 );
                deselectAllSources.addActionListener( new ActionListener() {
                    public void actionPerformed( ActionEvent e ) {
                        selectAllSources( false );
                    }
                });
                sourceButtonPanel.add( deselectAllSources );
                //  For each antenna ("station") add a header to this panel.  These will define
                //  the grid showing which source were observed with which stations.  These
                //  are NOT user-changeable items.
                int xOffset = 300;
                for ( Iterator<VexFileParser.Station> iter = vexData.stationList().iterator(); iter.hasNext(); ) {
                    VexFileParser.Station station = iter.next();
                    JLabel thisLabel = new JLabel( station.name );
                    thisLabel.setHorizontalAlignment( JLabel.CENTER );
                    thisLabel.setBounds( xOffset, 5, 75, 25 );
                    xOffset += 80;
                    sourceButtonPanel.add( thisLabel );
                }
                _sourcePane.addNode( sourceButtonPanel );
                //  Now add the individual sources to the source panel.
                for ( Iterator<VexFileParser.Source> iter = vexData.sourceList().iterator(); iter.hasNext(); ) {
                    VexFileParser.Source source = iter.next();
                    //  Make sure this source is used in one of the scans!  If not, we
                    //  ignore it, as the user should have no interest in it.
                    boolean keepSource = false;
                    ArrayList<VexFileParser.ScanStation> stationsUsed = null;
                    for ( Iterator<VexFileParser.Scan> jter = vexData.scanList().iterator(); jter.hasNext() && !keepSource; ) {
                        VexFileParser.Scan scan = jter.next();
                        if ( scan.source.equalsIgnoreCase( source.name ) ) {
                            keepSource = true;
                            stationsUsed = scan.station;
                        }
                    }
                    if ( keepSource ) {
                        if ( _sourcePanelList == null )
                            _sourcePanelList = new ArrayList<SourcePanel>();
                        SourcePanel panel = new SourcePanel( source, _vexData, _settings );
                        synchronized( _sourcePanelList ) {
                            _sourcePanelList.add( panel );
                        }
                        panel.addChangeListener( new ActionListener() {
                            public void actionPerformed( ActionEvent evt ) {
                                vexDataChange();
                            }
                        } );
                        _sourcePane.addNode( panel );
                    }
                }
            }
            //  Add panels containing EOP data from the .vex file and a location for EOP
            //  data from sources specified in the Settings area.
            _eopPane.clear();
            if ( _vexEOPPane != null )
                _vexEOPPane.clear();
            if ( _newEOPPane != null )
                _newEOPPane.clear();
            _vexEOPUseCheck = null;
            _newEOPUseCheck = null;
            if ( vexData.eopList() != null ) {
                //  This sets up the "container" panel for a list of EOP data from the .vex file.
                IndexedPanel vexEOPPanel = new IndexedPanel( "From .vex File" );
                vexEOPPanel.open( false );
                vexEOPPanel.closedHeight( 20 );
                vexEOPPanel.darkTitleBar( false );
                vexEOPPanel.drawFrame( false );
                _vexEOPUseCheck = new JCheckBox( "" );
                _vexEOPUseCheck.setBounds( 250, 2, 18, 16 );
                _vexEOPUseCheck.setSelected( true );
                _vexEOPUseCheck.setToolTipText( "Use the EOP data from the .vex file." );
                _vexEOPUseCheck.addActionListener( new ActionListener() {
                    public void actionPerformed( ActionEvent evt ) {
                        selectEOPSource( _vexEOPUseCheck );
                    }
                } );
                vexEOPPanel.add( _vexEOPUseCheck );
                vexEOPPanel.resizeOnTopBar( true );
                _eopPane.addNode( vexEOPPanel );
                _vexEOPPane = new NodeBrowserScrollPane( false );
                _vexEOPPane.drawFrame( false );
                _vexEOPPane.setLevel( 2 );
                _vexEOPPane.respondToResizeEvents( true );
                vexEOPPanel.addScrollPane( _vexEOPPane );
                boolean doHeader = true;
                //  Then add an IndexedPanel item for each EOP data item.
                for ( Iterator<VexFileParser.EOP> iter = vexData.eopList().iterator(); iter.hasNext(); ) {
                    VexFileParser.EOP eop = iter.next();
                    //  Put in a header line explaining what these things are (but only
                    //  if we are on the first line).
                    if ( doHeader ) {
                        doHeader = false;
                        IndexedPanel eopHeaderPanel = new IndexedPanel( "" );
                        eopHeaderPanel.openHeight( 20 );
                        eopHeaderPanel.closedHeight( 20 );
                        eopHeaderPanel.setLevel( 3 );
                        eopHeaderPanel.alwaysOpen( true );
                        eopHeaderPanel.noArrow( true );
                        eopHeaderPanel.drawFrame( false );
                        _vexEOPPane.addNode( eopHeaderPanel );
                        JLabel mjd = new JLabel( "MJD" );
                        mjd.setBounds( 90, 2, 120, 16 );
                        eopHeaderPanel.add( mjd );
                        JLabel tai = new JLabel( "TAI-UTC" );
                        tai.setToolTipText( "Leap second count" );
                        tai.setBounds( 230, 2, 100, 16 );
                        eopHeaderPanel.add( tai );
                        JLabel ut1 = new JLabel( "UT1-UTC" );
                        ut1.setToolTipText( "Earth rotation phase" );
                        ut1.setBounds( 350, 2, 100, 16 );
                        eopHeaderPanel.add( ut1 );
                        JLabel xp = new JLabel( "X Pole" );
                        xp.setToolTipText( "X component of spin axis offset" );
                        xp.setBounds( 470, 2, 100, 16 );
                        eopHeaderPanel.add( xp );
                        JLabel yp = new JLabel( "Y Pole" );
                        yp.setToolTipText( "Y component of spin axis offset" );
                        yp.setBounds( 590, 2, 100, 16 );
                        eopHeaderPanel.add( yp );
                    }
                    //  Add data and labels.  None of these can be changed, however a bunch
                    //  of them have to be converted to the units that DiFX uses.
                    //  Generate a reference date for this entry...
                    JulianCalendar theDate = new JulianCalendar();
                    theDate.clear();
                    theDate.set( Calendar.YEAR, Integer.parseInt( eop.eop_ref_epoch.trim().substring( 0, 4 ) ) );
                    theDate.set( Calendar.DAY_OF_YEAR, Integer.parseInt( eop.eop_ref_epoch.trim().substring( 5, eop.eop_ref_epoch.trim().indexOf( 'd' ) ) ) );
                    //  Find the number of points in this set.  Accomodate the possibility
                    //  that it is not specified.
                    Integer numPts = eop.num_eop_points;
                    if ( numPts == null )
                        numPts = 1;
                    //  Loop through the total number of points, creating a new line for each.
                    for ( int i = 0; i < numPts; ++i ) {
                        IndexedPanel eopPanel = new IndexedPanel( "" );
                        eopPanel.openHeight( 20 );
                        eopPanel.closedHeight( 20 );
                        eopPanel.setLevel( 3 );
                        eopPanel.alwaysOpen( true );
                        eopPanel.noArrow( true );
                        eopPanel.drawFrame( false );
                        _vexEOPPane.addNode( eopPanel );
                        //  Use the modified Julian Day for this date.
                        JLabel mjd = new JLabel( (int)(theDate.mjd()) + 
                                " (" + theDate.get( Calendar.YEAR ) + "/" + theDate.get( Calendar.DAY_OF_YEAR ) + ")" );
                        mjd.setBounds( 90, 2, 120, 16 );
                        eopPanel.add( mjd );
                        //  Add the date interval between points for the next point (there may not be one).
                        theDate.add( Calendar.HOUR, Integer.parseInt( eop.eop_interval.trim().substring( 0, eop.eop_interval.trim().indexOf( ' ' ) ) ) );
                        JLabel tai = new JLabel( eop.tai_utc.trim().substring( 0, eop.tai_utc.trim().indexOf( ' ' ) ) + " sec" );
                        tai.setBounds( 230, 2, 100, 16 );
                        eopPanel.add( tai );
                        JLabel ut1 = new JLabel( eop.ut1_utc.get(i).trim().substring( 0, eop.ut1_utc.get(i).trim().indexOf( ' ' ) ) + " sec" );
                        ut1.setBounds( 350, 2, 100, 16 );
                        eopPanel.add( ut1 );
                        JLabel xp = new JLabel( eop.x_wobble.get(i).trim().substring( 0, eop.x_wobble.get(i).trim().indexOf( ' ' ) ) + " asec" );
                        xp.setBounds( 470, 2, 100, 16 );
                        eopPanel.add( xp );
                        JLabel yp = new JLabel( eop.y_wobble.get(i).trim().substring( 0, eop.y_wobble.get(i).trim().indexOf( ' ' ) ) + " asec" );
                        yp.setBounds( 590, 2, 100, 16 );
                        eopPanel.add( yp );
                    }
                }
            }
            //  Generate EOP data from the EOP source (if available).  To do this, we first
            //  need to find the date of these observations (I'm using the midpoint of the
            //  observations, but it probably doesn't matter too much).
            JulianCalendar midTime = new JulianCalendar();
            synchronized ( _eopLock ) {
                if ( _eopMinTime != null && _eopMaxTime != null )
                    midTime.setTimeInMillis( _eopMinTime.getTimeInMillis() + ( _eopMaxTime.getTimeInMillis() - _eopMinTime.getTimeInMillis() ) / 2 );
            }
            _newEOP = _settings.eopData( midTime.julian() - 2.5, midTime.julian() + 2.5 );
            if ( _newEOP != null && _newEOP.size() > 0 ) {
                IndexedPanel newEOPPanel = new IndexedPanel( "Updated From Source" );
                newEOPPanel.open( false );
                newEOPPanel.closedHeight( 20 );
                newEOPPanel.darkTitleBar( false );
                newEOPPanel.drawFrame( false );
                _newEOPUseCheck = new JCheckBox( "" );
                _newEOPUseCheck.setBounds( 250, 2, 18, 16 );
                if ( _vexEOPUseCheck != null )
                    _vexEOPUseCheck.setSelected( false );
                _newEOPUseCheck.setSelected( true );
                _newEOPUseCheck.setToolTipText( "Use updated EOP data from source." );
                _newEOPUseCheck.addActionListener( new ActionListener() {
                    public void actionPerformed( ActionEvent evt ) {
                        selectEOPSource( _newEOPUseCheck );
                    }
                } );
                newEOPPanel.add( _newEOPUseCheck );
                newEOPPanel.resizeOnTopBar( true );
                _eopPane.addNode( newEOPPanel );
                _newEOPPane = new NodeBrowserScrollPane( false );
                _newEOPPane.drawFrame( false );
                _newEOPPane.setLevel( 2 );
                _newEOPPane.respondToResizeEvents( true );
                newEOPPanel.addScrollPane( _newEOPPane );
                replaceRemoteEOPData();
            }
            vexDataChange();
            //  Add a "listener" to pick up changes in the EOP data.  These will cause
            //  the EOP table to be rebuilt.
            _settings.eopChangeListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    replaceRemoteEOPData();
                    produceV2dFile();
                }
            } );
            }
        };
        //  We only parse the vex data within a thread if we are locating default
        //  data sources (as that process can be time consuming).  If we aren't doing
        //  that there are some good reasons NOT to run it as a thread.
        if ( _getDefaultSources )
            runit.start();
        else
            runit.run();
    }
    
    /*
     * Turn on or off the selection of all sources in the source list.
     */
    void selectAllSources( boolean on ) {
        synchronized( _sourcePanelList ) {
            //  This turns on/off all of the buttons.
            for ( Iterator<SourcePanel> iter = _sourcePanelList.iterator(); iter.hasNext(); )
                iter.next().use( on );
            //  This turns on/off the stations within the scans associated with the buttons.
            for ( Iterator<VexFileParser.Scan> iter = _vexData.scanList().iterator(); iter.hasNext(); ) {
            VexFileParser.Scan scan = iter.next();
                for ( Iterator<VexFileParser.ScanStation> iter2 = scan.station.iterator(); iter2.hasNext(); )
                    iter2.next().omitFlag = !on;
            }
        }
        vexDataChange();
    }
    
    /*
     * Fill the table containing EOP data from external sources (not from the .vex file)
     * using current data.  This is done when the table is created initially, or when there
     * are changes to the data.
     */
    protected void replaceRemoteEOPData() {
        //  This has to be here because a callback is triggered before this pane is built.
        if ( _eopLock == null )
            _eopLock = new Object();
        if ( _newEOPPane == null || _eopMinTime == null || _eopMaxTime == null )
            return;
        _newEOPPane.clear();
        //  Generate EOP data from the EOP source (if available).  To do this, we first
        //  need to find the date of these observations (I'm using the midpoint of the
        //  observations, but it probably doesn't matter too much).
        JulianCalendar midTime = new JulianCalendar();
        synchronized ( _eopLock ) {
            if ( _eopMinTime != null && _eopMaxTime != null )
                midTime.setTimeInMillis( _eopMinTime.getTimeInMillis() + ( _eopMaxTime.getTimeInMillis() - _eopMinTime.getTimeInMillis() ) / 2 );
        }
        synchronized ( _eopLock ) {
            if ( _eopMinTime != null && _eopMaxTime != null ) {
                _newEOP = _settings.eopData( midTime.julian() - 2.5, midTime.julian() + 2.5 );
            }
        }
        boolean doHeader = true;
        //  Then add an IndexedPanel item for each EOP data item.
        for ( Iterator<SystemSettings.EOPStructure> iter = _newEOP.iterator(); iter.hasNext(); ) {
            SystemSettings.EOPStructure eop = iter.next();
            //  Put in a header line explaining what these things are (but only
            //  if we are on the first line).
            if ( doHeader ) {
                doHeader = false;
                IndexedPanel eopHeaderPanel = new IndexedPanel( "" );
                eopHeaderPanel.openHeight( 20 );
                eopHeaderPanel.closedHeight( 20 );
                eopHeaderPanel.setLevel( 3 );
                eopHeaderPanel.alwaysOpen( true );
                eopHeaderPanel.noArrow( true );
                eopHeaderPanel.drawFrame( false );
                _newEOPPane.addNode( eopHeaderPanel );
                JLabel mjd = new JLabel( "MJD" );
                mjd.setBounds( 90, 2, 120, 16 );
                eopHeaderPanel.add( mjd );
                JLabel tai = new JLabel( "Leap Seconds" );
                tai.setToolTipText( "Leap second count" );
                tai.setBounds( 230, 2, 100, 16 );
                eopHeaderPanel.add( tai );
                JLabel ut1 = new JLabel( "UT1-UTC" );
                ut1.setToolTipText( "Earth rotation phase" );
                ut1.setBounds( 350, 2, 100, 16 );
                eopHeaderPanel.add( ut1 );
                JLabel xp = new JLabel( "X Pole" );
                xp.setToolTipText( "X component of spin axis offset" );
                xp.setBounds( 470, 2, 100, 16 );
                eopHeaderPanel.add( xp );
                JLabel yp = new JLabel( "Y Pole" );
                yp.setToolTipText( "Y component of spin axis offset" );
                yp.setBounds( 590, 2, 100, 16 );
                eopHeaderPanel.add( yp );
            }
            IndexedPanel eopPanel = new IndexedPanel( "" );
            eopPanel.openHeight( 20 );
            eopPanel.closedHeight( 20 );
            eopPanel.setLevel( 3 );
            eopPanel.alwaysOpen( true );
            eopPanel.noArrow( true );
            eopPanel.drawFrame( false );
            _newEOPPane.addNode( eopPanel );
            //  Add data and labels.  None of these can be changed, however a bunch
            //  of them have to be converted to the units that DiFX uses.
            JulianCalendar theDate = new JulianCalendar();
            theDate.clear();
            theDate.julian( eop.date );
            //  Use the modified Julian Day for this date.
            JLabel mjd = new JLabel( (int)(theDate.mjd()) + 
                    " (" + theDate.get( Calendar.YEAR ) + "/" + theDate.get( Calendar.DAY_OF_YEAR ) + ")" );
            mjd.setBounds( 90, 2, 120, 16 );
            eopPanel.add( mjd );
            JLabel tai = new JLabel( (int)eop.tai_utc + " sec" );
            tai.setBounds( 230, 2, 100, 16 );
            eopPanel.add( tai );
            JLabel ut1 = new JLabel( String.format( "%.6f sec ", eop.ut1_utc ) );
            ut1.setBounds( 350, 2, 100, 16 );
            eopPanel.add( ut1 );
            JLabel xp = new JLabel( String.format( "%.6f asec ", eop.xPole / 10.0 ) );
            xp.setBounds( 470, 2, 100, 16 );
            eopPanel.add( xp );
            JLabel yp = new JLabel( String.format( "%.6f asec ", eop.yPole / 10.0 ) );
            yp.setBounds( 590, 2, 100, 16 );
            eopPanel.add( yp );
        }
    }
    
    protected void selectEOPSource( JCheckBox source ) {
        if ( source == _vexEOPUseCheck ) {
            _vexEOPUseCheck.setSelected( true );
            if ( _newEOPUseCheck != null )
                _newEOPUseCheck.setSelected( false );
        }
        else if ( source == _newEOPUseCheck ) {
            _newEOPUseCheck.setSelected( true );
            if ( _vexEOPUseCheck != null )
                _vexEOPUseCheck.setSelected( false );
        }
        produceV2dFile();
    }

    public String name() { return _name.getText(); }
    public void name( String newVal ) { 
        _name.setText( newVal );
        _nameAsLabel.setText( newVal );
    }
    public Integer number() { return _number.intValue(); }
    public void number( Integer newVal ) { 
        _number.intValue( newVal );
        _numberAsLabel.setText( newVal.toString() );
    }
    public boolean inDataBase() { return _inDataBase.isSelected(); }
    public void inDataBase( boolean newVal ) { 
        _inDataBase.setSelected( newVal );
        _saveInDataBase = newVal;  //  Used to maintain a value in non-edit mode.
    };
    public void id( Integer newVal ) { 
        if ( newVal == null )
            _id.setText( "" );
        else
            _id.setText( newVal.toString() );
    }
    public boolean newExperimentMode() { return _newExperimentMode; }
    /*
     * In "new experiment mode" the name, id, directory, etc of the experiment are editable.
     * If not, they are considered set - they are displayed, but can't be edited.
     */
    public void newExperimentMode( boolean newVal ) { 
        _newExperimentMode = newVal;
        if ( _newExperimentMode ) {
            _name.setVisible( true );
            _nameAsLabel.setVisible( false );
//            _number.setVisible( true );
//            _numberAsLabel.setVisible( false );
            _directory.setVisible( true );
            _directoryAsLabel.setVisible( false );
        }
        else {
            _name.setVisible( false );
            _nameAsLabel.setVisible( true );
//            _number.setVisible( false );
//            _numberAsLabel.setVisible( true );
            _directory.setVisible( false );
            _directoryAsLabel.setVisible( true );
        }
        //  Set up new options for the status list.  We kind of count on this function
        //  being called each time this window is displayed...which ultimately may not
        //  bright.
        Iterator iter = _settings.experimentStatusList().entrySet().iterator();
        String saveStatus = status();
        for ( ; iter.hasNext(); )
            _statusList.addItem( ((SystemSettings.ExperimentStatusEntry)((Map.Entry)iter.next()).getValue()).status );
        _statusList.setVisible( true );
        this.status( saveStatus );
    }
    protected void inDataBaseAction() {
        if ( !_newExperimentMode )
            _inDataBase.setSelected( _saveInDataBase );
        else
            _saveInDataBase = _inDataBase.isSelected();
    }
    public void created( String newVal ) { _created.setText( newVal ); }
    public void status( String newVal ) { 
        _status.setText( newVal );
        for ( int i = 0; i < _statusList.getItemCount(); ++i ) {
            if ( ((String)_statusList.getItemAt( i )).contentEquals( newVal ) ) {
                _statusList.setSelectedIndex( i );
            }
        }
    }
    public String status() { return _status.getText(); }
    public void directory( String newVal ) { 
        _directory.setText( newVal );
        _directoryAsLabel.setText( newVal );
        _keepDirectory = true;
    }
    public String directory() { return _directory.getText(); }
    public void keepDirectory( boolean newVal ) { _keepDirectory = newVal; }
    
    public String passName() { return _passName.getText(); }
    public void passName( String newVal ) { 
        _passName.setText( newVal );
        autoSetPassDirectory();
    }
    
    public String passDirectory() { return _passDirectory.getText(); }
    public void passDirectory( String newVal ) { _passDirectory.setText( newVal ); }
    
    public void keepPassDirectory( boolean newVal ) { _keepPassDirectory = newVal; }
    
    public boolean createPass() { return _createPass.isSelected(); }
    public void createPass( boolean newVal ) { 
        _createPass.setSelected( newVal );
        createPassSetting();
    }
    
    public String passType() {
        return (String)_passTypeList.getItemAt( _passTypeList.getSelectedIndex() );
    }
    
    public void vexFileName( String newVal ) { _vexFileName.setText( newVal ); }
    public String vexFileName() { return _vexFileName.getText(); }
    
    public void v2dFileName( String newVal ) { _v2dFileName.setText( newVal ); }
    public String v2dFileName() { return _v2dFileName.getText(); }
    public String passLogFileName() { return v2dFileName().replace( ".v2d", ".passLog" ); }
    
    /*
     * A change has occurred in the name, which *maybe* should be propogated
     * to the directory, but only if the directory hasn't been previously
     * set (which probably indicates the experiment is new, but you can control
     * it using the "keepDirectory" function).
     */
    protected void nameChangeAction() {
        _nameAsLabel.setText( _name.getText() );
        if ( !_keepDirectory ) {
            directory( _settings.workingDirectory() + "/" + _name.getText() );
            vexFileName( _name.getText() + ".vex" );
            keepDirectory( false );
        }
    }
    protected void directoryChangeAction() {
        keepDirectory( true );
    }
    
    /*
     * Called when the "create pass" checkbox is changed.  This enables or
     * disables related items.
     */
    public void createPassSetting() {
        if ( _createPass.isSelected() ) {
            _passName.setEnabled( true );
            _passTypeList.setEnabled( true );
            _newJobsCheck.setEnabled( true );
            _newJobsCheck.setEnabled( true );
            _currentDataCheck.setEnabled( true );
            _dataLabel.setEnabled( true );
        }
        else {
            _passName.setEnabled( false );
            _passTypeList.setEnabled( false );
            _newJobsCheck.setEnabled( false );
            _newJobsCheck.setEnabled( false );
            _currentDataCheck.setEnabled( false );
            _dataLabel.setEnabled( false );
        }
    }
    
    /*
     * Launch the user's preferred text editor to edit the .vex file.  This involves
     * copying it to a temporary location, editing the copy, then copying it back.
     */
    protected void useMyEditor() {
        new AePlayWave( _settings.guiDocPath() + "/cantdo.wav" ).start();
        JOptionPane.showMessageDialog( this, "That feature has not been implemented." );
    }

    /*
     * Add a name to the current "vex file list".  This is a list of vex file names
     * that the experiment being edited knows about.  The names are listed in a
     * popup menu from which they can be picked.  We create a menu item and give it
     * a callback - all of the callbacks are to the same function.
     */
    public void addVexFileName( String name ) {
        JMenuItem newItem = new JMenuItem( name );
        final String finalName = name;
        newItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                pickVexFile( new String( finalName ) );
            }
        });        
        _previousVexFileMenu.add( newItem );
        _previousVexFileButton.setEnabled( true );
    }
    
    /*
     * This method is called when a "previous" vex file is chosen from the menu.
     */
    public void pickVexFile( String name ) {
        System.out.println( "picked " + name );
    }
    
    /*
     * Provide a list of scan names that have been chosen by the scan grid to
     * be part of the current pass.
     */
    public ArrayList<String> onScans() {
        return _scanGrid.onItems();
    }
    
    /*
     * Called when a change has been made by one of the menus to stations or scans.
     * This redraws all of the menus and forms a new .v2d file.
     */
    public void vexDataChange() {
        if ( _antennaList != null ) {
//            System.out.println( "vexDataChange() in the ExperiementEditor knows there are....\n" );
            synchronized ( _antennaList ) {
                if ( !_antennaList.useList().isEmpty() ) {
                    for ( Iterator<StationPanel> kter = _antennaList.useList().iterator(); kter.hasNext(); ) {
                        StationPanel antenna = kter.next();
//                        System.out.println( antenna.dataStreams().size() + " data streams for antenna " + antenna.name() + " \n" );
                    }
                }
            }
        }

        //  This causes changes to be shown in the scan/station timeline.
        if ( _scanStationTimeline != null )
            _scanStationTimeline.redraw();
        //  Change the source panel list.
        if ( _sourcePanelList != null ) {
            synchronized( _sourcePanelList ) {
                for ( Iterator<SourcePanel> iter = _sourcePanelList.iterator(); iter.hasNext(); )
                    iter.next().redraw();
            }
        }
        //  Change the scan grid.  Each scan is examined for the "omit" flag, which
        //  indicates it has been deliberately turned off.  Then its accompanying stations
        //  are looked at - if there are not enough to form a baseline the scan is
        //  turned off.
        for ( Iterator<ButtonGrid.GridButton> iter = _scanGrid.buttonList().iterator(); iter.hasNext(); ) {
            ButtonGrid.GridButton button = iter.next();
            VexFileParser.Scan scan = (VexFileParser.Scan)button.data();
            String tooltip = "<<bold>>" + scan.name + "<</bold>>\n" + scan.source + "\n" +
                    scan.start.get( Calendar.YEAR ) + "-" + scan.start.get( Calendar.DAY_OF_YEAR ) + " (" +
                    scan.start.get( Calendar.MONTH ) + "/" + scan.start.get( Calendar.DAY_OF_MONTH ) + ")  " +
                    String.format( "%02d", scan.start.get( Calendar.HOUR_OF_DAY ) ) + ":" +
                    String.format( "%02d", scan.start.get( Calendar.MINUTE ) ) + ":" + 
                    String.format( "%02d", scan.start.get( Calendar.SECOND ) ) + "\n";
            boolean on = true;
            if ( scan.omitFlag )
                on = false;
            int stationCount = 0;
            for ( Iterator<VexFileParser.ScanStation> iter2 = scan.station.iterator(); iter2.hasNext(); ) {
                VexFileParser.ScanStation station = iter2.next();
                if ( !station.omitFlag )
                    ++stationCount;
                tooltip += "<<fixed>>" + station.wholeString;
                if ( station.omitFlag )
                    tooltip += "<<red>> station omitted<</color>>\n";
                else
                    tooltip += "\n";
            }
            if ( stationCount < 2 )
                on = false;
            button.setToolTipText( tooltip );
            button.on( on );
        }
        //  Run through the list of scan grid buttons, figuring out which should be
        //  on and off.
        produceV2dFile();
    }
    
    /*
     * Check the scan selections against antennas that have been selected.  If less
     * than two antennas required for the scan have been selected, the scan is switched
     * off.  This should probably only be called after changes to the antenna
     * selections.
     */
    public void checkScansAgainstAntennas() {
        for ( Iterator<String> iter = _scanGrid.onItems().iterator(); iter.hasNext(); ) {
            String buttonName = iter.next();
            //  Check any scan button that is already on against chosen antennas.
            VexFileParser.Scan scan = (VexFileParser.Scan)_scanGrid.buttonData( buttonName );
            if ( scan != null ) {
                //  Little thread thing to make sure there is time to collect the "scan" information.
                int count = 10;
                while ( scan.station == null && count != 0 ) {
                    --count;
                    try { Thread.sleep( 100 ); } catch ( Exception e ) {}
                }
                int matchingStations = 0;
                for ( Iterator jter = scan.station.iterator(); jter.hasNext(); ) {
                    VexFileParser.ScanStation station = (VexFileParser.ScanStation)jter.next();
                    boolean stationMatch = false;
                    if ( _antennaList != null ) {
                        synchronized ( _antennaList ) {
                            if ( !_antennaList.useList().isEmpty() ) {
                                for ( Iterator<StationPanel> kter = _antennaList.useList().iterator(); kter.hasNext(); ) {
                                    StationPanel antenna = kter.next();
                                    if ( antenna.name().equalsIgnoreCase( station.name ) ) {
                                        stationMatch = true;
                                    }
                                }
                            }
                        }
                    }
                    if ( stationMatch )
                        matchingStations += 1;
                }
                if ( matchingStations >= 2 )
                    _scanGrid.namedButtonOn( buttonName, true );
                else
                    _scanGrid.namedButtonOn( buttonName, false );
            }
        }
    }
    
    /*
     * Check each scan against the sources that have been selected.  If the scan does
     * not include the source, it will be removed.
     */
    public void checkScansAgainstSources() {
        try {
            for ( Iterator<ButtonGrid.GridButton> iter = _scanGrid.buttonList().iterator(); iter.hasNext(); ) {
                ButtonGrid.GridButton button = iter.next();
                if ( button.on() ) {
                    VexFileParser.Scan scan = (VexFileParser.Scan)button.data();
                    boolean _sourceFound = false;
                    for ( Iterator<BrowserNode> jter = _sourcePane.browserTopNode().childrenIterator(); jter.hasNext() && !_sourceFound; ) {
                        //  Eliminate the button panel using the class cast exception.
                        try {
                            SourcePanel source = (SourcePanel)jter.next();
                            if ( scan != null && scan.source != null && source != null && scan.source.equalsIgnoreCase( source.name() ) && source.use() )
                                _sourceFound = true;
                        } catch ( java.lang.ClassCastException e ) {
                        } catch ( java.util.ConcurrentModificationException e ) {}
                    }
                    if ( !_sourceFound )
                        button.on( false );
                }
            }
        } catch ( java.util.ConcurrentModificationException e ) {}
    }
    
    /*
     * Use the current settings to produce a .v2d "file".  This is stored in the
     * v2d editor.
     */
    public void produceV2dFile() {
        //checkScansAgainstSources();
        //  Create a v2d file name, if one hasn't been used already....we use the
        //  .vex file name (if it exists).
        if ( _v2dFileName.getText().length() == 0 ) {
            String str = "";
            if ( vexFileName().length() > 0 )
                str = vexFileName().substring( 0, vexFileName().lastIndexOf( '.' ) ) + ".v2d";
            else
                str = name() + ".v2d";
            //  Remove underscores in the .v2d file name...these have special meaning
            //  to vex2difx and are thus illegal.
            str = str.replace( "_", "" );
            _v2dFileName.setText( str );
        }
        V2dFileParser v2dFileParser = new V2dFileParser( _startingV2dFileContent );
        v2dFileParser.headerComment( "#   -- Configuration file for vex2difx --\n"
                + "#   This file was automatically generated by the DiFX GUI\n"
                + "#   Created: " + new SimpleDateFormat( "MMMMM dd, yyyy hh:mm:ss z" ).format( new Date() ) + "\n"
                + "#   By:      " + System.getProperty("user.name") + "\n\n" );                    
        
        //  The .vex file - the only required parameter.  We give the full path
        //  to the file.
        v2dFileParser.vexFile( _vexFileName.getText() ); 
        
        //  This keeps vex2difx from splitting up jobs in an unexpected way
        v2dFileParser.maxGap( 180000.0 );
        v2dFileParser.maxLength( 360000.0 );
        
        //  Whether or not we should split into one scan per job
        v2dFileParser.singleScan( !_singleInputFileCheck.isSelected() );
        
        //  The base name of input files - we only do this if the user has set it.
        if ( _inputFileBaseName.getText() != null && !_inputFileBaseName.getText().trim().contentEquals( "" ) )
            v2dFileParser.jobSeries( _inputFileBaseName.getText() );
        
        //  This is where vex2difx will start numbering things.
        v2dFileParser.startSeries( _inputFileSequenceStart.intValue() );
        
        //  Setup section contains things from the "correlation parameters" settings.
        v2dFileParser.setup( "normalSetup" );
        if ( _tIntApply.isSelected() )
            v2dFileParser.setupTInt( "normalSetup", _tInt.value() );
        else
            v2dFileParser.setupTInt( "normalSetup", null );
        if ( _fftSpecResApply.isSelected() )
            v2dFileParser.setupFFTSpecRes( "normalSetup", _fftSpecRes.value() );
        else
            v2dFileParser.setupFFTSpecRes( "normalSetup", null );
        if ( _specResApply.isSelected() )
            v2dFileParser.setupSpecRes( "normalSetup", _specRes.value() );
        else
            v2dFileParser.setupSpecRes( "normalSetup", null );
        if ( _subintNSApply.isSelected() )
            v2dFileParser.setupSubintNS( "normalSetup", _subintNS.intValue() );
        else
            v2dFileParser.setupSubintNS( "normalSetup", null );
        if ( _doPolarApply.isSelected() )
            v2dFileParser.setupDoPolar( "normalSetup", _doPolar.isSelected() );
        else
            v2dFileParser.setupDoPolar( "normalSetup", null );
        if ( _strideLengthApply.isSelected() )
            v2dFileParser.setupStrideLength( "normalSetup", _strideLength.intValue() );
        else
            v2dFileParser.setupStrideLength( "normalSetup", null );
        if ( _xmacLengthApply.isSelected() )
            v2dFileParser.setupXmacLength( "normalSetup", _xmacLength.intValue() );
        else
            v2dFileParser.setupXmacLength( "normalSetup", null );
        if ( _bufferedFFTsApply.isSelected() )
            v2dFileParser.setupNumBufferedFFTs( "normalSetup", _bufferedFFTs.intValue() );
        else
            v2dFileParser.setupNumBufferedFFTs( "normalSetup", null );
        
        //  Produce a list of the scans we want to include, or indicate that all should
        //  be used.  The "all" specification is used either when we are actually using
        //  all of the scans in the "source" .vex file, or when the "excise unused scans" setting
        //  is set.  In the latter case any unused scans will be removed from the .vex
        //  file.
        String scanList = "";
        int usedScanCount = 0;
        int totalScanCount = 0;
        //  Count how many scans are used and create a string of them.
        if ( _vexData != null && _vexData.scanList() != null ) {
            for ( Iterator<VexFileParser.Scan> iter = _vexData.scanList().iterator(); iter.hasNext(); ) {
                VexFileParser.Scan scan = iter.next();
                //  Make sure the scan is not omitted.
                if ( !scan.omitFlag ) {
                    //  Make sure it has sufficient stations to form a baseline.
                    int stationCount = 0;
                    for ( Iterator<VexFileParser.ScanStation> iter2 = scan.station.iterator(); iter2.hasNext(); ) {
                        VexFileParser.ScanStation station = iter2.next();
                        if ( !station.omitFlag )
                            ++stationCount;
                    }
                    if ( stationCount > 1 ) {
                        ++usedScanCount;
                        scanList += scan.name;
                        if ( iter.hasNext() )
                            scanList += ",";
                    }
                }
                ++totalScanCount;
            }
        }
        //  Add this information to the v2d output.
        if ( usedScanCount > 0 ) {
            v2dFileParser.rule( "scansubset" );
            if ( usedScanCount == totalScanCount || _settings.exciseUnusedScansCheck() )
                v2dFileParser.ruleScan( "scansubset", "*" );
            else
                v2dFileParser.ruleScan( "scansubset", scanList );
            v2dFileParser.ruleSetup( "scansubset", "normalSetup" );
        }
        
        //  Describe specifics for each used antenna...data source, etc.  The following
        //  hash map allows us to locate the machines that each file source originated
        //  from.  This is needed when running jobs.
        if ( _fileToNodeMap == null )
            _fileToNodeMap = new HashMap<String,String>();
        _fileToNodeMap.clear();
        if ( _antennaList != null ) {
            synchronized ( _antennaList ) {
                if ( _antennaList != null && !_antennaList.useList().isEmpty() ) {
                    for ( Iterator<StationPanel> iter = _antennaList.useList().iterator(); iter.hasNext(); ) {
                        StationPanel antenna = iter.next();
                        if ( antenna.use() ) {
                            v2dFileParser.antenna( antenna.name() );
                            v2dFileParser.antennaPhaseCalInt( antenna.name(), antenna.phaseCalInt() );
                            v2dFileParser.antennaToneSelection( antenna.name(), antenna.toneSelection() );
                            if ( antenna.useVsn() ) {
                                v2dFileParser.antennaVsn( antenna.name(), antenna.vsnSource() );
                            }
                            else if ( antenna.useFile() ) {
                                ArrayList<String> fileList = antenna.fileList();
                                if ( fileList.size() > 0 ) {
                                    if ( antenna.useFileList() ) {
                                        v2dFileParser.antennaFileList( antenna.name(), antenna.fileListName() );
                                    }
                                    else {
                                        for ( Iterator<String> jter = fileList.iterator(); jter.hasNext(); ) {
                                            String filename = jter.next();
                                            v2dFileParser.antennaFile( antenna.name(), filename );
                                            _fileToNodeMap.put( filename, antenna.machineForFile( filename ) );
                                        }
                                    }
                                }
                            }
                            else if ( antenna.useEVLBI() )
                                v2dFileParser.antennaNetworkPort( antenna.name(), antenna.networkPort() );
                            else if ( antenna.useFake() )
                                v2dFileParser.antennaFake( antenna.name(), true );
                            //  Position changes.
                            if ( antenna.positionXChange() )
                                v2dFileParser.antennaX( antenna.name(), antenna.positionX() );
                            if ( antenna.positionYChange() )
                                v2dFileParser.antennaY( antenna.name(), antenna.positionY() );
                            if ( antenna.positionZChange() )
                                v2dFileParser.antennaZ( antenna.name(), antenna.positionZ() );
                            //  Clock settings.
                            if ( antenna.deltaClockChange() )
                                v2dFileParser.antennaDeltaClock( antenna.name(), antenna.deltaClock() );
                        }
                    }
                }
            }
        }
        
        //  Add EOP data if the user has requested it (this will normally be the case).
        if ( _newEOPUseCheck != null && _newEOPUseCheck.isSelected() && _newEOP.size() > 0 ) {
            for ( Iterator<SystemSettings.EOPStructure> iter = _newEOP.iterator(); iter.hasNext(); ) {
                SystemSettings.EOPStructure eop = iter.next();
                JulianCalendar theDate = new JulianCalendar();
                theDate.clear();
                theDate.julian( eop.date );
                //  Use the modified Julian Day for this date.
                v2dFileParser.eop( String.format( "%d", (int)theDate.mjd() ), 
                        eop.tai_utc, 
                        eop.ut1_utc, 
                        eop.xPole / 10.0, 
                        eop.yPole / 10.0 );
            }
            _deleteEOPFromVex = true;
        }
        else
            _deleteEOPFromVex = false;
        _v2dEditor.text( v2dFileParser.content() );
        
    }
    
    /*
     * Using the "file to node" map, find the data source (node name) associated
     * with a file.
     */
    public String nodeForFile( String filename ) {
        return _fileToNodeMap.get( filename );
    }
    
    /*
     * Return a string containing the .v2d file.
     */
    public String v2dFile() {
        return _v2dEditor.text();
    }
    
    /*
     * Actions performed when the "apply" button is hit.
     */
    protected void applyButtonAction() {
        
        if ( _operationRunning ) {
            _operationCancelled = true;
            _operationRunning = false;
            _okButton.setText( "Apply" );
        }
        else {
            _operationCancelled = false;
            _operationRunning = true;
            _okButton.setText( "Cancel" );
            ApplyThread app = new ApplyThread();
            app.start();
        }
        
    }
    
    //--------------------------------------------------------------------------
    //  Run only the calc process on the jobs specified by name (the name can
    //  contain wildcard characters).
    //--------------------------------------------------------------------------
    public void runCalcOnly( PassNode passNode, String name ) {
        ApplyThread app = new ApplyThread();
        app.calcOnly( passNode, name );
        app.start();
    }
    
    //--------------------------------------------------------------------------
    //  This thread can be used to create a completely new experiment, or simply
    //  to run calc on an existing experiment.
    //--------------------------------------------------------------------------
    protected class ApplyThread extends Thread {
        
        protected boolean _calcOnly;
        protected PassNode _passNode;
        protected String _name;
        
        public void calcOnly( PassNode passNode, String name ) {
            _calcOnly = true;
            _passNode = passNode;
            _name = name;
        }
        
        public void run() {
            
            if ( _calcOnly ) 
                calcOnlyStart();
            else
                fullCreateStart();
            
        }
        
        //----------------------------------------------------------------------
        //  Go through the complete creation of a new experiment.
        //----------------------------------------------------------------------
        protected void fullCreateStart() {
            
            boolean continueRun = true;
        
            //  Attempt a connection to the database.  We do this each time apply is
            //  hit so we can respond to changed circumstances (database is suddenly there,
            //  or suddenly not available).
            QueueDBConnection db = null;
            if ( _settings.useDatabase() ) {
                db = new QueueDBConnection( _settings );
                if ( !db.connected() )
                    db = null;
            }
            
            //  Create a new v2d file parser.  This is added to each job as it is
            //  created.
            _v2dFileParser = new V2dFileParser( _v2dEditor.text() );
            findScansInJobs();
            
            //  Create a "file read queue" which buffers requests for file transfers
            //  to/from the guiServer such that not too many occur simultaneously.
            //_fileReadQueue = new FileReadQueue();
            //_fileReadQueue.start();

            //  If this is a new experiment, create a node for it and fill it with
            //  appropriate data.  Also, create directories on the DiFX host and add it
            //  to the database (if it is available).
            if ( _thisExperiment == null ) {
                _statusLabel.setText( "creating experiment \"" + name() + "\"" );
                _thisExperiment = new ExperimentNode( name(), _settings );
                //  Let the experiment know where this editor is so it can bring it up in future.
                _thisExperiment.editor( _this );
                Integer newExperimentId = 1;
                String creationDate = _created.getText();
                //  Only add the item to the database if the user has requested it.
                if ( inDataBase() ) {
                    if ( db != null ) {
                        _statusLabel.setText( "adding experiment to database" );
                        db.newExperiment( name(), number(), _settings.experimentStatusID( status() ),
                                directory(), vexFileName() );
                        //  See which ID the data base assigned...it will be the largest one.  Also
                        //  save the creation date.
                        newExperimentId = 0;
                        ResultSet dbExperimentList = db.experimentList();
                        try {
                            while ( dbExperimentList.next() ) {
                                int newId = dbExperimentList.getInt( "id" );
                                if ( newId > newExperimentId ) {
                                    newExperimentId = newId;
                                    creationDate = dbExperimentList.getString( "dateCreated" );
                                }
                            }
                        } catch ( Exception e ) {
                                java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, null, e );
                        }
                    }
                    //  Produce a warning if that failed.  This isn't critical.
                    else {
                        JOptionPane.showMessageDialog( _this, "Unable to add this item to the database\n"
                                + "(you can add it later).",
                                "Database Warning", JOptionPane.WARNING_MESSAGE );
                        inDataBase( false );
                    }
                }
                //  Set items that shouldn't change.
                _thisExperiment.id( newExperimentId );
                _thisExperiment.creationDate( creationDate );
                _thisExperiment.inDatabase( inDataBase() );
                _thisExperiment.number( number() );
                _thisExperiment.status( status() );
                _thisExperiment.directory( directory() );
                _thisExperiment.vexFile( vexFileName() );  //  BLAT - accept multiple vex file names!!!!
                //  Make the directory on the DiFX host...
                _statusLabel.setText( "creating experiment directory on DiFX host" );
                DiFXCommand_mkdir mkdir = new DiFXCommand_mkdir( directory(), _settings );
                try {
                    mkdir.send();
                    try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                } catch ( java.net.UnknownHostException e ) {
                    //  BLAT Should be a pop-up and we shouldn't continue this operation
                    java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, null, e );
                } 
                //  Add the node to the queue browser.
                _settings.queueBrowser().addExperiment( _thisExperiment );
            }

            //  Create a pass if explicitly requested OR if jobs are being created.  If
            //  the latter is true without the former being true, a dummy pass called 
            //  "default" will be created.  This pass will be in the datebase but will
            //  not have its own directory on the DiFX host and the queue browser will
            //  know not to display it - its jobs simply appear under the experiment.
            if ( createPass() || _newJobsCheck.isSelected() ) {
                
                //  Do some sanity checking on the settings that went into the v2d file.
                if ( _doSanityCheck.isSelected() ) {
                    if ( _antennaList != null && !_antennaList.useList().isEmpty() ) {        
                        synchronized ( _antennaList ) {
                            for ( Iterator<StationPanel> iter = _antennaList.useList().iterator(); iter.hasNext(); ) {
                                StationPanel antenna = iter.next();
                                if ( antenna.use() ) {
                                    if ( antenna.useVsn() ) {
                                        if ( antenna.vsnSource() != null && antenna.vsnSource().length() > 0 ) {
                                            //  Try to locate the directory list.  If it does not
                                            //  exist, see if the user wants to continue.
                                            try {
                                                int exs = DiFXCommand_ls.fileExists( 4, antenna.dirListLocation(), _settings );
                                                if ( exs == DiFXCommand_ls.FILE_DOESNT_EXIST ) {
                                                    int ret = JOptionPane.showConfirmDialog( _this, 
                                                            "The directory listing for VSN " + antenna.vsnSource() + "\""
                                                            + antenna.dirListLocation() + "\" does not exist.\nDo you wish to continue?",
                                                            "Directory Listing Not Found", 
                                                            JOptionPane.YES_NO_OPTION );
                                                    if ( ret == JOptionPane.NO_OPTION )
                                                        continueRun = false;
                                                }
                                            } catch ( java.net.UnknownHostException e ) {
                                                //  BLAT handle this properly!
                                            }
                                        }
                                        else {
                                            int ret = JOptionPane.showConfirmDialog( _this, 
                                                    "No VSN has been specified for antenna " + antenna.name() + ".\nDo you wish to continue?",
                                                    antenna.name() + "Lacks Data Source", 
                                                    JOptionPane.YES_NO_OPTION );
                                            if ( ret == JOptionPane.NO_OPTION )
                                                continueRun = false;
                                        }
                                    }
                                    else if ( antenna.useFile() ) {
                                        ArrayList<String> fileList = antenna.fileList();
                                        if ( fileList.size() > 0 ) {
                                            for ( Iterator<String> jter = fileList.iterator(); jter.hasNext(); ) {
                                                String filename = jter.next();
                                                _fileToNodeMap.put( filename, antenna.machineForFile( filename ) );
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    else {
                        int ret = JOptionPane.showConfirmDialog( _this, 
                                "No antennas have been selected for these obeservations.\nDo you wish to continue?",
                                "No Antennas Selected", 
                                JOptionPane.YES_NO_OPTION );
                        if ( ret == JOptionPane.NO_OPTION )
                            continueRun = false;
                    }
                }
                
                //  Add the pass to the database...
                String newPassName = passName();
                if ( !createPass() )
                    newPassName = "default";
                else
                    _statusLabel.setText( "creating pass \"" + newPassName + "\"" );
                //  This creates a "node" that will appear in the browser (or won't in the
                //  case of the "default" pass).
                _newPass = new PassNode( newPassName, _settings );
                _newPass.type( (String)_passTypeList.getSelectedItem() );
                _newPass.stateLabel( "Creating Pass on Host", Color.yellow, true );
                //_fileReadQueue.pass( _newPass );
                if ( !createPass() )
                    _newPass.setHeight( 0 );
                _thisExperiment.addChild( _newPass );
                _newPass.experimentNode( _thisExperiment );
                _databasePassId = 0;
                if ( db != null ) {
                    _statusLabel.setText( "adding pass information to database" );
                    db.newPass( newPassName, _settings.passTypeID( passType() ), _thisExperiment.id() );
                    //  See which ID the data base assigned...it will be the largest one that
                    //  is associated with this experiment.  This is safer than looking for the
                    //  name (because a name might be duplicated).  We need the pass ID so we can
                    //  properly connect any jobs created to it in the database.
                    ResultSet dbPassList = db.passList();
                    try {
                        while ( dbPassList.next() ) {
                            int newId = dbPassList.getInt( "id" );
                            int experimentId = dbPassList.getInt( "experimentID" );
                            if ( experimentId == _thisExperiment.id() && newId > _databasePassId )
                                _databasePassId = newId;
                        }
                        _newPass.id( _databasePassId );
                        _newPass.inDatabase( true );
                    } catch ( Exception e ) {
                        java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, null, e );
                    }
                }
                //  Create the pass directory on the DiFX host if the user has requested a
                //  pass directory.
                if ( createPass() ) {
                    _statusLabel.setText( "creating pass directory on DiFX host" );
                    DiFXCommand_mkdir mkdir = new DiFXCommand_mkdir( directory() + "/" + passDirectory(), _settings );
                    try {
                        mkdir.send();
                        //  Delay for a second to make sure mk5daemon has a chance to do this and recover.
                        try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                    } catch ( java.net.UnknownHostException e ) {
                        JOptionPane.showMessageDialog( _this, "Error - DiFX host \"" + _settings.difxControlAddress()
                             + "\" unknown.",
                                "Host Unknown", JOptionPane.ERROR_MESSAGE );
                        continueRun = false;
                    } 
                }

                //  Create new jobs in the current pass (if requested).
                if ( _newJobsCheck.isSelected() ) {
                    //  Write the .vex file.  This will be put in the pass directory if one is being
                    //  created, otherwise in the experiment directory.
                    //  First, make sure there IS a vex file name.
                    if ( vexFileName().length() <= 0 ) {
                        JOptionPane.showMessageDialog( _this, "No .vex file name was specified.",
                                "Missing Filename", JOptionPane.WARNING_MESSAGE );
                    }
                    String passDir = "";
                    if ( createPass() )
                        passDir = passDirectory() + "/";
                    //  Prepare the .vex file content for sending.  This includes removing EOP data if necessary,
                    //  removing stations within scans if they have a "-1" code and eliminating scans that
                    //  are not actually used.
                    String newVexData = "";
                    if ( _deleteEOPFromVex ) {
                        newVexData = VexFileParser.deleteEOPData( _editor.text() );
                    }
                    else {
                        newVexData = _editor.text();
                    }
                    newVexData = VexFileParser.editScans( newVexData, _settings.exciseUnusedScansCheck(), _vexData.scanList() );
                    Component comp = _okButton;
                    while ( comp.getParent() != null )
                        comp = comp.getParent();
                    Point pt = _okButton.getLocationOnScreen();
                    SendFileMonitor sendVex = new SendFileMonitor( (Frame)comp, pt.x + 25, pt.y + 25,
                            directory() + "/" + passDir + vexFileName(), newVexData, _settings );
                    //  Delay for a bit to avoid having the following operation step on the end of this
                    //  one.  Mk5daemon may still be busy.
                    try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                    //  Create the .v2d file.  This will be put in the pass directory if it
                    //  exists, or the main experiment directory if not.
                    SendFileMonitor sendV2d = new SendFileMonitor( (Frame)comp, pt.x + 25, pt.y + 25,
                            directory() + "/" + passDir + v2dFileName(), _v2dEditor.text(), _settings );
                    try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                    //  Run vex2difx on the new pass and v2d file.
                    passDir = directory();
                    if ( createPass() )
                        passDir += "/" + passDirectory();
                    _newPass.stateLabel( "Creating Jobs on Host", Color.yellow, true );
                    //_newPass.fullPath( directory() + "/" + passDir );
                    _newPass.fullPath( passDir );
                    _newPass.v2dFileName( v2dFileName() );
                    _passNode = _newPass;
                    DiFXCommand_vex2difx v2d = new DiFXCommand_vex2difx( passDir, v2dFileName(), _settings, false );
                    v2d.addIncrementalListener( new ActionListener() {
                        public void actionPerformed( ActionEvent e ) {
                            newFileCallback( e.getActionCommand() );
                        }
                    });
                    v2d.addEndListener( new ActionListener() {
                        public void actionPerformed( ActionEvent e ) {
                            endCallback( e.getActionCommand() );
                        }
                    });
                    try {
                        v2d.send();
                        try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                    } catch ( java.net.UnknownHostException e ) {
                        JOptionPane.showMessageDialog( _this, "Error - DiFX host \"" + _settings.difxControlAddress()
                             + "\" unknown.",
                                "Host Unknown", JOptionPane.ERROR_MESSAGE );
                        continueRun = false;
                    }
                }
                
            }

            //  Move "current" data (i.e. all files in the experiment directory) into
            //  the pass directory.
            //  BLAT this needs to be done, obviously
            if ( _currentDataCheck.isSelected() ) {
            }

            //  The "create pass" checkbox setting is saved to the settings as the new default.
            _settings.defaultNames().createPassOnExperimentCreation = createPass();
            
            //  Make this window go away!
            _statusLabel.setText( "" );
            setVisible( false );

            //  Reset the Apply button for next time.
            _operationCancelled = true;
            _operationRunning = false;
            _okButton.setText( "Apply" );
            
        }
        
        //----------------------------------------------------------------------
        //  Run the calc process on a job or set of jobs belonging to this
        //  experiment and within a specific pass.
        //----------------------------------------------------------------------
        protected void calcOnlyStart() {
            _passNode.stateLabel( "Running Calc", Color.yellow, true );
            DiFXCommand_vex2difx v2d = new DiFXCommand_vex2difx( _passNode.fullPath(), _name, _settings, true );
            v2d.addIncrementalListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    newFileCallback( e.getActionCommand() );
                }
            });
            v2d.addEndListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    endCallback( e.getActionCommand() );
                }
            });
            try {
                v2d.send();
                try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
            } catch ( java.net.UnknownHostException e ) {
                JOptionPane.showMessageDialog( _this, "Error - DiFX host \"" + _settings.difxControlAddress()
                     + "\" unknown.",
                        "Host Unknown", JOptionPane.ERROR_MESSAGE );
            }
        }
          
        /*
         * This function is called when the vex2difx process produces a new file
         * (triggered by a callback from the vex2difx thread, thus the name).  If
         * the file is an ".input" file, it creates a new job based on it.
         */
        synchronized public void newFileCallback( String newFile ) {
//            _newPass.stateLabel( "Downloading Job Files", Color.yellow, true );
            //  Get a connection to the database if we are using it.
            QueueDBConnection db = null;
            if ( _settings.useDatabase() ) {
                db = new QueueDBConnection( _settings );
                if ( !db.connected() )
                    db = null;
            }
            //  Get the extension and "full name" (the full path without the extension) on this new file.
            //  If the file doesn't have an extension we are currently not interested in it.
            if ( newFile.lastIndexOf( '.' ) == -1 )
                return;
            String extn = newFile.substring( newFile.lastIndexOf( '.' ) + 1 ).trim();
            String fullName = newFile.substring( 0, newFile.lastIndexOf( '.' ) ).trim();
            int databaseJobId = 0;
            //  If its an .input file, create a new job based on it (unless one already exists
            //  in which case we recycle to old one).  It is expected that there will be only
            //  one trasmission of the .input file.
            if ( extn.contentEquals( "input" ) ) {
                //  Produce a job "name" based on the filenames produced by vex2difx.
                String jobName = newFile.substring( newFile.lastIndexOf( '/' ) + 1, newFile.lastIndexOf( '.' ) );
                //  See if we've already created this job by searching existing jobs for the
                //  "full name".
                JobNode newJob = null;
                for ( Iterator<BrowserNode> iter = _passNode.childrenIterator(); iter.hasNext(); ) {
                    JobNode thisJob = (JobNode)iter.next();
                    if ( fullName.contentEquals( thisJob.fullName() ) )
                        newJob = thisJob;
                }
                //  Job doesn't exist yet, we have to create it.
                if ( newJob == null ) {
                    //  Create a node associated with this new job, then put it into
                    //  the appropriate pass so that it will appear in the queue browser.
                    _statusLabel.setText( "creating new job \"" + jobName + "\"" );
                    _passNode.stateLabel( "creating new job \"" + jobName + "\"", Color.YELLOW, true );
                    //BLATSystem.out.println( "creating new job \"" + jobName + "\"" );
                    newJob = new JobNode( jobName, _settings );
                    newJob.fullName( fullName );
                    //  Add the job to the existing pass.
                    _passNode.addChild( newJob );
                    //  BLAT
                    _passNode.sortByName();
                    newJob.passNode( _passNode );
                    _settings.queueBrowser().addJob( newJob );
                    //  Add the new job to the database (if we are using it).
                    if ( db != null ) {
                        _statusLabel.setText( "adding job information to database" );
                        db.newJob( jobName, _databasePassId, 0, 0.0, 0.0, "", _settings.difxVersion(),
                                0, 0, _settings.jobStatusID( "unknown" ) );
                        //  See which ID the data base assigned to the job...it will be the largest one that
                        //  is associated with the current pass.  This is safer than looking for the
                        //  name (because a name might be duplicated).  We need the job ID so we can
                        //  properly change items associated with it as we find them.
                        ResultSet dbJobList = db.jobListByPassId( _databasePassId );
                        try {
                            while ( dbJobList.next() ) {
                                int newId = dbJobList.getInt( "id" );
                                if ( newId > databaseJobId )
                                    databaseJobId = newId;
                            }
                        } catch ( Exception e ) {
                            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, null, e );
                        }
                        newJob.inDatabase( true );
                        newJob.id( databaseJobId );
                    }
                    else {
                        newJob.id( databaseJobId );
                        newJob.inDatabase( false );
                    }
                    //  Set the state for this job to show the missing .im file (which
                    //  we expect soon!).
                    newJob.state().setText( "await .im file" );
                    newJob.state().setBackground( Color.YELLOW );
                    //  Apply the input file data to the job.
                    newJob.inputFile( newFile.trim(), false );
                    //  Set the source and scan name for the job.  This involves looking at the .vex and .v2d data.
                    newJob.setSource( _this );
                    newJob.setScan( _this );
                    //_fileReadQueue.queueRead( newJob, newFile.trim() );
                    //  Add the input file path to the database if we are using it.
                    if ( db != null ) {
                        db.updateJob( databaseJobId, "inputFile", newFile.trim() );
                    }
                }
            }
            else if ( extn.contentEquals( "im" ) ) {
                //  Produce a job "name" based on the filenames produced by vex2difx.
                String jobName = newFile.substring( newFile.lastIndexOf( '/' ) + 1, newFile.lastIndexOf( '.' ) );
                //  Locate the job node for this .im file - it should be created.  If it
                //  isn't, there is a problem (but we'll ignore it as stray .input files in
                //  the directory that we did not create will create similarly-named .im files).
                JobNode newJob = null;
                for ( Iterator<BrowserNode> iter = _passNode.childrenIterator(); iter.hasNext(); ) {
                    JobNode thisJob = (JobNode)iter.next();
                    //  Build the "full name" of this job if it doesn't exist yet.
                    if ( thisJob.fullName() == null )
                        thisJob.fullName( _passNode.fullPath() + "/" + thisJob.name() );
                    if ( fullName.contentEquals( thisJob.fullName() ) )
                        newJob = thisJob;
                }
                if ( newJob != null ) {
                    _passNode.stateLabel( "adding .im file for \"" + jobName + "\"", Color.YELLOW, true );
                    newJob.state().setText( "not started" );
                    newJob.state().setBackground( Color.LIGHT_GRAY );
                    //  No need for this - the job is brand new!
                    //newJob.parseLogFile();
                }
            }
            else {
                if ( _newPass != null )
                    _newPass.stateLabel( "Finalizing Job Creation", Color.yellow, true );
                else
                    _passNode.stateLabel( "Calc Complete", Color.yellow, true );
            }
        }

        /*
         * This callback occurs when the vex2difx process is complete.
         */
        synchronized public void endCallback( String newFile ) {
            _passNode.stateLabel( "", Color.yellow, false );
            _statusLabel.setText( "vex2difx process completed!" );
        }
        
        PassNode _newPass;

    }
    
    //  This class prevents too many threads being created to download files from the
    //  guiServer.  Each job created by an experiment requires the creation of threads
    //  to do these downloads.  If an experiment includes hundreds of jobs (which they
    //  can), too many simultaneous threads will be required.  This thread (yes, another
    //  thread), limits the number of simultaneous downloads to 10.  It accepts requests
    //  in a FIFO queue.  After a minute of inactivity, it kills itself.
    protected class FileReadQueue extends Thread {
        
        public FileReadQueue() {
            _creationDeque = new ArrayDeque<JobStructure>();
            _killCounter = 0;
            _passNode = null;
            _foundJob = false;
        }
        
        public void pass( PassNode passNode ) {
            _passNode = passNode;
        }
        
        @Override
        public void run() {
            while( _killCounter < 6000 ) {
                if ( _creationDeque.isEmpty() )
                    ++_killCounter;
                else {
                    _killCounter = 0;
                    //  Take anything out of the queue that is finished and count those
                    //  that are running.
                    int readingCount = 0;
                    synchronized ( _creationDeque ) {
                        for ( Iterator<JobStructure> iter = _creationDeque.iterator(); iter.hasNext(); ) {
                            JobStructure thisJob = (JobStructure)iter.next();
                            if ( thisJob.started && thisJob.jobNode.readingDataFile() ) {
                                //  Still running
                                ++readingCount;
                            }
                            else if ( thisJob.started ) {
                                //  Started and not running...must be finished.
                                iter.remove();
                                _foundJob = true;
                            }
                        }
                        //  If there are fewer than 10 running, start new ones.
                        for ( Iterator<JobStructure> iter = _creationDeque.iterator(); iter.hasNext() && readingCount < 10; ) {
                            JobStructure thisJob = (JobStructure)iter.next();
                            if ( !thisJob.started ) {
                                thisJob.started = true;
                                //thisJob.jobNode.inputFile( thisJob.fileName, true );
                                ReadQueue newQueue = new ReadQueue( thisJob );
                                ++readingCount;
//                                System.out.println( "read " + thisJob.fileName );
                            }
                        }
                    }
                }
                try { Thread.sleep( 100 ); } catch( Exception e ) {}
            }
//            System.out.println( "dissolving read queue!!!" );
            if ( !_foundJob && _passNode != null )
                _passNode.stateLabel( "No Jobs Created", Color.red, true );
        }
        
        public void queueRead( JobNode newNode, String newFile ) {
            JobStructure newStruct = new JobStructure();
            newStruct.jobNode = newNode;
            newStruct.jobNode.readDisable();
            newStruct.fileName = newFile;
            synchronized ( _creationDeque ) {
                _creationDeque.add( newStruct );
            }
//            System.out.println( "queue " + newFile );
        }
        
        protected class JobStructure {
            JobNode jobNode;
            String fileName;
            boolean started;
        }
        
        protected class ReadQueue extends Thread {
            JobStructure _readItem;
            ReadQueue( JobStructure newItem ) {
                _readItem = newItem;
                start();
            }
            public void run() {
                _readItem.jobNode.inputFile( _readItem.fileName, true );
            }
        }
        
        protected ArrayDeque<JobStructure> _creationDeque;
        protected int _killCounter;
        protected PassNode _passNode;
        protected boolean _foundJob;
    }
    
    protected FileReadQueue _fileReadQueue;
    
    protected ExperimentNode _thisExperiment;
    protected SaneTextField _name;
    protected JLabel _nameAsLabel;
    protected NumberBox _number;
    protected JLabel _numberAsLabel;
    protected JCheckBox _inDataBase;
    protected JLabel _id;
    protected JLabel _created;
    protected boolean _newExperimentMode;
    protected boolean _saveInDataBase;
    protected JButton _okButton;
    protected JLabel _status;
    protected JComboBox<String> _statusList;
    protected SystemSettings _settings;
    protected JLabel _directoryAsLabel;
    protected TabCompletedTextField _directory;
    protected boolean _keepDirectory;
    protected SaneTextField _vexFileName;
    protected JLabel _vexFileNameAsLabel;
    protected JButton _editVexFileButton;
    protected JCheckBox _createPass;
    protected SaneTextField _passName;
    protected JLabel _passLabel;
    protected ExperimentEditor _this;
    protected NodeBrowserScrollPane _scrollPane;
    protected JMenuBar _menuBar;
    protected boolean _allObjectsBuilt;
    protected SimpleTextEditor _editor;
    protected ZButton _parseEditorContent;
    protected SimpleTextEditor _v2dEditor;
    protected SaneTextField _v2dFileName;
    protected JCheckBox _fromHost;
    protected TabCompletedTextField _fromHostLocation;
    protected JCheckBox _viaHttp;
    protected SaneTextField _viaHttpLocation;
    protected JCheckBox _viaFtp;
    protected SaneTextField _viaFtpLocation;
    protected JCheckBox _localFile;
    protected SaneTextField _localFileLocation;
    protected JCheckBox _fromExperiment;
    protected JButton _vexBrowseButton;
    protected JButton _goButton;
    protected JCheckBox _v2dFromHost;
    protected TabCompletedTextField _v2dFromHostLocation;
    protected JCheckBox _v2dViaHttp;
    protected SaneTextField _v2dViaHttpLocation;
    protected JCheckBox _v2dViaFtp;
    protected SaneTextField _v2dViaFtpLocation;
    protected JCheckBox _localV2dFile;
    protected SaneTextField _localV2dFileLocation;
    protected JCheckBox _noV2dFile;
    protected JButton _goV2dButton;
    protected String _startingV2dFileContent;
    protected JButton _previousVexFileButton;
    protected JPopupMenu _previousVexFileMenu;
    protected JComboBox<String> _passTypeList;
    protected String _currentPassType;
    protected JLabel _dataLabel;
    protected JCheckBox _currentDataCheck;
    protected JCheckBox _newJobsCheck;
    protected ButtonGrid _scanGrid;
    protected JCheckBox _singleInputFileCheck;
    protected SaneTextField _passDirectory;
//    protected TabCompletedTextField _stagingArea;
    protected JCheckBox _useStagingArea;
    protected boolean _keepPassDirectory;
    protected NodeBrowserScrollPane _antennaPane;
    protected AntennaList _antennaList;
    protected SaneTextField _inputFileBaseName;
    protected NumberBox _inputFileSequenceStart;
    protected JCheckBox _inputJobNames;
    protected JCheckBox _scanJobNames;
    protected JLabel _statusLabel;
    protected boolean _operationComplete;
    protected boolean _operationCancelled;
    protected boolean _operationRunning;
    protected HashMap<String,String> _fileToNodeMap;
    protected JCheckBox _doSanityCheck;
    protected JLabel _doSanityLabel;
    protected JButton _selectAllScansButton;
    protected JButton _selectNoScansButton;
    protected NodeBrowserScrollPane _sourcePane;
    protected TimeLimitPanel _timeLimits;
    protected NodeBrowserScrollPane _eopPane;
    protected NodeBrowserScrollPane _vexEOPPane;
    protected NodeBrowserScrollPane _newEOPPane;
    protected JCheckBox _vexEOPUseCheck;
    protected JCheckBox _newEOPUseCheck;
    protected ArrayList<SystemSettings.EOPStructure> _newEOP;
    protected boolean _deleteEOPFromVex;
    protected int _databasePassId;
    protected NumberBox _tInt;
    protected ZCheckBox _tIntApply;
    protected NumberBox _fftSpecRes;
    protected ZCheckBox _fftSpecResApply;
    protected Power2NumberBox _fftNChan;
    protected NumberBox _specRes;
    protected ZCheckBox _specResApply;
    protected Power2NumberBox _nChan;
    protected ZCheckBox _doPolar;
    protected ZCheckBox _doPolarApply;
    protected NumberBox _subintNS;
    protected ZCheckBox _subintNSApply;
    protected NumberBox _strideLength;
    protected ZCheckBox _strideLengthApply;
    protected NumberBox _xmacLength;
    protected ZCheckBox _xmacLengthApply;
    protected NumberBox _bufferedFFTs;
    protected ZCheckBox _bufferedFFTsApply;
    protected double _bandwidth;
    protected Calendar _eopMinTime;
    protected Calendar _eopMaxTime;
    protected VexFileParser _vexData;
    protected V2dFileParser _v2dFileParser;
    
    protected ScanStationTimeline _scanStationTimeline;
    
    protected ArrayList<SourcePanel> _sourcePanelList;
    
    protected Object _eopLock;
    
    public VexFileParser vexFileParser() { return _vexData; }
    public V2dFileParser v2dFileParser() { return _v2dFileParser; }

}
