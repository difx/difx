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

import edu.nrao.difx.difxutilities.DiFXCommand_getFile;
import edu.nrao.difx.difxutilities.DiFXCommand_sendFile;
import edu.nrao.difx.difxutilities.DiFXCommand_mkdir;
import edu.nrao.difx.difxutilities.DiFXCommand_vex2difx;
import edu.nrao.difx.difxutilities.DiFXCommand_ls;
import edu.nrao.difx.difxutilities.V2dFileParser;

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

import java.awt.Frame;
import java.awt.Color;
import java.awt.Point;
import java.awt.Component;
import java.awt.Cursor;

import java.util.Map;
import java.util.Iterator;
import java.util.Calendar;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.GregorianCalendar;

import java.text.SimpleDateFormat;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseWheelEvent;

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

import java.sql.ResultSet;

public class ExperimentEditor extends JFrame {
        
    public ExperimentEditor( int x, int y, SystemSettings settings ) {
        _settings = settings;
        _settings.setLookAndFeel();
        this.setLayout( null );
        this.setBounds( x, y, _settings.windowConfiguration().experimentEditorW,
                _settings.windowConfiguration().experimentEditorH );
        this.getContentPane().setLayout( null );
        _this = this;
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
        _statusList = new JComboBox();
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
        _directory = new SaneTextField();
        _directory.setBounds( 100, 140, 310, 25 );
        _directory.setToolTipText( "\"Working\" directory that will contain all files for this experiment." );
        _directory.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                directoryChangeAction();
            }
        });
        namePanel.add( _directory );
        _directory.setVisible( false );
        _directoryAsLabel = new JLabel( "" );
        _directoryAsLabel.setBounds( 100, 140, 310, 25 );
        _directoryAsLabel.setToolTipText( "\"Working\" directory that will contain all files for this experiment." );
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
        _scrollPane.addNode( startingV2dPanel );
        _v2dFromHost = new JCheckBox( "from DiFX Host" );
        _v2dFromHost.setSelected( _settings.defaultNames().v2dFromHost );
        _v2dFromHost.setToolTipText( "Copy the .v2d file data from a named file on the DiFX Host." );
        _v2dFromHost.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                v2dSourceChoice( _v2dFromHost );
            }
        });
        startingV2dPanel.add( _v2dFromHost );
        _v2dFromHostLocation = new SaneTextField();
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
        _fromHostLocation = new SaneTextField();
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
                getVexFromSource();
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
        _useMyEditor = new JButton( "Alternate Editor" );
        _useMyEditor.setToolTipText( "Use your preferred text editor to edit the .vex file.\nThe preferred editor is specified in the Settings Window." );
        _useMyEditor.setBounds( 20, 30, 140, 25 );
        _useMyEditor.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                useMyEditor();
            }
        });
        editorPanel.add( _useMyEditor );
        
        //  This panel contains a few often-changed parameters that govern the
        //  correlation.  These (mostly) end up in the "setup" section of the v2d file.
        IndexedPanel correlationPanel = new IndexedPanel( "Correlation Parameters" );
        correlationPanel.openHeight( 155 );
        correlationPanel.closedHeight( 20 );
        correlationPanel.open( false );
        _scrollPane.addNode( correlationPanel );
        _tInt = new NumberBox();
        _tInt.setBounds( 180, 30, 100, 25 );
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
        JLabel tIntLabel = new JLabel( "Integration Time (sec):" );
        tIntLabel.setBounds( 20, 30, 155, 25 );
        tIntLabel.setHorizontalAlignment( JLabel.RIGHT );
        correlationPanel.add( tIntLabel );
        _doPolar = new JCheckBox( "Do Polar" );
        _doPolar.setBounds( 300, 30, 100, 25 );
        _doPolar.setToolTipText( "Correlate cross hands when possible." );
        _doPolar.setSelected( _settings.defaultNames().correlationDoPolar );
        _doPolar.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _settings.defaultNames().correlationDoPolar = _doPolar.isSelected();
                produceV2dFile();
            }
        });
        correlationPanel.add( _doPolar );
        _specRes = new NumberBox();
        _specRes.setBounds( 180, 60, 100, 25 );
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
        JLabel specResLabel = new JLabel( "Spectral Resolution:" );
        specResLabel.setHorizontalAlignment( JLabel.RIGHT );
        specResLabel.setBounds( 20, 60, 155, 25 );
        correlationPanel.add( specResLabel );
        _nChan = new Power2NumberBox();
        _nChan.setBounds( 350, 60, 100, 25 );
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
        nChanLabel.setBounds( 280, 60, 65, 25 );
        nChanLabel.setHorizontalAlignment( JLabel.RIGHT );
        correlationPanel.add( nChanLabel );
        _fftSpecRes = new NumberBox();
        _fftSpecRes.setBounds( 180, 90, 100, 25 );
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
        _fftNChan = new Power2NumberBox();
        _fftNChan.setBounds( 350, 90, 100, 25 );
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
        fftNChanLabel.setBounds( 280, 90, 65, 25 );
        fftNChanLabel.setHorizontalAlignment( JLabel.RIGHT );
        correlationPanel.add( fftNChanLabel );
        _subintNS = new NumberBox();
        _subintNS.setBounds( 180, 120, 100, 25 );
        _subintNS.precision( 0 );
        _subintNS.intValue( _settings.defaultNames().correlationSubintNS );
        _subintNS.setToolTipText( "The mpifxcorr sub integration time (in nanoseconds)" );
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

        
        //  This panel is used to display and adjust antenna information.
        IndexedPanel antennaPanel = new IndexedPanel( "Stations" );
        antennaPanel.openHeight( 220 );
        antennaPanel.closedHeight( 20 );
        antennaPanel.open( false );
        antennaPanel.resizeOnTopBar( true );
        _scrollPane.addNode( antennaPanel );
        _antennaPane = new NodeBrowserScrollPane();
        _antennaPane.drawFrame( false );
        _antennaPane.setLevel( 1 );
        _antennaPane.respondToResizeEvents( true );
        _antennaPane.noTimer();
        antennaPanel.addScrollPane( _antennaPane );
        
        //  This panel contains the list of sources.
        IndexedPanel sourcePanel = new IndexedPanel( "Sources" );
        sourcePanel.open( false );
        sourcePanel.closedHeight( 20 );
        sourcePanel.resizeOnTopBar( true );
        _scrollPane.addNode( sourcePanel );
        _sourcePane = new NodeBrowserScrollPane();
        _sourcePane.drawFrame( false );
        _sourcePane.setLevel( 1 );
        _sourcePane.respondToResizeEvents( true );
        _sourcePane.noTimer();
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
                produceV2dFile();
            }
        });
        scanPanel.add( _scanGrid );
        _selectAllScansButton = new JButton( "Select All" );
        _selectAllScansButton.setBounds( 10, 40, 100, 25 );
        _selectAllScansButton.setToolTipText( "Select all scans for which all restrictions apply." );
        _selectAllScansButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _scanGrid.allOn();
                _timeLimits.maxLimits();
                produceV2dFile();
            }
        });
        scanPanel.add( _selectAllScansButton );
        _selectNoScansButton = new JButton( "Clear All" );
        _selectNoScansButton.setBounds( 115, 40, 100, 25 );
        _selectNoScansButton.setToolTipText( "De-select all scans." );
        _selectNoScansButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _scanGrid.allOff();
                produceV2dFile();
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
        _eopPane = new NodeBrowserScrollPane();
        _eopPane.drawFrame( false );
        _eopPane.setLevel( 1 );
        _eopPane.respondToResizeEvents( true );
        _eopPane.noTimer();
        eopPanel.addScrollPane( _eopPane );
        
        //  This panel is used to determine file names and related matters.
        IndexedPanel namesPanel = new IndexedPanel( "Names, Etc." );
        namesPanel.openHeight( 160 );
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
        _inputJobNames = new JCheckBox( "Based on Input Files" );
        _inputJobNames.setBounds( 170, 90, 325, 25 );
        _inputJobNames.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _inputJobNames.setSelected( true );
                _scanJobNames.setSelected( false );
            }
        });
        namesPanel.add( _inputJobNames );
        _scanJobNames = new JCheckBox( "Use Scan Name" );
        _scanJobNames.setBounds( 170, 120, 325, 25 );
        _scanJobNames.addActionListener( new ActionListener() {
            public void actionPerformed(  ActionEvent e ) {
                _inputJobNames.setSelected( false );
                _scanJobNames.setSelected( true );
            }
        });
        namesPanel.add( _scanJobNames );
        if ( _settings.defaultNames().scanBasedJobNames ) {
            _scanJobNames.setSelected( true );
            _inputJobNames.setSelected( false );
        }
        else {
            _scanJobNames.setSelected( false );
            _inputJobNames.setSelected( true );
        }
        JLabel jobNameLabel = new JLabel( "Job Names:" );
        jobNameLabel.setBounds( 10, 90, 155, 25 );
        jobNameLabel.setHorizontalAlignment( JLabel.RIGHT );
        namesPanel.add( jobNameLabel );
        
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
        _stagingArea = new SaneTextField();
        _stagingArea.setBounds( 285, 80, 280, 25 );
        _stagingArea.setToolTipText( "Directory into which to copy all input files for processing." );
        _stagingArea.setText( _settings.stagingArea() );
        buttonPanel.add( _stagingArea );
        JLabel stagingAreaLabel = new JLabel( "Use Staging Area:" );
        stagingAreaLabel.setBounds( 10, 80, 240, 25 );
        stagingAreaLabel.setHorizontalAlignment( JLabel.RIGHT );
        buttonPanel.add( stagingAreaLabel );
        _useStagingArea = new JCheckBox( "" );
        _useStagingArea.setBounds( 255, 80, 25, 25 );
        _useStagingArea.setSelected( _settings.useStagingArea() );
        if ( _useStagingArea.isSelected() ) {
            _stagingArea.setEnabled( true );
        }
        else {
            _stagingArea.setEnabled( false );
        }
        _useStagingArea.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _useStagingArea.isSelected() ) {
                    _stagingArea.setEnabled( true );
                }
                else {
                    _stagingArea.setEnabled( false );
                }
            }
        });
        buttonPanel.add( _useStagingArea );
        _passTypeList = new JComboBox();
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
                    _passTypeList.addItem( _passTypeList.getEditor().getItem() );
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
        _settings.windowConfiguration().experimentEditorW = w;
        _settings.windowConfiguration().experimentEditorH = h;
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
            _goV2dButton.setBounds( w - 125, 150, 100, 25 );    
            _doSanityCheck.setBounds( w - 50, 110, 25, 25 );
            _doSanityLabel.setBounds( w - 250, 110, 195, 25 );
            _scanGrid.setBounds( 10, 70, w - 35, 320 );
            _timeLimits.setBounds( 330, 30, w - 355, 35 );
            _v2dFileName.setBounds( 100, 30, w - 125, 25 );
            _v2dEditor.setBounds( 10, 60, w - 35, 430 );
            _passDirectory.setBounds( 285, 50, w - 310, 25 );
            _stagingArea.setBounds( 285, 80, w - 310, 25 );
            //  These things set the panel sizes based on their content.
            _antennaPane.setBounds( 0, 20, w, _antennaPane.dataHeight() );
            for ( Iterator<BrowserNode> iter = _antennaPane.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                StationPanel thisPanel = (StationPanel)iter.next();
                thisPanel.newWidth( w );//- 25 );
            }
            try {
                _sourcePane.setBounds( 0, 20, w, _sourcePane.dataHeight() );
                for ( Iterator<BrowserNode> iter = _sourcePane.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                    SourcePanel thisPanel = (SourcePanel)iter.next();
                    thisPanel.newWidth( w - 25 );
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
        _settings.defaultNames().v2dFromHost = _v2dFromHost.isSelected();
        _settings.defaultNames().v2dViaHttp = _v2dViaHttp.isSelected();
        _settings.defaultNames().v2dViaFtp = _v2dViaFtp.isSelected();
        _settings.defaultNames().v2dFromLocal = _localV2dFile.isSelected();
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
            GetFileMonitor fileGet = new GetFileMonitor( (Frame)comp, pt.x + 25, pt.y + 25, _fromHostLocation.getText(), _settings );
            //fileGet.setVisible( true );
            //  We only use the content of the file if it was read successfully.
            if ( fileGet.success() ) {
                _settings.defaultNames().vexFileSource = _fromHostLocation.getText();
                _editor.text( fileGet.inString() );
                _editor.top();
                parseNewVexFile();
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
                parseNewVexFile();
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
                parseNewVexFile();
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
                parseNewVexFile();
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
            GetFileMonitor fileGet = new GetFileMonitor( (Frame)comp, pt.x + 25, pt.y + 25, _v2dFromHostLocation.getText(), _settings );
            //  We only use the content of the file if it was read successfully.
            if ( fileGet.success() ) {
                _settings.defaultNames().v2dFileSource = _v2dFromHostLocation.getText();
                _startingV2dFileContent = fileGet.inString();
                parseStartingV2dFile();
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
                parseStartingV2dFile();
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
                parseStartingV2dFile();
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
                parseStartingV2dFile();
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
    }
    
    /*
     * Parse the content of the "starting" .v2d file and change all settings to
     * match it.  The .v2d file content has already been stored as the String
     * "_startingV2dFileContent".
     */
    public void parseStartingV2dFile() {
        //  The parser eats the content and stores it in structures.
        V2dFileParser v2dFileParser = new V2dFileParser( _startingV2dFileContent );
        //  The content of the indicated .vex file should be dumped in the .vex file
        //  editor, assuming it exists.
        if ( v2dFileParser.vexFile() != null ) {
            System.out.println( "vex file name is " + v2dFileParser.vexFile() );
            this.vexFileName( v2dFileParser.vexFile() );
        }
        else
            System.out.println( _startingV2dFileContent );
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
    synchronized public void findOldV2dFile( String fileBase ) {
        _lastV2dPath = null;
        DiFXCommand_ls ls = new DiFXCommand_ls( fileBase + "*.v2d", _settings );
        //  Set the callback for when the list is complete.  
        ls.addEndListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  Found anything at all?
                if ( _lastV2dPath != null ) {
                    System.out.println( "hey, we have " + _lastV2dPath );
                    readV2dFile( _lastV2dPath );
                }
            }
        });
        //  Set the callback for when a new item is added to the list.
        ls.addIncrementalListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _lastV2dPath = e.getActionCommand().trim();
                System.out.println( "got \"" + _lastV2dPath + "\"" );
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
    public void readV2dFile( String fileName ) {
        Component comp = _this;
        while ( comp.getParent() != null )
            comp = comp.getParent();
        Point pt = new Point( 100, 100 );
        if ( _thisExperiment != null )
            pt = _thisExperiment.getLocationOnScreen();
        GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                fileName, _settings );
        if ( getFile.inString() != null && getFile.inString().length() > 0 ) {
            _startingV2dFileContent = getFile.inString();
            parseStartingV2dFile();
        }
    }
    
    /*
     * This class is used to display information about a source.  It is an IndexedPanel
     * with a few controls.  May need to be put in its own file (like StationPanel) if
     * it gets much more complex.
     */
    public class SourcePanel extends IndexedPanel {

        public SourcePanel( VexFileParser.Source source, SystemSettings settings ) {
            super( source.name );
            _settings = settings;
            _this = this;
            _changeListeners = new EventListenerList();
            this.closedHeight( 20 );
            this.openHeight( 20 );
            this.open( false );
            this.darkTitleBar( false );
            this.drawFrame( false );
            this.resizeOnTopBar( true );
            _useCheck = new JCheckBox( "" );
            _useCheck.setBounds( 200, 2, 18, 16 );
            _useCheck.setSelected( true );
            _useCheck.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    dispatchChangeCallback();
                }
            } );
            this.add( _useCheck );
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

        protected SourcePanel _this;
        protected JCheckBox _useCheck;
        protected EventListenerList _changeListeners;
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
    
    /*
     * Extension of the NumberBox class to make it handle only powers of 2.
     */
    protected class Power2NumberBox extends NumberBox {
        
        public Power2NumberBox() {
            precision( 0 );
            minimum( 2.0 );
        }
        
        public void mouseWheelMoved( MouseWheelEvent e ) {
            if ( e.getWheelRotation() > 0 )
                checkNewValue( value() * 2.0 * (double)( e.getWheelRotation() ) );
            else
                checkNewValue( value() / ( 2.0 * (double)( -e.getWheelRotation() ) ) );
        }

        protected void upKey() {
            checkNewValue( value() * 2.0 );
        }

        protected void downKey() {
            checkNewValue( value() / 2.0 );        
        }
    
    }

    /*
     * Read the current vex file, which is stored in the editor, and parse out items
     * that we can use in the .v2d file.
     */
    public void parseNewVexFile() {
        VexFileParser vexData = new VexFileParser();
        vexData.data( _editor.text() );
        _eopMinTime = null;
        _eopMaxTime = null;
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
                    if ( _eopMinTime == null ) {
                        _eopMinTime = startTime;
                        _eopMaxTime = endTime;
                    }
                    else {
                        if ( startTime.before( _eopMinTime ) )
                            _eopMinTime = startTime;
                        if ( endTime.after( _eopMaxTime ) )
                            _eopMaxTime = endTime;
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
        int adj = (int)( ( _eopMaxTime.getTimeInMillis() - _eopMinTime.getTimeInMillis() ) / 50 );
        _eopMinTime.add( Calendar.MILLISECOND, -adj );
        _eopMaxTime.add( Calendar.MILLISECOND, adj );
        _timeLimits.limits( _eopMinTime, _eopMaxTime );
        //  Add panels of information about each antenna.  First we clear the existing
        //  lists of antennas (these might have been formed the last time the .vex file
        //  was parsed).
        _antennaPane.clear();
        if ( _antennaList != null )
            _antennaList.clear();
        if ( vexData.stationList() != null ) {
            for ( Iterator<VexFileParser.Station> iter = vexData.stationList().iterator(); iter.hasNext(); ) {
                VexFileParser.Station station = iter.next();
                //  Make a new panel to hold this station.
                if ( _antennaList == null )
                    _antennaList = new AntennaList();
                StationPanel panel = new StationPanel( station, _settings );
                panel.addChangeListener( new ActionListener() {
                    public void actionPerformed( ActionEvent evt ) {
                        //  Slightly slippery here...this ignores user's specific selections
                        //  of scans and turns on/off all those that meet station restrictions
                        //  (as well as other restrictions - sources, etc.).
                        _scanGrid.allOn();
                        checkScansAgainstAntennas();
                        produceV2dFile();
                    }
                } );
                _antennaPane.addNode( panel );
                _antennaList.add( panel );
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
        if ( vexData.sourceList() != null ) {
            for ( Iterator<VexFileParser.Source> iter = vexData.sourceList().iterator(); iter.hasNext(); ) {
                VexFileParser.Source source = iter.next();
                //  Make sure this source is used in one of the scans!  If not, we
                //  ignore it, as the user should have no interest in it.
                boolean keepSource = false;
                for ( Iterator<VexFileParser.Scan> jter = vexData.scanList().iterator(); jter.hasNext() && !keepSource; ) {
                    VexFileParser.Scan scan = jter.next();
                    if ( scan.source.equalsIgnoreCase( source.name ) )
                        keepSource = true;
                }
                if ( keepSource ) {
                    SourcePanel panel = new SourcePanel( source, _settings );
                    panel.addChangeListener( new ActionListener() {
                        public void actionPerformed( ActionEvent evt ) {
                            //  Slightly slippery here...this ignores user's specific selections
                            //  of scans and turns on/off all those that meet station restrictions
                            //  (as well as other restrictions - sources, etc.).
                            _scanGrid.allOn();
                            produceV2dFile();
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
            _vexEOPPane = new NodeBrowserScrollPane();
            _vexEOPPane.drawFrame( false );
            _vexEOPPane.setLevel( 2 );
            _vexEOPPane.respondToResizeEvents( true );
            _vexEOPPane.noTimer();
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
        midTime.setTimeInMillis( _eopMinTime.getTimeInMillis() + ( _eopMaxTime.getTimeInMillis() - _eopMinTime.getTimeInMillis() ) / 2 );
        _newEOP = _settings.eopData( midTime.julian() - 2.0, midTime.julian() + 3.0 );
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
            _newEOPPane = new NodeBrowserScrollPane();
            _newEOPPane.drawFrame( false );
            _newEOPPane.setLevel( 2 );
            _newEOPPane.respondToResizeEvents( true );
            _newEOPPane.noTimer();
            newEOPPanel.addScrollPane( _newEOPPane );
            replaceRemoteEOPData();
        }
        produceV2dFile();
        //  Add a "listener" to pick up changes in the EOP data.  These will cause
        //  the EOP table to be rebuilt.
        _settings.eopChangeListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                replaceRemoteEOPData();
                produceV2dFile();
            }
        } );
    }
    
    /*
     * Fill the table containing EOP data from external sources (not from the .vex file)
     * using current data.  This is done when the table is created intialy, or when there
     * are changes to the data.
     */
    protected void replaceRemoteEOPData() {
        //  This has to be here because a callback is triggered before this pane is built.
        if ( _newEOPPane == null )
            return;
        _newEOPPane.clear();
        //  Generate EOP data from the EOP source (if available).  To do this, we first
        //  need to find the date of these observations (I'm using the midpoint of the
        //  observations, but it probably doesn't matter too much).
        JulianCalendar midTime = new JulianCalendar();
        midTime.setTimeInMillis( _eopMinTime.getTimeInMillis() + ( _eopMaxTime.getTimeInMillis() - _eopMinTime.getTimeInMillis() ) / 2 );
        _newEOP = _settings.eopData( midTime.julian() - 2.0, midTime.julian() + 3.0 );
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
            JLabel ut1 = new JLabel( String.format( "%.6f sec ", eop.ut1_tai / 1000000.0 + eop.tai_utc ) );
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
     * Check the scan selections against antennas that have been selected.  If all
     * antennas required for the scan have NOT been selected, the scan is switched
     * off.  This should probably only be called after changes to the antenna
     * selections.
     */
    public void checkScansAgainstAntennas() {
        for ( Iterator<ButtonGrid.GridButton> iter = _scanGrid.buttonList().iterator(); iter.hasNext(); ) {
            ButtonGrid.GridButton button = iter.next();
            //  Check any scan button that is already on against chosen antennas.
            if ( button.on() ) {
                VexFileParser.Scan scan = (VexFileParser.Scan)button.data();
                //  The list of stations/antennas that is on...all of the scans antennas must
                //  be included or it will be turned off.
                boolean _stationsMatch = true;
                for ( Iterator jter = scan.station.iterator(); jter.hasNext(); ) {
                    VexFileParser.ScanStation station = (VexFileParser.ScanStation)jter.next();
                    if ( _antennaList == null || _antennaList.useList().isEmpty() )
                        _stationsMatch = false;
                    else {
                        boolean stationFound = false;
                        for ( Iterator<StationPanel> kter = _antennaList.useList().iterator(); kter.hasNext(); ) {
                            StationPanel antenna = kter.next();
                            if ( antenna.name().equalsIgnoreCase( station.name ) ) {
                                stationFound = true;
                            }
                        }
                        if ( !stationFound )
                            _stationsMatch = false;
                    }
                }
                if ( !_stationsMatch )
                    button.on( false );
            }
        }
    }
    
    /*
     * Check each scan agains the sources that have been selected.  If the scan does
     * not include the source, it will be removed.
     */
    public void checkScansAgainstSources() {
        for ( Iterator<ButtonGrid.GridButton> iter = _scanGrid.buttonList().iterator(); iter.hasNext(); ) {
            ButtonGrid.GridButton button = iter.next();
            if ( button.on() ) {
                VexFileParser.Scan scan = (VexFileParser.Scan)button.data();
                boolean _sourceFound = false;
                for ( Iterator<BrowserNode> jter = _sourcePane.browserTopNode().childrenIterator(); jter.hasNext() && !_sourceFound; ) {
                    SourcePanel source = (SourcePanel)jter.next();
                    if ( scan.source.equalsIgnoreCase( source.name() ) && source.use() )
                        _sourceFound = true;
                }
                if ( !_sourceFound )
                    button.on( false );
            }
        }
    }
    
    /*
     * Use the current settings to produce a .v2d "file".  This is stored in the
     * v2d editor.
     */
    public void produceV2dFile() {
        checkScansAgainstSources();
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
        
        //  Whether or not we should split into one scan per job
        v2dFileParser.singleScan( !_singleInputFileCheck.isSelected() );
        
        //  The base name of input files - we only do this if the user has set it.
        if ( _inputFileBaseName.getText() != null && !_inputFileBaseName.getText().trim().contentEquals( "" ) )
            v2dFileParser.jobSeries( _inputFileBaseName.getText() );
        
        //  This is where vex2difx will start numbering things.
        v2dFileParser.startSeries( _inputFileSequenceStart.intValue() );
        
        //  Setup section contains things from the "correlation parameters" settings.
        v2dFileParser.setup( "normalSetup" );
        v2dFileParser.setupTInt( "normalSetup", _tInt.value() );
        v2dFileParser.setupFFTSpecRes( "normalSetup", _fftSpecRes.value() );
        v2dFileParser.setupSpecRes( "normalSetup", _specRes.value() );
        v2dFileParser.setupSubintNS( "normalSetup", _subintNS.intValue() );
        v2dFileParser.setupDoPolar( "normalSetup", _doPolar.isSelected() );
        
        //  Produce a list of the scans we want to include.  These come from the
        //  scan grid.  We only do this if any scans have been chosen (we produce
        //  a warning if there are no scans included).
        if ( _scanGrid.onItems().size() > 0 ) {
            v2dFileParser.rule( "scansubset" );
            String scanList = "";
            for ( Iterator<String> iter = _scanGrid.onItems().iterator(); iter.hasNext(); ) {
                scanList += iter.next();
                if ( iter.hasNext() )
                    scanList += ",";
            }
            v2dFileParser.ruleScan( "scansubset", scanList );
            v2dFileParser.ruleSetup( "scansubset", "normalSetup" );
        }
        else {
//            JOptionPane.showMessageDialog( _this, "No scans have been chosen for this processing\n(so no jobs will be created)!",
//                    "Zero Scans Included", JOptionPane.WARNING_MESSAGE );
        }
        
        //  Describe specifics for each used antenna...data source, etc.  The following
        //  hash map allows us to locate the machines that each file source originated
        //  from.  This is needed when running jobs.
        if ( _fileToNodeMap == null )
            _fileToNodeMap = new HashMap<String,String>();
        _fileToNodeMap.clear();
        if ( _antennaList != null && !_antennaList.useList().isEmpty() ) {
            for ( Iterator<StationPanel> iter = _antennaList.useList().iterator(); iter.hasNext(); ) {
                StationPanel antenna = iter.next();
                if ( antenna.use() ) {
                    v2dFileParser.antenna( antenna.name() );
                    v2dFileParser.antennaPhaseCalInt( antenna.name(), antenna.phaseCalInt() );
                    v2dFileParser.antennaToneSelection( antenna.name(), antenna.toneSelection() );
                    v2dFileParser.antennaFormat( antenna.name(), antenna.dataFormat() );
                    if ( antenna.useVsn() ) {
                        v2dFileParser.antennaVsn( antenna.name(), antenna.vsnSource() );
                    }
                    else if ( antenna.useFile() ) {
                        ArrayList<String> fileList = antenna.fileList();
                        if ( fileList.size() > 0 ) {
                            for ( Iterator<String> jter = fileList.iterator(); jter.hasNext(); ) {
                                String filename = jter.next();
                                v2dFileParser.antennaFile( antenna.name(), filename );
                                _fileToNodeMap.put( filename, antenna.machineForFile( filename ) );
                            }
                        }
                    }
                    else if ( antenna.useEVLBI() )
                        v2dFileParser.antennaNetworkPort( antenna.name(), antenna.networkPort() );
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
                        eop.ut1_tai / 1000000.0 + eop.tai_utc, 
                        eop.xPole / 10.0, 
                        eop.yPole / 10.0 );
            }
            _deleteEOPFromVex = true;
        }
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
    
    protected class ApplyThread extends Thread {
        
        public void run() {
            
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
                if ( !createPass() )
                    _newPass.setHeight( 0 );
                _thisExperiment.addChild( _newPass );
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
                    _statusLabel.setText( "Writing file \"" + directory() + "/" + passDir + vexFileName() + "\"" );
                    //  Remove the EOP data from the .vex file before sending if necessary.
                    String vexData = "";
                    if ( _deleteEOPFromVex ) {
                        vexData = VexFileParser.deleteEOPData( _editor.text() );
                    }
                    else {
                        vexData = _editor.text();
                    }
                    Component comp = _okButton;
                    while ( comp.getParent() != null )
                        comp = comp.getParent();
                    Point pt = _okButton.getLocationOnScreen();
                    SendFileMonitor sendVex = new SendFileMonitor( (Frame)comp, pt.x + 25, pt.y + 25,
                            directory() + "/" + passDir + vexFileName(), vexData, _settings );
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
          
        /*
         * This function is called when the vex2difx process produces a new file
         * (triggered by a callback from the vex2difx thread, thus the name).  If
         * the file is an ".input" file, it creates a new job based on it.
         */
        synchronized public void newFileCallback( String newFile ) {
            //  Get a connection to the database if we are using it.
            QueueDBConnection db = null;
            if ( _settings.useDatabase() ) {
                db = new QueueDBConnection( _settings );
                if ( !db.connected() )
                    db = null;
            }
            //  Get the extension and "full name" (the full path without the extension) on this new file...
            String extn = newFile.substring( newFile.lastIndexOf( '.' ) + 1 ).trim();
            String fullName = newFile.substring( 0, newFile.lastIndexOf( '.' ) ).trim();
            int databaseJobId = 0;
            //  If its an .input create a new job based on it.
            if ( extn.contentEquals( "input" ) ) {
                //  See if we've already created it by searching existing jobs for the
                //  "full name".
                JobNode newJob = null;
                for ( Iterator<BrowserNode> iter = _newPass.childrenIterator(); iter.hasNext(); ) {
                    JobNode thisJob = (JobNode)iter.next();
                    if ( fullName.contentEquals( thisJob.fullName() ) )
                        newJob = thisJob;
                }
                //  Okay...maybe we have to create it.
                if ( newJob == null ) {
                    //  Produce a job "name"...for the moment, these are based on the
                    //  filenames produced by vex2difx.  Ultimately we may give the user
                    //  some more palatable choices.
                    String jobName = newFile.substring( newFile.lastIndexOf( '/' ) + 1, newFile.lastIndexOf( '.' ) );
                    //  Create a node associated with this new job, then put it into
                    //  the appropriate pass so that it will appear in the queue browser.
                    _statusLabel.setText( "creating new job \"" + jobName + "\"" );
                    //BLATSystem.out.println( "creating new job \"" + jobName + "\"" );
                    newJob = new JobNode( jobName, _settings );
                    newJob.fullName( fullName );
                    //BLATSystem.out.println( "adding job name " + fullName );
                    _newPass.addChild( newJob );
                    newJob.passNode( _newPass );
                    _settings.queueBrowser().addJob( newJob );
                    newJob.editorMonitor().editor( _this );
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
                    
                    //  Apply the input file data to the job.
                    newJob.inputFile( newFile.trim(), true );
                    //  Add the input file path to the database if we are using it.
                    if ( db != null ) {
                        db.updateJob( databaseJobId, "inputFile", newFile.trim() );
                    }
                }
            }
        }

        /*
         * This callback occurs when the vex2difx process is complete.
         */
        synchronized public void endCallback( String newFile ) {
            _statusLabel.setText( "vex2difx process completed!" );
        }
        
        PassNode _newPass;

    }
    
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
    protected JComboBox _statusList;
    protected SystemSettings _settings;
    protected JLabel _directoryAsLabel;
    protected SaneTextField _directory;
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
    protected JButton _useMyEditor;
    protected SimpleTextEditor _v2dEditor;
    protected SaneTextField _v2dFileName;
    protected JCheckBox _fromHost;
    protected SaneTextField _fromHostLocation;
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
    protected SaneTextField _v2dFromHostLocation;
    protected JCheckBox _v2dViaHttp;
    protected SaneTextField _v2dViaHttpLocation;
    protected JCheckBox _v2dViaFtp;
    protected SaneTextField _v2dViaFtpLocation;
    protected JCheckBox _localV2dFile;
    protected SaneTextField _localV2dFileLocation;
    protected JButton _goV2dButton;
    protected String _startingV2dFileContent;
    protected JButton _previousVexFileButton;
    protected JPopupMenu _previousVexFileMenu;
    protected JComboBox _passTypeList;
    protected String _currentPassType;
    protected JLabel _dataLabel;
    protected JCheckBox _currentDataCheck;
    protected JCheckBox _newJobsCheck;
    protected ButtonGrid _scanGrid;
    protected JCheckBox _singleInputFileCheck;
    protected SaneTextField _passDirectory;
    protected SaneTextField _stagingArea;
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
    protected NumberBox _fftSpecRes;
    protected Power2NumberBox _fftNChan;
    protected NumberBox _specRes;
    protected Power2NumberBox _nChan;
    protected JCheckBox _doPolar;
    protected NumberBox _subintNS;
    protected double _bandwidth;
    protected Calendar _eopMinTime;
    protected Calendar _eopMaxTime;
    
}
