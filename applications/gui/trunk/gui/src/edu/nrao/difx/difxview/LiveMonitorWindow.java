/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxview;

import edu.nrao.difx.difxutilities.GuiServerConnection;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JSeparator;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.Component;
import java.awt.Color;

import mil.navy.usno.plotlib.PlotWindow;
import mil.navy.usno.plotlib.Plot2DObject;
import mil.navy.usno.plotlib.Track2D;
import mil.navy.usno.plotlib.DrawObject;

import java.awt.event.ComponentEvent;

import java.util.Date;
import java.util.Iterator;
import java.util.ArrayDeque;

import javax.swing.JFileChooser;

import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;

import java.text.SimpleDateFormat;

import edu.nrao.difx.difxutilities.SMARTMonitor;
import edu.nrao.difx.difxutilities.InputFileParser;
import java.awt.Font;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.net.Socket;
import javax.swing.*;
import mil.navy.usno.plotlib.Plot2DObject;
import mil.navy.usno.plotlib.PlotWindow;
import mil.navy.usno.plotlib.Track2D;
import mil.navy.usno.plotlib.Curve2D;
import mil.navy.usno.widgetlib.*;

public class LiveMonitorWindow extends JFrame implements WindowListener {
    
    public LiveMonitorWindow( int x, int y, SystemSettings settings, String inputFile ) {
        _settings = settings;
        _inputFile = inputFile;
        if ( _inputFile == null )
            _standAlone = true;
        _settings.setLookAndFeel();
        this.setLayout( null );
        this.setBounds( x, y, _settings.windowConfiguration().monitorDisplayW,
                _settings.windowConfiguration().monitorDisplayH );
        this.getContentPane().setLayout( null );
        _this = this;
    	this.addComponentListener( new java.awt.event.ComponentAdapter() {
            public void componentResized( ComponentEvent e ) {
                _settings.windowConfiguration().monitorDisplayW = _this.getWidth();
                _settings.windowConfiguration().monitorDisplayH = _this.getHeight();
                newSize();
            }
        });
        this.setTitle( "Real-Time Job Monitor" );
        _menuBar = new JMenuBar();
        JMenu helpMenu = new JMenu( "  Help  " );
        _menuBar.add( helpMenu );
        JMenuItem settingsHelpItem = new JMenuItem( "Settings Help" );
        settingsHelpItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.launchGUIHelp( "settings.html" );
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
        this.add( _menuBar );
        _scrollPane = new NodeBrowserScrollPane();
        _scrollPane.addTimeoutEventListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _this.newSize();
            }
        } );
        this.add( _scrollPane );
        
        //  Connection Controls
        _connectionPanel = new IndexedPanel( "Connection Controls" );
        _connectionPanel.openHeight( 120 );
        _connectionPanel.closedHeight( 20 );
        _scrollPane.addNode( _connectionPanel );
        _monitorHost = new SaneTextField();
        _monitorHost.setText( _settings.difxMonitorHost() );
        _monitorHost.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                changeConnection();
            }
        } );
        _connectionPanel.add( _monitorHost );
        _monitorHostLabel = new JLabel( "Monitor Host:" );
        _monitorHostLabel.setHorizontalAlignment( JLabel.RIGHT );
        _connectionPanel.add( _monitorHostLabel );
        /*
        _monitorPort = new NumberBox();
        _monitorPort.value( _settings.difxMonitorPort() );
        _monitorPort.setHorizontalAlignment( NumberBox.LEFT );
        _monitorPort.minimum( 0 );
        _monitorPort.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                makeConnection();
            }
        } );
        */
        _monitorPort = new JTextField();
        _monitorPort.setEditable( false );
        _monitorPort.setText( "" );
        _connectionPanel.add( _monitorPort );
        _monitorPortLabel = new JLabel( "Using Port:" );
        _monitorPortLabel.setHorizontalAlignment( JLabel.RIGHT );
        _connectionPanel.add( _monitorPortLabel );
        _connectionLight = new ActivityMonitorLight();
        _connectionLight.alertTime( 0 );
        _connectionLight.warningTime( 0 );
        _connectionLight.warning();
        _connectionPanel.add( _connectionLight );
        _connectionLabel = new JLabel( "not connected" );
        _connectionPanel.add( _connectionLabel );
        _connectionPlotWindow = new PlotWindow();
        _connectionPlotWindow.backgroundColor( this.getBackground() );
        _connectionPanel.add( _connectionPlotWindow );
        _connectionPlot = new Plot2DObject();
        _connectionPlot.title( "bytes/sec", Plot2DObject.LEFT_JUSTIFY );
        _connectionPlot.titlePosition( 0.0, -12.0 );
        _connectionPlot.titleColor( Color.WHITE, true );
        _connectionPlot.titleFont( new Font( "Dialog", Font.BOLD, 12 ) );
        _connectionTrack = new Track2D();
        _connectionPlot.addTrack( _connectionTrack );
        _connectionTrack.color( Color.GREEN );
        _connectionTrack.sizeLimit( 1000 );
        _connectionPlot.backgroundColor( Color.BLACK );
        _connectionPlotWindow.add2DPlot( _connectionPlot );
        _connectionMaxBytes = 10.0;
        
        //  Message Area
        _messagePanel = new IndexedPanel( "Messages" );
        _messagePanel.openHeight( 120 );
        _messagePanel.closedHeight( 20 );
        _scrollPane.addNode( _messagePanel );
        _messages = new MessageDisplayPanel();
        _messages.showSource( false );
        _messagePanel.add( _messages );
        
        //  Data Controls
        _dataPanel = new IndexedPanel( "Data Controls" );
        _dataPanel.openHeight( 160 );
        _dataPanel.closedHeight( 20 );
        _scrollPane.addNode( _dataPanel );
        //  Stand alone version maintains a combo box of available input files.  This
        //  gives the user the choice of which one to monitor.
        if ( _inputFile == null ) { 
            _inputFileComboBox = new JComboBox();
            _dataPanel.add( _inputFileComboBox );
        }
        else {
            _inputFileOutput = new JTextField();
            _inputFileOutput.setEditable( false );
            _inputFileOutput.setText( _inputFile );
            _dataPanel.add( _inputFileOutput );
        }
        _inputFileLabel = new JLabel( "Input File:" );
        _inputFileLabel.setHorizontalAlignment( JLabel.RIGHT );
        _dataPanel.add( _inputFileLabel );
        _telescope1GridLabel = new JLabel( "Telescope 1: " );
        _telescope1GridLabel.setBounds( 10, 60, 100, 25 );
        _telescope1GridLabel.setHorizontalAlignment( JLabel.RIGHT );
        _telescope1GridLabel.setVisible( false );
        _dataPanel.add( _telescope1GridLabel );
        _telescope2GridLabel = new JLabel( "Telescope 2: " );
        _telescope2GridLabel.setBounds( 10, 90, 100, 25 );
        _telescope2GridLabel.setHorizontalAlignment( JLabel.RIGHT );
        _telescope2GridLabel.setVisible( false );
        _dataPanel.add( _telescope2GridLabel );
        _frequenciesGridLabel = new JLabel( "Frequency: " );
        _frequenciesGridLabel.setBounds( 10, 90, 100, 25 );
        _frequenciesGridLabel.setHorizontalAlignment( JLabel.RIGHT );
        _frequenciesGridLabel.setVisible( false );
        _dataPanel.add( _frequenciesGridLabel );
        _productsLabel = new JLabel( "No Products Selected" );
        _productsLabel.setHorizontalAlignment( JLabel.RIGHT );
        _dataPanel.add( _productsLabel );
        _viewButton = new JToggleButton( "View" );
        _viewButton.setVisible( false );
        _viewButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _viewButton.isSelected() )
                    _tableScrollPane.setVisible( true );
                else
                    _tableScrollPane.setVisible( false );
                _automaticallyResize = true;
            }
        } );
        _dataPanel.add( _viewButton );
        _applyButton = new JButton( "Apply" );
        _applyButton.setVisible( false );
        _applyButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                applyButtonAction();
            }
        } );
        _dataPanel.add( _applyButton );
        _selectedCheck = new JCheckBox( "Selected" );
        _selectedCheck.setVisible( false );
        _selectedCheck.setSelected( true );
        _selectedCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _selectedCheck.isSelected() )
                    _allCheck.setSelected( false );
                else
                    _allCheck.setSelected( true );
                alterProductTable();
            }
        } );
        _dataPanel.add( _selectedCheck );
        _allCheck = new JCheckBox( "All" );
        _allCheck.setVisible( false );
        _allCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _allCheck.isSelected() )
                    _selectedCheck.setSelected( false );
                else
                    _selectedCheck.setSelected( true );
                alterProductTable();
            }
        } );
        _dataPanel.add( _allCheck ); 
        //_productTable = new DefaultTableModel();
        _productTable = new DefaultTableModel(
                new Object[]{ "Selected",
                              "Index",
                              "Baseline",
                              "Frequency",
                              "Phase Center",
                              "Pulsar Bin",
                              "Polarization",
                              "Offset",
                              "Freq. Channels"
                }, 0 );
        _tableScrollPane = new JScrollPane( new JTable( _productTable ) {
            //  This causes the first column to be a checkbox that the user can
            //  change.
            @Override
            public Class getColumnClass(int column) {
                switch (column) {
                    case 0:
                        return Boolean.class;
                    default:
                        return String.class;
                }
            }
        } );
        this.add( _tableScrollPane );
        _tableScrollPane.setVisible( false );
        _dataPanel.add( _tableScrollPane );
                
        
        //  Plots
        _plotPanel = new IndexedPanel( "Plots, etc." );
        _plotPanel.openHeight( 100 );
        _plotPanel.closedHeight( 20 );
        _plotPanel.alwaysOpen( true );
        _plotPanel.noArrow( true );
        _plotWindow = new PlotWindow();
        _plotPanel.add( _plotWindow );
        _scrollPane.addNode( _plotPanel );
        JButton _saveAsButton = new JButton( "Save As... \u25bc" );
        _saveAsButton.setBounds( 270, 2, 100, 18 ); 
        _saveAsButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _fileChooser == null )
                    _fileChooser = new JFileChooser();
                _fileChooser.setDialogTitle( "Save as Encapsulated PostScript File..." );
                int ret = _fileChooser.showSaveDialog( _this );
                if ( ret == JFileChooser.APPROVE_OPTION ) {
                    String foo = _plotWindow.postScriptDraw();
                    try {
                        BufferedWriter out = new BufferedWriter( new FileWriter( _fileChooser.getSelectedFile().getAbsolutePath() ) );
                        out.write( foo );
                        out.close();
                    } catch ( Exception ex ) {}
                }
            }
        } );
        _plotPanel.add( _saveAsButton );
        //  Some control buttons for the plot window.
        _showButton = new JButton( "Show \u25bc" );
        _showButton.setBounds( 150, 2, 100, 18 );
        _showButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showMenu.show( _showButton, 0, _showButton.getHeight() );
            }
        } );
        _plotPanel.add( _showButton );
        _showMenu = new JPopupMenu();
        _showAll = new JCheckBoxMenuItem( "All Accumulation Periods" );
        _showAll.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showLatest.setSelected( false );
                _showAll.setSelected( true );
                _showHistory.setSelected( false );
                updatePlotLocations();
            }
        } );
        _showAll.setSelected( true );
        _showMenu.add( _showAll );
        _showLatest = new JCheckBoxMenuItem( "Latest Only" );
        _showLatest.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showLatest.setSelected( true );
                _showAll.setSelected( false );
                _showHistory.setSelected( false );
                updatePlotLocations();
            }
        } );
        _showLatest.setSelected( false );
        _showMenu.add( _showLatest );
        _showHistory = new JCheckBoxMenuItem( "History" );
        _showHistory.setToolTipText( "The mousewheel will step through the time sequence of collected data.  Applies to the summary as well." );
        _showHistory.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showLatest.setSelected( false );
                _showAll.setSelected( false );
                _showHistory.setSelected( true );
                updatePlotLocations();
            }
        } );
        _showHistory.setSelected( false );
        _showMenu.add( _showHistory );
        _showSummary = new JCheckBoxMenuItem( "Summary" );
        _showSummary.setToolTipText( "Show plots displaying averages across channels and time." );
        _showSummary.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updatePlotLocations();
            }
        } );
        _showSummary.setSelected( false );
        _showMenu.add( _showSummary );
        _showMenu.add( new JSeparator() );
        _showPhase = new JCheckBoxMenuItem( "Phase" );
        _showPhase.setSelected( true );
        _showPhase.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updatePlotLocations();
            }
        } );
        _showMenu.add( _showPhase );
        _showAmp = new JCheckBoxMenuItem( "Amplitude" );
        _showAmp.setSelected( true );
        _showAmp.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updatePlotLocations();
            }
        } );
        _showMenu.add( _showAmp );
        _showLag = new JCheckBoxMenuItem( "Lag (Visibilities FFT)" );
        _showLag.setSelected( true );
        _showLag.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _showLag.isSelected() )
                    _showDelay.setEnabled( true );
                else
                    _showDelay.setEnabled( false );
                updatePlotLocations();
            }
        } );
        _showMenu.add( _showLag );
        _showDelay = new JCheckBoxMenuItem( "Delay Value" );
        _showDelay.setSelected( true );
        _showDelay.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updatePlotLocations();
            }
        } );
        _showMenu.add( _showDelay );
        
        _allObjectsBuilt = true;
        newSize();
        
        // Set ourselves up to intercept window operations (close, iconize, etc).
        addWindowListener( this );
        
        newConnection();
        
    }
    
    /*
     * Window event methods - we need each of these, even though we are only
     * interested in the "Closing" method.
     */
    @Override
    public void windowOpened( WindowEvent e ) {
    }

    @Override
    public void windowClosed( WindowEvent e ) {
    }

    @Override
    public void windowClosing( WindowEvent e ) {
        exitOperation();
    }

    @Override
    public void windowActivated( WindowEvent e ) {
    }

    @Override
    public void windowDeactivated( WindowEvent e ) {
    }

    @Override
    public void windowIconified( WindowEvent e ) {
    }

    @Override
    public void windowDeiconified( WindowEvent e ) {
    }

    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        newSize();
    }
    
    public void newSize() {
        if ( _allObjectsBuilt ) {
            int w = this.getContentPane().getSize().width;
            int h = this.getContentPane().getSize().height;
            _scrollPane.setBounds( 0, 0, w, h );
            _monitorHost.setBounds( 115, 30, 200, 25 );
            _monitorHostLabel.setBounds( 10, 30, 100, 25 );
            _monitorPort.setBounds( 115, 60, 100, 25 );
            _monitorPortLabel.setBounds( 10, 60, 100, 25 );
            _connectionLight.setBounds( 115, 96, 13, 13 );
            _connectionLabel.setBounds( 135, 90, 200, 25 );
            _connectionPlotWindow.setBounds( 335, 30, w - 365, 80 );
            _connectionPlot.frame( 0, 0, 1.0, 1.0 );
            if ( _inputFile == null )
                _inputFileComboBox.setBounds( 115, 30, w - 145, 25 );
            else
                _inputFileOutput.setBounds( 115, 30, w - 145, 25 );
            _inputFileLabel.setBounds( 10, 30, 100, 25 );
            _automaticallyResize = true;
            setControlObjectSizes();
            if ( _scrollPane.scrollBarVisible() )
                _messages.setBounds( 0, 20, w - 20, _messagePanel.getHeight() - 23 );
            else
                _messages.setBounds( 0, 20, w, _messagePanel.getHeight() - 23 );
            if ( h - _connectionPanel.getHeight() - _dataPanel.getHeight() < 0 )
                _plotPanel.openHeight( 0 );
            else
                _plotPanel.openHeight( h - _connectionPanel.getHeight() - _dataPanel.getHeight() - _messagePanel.getHeight() - 3 );
            _plotWindow.setBounds( 0, 20, w, _plotPanel.openHeight() - 16 );
        }
    }
    
    /*
     * This function is called when a new connection to the guiServer and
     * monitor_server is required.  It triggers new
     * data requests from the monitor_server via the guiServer.
     */
    public void newConnection() {
        //  Clear the data controls and plots - we are starting anew.
        
        //  Start some threads.  If this function is ever to be truly flexible it
        //  should stop the threads and restart them, or do something similar.
        
        //  This thread monitors the traffic on our data/control connection with
        //  the guiServer.
        _dataTransferMonitorThread = new DataTransferMonitorThread();
        _dataTransferMonitorThread.start();
        
        //  This thread actually forms and operates the data/control connection.
        _connectionThread = new ConnectionThread();
        _connectionThread.start();
        
    }
    
    /*
     * Change the connection light and associated message.
     */
    void connectionInfo( String status, String label ) {
        if ( status.equalsIgnoreCase( "CONNECTED" ) ) {
            _connectionLight.on( true );
        }
        else if ( status.equalsIgnoreCase( "CONNECTING" ) ) {
            _connectionLight.warning();
        }
        else if ( status.equalsIgnoreCase( "NOT CONNECTED" ) ) {
            _connectionLight.alert();
        }
        _connectionLabel.setText( label );
    }
    
    /*
     * Change the connection parameters.  This will cause the connection to close
     * and reform using the new parameters.
     */
    void changeConnection() {
        _connected = false;
    }

    /*
     * Try making a connection to the DiFX monitor server using the current settings
     * for host and port.
     */
    void makeConnection() {
        _connectionLight.warning();
        _connectionLabel.setText( "connecting..." );
        try {
            _socket = new Socket( _settings.difxMonitorHost(), _usingPort );
            _socket.setSoTimeout( _settings.timeout() );
            if ( _socket.isConnected() ) {
                _in = new ExtendedDataInputStream( _socket.getInputStream() );
                _out = new DataOutputStream( _socket.getOutputStream() );
                //  Read the connection status message from the server.  This should match
                //  the "no error" message.
                _connectionLight.on( true );
                _connectionLabel.setText( "connected" );
                _connected = true;
            }
            else {
                _connected = false;
                _connectionLabel.setText( "connection failed" );
                _connectionLight.alert();
            }
        } catch ( java.net.UnknownHostException e ) {
            _connectionLight.alert();
            _connected = false;
        } catch ( java.io.IOException e ) {
            _connectionLight.alert();
            _connected = false;
        }
    }
    
    //  Packet types used in the communications exchange with guiServer.
    protected final int MESSAGE                            = 100;
    protected final int WARNING                            = 101;
    protected final int ERROR                              = 102;
    protected final int INPUT_FILE_PATH                    = 103;
    protected final int CLOSE_CONNECTION                   = 104;
    protected final int NUM_BASELINES                      = 105;
    protected final int NUM_FREQUENCIES                    = 106;
    protected final int BASELINE                           = 107;
    protected final int FREQUENCY                          = 108;
    protected final int NUM_SCANS                          = 109;
    protected final int SCAN                               = 110;
    protected final int TELESCOPE_1                        = 111;
    protected final int TELESCOPE_2                        = 112;
    protected final int CORRELATION_PRODUCTS               = 113;
    protected final int NUM_PHASE_CENTERS                  = 114;
    protected final int PHASE_CENTER                       = 115;
    protected final int NUM_PULSAR_BINS                    = 116;
    protected final int PULSAR_BIN                         = 117;
    protected final int NUM_POL_PRODUCTS                   = 118;
    protected final int POL_PRODUCT                        = 119;
    protected final int NEW_PRODUCT                        = 120;
    protected final int AUTOCORRELATION                    = 121;
    protected final int PRODUCT_REQUEST                    = 122;
    protected final int START_PRODUCT_REQUESTS             = 123;
    protected final int END_PRODUCT_REQUESTS               = 124;
    protected final int VISIBILITY_DATA                    = 125;
    protected final int AMPLITUDE_DATA                     = 126;
    protected final int PHASE_DATA                         = 127;
    protected final int LAG_DATA                           = 128;
        
    /*
     * Send a packet with ID, number of bytes, and data.
     */
    public void sendPacket( int packetId, int nBytes, byte[] data ) {
        if ( _connected ) {
            try {
                _out.writeInt( packetId );
                _out.writeInt( nBytes );
                if ( data != null && data.length > 0 ) {
                    _out.write( data );
                }
            } catch ( java.io.IOException e ) {
                //  socket failure
                _connected = false;
            }
        }
    }

    /*
     * Measure and send a string along with a packet ID.
     */
    public void sendString( int packetId, String str ) {
        byte [] data = str.getBytes();
        sendPacket( packetId, data.length, data );
        data = null;
    }
        
    /*
     * Class for handling the data connection.
     */
    protected class ConnectionThread extends Thread {
        
        public ConnectionThread() {
            _keepGoing = true;
        }
        
        public void stopIt() {
            _keepGoing = false;
        }
        
        public void closeConnection() {
            sendPacket( CLOSE_CONNECTION, 0, null );
            _connected = false;
            _keepGoing = false;
        }
        
        public void run() {
            //  Send a request to the guiServer to start the thread that will
            //  talk to monitor_server.  The thread will start a server at the
            //  specified port.
            java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate(4);
            _usingPort = _settings.newDifxTransferPort();
            _monitorPort.setText( new Integer( _usingPort ).toString() );
            bb.putInt( _usingPort ); 
            byte [] intData = bb.array();
            _settings.guiServerConnection().sendPacket( _settings.guiServerConnection().START_DIFX_MONITOR, intData.length, intData );
            while ( _keepGoing ) {
                
                makeConnection();
                //  Send the path to the input file.
                sendString( INPUT_FILE_PATH, _inputFile );                    
                while ( _connected ) {
                    try {
                        //  Read the packet type as an integer.  The packet types
                        //  are defined above (within this class).
                        int packetType = _in.readInt();
                        //  Read the size of the incoming data (bytes).
                        int packetSize = _in.readInt();
                        //---------------------------------------------------------------------
                        //  These are messages that are written directly to the message window.
                        //---------------------------------------------------------------------
                        if ( packetType == MESSAGE ) {
                            byte [] data = null;
                            if ( packetSize > 0 ) {
                                data = new byte[packetSize];
                                _in.readFully( data );
                            }
                            if ( data != null && data.length > 0 )
                                _messages.message( 0, "", new String( data ) );
                        }
                        else if ( packetType == WARNING ) {
                            byte [] data = null;
                            if ( packetSize > 0 ) {
                                data = new byte[packetSize];
                                _in.readFully( data );
                            }
                            if ( data != null && data.length > 0 )
                                _messages.warning( 0, "", new String( data ) );
                        }
                        else if ( packetType == ERROR ) {
                            byte [] data = null;
                            if ( packetSize > 0 ) {
                                data = new byte[packetSize];
                                _in.readFully( data );
                            }
                            if ( data != null && data.length > 0 )
                                _messages.error( 0, "", new String( data ) );
                        }
                        //---------------------------------------------------------------------------
                        //  The following are data received from the input file parser.  These data
                        //  are used to tell us what data products are available for the job
                        //  specified by the input file.
                        //---------------------------------------------------------------------------
                        else if ( packetType == CORRELATION_PRODUCTS ) {
                            //  Indicates the beginning or end of desciptions of cross
                            //  correlations.
                            if ( _readingProducts ) {
                                updateDataProductControls();
                            }
                            else {
                                //  Blow away our current list of products.  We'll be making
                                //  a new one.
                                _products = new ArrayDeque<Product>();
                            }
                            _readingProducts = !_readingProducts;
                        }
                        else if ( packetType == NUM_SCANS ) {
                            //  Number of scans available.  This is probably always 1, ignore it
                            //  for now.
                            _nScans = _in.readInt();
                        }
                        else if ( packetType == SCAN ) {
                            //  Usually 0.  Ignore for now.
                            _scan = _in.readInt();
                        }
                        else if ( packetType == NUM_BASELINES ) {
                            //  Number of baselines in the current scan.
                            _nBaselines = _in.readInt();
                        }
                        else if ( packetType == BASELINE ) {
                            //  This is a baseline index number.
                            _currentBaseline = new Baseline();
                            _currentBaseline.index = _in.readInt();
                        }
                        else if ( packetType == TELESCOPE_1 ) {
                            //  "Reference" telescope in the current baseline pair.
                            byte [] data = new byte[packetSize];
                            _in.readFully( data );
                            _currentBaseline.telescope1 = new String( data );
                        }
                        else if ( packetType == TELESCOPE_2 ) {
                            //  Second telescope in the current baseline pair.
                            byte [] data = new byte[packetSize];
                            _in.readFully( data );
                            _currentBaseline.telescope2 = new String( data );
                        }
                        else if ( packetType == AUTOCORRELATION ) {
                            //  This is the name of a single telescope used in an autocorrelation.
                            //  These we essentially fake by making the "baseline" between the 
                            //  same telescope.
                            _currentBaseline = new Baseline();
                            byte [] data = new byte[packetSize];
                            _in.readFully( data );
                            _currentBaseline.telescope1 = new String( data );
                            _currentBaseline.telescope2 = new String( data );
                        }
                        else if ( packetType == NUM_FREQUENCIES ) {
                            //  Number of frequencies available for the current
                            //  baseline pair.
                            _nFrequencies = _in.readInt();
                        }
                        else if ( packetType == FREQUENCY ) {
                            //  This is one of the frequencies available to this baseline
                            //  pair.  It is a double.
                            _currentFrequency = _in.readDouble();
                        }
                        else if ( packetType == NUM_PHASE_CENTERS ) {
                            //
                            _nPhaseCenters = _in.readInt();
                        }
                        else if ( packetType == PHASE_CENTER ) {
                            //
                            _currentPhaseCenter = _in.readInt();
                        }
                        else if ( packetType == NUM_PULSAR_BINS ) {
                            //
                            _nPulsarBins = _in.readInt();
                        }
                        else if ( packetType == PULSAR_BIN ) {
                            //
                            _currentPulsarBin = _in.readInt();
                        }
                        else if ( packetType == NUM_POL_PRODUCTS ) {
                            //
                            _nPolProducts = _in.readInt();
                        }
                        else if ( packetType == POL_PRODUCT ) {
                            //
                            _currentPolProduct = _in.readInt();
                        }
                        else if ( packetType == NEW_PRODUCT ) {
                            Product newProduct = new Product();
                            newProduct.index = _in.readInt();
                            newProduct.offset = _in.readInt();
                            newProduct.freqChannels = _in.readInt();
                            newProduct.frequency = _currentFrequency;
                            newProduct.baseline = _currentBaseline;
                            newProduct.polProduct = _currentPolProduct;
                            _products.add( newProduct );
                        }
                        else if ( packetType == VISIBILITY_DATA ) {
                            byte [] data = null;
                            if ( packetSize > 0 ) {
                                data = new byte[packetSize];
                                _in.readFully( data );
                                _bytesTransfered += packetSize;
                            }
                        }
                        else if ( packetType == AMPLITUDE_DATA ) {
                            int iProduct = _in.readInt();
                            int nChannels = _in.readInt();
                            int timeStamp = _in.readInt();
                            IncPlot newPlot = new IncPlot( iProduct, nChannels, timeStamp );
                            double x = 0.0;
                            double maxVal = 0.0;
                            double xVals[] = new double[nChannels];
                            double yVals[] = new double[nChannels];
                            for ( int i = 0; i < nChannels; ++i ) {
                                double amp = _in.readStringDouble();                           
                                xVals[i] = x;
                                yVals[i] = amp;
                                x += 1.0;
                                if ( amp > maxVal )
                                    maxVal = amp;
                            }
                            //  The minimum for amplitude *should* be 0.0...
                            newPlot.min = 0.0;
                            newPlot.max = maxVal;
                            newPlot.curve( xVals, yVals );
                            _productPlots.productPlot( iProduct ).ampPlots.add( newPlot );
                        }
                        else if ( packetType == PHASE_DATA ) {
                            int iProduct = _in.readInt();
                            int nChannels = _in.readInt();
                            int timeStamp = _in.readInt();
                            IncPlot newPlot = new IncPlot( iProduct, nChannels, timeStamp );
                            double x = 0.0;
                            double xVals[] = new double[nChannels];
                            double yVals[] = new double[nChannels];
                            for ( int i = 0; i < nChannels; ++i ) {
                                double phase = _in.readStringDouble();                           
                                xVals[i] = x;
                                yVals[i] = phase;
                                x += 1.0;
                            }
                            DrawObject trackCircle = new DrawObject();
                            //  This draws a circle around the point...doesn't look so great
                            //trackCircle.circle( 0.0, 0.0, 3.0, true );
                            //  Try using a "cross" character...also looks crappy
                            //trackCircle.complexText( DrawObject.CENTER_JUSTIFY, 0.0, 0.0, "<size=0.5><y=-0.4>\u271a" );
                            //  Draw a cross using line segments - looks cleaner.
                            DrawObject path = new DrawObject();
                            path.startPath( 0.0, 0.0 );
                            DrawObject vertex = new DrawObject();
                            vertex.vertex( 0.0, -3.0 );
                            path.add( vertex );
                            vertex = new DrawObject();
                            vertex.vertex( 0.0, 3.0 );
                            path.add( vertex );
                            DrawObject stroke = new DrawObject();
                            stroke.stroke();
                            path.add( stroke );
                            trackCircle.add( path );
                            path = new DrawObject();
                            path.startPath( 0.0, 0.0 );
                            vertex = new DrawObject();
                            vertex.vertex( -3.0, 0.0 );
                            path.add( vertex );
                            vertex = new DrawObject();
                            vertex.vertex( 3.0, 0.0 );
                            path.add( vertex );
                            stroke = new DrawObject();
                            stroke.stroke();
                            path.add( stroke );
                            trackCircle.add( path );
                            newPlot.track( xVals, yVals, trackCircle );
                            newPlot.min = -180.0;
                            newPlot.max = 180.0;
                            _productPlots.productPlot( iProduct ).phasePlots.add( newPlot );
                        }
                        else if ( packetType == LAG_DATA ) {
                            int iProduct = _in.readInt();
                            int nChannels = _in.readInt();
                            int timeStamp = _in.readInt();
                            //  Create a new plot to hold these data.
                            IncPlot newPlot = new IncPlot( iProduct, nChannels, timeStamp );
                            newPlot.maxChannel = _in.readInt();
                            newPlot.delay = _in.readStringDouble();
                            newPlot.snr = _in.readStringDouble();
                            double x = (double)(-nChannels);
                            Double maxVal = null;
                            Double minVal = null;
                            double xVals[] = new double[2 * nChannels];
                            double yVals[] = new double[2 * nChannels];
                            for ( int i = 0; i < 2 * nChannels; ++i ) {
                                double lag = _in.readStringDouble();                           
                                if ( maxVal == null || lag > maxVal )
                                    maxVal = lag;
                                if ( minVal == null || lag < minVal )
                                    minVal = lag;
                                xVals[i] = x;
                                yVals[i] = lag;
                                x += 1.0;     
                            }
                            newPlot.curve( xVals, yVals );
                            newPlot.min = minVal;
                            newPlot.max = maxVal;
                            //  Add the plot to the product plot list for this index.
                            _productPlots.productPlot( iProduct ).lagPlots.add( newPlot );
                            updatePlotLocations();
                        }
                    } catch ( java.io.IOException e ) {
                        //  This should just be a timeout.
                    }
                }
                try {
                    if ( _socket != null )
                        _socket.close();
                } catch ( java.io.IOException e ) {}
                try { Thread.sleep( 1000 ); } catch( Exception e ) {}
            }
        }
        
        protected boolean _keepGoing;        
            
    }
    
    /*
     * Relocate all existing plots to fit in the current plot window.
     */
    public void updatePlotLocations() {
        //  Clear all plots - we are going to redraw them.
        _plotWindow.plotList().clear();
        
        //  Count the number of data products we are plotting (exit if there aren't
        //  any).
        if ( _productPlots == null || _productPlots.size() == 0 )
            return;
        double dataProducts = (double)_productPlots.size();
        //  Count the number of plots for each data product.  We find a maximum number
        //  so all of the plots will line up, although the number *should* be the same
        //  for all data products.
        int maxPlots = 0;
        for ( Iterator<ProductPlots> iter = _productPlots.iterator(); iter.hasNext(); ) {
            ProductPlots thisPP = iter.next();
            if ( thisPP.lagPlots.size() > maxPlots )
                maxPlots = thisPP.lagPlots.size();
        }        
        
        double yStart = 0.05;
        double yStep = 0.85 / dataProducts;
        double ySize = yStep * 0.97;
        for ( Iterator<ProductPlots> iter = _productPlots.iterator(); iter.hasNext(); ) {
            ProductPlots thisPP = iter.next();
            //  Find the "global" maximum and minimum values for lag and amplitude plots.
            double lagMin = 0.0;
            double lagMax = 0.0;
            boolean lagLimitsSet = false;
            for ( Iterator<IncPlot> pIter = thisPP.lagPlots.iterator(); pIter.hasNext(); ) {
                IncPlot incPlot = pIter.next();
                if ( lagLimitsSet ) {
                    if ( incPlot.min < lagMin )
                        lagMin = incPlot.min;
                    if ( incPlot.max > lagMax )
                        lagMax = incPlot.max;
                }
                else {
                    lagMin = incPlot.min;
                    lagMax = incPlot.max;
                    lagLimitsSet = true;
                }
            }
            double ampMin = 0.0;
            double ampMax = 0.0;
            boolean ampLimitsSet = false;
            for ( Iterator<IncPlot> pIter = thisPP.ampPlots.iterator(); pIter.hasNext(); ) {
                IncPlot incPlot = pIter.next();
                if ( ampLimitsSet ) {
                    if ( incPlot.min < ampMin )
                        ampMin = incPlot.min;
                    if ( incPlot.max > ampMax )
                        ampMax = incPlot.max;
                }
                else {
                    ampMin = incPlot.min;
                    ampMax = incPlot.max;
                    ampLimitsSet = true;
                }
            }
            //  This is done only if the LATEST plots are to be drawn.  They are scaled to fill
            //  the existing screen.
            if ( _showLatest.isSelected() ) {
                double xSize = 0.75;
                double xStart = 0.16;
                //  Draw a "background" plot.  This has any items we want below the data.
                Plot2DObject backgroundPlot = new Plot2DObject();
                backgroundPlot.backgroundColor( Color.WHITE );
                backgroundPlot.frame( xStart, yStart, xSize, ySize );
                backgroundPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                backgroundPlot.drawBackground( true );
                backgroundPlot.drawFrame( false );
                _plotWindow.add2DPlot( backgroundPlot );
                //  Clear "extra" items from each plot.  These will be redrawn as necessary.
                for ( Iterator<IncPlot> pIter = thisPP.lagPlots.iterator(); pIter.hasNext(); )
                    pIter.next().plot.clearExtraItems();
                for ( Iterator<IncPlot> pIter = thisPP.ampPlots.iterator(); pIter.hasNext(); )
                    pIter.next().plot.clearExtraItems();
                for ( Iterator<IncPlot> pIter = thisPP.phasePlots.iterator(); pIter.hasNext(); )
                    pIter.next().plot.clearExtraItems();
                //  The lag (delay) plots.....
                if ( _showLag.isSelected() && thisPP.lagPlots.peekLast() != null ) {
                    IncPlot incPlot = thisPP.lagPlots.peekLast();
                    Plot2DObject thisPlot = thisPP.lagPlots.peekLast().plot;
                    if ( thisPlot != null ) {
                        thisPlot.frame( xStart, yStart, xSize, ySize );
                        thisPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                        thisPlot.deleteLabels();
                        _plotWindow.add2DPlot( thisPlot );
                        incPlot.curve.color( Color.BLUE );
                        double useMin = incPlot.min;
                        double useMax = incPlot.max;
                        if ( lagLimitsSet ) {
                            useMin = lagMin;
                            useMax = lagMax;
                        }
                        thisPlot.limits( (double)(-incPlot.nChannels), (double)(incPlot.nChannels - 1), useMin, useMax );
                        thisPlot.drawBackground( false );
                        thisPlot.drawFrame( false );
                        //  Add X labels (to the last plot).
                        if ( !iter.hasNext() )
                            thisPlot.addLabels( Plot2DObject.X_AXIS, 10.0 );
                        //  Add product information to this plot.
                        plotProductInformation( incPlot, -0.14 / xSize );
                        //  If both the phase and amplitude are included, shrink this plot to 2/3 height.
                        if ( _showPhase.isSelected() && _showAmp.isSelected() ) {
                            useMax = useMin + 3.0 * ( useMax - useMin ) / 2.0;
                            thisPlot.limits( (double)(-incPlot.nChannels), (double)(incPlot.nChannels - 1), useMin, useMax );
                        }
                        //  If phase is shown, shrink to half height.
                        else if ( _showPhase.isSelected() ) {
                            useMax = useMin + 2.0 * ( useMax - useMin );
                            thisPlot.limits( (double)(-incPlot.nChannels), (double)(incPlot.nChannels - 1), useMin, useMax );
                        }
                        double stepSize = Plot2DObject.stepSize( lagMax - useMin, 2, 5 );
                        thisPlot.addLabels( Plot2DObject.Y_AXIS, null, lagMax, stepSize,
                                Color.BLUE, null, -5.0, -10.0,
                                Plot2DObject.RIGHT_JUSTIFY, null, false, false );
                        thisPlot.yTitle( "Lag", Plot2DObject.CENTER_JUSTIFY );
                        thisPlot.yTitleXPos( -60, false );
                        thisPlot.yTitleYPos( -0.5 * ( lagMax - useMin ) / ( useMax - useMin ), true );
                        thisPlot.yTitleRotate( -Math.PI / 2.0 );
                        thisPlot.yTitleColor( Color.BLUE, true );
                        if ( _showDelay.isSelected() ) {
                            thisPlot.addLabels( Plot2DObject.X_AXIS, (double)(incPlot.maxChannel),
                                    (double)(incPlot.maxChannel) + .1, 1.0, Color.MAGENTA, 
                                    new String( "  delay = " + incPlot.delay ),
                                    ySize, 0.0, Plot2DObject.LEFT_JUSTIFY,
                                    useMax, false, 
                                    false );
                        }
                    }
                }
                //  Amplitude plots...
                if ( _showAmp.isSelected() && thisPP.ampPlots.peekLast() != null ) {
                    IncPlot incPlot = thisPP.ampPlots.peekLast();
                    Plot2DObject thisPlot = thisPP.ampPlots.peekLast().plot;
                    if ( thisPlot != null ) {
                        thisPlot.frame( xStart, yStart, xSize, ySize );
                        thisPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                        thisPlot.deleteLabels();
                        _plotWindow.add2DPlot( thisPlot );
                        incPlot.curve.color( Color.RED );
                        thisPlot.drawBackground( false );
                        thisPlot.drawFrame( false );
                        //  Set limits to match the data.  These might be changed below.
                        double plotYMin = incPlot.min;
                        double plotYMax = incPlot.max;
                        if ( ampLimitsSet ) {
                            plotYMin = ampMin;
                            plotYMax = ampMax;
                        }
                        thisPlot.limits( 0.0, (double)(incPlot.nChannels - 1), plotYMin, plotYMax );
                        //  If there are other things being plotted, set the amp limits such that the
                        //  curve covers only the lower half of the plot.
                        if ( _showLag.isSelected() || _showPhase.isSelected() ) {
                            plotYMax = 2.0 * plotYMax;
                            thisPlot.limits( 0.0, (double)(incPlot.nChannels - 1), plotYMin, plotYMax );
                        }
                        //  If the lags weren't plotted, do these things...
                        if ( !_showLag.isSelected() ) {
                            //  Draw X labels (on the last plot).
                            if ( !iter.hasNext() )
                                thisPlot.addLabels( Plot2DObject.X_AXIS, 10.0 ); 
                            //  Add product information to this plot.
                            plotProductInformation( incPlot, -0.14 / xSize );
                        }
                        //  Find a reasonable step size for the amplitude.
                        double stepSize = Plot2DObject.stepSize( plotYMax - plotYMin, 2, 5 );
                        thisPlot.addLabels( Plot2DObject.Y_AXIS, plotYMin, plotYMax, stepSize,
                                Color.RED, null, 5.0, 10.0,
                                Plot2DObject.LEFT_JUSTIFY, (double)(incPlot.nChannels - 1), false, false );
                        thisPlot.yTitle( "Amplitude", Plot2DObject.CENTER_JUSTIFY );
                        thisPlot.yTitleXPos( 1.08, true );
                        thisPlot.yTitleYPos( -0.5, true );
                        thisPlot.yTitleRotate( -Math.PI / 2.0 );
                        thisPlot.yTitleColor( Color.RED, true );
                    }
                }
                //  Phase plots...
                if ( _showPhase.isSelected() && thisPP.phasePlots.peekLast() != null ) {
                    IncPlot incPlot = thisPP.phasePlots.peekLast();
                    Plot2DObject thisPlot = thisPP.phasePlots.peekLast().plot;
                    if ( thisPlot != null ) {
                        thisPlot.frame( xStart, yStart, xSize, ySize );
                        thisPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                        thisPlot.deleteLabels();
                        _plotWindow.add2DPlot( thisPlot );
                        incPlot.track.color( Color.BLACK );
                        thisPlot.drawBackground( false );
                        thisPlot.drawFrame( false );
                        //  Set limits for phase...-180 to 180.
                        thisPlot.limits( 0.0, (double)(incPlot.nChannels - 1), -180.0, 180.0 );
                        //  If the lag or amplitude has already been drawn, shut off the background and
                        //  squish this plot into the top half of the plotting area.
                        if ( _showLag.isSelected() || _showAmp.isSelected() ) {
                            thisPlot.frame( xStart, yStart, xSize, ySize / 2.0 );
                            thisPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                        }
                        //  These are some things we do if this is the ONLY plot.
                        if ( !_showLag.isSelected() && !_showAmp.isSelected() ) {
                            //  Draw X labels (on the last plot).
                            if ( !iter.hasNext() )
                                thisPlot.addLabels( Plot2DObject.X_AXIS, 10.0 ); 
                            //  Add product information to this plot.
                            plotProductInformation( incPlot, -0.14 / xSize );
                        }
                        thisPlot.addLabels( Plot2DObject.Y_AXIS, 180.0 );
                        thisPlot.yTitle( "Phase", Plot2DObject.CENTER_JUSTIFY );
                        thisPlot.yTitleXPos( -40, false );
                        thisPlot.yTitleYPos( -0.5, true );
                        thisPlot.yTitleRotate( -Math.PI / 2.0 );
                    }
                }
                //  Draw a "frame" plot.  This has any items we want drawn on top of the data.
                Plot2DObject framePlot = new Plot2DObject();
                framePlot.frameColor( Color.DARK_GRAY );
                framePlot.frame( xStart, yStart, xSize, ySize );
                framePlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                framePlot.drawBackground( false );
                framePlot.drawFrame( true );
                _plotWindow.add2DPlot( framePlot );
                yStart += yStep;
            }
            //  Here we show all plots in the series.
            else {
                double xStart = 0.16;
                double xStep = 0.75 / maxPlots;
                double xSize = xStep * 0.98;
                //  Draw a "background" plot below each plot.  This has any items we want below the data.
                for ( Iterator<IncPlot> pIter = thisPP.lagPlots.iterator(); pIter.hasNext(); ) {
                    IncPlot incPlot = pIter.next();
                    Plot2DObject backgroundPlot = new Plot2DObject();
                    backgroundPlot.backgroundColor( Color.WHITE );
                    backgroundPlot.frame( xStart, yStart, xSize, ySize );
                    backgroundPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                    backgroundPlot.drawBackground( true );
                    backgroundPlot.drawFrame( false );
                    _plotWindow.add2DPlot( backgroundPlot );
                    xStart += xStep;
                }
                //  Delay (lag) plots.
                if ( _showLag.isSelected() ) {
                    xStart = 0.16;
                    for ( Iterator<IncPlot> pIter = thisPP.lagPlots.iterator(); pIter.hasNext(); ) {
                        IncPlot incPlot = pIter.next();
                        Plot2DObject thisPlot = incPlot.plot;
                        thisPlot.deleteLabels();
                        thisPlot.deleteTitles();
                        thisPlot.clearExtraItems();
                        double useMin = incPlot.min;
                        double useMax = incPlot.max;
                        if ( lagLimitsSet ) {
                            useMin = lagMin;
                            useMax = lagMax;
                        }
                        thisPlot.limits( (double)(-incPlot.nChannels), (double)(incPlot.nChannels - 1), useMin, useMax );
                        thisPlot.frame( xStart, yStart, xSize, ySize );
                        xStart += xStep;
                        thisPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                        _plotWindow.add2DPlot( thisPlot );
                        incPlot.curve.color( Color.BLUE );
                        thisPlot.drawBackground( false );
                        thisPlot.drawFrame( false );
                        //  If both the phase and amplitude are included, shrink this plot to 2/3 height.
                        if ( _showPhase.isSelected() && _showAmp.isSelected() ) {
                            useMax = useMin + 3.0 * ( useMax - useMin ) / 2.0;
                            thisPlot.limits( (double)(-incPlot.nChannels), (double)(incPlot.nChannels - 1), useMin, useMax );
                        }
                        //  If phase is shown, shrink to half height.
                        else if ( _showPhase.isSelected() ) {
                            useMax = useMin + 2.0 * ( useMax - useMin );
                            thisPlot.limits( (double)(-incPlot.nChannels), (double)(incPlot.nChannels - 1), useMin, useMax );
                        }
                        //  If this is the first plot, add labels to y axis.
                        if ( incPlot == thisPP.lagPlots.peekFirst() ) {
                            double stepSize = Plot2DObject.stepSize( lagMax - useMin, 2, 5 );
                            thisPlot.addLabels( Plot2DObject.Y_AXIS, null, lagMax, stepSize,
                                    Color.BLUE, null, -5.0, -10.0,
                                    Plot2DObject.RIGHT_JUSTIFY, null, false, false );
                            thisPlot.yTitle( "Delay", Plot2DObject.CENTER_JUSTIFY );
                            thisPlot.yTitleXPos( -60, false );
                            thisPlot.yTitleYPos( -0.5 * ( lagMax - useMin ) / ( useMax - useMin ), true );
                            thisPlot.yTitleRotate( -Math.PI / 2.0 );
                            thisPlot.yTitleColor( Color.BLUE, true );
                        }
                    }
                }
                //  Amplitude plots...
                if ( _showAmp.isSelected() ) {
                    xStart = 0.16;
                    for ( Iterator<IncPlot> pIter = thisPP.ampPlots.iterator(); pIter.hasNext(); ) {
                        IncPlot incPlot = pIter.next();
                        Plot2DObject thisPlot = incPlot.plot;
                        if ( thisPlot != null ) {
                            thisPlot.frame( xStart, yStart, xSize, ySize );
                            xStart += xStep;
                            thisPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                            thisPlot.deleteLabels();
                            thisPlot.deleteTitles();
                            _plotWindow.add2DPlot( thisPlot );
                            incPlot.curve.color( Color.RED );
                            thisPlot.drawBackground( false );
                            thisPlot.drawFrame( false );
                            //  Set limits to match the data.  These might be changed below.
                            double plotYMin = incPlot.min;
                            double plotYMax = incPlot.max;
                            if ( ampLimitsSet ) {
                                plotYMin = ampMin;
                                plotYMax = ampMax;
                            }
                            thisPlot.limits( 0.0, (double)(incPlot.nChannels - 1), plotYMin, plotYMax );
                            //  If there are other things being plotted, set the amp limits such that the
                            //  curve covers only the lower half of the plot.
                            if ( _showLag.isSelected() || _showPhase.isSelected() ) {
                                plotYMax = 2.0 * plotYMax;
                                thisPlot.limits( 0.0, (double)(incPlot.nChannels - 1), plotYMin, plotYMax );
                            }
                            //  If this is the last plot, add labels for amplitude.
                            if ( !pIter.hasNext() ) {
                                double stepSize = Plot2DObject.stepSize( plotYMax - plotYMin, 2, 5 );
                                thisPlot.addLabels( Plot2DObject.Y_AXIS, plotYMin, plotYMax, stepSize,
                                        Color.RED, null, 5.0, 10.0,
                                        Plot2DObject.LEFT_JUSTIFY, (double)(incPlot.nChannels - 1), false, false );
                                thisPlot.yTitle( "Amplitude", Plot2DObject.CENTER_JUSTIFY );
                                thisPlot.yTitleXPos( 1.0 + 0.08 * (double)(thisPP.ampPlots.size()), true );
                                thisPlot.yTitleYPos( -0.5, true );
                                thisPlot.yTitleRotate( -Math.PI / 2.0 );
                                thisPlot.yTitleColor( Color.RED, true );
                            }
                        }
                    }
                }
                //  Phase plots...
                if ( _showPhase.isSelected() ) {
                    xStart = 0.16;
                    for ( Iterator<IncPlot> pIter = thisPP.phasePlots.iterator(); pIter.hasNext(); ) {
                        IncPlot incPlot = pIter.next();
                        Plot2DObject thisPlot = incPlot.plot;
                        if ( thisPlot != null ) {
                            thisPlot.frame( xStart, yStart, xSize, ySize );
                            thisPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                            thisPlot.deleteLabels();
                            thisPlot.deleteTitles();
                            _plotWindow.add2DPlot( thisPlot );
                            incPlot.track.color( Color.BLACK );
                            thisPlot.drawBackground( false );
                            thisPlot.drawFrame( false );
                            //  Set limits for phase...-180 to 180.
                            thisPlot.limits( 0.0, (double)(incPlot.nChannels - 1), -180.0, 180.0 );
                            //  If the lag or amplitude has already been drawn, shut off the background and
                            //  squish this plot into the top half of the plotting area.
                            if ( _showLag.isSelected() || _showAmp.isSelected() ) {
                                thisPlot.frame( xStart, yStart, xSize, ySize / 2.0 );
                                thisPlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                            }
                            if ( incPlot == thisPP.phasePlots.peekFirst() ) {
                                thisPlot.addLabels( Plot2DObject.Y_AXIS, 180.0 );
                                thisPlot.yTitle( "Phase", Plot2DObject.CENTER_JUSTIFY );
                                thisPlot.yTitleXPos( -40, false );
                                thisPlot.yTitleYPos( -0.5, true );
                                thisPlot.yTitleRotate( -Math.PI / 2.0 );
                            }
                            xStart += xStep;
                        }
                    }
                }
                //  Add product information to the first plot in the series.
                plotProductInformation( thisPP.lagPlots.peekFirst(), -0.14 / xStep );
                //  Draw a "frame" plot on top of each plot.  This has any items we want drawn on top of the data.
                //  We are using the lag plots as an iterator, but any data type would do.
                xStart = 0.16;
                for ( Iterator<IncPlot> pIter = thisPP.lagPlots.iterator(); pIter.hasNext(); ) {
                    IncPlot incPlot = pIter.next();
                    Plot2DObject framePlot = new Plot2DObject();
                    framePlot.frameColor( Color.DARK_GRAY );
                    framePlot.frame( xStart, yStart, xSize, ySize );
                    framePlot.resizeBasedOnWindow( _plotWindow.getWidth(), _plotWindow.getHeight() );
                    framePlot.drawBackground( false );
                    framePlot.drawFrame( true );
                    _plotWindow.add2DPlot( framePlot );
                    //  Add the time stamp if this is the last plot.
                    if ( !iter.hasNext() ) {
                        DrawObject newObject = framePlot.newExtraItem( 0.0, Plot2DObject.ExtraItem.BY_FRAME, 
                            -1.0, Plot2DObject.ExtraItem.BY_FRAME );
                        newObject.complexText( DrawObject.LEFT_JUSTIFY,
                            ( "<y=1.5>" + incPlot.timeStamp ) );                        
                    }
                    xStart += xStep;
                }
                yStart += yStep;
            }
        }
                
        _plotWindow.updateUI();
    }
    
    /*
     * Add information about the product to this plot.  This is all put on the left
     * hand side of the plot, which is assumed to be empty.
     */
    void plotProductInformation( IncPlot incPlot, double xPos ) {
        if ( incPlot == null )
            return;
        Plot2DObject thisPlot = incPlot.plot;
        //  Find the product we are interested in.
        Product product = null;
        for ( Iterator<Product> iter = _products.iterator(); iter.hasNext() && product == null; ) {
            Product thisProduct = iter.next();
            if ( thisProduct.index == incPlot.iProduct )
                product = thisProduct;
        }
        if ( product != null ) {
            DrawObject newObject = thisPlot.newExtraItem( xPos, Plot2DObject.ExtraItem.BY_FRAME, 
                    0.0, Plot2DObject.ExtraItem.BY_FRAME );
            newObject.complexText( DrawObject.LEFT_JUSTIFY,
                    " <y=1>" + product.baseline.telescope1 + "-" + product.baseline.telescope2 );
            newObject = thisPlot.newExtraItem( xPos, Plot2DObject.ExtraItem.BY_FRAME, 
                    0.0, Plot2DObject.ExtraItem.BY_FRAME );
            newObject.complexText( DrawObject.LEFT_JUSTIFY,
                    " <y=2>" + product.frequency + " MHz\n" );
        }
    }
    
    //  Simple class that locates strings in an ArrayDeque.
    protected class StringList extends ArrayDeque<String> {
        public boolean contains( String testString ) {
            for ( Iterator<String> iter = this.iterator(); iter.hasNext(); ) {
                if ( iter.next().contentEquals( testString ) )
                    return true;
            }
            return false;
        }
        //  Return true if a new string is unique.  Unique ones are added to the
        //  array deque.
        public boolean addUnique( String newString ) {
            if ( !contains( newString ) ) {
                add( newString );
                return true;
            }
            return false;
        }
        
    };
    
    /*
     * This is a callback for the buttons used to select products.  The list of available
     * products is examined to see which match the current button selections.
     */
    public void productSelectionChange() {
        int productCount = 0;
        for ( Iterator<Product> iter = _products.iterator(); iter.hasNext(); ) {
            Product thisProduct = iter.next();
            thisProduct.selected = false;
            //  Make sure buttons representing each of this item's requirements are
            //  pushed.
            boolean match = false;
            //  The telescopes are a little bit complicated, because they can match
            //  in either direction.
            boolean matchTel2 = false;
            for ( Iterator<JToggleButton> iter2 = _telescope1Grid.iterator(); iter2.hasNext() && !match; ) {
                JToggleButton thisButton = iter2.next();
                if ( thisButton.isSelected() ) {
                    if ( thisProduct.baseline.telescope1.contentEquals( thisButton.getText() ) ) {
                        match = true;
                    }
                    else if ( thisProduct.baseline.telescope2.contentEquals( thisButton.getText() ) ) {
                        match = true;
                        matchTel2 = true;
                    }
                }
            }
            if ( match ) {
                match = false;
                for ( Iterator<JToggleButton> iter2 = _telescope2Grid.iterator(); iter2.hasNext() && !match; ) {
                    JToggleButton thisButton = iter2.next();
                    if ( thisButton.isSelected() ) {
                        if ( matchTel2 ) {
                            if ( thisProduct.baseline.telescope1.contentEquals( thisButton.getText() ) ) {
                                match = true;
                            }
                        }
                        else {
                            if ( thisProduct.baseline.telescope2.contentEquals( thisButton.getText() ) ) {
                                match = true;
                            }
                        }
                    }
                }
            }
            if ( match ) {
                //  Now the frequency.  This is a double value, but stored as a string (the button label).  For
                //  out purposes, this is fine.
                match = false;
                for ( Iterator<JToggleButton> iter2 = _frequenciesGrid.iterator(); iter2.hasNext() && !match; ) {
                    JToggleButton thisButton = iter2.next();
                    if ( thisButton.isSelected() && thisProduct.frequency.toString().contentEquals( thisButton.getText() ) )
                        match = true;
                }
                //  If this product is a match, add it to the list of products.
                if ( match ) {
                    ++productCount;
                    thisProduct.selected = true;
                }
            }
        }
        //  Alter the name of the product panel header.
        if ( productCount == 0 )
            _productsLabel.setText( "No Products Selected" );
        else if ( productCount == 1 )
            _productsLabel.setText( "1 Product Selected" );
        else
            _productsLabel.setText( productCount + " Products Selected" );
        if ( productCount > 0 ) {
            _viewButton.setVisible( true );
            _selectedCheck.setVisible( true );
            _allCheck.setVisible( true );
            _applyButton.setVisible( true );
        }
        alterProductTable();
    }
    
    /*
     * Change the contents of the "products" table to reflect currently selected
     * products.
     */
    public void alterProductTable() {
        //  Empty the current table.
        while ( _productTable.getRowCount() > 0 )
            _productTable.removeRow( 0 );
        //  Build the table full of all selected products or all products, depending
        //  on what the user has requested.
        for ( Iterator<Product> iter = _products.iterator(); iter.hasNext(); ) {
            Product thisProduct = iter.next();
            thisProduct.tableRow = null;
            if ( _allCheck.isSelected() || thisProduct.selected ) {
                String checkStr = "";
                boolean selectedValue = false;
                if ( thisProduct.selected )
                    selectedValue = true;
                    //checkStr = "\u2714";
                String baseline = "";
                if ( thisProduct.baseline.telescope1.contentEquals( thisProduct.baseline.telescope2 ) )
                    baseline = thisProduct.baseline.telescope1 + " autocorr";
                else
                    baseline = thisProduct.baseline.telescope1 + "-" + thisProduct.baseline.telescope2;
                thisProduct.tableRow = _productTable.getRowCount();
                _productTable.insertRow( _productTable.getRowCount(),
                        new Object[]{ 
                            //( checkStr ), 
                            ( selectedValue ),
                            ( "" + thisProduct.index ),
                            ( baseline ),
                            ( "" + thisProduct.frequency ),
                            ( "" + thisProduct.phaseCenter ),
                            ( "" + thisProduct.pulsarBin ),
                            ( "" + thisProduct.polProduct ),
                            ( "" + thisProduct.offset ),
                            ( "" + thisProduct.freqChannels )
                        } );
            }
        }
    }
    
    /*
     * Called when the user hits "apply" in the data controls.  Figure out which
     * data products the user wants, set up to receive them, and send appropriate
     * requests for them to guiServer.
     */
    protected void applyButtonAction() {
        //  Count all of the products that have been selected by the user.  These will
        //  have a "true" check in their first table column.
        int productCount = 0;
        for ( Iterator<Product> iter = _products.iterator(); iter.hasNext(); ) {
            Product thisProduct = iter.next();
            if ( thisProduct.tableRow != null ) {
                if ( (Boolean)_productTable.getValueAt( thisProduct.tableRow, 0 ) )
                    ++productCount;
            }
        }
        
        //  Get out of here if there are no requested products.  The user needs to be
        //  warned!
        if ( productCount == 0 ) {
            JOptionPane.showMessageDialog( this, "No products were selected.", "No Products", JOptionPane.WARNING_MESSAGE );
            return;
        }
        
        //  Set up an array to hold the plots of all product data results.
        //  This *should* clean out all existing plots if the garbage collector works.
        _productPlots = new ProductPlotsList();
        
        //  Initiate the start of requests.
        sendPacket( START_PRODUCT_REQUESTS, 0, null );
        
        //  Send requests for each data product.
        for ( Iterator<Product> iter = _products.iterator(); iter.hasNext(); ) {
            Product thisProduct = iter.next();
            if ( thisProduct.tableRow != null ) {
                if ( (Boolean)_productTable.getValueAt( thisProduct.tableRow, 0 ) ) {
                    //  Send the three pieces of data we need to specify this product
                    //  to guiServer (and the monitor server).  These are index, offset,
                    //  and number of frequency channels.
                    if ( _connected ) {
                        try {
                            //  This is the "packet type", so guiServer can know what we are
                            //  doing.
                            _out.writeInt( PRODUCT_REQUEST );
                            //  This is the number of bytes of data in this request - 3 integers worth.
                            _out.writeInt( 12 );
                            //  The index.
                            _out.writeInt( thisProduct.index );
                            //  The offset.
                            _out.writeInt( thisProduct.offset );
                            //  And the number of frequency channels.
                            _out.writeInt( thisProduct.freqChannels );
                        } catch ( java.io.IOException e ) {
                            _connected = false;
                            _connectionLight.alert();
                            _connectionLabel.setText( "write failed" );
                            JOptionPane.showMessageDialog( this, 
                                e.getMessage() + " ", "Write failed.", 
                                JOptionPane.ERROR_MESSAGE );
                        }
                    }
                    else 
                        JOptionPane.showMessageDialog( this, 
                                "No Connection to the monitor server.", "No Connection", 
                                JOptionPane.ERROR_MESSAGE );
                }
            }
        }
        
        //  Wrap up product requests.  This will start data flow (we hope).
        sendPacket( END_PRODUCT_REQUESTS, 0, null );
    }
    
    /*
     * Change the data product controls to reflect the products currently
     * available.
     */
    protected void updateDataProductControls() {
        if ( _products != null ) {
            //  Two grids of buttons represent each telescope.
            StringList telescopes = new StringList();
            StringList frequencies = new StringList();
            if ( _telescope1Grid == null )
                _telescope1Grid = new ArrayDeque<JToggleButton>();
            else {
                for ( Iterator<JToggleButton> iter = _telescope1Grid.iterator(); iter.hasNext(); )
                    _dataPanel.remove( iter.next() );
            }
            _telescope1Grid.clear();
            if ( _telescope2Grid == null )
                _telescope2Grid = new ArrayDeque<JToggleButton>();
            else {
                for ( Iterator<JToggleButton> iter = _telescope2Grid.iterator(); iter.hasNext(); )
                    _dataPanel.remove( iter.next() );
            }
            _telescope2Grid.clear();
            if ( _frequenciesGrid == null )
                _frequenciesGrid = new ArrayDeque<JToggleButton>();
            else {
                for ( Iterator<JToggleButton> iter = _frequenciesGrid.iterator(); iter.hasNext(); )
                    _dataPanel.remove( iter.next() );
            }
            _frequenciesGrid.clear();
            for ( Iterator<Product> iter = _products.iterator(); iter.hasNext(); ) {
                Product newProduct = iter.next();
                if ( telescopes.addUnique( newProduct.baseline.telescope1 ) ) {
                    JToggleButton newButton = new JToggleButton( newProduct.baseline.telescope1 );
                    newButton.addActionListener( new ActionListener() {
                        public void actionPerformed( ActionEvent evt ) {
                            productSelectionChange();
                        }
                    } );
                    _dataPanel.add( newButton );
                    _telescope1Grid.add( newButton );
                    newButton = new JToggleButton( newProduct.baseline.telescope2 );
                    newButton.addActionListener( new ActionListener() {
                        public void actionPerformed( ActionEvent evt ) {
                            productSelectionChange();
                        }
                    } );
                    _dataPanel.add( newButton );
                    _telescope2Grid.add( newButton );
                }
                if ( telescopes.addUnique( newProduct.baseline.telescope2 ) ) {
                    System.out.println( "adding " + newProduct.baseline.telescope2 + " to the reference grid" );
                    JToggleButton newButton = new JToggleButton( newProduct.baseline.telescope2 );
                    newButton.addActionListener( new ActionListener() {
                        public void actionPerformed( ActionEvent evt ) {
                            productSelectionChange();
                        }
                    } );
                    _dataPanel.add( newButton );
                    _telescope1Grid.add( newButton );
                    newButton = new JToggleButton( newProduct.baseline.telescope1 );
                    newButton.addActionListener( new ActionListener() {
                        public void actionPerformed( ActionEvent evt ) {
                            productSelectionChange();
                        }
                    } );
                    _dataPanel.add( newButton );
                    _telescope2Grid.add( newButton );
                }
                if ( frequencies.addUnique( newProduct.frequency.toString() ) ) {
                    JToggleButton newButton = new JToggleButton( newProduct.frequency.toString() );
                    newButton.addActionListener( new ActionListener() {
                        public void actionPerformed( ActionEvent evt ) {
                            productSelectionChange();
                        }
                    } );
                    _dataPanel.add( newButton );
                    _frequenciesGrid.add( newButton );
                }
            }
            _automaticallyResize = true;
            _productsLabel.setText( "No Products Selected" );
        }
    }
    
    /*
     * Change the sizes and locations of controls and labels used to choose DiFX products.
     * These depend on what's available as well as the configuration of the window.
     */
    protected void setControlObjectSizes() {
        //  How we configure things depends on the width of the window.
        int w = this.getWidth();
        int y = 60;
        int x = 115;
        int buttonsOnRow = 0;
        int buttonsPerRow = ( w - 145 ) / 100;
        if ( buttonsPerRow < 1 )
            buttonsPerRow = 1;
        if ( _telescope1Grid != null && _telescope1Grid.size() > 0 ) {
            //  Make the "reference" buttons 100 pixels wide in as many rows as are
            //  required for their numbers.
            for ( Iterator<JToggleButton> iter = _telescope1Grid.iterator(); iter.hasNext(); ) {
                if ( buttonsOnRow == buttonsPerRow ) {
                    buttonsOnRow = 0;
                    x = 115;
                    y += 25;
                }
                iter.next().setBounds( x, y, 100, 25 );
                x += 100;
                buttonsOnRow += 1;
            }
            _telescope1GridLabel.setVisible( true );
        }
        else {
            _telescope1GridLabel.setVisible( false );
        }
        if ( _telescope2Grid != null && _telescope2Grid.size() > 0 ) {
            y += 30;
            buttonsOnRow = 0;
            x = 115;
            _telescope2GridLabel.setVisible( true );
            _telescope2GridLabel.setBounds( 10, y, 100, 25 );
            for ( Iterator<JToggleButton> iter = _telescope2Grid.iterator(); iter.hasNext(); ) {
                if ( buttonsOnRow == buttonsPerRow ) {
                    buttonsOnRow = 0;
                    x = 115;
                    y += 25;
                }
                iter.next().setBounds( x, y, 100, 25 );
                x += 100;
                buttonsOnRow += 1;
            }
        }
        else {
            _telescope2GridLabel.setVisible( false );
        }
        if ( _frequenciesGrid != null && _frequenciesGrid.size() > 0 ) {
            y += 30;
            buttonsOnRow = 0;
            x = 115;
            _frequenciesGridLabel.setVisible( true );
            _frequenciesGridLabel.setBounds( 10, y, 100, 25 );
            for ( Iterator<JToggleButton> iter = _frequenciesGrid.iterator(); iter.hasNext(); ) {
                if ( buttonsOnRow == buttonsPerRow ) {
                    buttonsOnRow = 0;
                    x = 115;
                    y += 25;
                }
                try {
                    iter.next().setBounds( x, y, 100, 25 );
                } catch ( java.util.ConcurrentModificationException e ) {}
                x += 100;
                buttonsOnRow += 1;
            }
        }
        else {
            _frequenciesGridLabel.setVisible( false );
        }
        _productsLabel.setBounds( 0, y + 35, 180, 25 );
        _viewButton.setBounds( 195, y + 35, 125, 25 );
        _selectedCheck.setBounds( 330, y + 35, 100, 25 );
        _allCheck.setBounds( 440, y + 35, 100, 25 );
        _applyButton.setBounds( w - 160, y + 35, 125, 25 );
        y += 35;
        if ( _automaticallyResize ) {
            _tableScrollPane.setBounds( 10, y + 30, w - 45, 100 );
            if ( _tableScrollPane.isVisible() )
                _dataPanel.openHeight( y + 135 );
            else
                _dataPanel.openHeight( y + 35 );
            _automaticallyResize = false;
        }
        else {
            int h = _dataPanel.openHeight();
            _tableScrollPane.setBounds( 10, y + 30, w - 45, h - y - 40 );
        }
    }
    
    /*
     * This is performed when the window is closed.  It tells the guiServer to
     * terminate connections and stop collecting data.
     */
    protected void exitOperation() {
        System.out.println( "window close operation" );
        _connectionThread.closeConnection();
        try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
    }
    
    /*
     * Simplistic class for handling data transfer rate.  This checks every seconds
     * for any new data transfer and reports it.
     */
    protected class DataTransferMonitorThread extends Thread {
        
        public DataTransferMonitorThread() {
            _keepGoing = true;
        }
        
        public void stopIt() {
            _keepGoing = false;
        }
        
        public void run() {
            while ( _keepGoing ) {
                //  Compute the size of the data transfer.
                long newData = 10 * ( _bytesTransfered - _lastBytes );
                _lastBytes = _bytesTransfered;
                if ( 1.25 * (double)newData > _connectionMaxBytes )
                    _connectionMaxBytes = 1.25 * (double)newData;
                _connectionPlot.limits( (double)(_connectionTrackSize - _connectionPlot.w()),
                        (double)(_connectionTrackSize), -.05 * _connectionMaxBytes, _connectionMaxBytes );
                _connectionTrack.add( (double)(_connectionTrackSize), (double)(newData) );
                _connectionTrackSize += 1;
                _connectionPlotWindow.updateUI();
                try { Thread.sleep( 100 ); } catch ( Exception e ) {}
            }
        }
            
        protected boolean _keepGoing;
        protected long _lastBytes;
               
    }
    
    /*
     * Send a request for data products from the monitor server.  What this request
     * contains depends on user settings.
     */
    void sendProductRequest() {
        //  This stuff is swiped from monserver.cpp...not certain exactly what
        //  each step is doing.
        
        //  Send the total number of products we will be requesting.
        try {
            _out.writeInt( 1 );
            //  Send the offset.
            _out.writeInt( 0 ); 
            //  Send the number of channels for this frequency.
            int freqIdx = 0;
            //  Send the "product number".  Not sure if this is used.
            _out.writeInt( 0 );
        } catch ( java.io.IOException e ) {
            _connected = false;
            _connectionLight.alert();
            _connectionLabel.setText( "write failed" );
        }
    }

    protected boolean _automaticallyResize;
    protected SystemSettings _settings;
    protected LiveMonitorWindow _this;
    protected boolean _allObjectsBuilt;
    protected JMenuBar _menuBar;
    protected NodeBrowserScrollPane _scrollPane;
    protected IndexedPanel _plotPanel;
    protected IndexedPanel _connectionPanel;
    protected IndexedPanel _dataPanel;
    protected SaneTextField _monitorHost;
    protected JLabel _monitorHostLabel;
    protected JTextField _monitorPort;
    protected JLabel _monitorPortLabel;
    protected ActivityMonitorLight _connectionLight;
    protected JLabel _connectionLabel;
    protected PlotWindow _connectionPlotWindow;
    protected Plot2DObject _connectionPlot;
    protected Track2D _connectionTrack;
    protected int _connectionTrackSize;
    protected long _bytesTransfered;
    protected double _connectionMaxBytes;
    protected DataTransferMonitorThread _dataTransferMonitorThread;
    protected Socket _socket;
    protected boolean _connected;
    protected ExtendedDataInputStream _in;
    protected DataOutputStream _out;
    protected ConnectionThread _connectionThread;
    protected JComboBox _inputFileComboBox;
    protected JTextField _inputFileOutput;
    protected boolean _standAlone;
    protected String _inputFile;
    protected JLabel _inputFileLabel;
    protected IndexedPanel _messagePanel;
    protected MessageDisplayPanel _messages;
    protected int _usingPort;
     
    //  These items are used to store "product" information collected from the monitor server.
    //  They describe the types of data we can download from the server.
    protected boolean _readingProducts;
    protected int _nScans;
    protected int _scan;
    protected int _nBaselines;
    protected class Baseline {
        public int index;
        public String telescope1;
        public String telescope2;
    }
    protected Baseline _currentBaseline;
    protected int _nFrequencies;
    protected double _currentFrequency;
    protected int _nPhaseCenters;
    protected int _currentPhaseCenter;
    protected int _nPulsarBins;
    protected int _currentPulsarBin;
    protected int _nPolProducts;
    protected int _currentPolProduct;
    protected class Product {
        public int index;
        public Baseline baseline;
        public Double frequency;
        public int phaseCenter;
        public int pulsarBin;
        public int polProduct;
        public int offset;
        public int freqChannels;
        public boolean selected;
        public Integer tableRow;
    };
    protected ArrayDeque<Product> _products;
    protected ArrayDeque<JToggleButton> _telescope1Grid;
    protected JLabel _telescope1GridLabel;
    protected ArrayDeque<JToggleButton> _telescope2Grid;
    protected JLabel _telescope2GridLabel;
    protected ArrayDeque<JToggleButton> _frequenciesGrid;
    protected JLabel _frequenciesGridLabel;
    protected JLabel _productsLabel;
    protected JToggleButton _viewButton;
    protected JButton _applyButton;
    protected JCheckBox _selectedCheck;
    protected JCheckBox _allCheck;
    protected DefaultTableModel _productTable;
    protected JScrollPane _tableScrollPane;
    protected PlotWindow _plotWindow;
    protected class IncPlot {
        public IncPlot( int newProduct, int newChannels, int newTime ) {
            plot = new Plot2DObject();
            timeStamp = newTime;
            iProduct = newProduct;
            nChannels = newChannels;
        }
        public void curve( double x[], double y[] ) {
            curve = new Curve2D( x, y );
            plot.addCurve( curve );
        }
        public void track( double x[], double y[], DrawObject obj ) {
            track = new Track2D();
            if ( obj != null )
                track.draw( false );
            for ( int i = 0; i < x.length; ++i ) {
                track.add( x[i], y[i] );
                if ( obj != null )
                    track.drawObject( obj );
            }
            plot.addTrack( track );
        }
        public Plot2DObject plot;
        public Curve2D curve;
        public Track2D track;
        public int timeStamp;
        public int nChannels;
        public int iProduct;
        public double min;
        public double max;
        public double delay;
        public double snr;
        public int maxChannel;
    }
    protected class ProductPlots {
        public ProductPlots( int newIndex ) {
            lagPlots = new ArrayDeque<IncPlot>();
            ampPlots = new ArrayDeque<IncPlot>();
            phasePlots = new ArrayDeque<IncPlot>();
            index = newIndex;
        }
        public ArrayDeque<IncPlot> lagPlots;
        public ArrayDeque<IncPlot> ampPlots;
        public ArrayDeque<IncPlot> phasePlots;
        public int index;
    }
    protected class ProductPlotsList extends ArrayDeque<ProductPlots> {
        //  This function returns the ProductPlots instance corresponding to a given
        //  product index number if it exists.  If it does not exist, it creates one
        //  and returns that.
        public ProductPlots productPlot( int index ) {
            ProductPlots foundPP = null;
            for ( Iterator<ProductPlots> iter = this.iterator(); iter.hasNext() && foundPP == null; ) {
                ProductPlots thisPP = iter.next();
                if ( thisPP.index == index )
                    foundPP = thisPP;
            }
            //  Create a new one if necessary.
            if ( foundPP == null ) {
                foundPP = new ProductPlots( index );
                this.add( foundPP );
            }
            return foundPP;
        }
    }
    protected ProductPlotsList _productPlots;
    JButton _showButton;
    JPopupMenu _showMenu;
    JCheckBoxMenuItem _showLatest;
    JCheckBoxMenuItem _showAll;
    JCheckBoxMenuItem _showSummary;
    JCheckBoxMenuItem _showHistory;
    JCheckBoxMenuItem _showAmp;
    JCheckBoxMenuItem _showPhase;
    JCheckBoxMenuItem _showLag;
    JCheckBoxMenuItem _showDelay;
    
    JFileChooser _fileChooser;
    
    public class ExtendedDataInputStream extends DataInputStream {
        public ExtendedDataInputStream( InputStream in ) {
            super( in );
        }
        public double readStringDouble() {
            byte [] data = new byte[14];
            try {
                readFully( data );
                return Double.parseDouble( new String( data ) );
            } 
            catch ( java.io.IOException e ) {
                return 0.0;
            }
        }
    }
    
}
