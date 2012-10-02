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
import javax.swing.JLabel;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.Component;
import java.awt.Color;

import java.awt.event.ComponentEvent;

import java.util.Date;
import java.util.Iterator;

import java.text.SimpleDateFormat;

import edu.nrao.difx.difxutilities.SMARTMonitor;
import java.awt.Font;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.net.Socket;
import javax.swing.*;
import mil.navy.usno.plotlib.Plot2DObject;
import mil.navy.usno.plotlib.PlotWindow;
import mil.navy.usno.plotlib.Track2D;
import mil.navy.usno.widgetlib.*;

public class LiveMonitorWindow extends JFrame {
    
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
        this.setTitle( "Job Monitor" );
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
        _monitorPort = new NumberBox();
        _monitorPort.value( _settings.difxMonitorPort() );
        _monitorPort.setHorizontalAlignment( NumberBox.LEFT );
        _monitorPort.minimum( 0 );
        _monitorPort.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                makeConnection();
            }
        } );
        _connectionPanel.add( _monitorPort );
        _monitorPortLabel = new JLabel( "Monitor Port:" );
        _monitorPortLabel.setHorizontalAlignment( JLabel.RIGHT );
        _connectionPanel.add( _monitorPortLabel );
        _connectionLight = new ActivityMonitorLight();
        _connectionLight.alertTime( 0 );
        _connectionLight.warningTime( 0 );
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
        
        //  Data Controls
        _dataPanel = new IndexedPanel( "Data Controls" );
        _dataPanel.openHeight( 100 );
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
        
        //  Plots
        _plotPanel = new IndexedPanel( "Plots, etc." );
        _plotPanel.openHeight( 100 );
        _plotPanel.closedHeight( 20 );
        _plotPanel.alwaysOpen( true );
        _plotPanel.noArrow( true );
        _scrollPane.addNode( _plotPanel );
        
        _allObjectsBuilt = true;
        newSize();
        
        //  Start some threads.  This one monitors the traffic on our data connection.
        _dataTransferMonitorThread = new DataTransferMonitorThread();
        _dataTransferMonitorThread.start();
        
        //  This one actually forms and operates the data connection.
        _connectionThread = new ConnectionThread();
        _connectionThread.start();
        
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
            if ( h - _connectionPanel.getHeight() - _dataPanel.getHeight() < 0 )
                _plotPanel.openHeight( 0 );
            else
                _plotPanel.openHeight( h - _connectionPanel.getHeight() - _dataPanel.getHeight() );
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
        }
    }
    
    /*
     * Change the connection parameters.  This will cause the connection to close
     * and reform using the new parameters.
     */
    void changeConnection() {
        _connected = false;
    }

    protected final int DIFXMON_NOERROR         = 0;
    protected final int DIFXMON_TOOMANYCLIENTS  = 1;
    protected final int DIFXMON_BADPRODUCTS     = 2;
    protected final int DIFXMON_MALLOCERROR     = 3;
    
    /*
     * Try making a connection to the DiFX monitor server using the current settings
     * for host and port.
     */
    void makeConnection() {
        System.out.println( "try connecton at " + _monitorHost.getText() + "  " + _monitorPort.intValue() );
        _connectionLight.warning();
        try {
            _socket = new Socket( _settings.difxControlAddress(), _settings.difxControlPort() );
            _socket.setSoTimeout( _settings.timeout() );
            _in = new DataInputStream( _socket.getInputStream() );
            _out = new DataOutputStream( _socket.getOutputStream() );
            //  Read the connection status message from the server.  This should match
            //  the "no error" message.
            int status = _in.readInt();
            if ( status == DIFXMON_NOERROR ) {
                _connectionLight.on();
                _connected = true;
            }
            else {
                _connected = false;
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
        
        public void run() {
            while ( _keepGoing ) {
                while ( _connected ) {
                }
            }
        }
        
        protected boolean _keepGoing;
            
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
    protected NumberBox _monitorPort;
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
    protected DataInputStream _in;
    protected DataOutputStream _out;
    protected ConnectionThread _connectionThread;
    protected JComboBox _inputFileComboBox;
    protected JTextField _inputFileOutput;
    protected boolean _standAlone;
    protected String _inputFile;
    protected JLabel _inputFileLabel;
     
}
