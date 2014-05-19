/*
 * All settings for the DiFX GUI are contained in this class.  The class provides
 * functions to allow other classes to obtain and set these data, as well as a
 * window that allows the user to view and set them via the GUI.  Facilities for
 * loading settings from files and saving them to files are provided as well.
 * 
 * The original DiFX GUI did what to me appears to be a rather strange thing when
 * it came to reading settings from a file - data were dumped into a DiFXObject and
 * then treated by the same structure that handles messages in the DataModel class.
 * The only reason I can imagine to do this is to allow settings to be issued via
 * network messaging, a procedure for which no facilities exist.  My belief that
 * this was rather strange might mean I didn't understand why it was valuable.
 * In any case it has been swept away.  
 * 
 * This class generates events when items are changed.  For instance, any change to
 * database parameters produces a "databaseChangeEvent".  Other classes can set
 * themselves up to listen for these using the "databaseChangeListener()" function.
 */
package edu.nrao.difx.difxview;

import edu.nrao.difx.xmllib.difxmessage.*;
import java.io.File;
import javax.swing.event.EventListenerList;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.Color;
import java.awt.Point;
import java.awt.MouseInfo;
import java.util.Calendar;
import java.awt.Insets;

import java.io.BufferedWriter;
import java.io.FileWriter;

import java.net.URL;
import java.io.InputStream;
import java.io.IOException;

import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;
import java.util.ArrayList;
import java.awt.Font;
import java.awt.Dimension;

import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.UIManager;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPasswordField;
import javax.swing.JTextField;
import javax.swing.JComboBox;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JPanel;
import javax.swing.JToolTip;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.DefaultCellEditor;
import javax.swing.table.TableColumn;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import mil.navy.usno.widgetlib.NodeBrowserScrollPane;
import mil.navy.usno.widgetlib.IndexedPanel;
import mil.navy.usno.widgetlib.NumberBox;
import mil.navy.usno.plotlib.PlotWindow;
import mil.navy.usno.plotlib.Plot2DObject;
import mil.navy.usno.plotlib.Track2D;
import mil.navy.usno.widgetlib.PingTest;
import mil.navy.usno.widgetlib.MessageScrollPane;
import mil.navy.usno.widgetlib.MessageNode;
import mil.navy.usno.widgetlib.SaneTextField;
import mil.navy.usno.widgetlib.SimpleTextEditor;
import mil.navy.usno.widgetlib.ActivityMonitorLight;
import mil.navy.usno.widgetlib.MessageDisplayPanel;
import mil.navy.usno.widgetlib.ComplexToolTip;
import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.FormattedTextField;
import mil.navy.usno.widgetlib.ZCheckBox;
import mil.navy.usno.widgetlib.ZButton;

import javax.swing.JFrame;

import edu.nrao.difx.difxdatabase.QueueDBConnection;
import edu.nrao.difx.difxutilities.GuiServerConnection;
import edu.nrao.difx.difxutilities.TabCompletedTextField;
import edu.nrao.difx.difxcontroller.DiFXMessageProcessor;

import java.sql.ResultSet;

public class SystemSettings extends JFrame {
    
    public SystemSettings( DiFXUI newUI, String settingsFile ) {
        
        _settings = this;
        
        //  Set the "top level" GUI window.  This will allow the settings menu to change
        //  the title (and might have other uses).
        difxUI( newUI );
        
        //  The "look and feel" isn't a setting in the sense that these others are...it
        //  must be set prior to building menus, so ONLY the default value will be
        //  used.  However it is put here due to a lack of anywhere better to put it.
        //  It is NOT saved as part of the settings file.  All JFrames should call
        //  the "setLookAndFeel()" function before they create any GUI components.
        //
        //  If left as "null" the look and feel will be whatever the local machine
        //  uses.  Although unpredictable, it does give us native file choosers.
        //  Unfortunately it is not wise to mix the look and feel within an application
        //  (thus we can't use the local one only for file choosers) - at least on the 
        //  Mac it causes confusion in the window manager and odd error messages.
        //_lookAndFeel = null;
        //  The "cross platform" look and feel is consistent across all platforms,
        //  which is why I tend to like it.  It gives us ugly and annoying file
        //  choosers though.
        _lookAndFeel = UIManager.getCrossPlatformLookAndFeelClassName();
        
        //  Some organizational structure here - some items are stored in class
        //  structures that are based on where they apply.  We need to create these
        //  structures here.
        _queueBrowserSettings = new QueueBrowserSettings();
        _windowConfiguration = new WindowConfiguration();
        _defaultNames = new DefaultNames();
        _jobColumnSpecs = new JobColumnSpecs();
        _hardwareColumnSpecs = new HardwareColumnSpecs();
        _jobLocationDefaults = new JobLocationDefaults();
        
        //  Create all of the components of the user interface (long, messy function).
        createGUIComponents();
        
        //  Set the default settings for all values (these are hard-coded).
        setDefaults();

        //  Try loading settings from the file specified in the call.  If this was
        //  null, use the "default" settings filename.
        if ( settingsFile == null )
            settingsFile( defaultSettingsFile() );
        else
            settingsFile( settingsFile );
            
        //  This stuff is used to trap resize events.
        this.addComponentListener(new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                        _this.newSize();
                }
                public void componentMoved( ComponentEvent e ) {
                        _this.newSize();
                }
        } );

        //  These threads are used to update from the database (updates can hang if
        //  the database can't be located).
        _databaseThread = new DatabaseThread( 1000 );
        _databaseThread.start();
        
        _updateDatabaseLoop = new UpdateDatabaseLoop();
        _updateDatabaseLoop.start();
        
        //  Start the connection thread that will try to maintain a TCP connection
        //  with the guiServer.
        TCPConnectionThread connectionThread = new TCPConnectionThread();
        connectionThread.start();

    }
    
    /*
     * This function creates all of the components of the user interface for the
     * settings window (there are many components).  It does not initialize values
     * for user-changeable fields (that is done in setDefaults()), nor does it
     * determine the size and position of components unless they are fixed (usually
     * labels and things).  Sizes are set on the fly in the "newSize()" function.
     * The settings are contained in a browsable list of panes, matching the design
     * of the overall DiFX GUI.
     */
    public void createGUIComponents() {
        
        //  Use the universal "look and feel" setting for this window.  This MUST
        //  be used (at least on the Mac) or Java barfs errors and doesn't draw
        //  things correctly.
        this.setLookAndFeel();

        //  One file chooser for all settings operations.  This means it will pop
        //  up with whatever directory the user previously gave it unless otherwise
        //  specified.
        _fileChooser = new JFileChooser();
        
        //  Lots of components need this created first!
         _guiDocPath = new JFormattedTextField();
         
        //  Build a user interface for all settings items and load with default
        //  values.
        _this = this;
        this.setLayout( null );
        this.setSize( 800, 775 );
        this.setTitle( "DiFX GUI Settings" );
        _menuBar = new JMenuBar();
        JMenu helpMenu = new JMenu( "  Help  " );
        _menuBar.add( helpMenu );
        JMenuItem settingsHelpItem = new JMenuItem( "Settings Help" );
        settingsHelpItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                launchGUIHelp( "settings.html" );
            }
        } );
        helpMenu.add( settingsHelpItem );
        JMenuItem helpIndexItem = new JMenuItem( "GUI Documentation" );
        helpIndexItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                launchGUIHelp( "intro.html" );
            }
        } );
        helpMenu.add( helpIndexItem );
        this.add( _menuBar );
        _scrollPane = new NodeBrowserScrollPane();
        _scrollPane.respondToResizeEvents( true );
        _scrollPane.addTimeoutEventListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _this.newSize();
            }
        } );
        this.add( _scrollPane );
        
        IndexedPanel settingsFilePanel = new IndexedPanel( "Settings File" );
        settingsFilePanel.openHeight( 130 );
        settingsFilePanel.closedHeight( 20 );
        _scrollPane.addNode( settingsFilePanel );
        _settingsFile = new FormattedTextField();
        _settingsFile.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _settingsFile.setToolTipText( "XML file containing system settings."
                + "  Red text indicates last attept to read this file failed." );
        _settingsFile.setEditable( false );
        settingsFilePanel.add( _settingsFile );
        JLabel settingsFileLabel = new JLabel( "Current:" );
        settingsFileLabel.setBounds( 10, 25, 100, 25 );
        settingsFileLabel.setHorizontalAlignment( JLabel.RIGHT );
        settingsFilePanel.add( settingsFileLabel );
        ZButton loadFromFileButton = new ZButton( "Open..." );
        loadFromFileButton.setBounds( 115, 55, 100, 25 );
        loadFromFileButton.setToolTipText( "Open a settings file" );
        loadFromFileButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                openSettingsFile();
            }
        } );
        settingsFilePanel.add( loadFromFileButton );
        ZButton saveButton = new ZButton( "Save" );
        saveButton.setBounds( 220, 55, 100, 25 );
        saveButton.setToolTipText( "Save current settings to the given file name." );
        saveButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                saveSettings();
            }
        } );
        settingsFilePanel.add( saveButton );
        ZButton saveAsButton = new ZButton( "Save As..." );
        saveAsButton.setBounds( 325, 55, 100, 25 );
        saveAsButton.setToolTipText( "Save the current settings to a new file name." );
        saveAsButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                saveSettingsAs();
            }
        } );
        settingsFilePanel.add( saveAsButton );
        JButton defaultsButton = new JButton( "Defaults" );
        defaultsButton.setBounds( 430, 55, 100, 25 );
        defaultsButton.setToolTipText( "Reset all settings to internal default values." );
        defaultsButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                setDefaults();
            }
        } );
        _title = new FormattedTextField();
        _title.setToolTipText( "Title of the main window of the GUI." );
        _title.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _difxUI.setTitle( _title.getText() );
            }
        } );
        settingsFilePanel.add( _title );
        JLabel titleLabel = new JLabel( "GUI Title:" );
        titleLabel.setBounds( 10, 85, 100, 25 );
        titleLabel.setHorizontalAlignment( JLabel.RIGHT );
        settingsFilePanel.add( titleLabel );
        settingsFilePanel.add( defaultsButton );
        
        IndexedPanel difxControlPanel = new IndexedPanel( "DiFX Control Connection" );
        difxControlPanel.openHeight( 270 );
        difxControlPanel.closedHeight( 20 );
        _scrollPane.addNode( difxControlPanel );
        _difxUDPCheck = new ZCheckBox( "Multicast" );
        _difxUDPCheck.setBounds( 160, 25, 100, 25 );
        _difxUDPCheck.setToolTipText( "Use UDP multicast messages to control DiFX directly via mk5daemon." );
        _difxUDPCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _difxUDPCheck.setSelected( true );
                _difxTCPCheck.setSelected( false );
                changeDifxControlConnection();
            }
        } );
        difxControlPanel.add( _difxUDPCheck );        
        _difxTCPCheck = new ZCheckBox( "guiServer" );
        _difxTCPCheck.setBounds( 260, 25, 100, 25 );
        _difxTCPCheck.setToolTipText( "Use a TCP connection with guiServer to route control commands to DiFX." );
        _difxTCPCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _difxUDPCheck.setSelected( false );
                _difxTCPCheck.setSelected( true );
                changeDifxControlConnection();
            }
        } );
        difxControlPanel.add( _difxTCPCheck );
        _guiServerConnectionLight = new ActivityMonitorLight();
        _guiServerConnectionLight.setBounds( 360, 32, 12, 12 );
        _guiServerConnectionLight.alertTime( 0 );
        _guiServerConnectionLight.warningTime( 0 );
        difxControlPanel.add( _guiServerConnectionLight );
        JLabel guiServerConnectionLabel = new JLabel( "guiServer Connection" );
        guiServerConnectionLabel.setBounds( 380, 25, 200, 25 );
        difxControlPanel.add( guiServerConnectionLabel );
        _difxControlAddress = new FormattedTextField();
        _difxControlAddress.setToolTipText( "Name of the host where <<italic>>guiServer<</italic>> is running.\n"
                + "Changing this name will trigger a new connection attempt." );
        _difxControlAddress.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _difxControlAddress.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                changeTCPConnectionSettings();
            }
        } );
        difxControlPanel.add( _difxControlAddress );
        JLabel ipAddressLabel = new JLabel( "DiFX Host:" );
        ipAddressLabel.setBounds( 10, 55, 150, 25 );
        ipAddressLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( ipAddressLabel );
        _difxControlPort = new NumberBox();
        _difxControlPort.setToolTipText( "Port number used to make a TCP connection to <<italic>>guiServer<</italic>>.\n"
                + "Changing this value will trigger a new connection attempt." );
        _difxControlPort.setHorizontalAlignment( NumberBox.LEFT );
        _difxControlPort.minimum( 0 );
        _difxControlPort.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                changeTCPConnectionSettings();
            }
        } );
        difxControlPanel.add( _difxControlPort );
        JLabel portLabel = new JLabel( "Control Port:" );
        portLabel.setBounds( 10, 85, 150, 25 );
        portLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( portLabel );
        _difxTransferPort = new NumberBox();
        _difxTransferPort.setToolTipText( "\"Starting\" port used for data transfer operations between\n"
                + "the GUI and <<italic>>guiServer<</italic>>.  Port numbers used will start at\n"
                + "this number and increment \"Max Open Ports\" times, then recycle." );
        _difxTransferPort.setHorizontalAlignment( NumberBox.LEFT );
        _difxTransferPort.minimum( 0 );
        difxControlPanel.add( _difxTransferPort );
        JLabel transferPortLabel = new JLabel( "Transfer Port:" );
        transferPortLabel.setBounds( 210, 85, 150, 25 );
        transferPortLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( transferPortLabel );
        _maxTransferPorts = new NumberBox();
        _maxTransferPorts.minimum( 1 );
        _maxTransferPorts.setHorizontalAlignment( NumberBox.LEFT );
        _maxTransferPorts.toolTip( "Maximum number of ports that may be opened simultaneously\n"
                + "for transfer operations.  Transfer port numbers start at the \n"
                + "\"Transfer Port\" value and increment this many times.\n"
                + "<<bold>><<italic>>Don't change this value while transfers are in progress!!<</italic>><</bold>>\n\n"
                + "See <<link=/settings_content.html#TRANSFER_PORTS>><<blue>><<underline>>Transfer Port and Max Open Ports<</underline>><</color>><</link>> for details.", _guiDocPath );
        _maxTransferPorts.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                maxTransferPorts();
            }
        } );
        difxControlPanel.add( _maxTransferPorts );
        JLabel maxTransferPortsLabel = new JLabel( "Max Open Ports:" );
        maxTransferPortsLabel.setBounds( 420, 85, 150, 25 );
        maxTransferPortsLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( maxTransferPortsLabel );
        _difxMonitorHost = new SaneTextField();
        _difxMonitorHost.setToolTipText( "The host running the monitor\n(better info in the future!)" );
        difxControlPanel.add( _difxMonitorHost );
        JLabel difxMonitorHostLabel = new JLabel( "Monitor Host:" );
        difxMonitorHostLabel.setBounds( 420, 55, 150, 25 );
        difxMonitorHostLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( difxMonitorHostLabel );
        _difxMonitorPort = new NumberBox();
        _difxMonitorPort.setHorizontalAlignment( NumberBox.LEFT );
        _difxMonitorPort.minimum( 0 );
        difxControlPanel.add( _difxMonitorPort );
        JLabel monitorPortLabel = new JLabel( "Monitor Port:" );
        monitorPortLabel.setBounds( 620, 85, 150, 25 );
        monitorPortLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( monitorPortLabel );
        _difxControlUser = new SaneTextField();
        _difxControlUser.setText( "N/A" );
        _difxControlUser.setToolTipText( "User name under which guiServer is running on the DiFX host." );
        _difxControlUser.setEditable( false );
        difxControlPanel.add( _difxControlUser );
        JLabel userAddressLabel = new JLabel( "Username:" );
        userAddressLabel.setBounds( 10, 145, 150, 25 );
        userAddressLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( userAddressLabel );
        _guiServerVersion = new SaneTextField();
        _guiServerVersion.setText( "N/A" );
        _guiServerVersion.setToolTipText( "Compiled version of guiServer - this does not (necessarily) match the DiFX version." );
        _guiServerVersion.setEditable( false );
        _guiServerVersion.setBackground( this.getBackground() );
        _guiServerVersion.setBounds( 165, 115, 100, 25 );
        difxControlPanel.add( _guiServerVersion );
        JLabel guiServerVersionLabel = new JLabel( "guiServer Version:" );
        guiServerVersionLabel.setBounds( 10, 115, 150, 25 );
        guiServerVersionLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( guiServerVersionLabel );
        _guiServerDifxVersion = new SaneTextField();
        _guiServerDifxVersion.setText( "N/A" );
        _guiServerDifxVersion.setEditable( false );
        _guiServerDifxVersion.setToolTipText( "Version of DiFX used to build guiServer.\n"
                + "This does not need to match the version of DiFX you are using\n"
                + "for processing." );
        _guiServerDifxVersion.setBackground( this.getBackground() );
        _guiServerDifxVersion.setBounds( 365, 115, 100, 25 );
        difxControlPanel.add( _guiServerDifxVersion );
        JButton viewEnvironmentVars = new JButton( "Host Environment Vars" );
        viewEnvironmentVars.setBounds( 480, 115, 175, 25 );
        viewEnvironmentVars.setToolTipText( "Show the environment variables on the DiFX host (as seen by guiServer)." );
        viewEnvironmentVars.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _environmentVariablesDisplay == null )
                    _environmentVariablesDisplay = new EnvironmentVariablesDisplay( MouseInfo.getPointerInfo().getLocation().x, 
                        MouseInfo.getPointerInfo().getLocation().y );
                _environmentVariablesDisplay.setVisible( true );
            }
        } );
        difxControlPanel.add( viewEnvironmentVars );
        _channelAllData = new ZCheckBox( "Channel All Data" );
        _channelAllData.setToolTipText( "Channel all data exchange between the GUI and <<italic>>guiServer<</italic>>\n"
                + "using the primary TCP communications port (i.e. with only one TCP\n"
                + "connection).  This is useful if you are remotely operating through\n"
                + "an SSH tunnel." );
        _channelAllData.setBounds( 550, 25, 175, 25 );
        _channelAllData.setEnabled( false );
        _channelAllData.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _channelAllData.isSelected() )
                    guiServerConnection().sendPacket( guiServerConnection().CHANNEL_ALL_DATA_ON, 0, null );
                else
                    guiServerConnection().sendPacket( guiServerConnection().CHANNEL_ALL_DATA_OFF, 0, null );
            }
        });
        difxControlPanel.add( _channelAllData );
        JLabel guiServerDifxVersionLabel = new JLabel( "built w/DiFX:" );
        guiServerDifxVersionLabel.setBounds( 210, 115, 150, 25 );
        guiServerDifxVersionLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( guiServerDifxVersionLabel );
        _difxVersion = new JComboBox();
        _difxVersion.setToolTipText( "Run all DiFX applications (vex2difx, mpifxcorr, etc.) using this DiFx version." );
        _difxVersion.setEditable( true );
        //  This little bit causes a typed-in item to be treated as a new version.
        _difxVersion.getEditor().addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  If not already in the list of difx versions, add this name.
                boolean found = false;
                for ( int i = 0; i < _difxVersion.getItemCount() && !found; ++i ) {
                    if ( _difxVersion.getItemAt( i ).equals( _difxVersion.getEditor().getItem() ) ) {
                        found = true;
                        _difxVersion.setSelectedIndex( i );
                    }
                }
                if ( !found ) {
                    _difxVersion.addItem( _difxVersion.getEditor().getItem() );
                    _difxVersion.setSelectedIndex( _difxVersion.getItemCount() - 1 );
                }
                //  Set the DiFX setup path to match this version.
                if ( _useDefaultStartScript.isSelected() )
                    _difxStartScript.setText( "rungeneric." + (String)_difxVersion.getSelectedItem() );
                guiServerConnection().sendPacket( guiServerConnection().DIFX_SETUP_PATH, 
                        _difxStartScript.getText().length(), _difxStartScript.getText().getBytes() );
                guiServerConnection().sendPacket( guiServerConnection().DIFX_RUN_LABEL,
                        ((String)_difxVersion.getSelectedItem()).length(), ((String)_difxVersion.getSelectedItem()).getBytes() );
            }
        });
        _difxVersion.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( !_difxVersionClearOperation ) {
                    synchronized ( _difxVersion ) {
                        //  Set the DiFX setup path to match this version.
                        if ( _useDefaultStartScript.isSelected() )
                            _difxStartScript.setText( "rungeneric." + (String)_difxVersion.getSelectedItem() );
                        guiServerConnection().sendPacket( guiServerConnection().DIFX_SETUP_PATH, 
                                _difxStartScript.getText().length(), _difxStartScript.getText().getBytes() );
                        guiServerConnection().sendPacket( guiServerConnection().DIFX_RUN_LABEL,
                                ((String)_difxVersion.getSelectedItem()).length(), ((String)_difxVersion.getSelectedItem()).getBytes() );
                    }
                }
            }
        });
        difxControlPanel.add( _difxVersion );
        JLabel versionAddressLabel = new JLabel( "Run w/DiFX Version:" );
        versionAddressLabel.setBounds( 10, 205, 150, 25 );
        versionAddressLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( versionAddressLabel );
        _difxBase = new SaneTextField();
        _difxBase.setEditable( false ); // BLAT the user should be able to change this...but a change needs to trigger a VersionRequest (see guiServer code)
        _difxBase.setToolTipText( "Path to DiFX directory tree on the DiFX host (version-specific directories should be below this)." );
        difxControlPanel.add( _difxBase );
        JLabel difxBaseLabel = new JLabel( "DiFX Base:" );
        difxBaseLabel.setBounds( 10, 175, 150, 25 );
        difxBaseLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxControlPanel.add( difxBaseLabel );
        _useDefaultStartScript = new ZCheckBox( "Use Default Start Script" );
        _useDefaultStartScript.setToolTipText( "Generate a start script based on the version of DiFX used.\n"
                + "The existing start script will be over-written when the DiFX version is changed." );
        _useDefaultStartScript.setBounds( 480, 205, 300, 25 );
        difxControlPanel.add( _useDefaultStartScript );
        _difxStartScript = new SaneTextField();
        _difxStartScript.setToolTipText( "Script used to execute all DiFX commands (may be blank)." );
        _difxStartScript.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  Send the new path to the guiServer
                guiServerConnection().sendPacket( guiServerConnection().DIFX_SETUP_PATH, 
                        _difxStartScript.getText().length(), _difxStartScript.getText().getBytes() );
                //  If this has been changed by hand, un-set the "use default" checkbox.
                _useDefaultStartScript.setSelected( false );
            }
        });
        difxControlPanel.add( _difxStartScript );
        JLabel difxSetupPathLabel = new JLabel( "DiFX Execute Script:" );
        difxSetupPathLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxSetupPathLabel.setBounds( 10, 235, 150, 25 );
        difxControlPanel.add( difxSetupPathLabel );
        
        IndexedPanel networkPanel = new IndexedPanel( "DiFX Multicast Messages" );
        networkPanel.openHeight( 270 );
        networkPanel.closedHeight( 20 );
        _scrollPane.addNode( networkPanel );
        _useTCPRelayCheck = new ZCheckBox( "Relay Using guiServer Connection" );
        _useTCPRelayCheck.setBounds( 160, 25, 300, 25 );
        _useTCPRelayCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                changeDifxControlConnection();
            }
        } );
        networkPanel.add( _useTCPRelayCheck );
        _useTCPRelayCheck.setToolTipText( "Instruct guiServer to collect all multicasts and relay them using its TCP connection." );
        _ipAddress = new JFormattedTextField();
        _ipAddress.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _ipAddress.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                multicastSettingsChange();
            }
        } );
        networkPanel.add( _ipAddress );
        JLabel difxControlAddressLabel = new JLabel( "Group Address:" );
        difxControlAddressLabel.setBounds( 10, 55, 150, 25 );
        difxControlAddressLabel.setHorizontalAlignment( JLabel.RIGHT );
        networkPanel.add( difxControlAddressLabel );
        _port = new NumberBox();
        _port.setHorizontalAlignment( NumberBox.LEFT );
        _port.minimum( 0 );
        _port.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                multicastSettingsChange();
            }
        } );
        networkPanel.add( _port );
        JLabel controlPortLabel = new JLabel( "Port:" );
        controlPortLabel.setBounds( 10, 85, 150, 25 );
        controlPortLabel.setHorizontalAlignment( JLabel.RIGHT );
        networkPanel.add( controlPortLabel );
        _bufferSize = new NumberBox();
        _bufferSize.setHorizontalAlignment( NumberBox.LEFT );
        _bufferSize.minimum( 0 );
        _bufferSize.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateBroadcastChangeEvent();
            }
        } );
        networkPanel.add( _bufferSize );
        JLabel bufferSizeLabel = new JLabel( "Buffer Size:" );
        bufferSizeLabel.setBounds( 10, 115, 150, 25 );
        bufferSizeLabel.setHorizontalAlignment( JLabel.RIGHT );
        networkPanel.add( bufferSizeLabel );
        _timeout = new NumberBox();
        _timeout.setHorizontalAlignment( NumberBox.LEFT );
        _timeout.minimum( 0 );
        _timeout.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateBroadcastChangeEvent();
            }
        } );
        networkPanel.add( _timeout );
        JLabel timeoutLabel = new JLabel( "Timeout (ms):" );
        timeoutLabel.setBounds( 10, 145, 150, 25 );
        timeoutLabel.setHorizontalAlignment( JLabel.RIGHT );
        networkPanel.add( timeoutLabel );
        _plotWindow = new PlotWindow();
        _plotWindow.backgroundColor( this.getBackground() );
        networkPanel.add( _plotWindow );
        _broadcastPlot = new Plot2DObject();
        _broadcastPlot.title( "Packet Traffic (bytes/buffer size)", Plot2DObject.LEFT_JUSTIFY );
        _broadcastPlot.titlePosition( 0.0, 4.0 );
        _broadcastTrack = new Track2D();
        _broadcastPlot.addTrack( _broadcastTrack );
        _broadcastTrack.color( Color.GREEN );
        _broadcastTrack.sizeLimit( 1000 );
        //_broadcastPlot.frame( 10, 23, 0.95, 90 );
        _broadcastPlot.frame( 10, 23, 0.95, 110 );
        _broadcastPlot.backgroundColor( Color.BLACK );
        _plotWindow.add2DPlot( _broadcastPlot );
        _suppressWarningsCheck = new ZCheckBox( "Suppress \"Unknown Message\" Warnings" );
        _suppressWarningsCheck.setBounds( 165, 175, 315, 25 );
        networkPanel.add( _suppressWarningsCheck );
        _identifyMark5sCheck = new ZCheckBox( "Identify Mark5 Unit Names by Pattern: " );
        _identifyMark5sCheck.setBounds( 165, 205, 305, 25 );
        networkPanel.add( _identifyMark5sCheck );
        _mark5Pattern = new SaneTextField();
        _mark5Pattern.setToolTipText( "Comma separated list of patterns used to match names of Mark5 units.\n"
                + "Patterns are REGULAR EXPRESSIONS!" );
        _mark5Pattern.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateMark5PatternList();
            }
        } );
        networkPanel.add( _mark5Pattern );
        _inactivityWarning = new NumberBox();
        _inactivityWarning.setHorizontalAlignment( NumberBox.LEFT );
        _inactivityWarning.setToolTipText( "Seconds of cluster node network inactivity (i.e. no messages received from) before a warning (yellow light) appears." );
        _inactivityWarning.minimum( 1 );
        _inactivityWarning.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                changeInactivitySettings();
            }
        } );
        networkPanel.add( _inactivityWarning );
        JLabel inactivityWarningLabel = new JLabel( "Inactivity Warning (sec):" );
        inactivityWarningLabel.setBounds( 10, 235, 150, 25 );
        inactivityWarningLabel.setHorizontalAlignment( JLabel.RIGHT );
        networkPanel.add( inactivityWarningLabel );
        _inactivityError = new NumberBox();
        _inactivityError.setToolTipText( "Seconds of cluster node network inactivity (i.e. no messages received from) before an error (red light) appears." );
        _inactivityError.setHorizontalAlignment( NumberBox.LEFT );
        _inactivityError.minimum( 1 );
        _inactivityError.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                changeInactivitySettings();
            }
        } );
        networkPanel.add( _inactivityError );
        JLabel inactivityErrorLabel = new JLabel( "Error (sec):" );
        inactivityErrorLabel.setBounds( 210, 235, 150, 25 );
        inactivityErrorLabel.setHorizontalAlignment( JLabel.RIGHT );
        networkPanel.add( inactivityErrorLabel );
        _viewDifxMessagesButton = new JButton( "View DiFX Messages" );
        _viewDifxMessagesButton.setToolTipText( "View incoming DiFX message traffic in detail" );
        _viewDifxMessagesButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _difxMessageProcessor != null )
                    _difxMessageProcessor.showWindow();
            }
        } );
        _viewDifxMessagesButton.setBounds( 480, 175, 175, 25 );
        networkPanel.add( _viewDifxMessagesButton );
        JLabel requestLabel = new JLabel( "Request: " );
        requestLabel.setBounds( 630, 175, 100, 25 );
        requestLabel.setHorizontalAlignment( JLabel.RIGHT );
        networkPanel.add( requestLabel );
        _requestAllMessages = new ZCheckBox( "All" );
        _requestAllMessages.setBounds( 730, 175, 50, 25 );
        _requestAllMessages.setToolTipText( "Instruct <<italic>>guiServer<</italic>> to send <<bold>>ALL<</bold>> DiFX message traffic.\n"
                + "Many DiFX messages are not interpreted by the GUI\n"
                + "and will be ignored." );
        _requestAllMessages.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _requestAllMessages.setSelected( true );
                _requestSpecificMessages.setSelected( false );
                _difxMessageListDisplay.changeCallback();
            }
        } );
        networkPanel.add( _requestAllMessages );
        _requestSpecificMessages = new ZCheckBox( "Selected" );
        _requestSpecificMessages.setBounds( 780, 175, 80, 25 );
        _requestSpecificMessages.setToolTipText( "Instruct <<italic>>guiServer<</italic>> to send only specific DiFX messages\n"
                + "This will limit socket traffic between the GUI and <<italic>>guiServer<</italic>>.\n"
                + "Messages can be chosen using the \"Requested Messages\" button." );
        _requestSpecificMessages.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _requestAllMessages.setSelected( false );
                _requestSpecificMessages.setSelected( true );
                _difxMessageListDisplay.changeCallback();
            }
        } );
        networkPanel.add( _requestSpecificMessages );
        _requestMessagesButton = new ZButton( "Selected Messages" );
        _requestMessagesButton.setBounds( 860, 175, 175, 25 );
        _requestMessagesButton.setToolTipText( "Pick the DiFX messages that will be sent by <<italic>>guiServer<</italic>>." );
        _requestMessagesButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _difxMessageListDisplay.position( _requestMessagesButton.getX() - 100,
                            _requestMessagesButton.getY() + 25 );
                _difxMessageListDisplay.setVisible( true );
            }
        } );
        networkPanel.add( _requestMessagesButton );
        
        IndexedPanel databasePanel = new IndexedPanel( "Database Configuration" );
        databasePanel.openHeight( 305 );
        databasePanel.closedHeight( 20 );
        databasePanel.labelWidth( 300 );
        _scrollPane.addNode( databasePanel );
        _dbUseDataBase = new ZCheckBox( "" );
        _dbUseDataBase.setBounds( 165, 25, 25, 25 );
        databasePanel.add( _dbUseDataBase );
        JLabel dbUseDataBaseLabel = new JLabel( "Use Data Base:" );
        dbUseDataBaseLabel.setBounds( 10, 25, 150, 25 );
        dbUseDataBaseLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbUseDataBaseLabel );
        _dbVersion = new JFormattedTextField();
        _dbVersion.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _dbVersion.setBounds( 285, 25, 180, 25 );
        databasePanel.add( _dbVersion );
        JLabel dbVersionLabel = new JLabel( "Version:" );
        dbVersionLabel.setBounds( 200, 25, 80, 25 );
        dbVersionLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbVersionLabel );
        _dbHost = new JFormattedTextField();
        _dbHost.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _dbHost.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                setDbURL();
            }
        } );
        databasePanel.add( _dbHost );
        JLabel dbHostLabel = new JLabel( "Host:" );
        dbHostLabel.setBounds( 10, 55, 150, 25 );
        dbHostLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbHostLabel );
        _dbPort = new JFormattedTextField();
        _dbPort.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _dbPort.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                setDbURL();
            }
        } );
        databasePanel.add( _dbPort );
        JLabel dbPortLabel = new JLabel( "Port:" );
        dbPortLabel.setBounds( 10, 85, 150, 25 );
        dbPortLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbPortLabel );
        _dbUser = new JFormattedTextField();
        _dbUser.setFocusLostBehavior( JFormattedTextField.COMMIT );
        databasePanel.add( _dbUser );
        _dbUser.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                setDbURL();
            }
        } );
        JLabel dbUserLabel = new JLabel( "User:" );
        dbUserLabel.setBounds( 10, 115, 150, 25 );
        dbUserLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbUserLabel );
        _dbPwd = new JPasswordField();
        _dbPwd.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateDatabaseChangeEvent();
            }
        } );
        databasePanel.add( _dbPwd );
        JLabel dbPwdLabel = new JLabel( "Password:" );
        dbPwdLabel.setBounds( 10, 145, 150, 25 );
        dbPwdLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbPwdLabel );
        _dbName = new JFormattedTextField();
        _dbName.setFocusLostBehavior( JFormattedTextField.COMMIT );
        databasePanel.add( _dbName );
        JLabel dbNameLabel = new JLabel( "DiFX Database:" );
        dbNameLabel.setBounds( 10, 175, 150, 25 );
        dbNameLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbNameLabel );
        _dbMS = new JFormattedTextField();
        _dbMS.setToolTipText( "Database Management System - mysql, postgres, etc." );
        _dbMS.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _dbMS.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateDatabaseChangeEvent();
            }
        } );
        databasePanel.add( _dbMS );
        JLabel dbMSLabel = new JLabel( "Management System:" );
        dbMSLabel.setBounds( 10, 205, 150, 25 );
        dbMSLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbMSLabel );
        _dbDriver = new JFormattedTextField();
        _dbDriver.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _dbDriver.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateDatabaseChangeEvent();
            }
        } );
        databasePanel.add( _dbDriver );
        JLabel dbDriverLabel = new JLabel( "JDBC Driver:" );
        dbDriverLabel.setBounds( 10, 235, 150, 25 );
        dbDriverLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbDriverLabel );
        _dbAutoUpdate = new ZCheckBox( "" );
        _dbAutoUpdate.setBounds( 165, 275, 25, 25 );
        databasePanel.add( _dbAutoUpdate );
        JLabel dbAutoUpdateLabel = new JLabel( "Periodic Update:" );
        dbAutoUpdateLabel.setBounds( 10, 275, 150, 25 );
        dbAutoUpdateLabel.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbAutoUpdateLabel );
        _dbAutoUpdateInterval = new NumberBox();
        _dbAutoUpdateInterval.setBounds( 230, 275, 50, 25 );
        _dbAutoUpdateInterval.minimum( 1.0 );
        databasePanel.add( _dbAutoUpdateInterval );
        JLabel dbAutoUpdateIntervalLabel1 = new JLabel( "every" );
        dbAutoUpdateIntervalLabel1.setBounds( 130, 275, 95, 25 );
        dbAutoUpdateIntervalLabel1.setHorizontalAlignment( JLabel.RIGHT );
        databasePanel.add( dbAutoUpdateIntervalLabel1 );
        JLabel dbAutoUpdateIntervalLabel2 = new JLabel( "seconds" );
        dbAutoUpdateIntervalLabel2.setBounds( 285, 275, 65, 25 );
        databasePanel.add( dbAutoUpdateIntervalLabel2 );
        _pingHostButton = new JButton( "Ping Host" );
        _pingHostButton.setToolTipText( "ping the database host" );
        _pingHostButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                pingDatabaseHost();
            }
        } );
        databasePanel.add( _pingHostButton );
        _testDatabaseButton = new JButton( "Test Database" );
        _testDatabaseButton.setToolTipText( "Run a connection test using the current database settings." );
        _testDatabaseButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                testDatabaseAction();
            }
        } );
        databasePanel.add( _testDatabaseButton );
        _databaseMessages = new MessageScrollPane();
        databasePanel.add( _databaseMessages );
         
        IndexedPanel jobSettingsPanel = new IndexedPanel( "Job Settings" );
        jobSettingsPanel.openHeight( 445 );
        jobSettingsPanel.closedHeight( 20 );
        jobSettingsPanel.labelWidth( 300 );
        _scrollPane.addNode( jobSettingsPanel );
        JLabel workingDirectoryLabel = new JLabel( "Working Directory:" );
        workingDirectoryLabel.setBounds( 10, 25, 150, 25 );
        workingDirectoryLabel.setHorizontalAlignment( JLabel.RIGHT );
        workingDirectoryLabel.setToolTipText( "Root directory on the DiFX host for Experiment data." );
        _workingDirectory = new TabCompletedTextField( this );
        _workingDirectory.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _workingDirectory.setToolTipText( "Root directory on the DiFX host for Experiment data." );
        jobSettingsPanel.add( workingDirectoryLabel );
        jobSettingsPanel.add( _workingDirectory );
        _stagingArea = new JFormattedTextField();
        _stagingArea.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _stagingArea.setToolTipText( "Staging area root directory on the DiFX host." );
        jobSettingsPanel.add( _stagingArea );
        JLabel useStagingAreaLabel = new JLabel( "Use Staging Area:" );
        useStagingAreaLabel.setBounds( 10, 55, 120, 25 );
        useStagingAreaLabel.setHorizontalAlignment( JLabel.RIGHT );
        useStagingAreaLabel.setToolTipText( "Use the staging area to run jobs (or don't)." );
        _useStagingArea = new ZCheckBox( "" );
        _useStagingArea.setBounds( 133, 55, 25, 25 );
        _useStagingArea.setToolTipText( "Use the staging area to run jobs (or don't)." );
        jobSettingsPanel.add( useStagingAreaLabel );
        jobSettingsPanel.add( _useStagingArea );
        _headNode = new SaneTextField();
        jobSettingsPanel.add( _headNode );
        JLabel headNodeLabel = new JLabel( "Head Node:" );
        headNodeLabel.setBounds( 10, 85, 150, 25 );
        headNodeLabel.setHorizontalAlignment( JLabel.RIGHT );
        jobSettingsPanel.add( headNodeLabel );
        _useHeadNodeCheck = new ZCheckBox( "Use Head Node in Processing" );
        _useHeadNodeCheck.setBounds( 480, 85, 250, 25 );
        _useHeadNodeCheck.toolTip( "Allow the head node to be used as a data source or processor.\n"
                + "A thread will be reserved for head node activities.", null );
        _useHeadNodeCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                defaultNames().restrictHeadnodeProcessing = !_useHeadNodeCheck.isSelected();
            }
        } );
        jobSettingsPanel.add( _useHeadNodeCheck );
        JLabel dataSourceLabel = new JLabel( "Data Source Defaults:" );
        dataSourceLabel.setFont( new Font( dataSourceLabel.getFont().getFamily(), Font.BOLD, dataSourceLabel.getFont().getSize() ) );
        dataSourceLabel.setBounds( 30, 115, 200, 25 );
        jobSettingsPanel.add( dataSourceLabel );
        _uniqueDataSource = new ZCheckBox( "Unique Node per Data Source" );
        _uniqueDataSource.toolTip( "Use a unique node as the data source for each antenna within a job.", null );
        _uniqueDataSource.setBounds( 165, 140, 300, 25 );
        jobSettingsPanel.add( _uniqueDataSource );
        _assignBasedOnPath = new ZCheckBox( "Assign Based on Path" );
        _assignBasedOnPath.setBounds( 165, 165, 160, 25 );
        _assignBasedOnPath.toolTip( "Use specific nodes for particular data paths.  Paths are assigned\n"
                + "to nodes using the \"Path Assignments\" settings.\n"
                + "A specific path assignment will override all other considerations.", null );
        _assignBasedOnPath.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _assignBasedOnPath.isSelected() ) {
                    _uniqueDataSource.setEnabled( false );
                    _shareDataSourcesBetweenJobs.setEnabled( false );
                }
                else {
                    _uniqueDataSource.setEnabled( true );
                    _shareDataSourcesBetweenJobs.setEnabled( true );
                }
            }
        } );
        jobSettingsPanel.add( _assignBasedOnPath );
        _pathAssignments = new ZButton( "Path Assignments" );
        _pathAssignments.setBounds( 325, 165, 140, 25 );
        _pathAssignments.toolTip( "Assign data file paths to specific node names.", null );
        _pathAssignments.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _sourceBasedOnPathDisplay == null ) {
                    _sourceBasedOnPathDisplay = new SourceBasedOnPathDisplay( MouseInfo.getPointerInfo().getLocation().x, 
                        MouseInfo.getPointerInfo().getLocation().y, _settings );
                    if ( _sourceBasedOnPathList != null ) {
                        for ( Iterator<String> iter = _sourceBasedOnPathList.keySet().iterator(); iter.hasNext(); ) {
                            String path = iter.next();
                            _sourceBasedOnPathDisplay.addPathNodePair( path, _sourceBasedOnPathList.get( path ) );
                        }
                    }
                }
                _sourceBasedOnPathDisplay.showAtPosition( MouseInfo.getPointerInfo().getLocation().x, 
                        MouseInfo.getPointerInfo().getLocation().y );
            }
        } );
        jobSettingsPanel.add( _pathAssignments );
        _shareDataSourcesAsProcessors = new ZCheckBox( "Share Data Nodes With Processing" );
        _shareDataSourcesAsProcessors.toolTip( "Allow processing on nodes being used as data sources.  Threads\n"
                + "will be reserved for data reading activities.", null );
        _shareDataSourcesAsProcessors.setBounds( 480, 140, 250, 25 );
        jobSettingsPanel.add( _shareDataSourcesAsProcessors );
        _shareDataSourcesBetweenJobs = new ZCheckBox( "Share Data Nodes Between Jobs" );
        _shareDataSourcesBetweenJobs.toolTip( "Allow multiple jobs to use the same nodes as data sources.  Threads\n"
                + "will be reserved for each.", null );
        _shareDataSourcesBetweenJobs.setBounds( 480, 165, 250, 25 );
        jobSettingsPanel.add( _shareDataSourcesBetweenJobs );
        _threadsPerDataSource = new NumberBox();
        _threadsPerDataSource.setBounds( 795, 140, 50, 25 );
        _threadsPerDataSource.minimum( 1 );
        _threadsPerDataSource.precision( 0 );
        jobSettingsPanel.add( _threadsPerDataSource );
        JLabel threadsPerLabel = new JLabel( "Threads Per Data Source" );
        threadsPerLabel.setBounds( 850, 140, 200, 25 );
        jobSettingsPanel.add( threadsPerLabel );
        JLabel useThreadsLabel = new JLabel( "Use:" );
        useThreadsLabel.setBounds( 740, 140, 50, 25 );
        useThreadsLabel.setHorizontalAlignment( JLabel.RIGHT );
        jobSettingsPanel.add( useThreadsLabel );
        
        _restrictSourcesCheck = new ZCheckBox( "Restrict Data Sources" );
        _restrictSourcesCheck.setBounds( 760, 165, 160, 25 );
        _restrictSourcesCheck.toolTip( "Restrict data sources to a list of \"allowed\" source nodes.\n"
                + "This setting should be used if only a subset of available nodes has\n"
                + "access to the storage systems containing source data.", null );
        _restrictSourcesCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
            }
        } );
        jobSettingsPanel.add( _restrictSourcesCheck );
        _viewRestrictedSourceList = new ZButton( "Allowed Sources" );
        _viewRestrictedSourceList.setBounds( 920, 165, 140, 25 );
        _viewRestrictedSourceList.toolTip( "View/edit the list nodes that are \"allowed\" data sources.", null );
        _viewRestrictedSourceList.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _restrictedSourceListDisplay == null ) {
                    _restrictedSourceListDisplay = new RestrictedSourceListDisplay( MouseInfo.getPointerInfo().getLocation().x, 
                        MouseInfo.getPointerInfo().getLocation().y, _settings );
                    if ( _restrictedSourceList != null ) {
                        for ( Iterator<String> iter = _restrictedSourceList.iterator(); iter.hasNext(); ) {
                            String nodeName = iter.next();
                            _restrictedSourceListDisplay.addNode( nodeName );
                        }
                    }
                }
                _restrictedSourceListDisplay.showAtPosition( MouseInfo.getPointerInfo().getLocation().x, 
                        MouseInfo.getPointerInfo().getLocation().y );
            }
        } );
        jobSettingsPanel.add( _viewRestrictedSourceList );
        
        JLabel processingLabel = new JLabel( "Processing Defaults:" );
        processingLabel.setBounds( 30, 195, 200, 25 );
        processingLabel.setFont( new Font( processingLabel.getFont().getFamily(), Font.BOLD, processingLabel.getFont().getSize() ) );
        jobSettingsPanel.add( processingLabel );
        JLabel defaultToLabel = new JLabel( "Run Using:" );
        defaultToLabel.setBounds( 10, 220, 150, 25 );
        defaultToLabel.setHorizontalAlignment( JLabel.RIGHT );
        jobSettingsPanel.add( defaultToLabel );
        _nodesPerCheck = new ZCheckBox( "" );
        _nodesPerCheck.setBounds( 165, 245, 25, 25 );
        _nodesPerCheck.toolTip( "Specify the number of nodes used in processing.", null );
        _nodesPerCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _nodesPerCheck.isSelected() ) {
                    _allThreadsCheck.setEnabled( true );
                    if ( _allThreadsCheck.isSelected()  )
                        _minThreadsPerNode.setEnabled( true );
                    _nodesPer.setEnabled( true );
                    _allNodesCheck.setSelected( false );
                }
                else {
                    _allThreadsCheck.setSelected( false );
                    _allThreadsCheck.setEnabled( false );
                    _threadsPerCheck.setSelected( true );
                    _threadsPerNode.setEnabled( true );
                    _minThreadsPerNode.setEnabled( false );
                    _nodesPer.setEnabled( false );
                }
            }
        } );
        jobSettingsPanel.add( _nodesPerCheck );
        _nodesPer = new NumberBox();
        _nodesPer.setBounds( 190, 245, 50, 25 );
        _nodesPer.minimum( 0 );
        _nodesPer.precision( 0 );
        _nodesPer.toolTip( "Number of nodes to assign to processing.", null );
        jobSettingsPanel.add( _nodesPer );
        JLabel nodesLabel = new JLabel( "Nodes" );
        nodesLabel.setBounds( 245, 245, 50, 25 );
        jobSettingsPanel.add( nodesLabel );
        _allNodesCheck = new ZCheckBox( "All Nodes" );
        _allNodesCheck.setBounds( 165, 220, 100, 25 );
        _allNodesCheck.toolTip( "Use all available nodes in processing.", null );
        _allNodesCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _allNodesCheck.isEnabled() ) {
                    _allThreadsCheck.setEnabled( true );
                    if ( _allThreadsCheck.isSelected()  )
                        _minThreadsPerNode.setEnabled( true );
                    _nodesPerCheck.setSelected( false );
                    _nodesPer.setEnabled( false );
                }
                else {
                    _allThreadsCheck.setSelected( false );
                    _allThreadsCheck.setEnabled( false );
                    _threadsPerCheck.setSelected( true );
                    _threadsPerNode.setEnabled( true );
                    _minThreadsPerNode.setEnabled( false );
                    _nodesPer.setEnabled( false );
                }
            }
        } );
        jobSettingsPanel.add( _allNodesCheck );
        JLabel withLabel = new JLabel( "With:" );
        withLabel.setBounds( 305, 220, 50, 25 );
        withLabel.setHorizontalAlignment( JLabel.RIGHT );
        jobSettingsPanel.add( withLabel );
        _allThreadsCheck = new ZCheckBox( "All Threads" );
        _allThreadsCheck.setBounds( 355, 220, 100, 25 );
        _allThreadsCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _allThreadsCheck.setSelected( true );
                _threadsPerCheck.setSelected( false );
                _threadsPerNode.setEnabled( false );
                _minThreadsPerNode.setEnabled( true );
            }
        } );
        _allThreadsCheck.toolTip( "Utilize all <<italic>>available<</italic>> threads in each node - available\n"
                + "threads will not include threads assigned as data sources or as the head node.", null );
        jobSettingsPanel.add( _allThreadsCheck );
        _minThreadsPerNode = new NumberBox();
        _minThreadsPerNode.setBounds( 505, 220, 50, 25 );
        _minThreadsPerNode.minimum( 1 );
        _minThreadsPerNode.precision( 0 );
        _minThreadsPerNode.toolTip( "Minimum number of threads required when selecting \"All\" threads\n"
                + "in a node.  If fewer are available for a node that otherwise meets\n"
                + "requirements, a new node will be chosen.", null );
        jobSettingsPanel.add( _minThreadsPerNode );
        JLabel minThreadsLabel = new JLabel( "Min:" );
        minThreadsLabel.setBounds( 450, 220, 50, 25 );
        minThreadsLabel.setHorizontalAlignment( JLabel.RIGHT );
        jobSettingsPanel.add( minThreadsLabel );
        _threadsPerNode = new NumberBox();
        _threadsPerNode.setBounds( 380, 245, 50, 25 );
        _threadsPerNode.minimum( 1 );
        _threadsPerNode.precision( 0 );
        jobSettingsPanel.add( _threadsPerNode );
        _threadsPerCheck = new ZCheckBox( "" );
        _threadsPerCheck.setBounds( 355, 245, 25, 25 );
        _threadsPerCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _allThreadsCheck.setSelected( false );
                _threadsPerCheck.setSelected( true );
                _threadsPerNode.setEnabled( true );
                _minThreadsPerNode.setEnabled( false );
            }
        } );
        _threadsPerCheck.toolTip( "Use this many threads - if the number of nodes is also selected this\n"
                + "number is interpreted as \"threads to use per node\" and only nodes\n"
                + "having this number of threads will be utilized.", null );
        jobSettingsPanel.add( _threadsPerCheck );
        JLabel threadsPerNodeLabel = new JLabel( "Threads" );
        threadsPerNodeLabel.setBounds( 435, 245, 100, 25 );
        jobSettingsPanel.add( threadsPerNodeLabel );
        JLabel forEachLabel = new JLabel( "For Each:" );
        forEachLabel.setHorizontalAlignment( JLabel.RIGHT );
        forEachLabel.setBounds( 555, 220, 75, 25 );
        jobSettingsPanel.add( forEachLabel );
        _baselineCheck = new ZCheckBox( "Baseline" );
        _baselineCheck.setBounds( 635, 220, 125, 25 );
        _baselineCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _jobCheck.setSelected( false );
                _baselineCheck.setSelected( true );
            }
        } );
        jobSettingsPanel.add( _baselineCheck );
        _jobCheck = new ZCheckBox( "Job" );
        _jobCheck.setBounds( 635, 245, 125, 25 );
        _jobCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _jobCheck.setSelected( true );
                _baselineCheck.setSelected( false );
            }
        } );
        jobSettingsPanel.add( _jobCheck );
        JLabel schedulerLabel = new JLabel( "Scheduler Settings:" );
        schedulerLabel.setBounds( 30, 275, 200, 25 );
        schedulerLabel.setFont( new Font( processingLabel.getFont().getFamily(), Font.BOLD, processingLabel.getFont().getSize() ) );
        jobSettingsPanel.add( schedulerLabel );
        JLabel runMultipleLabel = new JLabel( "Run Scheduled Jobs:" );
        runMultipleLabel.setHorizontalAlignment( JLabel.RIGHT );
        runMultipleLabel.setBounds( 10, 300, 150, 25 );
        jobSettingsPanel.add( runMultipleLabel );
        _sequentialCheck = new ZCheckBox( "Sequentially" );
        _sequentialCheck.setBounds( 165, 300, 125, 25 );
        _sequentialCheck.toolTip( "Jobs run using the scheduler will wait until all\n"
                + "previously scheduled jobs have been completed.", null );
        jobSettingsPanel.add( _sequentialCheck );
        _sequentialCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _sequentialCheck.setSelected( true );
                _simultaneousCheck.setSelected( false );
                _maxJobs.setEnabled( false );
            }
        } );
        _simultaneousCheck = new ZCheckBox( "Simultaneously" );
        _simultaneousCheck.setBounds( 165, 325, 125, 25 );
        _simultaneousCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _sequentialCheck.setSelected( false );
                _simultaneousCheck.setSelected( true );
                _maxJobs.setEnabled( true );
            }
        } );
        _simultaneousCheck.toolTip( "Jobs run using the scheduler will be run simultaneously\n"
                + "when resources sufficient to meet their default run criteria\n"
                + "are available.", null );
        jobSettingsPanel.add( _simultaneousCheck );
        _maxJobs = new NumberBox();
        _maxJobs.setBounds( 295, 325, 50, 25 );
        _maxJobs.minimum( 1 );
        _maxJobs.precision( 0 );
        _maxJobs.toolTip( "Maximum number of jobs to run simultaneously", null );
        jobSettingsPanel.add( _maxJobs );
        JLabel maxJobsLabel = new JLabel( "Maximum" );
        maxJobsLabel.setBounds( 350, 325, 100, 25 );
        jobSettingsPanel.add( maxJobsLabel );
        _useMaxSecondsForHardware = new ZCheckBox( "" );
        _useMaxSecondsForHardware.setBounds( 475, 300, 25, 25 );
        _useMaxSecondsForHardware.setToolTipText( "Use the maximum number of seconds to limit the time the scheduler\n"
                + "will devote to allocating hardware when no activity appears to be occurring." );
        jobSettingsPanel.add( _useMaxSecondsForHardware );
        _maxSecondsForHardware = new NumberBox();
        _maxSecondsForHardware.setToolTipText( "Maximum number of seconds allowed for allocating hardware resources\n"
                + "If the correlator appears to be inactive, jobs that exceed this limit\n"
                + "are assumed to be \"stuck\" and may be terminated by the scheduler." );
        _maxSecondsForHardware.setBounds( 505, 300, 70, 25 );
        _maxSecondsForHardware.minimum( 10 );
        _maxSecondsForHardware.precision( 0 );
        jobSettingsPanel.add( _maxSecondsForHardware );
        JLabel maxSecondsForHardwareLabel = new JLabel( "Resource Allocation Time Limit (Seconds)" );
        maxSecondsForHardwareLabel.setBounds( 580, 300, 300, 25 );
        jobSettingsPanel.add( maxSecondsForHardwareLabel );
        _useMaxSecondsForProcessing = new ZCheckBox( "" );
        _useMaxSecondsForProcessing.setBounds( 475, 325, 25, 25 );
        _useMaxSecondsForProcessing.setToolTipText( "Use the maximum number of seconds to limit the time the scheduler\n"
                + "will consider a job to be running when no activity appears to be occurring." );
        jobSettingsPanel.add( _useMaxSecondsForProcessing );
        _maxSecondsForProcessing = new NumberBox();
        _maxSecondsForProcessing.setToolTipText( "Maximum number of seconds the scheduler will consider a job \"running\".\n"
                + "If the correlator appears to be inactive, jobs that exceed this limit\n"
                + "are assumed to be \"stuck\" and may be removed from the queue by the\n"
                + "scheduler, thus allowing other jobs to start.  The job may continue to\n"
                + "run - the scheduler cannot stop a job once it has started." );
        _maxSecondsForProcessing.setBounds( 505, 325, 70, 25 );
        _maxSecondsForProcessing.minimum( 10 );
        _maxSecondsForProcessing.precision( 0 );
        jobSettingsPanel.add( _maxSecondsForProcessing );
        JLabel maxSecondsForProcessingLabel = new JLabel( "Processing Time Limit (Seconds)" );
        maxSecondsForProcessingLabel.setBounds( 580, 325, 300, 25 );
        jobSettingsPanel.add( maxSecondsForProcessingLabel );
        JLabel runLogLabel = new JLabel( "Run Log Settings:" );
        runLogLabel.setBounds( 30, 355, 200, 25 );
        runLogLabel.setFont( new Font( processingLabel.getFont().getFamily(), Font.BOLD, processingLabel.getFont().getSize() ) );
        jobSettingsPanel.add( runLogLabel );
        _runLogCheck = new ZCheckBox( "Log Run Data" );
        _runLogCheck.setBounds( 165, 380, 125, 25 );
        _runLogCheck.toolTip( "Collect performance statistics in the \"Run Log\" file\n"
                + "for each job run.", null );
        jobSettingsPanel.add( _runLogCheck );
        JLabel runLogFileLabel = new JLabel( "Run Log File:" );
        runLogFileLabel.setBounds( 10, 405, 150, 25 );
        runLogFileLabel.setHorizontalAlignment( JLabel.RIGHT );
        runLogFileLabel.setToolTipText( "File containing run log statistics." );
        _runLogFile = new FormattedTextField();
        _runLogFile.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _runLogFile.setToolTipText( "Root directory on the DiFX host for Experiment data." );
        jobSettingsPanel.add( runLogFileLabel );
        jobSettingsPanel.add( _runLogFile );

        IndexedPanel eopSettingsPanel = new IndexedPanel( "EOP Settings" );
        //  These editors may or may not be displayed, but they are used to hold
        //  EOP and leap second data regardless.
        _eopText = new SimpleTextEditor();
        _leapSecondText = new SimpleTextEditor();
        eopSettingsPanel.openHeight( 145 );
        eopSettingsPanel.closedHeight( 20 );
        eopSettingsPanel.labelWidth( 300 );
        _scrollPane.addNode( eopSettingsPanel );
        _eopURL = new SaneTextField();
        _eopURL.setToolTipText( "URL of the file containing EOP data." );
        _eopURL.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateEOPNow();
            }
        } );
        eopSettingsPanel.add( _eopURL );
        JLabel eopURLLabel = new JLabel( "EOP URL:" );
        eopURLLabel.setBounds( 10, 25, 150, 25 );
        eopURLLabel.setHorizontalAlignment( JLabel.RIGHT );
        eopSettingsPanel.add( eopURLLabel );
        _viewEOPFile = new JButton( "View" );
        _viewEOPFile.setToolTipText( "View the contents of the most recent EOP file (load new data if necessary)." );
        _viewEOPFile.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _eopDisplay == null ) {
                    _eopDisplay = new JFrame();
                    _eopDisplay.setTitle( "Current EOP Data" );
                    Point pt = _viewEOPFile.getLocationOnScreen();
                    _eopDisplay.setBounds( pt.x + 100, pt.y + 50, 500, 500 );
                    _eopDisplay.add( _eopText );
                }
                if ( _eopText.text() == null || _eopText.text().length() == 0 )
                    updateEOPNow();
                _eopDisplay.setVisible( true );
            }
        } );
        eopSettingsPanel.add( _viewEOPFile );
        _leapSecondsURL = new SaneTextField();
        _leapSecondsURL.setToolTipText( "URL of the file containing leap second data." );
        _leapSecondsURL.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateEOPNow();
            }
        } );
        eopSettingsPanel.add( _leapSecondsURL );
        _viewLeapSecondsFile = new JButton( "View" );
        _viewLeapSecondsFile.setToolTipText( "View the contents of the most recent leap seconds file (load new data if necessary)." );
        _viewLeapSecondsFile.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _leapSecondDisplay == null ) {
                    _leapSecondDisplay = new JFrame();
                    _leapSecondDisplay.setTitle( "Current Leap Second Data" );
                    Point pt = _viewLeapSecondsFile.getLocationOnScreen();
                    _leapSecondDisplay.setBounds( pt.x + 100, pt.y + 50, 500, 500 );
                    _leapSecondDisplay.add( _leapSecondText );
                }
                if ( _leapSecondText.text() == null || _leapSecondText.text().length() == 0 )
                    updateEOPNow();
                _leapSecondDisplay.setVisible( true );
            }
        } );
        eopSettingsPanel.add( _viewLeapSecondsFile );
        JLabel leapSecondsURLLabel = new JLabel( "Leap Seconds URL:" );
        leapSecondsURLLabel.setBounds( 10, 55, 130, 25 );
        leapSecondsURLLabel.setHorizontalAlignment( JLabel.RIGHT );
        eopSettingsPanel.add( leapSecondsURLLabel );
        _useLeapSecondsURL = new ZCheckBox( "" );
        _useLeapSecondsURL.setBounds( 140, 55, 20, 25 );
        _useLeapSecondsURL.setToolTipText( "Obtain leap second data from the given URL." );
        _useLeapSecondsURL.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                leapSecondChoice( _useLeapSecondsURL );
            }
        } );
        eopSettingsPanel.add( _useLeapSecondsURL );
        JLabel leapSecondsValueLabel = new JLabel( "static value:" );
        leapSecondsValueLabel.setBounds( 10, 85, 130, 25 );
        leapSecondsValueLabel.setHorizontalAlignment( JLabel.RIGHT );
        eopSettingsPanel.add( leapSecondsValueLabel );
        _useLeapSecondsValue = new ZCheckBox( "" );
        _useLeapSecondsValue.setBounds( 140, 85, 20, 25 );
        _useLeapSecondsValue.setToolTipText( "Use the given static value as the number of leap seconds." );
        _useLeapSecondsValue.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                leapSecondChoice( _useLeapSecondsValue );
            }
        } );
        eopSettingsPanel.add( _useLeapSecondsValue );
        _leapSecondsValue = new NumberBox();
        _leapSecondsValue.setBounds( 165, 85, 120, 25 );
        _leapSecondsValue.precision( 0 );
        _leapSecondsValue.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateEOPChangeEvent();
            }
        } );
        eopSettingsPanel.add( _leapSecondsValue );
        _autoUpdateEOP = new ZCheckBox( "" );
        _autoUpdateEOP.setBounds( 140, 115, 20, 25 );
        _autoUpdateEOP.setToolTipText( "Periodically update the EOP and leap second data." );
        _autoUpdateEOP.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _autoUpdateEOP.isSelected() ) {
                    updateEOPNow();
                }
            }
        } );
        eopSettingsPanel.add( _autoUpdateEOP );
        JLabel autoUpdateLabel = new JLabel( "auto update:" );
        autoUpdateLabel.setBounds( 10, 115, 130, 25 );
        autoUpdateLabel.setHorizontalAlignment( JLabel.RIGHT );
        eopSettingsPanel.add( autoUpdateLabel );
        _autoUpdateSeconds = new NumberBox();
        _autoUpdateSeconds.setBounds( 220, 115, 120, 25 );
        _autoUpdateSeconds.precision( 0 );
        _autoUpdateSeconds.minimum( 1 );
        _autoUpdateSeconds.setToolTipText( "Number of seconds between automatic updates of EOP and leap second data." );
        eopSettingsPanel.add( _autoUpdateSeconds );
        JLabel autoUpdateLabel1 = new JLabel( "every:" );
        autoUpdateLabel1.setBounds( 160, 115, 55, 25 );
        autoUpdateLabel1.setHorizontalAlignment( JLabel.RIGHT );
        eopSettingsPanel.add( autoUpdateLabel1 );
        JLabel autoUpdateLabel2 = new JLabel( "seconds" );
        autoUpdateLabel2.setBounds( 345, 115, 100, 25 );
        eopSettingsPanel.add( autoUpdateLabel2 );
        _updateEOPNow = new JButton( "Update Now" );
        _updateEOPNow.setToolTipText( "Update the EOP and leap second data now." );
        _updateEOPNow.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateEOPNow();
            }
        } );
        eopSettingsPanel.add( _updateEOPNow );
        
        _addressesPanel = new IndexedPanel( "Documentation Locations" );
        _addressesPanel.openHeight( 155 );
        _addressesPanel.closedHeight( 20 );
        _addressesPanel.labelWidth( 250 );
        JLabel guiDocPathLabel = new JLabel( "GUI Docs:" );
        guiDocPathLabel.setBounds( 10, 25, 100, 25 );
        guiDocPathLabel.setHorizontalAlignment( JLabel.RIGHT );
        guiDocPathLabel.setToolTipText( "Directory (or web address) containing all GUI documentation." );
//        _guiDocPath = new JFormattedTextField();
        _guiDocPath.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _guiDocPath.setToolTipText( "Directory (or web address) containing all GUI documentation." );
        _addressesPanel.add( guiDocPathLabel );
        _addressesPanel.add( _guiDocPath );
        _guiDocPathBrowseButton = new JButton( "Browse..." );
        _guiDocPathBrowseButton.setToolTipText( "Browse the local directory structure for the location of documentation." );
        _guiDocPathBrowseButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                guiDocPathBrowse();
            }
        } );
        _addressesPanel.add( _guiDocPathBrowseButton );
        JLabel difxUsersGroupURLLabel = new JLabel( "Users Group:" );
        difxUsersGroupURLLabel.setBounds( 10, 55, 100, 25 );
        difxUsersGroupURLLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxUsersGroupURLLabel.setToolTipText( "URL of the DiFX Users Group." );
        _difxUsersGroupURL = new JFormattedTextField();
        _difxUsersGroupURL.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _difxUsersGroupURL.setToolTipText( "URL of the DiFX Users Group." );
        _addressesPanel.add( difxUsersGroupURLLabel );
        _addressesPanel.add( _difxUsersGroupURL );
        JLabel difxWikiURLLabel = new JLabel( "DiFX Wiki:" );
        difxWikiURLLabel.setBounds( 10, 85, 100, 25 );
        difxWikiURLLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxWikiURLLabel.setToolTipText( "URL of the DiFX Wiki." );
        _difxWikiURL = new JFormattedTextField();
        _difxWikiURL.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _difxWikiURL.setToolTipText( "URL of the DiFX Wiki." );
        _addressesPanel.add( difxWikiURLLabel );
        _addressesPanel.add( _difxWikiURL );
        JLabel difxSVNLabel = new JLabel( "DiFX SVN:" );
        difxSVNLabel.setBounds( 10, 115, 100, 25 );
        difxSVNLabel.setHorizontalAlignment( JLabel.RIGHT );
        difxSVNLabel.setToolTipText( "URL of the DiFX Subversion repository." );
        _difxSVN = new JFormattedTextField();
        _difxSVN.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _difxSVN.setToolTipText( "URL of the DiFX Subversion repository." );
        _addressesPanel.add( difxSVNLabel );
        _addressesPanel.add( _difxSVN );
        _scrollPane.addNode( _addressesPanel );
        
        //  Start the EOP monitoring thread.  This is either set to automatically
        //  update the EOP data periodically, or can be triggered to do so
        //  immediately.
        _eopMonitorThread = new EOPMonitorThread();
        _eopMonitorThread.start();

        _allObjectsBuilt = true;
        
        //  This seems to be required to get the browser to draw the first time.
        //  Annoying and kludgey, but harmless.
        this.newSize();
        
    }
    
    /*
     * Enable the "Job Settings" buttons/checks related to nodes and threads based on
     * what is checked or not checked.
     */
    public void setNodeAndThreadButtons() {
        //  See if this is a node-based selection, selecting "all nodes" becomes
        //  an option.
        if ( _nodesPerCheck.isSelected() || _allNodesCheck.isSelected() ) {
            _allThreadsCheck.setEnabled( true );
            _nodesPer.setEnabled( true );
        }
        //  If not, it is not an option, nor should it be selected.
        else {
            _allThreadsCheck.setEnabled( false );
            _allThreadsCheck.setSelected( false );
            _threadsPerCheck.setSelected( true );
            _nodesPer.setEnabled( false );
        }
        //  If all threads is checked, enable the minimum number of threads.
        if ( _allThreadsCheck.isSelected() ) {
            _threadsPerNode.setEnabled( false );
            _minThreadsPerNode.setEnabled( true );
        }
        else {
            _threadsPerNode.setEnabled( true );
            _minThreadsPerNode.setEnabled( false );
        }
        if ( _assignBasedOnPath.isSelected() ) {
            _uniqueDataSource.setEnabled( false );
            _shareDataSourcesBetweenJobs.setEnabled( false );
        }
        else {
            _uniqueDataSource.setEnabled( true );
            _shareDataSourcesBetweenJobs.setEnabled( true );
        }
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
            _menuBar.setBounds( 0, 0, w, 25 );
            _scrollPane.setBounds( 0, 25, w, h - 25 );
            _settingsFile.setBounds( 115, 25, w - 145, 25 );
            _title.setBounds( 115, 85, w - 145, 25 );
            //  DiFX Controll Connection settings
            _difxControlAddress.setBounds( 165, 55, 300, 25 );
            _difxControlPort.setBounds( 165, 85, 100, 25 );
            _difxTransferPort.setBounds( 365, 85, 100, 25 );
            _difxMonitorHost.setBounds( 575, 55, 300, 25 );
            _maxTransferPorts.setBounds( 575, 85, 100, 25 );
            _difxMonitorPort.setBounds( 775, 85, 100, 25 );
            _difxControlUser.setBounds( 165, 145, 300, 25 );
            _difxVersion.setBounds( 165, 205, 300, 25 );
            _difxStartScript.setBounds( 165, 235, w - 195, 25 );
            _difxBase.setBounds( 165, 175, w - 195, 25 );
            //  Broadcast network settings
            _ipAddress.setBounds( 165, 55, 300, 25 );
            _port.setBounds( 165, 85, 300, 25 );
            _bufferSize.setBounds( 165, 115, 300, 25 );
            _timeout.setBounds( 165, 145, 300, 25 );
            _plotWindow.setBounds( 470, 33, w - 495, 140 );
            _mark5Pattern.setBounds( 480, 205, w - 510, 25 );
            _inactivityWarning.setBounds( 165, 235, 100, 25 );
            _inactivityError.setBounds( 365, 235, 100, 25 );
            //  Database Configuration
            _dbHost.setBounds( 165, 55, 300, 25 );
            _dbPort.setBounds( 165, 85, 300, 25 );
            _dbUser.setBounds( 165, 115, 300, 25 );
            _dbPwd.setBounds( 165, 145, 300, 25 );
            _dbName.setBounds( 165, 175, 300, 25 );
            _dbMS.setBounds( 165, 205, 300, 25 );
            _dbDriver.setBounds( 165, 235, 300, 25 );
            _pingHostButton.setBounds( 480, 55, 125, 25 );
            _testDatabaseButton.setBounds( 610, 55, 125, 25 );
            _databaseMessages.setBounds( 480, 85, w - 505, 175 );
            //  Documentation Addresses
            _guiDocPath.setBounds( 115, 25, w - 255, 25 );
            _guiDocPathBrowseButton.setBounds( w - 135, 25, 105, 25 );
            _difxUsersGroupURL.setBounds( 115, 55, w - 145, 25 );
            _difxWikiURL.setBounds( 115, 85, w - 145, 25 );
            _difxSVN.setBounds( 115, 115, w - 145, 25 );
            //  Job settings
            _workingDirectory.setBounds( 165, 25, w - 195, 25 );
            _stagingArea.setBounds( 165, 55, w - 195, 25 );
            _headNode.setBounds( 165, 85, 300, 25 );
            _eopURL.setBounds( 165, 25, w - 305, 25 );
            _viewEOPFile.setBounds( w - 135, 25, 105, 25 );
            _leapSecondsURL.setBounds( 165, 55, w - 305, 25 );
            _viewLeapSecondsFile.setBounds( w - 135, 55, 105, 25 );
            _updateEOPNow.setBounds( w - 260, 115, 120, 25 );
            _runLogFile.setBounds( 165, 405, w - 195, 25 );
        }
    }
    
    /*
     * Called when a change has been made in how commands are sent to DiFX - UDP
     * multicast or TCP.
     */
    public void changeDifxControlConnection() {
        if ( _difxUDPCheck.isSelected() ) {
            _useTCPRelayCheck.setEnabled( false );
            //  Close the current client to the guiServer if one is open.
            if ( _guiServerConnection != null ) {
                _guiServerConnection.close();
            }
        }
        else {
            _useTCPRelayCheck.setEnabled( true );
            //  Open a new client socket to the guiServer if we need to.
//            if ( _guiServerConnection == null ) {
//                _guiServerConnection = new GuiServerConnection( difxControlAddress(), 
//                        difxControlPort(), timeout(), this );
//            }
            //  Turn on/off the relay based on the checkbox (and the quality of the connection).
            if ( _guiServerConnection != null && _guiServerConnection.connected() ) {
                //  Tell guiServer which messages we are interested in relaying (if
                //  we are, in fact, relaying).
                if ( _useTCPRelayCheck.isSelected() )
                    _difxMessageListDisplay.changeCallback();
                _guiServerConnection.relayBroadcast( _useTCPRelayCheck.isSelected() );
            }
        }
        generateBroadcastChangeEvent();
    }
    
    /*
     * Called when the TCP connection settings have been changed.  This causes
     * the current connection (if it exists) to be closed.  The TCP connection
     * thread should then reform it.
     */
    public void changeTCPConnectionSettings() {
        if ( _guiServerConnection != null )
            _guiServerConnection.close();
    }
    
    /*
     * This thread makes connections to the guiServer.  It will continually try to
     * do so until a proper connection is made.  If that connection is broken, it
     * will try to make a new one.
     */
    protected class TCPConnectionThread extends Thread {      
        protected int _counter = 0;
        public void run() {
            //  Initial one second delay to make sure everything is set up before
            //  we make our first connection attempt.
            try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
            while ( true ) {
                //  Are we using the TCP connection?
                if ( _difxTCPCheck.isSelected() ) {
                    //  Change the connection status to "connecting".
                    _guiServerConnectionLight.warning();
                    if ( _queueBrowser != null ) {
                        _queueBrowser.guiServerConnectionLight().warning();
                        _queueBrowser.guiServerConnectionStatus( "connecting", "yellow" );
                    }
                    //  Make a new guiServer connection.
                    _guiServerConnection = new GuiServerConnection( _this, difxControlAddress(), 
                            difxControlPort(), timeout() );
                    //  Add callbacks for various actions.
                    _guiServerConnection.addConnectionListener( new ActionListener() {
                        public void actionPerformed( ActionEvent e ) {
                            if ( e.getActionCommand().contentEquals( "connected" ) ) {
                                _guiServerConnectionLight.on( true );
                                if ( _queueBrowser != null ) {
                                    _queueBrowser.guiServerConnectionLight().on( true );
                                    _queueBrowser.guiServerConnectionStatus( "connected", "green" );
                                }
                            }
                            else {
                                _guiServerConnectionLight.alert();
                                guiServerVersion( "N/A" );
                                guiServerDifxVersion( "N/A" );
                                if ( _queueBrowser != null ) {
                                    _queueBrowser.guiServerConnectionLight().alert();
                                    _queueBrowser.guiServerConnectionStatus( "connection failed", "red" );
                                }
                            }
                        }
                    } );
                    //  These might be useful for some sort of connection display...
//                    _guiServerConnection.addSendListener( new ActionListener() {
//                        public void actionPerformed( ActionEvent e ) {
//                            System.out.println( "sent " + e.getActionCommand() + " bytes" );
//                        }
//                    } );
//                    _guiServerConnection.addReceiveListener( new ActionListener() {
//                        public void actionPerformed( ActionEvent e ) {
//                            System.out.println( "received " + e.getActionCommand() + " bytes" );
//                        }
//                    } );
                    if ( _guiServerConnection.connect() ) {
                        _counter = 0;
                        if ( _messageCenter != null )
                            _messageCenter.message( 0, "SystemSettings - guiServer connection", "connection successful" );
                        //  Tell guiServer which messages we are interested in relaying (if
                        //  we are, in fact, relaying).
                        if ( _useTCPRelayCheck.isSelected() )
                            _difxMessageListDisplay.changeCallback();
                        //  This is used to relay multicast packets.
                        _guiServerConnection.relayBroadcast( _useTCPRelayCheck.isSelected() );
                        //  Hang out while this connection is running.  We'll notice a
                        //  break in a tenth of a second.
                        while ( _guiServerConnection.connected() ) {
                            try { Thread.sleep( 100 ); } catch ( Exception e ) {}
                        }
                        //  Connection lost
                        _guiServerConnectionLight.warning();
                        if ( _queueBrowser != null ) {
                            _queueBrowser.guiServerConnectionLight().warning();
                            _queueBrowser.guiServerConnectionStatus( "connection lost", "yellow" );
                        }
                    }
                    else {
                        if ( _counter <= 0 ) {
                            if ( _messageCenter != null ) {
                                _messageCenter.error( 0, "SystemSettings - guiServer connection", 
                                        difxControlAddress() + " port " + difxControlPort() + " - connection failed" );
                            }
                            else {
                                java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE,
                                            difxControlAddress() + " port " + difxControlPort() + " - connection failed" );
                                }
                            _counter = 10;
                        }
                        --_counter;
                        _guiServerConnectionLight.alert();
                        guiServerVersion( "N/A" );
                        guiServerDifxVersion( "N/A" );
                        if ( _queueBrowser != null ) {
                            _queueBrowser.guiServerConnectionLight().alert();
                            _queueBrowser.guiServerConnectionStatus( "connection broken", "red" );
                        }
                    }
                }
                else {
                    _guiServerConnectionLight.on( false );
                    guiServerVersion( "N/A" );
                    guiServerDifxVersion( "N/A" );
                    if ( _queueBrowser != null ) {
                        _queueBrowser.guiServerConnectionLight().on( false );
                        _queueBrowser.guiServerConnectionStatus( "not connected", null );
                    }
                }
                //  One second sleep between attempts to connect.
                try {
                    Thread.sleep( 1000 );
                } catch ( Exception e ) {}
            }
        }        
    }
    
    /*
     * Return the socket connection...
     */
    public GuiServerConnection guiServerConnection() { return _guiServerConnection; }

    /*
     * Lets us know when we are using the TCP relay or regular UDP to collect packets.
     * I think this is only used by the ReadMessageThread function, so possibly the
     * whole operation should be in here.
     */
    public boolean useTCPRelay() {
        if ( _guiServerConnection == null )
            return false;
        //  All of these things have to be true!!
        return ( _useTCPRelayCheck.isSelected() && _difxTCPCheck.isSelected() &&
                 _guiServerConnection.connected() );
    }
    
    /*
     * Send commands via TCP instead of UDP.
     */
    public boolean sendCommandsViaTCP() {
        if ( _guiServerConnection == null )
            return false;
        return ( _difxTCPCheck.isSelected() && _guiServerConnection.connected() );
    }
    
    public void messageCenter( MessageDisplayPanel newVal ) {
        _messageCenter = newVal;
    }
    public MessageDisplayPanel messageCenter() { return _messageCenter; }
    
    /*
     * Called when a change is made to the length of inactivity warning or error
     * settings.  These cause the network activity lights next to cluster nodes in
     * the hardware display to turn yellow and red after specific time intervals
     * during which no network messages are received.
     */
    protected void changeInactivitySettings() {
        if ( _inactivityChangeListeners == null )
            return;
        Object[] listeners = _inactivityChangeListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    /*
     * Add a new listener for changes to the inactivity settings.
     */
    public void inactivityChangeListener( ActionListener a ) {
        if ( _inactivityChangeListeners == null )
            _inactivityChangeListeners = new EventListenerList();
        _inactivityChangeListeners.add( ActionListener.class, a );
    }

    /*
     * Called when one of the checks associated with leap seconds is picked.
     */
    protected void leapSecondChoice( ZCheckBox check ) {
        if ( check == _useLeapSecondsURL ) {
            _useLeapSecondsURL.setSelected( true );
            _useLeapSecondsValue.setSelected( false );
            _leapSecondsURL.setEnabled( true );
            _leapSecondsValue.setEnabled( false );
            updateEOPNow();
        }
        else {
            _useLeapSecondsURL.setSelected( false );
            _useLeapSecondsValue.setSelected( true );
            _leapSecondsURL.setEnabled( false );
            _leapSecondsValue.setEnabled( true );
            generateEOPChangeEvent();
        }
    }
    
    /*
     * Called whenever an update is required for the EOP and leap second data.
     * Because EOP data queries can hang, this triggers the activity in the
     * independent thread.
     */
    protected void updateEOPNow() {
        _doEOPUpdate = true;
    }
    
    /*
     * Thread to search for EOP data remotely.  This runs every _EOPUpdateInterval
     * seconds (when that capability is on).
     */
    protected class EOPMonitorThread extends Thread {
        
        public boolean cancelThis;
        
        public void run() {
            while ( !cancelThis ) {
                try {
                    Thread.sleep( 1000 );
                } catch ( Exception e ) { }
                ++_eopTimerCount;
                if ( _eopTimerCount >= _autoUpdateSeconds.intValue() )
                    updateEOPNow();
                //  Do an update if one has been requested (or the count above
                //  was reached) and reset the timer.
                if ( _doEOPUpdate ) {
                    EOPUpdate();
                    _eopTimerCount = 0;
                    _doEOPUpdate = false;
                }
            }
        }
        
    }
    
    /*
     * This function performs the EOP updates from remote servers.  It can hang.
     */
    protected void EOPUpdate() {
//        boolean dontDo = true;
//        if ( dontDo ) {
//            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.WARNING,
//                "remote EOP updates are currently DISABLED" );
//            return;
//        }
        //  Read the specified EOP data.
        try {
            URL url = new URL( _eopURL.getText() );
            url.openConnection();
            InputStream reader = url.openStream();
            byte[] buffer = new byte[100000];
            int bytesRead = 0;
            _eopText.text( "" );
            while ( ( bytesRead = reader.read( buffer, 0, 99999 ) ) > 0 ) {
                _eopText.addText( new String( buffer ).substring( 0, bytesRead ) );
        }
        } catch ( IOException e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                "EOP URL " + _eopURL.getText() + " triggered exception: " + e.getMessage() );
        }
        //  If the leap second data is being read from a URL, do that.
        if ( _useLeapSecondsURL.isSelected() ) {
            try {
                URL url = new URL( _leapSecondsURL.getText() );
                url.openConnection();
                InputStream reader = url.openStream();
                byte[] buffer = new byte[100000];
                int bytesRead = 0;
                _leapSecondText.text( "" );
                while ( ( bytesRead = reader.read( buffer, 0, 99999 ) ) > 0 ) {
                    _leapSecondText.addText( new String( buffer ).substring( 0, bytesRead ) );
            }
            } catch ( IOException e ) {
                java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Leap Second URL " + _leapSecondsURL.getText() + " triggered exception: " + e.getMessage() );
            }
        }
        _eopTimerCount = 0;
        //  In case anyone is out there listening, generate callbacks indicating
        //  new EOP data exist.
        generateEOPChangeEvent();
        //eopData( 2447302.0 - 3.0, 2447302.0 + 3.0 );       
    }
    
    public class EOPStructure {
        double date;     //  Julian day
        double tai_utc;  //  Leap second for the day
        double xPole;    //  X pole coordinate in 0.1 arcsec
        double yPole;    //  Y pole coordinate in 0.1 arcsec
        double ut1_tai;  //  UT1 - TAI in microseconds of time
        double sXPole;   //  X pole uncertainty in 0.1 arcsec
        double sYPole;   //  Y pole uncertainty in 0.1 arcsec
        double sUt1_tai; //  UT1-TAI uncertainty in microseconds of time
    }

    /*
     * Generate an array list of structures containing EOP data for a specified range
     * of days.
     */
    public ArrayList<EOPStructure> eopData( double before, double after ) {
        ArrayList<EOPStructure> newList = new ArrayList<EOPStructure>();
        int fromIndex = 0;
        String content = _eopText.text();
        int toIndex = content.indexOf( '\n', 0 );
        while ( toIndex != -1 ) {
            try {
                double testDate = Double.parseDouble( content.substring( fromIndex, fromIndex + 9 ) );
                if ( testDate > before && testDate < after ) {
                    EOPStructure newEOP = new EOPStructure();
                    newEOP.date = testDate;
                    newEOP.tai_utc = leapSecond( testDate );
                    newEOP.xPole = Double.parseDouble( _eopText.text().substring( fromIndex + 10, fromIndex + 17 ) );
                    newEOP.yPole = Double.parseDouble( _eopText.text().substring( fromIndex + 18, fromIndex + 26 ) );
                    newEOP.ut1_tai = Double.parseDouble( _eopText.text().substring( fromIndex + 26, fromIndex + 35 ) );
                    newEOP.sXPole = Double.parseDouble( _eopText.text().substring( fromIndex + 36, fromIndex + 42 ) );
                    newEOP.sYPole = Double.parseDouble( _eopText.text().substring( fromIndex + 43, fromIndex + 49 ) );
                    newEOP.sUt1_tai = Double.parseDouble( _eopText.text().substring( fromIndex + 50, fromIndex + 57 ) );
                    newList.add( newEOP );
                }
            } catch ( java.lang.NumberFormatException e ) {
                //  We expect a few of these...comments, etc.
            }
            fromIndex = toIndex + 1;
            toIndex = content.indexOf( '\n', fromIndex );
        }
        return newList;
    }
    
    /*
     * Generate a leap second from the leap second data for a specific Julian date.
     */
    public double leapSecond( double date ) {
        //  See if we are using a constant value...in which case we just return
        //  that.
        if ( _useLeapSecondsValue.isSelected() ) {
            return _leapSecondsValue.value();
        }
        else {
            //  Plod through the leap second data looking for the correct date.
            if ( _leapSecondText.text() == null || _leapSecondText.text().length() == 0 ) {
                java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "No current leap second data available." );
                return 0.0;
            }
            else {
                //  Plod through the leap second data looking for the nearest date
                //  before the date we are looking for.
                int fromIndex = 0;
                int toIndex = _leapSecondText.text().indexOf( '\n', 0 );
                boolean found = false;
                double testDay = 0.0;
                double testLeap = 0.0;
                int count = 0;
                while ( toIndex != -1 && !found ) {
                    testDay = Double.parseDouble( _leapSecondText.text().substring( fromIndex + 17, fromIndex + 27 ) );
                    if ( testDay > date )
                        found = true;
                    testLeap = Double.parseDouble( _leapSecondText.text().substring( fromIndex + 38, fromIndex + 49 ) );
                    fromIndex = toIndex + 1;
                    toIndex = _leapSecondText.text().indexOf( '\n', fromIndex );
                    ++count;
                }
                //  See what happened...
                if ( found && count == 0 ) {
                    java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                        "Request for Julian date (" + date + ") prior to earliest date in leap second data ("
                            + testDay + ")." );
                }
                else if ( !found && count == 0 ) {
                    java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                        "Leap second data appears to be empty." );
                }
                else if ( !found ) {
                    java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                        "Request for Julian date (" + date + ") after last date in leap second data ("
                            + testDay + ")." );
                }
                return testLeap;
            }
        }
    }
    
    /*
     * Open a new file containing settings.  This uses the file chooser.
     */
    public void openSettingsFile() {
        _fileChooser.setDialogTitle( "Open System Settings File..." );
        _fileChooser.setFileSelectionMode( JFileChooser.FILES_AND_DIRECTORIES );
        _fileChooser.setApproveButtonText( "Open" );
        _fileChooser.setCurrentDirectory( new File( settingsFile() ) );
        int ret = _fileChooser.showOpenDialog( this );
        if ( ret == JFileChooser.APPROVE_OPTION )
            settingsFile( _fileChooser.getSelectedFile().getAbsolutePath() );
    }
    
    /*
     * Browse for the directory that contains GUI documentation.
     */
    public void guiDocPathBrowse() {
        _fileChooser.setDialogTitle( "GUI Documentation Directory..." );
        _fileChooser.setFileSelectionMode( JFileChooser.DIRECTORIES_ONLY );
        _fileChooser.setApproveButtonText( "Select" );
        int ret = _fileChooser.showOpenDialog( this );
        if ( ret == JFileChooser.APPROVE_OPTION )
            _guiDocPath.setText( "file://" + _fileChooser.getSelectedFile().getAbsolutePath() );
    }
    
    /*
     * Save setting to the current settings file.  If this is null, go to "saveSettingsAs()".
     */
    public void saveSettings() {
        if ( _settingsFile.getText() == null )
            saveSettingsAs();
        else
            saveSettingsToFile( _settingsFile.getText() );
    }
    
    /*
     * Open a file chooser to pick a file for saving the current settings.
     */
    public void saveSettingsAs() {
        _fileChooser.setDialogTitle( "Save System Settings to File..." );
        _fileChooser.setCurrentDirectory( new File( settingsFile() ) );
        _fileChooser.setApproveButtonText( "Save" );
        int ret = _fileChooser.showSaveDialog( this );
        if ( ret == JFileChooser.APPROVE_OPTION )
            saveSettingsToFile( _fileChooser.getSelectedFile().getAbsolutePath() );
        _settingsFile.setText( _fileChooser.getSelectedFile().getAbsolutePath() );
    }
    
    /*
     * Set all settings to their internal system defaults.
     */
    public void setDefaults() {
        _jaxbPackage = "edu.nrao.difx.xmllib.difxmessage";
        _home = "/home/swc/difx";
        _loggingEnabled = false;
        _statusValidDuration = 2000l;
        _useTCPRelayCheck.setSelected( false );
        _identifyMark5sCheck.setSelected( true );
        _mark5Pattern.setText( "mark5.*" );
        _requestAllMessages.setSelected( true );
        _requestSpecificMessages.setSelected( false );
        if ( _difxMessageListDisplay == null )
            _difxMessageListDisplay = new DiFXMessageListDisplay( 0, 0, _settings );
        _inactivityWarning.intValue( 20 );
        _inactivityError.intValue( 60 );
        generateMark5PatternList();
        _ipAddress.setText( "224.2.2.1" );
        _port.intValue( 52525 );
        _bufferSize.intValue( 1500 );
        _timeout.intValue( 100 );
        _difxUDPCheck.setSelected( false );
        _difxTCPCheck.setSelected( true );
        _difxControlAddress.setText( "guiServer.hostname" );
        _difxControlPort.intValue( 50200 );
        _difxTransferPort.intValue( 50300 );
        _maxTransferPorts.intValue( 100 );
        _channelAllData.setSelected( false );
        maxTransferPorts();
        _difxMonitorPort.intValue( 52300 );
        _difxMonitorHost.setText( "guiServer.hostname" );
        _difxBase.setText( "/usr/local/swc/difx" );
        _useDefaultStartScript.setSelected( true );
        _dbUseDataBase.setSelected( false );
        _dbVersion.setText( "unknown" );
        _dbHost.setText( "database.hostname" );
        _dbUser.setText( "difx" );
        _dbPwd.setText( "password" );
        _dbName.setText( "difxdb" );
        _dbMS.setText( "mysql" );
        _dbDriver.setText( "com.mysql.jdbc.Driver" );
        _dbPort.setText( "3306" );
        this.setDbURL();
        _reportLoc = "/users/difx/Desktop";
        _guiDocPath.setText( "file://" + System.getProperty( "user.dir" ) + "/../doc" );
        _difxUsersGroupURL.setText( "http://groups.google.com/group/difx-users/topics" );
        _difxWikiURL.setText( "http://cira.ivec.org/dokuwiki/doku.php/difx/start" );
        _difxSVN.setText( "https://svn.atnf.csiro.au/trac/difx" );
        _dbAutoUpdate.setSelected( false );
        _dbAutoUpdateInterval.intValue( 100 );
        _workingDirectory.setText( "/" );
        _stagingArea.setText( "/queue" );
        _useStagingArea.setSelected( false );
        _headNode.setText( _difxControlAddress.getText() );
        _nodesPer.value( 2 );
        _nodesPerCheck.setSelected( true );
        _allNodesCheck.setSelected( false );
        _allThreadsCheck.setSelected( false );
        _uniqueDataSource.setSelected( true );
        _assignBasedOnPath.setSelected( false );
        _shareDataSourcesAsProcessors.setSelected( false );
        _shareDataSourcesBetweenJobs.setSelected( false );
        _threadsPerDataSource.value( 1 );
        _threadsPerNode.value( 8 );
        _minThreadsPerNode.value( 1 );
        _threadsPerCheck.setSelected( true );
        _baselineCheck.setSelected( true );
        _jobCheck.setSelected( false );
        _sequentialCheck.setSelected( false );
        _simultaneousCheck.setSelected( true );
        _maxJobs.intValue( 3 );
        _maxSecondsForHardware.intValue( 600 );
        _useMaxSecondsForHardware.setSelected( false );
        _maxSecondsForProcessing.intValue( 1800 );
        _useMaxSecondsForProcessing.setSelected( false );
        _runLogCheck.setSelected( true );
        _runLogFile.setText( System.getProperty( "user.home" ) + "/.difxGuiRunLog" );
        _queueBrowserSettings.showCompleted = true;
        _queueBrowserSettings.showIncomplete = true;
        _queueBrowserSettings.showSelected = true;
        _queueBrowserSettings.showUnselected = true;
        _queueBrowserSettings.showExperimentScheduled = true;
        _queueBrowserSettings.showPassScheduled = true;
        _windowConfiguration.mainX = 0;
        _windowConfiguration.mainY = 0;
        _windowConfiguration.mainW = 1400;
        _windowConfiguration.mainH = 800;
        _windowConfiguration.title = "USNO DiFX GUI " + VersionWindow.version();
        _title.setText( _windowConfiguration.title );
        _difxUI.setTitle( _windowConfiguration.title );
        _windowConfiguration.verticalPanels = false;
        _windowConfiguration.mainDividerLocation = 650;
        _windowConfiguration.topDividerLocation = 700;
        _windowConfiguration.queueBrowserTearOff = false;
        _windowConfiguration.queueBrowserX = 0;
        _windowConfiguration.queueBrowserY = 0;
        _windowConfiguration.queueBrowserW = 350;
        _windowConfiguration.queueBrowserH = 600;
        _windowConfiguration.hardwareMonitorTearOff = false;
        _windowConfiguration.hardwareMonitorX = 0;
        _windowConfiguration.hardwareMonitorY = 0;
        _windowConfiguration.hardwareMonitorW = 350;
        _windowConfiguration.hardwareMonitorH = 600;
        _windowConfiguration.experimentEditorW = 500;
        _windowConfiguration.experimentEditorH = 430;
        _windowConfiguration.settingsWindowW = 800;
        _windowConfiguration.settingsWindowH = 775;
        this.setSize( _windowConfiguration.settingsWindowW, _windowConfiguration.settingsWindowH );
        _windowConfiguration.jobEditorMonitorWindowW = 900;
        _windowConfiguration.jobEditorMonitorWindowH = 500;
        _windowConfiguration.smartDisplayW = 600;
        _windowConfiguration.smartDisplayH = 300;
        _windowConfiguration.environmentVariableDisplayW = 600;
        _windowConfiguration.environmentVariableDisplayH = 300;
        _windowConfiguration.sourceBasedOnPathDisplayW = 600;
        _windowConfiguration.sourceBasedOnPathDisplayH = 300;
        _windowConfiguration.restrictedSourceListDisplayW = 400;
        _windowConfiguration.restrictedSourceListDisplayH = 300;
        _windowConfiguration.requestedMessageListDisplayW = 320;
        _windowConfiguration.requestedMessageListDisplayH = 500;
        _windowConfiguration.directoryDisplayW = 600;
        _windowConfiguration.directoryDisplayH = 500;
        _windowConfiguration.monitorDisplayW = 600;
        _windowConfiguration.monitorDisplayH = 500;
        _windowConfiguration.diskSearchRulesDisplayW = 600;
        _windowConfiguration.diskSearchRulesDisplayH = 500;
        _windowConfiguration.difxMessageWindowW = 600;
        _windowConfiguration.difxMessageWindowH = 500;
        _windowConfiguration.difxMessageWindowTopFraction = 0.2;
        _windowConfiguration.difxMessageWindowBottomFraction = 0.5;
        _windowConfiguration.difxMessageWindowMessageLimit = 1000;
        _defaultNames.vexFileSource = "";
        _defaultNames.viaHttpLocation = "";
        _defaultNames.viaFtpLocation = "";
        _defaultNames.localFileLocation = "";
        _defaultNames.vexFromHost = false;
        _defaultNames.vexViaHttp = false;
        _defaultNames.vexViaFtp = false;
        _defaultNames.vexFromLocal = false;
        _defaultNames.vexFromExperiment = false;
        _defaultNames.v2dFileSource = "";
        _defaultNames.v2dViaHttpLocation = "";
        _defaultNames.v2dViaFtpLocation = "";
        _defaultNames.localV2dFileLocation = "";
        _defaultNames.v2dFromHost = false;
        _defaultNames.v2dViaHttp = false;
        _defaultNames.v2dViaFtp = false;
        _defaultNames.v2dFromLocal = false;
        _defaultNames.noV2dFile = false;
        _defaultNames.createPassOnExperimentCreation = true;
        _defaultNames.singleInputFile = false;
        _defaultNames.scanBasedJobNames = true;
        _defaultNames.dirListLocation = "";
        _defaultNames.jobCreationSanityCheck = true;
        _defaultNames.restrictHeadnodeProcessing = true;
        _useHeadNodeCheck.setSelected( false );
        _restrictSourcesCheck.setSelected( false );
        _defaultNames.eliminateNonrespondingProcessors = true;
        _defaultNames.eliminateBusyProcessors = true;
        _defaultNames.chooseBasedOnModule = true;
        _defaultNames.busyPercentage = 50.0;
        _defaultNames.correlationDoPolar = true;
        _defaultNames.correlationFFTSpecRes = 0.125;
        _defaultNames.correlationSpecRes = 0.5;
        _defaultNames.correlationTInt = 2.0;
        _defaultNames.correlationNChan = 16;
        _defaultNames.correlationNFFTChan = 128;
        _defaultNames.phaseCalInt = 1;
        _defaultNames.correlationSubintNS = 100000000;
        _defaultNames.toneSelection = "smart";
        _defaultNames.dataFormat = "Mark5B";
        _defaultNames.runMonitor = false;
        _jobColumnSpecs.networkActivity.show = true;
        _jobColumnSpecs.name.show = true;
        _jobColumnSpecs.progressBar.show = true;
        _jobColumnSpecs.state.show = true;
        _jobColumnSpecs.experiment.show = false;
        _jobColumnSpecs.pass.show = false;
        _jobColumnSpecs.priority.show = false;
        _jobColumnSpecs.queueTime.show = false;
        _jobColumnSpecs.correlationStart.show = false;
        _jobColumnSpecs.correlationEnd.show = false;
        _jobColumnSpecs.correlationTime.show = false;
        _jobColumnSpecs.jobStart.show = false;
        _jobColumnSpecs.jobDuration.show = false;
        _jobColumnSpecs.inputFile.show = true;
        _jobColumnSpecs.outputFile.show = false;
        _jobColumnSpecs.outputSize.show = false;
        _jobColumnSpecs.difxVersion.show = false;
        _jobColumnSpecs.speedUpFactor.show = false;
        _jobColumnSpecs.numAntennas.show = false;
        _jobColumnSpecs.numForeignAntennas.show = false;
        _jobColumnSpecs.dutyCycle.show = false;
        _jobColumnSpecs.status.show = false;
        _jobColumnSpecs.active.show = false;
        _jobColumnSpecs.statusId.show = false;
        _jobColumnSpecs.weights.show = false;
        _jobColumnSpecs.weightsAsPlots.show = true;
        _jobColumnSpecs.weightsAsNumbers.show = false;
        
        _jobColumnSpecs.networkActivity.width = 20;
        _jobColumnSpecs.name.width = 150;
        _jobColumnSpecs.progressBar.width = 200;
        _jobColumnSpecs.state.width = 100;
        _jobColumnSpecs.experiment.width = 100;
        _jobColumnSpecs.pass.width = 100;
        _jobColumnSpecs.priority.width = 100;
        _jobColumnSpecs.queueTime.width = 170;
        _jobColumnSpecs.correlationStart.width = 100;
        _jobColumnSpecs.correlationEnd.width = 100;
        _jobColumnSpecs.correlationTime.width = 100;
        _jobColumnSpecs.jobStart.width = 100;
        _jobColumnSpecs.jobDuration.width = 100;
        _jobColumnSpecs.inputFile.width = 400;
        _jobColumnSpecs.outputFile.width = 100;
        _jobColumnSpecs.outputSize.width = 100;
        _jobColumnSpecs.difxVersion.width = 100;
        _jobColumnSpecs.speedUpFactor.width = 100;
        _jobColumnSpecs.numAntennas.width = 100;
        _jobColumnSpecs.numForeignAntennas.width = 100;
        _jobColumnSpecs.dutyCycle.width = 100;
        _jobColumnSpecs.status.width = 100;
        _jobColumnSpecs.active.width = 100;
        _jobColumnSpecs.statusId.width = 100;
        _jobColumnSpecs.weights.width = 200;
        _jobColumnSpecs.weightsAsPlots.width = 100;
        _jobColumnSpecs.weightsAsNumbers.width = 100;
        
        _hardwareColumnSpecs.Ignored.show = false;
        _hardwareColumnSpecs.broadcastMonitor.show = true;
        _hardwareColumnSpecs.NumCPUs.show = false;
        _hardwareColumnSpecs.NumCores.show = false;
        _hardwareColumnSpecs.BogusGHz.show = false;
        _hardwareColumnSpecs.Type.show = false;
        _hardwareColumnSpecs.TypeString.show = false;
        _hardwareColumnSpecs.State.show = true;
        _hardwareColumnSpecs.Enabled.show = false;
        _hardwareColumnSpecs.CpuLoad.show = false;
        _hardwareColumnSpecs.CpuLoadPlot.show = true;
        _hardwareColumnSpecs.UsedMem.show = false;
        _hardwareColumnSpecs.TotalMem.show = false;
        _hardwareColumnSpecs.MemLoad.show = false;
        _hardwareColumnSpecs.MemLoadPlot.show = true;
        _hardwareColumnSpecs.NetRxRate.show = true;
        _hardwareColumnSpecs.NetTxRate.show = true;
        _hardwareColumnSpecs.StateChanged.show = false;
        _hardwareColumnSpecs.BankAVSN.show = true;
        _hardwareColumnSpecs.BankBVSN.show = true;
        _hardwareColumnSpecs.StatusWord.show = false;
        _hardwareColumnSpecs.ActiveBank.show = false;
        _hardwareColumnSpecs.ScanNumber.show = true;
        _hardwareColumnSpecs.ScanName.show = true;
        _hardwareColumnSpecs.Position.show = false;
        _hardwareColumnSpecs.PlayRate.show = false;
        _hardwareColumnSpecs.DataMJD.show = false;
        _hardwareColumnSpecs.CurrentJob.show = true;

        _hardwareColumnSpecs.broadcastMonitor.width = 70;
        _hardwareColumnSpecs.NumCPUs.width = 70;
        _hardwareColumnSpecs.NumCores.width = 70;
        _hardwareColumnSpecs.BogusGHz.width = 70;
        _hardwareColumnSpecs.Type.width = 70;
        _hardwareColumnSpecs.TypeString.width = 70;
        _hardwareColumnSpecs.State.width = 100;
        _hardwareColumnSpecs.Enabled.width = 70;
        _hardwareColumnSpecs.CpuLoad.width = 70;
        _hardwareColumnSpecs.CpuLoadPlot.width = 70;
        _hardwareColumnSpecs.UsedMem.width = 70;
        _hardwareColumnSpecs.TotalMem.width = 70;
        _hardwareColumnSpecs.MemLoad.width = 70;
        _hardwareColumnSpecs.MemLoadPlot.width = 70;
        _hardwareColumnSpecs.NetRxRate.width = 70;
        _hardwareColumnSpecs.NetTxRate.width = 70;
        _hardwareColumnSpecs.StateChanged.width = 70;
        _hardwareColumnSpecs.BankAVSN.width = 70;
        _hardwareColumnSpecs.BankBVSN.width = 70;
        _hardwareColumnSpecs.StatusWord.width = 70;
        _hardwareColumnSpecs.ActiveBank.width = 70;
        _hardwareColumnSpecs.ScanNumber.width = 70;
        _hardwareColumnSpecs.ScanName.width = 180;
        _hardwareColumnSpecs.Position.width = 70;
        _hardwareColumnSpecs.PlayRate.width = 70;
        _hardwareColumnSpecs.DataMJD.width = 70;
        _hardwareColumnSpecs.CurrentJob.width = 200;

        _moduleFormatList = new ArrayList<String>();
        addModuleFormat( "Mark5B" );
        addModuleFormat( "MKIV" );
        addModuleFormat( "VLBA" );
        addModuleFormat( "S2" );
        addModuleFormat( "VDIF" );
        addModuleFormat( "INTERLACEDVDIF" );

        _toneSelectionList = new ArrayList<String>();
        addToneSelection( "smart" );
        addToneSelection( "vex" );
        addToneSelection( "none" );
        addToneSelection( "all" );
        addToneSelection( "ends" );
        addToneSelection( "most" );

        _eopURL.setText( "http://gemini.gsfc.nasa.gov/solve_save/usno_finals.erp" );
        _leapSecondsURL.setText( "http://gemini.gsfc.nasa.gov/500/oper/solve_apriori_files/ut1ls.dat" );
        _leapSecondsValue.value( 34 );
        _autoUpdateSeconds.value( 3600 );
        leapSecondChoice( _useLeapSecondsURL );
        _autoUpdateEOP.setSelected( false );
        
        //  Add a bunch of SMART Attribute information.  SMART attributes are identified
        //  by ID number.  In addition to the ID number they have a name, a boolean indicating
        //  whether high values are good, and a limiting value (whether high or low depends
        //  on the boolean).  Following the limit is a String that explains what going beyond
        //  the limits means - these are used as tooltips for bad values in tables.  Each of 
        //  these fields can be null.
        //  SMART attributes are standardized.  The items here are those produced by "getsmart"
        //  requests to Mark V units, but they represent only a subset of all possible values.
        //  These data are swiped verbatim from http://en.wikipedia.org/wiki/S.M.A.R.T. where 
        //  a complete list can be found.
        addSMARTAttribute( 1, "Read Error Rate", false, null, null );
        addSMARTAttribute( 2, "Throughput Performance", true, null, null );
        addSMARTAttribute( 3, "Spin-Up Time (millisec)", false, null, null );
        addSMARTAttribute( 4, "Start/Stop Count", null, null, null );
        addSMARTAttribute( 5, "Reallocated Sectors Count", false, 0, "A read/write/verification error causes a sector to be \"reallocated\". The raw value represents a count of the bad sectors. A drive which has had any reallocations at all is significantly more likely to fail in the near future." );
        addSMARTAttribute( 6, "Read Channel Margin", null, null, null );
        addSMARTAttribute( 7, "Seek Error Rate", null, null, null );
        addSMARTAttribute( 8, "Seek Time Performance", true, null, null );
        addSMARTAttribute( 9, "Power-On Hours", null, null, null );
        addSMARTAttribute( 10, "Spin Retry Count", false, null, null );
        addSMARTAttribute( 11, "Calibration Retry Count", false, null, null );
        addSMARTAttribute( 12, "Power Cycle Count", null, null, null );
        addSMARTAttribute( 13, "Soft Read Error Rate", false, null, null );
        addSMARTAttribute( 192, "Power-Off Retract Count", false, null, null );
        addSMARTAttribute( 193, "Load Cycle Count", false, null, null );
        addSMARTAttribute( 194, "Temperature", false, null, null );
        addSMARTAttribute( 195, "Hardware ECC Recovered", null, null, null );
        addSMARTAttribute( 196, "Reallocation Event Count", false, 0, "The total count of attempts to transfer data from reallocated sectors to a spare area. Both successful & unsuccessful attempts are counted." );
        addSMARTAttribute( 197, "Current Pending Sector Count", false, null, null );
        addSMARTAttribute( 198, "Uncorrectable Sector Count", false, null, null );
        addSMARTAttribute( 199, "UltraDMA CRC Error Count", false, null, null );
        addSMARTAttribute( 200, "Multi-Zone Error Rate", false, null, null );
        
        _jobLocationDefaults.fileFilter = "";
        _jobLocationDefaults.experimentBasedOnPath = true;
        _jobLocationDefaults.experimentNamed = false;
        _jobLocationDefaults.experimentName = "";
        _jobLocationDefaults.passBasedOnPath = true;
        _jobLocationDefaults.passNamed = false;
        _jobLocationDefaults.noPass = false;
        _jobLocationDefaults.passName = "";
        _jobLocationDefaults.autoUpdate = false;

        setNodeAndThreadButtons();
        
        //  Set up the communications based on current settings.
        changeDifxControlConnection();
        
    }
    
    /*
     * Use the string of Mark5 name-matching patterns to generate a list of individual
     * patterns.
     */
    protected void generateMark5PatternList() {
        //  List is a single comma-separated string.
        _mark5PatternList = _mark5Pattern.getText().split( "[,\\s]+" );
    }
    
    /*
     * Close this window.  At the moment this operation is not complicated.
     */
    public void closeWindow() {
        this.setVisible( false );
    }

    public String settingsFile() { 
        if ( _settingsFile.getText() == null )
            return defaultSettingsFile();
        return _settingsFile.getText();
    }
    
    /*
     * Attempt to read the given settings file.  The filename is put in the settings
     * file path selection area, but a failure to read the file will cause this text
     * to be red (and generate a popup window).
     */
    public void settingsFile( String newFile ) {
        _settingsFile.setText( newFile );
        if ( getSettingsFromFile( newFile ) ) {
            _settingsFile.setForeground( Color.BLACK );
            setNodeAndThreadButtons();
        }
        else {
            _settingsFile.setForeground( Color.RED );
            JOptionPane.showMessageDialog( this, "Settings file \"" + newFile
                    + "\"\ncould not be read.", 
                    "Settings File Read Error", JOptionPane.WARNING_MESSAGE );
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Settings file \"" + newFile
                    + "\" could not be read." );
        }
    }
    
    public void jaxbPackage( String newVal ) { _jaxbPackage = newVal; }
    public String jaxbPackage() { return _jaxbPackage; }
    
    public void home( String newVal ) { _home = newVal; }
    public String home() { return _home; }
    
    public void loggingEnabled( boolean newVal ) { _loggingEnabled = newVal; }
    public boolean loggingEnabled() { return _loggingEnabled; }
    
    public void statusValidDuration( long newVal ) { _statusValidDuration = newVal; }
    public long statusValidDuration() { return _statusValidDuration; }
    
    public void difxControlAddress( String newVal ) { _difxControlAddress.setText( newVal ); }
    public String difxControlAddress() { return _difxControlAddress.getText(); }
    
    public void difxControlPort( int newVal ) { _difxControlPort.intValue( newVal ); }
    public int difxControlPort() { return _difxControlPort.intValue(); }
    public void difxControlPort( String newVal ) { difxControlPort( Integer.parseInt( newVal ) ); }

    /*
     * Set the TCP "transfer" port.  The actual port used will increment up _maxTrasferPorts more than
     * this setting, at which point it will cycle back and use the original.  The
     * idea is that we want to be able to have multiple TCP sessions open at once.
     */
    public void difxTransferPort( int newVal ) { _difxTransferPort.intValue( newVal ); }
    public int difxTransferPort() { return _difxTransferPort.intValue(); }
    public void difxTransferPort( String newVal ) { difxTransferPort( Integer.parseInt( newVal ) ); }
    synchronized public int newDifxTransferPort( int retryAttempts, int sleepTime, boolean reportRetry, boolean reportFailure ) {
        int tryPort = _newDifxTransferPort;
        ++_newDifxTransferPort;
        if ( _newDifxTransferPort >= _maxTransferPorts.intValue() )
            _newDifxTransferPort = 0;
        int initialTryPort = tryPort;
        //  See if the port we want is "free".  If not, try subsequent ports until we
        //  find one that is free (there is a short sleep between each attempt).  If NONE
        //  of the ports are free, produce a warning message (if told to do so) and
        //  retry up to a specified number of times (because the specification decrements
        //  and stops at zero, you can force the retry cycle to go forever by giving it a
        //  zero or negative number).  A failure may also produce a message if desired.
        boolean gotPort = false;
//        System.out.println( "try for port " + tryPort + "(" + ( tryPort + _difxTransferPort.intValue() ) + ") " );
        while ( !gotPort ) {
            synchronized( _transferPortUsed ) {
                if ( !_transferPortUsed[tryPort] ) {
                    gotPort = true;
                    _transferPortUsed[tryPort] = true;
                }
            }
            if ( !gotPort ) {
                tryPort = _newDifxTransferPort;
                ++_newDifxTransferPort;
                if ( _newDifxTransferPort >= _maxTransferPorts.intValue() )
                    _newDifxTransferPort = 0;
                if ( tryPort == initialTryPort ) {
//                    System.out.println( "failed waiting for transfer port " + tryPort + "(" + ( tryPort + _difxTransferPort.intValue() ) + ") to free" );
                    --retryAttempts;
                    if ( retryAttempts == 0 ) {
                        //  We are giving up!
                        if ( reportFailure )
                            _messageCenter.error( 0, "SystemSettings - newDifxTransferPort()", 
                                    Thread.currentThread().getStackTrace()[2].getClassName() + ":" +
                                    Thread.currentThread().getStackTrace()[2].getMethodName() + "failed to find open port - giving up" );
                        return -1;
                    }
                    if ( reportRetry )
                        _messageCenter.warning( 0, "SystemSettings - newDifxTransferPort()", 
                                Thread.currentThread().getStackTrace()[2].getClassName() + ":" +
                                Thread.currentThread().getStackTrace()[2].getMethodName() + "cannot find open port - this may be bad (still trying though)" );
                }   
                try { Thread.sleep( sleepTime ); } catch ( Exception e ) {}
            }
        }
        //System.out.println( Thread.currentThread().getStackTrace()[2].getClassName() + ":"
        //        + Thread.currentThread().getStackTrace()[2].getMethodName() +
        //        " got port " + ( tryPort + _difxTransferPort.intValue() ) + " (" + tryPort + ")" );
        //_transferPortUsed[tryPort] = true;
        return tryPort + _difxTransferPort.intValue();
    }
    public void releaseTransferPort( int port ) {
//        System.out.println( "release port " + port + "(" + ( port - _difxTransferPort.intValue() ) + ")  - waiting for lock" );
        synchronized( _transferPortUsed ) {
           _transferPortUsed[port - _difxTransferPort.intValue()] = false;
        }
//        System.out.println( "got lock - port " + port + " was released" );
    }
    //  This function is called when the number of transfer ports is changed.  It
    //  allocates an array of booleans to let us know when ports are "free".
    protected void maxTransferPorts() {
        _transferPortUsed = new boolean[_maxTransferPorts.intValue()];
    }

    public void difxMonitorPort( int newVal ) { _difxMonitorPort.intValue( newVal ); }
    public int difxMonitorPort() { return _difxMonitorPort.intValue(); }

    public void difxMonitorHost( String newVal ) { _difxMonitorHost.setText( newVal ); }
    public String difxMonitorHost() { return _difxMonitorHost.getText(); }

    public void difxControlUser( String newVal ) { _difxControlUser.setText( newVal ); }
    public String difxControlUser() { return _difxControlUser.getText(); }
    
    /*
     * Set the difxVersion to match a particular string.
     */
    public void difxVersion( String newVal ) { 
        //  If not already in the list of difx versions, add this name.
        boolean found = false;
        for ( int i = 0; i < _difxVersion.getItemCount() && !found; ++i ) {
            if ( _difxVersion.getItemAt( i ).equals( newVal ) ) {
                found = true;
                _difxVersion.setSelectedIndex( i );
            }
        }
    }
    public String difxVersion() { 
        return (String)_difxVersion.getSelectedItem();
    }
    public void addDifxVersion( String newVal ) { 
        //  If not already in the list of difx versions, add this name.
        boolean found = false;
        for ( int i = 0; i < _difxVersion.getItemCount() && !found; ++i ) {
            if ( _difxVersion.getItemAt( i ).equals( newVal ) ) {
                found = true;
                _difxVersion.setSelectedIndex( i );
            }
        }
        if ( !found ) {
            _difxVersion.addItem( newVal );
            _difxVersion.updateUI();
        }
        _difxVersion.setEnabled( true );
        //  Set the current setting to whatever was specified in the XML settings
        //  file.
        if ( _difxVersionPreferred != null )
            this.difxVersion( _difxVersionPreferred );
    }
    public void clearDifxVersion() {
        synchronized ( _difxVersion ) {
            _difxVersionClearOperation = true;
            _difxVersion.removeAllItems();
            _difxVersion.setEnabled( false );
            _difxVersionClearOperation = false;
        }
    }
    
    protected boolean _difxVersionClearOperation;
    
    public void difxBase( String newVal ) { _difxBase.setText( newVal ); }
    public String difxBase() { return _difxBase.getText(); }
    
    public void ipAddress( String newVal ) { _ipAddress.setText( newVal ); }
    public String ipAddress() { return _ipAddress.getText(); }
    
    public void port( int newVal ) { _port.intValue( newVal ); }
    public int port() { return _port.intValue(); }
    public void port( String newVal ) { port( Integer.parseInt( newVal ) ); }
    
    public void bufferSize( int newVal ) { _bufferSize.intValue( newVal ); }
    public int bufferSize() { return _bufferSize.intValue(); }
    public void bufferSize( String newVal ) { bufferSize( Integer.parseInt( newVal ) ); }
    
    public void timeout( int newVal ) { _timeout.intValue( newVal ); }
    public int timeout() { return _timeout.intValue(); }
    public void timeout( String newVal ) { timeout( Integer.parseInt( newVal ) ); }
    
    public boolean useDatabase() { return _dbUseDataBase.isSelected(); }
    public String dbVersion() { return _dbVersion.getText(); }
    public void dbHost( String newVal ) { 
        _dbHost.setText( newVal );
        setDbURL();
    }
    public String dbHost() { return _dbHost.getText(); }
    
    public void dbUser( String newVal ) { 
        _dbUser.setText( newVal );
        setDbURL();
        generateDatabaseChangeEvent();
    }
    public String dbUser() { return _dbUser.getText(); }
    
    public void dbPwd( String newVal ) { 
        _dbPwd.setText( newVal );
        generateDatabaseChangeEvent();
    }
    public String dbPwd() { return new String( _dbPwd.getPassword() ); }
    
    public void dbName( String newVal ) { 
        _dbName.setText( newVal );
        setDbURL();
    }
    public String dbName() { return _dbName.getText(); }
    
    public void dbMS( String newVal ) { 
        _dbMS.setText( newVal );
        setDbURL();
    }
    public String dbMS() { return _dbMS.getText(); }
    
    public void dbDriver( String newVal ) { 
        _dbDriver.setText( newVal );
        generateDatabaseChangeEvent();
    }
    public String dbDriver() { return _dbDriver.getText(); }
    
    public void dbPort( String newVal ) { 
        _dbPort.setText( newVal );
        setDbURL();
    }
    public String dbPort() { return _dbPort.getText(); }
    
    public String dbURL() { return _dbURL; }
    public void dbURL( String newVal ) { 
        _dbURL = newVal;
        generateDatabaseChangeEvent();
    }
    protected void setDbURL() {
        //  Sets the dbURL using other items - this is not accessible to the outside.
        //_dbURL = "jdbc:oracle:thin:@" + _dbHost.getText() + ":" + _oracleJdbcPort.getText() + 
        _dbURL = "jdbc:" + this.dbMS().toLowerCase() + 
                                "://" + this.dbHost() + 
                                ":" + this.dbPort() + 
                                "/" + this.dbName();
        generateDatabaseChangeEvent();
    }
    public boolean dbAutoUpdate() { return _dbAutoUpdate.isSelected(); }
    public void dbAutoUpdate( boolean newVal ) { _dbAutoUpdate.setSelected( newVal ); }
    public int dbAutoUpdateInterval() { return _dbAutoUpdateInterval.intValue(); }
    
    public String workingDirectory() { return _workingDirectory.getText(); }
    public void workingDirectory( String newVal ) { _workingDirectory.setText( newVal ); }
    
    public String stagingArea() { return _stagingArea.getText(); }
    public void stagingArea( String newVal ) { _stagingArea.setText( newVal ); }
    
    public boolean useStagingArea() { return _useStagingArea.isSelected(); }
    public void useStagingArea( boolean newVal ) { _useStagingArea.setSelected( newVal ); }
    
    public String headNode() { return _headNode.getText(); }
    public void headNode( String newVal ) { _headNode.setText( newVal ); }
    
    public String guiDocPath() { return _guiDocPath.getText().substring( 7 ); }
    public String guiBrowsePath() { return _guiDocPath.getText(); }
    
    public int phaseCalInt() { return _defaultNames.phaseCalInt; }
    public void phaseCalInt( int newVal ) { _defaultNames.phaseCalInt = newVal; }
    public String toneSelection() { return _defaultNames.toneSelection; }
    public void toneSelection( String newVal ) { _defaultNames.toneSelection = newVal; }
    public String dataFormat() { return _defaultNames.dataFormat; }
    public void dataFormat( String newVal ) { _defaultNames.dataFormat = newVal; }
    
    /*
     * Set the look and feel for a new JFrame.  This needs to be called before any
     * GUI components are created.
     */
    public void setLookAndFeel() {
        //  Don't do anything if the look and feel is "null".
        if ( _lookAndFeel == null )
            return;
        try {
            UIManager.setLookAndFeel( UIManager.getCrossPlatformLookAndFeelClassName() );
        }
        catch ( Exception e ) {
            //  This thing throws exceptions, but we ignore them.  Shouldn't hurt
            //  us - if the look and feel isn't set, the default should at least
            //  be visible.
        }
    }
    
    public void launchGUIHelp( String topicAddress ) {
        //  See if there is a "#" extension to this address - which we need to strip
        //  off before we check if this file exists.
        String filename = _guiDocPath.getText().substring( 7 ) + "/" + topicAddress;
        if ( filename.contains( "#" ) )
            filename = filename.substring( 0, filename.indexOf( "#" ) );
        File file = new File( filename );
        if ( file.exists() )
            browseURL( _guiDocPath.getText() + "/" + topicAddress );
        else {
            //  The named file couldn't be found.  Since this file is formed by
            //  the GUI, the name is probably not wrong, so the path probably is.
            //  Generate a temporary file with instructions for setting the documentation
            //  path.
            try {
                BufferedWriter out = new BufferedWriter( new FileWriter( "/tmp/tmpIndex.html" ) );
                out.write( "<h2>Requested Documentation Not Found</h2>\n" );
                out.write( "\n" );
                out.write( "<p>The document you requested:\n" );
                out.write( "<br>\n" );
                out.write( "<pre>\n" );
                out.write( filename + "\n" );
                out.write( "</pre>\n" );
                out.write( "was not found.\n" );
                out.write( "\n" );
                out.write( "<p>The most likely reasons for this is\n" );
                out.write( "that the GUI Documentation Path is not set correctly.\n" );
                out.write( "Check the \"GUI Docs\" setting under the \"Documentation Locations\"\n" );
                out.write( "subset in the Settings Window (to launch the Settings Window pick\n" );
                out.write( "\"Settings/Show Settings\" from the menu bar of the main DiFX GUI Window).\n" );
                out.write( "<p>Note that the path should start with \"<code>file:///</code>\" followed by a complete\n" );
                out.write( "pathname, as in \"<code>file:///tmp/foo.html</code>\".\n" );
                out.close();
                browseURL( "file:///tmp/tmpIndex.html" );
            } catch ( IOException e ) {
            }
        }
    }
    
    public void browseURL( String url ) {
        java.awt.Desktop desktop = java.awt.Desktop.getDesktop();
        if ( !desktop.isSupported( java.awt.Desktop.Action.BROWSE ) ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                "Unable to open \"" + url + "\" - browsing not supported??" );
            return;
        }
        try {
            desktop.browse( new java.net.URI( url ) );
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                "Unable to open \"" + url + "\" - " + e.getMessage() );
        }
    }
    
    public void launchDiFXUsersGroup() {
        browseURL( _difxUsersGroupURL.getText() );
    }
    
    public void launchDiFXWiki() {
        browseURL( _difxWikiURL.getText() );
    }
    
    public void launchDiFXSVN() {
        browseURL( _difxSVN.getText() );
    }
    
    /*
     * Called when a change is made to the multicast settings - either the port
     * or group IP.
     */
    public void multicastSettingsChange() {
        //  Build a string out of the new group and port settings, with a newline
        //  inbetween.
        String dataStr = _ipAddress.getText() + "\n" + _port.getText();
        byte [] data = dataStr.getBytes();
        guiServerConnection().sendPacket( guiServerConnection().MULTICAST_SETTINGS_PACKET, data.length, data );
        generateBroadcastChangeEvent();
    }
    
    /*
     * Add a new listener for database changes.
     */
    public void databaseChangeListener( ActionListener a ) {
        if ( _databaseChangeListeners == null )
            _databaseChangeListeners = new EventListenerList();
        _databaseChangeListeners.add( ActionListener.class, a );
    }

    /*
     * Inform all listeners of a change to database-related items.
     */
    protected void generateDatabaseChangeEvent() {
        if ( _databaseChangeListeners == null )
            return;
        Object[] listeners = _databaseChangeListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    /*
     * Add a new listener for broadcast changes.
     */
    public void broadcastChangeListener( ActionListener a ) {
        if ( _broadcastChangeListeners == null )
            _broadcastChangeListeners = new EventListenerList();
        _broadcastChangeListeners.add( ActionListener.class, a );
    }

    /*
     * Inform all listeners of a change to broadcast-related items.
     */
    protected void generateBroadcastChangeEvent() {
        if ( _broadcastChangeListeners == null )
            return;
        Object[] listeners = _broadcastChangeListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    /*
     * Add a new listener for EOP data changes.
     */
    public void eopChangeListener( ActionListener a ) {
        if ( _eopChangeListeners == null )
            _eopChangeListeners = new EventListenerList();
        _eopChangeListeners.add( ActionListener.class, a );
    }

    /*
     * Inform all listeners of a change to broadcast-related items.
     */
    protected void generateEOPChangeEvent() {
        if ( _eopChangeListeners == null )
            return;
        Object[] listeners = _eopChangeListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    /*
     * Parse a settings file (XML).  We are set up to deal with "old" settings
     * files - those that may not have parameters we are looking for.  We try not
     * to over-write our default settings when such situations arise, although
     * different things happen depending on the variable type.  String settings
     * that don't exist return "null", which is easy to trap.  Numeric settings
     * return 0, which is easy to deal with unless 0 is a valid setting (there are some
     * of these).  Booleans return false when they don't exist, which we just have
     * to assume is the right setting.
     */
    public boolean getSettingsFromFile( String filename ) {
        //  Can't read a non-existent filename
        if ( filename == null || filename.length() < 1 )
            return false;
        //  Or a non-existent file
        File theFile = new File( filename );
        if ( !theFile.exists() ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                "Settings file " + filename + " does not exist." );
            return false;
        }
        //  Now parse the thing, or try to.
        ObjectFactory factory = new ObjectFactory();
        DoiSystemConfig doiConfig = factory.createDoiSystemConfig();
        try {
            javax.xml.bind.JAXBContext jaxbCtx = javax.xml.bind.JAXBContext.newInstance(doiConfig.getClass().getPackage().getName());
            javax.xml.bind.Unmarshaller unmarshaller = jaxbCtx.createUnmarshaller();
            doiConfig = (DoiSystemConfig) unmarshaller.unmarshal( theFile );
            if ( doiConfig.getDifxHome() != null )
                this.home( doiConfig.getDifxHome() );
            //if ( doiConfig.getResourcesFile() != null )   // BLAT do we still use this thing?
            //    this.resourcesFile( doiConfig.getResourcesFile() );
            if ( doiConfig.getDbHost() != null )
                this.dbHost( doiConfig.getDbHost() );
            if ( doiConfig.getDbSID() != null )
                this.dbUser( doiConfig.getDbSID() );
            if ( doiConfig.getDbPassword() != null )
                this.dbPwd( doiConfig.getDbPassword() );
            if ( doiConfig.getDbDriver() != null )
                this.dbDriver( doiConfig.getDbDriver() );
            if ( doiConfig.getDbPort() != null )
                this.dbPort( doiConfig.getDbPort() );
            if ( doiConfig.getDbUrl() != null )
                this.dbURL( doiConfig.getDbUrl() );
            setDbURL();  //  BLAT not sure we need to do this anymore
            if ( doiConfig.getIpAddress() != null )
                this.ipAddress( doiConfig.getIpAddress() );
            if ( doiConfig.getPort() != 0 )
                this.port( doiConfig.getPort() );
            if ( doiConfig.getBufferSize() != 0 )
                this.bufferSize( doiConfig.getBufferSize() );
            _suppressWarningsCheck.setSelected( doiConfig.isSuppressUnknownMessageWarnings() );
            //  Double negative to make checked a default.
            _identifyMark5sCheck.setSelected( !doiConfig.isDontIdentifyMark5SByPattern() );
            if ( doiConfig.getMark5Pattern() != null ) {
                _mark5Pattern.setText( doiConfig.getMark5Pattern() );
                generateMark5PatternList();
            }
            if ( doiConfig.isRequestSpecificMessages() ) {
                _requestAllMessages.setSelected( false );
                _requestSpecificMessages.setSelected( true );
            }
            else {
                _requestAllMessages.setSelected( true );
                _requestSpecificMessages.setSelected( false );
            }
            for ( Iterator<DoiSystemConfig.SelectedMessage> iter = doiConfig.getSelectedMessage().iterator(); iter.hasNext(); ) {
                if ( _difxMessageListDisplay == null )
                    _difxMessageListDisplay = new DiFXMessageListDisplay( 0, 0, _settings );
                _difxMessageListDisplay.selectMessage( iter.next().getType() );
            }
            this.loggingEnabled( doiConfig.isLoggingEnabled() );
            if ( doiConfig.getStatusValidDuration() != 0 )
                this.statusValidDuration( doiConfig.getStatusValidDuration() );
            _channelAllData.setSelected( doiConfig.isChannelAllData() );
            
            if ( doiConfig.getJaxbPackage() != null )
                this.jaxbPackage( doiConfig.getJaxbPackage() );
            if ( doiConfig.getTimeout() != 0 )
                this.timeout( doiConfig.getTimeout() );
            _difxTCPCheck.setSelected( doiConfig.isDifxTCPConnection() );
            _difxUDPCheck.setSelected( !_difxTCPCheck.isSelected() );
            _useTCPRelayCheck.setSelected( doiConfig.isDifxRelayBroadcastViaTCPConnection() );
            if ( doiConfig.getDifxControlAddress() != null )
                this.difxControlAddress( doiConfig.getDifxControlAddress() );
            if ( doiConfig.getDifxControlPort() != 0 )
                this.difxControlPort( doiConfig.getDifxControlPort() );
            if ( doiConfig.getDifxTransferPort() != 0 )
                this.difxTransferPort( doiConfig.getDifxTransferPort() );
            if ( doiConfig.getDifxTransferPortLimit() != 0 ) {
                this._maxTransferPorts.intValue( doiConfig.getDifxTransferPortLimit() );
                maxTransferPorts();
            }
            if ( doiConfig.getDifxMonitorHost() != null )
                this.difxMonitorHost( doiConfig.getDifxMonitorHost() );
            if ( doiConfig.getDifxMonitorPort() != 0 )
                this.difxMonitorPort( doiConfig.getDifxMonitorPort() );
            if ( doiConfig.isDontUseDefaultStartScript() )
                _useDefaultStartScript.setSelected( false );
            if ( doiConfig.getDifxStartScript() != null )
                _difxStartScript.setText( doiConfig.getDifxStartScript() );
            if ( doiConfig.getDifxVersion() != null ) {
                this.difxVersion( doiConfig.getDifxVersion() );
                _difxVersionPreferred = doiConfig.getDifxVersion();
            }
            if ( doiConfig.getDifxBase() != null )
                this.difxBase( doiConfig.getDifxBase() );
            _dbUseDataBase.setSelected( doiConfig.isDbUseDataBase() );
            if ( doiConfig.getDbVersion() != null )
                _dbVersion.setText( doiConfig.getDbVersion() );
            if ( doiConfig.getDbName() != null )
                this.dbName( doiConfig.getDbName() );
            if ( doiConfig.getDbMS() != null )
                this.dbMS( doiConfig.getDbMS() );
            if ( doiConfig.getInactivityWarning() != 0 )
                this.inactivityWarning( doiConfig.getInactivityWarning() );
            if ( doiConfig.getInactivityError() != 0 )
                this.inactivityError( doiConfig.getInactivityError() );
            if ( doiConfig.getReportLoc() != null )
                _reportLoc = doiConfig.getReportLoc();
            if ( doiConfig.getGuiDocPath() != null )
                _guiDocPath.setText( doiConfig.getGuiDocPath() );
            if ( doiConfig.getDifxUsersGroupURL() != null )
                _difxUsersGroupURL.setText( doiConfig.getDifxUsersGroupURL() );
            if ( doiConfig.getDifxWikiURL() != null )
                _difxWikiURL.setText( doiConfig.getDifxWikiURL() );
            if ( doiConfig.getDifxSVN() != null )
                _difxSVN.setText( doiConfig.getDifxSVN() );
            this.dbAutoUpdate( doiConfig.isDbAutoUpdate() );
            if ( doiConfig.getDbAutoUpdateInterval() != 0 )
                _dbAutoUpdateInterval.intValue( doiConfig.getDbAutoUpdateInterval() );
            if ( doiConfig.getWorkingDirectory() != null )
                _workingDirectory.setText( doiConfig.getWorkingDirectory() );
            if ( doiConfig.getStagingArea() != null )
                _stagingArea.setText( doiConfig.getStagingArea() );
            _useStagingArea.setSelected( doiConfig.isUseStagingArea() );
            if ( doiConfig.getDifxHeadNode() != null )
                this.headNode( doiConfig.getDifxHeadNode() );

            _queueBrowserSettings.showCompleted = doiConfig.isQueueShowCompleted();
            _queueBrowserSettings.showIncomplete = doiConfig.isQueueShowIncomplete();
            _queueBrowserSettings.showSelected = doiConfig.isQueueShowSelected();
            _queueBrowserSettings.showUnselected = doiConfig.isQueueShowUnselected();
            _queueBrowserSettings.showExperimentScheduled = doiConfig.isQueueShowExperimentScheduled();
            _queueBrowserSettings.showPassScheduled = doiConfig.isQueueShowPassScheduled();
            if ( doiConfig.getWindowConfigMainX() != 0 )
                _windowConfiguration.mainX = doiConfig.getWindowConfigMainX();
            if ( doiConfig.getWindowConfigMainY() != 0 )
                _windowConfiguration.mainY = doiConfig.getWindowConfigMainY();
            if ( doiConfig.getWindowConfigMainW() != 0 )
                _windowConfiguration.mainW = doiConfig.getWindowConfigMainW();
            if ( doiConfig.getWindowConfigMainH() != 0 )
                _windowConfiguration.mainH = doiConfig.getWindowConfigMainH();
            if ( doiConfig.getWindowConfigTitle() != null ) {
                _title.setText( doiConfig.getWindowConfigTitle() );
                _difxUI.setTitle( doiConfig.getWindowConfigTitle() );
            }
            _windowConfiguration.verticalPanels = doiConfig.isWindowConfigVerticalPanels();
            if ( doiConfig.getWindowConfigMainDividerLocation() != 0 )
                _windowConfiguration.mainDividerLocation = doiConfig.getWindowConfigMainDividerLocation();
            if ( doiConfig.getWindowConfigTopDividerLocation() != 0 )
                _windowConfiguration.topDividerLocation = doiConfig.getWindowConfigTopDividerLocation();
            _windowConfiguration.queueBrowserTearOff = doiConfig.isWindowConfigQueueBrowserTearOff();
            if ( doiConfig.getWindowConfigQueueBrowserX() != 0 )
                _windowConfiguration.queueBrowserX = doiConfig.getWindowConfigQueueBrowserX();
            if ( doiConfig.getWindowConfigQueueBrowserY() != 0 )
                _windowConfiguration.queueBrowserY = doiConfig.getWindowConfigQueueBrowserY();
            if ( doiConfig.getWindowConfigQueueBrowserW() != 0 )
                _windowConfiguration.queueBrowserW = doiConfig.getWindowConfigQueueBrowserW();
            if ( doiConfig.getWindowConfigQueueBrowserH() != 0 )
                _windowConfiguration.queueBrowserH = doiConfig.getWindowConfigQueueBrowserH();
            _windowConfiguration.hardwareMonitorTearOff = doiConfig.isWindowConfigHardwareMonitorTearOff();
            if ( doiConfig.getWindowConfigHardwareMonitorX() != 0 )
                _windowConfiguration.hardwareMonitorX = doiConfig.getWindowConfigHardwareMonitorX();
            if ( doiConfig.getWindowConfigHardwareMonitorY() != 0 )
                _windowConfiguration.hardwareMonitorY = doiConfig.getWindowConfigHardwareMonitorY();
            if ( doiConfig.getWindowConfigHardwareMonitorW() != 0 )
                _windowConfiguration.hardwareMonitorW = doiConfig.getWindowConfigHardwareMonitorW();
            if ( doiConfig.getWindowConfigHardwareMonitorH() != 0 )
                _windowConfiguration.hardwareMonitorH = doiConfig.getWindowConfigHardwareMonitorH();
            if ( doiConfig.getWindowConfigExperimentEditorW() != 0 )
                _windowConfiguration.experimentEditorW = doiConfig.getWindowConfigExperimentEditorW();
            if ( doiConfig.getWindowConfigExperimentEditorH() != 0 )
                _windowConfiguration.experimentEditorH = doiConfig.getWindowConfigExperimentEditorH();
            if ( doiConfig.getWindowConfigSettingsWindowW() != 0 )
                _windowConfiguration.settingsWindowW = doiConfig.getWindowConfigSettingsWindowW();
            if ( doiConfig.getWindowConfigSettingsWindowH() != 0 )
                _windowConfiguration.settingsWindowH = doiConfig.getWindowConfigSettingsWindowH();
            this.setSize( _windowConfiguration.settingsWindowW, _windowConfiguration.settingsWindowH );
            if ( doiConfig.getWindowConfigJobEditorMonitorWindowW() != 0 )
                _windowConfiguration.jobEditorMonitorWindowW = doiConfig.getWindowConfigJobEditorMonitorWindowW();
            if ( doiConfig.getWindowConfigJobEditorMonitorWindowH() != 0 )
                _windowConfiguration.jobEditorMonitorWindowH = doiConfig.getWindowConfigJobEditorMonitorWindowH();
            if ( doiConfig.getWindowConfigSmartDisplayW() != 0 )
                _windowConfiguration.smartDisplayW = doiConfig.getWindowConfigSmartDisplayW();
            if ( doiConfig.getWindowConfigSmartDisplayH() != 0 )
                _windowConfiguration.smartDisplayH = doiConfig.getWindowConfigSmartDisplayH();
            if ( doiConfig.getWindowConfigEnvironmentVariableDisplayW() != 0 )
                _windowConfiguration.environmentVariableDisplayW = doiConfig.getWindowConfigEnvironmentVariableDisplayW();
            if ( doiConfig.getWindowConfigEnvironmentVariableDisplayH() != 0 )
                _windowConfiguration.environmentVariableDisplayH = doiConfig.getWindowConfigEnvironmentVariableDisplayH();
            if ( doiConfig.getWindowConfigSourceBasedOnPathDisplayW() != 0 )
                _windowConfiguration.sourceBasedOnPathDisplayW = doiConfig.getWindowConfigSourceBasedOnPathDisplayW();
            if ( doiConfig.getWindowConfigSourceBasedOnPathDisplayH() != 0 )
                _windowConfiguration.sourceBasedOnPathDisplayH = doiConfig.getWindowConfigSourceBasedOnPathDisplayH();
            if ( doiConfig.getWindowConfigRestrictedSourceListDisplayW() != 0 )
                _windowConfiguration.restrictedSourceListDisplayW = doiConfig.getWindowConfigRestrictedSourceListDisplayW();
            if ( doiConfig.getWindowConfigRestrictedSourceListDisplayH() != 0 )
                _windowConfiguration.restrictedSourceListDisplayH = doiConfig.getWindowConfigRestrictedSourceListDisplayH();
            if ( doiConfig.getWindowConfigSelectedMessagesDisplayW() != 0 )
                _windowConfiguration.requestedMessageListDisplayW = doiConfig.getWindowConfigSelectedMessagesDisplayW();
            if ( doiConfig.getWindowConfigSelectedMessagesDisplayH() != 0 )
                _windowConfiguration.requestedMessageListDisplayH = doiConfig.getWindowConfigSelectedMessagesDisplayH();
            if ( doiConfig.getWindowConfigDirectoryDisplayW() != 0 )
                _windowConfiguration.directoryDisplayW = doiConfig.getWindowConfigDirectoryDisplayW();
            if ( doiConfig.getWindowConfigDirectoryDisplayH() != 0 )
                _windowConfiguration.directoryDisplayH = doiConfig.getWindowConfigDirectoryDisplayH();
            if ( doiConfig.getWindowConfigMonitorDisplayW() != 0 )
                _windowConfiguration.monitorDisplayW = doiConfig.getWindowConfigMonitorDisplayW();
            if ( doiConfig.getWindowConfigMonitorDisplayH() != 0 )
                _windowConfiguration.monitorDisplayH = doiConfig.getWindowConfigMonitorDisplayH();
            if ( doiConfig.getWindowConfigDiskSearchRulesDisplayW() != 0 )
                _windowConfiguration.diskSearchRulesDisplayW = doiConfig.getWindowConfigDiskSearchRulesDisplayW();
            if ( doiConfig.getWindowConfigDiskSearchRulesDisplayH() != 0 )
                _windowConfiguration.diskSearchRulesDisplayH = doiConfig.getWindowConfigDiskSearchRulesDisplayH();
            if ( doiConfig.getWindowConfigDifxMessageWindowW() != 0 )
                _windowConfiguration.difxMessageWindowW = doiConfig.getWindowConfigDifxMessageWindowW();
            if ( doiConfig.getWindowConfigDifxMessageWindowH() != 0 )
                _windowConfiguration.difxMessageWindowH = doiConfig.getWindowConfigDifxMessageWindowH();
            if ( doiConfig.getWindowConfigDifxMessageWindowTopFraction() != 0.0 )
                _windowConfiguration.difxMessageWindowTopFraction = doiConfig.getWindowConfigDifxMessageWindowTopFraction();
            if ( doiConfig.getWindowConfigDifxMessageWindowBottomFraction() != 0.0 )
                _windowConfiguration.difxMessageWindowBottomFraction = doiConfig.getWindowConfigDifxMessageWindowBottomFraction();
            if ( doiConfig.getWindowConfigDifxMessageWindowMessageLimit() != 0.0 )
                _windowConfiguration.difxMessageWindowMessageLimit = doiConfig.getWindowConfigDifxMessageWindowMessageLimit();

            if ( doiConfig.getDefaultNamesVexFileSource() != null )
                _defaultNames.vexFileSource = doiConfig.getDefaultNamesVexFileSource();
            if ( doiConfig.getDefaultNamesViaHttpLocation() != null )
                _defaultNames.viaHttpLocation = doiConfig.getDefaultNamesViaHttpLocation();
            if ( doiConfig.getDefaultNamesViaFtpLocation() != null )
                _defaultNames.viaFtpLocation = doiConfig.getDefaultNamesViaFtpLocation();
            if ( doiConfig.getDefaultNamesLocalFileLocation() != null )
                _defaultNames.localFileLocation = doiConfig.getDefaultNamesLocalFileLocation();
            if ( doiConfig.getDefaultNamesVexSourceChoice() != null ) {
                if ( doiConfig.getDefaultNamesVexSourceChoice().contentEquals( "host" ) )
                    _defaultNames.vexFromHost = true;
                else if ( doiConfig.getDefaultNamesVexSourceChoice().contentEquals( "http" ) )
                    _defaultNames.vexViaHttp = true;
                else if ( doiConfig.getDefaultNamesVexSourceChoice().contentEquals( "ftp" ) )
                    _defaultNames.vexViaFtp = true;
                else if ( doiConfig.getDefaultNamesVexSourceChoice().contentEquals( "local" ) )
                    _defaultNames.vexFromLocal = true;
                else if ( doiConfig.getDefaultNamesVexSourceChoice().contentEquals( "experiment" ) )
                    _defaultNames.vexFromExperiment = true;
            }
            if ( doiConfig.getDefaultNamesV2DFileSource() != null )
                _defaultNames.v2dFileSource = doiConfig.getDefaultNamesV2DFileSource();
            if ( doiConfig.getDefaultNamesV2DViaHttpLocation() != null )
                _defaultNames.v2dViaHttpLocation = doiConfig.getDefaultNamesV2DViaHttpLocation();
            if ( doiConfig.getDefaultNamesV2DViaFtpLocation() != null )
                _defaultNames.v2dViaFtpLocation = doiConfig.getDefaultNamesV2DViaFtpLocation();
            if ( doiConfig.getDefaultNamesLocalV2DFileLocation() != null )
                _defaultNames.localV2dFileLocation = doiConfig.getDefaultNamesLocalV2DFileLocation();
            if ( doiConfig.getDefaultNamesV2DSourceChoice() != null ) {
                if ( doiConfig.getDefaultNamesV2DSourceChoice().contentEquals( "host" ) )
                    _defaultNames.v2dFromHost = true;
                else if ( doiConfig.getDefaultNamesV2DSourceChoice().contentEquals( "http" ) )
                    _defaultNames.v2dViaHttp = true;
                else if ( doiConfig.getDefaultNamesV2DSourceChoice().contentEquals( "ftp" ) )
                    _defaultNames.v2dViaFtp = true;
                else if ( doiConfig.getDefaultNamesV2DSourceChoice().contentEquals( "local" ) )
                    _defaultNames.v2dFromLocal = true;
                else
                    _defaultNames.noV2dFile = true;
            }
            _defaultNames.singleInputFile = doiConfig.isDefaultSingleInputFile();
            _defaultNames.scanBasedJobNames = doiConfig.isDefaultNamesScanBasedJobNames();
            if ( doiConfig.getDefaultNamesDirListLocation() != null )
                _defaultNames.dirListLocation = doiConfig.getDefaultNamesDirListLocation();
            _defaultNames.jobCreationSanityCheck = doiConfig.isDefaultJobCreationSanityCheck();
            _defaultNames.restrictHeadnodeProcessing = doiConfig.isDefaultNamesRestrictHeadnodeProcessing();
            _useHeadNodeCheck.setSelected( !_defaultNames.restrictHeadnodeProcessing );
            
            _uniqueDataSource.setSelected( !doiConfig.isUniqueDataSource() );
            _assignBasedOnPath.setSelected( doiConfig.isAssignBasedOnPath() );
            _shareDataSourcesBetweenJobs.setSelected( doiConfig.isShareDataSourcesBetweenJobs() );
            _shareDataSourcesAsProcessors.setSelected( doiConfig.isShareDataSourcesAsProcessors() );
            _restrictSourcesCheck.setSelected( doiConfig.isRestrictSources() );
            if ( doiConfig.getThreadsPerDataSource() != 0 )
                _threadsPerDataSource.value( doiConfig.getThreadsPerDataSource() );
            _nodesPerCheck.setSelected( !doiConfig.isNodesPerCheck() );
            _allNodesCheck.setSelected( doiConfig.isNodesPerCheck() );
            if ( doiConfig.getNodesPer() != 0 )
                _nodesPer.value( doiConfig.getNodesPer() );
            _allThreadsCheck.setSelected( doiConfig.isAllThreadsCheck() );
            if ( doiConfig.getThreadsPerNode() != 0 )
                _threadsPerNode.value( doiConfig.getThreadsPerNode() );
            if ( doiConfig.getMinThreadsPerNode() != 0 )
                _minThreadsPerNode.value( doiConfig.getMinThreadsPerNode() );
            _threadsPerCheck.setSelected( !doiConfig.isThreadsPerCheck() );
            _baselineCheck.setSelected( !doiConfig.isBaselineCheck() );
            _jobCheck.setSelected( doiConfig.isJobCheck() );
            _sequentialCheck.setSelected( doiConfig.isSequentialCheck() );
            _simultaneousCheck.setSelected( !doiConfig.isSimultaneousCheck() );
            _maxJobs.setEnabled( !doiConfig.isSimultaneousCheck() );
            if ( doiConfig.getMaxJobs() != 0 )
                _maxJobs.intValue( doiConfig.getMaxJobs() );
            if ( doiConfig.getMaxSecondsForHardware() != 0 )
                _maxSecondsForHardware.intValue( doiConfig.getMaxSecondsForHardware() );
            _useMaxSecondsForHardware.setSelected( doiConfig.isUseMaxSecondsForHardware() );
            if ( doiConfig.getMaxSecondsForProcessing() != 0 )
                _maxSecondsForProcessing.intValue( doiConfig.getMaxSecondsForProcessing() );
            _useMaxSecondsForProcessing.setSelected( doiConfig.isUseMaxSecondsForProcessing() );

            _runLogCheck.setSelected( !doiConfig.isRunLogCheck() );
            if ( doiConfig.getRunLogFile() != null )
                _runLogFile.setText( doiConfig.getRunLogFile() );
            for ( Iterator<DoiSystemConfig.PathNodePair> iter = doiConfig.getPathNodePair().iterator(); iter.hasNext(); ) {
                DoiSystemConfig.PathNodePair pathNodePair = iter.next();
                //  Create a list for the source/path pairs.  We can't add them to the
                //  display yet because other items (hardware nodes, for instance) aren't necessarily
                //  known yet.  We'll use the list when we have user actions - displaying
                //  the source/path pairs, using them to set source nodes, etc.
                if ( _sourceBasedOnPathList == null ) {
                    _sourceBasedOnPathList = new HashMap<String,String>();
                }
                _sourceBasedOnPathList.put( pathNodePair.getPath(), pathNodePair.getNode() );
            }
            for ( Iterator<DoiSystemConfig.AllowedNode> iter = doiConfig.getAllowedNode().iterator(); iter.hasNext(); ) {
                DoiSystemConfig.AllowedNode allowedNode = iter.next();
                //  Create a list of allowed nodes.
                if ( _restrictedSourceList == null ) {
                    _restrictedSourceList = new ArrayList<String>();
                }
                _restrictedSourceList.add( allowedNode.getNode() );
            }

            _defaultNames.eliminateNonrespondingProcessors = doiConfig.isDefaultNamesEliminateNonrespondingProcessors();
            _defaultNames.eliminateBusyProcessors = doiConfig.isDefaultNamesElimnateBusyProcessors();
            _defaultNames.chooseBasedOnModule = doiConfig.isDefaultNamesChooseBasedOnModule();
            if ( doiConfig.getDefaultNamesBusyPercentage() != 0 )
                _defaultNames.busyPercentage = doiConfig.getDefaultNamesBusyPercentage();
            
            _defaultNames.correlationDoPolar = !doiConfig.isCorrelationDoPolarFalse();  //  double negative because true is default
            if ( _defaultNames.correlationTInt != 0.0 )
                _defaultNames.correlationTInt = doiConfig.getCorrelationTInt();
            if ( _defaultNames.correlationSpecRes != 0.0 )
                _defaultNames.correlationSpecRes = doiConfig.getCorrelationSpecRes();
            if ( _defaultNames.correlationNChan != 0 )
                _defaultNames.correlationNChan = doiConfig.getCorrelationNChan();
            if ( _defaultNames.correlationFFTSpecRes != 0.0 )
                _defaultNames.correlationFFTSpecRes = doiConfig.getCorrelationFFTSpecRes();
            if ( _defaultNames.correlationNFFTChan != 0 )
                _defaultNames.correlationNFFTChan = doiConfig.getCorrelationNFFTChan();
            if ( _defaultNames.correlationSubintNS != 0 )
                _defaultNames.correlationSubintNS = doiConfig.getCorrelationSubintNS();
            _defaultNames.runMonitor = !doiConfig.isDefaultNamesRunMonitorOff();
            
            _defaultNames.phaseCalInt = doiConfig.getDefaultNamesPhaseCalInt();
            if ( doiConfig.getDefaultNamesToneSelection() != null )
                _defaultNames.toneSelection = doiConfig.getDefaultNamesToneSelection();
            if ( doiConfig.getDefaultNamesDataFormat() != null )
                _defaultNames.dataFormat = doiConfig.getDefaultNamesDataFormat();
            
            if ( doiConfig.getEopURL() != null )
                _eopURL.setText( doiConfig.getEopURL() );
            if ( doiConfig.getLeapSecondsURL() != null && doiConfig.getLeapSecondsURL().length() > 0 );
                _leapSecondsURL.setText( doiConfig.getLeapSecondsURL() );
            _useLeapSecondsURL.setSelected( doiConfig.isUseLeapSecondsURL() );
            if ( _useLeapSecondsURL.isSelected() )
                leapSecondChoice( _useLeapSecondsURL );
            else
                leapSecondChoice( _useLeapSecondsValue );
            if ( doiConfig.getLeapSecondsValue() != 0 )
                _leapSecondsValue.intValue( doiConfig.getLeapSecondsValue() );
            _autoUpdateEOP.setSelected( doiConfig.isAutoUpdateEOP() );
            if ( doiConfig.getAutoUpdateSeconds() != 0 )
                _autoUpdateSeconds.value( doiConfig.getAutoUpdateSeconds() );
            if ( doiConfig.getJobNetworkActivity() != null ) {
                _jobColumnSpecs.networkActivity.show = doiConfig.getJobNetworkActivity().isShow();
                _jobColumnSpecs.networkActivity.width = doiConfig.getJobNetworkActivity().getWidth();                
            }
            if ( doiConfig.getJobName() != null ) {
                _jobColumnSpecs.name.show = doiConfig.getJobName().isShow();
                _jobColumnSpecs.name.width = doiConfig.getJobName().getWidth();                
            }
            if ( doiConfig.getJobProgressBar() != null ) {
                _jobColumnSpecs.progressBar.show = doiConfig.getJobProgressBar().isShow();
                _jobColumnSpecs.progressBar.width = doiConfig.getJobProgressBar().getWidth();                
            }
            if ( doiConfig.getJobState() != null ) {
                _jobColumnSpecs.state.show = doiConfig.getJobState().isShow();
                _jobColumnSpecs.state.width = doiConfig.getJobState().getWidth();                
            }
            if ( doiConfig.getJobExperiment() != null ) {
                _jobColumnSpecs.experiment.show = doiConfig.getJobExperiment().isShow();
                _jobColumnSpecs.experiment.width = doiConfig.getJobExperiment().getWidth();                
            }
            if ( doiConfig.getJobPass() != null ) {
                _jobColumnSpecs.pass.show = doiConfig.getJobPass().isShow();
                _jobColumnSpecs.pass.width = doiConfig.getJobPass().getWidth();                
            }
            if ( doiConfig.getJobPriority() != null ) {
                _jobColumnSpecs.priority.show = doiConfig.getJobPriority().isShow();
                _jobColumnSpecs.priority.width = doiConfig.getJobPriority().getWidth();                
            }
            if ( doiConfig.getJobQueueTime() != null ) {
                _jobColumnSpecs.queueTime.show = doiConfig.getJobQueueTime().isShow();
                _jobColumnSpecs.queueTime.width = doiConfig.getJobQueueTime().getWidth();                
            }
            if ( doiConfig.getJobCorrelationStart() != null ) {
                _jobColumnSpecs.correlationStart.show = doiConfig.getJobCorrelationStart().isShow();
                _jobColumnSpecs.correlationStart.width = doiConfig.getJobCorrelationStart().getWidth();                
            }
            if ( doiConfig.getJobCorrelationEnd() != null ) {
                _jobColumnSpecs.correlationEnd.show = doiConfig.getJobCorrelationEnd().isShow();
                _jobColumnSpecs.correlationEnd.width = doiConfig.getJobCorrelationEnd().getWidth();                
            }
            if ( doiConfig.getJobCorrelationTime() != null ) {
                _jobColumnSpecs.correlationTime.show = doiConfig.getJobCorrelationTime().isShow();
                _jobColumnSpecs.correlationTime.width = doiConfig.getJobCorrelationTime().getWidth();                
            }
            if ( doiConfig.getJobJobStart() != null ) {
                _jobColumnSpecs.jobStart.show = doiConfig.getJobJobStart().isShow();
                _jobColumnSpecs.jobStart.width = doiConfig.getJobJobStart().getWidth();                
            }
            if ( doiConfig.getJobJobDuration() != null ) {
                _jobColumnSpecs.jobDuration.show = doiConfig.getJobJobDuration().isShow();
                _jobColumnSpecs.jobDuration.width = doiConfig.getJobJobDuration().getWidth();                
            }
            if ( doiConfig.getJobInputFile() != null ) {
                _jobColumnSpecs.inputFile.show = doiConfig.getJobInputFile().isShow();
                _jobColumnSpecs.inputFile.width = doiConfig.getJobInputFile().getWidth();                
            }
            if ( doiConfig.getJobOutputFile() != null ) {
                _jobColumnSpecs.outputFile.show = doiConfig.getJobOutputFile().isShow();
                _jobColumnSpecs.outputFile.width = doiConfig.getJobOutputFile().getWidth();                
            }
            if ( doiConfig.getJobOutputSize() != null ) {
                _jobColumnSpecs.outputSize.show = doiConfig.getJobOutputSize().isShow();
                _jobColumnSpecs.outputSize.width = doiConfig.getJobOutputSize().getWidth();                
            }
            if ( doiConfig.getJobDifxVersion() != null ) {
                _jobColumnSpecs.difxVersion.show = doiConfig.getJobDifxVersion().isShow();
                _jobColumnSpecs.difxVersion.width = doiConfig.getJobDifxVersion().getWidth();                
            }
            if ( doiConfig.getJobSpeedUpFactor() != null ) {
                _jobColumnSpecs.speedUpFactor.show = doiConfig.getJobSpeedUpFactor().isShow();
                _jobColumnSpecs.speedUpFactor.width = doiConfig.getJobSpeedUpFactor().getWidth();                
            }
            if ( doiConfig.getJobNumAntennas() != null ) {
                _jobColumnSpecs.numAntennas.show = doiConfig.getJobNumAntennas().isShow();
                _jobColumnSpecs.numAntennas.width = doiConfig.getJobNumAntennas().getWidth();                
            }
            if ( doiConfig.getJobNumForeignAntennas() != null ) {
                _jobColumnSpecs.numForeignAntennas.show = doiConfig.getJobNumForeignAntennas().isShow();
                _jobColumnSpecs.numForeignAntennas.width = doiConfig.getJobNumForeignAntennas().getWidth();                
            }
            if ( doiConfig.getJobDutyCycle() != null ) {
                _jobColumnSpecs.dutyCycle.show = doiConfig.getJobDutyCycle().isShow();
                _jobColumnSpecs.dutyCycle.width = doiConfig.getJobDutyCycle().getWidth();                
            }
            if ( doiConfig.getJobStatus() != null ) {
                _jobColumnSpecs.status.show = doiConfig.getJobStatus().isShow();
                _jobColumnSpecs.status.width = doiConfig.getJobStatus().getWidth();                
            }
            if ( doiConfig.getJobActive() != null ) {
                _jobColumnSpecs.active.show = doiConfig.getJobActive().isShow();
                _jobColumnSpecs.active.width = doiConfig.getJobActive().getWidth();                
            }
            if ( doiConfig.getJobStatusId() != null ) {
                _jobColumnSpecs.statusId.show = doiConfig.getJobStatusId().isShow();
                _jobColumnSpecs.statusId.width = doiConfig.getJobStatusId().getWidth();                
            }
            if ( doiConfig.getJobWeights() != null ) {
                _jobColumnSpecs.weights.show = doiConfig.getJobWeights().isShow();
                _jobColumnSpecs.weights.width = doiConfig.getJobWeights().getWidth();                
            }
            if ( doiConfig.getJobWeightsAsPlots() != null ) {
                _jobColumnSpecs.weightsAsPlots.show = doiConfig.getJobWeightsAsPlots().isShow();
                _jobColumnSpecs.weightsAsPlots.width = doiConfig.getJobWeightsAsPlots().getWidth();                
            }
            if ( doiConfig.getJobWeightsAsNumbers() != null ) {
                _jobColumnSpecs.weightsAsNumbers.show = doiConfig.getJobWeightsAsNumbers().isShow();
                _jobColumnSpecs.weightsAsNumbers.width = doiConfig.getJobWeightsAsNumbers().getWidth();                
            }
            if ( doiConfig.getHardwareIgnored() != null ) {
                _hardwareColumnSpecs.Ignored.show = doiConfig.getHardwareIgnored().isShow();
                _hardwareColumnSpecs.Ignored.width = doiConfig.getHardwareIgnored().getWidth();
            }
            if ( doiConfig.getHardwareBroadcastMonitor() != null ) {
                _hardwareColumnSpecs.broadcastMonitor.show = doiConfig.getHardwareBroadcastMonitor().isShow();
                _hardwareColumnSpecs.broadcastMonitor.width = doiConfig.getHardwareBroadcastMonitor().getWidth();
            }
            if ( doiConfig.getHardwareNumCPUs() != null ) {
                _hardwareColumnSpecs.NumCPUs.show = doiConfig.getHardwareNumCPUs().isShow();
                _hardwareColumnSpecs.NumCPUs.width = doiConfig.getHardwareNumCPUs().getWidth();
            }
            if ( doiConfig.getHardwareNumCores() != null ) {
                _hardwareColumnSpecs.NumCores.show = doiConfig.getHardwareNumCores().isShow();
                _hardwareColumnSpecs.NumCores.width = doiConfig.getHardwareNumCores().getWidth();
            }
            if ( doiConfig.getHardwareBogusGHz() != null ) {
                _hardwareColumnSpecs.BogusGHz.show = doiConfig.getHardwareBogusGHz().isShow();
                _hardwareColumnSpecs.BogusGHz.width = doiConfig.getHardwareBogusGHz().getWidth();
            }
            if ( doiConfig.getHardwareType() != null ) {
                _hardwareColumnSpecs.Type.show = doiConfig.getHardwareType().isShow();
                _hardwareColumnSpecs.Type.width = doiConfig.getHardwareType().getWidth();
            }
            if ( doiConfig.getHardwareTypeString() != null ) {
                _hardwareColumnSpecs.TypeString.show = doiConfig.getHardwareTypeString().isShow();
                _hardwareColumnSpecs.TypeString.width = doiConfig.getHardwareTypeString().getWidth();
            }
            if ( doiConfig.getHardwareState() != null ) {
                _hardwareColumnSpecs.State.show = doiConfig.getHardwareState().isShow();
                _hardwareColumnSpecs.State.width = doiConfig.getHardwareState().getWidth();
            }
            if ( doiConfig.getHardwareEnabled() != null ) {
                _hardwareColumnSpecs.Enabled.show = doiConfig.getHardwareEnabled().isShow();
                _hardwareColumnSpecs.Enabled.width = doiConfig.getHardwareEnabled().getWidth();
            }
            if ( doiConfig.getHardwareCpuLoad() != null ) {
                _hardwareColumnSpecs.CpuLoad.show = doiConfig.getHardwareCpuLoad().isShow();
                _hardwareColumnSpecs.CpuLoad.width = doiConfig.getHardwareCpuLoad().getWidth();
            }
            if ( doiConfig.getHardwareCpuLoadPlot() != null ) {
                _hardwareColumnSpecs.CpuLoadPlot.show = doiConfig.getHardwareCpuLoadPlot().isShow();
                _hardwareColumnSpecs.CpuLoadPlot.width = doiConfig.getHardwareCpuLoadPlot().getWidth();
            }
            if ( doiConfig.getHardwareUsedMem() != null ) {
                _hardwareColumnSpecs.UsedMem.show = doiConfig.getHardwareUsedMem().isShow();
                _hardwareColumnSpecs.UsedMem.width = doiConfig.getHardwareUsedMem().getWidth();
            }
            if ( doiConfig.getHardwareTotalMem() != null ) {
                _hardwareColumnSpecs.TotalMem.show = doiConfig.getHardwareTotalMem().isShow();
                _hardwareColumnSpecs.TotalMem.width = doiConfig.getHardwareTotalMem().getWidth();
            }
            if ( doiConfig.getHardwareMemLoad() != null ) {
                _hardwareColumnSpecs.MemLoad.show = doiConfig.getHardwareMemLoad().isShow();
                _hardwareColumnSpecs.MemLoad.width = doiConfig.getHardwareMemLoad().getWidth();
            }
            if ( doiConfig.getHardwareMemLoadPlot() != null ) {
                _hardwareColumnSpecs.MemLoadPlot.show = doiConfig.getHardwareMemLoadPlot().isShow();
                _hardwareColumnSpecs.MemLoadPlot.width = doiConfig.getHardwareMemLoadPlot().getWidth();
            }
            if ( doiConfig.getHardwareNetRxRate() != null ) {
                _hardwareColumnSpecs.NetRxRate.show = doiConfig.getHardwareNetRxRate().isShow();
                _hardwareColumnSpecs.NetRxRate.width = doiConfig.getHardwareNetRxRate().getWidth();
            }
            if ( doiConfig.getHardwareNetTxRate() != null ) {
                _hardwareColumnSpecs.NetTxRate.show = doiConfig.getHardwareNetTxRate().isShow();
                _hardwareColumnSpecs.NetTxRate.width = doiConfig.getHardwareNetTxRate().getWidth();
            }
            if ( doiConfig.getHardwareStateChanged() != null ) {
                _hardwareColumnSpecs.StateChanged.show = doiConfig.getHardwareStateChanged().isShow();
                _hardwareColumnSpecs.StateChanged.width = doiConfig.getHardwareStateChanged().getWidth();
            }
            if ( doiConfig.getHardwareBankAVSN() != null ) {
                _hardwareColumnSpecs.BankAVSN.show = doiConfig.getHardwareBankAVSN().isShow();
                _hardwareColumnSpecs.BankAVSN.width = doiConfig.getHardwareBankAVSN().getWidth();
            }
            if ( doiConfig.getHardwareBankBVSN() != null ) {
                _hardwareColumnSpecs.BankBVSN.show = doiConfig.getHardwareBankBVSN().isShow();
                _hardwareColumnSpecs.BankBVSN.width = doiConfig.getHardwareBankBVSN().getWidth();
            }
            if ( doiConfig.getHardwareStatusWord() != null ) {
                _hardwareColumnSpecs.StatusWord.show = doiConfig.getHardwareStatusWord().isShow();
                _hardwareColumnSpecs.StatusWord.width = doiConfig.getHardwareStatusWord().getWidth();
            }
            if ( doiConfig.getHardwareActiveBank() != null ) {
                _hardwareColumnSpecs.ActiveBank.show = doiConfig.getHardwareActiveBank().isShow();
                _hardwareColumnSpecs.ActiveBank.width = doiConfig.getHardwareActiveBank().getWidth();
            }
            if ( doiConfig.getHardwareScanNumber() != null ) {
                _hardwareColumnSpecs.ScanNumber.show = doiConfig.getHardwareScanNumber().isShow();
                _hardwareColumnSpecs.ScanNumber.width = doiConfig.getHardwareScanNumber().getWidth();
            }
            if ( doiConfig.getHardwareScanName() != null ) {
                _hardwareColumnSpecs.ScanName.show = doiConfig.getHardwareScanName().isShow();
                _hardwareColumnSpecs.ScanName.width = doiConfig.getHardwareScanName().getWidth();
            }
            if ( doiConfig.getHardwarePosition() != null ) {
                _hardwareColumnSpecs.Position.show = doiConfig.getHardwarePosition().isShow();
                _hardwareColumnSpecs.Position.width = doiConfig.getHardwarePosition().getWidth();
            }
            if ( doiConfig.getHardwarePlayRate() != null ) {
                _hardwareColumnSpecs.PlayRate.show = doiConfig.getHardwarePlayRate().isShow();
                _hardwareColumnSpecs.PlayRate.width = doiConfig.getHardwarePlayRate().getWidth();
            }
            if ( doiConfig.getHardwareDataMJD() != null ) {
                _hardwareColumnSpecs.DataMJD.show = doiConfig.getHardwareDataMJD().isShow();
                _hardwareColumnSpecs.DataMJD.width = doiConfig.getHardwareDataMJD().getWidth();
            }
            if ( doiConfig.getHardwareCurrentJob() != null ) {
                _hardwareColumnSpecs.CurrentJob.show = doiConfig.getHardwareCurrentJob().isShow();
                _hardwareColumnSpecs.CurrentJob.width = doiConfig.getHardwareCurrentJob().getWidth();
            }

            if ( doiConfig.getJobLocationDefaultsFileFilter() != null )
                _jobLocationDefaults.fileFilter = doiConfig.getJobLocationDefaultsFileFilter();
            _jobLocationDefaults.experimentBasedOnPath = !doiConfig.isJobLocationDefaultsExperimentBasedOnPathFalse();
            _jobLocationDefaults.experimentNamed = doiConfig.isJobLocationDefaultsExperimentNamed();
            if ( doiConfig.getJobLocationDefaultsExperimentName() != null )
                _jobLocationDefaults.experimentName = doiConfig.getJobLocationDefaultsExperimentName();
            _jobLocationDefaults.passBasedOnPath = !doiConfig.isJobLocationDefaultsPassBasedOnPathFalse();
            _jobLocationDefaults.passNamed = doiConfig.isJobLocationDefaultsExperimentNamed();
            _jobLocationDefaults.noPass = doiConfig.isJobLocationDefaultsNoPass();
            if ( doiConfig.getJobLocationDefaultsPassName() != null )
                _jobLocationDefaults.passName = doiConfig.getJobLocationDefaultsPassName();
            _jobLocationDefaults.autoUpdate = doiConfig.isJobLocationDefaultsAutoUpdate();
            
            _invisibleProcessors = doiConfig.getInvisibleProcessors();
            _invisibleProcessorCores = doiConfig.getInvisibleProcessorCores();
            _invisibleMark5s = doiConfig.getInvisibleMark5S();
            
            if ( _hardwareMonitor != null )
                _hardwareMonitor.addInvisibleNodesFromSettings();
            
            updateEOPNow();
            changeDifxControlConnection();
            generateDatabaseChangeEvent();
            return true;
        } catch (javax.xml.bind.JAXBException ex) {
            // XXXTODO Handle exception
            java.util.logging.Logger.getLogger("global").log( java.util.logging.Level.SEVERE, null, ex );
            return false;
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger("global").log( java.util.logging.Level.SEVERE, null, e );
            return false;
        }
    }
    
    public String invisibleProcessors() { return _invisibleProcessors; }
    public String invisibleProcessorCores() { return _invisibleProcessorCores; }
    public String invisibleMark5s() { return _invisibleMark5s; }

    /*
     * Save all current settings to the given filename.
     */
    public boolean saveSettingsToFile( String filename ) {
        System.out.println( "write to " + filename );
        ObjectFactory factory = new ObjectFactory();
        DoiSystemConfig doiConfig = factory.createDoiSystemConfig();
        doiConfig.setDifxHome( this.home() );
        //doiConfig.setResourcesFile( this.resourcesFile() );
        doiConfig.setDbHost( this.dbHost() );
        doiConfig.setDbSID( this.dbUser() );
        doiConfig.setDbPassword( this.dbPwd() );
        doiConfig.setDbDriver( this.dbDriver() );
        doiConfig.setDbPort( this.dbPort() );
        doiConfig.setDbUrl( this.dbURL() );
        doiConfig.setIpAddress( this.ipAddress() );
        doiConfig.setPort( this.port() );
        doiConfig.setBufferSize( this.bufferSize() );
        doiConfig.setSuppressUnknownMessageWarnings( _suppressWarningsCheck.isSelected() );
        doiConfig.setDontIdentifyMark5SByPattern( !_identifyMark5sCheck.isSelected() );
        doiConfig.setRequestSpecificMessages( _requestSpecificMessages.isSelected() );
        if ( _difxMessageListDisplay != null ) {
            for ( Iterator<String> iter = _difxMessageListDisplay.selectedMessages().iterator(); iter.hasNext(); ) {
                DoiSystemConfig.SelectedMessage selectedMessage = factory.createDoiSystemConfigSelectedMessage();
                selectedMessage.setType( iter.next() );
                doiConfig.getSelectedMessage().add( selectedMessage );
            }
        }
        doiConfig.setMark5Pattern( _mark5Pattern.getText() );
        doiConfig.setLoggingEnabled( this.loggingEnabled() );
        doiConfig.setStatusValidDuration( this.statusValidDuration() );
        doiConfig.setChannelAllData( _channelAllData.isSelected() );
        
        doiConfig.setJaxbPackage( this.jaxbPackage() );
        doiConfig.setTimeout( this.timeout() );
        doiConfig.setDifxTCPConnection( _difxTCPCheck.isSelected() );
        doiConfig.setDifxRelayBroadcastViaTCPConnection( _useTCPRelayCheck.isSelected() );
        doiConfig.setDifxControlAddress( this.difxControlAddress() );
        doiConfig.setDifxControlPort( this.difxControlPort() );
        doiConfig.setDifxTransferPort( this.difxTransferPort() );
        doiConfig.setDifxTransferPortLimit( _maxTransferPorts.intValue() );
        doiConfig.setDifxMonitorHost( this.difxMonitorHost() );
        doiConfig.setDifxMonitorPort( this.difxMonitorPort() );
        //  Only save the DiFX version if a sensible setting exists.
        if ( this.difxVersion() != null && this.difxVersion().length() > 0 )
            doiConfig.setDifxVersion( this.difxVersion() );
        doiConfig.setDontUseDefaultStartScript( !_useDefaultStartScript.isSelected() );
        //  Likewise for the start script.
        if ( _difxStartScript.getText() != null && _difxStartScript.getText().length() > 0 )
        doiConfig.setDifxStartScript( _difxStartScript.getText() );
        doiConfig.setDifxBase( this.difxBase() );
        doiConfig.setDbUseDataBase( this.useDatabase() );
        doiConfig.setDbVersion( this.dbVersion() );
        doiConfig.setDbName( this.dbName() );
        doiConfig.setDbMS( this.dbMS() );
        doiConfig.setInactivityWarning( this.inactivityWarning() );
        doiConfig.setInactivityError( this.inactivityError() );
        doiConfig.setReportLoc( _reportLoc );
        doiConfig.setGuiDocPath( _guiDocPath.getText() );
        doiConfig.setDifxUsersGroupURL( _difxUsersGroupURL.getText() );
        doiConfig.setDifxWikiURL( _difxWikiURL.getText() );
        doiConfig.setDifxSVN( _difxSVN.getText() );
        doiConfig.setDbAutoUpdate( this.dbAutoUpdate() );
        doiConfig.setDbAutoUpdateInterval( _dbAutoUpdateInterval.intValue() );
        doiConfig.setQueueShowCompleted( _queueBrowserSettings.showCompleted );
        doiConfig.setQueueShowIncomplete( _queueBrowserSettings.showIncomplete );
        doiConfig.setQueueShowSelected( _queueBrowserSettings.showSelected );
        doiConfig.setQueueShowUnselected( _queueBrowserSettings.showUnselected );
        doiConfig.setQueueShowExperimentScheduled( _queueBrowserSettings.showExperimentScheduled );
        doiConfig.setQueueShowPassScheduled( _queueBrowserSettings.showPassScheduled );
        
        doiConfig.setWorkingDirectory( _workingDirectory.getText() );
        doiConfig.setStagingArea( _stagingArea.getText() );
        doiConfig.setUseStagingArea( _useStagingArea.isSelected() );
        doiConfig.setDifxHeadNode( this.headNode() );
        
        doiConfig.setWindowConfigMainX( _windowConfiguration.mainX );
        doiConfig.setWindowConfigMainY( _windowConfiguration.mainY );
        doiConfig.setWindowConfigMainW( _windowConfiguration.mainW );
        doiConfig.setWindowConfigMainH( _windowConfiguration.mainH );
        doiConfig.setWindowConfigTitle( _title.getText() );
        doiConfig.setWindowConfigVerticalPanels( _windowConfiguration.verticalPanels );
        doiConfig.setWindowConfigMainDividerLocation( _windowConfiguration.mainDividerLocation );
        doiConfig.setWindowConfigTopDividerLocation( _windowConfiguration.topDividerLocation );
        doiConfig.setWindowConfigQueueBrowserTearOff( _windowConfiguration.queueBrowserTearOff );
        doiConfig.setWindowConfigQueueBrowserX( _windowConfiguration.queueBrowserX );
        doiConfig.setWindowConfigQueueBrowserY( _windowConfiguration.queueBrowserY );
        doiConfig.setWindowConfigQueueBrowserW( _windowConfiguration.queueBrowserW );
        doiConfig.setWindowConfigQueueBrowserH( _windowConfiguration.queueBrowserH );
        doiConfig.setWindowConfigHardwareMonitorTearOff( _windowConfiguration.hardwareMonitorTearOff );
        doiConfig.setWindowConfigHardwareMonitorX( _windowConfiguration.hardwareMonitorX );
        doiConfig.setWindowConfigHardwareMonitorY( _windowConfiguration.hardwareMonitorY );
        doiConfig.setWindowConfigHardwareMonitorW( _windowConfiguration.hardwareMonitorW );
        doiConfig.setWindowConfigHardwareMonitorH( _windowConfiguration.hardwareMonitorH );
        doiConfig.setWindowConfigExperimentEditorW( _windowConfiguration.experimentEditorW );
        doiConfig.setWindowConfigExperimentEditorH( _windowConfiguration.experimentEditorH );
        doiConfig.setWindowConfigSettingsWindowW( this.getWidth() );
        doiConfig.setWindowConfigSettingsWindowH( this.getHeight() );
        doiConfig.setWindowConfigJobEditorMonitorWindowW( _windowConfiguration.jobEditorMonitorWindowW );
        doiConfig.setWindowConfigJobEditorMonitorWindowH( _windowConfiguration.jobEditorMonitorWindowH );
        doiConfig.setWindowConfigSmartDisplayW( _windowConfiguration.smartDisplayW );
        doiConfig.setWindowConfigSmartDisplayH( _windowConfiguration.smartDisplayH );
        doiConfig.setWindowConfigEnvironmentVariableDisplayW( _windowConfiguration.environmentVariableDisplayW );
        doiConfig.setWindowConfigEnvironmentVariableDisplayH( _windowConfiguration.environmentVariableDisplayH );
        doiConfig.setWindowConfigSourceBasedOnPathDisplayW( _windowConfiguration.sourceBasedOnPathDisplayW );
        doiConfig.setWindowConfigSourceBasedOnPathDisplayH( _windowConfiguration.sourceBasedOnPathDisplayH );
        doiConfig.setWindowConfigRestrictedSourceListDisplayW( _windowConfiguration.restrictedSourceListDisplayW );
        doiConfig.setWindowConfigRestrictedSourceListDisplayH( _windowConfiguration.restrictedSourceListDisplayH );
        doiConfig.setWindowConfigSelectedMessagesDisplayW( _windowConfiguration.requestedMessageListDisplayW );
        doiConfig.setWindowConfigSelectedMessagesDisplayH( _windowConfiguration.requestedMessageListDisplayH );
        doiConfig.setWindowConfigDirectoryDisplayW( _windowConfiguration.directoryDisplayW );
        doiConfig.setWindowConfigDirectoryDisplayH( _windowConfiguration.directoryDisplayH );
        doiConfig.setWindowConfigMonitorDisplayW( _windowConfiguration.monitorDisplayW );
        doiConfig.setWindowConfigMonitorDisplayH( _windowConfiguration.monitorDisplayH );
        doiConfig.setWindowConfigDiskSearchRulesDisplayW( _windowConfiguration.diskSearchRulesDisplayW );
        doiConfig.setWindowConfigDiskSearchRulesDisplayH( _windowConfiguration.diskSearchRulesDisplayH );
        doiConfig.setWindowConfigDifxMessageWindowW( _windowConfiguration.difxMessageWindowW );
        doiConfig.setWindowConfigDifxMessageWindowH( _windowConfiguration.difxMessageWindowH );
        doiConfig.setWindowConfigDifxMessageWindowTopFraction( _windowConfiguration.difxMessageWindowTopFraction );
        doiConfig.setWindowConfigDifxMessageWindowBottomFraction( _windowConfiguration.difxMessageWindowBottomFraction );
        doiConfig.setWindowConfigDifxMessageWindowMessageLimit( _windowConfiguration.difxMessageWindowMessageLimit );
        
        doiConfig.setDefaultNamesVexFileSource( _defaultNames.vexFileSource );
        doiConfig.setDefaultNamesViaHttpLocation( _defaultNames.viaHttpLocation );
        doiConfig.setDefaultNamesViaFtpLocation( _defaultNames.viaFtpLocation );
        doiConfig.setDefaultNamesLocalFileLocation( _defaultNames.localFileLocation );
        if ( _defaultNames.vexFromHost == true )
            doiConfig.setDefaultNamesVexSourceChoice( "host" );
        else if ( _defaultNames.vexViaHttp == true )
            doiConfig.setDefaultNamesVexSourceChoice( "http" );
        else if ( _defaultNames.vexViaFtp == true )
            doiConfig.setDefaultNamesVexSourceChoice( "ftp" );
        else if ( _defaultNames.vexFromLocal == true )
            doiConfig.setDefaultNamesVexSourceChoice( "local" );
        else if ( _defaultNames.vexFromExperiment == true )
            doiConfig.setDefaultNamesVexSourceChoice( "experiment" );
        doiConfig.setDefaultNamesV2DFileSource( _defaultNames.v2dFileSource );
        doiConfig.setDefaultNamesV2DViaHttpLocation( _defaultNames.v2dViaHttpLocation );
        doiConfig.setDefaultNamesV2DViaFtpLocation( _defaultNames.v2dViaFtpLocation );
        doiConfig.setDefaultNamesLocalV2DFileLocation( _defaultNames.localV2dFileLocation );
        if ( _defaultNames.v2dFromHost == true )
            doiConfig.setDefaultNamesV2DSourceChoice( "host" );
        else if ( _defaultNames.v2dViaHttp == true )
            doiConfig.setDefaultNamesV2DSourceChoice( "http" );
        else if ( _defaultNames.v2dViaFtp == true )
            doiConfig.setDefaultNamesV2DSourceChoice( "ftp" );
        else if ( _defaultNames.v2dFromLocal == true )
            doiConfig.setDefaultNamesV2DSourceChoice( "local" );
        else
            doiConfig.setDefaultNamesV2DSourceChoice( "none" );
        doiConfig.setDefaultSingleInputFile( _defaultNames.singleInputFile );
        doiConfig.setDefaultNamesScanBasedJobNames( _defaultNames.scanBasedJobNames );
        doiConfig.setDefaultNamesDirListLocation( _defaultNames.dirListLocation );
        doiConfig.setDefaultJobCreationSanityCheck( _defaultNames.jobCreationSanityCheck );
        doiConfig.setDefaultNamesRestrictHeadnodeProcessing( _defaultNames.restrictHeadnodeProcessing );
        doiConfig.setDefaultNamesEliminateNonrespondingProcessors( _defaultNames.eliminateNonrespondingProcessors );
        doiConfig.setDefaultNamesElimnateBusyProcessors( _defaultNames.eliminateBusyProcessors );
        doiConfig.setDefaultNamesChooseBasedOnModule( _defaultNames.chooseBasedOnModule );
        doiConfig.setDefaultNamesBusyPercentage( _defaultNames.busyPercentage );
        
        doiConfig.setCorrelationDoPolarFalse( !_defaultNames.correlationDoPolar );
        doiConfig.setCorrelationTInt( _defaultNames.correlationTInt );
        doiConfig.setCorrelationNChan( _defaultNames.correlationNChan );
        doiConfig.setCorrelationSpecRes( _defaultNames.correlationSpecRes );
        doiConfig.setCorrelationFFTSpecRes( _defaultNames.correlationFFTSpecRes );
        doiConfig.setCorrelationNFFTChan( _defaultNames.correlationNFFTChan );
        doiConfig.setCorrelationSubintNS( _defaultNames.correlationSubintNS );
        
        doiConfig.setDefaultNamesPhaseCalInt( _defaultNames.phaseCalInt );
        doiConfig.setDefaultNamesToneSelection( _defaultNames.toneSelection );
        doiConfig.setDefaultNamesDataFormat( _defaultNames.dataFormat );
        
        doiConfig.setDefaultNamesRunMonitorOff( !_defaultNames.runMonitor );
            
        doiConfig.setEopURL( _eopURL.getText() );
        doiConfig.setLeapSecondsURL( _leapSecondsURL.getText() );
        doiConfig.setUseLeapSecondsURL( _useLeapSecondsURL.isSelected() );
        doiConfig.setLeapSecondsValue( _leapSecondsValue.intValue() );
        doiConfig.setAutoUpdateEOP( _autoUpdateEOP.isSelected() );
        doiConfig.setAutoUpdateSeconds( _autoUpdateSeconds.intValue() );
        
        doiConfig.setJobNetworkActivity( factory.createColumnSpec() );
        doiConfig.getJobNetworkActivity().setShow( _jobColumnSpecs.networkActivity.show );
        doiConfig.getJobNetworkActivity().setWidth( _jobColumnSpecs.networkActivity.width );                
        doiConfig.setJobName( factory.createColumnSpec() );
        doiConfig.getJobName().setShow( _jobColumnSpecs.name.show );
        doiConfig.getJobName().setWidth( _jobColumnSpecs.name.width );                
        doiConfig.setJobProgressBar( factory.createColumnSpec() );
        doiConfig.getJobProgressBar().setShow( _jobColumnSpecs.progressBar.show );
        doiConfig.getJobProgressBar().setWidth( _jobColumnSpecs.progressBar.width );                
        doiConfig.setJobState( factory.createColumnSpec() );
        doiConfig.getJobState().setShow( _jobColumnSpecs.state.show );
        doiConfig.getJobState().setWidth( _jobColumnSpecs.state.width );                
        doiConfig.setJobExperiment( factory.createColumnSpec() );
        doiConfig.getJobExperiment().setShow( _jobColumnSpecs.experiment.show );
        doiConfig.getJobExperiment().setWidth( _jobColumnSpecs.experiment.width );                
        doiConfig.setJobPass( factory.createColumnSpec() );
        doiConfig.getJobPass().setShow( _jobColumnSpecs.pass.show );
        doiConfig.getJobPass().setWidth( _jobColumnSpecs.pass.width );                
        doiConfig.setJobPriority( factory.createColumnSpec() );
        doiConfig.getJobPriority().setShow( _jobColumnSpecs.priority.show );
        doiConfig.getJobPriority().setWidth( _jobColumnSpecs.priority.width );                
        doiConfig.setJobQueueTime( factory.createColumnSpec() );
        doiConfig.getJobQueueTime().setShow( _jobColumnSpecs.queueTime.show );
        doiConfig.getJobQueueTime().setWidth( _jobColumnSpecs.queueTime.width );                
        doiConfig.setJobCorrelationStart( factory.createColumnSpec() );
        doiConfig.getJobCorrelationStart().setShow( _jobColumnSpecs.correlationStart.show );
        doiConfig.getJobCorrelationStart().setWidth( _jobColumnSpecs.correlationStart.width );                
        doiConfig.setJobCorrelationEnd( factory.createColumnSpec() );
        doiConfig.getJobCorrelationEnd().setShow( _jobColumnSpecs.correlationEnd.show );
        doiConfig.getJobCorrelationEnd().setWidth( _jobColumnSpecs.correlationEnd.width );                
        doiConfig.setJobCorrelationTime( factory.createColumnSpec() );
        doiConfig.getJobCorrelationTime().setShow( _jobColumnSpecs.correlationTime.show );
        doiConfig.getJobCorrelationTime().setWidth( _jobColumnSpecs.correlationTime.width );                
        doiConfig.setJobJobStart( factory.createColumnSpec() );
        doiConfig.getJobJobStart().setShow( _jobColumnSpecs.jobStart.show );
        doiConfig.getJobJobStart().setWidth( _jobColumnSpecs.jobStart.width );                
        doiConfig.setJobJobDuration( factory.createColumnSpec() );
        doiConfig.getJobJobDuration().setShow( _jobColumnSpecs.jobDuration.show );
        doiConfig.getJobJobDuration().setWidth( _jobColumnSpecs.jobDuration.width );                
        doiConfig.setJobInputFile( factory.createColumnSpec() );
        doiConfig.getJobInputFile().setShow( _jobColumnSpecs.inputFile.show );
        doiConfig.getJobInputFile().setWidth( _jobColumnSpecs.inputFile.width );                
        doiConfig.setJobOutputFile( factory.createColumnSpec() );
        doiConfig.getJobOutputFile().setShow( _jobColumnSpecs.outputFile.show );
        doiConfig.getJobOutputFile().setWidth( _jobColumnSpecs.outputFile.width );                
        doiConfig.setJobOutputSize( factory.createColumnSpec() );
        doiConfig.getJobOutputSize().setShow( _jobColumnSpecs.outputSize.show );
        doiConfig.getJobOutputSize().setWidth( _jobColumnSpecs.outputSize.width );                
        doiConfig.setJobDifxVersion( factory.createColumnSpec() );
        doiConfig.getJobDifxVersion().setShow( _jobColumnSpecs.difxVersion.show );
        doiConfig.getJobDifxVersion().setWidth( _jobColumnSpecs.difxVersion.width );                
        doiConfig.setJobSpeedUpFactor( factory.createColumnSpec() );
        doiConfig.getJobSpeedUpFactor().setShow( _jobColumnSpecs.speedUpFactor.show );
        doiConfig.getJobSpeedUpFactor().setWidth( _jobColumnSpecs.speedUpFactor.width );                
        doiConfig.setJobNumAntennas( factory.createColumnSpec() );
        doiConfig.getJobNumAntennas().setShow( _jobColumnSpecs.numAntennas.show );
        doiConfig.getJobNumAntennas().setWidth( _jobColumnSpecs.numAntennas.width );                
        doiConfig.setJobNumForeignAntennas( factory.createColumnSpec() );
        doiConfig.getJobNumForeignAntennas().setShow( _jobColumnSpecs.numForeignAntennas.show );
        doiConfig.getJobNumForeignAntennas().setWidth( _jobColumnSpecs.numForeignAntennas.width );                
        doiConfig.setJobDutyCycle( factory.createColumnSpec() );
        doiConfig.getJobDutyCycle().setShow( _jobColumnSpecs.dutyCycle.show );
        doiConfig.getJobDutyCycle().setWidth( _jobColumnSpecs.dutyCycle.width );                
        doiConfig.setJobStatus( factory.createColumnSpec() );
        doiConfig.getJobStatus().setShow( _jobColumnSpecs.status.show );
        doiConfig.getJobStatus().setWidth( _jobColumnSpecs.status.width );                
        doiConfig.setJobActive( factory.createColumnSpec() );
        doiConfig.getJobActive().setShow( _jobColumnSpecs.active.show );
        doiConfig.getJobActive().setWidth( _jobColumnSpecs.active.width );                
        doiConfig.setJobStatusId( factory.createColumnSpec() );
        doiConfig.getJobStatusId().setShow( _jobColumnSpecs.statusId.show );
        doiConfig.getJobStatusId().setWidth( _jobColumnSpecs.statusId.width );                
        doiConfig.setJobWeights( factory.createColumnSpec() );
        doiConfig.getJobWeights().setShow( _jobColumnSpecs.weights.show );
        doiConfig.getJobWeights().setWidth( _jobColumnSpecs.weights.width );                
        doiConfig.setJobWeightsAsPlots( factory.createColumnSpec() );
        doiConfig.getJobWeightsAsPlots().setShow( _jobColumnSpecs.weightsAsPlots.show );
        doiConfig.getJobWeightsAsPlots().setWidth( _jobColumnSpecs.weightsAsPlots.width );                
        doiConfig.setJobWeightsAsNumbers( factory.createColumnSpec() );
        doiConfig.getJobWeightsAsNumbers().setShow( _jobColumnSpecs.weightsAsNumbers.show );
        doiConfig.getJobWeightsAsNumbers().setWidth( _jobColumnSpecs.weightsAsNumbers.width );                

        doiConfig.setHardwareIgnored( factory.createColumnSpec() );
        doiConfig.getHardwareIgnored().setShow( _hardwareColumnSpecs.Ignored.show );
        doiConfig.getHardwareIgnored().setWidth( _hardwareColumnSpecs.Ignored.width );
        doiConfig.setHardwareBroadcastMonitor( factory.createColumnSpec() );
        doiConfig.getHardwareBroadcastMonitor().setShow( _hardwareColumnSpecs.broadcastMonitor.show );
        doiConfig.getHardwareBroadcastMonitor().setWidth( _hardwareColumnSpecs.broadcastMonitor.width );
        doiConfig.setHardwareNumCPUs( factory.createColumnSpec() );
        doiConfig.getHardwareNumCPUs().setShow( _hardwareColumnSpecs.NumCPUs.show );
        doiConfig.getHardwareNumCPUs().setWidth( _hardwareColumnSpecs.NumCPUs.width );
        doiConfig.setHardwareNumCores( factory.createColumnSpec() );
        doiConfig.getHardwareNumCores().setShow( _hardwareColumnSpecs.NumCores.show );
        doiConfig.getHardwareNumCores().setWidth( _hardwareColumnSpecs.NumCores.width );
        doiConfig.setHardwareBogusGHz( factory.createColumnSpec() );
        doiConfig.getHardwareBogusGHz().setShow( _hardwareColumnSpecs.BogusGHz.show );
        doiConfig.getHardwareBogusGHz().setWidth( _hardwareColumnSpecs.BogusGHz.width );
        doiConfig.setHardwareType( factory.createColumnSpec() );
        doiConfig.getHardwareType().setShow( _hardwareColumnSpecs.Type.show );
        doiConfig.getHardwareType().setWidth( _hardwareColumnSpecs.Type.width );
        doiConfig.setHardwareTypeString( factory.createColumnSpec() );
        doiConfig.getHardwareTypeString().setShow( _hardwareColumnSpecs.TypeString.show );
        doiConfig.getHardwareTypeString().setWidth( _hardwareColumnSpecs.TypeString.width );
        doiConfig.setHardwareState( factory.createColumnSpec() );
        doiConfig.getHardwareState().setShow( _hardwareColumnSpecs.State.show );
        doiConfig.getHardwareState().setWidth( _hardwareColumnSpecs.State.width );
        doiConfig.setHardwareEnabled( factory.createColumnSpec() );
        doiConfig.getHardwareEnabled().setShow( _hardwareColumnSpecs.Enabled.show );
        doiConfig.getHardwareEnabled().setWidth( _hardwareColumnSpecs.Enabled.width );
        doiConfig.setHardwareCpuLoad( factory.createColumnSpec() );
        doiConfig.getHardwareCpuLoad().setShow( _hardwareColumnSpecs.CpuLoad.show );
        doiConfig.getHardwareCpuLoad().setWidth( _hardwareColumnSpecs.CpuLoad.width );
        doiConfig.setHardwareCpuLoadPlot( factory.createColumnSpec() );
        doiConfig.getHardwareCpuLoadPlot().setShow( _hardwareColumnSpecs.CpuLoadPlot.show );
        doiConfig.getHardwareCpuLoadPlot().setWidth( _hardwareColumnSpecs.CpuLoadPlot.width );
        doiConfig.setHardwareUsedMem( factory.createColumnSpec() );
        doiConfig.getHardwareUsedMem().setShow( _hardwareColumnSpecs.UsedMem.show );
        doiConfig.getHardwareUsedMem().setWidth( _hardwareColumnSpecs.UsedMem.width );
        doiConfig.setHardwareTotalMem( factory.createColumnSpec() );
        doiConfig.getHardwareTotalMem().setShow( _hardwareColumnSpecs.TotalMem.show );
        doiConfig.getHardwareTotalMem().setWidth( _hardwareColumnSpecs.TotalMem.width );
        doiConfig.setHardwareMemLoad( factory.createColumnSpec() );
        doiConfig.getHardwareMemLoad().setShow( _hardwareColumnSpecs.MemLoad.show );
        doiConfig.getHardwareMemLoad().setWidth( _hardwareColumnSpecs.MemLoad.width );
        doiConfig.setHardwareMemLoadPlot( factory.createColumnSpec() );
        doiConfig.getHardwareMemLoadPlot().setShow( _hardwareColumnSpecs.MemLoadPlot.show );
        doiConfig.getHardwareMemLoadPlot().setWidth( _hardwareColumnSpecs.MemLoadPlot.width );
        doiConfig.setHardwareNetRxRate( factory.createColumnSpec() );
        doiConfig.getHardwareNetRxRate().setShow( _hardwareColumnSpecs.NetRxRate.show );
        doiConfig.getHardwareNetRxRate().setWidth( _hardwareColumnSpecs.NetRxRate.width );
        doiConfig.setHardwareNetTxRate( factory.createColumnSpec() );
        doiConfig.getHardwareNetTxRate().setShow( _hardwareColumnSpecs.NetTxRate.show );
        doiConfig.getHardwareNetTxRate().setWidth( _hardwareColumnSpecs.NetTxRate.width );
        doiConfig.setHardwareStateChanged( factory.createColumnSpec() );
        doiConfig.getHardwareStateChanged().setShow( _hardwareColumnSpecs.StateChanged.show );
        doiConfig.getHardwareStateChanged().setWidth( _hardwareColumnSpecs.StateChanged.width );
        doiConfig.setHardwareBankAVSN( factory.createColumnSpec() );
        doiConfig.getHardwareBankAVSN().setShow( _hardwareColumnSpecs.BankAVSN.show );
        doiConfig.getHardwareBankAVSN().setWidth( _hardwareColumnSpecs.BankAVSN.width );
        doiConfig.setHardwareBankBVSN( factory.createColumnSpec() );
        doiConfig.getHardwareBankBVSN().setShow( _hardwareColumnSpecs.BankBVSN.show );
        doiConfig.getHardwareBankBVSN().setWidth( _hardwareColumnSpecs.BankBVSN.width );
        doiConfig.setHardwareStatusWord( factory.createColumnSpec() );
        doiConfig.getHardwareStatusWord().setShow( _hardwareColumnSpecs.StatusWord.show );
        doiConfig.getHardwareStatusWord().setWidth( _hardwareColumnSpecs.StatusWord.width );
        doiConfig.setHardwareActiveBank( factory.createColumnSpec() );
        doiConfig.getHardwareActiveBank().setShow( _hardwareColumnSpecs.ActiveBank.show );
        doiConfig.getHardwareActiveBank().setWidth( _hardwareColumnSpecs.ActiveBank.width );
        doiConfig.setHardwareScanNumber( factory.createColumnSpec() );
        doiConfig.getHardwareScanNumber().setShow( _hardwareColumnSpecs.ScanNumber.show );
        doiConfig.getHardwareScanNumber().setWidth( _hardwareColumnSpecs.ScanNumber.width );
        doiConfig.setHardwareScanName( factory.createColumnSpec() );
        doiConfig.getHardwareScanName().setShow( _hardwareColumnSpecs.ScanName.show );
        doiConfig.getHardwareScanName().setWidth( _hardwareColumnSpecs.ScanName.width );
        doiConfig.setHardwarePosition( factory.createColumnSpec() );
        doiConfig.getHardwarePosition().setShow( _hardwareColumnSpecs.Position.show );
        doiConfig.getHardwarePosition().setWidth( _hardwareColumnSpecs.Position.width );
        doiConfig.setHardwarePlayRate( factory.createColumnSpec() );
        doiConfig.getHardwarePlayRate().setShow( _hardwareColumnSpecs.PlayRate.show );
        doiConfig.getHardwarePlayRate().setWidth( _hardwareColumnSpecs.PlayRate.width );
        doiConfig.setHardwareDataMJD( factory.createColumnSpec() );
        doiConfig.getHardwareDataMJD().setShow( _hardwareColumnSpecs.DataMJD.show );
        doiConfig.getHardwareDataMJD().setWidth( _hardwareColumnSpecs.DataMJD.width );
        doiConfig.setHardwareCurrentJob( factory.createColumnSpec() );
        doiConfig.getHardwareCurrentJob().setShow( _hardwareColumnSpecs.CurrentJob.show );
        doiConfig.getHardwareCurrentJob().setWidth( _hardwareColumnSpecs.CurrentJob.width );
                
        doiConfig.setJobLocationDefaultsFileFilter( _jobLocationDefaults.fileFilter );
        doiConfig.setJobLocationDefaultsExperimentBasedOnPathFalse( !_jobLocationDefaults.experimentBasedOnPath );
        doiConfig.setJobLocationDefaultsExperimentNamed( _jobLocationDefaults.experimentNamed );
        doiConfig.setJobLocationDefaultsExperimentName( _jobLocationDefaults.experimentName );
        doiConfig.setJobLocationDefaultsPassBasedOnPathFalse( !_jobLocationDefaults.passBasedOnPath );
        doiConfig.setJobLocationDefaultsExperimentNamed( _jobLocationDefaults.passNamed );
        doiConfig.setJobLocationDefaultsNoPass( _jobLocationDefaults.noPass );
        doiConfig.setJobLocationDefaultsPassName( _jobLocationDefaults.passName );
        doiConfig.setJobLocationDefaultsAutoUpdate( _jobLocationDefaults.autoUpdate );
        
        doiConfig.setUniqueDataSource( !_uniqueDataSource.isSelected() );
        doiConfig.setAssignBasedOnPath( _assignBasedOnPath.isSelected( ) );
        doiConfig.setShareDataSourcesBetweenJobs( _shareDataSourcesBetweenJobs.isSelected() );
        doiConfig.setShareDataSourcesAsProcessors( _shareDataSourcesAsProcessors.isSelected() );
        doiConfig.setRestrictSources( _restrictSourcesCheck.isSelected() );
        doiConfig.setThreadsPerDataSource( _threadsPerDataSource.intValue() );
        doiConfig.setNodesPerCheck( !_nodesPerCheck.isSelected() );
        doiConfig.setNodesPer( _nodesPer.intValue() );
        doiConfig.setAllThreadsCheck( _allThreadsCheck.isSelected() );
        doiConfig.setThreadsPerNode( _threadsPerNode.intValue() );
        doiConfig.setMinThreadsPerNode( _minThreadsPerNode.intValue() );
        doiConfig.setThreadsPerCheck( !_threadsPerCheck.isSelected() );
        doiConfig.setBaselineCheck( !_baselineCheck.isSelected() );
        doiConfig.setJobCheck( _jobCheck.isSelected() );
        doiConfig.setSequentialCheck( _sequentialCheck.isSelected() );
        doiConfig.setMaxJobs( _maxJobs.intValue() );
        doiConfig.setMaxSecondsForHardware( _maxSecondsForHardware.intValue() );
        doiConfig.setUseMaxSecondsForHardware( _useMaxSecondsForHardware.isSelected() );
        doiConfig.setMaxSecondsForProcessing( _maxSecondsForProcessing.intValue() );
        doiConfig.setUseMaxSecondsForProcessing( _useMaxSecondsForProcessing.isSelected() );
        doiConfig.setSimultaneousCheck( !_simultaneousCheck.isSelected() );
        doiConfig.setRunLogCheck( !_runLogCheck.isSelected() );
        doiConfig.setRunLogFile( _runLogFile.getText() );

        if ( _sourceBasedOnPathDisplay != null && _sourceBasedOnPathDisplay.panels() != null ) {
            for ( Iterator<SourceBasedOnPathDisplay.PanelItem> iter = _sourceBasedOnPathDisplay.panels().iterator(); iter.hasNext(); ) {
                SourceBasedOnPathDisplay.PanelItem panelItem = iter.next();
                DoiSystemConfig.PathNodePair pathNodePair = factory.createDoiSystemConfigPathNodePair();
                pathNodePair.setPath( panelItem.textField.getText() );
                pathNodePair.setNode( (String)panelItem.comboBox.getSelectedItem() );
                doiConfig.getPathNodePair().add( pathNodePair );
            }
        }
        else if ( _sourceBasedOnPathList != null ) {
            for ( Iterator<String> iter = _sourceBasedOnPathList.keySet().iterator(); iter.hasNext(); ) {
                String path = iter.next();
                DoiSystemConfig.PathNodePair pathNodePair = factory.createDoiSystemConfigPathNodePair();
                pathNodePair.setPath( path );
                pathNodePair.setNode( _sourceBasedOnPathList.get( path ) );
                doiConfig.getPathNodePair().add( pathNodePair );
            }
        }
        if ( _restrictedSourceListDisplay != null && _restrictedSourceListDisplay.panels() != null ) {
            for ( Iterator<RestrictedSourceListDisplay.PanelItem> iter = _restrictedSourceListDisplay.panels().iterator(); iter.hasNext(); ) {
                RestrictedSourceListDisplay.PanelItem panelItem = iter.next();
                DoiSystemConfig.AllowedNode allowedNode = factory.createDoiSystemConfigAllowedNode();
                allowedNode.setNode( (String)panelItem.comboBox.getSelectedItem() );
                doiConfig.getAllowedNode().add( allowedNode );
            }
        }
        else if ( _restrictedSourceList != null ) {
            for ( Iterator<String> iter = _restrictedSourceList.iterator(); iter.hasNext(); ) {
                String nodeName = iter.next();
                DoiSystemConfig.AllowedNode allowedNode = factory.createDoiSystemConfigAllowedNode();
                allowedNode.setNode( nodeName );
                doiConfig.getAllowedNode().add( allowedNode );
            }
        }

        //  Build a lists of processors and Mark5s that are "invisible".  This comes
        //  from the hardware list.
        if ( _hardwareMonitor.processorNodes().children().size() > 0 ) {
            String nodeList = new String();
            String coreList = new String();
            for ( Iterator<BrowserNode> iter = _hardwareMonitor.processorNodes().childrenIterator(); iter.hasNext(); ) {
                ProcessorNode thisNode = (ProcessorNode)(iter.next());
                if ( thisNode.currentState().contentEquals( "invisible" ) ) {
                    if ( nodeList.length() > 0 )
                        nodeList += ",";
                    nodeList += thisNode.name();
                    if ( coreList.length() > 0 )
                        coreList += ",";
                    coreList += thisNode.numCores() + "";
                }
            }
            if ( nodeList.length() > 0 ) {
                doiConfig.setInvisibleProcessors( nodeList );
                doiConfig.setInvisibleProcessorCores( coreList );
            }
        }
        if ( _hardwareMonitor.mk5Modules().children().size() > 0 ) {
            String nodeList = new String();
            for ( Iterator<BrowserNode> iter = _hardwareMonitor.mk5Modules().childrenIterator(); iter.hasNext(); ) {
                ProcessorNode thisNode = (ProcessorNode)(iter.next());
                if ( thisNode.currentState().contentEquals( "invisible" ) ) {
                    if ( nodeList.length() > 0 )
                        nodeList += ",";
                    nodeList += thisNode.name();
                }
            }
            if ( nodeList.length() > 0 ) {
                doiConfig.setInvisibleMark5S( nodeList );
            }
        }

        try {
            javax.xml.bind.JAXBContext jaxbCtx = javax.xml.bind.JAXBContext.newInstance( doiConfig.getClass().getPackage().getName() );
            javax.xml.bind.Marshaller marshaller = jaxbCtx.createMarshaller();
            File theFile = new File( filename );
            theFile.createNewFile();
            marshaller.marshal( doiConfig, theFile );
            return true;
        } catch ( java.io.IOException e ) {
            System.out.println( "SystemSettings: can't write file \"" + filename + "\" - some appropriate complaint here." );
            return false;   
        } catch (javax.xml.bind.JAXBException ex) {
            // XXXTODO Handle exception
            java.util.logging.Logger.getLogger("global").log( java.util.logging.Level.SEVERE, null, ex );
            return false;
        } catch ( Exception e ) {
            return false;
        }
    }
    
    public String defaultSettingsFile() {
        //  See if a DIFXROOT environment variable has been defined.  If not,
        //  guess that the current working directory is the DIFXROOT.
        String difxRoot = System.getenv( "DIFXROOT" );
        if (difxRoot == null) {
            difxRoot = System.getProperty( "user.dir" );
        }
        return difxRoot + "/conf/guiSettings.xml";
    }
    
    /*
     * This function is called from the thread that receives broadcasts each time
     * it cycles.  It gives us the size of a received packet, or a 0 if this thread
     * timed out.  The information is added to a plot if this window is visible
     * (otherwise it is kind of a waste of time).
     */
    public void gotPacket( int newSize ) {
        if ( this.isVisible() )
            _broadcastPlot.limits( (double)(_broadcastTrackSize - _broadcastPlot.w()), (double)(_broadcastTrackSize), -.05, 1.0 );
        _broadcastTrack.add( (double)(_broadcastTrackSize), (double)(newSize)/(double)bufferSize() );
        _broadcastTrackSize += 1;
        _plotWindow.updateUI();
    }
    
    /*
     * Make a test connection to the database amd 
     */
    public void testDatabaseAction() {
        databaseSuccess( "" );
        databaseWarning( "Connecting to database (" + this.dbURL() + ")..." );
        QueueDBConnection db = new QueueDBConnection( this );
        try {
            databaseSuccess( "Connection successful...reading DiFX jobs..." );
            ResultSet jobInfo = db.jobList();

            Integer n = 0;
            while ( jobInfo.next() )
                ++n;
            databaseSuccess( "Database contains " + n.toString() + " jobs." );
            databaseSuccess( "" );

        } catch ( java.sql.SQLException e ) {
            databaseFailure( "SQLException: " + e.getMessage() );
            databaseSuccess( "" );
        } catch ( Exception e ) {
            databaseFailure( "Connection Failure (Exception): " + e.getMessage()  );
            databaseSuccess( "" );
        }
    }
    
    /*
     * Perform a "ping" test on the specified database host.
     */
    public void pingDatabaseHost() {
        databaseSuccess( "" );
        databaseWarning( "Starting ping test..." );
        final PingTest tester = new PingTest( _dbHost.getText() );
        tester.pings( 6 );
        tester.addSuccessListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                databaseSuccess( tester.lastMessage() );
            }
        } );            
        tester.addFailureListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                databaseFailure( tester.lastError() );
            }
        } );            
        tester.start();
    }
    
    protected void databaseWarning( String message ) {
        MessageNode node = new MessageNode( Calendar.getInstance().getTimeInMillis(), MessageNode.WARNING, null, message );
        node.showDate( false );
        node.showTime( true );
        node.showSource( false );
        node.showWarnings( true );
        _databaseMessages.addMessage( node );
        _databaseMessages.scrollToEnd();
    }
    
    protected void databaseSuccess( String message ) {
        MessageNode node = new MessageNode( 0, MessageNode.INFO, null, message );
        node.showDate( false );
        node.showTime( false );
        node.showSource( false );
        node.showMessages( true );
        _databaseMessages.addMessage( node );
        _databaseMessages.scrollToEnd();
    }
    
    protected void databaseFailure( String message ) {
        MessageNode node = new MessageNode( 0, MessageNode.ERROR, null, message );
        node.showDate( false );
        node.showTime( false );
        node.showSource( false );
        node.showErrors( true );
        _databaseMessages.addMessage( node );
        _databaseMessages.scrollToEnd();
    }
    
    public int inactivityWarning() { return _inactivityWarning.intValue(); }
    public void inactivityWarning( int newVal ) { _inactivityWarning.intValue( newVal ); }
    public int inactivityError() { return _inactivityError.intValue(); }
    public void inactivityError( int newVal ) { _inactivityError.intValue( newVal ); }
    
    public void hardwareMonitor( HardwareMonitorPanel newMonitor ) {
        _hardwareMonitor = newMonitor;
    }
    public HardwareMonitorPanel hardwareMonitor() {
        return _hardwareMonitor;
    }
    
    public void difxUI( DiFXUI newUI ) {
        _difxUI = newUI;
    }
    
    public void queueBrowser( QueueBrowserPanel newBrowser ) {
        _queueBrowser = newBrowser;
    }
    public QueueBrowserPanel queueBrowser() {
        return _queueBrowser;
    }
    public void difxMessageProcessor( DiFXMessageProcessor newProcessor ) {
        _difxMessageProcessor = newProcessor;
    }
    public DiFXMessageProcessor difxMessageProcessor() {
        return _difxMessageProcessor;
    }
    public QueueBrowserSettings queueBrowserSettings() { return _queueBrowserSettings; }
    public WindowConfiguration windowConfiguration() { return _windowConfiguration; }
    public DefaultNames defaultNames() { return _defaultNames; }
    public JobColumnSpecs jobColumnSpecs() { return _jobColumnSpecs; }
    public HardwareColumnSpecs hardwareColumnSpecs() { return _hardwareColumnSpecs; }
    
    /*
     * Return the current list of experiment status types.  If the list is empty and
     * the database is being employed, try to create a list from the database.
     */
    public Map<Integer, ExperimentStatusEntry> experimentStatusList() {
        if ( _experimentStatusList == null )
            _experimentStatusList = new HashMap<Integer, ExperimentStatusEntry>();
        if ( _experimentStatusList.isEmpty() ) {            
            if ( useDatabase() ) {
                updateFromDatabase();
            }
        }
        return _experimentStatusList;
    }
    
    /*
     * Obtain the "id" of an experiment status from its string form.
     */
    public Integer experimentStatusID( String status ) {
        if ( _experimentStatusList == null )
            return 0;
        Iterator iter = _experimentStatusList.entrySet().iterator();
        for ( ; iter.hasNext(); ) {
            Map.Entry m = (Map.Entry)iter.next();
            if ( ((ExperimentStatusEntry)m.getValue()).status.contentEquals( status ) )
                return ((Integer)m.getKey());
        }
        return 0;
    }
    
    /*
     * Obtain the string form of an experiment status from its ID.
     */
    public String experimentStatusString( Integer id ) {
        if ( _experimentStatusList == null )
            return null;
        Iterator iter = _experimentStatusList.entrySet().iterator();
        for ( ; iter.hasNext(); ) {
            Map.Entry m = (Map.Entry)iter.next();
            if ( (Integer)m.getKey() == id )
                return ((ExperimentStatusEntry)m.getValue()).status;
        }
        return null;
    }
    
    /*
     * Return the current list of job status types, or try to create one
     * from the database if it doesn't exist yet.
     */
    public Map<Integer, JobStatusEntry> jobStatusList() {
        if ( _jobStatusList == null )
            _jobStatusList = new HashMap<Integer, JobStatusEntry>();
        if ( _jobStatusList.isEmpty() ) {            
            if ( useDatabase() ) {
                updateFromDatabase();
            }
        }
        return _jobStatusList;
    }
    
    /*
     * Obtain the "id" of an experiment status from its string form.
     */
    public Integer jobStatusID( String status ) {
        Map<Integer, JobStatusEntry> jobStatusList = jobStatusList();
        if ( jobStatusList == null )
            return 0;
        Iterator iter = jobStatusList.entrySet().iterator();
        for ( ; iter.hasNext(); ) {
            Map.Entry m = (Map.Entry)iter.next();
            if ( ((JobStatusEntry)m.getValue()).status.contentEquals( status ) )
                return ((Integer)m.getKey());
        }
        return 0;
    }
    
    /*
     * Obtain the string form of an experiment status from its ID.
     */
    public String jobStatusString( Integer id ) {
        Map<Integer, JobStatusEntry> jobStatusList = jobStatusList();
        if ( jobStatusList == null )
            return null;
        Iterator iter = jobStatusList.entrySet().iterator();
        for ( ; iter.hasNext(); ) {
            Map.Entry m = (Map.Entry)iter.next();
            if ( (Integer)m.getKey() == id )
                return ((JobStatusEntry)m.getValue()).status;
        }
        return null;
    }
    
    /*
     * Return the current list of pass types, or try to create one
     * from the database if it doesn't exist yet.
     */
    public Map<Integer, String> passTypeList() {
        if ( _passTypeList == null )
            _passTypeList = new HashMap<Integer, String>();
        if ( _passTypeList.isEmpty() ) {
            if ( useDatabase() ) {
                updateFromDatabase();
            }
        }
        return _passTypeList;
    }
    
    /*
     * Obtain the "id" of a pass type from its string form.
     */
    public Integer passTypeID( String status ) {
        Map<Integer, String> passTypeList = passTypeList();
        if ( passTypeList == null )
            return 0;
        Iterator iter = passTypeList.entrySet().iterator();
        for ( ; iter.hasNext(); ) {
            Map.Entry m = (Map.Entry)iter.next();
            if ( ((String)m.getValue()).contentEquals( status ) )
                return ((Integer)m.getKey());
        }
        return 0;
    }
    
    /*
     * Obtain the string form of an pass type from its ID.
     */
    public String passTypeString( Integer id ) {
        Map<Integer, String> passTypeList = passTypeList();
        if ( passTypeList == null )
            return null;
        Iterator iter = passTypeList.entrySet().iterator();
        for ( ; iter.hasNext(); ) {
            Map.Entry m = (Map.Entry)iter.next();
            if ( (Integer)m.getKey() == id )
                return ((String)m.getValue());
        }
        return null;
    }
    
    public boolean suppressWarnings() { return _suppressWarningsCheck.isSelected(); }
    
    /*
     * Add a new data source.  The "name" is ostensibly what it is called - the VSN
     * for a module, full path for a file, etc.  The type tells us what kind it is -
     * module, file, eVLBI, etc.  The "source" describes where we got the above
     * information - from the database, because the module was detected in a data
     * node, or whatever.
     */
    public void addDataSource( String name, String type, String source ) {
        DataSource src = new DataSource();
        src.name = name;
        src.type = type;
        src.source = source;
        if ( _dataSourceList == null )
            _dataSourceList = new ArrayList<DataSource>();
        _dataSourceList.add( src );
    }
    
    /*
     * Return a list of all data sources of the given type.
     */
    public ArrayList<DataSource> listDataSources( String type ) {
        ArrayList<DataSource> list = new ArrayList<DataSource>();
        if ( _dataSourceList != null ) {
            for ( Iterator<DataSource> iter = _dataSourceList.iterator(); iter.hasNext(); ) {
                DataSource src = iter.next();
                if ( src.type.contentEquals( type ) )
                    list.add( src );
            }
        }
        return list;
    }
    
    /*
     * Determine whether the given data source name and type exist in the list.
     */
    public boolean dataSourceInList( String name, String type ) {
        if ( _dataSourceList != null && name != null && type != null ) {
            for ( Iterator<DataSource> iter = _dataSourceList.iterator(); iter.hasNext(); ) {
                DataSource src = iter.next();
                if ( src.name.contentEquals( name ) && src.type.contentEquals( type ) )
                    return true;
            }
        }
        return false;
    }
    
    /*
     * Timeout event for reading the database.  This is called every second to trigger
     * an update event at the "auto update interval".
     */
    public void databaseTimeoutEvent() {
        //  See if we are doing "auto" updates of the database information.
        if ( this.dbAutoUpdate() ) {
            //  Auto updates will be performed at the interval specified unless they
            //  have just been turned on (in which case an immediate update will be
            //  performed).
            if ( _dbTimeoutCounter == 0 )
                updateDatabaseNow( true );
            ++_dbTimeoutCounter;
            if ( _dbTimeoutCounter >= this.dbAutoUpdateInterval() )
                _dbTimeoutCounter = 0;
        }
        else
            _dbTimeoutCounter = 0;
    }
    
    /*
     * This thread calls the database timeout event.
     */
    public class DatabaseThread extends Thread {
        protected int _interval;
        protected boolean _keepGoing;
        public DatabaseThread( int interval ) {
            _interval = interval;
            _keepGoing = true;
        }
        public void keepGoing( boolean newVal ) {
            _keepGoing = newVal;
        }
        @Override
        public void run() {
            while ( _keepGoing ) {
                databaseTimeoutEvent();
                try {
                    Thread.sleep( _interval );
                } catch ( Exception e ) {
                    _keepGoing = false;
                }
            }
        }
    }
    
    /*
     * Setting this value to "true" will trigger a single update of the database
     * information.  This is the function that should be called to trigger this
     * activity from outside of this class.
     */
    public synchronized void updateDatabaseNow( boolean newVal ) { _updateDatabaseNow = newVal; }
    
    /*
     * Internal thread used to keep the database queries from tying up the graphics
     * update.  This function triggers the update in this class, which is limited to
     * status lists and other minor things.  It also generates a callback that other
     * classes (such as the queue browser) can watch to update larger data sets
     * (experiment lists, etc).
     */
    class UpdateDatabaseLoop extends Thread {
        
        public void run() {
            
            while ( true ) {
                try {
                    Thread.sleep( 100 );
                    if ( _updateDatabaseNow ) {
                        //  Empty the experiment, job, and pass type lists.  This will
                        //  force them to update the next time they are consulted....
                        if ( _jobStatusList != null )
                            _jobStatusList.clear();
                        if ( _experimentStatusList != null )
                            _experimentStatusList.clear();
                        if ( _passTypeList != null )
                            _passTypeList.clear();
                        //  ...and that consulting is done here.
                        updateFromDatabase();
                        //  Trigger callbacks to other classes that want to watch this periodic
                        //  update.  The queue browser uses this.
                        dispatchDatabaseUpdateEvent();
                        updateDatabaseNow( false );
                    }
                } catch( java.lang.InterruptedException e ) {
                }
            }
            
        }
        
    }
    
    /*
     * Collect current database information for the "status" lists.  An error is triggered if the
     * connection to the database fails.
     */
    protected void updateFromDatabase() {

        //  Don't do this if the user isn't using the database.
        if ( !this.useDatabase() )
            return;
        
        //  Get a new connection to the database.  Bail out if this doesn't work.
        QueueDBConnection db = new QueueDBConnection( this );
        if ( !db.connected() ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                "Automatic update from database failed - no connection established." );
            return;
        }
        
        //  Get the database information we are interested in.
        _dbPassTypeList = db.passTypeList();
        _dbJobStatusList = db.jobStatusList();
        _dbExperimentStatusList = db.experimentStatusList();
        _dbVersionHistoryList = db.versionHistoryList();
        
        //  Use the latest version number to update the version field.
        try { 
            _dbVersionHistoryList.last();
            _dbVersion.setText( _dbVersionHistoryList.getString( "major" ) + "." + _dbVersionHistoryList.getString( "minor" ) );
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                "Exception updating database version: - is the version history table empty? (" + e + ")" );
        }
        
        //  Update pass type.
        if ( _passTypeList == null )
            _passTypeList = new HashMap<Integer, String>();
        try {
            while ( _dbPassTypeList.next() ) {
                _passTypeList.put( _dbPassTypeList.getInt( "id" ), _dbPassTypeList.getString( "Type" ) );
            }
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, null, e );
        }
        //  Update job status.
        if ( _jobStatusList == null )
            _jobStatusList = new HashMap<Integer, JobStatusEntry>();
        try {
            while ( _dbJobStatusList.next() ) {
                _jobStatusList.put( _dbJobStatusList.getInt( "id" ),
                        new JobStatusEntry( _dbJobStatusList.getInt( "active" ),
                                _dbJobStatusList.getString( "status" ) ) );
            }
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, null, e );
        }
        //  Update experiment status.
        if ( _experimentStatusList == null )
            _experimentStatusList = new HashMap<Integer, ExperimentStatusEntry>();
        try {
            while ( _dbExperimentStatusList.next() ) {
                _experimentStatusList.put( _dbExperimentStatusList.getInt( "id" ),
                        new ExperimentStatusEntry( _dbExperimentStatusList.getInt( "statuscode" ),
                                _dbExperimentStatusList.getString( "experimentstatus" ) ) );
            }
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, null, e );
        }
        
    }

    public void addDatabaseUpdateListener( ActionListener a ) {
        if ( _databaseUpdateListeners == null )
            _databaseUpdateListeners = new EventListenerList();
        _databaseUpdateListeners.add( ActionListener.class, a );
    }

    protected void dispatchDatabaseUpdateEvent() {
        if ( _databaseUpdateListeners != null ) {
            Object[] listeners = _databaseUpdateListeners.getListenerList();
            // loop through each listener and pass on the event if needed
            int numListeners = listeners.length;
            for ( int i = 0; i < numListeners; i+=2 ) {
                if ( listeners[i] == ActionListener.class )
                    ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
            }
        }
    }
    
    public ArrayList<String> moduleFormatList() {
        return _moduleFormatList;
    }
    
    /*
     * Add a module format to the module format list.
     */
    public void addModuleFormat( String newFormat ) {
        _moduleFormatList.add( newFormat );
    }
    
    /*
     * Return true if a given module format is already in the list.
     */
    public boolean inModuleFormatList( String oldFormat ) {
        for ( Iterator<String> iter = _moduleFormatList.iterator(); iter.hasNext(); ) {
            String thisString = iter.next();
            if ( thisString.trim().contentEquals( oldFormat ) )
                return true;
        }
        return false;
    }
    
    /*
     * Remove the (first) instance of the given string in the module format list.
     * Return true if it was removed, false if it was not found.
     */
    public boolean removeModuleFormat( String oldFormat ) {
        for ( Iterator<String> iter = _moduleFormatList.iterator(); iter.hasNext(); ) {
            String thisString = iter.next();
            if ( thisString.trim().contentEquals( oldFormat ) ) {
                iter.remove();
                return true;
            }
        }
        return false;
    }

    public ArrayList<String> toneSelectionList() {
        return _toneSelectionList;
    }
    
    /*
     * Add a tone selection to the tone selection list.
     */
    public void addToneSelection( String newFormat ) {
        _toneSelectionList.add( newFormat );
    }
    
    /*
     * Return true if a given tone selection is already in the list.
     */
    public boolean inToneSelectionList( String oldFormat ) {
        for ( Iterator<String> iter = _toneSelectionList.iterator(); iter.hasNext(); ) {
            String thisString = iter.next();
            if ( thisString.trim().contentEquals( oldFormat ) )
                return true;
        }
        return false;
    }
    
    /*
     * Remove the (first) instance of the given string in the tone selection list.
     * Return true if it was removed, false if it was not found.
     */
    public boolean removeToneSelection( String oldFormat ) {
        for ( Iterator<String> iter = _toneSelectionList.iterator(); iter.hasNext(); ) {
            String thisString = iter.next();
            if ( thisString.trim().contentEquals( oldFormat ) ) {
                iter.remove();
                return true;
            }
        }
        return false;
    }

    public void guiServerVersion( String newVal ) {
        _guiServerVersion.setText( newVal );
    }
    public String guiServerVersion() {
        return _guiServerVersion.getText();
    }
    
    public void guiServerDifxVersion( String newVal ) {
        _guiServerDifxVersion.setText( newVal );
    }
    public String guiServerDifxVersion() {
        return _guiServerDifxVersion.getText();
    }
    
    public boolean isMark5Name( String newName ) {
        if ( _identifyMark5sCheck.isSelected() && _mark5PatternList != null ) {
            for ( int i = 0; i < _mark5PatternList.length; ++i ) {
                try {
                    if ( newName.matches( _mark5PatternList[i] ) )
                        return true;
                } catch ( java.util.regex.PatternSyntaxException e ) {
                    _messageCenter.error ( 0, "SystemSettings", "unparseable pattern in Mark5 pattern list - \"" + _mark5PatternList[i] + "\"" );
                }
            }
        }
        return false;
    }
    
    protected SystemSettings _this;
    
    protected boolean _allObjectsBuilt;

    protected JMenuBar _menuBar;
    protected FormattedTextField _settingsFile;
    protected FormattedTextField _title;
    
    protected String _jaxbPackage;
    protected String _home;
    protected boolean _loggingEnabled;
    protected long _statusValidDuration;
    //  DiFX Control Connection
    protected ZCheckBox _difxUDPCheck;
    protected ZCheckBox _difxTCPCheck;
    protected FormattedTextField _difxControlAddress;
    protected NumberBox _difxControlPort;
    protected NumberBox _difxTransferPort;
    protected NumberBox _maxTransferPorts;
    protected boolean[] _transferPortUsed;
    protected int _newDifxTransferPort;
    protected SaneTextField _difxMonitorHost;
    protected NumberBox _difxMonitorPort;
    protected SaneTextField _difxControlUser;
    protected JComboBox _difxVersion;
    protected ZCheckBox _useDefaultStartScript;
    protected SaneTextField _difxBase;
    protected GuiServerConnection _guiServerConnection;
    //  Broadcast network
    protected ZCheckBox _useTCPRelayCheck;
    protected JFormattedTextField _ipAddress;
    protected NumberBox _port;
    protected NumberBox _bufferSize;
    protected NumberBox _timeout;
    PlotWindow _plotWindow;
    Plot2DObject _broadcastPlot;
    Track2D _broadcastTrack;
    int _broadcastTrackSize;
    protected ZCheckBox _suppressWarningsCheck;
    protected ZCheckBox _identifyMark5sCheck;
    protected SaneTextField _mark5Pattern;
    protected NumberBox _inactivityWarning;
    protected NumberBox _inactivityError;
    protected JButton _viewDifxMessagesButton;
    //  Database configuration
    protected ZCheckBox _dbUseDataBase;
    protected JFormattedTextField _dbVersion;
    protected JFormattedTextField _dbHost;
    protected JFormattedTextField _dbUser;
    protected JPasswordField _dbPwd;
    protected JFormattedTextField _dbName;
    protected JFormattedTextField _dbMS;
    protected JFormattedTextField _dbDriver;
    protected JFormattedTextField _dbPort;
    protected String _dbURL;
    protected ZCheckBox _dbAutoUpdate;
    protected NumberBox _dbAutoUpdateInterval;
    protected JButton _pingHostButton;
    protected JButton _testDatabaseButton;
    protected MessageScrollPane _databaseMessages;
    //  Default report location
    protected String _reportLoc;
    
    //  These are locations for "help" - GUI and DiFX documentation.
    protected JFormattedTextField _guiDocPath;
    protected JButton _guiDocPathBrowseButton;
    protected JFormattedTextField _difxUsersGroupURL;
    protected JFormattedTextField _difxWikiURL;
    protected JFormattedTextField _difxSVN;
    
    //  Items that govern the creation and running of jobs.
    protected JFormattedTextField _workingDirectory;
    protected JFormattedTextField _stagingArea;
    protected ZCheckBox _useStagingArea;
    protected SaneTextField _headNode;
    
    //  EOP Settings items.
    protected SaneTextField _eopURL;
    protected SaneTextField _leapSecondsURL;
    protected JButton _viewEOPFile;
    protected JButton _viewLeapSecondsFile;
    protected ZCheckBox _useLeapSecondsURL;
    protected ZCheckBox _useLeapSecondsValue;
    protected NumberBox _leapSecondsValue;
    protected ZCheckBox _autoUpdateEOP;
    protected NumberBox _autoUpdateSeconds;
    protected JButton _updateEOPNow;
    
    protected EOPMonitorThread _eopMonitorThread;
    protected int _eopTimerCount;
    protected boolean _doEOPUpdate;
    protected SimpleTextEditor _eopText;
    protected SimpleTextEditor _leapSecondText;
    protected JFrame _eopDisplay;
    protected JFrame _leapSecondDisplay;
    
    //  The "look and feel" that applies to all GUI components.
    protected String _lookAndFeel;
    
    //  Settings in the queue browser.
    public class QueueBrowserSettings {
        boolean showSelected;
        boolean showUnselected;
        boolean showCompleted;
        boolean showIncomplete;
        boolean showExperimentScheduled;
        boolean showPassScheduled;
    }
    protected QueueBrowserSettings _queueBrowserSettings;
    
    //  Dimensions and other configurations related to windows
    public class WindowConfiguration {
        int mainX;
        int mainY;
        int mainW;
        int mainH;
        String title;
        boolean verticalPanels;
        int mainDividerLocation;
        int topDividerLocation;
        boolean queueBrowserTearOff;
        int queueBrowserX;
        int queueBrowserY;
        int queueBrowserW;
        int queueBrowserH;
        boolean hardwareMonitorTearOff;
        int hardwareMonitorX;
        int hardwareMonitorY;
        int hardwareMonitorW;
        int hardwareMonitorH;
        int experimentEditorW;
        int experimentEditorH;
        int settingsWindowW;
        int settingsWindowH;
        int jobEditorMonitorWindowW;
        int jobEditorMonitorWindowH;
        int smartDisplayW;
        int smartDisplayH;
        int environmentVariableDisplayW;
        int environmentVariableDisplayH;
        int sourceBasedOnPathDisplayW;
        int sourceBasedOnPathDisplayH;
        int restrictedSourceListDisplayW;
        int restrictedSourceListDisplayH;
        int requestedMessageListDisplayW;
        int requestedMessageListDisplayH;
        int directoryDisplayW;
        int directoryDisplayH;
        int monitorDisplayW;
        int monitorDisplayH;
        int diskSearchRulesDisplayW;
        int diskSearchRulesDisplayH;
        public int difxMessageWindowW;
        public int difxMessageWindowH;
        public double difxMessageWindowTopFraction;
        public double difxMessageWindowBottomFraction;
        public int difxMessageWindowMessageLimit;
    }
    protected WindowConfiguration _windowConfiguration;
    
    //  Defaults for a bunch of things that the user would likely change (not all
    //  of them names).  The common thread here is that these settings do not appear
    //  in the settings window.
    public class DefaultNames {
        String vexFileSource;
        String viaHttpLocation;
        String viaFtpLocation;
        String localFileLocation;
        boolean vexFromHost;
        boolean vexViaHttp;
        boolean vexViaFtp;
        boolean vexFromLocal;
        boolean vexFromExperiment;
        String v2dFileSource;
        String v2dViaHttpLocation;
        String v2dViaFtpLocation;
        String localV2dFileLocation;
        boolean v2dFromHost;
        boolean v2dViaHttp;
        boolean v2dViaFtp;
        boolean v2dFromLocal;
        boolean noV2dFile;
        boolean createPassOnExperimentCreation;
        boolean singleInputFile;
        boolean scanBasedJobNames;
        boolean jobCreationSanityCheck;
        String dirListLocation;
        boolean restrictHeadnodeProcessing;
        boolean eliminateNonrespondingProcessors;
        boolean eliminateBusyProcessors;
        double busyPercentage;
        boolean chooseBasedOnModule;
        boolean correlationDoPolar;
        double correlationTInt;
        double correlationSpecRes;
        int correlationNChan;
        double correlationFFTSpecRes;
        int correlationNFFTChan;
        int phaseCalInt;
        int correlationSubintNS;
        String toneSelection;
        String dataFormat;
        boolean runMonitor;
    }
    protected DefaultNames _defaultNames;
    
    /*
     * Default values for items in the "job location" windows (see the QueueBrowser).
     */
    public class JobLocationDefaults {
        String fileFilter;
        boolean experimentBasedOnPath;
        boolean experimentNamed;
        String experimentName;
        boolean passBasedOnPath;
        boolean passNamed;
        boolean noPass;
        String passName;
        boolean autoUpdate;
    }
    protected JobLocationDefaults _jobLocationDefaults;
    public JobLocationDefaults jobLocationDefaults() {
        return _jobLocationDefaults;
    }
    
    public class ColumnSpec {
        boolean show;
        int width;
    }
    
    public class JobColumnSpecs {
        ColumnSpec networkActivity = new ColumnSpec();
        ColumnSpec name = new ColumnSpec();
        ColumnSpec progressBar = new ColumnSpec();
        ColumnSpec state = new ColumnSpec();
        ColumnSpec experiment = new ColumnSpec();
        ColumnSpec pass = new ColumnSpec();
        ColumnSpec priority = new ColumnSpec();
        ColumnSpec queueTime = new ColumnSpec();
        ColumnSpec correlationStart = new ColumnSpec();
        ColumnSpec correlationEnd = new ColumnSpec();
        ColumnSpec correlationTime = new ColumnSpec();
        ColumnSpec jobStart = new ColumnSpec();
        ColumnSpec jobDuration = new ColumnSpec();
        ColumnSpec inputFile = new ColumnSpec();
        ColumnSpec outputFile = new ColumnSpec();
        ColumnSpec outputSize = new ColumnSpec();
        ColumnSpec difxVersion = new ColumnSpec();
        ColumnSpec speedUpFactor = new ColumnSpec();
        ColumnSpec numAntennas = new ColumnSpec();
        ColumnSpec numForeignAntennas = new ColumnSpec();
        ColumnSpec dutyCycle = new ColumnSpec();
        ColumnSpec status = new ColumnSpec();
        ColumnSpec active = new ColumnSpec();
        ColumnSpec statusId = new ColumnSpec();
        ColumnSpec weights = new ColumnSpec();
        ColumnSpec weightsAsPlots = new ColumnSpec();
        ColumnSpec weightsAsNumbers = new ColumnSpec();
    }
    protected JobColumnSpecs _jobColumnSpecs;
    
    public class HardwareColumnSpecs {
        ColumnSpec Ignored = new ColumnSpec();
        ColumnSpec broadcastMonitor = new ColumnSpec();
        ColumnSpec NumCPUs = new ColumnSpec();
        ColumnSpec NumCores = new ColumnSpec();
        ColumnSpec BogusGHz = new ColumnSpec();
        ColumnSpec Type = new ColumnSpec();
        ColumnSpec TypeString = new ColumnSpec();
        ColumnSpec State = new ColumnSpec();
        ColumnSpec Enabled = new ColumnSpec();
        ColumnSpec CpuLoad = new ColumnSpec();
        ColumnSpec CpuLoadPlot = new ColumnSpec();
        ColumnSpec UsedMem = new ColumnSpec();
        ColumnSpec TotalMem = new ColumnSpec();
        ColumnSpec MemLoad = new ColumnSpec();
        ColumnSpec MemLoadPlot = new ColumnSpec();
        ColumnSpec NetRxRate = new ColumnSpec();
        ColumnSpec NetTxRate = new ColumnSpec();
        ColumnSpec StateChanged = new ColumnSpec();
        ColumnSpec BankAVSN = new ColumnSpec();
        ColumnSpec BankBVSN = new ColumnSpec();
        ColumnSpec StatusWord = new ColumnSpec();
        ColumnSpec ActiveBank = new ColumnSpec();
        ColumnSpec ScanNumber = new ColumnSpec();
        ColumnSpec ScanName = new ColumnSpec();
        ColumnSpec Position = new ColumnSpec();
        ColumnSpec PlayRate = new ColumnSpec();
        ColumnSpec DataMJD = new ColumnSpec();
        ColumnSpec CurrentJob = new ColumnSpec();
    }
    protected HardwareColumnSpecs _hardwareColumnSpecs;
    
    //  Different lists of event listeners.  Other classes can be informed of
    //  setting changes by adding themselves to these lists.
    EventListenerList _databaseChangeListeners;
    EventListenerList _broadcastChangeListeners;
    EventListenerList _eopChangeListeners;
    EventListenerList _inactivityChangeListeners;
    
    //  All settings use the same file chooser.
    JFileChooser _fileChooser;
    
    NodeBrowserScrollPane _scrollPane;
    IndexedPanel _addressesPanel;
    
    //  These items are used by multiple classes - they are put here as a matter
    //  of convenience as all locations have access to this class.
    DiFXUI _difxUI;
    HardwareMonitorPanel _hardwareMonitor;
    QueueBrowserPanel _queueBrowser;
    MessageDisplayPanel _messageCenter;
    DiFXMessageProcessor _difxMessageProcessor;
    
    //  These lists contain "status" values that can be applied to different things.
    //  Nominally they come from the database, but in the absense of the database the
    //  user can create their own.
    public class ExperimentStatusEntry {
        public ExperimentStatusEntry( Integer newCode, String newStatus ) {
            code = newCode;
            status = newStatus;
        }
        public Integer code;
        public String status;
    }
    protected Map<Integer, ExperimentStatusEntry> _experimentStatusList;
    public class JobStatusEntry {
        public JobStatusEntry( Integer newActive, String newStatus ) {
            active = newActive;
            status = newStatus;
        }
        public Integer active;
        public String status;
    }
    protected Map<Integer, JobStatusEntry> _jobStatusList;
    
    //  These lists contain "type" values that can be applied to passes.
    //  Nominally they come from the database, but in the absense of the database the
    //  user can create their own.
    protected Map<Integer, String> _passTypeList;
    
    //  This class is used to contain information about a "data source", including
    //  its name, type, and where we got information about it (the "source" of the source).
    //  Since these are going to be used in a bunch of different ways, everything
    //  is a string.
    public class DataSource {
        String name;
        String type;
        String source;
    }
    
    //  List of pattens used to match Mark5 names
    protected String[] _mark5PatternList;
    
    //  Our list of the above class types.
    protected ArrayList<DataSource> _dataSourceList;
    
    //  Lists containing most recent database data.
    protected ResultSet _dbPassTypeList;
    protected ResultSet _dbJobStatusList;
    protected ResultSet _dbExperimentStatusList;
    protected ResultSet _dbVersionHistoryList;
    
    protected ArrayList<String> _moduleFormatList;
    protected ArrayList<String> _toneSelectionList;
    
    //  This stuff is used to run automatic updates of the database information.
    protected int _dbTimeoutCounter;
    protected boolean _updateDatabaseNow;
    protected UpdateDatabaseLoop _updateDatabaseLoop;
    EventListenerList _databaseUpdateListeners;

    protected ActivityMonitorLight _guiServerConnectionLight;
    
    protected DatabaseThread _databaseThread;
    
    protected SaneTextField _guiServerVersion;
    protected SaneTextField _guiServerDifxVersion;
    protected SaneTextField _difxStartScript;
    
    //  Used to store information about a specific SMART attribute type.
    //  This includes...
    //   - the name
    //   - whether higher values are better
    //   - a limiting value above or below which the value should be flagged as bad
    //   - a tool tip string that is used when bad values are found
    public class SMARTAttribute {
        String name;
        Boolean highIsGood;
        Integer badValue;
        String toolTip;
    }
    
    //  This is a map of the above, indexed by ID.
    protected Map<Integer, SMARTAttribute> _smartAttributeList;
    
    //  Function to add data to the SMART Attribute list (will change data if the
    //  given ID is already in the list).
    protected void addSMARTAttribute( int id, SMARTAttribute newAtt ) {
        //  If there is no list, create one.
        if ( _smartAttributeList == null )
            _smartAttributeList = new HashMap<Integer, SMARTAttribute>();
        //  Otherwise, find the new ID in the list and delete it if it exists.
        else 
            _smartAttributeList.remove( id );
        //  And make a new entry.
        _smartAttributeList.put( id, newAtt );
    }
    protected void addSMARTAttribute( int id, String name, Boolean highIsGood, Integer badValue, String toolTip ) {
        SMARTAttribute newAtt = new SMARTAttribute();
        newAtt.name = name;
        newAtt.highIsGood = highIsGood;
        newAtt.badValue = badValue;
        newAtt.toolTip = toolTip;
        addSMARTAttribute( id, newAtt );
    }
    
    /*
     * Public function to return a SMARTAttribute structure based on ID.
     */
    public SMARTAttribute smartAttribute( int id ) {
        if ( _smartAttributeList == null )
            return null;
        return _smartAttributeList.get( id );
    }
    
    //  This is a list of environment variables for the guiServer.  They are sent
    //  when the guiServer is connected.
    protected Map<String, String> _guiServerEnvironment;
    
    /*
     * Clear all guiServer environment variables.
     */
    public void clearGuiServerEnvironment() {
        if ( _guiServerEnvironment != null )
            _guiServerEnvironment.clear();
    }
    
    /*
     * Add a guiServer environment variable.  These are stored in a single string
     * that needs to be parsed.
     */
    public void addGuiServerEnvironment( String newEnv ) {
        if ( _guiServerEnvironment == null )
            _guiServerEnvironment = new HashMap<String, String>();
        int eq = newEnv.indexOf( "=" );
        String name = newEnv.substring( 0, eq );
        String val = newEnv.substring( eq + 1, newEnv.length() );
        _guiServerEnvironment.put( name, val );
    }
    
    /*
     * Return the String value of an environment variable.
     */
    public String getGuiServerEnvironmentVariable( String name ) {
        return _guiServerEnvironment.get( name );
    }
    
    /*
     * Class to display a table of environment variables.
     */
    public class EnvironmentVariablesDisplay extends JFrame {

        public EnvironmentVariablesDisplay( int x, int y ) {
            setLookAndFeel();
            this.setLayout( null );
            this.setBounds( x, y, windowConfiguration().environmentVariableDisplayW,
                windowConfiguration().environmentVariableDisplayH );
            this.getContentPane().setLayout( null );
            this.setTitle( "DiFX Host Environment Variables" );
            _this = this;
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    windowConfiguration().environmentVariableDisplayW = _this.getWidth();
                    windowConfiguration().environmentVariableDisplayH = _this.getHeight();
                    newSize();
                }
            });
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentShown( ComponentEvent e ) {
                    newSize();
                }
            });
            _tableModel = new DefaultTableModel();
            _table = new JTable( _tableModel );
            _scrollPane = new JScrollPane( _table );
            this.add( _scrollPane );
            _tableModel.addColumn( "Variable" );        
            _tableModel.addColumn( "Setting" );
            _difxOnly = new ZCheckBox( "DIFX" );
            _difxOnly.setSelected( false );
            _difxOnly.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _difxOnly.setSelected( true );
                    _all.setSelected( false );
                    getData();
                }
            } );
            _difxOnly.setBounds( 20, 20, 100, 25 );
            this.add( _difxOnly );
            _all = new ZCheckBox( "All" );
            _all.setSelected( true );
            _all.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _difxOnly.setSelected( false );
                    _all.setSelected( true );
                    getData();
                }
            } );
            _all.setBounds( 140, 20, 100, 25 );
            this.add( _all );
            getData();
        }

        @Override
        public void setBounds( int x, int y, int w, int h ) {
            super.setBounds( x, y, w, h );
            newSize();
        }

        public void newSize() {
            if ( _scrollPane != null ) {
                int w = this.getContentPane().getSize().width;
                int h = this.getContentPane().getSize().height;
                _scrollPane.setBounds( 0, 60, w, h - 60 );
                _table.updateUI();
            }
        }
        
        public void getData() {
            while ( _tableModel.getRowCount() > 0 )
                _tableModel.removeRow( 0 );
            if ( _guiServerEnvironment != null ) {
                for ( Iterator<String> iter = _guiServerEnvironment.keySet().iterator(); iter.hasNext(); ) {
                    String key = iter.next();
                    boolean useIt = true;
                    if ( _difxOnly.isSelected() ) {
                        if ( key.length() > 4 && key.substring( 0, 4 ).contentEquals( "DIFX" ) )
                            useIt = true;
                        else
                            useIt = false;
                    }
                    if ( useIt )
                        _tableModel.addRow( new Object[]{ key, _guiServerEnvironment.get( key ) } );
                }
            }
            _table.updateUI();
        }
        
        protected EnvironmentVariablesDisplay _this;
        protected JScrollPane _scrollPane;
        protected JTable _table;
        protected DefaultTableModel _tableModel;
        protected ZCheckBox _difxOnly;
        protected ZCheckBox _all;

    }
    
    protected EnvironmentVariablesDisplay _environmentVariablesDisplay;
    
    protected String _difxVersionPreferred;
        
    /*
     * Class to display and edit a list of path/data source pairs.
     */
    public class SourceBasedOnPathDisplay extends JFrame {

        public SourceBasedOnPathDisplay( int x, int y, SystemSettings settings ) {
            _settings = settings;
            setLookAndFeel();
            this.setLayout( null );
            this.setBounds( x, y, windowConfiguration().sourceBasedOnPathDisplayW,
                windowConfiguration().sourceBasedOnPathDisplayH );
            this.getContentPane().setLayout( null );
            this.setTitle( "Source Node Assignments Based On Path" );
            _this = this;
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    windowConfiguration().sourceBasedOnPathDisplayW = _this.getWidth();
                    windowConfiguration().sourceBasedOnPathDisplayH = _this.getHeight();
                    newSize();
                }
            });
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentShown( ComponentEvent e ) {
                    newSize();
                }
            });
            _viewPane = new JPanel();
            _viewPane.setLayout( null );
            _viewPane.setPreferredSize( new Dimension( 500, 500 ) );
            _scrollPane = new JScrollPane( _viewPane );
            this.add( _scrollPane );
            _addButton = new ZButton( "Add" );
            _addButton.setBounds( 20, 15, 100, 25 );
            _addButton.toolTip( "Add a new Path/Source combination to the list.", null );
            _addButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    addItem();
                }
            } );
            this.add( _addButton );
        }
        
        public void showAtPosition( int x, int y ) {
            setBounds( x, y, windowConfiguration().sourceBasedOnPathDisplayW,
                windowConfiguration().sourceBasedOnPathDisplayH );
            setVisible( true );
        }
        
        @Override
        public void setBounds( int x, int y, int w, int h ) {
            newSize();
            super.setBounds( x, y, w, h );
        }

        public void newSize() {
            if ( _scrollPane != null ) {
                int w = this.getContentPane().getSize().width;
                int h = this.getContentPane().getSize().height;
                _scrollPane.setBounds( 0, 50, w, h - 50 );
                if ( _panels != null ) {
                    int width = w - 4;
                    if ( 25 * _panels.size() > h - 50 )
                        width = w - 19;
                    _viewPane.setPreferredSize( new Dimension( width, 25 * _panels.size() ) );
                    _viewPane.setBounds( 0, 0, width, 25 * _panels.size() );
                    int i = 0;
                    for ( Iterator<PanelItem> iter = _panels.iterator(); iter.hasNext(); ) {
                        PanelItem panel = iter.next();
                        panel.setBounds( 0, 25 * i, width, 25 );
                        panel.textField.setBounds( 25 , 0, 3 * ( width - 25 ) / 5, 25 );
                        panel.comboBox.setBounds( 25 + 3 * ( width - 25 ) / 5, 0, 2 * ( width - 25 ) / 5, 25 );
                        ++i;
                    }
                    _viewPane.updateUI();
                }
            }
        }
        
        public PanelItem addItem() {
            PanelItem newPanel = new PanelItem();
            //  Create a new Text Field in which a path can be entered.
            newPanel.textField = new TabCompletedTextField( _settings );
            newPanel.textField.setToolTipText( "Path to associate with a node.  Any file that exists below this\n"
                    + "path will be assigned the selected node as a data source." );
            //  Create a new combo box with all possible data source nodes.
            newPanel.comboBox = new JComboBox();
            newPanel.comboBox.setToolTipText( "Node name to use as the data source for the given path." );
            newPanel.comboBox.setEditable( true );
            for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().processorNodes().children().iterator();
                    iter.hasNext(); ) {
                ProcessorNode thisModule = (ProcessorNode)(iter.next());
                newPanel.comboBox.addItem( thisModule.name() );
            }
            newPanel.setLayout( null );
            if ( _panels == null )
                _panels = new ArrayList<PanelItem>();
            newPanel.setBounds( 0, 25 * _panels.size(), 825, 25 );
            _viewPane.add( newPanel );
            _panels.add( newPanel );
            newPanel.delete = new ZButton( "\u2613" );
            newPanel.delete.setFont( new Font( "Dialog", Font.BOLD, 16 ) );
            newPanel.delete.setMargin( new Insets( 0, 0, 2, 0 ) );
            newPanel.delete.setToolTipText( "Delete this item." );
            final PanelItem deletePanel = newPanel;
            newPanel.delete.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _panels.remove( deletePanel );
                    _viewPane.remove( deletePanel );
                    newSize();
                    _scrollPane.updateUI();
                }
            } );
            newPanel.delete.setBounds( 0, 0, 25, 25 );
            newPanel.add( newPanel.delete );
            newPanel.add( newPanel.textField );
            newPanel.add( newPanel.comboBox );
            newPanel.textField.setBounds( 25, 0, 500, 25 );
            newPanel.comboBox.setBounds( 525, 0, 300, 25 );
            final JComboBox thisBox = newPanel.comboBox;
            newPanel.comboBox.addPopupMenuListener( new PopupMenuListener() {
                public void popupMenuCanceled( PopupMenuEvent e) {
                }
                public void popupMenuWillBecomeInvisible( PopupMenuEvent e) {
                }
                public void popupMenuWillBecomeVisible( PopupMenuEvent e) {
                    //  Check the list of possible data sources against those available.
                    //  Accomodates source nodes that might appear after this item is
                    //  created.
                    for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().processorNodes().children().iterator();
                            iter.hasNext(); ) {
                        ProcessorNode thisModule = (ProcessorNode)(iter.next());
                        boolean found = false;
                        for ( int i = 0; i < thisBox.getItemCount() && ! found; ++i )
                            if ( thisModule.name().contentEquals( (String)thisBox.getItemAt(i) ) )
                                found = true;
                        if ( !found )
                            thisBox.addItem( thisModule.name() );
                    }
                }
            });
            newSize();
            _scrollPane.updateUI();
            return newPanel;
        }
        
        public void addPathNodePair( String path, String node ) {
            PanelItem newPanel = addItem();
            newPanel.textField.setText( path );
            newPanel.comboBox.setSelectedItem( node );
            _scrollPane.updateUI();
        }
        
        public class PanelItem extends JPanel {
            public ZButton delete;
            public TabCompletedTextField textField;
            public JComboBox comboBox;
        }
        
        /*
         * Find the node associated with this file path, if one exists.  The path
         * associated with a node must be fully contained within the full file path.
         * We must be careful to find the most specific path, i.e. the path that
         * contains the largest portion of the full path.
         */
        public String nodeFromPath( String fullPath ) {
            if ( fullPath == null || fullPath.length() == 0 )
                return null;
            String matchNode = null;
            int matchNum = 0;
            if ( _panels != null ) {
                for ( Iterator<PanelItem> iter = _panels.iterator(); iter.hasNext(); ) {
                    PanelItem thisPanel = iter.next();
                    String thisText = thisPanel.textField.getText();
                    if ( thisText != null && thisText.length() > 0 ) {
                        if ( fullPath.indexOf( thisText ) == 0 ) {
                            if ( thisText.length() > matchNum ) {
                                matchNum = thisText.length();
                                matchNode = (String)(thisPanel.comboBox.getSelectedItem());
                            }
                        }
                    }
                }
            }
            return matchNode;
        }
        
        public ArrayList<PanelItem> panels() { return _panels; }
        
        protected SourceBasedOnPathDisplay _this;
        protected JScrollPane _scrollPane;
        protected JPanel _viewPane;
        protected ZButton _addButton;
        protected SystemSettings _settings;
        protected ArrayList<PanelItem> _panels;

    }
    
    public String nodeFromPath( String commonPath ) {        
        if ( _sourceBasedOnPathDisplay == null ) {
            //  Set up a new display (which won't be displayed) and fill it with
            //  the "list" data, if that exists (it came from the XML settings file).
            _sourceBasedOnPathDisplay = new SourceBasedOnPathDisplay( 0, 0, _settings );
            if ( _sourceBasedOnPathList != null ) {
                for ( Iterator<String> iter = _sourceBasedOnPathList.keySet().iterator(); iter.hasNext(); ) {
                    String path = iter.next();
                    _sourceBasedOnPathDisplay.addPathNodePair( path, _sourceBasedOnPathList.get( path ) );
                }
            }
            //  Blow away the list so we don't use it again.
            _sourceBasedOnPathList = null;
        }
        return _sourceBasedOnPathDisplay.nodeFromPath( commonPath );
    }
    
    protected SourceBasedOnPathDisplay _sourceBasedOnPathDisplay;
    protected HashMap<String,String> _sourceBasedOnPathList;
    
    /*
     * Class to display a list of nodes that are permitted to be used as source node.
     */
    public class RestrictedSourceListDisplay extends JFrame {

        public RestrictedSourceListDisplay( int x, int y, SystemSettings settings ) {
            _settings = settings;
            setLookAndFeel();
            this.setLayout( null );
            this.setBounds( x, y, windowConfiguration().restrictedSourceListDisplayW,
                windowConfiguration().restrictedSourceListDisplayH );
            this.getContentPane().setLayout( null );
            this.setTitle( "List of Permitted Source Nodes" );
            _this = this;
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    windowConfiguration().restrictedSourceListDisplayW = _this.getWidth();
                    windowConfiguration().restrictedSourceListDisplayH = _this.getHeight();
                    newSize();
                }
            });
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentShown( ComponentEvent e ) {
                    newSize();
                }
            });
            _viewPane = new JPanel();
            _viewPane.setLayout( null );
            _viewPane.setPreferredSize( new Dimension( 500, 500 ) );
            _scrollPane = new JScrollPane( _viewPane );
            this.add( _scrollPane );
            _addButton = new ZButton( "Add" );
            _addButton.setBounds( 20, 15, 100, 25 );
            _addButton.toolTip( "Add a source node to the list.", null );
            _addButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    addItem();
                }
            } );
            this.add( _addButton );
        }
        
        public void showAtPosition( int x, int y ) {
            setBounds( x, y, windowConfiguration().restrictedSourceListDisplayW,
                windowConfiguration().restrictedSourceListDisplayH );
            setVisible( true );
        }
        
        @Override
        public void setBounds( int x, int y, int w, int h ) {
            newSize();
            super.setBounds( x, y, w, h );
        }

        public void newSize() {
            if ( _scrollPane != null ) {
                int w = this.getContentPane().getSize().width;
                int h = this.getContentPane().getSize().height;
                _scrollPane.setBounds( 0, 50, w, h - 50 );
                if ( _panels != null ) {
                    int width = w - 4;
                    if ( 25 * _panels.size() > h - 50 )
                        width = w - 19;
                    _viewPane.setPreferredSize( new Dimension( width, 25 * _panels.size() ) );
                    _viewPane.setBounds( 0, 0, width, 25 * _panels.size() );
                    int i = 0;
                    for ( Iterator<PanelItem> iter = _panels.iterator(); iter.hasNext(); ) {
                        PanelItem panel = iter.next();
                        panel.setBounds( 0, 25 * i, width, 25 );
                        panel.comboBox.setBounds( 25, 0, width - 25, 25 );
                        ++i;
                    }
                    _viewPane.updateUI();
                }
            }
        }
        
        public PanelItem addItem() {
            PanelItem newPanel = new PanelItem();
            //  Create a new combo box with all possible data source nodes.
            newPanel.comboBox = new JComboBox();
            newPanel.comboBox.setEditable( true );
            for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().processorNodes().children().iterator();
                    iter.hasNext(); ) {
                ProcessorNode thisModule = (ProcessorNode)(iter.next());
                newPanel.comboBox.addItem( thisModule.name() );
            }
            final JComboBox thisBox = newPanel.comboBox;
            newPanel.comboBox.addPopupMenuListener( new PopupMenuListener() {
                public void popupMenuCanceled( PopupMenuEvent e) {
                }
                public void popupMenuWillBecomeInvisible( PopupMenuEvent e) {
                }
                public void popupMenuWillBecomeVisible( PopupMenuEvent e) {
                    //  Check the list of possible data sources against those available.
                    //  Accomodates source nodes that might appear after this item is
                    //  created.
                    for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().processorNodes().children().iterator();
                            iter.hasNext(); ) {
                        ProcessorNode thisModule = (ProcessorNode)(iter.next());
                        boolean found = false;
                        for ( int i = 0; i < thisBox.getItemCount() && ! found; ++i )
                            if ( thisModule.name().contentEquals( (String)thisBox.getItemAt(i) ) )
                                found = true;
                        if ( !found )
                            thisBox.addItem( thisModule.name() );
                    }
                }
            });
            newPanel.setLayout( null );
            if ( _panels == null )
                _panels = new ArrayList<PanelItem>();
            newPanel.setBounds( 0, 25 * _panels.size(), 825, 25 );
            _viewPane.add( newPanel );
            _panels.add( newPanel );
            newPanel.delete = new ZButton( "\u2613" );
            newPanel.delete.setFont( new Font( "Dialog", Font.BOLD, 16 ) );
            newPanel.delete.setMargin( new Insets( 0, 0, 2, 0 ) );
            newPanel.delete.setToolTipText( "Delete this item." );
            final PanelItem deletePanel = newPanel;
            newPanel.delete.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _panels.remove( deletePanel );
                    _viewPane.remove( deletePanel );
                    newSize();
                    _scrollPane.updateUI();
                }
            } );
            newPanel.delete.setBounds( 0, 0, 25, 25 );
            newPanel.add( newPanel.delete );
            newPanel.add( newPanel.comboBox );
            newPanel.comboBox.setBounds( 525, 0, 300, 25 );
            newSize();
            _scrollPane.updateUI();
            return newPanel;
        }
        
        public void addNode( String node ) {
            PanelItem newPanel = addItem();
            newPanel.comboBox.setSelectedItem( node );
            _scrollPane.updateUI();
        }
        
        public class PanelItem extends JPanel {
            public ZButton delete;
            public JComboBox comboBox;
        }
        
        /*
         * See if the given node is on our "permitted" list.
         */
        public boolean sourceNodePermitted( String nodeName ) {
            if ( _panels != null ) {
                for ( Iterator<PanelItem> iter = _panels.iterator(); iter.hasNext(); ) {
                    PanelItem thisPanel = iter.next();
                    String thisText = (String)thisPanel.comboBox.getSelectedItem();
                    if ( thisText != null && thisText.length() > 0 ) {
                        if ( thisText.contentEquals( nodeName ) )
                            return true;
                    }
                }
            }
            return false;
        }
        
        public ArrayList<PanelItem> panels() { return _panels; }
        
        protected RestrictedSourceListDisplay _this;
        protected JScrollPane _scrollPane;
        protected JPanel _viewPane;
        protected ZButton _addButton;
        protected SystemSettings _settings;
        protected ArrayList<PanelItem> _panels;

    }
    
    public boolean sourceNodePermitted( String nodeName ) {        
        if ( _restrictedSourceListDisplay == null ) {
            //  Set up a new display (which won't be displayed) and fill it with
            //  the "list" data, if that exists (it came from the XML settings file).
            _restrictedSourceListDisplay = new RestrictedSourceListDisplay( 0, 0, _settings );
            if ( _restrictedSourceList != null ) {
                for ( Iterator<String> iter = _restrictedSourceList.iterator(); iter.hasNext(); ) {
                    String path = iter.next();
                    _restrictedSourceListDisplay.addNode( path );
                }
            }
            //  Blow away the list so we don't use it again.
            _restrictedSourceList = null;
        }
        return _restrictedSourceListDisplay.sourceNodePermitted( nodeName );
    }
    
    protected RestrictedSourceListDisplay _restrictedSourceListDisplay;
    protected ArrayList<String> _restrictedSourceList;
    
    protected ZCheckBox _useHeadNodeCheck;
    protected ZCheckBox _uniqueDataSource;
    protected ZCheckBox _assignBasedOnPath;
    protected ZButton _pathAssignments;
    protected ZCheckBox _shareDataSourcesBetweenJobs;
    protected ZCheckBox _shareDataSourcesAsProcessors;
    protected NumberBox _threadsPerDataSource;
    protected ZCheckBox _restrictSourcesCheck;
    protected ZButton _viewRestrictedSourceList;
    protected ZCheckBox _allNodesCheck;
    protected ZCheckBox _nodesPerCheck;
    protected NumberBox _nodesPer;
    protected ZCheckBox _allThreadsCheck;
    protected NumberBox _threadsPerNode;
    protected NumberBox _minThreadsPerNode;
    protected ZCheckBox _threadsPerCheck;
    protected ZCheckBox _baselineCheck;
    protected ZCheckBox _jobCheck;
    protected ZCheckBox _sequentialCheck;
    protected ZCheckBox _simultaneousCheck;
    protected NumberBox _maxJobs;
    protected NumberBox _maxSecondsForHardware;
    protected ZCheckBox _useMaxSecondsForHardware;
    protected NumberBox _maxSecondsForProcessing;
    protected ZCheckBox _useMaxSecondsForProcessing;
    protected ZCheckBox _runLogCheck;
    protected FormattedTextField _runLogFile;
    
    public boolean useHeadNodeCheck() { return _useHeadNodeCheck.isSelected(); }
    public boolean restrictSources() { return _restrictSourcesCheck.isSelected(); }
    public boolean assignBasedOnPath() { return _assignBasedOnPath.isSelected(); }
    public boolean uniqueDataSource() { return _uniqueDataSource.isSelected(); }
    public int threadsPerDataSource() { return _threadsPerDataSource.intValue(); }
    public boolean shareDataSourcesBetweenJobs() { return _shareDataSourcesBetweenJobs.isSelected(); }
    public boolean shareDataSourcesAsProcessors() { return _shareDataSourcesAsProcessors.isSelected(); }
    public int nodesPer() { return _nodesPer.intValue(); }
    public boolean nodesPerCheck() { return _nodesPerCheck.isSelected(); }
    public boolean allNodesCheck() { return _allNodesCheck.isSelected(); }
    public boolean allThreadsCheck() { return _allThreadsCheck.isSelected(); }
    public int threadsPerNode() { return _threadsPerNode.intValue(); }
    public int minThreadsPerNode() { return _minThreadsPerNode.intValue(); }
    public boolean threadsPerCheck() { return _threadsPerCheck.isSelected(); }
    public boolean baselineCheck() { return _baselineCheck.isSelected(); }
    public boolean jobCheck() { return _jobCheck.isSelected(); }
    public boolean sequentialCheck() { return _sequentialCheck.isSelected(); }
    public boolean simultaneousCheck() { return _simultaneousCheck.isSelected(); }
    public boolean runLogCheck() { return _runLogCheck.isSelected(); }
    public String runLogFile() { return _runLogFile.getText(); }
    public int maxJobs() { return _maxJobs.intValue(); }
    public int maxSecondsForHardware() { return _maxSecondsForHardware.intValue(); }
    public int maxSecondsForProcessing() { return _maxSecondsForProcessing.intValue(); }
    public boolean useMaxSecondsForHardware() { return _useMaxSecondsForHardware.isSelected(); }
    public boolean useMaxSecondsForProcessing() { return _useMaxSecondsForProcessing.isSelected(); }
    
    protected ZCheckBox _requestAllMessages;
    protected ZCheckBox _requestSpecificMessages;
    protected ZButton _requestMessagesButton;
    protected DiFXMessageListDisplay _difxMessageListDisplay;
    
    protected ZCheckBox _channelAllData;
    
    public boolean channelAllData() {
        if ( _channelAllData.isEnabled() )
            return _channelAllData.isSelected();
        else
            return false;
    }
    
    public void channelAllDataAvailable( boolean newVal ) {
        _channelAllData.setEnabled( newVal );
    }

    protected String _invisibleProcessors;
    protected String _invisibleProcessorCores;
    protected String _invisibleMark5s;

    protected SystemSettings _settings;
    
    /*
     * Class to display a list of nodes that are permitted to be used as source node.
     */
    public class DiFXMessageListDisplay extends JFrame {

        public DiFXMessageListDisplay( int x, int y, SystemSettings settings ) {
            _settings = settings;
            setLookAndFeel();
            this.setLayout( null );
            this.setBounds( x, y, windowConfiguration().requestedMessageListDisplayW,
                windowConfiguration().requestedMessageListDisplayH );
            this.getContentPane().setLayout( null );
            this.setTitle( "Selected DiFX Message List" );
            _this = this;
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    windowConfiguration().requestedMessageListDisplayW = _this.getWidth();
                    windowConfiguration().requestedMessageListDisplayH = _this.getHeight();
                    newSize();
                }
            });
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentShown( ComponentEvent e ) {
                    newSize();
                }
            });
            int ny = 20;
            _alertItem = new ZCheckBox( "DifxAlertMessage" );
            _alertItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _alertItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _alertItem );
            _commandItem = new ZCheckBox( "DifxCommandMessage" );
            _commandItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _commandItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _commandItem );
            _fileOperationItem = new ZCheckBox( "DifxFileOperationMessage" );
            _fileOperationItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _fileOperationItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _fileOperationItem );
            _fileTransferItem = new ZCheckBox( "DifxFileTransferMessage" );
            _fileTransferItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _fileTransferItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _fileTransferItem );
            _getDirectoryItem = new ZCheckBox( "DifxGetDirectoryMessage" );
            _getDirectoryItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _getDirectoryItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _getDirectoryItem );
            _infoItem = new ZCheckBox( "DifxInfoMessage" );
            _infoItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _infoItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _infoItem );
            _loadItem = new ZCheckBox( "DifxLoadMessage" );
            _loadItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _loadItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _loadItem );
            _machinesDefinitionItem = new ZCheckBox( "DifxMachinesDefinitionMessage" );
            _machinesDefinitionItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _machinesDefinitionItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _machinesDefinitionItem );
            _mk5ControlItem = new ZCheckBox( "DifxMk5ControlMessage" );
            _mk5ControlItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _mk5ControlItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _mk5ControlItem );
            _smartItem = new ZCheckBox( "DifxSmartMessage" );
            _smartItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _smartItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _smartItem );
            _statusItem = new ZCheckBox( "DifxStatusMessage" );
            _statusItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _statusItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _statusItem );
            _stopItem = new ZCheckBox( "DifxStopMessage" );
            _stopItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _stopItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _stopItem );
            _vex2DifxRunItem = new ZCheckBox( "DifxVex2DifxRunMessage" );
            _vex2DifxRunItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _vex2DifxRunItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _vex2DifxRunItem );
            _weightItem = new ZCheckBox( "DifxWeightMessage" );
            _weightItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _weightItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _weightItem );
            _mark5StatusItem = new ZCheckBox( "Mark5StatusMessage" );
            _mark5StatusItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _mark5StatusItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _mark5StatusItem );
            _unknownItem = new ZCheckBox( "Unknown Message Type" );
            _unknownItem.setBounds( 10, ny, 300, 25 );
            ny += 25;
            _unknownItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    if ( _requestSpecificMessages.isSelected() )
                        changeCallback();
                }
            });
            this.add( _unknownItem );
            _close = new ZButton( "Close" );
            _close.setBounds( 200, ny + 10, 100, 25 );
            _close.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _this.setVisible( false );
                }
            } );
            this.add( _close );
        }
        
        public void position( int x, int y ) {
            this.setBounds( x, y, windowConfiguration().requestedMessageListDisplayW,
                windowConfiguration().requestedMessageListDisplayH );
        }
        
        /*
         * The user changed which messages are requested from guiServer.  Compose a
         * message to guiServer conveying the new state.
         */
        protected void changeCallback() {
            String messStr = "";
            if ( _requestAllMessages.isSelected() ) {
                messStr = "ALL_MESSAGES\n";
            }
            else {
                messStr = "SELECTED_MESSAGES\n";
                if ( _alertItem.isSelected() )
                    messStr += "ALERT_MESSAGES\n";
                if ( _commandItem.isSelected() )
                    messStr += "COMMAND_MESSAGES\n";
                if ( _fileOperationItem.isSelected() )
                    messStr += "FILEOPERATION_MESSAGES\n";
                if ( _fileTransferItem.isSelected() )
                    messStr += "FILETRANSFER_MESSAGES\n";
                if ( _getDirectoryItem.isSelected() )
                    messStr += "GETDIRECTORY_MESSAGES\n";
                if ( _infoItem.isSelected() )
                    messStr += "INFO_MESSAGES\n";
                if ( _loadItem.isSelected() )
                    messStr += "LOAD_MESSAGES\n";
                if ( _machinesDefinitionItem.isSelected() )
                    messStr += "MACHINESDEFINITION_MESSAGES\n";
                if ( _mk5ControlItem.isSelected() )
                    messStr += "MK5CONTROL_MESSAGES\n";
                if ( _smartItem.isSelected() )
                    messStr += "SMART_MESSAGES\n";
                if ( _statusItem.isSelected() )
                    messStr += "STATUS_MESSAGES\n";
                if ( _stopItem.isSelected() )
                    messStr += "STOP_MESSAGES\n";
                if ( _vex2DifxRunItem.isSelected() )
                    messStr += "VEX2DIFXRUN_MESSAGES\n";
                if ( _weightItem.isSelected() )
                    messStr += "WEIGHT_MESSAGES\n";
                if ( _mark5StatusItem.isSelected() )
                    messStr += "MARK5STATUS_MESSAGES\n";
                if ( _unknownItem.isSelected() )
                    messStr += "UNKNOWN_MESSAGES\n";
            }
            guiServerConnection().sendPacketWithString( _guiServerConnection.MESSAGE_SELECTION_PACKET, messStr );
        }
        
        /*
         * Select the "string version" of a message type.  This is used for the XML file.
         */
        public void selectMessage( String messStr ) {
            if ( messStr.contentEquals( "ALERT_MESSAGES" ) )
                _alertItem.setSelected( true );
            else if ( messStr.contentEquals( "COMMAND_MESSAGES" ) )
                _commandItem.setSelected( true );
            else if ( messStr.contentEquals( "FILEOPERATION_MESSAGES" ) )
                _fileOperationItem.setSelected( true );
            else if ( messStr.contentEquals( "FILETRANSFER_MESSAGES" ) )
                _fileTransferItem.setSelected( true );
            else if ( messStr.contentEquals( "GETDIRECTORY_MESSAGES" ) )
                _getDirectoryItem.setSelected( true );
            else if ( messStr.contentEquals( "INFO_MESSAGES" ) )
                _infoItem.setSelected( true );
            else if ( messStr.contentEquals( "LOAD_MESSAGES" ) )
                _loadItem.setSelected( true );
            else if ( messStr.contentEquals( "MACHINESDEFINITION_MESSAGES" ) )
                _machinesDefinitionItem.setSelected( true );
            else if ( messStr.contentEquals( "MK5CONTROL_MESSAGES" ) )
                _mk5ControlItem.setSelected( true );
            else if ( messStr.contentEquals( "SMART_MESSAGES" ) )
                _smartItem.setSelected( true );
            else if ( messStr.contentEquals( "STATUS_MESSAGES" ) )
                _statusItem.setSelected( true );
            else if ( messStr.contentEquals( "STOP_MESSAGES" ) )
                _stopItem.setSelected( true );
            else if ( messStr.contentEquals( "VEX2DIFX_MESSAGES" ) )
                _vex2DifxRunItem.setSelected( true );
            else if ( messStr.contentEquals( "WEIGHT_MESSAGES" ) )
                _weightItem.setSelected( true );
            else if ( messStr.contentEquals( "MARK5STATUS_MESSAGES" ) )
                _mark5StatusItem.setSelected( true );
            else if ( messStr.contentEquals( "UNKNOWN_MESSAGES" ) )
                _unknownItem.setSelected( true );
        }
        
        /*
         * Produce a list of message types that have been selected - for writing
         * XML.
         */
        public ArrayList<String> selectedMessages() {
            ArrayList<String> newList = new ArrayList<String>();
            if ( _alertItem.isSelected() )
                newList.add( "ALERT_MESSAGES" );
            if ( _commandItem.isSelected() )
                newList.add( "COMMAND_MESSAGES" );
            if ( _fileOperationItem.isSelected() )
                newList.add( "FILEOPERATION_MESSAGES" );
            if ( _fileTransferItem.isSelected() )
                newList.add( "FILETRANSFER_MESSAGES" );
            if ( _getDirectoryItem.isSelected() )
                newList.add( "GETDIRECTORY_MESSAGES" );
            if ( _infoItem.isSelected() )
                newList.add( "INFO_MESSAGES" );
            if ( _loadItem.isSelected() )
                newList.add( "LOAD_MESSAGES" );
            if ( _machinesDefinitionItem.isSelected() )
                newList.add( "MACHINESDEFINITION_MESSAGES" );
            if ( _mk5ControlItem.isSelected() )
                newList.add( "MK5CONTROL_MESSAGES" );
            if ( _smartItem.isSelected() )
                newList.add( "SMART_MESSAGES" );
            if ( _statusItem.isSelected() )
                newList.add( "STATUS_MESSAGES" );
            if ( _stopItem.isSelected() )
                newList.add( "STOP_MESSAGES" );
            if ( _vex2DifxRunItem.isSelected() )
                newList.add( "VEX2DIFXRUN_MESSAGES" );
            if ( _weightItem.isSelected() )
                newList.add( "WEIGHT_MESSAGES" );
            if ( _mark5StatusItem.isSelected() )
                newList.add( "MARK5STATUS_MESSAGES" );
            if ( _unknownItem.isSelected() )
                newList.add( "UNKNOWN_MESSAGES" );
            return newList;
         }
        
        protected DiFXMessageListDisplay _this;
        protected ZCheckBox _alertItem;
        protected ZCheckBox _commandItem;
        protected ZCheckBox _fileOperationItem;
        protected ZCheckBox _fileTransferItem;
        protected ZCheckBox _getDirectoryItem;
        protected ZCheckBox _infoItem;
        protected ZCheckBox _loadItem;
        protected ZCheckBox _machinesDefinitionItem;
        protected ZCheckBox _mk5ControlItem;
        protected ZCheckBox _smartItem;
        protected ZCheckBox _statusItem;
        protected ZCheckBox _stopItem;
        protected ZCheckBox _vex2DifxRunItem;
        protected ZCheckBox _weightItem;
        protected ZCheckBox _mark5StatusItem;
        protected ZCheckBox _unknownItem;
        protected ZButton _close;

    }

    /*
     * This stuff maintains an "active ID list" of unique (integer) job identifiers.
     * The list contains each job that is actively running, and, for each, a list
     * of all the jobs that have been at any time actively running while they were
     * running.
     */
    protected HashMap<Long,ArrayList<Long>> _activeIDList;
    
    /*
     * Add a new active ID to the active ID list.  This ID must first be added to
     * every job already in the list.  Then a new list entry is created for the
     * job itself.
     */
    public void addActiveID( long newID ) {
        //  First make sure there is an active ID list.
        if ( _activeIDList == null )
            _activeIDList = new HashMap<Long,ArrayList<Long>>();
        synchronized ( _activeIDList ) {
            //  Add this ID to each list in the active ID list (this is a new, running
            //  job, so it should be added to the lists of running job for each job that
            //  is already running.
            for ( Iterator<Long> iter = _activeIDList.keySet().iterator(); iter.hasNext(); ) {
                _activeIDList.get( iter.next() ).add( newID );
            }
            //  Then create a new list for this job and add it to the main list.
            _activeIDList.put( newID, new ArrayList<Long>() );
        }
    }
    
    /*
     * Take an active ID out of the list of active IDs and return a list of all other
     * IDs that were active while it was active.
     */
    public ArrayList<Long> endActiveID( long newID ) {
        ArrayList<Long> ret = null;
        //  First make sure there is an active ID list.
        if ( _activeIDList == null )
            _activeIDList = new HashMap<Long,ArrayList<Long>>();
        synchronized ( _activeIDList ) {
            if ( _activeIDList == null )
                ret = _activeIDList.remove( newID );
            }
        return ret;
    }
}
