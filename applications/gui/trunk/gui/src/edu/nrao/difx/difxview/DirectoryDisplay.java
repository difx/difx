/*
 * This window shows the directory listing of a Mark 5 module in table format and allows
 * copying of files from the module.
 */
package edu.nrao.difx.difxview;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import edu.nrao.difx.difxutilities.DiFXCommand;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.DifxGetDirectory;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.event.ComponentEvent;

import java.util.Date;
import java.util.StringTokenizer;

import java.net.SocketTimeoutException;

import mil.navy.usno.widgetlib.Spinner;
import mil.navy.usno.widgetlib.SaneTextField;

import edu.nrao.difx.difxutilities.DiFXCommand_mark5Control;
import edu.nrao.difx.difxutilities.ChannelServerSocket;
import edu.nrao.difx.difxcontroller.AttributedMessageListener;

public class DirectoryDisplay extends JFrame {
    
    public DirectoryDisplay( int x, int y, SystemSettings settings, Mark5Node host, String vsn ) {
        _settings = settings;
        _settings.setLookAndFeel();
        this.setLayout( null );
        this.setBounds( x, y, _settings.windowConfiguration().directoryDisplayW,
                _settings.windowConfiguration().directoryDisplayH );
        this.getContentPane().setLayout( null );
        _this = this;
	this.addComponentListener( new java.awt.event.ComponentAdapter() {
            public void componentResized( ComponentEvent e ) {
                _settings.windowConfiguration().directoryDisplayW = _this.getWidth();
                _settings.windowConfiguration().directoryDisplayH = _this.getHeight();
                newSize();
            }
        });
        this.setTitle( "Directory for " + vsn );
        _vsn = vsn;
        _host = host;
        //  The activity spinner tells us when this widget is doing something.  If nothing
        //  is going on the spinner is invisible.
        _activitySpinner = new Spinner();
        _activitySpinner.setBounds( 10, 10, 30, 30 );
        _activitySpinner.setVisible( false );
        add( _activitySpinner );
        
        //  The activity label tells us what the widget is doing (if anything).
        _activityLabel = new JLabel( "" );
        _activityLabel.setBounds( 20, 20, 200, 25 );
        this.add( _activityLabel );
        
        //  The Mark5 (host) label.  This is set at creation and cannot be changed.
        _hostName = new SaneTextField();
        _hostName.setText( hostName() );
        _hostName.setEditable( false );
        add( _hostName );
        JLabel hostNameLabel = new JLabel( "Mark 5:" );
        hostNameLabel.setBounds( 10, 50, 95, 25 );
        hostNameLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( hostNameLabel );
        
        //  The VSN.  This is set at creation and cannot be changed.
        _vsnName = new SaneTextField();
        _vsnName.setText( vsn );
        _vsnName.setEditable( false );
        add( _vsnName );
        JLabel vsnNameLabel = new JLabel( "VSN Label:" );
        vsnNameLabel.setBounds( 10, 80, 95, 25 );
        vsnNameLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( vsnNameLabel );
        
        //  The full path to the directory (on the Mark5).
        _directoryPath = new SaneTextField();
        _directoryPath.setText( "" );
        _directoryPath.setEditable( false );
        _directoryPath.setToolTipText( "Full path to the directory on " + host + " )." );
        add( _directoryPath );
        JLabel directoryPathLabel = new JLabel( "Full Path:" );
        directoryPathLabel.setBounds( 10, 110, 95, 25 );
        directoryPathLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( directoryPathLabel );
        
        //  This is the date the Mark5 thinks this directory was created.
        _creationDate = new SaneTextField();
        _creationDate.setText( "" );
        _creationDate.setEditable( false );
        _creationDate.setToolTipText( "Date this directory was created (according to " + host + " )." );
        add( _creationDate );
        JLabel creationDateLabel = new JLabel( "Created:" );
        creationDateLabel.setBounds( 10, 140, 95, 25 );
        creationDateLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( creationDateLabel );
        
        //  The directory version comes from the directory header.
        _dirVersion = new SaneTextField();
        _dirVersion.setText( "" );
        _dirVersion.setEditable( false );
        _dirVersion.setToolTipText( "0 for old style (pre-mark5-memo 81), otherwise version number for mark5-memo 81." );
        add( _dirVersion );
        JLabel dirVersionLabel = new JLabel( "Version:" );
        dirVersionLabel.setBounds( 10, 170, 95, 25 );
        dirVersionLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( dirVersionLabel );
        
        //  Bank for this VSN in the Mark 5 - more information from the header.
        _bank = new SaneTextField();
        _bank.setText( "" );
        _bank.setEditable( false );
        _bank.setToolTipText( "Mark 5 bank (A or B)." );
        add( _bank );
        _bankLabel = new JLabel( "Bank:" );
        _bankLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( _bankLabel );
        
        //  Bank for this VSN in the Mark 5 - more information from the header.
        _scans = new SaneTextField();
        _scans.setText( "" );
        _scans.setEditable( false );
        _scans.setToolTipText( "Number of scans on this VSN (from directory header)." );
        add( _scans );
        _scansLabel = new JLabel( "Scans:" );
        _scansLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( _scansLabel );
        
        //  Signature for this VSN (not sure what this is) from the header.
        _signature = new SaneTextField();
        _signature.setText( "" );
        _signature.setEditable( false );
        _signature.setToolTipText( "Signature this VSN (from directory header)." );
        add( _signature );
        _signatureLabel = new JLabel( "Signature:" );
        _signatureLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( _signatureLabel );
        
        //  Mode Name (not sure what this is) from the header.
        _modeName = new SaneTextField();
        _modeName.setText( "" );
        _modeName.setEditable( false );
        _modeName.setToolTipText( "Mode Name (from directory header)." );
        add( _modeName );
        _modeNameLabel = new JLabel( "Mode Name:" );
        _modeNameLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( _modeNameLabel );
        
        //  Fast (not sure what this is) from the header.
        _fast = new SaneTextField();
        _fast.setText( "" );
        _fast.setEditable( false );
        _fast.setToolTipText( "Fast - Yes or blank (from directory header)." );
        add( _fast );
        _fastLabel = new JLabel( "Fast:" );
        _fastLabel.setHorizontalAlignment( JLabel.RIGHT );
        add( _fastLabel );
        
        //  The Cancel button stops (or tries to) whatever activity the widget is currently
        //  engaged in (getting directories, saving files).  The button is made invisible
        //  when there is nothing running.
        _cancelButton = new JButton( "Cancel" );
        _cancelButton.setToolTipText( "Cancel current activity." );
        _cancelButton.setVisible( false );
        _cancelButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                cancelActivity();
            }
        } );
        this.add( _cancelButton );
        
        //  The "refresh" button obtains the existing directory as the Mark5 unit sees it.  It
        //  DOES NOT regenerate it (so it should be quick!).
        _refreshButton = new JButton( "Refresh Directory" );
        _refreshButton.setToolTipText( "Download the VSN directory as it currently exists." );
        _refreshButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                getDirectory();
            }
        } );
        this.add( _refreshButton );
        
        //  The "refresh" button obtains the existing directory as the Mark5 unit sees it.  It
        //  DOES NOT regenerate it (so it should be quick!).
        _generateDirectoryButton = new JButton( "Generate Directory" );
        _generateDirectoryButton.setToolTipText( "Generate a new directory for this VSN.  This takes a while!" );
        _generateDirectoryButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateDirectory();
            }
        } );
        this.add( _generateDirectoryButton );
        
        //  The "refresh" button obtains the existing directory as the Mark5 unit sees it.  It
        //  DOES NOT regenerate it (so it should be quick!).
        _createFileButton = new JButton( "Create File" );
        _createFileButton.setToolTipText( "Create a file from selected scans." );
        _createFileButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                createFile();
            }
        } );
        this.add( _createFileButton );
        
        _tableModel = new DefaultTableModel();
        _table = new JTable( _tableModel );
        _scrollPane = new JScrollPane( _table );
        this.add( _scrollPane );
        _tableModel.addColumn( "Start Byte" );        
        _tableModel.addColumn( "Length (bytes)" );        
        _tableModel.addColumn( "MJD" );
        _tableModel.addColumn( "sec" );
        _tableModel.addColumn( "Frame # in sec" );
        _tableModel.addColumn( "Frames/sec" );
        _tableModel.addColumn( "Duration" );
        _tableModel.addColumn( "Frame Bytes" );
        _tableModel.addColumn( "Frame Offset" );
        _tableModel.addColumn( "Tracks" );
        _tableModel.addColumn( "Format" );
        _tableModel.addColumn( "Scan Name" );
        _tableModel.addColumn( "Comments" );
        newSize();
        
        //  Some alert messages are produced by DiFX when running "get directory" and
        //  "create file" operations.  We want to trap these.
        _settings.difxMessageProcessor().addDifxAlertMessageListener( new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processDifxAlertMessage( difxMsg );
            }
        } );

        
        //  When this thing is first created, get the directory.
        getDirectory();
        
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
            _activityLabel.setBounds( 55, 15, w - 180, 25 );
            _hostName.setBounds( 110, 50, ( w - 390 )/2, 25 );
            _vsnName.setBounds( 110, 80, ( w - 390 )/2, 25 );
            _directoryPath.setBounds( 110, 110, ( w - 390 )/2, 25 );
            _creationDate.setBounds( 110, 140, ( w - 390 )/2, 25 );
            _dirVersion.setBounds( 110, 170, ( w - 390 )/2, 25 );
            _bank.setBounds( 220 + ( w - 390 )/2, 50, ( w - 390 )/2, 25 );
            _bankLabel.setBounds( 120 + ( w - 390 )/2, 50, 95, 25 );
            _scans.setBounds( 220 + ( w - 390 )/2, 80, ( w - 390 )/2, 25 );
            _scansLabel.setBounds( 120 + ( w - 390 )/2, 80, 95, 25 );
            _signature.setBounds( 220 + ( w - 390 )/2, 110, ( w - 390 )/2, 25 );
            _signatureLabel.setBounds( 120 + ( w - 390 )/2, 110, 95, 25 );
            _modeName.setBounds( 220 + ( w - 390 )/2, 140, ( w - 390 )/2, 25 );
            _modeNameLabel.setBounds( 120 + ( w - 390 )/2, 140, 95, 25 );
            _fast.setBounds( 220 + ( w - 390 )/2, 170, ( w - 390 )/2, 25 );
            _fastLabel.setBounds( 120 + ( w - 390 )/2, 170, 95, 25 );
            _cancelButton.setBounds( w - 160, 10, 150, 25 );
            _refreshButton.setBounds( w - 160, 50, 150, 25 );
            _generateDirectoryButton.setBounds( w - 160, 80, 150, 25 );
            _createFileButton.setBounds( w - 160, 110, 150, 25 );
            _scrollPane.setBounds( 0, 210, w, h - 180 );
        }
    }

    public String hostName() { return _host.name(); }
    public String vsn() { return _vsn; }
    
    public void getDirectory() {
        if ( !_lockActivityLabel ) {
            _activitySpinner.setVisible( true );
            _activitySpinner.ok();
            _activityLabel.setText( "Downloading directory for " + _vsn + " from " + hostName() );
        }

        //  Construct a Get Directory command.
        DiFXCommand command = new DiFXCommand( _settings );
        command.header().setType( "DifxGetDirectory" );
        command.mpiProcessId( "-1" );
        command.identifier( "gui" );
        int monitorPort = 0;

        // Specifics of the get directory command.
        DifxGetDirectory cmd = command.factory().createDifxGetDirectory();
        cmd.setMark5( hostName() );
        cmd.setVsn( _vsn );
        cmd.setDifxVersion( _settings.difxVersion() );
        cmd.setGenerateNew( 0 );

        // If we are using the TCP connection, set the address and port for diagnostic
        // reporting.
        if ( _settings.sendCommandsViaTCP() ) {
            cmd.setAddress( _settings.guiServerConnection().myIPAddress() );
            monitorPort = _settings.newDifxTransferPort( 0, 100, true, true );
            cmd.setPort( monitorPort );
        }
        
        //  Set up a monitor thread to collect and interpret diagnostic messages from
        //  guiServer as it sets up the threads and machine files.
        GetDirectoryMonitor monitor = null;
        if ( _settings.sendCommandsViaTCP() ) {
            monitor = new GetDirectoryMonitor( monitorPort );
            monitor.start();
        }
        
        // -- Create the XML defined messages and process through the system
        command.body().setDifxGetDirectory( cmd );
        try {
            //command.sendPacket( _settings.guiServerConnection().COMMAND_PACKET );
            command.send();
        } catch ( java.net.UnknownHostException e ) {
            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null,
                    e.getMessage() );  //BLAT should be a pop-up
        }    
    }
    
    /*
     * This thread opens and monitors a TCP socket for diagnostic reports from the
     * guiServer as it sets up thread and machine files.  The opposite side of this
     * communication link is in the file "guiServer/src/machinesDefinition.cpp" in
     * the DiFX source tree.
     */
    protected class GetDirectoryMonitor extends Thread {
        
        public GetDirectoryMonitor( int port ) {
            _port = port;
        }
        
        /*
         * These packet types are sent by the "JobMonitorConnection" class in the
         * guiServer application on the DiFX host.
         */
        protected final int GETDIRECTORY_STARTED                = 301;
        protected final int GETDIRECTORY_COMPLETED              = 302;
        protected final int GETDIRECTORY_FULLPATH               = 303;
        protected final int GETDIRECTORY_FAILED                 = 304;
        protected final int MPIRUN_ERROR                        = 305;
        protected final int NO_ENVIRONMENT_VARIABLE             = 306;
        protected final int GETDIRECTORY_DATE                   = 307;
        protected final int FILE_NOT_FOUND                      = 308;
        protected final int GETDIRECTORY_FILESTART              = 309;
        protected final int GETDIRECTORY_FILEDATA               = 310;
        protected final int GENERATE_DIRECTORY_STARTED          = 311;
        protected final int GENERATE_DIRECTORY_INFO             = 312;
        protected final int GENERATE_DIRECTORY_COMPLETED        = 313;
        protected final int GENERATE_DIRECTORY_ERRORS           = 314;
        
        @Override
        public void run() {
            //  Open a new server socket and await a connection.  The connection
            //  will timeout after a given number of seconds (nominally 10).
            try {
                ChannelServerSocket ssock = new ChannelServerSocket( _port, _settings );
                ssock.setSoTimeout( 10000 );  //  timeout is in millisec
                try {
                    ssock.accept();
                    //  Loop collecting diagnostic packets from the guiServer.  These
                    //  are identified by an initial integer, and then are followed
                    //  by a data length, then data.
                    boolean connected = true;
                    while ( connected ) {
                        //  Read the packet type as an integer.  The packet types
                        //  are defined above (within this class).
                        int packetType = ssock.readInt();
                        //  Read the size of the incoming data (bytes).
                        int packetSize = ssock.readInt();
                        //  Read the data (as raw bytes)
                        byte [] data = null;
                        if ( packetSize > 0 ) {
                            data = new byte[packetSize];
                            ssock.readFully( data, 0, packetSize );
                        }
                        //  Interpret the packet type.
                        if ( packetType == GETDIRECTORY_STARTED ) {
                            connected = true;
                            //  Clear existing data.
                            _tableModel.getDataVector().clear();
                            _directoryPath.setText( "" );
                            _creationDate.setText( "" );
                            _dirVersion.setText( "" );
                            _bank.setText( "" );
                            _scans.setText( "" );
                            _signature.setText( "" );
                            _modeName.setText( "" );
                            _fast.setText( "" );
                        }
                        else if ( packetType == GETDIRECTORY_COMPLETED ) {
                            if ( !_lockActivityLabel ) {
                                _activityLabel.setText( "" );
                                _activitySpinner.setVisible( false );
                            }
                            connected = false;
                        }
                        else if ( packetType == GETDIRECTORY_FULLPATH ) {
                            _directoryPath.setText( new String( data ) );
                        }
                        else if ( packetType == GETDIRECTORY_FAILED ) {
                            _activitySpinner.error();
                        }
                        else if ( packetType == MPIRUN_ERROR ) {
                            if ( !_lockActivityLabel )
                                _activityLabel.setText( "mpirun error: " + new String( data ) );
                        }
                        else if ( packetType == NO_ENVIRONMENT_VARIABLE ) {
                            if ( !_lockActivityLabel )
                                _activityLabel.setText( "No MARK5_DIR_PATH environment variable on " + hostName() );
                        }
                        else if ( packetType == GENERATE_DIRECTORY_STARTED ) {
                            if ( !_lockActivityLabel )
                                _activityLabel.setText( "Generating directory...this may take a while" );
                        }
                        else if ( packetType == GENERATE_DIRECTORY_INFO ) {
                            if ( !_lockActivityLabel )
                                _activityLabel.setText( new String( data ) );
                        }
                        else if ( packetType == GENERATE_DIRECTORY_COMPLETED ) {
                            if ( !_lockActivityLabel )
                                _activityLabel.setText( "Generating directory complete!" );
                        }
                        else if ( packetType == GENERATE_DIRECTORY_ERRORS ) {
                            if ( !_lockActivityLabel )
                                _activityLabel.setText( "Generating directory complete with errors." );
                        }
                        else if ( packetType == GETDIRECTORY_DATE ) {
                            _creationDate.setText( new String( data ) );
                        }
                        else if ( packetType == FILE_NOT_FOUND ) {
                            if ( !_lockActivityLabel )
                                _activityLabel.setText( "File \"" + _directoryPath.getText() + "\" was not found on " + hostName() );
                        }
                        else if ( packetType == GETDIRECTORY_FILESTART ) {
                            _absorbHeader = true;
                        }
                        else if ( packetType == GETDIRECTORY_FILEDATA ) {
                            // The first line is a header containing general information.
                            if ( _absorbHeader ) {
                                _absorbHeader = false;
                                int i = 0;
                                StringTokenizer strtok = new StringTokenizer( new String( data ) );
                                while ( strtok.hasMoreTokens() ) {
                                    //  The header contains a bunch of different items that we use to fill
                                    //  fields above our table.
                                    if ( i == 0 ) //  ignore the label - we know it already
                                        strtok.nextToken();
                                    else if ( i == 1 )
                                        _scans.setText( strtok.nextToken() );
                                    else if ( i == 2 )
                                        _bank.setText( strtok.nextToken() );
                                    else if ( i == 3 )
                                        _signature.setText( strtok.nextToken() );
                                    else if ( i == 4 )
                                        _dirVersion.setText( strtok.nextToken() );
                                    else if ( i == 5 )
                                        _modeName.setText( strtok.nextToken() );
                                    else if ( i == 6 )
                                        _fast.setText( "Yes" );
                                    ++i;
                                }
                            }
                            else {
                                Object [] dataItems = new Object[13];
                                int i = 0;
                                String rem = new String( "" );
                                StringTokenizer strtok = new StringTokenizer( new String( data ) );
                                while ( strtok.hasMoreElements() ) {
                                    if ( i < 12 )
                                        dataItems[i] = strtok.nextElement();
                                    else
                                        rem += strtok.nextElement() + " ";
                                    ++i;
                                }
                                dataItems[12] = (Object)rem;
                                _tableModel.insertRow( _tableModel.getRowCount(), dataItems );
                            }
                        }
                    }
                } catch ( SocketTimeoutException e ) {
                }
                ssock.close();
            } catch ( java.io.IOException e ) {
                e.printStackTrace();
            }
            _settings.releaseTransferPort( _port );
            _cancelActivated = false;
            _lockActivityLabel = false;
            _cancelButton.setVisible( false );
            _refreshButton.setEnabled( true );
            _generateDirectoryButton.setEnabled( true );
            _createFileButton.setEnabled( true );
            _runningGetDirectory = false;
        }
        
        protected int _port;
        
    }
    
    /*
     * Generate a new directory for the current VSN.  This is a time-consuming process,
     * so a warning is issued.
     */
    public void generateDirectory() {
        int n = JOptionPane.showOptionDialog( this, "Generating directory for " + _vsn +".\nThis can take a long time - do you wish to continue?",
                "Generate Directory for " + _vsn, JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, null, null, null );
        if ( n == 0 ) {
//            GenerateDirectoryThread generateDirectoryThread = new GenerateDirectoryThread();
//            generateDirectoryThread.start();
            if ( !_lockActivityLabel ) {
                _runningGetDirectory = true;
                _activitySpinner.setVisible( true );
                _activitySpinner.ok();
                _activityLabel.setText( "Generating a new directory for " + _vsn + " on " + hostName() );
                //  Lock other buttons and make the "cancel" button visible.
                _cancelActivated = false;
                _lockActivityLabel = true;
                _cancelButton.setVisible( true );
                _refreshButton.setEnabled( false );
                _generateDirectoryButton.setEnabled( false );
                _createFileButton.setEnabled( false );
            }

            //  Construct a Get Directory command.
            DiFXCommand command = new DiFXCommand( _settings );
            command.header().setType( "DifxGetDirectory" );
            command.mpiProcessId( "-1" );
            command.identifier( "gui" );
            int monitorPort = 0;

            // Specifics of the get directory command.
            DifxGetDirectory cmd = command.factory().createDifxGetDirectory();
            cmd.setMark5( hostName() );
            cmd.setVsn( _vsn );
            cmd.setDifxVersion( _settings.difxVersion() );
            cmd.setGenerateNew( 1 );

            // If we are using the TCP connection, set the address and port for diagnostic
            // reporting.
            if ( _settings.sendCommandsViaTCP() ) {
                cmd.setAddress( _settings.guiServerConnection().myIPAddress() );
                monitorPort = _settings.newDifxTransferPort( 0, 100, true, true );
                cmd.setPort( monitorPort );
            }

            //  Set up a monitor thread to collect and interpret diagnostic messages from
            //  guiServer as it sets up the threads and machine files.
            GetDirectoryMonitor monitor = null;
            if ( _settings.sendCommandsViaTCP() ) {
                monitor = new GetDirectoryMonitor( monitorPort );
                monitor.start();
            }

            // -- Create the XML defined messages and process through the system
            command.body().setDifxGetDirectory( cmd );
            try {
                //command.sendPacket( _settings.guiServerConnection().COMMAND_PACKET );
                command.send();
            } catch ( java.net.UnknownHostException e ) {
                java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null,
                        e.getMessage() );  //BLAT should be a pop-up
            }            
        }
    }
    
    /*
     * Thread used while generating a directory.
     */
    protected class GenerateDirectoryThread extends Thread {
        
        @Override
        public void run() {
            //  Set up buttons, spinners, etc.
            _runningGetDirectory = true;
            _activitySpinner.setVisible( true );
            _activitySpinner.ok();
            _activityLabel.setText( "Generating a new directory for " + _vsn + " on " + hostName() );
            //  Lock other buttons and make the "cancel" button visible.
            _cancelActivated = false;
            _lockActivityLabel = true;
            _cancelButton.setVisible( true );
            _refreshButton.setEnabled( false );
            _generateDirectoryButton.setEnabled( false );
            _createFileButton.setEnabled( false );
            //  Send the command to generate the directory.
            String commandStr = "getdirA";
            if ( _vsn.trim().equalsIgnoreCase( _host.bankBVSN().trim() ) )
                commandStr = "getdirB";
            DiFXCommand_mark5Control command = new DiFXCommand_mark5Control( commandStr, hostName(), _settings, false );
            try {
                command.send();
                //  Sleep for a bit to give this command a chance to take effect.
                int i = 0;
                while ( !_cancelActivated && i < 5 ) {
                    ++i;
                    try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                }
                //  Loop while the Mark5 status is "GetDirectory".  Allow the user to cancel
                //  this activity.
                getDirectory();
                while ( !_cancelActivated && _host.currentState().trim().equalsIgnoreCase( "GetDirectory" ) ) {
                    try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                    getDirectory();
                }
                try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                //  Reset buttons, spinners, etc. based on whether the task completed.
                if ( _cancelActivated ) {
                    _activityLabel.setText( "Generate Directory Cancelled" );
                    _activitySpinner.error();
                }
                else {
                    _activitySpinner.setVisible( false );
                    _activityLabel.setText( "" );
                }
            } catch ( Exception e ) {
                _activitySpinner.error();
                _activityLabel.setText( "Error enountered issuing mark5Control \"" + commandStr + "\": " + e.getMessage() );
            }
            _cancelActivated = false;
            _lockActivityLabel = false;
            _cancelButton.setVisible( false );
            _refreshButton.setEnabled( true );
            _generateDirectoryButton.setEnabled( true );
            _createFileButton.setEnabled( true );
            _runningGetDirectory = false;
        }
        
    }
    
    /*
     * Dump highlighted scans to a file.  The user is prompted for a filename.
     */
    public void createFile() {
        int[] rows = _table.getSelectedRows();
        if ( rows.length > 0 ) {
            _createFileDestination = "";
            _createFileDestination = JOptionPane.showInputDialog( this, "Specify a destination directory for the scans\n"
                    + "(this path must be writable by " + _settings.difxControlUser() + " on " + hostName() + "):", "Specify Destination", 
                    JOptionPane.QUESTION_MESSAGE );
            _createFileScans = " " + ( rows[0] + 1);
            if ( rows.length > 1 )
                _createFileScans += "-" + ( rows[rows.length-1] + 1);
            int n = JOptionPane.showOptionDialog( this, "Creating file \"" + _createFileDestination + "\"."
                    + "\nMark5: " + hostName() 
                    + "\nVSN: " + _vsn
                    + "\nScan(s): " + _createFileScans
                    + "\nThis can take a long time - do you wish to continue?",
                    "Copy Scans from " + _vsn, JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, null, null, null );
            if ( n == 0 ) {
                CreateFileThread createFileThread = new CreateFileThread();
                createFileThread.start();
            }
        }
        else {
            JOptionPane.showMessageDialog( this, "No scans have been selected from the directory listing.",
                    "No scans selected", JOptionPane.WARNING_MESSAGE );
        }
    }
    
    /*
     * Thread used to create a file from scans.
     */
    protected class CreateFileThread extends Thread {
        
        @Override
        public void run() {
            //  Set up buttons, spinners, etc.
            _runningCreateFile = true;
            _activitySpinner.setVisible( true );
            _activitySpinner.ok();
            _activityLabel.setText( "Creating files in \"" + _createFileDestination + "\" from scans " + _createFileScans + " on " + _vsn );
            //  Lock other buttons and make the "cancel" button visible.
            _cancelActivated = false;
            _lockActivityLabel = true;
            _cancelButton.setVisible( true );
            _refreshButton.setEnabled( false );
            _generateDirectoryButton.setEnabled( false );
            _createFileButton.setEnabled( false );
            //  Send the command to generate the directory.
            String commandStr = "copy A";
            if ( _vsn.trim().equalsIgnoreCase( _host.bankBVSN().trim() ) )
                commandStr = "copy B";
            commandStr += _createFileScans + " " + _createFileDestination;
            DiFXCommand_mark5Control command = new DiFXCommand_mark5Control( commandStr, hostName(), _settings, false );
            try {
                command.send();
                //  Sleep for a bit to give this command a chance to take effect.
                int i = 0;
                while ( !_cancelActivated && i < 5 ) {
                    ++i;
                    try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                }
                //  Loop while the Mark5 status is "Copy".  Allow the user to cancel
                //  this activity.  The Mark5 status will actually swap between "Copy" and
                //  "Idle" while it is copying.  This "_stillCopy" business tries to make
                //  certain that the Mark5 is done copying.
                int _stillCopy = 15;
                while ( !_cancelActivated && _stillCopy > 0 ) {
                    --_stillCopy;
                    if ( _host.currentState().trim().equalsIgnoreCase( "Copy" ) )
                        _stillCopy = 15;
                    try { Thread.sleep( 100 ); } catch ( Exception e ) {}
                }
                try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                //  Reset buttons, spinners, etc. based on whether the task completed.
                if ( _cancelActivated ) {
                    _activityLabel.setText( "Create File Operation Cancelled" );
                    _activitySpinner.error();
                }
                else {
                    _activitySpinner.setVisible( false );
                    _activityLabel.setText( "" );
                }
            } catch ( Exception e ) {
                _activitySpinner.error();
                _activityLabel.setText( "Error enountered issuing mark5Control \"" + commandStr + "\": " + e.getMessage() );
            }
            _cancelActivated = false;
            _lockActivityLabel = false;
            _cancelButton.setVisible( false );
            _refreshButton.setEnabled( true );
            _generateDirectoryButton.setEnabled( true );
            _createFileButton.setEnabled( true );
            _runningCreateFile = false;
        }
        
    }
    
    /*
     * Cancel the above activities.
     */
    public void cancelActivity() {
        _cancelActivated = true;
        if ( _runningGetDirectory ) {
            DiFXCommand_mark5Control command = new DiFXCommand_mark5Control( "stopdir", hostName(), _settings, false );
            try {
                command.send();
            } catch ( Exception e ) {
                _activitySpinner.error();
                _activityLabel.setText( "Error enountered issuing mark5Control \"stopdir\": " + e.getMessage() );
            }
        }
        if ( _runningCreateFile ) {
            DiFXCommand_mark5Control command = new DiFXCommand_mark5Control( "stopcopy", hostName(), _settings, false );
            try {
                command.send();
            } catch ( Exception e ) {
                _activitySpinner.error();
                _activityLabel.setText( "Error enountered issuing mark5Control \"stopcopy\": " + e.getMessage() );
            }
        }
    }
    
    /*
     * Process a difx alert message which may be caused by a "get directory" (mk5dir) or
     * "create file" (mk5cp) operation.  
     */
    public void processDifxAlertMessage( DifxMessage difxMsg ) {
        //  We are only interested in messages from this Mark5...
        if ( difxMsg.getHeader().getFrom().equalsIgnoreCase( hostName() ) ) {
            if ( difxMsg.getHeader().getIdentifier().equalsIgnoreCase( "mk5dir" ) ) {
                if ( _runningGetDirectory )
                    _activityLabel.setText( difxMsg.getBody().getDifxAlert().getAlertMessage() );
            }
            if ( difxMsg.getHeader().getIdentifier().equalsIgnoreCase( "mk5cp" ) ) {
                if ( _runningCreateFile )
                    _activityLabel.setText( difxMsg.getBody().getDifxAlert().getAlertMessage() );
            }
        }
    }   
    
    protected boolean _absorbHeader;
    protected SystemSettings _settings;
    protected DirectoryDisplay _this;
    protected Mark5Node _host;
    protected String _vsn;
    protected JButton _refreshButton;
    protected JScrollPane _scrollPane;
    protected JTable _table;
    protected DefaultTableModel _tableModel;
    protected Date _dateLabelDate;
    protected Spinner _activitySpinner;
    protected JLabel _activityLabel;
    protected SaneTextField _directoryPath;
    protected SaneTextField _vsnName;
    protected SaneTextField _hostName;
    protected SaneTextField _creationDate;
    protected SaneTextField _dirVersion;
    protected SaneTextField _bank;
    protected JLabel _bankLabel;
    protected SaneTextField _scans;
    protected JLabel _scansLabel;
    protected SaneTextField _signature;
    protected JLabel _signatureLabel;
    protected SaneTextField _modeName;
    protected JLabel _modeNameLabel;
    protected SaneTextField _fast;
    protected JLabel _fastLabel;
    protected JButton _cancelButton;
    protected JButton _generateDirectoryButton;
    protected JButton _createFileButton;
    protected boolean _cancelActivated;
    protected boolean _lockActivityLabel;
    protected boolean _runningGetDirectory;
    protected boolean _runningCreateFile;
    protected String _createFileDestination;
    protected String _createFileScans;

}
