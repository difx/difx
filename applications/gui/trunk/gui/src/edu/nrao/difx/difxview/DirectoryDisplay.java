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
import javax.swing.JDialog;

import edu.nrao.difx.difxutilities.DiFXCommand;
import edu.nrao.difx.difxutilities.DiFXCommand_sendFile;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.DifxGetDirectory;
import edu.nrao.difx.xmllib.difxmessage.DifxMark5Copy;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.event.ComponentEvent;

import java.util.Iterator;
import java.util.Date;
import java.util.StringTokenizer;
import java.util.Vector;

import java.net.SocketTimeoutException;

import mil.navy.usno.widgetlib.Spinner;
import mil.navy.usno.widgetlib.SaneTextField;
import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.ZButton;

import edu.nrao.difx.difxutilities.DiFXCommand_mark5Control;
import edu.nrao.difx.difxutilities.ChannelServerSocket;
import edu.nrao.difx.difxcontroller.AttributedMessageListener;
import edu.nrao.difx.difxutilities.TabCompletedTextField;

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
        _cancelButton = new ZButton( "Cancel" );
        _cancelButton.setToolTipText( "Cancel (or attempt to cancel) the current activity." );
        _cancelButton.setVisible( false );
        _cancelButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                cancelActivity();
            }
        } );
        this.add( _cancelButton );
        
        //  The "refresh" button obtains the existing directory as the Mark5 unit sees it.  It
        //  DOES NOT regenerate it (so it should be quick!).
        _refreshButton = new ZButton( "Refresh Directory" );
        _refreshButton.setToolTipText( "Download the VSN directory as it currently exists." );
        _refreshButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                getDirectory();
            }
        } );
        this.add( _refreshButton );
        
        //  Generate a new directory for this VSN.
        _generateDirectoryButton = new ZButton( "Generate Directory" );
        _generateDirectoryButton.setToolTipText( "Generate a new directory for this VSN.  This takes a while!" );
        _generateDirectoryButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                generateDirectory();
            }
        } );
        this.add( _generateDirectoryButton );
        
        //  Create files out of selected scans.
        _createFileButton = new ZButton( "Create File" );
        _createFileButton.setToolTipText( "Create a file from selected scans." );
        _createFileButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                createFile();
            }
        } );
        this.add( _createFileButton );
        
        //  Delete selected lines from the directory file.
        _removeEntriesButton = new ZButton( "Remove Entries" );
        _removeEntriesButton.setToolTipText( "Remove the selected lines from the directory file.\n"
                + "The file is permanently changed - however you can rebuild\n"
                + "the original using the \"Generate Directory\" button." );
        _removeEntriesButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                removeEntries();
            }
        } );
        this.add( _removeEntriesButton );
        
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
            _removeEntriesButton.setBounds( w - 160, 140, 150, 25 );
            _scrollPane.setBounds( 0, 210, w, h - 210 );
        }
    }

    public String hostName() { return _host.name(); }
    public String vsn() { return _vsn; }
    
    public void getDirectory() {
        _activitySpinner.setVisible( true );
        _activitySpinner.ok();
        _activityLabel.setText( "Downloading directory for " + _vsn + " from " + hostName() );
        _fileContents = null;

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
            _watchingScans = false;
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
                            _watchingScans = false;
                            _activitySpinner.setVisible( false );
                            _activityLabel.setText( "" );
                            connected = false;
                        }
                        else if ( packetType == GETDIRECTORY_FULLPATH ) {
                            _directoryPath.setText( new String( data ) );
                        }
                        else if ( packetType == GETDIRECTORY_FAILED ) {
                            _activitySpinner.error();
                        }
                        else if ( packetType == MPIRUN_ERROR ) {
                            if ( data != null && data.length > 0 )
                                _activityLabel.setText( "mpirun error: " + new String( data ) );
                        }
                        else if ( packetType == NO_ENVIRONMENT_VARIABLE ) {
                            _activityLabel.setText( "No MARK5_DIR_PATH environment variable on " + hostName() );
                        }
                        else if ( packetType == GENERATE_DIRECTORY_STARTED ) {
                            _activityLabel.setText( "Generating directory...this may take a while" );
                        }
                        else if ( packetType == GENERATE_DIRECTORY_INFO ) {
                            //  Try to pull out the number of scans in the data.  This may not work perfectly!
                            if ( data != null && data.length > 0 ) {
                                String str = new String( data );
                                if ( str.contains( "Number of scans" ) ) {
                                    _numScans = Integer.parseInt( str.substring( str.indexOf( "=" ) + 1 ).trim() );
                                    _activityLabel.setText( "Generating directory of " + _numScans + " scans." );
                                    _watchingScans = true;
                                    //  Start a thread to monitor the scans being worked on.  This can be
                                    //  used to (hopefully) create useful messages.
                                    Thread scanThread = new Thread() {
                                        public void run() {
                                            while ( _watchingScans ) {
                                                try { Thread.sleep( 300 ); } catch ( Exception to ) {}
                                                if ( _host.scanNumber() != null ) {
                                                    int num = _host.scanNumber();
                                                    if ( _watchingScans )
                                                        _activityLabel.setText( "Generating directory of " + _numScans + " scans, " + num + " complete." );
                                                }
                                            }
                                        }
                                    };
                                    scanThread.start();
                                }
                            }
                        }
                        else if ( packetType == GENERATE_DIRECTORY_COMPLETED ) {
                            _watchingScans = false;
                            _activityLabel.setText( "Directory generation complete!" );
                        }
                        else if ( packetType == GENERATE_DIRECTORY_ERRORS ) {
                            _watchingScans = false;
                            _activityLabel.setText( "Directory generation complete with error messages received." );
                        }
                        else if ( packetType == GETDIRECTORY_DATE ) {
                            _creationDate.setText( new String( data ) );
                        }
                        else if ( packetType == FILE_NOT_FOUND ) {
                            _activityLabel.setText( "File \"" + _directoryPath.getText() + "\" was not found on " + hostName() );
                        }
                        else if ( packetType == GETDIRECTORY_FILESTART ) {
                            _absorbHeader = true;
                        }
                        else if ( packetType == GETDIRECTORY_FILEDATA ) {
                            //  Here we receive the contents of the directory file, one line at a time.
                            //  Each line is saved for later transmission in the event the user edits
                            //  the file (see removeEntries()).
                            if ( _fileContents == null )
                                _fileContents = new Vector<String>();
                            _fileContents.add( new String( data ) );
                            //  The first line is a header containing general information.
                            if ( _absorbHeader ) {
                                _absorbHeader = false;
                                int i = 0;
                                StringTokenizer strtok = new StringTokenizer( new String( data ) );
                                while ( i < 7 ) {
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
            _watchingScans = false;
            _cancelButton.setVisible( false );
            _refreshButton.setEnabled( true );
            _generateDirectoryButton.setEnabled( true );
            _createFileButton.setEnabled( true );
            _runningGetDirectory = false;
        }
        
        protected int _port;
        protected int _numScans;
        protected boolean _watchingScans;
        
    }
    
    /*
     * Generate a new directory for the current VSN.  This is a time-consuming process,
     * so a warning is issued.
     */
    public void generateDirectory() {
        int n = JOptionPane.showOptionDialog( this, "Generating directory for " + _vsn +".\nThis can take a long time - do you wish to continue?",
                "Generate Directory for " + _vsn, JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, null, null, null );
        if ( n == 0 ) {
            _runningGetDirectory = true;
            _activitySpinner.setVisible( true );
            _activitySpinner.ok();
            _activityLabel.setText( "Generating a new directory for " + _vsn + " on " + hostName() );
            //  Lock other buttons and make the "cancel" button visible.
            _cancelActivated = false;
            _cancelButton.setVisible( true );
            _refreshButton.setEnabled( false );
            _generateDirectoryButton.setEnabled( false );
            _createFileButton.setEnabled( false );

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
    
    public void removeEntries() {
        int[] rows = _table.getSelectedRows();
        if ( rows.length > 0 ) {
            _activitySpinner.setVisible( true );
            _activityLabel.setText( "Removing selected lines from the directory contents." );
            //  Remove the lines from the file contents (which we saved last time we
            //  downloaded it).
            if ( _fileContents != null ) {
                for ( int i = rows[0] + 1; i < rows[rows.length - 1] + 2; ++i )
                    _fileContents.remove( rows[0] + 1 );
            }
            String outputFile = "";
            for ( Iterator<String> iter = _fileContents.iterator(); iter.hasNext(); ) {
                outputFile += iter.next() + "\n";
            }
            //  Download the result to the directory file location.
            _activityLabel.setText( "Sending the new directory to DiFX. \"" + _directoryPath.getText() + "\"" );
            DiFXCommand_sendFile sendFile = new DiFXCommand_sendFile( _directoryPath.getText(), _settings );
            sendFile.addEndListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    //  Reload the directory to reflect the changes.
                    getDirectory();
                }
            } );
            try { 
                sendFile.sendString( outputFile );
            }
            catch ( Exception e ) {
                _activityLabel.setText( "An error was encountered when trying to upload the new"
                        + " directory contents." );
                _activitySpinner.error();
            }
        }
        else {
            JOptionPane.showMessageDialog( this, "No scans have been selected for removal.",
                    "No scans selected", JOptionPane.WARNING_MESSAGE );
        }
    }
    
    protected boolean _okayPushed;
    protected JDialog _pathDialog;
    protected TabCompletedTextField _pathField;
    
    /*
     * Dump highlighted scans to a file.  The user is prompted for a filename.
     */
    public void createFile() {
        int[] rows = _table.getSelectedRows();
        if ( rows.length > 0 ) {
            _okayPushed = false;
            _createFileDestination = "";
            _pathDialog = new JDialog( this, "Specify File Destination", JDialog.DEFAULT_MODALITY_TYPE );
            _pathDialog.setLayout( null );
            _pathDialog.setSize( 500, 150 );
            _pathDialog.setLocationRelativeTo( null );
            _pathDialog.setResizable( false );
            JLabel label1 = new JLabel( "Specify a destination directory for the scans" );
            label1.setBounds( 20, 10, 460, 25 );
            _pathDialog.getContentPane().add( label1 );
            JLabel label2 = new JLabel( "(this path must be writable by " + _settings.difxControlUser() + " on " + hostName() + "):" );
            label2.setBounds( 20, 25, 460, 25 );
            _pathDialog.getContentPane().add( label2 );
            _pathField = new TabCompletedTextField( _settings );
            _pathField.setBounds( 20, 50, 460, 25 );
            _pathField.setText( _createFileDestination );
            _pathDialog.getContentPane().add( _pathField );
            ZButton okButton = new ZButton( "OK" );
            okButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _okayPushed = true;
                    _pathDialog.setVisible( false );
                }
            });
            okButton.setBounds( 290, 80, 80, 25 );
            _pathDialog.getContentPane().add( okButton );
            ZButton cancelButton = new ZButton( "Cancel" );
            cancelButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _okayPushed = false;
                    _pathDialog.setVisible( false );
                }
            });
            cancelButton.setBounds( 380, 80, 80, 25 );
            _pathDialog.getContentPane().add( cancelButton );
            
            _pathDialog.setVisible(true);
            if ( !_okayPushed )
                return;
            
            _createFileDestination = _pathField.getText();
            _createFileScans = " " + ( rows[0] + 1);
            _firstScan = rows[0] + 1;
            if ( rows.length > 1 )
                _createFileScans += "-" + ( rows[rows.length-1] + 1);
            int n = JOptionPane.showOptionDialog( this, "Creating file \"" + _createFileDestination + "\"."
                    + "\nMark5: " + hostName() 
                    + "\nVSN: " + _vsn
                    + "\nScan(s): " + _createFileScans
                    + "\nThis can take a long time - do you wish to continue?",
                    "Copy Scans from " + _vsn, JOptionPane.OK_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE, null, null, null );
            if ( n == 0 ) {
                //  Set up buttons, spinners, etc.
                _runningCreateFile = true;
                _activitySpinner.setVisible( true );
                _activitySpinner.ok();
                _activityLabel.setText( "Creating files in \"" + _createFileDestination + "\" from scans " + _createFileScans + " on " + _vsn );
                //  Lock other buttons and make the "cancel" button visible.
                _cancelActivated = false;
                _cancelButton.setVisible( true );
                _refreshButton.setEnabled( false );
                _generateDirectoryButton.setEnabled( false );
                _createFileButton.setEnabled( false );
          
                //  Construct a Get Directory command.
                DiFXCommand command = new DiFXCommand( _settings );
                command.header().setType( "DifxMark5Copy" );
                command.mpiProcessId( "-1" );
                command.identifier( "gui" );
                int monitorPort = 0;

                // Specifics of the get directory command.
                DifxMark5Copy cmd = command.factory().createDifxMark5Copy();
                cmd.setMark5( hostName() );
                cmd.setVsn( _vsn );
                cmd.setDifxVersion( _settings.difxVersion() );
                cmd.setScans( _createFileScans );
                cmd.setDestination( _createFileDestination );

                // If we are using the TCP connection, set the address and port for diagnostic
                // reporting.
                if ( _settings.sendCommandsViaTCP() ) {
                    cmd.setAddress( _settings.guiServerConnection().myIPAddress() );
                    monitorPort = _settings.newDifxTransferPort( 0, 100, true, true );
                    cmd.setPort( monitorPort );
                }

                //  Set up a monitor thread to collect and interpret diagnostic messages from
                //  guiServer as it sets up the threads and machine files.
                Mark5CopyMonitor monitor = null;
                if ( _settings.sendCommandsViaTCP() ) {
                    monitor = new Mark5CopyMonitor( monitorPort );
                    monitor.start();
                }

                // -- Create the XML defined messages and process through the system
                command.body().setDifxMark5Copy( cmd );
                try {
                    //command.sendPacket( _settings.guiServerConnection().COMMAND_PACKET );
                    command.send();
                } catch ( java.net.UnknownHostException e ) {
                    java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null,
                            e.getMessage() );  //BLAT should be a pop-up
                }            

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
    protected class Mark5CopyMonitor extends Thread {
        
        public Mark5CopyMonitor( int port ) {
            _port = port;
            _currentScan = 0;
        }
        
        protected final int MARK5COPY_STARTED                   = 301;
        protected final int MARK5COPY_COMPLETED                 = 302;
        protected final int MARK5COPY_FAILED                    = 304;
        protected final int MPIRUN_ERROR                        = 305;
        protected final int NO_ENVIRONMENT_VARIABLE             = 306;
        protected final int DIRECTORY_NOT_FOUND                 = 308;
        protected final int MARK5COPY_INFO                      = 312;
        protected final int MARK5COPY_ERRORS                    = 314;
        
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
                        if ( packetType == MARK5COPY_STARTED ) {
                            connected = true;
                            _watchingScans = true;
                            //  Start a thread to monitor the scans being worked on.  This can be
                            //  used to (hopefully) create useful messages.
                            Thread scanThread = new Thread() {
                                public void run() {
                                    while ( _watchingScans ) {
                                        try { Thread.sleep( 300 ); } catch ( Exception to ) {}
                                        String scanName = _host.scanName();
                                        if ( scanName != null ) {
                                            String percent = scanName.substring( scanName.indexOf( "[" ) + 1, scanName.indexOf( "]" ) );
                                            if ( _watchingScans )
                                                _activityLabel.setText( "Copying scan " + _currentScan + " to " + _outName + ": " + percent + " complete." );
                                        }
                                    }
                                }
                            };
                            scanThread.start();
                        }
                        else if ( packetType == MARK5COPY_COMPLETED ) {
                            _watchingScans = false;
                            _activitySpinner.setVisible( false );
                            _activityLabel.setText( "Mark5 copy complete." );
                            connected = false;
                        }
                        else if ( packetType == MARK5COPY_FAILED ) {
                            _watchingScans = false;
                            _activitySpinner.error();
                        }
                        else if ( packetType == MPIRUN_ERROR ) {
                            //_activityLabel.setText( "mpirun error: " + new String( data ) );
                            //  Useful (and non-error) messages are being sent to stderr...we are interested
                            //  in the output file name.
                            String str = new String( data );
                            if ( str.contains( "outName =" ) ) {
                                _outName = str.substring( str.indexOf( "=" ) + 1 ).trim();
                                if ( _currentScan == 0 )
                                    _currentScan = _firstScan;
                                else
                                    ++_currentScan;
                            }
                        }
                        else if ( packetType == NO_ENVIRONMENT_VARIABLE ) {
                            _activityLabel.setText( "No MARK5_DIR_PATH environment variable on " + hostName() );
                        }
                        else if ( packetType == MARK5COPY_INFO ) {
                            _activityLabel.setText( new String( data ) );
                        }
                        else if ( packetType == MARK5COPY_ERRORS ) {
                            _watchingScans = false;
                            _activityLabel.setText( "Mark5 copy complete." );
                            _activitySpinner.setVisible( false );
                            connected = false;
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
            _cancelButton.setVisible( false );
            _refreshButton.setEnabled( true );
            _generateDirectoryButton.setEnabled( true );
            _createFileButton.setEnabled( true );
            _runningGetDirectory = false;
        }
        
        protected int _port;
        protected String _outName;
        protected int _currentScan;
        protected boolean _watchingScans;
                
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
    protected ZButton _refreshButton;
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
    protected ZButton _cancelButton;
    protected ZButton _generateDirectoryButton;
    protected ZButton _createFileButton;
    protected ZButton _removeEntriesButton;
    protected boolean _cancelActivated;
    protected boolean _runningGetDirectory;
    protected boolean _runningCreateFile;
    protected String _createFileDestination;
    protected String _createFileScans;
    protected int _firstScan;
    protected Vector<String> _fileContents;

}
