/*
 * This window contains all of the settings for a single job, as well as controls
 * and displays to run it and monitor its progress.
 */
package edu.nrao.difx.difxview;

import javax.swing.Timer;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.plaf.basic.ComboPopup;
import javax.swing.plaf.basic.BasicComboBoxUI;
import javax.swing.plaf.basic.BasicComboPopup;

import edu.nrao.difx.difxutilities.DiFXCommand;
import edu.nrao.difx.difxutilities.InputFileParser;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.DifxMachinesDefinition;
import edu.nrao.difx.xmllib.difxmessage.DifxStart;
import edu.nrao.difx.xmllib.difxmessage.DifxStop;
import edu.nrao.difx.xmllib.difxmessage.DifxStatus;
import java.awt.*;

import javax.swing.JFrame;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JTextField;
import javax.swing.JFormattedTextField;
import javax.swing.JLabel;
import javax.swing.JCheckBox;
import javax.swing.JPopupMenu;
import javax.swing.JButton;
import javax.swing.JProgressBar;
import javax.swing.BorderFactory;
import javax.swing.border.Border;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.DataInputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;

import java.util.*;
import java.util.List;

import javax.swing.event.EventListenerList;

import mil.navy.usno.widgetlib.IndexedPanel;
import mil.navy.usno.widgetlib.NodeBrowserScrollPane;
import mil.navy.usno.widgetlib.NumberBox;
import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.SimpleTextEditor;
import mil.navy.usno.widgetlib.MessageDisplayPanel;

public class JobEditorMonitor extends JFrame {
    
    /*
     * The JobNode gives us access to all of the data known about this job.
     */
    public JobEditorMonitor( JobNode newNode, SystemSettings settings ) {
        super( "Job Editor/Monitor" );
        _jobNode = newNode;
        _settings = settings;
        _settings.setLookAndFeel();
        this.setLayout( null );
        this.setBounds( 500, 100, _settings.windowConfiguration().jobEditorMonitorWindowW,
                _settings.windowConfiguration().jobEditorMonitorWindowH );
        this.setTitle( "Control/Monitor for " + _jobNode.name() );
        _menuBar = new JMenuBar();
        _menuBar.setVisible( true );
        JMenu helpMenu = new JMenu( "  Help  " );
        _menuBar.add( helpMenu );
        JMenuItem controlHelpItem = new JMenuItem( "Control/Monitor Help" );
        controlHelpItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.launchGUIHelp( "Control_Monitor_Window.html" );
            }
        } );
        helpMenu.add( controlHelpItem );
        JMenuItem helpIndexItem = new JMenuItem( "GUI Documentation" );
        helpIndexItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.launchGUIHelp( "intro.html" );
            }
        } );
        helpMenu.add( helpIndexItem );
        this.add( _menuBar );

        _scrollPane = new NodeBrowserScrollPane();
        _scrollPane.respondToResizeEvents( true );
        _scrollPane.addTimeoutEventListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                newSize();
            }
        } );
        this.add( _scrollPane );
        
        //  This panel shows us the input file, which can be edited and sent to
        //  the DiFX host.  The existing file on the DiFX host can also be downloaded.
        IndexedPanel inputFilePanel = new IndexedPanel( "Input File" );
        inputFilePanel.openHeight( 400 );
        inputFilePanel.closedHeight( 20 );
        inputFilePanel.open( false );
        _scrollPane.addNode( inputFilePanel );
        _inputFileEditor = new SimpleTextEditor();
        inputFilePanel.add( _inputFileEditor );
        _inputFileName = new JLabel( "" );
        inputFilePanel.add( _inputFileName );
        _refreshInputButton = new JButton( "Refresh" );
        _refreshInputButton.setToolTipText( "Read the Input File stored on the DiFX host." );
        _refreshInputButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                statusInfo( "Obtaining file \"" + _inputFileName.getText() + "\" from DiFX host." );
                Component comp = _refreshInputButton;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = _refreshInputButton.getLocationOnScreen();
                GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        _inputFileName.getText(), _settings );
                if ( getFile.inString() != null && getFile.inString().length() > 0 ) {
                    _inputFileEditor.text( getFile.inString() );
                    parseInputFile();
                }
            }
        } );
        inputFilePanel.add( _refreshInputButton );
        _uploadInputButton = new JButton( "Save" );
        _uploadInputButton.setToolTipText( "Parse all settings from the editor text and upload to the Input File location on the DiFX host (not necessary unless you have changed the text)." );
        _uploadInputButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                parseInputFile();
                Component comp = _uploadInputButton;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = _uploadInputButton.getLocationOnScreen();
                SendFileMonitor sendFile = new SendFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        _inputFileName.getText(), _inputFileEditor.text(), _settings );
            }
        } );
        inputFilePanel.add( _uploadInputButton );

        //  This panel shows us the calc file, which can be edited and sent to
        //  the DiFX host.  The existing file on the DiFX host can also be downloaded.
        IndexedPanel calcFilePanel = new IndexedPanel( "Calc File" );
        calcFilePanel.openHeight( 400 );
        calcFilePanel.closedHeight( 20 );
        calcFilePanel.open( false );
        _scrollPane.addNode( calcFilePanel );
        _calcFileEditor = new SimpleTextEditor();
        calcFilePanel.add( _calcFileEditor );
        _calcFileName = new JLabel( "" );
        calcFilePanel.add( _calcFileName );
        _refreshCalcButton = new JButton( "Refresh" );
        _refreshCalcButton.setToolTipText( "Read the Calc File as it is stored on the DiFX host." );
        _refreshCalcButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                Component comp = _refreshCalcButton;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = _refreshCalcButton.getLocationOnScreen();
                GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        _calcFileName.getText(), _settings );
                if ( getFile.inString() != null && getFile.inString().length() > 0 ) {
                    _calcFileEditor.text( getFile.inString() );
                    parseCalcFile( _calcFileEditor.text() );
                }
            }
        } );
        calcFilePanel.add( _refreshCalcButton );
        _uploadCalcButton = new JButton( "Save" );
        _uploadCalcButton.setToolTipText( "Parse all settings from the editor text and upload to the Calc File location on the DiFX host (not necessary unless you have changed the text)." );
        _uploadCalcButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                parseCalcFile( _calcFileEditor.text() );
                Component comp = _uploadCalcButton;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = _uploadCalcButton.getLocationOnScreen();
                SendFileMonitor sendFile = new SendFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        _calcFileName.getText(), _calcFileEditor.text(), _settings );
            }
        } );
        calcFilePanel.add( _uploadCalcButton );

        IndexedPanel machinesListPanel = new IndexedPanel( "Machines List" );
        machinesListPanel.openHeight( 425 );
        machinesListPanel.closedHeight( 20 );
        _scrollPane.addNode( machinesListPanel );
        _dataSourcesPane = new NodeBrowserScrollPane();
        _dataSourcesPane.setBackground( Color.WHITE );
        _dataSourcesPane.decayCount( 5 );
        machinesListPanel.add( _dataSourcesPane );
        _dataSourcesLabel = new JLabel( "Data Sources:" );
        _dataSourcesLabel.setHorizontalAlignment( JLabel.LEFT );
        machinesListPanel.add( _dataSourcesLabel );
        _processorsPane = new NodeBrowserScrollPane();
        _processorsPane.setBackground( Color.WHITE );
        _processorsPane.decayCount( 5 );
        machinesListPanel.add( _processorsPane );
        _processorsLabel = new JLabel( "Processor Nodes:" );
        _processorsLabel.setHorizontalAlignment( JLabel.LEFT );
        machinesListPanel.add( _processorsLabel );
        _threadsLabel = new JLabel( "Threads:" );
        _threadsLabel.setHorizontalAlignment( JLabel.RIGHT );
        machinesListPanel.add( _threadsLabel );
        _coresLabel = new JLabel( "Cores:" );
        _coresLabel.setHorizontalAlignment( JLabel.RIGHT );
        machinesListPanel.add( _coresLabel );
        _cpuUsageLabel = new JLabel( "% CPU Usage:" );
        _cpuUsageLabel.setHorizontalAlignment( JLabel.RIGHT );
        machinesListPanel.add( _cpuUsageLabel );
        _mpiTestLabel = new JLabel( "mpi Test:" );
        _mpiTestLabel.setHorizontalAlignment( JLabel.RIGHT );
        machinesListPanel.add( _mpiTestLabel );
        _headNode = new JFormattedTextField();
        _headNode.setFocusLostBehavior( JFormattedTextField.COMMIT );
        _headNode.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.headNode( _headNode.getText() );
            }
        } );
        _headNode.setText( _settings.headNode() );
        machinesListPanel.add( _headNode );
        _headNodeLabel = new JLabel( "HeadNode:" );
        _headNodeLabel.setHorizontalAlignment( JLabel.LEFT );
        machinesListPanel.add( _headNodeLabel );
        _applyMachinesButton = new JButton( "Apply" );
        _applyMachinesButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  Track whether this button was ever pushed - indicating the user
                //  has already generated the .threads and .machines files.  This
                //  is used later when the "Start" button is pushed - it will need
                //  to generate these files if the user has not.
                _machinesAppliedByHand = true;
                applyMachinesAction();
            }
        } );
        machinesListPanel.add( _applyMachinesButton );
        _restrictHeadnodeProcessing = new JCheckBox( "Restrict Headnode Processing" );
        _restrictHeadnodeProcessing.setToolTipText( "Do not do any multicore DiFX processing on the headnode." );
        _restrictHeadnodeProcessing.setSelected( _settings.defaultNames().restrictHeadnodeProcessing );
        _restrictHeadnodeProcessing.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.defaultNames().restrictHeadnodeProcessing = _restrictHeadnodeProcessing.isSelected();
            }
        } );
        machinesListPanel.add( _restrictHeadnodeProcessing );
        _eliminateNonrespondingProcessors = new JCheckBox( "Eliminate Non-Responding Processors" );
        _eliminateNonrespondingProcessors.setSelected( _settings.defaultNames().eliminateNonrespondingProcessors );
        _eliminateNonrespondingProcessors.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.defaultNames().eliminateNonrespondingProcessors = _eliminateNonrespondingProcessors.isSelected();
            }
        } );
        machinesListPanel.add( _eliminateNonrespondingProcessors );
        _eliminateBusyProcessors = new JCheckBox( "Eliminate Processors Over" );
        _eliminateBusyProcessors.setSelected( _settings.defaultNames().eliminateNonrespondingProcessors );
        _eliminateBusyProcessors.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                changeBusyProcessorsSettings();
                _settings.defaultNames().eliminateBusyProcessors = _eliminateBusyProcessors.isSelected();
            }
        } );
        machinesListPanel.add( _eliminateBusyProcessors );
        _busyPercentage = new NumberBox();
        _busyPercentage.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                changeBusyProcessorsSettings();
                _settings.defaultNames().busyPercentage = _busyPercentage.value();
            }
        } );
        _busyPercentage.precision( 0 );
        _busyPercentage.value( _settings.defaultNames().busyPercentage );
        _busyPercentage.limits( 0.0, 100.0 );
        machinesListPanel.add( _busyPercentage );
        _busyPercentageLabel = new JLabel( "% Busy" );
        machinesListPanel.add( _busyPercentageLabel );
        _chooseBasedOnModule = new JCheckBox( "Choose Data Source Based on Module" );
        _chooseBasedOnModule.setSelected( _settings.defaultNames().chooseBasedOnModule );
        _chooseBasedOnModule.setToolTipText( "Pick Mark5 data sources that contain required modules." );
        _chooseBasedOnModule.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _chooseBasedOnModule.isSelected() )
                    buildDataSourceList();
                _settings.defaultNames().chooseBasedOnModule = _chooseBasedOnModule.isSelected();
            }
        } );
        _selectAllProcessorsButton = new JButton( "Select All" );
        _selectAllProcessorsButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectAllProcessors();
            }
        } );
        machinesListPanel.add( _selectAllProcessorsButton );
        _deselectAllProcessorsButton = new JButton( "Deselect All" );
        _deselectAllProcessorsButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                deselectAllProcessors();
            }
        } );
        machinesListPanel.add( _deselectAllProcessorsButton );
        machinesListPanel.add( _chooseBasedOnModule );

        //  This panel shows us the machines file, which can be edited and sent to
        //  the DiFX host.  The existing file on the DiFX host can also be downloaded.
        IndexedPanel machinesFilePanel = new IndexedPanel( "Machines File" );
        machinesFilePanel.openHeight( 300 );
        machinesFilePanel.closedHeight( 20 );
        machinesFilePanel.open( false );
        _scrollPane.addNode( machinesFilePanel );
        _machinesFileEditor = new SimpleTextEditor();
        machinesFilePanel.add( _machinesFileEditor );
        _machinesFileName = new JLabel( "" );
        machinesFilePanel.add( _machinesFileName );
        _refreshMachinesButton = new JButton( "Refresh" );
        _refreshMachinesButton.setToolTipText( "Read the Machines File as it is stored on the DiFX host." );
        _refreshMachinesButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                Component comp = _refreshMachinesButton;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = _refreshMachinesButton.getLocationOnScreen();
                GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        _machinesFileName.getText(), _settings );
                if ( getFile.inString() != null && getFile.inString().length() > 0 ) {
                    _machinesFileEditor.text( getFile.inString() );
                }
            }
        } );
        machinesFilePanel.add( _refreshMachinesButton );
        _uploadMachinesButton = new JButton( "Save" );
        _uploadMachinesButton.setToolTipText( "Parse all settings from the editor text and upload to the Machines File location on the DiFX host (not necessary unless you have changed the text)." );
        _uploadMachinesButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _machinesAppliedByHand = true;
                Component comp = _uploadMachinesButton;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = _uploadMachinesButton.getLocationOnScreen();
                SendFileMonitor sendFile = new SendFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        _machinesFileName.getText(), _machinesFileEditor.text(), _settings );
                statusInfo( "Sent machines file content (" + _machinesFileEditor.text().length()
                        + " chars) to \"" + _machinesFileName.getText() + "\" on DiFX host." );
            }
        } );
        machinesFilePanel.add( _uploadMachinesButton );

        //  This panel shows us the threads file, which can be edited and sent to
        //  the DiFX host.  The existing file on the DiFX host can also be downloaded.
        IndexedPanel threadsFilePanel = new IndexedPanel( "Threads File" );
        threadsFilePanel.openHeight( 300 );
        threadsFilePanel.closedHeight( 20 );
        threadsFilePanel.open( false );
        _scrollPane.addNode( threadsFilePanel );
        _threadsFileEditor = new SimpleTextEditor();
        threadsFilePanel.add( _threadsFileEditor );
        _threadsFileName = new JLabel( "" );
        threadsFilePanel.add( _threadsFileName );
        _refreshThreadsButton = new JButton( "Refresh" );
        _refreshThreadsButton.setToolTipText( "Read the Threads File as it is stored on the DiFX host." );
        _refreshThreadsButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                statusInfo( "Reading thread file \"" + _threadsFileName.getText() + "\" from DiFX host." );
                Component comp = _refreshThreadsButton;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = _refreshThreadsButton.getLocationOnScreen();
                GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        _threadsFileName.getText(), _settings );
                if ( getFile.inString() != null && getFile.inString().length() > 0 ) {
                    statusInfo( "\"" + _threadsFileName.getText() + "\" (" + getFile.inString().length() + " chars) read from DiFX host." );
                    _threadsFileEditor.text( getFile.inString() );
                }
                else {
                    if ( getFile.inString() == null )
                        statusWarning( "No \"" + _threadsFileName.getText() + "\" file found on DiFX host." );
                    else
                        statusWarning( "\"" + _threadsFileName.getText() + "\" was zero length on DiFX host." );
                }
            }
        } );
        threadsFilePanel.add( _refreshThreadsButton );
        _uploadThreadsButton = new JButton( "Save" );
        _uploadThreadsButton.setToolTipText( "Parse all settings from the editor text and upload to the Threads File location on the DiFX host (not necessary unless you have changed the text)." );
        _uploadThreadsButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _machinesAppliedByHand = true;
                Component comp = _uploadThreadsButton;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                Point pt = _uploadThreadsButton.getLocationOnScreen();
                SendFileMonitor sendFile = new SendFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                        _threadsFileName.getText(), _threadsFileEditor.text(), _settings );
                statusInfo( "Sent threads file content (" + _threadsFileEditor.text().length()
                        + " chars) to \"" + _threadsFileName.getText() + "\" on DiFX host." );
            }
        } );
        threadsFilePanel.add( _uploadThreadsButton );

        IndexedPanel runControlPanel = new IndexedPanel( "Run Controls" );
        runControlPanel.openHeight( 100 );
        runControlPanel.closedHeight( 20 );
        _scrollPane.addNode( runControlPanel );
        _startButton = new JButton( "Start" );
        _startButton.setBounds( 10, 30, 110, 25 );
        _startButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                startJob();
            }
        } );
        runControlPanel.add( _startButton );
        _stopButton = new JButton( "Stop" );
        _stopButton.setBounds( 10, 60, 110, 25 );
        _stopButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                stopJob();
            }
        } );
        runControlPanel.add( _stopButton );
        _restartAt = new JCheckBox( "Restart" );
        _restartAt.setBounds( 130, 30, 75, 25 );
        _restartAt.setSelected( false );
        runControlPanel.add( _restartAt );
        _restartSeconds = new NumberBox();
        _restartSeconds.precision( 4 );
        _restartSeconds.value( 0.0 );
        _restartSeconds.setBounds( 230, 30, 100, 25 );
        runControlPanel.add( _restartSeconds );
        _forceOverwrite = new JCheckBox( "Force Overwrite" );
        _forceOverwrite.setToolTipText( "Force the overwrite of the \".difx\" output file if one already exists." );
        _forceOverwrite.setSelected( true );
        _forceOverwrite.setBounds( 130, 60, 150, 25 );
        runControlPanel.add( _forceOverwrite );
        JLabel restartAtLabel = new JLabel( "at:" );
        restartAtLabel.setBounds( 180, 30, 45, 25 );
        restartAtLabel.setHorizontalAlignment( JLabel.RIGHT );
        runControlPanel.add( restartAtLabel );
        JLabel restartSecondsLabel = new JLabel( "seconds" );
        restartSecondsLabel.setBounds( 335, 30, 90, 25 );
        restartSecondsLabel.setHorizontalAlignment( JLabel.LEFT );
        runControlPanel.add( restartSecondsLabel );
        _showMonitorButton = new JButton( "Show Monitor" );
        _showMonitorButton.setToolTipText( "Launch real-time fringe monitoring for this job." );
        _showMonitorButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showLiveMonitor();
            }
        } );
        runControlPanel.add( _showMonitorButton );
        
        //  The Status Panel shows the current state of the job.
        _statusPanel = new IndexedPanel( "" );
        _scrollPane.addNode( _statusPanel );
        _statusPanel.openHeight( 60 );
        _statusPanel.alwaysOpen( true );
        _statusPanel.noArrow( true );
        _statusPanelBackground = _statusPanel.getBackground();
        _statusPanel.setBackground( _statusPanelBackground );
        _statusLabel = new JLabel( "" );
        _statusLabel.setHorizontalAlignment( JLabel.RIGHT );
        _statusPanel.add( _statusLabel );
        _state = new ColumnTextArea();
        _state.justify( ColumnTextArea.CENTER );
        _state.setText( "not started" );
        _statusPanel.add( _state );
        _progress = new JProgressBar( 0, 100 );
        _progress.setValue( 0 );
        _progress.setStringPainted( true );
        _statusPanel.add( _progress );
        
        //  The message panel shows raw message data pertaining to the job.
        IndexedPanel messagePanel = new IndexedPanel( "Messages" );
        _scrollPane.addNode( messagePanel );
        messagePanel.openHeight( 200 );
        messagePanel.closedHeight( 20 );
        _messageDisplayPanel = new MessageDisplayPanel();
        messagePanel.add( _messageDisplayPanel );
 
        _allObjectsBuilt = true;
        
        //  Start a thread that can be used to trigger repeated updates.
        _updateThread = new UpdateThread( 1000 );
        _updateThread.start();
        
        this.newSize();

    }
    
    protected class UpdateThread extends Thread {
        protected int _interval;
        protected boolean _keepGoing;
        public UpdateThread( int i ) {
            _interval = i;
            _keepGoing = true;
        }
        public void keepGoing( boolean newVal ) {
            _keepGoing = newVal;
        }
        @Override
        public void run() {
            while ( _keepGoing ) {
                timeoutIntervalEvent();
                try {
                    Thread.sleep( _interval );
                } catch ( Exception e ) {
                    _keepGoing = false;
                }
            }
        }
    }
                
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        newSize();
    }
    
    public void newSize() {
        //  The current sizes are saved in the settings...but we can't be certain
        //  the _settings variable has been set yet.
        if ( _settings != null ) {
            _settings.windowConfiguration().jobEditorMonitorWindowW = this.getWidth();
            _settings.windowConfiguration().jobEditorMonitorWindowH = this.getHeight();
        }
        if ( _allObjectsBuilt ) {
            int w = this.getContentPane().getSize().width;
            int h = this.getContentPane().getSize().height;
            _menuBar.setBounds( 0, 0, w, 25 );
            _scrollPane.setBounds( 0, 25, w, h - 25 );
            int thirdSize = ( w - 60 ) / 3;
            _dataSourcesLabel.setBounds( 10, 25, 2 * thirdSize, 25 );
            _dataSourcesPane.setBounds( 10, 50, 2 * thirdSize, 150 );
            _chooseBasedOnModule.setBounds( 30 + 2 * thirdSize, 50, thirdSize, 25 );
            _processorsLabel.setBounds( 10, 205, 2 * thirdSize, 25 );
            _processorsPane.setBounds( 10, 230, 2 * thirdSize, 150 );
            _threadsLabel.setBounds( 170, 205, 80, 25 );
            _coresLabel.setBounds( 240, 205, 80, 25 );
            _cpuUsageLabel.setBounds( 340, 205, 100, 25 );
            _mpiTestLabel.setBounds( 440, 205, 100, 25 );
            _headNodeLabel.setBounds( 30 + 2 * thirdSize, 205, thirdSize, 25 );
            _headNode.setBounds( 30 + 2 * thirdSize, 230, thirdSize, 25 );
            _applyMachinesButton.setBounds( 30 + 2 * thirdSize, 355, thirdSize/2 - 5, 25 );
            _restrictHeadnodeProcessing.setBounds( 30 + 2 * thirdSize, 265, thirdSize, 25 );
            _eliminateNonrespondingProcessors.setBounds( 30 + 2 * thirdSize, 295, thirdSize, 25 );
            _eliminateBusyProcessors.setBounds( 30 + 2 * thirdSize, 325, thirdSize - 110, 25 );
            _selectAllProcessorsButton.setBounds( 10, 390, 115, 25 );
            _deselectAllProcessorsButton.setBounds( 135, 390, 115, 25 );
            _busyPercentage.setBounds( w - 135, 325, 30, 25 );
            _busyPercentageLabel.setBounds( w - 100, 325, 100, 25 );
            _inputFileEditor.setBounds( 10, 60, w - 35, 330 );
            _calcFileEditor.setBounds( 10, 60, w - 35, 330 );
            _inputFileName.setBounds( 10, 30, w - 350, 25 );
            _calcFileName.setBounds( 10, 30, w - 350, 25 );
            _refreshInputButton.setBounds( w - 125, 30, 100, 25 );
            _refreshCalcButton.setBounds( w - 125, 30, 100, 25 );
            _uploadInputButton.setBounds( w - 230, 30, 100, 25 );
            _uploadCalcButton.setBounds( w - 230, 30, 100, 25 );
            _machinesFileEditor.setBounds( 10, 60, w - 35, 230 );
            _machinesFileName.setBounds( 10, 30, w - 350, 25 );
            _refreshMachinesButton.setBounds( w - 125, 30, 100, 25 );
            _uploadMachinesButton.setBounds( w - 230, 30, 100, 25 );
            _threadsFileEditor.setBounds( 10, 60, w - 35, 230 );
            _threadsFileName.setBounds( 10, 30, w - 350, 25 );
            _refreshThreadsButton.setBounds( w - 125, 30, 100, 25 );
            _uploadThreadsButton.setBounds( w - 230, 30, 100, 25 );
            _showMonitorButton.setBounds( w - 150, 60, 125, 25 );
            _messageDisplayPanel.setBounds( 2, 25, w - 23, 173 );
            _state.setBounds( 10, 30, 200, 25 );
            _progress.setBounds( 220, 30, w - 245, 25 );
            _statusLabel.setBounds( 10, 0, w - 35, 25 );
        }
    }
    
    public void showLiveMonitor() {
        if ( _liveMonitorWindow == null )
            _liveMonitorWindow = new LiveMonitorWindow( MouseInfo.getPointerInfo().getLocation().x, 
                MouseInfo.getPointerInfo().getLocation().y, _settings, _inputFileName.getText() );
        _liveMonitorWindow.setVisible( true );
    }
    
    public void statusInfo( String newText ) {
        _statusLabel.setForeground( Color.BLACK );
        _statusLabel.setText( newText );
    }
    
    public void statusWarning( String newText ) {
        _statusLabel.setForeground( Color.YELLOW );
        _statusLabel.setText( newText );
    }
    
    public void statusError( String newText ) {
        _statusLabel.setForeground( Color.RED );
        _statusLabel.setText( newText );
    }
    
    public void statusPanelColor( Color newColor ) {
        _statusPanel.setBackground( newColor );
    }
    
    public void inputFileName( String newName, String content ) { 
        _inputFileName.setText( newName );
        _inputFileEditor.text( content );
    }
    
    public void calcFileName( String newName ) { _calcFileName.setText( newName ); }
    
    public PaneProcessorNode processorNodeByName( String name ) {
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            PaneProcessorNode thisNode = (PaneProcessorNode)(iter.next());
            if ( thisNode.name().contentEquals( name ) )
                return thisNode;
        }
        return null;
    }

    public DataNode dataNodeByName( String name ) {
        for ( Iterator<BrowserNode> iter = _dataSourcesPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            DataNode thisNode = (DataNode)(iter.next());
            if ( thisNode.name().contentEquals( name ) )
                return thisNode;
        }
        return null;
    }

    public BrowserNode nodeByName( String name ) {
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            BrowserNode thisNode = iter.next();
            if ( thisNode.name().contentEquals( name ) )
                return thisNode;
        }
        for ( Iterator<BrowserNode> iter = _dataSourcesPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            BrowserNode thisNode = iter.next();
            if ( thisNode.name().contentEquals( name ) )
                return thisNode;
        }
        return null;
    }
    
    public void selectAllProcessors() {
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            PaneProcessorNode thisNode = (PaneProcessorNode)(iter.next());
            thisNode.handSelected( true );
        }
    }

    public void deselectAllProcessors() {
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            PaneProcessorNode thisNode = (PaneProcessorNode)(iter.next());
            thisNode.handSelected( false );
        }
    }

    /*
     * Send the current machines and thread settings to guiServer to produce .machines
     * and .threads files.
     */
    public MachinesDefinitionMonitor applyMachinesAction() {
        DiFXCommand command = new DiFXCommand( _settings );
        command.header().setType( "DifxMachinesDefinition" );
        command.mpiProcessId( "-1" );
        command.identifier( _jobNode.name() );
        int monitorPort = 0;

        //  We are going to test processor nodes, so remove the results of any previous
        //  tests.
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            ( (PaneProcessorNode)(iter.next()) ).clearTest();
        }
        
        // Create machines definition command
        DifxMachinesDefinition cmd = command.factory().createDifxMachinesDefinition();
        cmd.setInput( _jobNode.inputFile() );
        cmd.setDifxVersion( _settings.difxVersion() );

        // If we are using the TCP connection, set the address and port for diagnostic
        // reporting.
        if ( _settings.sendCommandsViaTCP() ) {
            cmd.setAddress( _settings.guiServerConnection().myIPAddress() );
            monitorPort = _settings.newDifxTransferPort();
            cmd.setPort( monitorPort );
        }
        
        // -- manager, enabled only
        DifxMachinesDefinition.Manager manager = command.factory().createDifxMachinesDefinitionManager();
        manager.setNode( _headNode.getText() );
        cmd.setManager( manager );

        // -- datastreams, enabled only
        DifxMachinesDefinition.Datastream dataStream = command.factory().createDifxMachinesDefinitionDatastream();

        //  Include all of the "checked" data stream node names...
        String dataNodeNames = "";
        for ( Iterator<BrowserNode> iter = _dataSourcesPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            DataSource thisSource = (DataSource)(iter.next());
            dataNodeNames += thisSource.sourceNode() + " ";
        }
        dataStream.setNodes( dataNodeNames );
        cmd.setDatastream(dataStream);

        // Add enabled processors and threads.  Don't include processors that have no
        // threads!  Also avoid the headnode if the user has indicated it should be
        // avoided (recommended if there are other processors).
        String processNodeNames = "";
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            PaneProcessorNode thisNode = (PaneProcessorNode)(iter.next());
            if ( thisNode.selected() ) {
                //  Avoid the headnode - if there are other nodes
                if ( _restrictHeadnodeProcessing.isSelected() && thisNode.name().contentEquals( _headNode.getText() ) &&
                        _processorsPane.browserTopNode().children().size() > 1 ) {}
                else {
                    DifxMachinesDefinition.Process process = command.factory().createDifxMachinesDefinitionProcess();
                    process.setNodes( thisNode.name() );
                    process.setThreads( thisNode.threadsText() );
                    cmd.getProcess().add( process );
                }
            }
        }
        
        //  Test the processors if the user wants to (this will generate feedback that
        //  will be reflected in the processors list).
        if ( _eliminateNonrespondingProcessors.isSelected() )
            cmd.setTestProcessors( true );
        else
            cmd.setTestProcessors( false );
        
        //  Send the names for the machine and thread files.  For the moment we are basing
        //  these on the input file name.
        String fileStr = _inputFileName.getText();
        cmd.setMachinesFile( fileStr.substring( 0, fileStr.lastIndexOf( '.' ) + 1 ).trim() + "machines" );
        cmd.setThreadsFile( fileStr.substring( 0, fileStr.lastIndexOf( '.' ) + 1 ).trim() + "threads" );

        //  Set up a monitor thread to collect and interpret diagnostic messages from
        //  guiServer as it sets up the threads and machine files.
        MachinesDefinitionMonitor monitor = null;
        if ( _settings.sendCommandsViaTCP() ) {
            monitor = new MachinesDefinitionMonitor( monitorPort );
            monitor.start();
        }
        
        statusInfo( "Using criteria to create .machines and .threads files." );

        // -- Create the XML defined messages and process through the system
        command.body().setDifxMachinesDefinition( cmd );
        try {
            //command.sendPacket( _settings.guiServerConnection().COMMAND_PACKET );
            command.send();
        } catch ( java.net.UnknownHostException e ) {
            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null,
                    e.getMessage() );  //BLAT should be a pop-up
        }    
        return monitor;
    }
    
    /*
     * This thread opens and monitors a TCP socket for diagnostic reports from the
     * guiServer as it sets up thread and machine files.  The opposite side of this
     * communication link is in the file "guiServer/src/machinesDefinition.cpp" in
     * the DiFX source tree.
     */
    protected class MachinesDefinitionMonitor extends Thread {
        
        public MachinesDefinitionMonitor( int port ) {
            _port = port;
        }
        
        /*
         * These packet types are sent by the "JobMonitorConnection" class in the
         * guiServer application on the DiFX host.
         */
        protected final int TASK_TERMINATED                     = 100;
        protected final int TASK_ENDED_GRACEFULLY               = 101;
        protected final int TASK_STARTED                        = 102;
        protected final int PARAMETER_CHECK_IN_PROGRESS         = 103;
        protected final int PARAMETER_CHECK_SUCCESS             = 104;
        protected final int FAILURE_NO_HEADNODE                 = 105;
        protected final int FAILURE_NO_DATASOURCES              = 106;
        protected final int FAILURE_NO_PROCESSORS               = 107;
        protected final int WARNING_NO_MACHINES_FILE_SPECIFIED  = 108;
        protected final int WARNING_NO_THREADS_FILE_SPECIFIED   = 109;
        protected final int THREADS_FILE_NAME                   = 110;
        protected final int MACHINES_FILE_NAME                  = 111;
        protected final int FAILURE_NO_FILES_SPECIFIED          = 112;
        protected final int FAILURE_OPEN_MACHINES_FILE          = 113;
        protected final int FAILURE_OPEN_THREADS_FILE           = 114;
        protected final int MACHINES_FILE_CREATED               = 115;
        protected final int THREADS_FILE_CREATED                = 116;
        protected final int FAILURE_FILE_REMOVAL                = 117;
        protected final int FAILURE_POPEN                       = 118;
        protected final int FAILURE_MPIRUN                      = 119;
        protected final int SUCCESS_MPIRUN                      = 120;
        protected final int LOW_THREAD_COUNT                    = 121;
        protected final int RUNNING_MPIRUN_TESTS                = 122;
        
        @Override
        public void run() {
            //  Open a new server socket and await a connection.  The connection
            //  will timeout after a given number of seconds (nominally 10).
            try {
                ServerSocket ssock = new ServerSocket( _port );
                ssock.setSoTimeout( 10000 );  //  timeout is in millisec
                try {
                    Socket sock = ssock.accept();
                    //  Turn the socket into a "data stream", which has useful
                    //  functions.
                    DataInputStream in = new DataInputStream( sock.getInputStream() );
                    
                    //  Loop collecting diagnostic packets from the guiServer.  These
                    //  are identified by an initial integer, and then are followed
                    //  by a data length, then data.
                    boolean connected = true;
                    int workState = 0;
                    while ( connected ) {
                        //  Read the packet type as an integer.  The packet types
                        //  are defined above (within this class).
                        int packetType = in.readInt();
                        //  Read the size of the incoming data (bytes).
                        int packetSize = in.readInt();
                        //  Read the data (as raw bytes)
                        byte [] data = null;
                        if ( packetSize > 0 ) {
                            data = new byte[packetSize];
                            in.readFully( data );
                        }
                        //  Interpret the packet type.
                        if ( packetType == TASK_TERMINATED ) {
                            _messageDisplayPanel.warning( 0, "machines monitor", "Task terminated prematurely." );
                            connected = false;
                        }
                        else if ( packetType == TASK_ENDED_GRACEFULLY ) {
                            _messageDisplayPanel.warning( 0, "machines monitor", "Task finished gracefully." );
                            statusInfo( ".machines and .threads files created." );
                            connected = false;
                        }
                        else if ( packetType == TASK_STARTED ) {
                            _messageDisplayPanel.message( 0, "machines monitor", "Task started by guiServer." );
                        }
                        else if ( packetType == PARAMETER_CHECK_IN_PROGRESS ) {
                            _messageDisplayPanel.message( 0, "machines monitor", "Checking parameters." );
                        }
                        else if ( packetType == PARAMETER_CHECK_SUCCESS ) {
                            _messageDisplayPanel.message( 0, "machines monitor", "Parameter check successful." );
                        }
                        else if ( packetType == FAILURE_NO_HEADNODE ) {
                            _messageDisplayPanel.error( 0, "machines monitor", "No headnone was specified." );
                            statusError( "Headnode needs to be specified to create .machines and .threads files." );
                        }
                        else if ( packetType == FAILURE_NO_DATASOURCES ) {
                            _messageDisplayPanel.error( 0, "machines monitor", "No valid data streams were specified." );
                            statusError( "No valid data streams were specified - could not create .mahcines and .threads files." );
                        }
                        else if ( packetType == FAILURE_NO_PROCESSORS ) {
                            _messageDisplayPanel.error( 0, "machines monitor", "No valid processors were specified." );
                            statusError( "No valid processors were specified - could not create .mahcines and .threads files." );
                        }
                        else if ( packetType == WARNING_NO_MACHINES_FILE_SPECIFIED ) {
                            workState = packetType;
                            _messageDisplayPanel.message( 0, "machines monitor", "No machines file name was specified - forming one using input file name." );
                        }
                        else if ( packetType == MACHINES_FILE_NAME ) {
                            _machinesFileName.setText( new String( data ) );
                            _messageDisplayPanel.message( 0, "machines monitor", "Creating machines file \"" + _machinesFileName.getText() + "\"" );
                            statusInfo( "creating \"" + _machinesFileName.getText() + "\"" );
                        }
                        else if ( packetType == THREADS_FILE_NAME ) {
                            _threadsFileName.setText( new String( data ) );
                            _messageDisplayPanel.message( 0, "machines monitor", "Creating threads file \"" + _threadsFileName.getText() + "\"" );
                            statusInfo( "creating \"" + _threadsFileName.getText() + "\"" );
                        }
                        else if ( packetType == WARNING_NO_THREADS_FILE_SPECIFIED ) {
                            workState = packetType;
                            _messageDisplayPanel.message( 0, "machines monitor", "No threads file name was specified - forming one using input file name." );
                        }
                        else if ( packetType == FAILURE_NO_FILES_SPECIFIED ) {
                            if ( workState == WARNING_NO_MACHINES_FILE_SPECIFIED )
                                _messageDisplayPanel.message( 0, "machines monitor", "No input file name specified - unable to form machines file name." );
                            else if ( workState == WARNING_NO_THREADS_FILE_SPECIFIED )
                                _messageDisplayPanel.message( 0, "machines monitor", "No input file name specified - unable to form threads file name." );
                            else
                                _messageDisplayPanel.message( 0, "machines monitor", "Unknown error involving missing file names." );
                        }
                        else if ( packetType == FAILURE_OPEN_MACHINES_FILE ) {
                            _machinesFileName.setText( new String( data ) );
                            _messageDisplayPanel.message( 0, "machines monitor", "Failure to open machines file (" + new String( data ) + ")" );
                            statusError( "could not open machines file \"" + new String( data ) + "\"" );
                        }
                        else if ( packetType == FAILURE_OPEN_THREADS_FILE ) {
                            _machinesFileName.setText( new String( data ) );
                            _messageDisplayPanel.message( 0, "machines monitor", "Failure to open threads file (" + new String( data ) + ")" );
                            statusError( "could not open threads file \"" + new String( data ) + "\"" );
                        }
                        else if ( packetType == MACHINES_FILE_CREATED ) {
                            _messageDisplayPanel.message( 0, "machines monitor", "machines file created" );
                            //  Download the machines file to its editor.
                            Component comp = _applyMachinesButton;
                            while ( comp.getParent() != null )
                                comp = comp.getParent();
                            Point pt = _applyMachinesButton.getLocationOnScreen();
                            GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                                    _machinesFileName.getText(), _settings );
                            if ( getFile.inString() != null && getFile.inString().length() > 0 ) {
                                _machinesFileEditor.text( getFile.inString() );
                            }
                            _messageDisplayPanel.message( 0, "machines monitor", "machines file successfully downloaded" );
                            statusInfo( ".machines file created" );
                        }
                        else if ( packetType == THREADS_FILE_CREATED ) {
                            _messageDisplayPanel.message( 0, "machines monitor", "threads file created" );
                            //  Download the threads file to its editor.
                            Component comp = _applyMachinesButton;
                            while ( comp.getParent() != null )
                                comp = comp.getParent();
                            Point pt = _applyMachinesButton.getLocationOnScreen();
                            GetFileMonitor getFile = new GetFileMonitor(  (Frame)comp, pt.x + 25, pt.y + 25,
                                    _threadsFileName.getText(), _settings );
                            if ( getFile.inString() != null && getFile.inString().length() > 0 ) {
                                _threadsFileEditor.text( getFile.inString() );
                            }
                            _messageDisplayPanel.message( 0, "machines monitor", "threads file successfully downloaded" );
                            statusInfo( ".threads file created" );
                        }
                        else if ( packetType == FAILURE_FILE_REMOVAL ) {
                            _messageDisplayPanel.error( 0, "machines monitor", "Failed to remove file on DiFX host: " + new String( data ) );
                            statusError( "permissions prevent removal of a file on DiFX host" );
                        }
                        else if ( packetType == FAILURE_POPEN ) {
                            _messageDisplayPanel.error( 0, "machines monitor", "Popen failed on DiFX host: " + new String( data ) );
                        }
                        else if ( packetType == FAILURE_MPIRUN ) {
                            PaneProcessorNode node = processorNodeByName( new String( data ) );
                            if ( node != null )
                                node.mpiTest( false, _eliminateNonrespondingProcessors.isSelected() );
                            if ( _eliminateNonrespondingProcessors.isSelected() )
                                _messageDisplayPanel.warning( 0, "machines monitor", "Processing node " + new String( data ) + " failed mpirun test - it will not be used" );
                            else
                                _messageDisplayPanel.warning( 0, "machines monitor", "Processing node " + new String( data ) + " failed mpirun test" );
                        }
                        else if ( packetType == SUCCESS_MPIRUN ) {
                            PaneProcessorNode node = processorNodeByName( new String( data ) );
                            if ( node != null )
                                node.mpiTest( true, true );
                        }
                        else if ( packetType == LOW_THREAD_COUNT ) {
                            _messageDisplayPanel.error( 0, "machines monitor", "Number of processing threads is zero" );
                        }
                        else if ( packetType == RUNNING_MPIRUN_TESTS ) {
                            _messageDisplayPanel.message( 0, "machines monitor", "Running mpirun tests." );
                            statusInfo( "running mpirun tests" );
                        }
                        else {
                            _messageDisplayPanel.warning( 0, "GUI", "Ignoring unrecongized job monitor packet type (" + packetType + ")." );
                        }
                    }
                    sock.close();
                } catch ( SocketTimeoutException e ) {
                }
                ssock.close();
            } catch ( java.io.IOException e ) {
                e.printStackTrace();
            }
            _settings.releaseTransferPort( _port );
        }
        
        protected int _port;
        
    }
    
    public void applyToAction() {
        _machinesApplyPopup.show( _applyToButton, 25, 25 );
    }
    
    /*
     * Called with the settings governing whether to check for "busy" processors
     * or how to define them are changed.
     */
    public void changeBusyProcessorsSettings() {
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            PaneProcessorNode thisNode = (PaneProcessorNode)(iter.next());
            boolean keepIt = thisNode.cpuTest( (float)_busyPercentage.value(),
                    _eliminateBusyProcessors.isSelected()  );
        }
    }
    
    public Iterator<BrowserNode> dataSourcesIterator() {
        return _dataSourcesPane.browserTopNode().children().iterator();
    }
    
    public Iterator<BrowserNode> processorsIterator() {
        return _processorsPane.browserTopNode().children().iterator();
    }
    
    public String headNode() { return _headNode.getText(); }
    public void headNode( String newVal ) { _headNode.setText( newVal ); }
    
    /*
     * Simple class to run and monitor the machines files generation routine in
     * a thread.
     */
    protected class ApplyMachinesThenStart extends Thread {
        @Override
        public void run() {
            MachinesDefinitionMonitor machines = applyMachinesAction();
            while ( machines.isAlive() )
                try { Thread.sleep( 100 ); } catch ( Exception e ) {}
            _machinesAppliedByHand = true;
            startJob();
        }
    }
    
    /*
     * This function runs the job based on all current settings.  Jobs can be
     * run using a TCP connection to guiServer, or through mk5daemon via UDP.  If
     * using the former, a thread is started to monitor the job progress via a
     * dedicated socket and report any errors.  In the latter case the job start
     * instruction is more of a "set and forget" kind of operation.
     */
    public void startJob() {
        _jobNode.running( true );
        setState( "Initializing", Color.YELLOW );
        setProgress( 0 );
        _jobNode.lockState( false );
        //  Has the user already generated .threads and .machines files (which is
        //  done when the "Apply" button in the Machines List settings is pushed)?
        //  Alternatively, has the use edited and uploaded .machines and .threads
        //  files by hand?  If these things have not been done, the files need to
        //  be generated before running.  The thread we create will do this and then
        //  restart the job.
        if ( !_machinesAppliedByHand ) {
            ApplyMachinesThenStart applyMachinesThread = new ApplyMachinesThenStart();
            applyMachinesThread.start();
            return;
        }
        DiFXCommand command = new DiFXCommand( _settings );
        command.header().setType( "DifxStart" );
        command.mpiProcessId( "-1" );
        command.identifier( _jobNode.name() );
        int monitorPort = 0;

        // Create start job command
        DifxStart jobStart = command.factory().createDifxStart();
        jobStart.setInput( _jobNode.inputFile() );

        // If we are using the TCP connection, set the address and port for diagnostic
        // reporting.
        if ( _settings.sendCommandsViaTCP() ) {
            jobStart.setAddress( _settings.guiServerConnection().myIPAddress() );
            monitorPort = _settings.newDifxTransferPort();
            jobStart.setPort( monitorPort );
        }
        
        jobStart.setFunction( "USNO" );

        // -- manager, enabled only
        DifxStart.Manager manager = command.factory().createDifxStartManager();
        manager.setNode( _headNode.getText() );
        jobStart.setManager( manager );

        // -- set difx version to use
        jobStart.setDifxVersion( _settings.difxVersion() );
        
        //  Set the "restart" time in seconds from the job start, if this has been
        //  requested.
        if ( _restartAt.isSelected() )
            jobStart.setRestartSeconds( _restartSeconds.value() );

        // -- datastreams, enabled only
        DifxStart.Datastream dataStream = command.factory().createDifxStartDatastream();

        //  Include all of the "checked" data stream node names.
        String dataNodeNames = "";
        for ( Iterator<BrowserNode> iter = _dataSourcesPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            DataSource thisNode = (DataSource)(iter.next());
            dataNodeNames += thisNode.sourceNode() + " ";
        }
        dataStream.setNodes( dataNodeNames );
        jobStart.setDatastream(dataStream);

        // Add enabled processors and threads.  Don't include processors that have no
        // threads!
        String processNodeNames = "";
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            PaneProcessorNode thisNode = (PaneProcessorNode)(iter.next());
            if ( thisNode.selected() ) {
                DifxStart.Process process = command.factory().createDifxStartProcess();
                process.setNodes( thisNode.name() );
                process.setThreads( thisNode.threadsText() );
                jobStart.getProcess().add( process );
            }
        }

        // force deletion of existing output file if this box has been checked.
        if ( _forceOverwrite.isSelected() )
            jobStart.setForce( 1 );
        else
            jobStart.setForce( 0 ); 
        
        //  Set up a monitor thread if this job is being run using guiServer.  This
        //  thread will collect and interpret diagnostic messages directly from
        //  guiServer as it sets up and runs the job.  If the job is being run by
        //  mk5daemon, it is simply started and forgotten.
        if ( _settings.sendCommandsViaTCP() ) {
            RunningJobMonitor runMonitor = new RunningJobMonitor( monitorPort );
            runMonitor.start();
        }
        
        // -- Create the XML defined messages and process through the system
        command.body().setDifxStart(jobStart);
        try {
            //command.sendPacket( _settings.guiServerConnection().COMMAND_PACKET );
            command.send();
        } catch ( java.net.UnknownHostException e ) {
            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null,
                    e.getMessage() );  //BLAT should be a pop-up
            setState( "Failed Start", Color.RED );
        }
    }
    
    /*
     * This thread opens and monitors a TCP socket for diagnostic reports from the
     * guiServer as it runs a job.
     */
    protected class RunningJobMonitor extends Thread {
        
        public RunningJobMonitor( int port ) {
            _port = port;
        }
        
        /*
         * These packet types are sent by the "JobMonitorConnection" class in the
         * guiServer application on the DiFX host.
         */
        protected final int JOB_TERMINATED                   = 100;
        protected final int JOB_ENDED_GRACEFULLY             = 101;
        protected final int JOB_STARTED                      = 102;
        protected final int PARAMETER_CHECK_IN_PROGRESS      = 103;
        protected final int PARAMETER_CHECK_SUCCESS          = 104;
        protected final int FAILURE_NO_HEADNODE              = 105;
        protected final int FAILURE_NO_DATASOURCES           = 106;
        protected final int FAILURE_NO_PROCESSORS            = 107;
        protected final int FAILURE_NO_INPUTFILE_SPECIFIED   = 108;
        protected final int FAILURE_INPUTFILE_NOT_FOUND      = 109;
        protected final int FAILURE_INPUTFILE_NAME_TOO_LONG  = 110;
        protected final int FAILURE_OUTPUT_EXISTS            = 111;
        protected final int DELETING_PREVIOUS_OUTPUT         = 112;
        protected final int STARTING_DIFX                    = 113;
        protected final int DIFX_MESSAGE                     = 114;
        protected final int DIFX_WARNING                     = 115;
        protected final int DIFX_ERROR                       = 116;
        protected final int DIFX_COMPLETE                    = 117;
        protected final int DATA_FILE_SIZE                   = 118;
        protected final int JOB_FAILED                       = 119;
        protected final int JOB_ENDED_WITH_ERRORS            = 120;
                
        @Override
        public void run() {
            //  Open a new server socket and await a connection.  The connection
            //  will timeout after a given number of seconds (nominally 10).
            try {
                ServerSocket ssock = new ServerSocket( _port );
                ssock.setSoTimeout( 10000 );  //  timeout is in millisec
                try {
                    Socket sock = ssock.accept();
//                    acceptCallback();
                    //  Turn the socket into a "data stream", which has useful
                    //  functions.
                    DataInputStream in = new DataInputStream( sock.getInputStream() );
                    
                    //  Loop collecting diagnostic packets from the guiServer.  These
                    //  are identified by an initial integer, and then are followed
                    //  by a data length, then data.
                    boolean connected = true;
                    while ( connected ) {
                        //  Read the packet type as an integer.  The packet types
                        //  are defined above (within this class).
                        int packetType = in.readInt();
                        //  Read the size of the incoming data (bytes).
                        int packetSize = in.readInt();
                        //  Read the data (as raw bytes)
                        byte [] data = null;
                        if ( packetSize > 0 ) {
                            data = new byte[packetSize];
                            in.readFully( data );
                        }
//                    _inString = "";
//                    incrementalCallback();
//                    while ( _inString.length() < _fileSize ) {
//                        int sz = _fileSize - _inString.length();
//                        if ( sz > 1024 )
//                            sz = 1024;
//                        byte [] data = new byte[sz];
//                        int n = in.read( data, 0, sz );
//                        //_inString += in.readUTF();
//                        _inString += new String( Arrays.copyOfRange( data, 0, n ) );
//                        incrementalCallback();
                        //  Interpret the packet type.
                        if ( packetType == JOB_FAILED ) {
                            _messageDisplayPanel.error( 0, "job monitor", "Job failed to complete." );
                            statusError( "job failed to complete" );
                            statusPanelColor( _statusPanelBackground.darker()  );
                            connected = false;
                            setState( "Failed", Color.RED );
                            _jobNode.lockState( true );
                        }
                        else if ( packetType == JOB_TERMINATED ) {
                            _messageDisplayPanel.warning( 0, "job monitor", "Job terminated by user." );
                            statusWarning( "job terminated by user" );
                            statusPanelColor( _statusPanelBackground.darker() );
                            connected = false;
                            setState( "Terminated", Color.RED );
                            _jobNode.lockState( true );
                        }
                        else if ( packetType == JOB_ENDED_GRACEFULLY ) {
                            _messageDisplayPanel.warning( 0, "job monitor", "Job finished gracefully." );
                            statusInfo( "job completed" );
                            connected = false;
                            statusPanelColor( _statusPanelBackground.darker() );
                        }
                        else if ( packetType == JOB_STARTED ) {
                            _doneWithErrors = false;
                            _messageDisplayPanel.message( 0, "job monitor", "Job started by guiServer." );
                            statusInfo( "job started" );
                        }
                        else if ( packetType == JOB_ENDED_WITH_ERRORS ) {
                            _doneWithErrors = true;
                            connected = false;
                        }
                        else if ( packetType == PARAMETER_CHECK_IN_PROGRESS ) {
                            _messageDisplayPanel.message( 0, "job monitor", "Checking parameters." );
                            statusInfo( "checking parameters..." );
                        }
                        else if ( packetType == PARAMETER_CHECK_SUCCESS ) {
                            _messageDisplayPanel.message( 0, "job monitor", "Parameter check successful." );
                        }
                        else if ( packetType == FAILURE_NO_HEADNODE ) {
                            _messageDisplayPanel.error( 0, "job monitor", "No headnone was specified." );
                        }
                        else if ( packetType == FAILURE_NO_DATASOURCES ) {
                            _messageDisplayPanel.error( 0, "job monitor", "No valid data sources were specified." );
                        }
                        else if ( packetType == FAILURE_NO_PROCESSORS ) {
                            _messageDisplayPanel.error( 0, "job monitor", "No valid processors were specified." );
                        }
                        else if ( packetType == FAILURE_NO_INPUTFILE_SPECIFIED ) {
                            _messageDisplayPanel.error( 0, "job monitor", "No input file was specified." );
                        }
                        else if ( packetType == FAILURE_INPUTFILE_NOT_FOUND ) {
                            _messageDisplayPanel.error( 0, "job monitor", "Input file " + _jobNode.inputFile() + " was not found on DiFX host." );
                        }
                        else if ( packetType == FAILURE_INPUTFILE_NAME_TOO_LONG ) {
                            _messageDisplayPanel.message( 0, "job monitor", "Input file name \"" + _jobNode.inputFile() + "\" is too long for DiFX." );
                        }
                        else if ( packetType == FAILURE_OUTPUT_EXISTS ) {
                            _messageDisplayPanel.error( 0, "job monitor", "Output exists for this job on DiFX host - use \"force\" to replace." );
                        }
                        else if ( packetType == DELETING_PREVIOUS_OUTPUT ) {
                            statusInfo( "force output - deleting existing output files" );
                            _messageDisplayPanel.warning( 0, "job monitor", "force output - deleting existing output files" );
                        }
                        else if ( packetType == STARTING_DIFX ) {
                            statusInfo( "DiFX running!" );
                            _messageDisplayPanel.warning( 0, "job monitor", "DiFX started!" );
                            statusPanelColor( Color.GREEN );                            //  turn the frame green!!!!
                            setState( "Starting", Color.YELLOW );
                        }
                        else if ( packetType == DIFX_MESSAGE ) {
                            if ( data != null )
                                _messageDisplayPanel.message( 0, "job monitor", new String( data ) );
                            else
                                _messageDisplayPanel.message( 0, "job monitor", "" );
                        }
                        else if ( packetType == DIFX_WARNING ) {
                            if ( data != null )
                                _messageDisplayPanel.warning( 0, "job monitor", new String( data ) );
                            else
                                _messageDisplayPanel.warning( 0, "job monitor", "" );
                        }
                        else if ( packetType == DIFX_ERROR ) {
                            if ( data != null )
                                _messageDisplayPanel.error( 0, "job monitor", new String( data ) );
                            else
                                _messageDisplayPanel.error( 0, "job monitor", "" );
                            statusPanelColor( Color.RED );
                            setState( "DiFX Error", Color.RED );
                        }
                        else if ( packetType == DIFX_COMPLETE ) {
                            statusInfo( "DiFX compete!" );
                            _messageDisplayPanel.warning( 0, "job monitor", "DiFX complete!" );
                            statusPanelColor( _statusPanelBackground.darker() );
                        }
                        else {
                            _messageDisplayPanel.warning( 0, "GUI", "Ignoring unrecongized job monitor packet type (" + packetType + ")." );
                        }
                    }
                    sock.close();
                } catch ( SocketTimeoutException e ) {
//                    _fileSize = -10;
                }
                ssock.close();
            } catch ( java.io.IOException e ) {
                e.printStackTrace();
//                _error = "IOException : " + e.toString();
//                _fileSize = -11;
            }
            //  We keep the state of this job "running" for a little bit so that we properly
            //  process any late messages.
            try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
            _jobNode.running( false );
            _settings.releaseTransferPort( _port );
        }
        
        protected int _port;
        
    }
    
    /*
     * Set the "state" of this job.  The state appears on both the editor/monitor
     * and the queue browser line.  It has a background color.
     */
    public void setState( String newState, Color newColor ) {
        _state.setText( newState );
        _state.setBackground( newColor );
        _jobNode.state().setText( newState );
        _jobNode.state().setBackground( newColor );
        _jobNode.state().updateUI();
    }
    
    /*
     * Set the "progress" of this job.  Progress appears on both the editor/monitor
     * and the queue browser line.
     */
    public void setProgress( int i ) {
        _progress.setValue( i );
        _jobNode.progress().setValue( i );
    }
    
    //  Consume a message for this job.  The source of these messages is mk5daemon
    //  processes on different nodes.
    public void consumeMessage( DifxMessage difxMsg ) {
        
        //  See what kind of message this is...try status first.
        if ( difxMsg.getBody().getDifxStatus() != null ) {
            if ( difxMsg.getBody().getDifxStatus().getVisibilityMJD() != null &&
                    difxMsg.getBody().getDifxStatus().getJobstartMJD() != null &&
                    difxMsg.getBody().getDifxStatus().getJobstopMJD() != null ) {
                _progress.setValue( (int)( 0.5 + 100.0 * ( Double.valueOf( difxMsg.getBody().getDifxStatus().getVisibilityMJD() ) -
                        Double.valueOf( difxMsg.getBody().getDifxStatus().getJobstartMJD() ) ) /
                        ( Double.valueOf( difxMsg.getBody().getDifxStatus().getJobstopMJD() ) -
                        Double.valueOf( difxMsg.getBody().getDifxStatus().getJobstartMJD() ) ) ) );
                //  Set the restart value.  Possibly some number of seconds should be added
                //  to this in the event a bad disk sector is causing a crash?
                _restartSeconds.value( ( Double.valueOf( difxMsg.getBody().getDifxStatus().getJobstopMJD() ) -
                        Double.valueOf( difxMsg.getBody().getDifxStatus().getJobstartMJD() ) ) / 3600.0 / 24.0 );
            }
            else if ( !difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase( "ending" ) )
                _progress.setValue( 0 );
            //  Only change the "state" of this job if it hasn't been "locked" by the GUI.  This
            //  happens when the GUI detects an error.  If this job is "starting" the state should
            //  be unlocked - it means another attempt is being made to run it.
            if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase( "starting" ) ) {
                _jobNode.lockState( false );
                _restartSeconds.value( 0.0 );
            }
            if ( !_jobNode.lockState() ) {
                _state.setText( difxMsg.getBody().getDifxStatus().getState() );
                if ( _state.getText().equalsIgnoreCase( "done" ) || _state.getText().equalsIgnoreCase( "mpidone" ) ) {
                    _restartSeconds.value( 0.0 );
                    if ( _doneWithErrors )  {
                        _state.setText( "Complete with Errors" );
                        _state.setBackground( Color.ORANGE );
                    }
                    else
                        _state.setBackground( Color.GREEN );
                    _progress.setValue( 100 );  
                }
                else if ( _state.getText().equalsIgnoreCase( "running" ) || _state.getText().equalsIgnoreCase( "starting" )
                         || _state.getText().equalsIgnoreCase( "ending" ) )
                    _state.setBackground( Color.YELLOW );
                else {
                    _state.setBackground( Color.LIGHT_GRAY );
                }
                _state.updateUI();
            }
            List<DifxStatus.Weight> weightList = difxMsg.getBody().getDifxStatus().getWeight();
            //  Create a new list of antennas/weights if one hasn't been created yet.
//            if ( _weights == null )
//                newWeightDisplay( weightList.size() );
//            for ( Iterator<DifxStatus.Weight> iter = weightList.iterator(); iter.hasNext(); ) {
//                DifxStatus.Weight thisWeight = iter.next();
//                weight( thisWeight.getAnt(), thisWeight.getWt() );
//            }
        }
        else if ( difxMsg.getBody().getDifxAlert() != null ) {
            //System.out.println( "this is an alert" );
            //System.out.println( difxMsg.getBody().getDifxAlert().getAlertMessage() );
            //System.out.println( difxMsg.getBody().getDifxAlert().getSeverity() );
        }
        
        //_messageDisplayPanel.message( 0, "mk5daemon", difxMsg.getBody().toString() );

    }
    
    public void pauseJob() {}
    
    public void stopJob() {
        setState( "Terminated", Color.RED );
        _jobNode.lockState( true );
        DiFXCommand command = new DiFXCommand( _settings );
        command.header().setType( "DifxStop" );
        command.mpiProcessId( "-1" );
        command.identifier( _jobNode.name() );

        // Create start job command
        DifxStop jobStop = command.factory().createDifxStop();
        jobStop.setInput( _jobNode.inputFile() );
        jobStop.setDifxVersion( _settings.difxVersion() );

        // -- Create the XML defined messages and process through the system
        command.body().setDifxStop( jobStop );
        try {
            //command.sendPacket( _settings.guiServerConnection().COMMAND_PACKET );
            command.send();
        } catch ( java.net.UnknownHostException e ) {
            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null,
                    e.getMessage() );  //BLAT should be a pop-up
        }
    }
    
    /*
     * Add a listener to "state change" events.  These occur when this class starts,
     * pauses, or stops a job, or recognizes that a job has finished running.
     */
    public void addActionListener( ActionListener a ) {
        _stateChangeListeners.add( ActionListener.class, a );
    }

    /*
     * Send a "state change" event to all listeners.
     */
    protected void stateChangeEvent() {
        Object[] listeners = _stateChangeListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }

    /*
     * This class is used to contain the name of a single data source node.  The data
     * source can be a Mark5, file, or network connection.  The controls necessary
     * for each of these options differ quite a bit, so they will be in inheriting
     * classes.
     */
    protected class DataNode extends BrowserNode {
        
        public DataNode( String name ) {
            super( name );
        }
        
        @Override
        public void createAdditionalItems() {
            _selected = new JCheckBox();
            _selected.setBackground( Color.WHITE );
            //  Any time the user specifically selects an item, we change it state
            //  slightly - it can't be automatically selected or unselected based
            //  on cpu load.
            _selected.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _handSelected = true;
                }
            });
            this.add( _selected );
            _dataObjectA = new JLabel( "" );
            this.add( _dataObjectA );
            _dataObjectB = new JLabel( "" );
            this.add( _dataObjectB );
            _antenna = new JLabel( "" );
            this.add( _antenna );
        }
        
        @Override
        public void positionItems() {
            super.positionItems();
            _selected.setBounds( 7, 2, 18, 18 );
            _label.setBounds( 30, 0, 215, _ySize );
            _dataObjectA.setBounds( 250, 0, 500, 25 );
            _dataObjectB.setBounds( 400, 0, 500, 25 );
            _antenna.setBounds( 550, 0, 500, 25 );
        }
        
        public void dataObjectA( String newVal ) { _dataObjectA.setText( newVal ); }
        public String dataObjectA() { return _dataObjectA.getText(); }
        public void dataObjectB( String newVal ) { _dataObjectB.setText( newVal ); }
        public String dataObjectB() { return _dataObjectB.getText(); }
        public void antenna( String newVal ) { _antenna.setText( newVal ); }
        public String antenna() { return _antenna.getText(); }
        
        public void foundA( boolean newVal ) {
            _foundA = newVal;
            if ( newVal ) {
                _dataObjectA.setForeground( Color.BLUE );
            }
            else {
                _dataObjectA.setForeground( Color.GRAY );
            }
        }
        public void foundB( boolean newVal ) {
            _foundB = newVal;
            if ( newVal ) {
                _dataObjectB.setForeground( Color.BLUE );
            }
            else {
                _dataObjectB.setForeground( Color.GRAY );
            }
        }
        public boolean foundA() { return _foundA; }
        public boolean foundB() { return _foundB; }
        
        public void checkFound( boolean doCheck ) {
            if ( doCheck && !_handSelected ) {
                if ( _foundA || _foundB )
                    _selected.setSelected( true );
                else
                    _selected.setSelected( false );
            }
        }
        
        public void missingA() {
            _missing = true;
            _dataObjectA.setForeground( Color.RED );
        }
        public boolean missing() { return _missing; }
        
        public boolean selected() { return _selected.isSelected(); }
        public void selected( boolean newVal ) { _selected.setSelected( newVal ); }
        public void hideSelection() { _selected.setVisible( false ); }
        public boolean hideSelected() { return !_selected.isVisible(); }
        public boolean handSelected() { return _handSelected; }
        public void handSelected( boolean newVal ) { _handSelected = newVal; }
        
        public boolean foundIt;
        protected JCheckBox _selected;
        protected boolean _handSelected;
        protected JLabel _dataObjectA;
        protected JLabel _dataObjectB;
        protected boolean _foundA;
        protected boolean _foundB;
        protected boolean _missing;
        protected JLabel _antenna;
    }
    
    /*
     * This class provides information about a "Processor".  It allows
     * the user to use the processor as the "head node" and tracks the number of
     * cores available.
     */
    protected class PaneProcessorNode extends BrowserNode {
        
        public PaneProcessorNode( String name ) {
            super( name );
        }
        
        @Override
        public void createAdditionalItems() {
            _selected = new JCheckBox();
            _selected.setBackground( Color.WHITE );
            //  Any time the user specifically selects an item, we change it state
            //  slightly - it can't be automatically selected or unselected based
            //  on cpu load.
            _selected.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _handSelected = true;
                }
            });
            this.add( _selected );
            _popup = new JPopupMenu();
            JMenuItem menuItem2 = new JMenuItem( "Make " + this.name() + " the head node" );
            menuItem2.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _headNode.setText( name() );
                }
            });
            _popup.add( menuItem2 );
            _threads = new NumberBox();
            _threads.setHorizontalAlignment( JTextField.RIGHT );
            _threads.intValue( 0 );
            _threads.minimum( 0 );
            this.add( _threads );
            _coresDisplay = new JLabel("");
            _coresDisplay.setHorizontalAlignment( JLabel.RIGHT );
            this.add( _coresDisplay );
            _cpuDisplay = new JLabel("");
            _cpuDisplay.setHorizontalAlignment( JLabel.RIGHT );
            this.add( _cpuDisplay );
            _mpiTestDisplay = new JLabel("");
            _mpiTestDisplay.setHorizontalAlignment( JLabel.RIGHT );
            this.add( _mpiTestDisplay );
        }
        
        @Override
        public void positionItems() {
            super.positionItems();
            _selected.setBounds( 7, 2, 18, 18 );
            _threads.setBounds( 210, 1, 30, 18 );
            _coresDisplay.setBounds( 280, 1, 30, 18 );
            _cpuDisplay.setBounds( 360, 1, 70, 18 );
            _mpiTestDisplay.setBounds( 460, 1, 70, 18 );
        }
        
        public void cores( int newVal ) { 
            _cores = newVal;
            _coresDisplay.setText( String.valueOf( newVal ) );
        }
        public int cores() { return _cores; }
        public void threads( int newVal ) { _threads.intValue( newVal ); }
        public Integer threads() { return _threads.intValue(); }
        public String threadsText() { return _threads.getText(); }
        public float cpu() { return Float.parseFloat( _cpuDisplay.getText() ); }
        public void cpu( String newVal ) {
            _cpuDisplay.setText( newVal );
        }
        /*
         * Change the color of the cpu load if it exceeds a limit.  Also, select
         * or deselect if desired.
         */
        public boolean cpuTest( float limit, boolean selectLimit ) {
            if ( Float.parseFloat( _cpuDisplay.getText() ) > limit ) {
                _cpuDisplay.setForeground( Color.RED );
                if ( selectLimit && !_handSelected )
                    _selected.setSelected( false );
                return false;
            }
            else {
                _cpuDisplay.setForeground( Color.BLACK );
                if ( selectLimit && !_handSelected )
                    _selected.setSelected( true );
                return true;
            }
        }
        /*
         * Set the result of the mpi test (and govern the selection if the test
         * failed).
         */
        public void mpiTest( boolean passed, boolean baseSelect ) {
            if ( passed ) {
                _mpiTestDisplay.setText( "OK" );
                _mpiTestDisplay.setForeground( Color.BLACK );
            }
            else {
                _mpiTestDisplay.setText( "Fail" );
                _mpiTestDisplay.setForeground( Color.RED );
                if ( baseSelect && !_handSelected ) {
                    _selected.setSelected( false );
                    _handSelected = true;
                }
            }           
        }
        
        public void clearTest() {
            _mpiTestDisplay.setText( "" );
        }
        
        public boolean selected() { return _selected.isSelected(); }
        public void selected( boolean newVal ) { _selected.setSelected( newVal ); }
        public void handSelected( boolean newVal ) { 
            _selected.setSelected( newVal );
            _handSelected = true;
        }        
        protected int _cores;
        protected NumberBox _threads;
        public boolean foundIt;
        protected JCheckBox _selected;
        protected JLabel _coresDisplay;
        protected JLabel _cpuDisplay;
        protected boolean _handSelected;
        protected JLabel _mpiTestDisplay;
        
    }
    
    /*
     * Fill the processor list from the Hardware Monitor and the data sources
     * list from our collected information.
     */
    public void loadHardwareLists() {
        
        //  We need to "relocate" everything in the existing processor list, so unset a
        //  "found" flag for each.
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); )
            ( (PaneProcessorNode)(iter.next()) ).foundIt = false;        
        //  These are all of the processing nodes that the hardware monitor knows
        //  about.  See if they are in the list.
        for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().processorNodes().children().iterator();
                iter.hasNext(); ) {
            BrowserNode thisModule = iter.next();
            if ( !( (ProcessorNode)(thisModule) ).ignore() ) {
                BrowserNode foundNode = null;
                //  Is this processor in our list?
                for ( Iterator<BrowserNode> iter2 = _processorsPane.browserTopNode().children().iterator();
                        iter2.hasNext() && foundNode == null; ) {
                    BrowserNode testNode = iter2.next();
                    if ( testNode.name().contentEquals( thisModule.name() ) ) {
                        foundNode = testNode;
                        ( (PaneProcessorNode)(testNode) ).foundIt = true;
                    }
                }
                //  New node?  Then add it to the list.
                if ( foundNode == null ) {
                    PaneProcessorNode newNode = new PaneProcessorNode( thisModule.name() );
                    newNode.cores( ((ProcessorNode)(thisModule)).numCores() );
                    if ( _restrictHeadnodeProcessing.isSelected() && thisModule.name().equalsIgnoreCase( _headNode.getText() ) )
                        newNode.threads( 0 );
                    else {
                        if ( ((ProcessorNode)(thisModule)).numCores() > 1 )
                            newNode.threads( ((ProcessorNode)(thisModule)).numCores() - 1 );
                        else
                            newNode.threads( 1 );
                    }
                    newNode.cpu( ((ProcessorNode)(thisModule)).cpuUsage() );
                    newNode.cpuTest( (float)_busyPercentage.value(),
                            _eliminateBusyProcessors.isSelected() );
                    newNode.foundIt = true;
                    newNode.selected( !_processorsEdited );
                    _processorsPane.addNode( newNode );
                }
                else {
                    ( (PaneProcessorNode)foundNode ).cpu( ((ProcessorNode)(thisModule)).cpuUsage() );
                    ( (PaneProcessorNode)foundNode ).cpuTest( (float)_busyPercentage.value(),
                            _eliminateBusyProcessors.isSelected() );
                }
            }
        }
        //  Now purge the list of any items that were not "found"....
        for ( Iterator<BrowserNode> iter = _processorsPane.browserTopNode().children().iterator();
                iter.hasNext(); ) {
            PaneProcessorNode testNode = (PaneProcessorNode)(iter.next());
            if ( !testNode.foundIt )
                _processorsPane.browserTopNode().removeChild( testNode );
            else {
                //  This lets us know if anyone is editing the list.  If so, we
                //  don't add new items "selected" by default.
                if ( !testNode.selected() )
                    _processorsEdited = true;
            }
        }
        
        //  Sort the remaining items.
        _processorsPane.browserTopNode().sortByName();
        
        updateDataSourceList();
    }
    
    /*
     * This class is used to contain the name of a single data source.  The data
     * source can be a module, file or list of files, or network connection.  The 
     * controls necessary for each of these options differ quite a bit, so they 
     * will be in inheriting classes.
     */
    protected class DataSource extends IndexedPanel {
        
        public DataSource( int index ) {
            super( "" );
            closedHeight( 20 );
            openHeight( 20 );
            drawFrame( false );
            noArrow( true );
            darkTitleBar( false );
            alwaysOpen( true );
            setBackground( Color.WHITE );
            _index = index;
            _antenna.setText( _inputFile.telescopeTable().idx[_index].name );
            _sourceFormat.setText( _inputFile.datastreamTable().idx[_index].dataFormat );
            _sourceType.setText( _inputFile.datastreamTable().idx[_index].dataSource );
            buildSourceNodeList();
            pickAppropriateSourceNode();
        }
        
        @Override
        public void createAdditionalItems() {
            _sourceNode = new StyledComboBox();
            _sourceNode.addPopupMenuListener( new PopupMenuListener() {
                public void popupMenuCanceled( PopupMenuEvent e) {
                }
                public void popupMenuWillBecomeInvisible( PopupMenuEvent e) {
                    setSelectedSourceNode();
                }
                public void popupMenuWillBecomeVisible( PopupMenuEvent e) {
                    buildSourceNodeList();
                }
            });
            _sourceNode.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                }
            });
            this.add( _sourceNode );
            _antenna = new JLabel( "" );
            _antenna.setHorizontalAlignment( JLabel.CENTER );
            this.add( _antenna );
            _sourceFormat = new JLabel( "" );
            _sourceFormat.setHorizontalAlignment( JLabel.LEFT );
            this.add( _sourceFormat );
            _sourceType = new JLabel( "" );
            _sourceType.setHorizontalAlignment( JLabel.LEFT );
            this.add( _sourceType );
        }
        
        @Override
        public void positionItems() {
            super.positionItems();
            _sourceNode.setBounds( 0, 1, 210, 18 );
            _antenna.setBounds( 210, 1, 40, 18 );
            _sourceFormat.setBounds( 250, 1, 70, 18 );
            _sourceType.setBounds( 330, 1, 70, 18 );
        }
        
        /*
         * Using all existing machines that can be used for the current data type,
         * build a list of nodes the user can pick from.  This generic form of the
         * function includes everything - Mark5s and processors.
         */
        public void buildSourceNodeList() {
            _sourceNode.removeAllItems();
            for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().processorNodes().children().iterator();
                    iter.hasNext(); ) {
                ProcessorNode thisModule = (ProcessorNode)(iter.next());
                _sourceNode.addItem( thisModule.name() );
            }
            for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().mk5Modules().children().iterator();
                    iter.hasNext(); ) {
                Mark5Node thisModule = (Mark5Node)(iter.next());
                _sourceNode.addItem( thisModule.name() );
            }
        }
        
        /*
         * This function is called when an item in the source node list is selected.
         * Some of the inheriting classes might do something special with this.
         */
        public void setSelectedSourceNode() {
        }
        
        /*
         * Of the current list of source nodes, pick the most appropriate one.  Which
         * one this is depends on the data source type so this is another inherited
         * class.
         */
        public void pickAppropriateSourceNode() {
        }
        
        public String sourceNode() { return (String)_sourceNode.getSelectedItem(); }
        public String sourceType() { return _sourceType.getText(); }
        
        protected int _index;
        protected StyledComboBox _sourceNode;
        protected JLabel _antenna;
        protected JLabel _sourceType;
        protected JLabel _sourceFormat;
 
        //  This class and the next make the popup list for the combo box as big as
        //  the items in it (as opposed to the same size as the combo box itself).
        class StyledComboBoxUI extends BasicComboBoxUI {
            protected ComboPopup createPopup() {
                BasicComboPopup popup = new BasicComboPopup( comboBox ) {
                    @Override
                    protected Rectangle computePopupBounds(int px,int py,int pw,int ph) {
                        return super.computePopupBounds(
                            px,py,Math.max(comboBox.getPreferredSize().width,pw),ph
                        );
                    }
                };
                popup.getAccessibleContext().setAccessibleParent(comboBox);
                return popup;
            }
        }

        class StyledComboBox extends JComboBox {
            public StyledComboBox() {
                setUI( new StyledComboBoxUI() );
                setBackground( Color.WHITE );
                this.setFont( new Font( this.getFont().getName(), Font.BOLD, this.getFont().getSize() ) );
            }
        }
    }
    
    /*
     * Data source from a module.
     */
    public class ModuleSource extends DataSource {
        
        public ModuleSource( int index ) {
            super( index );
            moduleName( _inputFile.dataTable().idx[index].file[0] );
            if ( _chooseBasedOnModule.isSelected() )
                pickAppropriateSourceNode();
        }
        
        public void createAdditionalItems() {
            super.createAdditionalItems();
            _moduleName = new JLabel( "" );
            _moduleName.setHorizontalAlignment( JLabel.LEFT );
            this.add( _moduleName );
        }
        
        public void positionItems() {
            super.positionItems();
            _moduleName.setBounds( 430, 1, 70, 18 );
        }

        /*
         * Search through the list of known source nodes and pick the best of them
         * for this module (the one that contains this module!).
         */
        public void pickAppropriateSourceNode() {
            for ( int i = 0; i < _sourceNode.getItemCount(); ++i ) {
                NodePanel selection = (NodePanel)_sourceNode.getItemAt( i );
                if ( selection.moduleA().contentEquals( _moduleName.getText() ) ||
                    selection.moduleB().contentEquals( _moduleName.getText() ) )
                    _sourceNode.setSelectedIndex( i );
            }
            checkSelectedSourceNode();  
        }
        
        /*
         * Check the modules contained in the selected source against the one we want.
         * Change the color of the label for the module we want to red if it doesn't
         * match one of those in the source.
         */
        public void checkSelectedSourceNode() {
            NodePanel selection = (NodePanel)_sourceNode.getSelectedItem();
             if ( selection == null ) return;
            if ( selection.moduleA().contentEquals( _moduleName.getText() ) ||
                 selection.moduleB().contentEquals( _moduleName.getText() ) ) {
                _moduleName.setForeground( Color.BLUE );
                _moduleName.setToolTipText( "" );
            }
            else {
                _moduleName.setForeground( Color.RED );
                _moduleName.setToolTipText( "Required module is not owned by this Mark5." );
            }
        }
        
        /*
         * Set the modules contained in the selected source.  The modules can change
         * over time (people can remove them or put new ones in) - calling this
         * function can respond to these changes.
         */
        public void updateSelectedSourceNode() {
            NodePanel selection = (NodePanel)_sourceNode.getSelectedItem();
            if ( selection == null ) return;
            for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().mk5Modules().children().iterator();
                    iter.hasNext(); ) {
                Mark5Node thisModule = (Mark5Node)(iter.next());
                if ( thisModule.name().contentEquals( selection.name() ) ) {
                    selection.moduleA( thisModule.bankAVSN() );
                    selection.moduleB( thisModule.bankBVSN() );
                }
            }
            checkSelectedSourceNode();
        }
        
        /*
         * Modules only exist on Mark5 machines - so only include them in the list
         * of possible machines.
         */
        public void buildSourceNodeList() {
            _sourceNode.removeAllItems();
            for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().mk5Modules().children().iterator();
                    iter.hasNext(); ) {
                Mark5Node thisModule = (Mark5Node)(iter.next());
                _sourceNode.addItem( new NodePanel( thisModule.name(), thisModule.bankAVSN(), thisModule.bankBVSN(),
                        thisModule.activeBank() ) );
            }
        }
        
        /*
         * We have to override this because we have complex objects instead of simple
         * strings in our source node list.
         */
        public String sourceNode() { 
            NodePanel selection = (NodePanel)_sourceNode.getSelectedItem();
            return selection.name();
        }
        
        public void moduleName( String newVal ) { _moduleName.setText( newVal ); }
        public String moduleName() { return _moduleName.getText(); }
        
        protected JLabel _moduleName;
        
        public class NodePanel extends JComponent {
            public NodePanel( String name, String moduleA, String moduleB, String activeBank ) {
                _name = name;
                _moduleA = moduleA;
                _moduleB = moduleB;
            }
            //  Override the "toString()" method to create a string out of the
            //  name and any modules contained.
            public String toString() {
                //  Try to get things to align properly...
                String rtn = _name + "                     ";
                if ( _moduleA.length() > 0 && !_moduleA.equalsIgnoreCase( "none" ) )
                    rtn += "          " + _moduleA;
                if ( _moduleB.length() > 0 && !_moduleB.equalsIgnoreCase( "none" ) )
                    rtn += "          " + _moduleB;
                return rtn;
            }
            public String name() { return _name; }
            public String moduleA() { return _moduleA; }
            public String moduleB() { return _moduleB; }
            public void moduleA( String newVal ) { _moduleA = newVal; }
            public void moduleB( String newVal ) { _moduleB = newVal; }
            public String activeBank() { return _activeBank; }
            protected String _name;
            protected String _moduleA;
            protected String _moduleB;
            protected String _activeBank;
            
        }
        
    }
    
    /*
     * Data source from a file.
     */
    public class FileSource extends DataSource {
        
        public FileSource( int index ) {
            super( index );
            //  File data can include a list of files, each of which we display.
            //  Find out how many files we have and create a label for each one.
            int n = _inputFile.dataTable().idx[index].numFiles;
            _file = new JLabel[n];
            for ( int i = 0; i < n; ++i ) {
                //  This first file in the list needs to indicate that the list is expandable.
                if ( i == 0 && n > 1 ) {
                    _shortLabel = _inputFile.dataTable().idx[index].file[i];
                    _longLabel = _shortLabel + "...";
                    _file[i] = new JLabel( _longLabel );
                    alwaysOpen( false );
                    _open = false;
                    _sillyFrame = new JLabel( "" );
                    _sillyFrame.setBounds( 410, 3, 14, 14 );
                    Border border = BorderFactory.createLineBorder( Color.BLACK );
                    _sillyFrame.setBorder(border);
                    this.add( _sillyFrame );
                    _openClose = new JLabel( "+" );
                    _openClose.setBounds( 411, 3, 12, 12 );
                    _openClose.setHorizontalAlignment( JLabel.CENTER );
                    _openClose.setVerticalAlignment( JLabel.CENTER );
                    this.add( _openClose );
                }
                else {
                    _file[i] = new JLabel( _inputFile.dataTable().idx[index].file[i] );
                    if ( n > 1 )
                        openHeight( openHeight() + 20 );
                }
                _file[i].setHorizontalAlignment( JLabel.LEFT );
                this.add( _file[i] );
            }
        }
        
        public void openCloseButton() {
            _open = ! _open;
            if ( _open ) {
                _openClose.setText( "-" );
                _file[0].setText( _shortLabel );
            }
            else {
                _openClose.setText( "+" );
                _file[0].setText( _longLabel );
            }
            this.updateUI();
        }
        
        public void positionItems() {
            super.positionItems();
            if ( _file != null ) {
                for ( int i = 0; i < _file.length; ++i )
                    _file[i].setBounds( 430, i * 20 + 1, 1000, 18 );
            }
        }
        
        

        /*
         * Override the mouseClicked event to make it work only when the "open/close"
         * button area is pushed.
         */
        @Override
        public void mouseClicked( MouseEvent e ) {
            if ( _openClose != null && e.getX() > 410 && e.getX() < 425 && e.getY() < 20 )
                openCloseButton();
        }

        protected JLabel[] _file;
        protected JLabel _openClose;
        protected JLabel _sillyFrame;
        protected String _shortLabel;
        protected String _longLabel;
    }
    
    /*
     * Data source from a network.
     */
    public class NetworkSource extends DataSource {
        
        public NetworkSource( int index ) {
            super( index );
        }
        
    }
    
    /*
     * Examine the current list of data sources for existence, sanity, and whatever
     * else.
     */
    synchronized public void buildDataSourceList() {
        //  Don't do anything if reading the input file failed.
        if ( _inputFile == null )
            return;
        //  Clear the existing data source list.
        _dataSourcesPane.clear();
        //  Add a new node for each data stream.  This is done based on source type.
        for ( int i = 0; i < _inputFile.commonSettings().activeDataStreams; ++i ) {
            if ( _inputFile.datastreamTable().idx[i].dataSource.contentEquals( "MODULE" ) )
                _dataSourcesPane.addNode( new ModuleSource( i ) );
            else if ( _inputFile.datastreamTable().idx[i].dataSource.contentEquals( "FILE" ) )
                _dataSourcesPane.addNode( new FileSource( i ) );
            else if ( _inputFile.datastreamTable().idx[i].dataSource.contentEquals( "NETWORK" ) )
                _dataSourcesPane.addNode( new NetworkSource( i ) );
            else //  Generic data source...not sure what good this is!
                _dataSourcesPane.addNode( new DataSource( i ) );
        }
    }
    
    /*
     * The data source list is built using the known data requirements (from
     * the .input file) in the "fillDataSourceList()" function.  Here was reflect any
     * changes in the state of data source machines or whatever associated with the
     * items on this list.
     */
    public void updateDataSourceList() {
        
        //  Look at each item in the data source list and check whether the node
        //  specified is appropriate to the data source itself - e.g. whether a chosen
        //  Mark5 has a needed module.
        for ( Iterator<BrowserNode> iter = _dataSourcesPane.browserTopNode().children().iterator();
            iter.hasNext(); ) {
            DataSource thisSource = (DataSource)iter.next();
            //  If this is a module source we check the modules "owned" by the source
            //  against those required.
            if ( thisSource.sourceType().contentEquals( "MODULE" ) ) {
                ModuleSource thisModuleSource = (ModuleSource)thisSource;
                thisModuleSource.updateSelectedSourceNode();
            }
            else if ( thisSource.sourceType().contentEquals( "FILE" ) ) {
            }
            else if ( thisSource.sourceType().contentEquals( "NETWORK" ) ) {
            }
            else {
            }
        }

    }
                
    /*
     * Called once a second to update things.
     */
    protected void timeoutIntervalEvent() {
        if ( this.isVisible() )
            loadHardwareLists();
    }
    
    /*
     * Override to load hardware lists before display.
     */
    @Override
    public void setVisible( boolean newVal ) {
        if ( newVal )
            loadHardwareLists();
        super.setVisible( newVal );
    }
    
    /*
     * Parse the string data in the .input file editor.
     */
    synchronized public void parseInputFile() {
        
        String str = _inputFileEditor.text();
        _inputFile = new InputFileParser( str );
        
        //  Extract some items from the .input file data.  
        _jobNode.calcFile( _inputFile.commonSettings().calcFile, true );
         _jobNode.outputFile( _inputFile.commonSettings().outputFile );
        executeTime( _inputFile.commonSettings().executeTime );
        startMJD( _inputFile.commonSettings().startMJD );
        startSeconds( _inputFile.commonSettings().startSeconds );
        _jobNode.numAntennas( _inputFile.telescopeTable().num );
        for ( int i = 0; i < _inputFile.telescopeTable().num; ++i )
            _jobNode.antennaName( i, _inputFile.telescopeTable().idx[i].name );
        
        
        _jobNode.jobStart( (double)startMJD() + (double)startSeconds() / 24.0 / 3600.0 );
        _jobNode.jobDuration( (double)executeTime() / 24.0 / 3600.0 );
        _jobNode.updateDatabase( "outputFile", _jobNode.outputFile() );
        _jobNode.updateDatabase( "jobStart", _jobNode.jobStart().toString() );
        _jobNode.updateDatabase( "jobDuration", _jobNode.jobDuration().toString() );
        _jobNode.updateDatabase( "numAntennas", _jobNode.numAntennas().toString() );
        
        buildDataSourceList();

    }
    
    /*
     * Give this job a link to the editor, which we need for some things.
     */
    public void editor( ExperimentEditor newVal ) { _editor = newVal; }

    /*
     * Parse the string data as if it came from an .input file (which, presumably,
     * it did).
     */
    public void parseCalcFile( String str ) {

        _calcFileEditor.text( str );
        Scanner strScan = new Scanner( str );
        strScan.useDelimiter( System.getProperty( "line.separator" ) );

        while ( strScan.hasNext() ) {
            String sCalc = strScan.next();
                if (sCalc.contains("JOB ID:")) {
                    sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
//                    setJobID(sCalc.trim());
                } else if (sCalc.contains("OBSCODE:")) {
                    sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
//                    setObsCode(sCalc.trim());
                } else if (sCalc.contains("JOB START TIME:")) {
                    sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
//                    setJobStartTimeMJD( new BigDecimal( sCalc.trim() ) );
                } else if (sCalc.contains("JOB STOP TIME:")) {
                    sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
//                    setJobStopTimeMJD(new BigDecimal(sCalc.trim()));
                } else if (sCalc.contains("NUM TELESCOPES:")) {
                    sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
//                    setNumTelescopes(Integer.parseInt(sCalc.trim()));
                } else if (sCalc.contains("DIFX VERSION:")) {
                    sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
                    _jobNode.difxVersion( sCalc.trim() );
//                    setDifxVersion(sCalc.trim());
                } else if ( sCalc.contains( "DUTY CYCLE:" ) ) {
                    sCalc = sCalc.substring( sCalc.indexOf( ":" ) + 1 );
                    _jobNode.dutyCycle( Double.parseDouble( sCalc.trim() ) );
                } else if (sCalc.contains("NAME:")) {
                    sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
                } else if (sCalc.contains("SHELF:")) {
                    String sCalcObjID = sCalc.substring(sCalc.indexOf("SHELF") - 3, sCalc.indexOf("SHELF"));
                    String sCalcShelf = sCalc.substring(sCalc.indexOf(":") + 1);

                    // Find antenna via object ID
///                    Module curMod = getModule(Integer.parseInt(sCalcObjID.trim()));

                    // update the antenna's shelf
//                    curMod.setShelf(sCalcShelf.trim());

                    //newJob.setNumTelescopes(Integer.parseInt(trimmed));
                }
        }

        _jobNode.updateDatabase( "difxVersion", _jobNode.difxVersion() );
        _jobNode.updateDatabase( "dutyCycle", _jobNode.dutyCycle().toString() );

//            frCalc.close();
            //System.out.printf("***************** Data model read input and calc file complete. \n");
    }
    
    public Integer startMJD() { return _startMJD; }
    public void startMJD( Integer newVal ) { _startMJD = newVal; }
    public Integer startSeconds() { return _startSeconds; }
    public void startSeconds( Integer newVal ) { _startSeconds = newVal; }
    public Integer executeTime() { return _executeTime; }
    public void executeTime( Integer newVal ) { _executeTime = newVal; }
   
    protected EventListenerList _stateChangeListeners;
    protected JobNode _jobNode;
    protected SystemSettings _settings;
    
    protected NodeBrowserScrollPane _scrollPane;
    
    protected JMenuBar _menuBar;
    protected JFormattedTextField _headNode;
    protected JLabel _headNodeLabel;
    protected NodeBrowserScrollPane _dataSourcesPane;
    protected JLabel _dataSourcesLabel;
    protected NodeBrowserScrollPane _processorsPane;
    protected JLabel _processorsLabel;
    protected JLabel _threadsLabel;
    protected JLabel _coresLabel;
    protected JLabel _cpuUsageLabel;
    protected JLabel _mpiTestLabel;
    protected JPopupMenu _machinesApplyPopup;
    protected JMenuItem _thisJobItem;
    protected JMenuItem _passJobsItem;
    protected JMenuItem _selectedJobsItem;
    protected JMenuItem _allJobsItem;
    protected JButton _applyToButton;
    protected JButton _applyMachinesButton;
    protected boolean _machinesAppliedByHand;
    protected JCheckBox _machinesLock;
    protected JCheckBox _forceOverwrite;
    protected JButton _selectAllProcessorsButton;
    protected JButton _deselectAllProcessorsButton;

    
    protected JButton _startButton;
    protected JButton _stopButton;
    protected NumberBox _restartSeconds;
    protected JCheckBox _restartAt;
    
    protected boolean _allObjectsBuilt;
    
    protected HardwareMonitorPanel _hardwareMonitor;
    
    protected boolean _processorsEdited;
    protected SimpleTextEditor _inputFileEditor;
    protected SimpleTextEditor _calcFileEditor;
    protected JButton _refreshInputButton;
    protected JButton _refreshCalcButton;
    protected JButton _uploadInputButton;
    protected JButton _uploadCalcButton;
    protected JLabel _calcFileName;
    protected JLabel _inputFileName;
    protected SimpleTextEditor _machinesFileEditor;
    protected JButton _refreshMachinesButton;
    protected JButton _uploadMachinesButton;
    protected JLabel _machinesFileName;
    protected SimpleTextEditor _threadsFileEditor;
    protected JButton _refreshThreadsButton;
    protected JButton _uploadThreadsButton;
    protected JLabel _threadsFileName;
    protected ExperimentEditor _editor;
    protected JCheckBox _restrictHeadnodeProcessing;
    protected JCheckBox _eliminateNonrespondingProcessors;
    protected JCheckBox _eliminateBusyProcessors;
    protected NumberBox _busyPercentage;
    protected JLabel _busyPercentageLabel;
    protected JCheckBox _chooseBasedOnModule;
    protected JLabel _statusLabel;
    protected JButton _showMonitorButton;
    
    protected ArrayList<String> _dataObjects;
    
    protected Integer _executeTime;
    protected Integer _startMJD;
    protected Integer _startSeconds;
    
    protected UpdateThread _updateThread;
    protected Timer _timeoutTimer;
    
    protected MessageDisplayPanel _messageDisplayPanel;
    protected IndexedPanel _statusPanel;
    protected Color _statusPanelBackground;
    
    protected JProgressBar _progress;
    protected ColumnTextArea _state;
    
    protected LiveMonitorWindow _liveMonitorWindow;
    
    protected boolean _doneWithErrors;
    public boolean doneWithErrors() { return _doneWithErrors; }
    //protected int _numDataStreams;
    //protected Vector<String> _dataSources;
    
    protected InputFileParser _inputFile;
    
}
