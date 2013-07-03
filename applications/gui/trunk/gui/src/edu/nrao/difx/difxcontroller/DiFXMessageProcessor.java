/*
 * This thread maintains and reads from a queue of DiFX multicast messages (the
 * messages sent from all DiFX processes).  These messages are collected by the
 * Multicast Monitor thread.
 * 
 * Messages are distributed based on their type in the "processPacket()" function.
 * In general this is done through event callbacks - classes must register their
 * interest in a particular message type.
 */
package edu.nrao.difx.difxcontroller;

import edu.nrao.difx.difxview.SystemSettings;

import edu.nrao.difx.xmllib.difxmessage.*;
import java.awt.Color;
import java.awt.Font;
import java.io.ByteArrayInputStream;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.event.EventListenerList;
import java.awt.EventQueue;
import java.awt.event.ComponentEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import mil.navy.usno.widgetlib.ActivityMonitorLight;
import mil.navy.usno.widgetlib.SimpleTextEditor;
import mil.navy.usno.widgetlib.NumberBox;
import mil.navy.usno.widgetlib.SaneTextField;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JSplitPane;
import javax.swing.JPanel;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.ArrayDeque;
import java.util.Iterator;
import javax.swing.JScrollPane;
import javax.swing.JScrollBar;
import javax.swing.JTable;
import javax.swing.JPopupMenu;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JSeparator;
import javax.swing.table.DefaultTableModel;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

public class DiFXMessageProcessor extends Thread
{

    protected boolean _done = false;
    protected BlockingQueue<ByteArrayInputStream> _messageQueue;
    protected JAXBPacketProcessor _packetProcessor;
    protected SystemSettings _settings;
    protected EventListenerList _difxStatusMessageListeners;
    protected EventListenerList _difxAlertMessageListeners;
    protected EventListenerList _difxLoadMessageListeners;
    protected EventListenerList _mark5StatusMessageListeners;
    protected EventListenerList _difxSmartMessageListeners;
    protected EventListenerList _difxInfoMessageListeners;

    public DiFXMessageProcessor( SystemSettings systemSettings )
    {
        _settings = systemSettings;
        _messageQueue = new LinkedBlockingQueue<ByteArrayInputStream>();
        _packetProcessor = new JAXBPacketProcessor( systemSettings.jaxbPackage() );
        _difxStatusMessageListeners = new EventListenerList();
        _difxAlertMessageListeners = new EventListenerList();
        _difxLoadMessageListeners = new EventListenerList();
        _mark5StatusMessageListeners = new EventListenerList();
        _difxSmartMessageListeners = new EventListenerList();
        _difxInfoMessageListeners = new EventListenerList();
        _difxMessageWindow = new DiFXMessageWindow( 500, 500 );
    }

    public void addDifxStatusMessageListener( AttributedMessageListener a ) {
        _difxStatusMessageListeners.add( AttributedMessageListener.class, a );
    }

    public void addDifxAlertMessageListener( AttributedMessageListener a ) {
        _difxAlertMessageListeners.add( AttributedMessageListener.class, a );
    }

    public void addDifxLoadMessageListener( AttributedMessageListener a ) {
        _difxLoadMessageListeners.add( AttributedMessageListener.class, a );
    }

    public void addMark5StatusMessageListener( AttributedMessageListener a ) {
        _mark5StatusMessageListeners.add( AttributedMessageListener.class, a );
    }

    public void addDifxSmartMessageListener( AttributedMessageListener a ) {
        _difxSmartMessageListeners.add( AttributedMessageListener.class, a );
    }

    public void addDifxInfoMessageListener( AttributedMessageListener a ) {
        _difxInfoMessageListeners.add( AttributedMessageListener.class, a );
    }

    public void shutDown() {
        _done = true;
    }

    /*
    * Add a new message to the queue.
    */
    public boolean add( ByteArrayInputStream pack ) {
        try {
            return ( _messageQueue.offer( pack ) );
        }
        catch ( NullPointerException e ) {
            return false;
        }
    }

    /*
     * Process a message packet - unmarshall into a DifxMessage and act on it
     * based on message type.  The actions for different messages are in functions
     * below simply to avoid cluttering this function.
     */
    public synchronized void processMessage( ByteArrayInputStream packet) {

        // Process the message packet into a DiFXMessage
        DifxMessage difxMsg = _packetProcessor.ConvertToJAXB( packet );
        packet.reset();

        //  Then figure out what to do with it based on its type.
        if ( difxMsg != null ) {
            Header header = difxMsg.getHeader();
            if ( _difxMessageWindow != null )
                _difxMessageWindow.newMessage( header.getType(), header.getFrom(), packet, difxMsg );

            if ( header.getType().equalsIgnoreCase( "DifxStatusMessage" ) ) {
                processDifxStatusMessage( difxMsg );

            } else if ( header.getType().equalsIgnoreCase( "Mark5StatusMessage" ) ) {
                processMark5StatusMessage( difxMsg );

            } else if ( header.getType().equalsIgnoreCase( "DifxLoadMessage" ) ) {
                processDifxLoadMessage( difxMsg );

            } else if ( header.getType().equalsIgnoreCase( "DifxAlertMessage" ) ) {
                processDifxAlertMessage( difxMsg );

            } else if ( header.getType().equalsIgnoreCase( "DifxSmartMessage" ) ) {
                processDifxSmartMessage( difxMsg );

            } else if ( header.getType().equalsIgnoreCase( "DifxInfoMessage" ) ) {
                processDifxInfoMessage( difxMsg );

            } else {
                if ( !_settings.suppressWarnings() ) {
                    java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.WARNING, "unknown DiFX message: \""
                            + header.getType() + "\"");
                }
            }

            // clean up
            header = null;
        }
        else {
            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.WARNING, "unparseable DiFX message type" );
        }

        // clean up
        difxMsg = null;

    }

    /*
     * The thread loops forever, dequeueing message packets and processing them.
     * It can be terminated using the "shutdown()" function.
     */
    @Override
    public void run() {
        ByteArrayInputStream packet = null;

        while ( !_done ) {
            
            try {
                packet = _messageQueue.take();
                if (packet != null) {
                    processMessage( packet );
                }
                else {
                    java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.WARNING, "null packet in queue" );
                }
                // clean up
                packet = null;

                if ( Thread.currentThread().isInterrupted() == true ) {
                    System.out.println( "**************** Process message thread interrupted." );
                    _done = true;
                }
            }
            catch (InterruptedException exception) {
                Thread.interrupted();
                System.out.println( "**************** Process message thread caught interrupt - done." );
                _done = true;
            }

        }
        
    }
    
    /*
     * What to do with a DiFX Status Message.
     */
    protected void processDifxStatusMessage( DifxMessage difxMsg ) {
        Object[] listeners = _difxStatusMessageListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == AttributedMessageListener.class )
                ((AttributedMessageListener)listeners[i+1]).update( difxMsg );
        }
    }

    /*
     * Process a DiFX Alert messages.  Not all "alerts" are bad, or even important
     * things - many of them are basically status messages.
     */
    protected void processDifxAlertMessage( DifxMessage difxMsg ) {
        Object[] listeners = _difxAlertMessageListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == AttributedMessageListener.class )
                ((AttributedMessageListener)listeners[i+1]).update( difxMsg );
        }

        //  Send this alert to the internal message reporting system, unless we don't
        //  have access to it - in which case we simply use the logging system.
        if ((difxMsg.getBody().getDifxAlert().getSeverity() >= 0)
                && (difxMsg.getBody().getDifxAlert().getSeverity() <= 4)) {

            if ( _settings.messageCenter() != null) {
                if (difxMsg.getBody().getDifxAlert().getSeverity() < 3) {
                    _settings.messageCenter().error(0, difxMsg.getHeader().getFrom() + " : "
                            + difxMsg.getHeader().getIdentifier(),
                            difxMsg.getBody().getDifxAlert().getAlertMessage().toString());
                } else if (difxMsg.getBody().getDifxAlert().getSeverity() < 4) {
                    _settings.messageCenter().warning(0, difxMsg.getHeader().getFrom() + " : "
                            + difxMsg.getHeader().getIdentifier(),
                            difxMsg.getBody().getDifxAlert().getAlertMessage().toString());
                } else {
                    _settings.messageCenter().message(0, difxMsg.getHeader().getFrom() + " : "
                            + difxMsg.getHeader().getIdentifier(),
                            difxMsg.getBody().getDifxAlert().getAlertMessage().toString());
                }
            } else {
                if (difxMsg.getBody().getDifxAlert().getSeverity() < 3) {
                    java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE,
                            difxMsg.getBody().getDifxAlert().getAlertMessage().toString());
                } else if (difxMsg.getBody().getDifxAlert().getSeverity() < 4) {
                    java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.WARNING,
                            difxMsg.getBody().getDifxAlert().getAlertMessage().toString());
                } else {
                    java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.INFO,
                            difxMsg.getBody().getDifxAlert().getAlertMessage().toString());
                }
            }
        }
            
    }

    protected void processDifxLoadMessage( DifxMessage difxMsg ) {
        Object[] listeners = _difxLoadMessageListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == AttributedMessageListener.class )
                ((AttributedMessageListener)listeners[i+1]).update( difxMsg );
        }
    }

    protected void processMark5StatusMessage( DifxMessage difxMsg ) {
        Object[] listeners = _mark5StatusMessageListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == AttributedMessageListener.class )
                ((AttributedMessageListener)listeners[i+1]).update( difxMsg );
        }
    }

    protected void processDifxSmartMessage( DifxMessage difxMsg ) {
        Object[] listeners = _difxSmartMessageListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == AttributedMessageListener.class )
                ((AttributedMessageListener)listeners[i+1]).update( difxMsg );
        }
    }

    protected void processDifxInfoMessage( DifxMessage difxMsg ) {
        Object[] listeners = _difxInfoMessageListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == AttributedMessageListener.class )
                ((AttributedMessageListener)listeners[i+1]).update( difxMsg );
        }
    }
    
    DiFXMessageWindow _difxMessageWindow;
    
    /*
     * Display a window showing details for each incoming message (see class below!).
     */
    public void showWindow() {
        if ( _difxMessageWindow == null )
            _difxMessageWindow = new DiFXMessageWindow( 500, 500 );
        _difxMessageWindow.setVisible( true );
    }
    
    /*
     * This class shows a window that can be used to examine messages received.
     */
    protected class DiFXMessageWindow extends JFrame {
        public DiFXMessageWindow( int x, int y ) {
            _settings.setLookAndFeel();
            this.setLayout( null );
            this.setBounds( x, y, _settings.windowConfiguration().difxMessageWindowW,
                    _settings.windowConfiguration().difxMessageWindowH );
            this.getContentPane().setLayout( null );
            _this = this;
            _this = this;
            _this.setTitle( "DiFX Message Traffic Monitor" );
            _activity = new ActivityMonitorLight();
            _this.add( _activity );
            _activityLabel = new JLabel( "Activity: " );
            _activityLabel.setHorizontalAlignment( JLabel.RIGHT );
            _this.add( _activityLabel );
            _messageLimit = new NumberBox();
            _messageLimit.precision( 0 );
            _messageLimit.setToolTipText( "Limit to the number of DiFX messages that are buffered." );
            _messageLimit.intValue( _settings.windowConfiguration().difxMessageWindowMessageLimit );
            _messageLimit.minimum( 1.0 );
            _this.add( _messageLimit );
            _messageLimitLabel = new JLabel( "Message Buffer: " );
            _messageLimitLabel.setHorizontalAlignment( JLabel.RIGHT );
            _this.add( _messageLimitLabel );
            _showButton = new JButton( "Show Type..." );
            _showButton.setToolTipText( "Show Selected DiFX Message Types." );
            _this.add( _showButton );
            _showButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _showMenu.show( _showButton, 0, 25 );
                }
            });
            this.add( _showButton );
            _showMenu = new JPopupMenu( "Show Message Types:" );
            _allItem = new JCheckBoxMenuItem( "All Messages" );
            _allItem.setSelected( true );
            _allItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _allItem );
            _showMenu.add( new JSeparator() );
            _alertItem = new JCheckBoxMenuItem( "DifxAlertMessage" );
            _alertItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _alertItem );
            _commandItem = new JCheckBoxMenuItem( "DifxCommandMessage" );
            _commandItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _commandItem );
            _fileOperationItem = new JCheckBoxMenuItem( "DifxFileOperationMessage" );
            _fileOperationItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _fileOperationItem );
            _fileTransferItem = new JCheckBoxMenuItem( "DifxFileTransferMessage" );
            _fileTransferItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _fileTransferItem );
            _getDirectoryItem = new JCheckBoxMenuItem( "DifxGetDirectoryMessage" );
            _getDirectoryItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _getDirectoryItem );
            _infoItem = new JCheckBoxMenuItem( "DifxInfoMessage" );
            _infoItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _infoItem );
            _loadItem = new JCheckBoxMenuItem( "DifxLoadMessage" );
            _loadItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _loadItem );
            _machinesDefinitionItem = new JCheckBoxMenuItem( "DifxMachinesDefinitionMessage" );
            _machinesDefinitionItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _machinesDefinitionItem );
            _mk5ControlItem = new JCheckBoxMenuItem( "DifxMk5ControlMessage" );
            _mk5ControlItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _mk5ControlItem );
            _smartItem = new JCheckBoxMenuItem( "DifxSmartMessage" );
            _smartItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _smartItem );
            _statusItem = new JCheckBoxMenuItem( "DifxStatusMessage" );
            _statusItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _statusItem );
            _stopItem = new JCheckBoxMenuItem( "DifxStopMessage" );
            _stopItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _stopItem );
            _vex2DifxRunItem = new JCheckBoxMenuItem( "DifxVex2DifxRunMessage" );
            _vex2DifxRunItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _vex2DifxRunItem );
            _weightItem = new JCheckBoxMenuItem( "DifxWeightMessage" );
            _weightItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _weightItem );
            _mark5StatusItem = new JCheckBoxMenuItem( "Mark5StatusMessage" );
            _mark5StatusItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _mark5StatusItem );
            _unknownItem = new JCheckBoxMenuItem( "Unknown Message Type" );
            _unknownItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    rebuildTable();
                }
            });
            _showMenu.add( _unknownItem );

            _mainSplitPane = new JSplitPane();
            _mainSplitPane.setDividerSize( 3 );
            _topPanel = new JPanel();
            _topPanel.setLayout( null );
            _bottomSplitPane = new JSplitPane();
            _bottomSplitPane.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    _this.newSize();
                }
                public void componentMoved( ComponentEvent e ) {
                    _this.newSize();
                }
            });
            _mainSplitPane.setTopComponent( _topPanel );
            _mainSplitPane.setOrientation( JSplitPane.VERTICAL_SPLIT );
            _mainSplitPane.setBottomComponent( _bottomSplitPane );
            _mainSplitPane.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    _this.newSize();
                }
                public void componentMoved( ComponentEvent e ) {
                    _this.newSize();
                }
            });
            _middlePanel = new JPanel();
            _middlePanel.setLayout( null );
            _middlePanel.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    _this.newSize();
                }
                public void componentMoved( ComponentEvent e ) {
                    _this.newSize();
                }
            });
            _bottomPanel = new JPanel();
            _bottomPanel.setLayout( null );
            _bottomPanel.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    _this.newSize();
                }
                public void componentMoved( ComponentEvent e ) {
                    _this.newSize();
                }
            });
            _bottomSplitPane.setTopComponent( _middlePanel );
            _bottomSplitPane.setOrientation( JSplitPane.VERTICAL_SPLIT );
            _bottomSplitPane.setDividerSize( 3 );
            _bottomSplitPane.setBottomComponent( _bottomPanel );
            _bottomSplitPane.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    _this.newSize();
                }
                public void componentMoved( ComponentEvent e ) {
                    _this.newSize();
                }
            });
            _this.add( _mainSplitPane );
            
            //  Collect resize events.
            this.addComponentListener(new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    _this.newSize();
                }
                public void componentMoved( ComponentEvent e ) {
                    _this.newSize();
                }
            });
            _messageTable = new DefaultTableModel(
                    new Object[]{ "Time",
                                  "Type",
                                  "From"
                    }, 0 );
            _messageJTable = new JTable( _messageTable );
            _messageJTable.setShowGrid( false );
            _messageJTable.getSelectionModel().addListSelectionListener( new ListSelectionListener() {
                public void valueChanged( ListSelectionEvent e ) {
                    tableSelect( _messageJTable.getSelectedRow() );
                }
            } );
            _tableScrollPane = new JScrollPane( _messageJTable );
            
            _topPanel.add( _tableScrollPane );
            
            _messageText = new SimpleTextEditor();
            _messageText.textArea().setFont( new Font( "Courier", Font.PLAIN, 12 ) );
            _middlePanel.add( _messageText );
            
            //  The bottom panel contains displays of all items in the message.
            //  The "header" is common to all message types.
            _type = new TextField( "Type: ", 150 );
            _bottomPanel.add( _type );
            _from = new TextField( "From: ", 150 );
            _bottomPanel.add( _from );
            _to = new TextField( "To: ", 150 );
            _bottomPanel.add( _to );
            _mpiProcessId = new TextField( "MPI Process ID: ", 150 );
            _bottomPanel.add( _mpiProcessId );
            _id = new TextField( "Identifier: ", 150 );
            _bottomPanel.add( _id );
            _sequenceNumber = new TextField( "Sequence Number: ", 150 );
            _bottomPanel.add( _sequenceNumber );

            //  Then there are specific panels for each type of message.
            _alertPanel = new JPanel();
            _alertPanel.setLayout( null );
            _alertMessage = new TextField( "Message: ", 150 );
            _alertPanel.add( _alertMessage );
            _alertSeverity = new TextField( "Severity: ", 150 );
            _alertPanel.add( _alertSeverity );
            _bottomPanel.add( _alertPanel );
            
            _commandPanel = new JPanel();
            _commandPanel.setLayout( null );
            _commandCommand = new TextField( "Command: ", 150 );
            _commandPanel.add( _commandCommand );
            _bottomPanel.add( _commandPanel );
            
            _fileOperationPanel = new JPanel();
            _fileOperationPanel.setLayout( null );
            _fileOperationOperation = new TextField( "Operation: ", 150 );
            _fileOperationPanel.add( _fileOperationOperation );
            _fileOperationArg = new TextField( "Arguments: ", 150 );
            _fileOperationPanel.add( _fileOperationArg );
            _fileOperationPath = new TextField( "Path: ", 150 );
            _fileOperationPanel.add( _fileOperationPath );
            _fileOperationDataNode = new TextField( "Data Node: ", 150 );
            _fileOperationPanel.add( _fileOperationDataNode );
            _fileOperationAddress = new TextField( "Address: ", 150 );
            _fileOperationPanel.add( _fileOperationAddress );
            _fileOperationPort = new TextField( "Port: ", 150 );
            _fileOperationPanel.add( _fileOperationPort );
            _bottomPanel.add( _fileOperationPanel );
            
            _fileTransferPanel = new JPanel();
            _fileTransferPanel.setLayout( null );
            _fileTransferPanel.setLayout( null );
            _fileTransferOrigin = new TextField( "Origin: ", 150 );
            _fileTransferPanel.add( _fileTransferOrigin );
            _fileTransferDestination = new TextField( "Destination: ", 150 );
            _fileTransferPanel.add( _fileTransferDestination );
            _fileTransferDirection = new TextField( "Direction: ", 150 );
            _fileTransferPanel.add( _fileTransferDirection );
            _fileTransferDataNode = new TextField( "Data Node: ", 150 );
            _fileTransferPanel.add( _fileTransferDataNode );
            _fileTransferAddress = new TextField( "Address: ", 150 );
            _fileTransferPanel.add( _fileTransferAddress );
            _fileTransferPort = new TextField( "Port: ", 150 );
            _fileTransferPanel.add( _fileTransferPort );
            _bottomPanel.add( _fileTransferPanel );
            
            _getDirectoryPanel = new JPanel();
            _getDirectoryPanel.setLayout( null );
            _getDirectoryMark5 = new TextField( "Mark 5: ", 150 );
            _getDirectoryPanel.add( _getDirectoryMark5 );
            _getDirectoryVSN = new TextField( "VSN: ", 150 );
            _getDirectoryPanel.add( _getDirectoryVSN );
            _getDirectoryGenerateNew = new TextField( "Generate New: ", 150 );
            _getDirectoryPanel.add( _getDirectoryGenerateNew );
            _getDirectoryDifxVersion = new TextField( "DiFX Version: ", 150 );
            _getDirectoryPanel.add( _getDirectoryDifxVersion );
            _getDirectoryAddress = new TextField( "Address: ", 150 );
            _getDirectoryPanel.add( _getDirectoryAddress );
            _getDirectoryPort = new TextField( "Port: ", 150 );
            _getDirectoryPanel.add( _getDirectoryPort );
            _bottomPanel.add( _getDirectoryPanel );
            
            _infoPanel = new JPanel();
            _infoPanel.setLayout( null );
            _bottomPanel.add( _infoPanel );
            
            _loadPanel = new JPanel();
            _loadPanel.setLayout( null );
            _bottomPanel.add( _loadPanel );
            _loadCpuLoad = new TextField( "CPU Load: ", 150 );
            _loadPanel.add( _loadCpuLoad );
            _loadNCore = new TextField( "Num. Cores: ", 150 );
            _loadPanel.add( _loadNCore );
            _loadNetRXRate = new TextField( "Receive Rate: ", 150 );
            _loadPanel.add( _loadNetRXRate );
            _loadNetTXRate = new TextField( "Transmit Rate: ", 150 );
            _loadPanel.add( _loadNetTXRate );
            _loadTotalMemory = new TextField( "Total Memory: ", 150 );
            _loadPanel.add( _loadTotalMemory );
            _loadUsedMemory = new TextField( "Used Memory: ", 150 );
            _loadPanel.add( _loadUsedMemory );
            
            _machinesDefinitionPanel = new JPanel();
            _machinesDefinitionPanel.setLayout( null );
            _machinesInputFile = new TextField( "Input File: ", 150 );
            _machinesDefinitionPanel.add( _machinesInputFile );
            _machinesMachinesFile = new TextField( "Machines File: ", 150 );
            _machinesDefinitionPanel.add( _machinesMachinesFile );
            _machinesThreadsFile = new TextField( "Threads File: ", 150 );
            _machinesDefinitionPanel.add( _machinesThreadsFile );
            _machinesDifxVersion = new TextField( "DiFX Version: ", 150 );
            _machinesDefinitionPanel.add( _machinesDifxVersion );
            _machinesMpiWrapper = new TextField( "MPI Wrapper: ", 150 );
            _machinesDefinitionPanel.add( _machinesMpiWrapper );
            _machinesMpiOptions = new TextField( "MPI Options: ", 150 );
            _machinesDefinitionPanel.add( _machinesMpiOptions );
            _machinesAddress = new TextField( "Address: ", 150 );
            _machinesDefinitionPanel.add( _machinesAddress );
            _machinesPort = new TextField( "Port: ", 150 );
            _machinesDefinitionPanel.add( _machinesPort );
            _machinesNodes = new TextField( "Nodes: ", 150 );
            _machinesDefinitionPanel.add( _machinesNodes );
            _machinesManager = new TextField( "Manager: ", 150 );
            _machinesDefinitionPanel.add( _machinesManager );
            JLabel machinesTableLabel = new JLabel( "Nodes and Threads" );
            machinesTableLabel.setHorizontalAlignment( JLabel.LEFT );
            machinesTableLabel.setBounds( 10, 160, 200, 25 );
            _machinesDefinitionPanel.add( machinesTableLabel );
            _machinesTableModel = new DefaultTableModel(
                    new Object[]{ "Node",
                                  "Threads"
                    }, 0 );
            _machinesTable = new JTable( _machinesTableModel );
            _machinesTablePane = new JScrollPane( _machinesTable );
            _machinesDefinitionPanel.add( _machinesTablePane );
            _bottomPanel.add( _machinesDefinitionPanel );
            
            _mk5ControlPanel = new JPanel();
            _mk5ControlPanel.setLayout( null );
            _mk5ControlCommand = new TextField( "Command: ", 150 );
            _mk5ControlPanel.add( _mk5ControlCommand );
            _mk5ControlTargetNode = new TextField( "Target Node: ", 150 );
            _mk5ControlPanel.add( _mk5ControlTargetNode );
            _mk5ControlAddress = new TextField( "Address: ", 150 );
            _mk5ControlPanel.add( _mk5ControlAddress );
            _mk5ControlPort = new TextField( "Port: ", 150 );
            _mk5ControlPanel.add( _mk5ControlPort );
            _bottomPanel.add( _mk5ControlPanel );
            
            _smartPanel = new JPanel();
            _smartPanel.setLayout( null );
            _smartVSN = new TextField( "VSN: ", 150 );
            _smartPanel.add( _smartVSN );
            _smartSlot = new TextField( "Slot: ", 150 );
            _smartPanel.add( _smartSlot );
            _smartMJD = new TextField( "MJD: ", 150 );
            _smartPanel.add( _smartMJD );
            JLabel smartTableLabel = new JLabel( "SMART Values" );
            smartTableLabel.setHorizontalAlignment( JLabel.LEFT );
            smartTableLabel.setBounds( 10, 40, 200, 25 );
            _smartPanel.add( smartTableLabel );
            _smartTableModel = new DefaultTableModel(
                    new Object[]{ "ID",
                                  "Value"
                    }, 0 );
            _smartTable = new JTable( _smartTableModel );
            _smartTablePane = new JScrollPane( _smartTable );
            _smartPanel.add( _smartTablePane );
            _bottomPanel.add( _smartPanel );
            
            _statusPanel = new JPanel();
            _statusPanel.setLayout( null );
            _statusMessage = new TextField( "Message: ", 150 );
            _statusPanel.add( _statusMessage );
            _statusState = new TextField( "State: ", 150 );
            _statusPanel.add( _statusState );
            _statusVisibilityMJD = new TextField( "Visibility MJD: ", 150 );
            _statusPanel.add( _statusVisibilityMJD );
            _statusJobStart = new TextField( "Job Start MJD: ", 150 );
            _statusPanel.add( _statusJobStart );
            _statusJobStop = new TextField( "Job Stop MJD: ", 150 );
            _statusPanel.add( _statusJobStop );
            JLabel statusTableLabel = new JLabel( "Antenna Weights" );
            statusTableLabel.setHorizontalAlignment( JLabel.LEFT );
            statusTableLabel.setBounds( 10, 100, 200, 25 );
            _statusPanel.add( statusTableLabel );
            _statusTableModel = new DefaultTableModel(
                    new Object[]{ "Antenna",
                                  "Weight"
                    }, 0 );
            _statusTable = new JTable( _statusTableModel );
            _statusTablePane = new JScrollPane( _statusTable );
            _statusPanel.add( _statusTablePane );
            _bottomPanel.add( _statusPanel );
            
            _stopPanel = new JPanel();
            _stopPanel.setLayout( null );
            _stopInputFile = new TextField( "Input File: ", 150 );
            _stopPanel.add( _stopInputFile );
            _stopMpiWrapper = new TextField( "MPI Wrapper: ", 150 );
            _stopPanel.add( _stopMpiWrapper );
            _stopDifxVersion = new TextField( "DiFX Version: ", 150 );
            _stopPanel.add( _stopDifxVersion );
            _bottomPanel.add( _stopPanel );
            
            _vex2DifxRunPanel = new JPanel();
            _vex2DifxRunPanel.setLayout( null );
            _vex2DifxFile = new TextField( "V2D File: ", 150 );
            _vex2DifxRunPanel.add( _vex2DifxFile );
            _vex2DifxNode = new TextField( "Node: ", 150 );
            _vex2DifxRunPanel.add( _vex2DifxNode );
            _vex2DifxPassPath = new TextField( "Pass Path: ", 150 );
            _vex2DifxRunPanel.add( _vex2DifxPassPath );
            _vex2DifxUser = new TextField( "User: ", 150 );
            _vex2DifxRunPanel.add( _vex2DifxUser );
            _vex2DifxCalcifOnly = new TextField( "Run Calcif Only: ", 150 );
            _vex2DifxRunPanel.add( _vex2DifxCalcifOnly );
            _vex2DifxDifxVersion = new TextField( "DiFX Version: ", 150 );
            _vex2DifxRunPanel.add( _vex2DifxDifxVersion );
            _vex2DifxAddress = new TextField( "Address: ", 150 );
            _vex2DifxRunPanel.add( _vex2DifxAddress );
            _vex2DifxPort = new TextField( "Port: ", 150 );
            _vex2DifxRunPanel.add( _vex2DifxPort );
            _bottomPanel.add( _vex2DifxRunPanel );
            
            _weightPanel = new JPanel();
            _weightPanel.setLayout( null );
            _weightAntenna = new TextField( "Antenna: ", 150 );
            _weightPanel.add( _weightAntenna );
            _weightWeight = new TextField( "Weight: ", 150 );
            _weightPanel.add( _weightWeight );
            _bottomPanel.add( _weightPanel );
            
            _mark5StatusPanel = new JPanel();
            _mark5StatusPanel.setLayout( null );
            _bottomPanel.add( _mark5StatusPanel );
            _mark5StatusActiveBank = new TextField( "Active Bank: ", 150 );
            _mark5StatusPanel.add( _mark5StatusActiveBank );
            _mark5StatusBankAVSN = new TextField( "Bank A VSN: ", 150 );
            _mark5StatusPanel.add( _mark5StatusBankAVSN );
            _mark5StatusBankBVSN = new TextField( "Bank B VSN: ", 150 );
            _mark5StatusPanel.add( _mark5StatusBankBVSN );
            _mark5StatusDataMJD = new TextField( "Data MJD: ", 150 );
            _mark5StatusPanel.add( _mark5StatusDataMJD );
            _mark5StatusPlayRate = new TextField( "Play Rate: ", 150 );
            _mark5StatusPanel.add( _mark5StatusPlayRate );
            _mark5StatusPosition = new TextField( "Position: ", 150 );
            _mark5StatusPanel.add( _mark5StatusPosition );
            _mark5StatusScanName = new TextField( "Scan Name: ", 150 );
            _mark5StatusPanel.add( _mark5StatusScanName );
            _mark5StatusScanNumber = new TextField( "Scan Number: ", 150 );
            _mark5StatusPanel.add( _mark5StatusScanNumber );
            _mark5StatusState = new TextField( "State: ", 150 );
            _mark5StatusPanel.add( _mark5StatusState );
            _mark5StatusStatusWord = new TextField( "Status Word: ", 150 );
            _mark5StatusPanel.add( _mark5StatusStatusWord );

            _bottomSplitPane.setDividerLocation( _settings.windowConfiguration().difxMessageWindowBottomFraction );
            _mainSplitPane.setDividerLocation( _settings.windowConfiguration().difxMessageWindowTopFraction );

            _allObjectsBuilt = true;
            this.newSize();        

            _bottomSplitPane.setDividerLocation( (int)( _mainSplitPane.getHeight() * 
                    ( 1.0 - _settings.windowConfiguration().difxMessageWindowTopFraction ) *
                    _settings.windowConfiguration().difxMessageWindowBottomFraction ) );
            _mainSplitPane.setDividerLocation( _settings.windowConfiguration().difxMessageWindowTopFraction );

        }
    
        public void newSize() {
            if ( _allObjectsBuilt ) {
                int w = this.getWidth();
                int h = this.getHeight();
                _settings.windowConfiguration().difxMessageWindowW = w;
                _settings.windowConfiguration().difxMessageWindowH = h;
                _activity.setBounds( w - 30, 10, 12, 12 );
                _activityLabel.setBounds( w - 95, 3, 60, 25 );
                _messageLimit.setBounds( w - 160, 3, 60, 25 );
                _messageLimitLabel.setBounds( w - 320, 3, 160, 25 );
                _showButton.setBounds( 20, 3, 120, 25 );
                _mainSplitPane.setBounds( 0, 30, w, h - 30 );
                _tableScrollPane.setBounds( 10, 1, w - 20, _topPanel.getHeight() - 2 );
                _messageText.setBounds( 10, 1, w - 20, _middlePanel.getHeight() - 2 );
                int w2 = ( w - 10 ) / 2;
                int w3 = ( w - 10 ) / 3;
                _type.setBounds( 0, 10, w2, 25 );
                _from.setBounds( 0, 40, w2, 25 );
                _to.setBounds( 0, 70, w2, 25 );
                _mpiProcessId.setBounds( w2, 10, w2, 25 );
                _id.setBounds( w2, 40, w2, 25 );
                _sequenceNumber.setBounds( w2, 70, w2, 25 );

                if ( _displayedPanel == _alertPanel ) {
                    _alertPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _alertMessage.setBounds( 0, 10, w - 10, 25 );
                    _alertSeverity.setBounds( 0, 40, w2, 25 );
                }

                else if ( _displayedPanel == _commandPanel ) {
                    _commandPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _commandCommand.setBounds( 0, 10, w - 10, 25 );
                }

                else if ( _displayedPanel == _fileOperationPanel ) {
                    _fileOperationPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _fileOperationOperation.setBounds( 0, 10, w2, 25 );
                    _fileOperationArg.setBounds( 0, 40, w2, 25 );
                    _fileOperationPath.setBounds( 0, 70, w2, 25 );
                    _fileOperationDataNode.setBounds( w2, 10, w2, 25 );
                    _fileOperationAddress.setBounds( w2, 40, w2, 25 );
                    _fileOperationPort.setBounds( w2, 70, w2, 25 );
                }

                else if ( _displayedPanel == _fileTransferPanel ) {
                    _fileTransferPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _fileTransferOrigin.setBounds( 0, 10, w2, 25 );
                    _fileTransferDestination.setBounds( 0, 40, w2, 25 );
                    _fileTransferDirection.setBounds( 0, 70, w2, 25 );
                    _fileTransferDataNode.setBounds( w2, 10, w2, 25 );
                    _fileTransferAddress.setBounds( w2, 40, w2, 25 );
                    _fileTransferPort.setBounds( w2, 70, w2, 25 );
                }

                else if ( _displayedPanel == _getDirectoryPanel ) {
                    _getDirectoryPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _getDirectoryMark5.setBounds( 0, 10, w2, 25 );
                    _getDirectoryVSN.setBounds( 0, 40, w2, 25 );
                    _getDirectoryGenerateNew.setBounds( 0, 70, w2, 25 );
                    _getDirectoryDifxVersion.setBounds( w2, 10, w2, 25 );
                    _getDirectoryAddress.setBounds( w2, 40, w2, 25 );
                    _getDirectoryPort.setBounds( w2, 70, w2, 25 );
                }

                else if ( _displayedPanel == _infoPanel ) {
                    _infoPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _infoMessage.setBounds( 0, 10, w - 10, 25 );
                }

                else if ( _displayedPanel == _loadPanel ) {
                    _loadPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _loadCpuLoad.setBounds( 0, 10, w3, 25 );
                    _loadNCore.setBounds( 0, 40, w3, 25 );
                    _loadNetRXRate.setBounds( w3, 10, w3, 25 );
                    _loadNetTXRate.setBounds( w3, 40, w3, 25 );
                    _loadTotalMemory.setBounds( 2 * w3, 10, w3, 25 );
                    _loadUsedMemory.setBounds( 2 * w3, 40, w3, 25 );
                }

                else if ( _displayedPanel == _machinesDefinitionPanel ) {
                    _machinesDefinitionPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _machinesInputFile.setBounds( 0, 10, w2, 25 );
                    _machinesMachinesFile.setBounds( 0, 40, w2, 25 );
                    _machinesThreadsFile.setBounds( 0, 70, w2, 25 );
                    _machinesDifxVersion.setBounds( w2, 10, w2, 25 );
                    _machinesMpiWrapper.setBounds( w2, 40, w2, 25 );
                    _machinesMpiOptions.setBounds( w2, 70, w2, 25 );
                    _machinesAddress.setBounds( w2, 100, w2, 25 );
                    _machinesPort.setBounds( w2, 130, w2, 25 );
                    _machinesNodes.setBounds( 0, 100, w2, 25 );
                    _machinesManager.setBounds( 0, 130, w2, 25 );
                    _machinesTablePane.setBounds( 10, 190, w - 20, _machinesDefinitionPanel.getHeight() - 200 );
                }

                else if ( _displayedPanel == _mk5ControlPanel ) {
                    _mk5ControlPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _mk5ControlCommand.setBounds( 0, 10, w2, 25 );
                    _mk5ControlTargetNode.setBounds( 0, 40, w2, 25 );
                    _mk5ControlAddress.setBounds( w2, 10, w2, 25 );
                    _mk5ControlPort.setBounds( w2, 40, w2, 25 );
                }

                else if ( _displayedPanel == _smartPanel ) {
                    _smartPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _smartVSN.setBounds( 0, 10, w3, 25 );
                    _smartSlot.setBounds( w3, 10, w3, 25 );
                    _smartMJD.setBounds( 2 * w3, 10, w3, 25 );
                    _smartTablePane.setBounds( 10, 70, w - 20, _smartPanel.getHeight() - 80 );
                }

                else if ( _displayedPanel == _statusPanel ) {
                    _statusPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _statusMessage.setBounds( 0, 10, w - 10, 25 );
                    _statusState.setBounds( 0, 40, w2, 25 );
                    _statusVisibilityMJD.setBounds( 0, 70, w2, 25 );
                    _statusJobStart.setBounds( w2, 40, w2, 25 );
                    _statusJobStop.setBounds( w2, 70, w2, 25 );
                    _statusTablePane.setBounds( 10, 130, w - 20, _statusPanel.getHeight() - 140 );
                }

                else if ( _displayedPanel == _stopPanel ) {
                    _stopPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _stopInputFile.setBounds( 0, 10, w - 10, 25 );
                    _stopMpiWrapper.setBounds( 0, 40, w2, 25 );
                    _stopDifxVersion.setBounds( w2, 40, w2, 25 );
                }

                else if ( _displayedPanel == _vex2DifxRunPanel ) {
                    _vex2DifxRunPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _vex2DifxFile.setBounds( 0, 10, w2, 25 );
                    _vex2DifxNode.setBounds( 0, 40, w2, 25 );
                    _vex2DifxPassPath.setBounds( 0, 70, w2, 25 );
                    _vex2DifxUser.setBounds( 0, 100, w2, 25 );
                    _vex2DifxCalcifOnly.setBounds( w2, 10, w2, 25 );
                    _vex2DifxDifxVersion.setBounds( w2, 40, w2, 25 );
                    _vex2DifxAddress.setBounds( w2, 70, w2, 25 );
                    _vex2DifxPort.setBounds( w2, 100, w2, 25 );
                }

                else if ( _displayedPanel == _weightPanel ) {
                    _weightPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _weightAntenna.setBounds( 0, 10, w2, 25 );
                    _weightWeight.setBounds( 0, 40, w2, 25 );
                }

                else if ( _displayedPanel == _mark5StatusPanel ) {
                    _mark5StatusPanel.setBounds( 0, 100, w, _bottomPanel.getHeight() - 100 );
                    _mark5StatusActiveBank.setBounds( 0, 10, w3, 25 );
                    _mark5StatusBankAVSN.setBounds( w3, 10, w3, 25 );
                    _mark5StatusBankBVSN.setBounds( 2 * w3, 10, w3, 25 );
                    _mark5StatusDataMJD.setBounds( 0, 40, w3, 25 );
                    _mark5StatusPlayRate.setBounds( w3, 40, w3, 25 );
                    _mark5StatusPosition.setBounds( 2 * w3, 40, w3, 25 );
                    _mark5StatusScanName.setBounds( 0, 70, w2, 25 );
                    _mark5StatusScanNumber.setBounds( w2, 70, w2, 25 );
                    _mark5StatusState.setBounds( 0, 100, w2, 25 );
                    _mark5StatusStatusWord.setBounds( w2, 100, w2, 25 );
                }
                
            }

        }
        
        /*
         * Pick a message out of the table.
         */
        synchronized public void tableSelect( int i ) {
            //  Find the message associated with this index in the table.
            boolean found = false;
            Message theMessage = null;
            for ( Iterator<Message> iter = _messages.iterator(); iter.hasNext() && !found; ) {
                Message msg = iter.next();
                if ( msg.tableRow == i ) {
                    found = true;
                    theMessage = msg;
                }
            }
            if ( theMessage != null ) {
                //  Put the message text in the editor window.
                _messageText.text( theMessage.raw );
                //  Display the header information.
                _type.text( theMessage.difxMsg.getHeader().getType() );
                _from.text( theMessage.difxMsg.getHeader().getFrom() );
                _to.text( theMessage.difxMsg.getHeader().getTo() );
                _mpiProcessId.text( theMessage.difxMsg.getHeader().getMpiProcessId() );
                _id.text( theMessage.difxMsg.getHeader().getIdentifier());
                _sequenceNumber.text( theMessage.difxMsg.getBody().getSeqNumber() );
                //  Then information based on the message type.  We also only show the
                //  panel that corresponds to this message type.
                if ( theMessage.type.contentEquals( "DifxAlertMessage" ) ) {
                    if ( _displayedPanel != _alertPanel ) {
                        _alertPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _alertPanel;
                    }
                    _alertMessage.text( theMessage.difxMsg.getBody().getDifxAlert().getAlertMessage() );
                    _alertSeverity.text( theMessage.difxMsg.getBody().getDifxAlert().getSeverity() + "" );
                }
                else if ( theMessage.type.contentEquals( "DifxCommandMessage" ) ) {
                    if ( _displayedPanel != _commandPanel ) {
                        _commandPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _commandPanel;
                    }
                    _commandCommand.text( theMessage.difxMsg.getBody().getDifxCommand().getCommand() );
                }
                else if ( theMessage.type.contentEquals( "DifxFileOperationMessage" ) ) {
                    if ( _displayedPanel != _fileOperationPanel ) {
                        _fileOperationPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _fileOperationPanel;
                    }
                    _fileOperationOperation.text( theMessage.difxMsg.getBody().getDifxFileOperation().getOperation() );
                    _fileOperationArg.text( theMessage.difxMsg.getBody().getDifxFileOperation().getArg() );
                    _fileOperationPath.text( theMessage.difxMsg.getBody().getDifxFileOperation().getPath() );
                    _fileOperationDataNode.text( theMessage.difxMsg.getBody().getDifxFileOperation().getDataNode() );
                    _fileOperationAddress.text( theMessage.difxMsg.getBody().getDifxFileOperation().getAddress() );
                    _fileOperationPort.text( theMessage.difxMsg.getBody().getDifxFileOperation().getPort() + "" );
                }
                else if ( theMessage.type.contentEquals( "DifxFileTransferMessage" ) ) {
                    if ( _displayedPanel != _fileTransferPanel ) {
                        _fileTransferPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _fileTransferPanel;
                    }
                    _fileTransferOrigin.text( theMessage.difxMsg.getBody().getDifxFileTransfer().getOrigin() );
                    _fileTransferDestination.text( theMessage.difxMsg.getBody().getDifxFileTransfer().getDestination());
                    _fileTransferDirection.text( theMessage.difxMsg.getBody().getDifxFileTransfer().getDirection() );
                    _fileTransferDataNode.text( theMessage.difxMsg.getBody().getDifxFileTransfer().getDataNode() );
                    _fileTransferAddress.text( theMessage.difxMsg.getBody().getDifxFileTransfer().getAddress() );
                    _fileTransferPort.text( theMessage.difxMsg.getBody().getDifxFileTransfer().getPort() + "" );
                }
                else if ( theMessage.type.contentEquals( "DifxGetDirectoryMessage" ) ) {
                    if ( _displayedPanel != _getDirectoryPanel ) {
                        _getDirectoryPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _getDirectoryPanel;
                    }
                    _getDirectoryMark5.text( theMessage.difxMsg.getBody().getDifxGetDirectory().getMark5() );
                    _getDirectoryVSN.text( theMessage.difxMsg.getBody().getDifxGetDirectory().getVsn() );
                    _getDirectoryGenerateNew.text( theMessage.difxMsg.getBody().getDifxGetDirectory().getGenerateNew() + "" );
                    _getDirectoryDifxVersion.text( theMessage.difxMsg.getBody().getDifxGetDirectory().getDifxVersion() );
                    _getDirectoryAddress.text( theMessage.difxMsg.getBody().getDifxGetDirectory().getAddress() );
                    _getDirectoryPort.text( theMessage.difxMsg.getBody().getDifxGetDirectory().getPort() + "" );
                }
                else if ( theMessage.type.contentEquals( "DifxInfoMessage" ) ) {
                    if ( _displayedPanel != _infoPanel ) {
                        _infoPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _infoPanel;
                    }
                    _infoMessage.text( theMessage.difxMsg.getBody().getDifxInfo().getMessage() );
                }
                else if ( theMessage.type.contentEquals( "DifxLoadMessage" ) ) {
                    if ( _displayedPanel != _loadPanel ) {
                        _loadPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _loadPanel;
                    }
                    _loadCpuLoad.text( theMessage.difxMsg.getBody().getDifxLoad().getCpuLoad() + "" );
                    _loadNCore.text( theMessage.difxMsg.getBody().getDifxLoad().getNCore() + "" );
                    _loadNetRXRate.text( theMessage.difxMsg.getBody().getDifxLoad().getNetRXRate() + "" );
                    _loadNetTXRate.text( theMessage.difxMsg.getBody().getDifxLoad().getNetTXRate() + "" );
                    _loadTotalMemory.text( theMessage.difxMsg.getBody().getDifxLoad().getTotalMemory() + "" );
                    _loadUsedMemory.text( theMessage.difxMsg.getBody().getDifxLoad().getUsedMemory() + "" );
                }
                else if ( theMessage.type.contentEquals( "DifxMachinesDefinitionMessage" ) ) {
                    if ( _displayedPanel != _machinesDefinitionPanel ) {
                        _machinesDefinitionPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _machinesDefinitionPanel;
                    }
                    _machinesInputFile.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getInput() );
                    _machinesMachinesFile.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getMachinesFile() );
                    _machinesThreadsFile.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getThreadsFile() );
                    _machinesDifxVersion.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getDifxVersion() );
                    _machinesMpiWrapper.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getMpiWrapper() );
                    _machinesMpiOptions.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getMpiOptions() );
                    _machinesAddress.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getAddress() );
                    _machinesPort.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getPort() + "" );
                    _machinesNodes.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getDatastream().getNodes() );
                    _machinesManager.text( theMessage.difxMsg.getBody().getDifxMachinesDefinition().getManager().getNode() );
                    while ( _machinesTableModel.getRowCount() > 0 )
                        _machinesTableModel.removeRow( 0 );
                    for ( Iterator<DifxMachinesDefinition.Process> iter = theMessage.difxMsg.getBody().getDifxMachinesDefinition().getProcess().iterator();
                            iter.hasNext(); ) {
                        DifxMachinesDefinition.Process thisProcess = iter.next();
                        _machinesTableModel.insertRow( _machinesTableModel.getRowCount(),
                            new Object[] { 
                                ( thisProcess.getNodes() ),
                                ( thisProcess.getThreads() )
                            } );
                    }
                }
                else if ( theMessage.type.contentEquals( "DifxMk5ControlMessage" ) ) {
                    if ( _displayedPanel != _mk5ControlPanel ) {
                        _mk5ControlPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _mk5ControlPanel;
                    }
                    _mk5ControlCommand.text( theMessage.difxMsg.getBody().getDifxMk5Control().getCommand() );
                    _mk5ControlTargetNode.text( theMessage.difxMsg.getBody().getDifxMk5Control().getTargetNode() );
                    _mk5ControlAddress.text( theMessage.difxMsg.getBody().getDifxMk5Control().getAddress() );
                    _mk5ControlPort.text( theMessage.difxMsg.getBody().getDifxMk5Control().getPort() + "" );
                }
                else if ( theMessage.type.contentEquals( "DifxSmartMessage" ) ) {
                    if ( _displayedPanel != _smartPanel ) {
                        _smartPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _smartPanel;
                    }
                    _smartVSN.text( theMessage.difxMsg.getBody().getDifxSmart().getVsn() );
                    _smartSlot.text( theMessage.difxMsg.getBody().getDifxSmart().getSlot() + "" );
                    _smartMJD.text( theMessage.difxMsg.getBody().getDifxSmart().getMjd() + "" );
                    while ( _smartTableModel.getRowCount() > 0 )
                        _smartTableModel.removeRow( 0 );
                    for ( Iterator<DifxSmart.Smart> iter = theMessage.difxMsg.getBody().getDifxSmart().getSmart().iterator();
                            iter.hasNext(); ) {
                        DifxSmart.Smart thisSmart = iter.next();
                        _smartTableModel.insertRow( _smartTableModel.getRowCount(),
                            new Object[] { 
                                ( thisSmart.getId() + "" ),
                                ( thisSmart.getValue() + "" )
                            } );
                    }
                }
                else if ( theMessage.type.contentEquals( "DifxStatusMessage" ) ) {
                    if ( _displayedPanel != _statusPanel ) {
                        _statusPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _statusPanel;
                    }
                    _statusMessage.text( theMessage.difxMsg.getBody().getDifxStatus().getMessage() );
                    _statusState.text( theMessage.difxMsg.getBody().getDifxStatus().getState() );
                    _statusVisibilityMJD.text( theMessage.difxMsg.getBody().getDifxStatus().getVisibilityMJD() );
                    _statusJobStart.text( theMessage.difxMsg.getBody().getDifxStatus().getJobstartMJD());
                    _statusJobStop.text( theMessage.difxMsg.getBody().getDifxStatus().getJobstopMJD() );
                    while ( _statusTableModel.getRowCount() > 0 )
                        _statusTableModel.removeRow( 0 );
                    for ( Iterator<DifxStatus.Weight> iter = theMessage.difxMsg.getBody().getDifxStatus().getWeight().iterator();
                            iter.hasNext(); ) {
                        DifxStatus.Weight thisWeight = iter.next();
                        _statusTableModel.insertRow( _statusTableModel.getRowCount(),
                            new Object[] { 
                                ( thisWeight.getAnt() ),
                                ( thisWeight.getWt() )
                            } );
                    }
                }
                else if ( theMessage.type.contentEquals( "DifxStopMessage" ) ) {
                    if ( _displayedPanel != _stopPanel ) {
                        _stopPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _stopPanel;
                    }
                    _stopInputFile.text( theMessage.difxMsg.getBody().getDifxStop().getInput() );
                    _stopMpiWrapper.text( theMessage.difxMsg.getBody().getDifxStop().getMpiWrapper() );
                    _stopDifxVersion.text( theMessage.difxMsg.getBody().getDifxStop().getDifxVersion() );
                }
                else if ( theMessage.type.contentEquals( "DifxVex2DifxRunMessage" ) ) {
                    if ( _displayedPanel != _vex2DifxRunPanel ) {
                        _vex2DifxRunPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _vex2DifxRunPanel;
                    }
                    _vex2DifxFile.text( theMessage.difxMsg.getBody().getDifxVex2DifxRun().getFile() );
                    _vex2DifxNode.text( theMessage.difxMsg.getBody().getDifxVex2DifxRun().getNode() );
                    _vex2DifxPassPath.text( theMessage.difxMsg.getBody().getDifxVex2DifxRun().getPassPath() );
                    _vex2DifxUser.text( theMessage.difxMsg.getBody().getDifxVex2DifxRun().getUser() );
                    _vex2DifxCalcifOnly.text( theMessage.difxMsg.getBody().getDifxVex2DifxRun().getCalcifOnly() + "" );
                    _vex2DifxDifxVersion.text( theMessage.difxMsg.getBody().getDifxVex2DifxRun().getDifxVersion() );
                    _vex2DifxAddress.text( theMessage.difxMsg.getBody().getDifxVex2DifxRun().getAddress() );
                    _vex2DifxPort.text( theMessage.difxMsg.getBody().getDifxVex2DifxRun().getPort() + "" );
                }
                else if ( theMessage.type.contentEquals( "DifxWeightMessage" ) ) {
                    if ( _displayedPanel != _weightPanel ) {
                        _weightPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _weightPanel;
                    }
                    _weightAntenna.text( theMessage.difxMsg.getBody().getDifxWeight().getAntenna() );
                    _weightWeight.text( theMessage.difxMsg.getBody().getDifxWeight().getWeight() );
                }
                else if ( theMessage.type.contentEquals( "Mark5StatusMessage" ) ) {
                    if ( _displayedPanel != _mark5StatusPanel ) {
                        _mark5StatusPanel.setVisible( true );
                        if ( _displayedPanel != null )
                            _displayedPanel.setVisible( false );
                        _displayedPanel = _mark5StatusPanel;
                    }
                    _mark5StatusActiveBank.text( theMessage.difxMsg.getBody().getMark5Status().getActiveBank() );
                    _mark5StatusBankAVSN.text( theMessage.difxMsg.getBody().getMark5Status().getBankAVSN() );
                    _mark5StatusBankBVSN.text( theMessage.difxMsg.getBody().getMark5Status().getBankBVSN() );
                    _mark5StatusDataMJD.text( theMessage.difxMsg.getBody().getMark5Status().getDataMJD() );
                    _mark5StatusPlayRate.text( theMessage.difxMsg.getBody().getMark5Status().getPlayRate() + "" );
                    _mark5StatusPosition.text( theMessage.difxMsg.getBody().getMark5Status().getPosition() + "" );
                    _mark5StatusScanName.text( theMessage.difxMsg.getBody().getMark5Status().getScanName() );
                    _mark5StatusScanNumber.text( theMessage.difxMsg.getBody().getMark5Status().getScanNumber() + "" );
                    _mark5StatusState.text( theMessage.difxMsg.getBody().getMark5Status().getState() );
                    _mark5StatusStatusWord.text( theMessage.difxMsg.getBody().getMark5Status().getStatusWord() );
                }
                else if ( _unknownItem.isSelected() ) {
                    _displayedPanel.setVisible( false );
                    _displayedPanel = null;
                }
            }
        }
        
        /*
         * Absorb a new message.
         */
        void newMessage( String type, String from, ByteArrayInputStream is, DifxMessage difxMsg ) {
            _activity.data();
            Message msg = new Message();
            msg.type = type;
            msg.from = from;
            //  Extract the "raw" message string from the input stream.  Typical Java
            //  intuitive stuff here...
            int size = is.available();
            char[] theChars = new char[size];
            byte[] bytes    = new byte[size];
            is.read( bytes, 0, size );
            for (int i = 0; i < size;)
                theChars[i] = (char)(bytes[i++]&0xff);
            String raw = new String( theChars );
            //  Try to make the formatting of this string a little clearer.  This probably
            //  won't work 100% of the time.            
            msg.raw = "";
            String [] s = raw.split( "><" );
            int indentN = 0;
            for ( int i = 0; i < s.length; ++i ) {
                if ( s[i].startsWith( "/" ) )
                    indentN -= 1;
                else if ( !s[i].startsWith( "<" ) && !s[i].contains( "/" ) )
                    indentN += 1;
                String indent = "";
                for ( int j = 0; j < indentN; ++j )
                    indent = indent.concat( "    " );
                if ( i == 0 )
                    msg.raw = msg.raw.concat( "<" + s[i] );
                else
                    msg.raw = msg.raw.concat( ">\n" + indent + "<" + s[i] );
            }
            msg.difxMsg = difxMsg;
            Calendar cal = Calendar.getInstance();
            long time = cal.getTimeInMillis();
            SimpleDateFormat sdf = new SimpleDateFormat( "yyyy-MM-dd" );
            msg.date = sdf.format( time );
            sdf = new SimpleDateFormat( "HH:mm:ss.SSS" );
            msg.time = sdf.format( time );
            if ( _messages == null )
                _messages = new ArrayDeque<Message>();
            _messages.add( msg );
            while ( _messages.size() > _messageLimit.intValue() )
                _messages.removeFirst();
            rebuildTable();
        }
        
        /*
         * Rebuild the items in the table based on current selections.
         */
        synchronized void rebuildTable() {
            //  Save the selected item.
            int selectedRow = _messageJTable.getSelectedRow();
            final int scrollSetting = _tableScrollPane.getVerticalScrollBar().getValue();
            if ( selectedRow >= _messageTable.getRowCount() )
                selectedRow = -1;
            //  Empty the current table.
            while ( _messageTable.getRowCount() > 0 )
                _messageTable.removeRow( 0 );
            //  Figure out which messages we want to display.
            for ( Iterator<Message> iter = _messages.iterator(); iter.hasNext(); ) {
                Message msg = iter.next();
                msg.tableRow = _messageTable.getRowCount();
                boolean showMessage = false;
                if ( _allItem.isSelected() )
                    showMessage = true;
                else {
                    if ( msg.type.contentEquals( "DifxAlertMessage" ) ) {
                        if ( _alertItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxCommandMessage" ) ) {
                        if ( _commandItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxFileOperationMessage" ) ) {
                        if ( _fileOperationItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxFileTransferMessage" ) ) {
                        if ( _fileTransferItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxGetDirectoryMessage" ) ) {
                        if ( _getDirectoryItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxInfoMessage" ) ) {
                        if ( _infoItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxLoadMessage" ) ) {
                        if ( _loadItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxMachinesDefinitionMessage" ) ) {
                        if ( _machinesDefinitionItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxMk5ControlMessage" ) ) {
                        if ( _mk5ControlItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxSmartMessage" ) ) {
                        if ( _smartItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxStatusMessage" ) ) {
                        if ( _statusItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxStopMessage" ) ) {
                        if ( _stopItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxVex2DifxRunMessage" ) ) {
                        if ( _vex2DifxRunItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "DifxWeightMessage" ) ) {
                        if ( _weightItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( msg.type.contentEquals( "Mark5StatusMessage" ) ) {
                        if ( _mark5StatusItem.isSelected() )
                            showMessage = true;
                    }
                    else if ( _unknownItem.isSelected() )
                            showMessage = true;
                }
                if ( showMessage ) {
                    _messageTable.insertRow( _messageTable.getRowCount(),
                        new Object[] { 
                            ( msg.date + " " + msg.time ),
                            ( msg.type ),
                            ( msg.from )
                        } );
                }
            }
            //  Reset the selection (if there was one).
            final JScrollBar vbar = _tableScrollPane.getVerticalScrollBar();
            if ( selectedRow == -1 ) {
                EventQueue.invokeLater( new Runnable () {
                    public void run () {
                        EventQueue.invokeLater( new Runnable () {
                            public void run () {
                                vbar.setValue( vbar.getMaximum() );
                            }
                        });
                    }
                });
                tableSelect( _messageTable.getRowCount() - 1 );
            }
            else {
                EventQueue.invokeLater( new Runnable () {
                    public void run () {
                        EventQueue.invokeLater( new Runnable () {
                            public void run () {
                                vbar.setValue( scrollSetting );
                            }
                        });
                    }
                });
            }
        }
        
        protected DiFXMessageWindow _this;
        protected boolean _allObjectsBuilt;
        protected ActivityMonitorLight _activity;
        protected JLabel _activityLabel;
        protected JScrollPane _tableScrollPane;
        protected DefaultTableModel _messageTable;
        protected JTable _messageJTable;
        protected class Message {
            public String type;
            public String from;
            public String raw;
            public DifxMessage difxMsg;
            public String date;
            public String time;
            public int tableRow;
        }
        protected ArrayDeque<Message> _messages;
        protected SimpleTextEditor _messageText;
        protected JSplitPane _mainSplitPane;
        protected JSplitPane _bottomSplitPane;
        protected JPanel _topPanel;
        protected JPanel _middlePanel;
        protected JPanel _bottomPanel;
        protected NumberBox _messageLimit;
        protected JLabel _messageLimitLabel;
        protected JButton _showButton;
        protected JPopupMenu _showMenu;
        protected JCheckBoxMenuItem _allItem;
        protected JCheckBoxMenuItem _alertItem;
        protected JCheckBoxMenuItem _commandItem;
        protected JCheckBoxMenuItem _fileOperationItem;
        protected JCheckBoxMenuItem _fileTransferItem;
        protected JCheckBoxMenuItem _getDirectoryItem;
        protected JCheckBoxMenuItem _infoItem;
        protected JCheckBoxMenuItem _loadItem;
        protected JCheckBoxMenuItem _machinesDefinitionItem;
        protected JCheckBoxMenuItem _mk5ControlItem;
        protected JCheckBoxMenuItem _smartItem;
        protected JCheckBoxMenuItem _statusItem;
        protected JCheckBoxMenuItem _stopItem;
        protected JCheckBoxMenuItem _vex2DifxRunItem;
        protected JCheckBoxMenuItem _weightItem;
        protected JCheckBoxMenuItem _mark5StatusItem;
        protected JCheckBoxMenuItem _unknownItem;
        public class TextField extends JPanel {
            public TextField( String label, int split ) {
                super();
                this.setLayout( null );
                _split = split;
                _label = new JLabel( label );
                _label.setHorizontalAlignment( JLabel.RIGHT );
                _data = new SaneTextField();
                _data.setEditable( false );
                this.add( _label );
                this.add( _data );
            }
            public void text( String data ) {
                _data.setText( data );
            }
            public void setBounds( int x, int y, int w, int h ) {
                super.setBounds( x, y, w, h );
                _label.setBounds( 0, 0, _split, h );
                _data.setBounds( _split, 0, w - _split, h );
            }
            public SaneTextField _data;
            public JLabel _label;
            public int _split;
        }
        protected TextField _from;
        protected TextField _to;
        protected TextField _mpiProcessId;
        protected TextField _id;
        protected TextField _type;
        protected TextField _sequenceNumber;
        
        protected JPanel _alertPanel;
        protected TextField _alertMessage;
        protected TextField _alertSeverity;
        
        protected JPanel _commandPanel;
        protected TextField _commandCommand;
        
        protected JPanel _fileOperationPanel;
        protected TextField _fileOperationOperation;
        protected TextField _fileOperationArg;
        protected TextField _fileOperationPath;
        protected TextField _fileOperationDataNode;
        protected TextField _fileOperationAddress;
        protected TextField _fileOperationPort;
        
        protected JPanel _fileTransferPanel;
        protected TextField _fileTransferOrigin;
        protected TextField _fileTransferDestination;
        protected TextField _fileTransferDirection;
        protected TextField _fileTransferDataNode;
        protected TextField _fileTransferAddress;
        protected TextField _fileTransferPort;
        
        protected JPanel _getDirectoryPanel;
        protected TextField _getDirectoryMark5;
        protected TextField _getDirectoryVSN;
        protected TextField _getDirectoryGenerateNew;
        protected TextField _getDirectoryDifxVersion;
        protected TextField _getDirectoryAddress;
        protected TextField _getDirectoryPort;
        
        protected JPanel _infoPanel;
        protected TextField _infoMessage;
        
        protected JPanel _loadPanel;
        protected TextField _loadCpuLoad;
        protected TextField _loadNCore;
        protected TextField _loadNetRXRate;
        protected TextField _loadNetTXRate;
        protected TextField _loadTotalMemory;
        protected TextField _loadUsedMemory;
        
        protected JPanel _machinesDefinitionPanel;
        protected TextField _machinesInputFile;
        protected TextField _machinesMachinesFile;
        protected TextField _machinesThreadsFile;
        protected TextField _machinesDifxVersion;
        protected TextField _machinesMpiWrapper;
        protected TextField _machinesMpiOptions;
        protected TextField _machinesAddress;
        protected TextField _machinesPort;
        protected TextField _machinesNodes;
        protected TextField _machinesManager;
        protected DefaultTableModel _machinesTableModel;
        protected JTable _machinesTable;
        protected JScrollPane _machinesTablePane;
        
        protected JPanel _mk5ControlPanel;
        protected TextField _mk5ControlCommand;
        protected TextField _mk5ControlTargetNode;
        protected TextField _mk5ControlAddress;
        protected TextField _mk5ControlPort;
        
        protected JPanel _smartPanel;
        protected TextField _smartVSN;
        protected TextField _smartSlot;
        protected TextField _smartMJD;
        protected JScrollPane _smartTablePane;
        protected DefaultTableModel _smartTableModel;
        protected JTable _smartTable;
        
        protected JPanel _statusPanel;
        protected TextField _statusMessage;
        protected TextField _statusState;
        protected TextField _statusVisibilityMJD;
        protected TextField _statusJobStart;
        protected TextField _statusJobStop;
        protected JScrollPane _statusTablePane;
        protected DefaultTableModel _statusTableModel;
        protected JTable _statusTable;
        
        protected JPanel _stopPanel;
        protected TextField _stopInputFile;
        protected TextField _stopMpiWrapper;
        protected TextField _stopDifxVersion;
        
        protected JPanel _vex2DifxRunPanel;
        protected TextField _vex2DifxFile;
        protected TextField _vex2DifxNode;
        protected TextField _vex2DifxPassPath;
        protected TextField _vex2DifxUser;
        protected TextField _vex2DifxCalcifOnly;
        protected TextField _vex2DifxDifxVersion;
        protected TextField _vex2DifxAddress;
        protected TextField _vex2DifxPort;
        
        protected JPanel _weightPanel;
        protected TextField _weightAntenna;
        protected TextField _weightWeight;
        
        protected JPanel _mark5StatusPanel;
        protected TextField _mark5StatusActiveBank;
        protected TextField _mark5StatusBankAVSN;
        protected TextField _mark5StatusBankBVSN;
        protected TextField _mark5StatusDataMJD;
        protected TextField _mark5StatusPlayRate;
        protected TextField _mark5StatusPosition;
        protected TextField _mark5StatusScanName;
        protected TextField _mark5StatusScanNumber;
        protected TextField _mark5StatusState;
        protected TextField _mark5StatusStatusWord;
        
        protected JPanel _displayedPanel;
    }
}
