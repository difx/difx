/*
 * This is a BrowserNode for a cluster processing module.  In much of the GUI
 * these are referred to as "Processor Nodes", however there was already a
 * ProcessorNode class so I couldn't use that name.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.ActivityMonitorLight;
import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.JCheckBoxMenuItem;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Color;
import java.awt.Component;
import java.util.ArrayList;
import java.util.Iterator;

import java.text.DecimalFormat;

import mil.navy.usno.plotlib.PlotWindow;
import mil.navy.usno.plotlib.Plot2DObject;
import mil.navy.usno.plotlib.Track2D;

import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.difxutilities.DiFXCommand_mark5Control;

public class ProcessorNode extends BrowserNode {
    
    public ProcessorNode( String name, SystemSettings settings ) {
        super( name );
        _settings = settings;
        _columnColor = new Color( 204, 204, 255 );
        _dec = new DecimalFormat();
        _popupButton.setVisible( true );
        _networkActivity.warningTime( _settings.inactivityWarning() * 10 );
        _networkActivity.alertTime( _settings.inactivityError() * 10 );
        _usageList = new ArrayList<CurrentUse>();
    }
    
    @Override
    public void createAdditionalItems() {
        addSelectionButton( null, null );
        selectedColor( Color.BLUE );
        //  This is the data monitor light.
        _networkActivity = new ActivityMonitorLight();
        this.add( _networkActivity );
        _numCPUs = new ColumnTextArea();
        _numCPUs.justify( ColumnTextArea.RIGHT );
        this.add( _numCPUs );
        _numCores = new ColumnTextArea();
        _numCores.justify( ColumnTextArea.RIGHT );
        this.add( _numCores );
        _threadsUsed = new ColumnTextArea();
        _threadsUsed.justify( ColumnTextArea.RIGHT );
        this.add( _threadsUsed );
        _bogusGHz = new ColumnTextArea();
        _bogusGHz.justify( ColumnTextArea.RIGHT );
        this.add( _bogusGHz );
        _type = new ColumnTextArea();
        _type.justify( ColumnTextArea.RIGHT );
        this.add( _type );
        _typeString = new ColumnTextArea();
        _typeString.justify( ColumnTextArea.CENTER );
        this.add( _typeString );
        _state = new ColumnTextArea();
        _state.justify( ColumnTextArea.CENTER );
        this.add( _state );
        _enabledLight = new ActivityMonitorLight();
        this.add( _enabledLight );
        _cpuLoad = new ColumnTextArea();
        _cpuLoad.justify( ColumnTextArea.RIGHT );
        this.add( _cpuLoad );
        _cpuLoadPlot = new PlotWindow();
        this.add( _cpuLoadPlot );
        _cpuPlot = new Plot2DObject();
        _cpuLoadPlot.add2DPlot( _cpuPlot );
        _cpuPlot.name( "CPU Plot" );
        _cpuPlot.drawBackground( true );
        _cpuPlot.drawFrame( true );
        _cpuPlot.frameColor( Color.GRAY );
        _cpuPlot.clip( true );
        _cpuPlot.addTopGrid( Plot2DObject.X_AXIS, 10.0, Color.BLACK );
        _cpuTrack = new Track2D();
        _cpuTrack.fillCurve( true );
        _cpuPlot.addTrack( _cpuTrack );
        _cpuTrack.color( new Color( 210, 190, 130 ) );
        _cpuTrack.sizeLimit( 200 );
        _cpuPlot.frame( 0.0, 0.0, 1.0, 1.0 );
        _cpuPlot.backgroundColor( Color.BLACK );
        _usedMem = new ColumnTextArea();
        _usedMem.justify( ColumnTextArea.RIGHT );
        this.add( _usedMem );
        _totalMem = new ColumnTextArea();
        _totalMem.justify( ColumnTextArea.RIGHT );
        this.add( _totalMem );
        _memLoad = new ColumnTextArea();
        _memLoad.justify( ColumnTextArea.RIGHT );
        this.add( _memLoad );
        _memLoadPlot = new PlotWindow();
        this.add( _memLoadPlot );
        _memPlot = new Plot2DObject();
        _memLoadPlot.add2DPlot( _memPlot );
        _memPlot.name( "Mem Plot" );
        _memPlot.drawBackground( true );
        _memPlot.drawFrame( true );
        _memPlot.frameColor( Color.GRAY );
        _memPlot.clip( true );
        _memPlot.addTopGrid( Plot2DObject.X_AXIS, 10.0, Color.BLACK );
        _memTrack = new Track2D();
        _memTrack.fillCurve( true );
        _memPlot.addTrack( _memTrack );
        _memTrack.color( new Color( 50, 250, 200 ) );
        _memTrack.sizeLimit( 200 );
        _memPlot.frame( 0.0, 0.0, 1.0, 1.0 );
        _memPlot.backgroundColor( Color.BLACK );
        _netRxRate = new ColumnTextArea();
        _netRxRate.justify( ColumnTextArea.RIGHT );
        this.add( _netRxRate );
        _netTxRate = new ColumnTextArea();
        _netTxRate.justify( ColumnTextArea.RIGHT );
        this.add( _netTxRate );
//        _alertWindow = new MessageWindow( "" );
//        _alertWindow.setTitle( "Alerts for " + _label.getText() );
//        _alertWindow.setBounds( 200, 200, 700, 300 );
        //  Create a popup menu appropriate to a "job".
        _popup = new JPopupMenu();
        _activeJob = new ColumnTextArea();
        _activeJob.justify( ColumnTextArea.RIGHT );
        _activeJob.flashActive( true );
        _activeJob.flashTime( 600 );
        _activeJob.expireTime( 600 );
        _activeJob.expireActive( true );
        this.add( _activeJob );
        _dataConsumed = new ColumnTextArea();
        _dataConsumed.justify( ColumnTextArea.RIGHT );
        this.add( _dataConsumed );
        _inputDatarate = new ColumnTextArea();
        _inputDatarate.justify( ColumnTextArea.RIGHT );
        this.add( _inputDatarate );
        _memoryUsage = new ColumnTextArea();
        _memoryUsage.justify( ColumnTextArea.RIGHT );
        this.add( _memoryUsage );
        _numBufElements = new ColumnTextArea();
        _numBufElements.justify( ColumnTextArea.RIGHT );
        this.add( _numBufElements );
        _startBufElement = new ColumnTextArea();
        _startBufElement.justify( ColumnTextArea.RIGHT );
        this.add( _startBufElement );
        _activeBufElement = new ColumnTextArea();
        _activeBufElement.justify( ColumnTextArea.RIGHT );
        this.add( _activeBufElement );
        _threadID = new ColumnTextArea();
        _threadID.justify( ColumnTextArea.RIGHT );
        this.add( _threadID );
        _processMicrosec = new ColumnTextArea();
        _processMicrosec.justify( ColumnTextArea.RIGHT );
        this.add( _processMicrosec );
        _subintsLost = new ColumnTextArea();
        _subintsLost.justify( ColumnTextArea.RIGHT );
        this.add( _subintsLost );
        generatePopupMenu();
    }
    
    @Override
    public void generatePopupMenu() {
        _popup.removeAll();
        JMenuItem headerItem = new JMenuItem( "Controls for \"" + _label.getText() + "\"" );
        _popup.add( headerItem );
        _popup.add( new JSeparator() );
        _ignoreItem = new JCheckBoxMenuItem( "Ignore This Node" );
        _ignoreItem.setState( _ignoreState );
        _ignoreItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _ignoreState = !_ignoreState;
                changeIgnoreState();
            }
        });
        _popup.add( _ignoreItem );
        JMenuItem monitorItem;
        monitorItem = new JMenuItem( "Show Monitor Plots" );
        monitorItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                monitorAction( e );
            }
        });
        _popup.add( monitorItem );
//        JMenuItem alertItem;
//        alertItem = new JMenuItem( "Show Alert Messages" );
//        alertItem.addActionListener(new ActionListener() {
//            public void actionPerformed( ActionEvent e ) {
//                alertWindowAction( e );
//            }
//        });
//        _popup.add( alertItem );
        JMenuItem resetItem = new JMenuItem( "Reset" );
        resetItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                sendDiFXCommandMessage( "ResetMark5" );
            }
        });
        _popup.add( resetItem );
        JMenuItem rebootItem = new JMenuItem( "Reboot" );
        rebootItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                sendDiFXCommandMessage( "Reboot" );
            }
        });
        _popup.add( rebootItem );
        JMenuItem powerOffItem = new JMenuItem( "Power Off" );
        powerOffItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                sendDiFXCommandMessage( "Poweroff" );
            }
        });
        _popup.add( powerOffItem );
    }
    
    @Override
    public void positionItems() {
        _xOff = _level * 20;
        //  The network activity light is positioned to the left of the name
        if ( _showNetworkActivity ) {
            _networkActivity.setBounds( _xOff, 6, 10, 10 );
            _xOff += 14;
        }
        //  We always show the name of the node
        _label.setBounds( _xOff, 0, 180, _ySize );
        _xOff += 180;
        _popupButton.setBounds( _xOff + 2, 2, 16, _ySize - 4 );//16, _ySize - 4 );
        _xOff += 20;
        //  Then everything else.  For items that are simply values (as opposed
        //  to plots) we show labels backed by alternating colors so the columns
        //  will be easy to follow.
        _colorColumn = false;
        if( _showNumCPUs )
            setTextArea( _numCPUs, _widthNumCPUs );
        if ( _showThreadsUsed )
            setTextArea( _threadsUsed, _widthThreadsUsed );
        if ( _showNumCores )
            setTextArea( _numCores, _widthNumCores );
        if ( _showBogusGHz )
            setTextArea( _bogusGHz, _widthBogusGHz );
        if ( _showType )
            setTextArea( _type, _widthType );
        if ( _showTypeString )
            setTextArea( _typeString, _widthTypeString );
        if ( _showState ) {
            _state.setBounds( _xOff, 1, _widthState, _ySize - 2);
            //       Assign a background color that is appropriate to the "state"
            //  string.  Note that this is a bit kludgey - most of the states apply
            //  to Mark5Nodes (a class that inherits this one).  In fact, the only
            //  "state" that is transmitted from difx is for Mark5s.  The "state"
            //  that applies to cluster nodes is manufactured based on message
            //  timeliness, and can only be "Lost" or "Idle".  "Lost" actually
            //  appears quite a bit (more than it should) so I've made it yellow
            //  instead of the seemingly more logical red.
            //       The Mark5 states are taken verbatim out of the XML messages
            //  from difx.  The text is defined in the file:
            //     .../difx/libraries/difxmessage/trunk/difxmessage/difxmessage.c
            if ( _state.getText().equals( "Error" ) ||
                 _state.getText().equals( "PowerOff" ) ||
                 _state.getText().equals( "CondError" ) )
                _state.setBackground( Color.RED );
            else if ( _state.getText().equals( "Lost" ) ||
                 _state.getText().equals( "Resetting" ) ||
                 _state.getText().equals( "Rebooting" ) ||
                 _state.getText().equals( "NoMoreData" ) ||
                 _state.getText().equals( "PlayInvalid" ) ||
                 _state.getText().equals( "invisible" ) ||
                 _state.getText().equals( "Condition" ) )
                _state.setBackground( Color.YELLOW );
            else if ( _state.getText().equals( "Opening" ) ||
                 _state.getText().equals( "Open" ) ||
                 _state.getText().equals( "Close" ) ||
                 _state.getText().equals( "GetDirectory" ) ||
                 _state.getText().equals( "GotDirectory" ) ||
                 _state.getText().equals( "Play" ) ||
                 _state.getText().equals( "Busy" ) ||
                 _state.getText().equals( "Initializing" ) ||
                 _state.getText().equals( "PlayStart" ) ||
                 _state.getText().equals( "Copy" ) )
                _state.setBackground( Color.GREEN );
            else if ( _state.getText().equals( "Online" ) ||
                 _state.getText().equals( "Idle" ) ||
                 _state.getText().equals( "NoData" ) ||
                 _state.getText().equals( "Test" ) ||
                 _state.getText().equals( "TestWrite" ) ||
                 _state.getText().equals( "TestRead" ) )
                _state.setBackground( Color.LIGHT_GRAY );
            else
                _state.setBackground( Color.WHITE );
            _xOff += _widthState;
            _colorColumn = false;
        }
        if ( _showEnabled ) {
            _enabledLight.setBounds( _xOff + ( _widthEnabled - 10 ) / 2, 6, 10, 10 );
            _xOff += _widthEnabled;
            _colorColumn = false;
        }
        if ( _showCpuLoad )
            setTextArea( _cpuLoad, _widthCpuLoad );
        if ( _showCpuLoadPlot ) {
            _cpuLoadPlot.setBounds( _xOff, 1, _widthCpuLoadPlot, _ySize - 2 );
            _cpuPlot.resizeBasedOnWindow( _widthCpuLoadPlot, _ySize - 2 );
            _xOff += _widthCpuLoadPlot;
            _colorColumn = false;
        }
        if ( _showUsedMem )
            setTextArea( _usedMem, _widthUsedMem );
        if ( _showTotalMem )
            setTextArea( _totalMem, _widthTotalMem );
        if ( _showMemLoad )
            setTextArea( _memLoad, _widthMemLoad );
        if ( _showMemLoadPlot ) {
            _memLoadPlot.setBounds( _xOff, 1, _widthMemLoadPlot, _ySize - 2 );
            _memPlot.resizeBasedOnWindow( _widthMemLoadPlot, _ySize - 2 );
            _xOff += _widthMemLoadPlot;
            _colorColumn = false;
        }
        if ( _showNetRxRate )
            setTextArea( _netRxRate, _widthNetRxRate );
        if ( _showNetTxRate )
            setTextArea( _netTxRate, _widthNetTxRate );
        
        if ( _showActiveJob )
            setTextArea( _activeJob, _widthActiveJob );
        if ( _showDataConsumed )
            setTextArea( _dataConsumed, _widthDataConsumed );
        if ( _showInputDatarate )
            setTextArea( _inputDatarate, _widthInputDatarate );
        if ( _showMemoryUsage )
            setTextArea( _memoryUsage, _widthMemoryUsage );
        if ( _showNumBufElements )
            setTextArea( _numBufElements, _widthNumBufElements );
        if ( _showStartBufElement )
            setTextArea( _startBufElement, _widthStartBufElement );
        if ( _showActiveBufElement )
            setTextArea( _activeBufElement, _widthActiveBufElement );
        if ( _showThreadID )
            setTextArea( _threadID, _widthThreadID );
        if ( _showProcessMicrosec )
            setTextArea( _processMicrosec, _widthProcessMicrosec );
        if ( _showSubintsLost )
            setTextArea( _subintsLost, _widthSubintsLost );
    }
    
    /*
     * Private function used repeatedly in positionItems().
     */
    protected void setTextArea( Component area, int xSize ) {
        area.setBounds( _xOff, 1, xSize, _ySize - 2);
        _xOff += xSize;
        if ( _colorColumn )
            area.setBackground( _columnColor );
        else
            area.setBackground( Color.WHITE );
        _colorColumn = !_colorColumn;
    }
    
    @Override
    public void paintComponent( Graphics g ) {
        //  Use anti-aliasing on the text (looks much better)
        Graphics2D g2 = (Graphics2D)g;
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        super.paintComponent( g );
    }
    
    /*
     * Show the "monitor" window.  If one has not been created yet, create it first.
     */
    public void monitorAction( ActionEvent e ) {
        if ( _monitor == null ) {
            _monitor = new ProcessorMonitorWindow();
            _monitor.setTitle( _label.getText() );
        }
        _monitor.setVisible( true );
    }
    
    /*
     * Show the alert message window.
     */
//    public void alertWindowAction( ActionEvent e ) {
//        if ( _alertWindow == null ) {
//            _alertWindow = new MessageWindow( "" );
//            _alertWindow.setTitle( "Alerts for " + _label.getText() );
//            _alertWindow.setBounds( 200, 200, 500, 400 );
//        }
//        _alertWindow.setVisible( true );
//    }
    
    /*
     * Change the intervals before the network activity light turn yellow (warning)
     * or red (error).  Intervals are in tenths of seconds.
     */
    public void inactivitySettings( int warning, int alert ) {
        _networkActivity.warningTime( warning );
        _networkActivity.alertTime( alert );
    }
    
    /*
     * Send a "mark5" command to this processor.  See "monitor.cpp" in the
     * mark5daemon code to see allowed commands.
     */
    protected void sendDiFXCommandMessage( String cmd ) {
        DiFXCommand_mark5Control command = new DiFXCommand_mark5Control( cmd, name(), _settings, false );
        try {
            command.send();
        } catch ( Exception e ) {
            System.out.println( e.getMessage() );
        }
    }

    public void showNetworkActivity( boolean newVal ) {
        _showNetworkActivity = newVal;
        _networkActivity.setVisible( _showNetworkActivity );            
    }
    
    public void showNumCPUs( boolean newVal ) {
        _showNumCPUs = newVal;
        _numCPUs.setVisible( newVal );
    }
    
    public void showThreadsUsed( boolean newVal ) {
        _showThreadsUsed = newVal;
        _threadsUsed.setVisible( newVal );
    }
    
    public void showNumCores( boolean newVal ) {
        _showNumCores = newVal;
        _numCores.setVisible( newVal );
    }
    
    public void showBogusGHz( boolean newVal ) {
        _showBogusGHz = newVal;
        _bogusGHz.setVisible( newVal );
    }

    public void showType( boolean newVal ) {
        _showType = newVal;
        _type.setVisible( newVal );
    }

    public void showTypeString( boolean newVal ) {
        _showTypeString = newVal;
        _typeString.setVisible( newVal );
    }

    public void showState( boolean newVal ) {
        _showState = newVal;
        _state.setVisible( newVal );
    }

    public void showEnabled( boolean newVal ) {
        _showEnabled = newVal;
        _enabledLight.setVisible( newVal );
    }

    public void showCpuLoad( boolean newVal ) {
        _showCpuLoad = newVal;
        _cpuLoad.setVisible( newVal );
    }

    public void showCpuLoadPlot( boolean newVal ) {
        _showCpuLoadPlot = newVal;
        _cpuLoadPlot.setVisible( newVal );            
    }

    public void showUsedMem( boolean newVal ) {
        _showUsedMem = newVal;
        _usedMem.setVisible( newVal );
    }

    public void showTotalMem( boolean newVal ) {
        _showTotalMem = newVal;
        _totalMem.setVisible( newVal );
    }

    public void showMemLoad( boolean newVal ) {
        _showMemLoad = newVal;
        _memLoad.setVisible( newVal );
    }

    public void showMemLoadPlot( boolean newVal ) {
        _showMemLoadPlot = newVal;
        _memLoadPlot.setVisible( newVal );            
    }

    public void showNetRxRate( boolean newVal ) {
        _showNetRxRate = newVal;
        _netRxRate.setVisible( newVal );
    }

    public void showNetTxRate( boolean newVal ) {
        _showNetTxRate = newVal;
        _netTxRate.setVisible( newVal );
    }

    public void showActiveJob( boolean newVal ) {
        _activeJob.setVisible( newVal );
        _showActiveJob = newVal;
    }
    
    public void showDataConsumed( boolean newVal ) {
        _dataConsumed.setVisible( newVal );
        _showDataConsumed = newVal;
    }

    public void showInputDatarate( boolean newVal ) {
        _inputDatarate.setVisible( newVal );
        _showInputDatarate = newVal;
    }

    public void showMemoryUsage( boolean newVal ) {
        _memoryUsage.setVisible( newVal );
        _showMemoryUsage = newVal;
    }

    public void showNumBufElements( boolean newVal ) {
        _numBufElements.setVisible( newVal );
        _showNumBufElements = newVal;
    }

    public void showStartBufElement( boolean newVal ) {
        _startBufElement.setVisible( newVal );
        _showStartBufElement = newVal;
    }

    public void showActiveBufElement( boolean newVal ) {
        _activeBufElement.setVisible( newVal );
        _showActiveBufElement = newVal;
    }

    public void showThreadID( boolean newVal ) {
        _threadID.setVisible( newVal );
        _showThreadID = newVal;
    }

    public void showProcessMicrosec( boolean newVal ) {
        _processMicrosec.setVisible( newVal );
        _showProcessMicrosec = newVal;
    }

    public void showSubintsLost( boolean newVal ) {
        _subintsLost.setVisible( newVal );
        _showSubintsLost = newVal;
    }


    public void widthNumCPUs( int newVal ) { _widthNumCPUs = newVal; }
    public void widthThreadsUsed( int newVal ) { _widthThreadsUsed = newVal; }
    public void widthNumCores( int newVal ) { _widthNumCores = newVal; }
    public void widthBogusGHz( int newVal ) { _widthBogusGHz = newVal; }
    public void widthType( int newVal ) { _widthType = newVal; }
    public void widthTypeString( int newVal ) { _widthTypeString = newVal; }
    public void widthState( int newVal ) { _widthState = newVal; }
    public void widthEnabled( int newVal ) { _widthEnabled = newVal; }
    public void widthCpuLoad( int newVal ) { _widthCpuLoad = newVal; }
    public void widthCpuLoadPlot( int newVal ) { _widthCpuLoadPlot = newVal; }
    public void widthUsedMem( int newVal ) { _widthUsedMem = newVal; }
    public void widthTotalMem( int newVal ) { _widthTotalMem = newVal; }
    public void widthMemLoad( int newVal ) { _widthMemLoad = newVal; }
    public void widthMemLoadPlot( int newVal ) { _widthMemLoadPlot = newVal; }
    public void widthNetRxRate( int newVal ) { _widthNetRxRate = newVal; }
    public void widthNetTxRate( int newVal ) { _widthNetTxRate = newVal; }
    
    public void widthActiveJob( int newVal ) { _widthActiveJob = newVal; }
    public void widthDataConsumed( int newVal ) { _widthDataConsumed = newVal; }
    public void widthInputDatarate( int newVal ) { _widthInputDatarate = newVal; }
    public void widthMemoryUsage( int newVal ) { _widthMemoryUsage = newVal; }
    public void widthNumBufElements( int newVal ) { _widthNumBufElements = newVal; }
    public void widthStartBufElement( int newVal ) { _widthStartBufElement = newVal; }
    public void widthActiveBufElement( int newVal ) { _widthActiveBufElement = newVal; }
    public void widthThreadID( int newVal ) { _widthThreadID = newVal; }
    public void widthProcessMicrosec( int newVal ) { _widthProcessMicrosec = newVal; }
    public void widthSubintsLost( int newVal ) { _widthSubintsLost = newVal; }

    /*
     * Absorb the data from a Mark5 status message.  These are used from time to time to convey
     * the "state" of a processor - "rebooting", "error", etc.  These messages are used even 
     * though the processor isn't technically a Mark5.  Beyond the "state" there isn't anything
     * particularly interesting in the message for us.
     */
    public void statusMessage( DifxMessage difxMsg ) {
        _networkActivity.data();
        _state.setText( difxMsg.getBody().getMark5Status().getState() );
    }
    
    public void loadMessage( DifxMessage difxMsg ) {
        _networkActivity.data();
        _numCPUs.setText( "0" );
        _threadsUsed.setText( threadsUsed() + "/" + ( difxMsg.getBody().getDifxLoad().getNCore() - 1 ) );
        _numCores.setText( "" + difxMsg.getBody().getDifxLoad().getNCore() );
        _bogusGHz.setText( "0" );
        _type.setText( "0" );
        _typeString.setText( "processor" );
        //  If this is a Mark5, avoid setting this uninformative state...Mark5 messages have much
        //  more interesting state values.
        if ( !_isMark5 )
            _state.setText( "Online" );
        _cpuLoad.setText( String.format( "%10.1f", 100.0 * difxMsg.getBody().getDifxLoad().getCpuLoad()
                    / ( (float)(difxMsg.getBody().getDifxLoad().getNCore() ) ) ) );
        _cpuPlot.limits( (double)(_cpuTrackSize - 100), (double)(_cpuTrackSize), 0.0, 100.0 );
        _cpuTrack.add( (double)(_cpuTrackSize), 100.0 * difxMsg.getBody().getDifxLoad().getCpuLoad()
                    / ( (float)(difxMsg.getBody().getDifxLoad().getNCore() ) ) );
        _cpuTrackSize += 1;
        _cpuLoadPlot.updateUI();
        _enabledLight.on( false );
        _memLoad.setText( String.format( "%10.1f", 100.0 * (float) difxMsg.getBody().getDifxLoad().getUsedMemory()
                    / difxMsg.getBody().getDifxLoad().getTotalMemory() ) );
        _memPlot.limits( (double)(_memTrackSize - 100), (double)(_memTrackSize), 0.0, 100.0 );
        _memTrack.add( (double)(_memTrackSize), 100.0 * (float) difxMsg.getBody().getDifxLoad().getUsedMemory()
                    / difxMsg.getBody().getDifxLoad().getTotalMemory() );
        _memTrackSize += 1;
        _memLoadPlot.updateUI();
        _totalMem.setText( String.format( "%10d", difxMsg.getBody().getDifxLoad().getTotalMemory() ) );
        _usedMem.setText( String.format( "%10d", difxMsg.getBody().getDifxLoad().getUsedMemory() ) );
        //  Convert transmit and receive rates to Mbits/second (instead of Bytes/sec).
        double newRx = 8.0 * (double)(difxMsg.getBody().getDifxLoad().getNetRXRate()) / 1024.0 / 1024.0;
        double newTx = 8.0 * (double)(difxMsg.getBody().getDifxLoad().getNetTXRate()) / 1024.0 / 1024.0;
        _netRxRate.setText( String.format( "%10.3f", newRx ) );
        _netTxRate.setText( String.format( "%10.3f", newTx ) );
        //  Use the same data to update the monitor window.
        if ( _monitor != null ) {
            _monitor.setNumCPUs( 0 );
            _monitor.setNumCores( difxMsg.getBody().getDifxLoad().getNCore() );
            _monitor.setBogusGHz( 0 );
            _monitor.setType( 0 );
            _monitor.setTypeString( "processor" );
            //if ( dataNode.isStatusCurrent() )
                _monitor.setState( "Online" );
            //else
            //    _monitor.setState( "Lost" );
            _monitor.setCpuLoad( (float)100.0 * difxMsg.getBody().getDifxLoad().getCpuLoad() );
            _monitor.setSysEnabled( false );
            _monitor.setMemLoad( (float)100.0 * (float) difxMsg.getBody().getDifxLoad().getUsedMemory()
                    / difxMsg.getBody().getDifxLoad().getTotalMemory() );
            _monitor.setTotalMem( difxMsg.getBody().getDifxLoad().getTotalMemory() );
            _monitor.setUsedMem( difxMsg.getBody().getDifxLoad().getUsedMemory() );
            _monitor.setNetRxRate( newRx );
            _monitor.setNetTxRate( newTx );
        }
        updateUI();
    }
    
    //--------------------------------------------------------------------------
    //!  Process a diagnostic message associated with this node.
    //--------------------------------------------------------------------------
    public void diagnosticMessage( DifxMessage difxMsg ) {
        _networkActivity.data();
        _activeJob.setText( difxMsg.getHeader().getIdentifier() );
        if ( difxMsg.getBody().getDifxDiagnostic().getDiagnosticType().equalsIgnoreCase( "DataConsumed" ) ) {
            _dataConsumed.setText( "" + difxMsg.getBody().getDifxDiagnostic().getBytes() );
        }
        else if ( difxMsg.getBody().getDifxDiagnostic().getDiagnosticType().equalsIgnoreCase( "InputDatarate" ) ) {
            _inputDatarate.setText( "" + difxMsg.getBody().getDifxDiagnostic().getBytes() );
        }
        else if ( difxMsg.getBody().getDifxDiagnostic().getDiagnosticType().equalsIgnoreCase( "MemoryUsage" ) ) {
            _memoryUsage.setText( "" + difxMsg.getBody().getDifxDiagnostic().getBytes() );
        }
        else if ( difxMsg.getBody().getDifxDiagnostic().getDiagnosticType().equalsIgnoreCase( "NumSubintsLost" ) ) {
            _subintsLost.setText( "" + difxMsg.getBody().getDifxDiagnostic().getNumSubintsLost() );
        }
        else if ( difxMsg.getBody().getDifxDiagnostic().getDiagnosticType().equalsIgnoreCase( "ProcessingTime" ) ) {
            _threadID.setText( "" + difxMsg.getBody().getDifxDiagnostic().getThreadId() );
            _processMicrosec.setText( String.format( "%10.3f", difxMsg.getBody().getDifxDiagnostic().getMicrosec() ) );
        }
        else if ( difxMsg.getBody().getDifxDiagnostic().getDiagnosticType().equalsIgnoreCase( "BufferStatus" ) ) {
            _numBufElements.setText( "" + difxMsg.getBody().getDifxDiagnostic().getNumBufElements() );
            _startBufElement.setText( "" + difxMsg.getBody().getDifxDiagnostic().getStartBufElement() );
            _activeBufElement.setText( "" + difxMsg.getBody().getDifxDiagnostic().getActiveBufElements() );
        }
    }
    
    //--------------------------------------------------------------------------
    //!  Process an "alert" message associated with this node.
    //--------------------------------------------------------------------------
    public void alertMessage( DifxMessage difxMsg ) {
        _networkActivity.data();
        //  See if this alert indicates a job termination.  For this we key on a string
        //  sequence, which seems....non-optimal.
        if ( difxMsg.getBody().getDifxAlert().getAlertMessage().contains( "says BYE!" ) ) {
            _activeJob.flashOff();
            _activeJob.expireNow();
        }
        else
            _activeJob.setText( difxMsg.getHeader().getIdentifier() );
    }

//    public void newAlert( DifxMessage difxMsg ) {
//        _alertWindow.messagePanel().message( 0, 
//                ( difxMsg.getHeader().getFrom() + 
//                " severity " + 
//                difxMsg.getBody().getDifxAlert().getSeverity() ),
//                difxMsg.getBody().getDifxAlert().getAlertMessage() );
//    }
    
    public void showIgnored( boolean newVal ) {
        _showIgnored = newVal;
        changeIgnoreState();
    }

    void changeIgnoreState() {
        this.showThis( _showIgnored || !_ignoreItem.getState() );
    }
    
    public void ignore( boolean newVal ) {
        _ignoreItem.setState( newVal );
    }
    public boolean ignore() { return _ignoreItem.getState(); }
    
    public int numCores() { return Integer.parseInt( _numCores.getText() ); }
    public int numCPUs() { return Integer.parseInt( _numCPUs.getText() ); }
    public String cpuUsage() { return _cpuLoad.getText(); }
    
    public String currentState() { return _state.getText(); }
    public void currentState( String newText ) { _state.setText( newText ); }
    
    public void numCores( int newVal ) { _numCores.setText( "" + newVal ); }
    
    public String activeJob() { return _activeJob.getText(); }
    public void clearActiveJob() { _activeJob.setText( "" ); }
        
    ProcessorMonitorWindow _monitor;
//    MessageWindow _alertWindow;
    ActivityMonitorLight _networkActivity;
    boolean _showNetworkActivity;
    ColumnTextArea _numCPUs;
    boolean _showNumCPUs;
    ColumnTextArea _threadsUsed;
    boolean _showThreadsUsed;
    ColumnTextArea _numCores;
    boolean _showNumCores;
    ColumnTextArea _bogusGHz;
    boolean _showBogusGHz;
    ColumnTextArea _type;
    boolean _showType;
    ColumnTextArea _typeString;
    boolean _showTypeString;
    ColumnTextArea _state;
    boolean _showState;
    boolean _showEnabled;
    ActivityMonitorLight _enabledLight;
    ColumnTextArea _cpuLoad;
    boolean _showCpuLoad;
    PlotWindow _cpuLoadPlot;
    Plot2DObject _cpuPlot;
    Track2D _cpuTrack;
    int _cpuTrackSize;
    boolean _showCpuLoadPlot;
    ColumnTextArea _usedMem;
    boolean _showUsedMem;
    ColumnTextArea _totalMem;      //  Comes from load messages
    boolean _showTotalMem;
    ColumnTextArea _memLoad;
    boolean _showMemLoad;
    PlotWindow _memLoadPlot;
    Plot2DObject _memPlot;
    Track2D _memTrack;
    int _memTrackSize;
    boolean _showMemLoadPlot;
    ColumnTextArea _netRxRate;
    boolean _showNetRxRate;
    ColumnTextArea _netTxRate;
    boolean _showNetTxRate;
    boolean _isMark5;
    
    ColumnTextArea _activeJob;
    boolean _showActiveJob;
    ColumnTextArea _dataConsumed;
    boolean _showDataConsumed;
    ColumnTextArea _inputDatarate;
    boolean _showInputDatarate;
    ColumnTextArea _memoryUsage;     //  Comes from diagnostic messages
    boolean _showMemoryUsage;
    ColumnTextArea _numBufElements;
    boolean _showNumBufElements;
    ColumnTextArea _startBufElement;
    boolean _showStartBufElement;
    ColumnTextArea _activeBufElement;
    boolean _showActiveBufElement;
    ColumnTextArea _threadID;
    boolean _showThreadID;
    ColumnTextArea _processMicrosec;
    boolean _showProcessMicrosec;
    ColumnTextArea _subintsLost;
    boolean _showSubintsLost;
    
    boolean _ignoreState;
    
    protected JCheckBoxMenuItem _ignoreItem;
    protected boolean _showIgnored;
    
    Color _columnColor;
    boolean _colorColumn;
    int _xOff;
    DecimalFormat _dec;

    int _widthNumCPUs;
    int _widthThreadsUsed;
    int _widthNumCores;
    int _widthBogusGHz;
    int _widthType;
    int _widthTypeString;
    int _widthState;
    int _widthEnabled;
    int _widthCpuLoad;
    int _widthCpuLoadPlot;
    int _widthUsedMem;
    int _widthTotalMem;
    int _widthMemLoad;
    int _widthMemLoadPlot;
    int _widthNetRxRate;
    int _widthNetTxRate;

    int _widthActiveJob;
    int _widthDataConsumed;
    int _widthInputDatarate;
    int _widthMemoryUsage;
    int _widthNumBufElements;
    int _widthStartBufElement;
    int _widthActiveBufElement;
    int _widthThreadID;
    int _widthProcessMicrosec;
    int _widthSubintsLost;
    
    protected SystemSettings _settings;

    //  Used to keep track of a "use" of this processor by a job.  The job is
    //  identified by its editor/monitor.
    public class CurrentUse {
        public static final int HEADNODE = 1;
        public static final int DATASOURCE = 2;
        public static final int PROCESSOR = 3;
        
        public JobEditorMonitor jobEditor;
        public int threads;
        public int use;
    };
    
    ArrayList<CurrentUse> _usageList;
    
    public ArrayList<CurrentUse> usageList() { return _usageList; }
    public int threadsUsed() {
        int count = 0;
        for ( Iterator<CurrentUse> iter = _usageList.iterator(); iter.hasNext(); ) {
            CurrentUse thisUse = iter.next();
            count += thisUse.threads;
        }
        return count;
    }
    public void removeJob( JobEditorMonitor job ) {
        for ( Iterator<CurrentUse> iter = _usageList.iterator(); iter.hasNext(); ) {
            CurrentUse thisUse = iter.next();
            if ( thisUse.jobEditor == job )
                iter.remove();
        }
        changeThreadsUsed();
    }
    public void addJob( JobEditorMonitor job, int threads, int use ) {
        CurrentUse newUse = new CurrentUse();
        newUse.jobEditor = job;
        newUse.threads = threads;
        newUse.use = use;
        _usageList.add( newUse );
        changeThreadsUsed();
//        System.out.print( "add job " + job.getName() + " thread: " + threads );
//        if ( use == CurrentUse.DATASOURCE )
//            System.out.println( "  for DATA SOURCE " );
//        if ( use == CurrentUse.PROCESSOR )
//            System.out.println( "  for PROCESSOR " );
//        if ( use == CurrentUse.HEADNODE )
//            System.out.println( "  for HEAD NODE " );
    }
    public boolean isDataSource() {
        for ( Iterator<CurrentUse> iter = _usageList.iterator(); iter.hasNext(); ) {
            CurrentUse thisUse = iter.next();
            if ( thisUse.use == thisUse.DATASOURCE )
                return true;
        }
        return false;
    }
    public boolean isProcessor() {
        for ( Iterator<CurrentUse> iter = _usageList.iterator(); iter.hasNext(); ) {
            CurrentUse thisUse = iter.next();
            if ( thisUse.use == thisUse.PROCESSOR )
                return true;
        }
        return false;
    }
    
    public void changeThreadsUsed() {
        _threadsUsed.setText( threadsUsed() + "/" + ( numCores() - 1 ) );
    }
}
