/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.NodeBrowserScrollPane;
import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.TearOffPanel;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.JLabel;

import java.awt.Font;
import java.awt.Color;

import java.util.Iterator;

import edu.nrao.difx.difxcontroller.DiFXMessageProcessor;
import edu.nrao.difx.difxcontroller.AttributedMessageListener;

import edu.nrao.difx.difxutilities.SMARTMonitor;

import edu.nrao.difx.xmllib.difxmessage.DifxMessage;

public class HardwareMonitorPanel extends TearOffPanel {

    public HardwareMonitorPanel( SystemSettings settings ) {
        _settings = settings;
        _settings.hardwareMonitor( this );
        initComponents();
    }

    /*
     * This method allows me to control resize behavior.  Otherwise I have to
     * leave it up to the layouts, which is a disaster.
     */
    @Override
    public void setBounds(int x, int y, int width, int height) {
        _browserPane.setBounds( 0, 30, width, height - 30 );
        super.setBounds( x, y, width, height );
    }


    private void initComponents() {
        setLayout( null );
        _browserPane = new NodeBrowserScrollPane();
        this.add( _browserPane );
        _browserPane.setBackground( Color.WHITE );
        _mainLabel = new JLabel( "Hardware Monitor" );
        _mainLabel.setBounds( 5, 0, 150, 20 );
        _mainLabel.setFont( new Font( "Dialog", Font.BOLD, 14 ) );
        add( _mainLabel );
        _clusterNodes = new ProcessorNodesHeader( "Processor Nodes", _settings );
        _clusterNodes.addColumnChangeListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _mk5Modules.initializeDisplaySettings();
                _mk5Modules.updateDisplayedData();
                _mk5Modules.setChildColumnWidths();
            }
        } );
        _clusterNodes.backgroundColor( new Color( 255, 204, 153 ) );
        _browserPane.addNode( _clusterNodes );
        _mk5Modules = new Mark5NodesHeader( "Mark5 Modules", _settings );
        _mk5Modules.addColumnChangeListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _clusterNodes.initializeDisplaySettings();
                _clusterNodes.updateDisplayedData();
                _clusterNodes.setChildColumnWidths();
            }
        } );
        _mk5Modules.backgroundColor( new Color( 255, 204, 153 ) );
        _browserPane.addNode( _mk5Modules );
        //  Callback when the user changes the length of network inactivity intervals.
        //  These are measured in seconds in the Settings window, but are set in tenths
        //  of seconds.
        _settings.inactivityChangeListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _clusterNodes.inactivitySettings( _settings.inactivityWarning() * 10,
                        _settings.inactivityError() * 10 );
                _mk5Modules.inactivitySettings( _settings.inactivityWarning() * 10,
                        _settings.inactivityError() * 10 );
            }
        } );
    }
         
    /*
     * Sign up for callbacks from the DiFX message processor for different types
     * of messages.
     */
    public void difxMessageProcessor( DiFXMessageProcessor processor ) {
        processor.addDifxAlertMessageListener(new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processDifxAlertMessage( difxMsg );
            }
        } );
        processor.addDifxLoadMessageListener(new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processDifxLoadMessage( difxMsg );
            }
        } );
        processor.addMark5StatusMessageListener(new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processMark5StatusMessage( difxMsg );
            }
        } );
        //  This may not be the place to absorb DiFX Info messages
        processor.addDifxInfoMessageListener(new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processDifxInfoMessage( difxMsg );
            }
        } );
        _smartMonitor = new SMARTMonitor( _settings );
        _smartMonitor.difxMessageProcessor( processor );
    }
    
    protected void processDifxAlertMessage( DifxMessage difxMsg ) {
        //  Hardware alerts *appear* to only come from mk5daemon, so we key on it
        //  when deciding whether to use them.
        if ( difxMsg.getHeader().getIdentifier().trim().equals( "mk5daemon" ) ) {
            //serviceUpdate( difxMsg );
        }
//        else
//            System.out.println( "Hardware Monitor Panel received DiFX Alert from " + difxMsg.getHeader().getIdentifier() + " - no idea what to do with this!" );
    }
    
    /*
     * DiFX Info messages are probably from DiFX hardware, thus I'm including them here.
     * This may ultimately not be the correct place to respond to them.  In any case, they
     * are assumed to be important and are forwarded as "warnings" to the message center.
     */
    protected void processDifxInfoMessage( DifxMessage difxMsg ) {
        _settings.messageCenter().warning( 0, difxMsg.getHeader().getFrom(), difxMsg.getBody().getDifxInfo().getMessage() );
    }
    
    public synchronized void processMark5StatusMessage( DifxMessage difxMsg ) {

        if ( isFromMark5( difxMsg ) ) {
            
            Mark5Node mk5Module = null;
            for ( Iterator<BrowserNode> iter = _mk5Modules.children().iterator(); iter.hasNext() && mk5Module == null; ) {
                BrowserNode thisModule = iter.next();
                if ( thisModule.name().contentEquals( difxMsg.getHeader().getFrom() ) )
                    mk5Module = (Mark5Node)thisModule;
            }
            if ( mk5Module == null ) {
                mk5Module = new Mark5Node( difxMsg.getHeader().getFrom(), _settings, _smartMonitor );
                _mk5Modules.addChild( mk5Module );
            }
            mk5Module.statusMessage( difxMsg );
            
        } else {

            //  Mark5 status messages may actually be from processors.
            ProcessorNode processor = null;
            for ( Iterator<BrowserNode> iter = _clusterNodes.children().iterator(); iter.hasNext() && processor == null; ) {
                BrowserNode thisModule = iter.next();
                if ( thisModule.name().contentEquals( difxMsg.getHeader().getFrom() ) )
                    processor = (ProcessorNode)thisModule;
            }
            if ( processor == null ) {
                processor = new ProcessorNode( difxMsg.getHeader().getFrom(), _settings );
                _clusterNodes.addChild( processor );
            }
            processor.statusMessage( difxMsg ); 

        }

    }

    public synchronized void processDifxLoadMessage( DifxMessage difxMsg ) {

        if ( isFromMark5( difxMsg ) ) {

            Mark5Node mk5Module = null;
            for ( Iterator<BrowserNode> iter = _mk5Modules.children().iterator(); iter.hasNext() && mk5Module == null; ) {
                BrowserNode thisModule = iter.next();
                if ( thisModule.name().contentEquals( difxMsg.getHeader().getFrom() ) )
                    mk5Module = (Mark5Node)thisModule;
            }
            if ( mk5Module == null ) {
                mk5Module = new Mark5Node( difxMsg.getHeader().getFrom(), _settings, _smartMonitor );
                _mk5Modules.addChild( mk5Module );
            }
            mk5Module.loadMessage( difxMsg );
            
        } else {

            ProcessorNode processor = null;
            for ( Iterator<BrowserNode> iter = _clusterNodes.children().iterator(); iter.hasNext() && processor == null; ) {
                BrowserNode thisModule = iter.next();
                if ( thisModule.name().contentEquals( difxMsg.getHeader().getFrom() ) )
                    processor = (ProcessorNode)thisModule;
            }
            if ( processor == null ) {
                processor = new ProcessorNode( difxMsg.getHeader().getFrom(), _settings );
                _clusterNodes.addChild( processor );
            }
            processor.loadMessage( difxMsg ); 

        }

    }

    /*
     * DiFX messages come from CPUs, either processors or the Mark5 systems.  At
     * the moment (and this is gross, and will be fixed) we are determining whether
     * the source of a message is a cluster node or a Mark5 unit by looking at its
     * name.  Anything that is not a Mark5 is assumed to be a cluster node.
     */
    public boolean isFromMark5( DifxMessage difxMsg ) {
        return _settings.isMark5Name( difxMsg.getHeader().getFrom() );
        //return difxMsg.getHeader().getFrom().substring(0, 5).equalsIgnoreCase( "mark5" );
    }
    
    public BrowserNode processorNodes() { return _clusterNodes; }
    public BrowserNode mk5Modules() { return _mk5Modules; }
    
    private NodeBrowserScrollPane _browserPane;
    protected ProcessorNodesHeader _clusterNodes;
    protected Mark5NodesHeader _mk5Modules;
    private JLabel _mainLabel;
    protected SystemSettings _settings;
    protected SMARTMonitor _smartMonitor;
    
}
