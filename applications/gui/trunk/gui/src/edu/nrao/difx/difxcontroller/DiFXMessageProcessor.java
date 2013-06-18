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
import java.io.ByteArrayInputStream;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.event.EventListenerList;

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

        //  Then figure out what to do with it based on its type.
        if ( difxMsg != null ) {
            Header header = difxMsg.getHeader();
            //System.out.println( header.getFrom() );
            //System.out.println( "         " + header.getType() );

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

}
