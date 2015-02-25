/*
 * The SMARTMonitor class maintains a list of S.M.A.R.T. results for different Mark5
 * disks.  To do this it requests callbacks from the DiFXMessageProcessor when
 * DifxSmartMessage messages are received.  Disks are identified by VSN and host
 * name - if a disk is moved from one host to another it becomes a "new disk" in
 * the eyes of this class.
 * 
 * External calls for S.M.A.R.T. information on a disk/host pair either return a pointer
 * to that information or generate a new request (via DiFXCommand_mark5Control) a
 * "getsmart" command on the DiFX host.  This will (hopefully) generate DifxSmartMessages
 * that this class will receive.
 * 
 * Any receipt of data generates callbacks to the external calling classes which then
 * know that new data are available.
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxcontroller.DiFXMessageProcessor;
import edu.nrao.difx.difxcontroller.AttributedMessageListener;

import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.DifxSmart;

import mil.navy.usno.widgetlib.JulianCalendar;

import edu.nrao.difx.difxview.SystemSettings;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Date;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.Calendar;

import javax.swing.event.EventListenerList;

public class SMARTMonitor {
    
    public class SMARTdatum {
        public int id;
        public int value;
    }
    /*
     * Class containing data returned by a SMART inquiry, as well as information about the
     * disk examined, date, and host name.
     */
    public class SMARTData {
        public String host;
        public String vsn;
        public int slot;
        public Date date;
        public ArrayList<SMARTdatum> data = new ArrayList<SMARTdatum>();
    }
    
    protected ArrayList<SMARTData> _smartData;
    protected SystemSettings _settings;
    protected EventListenerList _newDataListeners;
    
    public SMARTMonitor( SystemSettings settings ) {
        _smartData = new ArrayList<SMARTData>();
        _settings = settings;
        _newDataListeners = new EventListenerList();
    }
    
    /*
     * Sign up for callbacks from the DiFXMessageProcessor for "SMART" messages.  These
     * contain the data on disks.
     */
    public void difxMessageProcessor( DiFXMessageProcessor processor ) {
        processor.addDifxSmartMessageListener(new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processDifxSmartMessage( difxMsg );
            }
        } );
    }
    
    /*
     * Parse a DifxSmartMessage and store in a new SMARTData structure (which is
     * then added to the list).
     */
    synchronized protected void processDifxSmartMessage( DifxMessage difxMsg ) {
        JulianCalendar newDate = new JulianCalendar();
        newDate.mjd( difxMsg.getBody().getDifxSmart().getMjd() );
        SMARTData newData = new SMARTData();
        newData.host = difxMsg.getHeader().getFrom();
        newData.vsn = difxMsg.getBody().getDifxSmart().getVsn();
        newData.slot = difxMsg.getBody().getDifxSmart().getSlot();
        newData.date = newDate.getTime();
        for ( Iterator<DifxSmart.Smart> iter = difxMsg.getBody().getDifxSmart().getSmart().iterator(); iter.hasNext(); ) {
            DifxSmart.Smart datum = iter.next();
            SMARTdatum newDatum = new SMARTdatum();
            newDatum.id = datum.getId();
            newDatum.value = datum.getValue();
            newData.data.add( newDatum );
        }
        //  See if this VSN/host/slot combination already exists in our list.  If so,
        //  remove it - it will be replaced by this new one.
        boolean found = false;
        for ( Iterator<SMARTData> iter = _smartData.iterator(); iter.hasNext() && !found; ) {
            SMARTData testData = iter.next();
            if ( testData.host.contentEquals( newData.host ) && testData.vsn.contentEquals( newData.vsn ) && testData.slot == newData.slot ) {
                found = true;
                iter.remove();
            }
        }
        _smartData.add( newData );
        //  Let all listeners know we have new data (of unspecified type).
        Object[] listeners = _newDataListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
    
    /*
     * Return the SMARTData structure for a specific host, VSN, and slot number.
     * A null return means it wasn't found.
     */
    synchronized public SMARTData data( String host, String vsn, int slot ) {
        SMARTData ret = null;
        for ( Iterator<SMARTData> iter = _smartData.iterator(); iter.hasNext() && ret == null; ) {
            SMARTData testData = iter.next();
            if ( testData.host.contentEquals( host ) && testData.vsn.contentEquals( vsn ) && testData.slot == slot ) {
                ret = testData;
            }
        }
        return ret;
    }
    
    /*
     * Generate SMART data for a host.  All VSN's that the host can access are examined,
     * so VSN and slot number are not specified - you get them all.
     */
    public void generate( String host ) {
        DiFXCommand_mark5Control command = new DiFXCommand_mark5Control( "getsmart", host, _settings, false );
        try {
            command.send();
        } catch ( Exception e ) {
            System.out.println( e.getMessage() );
        }
        
    }

    public void addNewDataListener( ActionListener a ) {
        _newDataListeners.add( ActionListener.class, a );
    }

}
