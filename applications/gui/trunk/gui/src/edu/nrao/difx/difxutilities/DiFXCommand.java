/*
 * Base class for commands sent via XML to the DiFX host
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;

import edu.nrao.difx.xmllib.difxmessage.ObjectFactory;
import edu.nrao.difx.xmllib.difxmessage.Header;
import edu.nrao.difx.xmllib.difxmessage.Body;

import edu.nrao.difx.difxcontroller.JAXBDiFXProcessor;

import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

/**
 *
 * @author jspitzak
 */
public class DiFXCommand {
    
    /*
     * Construct the skeleton of a generic message.
     */
    public DiFXCommand( SystemSettings settings ) {
        _settings = settings;
        _factory = new ObjectFactory();
        _header = _factory.createHeader();
        _header.setFrom( "doi" );
        _header.setTo( _settings.difxControlAddress() );
        _header.setMpiProcessId( "0" );
        _header.setIdentifier( "doi" );
        _body = _factory.createBody();
        _difxMsg = _factory.createDifxMessage();
    }
    
    /*
     * Used to set the "identifier" if something other than the default is needed.
     */
    public void identifier( String newVal ) {
        _header.setIdentifier( newVal );
    }
    
    /*
     * Similarly, change "from".
     */
    public void from( String newVal ) {
        _header.setFrom( newVal );
    }
    
    /*
     * Same thing for the Mpi Process Id.
     */
    public void mpiProcessId( String newVal ) {
        _header.setMpiProcessId( newVal );
    }

    /*
     * Send a command as a relay command packet - if TCP relay is being used these
     * are sent to the guiServer and then relayed via UDP to mk5daemon (if guiServer
     * isn't being used they are sent to mk5daemon directly using UDP).
     */
    public void send() throws java.net.UnknownHostException {
        try {
            sendPacket( _settings.guiServerConnection().COMMAND_PACKET );
        } catch ( java.net.UnknownHostException e ) {
            throw( e );
        }
    }
    
    /*
     * Convert the message to XML and send it with the specified packet type.  The
     * packet type only applies when the connection is to the guiServer - otherwise
     * UDP is used with no packet type (and the instructions go directly to mk5daemon).
     */
    public void sendPacket( int packetType ) throws java.net.UnknownHostException {
        _difxMsg.setHeader( _header );
        _difxMsg.setBody( _body );
        JAXBDiFXProcessor xmlProc = new JAXBDiFXProcessor( _difxMsg );
        String xmlString = xmlProc.ConvertToXML();
//        System.out.println( xmlString );
        if ( xmlString != null ) {
            if ( _settings.sendCommandsViaTCP() ) {
                byte [] data = xmlString.getBytes();
                _settings.guiServerConnection().sendPacket( packetType, data.length, data );
                data = null;
            }
            else {
                try {
                    MulticastSocket sock = new MulticastSocket();

                    byte[] buffer = xmlString.getBytes();
                    DatagramPacket packet = new DatagramPacket(buffer, buffer.length,
                                                            InetAddress.getByName( _settings.ipAddress() ),
                                                            _settings.difxControlPort() );
                    sock.setTimeToLive(5);
                    sock.send(packet);
                    sock.close();
                } catch ( java.net.UnknownHostException e ) {
                    throw( e );
                } catch (IOException ex) {
                    java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null, ex);
                }
            }
        }
    }
    
    /*
     * Return a string representation of this XML command.
     */
    public String convertToXML() {
        _difxMsg.setHeader( _header );
        _difxMsg.setBody( _body );
        JAXBDiFXProcessor xmlProc = new JAXBDiFXProcessor( _difxMsg );
        return xmlProc.ConvertToXML();
    }
    
    public ObjectFactory factory() { return _factory; }
    public Body body() { return _body; }
    public Header header() { return _header; }
    
    protected SystemSettings _settings;
    protected Header _header;
    protected Body _body;
    protected DifxMessage _difxMsg;
    protected ObjectFactory _factory;
    
}
