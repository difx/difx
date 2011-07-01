  /*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.simcontroller;

import java.net.*;
import java.io.*;
import javax.xml.bind.*;

import edu.nrao.difx.xmllib.difxmessage.*;

/**
*
* @author mguerra
*/
public class JAXBPacketProcessor
{

    /**
    * JAXBPacketProcessor is a thread that handles the packet processing.
    * The packets contain XML messages and upon receipt they are
    * unmarshalled into JAXB objects. The JAXB object is then stuffed
    * into a MulticastGroupEvent object which is received by
    * registered listeners via a callback method invocation.
    */
    private DatagramPacket packet;
    private Unmarshaller unmarshaller;
    private JAXBContext  jaxbContext;
    
    /**
    * Constructor for this class that accepts a datagram packet as
    * input.
    */
    public JAXBPacketProcessor(DatagramPacket packet)
    {
        this.packet = packet;
    }

    /**
    * This is where the contents of the packet are read and converted
    * to a JAXB object.
    */
    public DifxMessage ConvertToJAXB()
    {
        ObjectFactory factory = new ObjectFactory();
        DifxMessage difxMsg = factory.createDifxMessage();
        
        try
        {
            byte[] bytes = packet.getData();
            String xml  = new String(bytes);
            xml = xml.trim();
            // System.out.println("**" + xml + "**");

            ByteArrayInputStream is = new ByteArrayInputStream(xml.getBytes());

            javax.xml.bind.JAXBContext jaxbCtx = 
                    javax.xml.bind.JAXBContext.newInstance(difxMsg.getClass().getPackage().getName());
            
            unmarshaller = jaxbCtx.createUnmarshaller();
            difxMsg = (DifxMessage)unmarshaller.unmarshal(is);
        }
        catch (javax.xml.bind.JAXBException ex) 
        {
            // XXXTODO Handle exception
            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null, ex); //NOI18N
        }         
        catch (Exception e)
        {
            e.printStackTrace();
        }
        
        // return unmarshalled message to client
        return difxMsg;
    }        
    
}
