  /*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxcontroller;

import java.net.*;
import java.io.*;
import javax.xml.bind.*;

import edu.nrao.difx.xmllib.difxmessage.*;

/**
*
* @author mguerra
*/
public class JAXBDiFXProcessor
{

    /**
    * JAXBDiFXProcessor is a thread that handles the DiFX message processing.
    * The messages contain DOI messages and upon receipt they are
    * marshalled into XML strings. The XML is then processed through
    * the DiFX Controller.
    */
    private DifxMessage    message;
    private Marshaller     marshaller;
    
    /**
    * Constructor for this class that accepts a datagram packet as
    * input.
    */
    public JAXBDiFXProcessor(DifxMessage msgToProcess)
    {
        this.message = msgToProcess;
    }

    
    /**
    * This is where the contents of a DiFXxMessage is converted to an XML string.
    */
    public String ConvertToXML()
    {
         String xmlString = null;

         try
         {
            javax.xml.bind.JAXBContext jaxbCtx = 
                    javax.xml.bind.JAXBContext.newInstance(message.getClass().getPackage().getName());

            marshaller = jaxbCtx.createMarshaller();

//            ObjectFactory factory = new ObjectFactory();
            marshaller.setProperty(Marshaller.JAXB_ENCODING, "UTF-8");
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
            StringWriter writer = new StringWriter();
            marshaller.marshal(message, writer);
            xmlString = writer.toString();
            //marshaller.marshal(difxMsg, System.out);
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

         return xmlString;
    }           
    
}
