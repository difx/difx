/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxcontroller;

import java.io.ByteArrayInputStream;
import java.net.DatagramPacket;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

/**
 *
 * @author mguerra
 */
public class JaxbConverter
{
   /** The JAXB context. */
    private JAXBContext jc;

    /** The JAXB unmarshaller. */
    private Unmarshaller unmarshaller;

    JaxbConverter(String pkg)
    {
        try
        {
            jc = JAXBContext.newInstance(pkg);
            unmarshaller = jc.createUnmarshaller();
        } 
        catch (Exception e)
        {
         e.printStackTrace();
        }
    }

    public Object convert(DatagramPacket packet)
    {
        Object jaxbElement = null;
        if (packet != null)
        {
            //convert to EvlaMessage
            ByteArrayInputStream is = new ByteArrayInputStream(packet.getData(),
                                                               0,
                                                               packet.getLength());
            try
            {
                jaxbElement = unmarshaller.unmarshal(is);
            } 
            catch (Exception e)
            {
               e.printStackTrace();
            }
        }
        return jaxbElement;
    }

}
