/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
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
