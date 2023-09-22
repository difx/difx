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
import edu.nrao.difx.xmllib.difxmessage.ObjectFactory;

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
   private Unmarshaller   mUnmarshaller;
   private ObjectFactory  mFactory;
   private JAXBContext    mJaxbCtx;
   /**
    * Constructor for this class that accepts a datagram packet as
    * input.
    */
   public JAXBPacketProcessor(String pkg)
   {
      try
      {
         System.out.println(pkg);
         mFactory = new ObjectFactory();
         mJaxbCtx = JAXBContext.newInstance(pkg);
         mUnmarshaller = mJaxbCtx.createUnmarshaller();
      }
      catch (Exception e)
      {
         e.printStackTrace();
      }
   }

   /**
    * Convert a data buffer directly into a JAXB object (i.e. avoid that DatagramPacket
    * stuff).
    */
   public DifxMessage ConvertToJAXB( ByteArrayInputStream is )
   {
      // message to return to client
      DifxMessage difxMsg = mFactory.createDifxMessage();

      try
      {
         difxMsg = (DifxMessage) mUnmarshaller.unmarshal(is);
         is = null;
      }
      catch (javax.xml.bind.JAXBException ex)
      {
          System.out.println( "JAXBException");
          Reader r = new InputStreamReader( is );  
            StringWriter sw = new StringWriter();  
            char[] buffer = new char[1024];  
            try {
            for (int n; (n = r.read(buffer)) != -1; )  
                sw.write(buffer, 0, n);  
            }
            catch ( Exception e ) {}
            String str = sw.toString();
            System.out.println( str );
          System.out.println( is.toString() );
         // XXXTODO Handle exception
         java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null, ex); //NOI18N
      }
        catch (Exception e) {
            e.printStackTrace();
        }

      // return unmarshalled message to client
      return difxMsg;
   }
}
