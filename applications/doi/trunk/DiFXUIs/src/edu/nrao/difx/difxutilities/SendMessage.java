/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxdatamodel.DOISystemConfig;
import edu.nrao.difx.difxdatamodel.DiFXSystemConfig;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

/**
 *
 * @author mguerra
 */
public class SendMessage {

   // Print a datagram packet to console
   private static void printPacket(DatagramPacket packet)
   {
      System.out.println("**************** Send message packet data from: " + packet.getAddress().toString() +
              ":" + packet.getPort() + " with length: " +
              packet.getLength());

      System.out.write(packet.getData(), 0, packet.getLength());
      System.out.println();
   }

    public static void writeToSocket(String outString)
    {
        try
        {
            MulticastSocket sock = new MulticastSocket();
            
            // Marchall bytes, create packet and send
            byte[] buffer = outString.getBytes();            
            DatagramPacket packet = new DatagramPacket(buffer, buffer.length,
                                                       InetAddress.getByName(DOISystemConfig.IpAddress),
                                                       DOISystemConfig.Port);
            //printPacket(packet);
            sock.setTimeToLive(5);
            sock.send(packet);
            sock.close();
        }
        catch (IOException ex)
        {
            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null, ex); //NOI18N
        }   

    }
    
   
}
