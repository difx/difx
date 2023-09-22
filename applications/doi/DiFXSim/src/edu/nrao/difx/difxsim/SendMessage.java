/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxsim;

import edu.nrao.difx.simcontroller.DiFXSystemConfig;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

/**
 *
 * @author mguerra
 */
public class SendMessage {

    public void WriteToSocket(String outString)
    {
        try
        {
            MulticastSocket sock = new MulticastSocket();
            
            // If sending, no need to join multicast group.
            // message.joinGroup();
        
            byte[] buffer = outString.getBytes();            
            DatagramPacket packet = new DatagramPacket(buffer, buffer.length,
                                                       InetAddress.getByName(DiFXSystemConfig.IpAddress),
                                                       DiFXSystemConfig.Port);
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
