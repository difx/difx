/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.simcontroller;


import java.net.*;
import java.io.*;


/**
 *
 * @author mguerra
 */
public class MulticastMessage {

    private MulticastSocket socket;
    private int             port     = DiFXSystemConfig.Port;
    private String          address  = DiFXSystemConfig.IpAddress;
    private byte            buffer[] = new byte[DiFXSystemConfig.BufferSize];
    private DatagramPacket  packet;

    public DatagramPacket GetPacket()
    {
        return packet;
    }
    
    public void SetPacket(DatagramPacket inPack)
    {
        packet = inPack;
        packet.setPort(inPack.getPort());
        packet.setAddress(inPack.getAddress());
    }

    public MulticastMessage()
    {
        initializeMembers();
    }
    
    public void joinGroup()
    {
        try
        {
            socket.joinGroup(InetAddress.getByName(address));
        }
        catch(IOException e)
        {
            System.out.println("Multicast message failed to join multicaast group by address. \n");
        }
    }
    
    public void leaveGroup()
    {
        try
        {
            socket.leaveGroup(InetAddress.getByName(address));
        }
        catch(IOException e)
        {
            System.out.println("Multicast message failed to leave multicaast group by address. \n");
        }
    }

    public void createSocket()
    {
        try
        {
            socket = new MulticastSocket(port);
            socket.setTimeToLive(10);
        }
        catch(IOException e) 
        {
            System.out.println("Multicast message failed to connect multicaast socket to port. \n");
        }               
    }
    
    public void closeSocket()
    {
        try
        {
            socket.leaveGroup(InetAddress.getByName(address));
            socket.close();
        }
        catch(IOException e) 
        {
            System.out.println("Multicast message failed to close socket to port. \n");
        }               
    }

    public void createPacket()
    {
        packet = new DatagramPacket(buffer, buffer.length);
    }
    
    public void receivePacket()
    {
        try
        {
            socket.receive(packet);
        }
        catch(IOException e)
        {
            System.out.println("Multicast message failed to receive data packet from socket. \n");          
        }
   }

    public void sendPacket(DatagramPacket packToSend)
    {
        try
        {
            socket.send(packToSend);
            System.out.println("Multicast message socket sent packet. \n");
        }
        catch(IOException e)
        {
            System.out.println("Multicast message failed to send data packet to socket. \n");          
        }
    }

    public void sendPacket()
    {
        try
        {
            socket.send(packet);
        }
        catch(IOException e)
        {
            System.out.println("Multicast message failed to send data packet to socket. \n");          
        }
    }

    public void printPacket()
    {
        System.out.println("**** Multicast message received data from: " + packet.getAddress().toString() + 
                           ":" + packet.getPort() + " with length: " + 
                           packet.getLength());
        
        System.out.write(packet.getData(), 0, packet.getLength());
        System.out.println();
    }

    private void initializeMembers()
    {
        buffer  = new byte[1024];
        createSocket();
        createPacket();
        joinGroup();
    }
    
}
