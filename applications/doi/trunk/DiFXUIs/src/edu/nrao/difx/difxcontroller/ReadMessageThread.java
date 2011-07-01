/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxcontroller;

import edu.nrao.difx.difxdatamodel.DOISystemConfig;
import edu.nrao.difx.difxdatamodel.DiFXSystemConfig;
import java.net.*;
import java.io.*;

import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author mguerra
 */
public class ReadMessageThread implements Runnable
{

   private String  mThreadName;
   private boolean mDone = false;

   // -- always start the process message thread before this thread.
   private ProcessMessageThread mMessageQueue;

   // Constructor, give the thread a name
   public ReadMessageThread(String name)
   {
      mThreadName = name;
   }

   // Stop thread
   public void shutDown()
   {
      mDone = true;
   }

   // Methods specific to the message queue
   public void addQueue(ProcessMessageThread queue)
   {
      mMessageQueue = queue;
   }

   public ProcessMessageThread getQueue()
   {
      return mMessageQueue;
   }

   private void printPacket(DatagramPacket packet)
   {
      System.out.println("******** Read message packet received data from: " + packet.getAddress().toString() +
              ":" + packet.getPort() + " with length: " +
              packet.getLength());

      System.out.write(packet.getData(), 0, packet.getLength());
      System.out.println();
   }

   // Implement the thread interface
   @Override
   public void run()
   {
      synchronized (this)
      {
         try
         {
            // start time stamp
            String startDate = Calendar.getInstance().getTime().toString();

            // create socket and join group
            int    port    = DOISystemConfig.Port;
            String address = DOISystemConfig.IpAddress;

            // create multicast socket
            MulticastSocket socket = new MulticastSocket(port);
            socket.setSoTimeout(100);              // timeout 100ms
            socket.setReceiveBufferSize(512000);   // max buffer size 512k Bytes
            socket.joinGroup(InetAddress.getByName(address));

            // loop forever, read and queue datagram packets
            while (!mDone)
            {
               try
               {
                  // create buffer and datagram packet
                  byte[] buffer = new byte[DOISystemConfig.BufferSize];    //1050
                  DatagramPacket packet = new DatagramPacket(buffer, buffer.length);

                  // Wait for datagram packet
                  if (packet != null)
                  {
                     // do not process empty packets.
                     try
                     {
                        // Insert raw packet into the queue
                        socket.receive(packet);
                        if ( !mMessageQueue.add(packet) )
                        {
                           System.out.printf("******** Read message thread packet FAILED to add into queue. \n");
                        }
                     }
                     catch (SocketTimeoutException exception)
                     {
                        // socket did not receive message within 100ms, clean up
                        buffer = null;
                        packet = null;
                        Thread.yield();
                     }

                  }
                  else
                  {
                     System.out.printf("******** Read message empty null packet - continue. \n", mThreadName);
                  }

                  // No need to throttle packet read

                  // Do not leave group and do not close socket

                  // clean up
                  buffer = null;
                  packet = null;

                  // catch an interrupt, stop thread
                  if (Thread.currentThread().isInterrupted() == true)
                  {
                     System.out.printf("******** Read message thread %s interrupted. \n", mThreadName);
                     mDone = true;
                  }
               }
               catch (OutOfMemoryError exception)
               {
                  System.out.printf("******** Read message %s caught OutOfMemoryError(%s  %s) - done. \n",
                          mThreadName, startDate, Calendar.getInstance().getTime().toString());
                  mDone = true;
                  exception.printStackTrace();
               }

            } // -- while (!mDone)

            System.out.printf("******** Read message thread %s done. \n", mThreadName);
         }
         catch (IOException ex)
         {
            Logger.getLogger(ReadMessageThread.class.getName()).log(Level.SEVERE, null, ex);
         }
      }
   }
}
