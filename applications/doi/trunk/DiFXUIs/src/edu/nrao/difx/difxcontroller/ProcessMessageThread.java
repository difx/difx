/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxcontroller;

import edu.nrao.difx.difxdatamodel.DOISystemConfig;
import java.net.*;
import java.util.*;

import edu.nrao.difx.xmllib.difxmessage.*;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author mguerra
 */
public class ProcessMessageThread implements Runnable
{

   private String  mThreadName;
   private boolean mDone = false;

   private BlockingQueue<DatagramPacket> mBlockQueue;

   private DiFXController      mTheController;
   private JAXBPacketProcessor mThePacketProcessor;

   // Constructor, give the thread a name
   public ProcessMessageThread(String name)
   {
      mThreadName         = name;
      mBlockQueue         = new LinkedBlockingQueue<DatagramPacket>();
      mThePacketProcessor = new JAXBPacketProcessor(DOISystemConfig.DiFXJaxbPackage);
   }

   // Stop thread
   public void shutDown()
   {
      mDone = true;
   }

   // Methods specific to Queue
   public boolean add(DatagramPacket pack)
   {
      // no null entries allowed
      try
      {
         return ( mBlockQueue.offer(pack) );
      }
      catch (NullPointerException e)
      {
         return false;
      }
   }

   public DatagramPacket remove() throws InterruptedException
   {
      return ( mBlockQueue.take() );
   }

   // Assign a controller, DiFX has a single GUI controller
   public void setController(DiFXController controller)
   {
      mTheController = controller;
   }

   // Process a datagram - unmarshall into DifxMessage and send to controller.
   // The controller is responsible for updating the data model.
   public synchronized void processMessage(DatagramPacket packet)
   {
      //System.out.printf("**************** Process message queue process message packet. \n");

      // Process message into DiFXMessage
      // printPacket(packet);
      DifxMessage difxMsg = mThePacketProcessor.ConvertToJAXB(packet);
      if (difxMsg != null)
      {
         // service data model - update the internal data
         serviceDataModel(difxMsg);

      }
      else
      {
         System.out.printf("**************** Process message queue DifxMessage not defined. \n");
      }

      // clean up
      difxMsg = null;

      //System.out.println("**************** Process message queue process message packet complete. \n");
   }

   // Service Data Model. . .send the message to be processed
   protected synchronized void serviceDataModel(DifxMessage difxMsg)
   {
      //System.out.printf("**************** Process message service data model. \n");

      // Process message through the data model
      if (mTheController != null)
      {
         // have the controller process the message to service data model
         mTheController.processMessage(difxMsg);

      }
      else
      {
         System.out.printf("**************** Process message queue DiFX Controller not defined. \n");
      }

      //System.out.println("**************** Process message service data model complete. \n");
   }

   // Print a datagram packet to console
   public void printPacket(DatagramPacket packet)
   {
      System.out.println("**************** Process message packet data from: " + packet.getAddress().toString() +
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
         // Element to take from queue
         DatagramPacket packet = null;

         // Loop forever, dequeue and process datagram packets
         while (!mDone)
         {
            try
            {
               // Check queue for the packet
               if (mBlockQueue != null)
               {
                  // Wait and get packet from queue
                  packet = mBlockQueue.take();

                  // process the message packet
                  if (packet != null)
                  {
                     processMessage( packet );

                     //Update the UIs
                     UpdateViewThread.mUpdateNow = true;
                  }
                  else
                  {
                     System.out.printf("******************************** Process message queue take returned null packet ==> Should never occur. \n");
                  }

                  // no need to throttle the queue loop

                  // clean up
                  packet = null;

                  if (Thread.currentThread().isInterrupted() == true)
                  {
                     System.out.printf("**************** Process message thread %s interrupted. \n", mThreadName);
                     mDone = true;
                  }

               } // -- if (mBlockQueue != null)
            }
            catch (InterruptedException exception)
            {
               Thread.interrupted();
               System.out.printf("**************** Process message %s caught interrupt - done. \n", mThreadName);
               mDone = true;
            }
            catch ( NoSuchElementException exception )
            {
               System.out.printf("**************** Process message queue empty exception - continue. \n", mThreadName);
               mDone = false;
               try
               {
                  Thread.sleep(3);
                  System.gc();
               }
               catch (InterruptedException ex)
               {
                  Logger.getLogger(ProcessMessageThread.class.getName()).log(Level.SEVERE, null, ex);
               }
            }

         } // -- while (!mDone)

         System.out.printf("**************** Process message thread %s done. \n", mThreadName);
      }
   }
}
