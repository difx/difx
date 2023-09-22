/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.simcontroller;


import java.io.*;
import java.util.*;

/**
 *
 * @author mguerra
 */
public class MessageThread implements Runnable{

    private String threadName;
    private boolean done = false;
    private MessageQueue messageQueue;
    
    public MessageThread(String name)
    {
        threadName   = name;
    }
    
    public void shutDown()
    {
        done = true;
    }
    
    public void AddQueue(MessageQueue queue)
    {
        messageQueue = queue;
    }
    
    public MessageQueue GetQueue()
    {
        return messageQueue;
    }

    public void run()
    {
        synchronized(this)
        {
            int i = 1;
            while (!done)
            {
                try
                {
                    //System.out.printf("Message thread %s running. \n \n", threadName);

                    // Wait for the message
                    MulticastMessage message = new MulticastMessage();
                    message.receivePacket();
                    System.out.printf("Message thread received packet(%d). \n", i);
                    //message.printPacket();
                    i++;

                    Thread.sleep(0);
                    
                    //Insert raw multicast message into the queue
                    if (messageQueue.add(message))
                    {
                        System.out.printf("Message thread message added into queue. \n");                      
                    }
                    
                    // Leave group and do not close socket
                    // message.leaveGroup();
                    
                    if (Thread.currentThread().isInterrupted() == true)
                    {
                        System.out.printf("Message thread %s interrupted. \n", threadName);
                        done = true;
                    }
                }
                catch ( InterruptedException exception )
                {
                    Thread.interrupted();
                    System.out.printf("Message thread %s caught interrupt - done. \n", threadName);
                    done = true;
                    exception.printStackTrace();
                }
                catch ( IllegalMonitorStateException exception)
                {
                    System.out.printf("Message thread %s caught wait illegal monitor interrupt - done. \n", threadName);
                    done = true;
                    exception.printStackTrace();
                }
            }
            
            System.out.printf("Message thread %s done. \n", threadName);
        }
    }
}
