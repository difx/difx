/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.simcontroller;

import java.util.*;
import edu.nrao.difx.xmllib.difxmessage.*;  

/**
 *
 * @author mguerra
 */

public class MessageQueue implements Runnable {

    private String queueName;
    private boolean done = false;
    private Queue<MulticastMessage> theQueue;
    private SimController theController;
            
    public MessageQueue(String name)
    {
        queueName   = name;
        theQueue = new LinkedList<MulticastMessage>();
    }

    public void shutDown()
    {
        done = true;
    }
    
    public boolean add(MulticastMessage msg)
    {
        return (theQueue.add(msg));
    }
    
    public MulticastMessage remove()
    {
        return(theQueue.remove());
    }

    public void SetController(SimController controller)
    {
        theController = controller;
    }
    

    public void ProcessMessage(MulticastMessage msg)
    {
        //System.out.printf("******** Message queue process message. \n");                       

        // Process message into JAXB
        JAXBPacketProcessor packProc = new JAXBPacketProcessor(msg.GetPacket());
        DifxMessage difxMsg = packProc.ConvertToJAXB();
        msg.printPacket();
        
        // Service the data model
        // ServiceDataModel( difxMsg );

        // Have the controller process the message
        theController.ProcessMessage(difxMsg);
        
        //System.out.println("******** Message queue process message complete. \n");
    }   

    protected synchronized void ServiceDataModel(DifxMessage difxMsg)
    {
        System.out.printf("******** Message queue service data model. \n");                       
/*        
        if (theModel != null)
        {
            theModel.ServiceDataModel(difxMsg);
        }
        else
        {
            System.out.printf("******** Message queue data model not defined. \n");                               
        }        
*/
        System.out.println("******** Message queue service data model complete. \n");

    }
    
    public void run()
    {
        synchronized(this)
        {
            int i = 0;
            while (!done)
            {
                try
                {
                    //System.out.printf("******** Sim Message queue thread %s running. \n \n", queueName);

                    // Check queue for the message
                    if (!theQueue.isEmpty())
                    {
                        // get and process the message
                        MulticastMessage message = remove();
                        System.out.printf("******** DiFX-Sim message queue read message(%d) read from %s. \n", i, queueName);                       

                        // process the message
                        ProcessMessage( message );
                        i++;
                    }
                    
                    Thread.sleep(100);

                    if (Thread.currentThread().isInterrupted() == true)
                    {
                        System.out.printf("Message queue thread %s interrupted. \n", queueName);
                        done = true;
                    }
                }
                catch ( InterruptedException exception )
                {
                    Thread.interrupted();
                    System.out.printf("Message queue thread %s caught interrupt - done. \n", queueName);
                    done = true;
                    exception.printStackTrace();
                }
                catch ( IllegalMonitorStateException exception)
                {
                    System.out.printf("Message queue %s caught wait illegal monitor interrupt - done. \n", queueName);
                    done = true;
                    exception.printStackTrace();
                }
            }
            
            System.out.printf("Message queue %s done. \n", queueName);
        }
    }
}
