/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.simcontroller;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

/**
 *
 * @author mguerra
 */
public class ThreadManager {

    volatile boolean done = false;
    private MessageThread readThread;
    private MessageQueue  queueThread;
    private ExecutorService threadExecutor;
    
    public ThreadManager()
    {
        // create thread manager with multisocket and message queue threads, and
        // an executor to manage the threads.
        readThread      = new MessageThread("SimReadSocketThread");
        queueThread     = new MessageQueue("SimMessageQueueThread");
        threadExecutor  = Executors.newFixedThreadPool(3);       
    }
    
    public MessageQueue GetMessageQueue()
    {
        return queueThread;
    }
    
    public void startThreads() throws InterruptedException
    {
        // start the threads
        threadExecutor.execute(queueThread);
        
        // assign a message queue to the read thread
        readThread.AddQueue(queueThread);
        threadExecutor.execute(readThread);
        
        Thread.sleep(100);
        
        System.out.println("Thread manager threads started \n");        
    }
    
    public void stopThreads()
    {
        threadExecutor.shutdown();
        try 
        {
            if (!threadExecutor.awaitTermination(2, TimeUnit.SECONDS))
            {
                threadExecutor.shutdownNow();
                if (!threadExecutor.awaitTermination(2, TimeUnit.SECONDS))
                {
                    System.err.println("Thread manager pool did not terminate");
                }
            }
        }
        catch (InterruptedException e)
        {
            threadExecutor.shutdownNow();
            Thread.currentThread().interrupt();
        }
        
        System.out.println("Thread manager threads stopped \n");        
    }
    
     /**
     * @param args the command line arguments
     */
    public static void main(String args[]) throws InterruptedException 
    {
        MessageThread messageThread1 = new MessageThread("MessageThread1");
        MessageThread messageThread2 = new MessageThread("MessageThread2");
        MessageThread messageThread3 = new MessageThread("MessageThread3");

        System.out.println("Thread manager starting threads. \n");
        
        ExecutorService threadExecutor = Executors.newFixedThreadPool(3);
        
        threadExecutor.execute(messageThread1);
        threadExecutor.execute(messageThread2);
        threadExecutor.execute(messageThread3);
        
        Thread.sleep(100);

        threadExecutor.shutdown();
        try 
        {
            if (!threadExecutor.awaitTermination(60, TimeUnit.SECONDS))
            {
                threadExecutor.shutdownNow();
                if (!threadExecutor.awaitTermination(60, TimeUnit.SECONDS))
                {
                    System.err.println("Pool did not terminate");
                }
            }
        }
        catch (InterruptedException e)
        {
            threadExecutor.shutdownNow();
            Thread.currentThread().interrupt();
        }
        
        System.out.println("Thread manager started threads, main thread ends \n");
    }
}
