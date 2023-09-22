/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxcontroller;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

/**
 *
 * @author mguerra
 */
public class ThreadManager {

   private ReadMessageThread     mReadThread;
   private ProcessMessageThread  mProcessThread;
   private UpdateViewThread      mUpdateThread;
   private JobStateThread        mJobStateThread;
   private ExecutorService       mThreadExecutor;

   public ThreadManager()
   {
      // create thread manager with multisocket, message queue, and update view threads, and
      // an executor to manage the threads.
      mReadThread     = new ReadMessageThread("ReadMessageThread");
      mProcessThread  = new ProcessMessageThread("ProcessMessageThread");
      mUpdateThread   = new UpdateViewThread("UpdateViewThread");
      mJobStateThread = new JobStateThread("JobStateThread");
      mThreadExecutor = Executors.newFixedThreadPool(4);
   }

   public ProcessMessageThread getProcessThread()
   {
      return mProcessThread;
   }

   public ReadMessageThread getReadThread()
   {
      return mReadThread;
   }

   public UpdateViewThread getUpdateThread()
   {
      return mUpdateThread;
   }

   public JobStateThread getJobStateThread()
   {
      return mJobStateThread;
   }

   public void startThreads() throws InterruptedException
   {
      System.out.println("Thread manager threads starting.");

      // Start the threads
      mThreadExecutor.execute(mUpdateThread);
      mThreadExecutor.execute(mProcessThread);

      // Assign the preccess message thread to the read thread
      mReadThread.addQueue(mProcessThread);

      // Start read thread, and wait to synch up
      mThreadExecutor.execute(mReadThread);
      Thread.sleep(1);

      // start this one dead last
      mThreadExecutor.execute(mJobStateThread);

      System.out.println("Thread manager threads started. \n");
   }

   public void stopThreads() {
      mThreadExecutor.shutdown();
      try {
         if (!mThreadExecutor.awaitTermination(1000, TimeUnit.MILLISECONDS))
         {
            mThreadExecutor.shutdownNow();
            if (!mThreadExecutor.awaitTermination(1000, TimeUnit.MILLISECONDS))
            {
               System.err.println("Thread manager pool did not terminate");
            }
         }
      } 
      catch (InterruptedException e)
      {
         mThreadExecutor.shutdownNow();
         Thread.currentThread().interrupt();
      }

      System.out.println("Thread manager threads stopped \n");
   }

   /**
    * @param args the command line arguments
    */
   public static void main(String args[]) throws InterruptedException {
      ReadMessageThread messageThread1 = new ReadMessageThread("MessageThread1");
      ReadMessageThread messageThread2 = new ReadMessageThread("MessageThread2");
      ReadMessageThread messageThread3 = new ReadMessageThread("MessageThread3");

      System.out.println("Thread manager starting threads. \n");

      ExecutorService threadExecutor = Executors.newFixedThreadPool(3);

      threadExecutor.execute(messageThread1);
      threadExecutor.execute(messageThread2);
      threadExecutor.execute(messageThread3);

      Thread.sleep(25);

      threadExecutor.shutdown();
      try {
         if (!threadExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
            threadExecutor.shutdownNow();
            if (!threadExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
               System.err.println("Pool did not terminate");
            }
         }
      } catch (InterruptedException e) {
         threadExecutor.shutdownNow();
         Thread.currentThread().interrupt();
      }

      System.out.println("Thread manager started threads, main thread ends \n");
   }
}
