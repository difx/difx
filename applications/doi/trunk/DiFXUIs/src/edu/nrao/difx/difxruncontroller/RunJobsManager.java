/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxruncontroller;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;

/**
 *
 * @author mguerra
 */
public class RunJobsManager {

   private RunJobThread          mJobThread;
   private ExecutorService       mThreadExecutor;


   public RunJobsManager()
   {
      // create a thread for each job
      mJobThread = new RunJobThread("RunJobThread");
   }

   public RunJobThread getRunJobThread()
   {
      return mJobThread;
   }

   public void startThreads() throws InterruptedException
   {

      System.out.println("RunJobsManager threads starting.");

      // Start the threads
      mThreadExecutor.execute(mJobThread);
      mThreadExecutor.execute(mJobThread);
      Thread.sleep(1);

      System.out.println("RunJobsManager threads started. \n");
   }

   public void stopThreads() {
      mThreadExecutor.shutdown();
      try {
         if (!mThreadExecutor.awaitTermination(1000, TimeUnit.MILLISECONDS))
         {
            mThreadExecutor.shutdownNow();
            if (!mThreadExecutor.awaitTermination(1000, TimeUnit.MILLISECONDS))
            {
               System.err.println("RunJobsManager pool did not terminate");
            }
         }
      } 
      catch (InterruptedException e)
      {
         mThreadExecutor.shutdownNow();
         Thread.currentThread().interrupt();
      }

      System.out.println("RunJobsManager threads stopped \n");
   }

   /**
    * @param args the command line arguments
    */
   public static void main(String args[]) throws InterruptedException {
      RunJobThread jobThread1 = new RunJobThread("JobThread1");
      RunJobThread jobThread2 = new RunJobThread("JobThread2");
      RunJobThread jobThread3 = new RunJobThread("JobThread3");

      System.out.println("RunJobsManager starting threads. \n");

      ExecutorService threadExecutor = Executors.newFixedThreadPool(3);

      threadExecutor.execute(jobThread1);
      threadExecutor.execute(jobThread2);
      threadExecutor.execute(jobThread3);

      Thread.sleep(25);

      threadExecutor.shutdown();
      try {
         if (!threadExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
            threadExecutor.shutdownNow();
            if (!threadExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
               System.err.println("RunJobsManager pool did not terminate");
            }
         }
      } catch (InterruptedException e) {
         threadExecutor.shutdownNow();
         Thread.currentThread().interrupt();
      }

      System.out.println("RunJobsManager started threads, main thread ends \n");
   }
}
