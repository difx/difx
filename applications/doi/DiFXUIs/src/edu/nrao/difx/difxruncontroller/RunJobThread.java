/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxruncontroller;

import java.util.logging.Level;
import java.util.logging.Logger;


/**
 *
 * @author mguerra
 */
public class RunJobThread implements Runnable
{

   private String  mThreadName;
   private boolean mDone = false;

   public RunJobThread(String name)
   {
      mThreadName = name;
   }

   public void StartJob()
   {
   }

   public void Pause()
   {
   }

   public void StopJob()
   {
   }

   public void StartAll()
   {
   }

   public void StopAll()
   {
   }

   public void Restart()
   {
   }

   @Override
   public void run()
   {
      synchronized (this)
      {
         try
         {
            long i = 0l;
            while (!mDone)
            {
               try
               {
                  System.out.printf("Jobs queue thread %s running. \n \n", mThreadName);

                  // No need to throttle packet read
                  Thread.sleep(0);

                  // Do not leave group and do not close socket

                  // catch an interrupt, stop thread
                  if (Thread.currentThread().isInterrupted() == true)
                  {
                     System.out.printf("******** Jobs queue thread %s interrupted. \n", mThreadName);
                     mDone = true;
                  }
               }
               catch (InterruptedException exception)
               {
                  Thread.interrupted();
                  System.out.printf("******** Jobs queue thread %s caught interrupt - done. \n", mThreadName);
                  mDone = true;
                  exception.printStackTrace();
               }
               catch (IllegalMonitorStateException exception)
               {
                  System.out.printf("******** Jobs queue thread %s caught wait illegal monitor interrupt - done. \n", mThreadName);
                  mDone = true;
                  exception.printStackTrace();
               }
            }

            System.out.printf("******** Jobs queue thread %s done. \n", mThreadName);
         }
         catch (Exception ex) //IOException ex)
         {
            Logger.getLogger(RunJobThread.class.getName()).log(Level.SEVERE, null, ex);
         }
      }
   }
}
