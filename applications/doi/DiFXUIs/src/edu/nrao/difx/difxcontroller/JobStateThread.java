/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxcontroller;

/**
 *
 * @author mguerra
 */
public class JobStateThread implements Runnable
{
   private String  mThreadName;
   private boolean mDone = false;

   private DiFXController mTheController;

   // Constructor, give the thread a name
   public JobStateThread(String name)
   {
      mThreadName  = name;
   }

   // Stop thread
   public void shutDown()
   {
      mDone = true;
   }

   // Assign a controller, DiFX has a single GUI controller
   public void setController(DiFXController controller)
   {
      mTheController = controller;
   }

   // Serivce the datamodel and update the state of all the jobs.
   public synchronized void serviceDataModel()
   {
      //System.out.printf("************************ Job state service data model. \n");

      // Service the data model
      if (mTheController != null)
      {
         // have the controller process to inform the data model to determine state of all jobs
         if (mTheController.getDataModel() != null)
         {
            (mTheController.getDataModel()).determineStateOfAllJobs();
            //System.out.printf("************************ Job state thread service data model determine jobs state. \n");
         }

      }
      else
      {
         System.out.printf("************************ Job state thread DiFX Controller not defined. \n");
      }

      //System.out.printf("************************ Job state thread service data model complete. \n");
   }

   // Implement the thread interface
   @Override
   public void run()
   {
      synchronized (this)
      {
         while (!mDone)
         {
            try
            {
               // System.out.println("************************ Job state thread running.");
               serviceDataModel();

               // deterine all job state ever 1000ms
               Thread.sleep(250);
               if (Thread.currentThread().isInterrupted() == true)
               {
                  System.out.printf("************************ Job state thread %s interrupted. \n", mThreadName);
                  mDone = true;
               }
            }
            catch ( InterruptedException exception )
            {
               Thread.interrupted();
               System.out.printf("************************ Job state thread %s caught interrupt - done. \n", mThreadName);
               mDone = true;
               //exception.printStackTrace();
            }
            catch ( IllegalMonitorStateException exception )
            {
               System.out.printf("************************ Job state thread %s caught wait illegal monitor interrupt - done. \n", mThreadName);
               mDone = true;
               //exception.printStackTrace();
            }
            catch ( NullPointerException exception )
            {
               System.out.printf("************************ Job state thread %s caught NullPointerException - continue. \n", mThreadName);
               mDone = false;
               exception.printStackTrace();
            }

         } // while (!done)

         System.out.printf("************************ Job state thread %s done. \n", mThreadName);
      }
   }

}
