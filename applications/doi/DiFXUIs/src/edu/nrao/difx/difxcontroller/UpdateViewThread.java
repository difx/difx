/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxcontroller;

import java.util.ConcurrentModificationException;

/**
 *
 * @author mguerra
 */
public class UpdateViewThread  implements Runnable
{
   static boolean mUpdateNow = true;

   private String  mThreadName;
   private boolean mDone = false;

   private DiFXController mTheController;

   // Constructor, give the thread a name
   public UpdateViewThread(String name)
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

   // Process a datagram - unmarshall into DifxMessage and send to controller.
   // The controller is responsible for updating the data model.
   public synchronized void updateView()
   {
      //System.out.printf("************************ Update view. \n");

      // Service the data model
      if (mTheController != null)
      {
         // have the controller process the message to service data model
         if (mTheController.getDataModel() != null)
         {
            (mTheController.getDataModel()).notifyListeners();
         }

      } else
      {
         System.out.printf("************************ Update view DiFX Controller not defined. \n");
      }

      //System.out.printf("************************ Update view complete. \n");
   }

   // Implement the thread interface
   @Override
   public void run()
   {
      synchronized (this)
      {
         long start        = 0l;
         long elapsed_time = 0l;

         long i = 0l;
         while (!mDone)
         {
            try
            {
               // System.out.println("************************ Update view thread running.");

               // no need to throttle the queue loop
               if (UpdateViewThread.mUpdateNow)
               {
                  UpdateViewThread.mUpdateNow = true;
                  start = System.currentTimeMillis();
                  updateView();
                  i++;
                  elapsed_time = System.currentTimeMillis()-start;
                   // System.out.println("************************ Update view thread (" + i + ") took " + elapsed_time + " milliseconds.");
               }

               // wait 1000ms for next update
               Thread.sleep(1000);
               if (Thread.currentThread().isInterrupted() == true)
               {
                  System.out.printf("************************ Update view thread %s interrupted. \n", mThreadName);
                  mDone = true;
               }
            }
            catch ( InterruptedException exception )
            {
               Thread.interrupted();
               System.out.printf("************************ Update view thread %s caught interrupt - done. \n", mThreadName);
               mDone = true;
               //exception.printStackTrace();
            }
            catch ( IllegalMonitorStateException exception )
            {
               System.out.printf("************************ Update view thread %s caught wait illegal monitor interrupt - done. \n", mThreadName);
               mDone = true;
               //exception.printStackTrace();
            }
            catch ( ConcurrentModificationException exception )
            {
               System.out.printf("************************ Update view thread %s caught ConcurrentModificationException - continue. \n", mThreadName);
               mDone = false;
               exception.printStackTrace();
            }
            catch ( NullPointerException exception )
            {
               System.out.printf("************************ Update view thread %s caught NullPointerException - continue. \n", mThreadName);
               mDone = false;
               exception.printStackTrace();
            }
            catch ( IllegalArgumentException exception )
            {
               System.out.printf("************************ Update view thread %s caught IllegalException - continue. \n", mThreadName);
               mDone = false;
               exception.printStackTrace();
            }

         } // while (!done)

         System.out.printf("************************ Update view thread %s done. \n", mThreadName);
      }
   }

}
