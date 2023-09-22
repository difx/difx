/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

import edu.nrao.difx.difxdatamodel.*;

/**
 *
 * @author mguerra
 */
public class QueueDone implements QueueState
{
   private static QueueDone mInstance = null;

   protected void QueueDone()
   {
      // singleton
   }

   public static QueueState instance()
   {
      if (mInstance == null)
      {
         mInstance = new QueueDone();
      }

      return mInstance;
   }

   @Override
   public void add(Queue queue, Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public void addRun(Queue queue, Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public void addRunNow(Queue queue, Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public boolean remove(Queue queue, Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public boolean removeRun(Queue queue, Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public void run(Queue queue)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public void done(Queue queue, Queue.QueueStopEvents qEvt)
   {
      if (queue != null)
      {
         // change the state
         switch (qEvt)
         {
            case DONE:
            {
System.out.printf("QueueDone done() - DONE. \n");
               //changeState(queue, QueueError.instance());
               changeState(queue, QueueDone.instance());
               break;
            }
            case MPIDONE:
            {
System.out.printf("QueueDone done() - MPIDONE. \n");
               // if the queue is paused then pause
               if (queue.isPause() == true)
               {
                  changeState(queue, QueuePause.instance());
               }
               else // -- continue to run the queue
               {
                  changeState(queue, QueueIdle.instance());
                  queue.run();
               }
               break;
            }
            case ERROR:
            {
System.out.printf("QueueDone done() - ERROR. \n");
               changeState(queue, QueueError.instance());
               break;
            }
            default:
            {
System.out.printf("QueueDone done() - DEFAULT. \n");
               // some other error, change state to error
               changeState(queue, QueueError.instance());
            }
         }

      } // -- if (queue != null)
   }

   @Override
   public void stop(Queue queue)
   {
      //throw new UnsupportedOperationException("Not supported yet.");
      // Stop the current running job
      if (queue != null)
      {
         // get current job, test, and stop
         Job job = queue.getCurrentJob();
         if (job != null)
         {
            queue.stopJob(job);
         }

         changeState(queue, QueueError.instance());
         //throw new UnsupportedOperationException("Job " + job.getObjName() + " stopped. \n");
      }
   }

   @Override
   public void stopRun(Queue queue, Job job)
   {
      // Stop the current job running, and leave in run state
      if (queue != null)
      {
         // stop job stop
         if (job != null)
         {
            queue.stopJob(job);
         }

         // -- do not change state
      }
   }
   
   @Override
   public void accept(Queue queue)
   {
      if (queue != null)
      {
         // delete all jobs to run and change the state
         //queue.deleteJobsToRun();
         //queue.deleteCurrentJob();
         changeState(queue, QueueIdle.instance());
      }
   }

   @Override
   public void changeState(Queue queue, QueueState state)
   {
      queue.changeState(state);
   }

   @Override
   public void pause(Queue queue)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

}
