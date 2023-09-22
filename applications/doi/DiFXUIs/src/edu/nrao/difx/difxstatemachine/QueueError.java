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
public class QueueError implements QueueState
{

   private static QueueError mInstance = null;

   protected QueueError()
   {
      // singleton
   }

   public static QueueState instance()
   {
      if (mInstance == null)
      {
         mInstance = new QueueError();
      }

      return mInstance;
   }

   @Override
   public void add(Queue queue, Job job)
   {
      // do not do a thing, stay in error state
      // throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public void addRun(Queue queue, Job job)
   {
      // do not do a thing, stay in error state
      // throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public void addRunNow(Queue queue, Job job)
   {
      // do not do a thing, stay in error state
      // throw new UnsupportedOperationException("Not supported yet.");
   }

   @Override
   public boolean remove(Queue queue, Job job)
   {
      // do not do a thing, stay in error state
      // throw new UnsupportedOperationException("Not supported yet.");
      return false;
   }

   @Override
   public boolean removeRun(Queue queue, Job job)
   {
      // do not do a thing, stay in error state
      // throw new UnsupportedOperationException("Not supported yet.");
      return false;
   }

   @Override
   public void run(Queue queue)
   {
      // can not run in the error state
      throw new UnsupportedOperationException("Queue in error, clear error and re-run.");
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
System.out.printf("QueueError done() - DONE. \n");
               break;
            }
            case MPIDONE:
            {
System.out.printf("QueueError done() - MPIDONE. \n");
               break;
            }
            case ERROR:
            {
System.out.printf("QueueError done() - ERROR. \n");
               // get current job
               Job job = queue.getCurrentJob();

               // change into error state, process error
               changeState(queue, QueueError.instance());
               queue.processError(job);
               queue.setCurrentJob(null);
               break;
            }
            default:
            {
System.out.printf("QueueError done() - DEFAULT. \n");
               // some other error, change state to error, stop
               changeState(queue, QueueError.instance());
               break;
            }

         } // -- switch (qEvt)

      } // -- if (queue != null)

   }

   @Override
   public void stop(Queue queue)
   {
      // Stop the current running job
      if (queue != null)
      {
         // get current job, test, and stop
         Job job = queue.getCurrentJob();
         if (job != null)
         {
            queue.stopJob(job);
         }

         // roll back the queue
         queue.setCurrentJobIndex(-1);
         changeState(queue, QueueIdle.instance());
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
         // Set state to idle
         
         // never allow index below -1
         if (queue.getCurrentJobIndex() > -1)
         {
            queue.setCurrentJobIndex(queue.getCurrentJobIndex()-1);
         }
         changeState(queue, QueueIdle.instance());
      }
   }

   @Override
   public void changeState(Queue queue, QueueState state)
   {
      if (queue != null)
      {
         queue.changeState(state);
      }
   }

   @Override
   public void pause(Queue queue)
   {
      // do not do a thing, stay in error state
      // throw new UnsupportedOperationException("Not supported yet.");
   }

}
