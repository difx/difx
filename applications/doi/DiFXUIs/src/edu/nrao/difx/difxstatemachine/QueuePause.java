/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

import edu.nrao.difx.difxdatamodel.DiFXSystemStatus;
import edu.nrao.difx.difxdatamodel.Job;
import edu.nrao.difx.difxdatamodel.Queue;
import edu.nrao.difx.difxdatamodel.Queue.QueueStopEvents;
import java.util.ArrayList;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

/**
 *
 * @author mguerra
 */
public class QueuePause implements QueueState{

   private static QueuePause mInstance = null;

   protected void QueuePause()
   {
      // singleton
   }

   public static QueueState instance()
   {
      if (mInstance == null)
      {
         mInstance = new QueuePause();
      }

      return mInstance;
   }

   @Override
   public void add(Queue queue, Job job)
   {
      // add job into run queue
      if (queue != null)
      {
         // add to job list
         queue.addJob(job);
      }
   }

   @Override
   public void addRun(Queue queue, Job job)
   {
      // add the job
      if (queue != null)
      {
         if (queue.addJobToRun(job))
         {
            job.setQueued(true);
         }
      }
   }

   @Override
   public void addRunNow(Queue queue, Job job)
   {
      // add the job
      if (queue != null)
      {
         //add job to run queue
         int loc = queue.getCurrentJobIndex() + 1;
         queue.addJobToRun(loc, job);
         job.setQueued(true);

      } // -- if (queue != null)
   }

   @Override
   public boolean remove(Queue queue, Job job)
   {
      boolean removeJob = false;

      // delete job from queue
      if ((queue != null) && (job.isRunning() != true))
      {
         removeRun(queue, job);
         removeJob = queue.removeJob(job);
      }

      return removeJob;
   }

   @Override
   public boolean removeRun(Queue queue, Job job)
   {
      boolean removeJob = false;

      // delete job from run queue
      if ((queue != null) && (job.isRunning() != true))
      {
         job.setQueued(false);
         removeJob = queue.removeJobToRun(job);
      }

      return removeJob;
   }

   @Override
   public void run(Queue queue)
   {
      if (queue != null)
      {         
         queue.setPause(false);

         // return to idle state and run
         changeState(queue, QueueIdle.instance());
         queue.run();

      } // -- if (queue != null)
   }

   @Override
   public void done(Queue queue, QueueStopEvents qEvt)
   {
      if (queue != null)
      {
         // change the state
         switch (qEvt)
         {
            case DONE:
            {
               // go to done state, done will return us to idle
               changeState(queue, QueueDone.instance());
               break;
            }
            case MPIDONE:
            case ERROR:
            {
               // get current job
               Job job = queue.getCurrentJob();

               // change into error state
               changeState(queue, QueueError.instance());

               // Raise dialog,job run exception
               Object[] options = {"Complete", "Fail", "Ready", "Unknown"};
               JFrame frame = null;
               int opt = JOptionPane.showOptionDialog(frame,
                         "Error occured running job: " + job.getObjName() + "\n" +
                         "For the project: " + job.getProjectName() + "\n\n" +
                         "Select an option? \n",
                         "Job Queue - Error",
                         JOptionPane.YES_NO_OPTION,
                         JOptionPane.ERROR_MESSAGE,
                         null, options, options[0]);

               // purge current job
               queue.setCurrentJob(null);

               // process option
               switch (opt)
               {
                  case 0: // complete
                  {
                     // change current job state to COMPLETE, do not run next job
                     job.setState(DiFXSystemStatus.JobStates.COMPLETE);
                     changeState(queue, QueueIdle.instance());
                     break;
                  }
                  case 1: // failed
                  {
                     // change current job state to failed
                     job.setState(DiFXSystemStatus.JobStates.FAILED);
                     changeState(queue, QueueError.instance());
                     break;
                  }
                  case 2: // ready
                  {
                     // change queue to idle and current job state to ready
                     job.setState(DiFXSystemStatus.JobStates.READY);
                     changeState(queue, QueueIdle.instance());
                     break;
                  }
                  default: // unknown
                  {
                     // remain in current state
                     changeState(queue, QueueError.instance());
                     break;
                  }

               } // switch (opt)

               break;
            }
            default:
            {
               // some other error, change state to error
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
      if (queue != null)
      {
         // change queue to idle.
         changeState(queue, QueueIdle.instance());         
      }
   }

   @Override
   public void accept(Queue queue)
   {
      if (queue != null)
      {
         // change queue to idle.
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
      if (queue != null)
      {
         queue.setPause(true);
      }
   }

}
