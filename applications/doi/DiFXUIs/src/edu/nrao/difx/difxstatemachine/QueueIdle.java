/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

import edu.nrao.difx.difxdatamodel.*;
import java.util.ArrayList;

/**
 *
 * @author mguerra
 */
public class QueueIdle implements QueueState
{
   private static QueueIdle mInstance = null;

   protected void QueueIdle()
   {
      // singleton
   }

   public static QueueState instance()
   {
      if (mInstance == null)
      {
         mInstance = new QueueIdle();
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

      } // -- if (queue != null)
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

      } // -- if (queue != null)
   }

   @Override
   public void addRunNow(Queue queue, Job job)
   {
      // add the job
      if (queue != null)
      {
         // add next job to run
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
      if (queue != null)
      {
         // remove job from 2 lists: mJobsToRun and mJobs
         queue.removeJobToRun(job);
         removeJob = queue.removeJob(job);

         // If queue is empty, change state
         ArrayList<Job> jobs = queue.getJobs();
         if ( (jobs == null) || (jobs.isEmpty() == true) )
         {
            changeState(queue, QueueEmpty.instance());
         }

      } // -- if (queue != null)

      return removeJob;
   }

   @Override
   public boolean removeRun(Queue queue, Job job)
   {
      boolean removeJob = false;

      // delete job from run queue
      if (queue != null)
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
System.out.printf("QueueIdle run(). \n");

         queue.setPause(false);

         // Are there jobs to run?
         ArrayList<Job> jobsToRun = queue.getJobsToRun();
         if ( (jobsToRun != null) && (jobsToRun.isEmpty() != true) )
              //&& (queue.getCurrentJobIndex() < (jobsToRun.size()-1))  )
         {
            // Change state and run the job
            changeState(queue, QueueRun.instance());
            queue.run();
         }
         else // -- no jobs to run or complete
         {
            // reset current job, no need to raise an exception
            queue.setCurrentJob(null);
            queue.setCurrentJobIndex(-1);
         }

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
            job.setStarted(false);
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
   public void done(Queue queue, Queue.QueueStopEvents qEvt)
   {
      // do not do a thing, stay in idle state
      // throw new UnsupportedOperationException("Queue Idle: message not supported yet.");
   }

   @Override
   public void accept(Queue queue)
   {
      // do not do a thing, stay in idle state
      // throw new UnsupportedOperationException("Queue Idle: message not supported yet.");
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
      // Change state and run the job
      changeState(queue, QueuePause.instance());
      queue.pause();
   }

}
