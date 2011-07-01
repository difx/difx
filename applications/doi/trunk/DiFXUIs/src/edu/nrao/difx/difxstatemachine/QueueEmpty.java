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
public class QueueEmpty implements QueueState {

   private static QueueEmpty mInstance = null;
   
   protected QueueEmpty()
   {
      // singleton
   }

   public static QueueState instance()
   {
      if (mInstance == null)
      {
         mInstance = new QueueEmpty();
      }

      return mInstance;
   }

   @Override
   public void add(Queue queue, Job job)
   {
      // add the job
      if (queue != null)
      {
         // add to job list
         queue.addJob(job);

         // If queue is not empty, change state
         ArrayList<Job> jobs = queue.getJobs();
         if ( (jobs != null) && (jobs.isEmpty() != true) )
         {
            changeState(queue, QueueIdle.instance());
         }
      }
   }

   @Override
   public void addRun(Queue queue, Job job)
   {
      throw new UnsupportedOperationException("Jobs queue is empty, can not add run job.");
   }

   @Override
   public void addRunNow(Queue queue, Job job)
   {
      // add the job
      if (queue != null)
      {
         // add job to run queue
         int loc = queue.getCurrentJobIndex() + 1;
         queue.addJobToRun(loc, job);
         //queue.addJobToRun(0, job);
         job.setQueued(true);

         // If queue is not empty, change state
         ArrayList<Job> jobs = queue.getJobsToRun();
         if ( (jobs != null) && (jobs.isEmpty() != true) )
         {
            changeState(queue, QueueIdle.instance());
         }

      } // -- if (queue != null)
   }

   @Override
   public boolean remove(Queue queue, Job job)
   {
      throw new UnsupportedOperationException("Jobs queue is empty, no job(s) to delete.");
   }

   @Override
   public boolean removeRun(Queue queue, Job job)
   {
      throw new UnsupportedOperationException("Jobs queue is empty, no run job(s) to delete.");
   }

   @Override
   public void run(Queue queue)
   {
      throw new UnsupportedOperationException("Jobs queue is empty, add job(s) to queue.");
   }

   @Override
   public void stop(Queue queue)
   {
      throw new UnsupportedOperationException("Jobs queue is not running, no job(s) to stop.");
   }

   @Override
   public void stopRun(Queue queue, Job job)
   {
      // Stop the current job running
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
      // -- Do nothing.
      // throw new UnsupportedOperationException("Queue is not running.");
   }

   @Override
   public void accept(Queue queue)
   {
      if (queue != null)
      {
         // delete all jobs to run and do not change the state
         //queue.deleteJobsToRun();
         //queue.deleteCurrentJob();
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
