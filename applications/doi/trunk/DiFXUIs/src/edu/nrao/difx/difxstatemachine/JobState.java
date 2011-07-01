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
public abstract class JobState {

   public void resetReady(Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   };

   public void resetComplete(Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   };

   public void start(Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   };

   public void stop(Job job, Job.JobStopEvents qEvt)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

   public void accept(Job job)
   {
      throw new UnsupportedOperationException("Not supported yet.");
   }

   protected void changeState(Job job, JobState state)
   {
      // The job state will change while jobs are running
      job.changeState(state);
   }
}
