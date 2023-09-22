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
public class JobNotQueued extends JobState
{
   private static JobNotQueued mInstance = null;

   protected JobNotQueued()
   {
      // singleton
   }

   public static JobState instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobNotQueued();
      }

      return mInstance;
   }

   @Override
   public void resetReady(Job job)
   {
      if ( job != null )
      {
         changeState(job, JobReady.instance());
      }
      
   };
}
