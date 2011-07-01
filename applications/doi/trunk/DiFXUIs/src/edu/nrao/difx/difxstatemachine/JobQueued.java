/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

/**
 *
 * @author mguerra
 */
public class JobQueued extends JobState
{
   private static JobQueued mInstance = null;

   protected JobQueued()
   {
      // singleton
   }

   public static JobState instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobQueued();
      }

      return mInstance;
   }

}
