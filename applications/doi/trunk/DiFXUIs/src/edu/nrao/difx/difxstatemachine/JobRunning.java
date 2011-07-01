/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

/**
 *
 * @author mguerra
 */
public class JobRunning  extends JobState
{
   private static JobState mInstance = null;

   protected JobRunning()
   {
      // singleton
   }

   public static JobState instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobRunning();
      }

      return mInstance;
   }

}
