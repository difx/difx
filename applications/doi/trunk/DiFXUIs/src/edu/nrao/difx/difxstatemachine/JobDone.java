/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

/**
 *
 * @author mguerra
 */
public class JobDone  extends JobState
{
   private static JobDone mInstance = null;

   protected JobDone()
   {
      // singleton
   }

   public static JobState instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobDone();
      }

      return mInstance;
   }

}
