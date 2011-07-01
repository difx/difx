/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

/**
 *
 * @author mguerra
 */
public class JobWaiting  extends JobState
{
   private static JobWaiting mInstance = null;

   protected JobWaiting()
   {
      // singleton
   }

   public static JobState instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobWaiting();
      }

      return mInstance;
   }

}
