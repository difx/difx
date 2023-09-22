/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

/**
 *
 * @author mguerra
 */
public class JobConflict extends JobState
{
   private static JobConflict mInstance = null;

   protected JobConflict()
   {
      // singleton
   }

   public static JobState instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobConflict();
      }

      return mInstance;
   }

}
