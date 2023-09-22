/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

/**
 *
 * @author mguerra
 */
public class JobComplete  extends JobState
{
   private static JobComplete mInstance = null;

   protected JobComplete()
   {
      // singleton
   }

   public static JobState instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobComplete();
      }

      return mInstance;
   }

}
