/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

/**
 *
 * @author mguerra
 */
public class JobKilled  extends JobState
{
   private static JobKilled mInstance = null;
   
   protected JobKilled()
   {
      // singleton
   }

   public static JobState instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobKilled();
      }

      return mInstance;
   }

}
