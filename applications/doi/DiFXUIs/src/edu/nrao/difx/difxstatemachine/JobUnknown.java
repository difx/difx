/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

/**
 *
 * @author mguerra
 */
public class JobUnknown extends JobState
{
   private static JobUnknown mInstance = null;

   protected JobUnknown()
   {
      // singleton
   }

   public static JobUnknown instance()
   {
      if (mInstance == null)
      {
         mInstance = new JobUnknown();
      }

      return mInstance;
   }

}
