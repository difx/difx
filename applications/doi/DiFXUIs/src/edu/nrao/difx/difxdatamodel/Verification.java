/*
 * Helper class to verify the resources necessary to run a job. In order to do
 * complete the task, the datamodel is needed.
 */
package edu.nrao.difx.difxdatamodel;

/**
 *
 * @author mguerra
 */
public class Verification {

   private DiFXDataModel mDataModel;
   private DiFXObject    mObject;

   public Verification(DiFXDataModel dataModel, DiFXObject object)
   {
      mDataModel = dataModel;
      mObject    = object;
   }

   protected boolean verifyResources()
   {
      System.out.println("***************** Verification verify resources. \n");
      if (mObject instanceof Job)
      {

      }

      return true;
   }

}
