/*
 * This is a help class for starting a job. It provides methods for running the
 * job start procedure.
 *
 * Job start procedure:
 * 1. Verify job is ready
 * 2. Queue up the job
 * 3. Start job
 */
package edu.nrao.difx.difxdatamodel;

import edu.nrao.difx.xmllib.difxmessage.*;

/**
 *
 * @author mguerra
 */
public class JobSequence {

   private Job mJob;

   public JobSequence(Job job)
   {
      mJob = job;
   }

   public void JobStart(DifxMessage message)
   {

      if (mJob.isVerified() == true)
      {
         if (VerifyInputFiles() == true)
         {
            StartCorrelation();
         }
         else
         {
            System.out.println("***************** Job start sequence verify resources failed. \n");

         }

      }
      else
      {
         System.out.println("***************** Job start sequence verify resources failed. \n");

      }
   }

   public void JobStop()
   {
      System.out.println("***************** Job start job stop. \n");
   }

   protected boolean VerifyResources()
   {
      System.out.println("***************** Job start sequence verify resources. \n");
      return true;
   }

   protected boolean VerifyInputFiles()
   {
      System.out.println("***************** Job start sequence verify input files. \n");
      return true;
   }

   protected void StartCorrelation()
   {
      System.out.println("***************** Job start sequence start correlation. \n");


   }

}
