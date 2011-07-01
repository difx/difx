/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxstatemachine;

import edu.nrao.difx.difxdatamodel.*;
import edu.nrao.difx.difxdatamodel.DiFXSystemStatus.JobStates;
import java.util.ArrayList;
import java.util.Iterator;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

/**
 *
 * @author mguerra
 */
public class QueueRun implements QueueState
{

   private static QueueRun mInstance = null;

   protected void QueueRun()
   {
      // singleton
   }

   public static QueueState instance()
   {
      if (mInstance == null)
      {
         mInstance = new QueueRun();
      }

      return mInstance;
   }

   @Override
   public void add(Queue queue, Job job)
   {
      // add job into run queue
      if (queue != null)
      {
         // add to job list
         queue.addJob(job);
      } 
   }

   @Override
   public void addRun(Queue queue, Job job)
   {
      // add the job
      if (queue != null)
      {
         if (queue.addJobToRun(job))
         {
            job.setQueued(true);
         }
         
      } // -- if (queue != null)
   }

   @Override
   public void addRunNow(Queue queue, Job job)
   {
      // add the job
      if (queue != null)
      {
         //add next job to run
         int loc = queue.getCurrentJobIndex() + 1;
         queue.addJobToRun(loc, job);
         job.setQueued(true);
         
      } // -- if (queue != null)
   }

   @Override
   public boolean remove(Queue queue, Job job)
   {
      boolean removeJob = false;

      // delete job from queue
      if ((queue != null) && (job.isRunning() != true))
      {
         removeRun(queue, job);
         removeJob = queue.removeJob(job);
      }

      return removeJob;
   }

   @Override
   public boolean removeRun(Queue queue, Job job)
   {
      boolean removeJob = false;

      // delete job from run queue
      if ((queue != null) && (job.isRunning() != true))
      {
         job.setQueued(false);
         removeJob = queue.removeJobToRun(job);
      }

      return removeJob;
   }

//////

   @Override
   public void run(Queue queue)
   {
      if (queue != null)
      {
System.out.printf("QueueRun run(). \n");
         //ArrayList<Job> setOfJobs = queue.getDisjointJobs();
         ArrayList<Job> setOfJobs = queue.getNextJobToRun();

         if ( setOfJobs != null && !setOfJobs.isEmpty() )
         {
            // Start each job in the run queue
            Iterator it = setOfJobs.iterator();
            while (it.hasNext() == true)
            {
               Job element = (Job) it.next();
               if ( !element.isStarted() && (!element.conflict()) ) // && (element.isReady() || element.isJobReady()) )
               {
System.out.printf("QueueRun run() ==> %s. \n", element.getObjName());
                  // delete *.difx and assume job will run
                  int opt = 0;

                  // Force the job or does a .difx file exist?
                  if ( (!queue.toForce()) && (element.diFxFileExists() == true) )
                  {
                     // Raise dialog: delete *.difx file
                     Object[] options = {"Yes", "No"};
                     JFrame frame = null;
                     opt = JOptionPane.showOptionDialog(frame,
                              element.getObjName() + ".difx file exists. \nDo you wish to delete the file and continue the job? \n\n",
                              "Job Queue - .difx exists",
                              JOptionPane.YES_NO_OPTION,
                              JOptionPane.QUESTION_MESSAGE,
                              null, options, options[0]);
                  }

                  // selected to run the job and delete *.difx file
                  if (opt == 0)
                  {
                     queue.setForce(true);

                     // send XML message to kick off the job: set and timestamp the
                     // current job, and run it.
                     element.setStarted(true);
                     queue.runJob(element);
                  }
                  else // -- assume this job is done, get next job
                  {
                     //change state to done
                     changeState(queue, QueueDone.instance());

                     // simulate a stop mpi processes ended, change state, run next
                     queue.stopMPIDone();

                  } // -- if (opt == 0)

               } // if ( element.getState() != JobStates.RUNNING )

               // clean up
               element = null;

            } // while (it.hasNext() == true)

            // clean up
            setOfJobs = null;
         }
         else // -- no ready jobs, return to idle
         {
System.out.printf("QueueRun run() ==> no ready disjoint jobs, are any jobs running? \n");
            if ( !queue.anyJobsRunning() )
            {
System.out.printf("QueueRun run() ==> no ready disjoint jobs, no jobs running, return to idle, stop. \n");
               changeState(queue, QueueIdle.instance());
            }

         } // if ( setOfJobs != null && !setOfJobs.isEmpty() )

      } // -- if (queue != null)

   }


//////

//////

   public void XrunX(Queue queue)
   {
      if (queue != null)
      {
         // Start each job in the run queue
         Iterator it = (queue.getJobsToRun()).iterator();
         while (it.hasNext() == true)
         {
            Job element = (Job) it.next();
            if ( element.getState() != JobStates.RUNNING )
            {
               // delete *.difx
               queue.setForce(true);

               // set and timestamp the current job, and run it
               queue.runJob(element);
               
            } // if ( element.getState() != JobStates.RUNNING )

            element = null;

         } // while (it.hasNext() == true)

      } // -- if (queue != null)

   }


//////

//////
   public void YrunY(Queue queue)
   {
      if (queue != null)
      {
         if ( queue.isAnyJobReady() )
         {
            // start the next job and change state
            Job job = queue.getNexxtJobToRun();
            while (job != null)
            {
                  // Only run the ready jobs - mark5 units are idle or closed.
                  if (job.isJobReady() == true)
                  {
                     // delete *.difx
                     queue.setForce(true);

                     // set and timestamp the current job, and run it
                     queue.runJob(job);
                     
                  } // if (job.isJobReady() == true)

                  // wait before firing next job
                  try
                  {
                     Thread.sleep(15000);Thread.yield();
                  }
                  catch (InterruptedException ex)
                  {
                     // do nothing
                     // Logger.getLogger(QueueRun.class.getName()).log(Level.SEVERE, null, ex);
                  }

                  // get next job
                  job = queue.getNexxtJobToRun();
            
            } // while (job != null)

            // clean up
            job = null;

         }
         else // -- no ready jobs, return to idle
         {
            changeState(queue, QueueIdle.instance());

         } // if (isAnyJobReady())

      } // -- if (queue != null)

   }

//////

//////

   public void XXrunXX(Queue queue)
   {
      if (queue != null)
      {
         // start the next job and change state
         Job job = queue.getNexxtJobToRun();
         if (job != null)
         {
            // Only run the ready jobs - mark5 units are idle or closed.
            if (job.isJobReady() == true)
            {
               // assume force *.difx deletion is false
               queue.setForce(false);

               // assume job will run
               int opt = 0;

               // Force the job or does a .difx file exist?
               if ( (!queue.toForce()) && (job.diFxFileExists() == true) )
               {
                  // Raise dialog: delete *.difx file
                  Object[] options = {"Yes", "No"};
                  JFrame frame = null;
                  opt = JOptionPane.showOptionDialog(frame,
                           job.getObjName() + ".difx file exists. \nDo you wish to delete the file and continue the job? \n\n",
                           "Job Queue - .difx exists",
                           JOptionPane.YES_NO_OPTION,
                           JOptionPane.QUESTION_MESSAGE,
                           null, options, options[0]);
               }

               // selected to run the job and delete *.difx file
               if (opt == 0)
               {
                  // delete *.difx
                  queue.setForce(true);

                  // set and timestamp the current job, and run it
                  queue.runJob(job);
               }
               else // -- assume this job is done, get next job
               {
                  //change state to done
                  changeState(queue, QueueDone.instance());

                  // simulate a stop mpi processes ended, change state, run next
                  queue.stopMPIDone();
               }

            }
            else // -- not ready, raise dialog
            {
               changeState(queue, QueueIdle.instance());

               // Raise dialog
               Object[] options = {"OK"};
               JFrame frame = null;
               int opt = JOptionPane.showOptionDialog(frame,
                         "Job: "                   +
                          job.getObjName()         +
                         " is not ready to run. The mark5 units are not ready.\n" +
                         "Job State: "             +
                         job.getStateString() +"\n\n"+
                         "Ready the job and restart.",
                         "Job Queue - Error",
                         JOptionPane.OK_OPTION,
                         JOptionPane.ERROR_MESSAGE,
                         null, options, options[0]);
               //throw new UnsupportedOperationException("Job "
               //                                        + job.getObjName()
               //                                        + " not ready to run. Mark5 units are not ready. \n"
               //                                        + "Ready a job and restart the queue.");
            } // -- if (job.isJobReady() == true)
         }
         else // -- job is not ready
         {
            changeState(queue, QueueIdle.instance());

            // Raise dialog
            //Object[] options = {"OK"};
            //JFrame frame = null;
            //int opt = JOptionPane.showOptionDialog(frame,
            //          "A job is not ready to run. Get a job ready and re-run. \n",
            //          "Job Queue - Error",
            //          JOptionPane.OK_OPTION,
            //          JOptionPane.ERROR_MESSAGE,
            //          null, options, options[0]);
            //throw new UnsupportedOperationException("Job not ready to run. \n"
            //                                        + "Ready a job and restart the queue.");
         } // -- if (job != null)

         // clean up
         job = null;

      } // -- if (queue != null)

   }

//////


   @Override
   public void done(Queue queue, Queue.QueueStopEvents qEvt)
   {
      if (queue != null)
      {
         // change the state
         switch (qEvt)
         {
            case DONE:
            {
System.out.printf("QueueRun done() - DONE. \n");
               //if ( !queue.isAnyJobReady() )
               //if ( queue.allJobsFinished() )
               // verify no jobs to run before changing that state to done. . .
               if (queue.anyJobsToRun() == false)
               {
                  changeState(queue, QueueDone.instance());
               }
               break;
            }
            case MPIDONE:
            {
System.out.printf("QueueRun done() - MPIDONE. \n");
               // -- if job is not done, fall through into error
               Job job = queue.getCurrentJob();
               if ( job.isComplete() == true )
               {
                  // All jobs w/o errors receiving MPI Done should fall through here . . .
                  if (queue.anyJobsToRun() == true)
                  {
                     // if the queue is paused then pause
                     if (queue.isPause() == true)
                     {
                        changeState(queue, QueuePause.instance());
                     }
                     else // -- continue to run the queue
                     {
                        changeState(queue, QueueIdle.instance());
                        queue.run();
                     }
                  }

                  // job is done so break and continue
                  break;
               }
            }
            case ERROR:
            {
System.out.printf("QueueRun done() - ERROR. \n");
               // get current job
               Job job = queue.getCurrentJob();

               // change into error state, process error
               changeState(queue, QueueError.instance());
               queue.processError(job);
               queue.setCurrentJob(null);
               break;
            }
            default:
            {
System.out.printf("QueueRun done() - DEFAULT. \n");
               // some other error, change state to error, stop
               changeState(queue, QueueError.instance());
               break;
            }

         } // -- switch (qEvt)

      } // -- if (queue != null)
   }

   @Override
   public void stop(Queue queue)
   {
      // Stop the current running job
      if (queue != null)
      {
         // get current job, test, and stop
         Job job = queue.getCurrentJob();
         if (job != null)
         {
            job.setStarted(false);
            queue.stopJob(job);
         }

         // roll back the queue
         queue.setCurrentJobIndex(-1);
         changeState(queue, QueueIdle.instance());
      }
   }

   @Override
   public void stopRun(Queue queue, Job job)
   {
      // Stop the job, and leave in run state
      if (queue != null)
      {
         // stop job stop
         if (job != null)
         {
            queue.stopJob(job);
         }

         // -- do not change state

      }
   }

   @Override
   public void accept(Queue queue)
   {
      if (queue != null)
      {
         // get current job, reset not started
         Job job = queue.getCurrentJob();
         if (job != null)
         {
            job.setStarted(false);
         }

         // change the state to idle
         changeState(queue, QueueIdle.instance());
      }
   }

   @Override
   public void changeState(Queue queue, QueueState state)
   {
      if (queue != null)
      {
         queue.changeState(state);
      }
   }

   @Override
   public void pause(Queue queue)
   {
      if (queue != null)
      {
         //change state to pause
         changeState(queue, QueuePause.instance());
         queue.pause();
      }
   }

}
