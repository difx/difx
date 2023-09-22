/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxdatamodel;

import edu.nrao.difx.difxdatamodel.DiFXSystemStatus.JobStates;
import edu.nrao.difx.difxstatemachine.QueueError;
import edu.nrao.difx.difxstatemachine.QueueDone;
import edu.nrao.difx.difxstatemachine.QueueRun;
import edu.nrao.difx.difxstatemachine.QueueEmpty;
import edu.nrao.difx.difxstatemachine.QueueIdle;
import edu.nrao.difx.difxstatemachine.QueueState;
import edu.nrao.difx.difxcontroller.JAXBDiFXProcessor;
import edu.nrao.difx.difxstatemachine.QueuePause;
import edu.nrao.difx.difxutilities.DiFXDialog;
import edu.nrao.difx.difxutilities.SendMessage;
import edu.nrao.difx.xmllib.difxmessage.Body;
import edu.nrao.difx.xmllib.difxmessage.DifxCommand;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.Header;
import edu.nrao.difx.xmllib.difxmessage.ObjectFactory;
import java.awt.Toolkit;
import java.util.*;
import java.text.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Queue extends DiFXObject
{
   public enum QueueStopEvents {
      DONE, MPIDONE, ERROR
   };

   // status and state
   private String mStatus = "EMPTY";
   private DiFXSystemStatus.QueueStates mState =  DiFXSystemStatus.QueueStates.EMPTY;

   private Date             mToday     = new Date();
   private SimpleDateFormat mFormatter = new SimpleDateFormat("hh:mm:ss");

   private String mTotalTime     = mFormatter.format(mToday);
   private String mRemainingTime = mFormatter.format(mToday);
   private String mWallTime      = mFormatter.format(mToday);

   // queued jobs
   private int            mCurrentJobIndex = -1;
   private Job            mCurrentJob      = null;

   private ArrayList<Job> mJobs      = new ArrayList<Job>(25);
   private ArrayList<Job> mJobsToRun = new ArrayList<Job>(25);
   private ArrayList<Job> mDisjointJobs = new ArrayList<Job>(25);


   // Queue state
   private QueueState mQueueState = QueueEmpty.instance();

   // Time members
   private long  mTimeQueueStarted = 0l;
   private long  mTimeQueueStopped = 0l;

   // Force run, delete files
   private boolean mForce = false;

   // Pause queue
   private boolean mPause = false;

   private Queue()
   {
      // some jobs need to verify, so hand it the datamodel
      this.setDataModel(null);
   }

   public Queue(DiFXDataModel model)
   {
      // some jobs need to verify, so hand it the datamodel
      this.setDataModel(model);
   }

//////////////

   public synchronized void clearAllStartedFlags()
   {
      // Verify jobs to run is populated
      if (mJobsToRun != null)
      {
         // Step through all the jobs to run
         Iterator jobit = mJobsToRun.iterator();
         while ( jobit.hasNext() )
         {
            // Get the module's VSN
            Job job = (Job) jobit.next();
            job.setStarted(false);
         } // -- while ( jobit.hasNext() )

      } // -- if (mJobsToRun != null)

   }

   public synchronized ArrayList<Job> getNewDisjointJobs()
   {
      // Disjoint jobs
      ArrayList<Job> jobs = new ArrayList<Job>();

      // Verify jobs to run is populated
      if (mJobsToRun != null)
      {
         // create mark5 to job mapping
         HashMap<String, String> hm = new HashMap<String, String>();

         // Step through all the jobs to run
         Iterator jobit = mJobsToRun.iterator();
         while ( jobit.hasNext() )
         {
            // check if ready job is disjoint
            Job job = (Job) jobit.next();
System.out.printf("Find disjoint job ==> %s %s %s %s. \n", job.getObjName(), job.getState(), job.getStateString(), job.getStatus());
            if ( job.getState() == JobStates.READY )
            {
               // get the job's mark5s
               List<Module> modules = job.getModules();
               if (modules != null)
               {
                  // assume the job is disjoint
                  boolean disjoint = true;
                  
                  // Step through the list of modules
                  Iterator modit = modules.iterator();
                  while ( (disjoint) && modit.hasNext() )
                  {
                     // Get the module's VSN
                     Module module = (Module) modit.next();
                     String vsn    = module.getModuleVSN();

                     // if module can not be added to the disjoinst list of modules
                     //    flag job as disjoint
                     Mark5Unit mark5  = (this.getDataModel()).getMark5UnitViaVSN(vsn);
                     if (mark5 != null)
                     {
                        // Get name, a job can not have 2 VSNs in the same mark5
                        String mark5Name = mark5.getObjName();
                        if ( !hm.containsKey(mark5Name) )
                        {
                           hm.put(mark5Name, job.getObjName());
                        }
                        else // -- not a disjoint job
                        {
                           disjoint = false;
                        }

                     } // -- if (mark5 != null)

                  } // -- while ( (disjoint) && modit.hasNext() )

                  // job is disjoint, add to disjoint job set
                  if (disjoint)
                  {
                        jobs.add(job);
System.out.printf("Add disjoint job ==> %s %s %s %s. \n", job.getObjName(), job.getState(), job.getStateString(), job.getStatus());

                  }
                  else // -- not disjoint remove <mark5,job> mappings from disjoint job set
                  {

                  }

               } // -- if (modules != null)

            } // -- if ( job.getState() == JobStates.READY )

         } // -- while ( jobit.hasNext() )

      } // -- if (mJobsToRun != null)

      return jobs;

   }

   public synchronized void clearDisjointJobs()
   {
      mDisjointJobs.clear();
   }

   public synchronized ArrayList<Job> getDisjointJobs()
   {
      // Disjoint jobs
      ArrayList<Job> jobs = new ArrayList<Job>();

      // Verify jobs to run is populated
      if (mJobsToRun != null)
      {
         // Create set of mark5s, no duplicates allowed
         Set<String> mark5s = new HashSet<String>();

         // Step through all the jobs to run
         Iterator jobit = mJobsToRun.iterator();
         while ( jobit.hasNext() )
         {
            // Get the module's VSN
            Job job = (Job) jobit.next();
System.out.printf("Find disjoint job ==> %s %s %s %s. \n", job.getObjName(), job.getState(), job.getStateString(), job.getStatus());

            // Only check jobs that have not been run
            //if ( (job.getState() != JobStates.COMPLETE) &&
            //     (job.getState() != JobStates.KILLED  ) &&
            //     (job.getState() != JobStates.FAILED  ) &&
            //     (job.getState() != JobStates.NOTREADY) &&
            //     (job.getState() != JobStates.CONFLICT) &&
            //     (job.getState() != JobStates.UNKNOWN ) )
            if (job.getState() == JobStates.READY) //|| (job.getState() == JobStates.WAITING) )
            {
               // get associated mark5s
               List<Module> modules = job.getModules();
               if (modules != null)
               {
                  // assume the job is disjoint
                  boolean disjoint = true;

                  // create temp mark list
                  Set<String> tempSet = new HashSet<String>();
                  tempSet.addAll(mark5s);

                  // Step through the list of modules
                  Iterator modit = modules.iterator();
                  while ( (disjoint) && modit.hasNext() )
                  {
                     // Get the module's VSN
                     Module module = (Module) modit.next();
                     String vsn    = module.getModuleVSN();

                     // if module can not be added to the disjoinst list of modules
                     //    flag job as disjoint
                     Mark5Unit mark5  = (this.getDataModel()).getMark5UnitViaVSN(vsn);
                     if (mark5 != null)
                     {
                        // Get name, a job can not have 2 VSNs in the same mark5
                        String mark5Name = mark5.getObjName();

                        // If insert fails, conflict found because 2 modules exist in the same mark5
                        if ( !tempSet.add(mark5Name) )
                        {
                           disjoint = false;
                        }

                     } // -- if (mark5 != null)

                  } // -- while (modit.hasNext())

                  // job is disjoint, add to disjoint job set
                  if (disjoint)
                  {
                     // now it is safe to add job and mark5s to the mark5 list
                     jobs.add(job);
                     mark5s.clear();
                     mark5s.addAll(tempSet);
System.out.printf("Add disjoint job ==> %s %s %s %s. \n", job.getObjName(), job.getState(), job.getStateString(), job.getStatus());
                  } // -- if (disjoint)

               } // -- if (modules != null)

            } // -- if ( (job.getState() == JobStates.READY) )

         } // -- while ( jobit.hasNext() )

      } // -- if (mJobsToRun != null)

      return jobs;
   }

   //
   // Determine if this job is in the conflict state
   //
   public synchronized void findDisjointJobs()
   {
      // Verify jobs to run is populated
      if (mJobsToRun != null)
      {
         // Create set of mark5s, no duplicates allowed
         Set<String> mark5s = new HashSet<String>();

         // Step through all the jobs to run
         Iterator jobit = mJobsToRun.iterator();
         while ( jobit.hasNext() )
         {
            // Get the module's VSN
            Job job = (Job) jobit.next();

            // get associated mark5s
            List<Module> modules = job.getModules();
            if (modules != null)
            {
               // assume the job is disjoint
               boolean disjoint = true;

               // Step through the list of modules
               Iterator modit = modules.iterator();
               while ( (disjoint) && modit.hasNext() )
               {
                  // Get the module's VSN
                  Module module = (Module) modit.next();
                  String vsn    = module.getModuleVSN();

                  // if module can not be added to the disjoinst list of modules
                  //    flag job as disjoint
                  Mark5Unit mark5  = (this.getDataModel()).getMark5UnitViaVSN(vsn);
                  if (mark5 != null)
                  {
                     // Get name, a job can not have 2 VSNs in the same mark5
                     String mark5Name = mark5.getObjName();

                     // If insert fails, conflict found because 2 modules exist in the same mark5
                     if ( !mark5s.add(mark5Name) )
                     {
                        disjoint = false;
                     }

                  } // -- if (mark5 != null)

               } // -- while (modit.hasNext())

               // job is disjoint, add to dis joint job set
               if (disjoint)
               {
                  mDisjointJobs.add(job);
               }

            } // -- if (modules != null)

         } // -- while ( jobit.hasNext() )

      } // -- if (mJobsToRun != null)

      int i = 0;
   }

//////////////

   // State methods
   public void add(Job job)
   {
      // for alerts add a try catch, have the state throw exception
      // job.resetReady(); // if job state machine is ever implemented, start here.
      
      mQueueState.add(this, job);
   }

   public void addRun(Job job)
   {
      mQueueState.addRun(this, job);
   }

   public void addRunNow(Job job)
   {
      mQueueState.addRunNow(this, job);
   }

   public boolean remove(Job job )
   {
      return(mQueueState.remove(this, job));
   }

   public boolean removeRun(Job job )
   {
      return(mQueueState.removeRun(this, job));
   }

   public void run()
   {
      mQueueState.run(this);
   }

   public void pause()
   {
      mQueueState.pause(this);
   }

   public void stop()
   {
      mQueueState.stop(this);
   }

   public void stopRun(Job job)
   {
      mQueueState.stopRun(this, job);
   }

   public void stopDone()
   {
      mQueueState.done(this, Queue.QueueStopEvents.DONE);
   }

   public void stopError()
   {
      mQueueState.done(this, Queue.QueueStopEvents.ERROR);
   }

   public void stopMPIDone()
   {
      mQueueState.done(this, Queue.QueueStopEvents.MPIDONE);
   }

   public void accept()
   {
      mQueueState.accept(this);
   }
   
   public void changeState(QueueState qState)
   {
      mQueueState = qState;
      if (mQueueState instanceof QueueEmpty)
      {
         setState(DiFXSystemStatus.QueueStates.EMPTY);
         setStatus("EMPTY");
      }
      else if (mQueueState instanceof QueueIdle)
      {
         setState(DiFXSystemStatus.QueueStates.IDLE);
         setStatus("IDLE");
      }
      else if (mQueueState instanceof QueueRun)
      {
         setState(DiFXSystemStatus.QueueStates.RUN);
         setStatus("RUN");
      }
      else if (mQueueState instanceof QueueDone)
      {
         setState(DiFXSystemStatus.QueueStates.DONE);
         setStatus("DONE");
      }
      else if (mQueueState instanceof QueueError)
      {
         setState(DiFXSystemStatus.QueueStates.ERROR);
         setStatus("ERROR");
      }
      else if (mQueueState instanceof QueuePause)
      {
         setState(DiFXSystemStatus.QueueStates.PAUSE);
         setStatus("PAUSE");
      }

   }

   // Other methods
   public boolean exists(Job job)
   {
      return (mJobs.contains(job));
   }

   public boolean existsRun(Job job)
   {
      return (this.mJobsToRun.contains(job));
   }

   public QueueState getQueueState()
   {
      return mQueueState;
   }

   public void setQueueState(QueueState qState)
   {
      mQueueState = qState;
   }

   public long getTimeQueueStarted()
   {
      return mTimeQueueStarted;
   }

   public void setTimeQueueStarted()
   {
      mTimeQueueStarted = System.currentTimeMillis();
   }

   public long getTimeQueueStopped()
   {
      return mTimeQueueStopped;
   }

   public void setTimeQueueStopped()
   {
      mTimeQueueStopped = System.currentTimeMillis();
   }

   public boolean toForce() {
      return mForce;
   }

   public void setForce(boolean force) {
      mForce = force;
   }

   public boolean isPause() {
      return mPause;
   }

   public void setPause(boolean pause) {
      mPause = pause;
   }

   // Is queue ready to execute a job?
   public boolean isQueueIdle()
   {
      return (this.mState == DiFXSystemStatus.QueueStates.IDLE);
   }

   public boolean isQueueEmpty()
   {
      return (this.mState == DiFXSystemStatus.QueueStates.EMPTY);
   }

   public boolean isQueueRun()
   {
      return (this.mState == DiFXSystemStatus.QueueStates.RUN);
   }

   public boolean isQueueError()
   {
      return (this.mState == DiFXSystemStatus.QueueStates.ERROR);
   }

   public boolean isQueuePause()
   {
      return (this.mState == DiFXSystemStatus.QueueStates.PAUSE);
   }

   public String getStatus()
   {
      return mStatus;
   }

   public void setStatus(String newStatus)
   {
      mStatus = newStatus;
   }

   public Date getToday()
   {
      return mToday;
   }

   public void setToday(Date newDate)
   {
      mToday = newDate;
   }

   public String getTotalTime()
   {
      return mTotalTime;
   }

   public void setTotalTime(String newTime)
   {
      mTotalTime = newTime;
   }

   public String getRemainingTime()
   {
      return mRemainingTime;
   }

   public void setRemainingTime(String newTime)
   {
      mRemainingTime = newTime;
   }

   public String getWallTime()
   {
      return mWallTime;
   }

   public void setWallTime(String newTime)
   {
      mWallTime = newTime;
   }

   public int getCurrentJobIndex()
   {
      return mCurrentJobIndex;
   }

   public void setCurrentJobIndex(int newJobIndex)
   {
      mCurrentJobIndex = newJobIndex;
   }

   public Job getCurrentJob()
   {
      return mCurrentJob;
   }

   public void setCurrentJob(Job newJob)
   {
      mCurrentJob = newJob;
   }

   public DiFXSystemStatus.QueueStates getState()
   {
      return mState;
   }

   public void setState(DiFXSystemStatus.QueueStates newState)
   {
      mState = newState;
   }


   //
   // -- Jobs is the complete set of jobs in the queue
   //
   public ArrayList<Job> getJobs()
   {
      return mJobs;
   }

   public void addJob(Job job)
   {
      mJobs.add(job);
   }

   public Job getJob(int index)
   {
      return( mJobs.get(index) );
   }

   public boolean removeJob(Job job)
   {
      return (mJobs.remove(job));
   }

   public boolean insertJob(int index, Job job)
   {
      try
      {
         mJobs.add(index, job);
         return true;
      }
      catch (IndexOutOfBoundsException e)
      {
         return false;
      }
   }

   public Job getJob(String jobName)
   {
      Iterator it = mJobs.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if (element.getObjName().equalsIgnoreCase(jobName))
         {
            // found, return job
            return element;
         }

         // clean up
         element = null;
      }

      // not found
      return null;
   }

   public Job getJob(String jobName, String jobPath)
   {
      Iterator it = mJobs.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( element.getObjName().equalsIgnoreCase(jobName)     &&
              element.getProjectPath().equalsIgnoreCase(jobPath) )
         {
            // found, return job
            return element;
         }

         // clean up
         element = null;
      }

      // not found
      return null;
   }

   public boolean isEmpty()
   {
      return (mJobs.isEmpty());
   }

   public ArrayList<Job> getJobsCopy()
   {
      ArrayList<Job> newJobs = new ArrayList<Job>();
      if (this.mJobs != null)
      {
         // => newJobs = (ArrayList<Job>) this.jobs.clone();
         newJobs.addAll(this.mJobs);
      }

      return newJobs;
   }

   //
   // -- JobsToRun is a subset of jobs
   //
   public ArrayList<Job> getJobsToRun()
   {
      return mJobsToRun;
   }

   public boolean addJobToRun(Job job)
   {
      // assume failure
      boolean success = false;

      if (mJobsToRun != null)
      {
         success = mJobsToRun.add(job);
      }

      return success;
   }

   public void addJobToRun(int index, Job job)
   {
      if (mJobsToRun != null)
      {
         mJobsToRun.add(index, job);
      }
   }

   public boolean removeJobToRun(Job job)
   {
      // assume failure
      boolean success = false;

      if (mJobsToRun != null)
      {
         success = mJobsToRun.remove(job);
      }

      return success;
   }

   public void deleteJobsToRun()
   {
      if (mJobsToRun != null)
      {
         Iterator it = mJobsToRun.iterator();
         while (it.hasNext() == true)
         {
            Job element = (Job) it.next();
            element.setQueued(false);
            element = null;
         }
      }

      mJobsToRun.clear();
   }

   public boolean isAnyJobReady()
   {
      // assume no ready jobs
      boolean anyReady = false;

      if (mJobsToRun != null)
      {
         Iterator it = mJobsToRun.iterator();
         while ( !anyReady && (it.hasNext() == true) )
         {
            Job element = (Job) it.next();
            anyReady = element.isReady();
            element = null;
         }
      }

      return anyReady;
   }

   public synchronized ArrayList<Job> getNextSetOfJobsToRun()
   {
      ArrayList<Job> jobs = new ArrayList<Job>();
      JobStates state     = JobStates.INVALID;

      // Get next job in the queue
      if ( (mJobsToRun != null) && (mJobsToRun.isEmpty() != true) )
      {
         Iterator it = mJobsToRun.iterator();
         while ( it.hasNext() == true)
         {
            Job element = (Job) it.next();
            state = element.getState();
            // found a ready job, so run it
            if ( (state == JobStates.READY  || element.isJobReady() == true)
                 &&
                 ((state != JobStates.COMPLETE) &&
                  (state != JobStates.KILLED  ) &&
                  (state != JobStates.FAILED  ) &&
                  (state != JobStates.UNKNOWN )) )
            {
               jobs.add(element);
            }
            
         } // while ( it.hasNext() == true)

      } // -- if ( (mJobsToRun != null) && (mJobsToRun.isEmpty() != true) )

      // return job to run
      //return job;
      return jobs;
   }

   public synchronized ArrayList<Job> getNextJobToRun()
   {
      ArrayList<Job> jobs = new ArrayList<Job>();
      JobStates state     = JobStates.INVALID;
      boolean jobNotFound = true;

      // Get next job in the queue
      if ( (mJobsToRun != null) && (mJobsToRun.isEmpty() != true) )
      {
         Iterator it = mJobsToRun.iterator();
         while ( (jobNotFound) && (it.hasNext() == true) )
         {
            Job element = (Job) it.next();
            state = element.getState();
            // found a ready job, so run it
            if ( state == JobStates.READY )// || element.isJobReady() == true)
            {
               jobs.add(element);
               jobNotFound = false;
            }

         } // while ( it.hasNext() == true)

      } // -- if ( (mJobsToRun != null) && (mJobsToRun.isEmpty() != true) )

      // return job to run
      return jobs;
   }

   public Job getNexxtJobToRun()
   {
      Job job = null;
      JobStates state     = JobStates.INVALID;
      boolean jobNotFound = true;

      // Get next job in the queue
      if ( (mJobsToRun != null) && (mJobsToRun.isEmpty() != true) )
      {
         // Find job that's ready to run . . .
        mCurrentJobIndex = -1;
         while (jobNotFound)
         {
            // zero based
            if ( (mCurrentJobIndex >= -1) && (mCurrentJobIndex < (mJobsToRun.size()-1)) )
            {
               ++mCurrentJobIndex;
               job = mJobsToRun.get(mCurrentJobIndex);
               state  = job.getState();

               // found a ready job, so run it
               if ( (state == JobStates.READY  || job.isJobReady() == true)
                    &&
                    ( (state != JobStates.COMPLETE) &&
                      (state != JobStates.KILLED  ) &&
                      (state != JobStates.FAILED  ) ) )
               {
                  jobNotFound = false;
               }
            }
            else // -- exit, not found
            {
               mCurrentJobIndex = -1;
               jobNotFound = false;
               job = null;

            } // -- if ( (mCurrentJobIndex >= -1) && (mCurrentJobIndex < (mJobsToRun.size()-1)) )

         } // -- while (jobNotFound)

      } // -- if ( (mJobsToRun != null) && (mJobsToRun.isEmpty() != true) )

      // return job to run
      return job;
   }

   public String getStatusOfCurrentJob()
   {
      // Get status of current job
      if ( (mJobsToRun != null) && (mJobsToRun.isEmpty() != true) )
      {
         if ( (mCurrentJobIndex > -1) && (mCurrentJobIndex < mJobsToRun.size()) )
         {
            return (mJobsToRun.get(mCurrentJobIndex).getStatus());
         }
         else if (mCurrentJobIndex == -1)
         {
            return "Missing Job";
         }
      }

      // No current jobs
      return null;
   }


   /**
    * Start a job
    * @param job 
    */
   public void runJob(Job job)
   {
      if (job != null)
      {
         // send out the message....
         DifxMessage difxMsg = job.createDiFXStartMessage();
         if (difxMsg != null)
         {
            // Convert DiFX message into a XML string
            JAXBDiFXProcessor xmlProc = new JAXBDiFXProcessor(difxMsg);
            String xmlString = xmlProc.ConvertToXML();

            // Conversion succesful, so send the XML
            if (xmlString != null)
            {
               // right before the job is started: timestamp it, flag as started from here
               // and set job as current job
               job.setCorrelationStartUTC();
               setCurrentJob(job);

               SendMessage.writeToSocket( xmlString );
            }
            else
            {
               System.out.printf("***************** Jobs Queue XML not defined or sent. \n");
            }

            // clean up
            xmlString = null;
            xmlProc   = null;
         }
         else // -- failed start the job
         {
            // raise error dialog
            processError(job, "Datastream node mismatch");
         }
      }
   }

   public void stopJob(Job job)
   {
      if (job != null)
      {
         DifxMessage difxMsg = job.createDiFXStopMessage();

         // send out the message....
         if (difxMsg != null)
         {
            // Convert DiFX message into a XML string
            JAXBDiFXProcessor xmlProc = new JAXBDiFXProcessor(difxMsg);
            String xmlString = xmlProc.ConvertToXML();

            // Conversation succesful, so send the XML
            if (xmlString != null)
            {
                // do not set the current job to null until the mpidone is received.
                // setCurrentJob(null);

                SendMessage.writeToSocket( xmlString );
            }
            else
            {
                System.out.printf("***************** Jobs Queue XML not defined or sent. \n");
            }

            // clean up
            xmlString = null;
            xmlProc   = null;
         }
      }
   }

   // Support for running multiple jobs
   public boolean allJobsRunning()
   {
      Iterator it = mJobsToRun.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( element.getState() != JobStates.RUNNING )
         {
            // found, return false
            return false;
         }
         element = null;
      }

      return true;
   }

   public boolean anyJobsRunning()
   {
      Iterator it = mJobsToRun.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( element.getState() == JobStates.RUNNING )
         {
            // found, return true
            return true;
         }
         element = null;
      }

      return false;
   }

   public boolean anyJobsToRun()
   {
      Iterator it = mJobsToRun.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( (element.getState() == JobStates.READY   ) ||
              (element.getState() == JobStates.WAITING ) )
          {
            // there exists a job to be run, return true
            return true;
         }
         element = null;
      }

      // no jobs to run
      return false;
   }

   public boolean anyJobNotReady()
   {
      Iterator it = mJobsToRun.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( element.getState() == JobStates.READY )
          {
            // found, return false
            return false;
         }
         element = null;
      }

      return true;
   }

   public boolean allJobsDoneComplete()
   {
      Iterator it = mJobsToRun.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( ( element.getState() != JobStates.DONE     ) &&
              ( element.getState() != JobStates.COMPLETE ) )
          {
            // found, return false
            return false;
         }
         element = null;
      }

      return true;
   }

   public boolean allJobsFinished()
   {
      boolean retVal = true;

      Iterator it = mJobsToRun.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( (element.getState() != JobStates.COMPLETE) &&
              (element.getState() != JobStates.UNKNOWN ) &&
              (element.getState() != JobStates.FAILED  ) )
         {
            // found, return false
            return false;
         }
         element = null;
      }

      return true;
   }

   public boolean anyJobFailed()
   {
      Iterator it = mJobsToRun.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( element.getState() == JobStates.FAILED)
         {
            // found, return true
            return true;
         }
         element = null;
      }

      return false;
   }

   // Special kill method
   public void killMpifxcorr()
   {
      // just kill mpifxcorr
      DifxMessage difxMsg = createKillMpifxcorrMessage();

      // send out the message....
      if (difxMsg != null)
      {
         // Convert DiFX message into a XML string
         JAXBDiFXProcessor xmlProc = new JAXBDiFXProcessor(difxMsg);
         String xmlString = xmlProc.ConvertToXML();

         // Conversion succesful, so send the XML
         if (xmlString != null)
         {
            // do not set the current job to null until the mpidone is received.
            setCurrentJob(null);

            // do not send until fully tested.
            SendMessage.writeToSocket( xmlString );
         }
         else
         {
             System.out.printf("***************** Jobs Queue XML not defined or sent. \n");
         }

         // clean up
         xmlString = null;
         xmlProc   = null;
      }
   }

   // Send misc message
   public void sendMessage(DifxMessage difxMsg)
   {
      // send out the message....
      if (difxMsg != null)
      {
         // Convert DiFX message into a XML string
         JAXBDiFXProcessor xmlProc = new JAXBDiFXProcessor(difxMsg);
         String xmlString = xmlProc.ConvertToXML();

         // Conversion succesful, so send the XML
         if (xmlString != null)
         {
            // do not set the current job to null until the mpidone is received.
            setCurrentJob(null);

            // do not send until fully tested.
            SendMessage.writeToSocket( xmlString );
         }
         else
         {
             System.out.printf("***************** Jobs Queue XML not defined or sent. \n");
         }

         // clean up
         xmlString = null;
         xmlProc   = null;
      }
   }

   public DifxMessage createKillMpifxcorrMessage()
   {
      ObjectFactory factory = new ObjectFactory();

      // Create header
      Header header = factory.createHeader();
      header.setFrom("doi");
      header.setTo("all");
      header.setMpiProcessId("0");
      header.setIdentifier("doi");
      header.setType("DifxCommand");

      // Create stop job command
      DifxCommand mpiCommand = factory.createDifxCommand();
      mpiCommand.setCommand("killmpifxcorr");


      // -- Create the XML defined messages and process through the system
      Body body = factory.createBody();
      body.setDifxCommand(mpiCommand);

      DifxMessage difxMsg = factory.createDifxMessage();
      difxMsg.setHeader(header);
      difxMsg.setBody(body);

      return difxMsg;
   }

   public void processError(Job jobInError)
   {
			try
			{
				// rin the bell and raise a dialog
				Toolkit.getDefaultToolkit().beep();
				Thread.sleep(250);
				Toolkit.getDefaultToolkit().beep();
				Thread.sleep(250);
				Toolkit.getDefaultToolkit().beep();
				String dlgMsg = "<html><p align=left>" + "Error occured running job: " + jobInError.getObjName() + ".<br>" + "For the project: " + jobInError.getProjectName() + ".<br><br>" + "Select an option.";
				DiFXDialog dialog = new DiFXDialog(null, "Job Queue - Error", dlgMsg, jobInError);
				dialog.setVisible(true);
				String msg = "Error occured running job: " + jobInError.getObjName() + " project: " + jobInError.getProjectName();
				DifxMessage errorMessage = (getDataModel()).CreateDOIErrorMessage(msg);
				sendMessage(errorMessage);
			}
			catch (InterruptedException ex)
			{
				Logger.getLogger(Queue.class.getName()).log(Level.SEVERE, null, ex);
			}
   }

   public void processError(Job jobInError, String message)
   {
			try
			{
				// rin the bell and raise a dialog
				Toolkit.getDefaultToolkit().beep();
				Thread.sleep(250);
				Toolkit.getDefaultToolkit().beep();
				Thread.sleep(250);
				Toolkit.getDefaultToolkit().beep();
				String dlgMsg = "<html><p align=left>" + "Error occured running job: " + jobInError.getObjName() + ".<br>" + "For the project: " + jobInError.getProjectName() + ".<br><br>" + message + ".<br><br>" + "Select an option.";
				DiFXDialog dialog = new DiFXDialog(null, "Job Queue - Error", dlgMsg, jobInError);
				dialog.setVisible(true);
				String msg = "Error occured running job: " + jobInError.getObjName() + " project: " + jobInError.getProjectName() + " :: " + message;
				DifxMessage errorMessage = (getDataModel()).CreateDOIErrorMessage(msg);
				sendMessage(errorMessage);
			}
			catch (InterruptedException ex)
			{
				Logger.getLogger(Queue.class.getName()).log(Level.SEVERE, null, ex);
			}
   }

   @Override
   public void updateObject(DiFXObject newData)
   {
      this.mStatus        = ((Queue) newData).getStatus();
      this.mToday         = ((Queue) newData).getToday();
      this.mTotalTime     = ((Queue) newData).getTotalTime();
      this.mRemainingTime = ((Queue) newData).getRemainingTime();
      this.mWallTime      = ((Queue) newData).getWallTime();
      this.mState         = ((Queue) newData).getState();
      this.mCurrentJob    = ((Queue) newData).getCurrentJob();
      this.mJobs.addAll(((Queue) newData).getJobs());
      this.mJobsToRun.addAll(((Queue) newData).getJobsToRun());
      super.updateObject(newData);
   }

   @Override
   public boolean isEqual(DiFXObject objToCompare)
   {
      return (this.mStatus.equals(((Queue) objToCompare).getStatus())               &&
              this.mToday == ((Queue) objToCompare).getToday()                      &&
              this.mTotalTime.equals(((Queue) objToCompare).getTotalTime())         &&
              this.mRemainingTime.equals(((Queue) objToCompare).getRemainingTime()) &&
              this.mWallTime.equals(((Queue) objToCompare).getWallTime())           &&
              this.mState == ((Queue) objToCompare).getState()                      &&
              this.mCurrentJob == ((Queue) objToCompare).getCurrentJob()            &&
              this.mJobs.addAll(((Queue) objToCompare).getJobs())                   &&
              this.mJobsToRun.addAll(((Queue) objToCompare).getJobsToRun())         &&
              super.isEqual(objToCompare));
   }
}

