/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxdatamodel;

import edu.nrao.difx.difxdatabase.DBConnection;
import edu.nrao.difx.difxdatamodel.DiFXSystemStatus.JobStates;
import edu.nrao.difx.xmllib.difxmessage.*;


import edu.nrao.difx.xmllib.difxmessage.DifxStatus.Weight;
import java.awt.Toolkit;
import java.io.*;
import java.math.BigDecimal;
import java.sql.ResultSet;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author mguerra
 */
public class DiFXDataModel {

   private DiFXSystemStatus    mSystemStatus   = new DiFXSystemStatus();

   private List<Module>        mModules        = new ArrayList<Module>();
   private List<Mark5Unit>     mMark5Units     = new ArrayList<Mark5Unit>(24);
   private List<ProcessorNode> mProcessorNodes = new ArrayList<ProcessorNode>(10);

   private List<Job>           mJobs           = new ArrayList<Job>();
   private List<Project>       mProjects       = new ArrayList<Project>();
   private List<JobProject>    mJobsProjects   = new ArrayList<JobProject>();

   private Queue               mQueue          = new Queue(this);

   private DBConnection        mDBConnection   = null;

   private List<Object>        mListeners      = Collections.synchronizedList(new ArrayList<Object>());

   // alert messages
   private ArrayList<String>   mAlerts         = new ArrayList<String>();
   private ArrayList<String>   mErrors         = new ArrayList<String>();

   // private number of records read from the database
   private int mRecCount = 0;

   public DiFXDataModel() 
   {
      // nothing to do
   }

   public void setDBConnection()
   {
      mDBConnection   = new edu.nrao.difx.difxdatabase.DBConnection( DOISystemConfig.DB_URL, DOISystemConfig.ORACLE_JDBC_DRIVER,
                                                                     DOISystemConfig.DB_SID, DOISystemConfig.DB_PWD );
   }

    /**
     * Returns the current number of queue records
     * @return
     */
    public int getRecCount()
    {
         return mRecCount;
    }
	 
   /**
    * Returns the list of alerts
    * @return
    */
   public ArrayList<String> getAlerts()
   {
      return this.mAlerts;
   }

   /**
    * Appends an alert to the alert list
    * @param alert the alert message to add
    */
   public void addAlert(String alert)
   {
      mAlerts.add(alert);
   }

   /**
    * Clears the alert list
    */
   public void clearAlerts()
   {
      mAlerts.clear();
   }

   /**
    * Returns the list of errors
    * @return
    */
   public ArrayList<String> getErrors()
   {
      return this.mErrors;
   }

   /**
    * Appends an error to the error list
    * @param error the error message
    */
   public void addError(String error)
   {
      mErrors.add(error);
   }

   /**
    * Clears the error list.
    */
   public void clearErrors()
   {
      mErrors.clear();
   }

   // Module get/set methods
   public List<Module> getModules()
   {
      return this.mModules;
   }

   public void addModule(Module module)
   {
      mModules.add(module);
   }

   public synchronized Module getModule(String modName)
   {
      Iterator it = mModules.iterator();
      while (it.hasNext() == true)
      {
         Module element = (Module) it.next();
         if ( element.getObjName().equalsIgnoreCase(modName) )
         {
            // found, return module
            return element;
         }
         element = null;
      }

      // not found
      return null;
   }

   /**
    * Returns the list of mark5 units
    * @return
    */
   public List<Mark5Unit> getMark5Units()
   {
      return this.mMark5Units;
   }

   /**
    * Appends a mark5 unit to the list of mark5 units
    * @param m5Unit
    */
   public void addMark5Unit(Mark5Unit m5Unit)
   {
      mMark5Units.add(m5Unit);
   }

   /**
    * Returns a Mark5Unit object based on its name.
    * @param m5Name the name of the mark5 unit to return
    * @return the Mark5Unit object
    * @return null in case no matching mark5  units can be found
    */
   public synchronized Mark5Unit getMark5Unit(String m5Name)
   {
      Iterator it = mMark5Units.iterator();
      while (it.hasNext() == true)
      {
         Mark5Unit element = (Mark5Unit) it.next();
         if (element.getObjName().equalsIgnoreCase(m5Name))
         {
            // found, return job
            return element;
         }
         element = null;
      }

      // not found
      return null;
   }

   /**
    * Returns the mark5 unit that contains the module with the given vsn
    * @param vsn the module vsn to search for
    * @return the Mark5Unit object that corresponds to the found unit
    * @return null in case no matching mark5 unit can be found
    */
   public synchronized Mark5Unit getMark5UnitViaVSN(String vsn)
   {
      Iterator it = mMark5Units.iterator();
      while (it.hasNext() == true)
      {
         Mark5Unit element = (Mark5Unit) it.next();
         if ( element.getBankAVSN() != null &&
              element.getBankBVSN() != null )
         {
             if ( element.getBankAVSN().equalsIgnoreCase(vsn) ||
                  element.getBankBVSN().equalsIgnoreCase(vsn) )
             {
                // found, return job
                return element;
             }
         }
         element = null;
      }

      // not found
      return null;
   }

   /**
    * Returns the list of processinf nodes
    * @return
    */
   public List<ProcessorNode> getProcessorNodes()
   {
      return this.mProcessorNodes;
   }

   /**
    * Appends a processing node to the current list of processing nodes
    * @param procNode
    */
   public void addProcessorNode(ProcessorNode procNode)
   {
      mProcessorNodes.add(procNode);
   }

   /**
    * Returns a ProcessorNode object based on its node node
    * @param procNodeName the name of the processing node
    * @return the ProcessorNode object that matches the given node name
    * @return null in case no matching processing node can be found
    */
   public synchronized ProcessorNode getProcessorNode(String procNodeName)
   {
      Iterator it = mProcessorNodes.iterator();
      while (it.hasNext() == true)
      {
         ProcessorNode element = (ProcessorNode)it.next();
         if ( element.getObjName().equalsIgnoreCase(procNodeName) )
         {
            // found, return job
            return element;
         }
         element = null;
      }

      // not found
      return null;
   }

   /**
    * Returns the list containing the current jobs
    * @return the List containing the jobs
    */
   public List<Job> getJobs()
   {
      return this.mJobs;
   }

   /**
    * Sets the job list
    * @param newJobs the List holding the jobs
    */
   public void setJobs(List<Job> newJobs)
   {
      this.mJobs = newJobs;
   }

   /**
    * Appends a job to the current jobn List
    * @param mJob
    */
   public void addJob(Job mJob)
   {
      mJobs.add(mJob);
   }

   /**
    * Returns the jobs that matches the given job name
    * @param jobName the job name to search for
    * @return the Job object that matches the job name
    * @return null if no matching job can be found
    */
   public synchronized Job getJob(String jobName)
   {
      Iterator it = mJobs.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( element.getObjName().equalsIgnoreCase(jobName) )
         {
            // found, return job
            return element;
         }
         element = null;
      }

      // not found
      return null;
   }

   public synchronized Job getJob(String jobName, String jobPath)
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
         element = null;
      }

      // not found
      return null;
   }

   public synchronized List<Job> getJobs(String projName, String projPath)
   {
      List<Job> jobsList = new ArrayList<Job>();

      Iterator it = mJobs.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( element.getProjectName().equalsIgnoreCase(projName) &&
              element.getProjectPath().equalsIgnoreCase(projPath) )
         {
            // found, return job
            jobsList.add(element);
         }
      }

      // return job list
      return jobsList;
   }

   /**
    * Returns all jobs that belong to the given project name
    * @param projName the name of the project to seach for
    * @return the List containing the Job obects matching the prject name
    */
   public synchronized List<Job> getJobs(String projName)
   {
      List<Job> jobsList = new ArrayList<Job>();
         
      Iterator it = mJobs.iterator();
      while (it.hasNext() == true)
      {
         Job element = (Job) it.next();
         if ( element.getProjectName().equalsIgnoreCase(projName) )
         {
            // found, return job
            jobsList.add(element);
         }
      }

      // return job list
      return jobsList;
   }

   /**
    * Returns the list of projects
    * @return the List holding all current Project objects
    */
   public List<Project> getProjects()
   {
      return this.mProjects;
   }

   public void setProjects(List<Project> newProjects)
   {
      this.mProjects = newProjects;
   }

   public void addProject(Project project)
   {
      mProjects.add(project);
   }

   public synchronized Project getProject(String projectName)
   {
      Iterator it = mProjects.iterator();
      while (it.hasNext() == true)
      {
         Project element = (Project) it.next();
         if (element.getObjName().equalsIgnoreCase(projectName))
         {
            // found, return project
            return element;
         }
      }

      // not found
      return null;
   }
   
   public List<JobProject> getJobsProjects() // remove, no need?
   {
      return this.mJobsProjects;
   }

   public void setJobsProjects(List<JobProject> newJobsProjects) // remove, no need?
   {
      this.mJobsProjects = newJobsProjects;
   }

   // Queue get/set methods
   public Queue getQueue()
   {
      return this.mQueue;
   }

   public void setQueue(Queue newQueue)
   {
      this.mQueue = newQueue;
   }


   public void setSystemStatus(DiFXSystemStatus sysStatus)
   {
      mSystemStatus = sysStatus;
   }

   // Queue get/set methods
   public DBConnection getDBConnection()
   {
      return this.mDBConnection;
   }

   public void setDBConnection(DBConnection newDBConnection)
   {
      this.mDBConnection = newDBConnection;
   }

   // Listener attach/detach methods
   public void attachListener(MessageListener l)
   {
      mListeners.add(l);
   }

   public void detachListener(MessageListener l)
   {
      mListeners.remove(l);
   }

   // Process the DifxObjects into the Data Model
   //    This method is always called via the Views
   public synchronized void serviceDataModel(DiFXObject difxObj)
   {
      // -- just pass it through
      updateDataModel(difxObj);

      // leave the notify/update up to thier view...View's are updated in separate thread.
      // NotifyListeners();
   }

   // Process the DifxMessage into the Data Model
   //    This method is always called via the DiFX Controller
   public synchronized void serviceDataModel(DifxMessage difxMsg)
   {
      // -- Convert a DifxMessage into a DiFXObject

      // Determine the type of message
      Header header = difxMsg.getHeader();
      if (header.getType().equalsIgnoreCase("DifxStatusMessage"))
      {
         processDifxStatusMessage(difxMsg);
      }
      else if (header.getType().equalsIgnoreCase("Mark5StatusMessage"))
      {
         processMark5StatusMessage(difxMsg);

         // Determine state of all queued jobs
         // determineStateOfAllJobs();
      }
      else if (header.getType().equalsIgnoreCase("DifxLoadMessage"))
      {
         processDifxLoadMessage(difxMsg);
      }
      else if (header.getType().equalsIgnoreCase("DifxAlertMessage"))
      {
         processDifxAlertMessage(difxMsg);
      }
      else if (header.getType().equalsIgnoreCase("DOIMessage"))
      {
         processDOIMessage(difxMsg);
      }
      else if (header.getType().equalsIgnoreCase("DoiErrorMessage"))
      {
         processDOIMessage(difxMsg);
      }
      
      // Notify listeners to update thier view...Move into UpdateViewThread
      // NotifyListeners();

      // clean up
      header = null;
   }

   private synchronized void processDifxStatusMessageOld(DifxMessage difxMsg)
   {
      // -- catch some exceptions and keep the program from terminating. . .
      // Message must originate from mpifxcorr or swc000
      int len = (difxMsg.getHeader().getFrom().trim().length());
      if ( (difxMsg.getHeader().getFrom().substring(0, len).equalsIgnoreCase("mpifxcorr")) ||
           (difxMsg.getHeader().getFrom().substring(0, len).equalsIgnoreCase("swc000"   )) )
      {

         // Update the jobs status, no need to call UpdateDataModel()
         Job job = getJob(difxMsg.getHeader().getIdentifier());
         String seqNum = difxMsg.getBody().getSeqNumber();

         if (job != null)
         {
            // set the current job
            getQueue().setCurrentJob(job);

            // Get current/previous job state, save it for an error comparision
            DiFXSystemStatus.JobStates previousState = job.getState();

            // Current job is running - set the job running alert once only
            if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Running") &&
                 (previousState == DiFXSystemStatus.JobStates.READY) )
            {
               // set the current wall time
               job.setStartWallTimeUTC( System.currentTimeMillis() );

               String stateString   = difxMsg.getBody().getDifxStatus().getState();
               Calendar cal         = Calendar.getInstance();
               SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
               String timeStamp     = sdf.format(cal.getTime());

               String strToAdd = timeStamp + " " +
                                 difxMsg.getHeader().getFrom()       + " : " +
                                 difxMsg.getHeader().getIdentifier() + " : " +
                                 stateString                         + " : " +
                                 difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

               this.addAlert(strToAdd);

            } // -- if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Running") &&
              // --      (previousState == DiFXSystemStatus.JobStates.READY) )

            // For jobs, convert difxmessage into status, run state may not change
            String newStatus = (difxMsg.getBody().getDifxStatus()).getState();
            job.setStatus(DiFXSystemStatus.ConvertDiFXStatusIntoQueueJobStatus( newStatus ));
            //System.out.printf("***************** Job(%s) currstatus(%s) prevstatestr(%s) prevstate(%s) seqNum(%s). \n",
            //                  job.getObjName(), job.getStatus(), job.getStateString(), previousState, seqNum);

            if ( newStatus.equalsIgnoreCase("Done") )
            {
               // Free the jobs resources
               //job.setStarted(false);
System.out.println("***************** Job free resources.");
               job.freeResources();
System.out.println("***************** Job free resources complete.");

               // stop correlation complete (100%), change state
               job.setCompletion(100.00f);

               // update job states so we can run another job
System.out.printf("DifXDataModel determineStateOfAllJobs().");
               determineStateOfAllJobs();
System.out.printf("DifXDataModel determineStateOfAllJobs() complete.");

            } // Current job running, determine lost resources
            else if ( newStatus.equalsIgnoreCase("Running") )
            {
               job.setAllResponding( job.isAllResponding() );

            } // -- if ( newStatus.equalsIgnoreCase("MpiDone") )
            else if ( newStatus.equalsIgnoreCase("MpiDone") )
            {
System.out.println("DifXDataModel job free resources.");
               job.freeResources();
System.out.println("DifXDataModel job free resources complete.");

               if ( previousState == DiFXSystemStatus.JobStates.RUNNING )
               {
                  job.setState(DiFXSystemStatus.JobStates.UNKNOWN);
                  getQueue().setCurrentJob(job);
               }
               else if ( previousState != DiFXSystemStatus.JobStates.DONE )
               {
                  job.setState(DiFXSystemStatus.JobStates.UNKNOWN);
                  getQueue().setCurrentJob(job);
               }
               else if ( previousState == DiFXSystemStatus.JobStates.KILLED )
               {
                  job.setState(DiFXSystemStatus.JobStates.KILLED);
               }

               // Time stamp completion
               job.setCorrelationStopUTC();

            } // -- if ( newStatus.equalsIgnoreCase("Done") )

            // If not in an error condition, continue processing jobs
            if ( (previousState   != DiFXSystemStatus.JobStates.KILLED) &&
                 (job.isUnknown() != true) )
            {
               job.setState(DiFXSystemStatus.ConvertJobStatus( job.getStatus() ));
               if (job.getState() == JobStates.COMPLETE)
               {
                  try
                  {
                     // Reset and post complete records to database
                     postUpdatesToDatabase(job); // MAG
                     job.setStarted(false);
                  }
                  catch (Exception ex)
                  {
                     Logger.getLogger(DiFXDataModel.class.getName()).log(Level.SEVERE, null, ex);
                  }

               } // -- if (job.getState() == JobStates.COMPLETE)

               job.setMessage(difxMsg.getBody().getDifxStatus().getMessage());
               job.setVisibilityMJD(new BigDecimal(difxMsg.getBody().getDifxStatus().getVisibilityMJD().trim()));
               job.setStatusTimeStampUTC();

               float completion = job.calculatePercentComplete();
               if ( (completion >= 0.000f) && (completion <= 100.000f) )
               {
                  job.setCompletion(completion);
               }

               // Assign min weight to antenna
               if ( difxMsg.getBody().getDifxStatus().getWeight() != null )
               {
                  if ( difxMsg.getBody().getDifxStatus().getWeight().isEmpty() == false )
                  {
                     // process all the <antenna, weight> tuples
                     Iterator<Weight> wit = difxMsg.getBody().getDifxStatus().getWeight().iterator();
                     while (wit.hasNext() == true)
                     {
                        Weight weight = wit.next();

                        // test weight
                        if ( (weight.getAnt() != null) &&
                             (weight.getWt()  != null) )
                        {
                           // zero-based
                           Module mod = job.getModule(Integer.parseInt(weight.getAnt().trim()));
                           if (mod != null)
                           {
                              mod.setWeight(Float.parseFloat(weight.getWt()));
                              mod.setStatusTimeStampUTC();
                           }
                           mod = null;
                        }
                        else // -- invalid
                        {
                           System.out.printf("***************** Data model DiFX Status invalid weight. \n");
                        }

                     } // -- while (wit.hasNext() == true)

                  } // -- if ( difxMsg.getBody().getDifxStatus().getWeight().isEmpty() == false )

               } // -- if ( difxMsg.getBody().getDifxStatus().getWeight() != null )

            } // -- if ( previousState != DiFXSystemStatus.JobStates.KILLED )

         } // -- if (job != null)

         // Lets update the queue status, and maybe start another job
         Queue queue = getQueue();
         if ( queue != null )
         {
            // Time stamp the status
            queue.setStatusTimeStampUTC();

            // Current job in error
            if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Aborting"   ) ||
                 difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Terminating") ||
                 difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Terminated" ) ||
                 difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Crashed"    ) )
            {
               String stateString   = difxMsg.getBody().getDifxStatus().getState();
               Calendar cal         = Calendar.getInstance();
               SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
               String timeStamp     = sdf.format(cal.getTime());

               String strToAdd = timeStamp   + " "   +
                                 difxMsg.getHeader().getFrom()       + " : " +
                                 difxMsg.getHeader().getIdentifier() + " : " +
                                 stateString                         + " : " +
                                 difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

               this.addAlert(strToAdd);

               // Error occured, stop, change state. . .But do not get next job
               queue.stopError();

            } // Current job spawning - just print message, do not print every run message
            else if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Spawning") )
            {
               String stateString   = difxMsg.getBody().getDifxStatus().getState();
               Calendar cal         = Calendar.getInstance();
               SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
               String timeStamp     = sdf.format(cal.getTime());

               String strToAdd = timeStamp   + " "   +
                                 difxMsg.getHeader().getFrom()       + " : " +
                                 difxMsg.getHeader().getIdentifier() + " : " +
                                 stateString                         + " : " +
                                 difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

               this.addAlert(strToAdd);

            } // Current job starting - just print message, do not print every run message
            else if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Starting") )
            {
               String stateString   = difxMsg.getBody().getDifxStatus().getState();
               Calendar cal         = Calendar.getInstance();
               SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
               String timeStamp     = sdf.format(cal.getTime());

               String strToAdd = timeStamp   + " "   +
                                 difxMsg.getHeader().getFrom()       + " : " +
                                 difxMsg.getHeader().getIdentifier() + " : " +
                                 stateString                         + " : " +
                                 difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

               this.addAlert(strToAdd);

            }  // Current job done, change state
            else if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Done") )
            {
               String stateString   = difxMsg.getBody().getDifxStatus().getState();
               Calendar cal         = Calendar.getInstance();
               SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
               String timeStamp     = sdf.format(cal.getTime());

               String strToAdd = timeStamp   + " "   +
                                 difxMsg.getHeader().getFrom()       + " : " +
                                 difxMsg.getHeader().getIdentifier() + " : " +
                                 stateString                         + " : " +
                                 difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

               this.addAlert(strToAdd);

               // -- verify no jobs to run before changing that state . . .
               //if (queue.allJobsDoneComplete())
               //if (queue.anyJobsToRun() == false)
               //{
               // Verify no jobs to run before changing that state, if no jobs
               // stop queue and correlation complete (100%)
               queue.stopDone();
               //}

            }// Mpi Done, change state, run next job
            else if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("MpiDone") )
            {
               String stateString   = difxMsg.getBody().getDifxStatus().getState();
               Calendar cal         = Calendar.getInstance();
               SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
               String timeStamp     = sdf.format(cal.getTime());

               String strToAdd = timeStamp   + " "   +
                                 difxMsg.getHeader().getFrom()       + " : " +
                                 difxMsg.getHeader().getIdentifier() + " : " +
                                 stateString                         + " : " +
                                 difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

               this.addAlert(strToAdd);

               // -- verify that all jobs are done before changing that state . . .
               //if (queue.allJobsFinished())
               //{
                  // stop mpi processes ended, change state, run next
                  queue.stopMPIDone();
               //}
            }

         } // -- if (queue != null)

         // clean up
         job   = null;
         queue = null;

      } // -- if ( (difxMsg.getHeader().getFrom().substring(0, len).equalsIgnoreCase("mpifxcorr")) ||
        // --      (difxMsg.getHeader().getFrom().substring(0, len).equalsIgnoreCase("swc000"   )) )

   }

   private synchronized void processDifxStatusMessage(DifxMessage difxMsg)
   {
      // -- catch some exceptions and keep the program from terminating. . .
      // Message must originate from mpifxcorr or swc000
      int len = (difxMsg.getHeader().getFrom().trim().length());
      if ( (difxMsg.getHeader().getFrom().substring(0, len).equalsIgnoreCase("mpifxcorr")) ||
           (difxMsg.getHeader().getFrom().substring(0, len).equalsIgnoreCase("swc000"   )) )
      {

         // -- Update the jobs status, no need to call UpdateDataModel()
         Job job = getJob(difxMsg.getHeader().getIdentifier());
         if (job != null)
         {
            // Get current/previous job state, save it for an error comparision
            DiFXSystemStatus.JobStates previousState = job.getState();

            // For jobs, convert difxmessage into status, run state may not change
            String seqNum    = difxMsg.getBody().getSeqNumber();
            String newStatus = (difxMsg.getBody().getDifxStatus()).getState();
            job.setStatus(DiFXSystemStatus.ConvertDiFXStatusIntoQueueJobStatus( newStatus ));
            //System.out.printf("***************** Job(%s) currstatus(%s) prevstatestr(%s) prevstate(%s) seqNum(%s). \n",
            //                  job.getObjName(), job.getStatus(), job.getStateString(), previousState, seqNum);
            job.setState(DiFXSystemStatus.ConvertJobStatus( job.getStatus() ));

            // set message text and time stamp the message
            job.setMessage(difxMsg.getBody().getDifxStatus().getMessage());
            job.setVisibilityMJD(new BigDecimal(difxMsg.getBody().getDifxStatus().getVisibilityMJD().trim()));
            job.setStatusTimeStampUTC();

            // Calculate and set completion
            float completion = job.calculatePercentComplete();
            if ( (completion >= 0.000f) && (completion <= 100.000f) )
            {
               job.setCompletion(completion);
            }

            // Determine run states
            if ( newStatus.equalsIgnoreCase("Running") )
            {
               // Set the job running alert once only
               if ( previousState == DiFXSystemStatus.JobStates.READY )
               {
                  // set the current wall time
                  job.setStartWallTimeUTC( System.currentTimeMillis() );

                  // create initial running message
                  Calendar cal         = Calendar.getInstance();
                  SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
                  String timeStamp     = sdf.format(cal.getTime());

                  String strToAdd = timeStamp + " " +
                                    difxMsg.getHeader().getFrom()       + " : " +
                                    "doi"       + " : "  +
                                    "INFO"      + " :: " +
                                    "Current Job: "      +
                                    difxMsg.getHeader().getIdentifier() + " :: "  +
                                    newStatus   + " ::: " +
                                    difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

                  this.addAlert(strToAdd);

               } // -- if ( previousState == DiFXSystemStatus.JobStates.READY )

               // flag all resources are responding
               job.setAllResponding( job.isAllResponding() );

            } // -- if ( newStatus.equalsIgnoreCase("MpiDone") )
            else if ( newStatus.equalsIgnoreCase("Done") )
            {
               // Free the jobs resources
               //job.setStarted(false);
               job.freeResources();

               // stop correlation complete (100%), change state
               job.setCompletion(100.00f);

               // update job states so we can run another job
               determineStateOfAllJobs();

            } // Current job running, determine lost resources
            else if ( newStatus.equalsIgnoreCase("MpiDone") )
            {
               // Free the jobs resources
               //job.setStarted(false);
               job.freeResources();

               if ( previousState != DiFXSystemStatus.JobStates.DONE )
               {
                  job.setState(DiFXSystemStatus.JobStates.UNKNOWN);
               }
               else if ( previousState == DiFXSystemStatus.JobStates.KILLED )
               {
                  job.setState(DiFXSystemStatus.JobStates.KILLED);
               }

               // Time stamp completion
               job.setCorrelationStopUTC();

               // update job states so we can run another job
               determineStateOfAllJobs();

            } // -- if ( newStatus.equalsIgnoreCase("Running") )

            // Assign min weight to antenna
            if ( difxMsg.getBody().getDifxStatus().getWeight() != null )
            {
               if ( difxMsg.getBody().getDifxStatus().getWeight().isEmpty() == false )
               {
                  // process all the <antenna, weight> tuples
                  Iterator<Weight> wit = difxMsg.getBody().getDifxStatus().getWeight().iterator();
                  while (wit.hasNext() == true)
                  {
                     Weight weight = wit.next();

                     // test weight
                     if ( (weight.getAnt() != null) &&
                          (weight.getWt()  != null) )
                     {
                        // zero-based
                        Module mod = job.getModule(Integer.parseInt(weight.getAnt().trim()));
                        if (mod != null)
                        {
                           mod.setWeight(Float.parseFloat(weight.getWt()));
                           mod.setStatusTimeStampUTC();
                        }
                        mod = null;
                     }
                     else // -- invalid
                     {
                        System.out.printf("***************** Data model DiFX Status invalid weight. \n");
                     }

                  } // -- while (wit.hasNext() == true)

               } // -- if ( difxMsg.getBody().getDifxStatus().getWeight().isEmpty() == false )

            } // -- if ( difxMsg.getBody().getDifxStatus().getWeight() != null )

            // Update the database
            if ( (job.isComplete() == true) || (job.isKilled() == true) || 
                 (job.isUnknown() == true ) || (job.isFailed() == true) )
            {
               try
               {
                  // job was started locally, update the database
                  if ( job.isStarted() )
                  {
                     // update DB and spit out alert message
                     postUpdatesToDatabase(job);
                     String stateString   = difxMsg.getBody().getDifxStatus().getState();
                     Calendar cal         = Calendar.getInstance();
                     SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
                     String timeStamp     = sdf.format(cal.getTime());

                     String strToAdd = timeStamp   + " "   +
                                       difxMsg.getHeader().getFrom()       + " : " +
                                       "doi"       + " : "  +
                                       "INFO"      + " :: " +
                                       "Current Job: "      +
                                       difxMsg.getHeader().getIdentifier() + " :: "  +
                                       "Database insert complete"          + " ::: " + "\n";

                     this.addAlert(strToAdd);

                  }
                  job.setStarted(false);
               }
               catch (Exception ex)
               {
                  Logger.getLogger(DiFXDataModel.class.getName()).log(Level.SEVERE, null, ex);
               }

            } // -- if ( previousState != DiFXSystemStatus.JobStates.KILLED )

            // Lets update the queue status, and maybe start another job
            Queue queue = getQueue();
            if ( queue != null )
            {
               // set the current job
               getQueue().setCurrentJob(job);

               // Time stamp the status
               queue.setStatusTimeStampUTC();

               // Current job in error
               if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Aborting"   ) ||
                    difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Terminating") ||
                    difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Terminated" ) ||
                    difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Crashed"    ) )
               {
                  String stateString   = difxMsg.getBody().getDifxStatus().getState();
                  Calendar cal         = Calendar.getInstance();
                  SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
                  String timeStamp     = sdf.format(cal.getTime());

                  String strToAdd = timeStamp   + " "   +
                                    difxMsg.getHeader().getFrom()       + " : " +
                                    "doi"       + " : "  +
                                    "INFO"      + " :: " +
                                    "Current Job: "      +
                                    difxMsg.getHeader().getIdentifier() + " :: "  +
                                    stateString                         + " ::: " +
                                    difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

                  this.addAlert(strToAdd);

                  // Error occured, stop, change state. . .But do not get next job
                  queue.stopError();

               } // Current job spawning - just print message, do not print every run message
               else if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Spawning") )
               {
                  String stateString   = difxMsg.getBody().getDifxStatus().getState();
                  Calendar cal         = Calendar.getInstance();
                  SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
                  String timeStamp     = sdf.format(cal.getTime());

                  String strToAdd = timeStamp   + " "   +
                                    difxMsg.getHeader().getFrom()       + " : " +
                                    "doi"       + " : "  +
                                    "INFO"      + " :: " +
                                    "Current Job: "      +
                                    difxMsg.getHeader().getIdentifier() + " :: "  +
                                    stateString                         + " ::: " +
                                    difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

                  this.addAlert(strToAdd);

               } // Current job starting - just print message, do not print every run message
               else if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Starting") )
               {
                  String stateString   = difxMsg.getBody().getDifxStatus().getState();
                  Calendar cal         = Calendar.getInstance();
                  SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
                  String timeStamp     = sdf.format(cal.getTime());

                  String strToAdd = timeStamp   + " "   +
                                    difxMsg.getHeader().getFrom()       + " : " +
                                    "doi"       + " : "  +
                                    "INFO"      + " :: " +
                                    "Current Job: "      +
                                    difxMsg.getHeader().getIdentifier() + " :: "  +
                                    stateString                         + " ::: " +
                                    difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

                  this.addAlert(strToAdd);

               }  // Current job done, change state
               else if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("Done") )
               {
                  String stateString   = difxMsg.getBody().getDifxStatus().getState();
                  Calendar cal         = Calendar.getInstance();
                  SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
                  String timeStamp     = sdf.format(cal.getTime());

                  String strToAdd = timeStamp   + " "   +
                                    difxMsg.getHeader().getFrom()       + " : " +
                                    "doi"       + " : "  +
                                    "INFO"      + " :: " +
                                    "Current Job: "      +
                                    difxMsg.getHeader().getIdentifier() + " :: " +
                                    stateString                         + " ::: " +
                                    difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

                  this.addAlert(strToAdd);

                  // -- verify no jobs to run before changing that state . . .
                  //if (queue.allJobsDoneComplete())
                  //if (queue.anyJobsToRun() == false)
                  //{
                  // Verify no jobs to run before changing that state, if no jobs
                  // stop queue and correlation complete (100%)
                  queue.stopDone();
                  //}

               }// Mpi Done, change state, run next job
               else if ( difxMsg.getBody().getDifxStatus().getState().equalsIgnoreCase("MpiDone") )
               {
                  String stateString   = difxMsg.getBody().getDifxStatus().getState();
                  Calendar cal         = Calendar.getInstance();
                  SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
                  String timeStamp     = sdf.format(cal.getTime());

                  String strToAdd = timeStamp   + " "   +
                                    difxMsg.getHeader().getFrom()       + " : " +
                                    "doi"       + " : "  +
                                    "INFO"      + " :: " +
                                    "Current Job: "      +
                                    difxMsg.getHeader().getIdentifier() + " :: "  +
                                    stateString                         + " ::: " +
                                    difxMsg.getBody().getDifxStatus().getMessage().toString() + "\n";

                  this.addAlert(strToAdd);

                  // -- verify that all jobs are done before changing that state . . .
                  //if (queue.allJobsFinished())
                  //{
                     // stop mpi processes ended, change state, run next
                     queue.stopMPIDone();
                  //}
               }

            } // -- if (queue != null)

            // clean up
            queue = null;

         } // -- if (job != null)

         // clean up
         job   = null;

      } // -- if ( (difxMsg.getHeader().getFrom().substring(0, len).equalsIgnoreCase("mpifxcorr")) ||
        // --      (difxMsg.getHeader().getFrom().substring(0, len).equalsIgnoreCase("swc000"   )) )

   }

   private synchronized void processMark5StatusMessage(DifxMessage difxMsg)
   {
      // -- catch some exceptions and keep the program from terminating. . .
      try
      {
         // Create DiFXObject from DifxMessage
         if (difxMsg.getHeader().getFrom().substring(0, 3).equalsIgnoreCase("doi"))
         {
            // create alert text
					  String lostString = "";
						if (difxMsg.getBody().getMark5Status().getState().equalsIgnoreCase("lost"))
						{
							lostString = " connection and not responding.";
              Toolkit.getDefaultToolkit().beep();
							Thread.sleep(250);
              Toolkit.getDefaultToolkit().beep();
						}
            String alertMessage =  difxMsg.getHeader().getIdentifier() + " " +
                                   difxMsg.getBody().getMark5Status().getState() +
																	 lostString;

            DifxMessage difxAlertMsg = CreateDiFXAlertMessage(alertMessage);
            this.serviceDataModel(difxAlertMsg);
         }
         else if (difxMsg.getHeader().getFrom().substring(0, 5).equalsIgnoreCase("mark5"))
         {
            // Create object to update the data model
            Mark5Unit mark5 = new Mark5Unit();
            mark5.setObjType("mark5");
            mark5.setObjName(difxMsg.getHeader().getFrom());
            mark5.setMsgSrcId(difxMsg.getHeader().getIdentifier());  // job id or mark5Daemon

            Mark5Unit existingM5 = getMark5Unit(difxMsg.getHeader().getFrom());
            if (existingM5 != null)
            {
               mark5.updateObject(existingM5);
            }

            // Update the rest of the fields with difx message
            if (mark5.getState().equalsIgnoreCase(difxMsg.getBody().getMark5Status().getState()) == true)
            {
               mark5.setStateChanged(false);
            }
            else
            {
               mark5.setStateChanged(true);
            }
            mark5.setState(difxMsg.getBody().getMark5Status().getState());
            mark5.setBankAVSN(difxMsg.getBody().getMark5Status().getBankAVSN());
            mark5.setBankBVSN(difxMsg.getBody().getMark5Status().getBankBVSN());
            mark5.setStatusWord(difxMsg.getBody().getMark5Status().getStatusWord());
            mark5.setActiveBank(difxMsg.getBody().getMark5Status().getActiveBank());
            mark5.setScanNumber(difxMsg.getBody().getMark5Status().getScanNumber());
            mark5.setScanName(difxMsg.getBody().getMark5Status().getScanName());
            mark5.setPosition(difxMsg.getBody().getMark5Status().getPosition());
            mark5.setPlayRate(difxMsg.getBody().getMark5Status().getPlayRate());
            mark5.setDataMJD(new BigDecimal(difxMsg.getBody().getMark5Status().getDataMJD().trim()));
            mark5.setCurrentJob(difxMsg.getHeader().getIdentifier());
            mark5.setStatusTimeStampUTC(); // current wall time UTC

            // Now update the actual data structures contained in the data model
            updateDataModel(mark5);

            // clean up
            existingM5 = null;
            mark5      = null;
         }
      }
      catch (Exception e)
      {
         System.err.println("uncaught exception: " + e);
      }
   }
   
   private synchronized void processDifxLoadMessage(DifxMessage difxMsg)
   {
      // -- catch some exceptions and keep the program from terminating. . .
      try
      {
         // Create DiFXObject from DifxMessage
         if (difxMsg.getHeader().getFrom().substring(0, 5).equalsIgnoreCase("mark5"))
         {
            // Create object to update the data model
            Mark5Unit mark5 = new Mark5Unit();
            mark5.setObjType("mark5");
            mark5.setObjName(difxMsg.getHeader().getFrom());
            mark5.setMsgSrcId(difxMsg.getHeader().getIdentifier());

            // -- Copy the existing fields not updated via this message
            Mark5Unit existingMark5 = getMark5Unit(difxMsg.getHeader().getFrom());
            if (existingMark5 != null)
            {
               mark5.updateObject(existingMark5);
            }

            // Update the rest of the fields with difx message
            mark5.setCpuLoad(difxMsg.getBody().getDifxLoad().getCpuLoad());
            mark5.setTotalMem(difxMsg.getBody().getDifxLoad().getTotalMemory());
            mark5.setUsedMem(difxMsg.getBody().getDifxLoad().getUsedMemory());
            mark5.setMemLoad((float) difxMsg.getBody().getDifxLoad().getUsedMemory() /
                             difxMsg.getBody().getDifxLoad().getTotalMemory());
            mark5.setNetRxRate(difxMsg.getBody().getDifxLoad().getNetRXRate());
            mark5.setNetTxRate(difxMsg.getBody().getDifxLoad().getNetTXRate());
            //mark5.setStatusTimeStampUTC();

            // Now update the actual data structures contained in the data model
            updateDataModel(mark5);

            // clean up
            existingMark5 = null;
            mark5         = null;
         }
         else if (difxMsg.getHeader().getFrom().substring(0, 3).equalsIgnoreCase("swc"))
         {
            // Create object to update the data model
            ProcessorNode proc = new ProcessorNode();
            proc.setObjType("processor");
            proc.setObjName(difxMsg.getHeader().getFrom());
            proc.setMsgSrcId(difxMsg.getHeader().getIdentifier());

            // -- Copy the existing fields not updated via this message
            ProcessorNode existingProc = getProcessorNode(difxMsg.getHeader().getFrom());
            if (existingProc != null)
            {
               proc.updateObject(existingProc);
            }

            // Update the rest of the fields with difx message
            proc.setState("Online"); // assume processor is    
            proc.setCpuLoad(difxMsg.getBody().getDifxLoad().getCpuLoad());
            proc.setTotalMem(difxMsg.getBody().getDifxLoad().getTotalMemory());
            proc.setUsedMem(difxMsg.getBody().getDifxLoad().getUsedMemory());
            proc.setMemLoad((float) difxMsg.getBody().getDifxLoad().getUsedMemory() /
                            difxMsg.getBody().getDifxLoad().getTotalMemory());
            proc.setNetRxRate(difxMsg.getBody().getDifxLoad().getNetRXRate());
            proc.setNetTxRate(difxMsg.getBody().getDifxLoad().getNetTXRate());
            proc.setStatusTimeStampUTC();

            // Now update the actual data structures contained in the data model
            updateDataModel(proc);

            // clean up
            existingProc = null;
            proc         = null;
         }
      }
      catch (Exception e)
      {
         System.err.println("uncaught exception: " + e);
      }
   }

   private synchronized void processDifxAlertMessage(DifxMessage difxMsg)
   {
      // -- catch some exceptions and keep the program from terminating. . .
      try
      {
         // Just store the alert message, no need to create an object and call updateDataModel()
         if ( (difxMsg.getBody().getDifxAlert().getSeverity() >= 0) &&
              (difxMsg.getBody().getDifxAlert().getSeverity() <= 4) )
         {
            String alertString   = DiFXSystemStatus.ConvertDiFXAlertIntoString(
                                     DiFXSystemStatus.DiFXAlerts.convert(difxMsg.getBody().getDifxAlert().getSeverity()) );
            Calendar cal         = Calendar.getInstance();
            SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
            String timeStamp     = sdf.format(cal.getTime());

            String strToAdd = timeStamp   + " "   +
                              difxMsg.getHeader().getFrom()       + " : " +
                              difxMsg.getHeader().getIdentifier() + " : " +
                              alertString                         + " : " +
                              difxMsg.getBody().getDifxAlert().getAlertMessage().toString() + "\n";

            // add check for high water mark, and clear text area if exceeded. . .
            this.addAlert(strToAdd);
         }
      }
      catch (Exception e)
      {
         System.err.println("uncaught exception: " + e);
      }
   }

   private synchronized void processDOIMessage(DifxMessage difxMsg)
   {
      Body body = difxMsg.getBody();
      if (body.getDoiAntenna() != null)
      {
         processDOIAntennaMessage(difxMsg);
      }
      else if (body.getDoiModule() != null)
      {
         processDOIModuleMessage(difxMsg);
      }
      else if (body.getDoiResourceConfig() != null)
      {
         processDOIResourceConfigMessage(difxMsg);
      }
      else if (body.getDoiProject() != null)
      {
         processDOIProjectMessage(difxMsg);
      }
      else if (body.getDoiJob() != null)
      {
         processDOIJobMessage(difxMsg);
      }
      else if (body.getDoiJobProject() != null)
      {
         processDOIJobProjectMessage(difxMsg);
      }
      else if (body.getDoiJobCommand() != null)
      {
         processDOIJobCommandMessage(difxMsg);
      }
      else if (body.getDoiQueueCommand() != null)
      {
         processDOIQueueCommandMessage(difxMsg);
      }
      else if (body.getDoiSystemConfig() != null)
      {
         processDOISystemConfigMessage(difxMsg);
      }
      else if (body.getDoiError() != null)
      {
         processDOIErrorMessage(difxMsg);
      }
      
      // clean up
      body = null;
   }

   private void processDOIResourceConfigMessage(DifxMessage difxMsg)
   {
      String  name     = difxMsg.getBody().getDoiResourceConfig().getName();
      Short   numCPUs  = difxMsg.getBody().getDoiResourceConfig().getNumCPUs();
      Short   numCores = difxMsg.getBody().getDoiResourceConfig().getNumCores();
      Float   bogusGHz = difxMsg.getBody().getDoiResourceConfig().getBogusGHz();
      Boolean enabled  = difxMsg.getBody().getDoiResourceConfig().isEnabled();

      Short type = difxMsg.getBody().getDoiResourceConfig().getType();
      switch (type)
      {
         case 0: // Processor Node
         {
            ProcessorNode node = new ProcessorNode();
            node.setObjType("processor");
            node.setObjName(name);
            node.setObjId(0);
            node.setNumCPUs(numCPUs);
            node.setNumCores(numCores);
            node.setBogusGHz(bogusGHz);
            node.setType(type);
            node.setTypeString("node");
            node.setEnabled(enabled);
            updateDataModel(node);
            node = null;
            break;
         }
         case 1: // Mark 5 Unit
         {
            Mark5Unit mark5 = new Mark5Unit();
            mark5.setObjType("mark5");
            mark5.setObjName(name);
            mark5.setObjId(0);
            mark5.setNumCPUs(numCPUs);
            mark5.setNumCores(numCores);
            mark5.setBogusGHz(bogusGHz);
            mark5.setType(type);
            mark5.setTypeString("mark5");
            mark5.setEnabled(enabled);
            updateDataModel(mark5);
            mark5 = null;
            break;
         }
         case 2: // Processor Manager
         {
            ProcessorNode manager = new ProcessorNode();
            manager.setObjType("processor");
            manager.setObjName(name);
            manager.setObjId(0);
            manager.setNumCPUs(numCPUs);
            manager.setNumCores(numCores);
            manager.setBogusGHz(bogusGHz);
            manager.setType(type);
            manager.setTypeString("manager");
            manager.setEnabled(enabled);
            updateDataModel(manager);
            manager = null;
            break;
         }
         default:
         {
            // -- Invalid type
            System.out.printf("******** Data model service model - invalid resource type \n");
         }
         
      } // -- switch (type)
   }

   private void processDOIAntennaMessage(DifxMessage difxMsg)
   {
      String  jobName   = difxMsg.getBody().getDoiAntenna().getJobName();
      String  antName   = difxMsg.getBody().getDoiAntenna().getAntennaName();
      int     id        = difxMsg.getBody().getDoiAntenna().getId();
      String  moduleVSN = difxMsg.getBody().getDoiAntenna().getModuleVSN();
      String  shelf     = difxMsg.getBody().getDoiAntenna().getShelf();

      // Create and populate the antenna
      //Antenna ant = new Antenna();
      //ant.setObjName(antName);
      //ant.setObjId(id);
      //ant.setObjType("antenna");
      //ant.setModuleVSN(moduleVSN);
      //ant.setShelf(shelf);

      // This is a kluge until something better....
      //
      // NOTE:
      // 2 step process.
      // 1. Associate the antenna to the job via addAntenna() method
      // 2. Insert/Update the antenna into the data model done in UpdateModel()
      //Job job = GetJob(jobName);
      //job.addAntenna(ant);

      //UpdateDataModel(ant);
   }

   private void processDOIModuleMessage(DifxMessage difxMsg)
   {
      String  jobName   = difxMsg.getBody().getDoiModule().getJobName();
      String  antName   = difxMsg.getBody().getDoiModule().getAntennaName();
      int     id        = difxMsg.getBody().getDoiModule().getId();
      String  moduleVSN = difxMsg.getBody().getDoiModule().getModuleVSN();
      String  shelf     = difxMsg.getBody().getDoiModule().getShelf();

      // Create and populate the module
      Module mod = new Module();
      mod.setObjName(antName);
      mod.setObjId(id);
      mod.setObjType("antenna");
      mod.setModuleVSN(moduleVSN);
      mod.setShelf(shelf);

      // This is a kluge until something better....
      //
      // NOTE:
      // 2 step process.
      // 1. Associate the antenna to the job via addAntenna() method
      // 2. Insert/Update the antenna into the data model done in UpdateModel()
      Job job = getJob(jobName);
      job.addModule(mod);

      updateDataModel(mod);
      mod = null;
      job = null;
   }

   private void processDOIProjectMessage(DifxMessage difxMsg)
   {
      // Add project to the datamodel
      Project proj = new Project();
      proj.setObjType("project");
      proj.setObjName(difxMsg.getBody().getDoiProject().getProjectName());
      proj.setObjId(0);
      proj.setProjectPath(difxMsg.getBody().getDoiProject().getProjectPath());
      updateDataModel(proj);
      proj = null;
   }

   private void processDOIJobMessage(DifxMessage difxMsg)
   {
      // -- Create a job, load the data and add job to datamodel

      // Create job, hand it a copy of data model for verification
      Job job = new Job(this);

      // Fill in the values from difx message
      job.setObjType("job");
      job.setObjName(difxMsg.getBody().getDoiJob().getJobName());
      job.setObjId(0);
      job.setJobPath(difxMsg.getBody().getDoiJob().getJobPath());
      job.setProjectName(difxMsg.getBody().getDoiJob().getProjectName());
      job.setProjectPath(difxMsg.getBody().getDoiJob().getProjectPath());
      job.setSegment(difxMsg.getBody().getDoiJob().getSegment());
      job.setJobPass(difxMsg.getBody().getDoiJob().getJobPass());
      job.setJobNumber(difxMsg.getBody().getDoiJob().getJobNumber());
      job.setPriority(difxMsg.getBody().getDoiJob().getPriority());
      job.setActualSpeedUp(difxMsg.getBody().getDoiJob().getActualSpeedUp());
      job.setPredictedSpeedUp(difxMsg.getBody().getDoiJob().getPredictedSpeedUp());
      job.setJobStartTimeMJD( BigDecimal.valueOf(difxMsg.getBody().getDoiJob().getJobStartTimeMJD()) );
      job.setJobStopTimeMJD( BigDecimal.valueOf(difxMsg.getBody().getDoiJob().getJobStopTimeMJD()) );
      job.setNumAntennas(difxMsg.getBody().getDoiJob().getNumAntennas());
      job.setDbJob(difxMsg.getBody().getDoiJob().isDbJob());

      // Read the job's data file
      job.readJobData();

      // Put the job into the data model
      updateDataModel(job);
      job = null;
   }

   private void processDOIJobProjectMessage(DifxMessage difxMsg)
   {
      // This is a kluge until something better....
      //
      // NOTE:
      // This is a 2 step process
      //  1. associate job and project (done here), and
      //  2. insert the association into the jobs queue map (done in UpdateDataModel)... Bad comments
      JobProject jobProject = new JobProject();
      jobProject.setObjType("jobProject");
      jobProject.setObjId(0);
      jobProject.setObjName(difxMsg.getBody().getDoiJobProject().getProjectName() + "+" +
         difxMsg.getBody().getDoiJobProject().getJobName());
      jobProject.SetJobName(difxMsg.getBody().getDoiJobProject().getJobName());
      jobProject.SetProject(difxMsg.getBody().getDoiJobProject().getProjectName());
      jobProject.SetOption(difxMsg.getBody().getDoiJobProject().getOption());
      jobProject.setStatusTimeStampUTC();
      updateDataModel(jobProject);
      jobProject = null;
   }

   private void processDOIJobCommandMessage(DifxMessage difxMsg)
   {
      // get the job
      Job job = getJob( difxMsg.getBody().getDoiJobCommand().getJobName(),
                        difxMsg.getBody().getDoiJobCommand().getFullPath() );
      // command it
      if (job != null)
      {
         if ( difxMsg.getBody().getDoiJobCommand().getCommand().equalsIgnoreCase("ResetReady") )
         {
            job.setStarted(false);
            job.setStatus("Ready");
            job.setState(DiFXSystemStatus.JobStates.READY);
            job.setCompletion(0.0f);
            //job.setQueued(false);
         }
         else if ( difxMsg.getBody().getDoiJobCommand().getCommand().equalsIgnoreCase("ResetFail") )
         {
            job.setStatus("Failed");
            job.setState(DiFXSystemStatus.JobStates.FAILED);
            job.setCompletion(0.0f);
            //job.setQueued(false);
         }
         else if ( difxMsg.getBody().getDoiJobCommand().getCommand().equalsIgnoreCase("ResetComplete") )
         {
            job.setStatus("Complete");
            job.setState(DiFXSystemStatus.JobStates.COMPLETE);
            job.setCompletion(0.0f);
            //job.setQueued(false);
         }
         else if ( difxMsg.getBody().getDoiJobCommand().getCommand().equalsIgnoreCase("ResetUnknown") )
         {
            job.setStatus("Unknown");
            job.setState(DiFXSystemStatus.JobStates.UNKNOWN);
            job.setCompletion(0.0f);
            //job.setQueued(false);
         }
      }
      
      // clean up
      job = null;
   }

   private void processDOIQueueCommandMessage(DifxMessage difxMsg)
   {
      // get the queue

      // command it
   }

   private void processDOISystemConfigMessage(DifxMessage difxMsg)
   {
      // fill in the System Config object
      // DOISystemConfig config = new DOISystemConfig();

      DOISystemConfig.DiFXHome            = (difxMsg.getBody().getDoiSystemConfig()).getDifxHome();
      DOISystemConfig.ResourcesFile       = (difxMsg.getBody().getDoiSystemConfig()).getResourcesFile();
      DOISystemConfig.DB_HOST             = (difxMsg.getBody().getDoiSystemConfig()).getDbHost();
      DOISystemConfig.DB_SID              = (difxMsg.getBody().getDoiSystemConfig()).getDbSID();
      DOISystemConfig.DB_PWD              = (difxMsg.getBody().getDoiSystemConfig()).getDbPassword();
      DOISystemConfig.ORACLE_JDBC_DRIVER  = (difxMsg.getBody().getDoiSystemConfig()).getDbJdbcDriver();
      DOISystemConfig.ORACLE_JDBC_PORT    = (difxMsg.getBody().getDoiSystemConfig()).getDbJdbcPort();
      DOISystemConfig.DB_URL              = (difxMsg.getBody().getDoiSystemConfig()).getDbUrl();
      DOISystemConfig.IpAddress           = (difxMsg.getBody().getDoiSystemConfig()).getIpAddress();
      DOISystemConfig.Port                = (difxMsg.getBody().getDoiSystemConfig()).getPort();
      DOISystemConfig.BufferSize          = (difxMsg.getBody().getDoiSystemConfig()).getBufferSize();
      DOISystemConfig.LoggingEnabled      = (difxMsg.getBody().getDoiSystemConfig()).isLoggingEnabled();
      DOISystemConfig.StatusValidDuration = (difxMsg.getBody().getDoiSystemConfig()).getStatusValidDuration();

      // updateDataModel(config);
      // config = null;
   }

   private void processDOIErrorMessage(DifxMessage difxMsg)
   {
      // -- catch some exceptions and keep the program from terminating. . .
      try
      {
         // Just store the error message, no need to create an object and call updateDataModel()
         if ( (difxMsg.getBody().getDoiError().getSeverity() >= 0) &&
              (difxMsg.getBody().getDoiError().getSeverity() <= 4) )
         {
            String alertString   = DiFXSystemStatus.ConvertDiFXAlertIntoString(
                                     DiFXSystemStatus.DiFXAlerts.convert(difxMsg.getBody().getDoiError().getSeverity()) );
            Calendar cal         = Calendar.getInstance();
            SimpleDateFormat sdf = new SimpleDateFormat(DOISystemConfig.DATE_TIME_FORMAT);
            String timeStamp     = sdf.format(cal.getTime());

            String strToAdd = timeStamp   + " "   +
                              difxMsg.getHeader().getFrom()       + " : " +
                              difxMsg.getHeader().getIdentifier() + " : " +
                              alertString                         + " : " +
                              difxMsg.getBody().getDoiError().getErrorMessage().toString() + "\n";

            this.addAlert(strToAdd);
         }
      }
      catch (Exception e)
      {
         System.err.println("uncaught exception: " + e);
      }
   }

   public DifxMessage CreateDOIErrorMessage(String message)
   {
      ObjectFactory factory = new ObjectFactory();

      // Create header
      Header header = factory.createHeader();
      header.setFrom("doi");
      header.setTo("doi");
      header.setMpiProcessId("-1");
      header.setIdentifier("doi");
      header.setType("DoiErrorMessage");

      // Create alert informational message
      DoiError errorMessage = factory.createDoiError();
      errorMessage.setErrorMessage(message);
      errorMessage.setSeverity(4);

      // -- Create the XML defined messages and process through the system
      Body body = factory.createBody();
      body.setDoiError(errorMessage);

      DifxMessage difxMsg = factory.createDifxMessage();
      difxMsg.setHeader(header);
      difxMsg.setBody(body);

      return difxMsg;
   }

   public DifxMessage CreateDiFXAlertMessage(String message)
   {
      ObjectFactory factory = new ObjectFactory();

      // Create header
      Header header = factory.createHeader();
      header.setFrom("doi");
      header.setTo("doi");
      header.setMpiProcessId("-1");
      header.setIdentifier("doi");
      header.setType("DifxAlertMessage");

      // Create alert informational message
      DifxAlert alertMessage = factory.createDifxAlert();
      alertMessage.setAlertMessage(message);
      alertMessage.setSeverity(4);

      // -- Create the XML defined messages and process through the system
      Body body = factory.createBody();
      body.setDifxAlert(alertMessage);

      DifxMessage difxMsg = factory.createDifxMessage();
      difxMsg.setHeader(header);
      difxMsg.setBody(body);

      return difxMsg;
   }

   /**
    * Reads an external config file that contains paramters to be set by the user.
    * @param fileToOpen the full path name of the config file
    */
   public void readSystemConfig(String fileToOpen)
   {
     ObjectFactory   factory = new ObjectFactory();
     DoiSystemConfig doiConfig = factory.createDoiSystemConfig();

     try
     {
         javax.xml.bind.JAXBContext jaxbCtx = javax.xml.bind.JAXBContext.newInstance(doiConfig.getClass().getPackage().getName());
         javax.xml.bind.Unmarshaller unmarshaller = jaxbCtx.createUnmarshaller();
         doiConfig = (DoiSystemConfig) unmarshaller.unmarshal(new java.io.File(fileToOpen));

         Header header = factory.createHeader();
         header.setFrom("DOIView");
         header.setTo("DOIModel");
         header.setMpiProcessId("0");
         header.setIdentifier("doi");
         header.setType("DOIMessage");

         Body body = factory.createBody();

         // set resource data into the body
         body.setDoiSystemConfig(doiConfig);

         // update the data model with DifxMessage
         DifxMessage difxMsg = factory.createDifxMessage();
         difxMsg.setHeader(header);
         difxMsg.setBody(body);
         serviceDataModel(difxMsg);
     }
     catch (javax.xml.bind.JAXBException ex)
     {
         // XXXTODO Handle exception
         java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null, ex);
     }
   }

   /**
    * Reads the resources config file containing available
    * mark5 units and processing nodes
    * @param fileToOpen the path to the resource config file
    */
   public void readResourcesConfig(String fileToOpen)
   {
      //System.out.printf("******** Data model read resources config file data. \n");

      // Create factory to create messages
      ObjectFactory factory = new ObjectFactory();

      // Read the resource config file into diFxMessage and process one line at a time
      Scanner s = null;
      try
      {
         FileReader     fReader = new FileReader(fileToOpen);
         BufferedReader bReader = new BufferedReader(fReader);
         s = new Scanner(bReader);
         while (s.hasNext())
         {
            Header header = factory.createHeader();
            header.setFrom("DOIView");
            header.setTo("DOIModel");
            header.setMpiProcessId("0");
            header.setIdentifier("doi");
            header.setType("DOIMessage");

            Body body = factory.createBody();
            DoiResourceConfig resource = factory.createDoiResourceConfig();

            String strtok = s.next().trim();
            if (strtok.contains("mark5fx") || strtok.contains("swc"))
            {
               resource.setName(strtok);
               strtok = s.next();
               resource.setNumCPUs(Short.valueOf(strtok));
               strtok = s.next();
               resource.setNumCores(Short.valueOf(strtok));
               strtok = s.next();
               resource.setBogusGHz(Float.valueOf(strtok));
               strtok = s.next();
               resource.setType(Short.valueOf(strtok));

            
               // TODO this needs to be revisited...assume resource enabled
               resource.setEnabled(true);

               // set resource data into the body
               body.setDoiResourceConfig(resource);

               // update the data model with DifxMessage
               DifxMessage difxMsg = factory.createDifxMessage();
               difxMsg.setHeader(header);
               difxMsg.setBody(body);
               serviceDataModel(difxMsg);
            }

         } // -- while (s.hasNext())
      }
      catch (Exception e)
      {
         System.out.println("Exception: " + e);
      }
      finally
      {
         if (s != null)
         {
            s.close();
         }
      }

      //System.out.printf("******** Data model read resources config file complete. \n");
   }

    public int loadQueueFromDatabase() throws Exception
    {

      // maintain count
      mRecCount = 0;

      // Connect to DB
      mDBConnection.connectToDB();

      // get all data from DIFXQUEUE
     // ResultSet rs = mDBConnection.selectData("select * from DIFXQUEUE where INPUT_FILE like \'%.input\' and STATUS != \'COMPLETE\' order by PRIORITY, JOB_START ASC");
       ResultSet rs = mDBConnection.selectData("select * from vDOIQueue ");
      // fetch each row from the result set
      while (rs.next())
      {
         mRecCount++;
         //String proposal = rs.getString("PROPOSAL");
         String proposal = rs.getString("code");
         String segment  = "";
         //if (rs.getString("SEGMENT") != null)
         if (rs.getString("segment") != null)
         {
            //segment = rs.getString("SEGMENT");
            segment = rs.getString("segment");
         }

         String jobPass   = rs.getString("passName");
         int    jobNumber = rs.getInt("jobNumber");
         int    priority  = rs.getInt("priority");
         long jobStart = rs.getLong("jobStart");
         long jobStop = rs.getLong("jobStart");
       //  long   jobStart  = (rs.getDate("jobStart")).getTime();
       //  long   jobStop   = (rs.getDate("jobStart")).getTime();
         float  speedUp   = rs.getFloat("speedupFactor");
         String inputFile = rs.getString("inputFile");
         String status    = rs.getString("status");
         int    numAnt    = rs.getInt("numAntennas");

         // Create DOI message and service the data model
         ObjectFactory factory = new ObjectFactory();
         Header header = factory.createHeader();
         header.setFrom("DOIView");
         header.setTo("DOIModel");
         header.setMpiProcessId("0");
         header.setIdentifier("doi");
         header.setType("DOIMessage");

         // Service the datamodel with project data
         String projectName   = proposal;
         int last_slash       = inputFile.lastIndexOf("/");
         String projectPath   = inputFile.substring(0, last_slash);

         // fill in the project data
         DoiProject project = factory.createDoiProject();
         project.setProjectName(projectName);
         project.setProjectPath(projectPath);

         // set project data into the body
         Body body = factory.createBody();
         body.setDoiProject(project);

         // create only one DifxMessage, fill in header and set project data into body
         DifxMessage difxMsg = factory.createDifxMessage();
         difxMsg.setHeader(header);
         difxMsg.setBody(body);

         // process the message
         serviceDataModel(difxMsg);
         body.setDoiProject(null);
         body    = null;
         project = null;

         // Service the datamodel with job data
         int last_dot   = inputFile.lastIndexOf(".");
         String jobName = inputFile.substring(last_slash+1, last_dot);

         // fill in the job data
         DoiJob job = factory.createDoiJob();
         job.setJobName(jobName);
         job.setJobPath(projectPath);  // -- project path is the job path
         job.setProjectName(projectName);
         job.setProjectPath(projectPath);
         job.setSegment(segment);
         job.setJobPass(jobPass);
         job.setJobNumber(jobNumber);
         job.setPriority(priority);
         job.setActualSpeedUp(0.0f);
         job.setPredictedSpeedUp(speedUp);
         job.setStatus(status);
         job.setJobStartTimeMJD(jobStart);
         job.setJobStopTimeMJD(jobStop);
         job.setNumAntennas((byte) numAnt);
         job.setDbJob(true);

         // set job data into the body
         body = factory.createBody();
         body.setDoiJob(job);
         difxMsg.setBody(body);

         // process the message
         serviceDataModel(difxMsg);
         body.setDoiJob(null);
         body = null;
         job  = null;

         // Service the data model with job project relationshop

         // fill in job project relationship data
         DoiJobProject jobProj = factory.createDoiJobProject();
         jobProj.setJobName(jobName);
         jobProj.setProjectName(projectPath);   // use complete project name, include path
         jobProj.setOption("Insert");

         // set job project data into the body
         body = factory.createBody();
         body.setDoiJobProject(jobProj);
         difxMsg.setBody(body);

         // process the message
         serviceDataModel(difxMsg);
         body.setDoiJobProject(null);
         body    = null;
         jobProj = null;

      } // --  while (rs.next())

      // Close DB connection
      mDBConnection.close();
      return mRecCount;
    }

    public void postUpdatesToDatabase(Job job) throws Exception
    {

      if ( (job != null) && (job.isDbJob()) )
      {
         mDBConnection.connectToDB();

         // -- Update queue data
         int updateData = mDBConnection.updateData("update DIFXQUEUE set STATUS = "   +
                                                   "\'" + job.getStateString() + "\'" +
                                                   "where INPUT_FILE = "              +
                                                   "\'" + job.getInputFile()   + "\'");

         // get output file size in KBytes
         String fname = job.getOutputFile();
         File   file  = new File(fname);
         long   fsize = file.length();

         SimpleDateFormat formatter = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss");

         Date dStart = new Date(job.getCorrelationStartUTC());
         Date dStop  = new Date(job.getCorrelationStopUTC());
         String start = formatter.format(dStart);
         String stop  = formatter.format(dStop);

         // -- Insert log data
         mDBConnection.insertData("insert into DIFXLOG (PROPOSAL, SEGMENT, JOB_PASS, JOB_NUMBER, CORR_START, CORR_STOP, SPEEDUP, INPUT_FILE, OUTPUT_FILE, OUTPUT_SIZE, CORR_STATUS) values (" +
                                  "\'" + job.getProjectName() + "\', " +
                                  "\'" + job.getSegment().trim() + "\', " +
                                  "\'" + job.getJobPass()     + "\', " +
                                  ""   + job.getJobNumber()   + ",   " +
                                  "TO_DATE(  \'" + start  + "\', \'DD-MON-YYYY HH24:MI:SS\' ), " +
                                  "TO_DATE(  \'" + stop   + "\', \'DD-MON-YYYY HH24:MI:SS\' ), " +
                                  ""   + job.getPredictedSpeedUp()     + ", " +
                                  "\'" + job.getInputFile()   + "\', " +
                                  "\'" + job.getOutputFile()  + "\', " +
                                  ""   + fsize                + ",   " +
                                  "\'" + job.getStateString() + "\'  " +
                                  ")" );

         mDBConnection.close();

      } // -- if ( (job != null) && (job.isDbJob()) )
    }

   // Update the data structure in the Data Model
   private synchronized void updateDataModel(DiFXObject difxObj)
   {
      // -- Store a DiFXObject into data model

      //System.out.printf("******** Data model update model. \n");

      if (difxObj.getObjType().equalsIgnoreCase("jobQueue"))
      {
         // find the jobsQueue
         mQueue.updateObject(difxObj);
      }
      else if (difxObj.getObjType().equalsIgnoreCase("module"))
      {
         // -- test, update existing or insert new object
         boolean exists = false;

         // find the module
         Iterator it = mModules.iterator();
         while ((!exists) && it.hasNext())
         {
            Module element = (Module)it.next();
            if (element.getObjName().equalsIgnoreCase(difxObj.getObjName()))
            {
               // update the data model
               element.updateObject((Module) difxObj);
               exists = true;
            }
            element = null;
         }

         // insert new object
         if (!exists)
         {
            addModule((Module) difxObj);
         }
      }
      else if (difxObj.getObjType().equalsIgnoreCase("mark5"))
      {
         // -- update existing or insert new mark5
         boolean exists = false;

         // find the mark5
         Iterator it = mMark5Units.iterator();
         while ((!exists) && it.hasNext())
         {
            Mark5Unit element = (Mark5Unit) it.next();
            if (element.getObjName().equalsIgnoreCase(difxObj.getObjName()))
            {
               // update the data model - mark5 and re-associate to job/module
               element.updateObject((Mark5Unit) difxObj);
               exists = true;
            }
            element = null;
         }

         // insert new object
         if (!exists)
         {
            addMark5Unit((Mark5Unit) difxObj);
         }
      }
      else if (difxObj.getObjType().equalsIgnoreCase("processor"))
      {
         // -- test, update existing or insert new object
         boolean exists = false;

         // find the processor
         Iterator it = mProcessorNodes.iterator();
         while ((!exists) && it.hasNext())
         {
            ProcessorNode element = (ProcessorNode) it.next();
            if (element.getObjName().equalsIgnoreCase(difxObj.getObjName()))
            {
               // update the data model
               element.updateObject((ProcessorNode) difxObj);
               exists = true;
            }
            element = null;
         }

         // insert new object
         if (!exists)
         {
            addProcessorNode((ProcessorNode) difxObj);
         }
      }
      else if (difxObj.getObjType().equalsIgnoreCase("project"))
      {
         // -- test, update existing or insert new object
         boolean exists = false;
         
         // find the project and update
         Iterator it = mProjects.iterator();
         while ((!exists) && it.hasNext())
         {
            Project element = (Project) it.next();
            if ( element.getObjName().equalsIgnoreCase(((Project)difxObj).getObjName()) &&
                 element.getProjectPath().equalsIgnoreCase(((Project)difxObj).getProjectPath()) )
            {
               // update the data model
               element.updateObject((Project) difxObj);
               exists = true;
            }
            element = null;
         }

         // insert new project
         if (!exists)
         {
            addProject((Project) difxObj);
         }
      }
      else if (difxObj.getObjType().equalsIgnoreCase("job"))
      {
         // -- test, update existing or insert new object
         boolean exists = false;
         
         // find and update job         
         Iterator it = mJobs.iterator();
         while ((!exists) && it.hasNext())
         {
            Job element = (Job) it.next();
            if ( element.getObjName().equalsIgnoreCase( ((Job)difxObj).getObjName() ) &&
                 element.getJobPath().equalsIgnoreCase( ((Job)difxObj).getJobPath() ) )
            {
               // update the data model
               element.updateObject((Job) difxObj);
               exists = true;
            }
            element = null;
         }

         // insert new job and add to project
         if (!exists)
         {
            addJob((Job) difxObj);

            // Link the job to the project
            it = mProjects.iterator();
            while (it.hasNext())
            {
               // ProjectPath and JobPath are the same, paths exist in the project directory
               Project element = (Project) it.next();
               if ( element.getObjName().equalsIgnoreCase(     ((Job) difxObj).getProjectName() ) &&
                    element.getProjectPath().equalsIgnoreCase( ((Job) difxObj).getProjectPath() ) )
               {
                  // update the data model
                  element.addJob((Job) difxObj);
               }
               element = null;
            }
         }
      }
      else if (difxObj.getObjType().equalsIgnoreCase("jobProject"))
      {
         // -- update existing or insert new objects
         boolean exists = false;
         
         // Find and update jobProject and jobQueue
         Iterator it = mJobsProjects.iterator();
         while ((!exists) && it.hasNext())
         {
            // Does the job already exist in the project/job association?
            String jobName     = ((JobProject)difxObj).GetJobName();
            String projectPath = ((JobProject)difxObj).GetProject(); //-- need project full path

            JobProject element = (JobProject) it.next();
            if ( element.GetJobName().equalsIgnoreCase( jobName )     &&
                 element.GetProject().equalsIgnoreCase( projectPath ) )
            {
               // always update existing JobProject relationship
               element.updateObject((JobProject) difxObj);

               // Now update the actual queue
               Job job = getJob(jobName, projectPath);

               // Either insert/update the job in the queue, or delete it
               if (( ((JobProject)difxObj).GetOption().equalsIgnoreCase("Insert") ))
               {
                  job.setState(DiFXSystemStatus.JobStates.NOTREADY);
                  if ( !mQueue.exists(job) )
                  {
                     mQueue.add(job);
                  }
               }
               else if (( ((JobProject)difxObj).GetOption().equalsIgnoreCase("Delete") ))
               {
                  // If the job can be removed from the queue, delete it.
                  if ( (mQueue.remove(job) == true) )
                  {
                     job.setState(DiFXSystemStatus.JobStates.NOTQUEUED);
                  }
                  else // hrnnn some sort of issue....
                  {
                     System.out.println("******** Data model job not removed from queue. \n");
                  }
               }
               else if (( ((JobProject)difxObj).GetOption().equalsIgnoreCase("EnQueue") ))
               {
                  // do not insert duplicate job into run queue
                  if ( mQueue.existsRun(job) != true )
                  {
                     //job.setQueued(true);
                     mQueue.addRun(job);
                  }
               }
               else if (( ((JobProject)difxObj).GetOption().equalsIgnoreCase("DeQueue") ))
               {
                  //job.setQueued(false);
                  mQueue.removeRun(job);
               }

               // found it, so exit
               job = null;
               exists = true;

            } // -- if ( element.GetJobName().equalsIgnoreCase( jobName )     &&
              // --      element.GetProject().equalsIgnoreCase( projectPath ) )

            // clean up
            element = null;
            jobName     = null;
            projectPath = null;

         } // --  while ((!exists) && it.hasNext())

         // insert new jobProject, and add job into JobQueue
         if (!exists)
         {
            // -- Verify option is an insert
            if (( ((JobProject)difxObj).GetOption().equalsIgnoreCase("Insert") ))
            {
               // associate job to a specific project
               mJobsProjects.add((JobProject) difxObj);

               // insert job into jobs queue
               String jobName      = ((JobProject) difxObj).GetJobName();
               String projPathName = ((JobProject) difxObj).GetProject(); //-- need project full path
               Job job = getJob(jobName, projPathName);
               job.setState(DiFXSystemStatus.JobStates.NOTREADY);
               mQueue.add(job);

               // clean up
               job = null;
               projPathName = null;
               jobName      = null;
            }
         } // -- if (!exists)
      }

   }

   public synchronized void determineLostResources()
   {
      // Get list of all the mark5 units
      List<Mark5Unit> mark5s = this.getMark5Units();
      if (mark5s != null)
      {
         // march though the mark5 units, and verify current status
         Iterator mit = mark5s.iterator();
         while (mit.hasNext())
         {
            // get mark5, is status current?
            Mark5Unit mark5 = (Mark5Unit) mit.next();
            if ( (mark5.isStatusCurrent() != true) &&
                 (mark5.isStateChanged()  == true) )
            {
               // remove VSN association, break job connections
               mark5.setBankAVSN("");
               mark5.setBankBVSN("");
               mark5.setActiveBank("");
               mark5.setCurrentJob("");

               // generate mark5statusmessage
               DifxMessage difxMsg = createMark5StatusMessageLost(mark5);
               serviceDataModel(difxMsg);
            }

            mark5 = null;

         } // -- while (mit.hasNext())

      } // -- if (mark5s != null)

      mark5s = null;

      // no need to process these, but just a place holder
      List<ProcessorNode> processors = this.getProcessorNodes();

   }

   // Create various DiFX messages
   public DifxMessage createMark5StatusMessageLost(Mark5Unit mark5)
   {
      ObjectFactory factory = new ObjectFactory();

      // Create header
      Header header = factory.createHeader();
      header.setFrom("doi");
      header.setTo("doi");
      header.setMpiProcessId("-1");
      header.setIdentifier(mark5.getObjName());
      header.setType("Mark5StatusMessage");

      // -- Create the XML defined messages and process through the system
      Mark5Status mark5Status = factory.createMark5Status();
      mark5Status.setState("Lost");

      Body body = factory.createBody();
      body.setMark5Status(mark5Status);

      DifxMessage difxMsg = factory.createDifxMessage();
      difxMsg.setHeader(header);
      difxMsg.setBody(body);

      return difxMsg;

   }

   public synchronized void determineStateOfAllJobs()
   {
      if (mQueue != null)
      {
         // Search each job for the VSN
         List<Job> jobs = mQueue.getJobs();
         if (jobs != null)
         {
            Iterator jit   = jobs.iterator();
            while (jit.hasNext())
            {
               // get job
               Job job = (Job) jit.next();

               // do not reset the state for running, complete, killed, done,
               // unknown or failed jobs
               if ( (job.isRunning() != true) && (job.isComplete() != true) &&
                    (job.isKilled()  != true) && (job.isDone()     != true) &&
                    (job.isFailed()  != true) && (job.isUnknown()  != true) )
               {
                  //System.out.println("******** Data model determine state of queued job:" + job.getObjName());
                  if (job.notReady() == true)
                  {
                     job.setState(DiFXSystemStatus.JobStates.NOTREADY);
                  }
                  else if ((job.conflict() == true))
                  {
                     job.setState(DiFXSystemStatus.JobStates.CONFLICT);
                  }
                  else if ((job.waiting() == true))
                  {
                     job.setState(DiFXSystemStatus.JobStates.WAITING);
                  }
                  else //if ((job.ready() == true))
                  {
                     //System.out.println("Ready detected: " + job.getObjName());
                     job.setState(DiFXSystemStatus.JobStates.READY);
                  }
               }

               // clean up
               job = null;

            } // -- while (jit.hasNext()
         }

      } // -- if (mJobsQueue != null)
   }

   public void determineReadyQueuedJobs()
   {
      // Calculate all the queued jobs' state - job must be in the queue
      ArrayList<Job> qJobs = mQueue.getJobs();
      if ( (qJobs != null) && (qJobs.isEmpty() != true))
      {
         Iterator it = qJobs.iterator();
         while (it.hasNext())
         {
            // The queued job has not been run, it's resources are available and it is ready
            Job job = (Job) it.next();
            if ( (job.isComplete() != true) && (job.isUnknown()  != true) &&
                 (job.isFailed()   != true) && (job.isJobReady() == true) )
            {
               job.setState(DiFXSystemStatus.JobStates.READY);
            }
            job = null;
         }
      }

   }

   public void determineReadyJobs()
   {
      // Determine if job state is ready
      List<Job> jobs = getJobs();
      if (jobs != null)
      {
         Iterator jit = jobs.iterator();
         while (jit.hasNext())
         {
            // get job
            Job job = (Job) jit.next();
            if ( (job.isComplete() == false) && (job.isJobReady() == true) )
            {
               job.setState(DiFXSystemStatus.JobStates.READY);
            }
            job = null;

         } // -- while (jit.hasNext()
      }

   }

   public void determineNotReadyJobs()
   {
      // Determine if job state is not ready
      List<Job> jobs = getJobs();
      if (jobs != null)
      {
         Iterator jit   = jobs.iterator();
         while (jit.hasNext())
         {
            // get job
            Job job = (Job) jit.next();
            if (job.notReady() == true)
            {
               job.setState(DiFXSystemStatus.JobStates.NOTREADY);
            }
            job = null;

         } // -- while (jit.hasNext()
      }

   }

   public void determineConflictingJobs()
   {
      // Determine if 2 of the VSN for a given job match the same mark5

      // Determine if job state is conflict
      List<Job> jobs = getJobs();
      if (jobs != null)
      {
         Iterator jit   = jobs.iterator();
         while (jit.hasNext())
         {
            // get job
            Job job = (Job) jit.next();
            if (job.conflict() == true)
            {
               job.setState(DiFXSystemStatus.JobStates.CONFLICT);
            }
            job = null;

         } // -- while (jit.hasNext()

      } // -- if (jobs != null)
   }

   public void determineWaitingJobs()
   {
      // Determine if the job is missing a mark5

      // Determine if job state is waiting
      List<Job> jobs = getJobs();
      if (jobs != null)
      {
         Iterator jit = jobs.iterator();
         while (jit.hasNext())
         {
            // get job
            Job job = (Job) jit.next();
            if (job.waiting() == true)
            {
               job.setState(DiFXSystemStatus.JobStates.WAITING);
            }
            job = null;

         } // -- while (jit.hasNext()

      } // -- if (jobs != null)
   }
   

   //protected synchronized void NotifyListeners()
   public synchronized void notifyListeners()
   {
      //System.out.printf("******** Data model notify listeners. \n");

      if (mListeners != null)
      {
         Iterator it = mListeners.iterator();
         while (it.hasNext())
         {
            MessageListener l = (MessageListener) it.next();
            l.update();
         }
      }
      else
      {
         System.out.printf("******** Data model no attached listeners. \n");
      }

      //System.out.println("******** Data model notify listeners complete. \n");
   }
}
