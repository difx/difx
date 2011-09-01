/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxdatamodel;

import edu.nrao.difx.difxstatemachine.*;
import edu.nrao.sss.measure.JulianDate;
import edu.nrao.sss.measure.TimeDuration;
import edu.nrao.sss.measure.TimeUnits;
import edu.nrao.difx.xmllib.difxmessage.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.*;
import java.math.BigDecimal;
import java.math.MathContext;

public class Job extends DiFXObject {

   public enum JobStopEvents {
      DONE, MPIDONE, ERROR
   };

   // config data from a .input text file
   private String jobPath = "";
   private String projectName = "";
   private String projectPath = "";
   private String segment = "";
   private String jobPass = "";
   private int    jobNumber = 0;
   private int    priority  = 0;
   private float  actualSpeedUp    = 0.0f;
   private float  predictedSpeedUp = 0.0f;
   private String delayFile = "";
   private String uvwFile   = "";
   private String coreConfigFile = "";
   private int    executeTimeSeconds = 0;
   private int    startMJD = 0;
   private int    startSeconds = 0;
   private int    activeDatastreams = 0;
   private int    activeBaselines   = 0;
   private int    numChannels = 0;
   private int    numAntennas = 0;
   
   // config data from a .calc text file
   private String     jobID   = "";
   private String     obsCode = "";
   private BigDecimal jobStartTimeMJD = new BigDecimal(0.0);
   private BigDecimal jobStopTimeMJD  = new BigDecimal(0.0);
   private int        numTelescopes = 0;
   private String     difxVersion = "";

   // location of .input, and .calc and .difx files
   private String     inputFile  = "";
   private String     calcFile   = "";
   private String     outputFile = "";
   
   // status fields
   private String     status     = "";
   private float      completion = 0.0f;
   private String     message    = "";
   private BigDecimal visibilityMJD    = new BigDecimal(0.0); // T_obs time;
   private BigDecimal startWallTimeMJD = new BigDecimal(0.0); // time the sw correlator started the job
   private long       startWallTimeUTC = 0l;                  // time the sw correlator started the job

   // correlation start and stop
   private long       correlationStartUTC = 0l;
   private long       correlationStopUTC  = 0l;

   // state is not status
   private DiFXSystemStatus.JobStates state = DiFXSystemStatus.JobStates.NOTQUEUED;

   // modules, unitis and resources
   private List<Module>        modules = new ArrayList<Module>(12);
   private List<ProcessorNode> processorNodes = new ArrayList<ProcessorNode>(10);

   // Job state
   private JobState mJobState = JobNotQueued.instance();

   // Database job
   private boolean dbJob = false;

   // Added to run queue
   private boolean queued = false;

   // Job has been started
   private boolean started = false;

   // All resources responding
   private boolean allResponding = true;


   private Job()
   {
      // some jobs need to verify, so hand it the datamodel
      this.setDataModel(null);

   }

   public Job(DiFXDataModel model)
   {
      // some jobs need to verify, so hand it the datamodel
      this.setDataModel(model);

   }

   // State methods - the job will change state as it is being run. . .See class: JobState
   public void changeState(JobState state)
   {
      mJobState = state;
      if (mJobState instanceof JobNotQueued)
      {
         setState(DiFXSystemStatus.JobStates.NOTQUEUED);
      }
      if (mJobState instanceof JobReady)
      {
         setState(DiFXSystemStatus.JobStates.READY);
      }
      if (mJobState instanceof JobConflict)
      {
         setState(DiFXSystemStatus.JobStates.CONFLICT);
      }
      if (mJobState instanceof JobUnknown)
      {
         setState(DiFXSystemStatus.JobStates.UNKNOWN);
      }
      if (mJobState instanceof JobWaiting)
      {
         setState(DiFXSystemStatus.JobStates.WAITING);
      }
      if (mJobState instanceof JobQueued)
      {
         setState(DiFXSystemStatus.JobStates.NOTREADY);
      }
      if (mJobState instanceof JobRunning)
      {
         setState(DiFXSystemStatus.JobStates.RUNNING);
      }
      if (mJobState instanceof JobComplete)
      {
         setState(DiFXSystemStatus.JobStates.COMPLETE);
      }
      if (mJobState instanceof JobKilled)
      {
         setState(DiFXSystemStatus.JobStates.KILLED);
      }
      if (mJobState instanceof JobDone)
      {
         setState(DiFXSystemStatus.JobStates.DONE);
      }
   }

   public void resetReady()
   {
      mJobState.resetReady(this);
   }

   public void resetComplete()
   {
      mJobState.resetComplete(this);
   }

   public JobState getJobState()
   {
      return mJobState;
   }

   public void start()
   {

   }

   public void stop(JobStopEvents stopEvent)
   {

   }

   public void accept()
   {

   }

   public void setJobState(JobState state)
   {
      mJobState = state;
   }

   // Is job waiting?
   public boolean isWaiting()
   {
      return (this.state == DiFXSystemStatus.JobStates.WAITING);
   }

   // Is job not ready?
   public boolean isNotReady()
   {
      return (this.state == DiFXSystemStatus.JobStates.NOTREADY);
   }

   // Is job complete?
   public boolean isComplete()
   {
      return (this.state == DiFXSystemStatus.JobStates.COMPLETE);
   }

   // Has job been killed?
   public boolean isKilled()
   {
      return (this.state == DiFXSystemStatus.JobStates.KILLED);
   }

   // Is job ready to run?
   public boolean isReady()
   {
      return (this.state == DiFXSystemStatus.JobStates.READY);
   }

   // Is job already running?
   public boolean isRunning()
   {
      return (this.state == DiFXSystemStatus.JobStates.RUNNING);
   }

   // Is job lost?
   public boolean isUnknown()
   {
      return (this.state == DiFXSystemStatus.JobStates.UNKNOWN);
   }

   // Is job failed?
   public boolean isFailed()
   {
      return (this.state == DiFXSystemStatus.JobStates.FAILED);
   }

   // Is job Done?
   public boolean isDone()
   {
      return (this.state == DiFXSystemStatus.JobStates.DONE);
   }

   public synchronized boolean ready()
   {
      return ( (conflict() == false) &&
               (waiting()  == false) &&
               (notReady() == false) );
   }

   //
   // Determine if all resources are responding. . . .
   //
   public synchronized boolean allResponding()
   {
      // Assume all resoucres are responding
      boolean resp = true;

      // Traverse the modules
      Iterator modIt = modules.iterator();
      while ( (resp == true) && (modIt.hasNext() == true) )
      {
         Module module = (Module) modIt.next();

         // get mark5s and verify the state
         String SVN = module.getModuleVSN();
         Mark5Unit mark5 = (this.getDataModel()).getMark5UnitViaVSN(SVN);
         if (mark5 != null)
         {
            // is mark5 ready to run? (idle or close)
            resp = mark5.isStatusCurrent();
         }

      } // while ( (lost == false) && (modIt.hasNext() == true) )

      // Step through the list of processors
      Iterator pit = processorNodes.iterator();
      while ( (resp) && (pit.hasNext()) )
      {
         // verify state of processor
         ProcessorNode processor = (ProcessorNode)pit.next();
         if (processor != null)
         {
            // is processor ready to run?
            resp = processor.isStatusCurrent();
         }
      }

      return resp;
   }

   //
   // Determine if this job is in the conflict state
   //
   public synchronized boolean conflict()
   {
      // assume no conflict
      boolean conflict = false;

      // sanity check, no need to test a complete job.
      if ( isComplete() )
      {
         conflict = true;
      }
      else // -- job not complete
      {
         // Create set of mark5s, no duplicates allowed
         Set<String> mark5s = new HashSet<String>();

         // Step through the list of modules
         Iterator modit = modules.iterator();
         while ( modit.hasNext() )
         {
            // Get the module's VSN
            Module module = (Module) modit.next();
            String vsn    = module.getModuleVSN();

            // Get mark 5
            Mark5Unit mark5  = (this.getDataModel()).getMark5UnitViaVSN(vsn);
            if (mark5 != null)
            {
               // Get name, a job can not have 2 VSNs in the same mark5
               String mark5Name = mark5.getObjName();

               // If insert fails, conflict found because 2 modules exist in the same mark5
               if ( !mark5s.add(mark5Name) )
               {
                   conflict = true;
                   // System.out.println("Conflict detected: " + vsn + ":" + mark5Name);
               }

            } // -- if (mark5 != null)

         } // -- while (ait.hasNext())
      }

      return (conflict == true);
   }

   //
   // Determine if this job is in the waiting state
   //
   public synchronized boolean waiting()
   {
      // assume no conflict
      boolean waiting = false;

      // Sanity check, no need to test a complete job.
      if ( isComplete() != true)
      {
         // Step through the list of antennas, exit if waiting
         Iterator modit = modules.iterator();
         while ( (!waiting) && (modit.hasNext()) )
         {
            // Get the modules's VSN
            Module module = (Module) modit.next();
            String vsn    = module.getModuleVSN();

            // Find the mark5's module with this VSN
            DiFXDataModel dataModel = getDataModel();
            if (dataModel != null)
            {
               // Get mark 5
               Mark5Unit mark5  = dataModel.getMark5UnitViaVSN(vsn);
               if (mark5 != null)
               {
                  // If mark5 is not running a job, flag it as not waiting
                  String m5Status = mark5.getState();
                  if ( (m5Status.equalsIgnoreCase("Idle"  )) ||
                       (m5Status.equalsIgnoreCase("Closed")) )
                  {
                     waiting = false;
                  }
                  else // -- not idle not closed, so determine which job is using this mark5
                  {
                     // Compare the current job running on the mark5 against this jobs name
                     String mark5JobName = mark5.getCurrentJob();
                     String jobName      = getObjName();

                     // If job names do not match, then the mark 5 is running another
                     //    job and this job is waiting for something...
                     if ( (mark5JobName.isEmpty()                  == false)  &&
//                          ((m5Status.equalsIgnoreCase("Busy")      == true )  &&
                           (jobName.equalsIgnoreCase(mark5JobName) == false) )
                     {
                        waiting = true;
                        //System.out.println("Wait detected: " + jobName + ":" + mark5JobName);
                     }

                  } // -- if ( (m5Status.equalsIgnoreCase("Idle"  )) ||
                    // --      (m5Status.equalsIgnoreCase("Closed")) )

               } // -- if (mark5 != null)

            } // -- if (dataModel != null)

         } // -- while ( (!waiting) && (modit.hasNext()) )
      }

      return (waiting == true);
   }

   //
   // Determine of this job is in the not ready state
   //
   public synchronized boolean notReady()
   {
      // Assume not in notReady state
      boolean notReady = false;

      // Sanity check, no need to test a complete job.
      if ( isComplete() == true)
      {
         notReady = true;
      }
      else // -- job not complete
      {
         // Step through the list of modules, find the VSN
         Iterator modit = modules.iterator();
         while (modit.hasNext())
         {
            // Get the modules's VSN
            Module module = (Module) modit.next();
            String vsn    = module.getModuleVSN();

            // Find the mark5's module with this VSN
            DiFXDataModel dataModel = getDataModel();
            if (dataModel != null)
            {
               // If this VSN/module is not assigned to a mark5, the module is
               //    not loaded into the system
               Mark5Unit mark5 = dataModel.getMark5UnitViaVSN(vsn);
               if (mark5 == null)
               {
                   // job is queued up
                   notReady = true;
                   //String jobName = getObjName();
                   //System.out.println("Queued detected: " + jobName + ":" + vsn);
               }
            }

         } // -- while (ait.hasNext())
      }

      return (notReady == true);
   }

   public String getJobPath() {
      return jobPath;
   }

   public void setJobPath(String val) {
      jobPath = val;
   }

   public String getProjectName() {
      return projectName;
   }

   public void setProjectName(String val) {
      projectName = val;
   }

   public String getProjectPath() {
      return projectPath;
   }

   public void setProjectPath(String val) {
      projectPath = val;
   }

   public String getSegment() {
      return segment;
   }

   public void setSegment(String val) {
      segment = val;
   }

   public String getJobPass() {
      return jobPass;
   }

   public void setJobPass(String val) {
      jobPass = val;
   }

   public int getJobNumber() {
      return jobNumber;
   }

   public void setJobNumber(int val) {
      jobNumber = val;
   }

   public int getPriority() {
      return priority;
   }

   public void setPriority(int val) {
      priority = val;
   }

   public float getActualSpeedUp() {
      return actualSpeedUp;
   }

   public void setActualSpeedUp(float val) {
      actualSpeedUp = val;
   }

   public float getPredictedSpeedUp() {
      return predictedSpeedUp;
   }

   public void setPredictedSpeedUp(float val) {
      predictedSpeedUp = val;
   }

   public String getDelayFile() {
      return delayFile;
   }

   public void setDelayFile(String val) {
      delayFile = val;
   }

   public String getUvwFile() {
      return uvwFile;
   }

   public void setUvwFile(String val) {
      uvwFile = val;
   }

   public String getCoreConfigFile() {
      return coreConfigFile;
   }

   public void setCoreConfigFile(String val) {
      coreConfigFile = val;
   }

   public int getExecuteTimeSeconds() {
      return executeTimeSeconds;
   }

   public void setExecuteTimeSeconds(int val) {
      executeTimeSeconds = val;
   }

   public int getStartMJD() {
      return startMJD;
   }

   public void setStartMJD(int val) {
      startMJD = val;
   }

   public int getStartSeconds() {
      return startSeconds;
   }

   public void setStartSeconds(int val) {
      startSeconds = val;
   }

   public int getActiveDatastreams() {
      return activeDatastreams;
   }

   public void setActiveDatastreams(int val) {
      activeDatastreams = val;
   }

   public int getActiveBaselines() {
      return activeBaselines;
   }

   public void setActiveBaselines(int val) {
      activeBaselines = val;
   }
    
   public int getNumChannels() {
      return numChannels;
   }

   public void setNumChannels(int val) {
      numChannels = val;
   }
      
   public int getNumAntennas() {
      return numAntennas;
   }

   public void setNumAntennas(int val) {
      numAntennas = val;
   }

   public String getJobID() {
      return jobID;
   }

   public void setJobID(String val) {
      jobID = val;
   }

   public String getObsCode() {
      return obsCode;
   }

   public void setObsCode(String val) {
      obsCode = val;
   }
      
   public BigDecimal getJobStartTimeMJD() {
      return jobStartTimeMJD;
   }

   public void setJobStartTimeMJD(BigDecimal val) {
      jobStartTimeMJD = val;
   }

   public BigDecimal getJobStopTimeMJD() {
      return jobStopTimeMJD;
   }

   public void setJobStopTimeMJD(BigDecimal val) {
      jobStopTimeMJD = val;
   }
   
   public int getNumTelescopes() {
      return numTelescopes;
   }

   public void setNumTelescopes(int val) {
      numTelescopes = val;
   }

   public String getDifxVersion() {
       return difxVersion;
   }

   public void setDifxVersion(String val) {
       difxVersion = val;
   }

   public String getInputFile() {
      return inputFile;
   }

   public void setInputFile(String val) {
      inputFile = val;
   }

   public String getCalcFile() {
      return calcFile;
   }

   public void setCalcFile(String val) {
      calcFile = val;
   }
   
   public String getOutputFile() {
      return outputFile;
   }

   public void setOutputFile(String val) {
      outputFile = val;
   }

   public String getStatus() {
      return status;
   }

   public void setStatus(String val) {
      status = val;
   }

   public void setStatusString() {
      status = getStateString();
   }

   public float getCompletion() {
      return completion;
   }

   public void setCompletion(float val) {
      completion = val;
   }

   public String getMessage() {
      return message;
   }

   public void setMessage(String val) {
      message = val;
   }

   public BigDecimal getVisibilityMJD() {
      return visibilityMJD;
   }

   public void setVisibilityMJD(BigDecimal val) {
      visibilityMJD = val;
   }

   public BigDecimal getStartWallTimeMJD() {
      return startWallTimeMJD;
   }

   public void setStartWallTimeMJD(BigDecimal val) {
      startWallTimeMJD = val;
   }

   public long getStartWallTimeUTC() {
      return startWallTimeUTC;
   }

   public void setStartWallTimeUTC(long val) {
      startWallTimeUTC = val;
   }

   public long getCorrelationStartUTC()
   {
      return correlationStartUTC;
   }

   public void setCorrelationStartUTC()
   {
      correlationStartUTC = System.currentTimeMillis();
   }

   public long getCorrelationStopUTC()
   {
      return correlationStopUTC;
   }

   public void setCorrelationStopUTC()
   {
      correlationStopUTC = System.currentTimeMillis();
   }

   public DiFXSystemStatus.JobStates getState()
   {
      return state;
   }

   public String getStateString()
   {
      return DiFXSystemStatus.ConvertQueueJobStateIntoString(state);
   }

   public void setState(DiFXSystemStatus.JobStates newState)
   {
      state = newState;
      setStatusString();
   }

   // Modules get/set methods
   public List<Module> getModules()
   {
      return this.modules;
   }

   public void addModule(Module module)
   {
      modules.add(module);
   }

   public synchronized Module getModule(String modName)
   {
      Iterator it = modules.iterator();
      while (it.hasNext() == true)
      {
         Module element = (Module) it.next();
         if (element.getObjName().equalsIgnoreCase(modName))
         {
            // found, return module
            return element;
         }
      }

      // not found
      return null;
   }

   public synchronized Module getModule(int objId)
   {
      Iterator it = modules.iterator();
      while (it.hasNext() == true)
      {
         Module element = (Module) it.next();
         if (element.getObjId() == objId)
         {
            // found, return antenna
            return element;
         }
      }

      // not found
      return null;
   }

   // Processor get/set methods
   public List<ProcessorNode> getProcessorNodes()
   {
      return this.processorNodes;
   }

   public void addProcessorNode(ProcessorNode procNode)
   {
      processorNodes.add(procNode);
   }

   public ProcessorNode getProcessorNode(String procNodeName)
   {
      Iterator it = processorNodes.iterator();
      while (it.hasNext() == true)
      {
         ProcessorNode element = (ProcessorNode)it.next();
         if (element.getObjName().equalsIgnoreCase(procNodeName))
         {
            // found, return job
            return element;
         }
      }

      // not found
      return null;
   }

   // Is job from database?
   public boolean isDbJob()
   {
      return dbJob;
   }

   public void setDbJob(boolean isdb)
   {
      dbJob = isdb;
   }

   // Is job queued?
   public boolean isQueued()
   {
      return queued;
   }

   public void setQueued(boolean added)
   {
      queued = added;
   }

   // Is job started?
   public boolean isStarted()
   {
      return started;
   }

   public void setStarted(boolean start)
   {
      started = start;
   }

   // Is a resource lost?
   public boolean isAllResponding()
   {
      return allResponding;
   }

   public void setAllResponding(boolean allResp)
   {
      allResponding = allResp;
   }

   // Create various DiFX messages
   /**
    * Create DiFX start message
    * @return 
    */
   public DifxMessage createDiFXStartMessage()
   {
      ObjectFactory factory = new ObjectFactory();

      // Create header
      Header header = factory.createHeader();
      header.setFrom("doi");
      
      header.setTo(this.getDataModel().getManagerNode().getObjName());
      header.setMpiProcessId("-1");
      header.setIdentifier(getObjName());
      header.setType("DifxStart");

      // Create start job command
      DifxStart jobStart = factory.createDifxStart();
      jobStart.setInput(getJobPath() + "/" + getObjName() + ".input");

      // -- manager, enabled only
      DifxStart.Manager manager = factory.createDifxStartManager();
      manager.setNode(this.getDataModel().getManagerNode().getObjName());
      jobStart.setManager(manager);

      // Get a string of Mark5 Units
      String mark5String = getStringOfMark5Units();

      // -- set difx version to use
      jobStart.setDifxVersion(getDifxVersion());

      // -- datastreams, enabled only
      DifxStart.Datastream dataStream = factory.createDifxStartDatastream();
      dataStream.setNodes(mark5String);
      jobStart.setDatastream(dataStream);

      // -- process and threads, enabled only
      DifxStart.Process process = factory.createDifxStartProcess();
      DifxStart.Process process2 = factory.createDifxStartProcess();
      process.setNodes(this.getDataModel().getProcessorNodesAsString());
      // TODO make number of threads configurable
      process.setThreads("7");
      jobStart.getProcess().add(process);
      //process2.setNodes("SWC000");
      //process2.setThreads("5");
      //jobStart.getProcess().add(process2);

      // force deletion of existing output file
      jobStart.setForce(1);
      
      // -- Create the XML defined messages and process through the system
      Body body = factory.createBody();
      body.setDifxStart(jobStart);

      DifxMessage difxMsg = factory.createDifxMessage();
      difxMsg.setHeader(header);
      difxMsg.setBody(body);

      // -- return null if the message is invalid, otherwise return the message
      if (mark5String.equals("") || mark5String.isEmpty())
      {
         return null; // did not create the proper list of mark units.
      }
      else
      {
         return difxMsg;
      }
   }

   /**
    * Returns a DiFX stop messages
    * @return 
    */
   public DifxMessage createDiFXStopMessage()
   {
      ObjectFactory factory = new ObjectFactory();

      // Create header
      Header header = factory.createHeader();
      header.setFrom("doi");
      header.setTo(this.getDataModel().getManagerNode().getObjName());
      header.setMpiProcessId("0");
      header.setIdentifier(getObjName());
      header.setType("DifxStop");

      // Create stop job command
      DifxStop jobStop = factory.createDifxStop();
      jobStop.setInput(getJobPath() + "/" + getObjName());

      // -- Create the XML defined messages and process through the system
      Body body = factory.createBody();
      body.setDifxStop(jobStop);

      DifxMessage difxMsg = factory.createDifxMessage();
      difxMsg.setHeader(header);
      difxMsg.setBody(body);

      return difxMsg;
   }

   public boolean isJobReady()
   {
      // Verify resources to determine if job is ready to run - mark 5 units
      // are idle or closed
      return (verifyResources());
   }

   
   public boolean verifyResources()
   {
      //System.out.println("***************** Job verify resources.");

      // assume verified
      boolean verified = true;

      // Traverse the modules
      Iterator modIt = modules.iterator();
      while ( verified && (modIt.hasNext() == true) )
      {
         Module module = (Module) modIt.next();

         // get mark5s and verify the state
         String SVN = module.getModuleVSN();
         Mark5Unit mark5 = (this.getDataModel()).getMark5UnitViaVSN(SVN);
         if (mark5 != null)
         {
            // is mark5 ready to run? (idle or close)
            verified = mark5.isVerified();
         }
         else
         {
            // mark 5 missing, fail varification
            verified = false;
         }

      }

      // Step through the list of processors
      Iterator pit = processorNodes.iterator();
      while ( verified && (pit.hasNext()) )
      {
         // verify state of processor
         ProcessorNode processor = (ProcessorNode)pit.next();
         if (processor != null)
         {
            // is processor ready to run?
            verified = processor.isVerified();
         }
      }
      
      return verified;
   }

   public boolean verifyInputFiles()
   {
      boolean verified = ( new File(this.uvwFile).exists()        &&
                           new File(this.inputFile).exists()      &&
                           new File(this.calcFile).exists()       &&
                           new File(this.delayFile).exists() );
      return (verified);
   }
 
   public boolean calcFileExists()
   {
      boolean verified = ( new File(this.calcFile).exists() );
      return (verified);
   }

   public boolean inputFileExists()
   {
      boolean verified = ( new File( this.inputFile).exists() );
      return (verified);
   }

   public boolean diFxFileExists()
   {
      boolean verified = ( new File(this.outputFile).exists() );
      return (verified);
   }

   public void claimResources()
   {
      // After resources deemed available, mark as assigned
      System.out.println("***************** Job claim reqources. \n");
   }

   public void freeResources()
   {
		  // Force the this job to free up all its module/resources
      if (modules != null)
      {
         // Step through the list of modules
         Iterator modit = modules.iterator();
         while ( modit.hasNext() )
         {
            // Get the module's VSN and get mark5
            Module module    = (Module) modit.next();
            String vsn       = module.getModuleVSN();
            Mark5Unit mark5  = (getDataModel()).getMark5UnitViaVSN(vsn);

            if (mark5 != null)
            {
               mark5.setCurrentJob("");
               mark5.setState("NoMoreData");

            } // -- if (mark5 != null)

         } // -- while (modit.hasNext())

      } // -- if (modules != null)
   }

   public void generateFiles()
   {
      // Generate .machines and .threads files
      System.out.println("***************** Job generate files. \n");
   }

   private String getStringOfMark5Units()
   {
      // Create string of mark5 units associated with this job
      String retStr = "";

      // Traverse the modules
      Iterator modIt = modules.iterator();
      while (modIt.hasNext() == true)
      {
         Module module = (Module) modIt.next();

         String SVN = module.getModuleVSN();
         Mark5Unit mark5 = (this.getDataModel()).getMark5UnitViaVSN(SVN);
         if (mark5 != null)
         {
            retStr = retStr + " " + mark5.getObjName();
         }
         else
         {
            // invalid set of mark5s, so exit.
            retStr = "";
            return retStr;
         }

      }

      return retStr.trim();
   }

   public float calculatePercentComplete()
   {
      float complete = 0.0f;

      // -- Job Start Time
      TimeZone.setDefault(TimeZone.getTimeZone("UTC"));

      TimeDuration timeOffset = new TimeDuration(new BigDecimal(getStartSeconds()),
                                                 TimeUnits.SECOND);
      BigDecimal MJDStart = new BigDecimal(getStartMJD(), MathContext.UNLIMITED);
      JulianDate JDStart  = JulianDate.makeFromMjd(MJDStart);
      JDStart.add(timeOffset);

      // Job Stop Time, add duration to start
      TimeDuration jobDuration = new TimeDuration(new BigDecimal(getExecuteTimeSeconds()),
                                                  TimeUnits.SECOND);
      JulianDate JDStop = JDStart;
      JDStop.add(jobDuration);
      Date stopDateTime = JDStop.toDate();

      // Current date/time
      BigDecimal MJDCurr  = getVisibilityMJD();
      JulianDate JDCurr   = JulianDate.makeFromMjd(MJDCurr);
      Date       currDate = JDCurr.toDate();

      // Convert to millis remaining
      float millisRemaining  = (float)(stopDateTime.getTime() - currDate.getTime());

      long executeTimeMillis = (long)(getExecuteTimeSeconds()*1000);
      complete = ((float)executeTimeMillis - millisRemaining) / (float)executeTimeMillis;
      
      return (complete*100.00f);
   }


   public void readJobData()
   {
      //System.out.printf("***************** Data Model read input and calc file. \n");

      // -- read the job's data files: .input and .calc
      try
      {
         // -- read *.input

         String jobFile  = getProjectPath() + "/" + getObjName();
         FileReader     frInput = new FileReader(jobFile + ".input");
         BufferedReader brInput = new BufferedReader(frInput);

         String sInput;
         while ((sInput = brInput.readLine()) != null)
         {
            setInputFile(jobFile  + ".input");
            setCalcFile(jobFile   + ".calc");
            setOutputFile(jobFile + ".difx");

            if (sInput.contains("DELAY FILENAME:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setDelayFile(sInput.trim());
            }
            else if (sInput.contains("UVW FILENAME:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setUvwFile(sInput.trim());
            }
            else if (sInput.contains("CORE CONF FILENAME:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setCoreConfigFile(sInput.trim());
            }
            else if (sInput.contains("EXECUTE TIME (SEC):"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setExecuteTimeSeconds(Integer.parseInt(sInput.trim()));
            }
            else if (sInput.contains("START MJD:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setStartMJD(Integer.parseInt(sInput.trim()));
            }
            else if (sInput.contains("START SECONDS:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               if (sInput.contains("."))
               {
                  sInput = sInput.substring(0, sInput.indexOf(".") );
               }
               setStartSeconds(Integer.parseInt(sInput.trim()));
            }
            else if (sInput.contains("ACTIVE DATASTREAMS:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setActiveDatastreams(Integer.parseInt(sInput.trim()));
            }
            else if (sInput.contains("ACTIVE BASELINES:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setActiveBaselines(Integer.parseInt(sInput.trim()));
            }
            else if (sInput.contains("VIS BUFFER LENGTH:") ||
                     sInput.contains("OUTPUT FORMAT:")     ||
                     sInput.contains("OUTPUT FILENAME:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
            }
            else if (sInput.contains("NUM CHANNELS:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setNumChannels(Integer.parseInt(sInput.trim()));
            }
            else if (sInput.contains("TELESCOPE ENTRIES:"))
            {
               sInput = sInput.substring(sInput.indexOf(":") + 1);
               setNumAntennas(Integer.parseInt(sInput.trim()));
            }
            else if (sInput.contains("TELESCOPE NAME "))
            {
               // Create antenna for the job
               Module newMod = new Module();
               newMod.setObjType("Module");

               String sInputObjID   = sInput.substring(sInput.indexOf(":")-2, sInput.indexOf(":"));
               String sInputObjName = sInput.substring(sInput.indexOf(":") + 1);

               // Note: the .input file is zero based
               newMod.setObjId( Integer.parseInt(sInputObjID.trim()));
               newMod.setObjName(sInputObjName.trim());

               addModule(newMod);
            }
            else if (sInput.contains("FILE "))
            {
               String sInputObjID = sInput.substring(sInput.indexOf("/")-2, sInput.indexOf("/"));
               String sInputVSN   = sInput.substring(sInput.indexOf(":") + 1);

               // -- Each job contains an module, and VSN
               // Find module via object ID
               Module curMod = getModule(Integer.parseInt(sInputObjID.trim()));

               // Update the antenna's VSN (module)
               curMod.setModuleVSN(sInputVSN.trim());

            }
         }

         frInput.close();

         // -- read *.calc

         FileReader     frCalc = new FileReader(jobFile + ".calc");
         BufferedReader brCalc = new BufferedReader(frCalc);

         String sCalc;
         while ((sCalc = brCalc.readLine()) != null)
         {
            if (sCalc.contains("JOB ID:"))
            {
               sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
               setJobID(sCalc.trim());
            }
            else if (sCalc.contains("OBSCODE:"))
            {
               sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
               setObsCode(sCalc.trim());
            }
            else if (sCalc.contains("JOB START TIME:"))
            {
               sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
               setJobStartTimeMJD(new BigDecimal(sCalc.trim()));
            }
            else if (sCalc.contains("JOB STOP TIME:"))
            {
               sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
               setJobStopTimeMJD(new BigDecimal(sCalc.trim()));
            }
            else if (sCalc.contains("NUM TELESCOPES:"))
            {
               sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
               setNumTelescopes(Integer.parseInt(sCalc.trim()));
            }
            else if (sCalc.contains("DIFX VERSION:"))
            {
                sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
                setDifxVersion(sCalc.trim());
            }
            else if (sCalc.contains("NAME:"))
            {
               sCalc = sCalc.substring(sCalc.indexOf(":") + 1);
            }
            else if (sCalc.contains("SHELF:"))
            {
               String sCalcObjID = sCalc.substring(sCalc.indexOf("SHELF")-3, sCalc.indexOf("SHELF"));
               String sCalcShelf = sCalc.substring(sCalc.indexOf(":") + 1);

               // Find antenna via object ID
               Module curMod = getModule(Integer.parseInt(sCalcObjID.trim()));

               // update the antenna's shelf
               curMod.setShelf(sCalcShelf.trim());

               //newJob.setNumTelescopes(Integer.parseInt(trimmed));
            }
         }

         frCalc.close();
         //System.out.printf("***************** Data model read input and calc file complete. \n");
      }
      catch (Exception e)
      {
         System.out.println("Exception: " + e);
      }
   }
   
   @Override
   public void updateObject(DiFXObject newData) 
   {   
      this.jobPath            = ((Job)newData).getJobPath();
      this.projectName        = ((Job)newData).getProjectName();        
      this.projectPath        = ((Job)newData).getProjectPath();
      this.segment            = ((Job)newData).getSegment();
      this.jobPass            = ((Job)newData).getJobPass();
      this.jobNumber          = ((Job)newData).getJobNumber();
      this.priority           = ((Job)newData).getPriority();
      this.actualSpeedUp      = ((Job)newData).getActualSpeedUp();
      this.predictedSpeedUp   = ((Job)newData).getPredictedSpeedUp();
      this.delayFile          = ((Job)newData).getDelayFile();
      this.uvwFile            = ((Job)newData).getUvwFile();
      this.coreConfigFile     = ((Job)newData).getCoreConfigFile();
      this.executeTimeSeconds = ((Job)newData).getExecuteTimeSeconds();
      this.startMJD           = ((Job)newData).getStartMJD();
      this.startSeconds       = ((Job)newData).getStartSeconds();
      this.activeDatastreams  = ((Job)newData).getActiveDatastreams();
      this.activeBaselines    = ((Job)newData).getActiveBaselines();
      this.numChannels        = ((Job)newData).getNumChannels(); 
      this.numAntennas        = ((Job)newData).getNumAntennas();

      this.jobID              = ((Job)newData).getJobID();
      this.obsCode            = ((Job)newData).getObsCode();
      this.jobStartTimeMJD    = ((Job)newData).getJobStartTimeMJD();
      this.jobStopTimeMJD     = ((Job)newData).getJobStopTimeMJD();
      this.numTelescopes      = ((Job)newData).getNumTelescopes();

      this.inputFile          = ((Job)newData).getInputFile();
      this.calcFile           = ((Job)newData).getCalcFile();
      this.outputFile         = ((Job)newData).getOutputFile();

      this.status             = ((Job)newData).getStatus();
      this.completion         = ((Job)newData).getCompletion();
      this.message            = ((Job)newData).getMessage();
      this.visibilityMJD      = ((Job)newData).getVisibilityMJD();
      this.startWallTimeMJD   = ((Job)newData).getStartWallTimeMJD();
      this.startWallTimeUTC   = ((Job)newData).getStartWallTimeUTC();

      this.state              = ((Job)newData).getState();
      this.dbJob              = ((Job)newData).isDbJob();
      //this.queued             = ((Job)newData).isQueued(); // do not update queued
      this.started            = ((Job)newData).isStarted();
      this.allResponding      = ((Job)newData).isAllResponding();

      super.updateObject(newData);
   }

   @Override
   public boolean isEqual(DiFXObject objToCompare) 
   {
      return( this.jobPath.equals(((Job)objToCompare).getJobPath())                  &&
              this.projectName.equals(((Job)objToCompare).getProjectName())          &&
              this.projectPath.equals(((Job)objToCompare).getProjectPath())          &&
              this.segment.equals(((Job)objToCompare).getSegment())                  &&
              this.jobPass.equals(((Job)objToCompare).getJobPass())                  &&
              this.jobNumber          == ((Job)objToCompare).getJobNumber()          &&
              this.priority           == ((Job)objToCompare).getPriority()           &&
              this.actualSpeedUp      == ((Job)objToCompare).getActualSpeedUp()      &&
              this.predictedSpeedUp   == ((Job)objToCompare).getPredictedSpeedUp()   &&
              this.delayFile.equals(((Job)objToCompare).getDelayFile())              &&
              this.uvwFile.equals(((Job)objToCompare).getUvwFile())                  &&
              this.coreConfigFile.equals(((Job)objToCompare).getCoreConfigFile())    &&
              this.executeTimeSeconds == ((Job)objToCompare).getExecuteTimeSeconds() &&
              this.startMJD           == ((Job)objToCompare).getStartMJD()           &&
              this.startSeconds       == ((Job)objToCompare).getStartSeconds()       &&
              this.activeDatastreams  == ((Job)objToCompare).getActiveDatastreams()  &&
              this.activeBaselines    == ((Job)objToCompare).getActiveBaselines()    &&
              this.numChannels        == ((Job)objToCompare).getNumChannels()        &&
              this.numAntennas        == ((Job)objToCompare).getNumAntennas()        &&
              this.jobID.equals(((Job)objToCompare).getJobID())                      &&
              this.obsCode.equals(((Job)objToCompare).getObsCode())                  &&
              this.jobStartTimeMJD    ==  ((Job)objToCompare).getJobStartTimeMJD()   &&
              this.jobStopTimeMJD     ==  ((Job)objToCompare).getJobStopTimeMJD()    &&
              this.numTelescopes      ==  ((Job)objToCompare).getNumTelescopes()     &&       
              this.inputFile.equals(((Job)objToCompare).getInputFile())              &&
              this.calcFile.equals(((Job)objToCompare).getCalcFile())                &&
              this.outputFile.equals(((Job)objToCompare).getOutputFile())            &&
              this.status.equals(((Job)objToCompare).getStatus())                    &&
              this.completion         == ((Job)objToCompare).getCompletion()         &&
              this.message.equals(((Job)objToCompare).getMessage())                  &&
              this.visibilityMJD      == ((Job)objToCompare).getVisibilityMJD()      &&
              this.startWallTimeMJD   == ((Job)objToCompare).getStartWallTimeMJD()   &&
              this.startWallTimeUTC   == ((Job)objToCompare).getStartWallTimeUTC()   &&
              this.state              == ((Job)objToCompare).getState()              &&
              this.dbJob              == ((Job)objToCompare).isDbJob()               &&
              this.queued             == ((Job)objToCompare).isQueued()              &&
              this.started            == ((Job)objToCompare).isStarted()             &&
              this.allResponding      == ((Job)objToCompare).isAllResponding()       &&
              super.isEqual(objToCompare) );
   }

   @Override
   public boolean isVerified()
   {
      if ( this.isReady() )
      {
         // Verify resources and files associated with this job
         if (verifyResources() == true)
         {
            if (verifyInputFiles() == true)
            {
               return true;
            }
            else
            {
               System.out.println("***************** Job verify input files failed. \n");
            }
         }
         else
         {
            System.out.println("***************** Job verify resources failed. \n");
         }
      }
      else
      {
         System.out.println("***************** Job verify state not ready. \n");
      }

      return false;
   }

}

