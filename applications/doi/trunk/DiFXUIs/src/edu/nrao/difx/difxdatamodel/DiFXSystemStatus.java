package edu.nrao.difx.difxdatamodel;

public class DiFXSystemStatus {

   // Define various alerts, errors and states
   public enum DiFXAlerts{
      FATAL, SERVERE, ERROR, WARNING, INFO, VERBOSE, DEBUG;
      public static DiFXAlerts convert( int i )
      {
         for ( DiFXAlerts current : values() ) 
         {
             if ( current.ordinal() == i ) 
             {
                return current;

             }
         }
         return DEBUG;
      }
   }

   public enum DOIErrors{
      FATAL, SERVERE, ERROR, WARNING, INFO, VERBOSE, DEBUG;
      public static DOIErrors convert( int i )
      {
         for ( DOIErrors current : values() )
         {
             if ( current.ordinal() == i )
             {
                return current;

             }
         }
         return DEBUG;
      }
   }

   public enum DiFXStatus {
      SPAWNING, STARTING, RUNNING, ENDING, DONE, ABORTING, TERMINATING,
      TERMINATED, MPIDONE
   };

   public enum QueueStates {
      INVALID, EMPTY, IDLE, PAUSE, RUNONE, RUN, DONE, ERROR
   };

   public enum ProjectStates {
      INVALID, NOTQUEUED, SPAWNING, STARTING, RUNNING, ENDING, DONE, ABORTING,
      TERMINATING, TERMINATED, MPIDONE
   };

   public enum JobStates {
      INVALID, NOTQUEUED, NOTREADY, READY, CONFLICT, WAITING, SPAWNING, STARTING, 
      RUNNING, DONE, KILLED, COMPLETE, UNKNOWN, FAILED
   };

   public enum ClusterNodeStates {
      LOST, IDLE, ASSIGNED, ERROR, OFFLINE, BUSY
   };

   public enum Mark5States {
      OPENING, OPEN, CLOSE,
      GETDIRECTORY, GOTDIRECTORY,
      PLAY, PLAYSTART, PLAYINVALID, IDLE,
      ERROR, BUSY,
      INITIALIZING, RESETTING, REBOOTING, POWEROFF,
      NODATA, NOMOREDATA, COPY
   };

   // Define alerts and serverities
   public enum AltertSeverity {
      PROCESSINGFAILED, BADSTATIONDATA, BADMULTIPLESTATIONDATA, MINIORERROR,
      INFORMATIONONLY
   };

   // Define resource types
   public enum ResourceTypes {
      PROCESSOR, MARK5, MANAGER
   };

   static public String ConvertDiFXAlertIntoString(DiFXAlerts alert)
   {
      String alertString = "Invalid Alert";
      switch (alert)
      {
         case FATAL:
         {
            alertString = "FATAL";
            break;
         }
         case SERVERE:
         {
            alertString = "SEVERE";
            break;
         }
         case ERROR:
         {
            alertString = "ERROR";
            break;
         }
         case WARNING:
         {
            alertString = "WARNING";
            break;
         }
         case INFO:
         {
            alertString = "INFO";
            break;
         }
         case VERBOSE:
         {
            alertString = "VERBOSE";
            break;
         }
         case DEBUG:
         {
            alertString = "DEBUG";
            break;
         }
         default:
         {
            alertString = "Invalid Alert";

         }
      }

      return alertString;
   }

   static public String ConvertDOIErrorIntoString(DOIErrors error)
   {
      String errorString = "Invalid Error";
      switch (error)
      {
         case FATAL:
         {
            errorString = "FATAL";
            break;
         }
         case SERVERE:
         {
            errorString = "SEVERE";
            break;
         }
         case ERROR:
         {
            errorString = "ERROR";
            break;
         }
         case WARNING:
         {
            errorString = "WARNING";
            break;
         }
         case INFO:
         {
            errorString = "INFO";
            break;
         }
         case VERBOSE:
         {
            errorString = "VERBOSE";
            break;
         }
         case DEBUG:
         {
            errorString = "DEBUG";
            break;
         }
         default:
         {
            errorString = "Invalid Alert";

         }
      }

      return errorString;
   }

   static public JobStates ConvertQueueJobStateIntoEnum(String stateString)
   {
      JobStates state = JobStates.INVALID;

      if (stateString.equalsIgnoreCase("NOT QUEUED"))
      {
         state = JobStates.NOTQUEUED;
      }
      else if (stateString.equalsIgnoreCase("NOT READY"))
      {
         state = JobStates.NOTREADY;
      }
      else if (stateString.equalsIgnoreCase("READY"))
      {
         state = JobStates.READY;
      }
      else if (stateString.equalsIgnoreCase("CONFLICT"))
      {
         state = JobStates.CONFLICT;
      }
      else if (stateString.equalsIgnoreCase("WAITING"))
      {
         state = JobStates.WAITING;
      }
      else if (stateString.equalsIgnoreCase("RUNNING"))
      {
         state = JobStates.RUNNING;
      }
      else if (stateString.equalsIgnoreCase("KILLED"))
      {
         state = JobStates.KILLED;
      }
      else if (stateString.equalsIgnoreCase("COMPLETE"))
      {
         state = JobStates.COMPLETE;
      }
      else if (stateString.equalsIgnoreCase("UNKNOWN"))
      {
         state = JobStates.UNKNOWN;
      }
      else if (stateString.equalsIgnoreCase("FAILED"))
      {
         state = JobStates.FAILED;
      }
      else // -- default invalid
      {
         state = JobStates.INVALID;
      }

      return state;
   }

   static public String ConvertQueueJobStateIntoString(JobStates state)
   {
      String stateString = "Invalid State";
      switch (state)
      {
         case NOTQUEUED:
         {
            stateString = "NOTQUEUED";
            break;
         }
         case NOTREADY:
         {
            stateString = "NOTREADY";
            break;
         }
         case READY:
         {
            stateString = "READY";
            break;
         }
         case CONFLICT:
         {
            stateString = "CONFLICT";
            break;
         }
         case WAITING:
         {
            stateString = "WAITING";
            break;
         }
         case SPAWNING:
         {
            stateString = "SPAWNING";
            break;
         }
         case STARTING:
         {
            stateString = "STARTING";
            break;
         }
         case RUNNING:
         {
            stateString = "RUNNING";
            break;
         }
         case DONE:
         {
            stateString = "DONE";
            break;
         }
         case KILLED:
         {
            stateString = "KILLED";
            break;
         }
         case COMPLETE:
         {
            stateString = "COMPLETE";
            break;
         }
         case UNKNOWN:
         {
            stateString = "UNKNOWN";
            break;
         }
         case FAILED:
         {
            stateString = "FAILED";
            break;
         }
         default:
         {
            stateString = "Invalid State";

         }
      }

      return stateString;
   }

   static public String ConvertDiFXStatusIntoQueueStatus (String difxStatus)
   {
      String queueStatus = "Invalid State";
      if (difxStatus.equalsIgnoreCase("Spawning"))
      {
         queueStatus = "Spawning";
      }
      else if (difxStatus.equalsIgnoreCase("Starting"))
      {
         queueStatus = "Starting";
      }
      else if (difxStatus.equalsIgnoreCase("Running"))
      {
         queueStatus = "Running";
      }
      else if (difxStatus.equalsIgnoreCase("Ending"))
      {
         queueStatus = "Ending";
      }
      else if (difxStatus.equalsIgnoreCase("Done"))
      {
         queueStatus = "Done";
      }
      else if (difxStatus.equalsIgnoreCase("Aborting"))
      {
         queueStatus = "ErrorDone";
      }
      else if (difxStatus.equalsIgnoreCase("Terminating"))
      {
         queueStatus = "ErrorDone";
      }
      else if (difxStatus.equalsIgnoreCase("Terminated"))
      {
         queueStatus = "ErrorDone";
      }
      else if (difxStatus.equalsIgnoreCase("MpiDone"))
      {
         queueStatus = "MpiDone";
      }
      else
      {
         queueStatus = "Invalid State";
      }

      return queueStatus;
   }

   static public QueueStates ConvertQueueStatusIntoState (String status)
   {
      QueueStates queueState;
      if (status.equalsIgnoreCase("Running"))
      {
         queueState = QueueStates.RUN;
      }
      else if (status.equalsIgnoreCase("ErrorDone"))
      {
         queueState = QueueStates.ERROR;
      }
      else if (status.equalsIgnoreCase("Done"))
      {
         queueState = QueueStates.DONE;
      }
      else if (status.equalsIgnoreCase("MpiDone"))
      {
         queueState = QueueStates.IDLE;
      }
      else
      {
         queueState = QueueStates.INVALID;
      }

      return queueState;
   }

   static public String ConvertQueueStateIntoString (QueueStates state)
   {
      String stateString = "Invalid State";
      switch (state)
      {
         case EMPTY:
         {
            stateString = "EMPTY";
            break;
         }
         case IDLE:
         {
            stateString = "IDLE";
            break;
         }
         case PAUSE:
         {
            stateString = "PAUSE";
            break;
         }
         case RUNONE:
         {
            stateString = "RUN ONE";
            break;
         }
         case RUN:
         {
            stateString = "RUN";
            break;
         }
         case DONE:
         {
            stateString = "DONE";
            break;
         }
         case ERROR:
         {
            stateString = "ERROR";
            break;
         }
         default:
         {
            stateString = "Invalid State";

         }
      }

      return stateString;
   }

   static public String ConvertDiFXStatusIntoQueueJobStatus (String difxStatus)
   {
      String jobStatus = "Invalid State";
      if (difxStatus.equalsIgnoreCase("Spawning"))
      {
         jobStatus = "Spawning";
      }
      else if (difxStatus.equalsIgnoreCase("Starting"))
      {
         jobStatus = "Starting";
      }
      else if (difxStatus.equalsIgnoreCase("Running"))
      {
         jobStatus = "Running";
      }
      else if (difxStatus.equalsIgnoreCase("Ending"))
      {
         jobStatus = "Ending";
      }
      else if (difxStatus.equalsIgnoreCase("Done"))
      {
         jobStatus = "Done";
      }
      else if (difxStatus.equalsIgnoreCase("Aborting"))
      {
         jobStatus = "Aborting";
      }
      else if (difxStatus.equalsIgnoreCase("Terminating"))
      {
         jobStatus = "Terminating";
      }
      else if (difxStatus.equalsIgnoreCase("Terminated"))
      {
         jobStatus = "Terminated";
      }
      else if (difxStatus.equalsIgnoreCase("MpiDone"))
      {
         jobStatus = "MpiDone";
      }
      else
      {
         jobStatus = "Invalid State";
      }

      return jobStatus;
   }

   static public JobStates ConvertJobStatus (String status)
   {
      JobStates queueJobState = JobStates.INVALID;

      if ( status.equalsIgnoreCase("Spawning") )
      {
         queueJobState = JobStates.SPAWNING;
      }
      else if ( status.equalsIgnoreCase("Starting") )
      {
         queueJobState = JobStates.STARTING;
      }
      else if ( status.equalsIgnoreCase("Running" ) ||
                status.equalsIgnoreCase("Ending"  ) )
      {
         queueJobState = JobStates.RUNNING;
      }
      else if ( status.equalsIgnoreCase("Done") )
      {
         queueJobState = JobStates.DONE;
      }
      else if ( status.equalsIgnoreCase("Aborting"   ) ||
                status.equalsIgnoreCase("Terminating") ||
                status.equalsIgnoreCase("Terminated"))
      {
         queueJobState = JobStates.KILLED;
      }
      else if (status.equalsIgnoreCase("MpiDone"))
      {
         queueJobState = JobStates.COMPLETE;
      }
      else if (status.equalsIgnoreCase("Unknown"))
      {
         queueJobState = JobStates.UNKNOWN;
      }
      else
      {
         queueJobState = JobStates.INVALID;
      }

      return queueJobState;
   }

}

