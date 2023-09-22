package edu.nrao.difx.difxdatamodel;

import java.util.*;

public class Project extends DiFXObject
{
   // directory
   private String projectPath;

   // state is not status
   private DiFXSystemStatus.ProjectStates state; // NOTQUEUED, SPAWNING, STARTING,
                                                 // RUNNING, ENDING, DONE, ABORTING,
                                                 // TERMINATING, TERMINATED, MPIDONE

   // associated jobs
   private List<Job> jobs;


   public Project()
   {
      jobs = new ArrayList<Job>();
   }

   public String getProjectPath()
   {
      return this.projectPath;
   }

   public void setProjectPath(String newPath)
   {
      this.projectPath = newPath;
   }

   public DiFXSystemStatus.ProjectStates getState()
   {
      return state;
   }

   public void setState(DiFXSystemStatus.ProjectStates newState)
   {
      state = newState;
   }

   public List<Job> getJobs()
   {
      return this.jobs;
   }

   public void setJobs(List<Job> newJobs)
   {
      this.jobs = newJobs;
   }

   public void addJob(Job mJob)
   {
      jobs.add(mJob);
   }

   @Override
   public void updateObject(DiFXObject newData)
   {
      projectPath = ((Project) newData).getProjectPath();
      state       = ((Project) newData).getState();
      jobs.addAll(((Project) newData).getJobs());
      super.updateObject(newData);
   }

   @Override
   public boolean isEqual(DiFXObject objToCompare)
   {
      return ( projectPath.equals(((Project) objToCompare).getProjectPath()) &&
               state == ((Project) objToCompare).getState()                  &&
               jobs.equals(((Project) objToCompare).getJobs())               &&
               super.isEqual(objToCompare) );
   }
}

