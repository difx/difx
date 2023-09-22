/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxdatamodel;


/**
 * This is a strange intermediary class that may need to be revisited and removed.
 * 
 * @author MIGUEL
 */
public class JobProject extends DiFXObject {

    private String  jobName;
    private String  projectName;
    private String  option;
    
    public JobProject()
    {
        jobName     = "";
        projectName = "";
        option      = "";
    }        

    public JobProject(String job, String project)
    {
        jobName     = job;
        projectName = project;
        option      = "";
    }        
    
    public String GetJobName()
    {
        return jobName;
    }

    public void SetJobName( String job )
    {
        jobName = job;
    }

    public String GetProject()
    {
        return projectName;
    }

    public void SetProject( String project )
    {
        projectName = project;
    }

    public String GetOption()
    {
        return option;
    }

    public void SetOption( String newOption )
    {
        option = newOption;
    }

    @Override
    public void updateObject ( DiFXObject newData ) 
    {
        this.jobName     = ((JobProject)newData).jobName;
        this.projectName = ((JobProject)newData).projectName;
        this.option      = ((JobProject)newData).option;
        
        super.updateObject(newData);
    }

    @Override
    public boolean isEqual (DiFXObject objToCompare) 
    {
        return(this.jobName.equals(((JobProject)objToCompare).jobName)         &&        
               this.projectName.equals(((JobProject)objToCompare).projectName) &&        
               this.option.equals(((JobProject)objToCompare).option)           &&        
               super.isEqual(objToCompare));

    }
    
}
