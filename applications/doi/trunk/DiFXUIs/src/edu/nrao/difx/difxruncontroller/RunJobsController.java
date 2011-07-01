/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxruncontroller;

import edu.nrao.difx.difxview.*;
import edu.nrao.difx.difxdatamodel.*;


/**
 *
 * @author mguerra
 */
public class RunJobsController
{

   // Allow only one instance of thread manager and view, and datamodel
   RunJobsManager threadMgr;
   DiFXDataModel  theModel;

   // Support a view
   QueueManagerUI theView;

   // Call back listener
   MessageListener mListener;

   public RunJobsController()
   {
      // create thread manager, set the view and datamodel in the Initialize
      // method
      threadMgr = new RunJobsManager();
   }

   public RunJobsManager GetThreadManager()
   {
      return this.threadMgr;
   }

   public void SetThreadManager(RunJobsManager newMgr)
   {
      this.threadMgr = newMgr;
   }

   public QueueManagerUI GetView()
   {
      return this.theView;
   }

   public void SetView(QueueManagerUI newView)
   {
      this.theView = newView;
   }

   public DiFXDataModel GetModel()
   {
      return this.theModel;
   }

   public void SetView(DiFXDataModel newModel)
   {
      this.theModel = newModel;
   }

   public void Initialize(DiFXDataModel model, QueueManagerUI view)
   {
      // initialize the model and view
      theView = view;
      theModel = model;
   }

   public void StartController() throws InterruptedException
   {
      //(threadMgr.GetMessageQueue()).SetController(this);
      threadMgr.startThreads();

   }

   public void StopController()
   {
      threadMgr.stopThreads();

   }

   public void StartJob()
   {
   }

   public void Pause()
   {
   }

   public void StopJob()
   {
   }

   public void StartAll()
   {
   }

   public void StopAll()
   {
   }

   public void Restart()
   {
   }

   /**
    * @param args the command line arguments
    */
   public static void main(String[] args)
   {
      // TODO code application logic here
   }
}
