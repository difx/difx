/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxcontroller;

import edu.nrao.difx.difxview.*;
import edu.nrao.difx.difxdatamodel.*;

import edu.nrao.difx.difxutilities.SendMessage;
import edu.nrao.difx.xmllib.difxmessage.*;

import javax.swing.*;

/**
 *
 * @author mguerra
 */
public class DiFXController {

   // Allow only one instance of thread manager and view, and datamodel
   static ThreadManager mThreadMgr;
   static DiFXDataModel mDataModel;

   // Support a view
   JFrame mView;

   // Call back listener
   MessageListener mListener;

   // Constructor
   public DiFXController()
   {
      // create thread manager, set the view and datamodel in the Initialize 
      // method
      DiFXController.mThreadMgr = new ThreadManager();
   }

   // Methods specific to the thread manager
   public ThreadManager getThreadManager()
   {
      return DiFXController.mThreadMgr;
   }

   public void setThreadManager(ThreadManager newMgr)
   {
      DiFXController.mThreadMgr = newMgr;
   }

   // Methods specific to view
   public JFrame getView()
   {
      return this.mView;
   }

   public void setView(JFrame newView)
   {
      this.mView = newView;
   }

   // Mthods specific to Datamodel
   public DiFXDataModel getDataModel()
   {
      return DiFXController.mDataModel;
   }

   public void setDataModel(DiFXDataModel newModel)
   {
      DiFXController.mDataModel = newModel;
   }

   // Process a DifxMessage, service the data model. This controller is responsible
   // for updating the data model - DiFX has a sinlge GUI controller.
   protected synchronized void processMessage(DifxMessage difxMsg)
   {
      //System.out.printf("************ DiFX Controller process message. \n");

      // This is where the controller needs to determine the message type and
      // act accordingly.
      
      // Just pass it through....
      if (mDataModel != null)
      {
         mDataModel.serviceDataModel(difxMsg);
      }

      //System.out.println("************ DiFX Controller process message complete. \n");
   }

   // Update the controller, not implemented
   public synchronized void updateController()
   {
      //System.out.printf("***************** DiFX Controller update controller. \n");
      //System.out.printf("***************** DiFX Controller update controller complete. \n");
   }

   // Init the view, model and message listener
   public void initialize(DiFXDataModel model, DiFXManagerUI view)
   {
      // initialize the model and view
      mDataModel = model;
      mView      = view;

      // create this controller's listener implementation of update()...
      mListener = new MessageListener() {
         @Override
         public void update()
         {
            // Get handle to conroller and update
            //System.out.printf("***************** DiFX update controller. \n");
            //UpdateController();
            //System.out.println("***************** DiFX update controller complete. \n");
         }
         
      };

      // attach this controller's listener to the model
      mDataModel.attachListener(mListener);
   }

   // Start the controller thread
   public void startController() throws InterruptedException
   {
      // Set the controller, used to update data model from process message queue
      (mThreadMgr.getProcessThread()).setController(this);
      (mThreadMgr.getUpdateThread()).setController(this);
      (mThreadMgr.getJobStateThread()).setController(this);

      // kick the threads
      mThreadMgr.startThreads();
   }

   // Stop the controller thread
   public void stopController()
   {
      mThreadMgr.stopThreads();
   }
   
   public void writeToSocket( DifxMessage difxMsg )
   {
      //System.out.printf("***************** DiFX Controller write to socket. \n");

      // Convert DiFX message into a XML string
      JAXBDiFXProcessor xmlProc = new JAXBDiFXProcessor(difxMsg);
      String xmlString = xmlProc.ConvertToXML();
        
      // Conversation succesful, so send the XML
      if (xmlString != null)
      {
          SendMessage.writeToSocket( xmlString );
      }
      else
      {
          System.out.printf("***************** DiFX Controller XML not defined or sent. \n");
      }

      // clean up
      xmlString = null;
      xmlProc   = null;
      
      //System.out.printf("***************** DiFX Controller write to socket complete. \n");
   }
   
}


