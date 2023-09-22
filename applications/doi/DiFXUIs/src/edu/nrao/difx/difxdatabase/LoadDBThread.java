/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxdatabase;

import edu.nrao.difx.difxdatamodel.DiFXDataModel;
import edu.nrao.difx.xmllib.difxmessage.Body;
import edu.nrao.difx.xmllib.difxmessage.DifxAlert;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.Header;
import edu.nrao.difx.xmllib.difxmessage.ObjectFactory;
import java.awt.Toolkit;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author mguerra
 *
 * Change History
 */
public class LoadDBThread extends Thread
{

   private DiFXDataModel mDataModel;
   private int mRecCount = 0;

   public LoadDBThread(String str)
   {
      super(str);
   }

   public void setDataModel(DiFXDataModel dataModel)
   {
      mDataModel = dataModel;
   }

   private void updateAlertsTextArea(String message)
   {
      // Get data from the data model and update the view
      if (mDataModel.getQueue() != null)
      {
         // add text to top
         String alertMessage = message;

         ObjectFactory factory = new ObjectFactory();

         // Create header
         Header header = factory.createHeader();
         header.setFrom("doi");
         header.setTo("doi");
         header.setMpiProcessId("0");
         header.setIdentifier("doi");
         header.setType("DifxAlertMessage");

         // Create alert informational message
         DifxAlert alert = factory.createDifxAlert();
         alert.setAlertMessage(alertMessage);
         alert.setSeverity(4);

         // -- Create the XML defined messages and process through the system
         Body body = factory.createBody();
         body.setDifxAlert(alert);

         DifxMessage difxMsg = factory.createDifxMessage();
         difxMsg.setHeader(header);
         difxMsg.setBody(body);

         mDataModel.serviceDataModel(difxMsg);

         // send out the message
         //if ((mController != null) && (difxMsg != null))
         //{
         //   mController.writeToSocket(difxMsg);
         //}

         // clean up
         alertMessage = null;

      } // -- if (mDataModel.getQueue() != null)

   }

   @Override
   public void run()
   {
      try
      {
         if (mDataModel != null)
         {
            Toolkit.getDefaultToolkit().beep();
            updateAlertsTextArea( "Load queue from database." );
            mRecCount = mDataModel.loadQueueFromDatabase();
            updateAlertsTextArea( "Load queue from database complete (" + mRecCount + ")." );
            Toolkit.getDefaultToolkit().beep();
         }
         //sleep((int)(Math.random() * 1000));
      }
      catch (InterruptedException ex)
      {
         Logger.getLogger(LoadDBThread.class.getName()).log(Level.SEVERE, null, ex);         
      }
      catch (Exception ex)
      {
         Logger.getLogger(LoadDBThread.class.getName()).log(Level.SEVERE, null, ex);
      }

      System.out.println("DONE! " + getName() + " (" + mRecCount + ")");
   }
}
