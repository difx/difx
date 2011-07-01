/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxdatamodel;

import edu.nrao.sss.measure.JulianDate;
import edu.nrao.sss.measure.TimeDuration;
import edu.nrao.sss.measure.TimeUnits;
import edu.nrao.difx.difxdatabase.LoadDBThread;

import edu.nrao.difx.difxdatamodel.DiFXDataModel;
import edu.nrao.difx.difxdatamodel.DiFXSystemConfig;
import edu.nrao.difx.difxdatamodel.Job;
import edu.nrao.difx.difxdatamodel.Queue;

import edu.nrao.difx.xmllib.difxmessage.Body;
import edu.nrao.difx.xmllib.difxmessage.DifxAlert;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.Header;
import edu.nrao.difx.xmllib.difxmessage.ObjectFactory;

import java.awt.Toolkit;
import java.math.BigDecimal;
import java.math.MathContext;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.RowFilter;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

/**
 *
 * @author mguerra
 *
 * Change History
 */
public class LoadProductionQueueThread extends Thread
{
   private DiFXDataModel     mDataModel;
   private DefaultTableModel mTableModel;
   private TableRowSorter<TableModel> mRowSorter;

   int mRecCount = 0;

   public LoadProductionQueueThread(String str)
   {
      super(str);
   }

   public void setDataModel( DiFXDataModel dataModel )
   {
      mDataModel = dataModel;
   }

   public void setTableModel( DefaultTableModel tableModel )
   {
      mTableModel = tableModel;
   }

   public void setTableRowSorter( TableRowSorter<TableModel> rowSorter )
   {
      mRowSorter = rowSorter;
   }

   private int updateProductionQueueTable()
   {
      // -- Get the data from the model
      int tableRow = 0;

      // Get data from the data model and update the view
      Queue queue = mDataModel.getQueue();
      if (queue != null)
      {
         // Get jobs from jobs queue
         if ( queue.isEmpty() != true )
         {
            // Verify jobs actually exist
            ArrayList<Job> jobs = queue.getJobs();
            if (jobs != null)
            {
               // Size up the table
               int numRows = mTableModel.getRowCount();
               int numJobs = jobs.size();

               while (numJobs > numRows)
               {
                  mTableModel.addRow(new Object[] {null, null, null, null, null, null, null, null, null, null, null, null, null, null});
                  numJobs--;
               }

               // fill in jobs table
               Iterator it = jobs.iterator();
               while (it.hasNext())
               {
                  // get job data from model to the screen
                  Job job = (Job) it.next();
                  if (job != null)
                  {
                     // insert job data into the table
                     mTableModel.setValueAt(job.getObjName(), tableRow, 0);  // job
                     if (job.isComplete())
                     {
                        mTableModel.setValueAt(true, tableRow, 1);  // checkoff
                     }
                     else
                     {
                        mTableModel.setValueAt(false, tableRow, 1);  // checkoff
                     }
                     mTableModel.setValueAt(job.getObsCode(), tableRow, 2);  // use obscode, not project
                     mTableModel.setValueAt(job.getPriority(),tableRow, 3);  // priority

                     // -- Calculate job time
                     TimeDuration timeOffset = new TimeDuration(new BigDecimal(job.getStartSeconds()), TimeUnits.SECOND);
                     BigDecimal   startMJD   = new BigDecimal(job.getStartMJD(), MathContext.UNLIMITED);
                     JulianDate   startJD    = JulianDate.makeFromMjd(startMJD);

                     // Job start date
                     startJD.add(timeOffset);
                     Date startDateTime = startJD.toDate();

                     SimpleDateFormat dateFormat = new SimpleDateFormat(DiFXSystemConfig.DATE_TIME_FORMAT);
                     mTableModel.setValueAt(dateFormat.format(startDateTime), tableRow, 4);  // start time
                     float obsLHours = (float) (job.getExecuteTimeSeconds() / 3600.0);
                     mTableModel.setValueAt(String.format("%.2f",obsLHours),  tableRow, 5);  // duration in hours

                     String HH = "00";
                     String mm = "00";
                     if (job.getPredictedSpeedUp() > 0)
                     {
                        float  corLhhmm  = (float) (obsLHours / job.getPredictedSpeedUp());//job.getActualSpeedUp());
                        String corLStr   = String.format("%.2f",corLhhmm);
                        HH = corLStr.substring(0, corLStr.indexOf("."));
                        if (HH.length() == 1)
                        {
                           HH = "0" + HH;
                        }
                        mm = corLStr.substring(corLStr.indexOf(".")+1, corLStr.length());
                     }
                     mTableModel.setValueAt(HH + ":" + mm, tableRow, 6);  // corr duration HH:mm

                     mTableModel.setValueAt(0.0,  tableRow, 7);  // data size
                     mTableModel.setValueAt(0.0,  tableRow, 8);  // data rate

                     mTableModel.setValueAt(job.getPredictedSpeedUp(), tableRow,  9);  // speed up
                     mTableModel.setValueAt("",                        tableRow, 10);  // version
                     mTableModel.setValueAt(job.getNumAntennas(),      tableRow, 11);  // num antennas
                     mTableModel.setValueAt(0,                         tableRow, 12);  // foreign stations
                     mTableModel.setValueAt(HH + ":" + mm,             tableRow, 13);  // cumulative hours remaining

                     // cleanup
                     timeOffset = null;
                     startMJD   = null;
                     startJD    = null;

                  } // -- if (job != null)

                  // inc row
                  ++tableRow;

                  // clean up
                  job = null;

               } // -- while (it.hasNext())

               // clean up
               mTableModel = null;

            } // -- if (jobs != null)

            // clean up
            jobs = null;

         } // -- if ( (mJobsQueue != null)

         // Filter out empty rows
         if (mRowSorter != null)
         {
            mRowSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
            //mRowSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
         }
      }
      else
      {
         System.out.printf("***************** Queue Manager jobs queue not defined. \n");
      }

      return tableRow;

   }

   private void updateAlertsTextArea(String message)
   {
      // Get data from the data model and update the view
      if (mDataModel.getQueue() != null)
      {
         // add text to top
         String alertMessage =  message;

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
               updateAlertsTextArea("Load production queue.");
               mRecCount = updateProductionQueueTable();
               updateAlertsTextArea("Load production queue complete (" + mRecCount + ").");
               Toolkit.getDefaultToolkit().beep();
            }
            //sleep((int)(Math.random() * 1000));
			}
			//catch (InterruptedException e) {}
         catch (Exception ex)
			{
				Logger.getLogger(LoadDBThread.class.getName()).log(Level.SEVERE, null, ex);
			}

			System.out.println("DONE! " + getName() + " (" + mRecCount + ")");
		}
}
