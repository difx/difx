/*
 * QueueManagerUI.java
 *
 * Created on May 12, 2008, 1:34 PM
 */
package edu.nrao.difx.difxview;

import edu.nrao.sss.measure.JulianDate;
import edu.nrao.sss.measure.TimeDuration;
import edu.nrao.sss.measure.TimeUnits;
import edu.nrao.difx.xmllib.difxmessage.DifxAlert;
import java.awt.print.PrinterException;
import java.text.ParseException;
import java.util.ArrayList;

import edu.nrao.difx.difxdatamodel.*;
import edu.nrao.difx.difxcontroller.*;

import edu.nrao.difx.difxdatabase.DBConnection;
import edu.nrao.difx.difxdatabase.LoadDBThread;
import edu.nrao.difx.difxdatamodel.LoadProductionQueueThread;
import edu.nrao.difx.difxutilities.*;
import edu.nrao.difx.difxutilities.TextAreaFIFO;

import edu.nrao.difx.xmllib.difxmessage.Body;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.DoiJobCommand;
import edu.nrao.difx.xmllib.difxmessage.Header;
import edu.nrao.difx.xmllib.difxmessage.ObjectFactory;
import java.awt.Color;
import java.awt.Font;
import java.math.BigDecimal;
import java.math.MathContext;
import java.text.NumberFormat;
import java.text.SimpleDateFormat;

import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.RowFilter;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.EtchedBorder;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;

/**
 *
 * Singleton
 */
public class QueueManagerUI extends javax.swing.JFrame
{

   private static final long serialVersionUID = 4;
   private static QueueManagerUI mInstance = null;

   // Allow only one data model and controller instance
   private static DiFXDataModel  mDataModel  = null;
   private static DiFXController mController = null;

   // Listen for data model updates
   private MessageListener mListener = null;

   // local members to contain the selected project and job
   private String       mProjectPath = "";
   private String       mJobName     = "";
   private int          mJobsCount   = 0;
   private boolean      mUpdate      = true;

   // Table model and custom table sorter
   TableRowSorter<TableModel> mSorter1 = null;
   TableRowSorter<TableModel> mSorter2 = null;
   TableRowSorter<TableModel> mSorter3 = null;

   // Drag and drop members
   private boolean mDraggingRow    = false;
   private int     mStartDragPoint = 0;
   private int     mDyOffset       = 0;

   // flag for lost dialog
   private boolean mDialogUp = false;

   // Scroll adjustment listener
   DiFXAdjustmentListener scrollListener;

   // lock scroll button
   JPanelButton buttonCorner = null;

   /** Creates new form QueueManagerUI */
   private QueueManagerUI()
   {
      // initialize GUI controls
      initComponents();
      createDiskListTree();

      mSorter1 = new TableRowSorter<TableModel>(jobsTable.getModel());
      jobsTable.setRowSorter(mSorter1);
      mSorter1.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      mSorter2 = new TableRowSorter<TableModel>(productionQueueTable.getModel());
      productionQueueTable.setRowSorter(mSorter2);
      mSorter2.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      mSorter3 = new TableRowSorter<TableModel>(runQueueTable.getModel());
      runQueueTable.setRowSorter(mSorter3);
      mSorter3.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      scrollListener = new DiFXAdjustmentListener();
      scrollListener.setTextArea(alertsTextArea);
      alertsScrollPane.getVerticalScrollBar().addAdjustmentListener(scrollListener);

      buttonCorner  = new JPanelButton();
      textScrollPane.setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, buttonCorner);
   }

   /** Creates new form QueueManagerUI */
   private QueueManagerUI(DiFXDataModel theModel)
   {
      // initialize GUI controls and data model
      initComponents();
      createDiskListTree();

      mDataModel = theModel;

      mSorter1 = new TableRowSorter<TableModel>(jobsTable.getModel());
      jobsTable.setRowSorter(mSorter1);
      mSorter1.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      mSorter2 = new TableRowSorter<TableModel>(productionQueueTable.getModel());
      productionQueueTable.setRowSorter(mSorter2);
      mSorter2.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      mSorter3 = new TableRowSorter<TableModel>(runQueueTable.getModel());
      runQueueTable.setRowSorter(mSorter3);
      mSorter3.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      scrollListener = new DiFXAdjustmentListener();
      scrollListener.setTextArea(alertsTextArea);
      alertsScrollPane.getVerticalScrollBar().addAdjustmentListener(scrollListener);

      buttonCorner  = new JPanelButton();
      textScrollPane.setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, buttonCorner);
   }

   /** Creates new form QueueManagerUI */
   public QueueManagerUI(DiFXDataModel theModel, DiFXController theController)
   {
      // initialize GUI controls and data model
      initComponents();
      createDiskListTree();

      mDataModel  = theModel;
      mController = theController;

      mSorter1 = new TableRowSorter<TableModel>(jobsTable.getModel());
      jobsTable.setRowSorter(mSorter1);
      mSorter1.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      mSorter2 = new TableRowSorter<TableModel>(productionQueueTable.getModel());
      productionQueueTable.setRowSorter(mSorter2);
      mSorter2.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      mSorter3 = new TableRowSorter<TableModel>(runQueueTable.getModel());
      runQueueTable.setRowSorter(mSorter3);
      mSorter3.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      scrollListener = new DiFXAdjustmentListener();
      scrollListener.setTextArea(alertsTextArea);
      alertsScrollPane.getVerticalScrollBar().addAdjustmentListener(scrollListener);

      buttonCorner  = new JPanelButton();
      textScrollPane.setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, buttonCorner);
   }

   public static QueueManagerUI instance(DiFXDataModel theModel, DiFXController theController)
   {
      if (mInstance == null)
      {
         mInstance = new QueueManagerUI(theModel, theController);
         mInstance.attachListenerCallback();
      }

      return mInstance;
   }

   private void scrollPaneToBottom() {

   SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run() {

      //if (alertsScrollPane.getVerticalScrollBar().getValue() == alertsScrollPane.getVerticalScrollBar().getMaximum())
      //{
         //alertsTextArea.setCaretPosition( alertsTextArea.getText().length() );
      //}
      //if (textScrollPane.getVerticalScrollBar().getValue() == textScrollPane.getVerticalScrollBar().getMaximum())
      //{
         //allMessagesTextArea.setCaretPosition( allMessagesTextArea.getText().length() );
         //allMessagesTextArea.setCaretPosition( allMessagesTextArea.getDocument.getLength() );
      //}
      alertsScrollPane.getVerticalScrollBar().setValue(alertsScrollPane.getVerticalScrollBar().getMaximum());
      //textScrollPane.getVerticalScrollBar().setValue(textScrollPane.getVerticalScrollBar().getMaximum());
      }

      });

   }

   private void printAlertsText()
   {
      try
      {
         allMessagesTextArea.setFont( new Font(allMessagesTextArea.getFont().getName(),
                         Font.PLAIN, 8));
         allMessagesTextArea.print();
         allMessagesTextArea.setFont( new Font(allMessagesTextArea.getFont().getName(),
                         Font.PLAIN, 12));

      }
      catch (PrinterException ex)
      {
         Logger.getLogger(QueueManagerUI.class.getName()).log(Level.SEVERE, null, ex);
      }
   }

   private void displayProductionQueue()
   {
      // Size up the table
      DefaultTableModel tableModel = (DefaultTableModel) productionQueueTable.getModel();
      Vector prodVector = tableModel.getDataVector();
      String header = "Job\t\t\t\tProject\t\tPri ObsStart\t\t\tObsL CorL\tDMSB\tRATE    SU    Vers    #S  FS  Hours\n";
      String line   = "=====================================================================================\n";
      String footer = "--------------------------------------------------------------------------------------------------------------------\n"
                    + "Job #\t\t: Job number \n"
                    + "Project\t\t: Project name (proposal/segment) \n"
                    + "Pri\t\t: Priority \n"
                    + "ObsStart\t: Start data \n"
                    + "ObsL\t\t: Observation length(hours) \n"
                    + "CorL\t\t: Estimated correlation time (HH:mm) \n"
                    + "DMSB\t\t: Estimated disk space (Megabytes) \n"
                    + "RATE\t\t: Estimated data rate (kilobytes/second) \n"
                    + "SU\t\t: Speedup \n"
                    + "Version\t: Software Version \n"
                    + "#S\t\t: Number of VLBA stations \n"
                    + "FS\t\t: Number of foreign stations \n"
                    + "Hours\t\t: Total for estimated correlation time.\n";

      String text = header + line;
      String projBreak = (String)(((Vector)prodVector.elementAt(0)).elementAt(2));

      int numJobs = 0;
      ArrayList<Job> jobs = (mDataModel.getQueue()).getJobs();
      if (jobs != null)
      {
       numJobs = jobs.size();
      }

      for (int i = 0; i < numJobs; i++)
      {
       // Add new project break
       if ( !projBreak.equalsIgnoreCase((String)(((Vector)prodVector.elementAt(i)).elementAt(2))) )
       {
          text = text + "\n" + line;
          projBreak = (String)(((Vector)prodVector.elementAt(i)).elementAt(2));
       }

       String Str = (String)(((Vector)prodVector.elementAt(i)).elementAt(0));
       String padJobStr  = String.format("%1$-16s", Str);

       Str = " ";
       boolean flag = (Boolean)(((Vector)prodVector.elementAt(i)).elementAt(1));
       if (flag == true)
       {
          Str = "X";
       }
       String padFlagStr = String.format("%1$1s", Str);

       Str = (String)(((Vector)prodVector.elementAt(i)).elementAt(2));
       String padProjStr = String.format("%1$-10s", Str);
       Str = (String)(((Vector)prodVector.elementAt(i)).elementAt(5));
       String padObsStr  = String.format("%1$4s", Str);
       Str = (String)(((Vector)prodVector.elementAt(i)).elementAt(6));
       String padCorStr  = String.format("%1$5s", Str);
       Str = String.format("%.2f", (((Vector)prodVector.elementAt(i)).elementAt(7)));
       String padDMSB    = String.format("%1$5s", Str);
       Str = String.format("%.2f", (((Vector)prodVector.elementAt(i)).elementAt(8)));
       String padRate    = String.format("%1$6s", Str);
       Str = String.format("%.2f", (((Vector)prodVector.elementAt(i)).elementAt(9)));
       String padSpeedUp = String.format("%1$6s", Str);
       Str = (String)(((Vector)prodVector.elementAt(i)).elementAt(10));
       String padVersion = String.format("%1$10s", Str);
       Str = String.format("%d", (((Vector)prodVector.elementAt(i)).elementAt(11)));
       String padStats   = String.format("%1$3s", Str);
       Str = String.format("%d", (((Vector)prodVector.elementAt(i)).elementAt(12)));
       String padFStats  = String.format("%1$3s", Str);
       Str = (String)(((Vector)prodVector.elementAt(i)).elementAt(13));
       String padCumHrs  = String.format("%1$5s", Str);
       text = text + padJobStr  + "\t"
                   + " [ " + padFlagStr +" ]\t"
                   + padProjStr + "\t "
                   + (((Vector)prodVector.elementAt(i)).elementAt(3)) + " "
                   + (((Vector)prodVector.elementAt(i)).elementAt(4)) + "\t"
                   + padObsStr  + " "
                   + padCorStr  + "\t"
                   + padDMSB    + " "
                   + padRate    + " "
                   + padSpeedUp + " "
                   + padVersion + " "
                   + padStats   + " "
                   + padFStats  + " "
                   + padCumHrs  + " "
                   +
                   i + "\n";
      }

      // Display the UI
      text = text + footer;
      PrintDataManagerUI thePDM = new PrintDataManagerUI(text);
      thePDM.setVisible(true);
   }

   private void displayDiskPacks()
   {
      String title = "VLBA SW Correlator Production Queue Disk List \n";
      String seperator = "---------------------------------------------------------------------------------------------------\n";
      String footer    = "-------------------------------------------------------------\n";
      String text = title + "\n";

      String jobRange = "JobRange: \n";

      // Create set of unique project names
      SortedSet<String> projSet = new TreeSet<String>();

      // Get set of unique unique projects
      List<Job> jobs = (mDataModel.getQueue()).getJobs();
      Iterator jit  = jobs.iterator();
      while ( jit.hasNext() )
      {
         // Add project names to project set
         Job job = (Job)jit.next();
         String projName = job.getProjectName();
         projSet.add(projName);

      } // -- ( jit1.hasNext() )

      // For each project create a set of unique antenna
      Iterator pit  = projSet.iterator();
      while ( pit.hasNext() )
      {
         // Get project name, use it to compare
         String projName = (String)pit.next();
         text = text + projName + " " + seperator;

         // Create set of unique antenna names
         SortedSet<String> antSet = new TreeSet<String>();

         // Step through all the jobs and create a list of antnnas
         jit  = jobs.iterator();
         while ( jit.hasNext() )
         {
            // Add antenna name to antenna set
            Job job = (Job)jit.next();

            // Process per project
            if ( projName.equalsIgnoreCase(job.getProjectName()) )
            {
               // Get modules to create a set of antennas
               List<Module> modules = job.getModules();
               Iterator mit  = modules.iterator();
               while ( mit.hasNext() )
               {
                  Module module = (Module)mit.next();
                  String antName = module.getObjName();
                  if ( antSet.add(antName) == true )
                  {
                     // Create set of unique svns
                     SortedSet<String> svnSet = new TreeSet<String>();

                     // get antenna name, use it to compare
                     text = text + "\n\t" + antName + ":\t";
                     int cnt = 0;

                     // Group all the jobs with antenna
                     Iterator jit2  = jobs.iterator();
                     while ( jit2.hasNext() )
                     {
                        // Get modules associated with the antenna and extract VSN and shelf location
                        Job job2 = (Job)jit2.next();
                        List<Module> modules2 = job2.getModules();
                        Iterator mit2  = modules2.iterator();
                        while ( mit2.hasNext() )
                        {
                           Module module2 = (Module)mit2.next();
                           if ( antName.equalsIgnoreCase(module2.getObjName())  &&
                                projName.equalsIgnoreCase(job2.getProjectName()) )
                           {
                              String vsnName = module2.getModuleVSN();
                              if ( svnSet.add(vsnName) == true )
                              {
                                 text = text + "(" + job2.getObjName() + ", " + module2.getModuleVSN() + ", " + module2.getShelf() + ")" + "\t";
                                 cnt++;
                                 if ((cnt%3) == 0)
                                 {
                                    text = text + "\n\t\t";
                                 }

                              } // -- if ( antSet.add(antName) == true )

                           } // -- if ( antName.equalsIgnoreCase(module.getObjName()) )

                        } // -- while ( mit2.hasNext() )

                     } // -- while ( jit2.hasNext() )

                     // remove the past tabs
                     int loc = text.lastIndexOf("\t\t");
                     if (loc != -1 )
                     {
                        //text = text.substring(0, loc);
                     }

                  } // -- if ( antSet.add(antName) == true )

               } // -- while ( mit.hasNext() )

            } // -- if ( projName.equalsIgnoreCase(job.getProjectName()) )

         } // -- while ( jit.hasNext() )

         text = text + "\n" + "Job Range:" + "\n\n";

      } // -- while ( pit.hasNext() )

      // Display the UI
      PrintDataManagerUI thePDM = new PrintDataManagerUI(text);
      thePDM.setVisible(true);
   }

   private void serviceDataModel(DifxMessage difxMessage)
   {
      // pass the message thru to the data model
      if (mDataModel != null)
      {
         mDataModel.serviceDataModel(difxMessage);
      }
   }

   private void updateView()
   {      
      // Add code to update the view
      if (mDataModel != null)
      {
         if (mUpdate == true)
         {
            // -- Get the data from the model
            Queue queue = mDataModel.getQueue();
            if (queue != null)
            {
               // update alerts text area
               ArrayList<String> alerts = mDataModel.getAlerts();
               if (alertsTextArea.getLineCount() > 5000)
               {
                  alertsTextArea.setText("");
               }

               Iterator it = alerts.iterator();
               while (it.hasNext() == true)
               {
                   String element = (String) it.next();
                   // weed out messages
                   if ( element.contains("FATAL") || element.contains("SEVERE")  ||
                        element.contains("ERROR") || element.contains("WARNING") ||
                        element.contains("doi :") )
                   {
                     alertsTextArea.append( element );
                   }

                   // -- append all
                   allMessagesTextArea.append( element );
                   if (buttonCorner.getState() == true)
                   {
                       allMessagesTextArea.setCaretPosition( allMessagesTextArea.getDocument().getLength() );
                   }
                   element = null;
               }
               alertsTextArea.setCaretPosition( alertsTextArea.getDocument().getLength() );
               mDataModel.clearAlerts();
               //scrollPaneToBottom();

               // Update state and wall time, enable/disable Run/Stop button
               queueStateTextField.setText(DiFXSystemStatus.ConvertQueueStateIntoString(queue.getState()));
               Calendar cal = Calendar.getInstance();
               SimpleDateFormat sdf = new SimpleDateFormat(DiFXSystemConfig.TIME_FORMAT);
               wallTimeTextField.setText(sdf.format(cal.getTime()));
              
               // Update the jobs available and queue tables
               updateJobsAvailableTable(queue);
               updateRunQueueTable(queue);

               // Calculate time
               int totalSeconds = 0;
               DefaultTableModel tableModel = (DefaultTableModel) runQueueTable.getModel();
               NumberFormat nf = NumberFormat.getIntegerInstance();
               for (int i = 0; i < runQueueTable.getRowCount(); i++)
               {
                  // Calculate total time of selected jobs in seconds (duration)
                  Number numSec;
                  try
                  {
                     numSec = nf.parse(tableModel.getValueAt(i, 4).toString());
                     totalSeconds += numSec.intValue();
                  }
                  catch (ParseException ex)
                  {
                     Logger.getLogger(QueueManagerUI.class.getName()).log(Level.SEVERE, null, ex);
                  }
               }

               tableModel = null;
               nf = null;

               // From when the queue started, calculate the remaining time
               // to finsish all selected jobs to be run
               int  timeRemainingSeconds = totalSeconds;
               if ( queue.getState() == DiFXSystemStatus.QueueStates.RUN )
               {
                  long stopTimeMillis       = (totalSeconds * 1000l) + queue.getTimeQueueStarted();
                  long timeRemainingMillis  = stopTimeMillis - System.currentTimeMillis();
                  timeRemainingSeconds      = (int)(timeRemainingMillis/1000l);
               }

               totalTimeTextField.setText( Elapsed.calcHMS(totalSeconds) );
               timeRemainingTextField.setText( Elapsed.calcHMS(timeRemainingSeconds) );
                  
               // Update job and project fields, and progress bar
               // -- only get current job, project pair
               float progress = 0.0f;
               Job currJob = queue.getCurrentJob();
               if (currJob != null)
               {
                  //jobTextField.setText(currJob.getObjName());
                  //projectTextField.setText(currJob.getObsCode());
                  progress = currJob.getCompletion();

                  if ( (currJob.isAllResponding() != true) && (mDialogUp == false) )
                  {
                     List<Module> modules = currJob.getModules();
                     List<ProcessorNode> processors = currJob.getProcessorNodes();
                     String text = currJob.getObjName() + " lost a resource during run \n";
                     //lostResourceDialog(text);
                     //pauseQueue();
                     
                     // clean up
                     modules    = null;
                     processors = null;
                  }

                  // clean up
                  currJob = null;
               }
               else
               {
                  //jobTextField.setText("");
                  //projectTextField.setText("");
               }

               // Update color scheme
               switch (queue.getState())
               {
                  case EMPTY:
                  case PAUSE:
                  {
                     //Border whiteLine = BorderFactory.createLineBorder(Color.cyan, 2);
                     Border whiteLine = BorderFactory.createLineBorder(Color.LIGHT_GRAY, 2);
                     //TitledBorder border = BorderFactory.createTitledBorder(blackLine, "Queue Status");
                     //queueStatePanel.setBorder(border);
                     queueStatePanel.setBorder(whiteLine);
                     runQueueScrollPane.setBorder(whiteLine);
                     jobsToolBar.setBackground(Color.WHITE);
                     queueToolBar.setBackground(Color.WHITE);
                     appsToolBar.setBackground(Color.WHITE);
                     break;
                  }
                  case IDLE:
                  {
                     //Border blueLine = BorderFactory.createLineBorder(Color.BLUE, 2);
                     //TitledBorder border = BorderFactory.createTitledBorder(blueLine, "Queue Status");
                     //queueStatePanel.setBorder(border);
                     Border blueLine = BorderFactory.createEtchedBorder(EtchedBorder.LOWERED, Color.BLUE, null);
                     queueStatePanel.setBorder(blueLine);
                     runQueueScrollPane.setBorder(blueLine);
                     jobsToolBar.setBackground(Color.BLUE);
                     queueToolBar.setBackground(Color.BLUE);
                     appsToolBar.setBackground(Color.BLUE);
                     break;
                  }
                  case RUNONE:
                  case RUN:
                  case DONE:
                  {
                     Border greenLine = BorderFactory.createLineBorder(Color.GREEN, 2);
                     //TitledBorder border = BorderFactory.createTitledBorder(greenLine, "Queue Status");
                     //queueStatePanel.setBorder(border);
                     queueStatePanel.setBorder(greenLine);
                     runQueueScrollPane.setBorder(greenLine);
                     jobsToolBar.setBackground(Color.GREEN);
                     queueToolBar.setBackground(Color.GREEN);
                     appsToolBar.setBackground(Color.GREEN);
                     break;
                  }
                  case ERROR:
                  {
                     Border redLine = BorderFactory.createLineBorder(Color.RED, 2);
                     //TitledBorder border = BorderFactory.createTitledBorder(redLine, "Queue Status");
                     //queueStatePanel.setBorder(border);
                     queueStatePanel.setBorder(redLine);
                     runQueueScrollPane.setBorder(redLine);
                     jobsToolBar.setBackground(Color.RED);
                     queueToolBar.setBackground(Color.RED);
                     appsToolBar.setBackground(Color.RED);
                     break;
                  }
                  default: // -- INVALID STATE
                  {
                     Border redLine = BorderFactory.createLineBorder(Color.RED, 2);
                     //TitledBorder border = BorderFactory.createTitledBorder(redLine, "Queue Status");
                     //queueStatePanel.setBorder(border);
                     queueStatePanel.setBorder(redLine);
                     runQueueScrollPane.setBorder(redLine);
                     jobsToolBar.setBackground(Color.RED);
                     queueToolBar.setBackground(Color.RED);
                     appsToolBar.setBackground(Color.RED);
                  }

               } // -- switch (queue.getState())

               jobsCountTextField.setText(String.format("%d", runQueueTable.getRowCount()));            
               jobProgressBar.setValue(Math.round(progress));

            } // -- if (mQueue != null)

            // cleanup
            queue = null;
            //System.out.printf("***************** Queue Manager update view complete. \n");

         } // -- if (mUpdate == true)

      }
      else // -- lost the datamodel
      {
         System.out.printf("***************** Queue Manager data model not defined. \n");
      }
   }

   private void lostResourceDialog(String message)
   {
      if (mDialogUp == false)
      {
         JFrame frame = null;
         DiFXDialog dialog = new DiFXDialog(this, "Lost Resource", message, null);
         mDialogUp = true;
      }
   }

   private void updateRunQueueTable(Queue queue)
   {
      // -- Get the data from the model
      int tableRow = 0;

      // Get data from the data model and update the view
      if (queue != null)
      {
         // Get jobs from jobs queue
         if ( queue.isEmpty() != true )
         {
            // Verify jobs actually exist
            ArrayList<Job> jobs = queue.getJobsToRun();
            if (jobs != null)
            {
               // Size up the table
               //DefaultTableModel tableModel = (DefaultTableModel) runQueueTable.getModel();

               //int numRows = tableModel.getRowCount();
               //while (numRows > 0)
               //{
                  //tableModel.removeRow(numRows-1); // zero based
                  //numRows--;
               //}

               // Size up the table
               DefaultTableModel tableModel = (DefaultTableModel)runQueueTable.getModel();
               int numRows = tableModel.getRowCount();
               int numJobs = jobs.size();

               while (numJobs > numRows)
               {
                  tableModel.addRow(new Object[] {null, null, null, null, null});
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
                     tableModel.setValueAt(job.getObjName(),     tableRow, 0);         // job
                     tableModel.setValueAt(job.getStateString(), tableRow, 1);         // state
                     tableModel.setValueAt(String.format("%.2f", job.getCompletion()), // complete
                                           tableRow, 2);
                     tableModel.setValueAt(job.getProjectPath(), tableRow, 3);         // project path
                     tableModel.setValueAt(job.getExecuteTimeSeconds(), tableRow, 4);  // duration secs

                  } // -- if (job != null)

                  // inc row
                  ++tableRow;

                  // clean up
                  job = null;

               } // -- while (it.hasNext())

               // clean up
               tableModel = null;

            } // -- if (jobs != null)

            // clean up
            jobs = null;
				 }
				 else // -- run queue contains no jobs but delete all rows from table
				 {
            DefaultTableModel tableModel = (DefaultTableModel)runQueueTable.getModel();
            int numRows = tableModel.getRowCount();
						while ( numRows > 0 )
						{
							tableModel.removeRow(numRows-1);
							numRows = tableModel.getRowCount();
						}

         } // if ( (mJobsRunQueue != null)

         // Filter out empty rows
         mSorter3.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
      }
      else
      {
         System.out.printf("***************** Queue Manager jobs queue not defined. \n");
      }

      mJobsCount = tableRow;
   }

   private void updateJobsAvailableTable(Queue queue)
   {
      // -- Get the data from the model
      int tableRow = 0;

      // Get data from the data model and update the view
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
               DefaultTableModel tableModel = (DefaultTableModel) jobsTable.getModel();
               int numRows = tableModel.getRowCount();
               int numJobs = jobs.size();

               while (numJobs > numRows)
               {
                  tableModel.addRow(new Object[] {null, null, null, null, null, null, null, null, null});
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
                     tableModel.setValueAt(job.getObjName(),     tableRow, 0);  // job
                     tableModel.setValueAt(job.isQueued(),       tableRow, 1);  // queued
                     tableModel.setValueAt(job.getObsCode(),     tableRow, 2);  // use obscode, not project
                     //tableModel.setValueAt(job.getStatus(),      tableRow, 2);  // status
                     tableModel.setValueAt(job.getStateString(), tableRow, 3);  // state
                     tableModel.setValueAt(job.getNumAntennas(), tableRow, 4);  // antennas

                     // -- Calculate job time
                     TimeDuration timeOffset = new TimeDuration(new BigDecimal(job.getStartSeconds()), TimeUnits.SECOND);
                     BigDecimal   startMJD   = new BigDecimal(job.getStartMJD(), MathContext.UNLIMITED);
                     JulianDate   startJD    = JulianDate.makeFromMjd(startMJD);

                     // Job start date
                     startJD.add(timeOffset);
                     Date startDateTime = startJD.toDate();

                     SimpleDateFormat dateFormat = new SimpleDateFormat(DiFXSystemConfig.DATE_TIME_FORMAT);
                     tableModel.setValueAt(dateFormat.format(startDateTime), tableRow, 5);  // start time
                     tableModel.setValueAt(job.getExecuteTimeSeconds(),      tableRow, 6);  // duration
                     tableModel.setValueAt(String.format("%.2f", job.getCompletion()),
                                           tableRow, 7);                                    // complete
                     //tableModel.setValueAt(DiFXSystemStatus.ConvertQueueJobStateIntoString(job.getState()),
                     //                      tableRow, 7);                                    // job queue state
                     tableModel.setValueAt(job.getProjectPath(),             tableRow, 8);  // project path

                     // -- Upate the run queue table with job
                     // updateRunQueueTable(job);
                     
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
               tableModel = null;
               
            } // -- if (jobs != null)

            // clean up
            jobs = null;
				 }
				 else // -- queue contains no jobs but delete all rows from table
				 {
            DefaultTableModel tableModel = (DefaultTableModel) jobsTable.getModel();
            int numRows = tableModel.getRowCount();
						while ( numRows > 0 )
						{
							tableModel.removeRow(numRows-1);
							numRows = tableModel.getRowCount();
						}
						
         } // if ( (mJobsQueue != null)

         // Filter out empty rows
         mSorter1.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
      }
      else
      {
         System.out.printf("***************** Queue Manager jobs queue not defined. \n");
      }

      mJobsCount = tableRow;
   }

   private void updateProductionQueueTable(Queue queue)
   {
      // -- Get the data from the model
      int tableRow = 0;

      // Get data from the data model and update the view
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
               DefaultTableModel tableModel = (DefaultTableModel) productionQueueTable.getModel();
               int numRows = tableModel.getRowCount();
               int numJobs = jobs.size();

               while (numJobs > numRows)
               {
                  tableModel.addRow(new Object[] {null, null, null, null, null, null, null, null, null, null, null, null, null, null});
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
                     tableModel.setValueAt(job.getObjName(), tableRow, 0);  // job
                     if (job.isComplete())
                     {
                        tableModel.setValueAt(true, tableRow, 1);  // checkoff
                     }
                     else
                     {
                        tableModel.setValueAt(false, tableRow, 1);  // checkoff
                     }
                     tableModel.setValueAt(job.getObsCode(), tableRow, 2);  // use obscode, not project
                     tableModel.setValueAt(job.getPriority(),tableRow, 3);  // priority

                     // -- Calculate job time
                     TimeDuration timeOffset = new TimeDuration(new BigDecimal(job.getStartSeconds()), TimeUnits.SECOND);
                     BigDecimal   startMJD   = new BigDecimal(job.getStartMJD(), MathContext.UNLIMITED);
                     JulianDate   startJD    = JulianDate.makeFromMjd(startMJD);

                     // Job start date
                     startJD.add(timeOffset);
                     Date startDateTime = startJD.toDate();

                     SimpleDateFormat dateFormat = new SimpleDateFormat(DiFXSystemConfig.DATE_TIME_FORMAT);
                     tableModel.setValueAt(dateFormat.format(startDateTime), tableRow, 4);  // start time
                     float obsLHours = (float) (job.getExecuteTimeSeconds() / 3600.0);
                     tableModel.setValueAt(String.format("%.2f",obsLHours),  tableRow, 5);  // duration in hours

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
                     tableModel.setValueAt(HH + ":" + mm, tableRow, 6);  // corr duration HH:mm

                     tableModel.setValueAt(0.0,  tableRow, 7);  // data size
                     tableModel.setValueAt(0.0,  tableRow, 8);  // data rate

                     tableModel.setValueAt(job.getPredictedSpeedUp(), tableRow,  9);  // speed up
                     tableModel.setValueAt("",                        tableRow, 10);  // version
                     tableModel.setValueAt(job.getNumAntennas(),      tableRow, 11);  // num antennas
                     tableModel.setValueAt(0,                         tableRow, 12);  // foreign stations
                     tableModel.setValueAt(HH + ":" + mm,             tableRow, 13);  // cumulative hours remaining

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
               tableModel = null;

            } // -- if (jobs != null)

            // clean up
            jobs = null;

         } // -- if ( (mJobsQueue != null)

         // Filter out empty rows
         mSorter2.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
      }
      else
      {
         System.out.printf("***************** Queue Manager jobs queue not defined. \n");
      }

   }

   private void createDiskListTree()
   {
      // Create node hierarchy
      DefaultMutableTreeNode root = new DefaultMutableTreeNode("Disk Packs");

      // Set root, create model
      TreeNode rootNode = root;
      DefaultTreeModel m_model = new DefaultTreeModel(rootNode);

      // set model and display
      disksTree.setModel(m_model);
      disksScrollPane.setViewportView(disksTree);

   }

   private void updateDiskListTree()
   {

     // -- create a tree of <project, antenna, jobs, and modules>

     // Create node hierarchy
     DefaultMutableTreeNode root = new DefaultMutableTreeNode("Disk Packs");

     // Find project nodes and add to tree
     List<Project> projects = mDataModel.getProjects();
     Iterator pit           = projects.iterator();
     while ( pit.hasNext() )
     {
         // get and create project node
         Project project = (Project)pit.next();
         String pname    = project.getObjName();
         DefaultMutableTreeNode pnode = new DefaultMutableTreeNode(pname);
         root.add(pnode);

         // Create set of antennas, to be used to maintain uniqueness in tree
         Set<String> set = new HashSet<String>();

         // get the project's jobs
         List<Job> jobs = project.getJobs();
         Iterator jit1  = jobs.iterator();
         while ( jit1.hasNext() )
         {
            // get jobs and modules
            Job job1 = (Job)jit1.next();
            List<Module> modules1 = job1.getModules();

            // -- Create tree containing antenna, job and module data
            // Step through the list of modules
            Iterator ait = modules1.iterator();
            while ( ait.hasNext() )
            {
               // Create unique antenna nodes
               Module modAnt = (Module) ait.next();
               if (modAnt != null)
               {
                  // if antenna node does not exist then add it into the tree
                  String antName = modAnt.getObjName();
                  DefaultMutableTreeNode anode = new DefaultMutableTreeNode(antName);
                  if ( set.add(antName) == true )
                  {
                     pnode.add(anode);
                  }

                  // Once again step through the list of jobs and modules to get the data and link to the antenna
                  Iterator jit2  = jobs.iterator();
                  while ( jit2.hasNext() )
                  {
                     // get job's and modules
                     Job job2 = (Job)jit2.next();
                     String jname = job2.getObjName();
                     List<Module> modules2 = job2.getModules();

                     Iterator mit = modules2.iterator();
                     while ( mit.hasNext() )
                     {
                        // Create module data nodes and link to antenna
                        Module modData = (Module) mit.next();
                        if (modData != null)
                        {
                           String antToCompare = modData.getObjName();
                           if ( antName.equalsIgnoreCase(antToCompare) )
                           {
                              // the data belongs to the antenna so link up the nodes
                              // get modeule's vsn and shelf data, create node and add to tree
                              String vsn   = modData.getModuleVSN();
                              String shelf = modData.getShelf();
                              DefaultMutableTreeNode dataNode = new DefaultMutableTreeNode("(" + jname + ", " + vsn + ", " + shelf + ")");
                              anode.add(dataNode);

                           } // -- if ( antName.equalsIgnoreCase(antToCompare) )

                        } // -- if (modData != null)

                     } // -- while (mit.hasNext())

                  } // -- while ( jit2.hasNext() )

               } // -- if (modAnt != null)

            } // -- while (ait.hasNext())

         } // -- while ( jit.hasNext() )
     }

     // Set root, create the tree containing the data
     TreeNode rootNode = root;
     DefaultTreeModel m_model = new DefaultTreeModel(rootNode);

     // set model and display
     disksTree.setModel(m_model);
     disksScrollPane.setViewportView(disksTree);

     // expand the entire tree
     expandJTree(disksTree, -1);
     
   }

	 private void loadJobsQueueFromDB()
	 {
			// stop updates
			mUpdate = false;

			// DB load thread
			LoadDBThread loadDBThread = new LoadDBThread("LoadDbThread");

			// connect and query the database
			if (mDataModel != null)
			{
				DBConnection dBConnection = mDataModel.getDBConnection();
				if (dBConnection != null)
				{
					try
					{
						 loadDBThread.setDataModel(mDataModel);
						 loadDBThread.start();
					}
					catch (Exception ex)
					{
						 Logger.getLogger(QueueManagerUI.class.getName()).log(Level.SEVERE, null, ex);
					}

				} // -- if (dBConnection != null)

			} // -- if (mDataModel != null)

			// Allow updates
			mUpdate = true;
	 }

	 private void updateProductionQueueTable()
	 {
			// stop updates
			mUpdate = false;

			// DB load thread
			LoadProductionQueueThread loadProductionQueueThread = new LoadProductionQueueThread("LoadProductionQueueThread");

			// connect and query the database
			if (mDataModel != null)
			{
				DBConnection dBConnection = mDataModel.getDBConnection();
				if (dBConnection != null)
				{
					try
					{
						 // Write message and load the queue from the database
						 loadProductionQueueThread.setDataModel(mDataModel);
                   loadProductionQueueThread.setTableModel((DefaultTableModel) productionQueueTable.getModel());
                   loadProductionQueueThread.setTableRowSorter(mSorter2);
						 loadProductionQueueThread.start();
					}
					catch (Exception ex)
					{
						 Logger.getLogger(QueueManagerUI.class.getName()).log(Level.SEVERE, null, ex);
					}

				} // -- if (dBConnection != null)

			} // -- if (mDataModel != null)

			// Allow updates
			mUpdate = true;
	 }

/**
 * Expands all nodes in a JTree.
 *
 * @param tree      The JTree to expand.
 * @param depth     The depth to which the tree should be expanded.  Zero
 *                  will just expand the root node, a negative value will
 *                  fully expand the tree, and a positive value will
 *                  recursively expand the tree to that depth.
 */
public void expandJTree (javax.swing.JTree tree, int depth)
{
    javax.swing.tree.TreeModel model = tree.getModel();
    expandJTreeNode(tree, model, model.getRoot(), 0, depth);
} // expandJTree()


/**
 * Expands a given node in a JTree.
 *
 * @param tree      The JTree to expand.
 * @param model     The TreeModel for tree.
 * @param node      The node within tree to expand.
 * @param row       The displayed row in tree that represents
 *                  node.
 * @param depth     The depth to which the tree should be expanded.
 *                  Zero will just expand node, a negative
 *                  value will fully expand the tree, and a positive
 *                  value will recursively expand the tree to that
 *                  depth relative to node.
 */
public int expandJTreeNode (javax.swing.JTree tree,
                            javax.swing.tree.TreeModel model,
                            Object node, int row, int depth)
{
   if (node != null  &&  !model.isLeaf(node))
   {
      tree.expandRow(row);
      if (depth != 0)
      {
         for (int index = 0;
              row + 1 < tree.getRowCount() && index < model.getChildCount(node);
              index++)
         {
            row++;
            Object child = model.getChild(node, index);
            if (child == null)
            {
               break;
            }
            javax.swing.tree.TreePath path;
            while ( (path = tree.getPathForRow(row)) != null  &&
                     path.getLastPathComponent()     != child )
            {
                  row++;
            }
            if (path == null)
            {
                  break;
            }
            
            row = expandJTreeNode(tree, model, child, row, depth - 1);
         }
      }
   }

   return row;

} // expandJTreeNode()


   private void updateAlertsTextArea(String message)
   {
      // Get data from the data model and update the view
      if (mDataModel.getQueue() != null)
      {
         String currJob      = "None";
         String currJobState = "";
         if ((mDataModel.getQueue()).getCurrentJob() != null)
         // if ((mDataModel.getQueue()).getNextJobToRun() != null)
         {
            currJob      = (mDataModel.getQueue()).getCurrentJob().getObjName();
            currJobState = (mDataModel.getQueue()).getCurrentJob().getStateString();

         } // -- if ((mDataModel.getQueue()).getCurrentJob() != null)

         // add text to top
         String alertMessage =  "Queue State: " +
                                (mDataModel.getQueue()).getStatus() +
                                " :: "           +
                                "Current Job: " + currJob          +
                                " :: "           + currJobState     +
                                " ::: "          + message;

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

   private void updateDatamodel(String projectName, String jobName, String option)
   {
      JobProject jobProject = new JobProject();
      jobProject.setObjType("jobProject");
      jobProject.setObjId(0);
      jobProject.setObjName(projectName + "+" + jobName);
      jobProject.SetJobName(jobName);
      jobProject.SetProject(projectName);
      jobProject.SetOption(option);

      mDataModel.serviceDataModel(jobProject);
      jobProject = null;
   }

   private void selectReadyToggle()
   {
       // get table model and set the queue flag
       DefaultTableModel queueTableModel = (DefaultTableModel)runQueueTable.getModel();
       DefaultTableModel jobsTableModel = (DefaultTableModel) jobsTable.getModel();
       boolean queueJobs = false;
          
       // Select/deselect jobs
       int rowCnt = jobsTable.getRowCount();
       if (rowCnt > 0)
       {
          // Select/deselect all jobs
          //if (selectReadyToggleButton.isSelected() == true)
          //{
             //queueJobs = true;
             //selectReadyToggleButton.setText("Deselect Ready");
          //}
          //else if (selectReadyToggleButton.isSelected() == false)
          //{
             //jobsTable.clearSelection();
             //selectReadyToggleButton.setText("Select Ready");
             
          //} // -- if (selectReadyToggleButton.isSelected() == true)

          // set the queue flag for any ready jobs and add to the run queue
          for (int row = 0; row < rowCnt; row++)
          {
             int i         = jobsTable.convertRowIndexToModel(row);
             Object object = jobsTable.getModel().getValueAt(i,3);
             boolean isReady = (object.toString()).equalsIgnoreCase("Ready");
             if (isReady)
             {
               //jobsTableModel.setValueAt(true, i, 1);  // ready to run
               //jobsTable.addRowSelectionInterval(row, row);

               // add to the run queue and update the data model
               //queueTableModel.addRow(new Object[]{"", "", "", ""});
               //int cnt = queueTableModel.getRowCount();
               //queueTableModel.setValueAt(jobsTableModel.getValueAt(row, 0), cnt-1, 0);
               //queueTableModel.setValueAt(jobsTableModel.getValueAt(row, 3), cnt-1, 1);
               //queueTableModel.setValueAt(jobsTableModel.getValueAt(row, 7), cnt-1, 2);
               //queueTableModel.setValueAt(jobsTableModel.getValueAt(row, 8), cnt-1, 3);
               updateDatamodel((String)jobsTableModel.getValueAt(row, 8),
                               (String)jobsTableModel.getValueAt(row, 0), "EnQueue");
             }
             else // -- not ready to run
             {
               jobsTableModel.setValueAt(false, row, 1);
             }

          } // -- for (int row = 0; row < rowCnt; row++)

          // Remove all jobs from the jobs queue
          if (!queueJobs)
          {
             int i = queueTableModel.getRowCount();
             while ( i > 0 )
             {
                String jobName  = (String)queueTableModel.getValueAt(i-1, 0);
                String projName = (String)queueTableModel.getValueAt(i-1, 3);
                queueTableModel.setValueAt("", i-1, 0);
                queueTableModel.setValueAt("", i-1, 1);
                queueTableModel.setValueAt("", i-1, 2);
                queueTableModel.setValueAt("", i-1, 3);
                queueTableModel.removeRow(i-1);
                updateDatamodel(projName, jobName, "DeQueue");
                --i;
             }

          } // -- if (!queueJobs)

       }
       else // -- jobs table is empty
       {
          jobsTable.clearSelection();
          //selectReadyToggleButton.setSelected(false);
          //selectReadyToggleButton.setText("Select Ready");

       } // -- if (rowCnt > 0)
      
   }

   private void pauseQueue()
   {
      // Queue and run the selected jobs
      if (mDataModel != null)
      {
         Queue queue = mDataModel.getQueue();
         if (queue != null)
         {
            queue.pause();
            
         } // -- (queue != null)

      } // -- if (mDataModel != null)
   }

   private void runQueue()
   {
      // Verify the run queue exists and run the jobs
      if (mDataModel != null)
      {
         Queue queue = mDataModel.getQueue();
         if (queue != null)
         {
            // Run the jobs
            try
            {
               // set time stamp and run queue
               updateAlertsTextArea("Start queue");
               queue.setTimeQueueStarted();
               queue.run();
            }
            catch (UnsupportedOperationException ex)
            {
               // Raise dialog,job run exception
               String error = ex.toString();
               error = error.substring(error.indexOf(":")+1);
               updateAlertsTextArea(error);
               Object[] options = {"OK"};
               JFrame frame = null;
               int opt = JOptionPane.showOptionDialog(frame,
                         error,
                         "Job Queue - Run Queue Error",
                         JOptionPane.OK_OPTION,
                         JOptionPane.ERROR_MESSAGE,
                         null, options, options[0]);

            } // -- if (error.contains(".difx file exists."))

         } // -- if (queue != null)

      } // -- if (mDataModel != null)

   }

   private void stopQueue()
   {

      // Stop the current running job
      if (mDataModel != null)
      {
         Queue queue = mDataModel.getQueue();
         if (queue != null)
         {
            // Get list of jobs to run
            ArrayList<Job> jobs = queue.getJobsToRun();

            // No need to do anything unless jobs exist
            if ( (jobs != null) && (jobs.isEmpty() != true) )
            {
               // Raise dialog, verify stop current job
               Job curJob = queue.getCurrentJob();

               // Stop the current job, else do nothing
               if (curJob != null)
               {
                  String jobName  = curJob.getObjName();
                  String projName = curJob.getProjectName();

                  Object[] options ={"Yes", "No"};
                  JFrame frame = null;
                  int opt = JOptionPane.showOptionDialog(frame,
                        "Queue will be stopped.   \n" +
                        "Job: "     + jobName  + "\n" +
                        "Project: " + projName + "\n\n" +
                        "Do you wish to stop the queue?",
                        "Job Queue - Stop",
                        JOptionPane.YES_NO_OPTION,
                        JOptionPane.QUESTION_MESSAGE,
                        null, options, options[1]);

                  // Yes, stop the job
                  if (opt == 0)
                  {
                     try
                     {
                        // Set time stamp and stop queue
                        updateAlertsTextArea("Stop queue");
                        queue.setTimeQueueStopped();

                        queue.stop();
                     }
                     catch (UnsupportedOperationException ex)
                     {
                           String error = ex.toString();
                           error = error.substring(error.indexOf(":")+1);

                           // Raise dialog, not in proper state to stop
                           Object[] options2 = {"OK"};
                           frame = null;
                           opt = JOptionPane.showOptionDialog(frame,
                                 error,
                                 "Job Queue - Stop Queue Error",
                                 JOptionPane.OK_OPTION,
                                 JOptionPane.ERROR_MESSAGE,
                                 null, options2, options[1]);
                     }

                  } // -- (if opt == 0)

               } // -- if (curJob != null)

            } // -- if ( (jobs != null) && (jobs.isEmpty() != true) )
         }
         else // -- jobs queue is lost
         {
             // Raise dialog
             JFrame frame = null;
             JOptionPane.showMessageDialog(frame,
                     "Job queue is lost.",
                     "Job Queue - Stop",
                     JOptionPane.ERROR_MESSAGE);
         }

         // clean up
         queue = null;

      } // if (mDatamodel != null)

   }

   private void killMpifxcorr()
   {

      // Stop the current running job
      if (mDataModel != null)
      {
         Queue queue = mDataModel.getQueue();
         if (queue != null)
         {

            Object[] options ={"Yes", "No"};
            JFrame frame = null;
            int opt = JOptionPane.showOptionDialog(frame,
                  "Kill mpifxcorr: this will stop all instances of mpifxcorr\n" +
                  "running on every software correlator node. \n\n"             +
                  "It should be used as a last resort before rebooting. \n"     +
                  "Do you wish to continue?",
                  "Job Queue - Kill",
                  JOptionPane.YES_NO_OPTION,
                  JOptionPane.QUESTION_MESSAGE,
                  null, options, options[1]);

            // Yes, stop the job
            if (opt == 0)
            {
               try
               {
                  // Set time stamp and stop queue
                  updateAlertsTextArea("Kill mpifxcorr");
                  queue.setTimeQueueStopped();

                  queue.killMpifxcorr();
               }
               catch (UnsupportedOperationException ex)
               {
                     String error = ex.toString();
                     error = error.substring(error.indexOf(":")+1);

                     // Raise dialog, not in proper state to stop
                     Object[] options2 = {"OK"};
                     frame = null;
                     opt = JOptionPane.showOptionDialog(frame,
                           error,
                           "Job Queue - Kill Queue Error",
                           JOptionPane.OK_OPTION,
                           JOptionPane.ERROR_MESSAGE,
                           null, options2, options[1]);
               }

            } // -- (if opt == 0)

         }
         else // -- jobs queue is lost
         {
             // Raise dialog
             JFrame frame = null;
             JOptionPane.showMessageDialog(frame,
                     "Job queue is lost.",
                     "Job Queue - Kill",
                     JOptionPane.ERROR_MESSAGE);
         }

         // clean up
         queue = null;

      } // if (mDatamodel != null)

   }

   public void attachListenerCallback()
   {
      // check dataModel and hand it an implementation of update()...
      if (mDataModel != null)
      {
         this.setTitle(this.getTitle() + " " + DOISystemConfig.DOIVersion);
         System.out.println("***************** Queue Manager attach listener.");

         // create listener implementation of update()...
         mListener = new MessageListener()
         {
            @Override
            public void update()
            {
               // Get handle to GUI and UpdateGUI()
               updateView();
               //ServiceDataModel();
            }
         };

         // hand DataModel a call back listener
         mDataModel.attachListener(mListener);

      }
      else
      {
         System.out.println("***************** Queue Manager listener not attached .\n");
      }
   }

   public void detachListener()
   {
      // remove message listener
      if (mDataModel != null)
      {
         System.out.printf("***************** Queue Manager detach listener. \n");
         mDataModel.detachListener(mListener);
      }
      else
      {
         System.out.println("***************** Queue Manager listener not detached. \n");
      }

   }

   /** This method is called from within the constructor to
    * initialize the form.
    * WARNING: Do NOT modify this code. The content of this method is
    * always regenerated by the Form Editor.
    */
   // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
   private void initComponents() {

      jobsToolBar = new javax.swing.JToolBar();
      selectAllButton = new javax.swing.JButton();
      deselectAllButton = new javax.swing.JButton();
      selectReadyButton = new javax.swing.JButton();
      deselectReadyButton = new javax.swing.JButton();
      removeButton = new javax.swing.JButton();
      validateButton = new javax.swing.JButton();
      jobManagerButton = new javax.swing.JButton();
      appsToolBar = new javax.swing.JToolBar();
      killButton = new javax.swing.JButton();
      jSplitPane1 = new javax.swing.JSplitPane();
      jobsTabbedPane = new javax.swing.JTabbedPane();
      jobsPanel = new javax.swing.JPanel();
      jobsScrollPane = new javax.swing.JScrollPane();
      jobsTable = new javax.swing.JTable();
      textPanel = new javax.swing.JPanel();
      textScrollPane = new javax.swing.JScrollPane();
      allMessagesTextArea = new TextAreaFIFO(5000);
      productionQueuePanel = new javax.swing.JPanel();
      productionQueueScrollPane = new javax.swing.JScrollPane();
      productionQueueTable = new javax.swing.JTable();
      disksPanel = new javax.swing.JPanel();
      disksScrollPane = new javax.swing.JScrollPane();
      disksTree = new javax.swing.JTree();
      runQueuePanel = new javax.swing.JPanel();
      queueStatePanel = new javax.swing.JPanel();
      jobProgressBar = new javax.swing.JProgressBar();
      queueStateLabel = new javax.swing.JLabel();
      queueStateTextField = new javax.swing.JTextField();
      noJobsLabel = new javax.swing.JLabel();
      jobsCountTextField = new javax.swing.JTextField();
      totalTimeLabel = new javax.swing.JLabel();
      totalTimeTextField = new javax.swing.JTextField();
      currentLabel = new javax.swing.JLabel();
      wallTimeTextField = new javax.swing.JTextField();
      timeRemainingLabel = new javax.swing.JLabel();
      timeRemainingTextField = new javax.swing.JTextField();
      runQueueScrollPane = new javax.swing.JScrollPane();
      runQueueTable = new javax.swing.JTable();
      queueToolBar = new javax.swing.JToolBar();
      startButton = new javax.swing.JButton();
      pauseButton = new javax.swing.JButton();
      stopButton = new javax.swing.JButton();
      clearButton = new javax.swing.JButton();
      readyButton = new javax.swing.JButton();
      resetButton = new javax.swing.JButton();
      bottomButtonPanel = new javax.swing.JPanel();
      printTabButton = new javax.swing.JButton();
      CloseButton = new javax.swing.JButton();
      loadQueueButton = new javax.swing.JButton();
      alertsScrollPane = new javax.swing.JScrollPane();
      alertsTextArea = new TextAreaFIFO(1000) ;

      setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
      setTitle("Queue Manager");
      setName("queueFrame"); // NOI18N

      jobsToolBar.setBackground(java.awt.Color.lightGray);
      jobsToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
      jobsToolBar.setFloatable(false);
      jobsToolBar.setRollover(true);
      jobsToolBar.setMaximumSize(new java.awt.Dimension(302, 34));
      jobsToolBar.setMinimumSize(new java.awt.Dimension(302, 34));
      jobsToolBar.setPreferredSize(new java.awt.Dimension(302, 34));

      selectAllButton.setText("Select-All");
      selectAllButton.setFocusable(false);
      selectAllButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      selectAllButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      selectAllButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            selectAllButtonActionPerformed(evt);
         }
      });
      jobsToolBar.add(selectAllButton);

      deselectAllButton.setText("Deselect-All");
      deselectAllButton.setFocusable(false);
      deselectAllButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      deselectAllButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      deselectAllButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            deselectAllButtonActionPerformed(evt);
         }
      });
      jobsToolBar.add(deselectAllButton);

      selectReadyButton.setText("Select-Ready");
      selectReadyButton.setFocusable(false);
      selectReadyButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      selectReadyButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      selectReadyButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            selectReadyButtonActionPerformed(evt);
         }
      });
      jobsToolBar.add(selectReadyButton);

      deselectReadyButton.setText("Deselect-Ready");
      deselectReadyButton.setFocusable(false);
      deselectReadyButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      deselectReadyButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      deselectReadyButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            deselectReadyButtonActionPerformed(evt);
         }
      });
      jobsToolBar.add(deselectReadyButton);

      removeButton.setText("Remove");
      removeButton.setMaximumSize(new java.awt.Dimension(61, 27));
      removeButton.setMinimumSize(new java.awt.Dimension(61, 27));
      removeButton.setPreferredSize(new java.awt.Dimension(61, 27));
      removeButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            removeButtonActionPerformed(evt);
         }
      });
      jobsToolBar.add(removeButton);

      validateButton.setText("Verify Files");
      validateButton.setFocusable(false);
      validateButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      validateButton.setMaximumSize(new java.awt.Dimension(95, 27));
      validateButton.setMinimumSize(new java.awt.Dimension(95, 27));
      validateButton.setPreferredSize(new java.awt.Dimension(95, 27));
      validateButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      validateButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            validateButtonActionPerformed(evt);
         }
      });
      jobsToolBar.add(validateButton);

      jobManagerButton.setText("Job Manager");
      jobManagerButton.setFocusable(false);
      jobManagerButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      jobManagerButton.setMaximumSize(new java.awt.Dimension(91, 27));
      jobManagerButton.setMinimumSize(new java.awt.Dimension(91, 27));
      jobManagerButton.setPreferredSize(new java.awt.Dimension(91, 27));
      jobManagerButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      jobManagerButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            jobManagerButtonActionPerformed(evt);
         }
      });
      jobsToolBar.add(jobManagerButton);

      appsToolBar.setBackground(java.awt.Color.lightGray);
      appsToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
      appsToolBar.setFloatable(false);
      appsToolBar.setRollover(true);
      appsToolBar.setMaximumSize(new java.awt.Dimension(406, 34));
      appsToolBar.setMinimumSize(new java.awt.Dimension(406, 34));
      appsToolBar.setPreferredSize(new java.awt.Dimension(406, 34));

      killButton.setText("Kill Mpifxcorr");
      killButton.setFocusable(false);
      killButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      killButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      killButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            killButtonActionPerformed(evt);
         }
      });
      appsToolBar.add(killButton);

      jSplitPane1.setBorder(javax.swing.BorderFactory.createEtchedBorder(new java.awt.Color(153, 255, 255), null));

      jobsTabbedPane.addMouseListener(new java.awt.event.MouseAdapter() {
         public void mouseClicked(java.awt.event.MouseEvent evt) {
            jobsTabbedPaneMouseClicked(evt);
         }
      });

      jobsPanel.setBorder(javax.swing.BorderFactory.createEtchedBorder());

      jobsScrollPane.setBorder(javax.swing.BorderFactory.createEtchedBorder());

      jobsTable.setBorder(javax.swing.BorderFactory.createEtchedBorder());
      jobsTable.setModel(new javax.swing.table.DefaultTableModel(
         new Object [][] {
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null}
         },
         new String [] {
            "Job Name", "Queued", "Project", "State", "Num of Antennas", "Start Time", "Duration (seconds)", "Complete(%)", "Path"
         }
      ) {
         Class[] types = new Class [] {
            java.lang.String.class, java.lang.Boolean.class, java.lang.String.class, java.lang.String.class, java.lang.Byte.class, java.lang.String.class, java.lang.Integer.class, java.lang.String.class, java.lang.String.class
         };
         boolean[] canEdit = new boolean [] {
            false, true, false, false, false, false, false, false, false
         };

         public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
         }

         public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
         }
      });
      jobsTable.setDragEnabled(true);
      jobsTable.addMouseListener(new java.awt.event.MouseAdapter() {
         public void mouseClicked(java.awt.event.MouseEvent evt) {
            jobsTableMouseClicked(evt);
         }
         public void mouseReleased(java.awt.event.MouseEvent evt) {
            jobsTableMouseReleased(evt);
         }
      });
      jobsTable.addMouseMotionListener(new java.awt.event.MouseMotionAdapter() {
         public void mouseDragged(java.awt.event.MouseEvent evt) {
            mouseDraggedHandler(evt);
         }
      });
      jobsScrollPane.setViewportView(jobsTable);
      jobsTable.getColumnModel().getColumn(3).setCellRenderer(new DiFXJobTableCellRenderer());

      javax.swing.GroupLayout jobsPanelLayout = new javax.swing.GroupLayout(jobsPanel);
      jobsPanel.setLayout(jobsPanelLayout);
      jobsPanelLayout.setHorizontalGroup(
         jobsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addComponent(jobsScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 737, Short.MAX_VALUE)
      );
      jobsPanelLayout.setVerticalGroup(
         jobsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addComponent(jobsScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 338, Short.MAX_VALUE)
      );

      jobsTabbedPane.addTab("Jobs Available", jobsPanel);

      textScrollPane.setAutoscrolls(true);
      textScrollPane.addMouseListener(new java.awt.event.MouseAdapter() {
         public void mouseExited(java.awt.event.MouseEvent evt) {
            textScrollPaneMouseExited(evt);
         }
      });

      allMessagesTextArea.setColumns(20);
      allMessagesTextArea.setRows(5);
      allMessagesTextArea.setWrapStyleWord(true);
      allMessagesTextArea.setDoubleBuffered(true);
      textScrollPane.setViewportView(allMessagesTextArea);

      javax.swing.GroupLayout textPanelLayout = new javax.swing.GroupLayout(textPanel);
      textPanel.setLayout(textPanelLayout);
      textPanelLayout.setHorizontalGroup(
         textPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addComponent(textScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 741, Short.MAX_VALUE)
      );
      textPanelLayout.setVerticalGroup(
         textPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addComponent(textScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 342, Short.MAX_VALUE)
      );

      jobsTabbedPane.addTab("DiFX Messages", textPanel);

      productionQueueTable.setBorder(javax.swing.BorderFactory.createEtchedBorder());
      productionQueueTable.setModel(new javax.swing.table.DefaultTableModel(
         new Object [][] {
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null},
            {null, null, null, null, null, null, null, null, null, null, null, null, null, null}
         },
         new String [] {
            "Job", "", "Projects", "Priority", "Obs Start (UT)", "Obs Length", "Corr Length", "Data Size (MB)", "Date Rate", "Speed Up", "Version", "Num Ants", "Foreign Ants", "Hours Remaining"
         }
      ) {
         Class[] types = new Class [] {
            java.lang.String.class, java.lang.Boolean.class, java.lang.String.class, java.lang.Integer.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.Float.class, java.lang.Float.class, java.lang.Float.class, java.lang.String.class, java.lang.Integer.class, java.lang.Integer.class, java.lang.String.class
         };
         boolean[] canEdit = new boolean [] {
            false, true, false, false, false, false, false, false, false, false, false, false, false, false
         };

         public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
         }

         public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
         }
      });
      productionQueueTable.setRowSelectionAllowed(false);
      productionQueueScrollPane.setViewportView(productionQueueTable);

      javax.swing.GroupLayout productionQueuePanelLayout = new javax.swing.GroupLayout(productionQueuePanel);
      productionQueuePanel.setLayout(productionQueuePanelLayout);
      productionQueuePanelLayout.setHorizontalGroup(
         productionQueuePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addGap(0, 741, Short.MAX_VALUE)
         .addGroup(productionQueuePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(productionQueueScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 741, Short.MAX_VALUE))
      );
      productionQueuePanelLayout.setVerticalGroup(
         productionQueuePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addGap(0, 342, Short.MAX_VALUE)
         .addGroup(productionQueuePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(productionQueueScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 342, Short.MAX_VALUE))
      );

      jobsTabbedPane.addTab("Production Queue", productionQueuePanel);

      disksTree.setAutoscrolls(true);
      disksTree.setShowsRootHandles(true);
      disksScrollPane.setViewportView(disksTree);

      javax.swing.GroupLayout disksPanelLayout = new javax.swing.GroupLayout(disksPanel);
      disksPanel.setLayout(disksPanelLayout);
      disksPanelLayout.setHorizontalGroup(
         disksPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addComponent(disksScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 741, Short.MAX_VALUE)
      );
      disksPanelLayout.setVerticalGroup(
         disksPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addComponent(disksScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 342, Short.MAX_VALUE)
      );

      jobsTabbedPane.addTab("Disk Pack", disksPanel);

      jSplitPane1.setLeftComponent(jobsTabbedPane);
      jobsTabbedPane.getAccessibleContext().setAccessibleName("Job in Queue");

      queueStatePanel.setBorder(javax.swing.BorderFactory.createEtchedBorder(new java.awt.Color(153, 255, 255), null));

      jobProgressBar.setForeground(java.awt.Color.green);
      jobProgressBar.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED), "Progress . . .", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 11))); // NOI18N
      jobProgressBar.setStringPainted(true);

      queueStateLabel.setHorizontalAlignment(javax.swing.SwingConstants.LEFT);
      queueStateLabel.setText("State");

      queueStateTextField.setBackground(new java.awt.Color(238, 238, 238));
      queueStateTextField.setHorizontalAlignment(javax.swing.JTextField.CENTER);
      queueStateTextField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

      noJobsLabel.setText("No. of Jobs");

      jobsCountTextField.setBackground(new java.awt.Color(238, 238, 238));
      jobsCountTextField.setHorizontalAlignment(javax.swing.JTextField.CENTER);
      jobsCountTextField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

      totalTimeLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
      totalTimeLabel.setText("Total Time:");

      totalTimeTextField.setEditable(false);
      totalTimeTextField.setHorizontalAlignment(javax.swing.JTextField.CENTER);
      totalTimeTextField.setText("00:00:00");
      totalTimeTextField.setBorder(null);

      currentLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
      currentLabel.setText("Current:");

      wallTimeTextField.setBackground(new java.awt.Color(238, 238, 238));
      wallTimeTextField.setHorizontalAlignment(javax.swing.JTextField.CENTER);
      wallTimeTextField.setText("00:00:00");
      wallTimeTextField.setBorder(null);

      timeRemainingLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
      timeRemainingLabel.setText("Remaining:");

      timeRemainingTextField.setBackground(new java.awt.Color(238, 238, 238));
      timeRemainingTextField.setHorizontalAlignment(javax.swing.JTextField.CENTER);
      timeRemainingTextField.setText("00:00:00");
      timeRemainingTextField.setBorder(null);

      javax.swing.GroupLayout queueStatePanelLayout = new javax.swing.GroupLayout(queueStatePanel);
      queueStatePanel.setLayout(queueStatePanelLayout);
      queueStatePanelLayout.setHorizontalGroup(
         queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addComponent(jobProgressBar, javax.swing.GroupLayout.DEFAULT_SIZE, 354, Short.MAX_VALUE)
         .addGroup(queueStatePanelLayout.createSequentialGroup()
            .addContainerGap()
            .addGroup(queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
               .addComponent(queueStateLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 69, Short.MAX_VALUE)
               .addComponent(noJobsLabel))
            .addGap(12, 12, 12)
            .addGroup(queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
               .addComponent(jobsCountTextField, javax.swing.GroupLayout.PREFERRED_SIZE, 65, javax.swing.GroupLayout.PREFERRED_SIZE)
               .addComponent(queueStateTextField, javax.swing.GroupLayout.PREFERRED_SIZE, 79, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
               .addComponent(currentLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 72, Short.MAX_VALUE)
               .addComponent(totalTimeLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 72, Short.MAX_VALUE)
               .addComponent(timeRemainingLabel, javax.swing.GroupLayout.Alignment.TRAILING))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
               .addComponent(wallTimeTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 74, Short.MAX_VALUE)
               .addComponent(totalTimeTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 74, Short.MAX_VALUE)
               .addComponent(timeRemainingTextField, javax.swing.GroupLayout.DEFAULT_SIZE, 74, Short.MAX_VALUE))
            .addContainerGap())
      );
      queueStatePanelLayout.setVerticalGroup(
         queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addGroup(queueStatePanelLayout.createSequentialGroup()
            .addComponent(jobProgressBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
            .addGroup(queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
               .addComponent(queueStateLabel)
               .addComponent(totalTimeTextField, javax.swing.GroupLayout.PREFERRED_SIZE, 19, javax.swing.GroupLayout.PREFERRED_SIZE)
               .addComponent(totalTimeLabel)
               .addComponent(queueStateTextField))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
               .addComponent(noJobsLabel)
               .addComponent(jobsCountTextField)
               .addComponent(wallTimeTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
               .addComponent(currentLabel))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(queueStatePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
               .addComponent(timeRemainingLabel)
               .addComponent(timeRemainingTextField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addContainerGap(16, Short.MAX_VALUE))
      );

      runQueueScrollPane.setBorder(javax.swing.BorderFactory.createEtchedBorder(new java.awt.Color(153, 255, 255), null));
      runQueueScrollPane.setViewportBorder(javax.swing.BorderFactory.createEtchedBorder());
      runQueueScrollPane.setAutoscrolls(true);

      runQueueTable.setBorder(javax.swing.BorderFactory.createEtchedBorder());
      runQueueTable.setModel(new javax.swing.table.DefaultTableModel(
         new Object [][] {

         },
         new String [] {
            "Job Name", "State", "Complete (%)", "Path", "Duration"
         }
      ) {
         Class[] types = new Class [] {
            java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.Integer.class
         };
         boolean[] canEdit = new boolean [] {
            false, false, false, false, false
         };

         public Class getColumnClass(int columnIndex) {
            return types [columnIndex];
         }

         public boolean isCellEditable(int rowIndex, int columnIndex) {
            return canEdit [columnIndex];
         }
      });
      runQueueTable.setDragEnabled(true);
      runQueueScrollPane.setViewportView(runQueueTable);
      runQueueTable.getColumnModel().getColumn(1).setCellRenderer(new DiFXJobTableCellRenderer());
      runQueueTable.getColumnModel().getColumn(3).setMinWidth(0);
      runQueueTable.getColumnModel().getColumn(3).setMaxWidth(0);
      runQueueTable.getColumnModel().getColumn(4).setMinWidth(0);
      runQueueTable.getColumnModel().getColumn(4).setMaxWidth(0);

      queueToolBar.setBackground(java.awt.Color.lightGray);
      queueToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
      queueToolBar.setFloatable(false);
      queueToolBar.setRollover(true);

      startButton.setText("Start");
      startButton.setFocusable(false);
      startButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      startButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      startButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            startButtonActionPerformed(evt);
         }
      });
      queueToolBar.add(startButton);

      pauseButton.setText("Pause");
      pauseButton.setFocusable(false);
      pauseButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      pauseButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      pauseButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            pauseButtonActionPerformed(evt);
         }
      });
      queueToolBar.add(pauseButton);

      stopButton.setText("Stop");
      stopButton.setFocusable(false);
      stopButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      stopButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      stopButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            stopButtonActionPerformed(evt);
         }
      });
      queueToolBar.add(stopButton);

      clearButton.setText("Clear Complete");
      clearButton.setMaximumSize(new java.awt.Dimension(125, 27));
      clearButton.setMinimumSize(new java.awt.Dimension(125, 27));
      clearButton.setPreferredSize(new java.awt.Dimension(50, 27));
      clearButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            clearButtonActionPerformed(evt);
         }
      });
      queueToolBar.add(clearButton);

      readyButton.setText("Ready");
      readyButton.setFocusable(false);
      readyButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      readyButton.setMaximumSize(new java.awt.Dimension(50, 27));
      readyButton.setMinimumSize(new java.awt.Dimension(50, 27));
      readyButton.setPreferredSize(new java.awt.Dimension(50, 27));
      readyButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      readyButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            readyButtonActionPerformed(evt);
         }
      });
      queueToolBar.add(readyButton);

      resetButton.setText("Reset");
      resetButton.setFocusable(false);
      resetButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
      resetButton.setMaximumSize(new java.awt.Dimension(48, 27));
      resetButton.setMinimumSize(new java.awt.Dimension(48, 27));
      resetButton.setPreferredSize(new java.awt.Dimension(48, 27));
      resetButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
      resetButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            resetButtonActionPerformed(evt);
         }
      });
      queueToolBar.add(resetButton);

      javax.swing.GroupLayout runQueuePanelLayout = new javax.swing.GroupLayout(runQueuePanel);
      runQueuePanel.setLayout(runQueuePanelLayout);
      runQueuePanelLayout.setHorizontalGroup(
         runQueuePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addComponent(queueToolBar, javax.swing.GroupLayout.DEFAULT_SIZE, 358, Short.MAX_VALUE)
         .addComponent(queueStatePanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
         .addGroup(runQueuePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(runQueueScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 358, Short.MAX_VALUE))
      );
      runQueuePanelLayout.setVerticalGroup(
         runQueuePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, runQueuePanelLayout.createSequentialGroup()
            .addComponent(queueStatePanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 197, Short.MAX_VALUE)
            .addComponent(queueToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
         .addGroup(runQueuePanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, runQueuePanelLayout.createSequentialGroup()
               .addGap(135, 135, 135)
               .addComponent(runQueueScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 202, Short.MAX_VALUE)
               .addGap(32, 32, 32)))
      );

      jSplitPane1.setRightComponent(runQueuePanel);

      bottomButtonPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));

      printTabButton.setText("Print Tab");
      printTabButton.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED, null, new java.awt.Color(153, 153, 153)));
      printTabButton.setMaximumSize(new java.awt.Dimension(75, 19));
      printTabButton.setMinimumSize(new java.awt.Dimension(75, 19));
      printTabButton.setPreferredSize(new java.awt.Dimension(75, 19));
      printTabButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            printTabButtonActionPerformed(evt);
         }
      });

      CloseButton.setText("Close");
      CloseButton.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED));
      CloseButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            CloseButtonActionPerformed(evt);
         }
      });

      loadQueueButton.setText("Load");
      loadQueueButton.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED, null, new java.awt.Color(153, 153, 153)));
      loadQueueButton.setMaximumSize(new java.awt.Dimension(75, 19));
      loadQueueButton.setMinimumSize(new java.awt.Dimension(75, 19));
      loadQueueButton.setPreferredSize(new java.awt.Dimension(75, 19));
      loadQueueButton.addActionListener(new java.awt.event.ActionListener() {
         public void actionPerformed(java.awt.event.ActionEvent evt) {
            loadQueueButtonActionPerformed(evt);
         }
      });

      alertsScrollPane.setBackground(new java.awt.Color(153, 204, 255));
      alertsScrollPane.setBorder(javax.swing.BorderFactory.createEtchedBorder());
      alertsScrollPane.setViewportBorder(javax.swing.BorderFactory.createEtchedBorder());
      alertsScrollPane.setAutoscrolls(true);

      alertsTextArea.setBackground(new java.awt.Color(51, 51, 51));
      alertsTextArea.setColumns(20);
      alertsTextArea.setForeground(new java.awt.Color(255, 255, 255));
      alertsTextArea.setLineWrap(true);
      alertsTextArea.setSelectionColor(new java.awt.Color(255, 255, 255));
      alertsScrollPane.setViewportView(alertsTextArea);

      javax.swing.GroupLayout bottomButtonPanelLayout = new javax.swing.GroupLayout(bottomButtonPanel);
      bottomButtonPanel.setLayout(bottomButtonPanelLayout);
      bottomButtonPanelLayout.setHorizontalGroup(
         bottomButtonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, bottomButtonPanelLayout.createSequentialGroup()
            .addComponent(alertsScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 983, Short.MAX_VALUE)
            .addGap(18, 18, 18)
            .addGroup(bottomButtonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING)
               .addComponent(loadQueueButton, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE)
               .addComponent(CloseButton, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE)
               .addComponent(printTabButton, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addContainerGap())
      );
      bottomButtonPanelLayout.setVerticalGroup(
         bottomButtonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addGroup(bottomButtonPanelLayout.createSequentialGroup()
            .addComponent(loadQueueButton, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(printTabButton, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 12, Short.MAX_VALUE)
            .addComponent(CloseButton, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addContainerGap())
         .addComponent(alertsScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 123, Short.MAX_VALUE)
      );

      javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
      getContentPane().setLayout(layout);
      layout.setHorizontalGroup(
         layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
            .addComponent(jobsToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, 619, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 398, Short.MAX_VALUE)
            .addComponent(appsToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, 101, javax.swing.GroupLayout.PREFERRED_SIZE))
         .addComponent(bottomButtonPanel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
         .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 1118, Short.MAX_VALUE))
      );
      layout.setVerticalGroup(
         layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
         .addGroup(layout.createSequentialGroup()
            .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
               .addComponent(appsToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
               .addComponent(jobsToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, 34, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 384, Short.MAX_VALUE)
            .addComponent(bottomButtonPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
         .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
               .addGap(45, 45, 45)
               .addComponent(jSplitPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 373, Short.MAX_VALUE)
               .addGap(127, 127, 127)))
      );

      pack();
   }// </editor-fold>//GEN-END:initComponents

    private void CloseButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_CloseButtonActionPerformed

       // stop listening for updates
       detachListener();

       // close window
       mInstance = null;
       this.dispose();

    }//GEN-LAST:event_CloseButtonActionPerformed

    private void removeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_removeButtonActionPerformed

      // -- Stop updates and remove selected jobs from the queue

      // stop updates
      mUpdate = false;

      // Save the indexes of the rows/jobs selected to run
      int[] jobsToRemove = jobsTable.getSelectedRows();
      int jobCount       = jobsToRemove.length;

      // Verify data structures exist, and start removing jobs
      if ( (jobCount > 0) && (mDataModel != null) )
      {
         Queue queue = mDataModel.getQueue();
         if ( (queue != null) && (queue.isEmpty() != true) )
         {
             // Raise dialog, verify purge jobs
             JFrame frame     = null;
             Object[] options = {"Yes", "No"};
             int opt = JOptionPane.showOptionDialog(frame,
                     "Selected jobs will be removed from the jobs available table. \n" +
                     "Do you wish to continue and remove selected jobs?",
                     "Queue Manager - Remove",
                     JOptionPane.YES_NO_OPTION,
                     JOptionPane.QUESTION_MESSAGE,
                     null, options, options[1]);

             // No, do not overwrite option selected
             if (opt == 0)
             {
                // Only purge when queue is idle
                if (queue.isQueueIdle() == true)
                {
                   // Create a chache of jobs to be purged
                   ArrayList<Job> jobs = queue.getJobs();
                   if ( (jobs != null) && (jobs.isEmpty() != true) )
                   {
                      // Create cache of selected job name and path
                      int rows[] = jobsTable.getSelectedRows();

                      // convert rows from view into tablemodel perspective
                      for (int i = 0; i < rows.length; i++)
                      {
                         rows[i] = jobsTable.convertRowIndexToModel(rows[i]);
                      }

                      // Is a row selected?
                      if (rows.length > 0)
                      {
                         String[] projectPaths = new String[rows.length];
                         String[] jobNames     = new String[rows.length];

                         DefaultTableModel model = (DefaultTableModel) (jobsTable.getModel());

                         // Jobs are ordered in the array similar to table ordering
                         int numJobs = rows.length;
                         for (int index = 0; index < numJobs; index++)
                         {
                            String jobName      = (model.getValueAt(rows[index], 0)).toString();//job.getObjName();
                            String projectPath  = (model.getValueAt(rows[index], 8)).toString();//job.getProjectPath();
                            projectPaths[index] = projectPath;
                            jobNames[index]     = jobName;
                         }

                         // march through the rows of jobs, and pretty up the table
                         for (int index = 0; index < jobsTable.getRowCount(); index++)
                         {
                            //int row = rows[jobIndex];
                            model.setValueAt("", index, 0);
                            model.setValueAt(false, index, 1);
                            model.setValueAt("", index, 2);
                            model.setValueAt("", index, 3);
                            model.setValueAt("", index, 4);
                            model.setValueAt("", index, 5);
                            model.setValueAt("", index, 6);
                            model.setValueAt("", index, 7);
                            model.setValueAt("", index, 8);
                         }

                         // march through the rows of jobs, and delete from datamodel
                         for (int jobIndex = 0; jobIndex < numJobs; jobIndex++)
                         {
                            // Remove job from jobs queue in data model
                            updateDatamodel(projectPaths[jobIndex], jobNames[jobIndex], "Delete");
                         }

                         // Update production queue tab
                         // updateProductionQueueTable(queue);
                      }
                      else // -- row not selected
                      {
                         // Raise dialog
                         JOptionPane.showMessageDialog(frame,
                                "Job not selected, select job.",
                                "Queue Manager",
                                JOptionPane.WARNING_MESSAGE);
                      }
                   }
                   else // -- job queue is empty, no jobs
                   {
                      // Raise dialog
                      JOptionPane.showMessageDialog(frame,
                             "Job queue is empty.",
                             "Queue Manager",
                             JOptionPane.WARNING_MESSAGE);
                   }
                }
                else // -- Jos queue is not paused
                {
                   // Raise dialog
                   JOptionPane.showMessageDialog(frame,
                          "Job queue not paused, wait.",
                          "Queue Manager",
                          JOptionPane.WARNING_MESSAGE);
                }

             }
             else // -- do not remove, deselect
             {
                jobsTable.clearSelection();
             } // -- if (opt == 0)
         }
         else // -- jobQueue is empty
         {
             // Raise dialog
             JFrame frame = null;
             JOptionPane.showMessageDialog(frame,
                     "Job queue is empty. \n" +
                     "Add jobs to the queue.",
                     "Job Queue - Empty",
                     JOptionPane.ERROR_MESSAGE);
         }

      } // -- if ( (jobCount > 0) && (mDataModel != null) )

      // allow update
      mUpdate = true;

      // clear selection
      jobsTable.clearSelection();

    }//GEN-LAST:event_removeButtonActionPerformed

    private void jobManagerButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_jobManagerButtonActionPerformed

       // get the job manager associated with the selected job
       int row = jobsTable.getSelectedRow();
       if (row != -1)
       {
          // get name of the job
          String jobName = jobsTable.getValueAt(row, 0).toString();

          // Display the GUI,
          JobManagerUI theJM = JobManagerUI.instance(mDataModel, mController, jobName);
          if (theJM != null)
          {
            theJM.attachListenerCallback();
            theJM.setVisible(true);
          }
       }

       // clear selection
       jobsTable.clearSelection();

    }//GEN-LAST:event_jobManagerButtonActionPerformed


    private void validateButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_validateButtonActionPerformed
    {//GEN-HEADEREND:event_validateButtonActionPerformed

      // -- Verify input files exist for selected job

      // stop update
      mUpdate = false;

      // Remove all the jobs from the queue
      if (mDataModel != null)
      {
          // Verify the job, raise failure dialog
          int row = jobsTable.getSelectedRow();
          if (row != -1)
          {
             // get name of the job
             mJobName     = jobsTable.getValueAt(row, 0).toString();
             mProjectPath = jobsTable.getValueAt(row, 8).toString();

             // Create start message: verify and start the job
             Job job = mDataModel.getJob(mJobName, mProjectPath);
             if ((job != null) && (job.verifyInputFiles() == true))
             {
                // Raise dialog
                JFrame frame = null;
                JOptionPane.showMessageDialog(frame,
                        "Job passed file verification.\n" + "The .uvw, .input, .calc and .delay files exist.",
                        "Queue Manager",
                        JOptionPane.INFORMATION_MESSAGE);
             }
             else
             {
                // Raise dialog
                JFrame frame = null;
                JOptionPane.showMessageDialog(frame,
                        "Job failed file verification. \n" + "Please check .uvw, .input, .calc and .delay files.",
                        "Queue Manager",
                        JOptionPane.WARNING_MESSAGE);

             } // -- if ((job != null) && (job.verifyInputFiles() == true))

             // clean up
             job = null;

          } // -- if (row != -1)

      } // -- if (mDataModel != null)

      // allow update
      mUpdate = true;

      // clear selection
      jobsTable.clearSelection();

}//GEN-LAST:event_validateButtonActionPerformed

    private void clearButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_clearButtonActionPerformed
    {//GEN-HEADEREND:event_clearButtonActionPerformed

      // -- Clear all the complete jobs from queue

      // stop update
      mUpdate = false;

      // -- remove all completed jobs from the run queue table
      DefaultTableModel queueTableModel = (DefaultTableModel)runQueueTable.getModel();
      int row = ( queueTableModel.getRowCount() - 1);
      while ( row >= 0 )
      {
         String state = ((String)queueTableModel.getValueAt(row, 1));
         if (state.equalsIgnoreCase("COMPLETE"))
         {
            String jobName  = (String)queueTableModel.getValueAt(row, 0);
            String projName = (String)queueTableModel.getValueAt(row, 3);
            queueTableModel.setValueAt("", row, 0);
            queueTableModel.setValueAt("", row, 1);
            queueTableModel.setValueAt("", row, 2);
            queueTableModel.setValueAt("", row, 3);
            queueTableModel.removeRow(row);
            updateDatamodel(projName, jobName, "DeQueue");
         }

         // get next
         --row;

      } // -- while ( row >= 0 )

      // allow update
      mUpdate = true;

    }//GEN-LAST:event_clearButtonActionPerformed

    private void resetButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_resetButtonActionPerformed
    {//GEN-HEADEREND:event_resetButtonActionPerformed

       // stop updates
       mUpdate = false;

       // reset the queue?
       if (mDataModel != null)
       {
          Queue queue = mDataModel.getQueue();
          if ( (queue != null) && (queue.isEmpty() != true) )
          {
            // Raise dialog, verify rest of queue
            Object[] options ={"Yes", "No"};
            JFrame frame = null;
            int opt = JOptionPane.showOptionDialog(frame,
                                 "Do you wish to reset the queue?",
                                 "Job Queue - Reset",
                                 JOptionPane.YES_NO_OPTION,
                                 JOptionPane.QUESTION_MESSAGE,
                                 null, options, options[1]);

            // Reset the queue back to idle
            if (opt == 0)
            {
               try
               {
                  queue.accept();
               }
               catch (UnsupportedOperationException ex)
               {
                  String error = ex.toString();
                  error = error.substring(error.indexOf(":")+1);

                  // Raise dialog, queue is currently running
                  Object[] optionsEx = {"OK"};
                  opt = JOptionPane.showOptionDialog(frame,
                           error,
                           "Job Queue",
                           JOptionPane.OK_OPTION,
                           JOptionPane.ERROR_MESSAGE,
                           null, optionsEx, options[0]);
               }
               
            } // -- if (opt == 0)
         }
         else // -- queue does not exist
         {
             // Raise dialog
             JFrame frame = null;
             JOptionPane.showMessageDialog(frame,
                     "Job queue is empty. \n" +
                     "Add jobs to the queue.",
                     "Job Queue - Reset",
                     JOptionPane.ERROR_MESSAGE);
         }

         // clean up
         queue = null;

       } // -- if (mDataModel != null)

       // enable updates
       mUpdate = true;
       
       // clear selection
       jobsTable.clearSelection();

}//GEN-LAST:event_resetButtonActionPerformed

    private void readyButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_readyButtonActionPerformed
    {//GEN-HEADEREND:event_readyButtonActionPerformed
      // -- Change state to ready

      // stop updates
      mUpdate = false;

      // -- Step through the selected jobs in the run queue table
      int rows[]  = runQueueTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         //rows[i] = jobsTable.convertRowIndexToModel(rows[i]);
         rows[i] = runQueueTable.convertRowIndexToModel(rows[i]);
      }

      // Step through the selected rows and get the job from the data model
      DefaultTableModel tableModel = (DefaultTableModel) runQueueTable.getModel();
      for (int i = 0; i < numRows; i++)
      {
         // get job and path name
         String jobName  = (String)tableModel.getValueAt(rows[i], 0);
         String projPath = (String)tableModel.getValueAt(rows[i], 3);

         Job job = (mDataModel.getQueue()).getJob(jobName, projPath);
         if (job != null)
         {
            // job found, create message and send
            if ( jobName.equalsIgnoreCase(job.getObjName())  &&
                 projPath.equalsIgnoreCase(job.getJobPath()) )
            {
               // Create DOI message and service the data model
               ObjectFactory factory = new ObjectFactory();
               Header header = factory.createHeader();
               header.setFrom("DOIView");
               header.setTo("DOIModel");
               header.setMpiProcessId("0");
               header.setIdentifier("doi");
               header.setType("DOIMessage");

               // fill in the resource data
               DoiJobCommand jobCommand = factory.createDoiJobCommand();
               jobCommand.setJobName(job.getObjName());
               jobCommand.setProject(job.getProjectName());
               jobCommand.setFullPath(job.getProjectPath());
               jobCommand.setCommand("ResetReady");

               // set resource data into the body
               Body body = factory.createBody();
               body.setDoiJobCommand(jobCommand);

               // update the data model with DifxMessage
               DifxMessage difxMsg = factory.createDifxMessage();
               difxMsg.setHeader(header);
               difxMsg.setBody(body);

               // process the message
               serviceDataModel(difxMsg);

               // clean up
               factory    = null;
               header     = null;
               jobCommand = null;
               body       = null;
               difxMsg    = null;

            } // -- if ( jobName.equalsIgnoreCase(job.getObjName())  &&
              // --      projPath.equalsIgnoreCase(job.getJobPath()) )

         } // -- if (job != null)

      } // -- for (int i = 0; i < numRows; i++)

      // Allow updates
      runQueueTable.clearSelection();
      mUpdate = true;
 
    }//GEN-LAST:event_readyButtonActionPerformed

    private void loadQueueButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_loadQueueButtonActionPerformed
    {//GEN-HEADEREND:event_loadQueueButtonActionPerformed

			// Get tab selected
			int sel = jobsTabbedPane.getSelectedIndex();


			//Update the jobs queu, production table or disk list tree
			if ( sel == 0 )
			{
		     loadJobsQueueFromDB();
			}
			if ( sel == 2 )
			{
			   updateProductionQueueTable();
			}
			else if ( sel == 3 )
			{
			    updateDiskListTree();
			}
       
    }//GEN-LAST:event_loadQueueButtonActionPerformed

    private void printTabButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_printTabButtonActionPerformed
    {//GEN-HEADEREND:event_printTabButtonActionPerformed

       // Determine the tab selected and display
       int tabSelected = jobsTabbedPane.getSelectedIndex();
       if (tabSelected == 1)
       {
          printAlertsText();
       }
       else if (tabSelected == 2)
       {
          displayProductionQueue();
       }
       else if (tabSelected == 3)
       {
          displayDiskPacks();
       }

}//GEN-LAST:event_printTabButtonActionPerformed

    private void mouseDraggedHandler(java.awt.event.MouseEvent evt)//GEN-FIRST:event_mouseDraggedHandler
    {//GEN-HEADEREND:event_mouseDraggedHandler
       int fromRow = jobsTable.getSelectedRow();
       if (fromRow >= 0)
       {
          mDraggingRow = true;
          int rowHeight = jobsTable.getRowHeight();
          int middleOfSelectedRow = (rowHeight*fromRow) + (rowHeight/2);
          int toRow = -1;
          int yMousePoint = (int)evt.getPoint().getY();
          if (yMousePoint < (middleOfSelectedRow - rowHeight))
          {
             // move row up
             toRow = fromRow - 1;
          }
          else if (yMousePoint > (middleOfSelectedRow + rowHeight))
          {
             // Move row down
             toRow = fromRow + 1;
          }

          if ( (toRow >= 0) && (toRow < jobsTable.getRowCount()) )
          {
             TableModel model = jobsTable.getModel();
             for (int i = 0; i < model.getColumnCount(); i++)
             {
                Object fromValue = model.getValueAt(fromRow, i);
                Object toValue   = model.getValueAt(toRow, i);

                model.setValueAt(toValue, fromRow, i);
                model.setValueAt(fromValue, toRow, i);
             }

             jobsTable.setRowSelectionInterval(toRow, toRow);
             mStartDragPoint = yMousePoint;

          }

          mDyOffset = (mStartDragPoint - yMousePoint) * -1;
          jobsTable.repaint();
       }

}//GEN-LAST:event_mouseDraggedHandler

    private void jobsTabbedPaneMouseClicked(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jobsTabbedPaneMouseClicked
    {//GEN-HEADEREND:event_jobsTabbedPaneMouseClicked

/*
       // Get tab selected
       JTabbedPane pane = (JTabbedPane)evt.getSource();
       int sel = pane.getSelectedIndex();


			 //Update the production table and disk list tree
       //updateJobsAvailableTable(mDataModel.getQueue());
			 if ( sel == 2 )
			 {
				 updateProductionQueueTable(mDataModel.getQueue());
			 }

			 if ( sel == 3 )
			 {
         updateDiskListTree();
			 }
*/
			
    }//GEN-LAST:event_jobsTabbedPaneMouseClicked

    private void jobsTableMouseClicked(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jobsTableMouseClicked
    {//GEN-HEADEREND:event_jobsTableMouseClicked

       /*
       DefaultTableModel jobsTableModel  = (DefaultTableModel)jobsTable.getModel();
       DefaultTableModel queueTableModel = (DefaultTableModel)runQueueTable.getModel();
       int row = jobsTable.rowAtPoint(evt.getPoint());
       int col = jobsTable.columnAtPoint(evt.getPoint());

       // check if queued check box has clicked
       if (col == 1)
       {
          if ( (Boolean)jobsTableModel.getValueAt(row, 1) == true )
          {
             queueTableModel.addRow(new Object[]{"", "", "", ""});
             int cnt = queueTableModel.getRowCount();
             queueTableModel.setValueAt(jobsTableModel.getValueAt(row, 0), cnt-1, 0);
             queueTableModel.setValueAt(jobsTableModel.getValueAt(row, 3), cnt-1, 1);
             queueTableModel.setValueAt(jobsTableModel.getValueAt(row, 7), cnt-1, 2);
             queueTableModel.setValueAt(jobsTableModel.getValueAt(row, 8), cnt-1, 3);
             updateDatamodel((String)jobsTableModel.getValueAt(row, 8),
                             (String)jobsTableModel.getValueAt(row, 0), "EnQueue");
          }
          else // -- remove from run queue
          {
             for (int i = 0; i < queueTableModel.getRowCount(); i++)
             {
                String jobName  = (String)jobsTableModel.getValueAt(row, 0);
                String projName = (String)jobsTableModel.getValueAt(row, 8);
                if ( jobName.equalsIgnoreCase((String)queueTableModel.getValueAt(i, 0))  &&
                     projName.equalsIgnoreCase((String)queueTableModel.getValueAt(i, 3)) )
                {
                  queueTableModel.setValueAt("", i, 0);
                  queueTableModel.setValueAt("", i, 1);
                  queueTableModel.setValueAt("", i, 2);
                  queueTableModel.setValueAt("", i, 3);
                  queueTableModel.removeRow(i);
                  updateDatamodel(projName, jobName, "DeQueue");
                }

             } // -- for (int i = 0; i < queueTableModel.getRowCount(); i++)

          } // -- if ( (Boolean)jobsTableModel.getValueAt(row, 1) == true )

       } // -- if (col == 1)
       */

    }//GEN-LAST:event_jobsTableMouseClicked

    private void jobsTableMouseReleased(java.awt.event.MouseEvent evt)//GEN-FIRST:event_jobsTableMouseReleased
    {//GEN-HEADEREND:event_jobsTableMouseReleased
      
       int row = jobsTable.rowAtPoint(evt.getPoint());
       int col = jobsTable.columnAtPoint(evt.getPoint());

       // check if queued check box has clicked
       if (col == 1)
       {
          // Jobs are ordered in the array similar to table ordering
          int rows[]  = jobsTable.getSelectedRows();
          int numRows = rows.length;

          // convert rows from view into tablemodel perspective
          for (int i = 0; i < numRows; i++)
          {
             rows[i] = jobsTable.convertRowIndexToModel(rows[i]);
          }

          // Is a row selected?
          if (rows.length > 0)
          {
             String[]  projectPaths = new String[rows.length];
             String[]  jobNames     = new String[rows.length];
             boolean[] queuedFlags  = new boolean[rows.length];

             DefaultTableModel jobsTableModel   = (DefaultTableModel)jobsTable.getModel();
             DefaultTableModel queueTableModel  = (DefaultTableModel)runQueueTable.getModel();

             // Jobs are ordered in the array similar to table ordering
             int numJobs = rows.length;
             for (int index = 0; index < numJobs; index++)
             {
                String jobName      = (jobsTableModel.getValueAt(rows[index], 0)).toString();
                String projectPath  = (jobsTableModel.getValueAt(rows[index], 8)).toString();
                boolean queued      = (jobsTableModel.getValueAt(rows[index], 1)).equals(true);
                projectPaths[index] = projectPath;
                jobNames[index]     = jobName;
                queuedFlags[index]  = queued;

             } // for (int index = 0; index < numJobs; index++)

             // march through the rows of jobs, and  update datamodel
             for (int jobIndex = 0; jobIndex < numJobs; jobIndex++)
             {
                 //if ( (Boolean)jobsTableModel.getValueAt(row, 1) == true )
                 if ( queuedFlags[jobIndex] == true )
                 {
                    updateDatamodel(projectPaths[jobIndex], jobNames[jobIndex], "Enqueue");

                 }
                 // else if ((Boolean)jobsTableModel.getValueAt(row, 1) == false)
                 else if ( queuedFlags[jobIndex] == false )
                 {
                    for (int i = 0; i < queueTableModel.getRowCount(); i++)
                    {
                       //String jobName     = (String)jobsTableModel.getValueAt(row, 0);
                       //String projectPath = (String)jobsTableModel.getValueAt(row, 8);
                       String jobName     = jobNames[jobIndex];
                       String projectPath = projectPaths[jobIndex];
                       if ( jobName.equalsIgnoreCase((String)queueTableModel.getValueAt(i, 0))  &&
                            projectPath.equalsIgnoreCase((String)queueTableModel.getValueAt(i, 3)) )
                       {
                          // Dequeue the jobs
                          queueTableModel.setValueAt("", i, 0);
                          queueTableModel.setValueAt("", i, 1);
                          queueTableModel.setValueAt("", i, 2);
                          queueTableModel.setValueAt("", i, 3);
                          queueTableModel.removeRow(i);
                          updateDatamodel(projectPaths[jobIndex], jobNames[jobIndex], "Dequeue");
                       }

                   } // -- for (int i = 0; i < queueTableModel.getRowCount(); i++)

                 } // -- if ( (Boolean)jobsTableModel.getValueAt(row, 1) == true )

             } // -- for (int jobIndex = 0; jobIndex < numJobs; jobIndex++)

          } // -- if (rows.length > 0)

       } // -- if (col == 1)

    }//GEN-LAST:event_jobsTableMouseReleased

    private void pauseButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_pauseButtonActionPerformed
    {//GEN-HEADEREND:event_pauseButtonActionPerformed

       // stop updates
       mUpdate = false;

       pauseQueue();
       
       // Allow updates
       mUpdate = true;

    }//GEN-LAST:event_pauseButtonActionPerformed

    private void startButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_startButtonActionPerformed
    {//GEN-HEADEREND:event_startButtonActionPerformed
       // stop updates
       mUpdate = false;

       // Verify the jobsQueue contains jobs and is ready to go
       int numJobs = runQueueTable.getRowCount();
       jobsCountTextField.setText(String.format("%d", numJobs));

       // if jobs exist in the run queue, well run them
       if (numJobs > 0)
       {
          runQueue();
       }
       
       // Allow updates
       mUpdate = true;

    }//GEN-LAST:event_startButtonActionPerformed

    private void stopButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_stopButtonActionPerformed
    {//GEN-HEADEREND:event_stopButtonActionPerformed
       // stop updates and stop queue
       mUpdate = false;
       
       stopQueue();

       // Allow updates
       mUpdate = true;


    }//GEN-LAST:event_stopButtonActionPerformed

    private void killButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_killButtonActionPerformed
    {//GEN-HEADEREND:event_killButtonActionPerformed

       // stop updates and stop queue
       mUpdate = false;

       killMpifxcorr();

       // Allow updates
       mUpdate = true;

}//GEN-LAST:event_killButtonActionPerformed

    private void textScrollPaneMouseExited(java.awt.event.MouseEvent evt)//GEN-FIRST:event_textScrollPaneMouseExited
    {//GEN-HEADEREND:event_textScrollPaneMouseExited
       // TODO add your handling code here:

    }//GEN-LAST:event_textScrollPaneMouseExited

    private void deselectAllButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_deselectAllButtonActionPerformed
    {//GEN-HEADEREND:event_deselectAllButtonActionPerformed
       // Deselect all jobs
       int rowCnt = jobsTable.getRowCount();

       // remove all jobs from the run queue table
       DefaultTableModel jobsTableModel = (DefaultTableModel) jobsTable.getModel();
       DefaultTableModel queueTableModel = (DefaultTableModel)runQueueTable.getModel();
       for (int i = 0; i < rowCnt; i++)
       {
         // unset queued flag and remove all jobs from run queue
         jobsTableModel.setValueAt(false, i, 1);
         for (int j = 0; j < queueTableModel.getRowCount(); j++)
         {
            // search the run queue for the job
            String jobName = (String)jobsTableModel.getValueAt(i, 0);
            String projName = (String)jobsTableModel.getValueAt(i, 8);
            if ( jobName.equalsIgnoreCase((String)queueTableModel.getValueAt(j, 0))  &&
                 projName.equalsIgnoreCase((String)queueTableModel.getValueAt(j, 3)) )
            {
               // remove the row and update the datamodel
               queueTableModel.setValueAt("", j, 0);
               queueTableModel.setValueAt("", j, 1);
               queueTableModel.setValueAt("", j, 2);
               queueTableModel.setValueAt("", j, 3);
               queueTableModel.removeRow(j);
               updateDatamodel(projName, jobName, "DeQueue");
            }

         } // -- for (int j = 0; j < queueTableModel.getRowCount(); j++)

       } // -- for (int i = 0; i < rowCnt; i++)

    }//GEN-LAST:event_deselectAllButtonActionPerformed

    private void deselectReadyButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_deselectReadyButtonActionPerformed
    {//GEN-HEADEREND:event_deselectReadyButtonActionPerformed
       // get table model and set the queue flag
       DefaultTableModel queueTableModel = (DefaultTableModel)runQueueTable.getModel();
 
       // Remove all jobs from the jobs run queue
       int i = queueTableModel.getRowCount();
       while ( i > 0 )
       {
          Object object = queueTableModel.getValueAt(i-1,1);
          boolean isReady = (object.toString()).equalsIgnoreCase("Ready");
          if (isReady)
          {
             String jobName  = (String)queueTableModel.getValueAt(i-1, 0);
             String projName = (String)queueTableModel.getValueAt(i-1, 3);
             queueTableModel.setValueAt("", i-1, 0);
             queueTableModel.setValueAt("", i-1, 1);
             queueTableModel.setValueAt("", i-1, 2);
             queueTableModel.setValueAt("", i-1, 3);
             queueTableModel.removeRow(i-1);
             updateDatamodel(projName, jobName, "DeQueue");
          }
          --i;
       }

    }//GEN-LAST:event_deselectReadyButtonActionPerformed

    private void selectAllButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_selectAllButtonActionPerformed
    {//GEN-HEADEREND:event_selectAllButtonActionPerformed
       // Select all jobs
       int rowCnt = jobsTable.getRowCount();

       // add all jobs to the run queue table
       DefaultTableModel jobsTableModel = (DefaultTableModel) jobsTable.getModel();
       DefaultTableModel queueTableModel = (DefaultTableModel)runQueueTable.getModel();
       for (int i = 0; i < rowCnt; i++)
       {
         jobsTableModel.setValueAt(true, i, 1);  // queued

         // add a row and update the datamodel
         //queueTableModel.addRow(new Object[]{"", "", "", ""});
         //int cnt = queueTableModel.getRowCount();
         //queueTableModel.setValueAt(jobsTableModel.getValueAt(i, 0), cnt-1, 0);
         //queueTableModel.setValueAt(jobsTableModel.getValueAt(i, 3), cnt-1, 1);
         //queueTableModel.setValueAt(jobsTableModel.getValueAt(i, 7), cnt-1, 2);
         //queueTableModel.setValueAt(jobsTableModel.getValueAt(i, 8), cnt-1, 3);
         updateDatamodel((String)jobsTableModel.getValueAt(i, 8),
                         (String)jobsTableModel.getValueAt(i, 0), "EnQueue");
       } // -- for (int i = 0; i < rowCnt; i++)

    }//GEN-LAST:event_selectAllButtonActionPerformed

    private void selectReadyButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_selectReadyButtonActionPerformed
    {//GEN-HEADEREND:event_selectReadyButtonActionPerformed
       // get table model and set the queue flag
       DefaultTableModel jobsTableModel = (DefaultTableModel) jobsTable.getModel();

       // Select/deselect jobs
       int rowCnt = jobsTable.getRowCount();
       if (rowCnt > 0)
       {
          // set the queue flag for any ready jobs and add to the run queue
          for (int row = 0; row < rowCnt; row++)
          {
             int i         = jobsTable.convertRowIndexToModel(row);
             Object object = jobsTable.getModel().getValueAt(i,3);
             boolean isReady = (object.toString()).equalsIgnoreCase("Ready");
             if (isReady)
             {
               updateDatamodel((String)jobsTableModel.getValueAt(row, 8),
                               (String)jobsTableModel.getValueAt(row, 0), "EnQueue");
             }
             else // -- not ready to run
             {
               jobsTableModel.setValueAt(false, row, 1);
             }

          } // -- for (int row = 0; row < rowCnt; row++)

       }
       else // -- jobs table is empty
       {
          jobsTable.clearSelection();

       } // -- if (rowCnt > 0)

    }//GEN-LAST:event_selectReadyButtonActionPerformed

   /**
    * @param args the command line arguments
    */
   public static void main(String args[])
   {

      java.awt.EventQueue.invokeLater(new Runnable()
      {
         @Override
         public void run()
         {

            // Instantiate the GUI
            QueueManagerUI theGUI = new QueueManagerUI();

            // Display the GUI.
            theGUI.setVisible(true);
         }
      });
   }

   // Variables declaration - do not modify//GEN-BEGIN:variables
   private javax.swing.JButton CloseButton;
   private javax.swing.JScrollPane alertsScrollPane;
   private javax.swing.JTextArea alertsTextArea;
   private javax.swing.JTextArea allMessagesTextArea;
   private javax.swing.JToolBar appsToolBar;
   private javax.swing.JPanel bottomButtonPanel;
   private javax.swing.JButton clearButton;
   private javax.swing.JLabel currentLabel;
   private javax.swing.JButton deselectAllButton;
   private javax.swing.JButton deselectReadyButton;
   private javax.swing.JPanel disksPanel;
   private javax.swing.JScrollPane disksScrollPane;
   private javax.swing.JTree disksTree;
   private javax.swing.JSplitPane jSplitPane1;
   private javax.swing.JButton jobManagerButton;
   private javax.swing.JProgressBar jobProgressBar;
   private javax.swing.JTextField jobsCountTextField;
   private javax.swing.JPanel jobsPanel;
   private javax.swing.JScrollPane jobsScrollPane;
   private javax.swing.JTabbedPane jobsTabbedPane;
   private javax.swing.JTable jobsTable;
   private javax.swing.JToolBar jobsToolBar;
   private javax.swing.JButton killButton;
   private javax.swing.JButton loadQueueButton;
   private javax.swing.JLabel noJobsLabel;
   private javax.swing.JButton pauseButton;
   private javax.swing.JButton printTabButton;
   private javax.swing.JPanel productionQueuePanel;
   private javax.swing.JScrollPane productionQueueScrollPane;
   private javax.swing.JTable productionQueueTable;
   private javax.swing.JLabel queueStateLabel;
   private javax.swing.JPanel queueStatePanel;
   private javax.swing.JTextField queueStateTextField;
   private javax.swing.JToolBar queueToolBar;
   private javax.swing.JButton readyButton;
   private javax.swing.JButton removeButton;
   private javax.swing.JButton resetButton;
   private javax.swing.JPanel runQueuePanel;
   private javax.swing.JScrollPane runQueueScrollPane;
   private javax.swing.JTable runQueueTable;
   private javax.swing.JButton selectAllButton;
   private javax.swing.JButton selectReadyButton;
   private javax.swing.JButton startButton;
   private javax.swing.JButton stopButton;
   private javax.swing.JPanel textPanel;
   private javax.swing.JScrollPane textScrollPane;
   private javax.swing.JLabel timeRemainingLabel;
   private javax.swing.JTextField timeRemainingTextField;
   private javax.swing.JLabel totalTimeLabel;
   private javax.swing.JTextField totalTimeTextField;
   private javax.swing.JButton validateButton;
   private javax.swing.JTextField wallTimeTextField;
   // End of variables declaration//GEN-END:variables
}
