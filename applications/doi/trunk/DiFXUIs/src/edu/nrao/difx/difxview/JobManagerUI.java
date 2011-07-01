/*
 * JobManagerUI.java
 *
 * Created on May 16, 2008, 3:01 PM
 */
package edu.nrao.difx.difxview;

import edu.nrao.difx.difxdatamodel.*;
import edu.nrao.difx.difxcontroller.*;

import java.util.Iterator;
import java.util.List;
import java.math.BigDecimal;
import java.math.MathContext;

import edu.nrao.sss.measure.JulianDate;
import edu.nrao.sss.measure.TimeDuration;
import edu.nrao.sss.measure.TimeUnits;

import edu.nrao.difx.difxutilities.Elapsed;
import java.awt.Color;
import java.io.BufferedReader;
import java.io.FileReader;
import java.util.Date;
import java.util.Calendar;
import java.util.TimeZone;
import java.text.SimpleDateFormat;

import java.util.HashSet;
import java.util.Scanner;
import java.util.Set;
import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.RowFilter;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.table.TableRowSorter;

import edu.nrao.difx.difxutilities.DiFXModuleTableCellRenderer;
import edu.nrao.difx.difxutilities.DiFXWeightCellRenderer;

/**
 *
 * @author  mguerra
 */
public class JobManagerUI extends javax.swing.JFrame
{

   private static final long serialVersionUID = 2;
   private static Set<String> mGuiInstances = new HashSet<String>();


   // Allow only one data model and controller instance
   static DiFXDataModel mDataModel;
   static DiFXController mController;

   // Listen for data model updates
   MessageListener mListener;

   // Current job selected
   String mJobName;

   // Current job GUI
   boolean mCurrentJob = false;

   // Custom table sorter
   TableRowSorter<TableModel> mSorter;

   /** Creates new form JobManagerUI */
   private JobManagerUI()
   {

      // initialize GUI controls
      initComponents();

      mDataModel  = null;
      mController = null;

      mSorter = new TableRowSorter<TableModel>(stationsTable.getModel());
      stationsTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   /** Creates new form JobManagerUI */
   private JobManagerUI(DiFXDataModel theModel)
   {

      // initialize GUI controls and data model
      initComponents();

      mDataModel  = theModel;
      mController = null;

      mSorter = new TableRowSorter<TableModel>(stationsTable.getModel());
      stationsTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   /** Creates new form JobManagerUI */
   private JobManagerUI(DiFXDataModel theModel, String job)
   {

      // initialize GUI controls and data model
      initComponents();

      mDataModel  = theModel;
      mController = null;
      mJobName    = job;

      mSorter = new TableRowSorter<TableModel>(stationsTable.getModel());
      stationsTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

   }

   /** Creates new form JobManagerUI */
   public JobManagerUI(DiFXDataModel theModel, DiFXController theController, String job)
   {

      // initialize GUI controls, data model and controller
      initComponents();
      mDataModel  = theModel;
      mController = theController;
      mJobName    = job;

      mSorter = new TableRowSorter<TableModel>(stationsTable.getModel());
      stationsTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

   }

   /** Creates new form JobManagerUI */
   public JobManagerUI(DiFXDataModel theModel, DiFXController theController, String job, boolean current)
   {

      // initialize GUI controls, data model and controller
      initComponents();
      mDataModel  = theModel;
      mController = theController;
      mJobName    = job;
      mCurrentJob = current;

      mSorter = new TableRowSorter<TableModel>(stationsTable.getModel());
      stationsTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

   }

   public static JobManagerUI instance(DiFXDataModel theModel, DiFXController theController, String jobName)
   {
      JobManagerUI jobManagerUI = null;

      if (mGuiInstances.add(jobName))
      {
         jobManagerUI = new JobManagerUI(theModel, theController, jobName);
      }

      return jobManagerUI;
   }

   private void serviceDataModel()
   {
      // Add code to service the data model
      System.out.printf("***************** Job Manager service data model. \n");

   }

   private void updateView()
   {
      // Add code to update the view
      // System.out.printf("***************** Job Manager update view. \n");
      if (mCurrentJob)
      {
         Queue queue = mDataModel.getQueue();
         if (queue != null)
         {
            Job job = queue.getCurrentJob();
            if (job != null)
            {
               mJobName = job.getObjName();
            }
         }
         
      } // if (mCurrentJob)

      // get job data from data model and display the data
      Job job = mDataModel.getJob(mJobName);
      if (job != null)
      {
         // -- Job Status
         String state = job.getStateString();
         stateField.setText(state);

         TimeZone.setDefault(TimeZone.getTimeZone("UTC"));

         // -- Job start date time
         TimeDuration timeOffset = new TimeDuration(new BigDecimal(job.getStartSeconds()),
                                                    TimeUnits.SECOND);
         BigDecimal startMJD = new BigDecimal(job.getStartMJD(), MathContext.UNLIMITED);
         JulianDate startJD  = JulianDate.makeFromMjd(startMJD);
         startJD.add(timeOffset);

         Date startDateTime          = startJD.toDate();
         SimpleDateFormat dateFormat = new SimpleDateFormat(DiFXSystemConfig.DATE_TIME_FORMAT);
         jobStartDateField.setText(dateFormat.format(startDateTime));

         // -- Job stop date time: add duration to start date.
         TimeDuration jobDuration = new TimeDuration(new BigDecimal(job.getExecuteTimeSeconds()),
                                                     TimeUnits.SECOND);
         JulianDate stopJD = startJD;
         stopJD.add(jobDuration);

         Date stopDateTime = stopJD.toDate();
         jobStopDateField.setText(dateFormat.format(stopDateTime));

         // -- Job current time
         BigDecimal currMJD  = job.getVisibilityMJD();
         JulianDate currJD   = JulianDate.makeFromMjd(currMJD);
         Date       currDate = currJD.toDate();

         SimpleDateFormat currFormat = new SimpleDateFormat(DiFXSystemConfig.DATE_TIME_FORMAT);
         currentTimeField.setText("");

         // -- Do not display the year 1858
         if (currMJD.compareTo(new BigDecimal(0.0)) > 0)
         {
            currentTimeField.setText(currFormat.format(currDate));
         }
         else // -- display current wall time
         {
            Calendar cal = Calendar.getInstance();
            SimpleDateFormat sdf = new SimpleDateFormat(DiFXSystemConfig.DATE_TIME_FORMAT);
            currentTimeField.setText(sdf.format(cal.getTime()));
         }

         // -- Speed Up
         JulianDate visibilityJD = JulianDate.makeFromMjd(job.getVisibilityMJD());
         JulianDate jobStartJD   = JulianDate.makeFromMjd(job.getJobStartTimeMJD());

         long jobStartWallTime = job.getStartWallTimeUTC(); // UTC
         long currentWallTime  = (Calendar.getInstance()).getTimeInMillis(); // UTC

         // Convert all to UTC milliseconds, since 1970
         float elapsedJob   = (float)(visibilityJD.toDate().getTime() - jobStartJD.toDate().getTime());
         float elapsedWall  = (float)(currentWallTime - jobStartWallTime);
         float speedUpRatio = 0.0f;

         if (elapsedWall != 0)
         {
            speedUpRatio = (elapsedJob / elapsedWall);
         }

         if (elapsedJob < 2000.0f)
         {
            speedUpRatio = 0.0f;
         }

         speedUpField.setText( String.format("%.3f", speedUpRatio) + "   " + String.format("%.3f", job.getPredictedSpeedUp()) );

         // -- convert milliseconds to seconds, convert to minutes remaining
         float secondsRemaining = ( (stopDateTime.getTime() - currDate.getTime()) / 1000.0f );
         float minutesRemaining = ( secondsRemaining / 60.0f );
         float remainingRatio   = 0.0f;
         if (speedUpRatio != 0.0f)
         {
            remainingRatio = (minutesRemaining / speedUpRatio);
         }
         else
         {
            if (job.getStateString().equalsIgnoreCase("RUNNING"))
            {
               remainingRatio = (minutesRemaining / job.getPredictedSpeedUp());
            }
         }
         
         // Calculate percent complete...
         long  totalJobTimeUTC = stopDateTime.getTime() - startDateTime.getTime();  // millis
         float totalSeconds    = (float)(totalJobTimeUTC/1000);
         float secondsExpended = (totalSeconds - secondsRemaining);
         float percentComplete = 1.0f - (secondsRemaining/totalSeconds);
         float progress = (percentComplete * 100.0f);

         // Leave complete progress on the GUI
         if (state.equalsIgnoreCase("Complete") != true)
         {
            jobProgressBar.setValue(Math.round(progress));
         }
         
         int remainingTime = (int)Math.round(remainingRatio * 60.0f);

         //timeRemainingField.setText( String.format("%.3f",  remainingRatio) );
         timeRemainingField.setText( Elapsed.calcHMS(remainingTime) );

         // -- Stations/Antennas
         Integer stat = job.getNumAntennas();
         stationsField.setText(stat.toString());

         // -- Baselines
         Integer base = job.getActiveBaselines();
         baselinesField.setText(base.toString());

         // -- Channels
         int chan = job.getNumChannels();
         channelsField.setText(Integer.toString(chan));

         // Update playrate if current running job
         if ( job.isRunning() )
         {
            // update the GUI title
            Border greenLine = BorderFactory.createLineBorder(Color.GREEN);
            TitledBorder border = BorderFactory.createTitledBorder(greenLine, mJobName);
            String path   = job.getJobPath();
            border.setTitle(path+"/"+mJobName);
            statusPanel.setBorder(border);

            // highlight tool bar
            jobToolBar.setBackground(Color.GREEN);
            diskToolBar.setBackground(Color.GREEN);
            stationPanel.setBackground(Color.GREEN);

            // Get job's modules
            List<Module> modules = job.getModules();

            // Step through the list of modules
            Iterator mit = modules.iterator();
            while (mit.hasNext())
            {
               // get job's antenna data from model to the screen
               Module module = (Module) mit.next();
               if (module != null)
               {
                  // get mark5 playrate
                  Mark5Unit mark5 = mDataModel.getMark5UnitViaVSN(module.getModuleVSN());
                  if (mark5 != null)
                  {
                     // -- Play/Disk rate
                     if (mark5.getState().equalsIgnoreCase("Play"))
                     {
                        playRateField.setText( Float.toString(mark5.getPlayRate()) );
                     }

                  } // if (mark5 != null)

               } // if (module != null)

            } // while (it.hasNext())

         }
         else // -- job not running, so clear playrate
         {
            playRateField.setText("");

            // Update color scheme
            if ( job.isReady() )
            {
               // update the GUI title
               Border blueLine = BorderFactory.createLineBorder(Color.BLUE);
               TitledBorder border = BorderFactory.createTitledBorder(blueLine, mJobName);
               String path = job.getJobPath();
               border.setTitle(path+"/"+mJobName);
               statusPanel.setBorder(border);

               // highlight tool bar
               jobToolBar.setBackground(Color.BLUE);
               diskToolBar.setBackground(Color.BLUE);
               stationPanel.setBackground(Color.BLUE);
            }
            else if ( job.isWaiting() )
            {
               // update the GUI title
               Border yellowLine = BorderFactory.createLineBorder(Color.YELLOW, 2);
               TitledBorder border = BorderFactory.createTitledBorder(yellowLine, mJobName);
               String path = job.getJobPath();
               border.setTitle(path+"/"+mJobName);
               statusPanel.setBorder(border);

               // highlight tool bar
               jobToolBar.setBackground(Color.YELLOW);
               diskToolBar.setBackground(Color.YELLOW);
               stationPanel.setBackground(Color.YELLOW);
            }
            else if ( job.isNotReady() )
            {
               // update the GUI title
               Border orangeLine = BorderFactory.createLineBorder(Color.ORANGE, 2);
               TitledBorder border = BorderFactory.createTitledBorder(orangeLine, mJobName);
               String path = job.getJobPath();
               border.setTitle(path+"/"+mJobName);
               statusPanel.setBorder(border);

               // highlight tool bar
               jobToolBar.setBackground(Color.ORANGE);
               diskToolBar.setBackground(Color.ORANGE);
               stationPanel.setBackground(Color.ORANGE);
            }
            else if ( job.isComplete() )
            {
               // update the GUI title
               Border blackLine = BorderFactory.createLineBorder(Color.BLACK, 2);
               TitledBorder border = BorderFactory.createTitledBorder(blackLine, mJobName);
               String path = job.getJobPath();
               border.setTitle(path+"/"+mJobName);
               statusPanel.setBorder(border);

               // highlight tool bar
               jobToolBar.setBackground(Color.WHITE);
               diskToolBar.setBackground(Color.WHITE);
               stationPanel.setBackground(Color.WHITE);
            }
            else if ( job.isFailed() || job.isUnknown() )
            {
               // update the GUI title
               Border redLine = BorderFactory.createLineBorder(Color.RED, 2);
               TitledBorder border = BorderFactory.createTitledBorder(redLine, mJobName);
               String path = job.getJobPath();
               border.setTitle(path+"/"+mJobName);
               statusPanel.setBorder(border);

               // highlight tool bar
               jobToolBar.setBackground(Color.RED);
               diskToolBar.setBackground(Color.RED);
               stationPanel.setBackground(Color.RED);
            }
            else
            {
               // update the GUI title
               Border blackLine = BorderFactory.createLineBorder(Color.BLACK, 2);
               TitledBorder border = BorderFactory.createTitledBorder(blackLine, mJobName);
               String path = job.getJobPath();
               border.setTitle(path+"/"+mJobName);
               statusPanel.setBorder(border);

               jobToolBar.setBackground(Color.lightGray);
               diskToolBar.setBackground(Color.lightGray);
               stationPanel.setBackground(Color.lightGray);
            }
         }

         // update the table
         updateTable(job);
      }

      // System.out.printf("***************** Job Manager update view complete. \n");
   }

   public void updateTable(Job job)
   {
      // -- Get the data from the model

      // Get job's modules and current job running
      List<Module> modules = job.getModules();
      Job currJob = mDataModel.getQueue().getCurrentJob();

      int tableRow = 0;

      if ( modules != null )
      {
         // Size up the table
         DefaultTableModel tableModel = (DefaultTableModel) stationsTable.getModel();
         int numRows = tableModel.getRowCount();
         int numAnts = modules.size();

         while (numAnts > numRows)
         {
            tableModel.addRow(new Object[] {null, null, null, null, null, null, null, null});
            numAnts--;
         }

         // Step through the list of modules
         Iterator mit = modules.iterator();
         while (mit.hasNext())
         {
            // get job's module data from model to the screen
            Module module = (Module) mit.next();
            if (module != null)
            {
               // insert job data into the table
               tableModel.setValueAt(module.getObjName(),   tableRow, 0);  // antenna/station
               tableModel.setValueAt(module.getModuleVSN(), tableRow, 1);  // VSN/module
               tableModel.setValueAt(module.getShelf(),     tableRow, 2);  // shelf
               tableModel.setValueAt(module.getWeight(),    tableRow, 3);  // antenna min weight

               // Get associated mark5
               String VSN = module.getModuleVSN();
               Mark5Unit mark5 = mDataModel.getMark5UnitViaVSN(VSN);
               if (mark5 != null)
               {
                  // Determine assigned bank here
                  String bank = "";
                  if (module.getModuleVSN().equalsIgnoreCase(mark5.getBankAVSN()))
                  {
                     bank = "A";
                  }
                  else if (module.getModuleVSN().equalsIgnoreCase(mark5.getBankBVSN()))
                  {
                     bank = "B";
                  }
                  
                  // insert mark5 data into the table
                  tableModel.setValueAt(mark5.getObjName(),    tableRow, 4);  // mark5 name
                  
                  // either display this job's state or the name of the current job
                  //if ( (currJob != null) &&
                  //     (currJob.getObjName().equalsIgnoreCase(job.getObjName()) != true) )
                  if ( (mark5.getCurrentJob().equalsIgnoreCase("") != true) &&
                       (mark5.getCurrentJob().equalsIgnoreCase("mk5daemon") != true) &&
                       (mark5.getCurrentJob().equalsIgnoreCase(job.getObjName()) != true))
                  {
                     // display current running job, a different job is running
                     // tableModel.setValueAt(currJob.getObjName(), tableRow, 5);  // state
                     tableModel.setValueAt(mark5.getCurrentJob(), tableRow, 5);  // job
                  }
                  else // -- this must be the job running
                  {
                     // display mrak 5 status of this job
                     tableModel.setValueAt(mark5.getState(), tableRow, 5);  // state
                  }

                  tableModel.setValueAt(bank, tableRow, 6);  // active bank
                  tableModel.setValueAt(mark5.getScanNumber(), tableRow, 7);  // scan number
               }
               else
               {
                  // clear out mark5 data
                  tableModel.setValueAt(null, tableRow, 4);  // mark5 name
                  tableModel.setValueAt(null, tableRow, 5);  // state
                  tableModel.setValueAt(null, tableRow, 6);  // active bank
                  tableModel.setValueAt(null, tableRow, 7);  // scan number
               }
            }

            // inc row
            ++tableRow;

         } // -- Step through the list of modules

         // Filter out empty rows
         mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      } // if ( modules != null )

   }

   public void attachListenerCallback()
   {
      // Hand the thread manager the implementation of update()...
      if (mDataModel != null)
      {
         this.setTitle(this.getTitle() + " " + DOISystemConfig.DOIVersion);
         System.out.println("***************** Job Manager attach listener.");

         // create listener implementation of update()...
         mListener = new MessageListener()
         {
            @Override
            public void update()
            {
               // Get handle to GUI and UpdateGUI()
               //System.out.printf("***************** Job Manager service data model and view. \n");
               updateView();
               //System.out.println("***************** Job Manager Update data model and view complete. \n");
            }
         };

         // hand data model a listener
         mDataModel.attachListener(mListener);
      }
      else
      {
         System.out.println("***************** Job Manager listener not attached. \n");
      }
   }

   public void detachListener()
   {
      // remove message listener
      if (mDataModel != null)
      {
         System.out.println("***************** Job Manager detach listener. \n");
         mDataModel.detachListener(mListener);
      }
      else
      {
         System.out.println("***************** Job Manager listener not detached. \n");
      }

   }

   private boolean isVerified( Job job )
   {
      // assume failure
      boolean readyToRun = false;

      if ( job != null )
      {
         JFrame frame = null;
         if (job.isReady() == true)
         {
            if (job.verifyResources() == true)
            {
               if (job.verifyInputFiles() == true)
               {
                  // Raise dialog
                  JOptionPane.showMessageDialog(frame,
                     "Job passed verification and ready to run. \n" +
                     "The mark5 units and processors are available \n" +
                     "and .uvw, .input, .calc, .delay files exist.",
                     "Job information",
                     JOptionPane.INFORMATION_MESSAGE);

                  // job is ready
                  readyToRun = true;
               }
               else // failed to verify input files
               {
                  // Raise dialog
                  JOptionPane.showMessageDialog(frame,
                     "Job failed input file verification.",
                     "Job warning",
                     JOptionPane.WARNING_MESSAGE);

               }
            }
            else // failed to verify resources
            {
               // Raise dialog
               JOptionPane.showMessageDialog(frame,
                  "Job failed resource verification.",
                  "Job warning",
                  JOptionPane.WARNING_MESSAGE);
            }
         }
         else // job not ready
         {
            // Raise dialog
            JOptionPane.showMessageDialog(frame,
               "Job not ready to run.",
               "Job warning",
               JOptionPane.WARNING_MESSAGE);
         }
      }

      // return
      return readyToRun;
   }

   /** This method is called from within the constructor to
    * initialize the form.
    * WARNING: Do NOT modify this code. The content of this method is
    * always regenerated by the Form Editor.
    */
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    jobToolBar = new javax.swing.JToolBar();
    runButton = new javax.swing.JButton();
    stopButton = new javax.swing.JButton();
    validateButton = new javax.swing.JButton();
    viewLogButton = new javax.swing.JButton();
    diskToolBar = new javax.swing.JToolBar();
    modulesButton = new javax.swing.JButton();
    statusPanel = new javax.swing.JPanel();
    stateLabel = new javax.swing.JLabel();
    stateField = new javax.swing.JTextField();
    jobDateLabel = new javax.swing.JLabel();
    jobStartDateField = new javax.swing.JTextField();
    currentTimeLabel = new javax.swing.JLabel();
    currentTimeField = new javax.swing.JTextField();
    stopTimeLabel = new javax.swing.JLabel();
    jobStopDateField = new javax.swing.JTextField();
    playRateLabel = new javax.swing.JLabel();
    playRateField = new javax.swing.JTextField();
    timeRemainLabel = new javax.swing.JLabel();
    timeRemainingField = new javax.swing.JTextField();
    speedUpLabel = new javax.swing.JLabel();
    speedUpField = new javax.swing.JTextField();
    stationsLabel = new javax.swing.JLabel();
    stationsField = new javax.swing.JTextField();
    baselinesLabel = new javax.swing.JLabel();
    baselinesField = new javax.swing.JTextField();
    channelsLabel = new javax.swing.JLabel();
    channelsField = new javax.swing.JTextField();
    stationPanel = new javax.swing.JPanel();
    stationsScrollPane = new javax.swing.JScrollPane();
    stationsTable = new javax.swing.JTable();
    bottomPanel = new javax.swing.JPanel();
    jobProgressBar = new javax.swing.JProgressBar();
    closeButton = new javax.swing.JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
    setTitle("Job Manager");

    jobToolBar.setBackground(java.awt.Color.lightGray);
    jobToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
    jobToolBar.setRollover(true);

    runButton.setText("Run");
    runButton.setFocusable(false);
    runButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    runButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    runButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        runButtonActionPerformed(evt);
      }
    });
    jobToolBar.add(runButton);

    stopButton.setText("Stop");
    stopButton.setFocusable(false);
    stopButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    stopButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    stopButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        stopButtonActionPerformed(evt);
      }
    });
    jobToolBar.add(stopButton);

    validateButton.setText("Validate");
    validateButton.setFocusable(false);
    validateButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    validateButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    validateButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        validateButtonActionPerformed(evt);
      }
    });
    jobToolBar.add(validateButton);

    viewLogButton.setText("View Log");
    viewLogButton.setFocusable(false);
    viewLogButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    viewLogButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    viewLogButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        viewLogButtonActionPerformed(evt);
      }
    });
    jobToolBar.add(viewLogButton);

    diskToolBar.setBackground(java.awt.Color.lightGray);
    diskToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
    diskToolBar.setRollover(true);

    modulesButton.setText("Modules");
    modulesButton.setFocusable(false);
    modulesButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    modulesButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    modulesButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        modulesButtonActionPerformed(evt);
      }
    });
    diskToolBar.add(modulesButton);

    statusPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 2), "Job"));
    statusPanel.setFont(new java.awt.Font("Dialog", 1, 12));

    stateLabel.setText("State:");
    stateLabel.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);

    stateField.setBackground(new java.awt.Color(238, 238, 238));
    stateField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    jobDateLabel.setText("Job Start:");
    jobDateLabel.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);

    jobStartDateField.setBackground(new java.awt.Color(238, 238, 238));
    jobStartDateField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    currentTimeLabel.setText("  Current:");

    currentTimeField.setBackground(new java.awt.Color(238, 238, 238));
    currentTimeField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    stopTimeLabel.setText("       Stop:");

    jobStopDateField.setBackground(new java.awt.Color(238, 238, 238));
    jobStopDateField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    playRateLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
    playRateLabel.setText("Play Rate:");
    playRateLabel.setHorizontalTextPosition(javax.swing.SwingConstants.LEFT);

    playRateField.setBackground(new java.awt.Color(238, 238, 238));
    playRateField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    timeRemainLabel.setHorizontalAlignment(javax.swing.SwingConstants.TRAILING);
    timeRemainLabel.setText("Time Remaining:");
    timeRemainLabel.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);

    timeRemainingField.setBackground(new java.awt.Color(238, 238, 238));
    timeRemainingField.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
    timeRemainingField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    speedUpLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
    speedUpLabel.setText("SpeedUp:");
    speedUpLabel.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);

    speedUpField.setBackground(new java.awt.Color(238, 238, 238));
    speedUpField.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
    speedUpField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    stationsLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
    stationsLabel.setText("Stations:");
    stationsLabel.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);

    stationsField.setBackground(new java.awt.Color(238, 238, 238));
    stationsField.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
    stationsField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    baselinesLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
    baselinesLabel.setText("Baselines:");
    baselinesLabel.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);

    baselinesField.setBackground(new java.awt.Color(238, 238, 238));
    baselinesField.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
    baselinesField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    channelsLabel.setHorizontalAlignment(javax.swing.SwingConstants.RIGHT);
    channelsLabel.setText("Channels:");
    channelsLabel.setHorizontalTextPosition(javax.swing.SwingConstants.RIGHT);

    channelsField.setBackground(new java.awt.Color(238, 238, 238));
    channelsField.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
    channelsField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

    javax.swing.GroupLayout statusPanelLayout = new javax.swing.GroupLayout(statusPanel);
    statusPanel.setLayout(statusPanelLayout);
    statusPanelLayout.setHorizontalGroup(
      statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(statusPanelLayout.createSequentialGroup()
        .addContainerGap()
        .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
          .addGroup(statusPanelLayout.createSequentialGroup()
            .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
              .addComponent(stopTimeLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 59, Short.MAX_VALUE)
              .addComponent(currentTimeLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 59, Short.MAX_VALUE)
              .addComponent(jobDateLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 59, Short.MAX_VALUE)
              .addComponent(playRateLabel))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
              .addComponent(playRateField, javax.swing.GroupLayout.PREFERRED_SIZE, 70, javax.swing.GroupLayout.PREFERRED_SIZE)
              .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                .addComponent(jobStopDateField, javax.swing.GroupLayout.DEFAULT_SIZE, 186, Short.MAX_VALUE)
                .addComponent(currentTimeField, javax.swing.GroupLayout.DEFAULT_SIZE, 186, Short.MAX_VALUE)
                .addComponent(jobStartDateField, javax.swing.GroupLayout.DEFAULT_SIZE, 186, Short.MAX_VALUE)))
            .addGap(169, 169, 169))
          .addGroup(statusPanelLayout.createSequentialGroup()
            .addComponent(stateLabel)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addComponent(stateField, javax.swing.GroupLayout.PREFERRED_SIZE, 88, javax.swing.GroupLayout.PREFERRED_SIZE)
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
        .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(timeRemainLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
          .addComponent(speedUpLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 104, Short.MAX_VALUE)
          .addComponent(stationsLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 104, Short.MAX_VALUE)
          .addComponent(baselinesLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 104, Short.MAX_VALUE)
          .addComponent(channelsLabel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 104, Short.MAX_VALUE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addComponent(timeRemainingField, javax.swing.GroupLayout.DEFAULT_SIZE, 97, Short.MAX_VALUE)
          .addComponent(speedUpField, javax.swing.GroupLayout.DEFAULT_SIZE, 97, Short.MAX_VALUE)
          .addComponent(stationsField, javax.swing.GroupLayout.DEFAULT_SIZE, 97, Short.MAX_VALUE)
          .addComponent(baselinesField, javax.swing.GroupLayout.DEFAULT_SIZE, 97, Short.MAX_VALUE)
          .addComponent(channelsField, javax.swing.GroupLayout.DEFAULT_SIZE, 97, Short.MAX_VALUE)))
    );

    statusPanelLayout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {baselinesLabel, channelsLabel, speedUpLabel, stationsLabel});

    statusPanelLayout.setVerticalGroup(
      statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(statusPanelLayout.createSequentialGroup()
        .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
          .addGroup(statusPanelLayout.createSequentialGroup()
            .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
              .addComponent(timeRemainLabel)
              .addComponent(timeRemainingField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
              .addComponent(stateLabel)
              .addComponent(stateField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
              .addComponent(jobDateLabel)
              .addComponent(speedUpLabel)
              .addComponent(speedUpField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
              .addComponent(jobStartDateField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addGap(6, 6, 6)
            .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
              .addComponent(currentTimeLabel)
              .addComponent(stationsLabel)
              .addComponent(stationsField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
              .addComponent(currentTimeField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
              .addComponent(stopTimeLabel)
              .addComponent(baselinesLabel)
              .addComponent(baselinesField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
              .addComponent(jobStopDateField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
            .addGroup(statusPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
              .addComponent(playRateLabel)
              .addComponent(playRateField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
          .addGroup(javax.swing.GroupLayout.Alignment.CENTER, statusPanelLayout.createSequentialGroup()
            .addGap(92, 92, 92)
            .addComponent(channelsField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
          .addGroup(javax.swing.GroupLayout.Alignment.CENTER, statusPanelLayout.createSequentialGroup()
            .addGap(93, 93, 93)
            .addComponent(channelsLabel)))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    statusPanelLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {baselinesField, channelsField, speedUpField, stationsField, timeRemainingField});

    statusPanelLayout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {baselinesLabel, channelsLabel, speedUpLabel, stationsLabel, timeRemainLabel});

    stationPanel.setBackground(java.awt.Color.lightGray);
    stationPanel.setBorder(javax.swing.BorderFactory.createEtchedBorder());

    stationsTable.setBorder(javax.swing.BorderFactory.createEtchedBorder());
    stationsTable.setModel(new javax.swing.table.DefaultTableModel(
      new Object [][] {
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null},
        {null, null, null, null, null, null, null, null}
      },
      new String [] {
        "Station", "VSN", "Shelf", "Weight", "Mark5 Unit", "State", "Bank", "Scan Num"
      }
    ) {
      Class[] types = new Class [] {
        java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.Float.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.Integer.class
      };
      boolean[] canEdit = new boolean [] {
        false, false, false, false, false, false, false, false
      };

      public Class getColumnClass(int columnIndex) {
        return types [columnIndex];
      }

      public boolean isCellEditable(int rowIndex, int columnIndex) {
        return canEdit [columnIndex];
      }
    });
    stationsScrollPane.setViewportView(stationsTable);
    stationsTable.getColumnModel().getColumn(3).setCellRenderer(new DiFXWeightCellRenderer());
    stationsTable.getColumnModel().getColumn(5).setCellRenderer(new DiFXModuleTableCellRenderer());

    javax.swing.GroupLayout stationPanelLayout = new javax.swing.GroupLayout(stationPanel);
    stationPanel.setLayout(stationPanelLayout);
    stationPanelLayout.setHorizontalGroup(
      stationPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGap(0, 659, Short.MAX_VALUE)
      .addGroup(stationPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addComponent(stationsScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 659, Short.MAX_VALUE))
    );
    stationPanelLayout.setVerticalGroup(
      stationPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGap(0, 200, Short.MAX_VALUE)
      .addGroup(stationPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addComponent(stationsScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 200, Short.MAX_VALUE))
    );

    bottomPanel.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));

    jobProgressBar.setForeground(new java.awt.Color(51, 255, 0));
    jobProgressBar.setBorder(javax.swing.BorderFactory.createTitledBorder(new javax.swing.border.LineBorder(new java.awt.Color(0, 0, 0), 1, true), "Job Progress. . .", javax.swing.border.TitledBorder.DEFAULT_JUSTIFICATION, javax.swing.border.TitledBorder.DEFAULT_POSITION, new java.awt.Font("Dialog", 1, 11))); // NOI18N
    jobProgressBar.setStringPainted(true);

    closeButton.setText("Close");
    closeButton.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED, null, new java.awt.Color(153, 153, 153)));
    closeButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        closeButtonActionPerformed(evt);
      }
    });

    javax.swing.GroupLayout bottomPanelLayout = new javax.swing.GroupLayout(bottomPanel);
    bottomPanel.setLayout(bottomPanelLayout);
    bottomPanelLayout.setHorizontalGroup(
      bottomPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(bottomPanelLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(jobProgressBar, javax.swing.GroupLayout.PREFERRED_SIZE, 279, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 274, Short.MAX_VALUE)
        .addComponent(closeButton, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap())
    );
    bottomPanelLayout.setVerticalGroup(
      bottomPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(jobProgressBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
      .addGroup(bottomPanelLayout.createSequentialGroup()
        .addComponent(closeButton, javax.swing.GroupLayout.DEFAULT_SIZE, 32, Short.MAX_VALUE)
        .addContainerGap())
    );

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(layout.createSequentialGroup()
        .addComponent(jobToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 379, Short.MAX_VALUE)
        .addComponent(diskToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
      .addComponent(bottomPanel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addComponent(statusPanel, javax.swing.GroupLayout.DEFAULT_SIZE, 663, Short.MAX_VALUE)
      .addComponent(stationPanel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    layout.setVerticalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(layout.createSequentialGroup()
        .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
          .addComponent(diskToolBar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
          .addComponent(jobToolBar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(statusPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(stationPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
        .addComponent(bottomPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
    );

    pack();
  }// </editor-fold>//GEN-END:initComponents

   private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed

      // remove from instances list
      mGuiInstances.remove(mJobName);
      
      // remove this listener
      detachListener();

      // close window
      this.dispose();

}//GEN-LAST:event_closeButtonActionPerformed

   private void stopButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_stopButtonActionPerformed
   {//GEN-HEADEREND:event_stopButtonActionPerformed

      // -- Create DifxMessage to stop the job,
      // -- this is done a bit differnet, allow the user to stop a runaway job.

      // Stop the current running job
      if (mDataModel != null)
      {
         // Get the job from job pool and issue stop
         Job job = mDataModel.getJob(mJobName);
         if ( job != null )
         {
            Queue queue = mDataModel.getQueue();
            if (queue != null)
            {
               // Raise dialog, verify job stop
               Object[] options = {"Yes", "No"};
               JFrame frame = null;
               int opt = JOptionPane.showOptionDialog(frame,
                           "Do you wish to continue and issue stop command.",
                           "Job - Stop",
                           JOptionPane.YES_NO_OPTION,
                           JOptionPane.QUESTION_MESSAGE,
                           null, options, options[1]);

               // Yes, issue the command
               if (opt == 0)
               {
                  // force a stop now, odd ball case
                  queue.stopRun(job);
               }
            }
            else
            {
               // Raise dialog
               JFrame frame = null;
               JOptionPane.showMessageDialog(frame,
                  "Queue does not exist and do not issue stop.",
                  "Job",
                  JOptionPane.WARNING_MESSAGE);
            }
         }
         else
         {
            // Raise dialog
            JFrame frame = null;
            JOptionPane.showMessageDialog(frame,
               "Job does not exist and do not issue stop.",
               "Job",
               JOptionPane.WARNING_MESSAGE);
         }

      } // if (mDatamodel != null)

}//GEN-LAST:event_stopButtonActionPerformed

   private void validateButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_validateButtonActionPerformed
   {//GEN-HEADEREND:event_validateButtonActionPerformed

      // Verify the job, raise failure dialog
      JFrame frame = null;

      // If queue is busy, raise a dialog
      Queue queue = mDataModel.getQueue();
      if (queue != null)
      {
          // Get and vaifate the job
          Job job = mDataModel.getJob(mJobName);
          if (job != null)
          {
              if (job.isReady() == true)
              {
                 if (job.verifyResources() == true)
                 {
                    if (job.verifyInputFiles() == true)
                    {
                       // Raise dialog
                       JOptionPane.showMessageDialog(frame,
                          "Job passed validation and ready to run. \n" +
                          "The mark5 units and processors are available \n" +
                          "and .uvw, .input, .calc, .delay files exist.",
                          "Job information",
                          JOptionPane.INFORMATION_MESSAGE);
                    }
                    else // failed to verify input files
                    {
                       // Raise dialog
                       JOptionPane.showMessageDialog(frame,
                          "Job failed input file verification.",
                          "Job warning",
                          JOptionPane.WARNING_MESSAGE);
                    }
                 }
                 else // failed to verify resources
                 {
                    // Raise dialog
                    JOptionPane.showMessageDialog(frame,
                       "Job failed resource verification.",
                       "Job warning",
                       JOptionPane.WARNING_MESSAGE);
                 }
              }
              else // job not ready
              {
                 // Raise dialog
                 JOptionPane.showMessageDialog(frame,
                    "Job not ready to run.",
                    "Job warning",
                    JOptionPane.WARNING_MESSAGE);
              }
          }
          else
          {
             // Raise dialog
             JOptionPane.showMessageDialog(frame,
                     "Job not in Queue and failed to verify.",
                     "Job warning",
                     JOptionPane.WARNING_MESSAGE);
          }
      }

   }//GEN-LAST:event_validateButtonActionPerformed

   private void modulesButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_modulesButtonActionPerformed
   {//GEN-HEADEREND:event_modulesButtonActionPerformed

       // Display the GUI
       ModuleManagerUI theMM = ModuleManagerUI.instance(mDataModel, mController);
       // singleton, attach listener in the ResourceManagerUI class not here.
       // theMM.attachListenerCallback();
       theMM.setVisible(true);


}//GEN-LAST:event_modulesButtonActionPerformed

   private void viewLogButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_viewLogButtonActionPerformed
   {//GEN-HEADEREND:event_viewLogButtonActionPerformed
      // Display the UI
      PrintDataManagerUI thePDM = new PrintDataManagerUI();
      thePDM.setVisible(true);

      // get name of the job
      TitledBorder border = (TitledBorder) statusPanel.getBorder(); //BorderFactory.createTitledBorder(mJobName);
      String jobLogFile   = border.getTitle() + ".difxlog";
      thePDM.append(jobLogFile);

      // Read the resource config file into diFxMessage and process one line at a time
      Scanner s = null;
      try
      {
         FileReader     fReader = new FileReader(jobLogFile);
         BufferedReader bReader = new BufferedReader(fReader);
         s = new Scanner(bReader);
         while (s.hasNextLine())
         {
            String strLine = s.nextLine();
            thePDM.append(strLine);

         } // -- while (s.hasNext())
      }
      catch (Exception e)
      {
         System.out.println("Exception: " + e);
         thePDM.append("Exception: " + e);
      }
      finally
      {
         if (s != null)
         {
            s.close();
         }
      }

}//GEN-LAST:event_viewLogButtonActionPerformed

   private void runButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_runButtonActionPerformed
   {//GEN-HEADEREND:event_runButtonActionPerformed

      // -- Hand the job to job queue as next job to run,
      // -- job will not start if not in the queue.
      if (mDataModel != null)
      {
         // If queue is idle, paused or empty, run the job
         Queue queue = mDataModel.getQueue();
         if ( queue != null )
         {
            if (queue.isQueuePause() || queue.isQueueIdle() || queue.isQueueEmpty())
            {
               try
               {
                  // Get job from data model
                  Job job = mDataModel.getJob(mJobName);

                  // If job is available, run it, or else raise a dialog
                  if  (job != null)
                  {
                     if ( (job.isQueued() == false) )
                     {
                        // put selected job to front of the run queue
                        // queue.addRunNow(job);

                        // run job now
                        job.setStarted(true);
                        queue.runJob(job);
                     }
                     else // -- job is already in the queue
                     {
                        // Raise dialog
                        JFrame frame = null;
                        JOptionPane.showMessageDialog(frame,
                          "Job is already in the Queue. \n" +
                          "Job : " + mJobName + "\n" +
                          "Project : " + job.getProjectName() + "\n\n" +
                          "Remove job from the Queue.",
                          "Job - Run Failed",
                          JOptionPane.ERROR_MESSAGE);

                     } // -- if (job.isQueued() == false)

                  } // -- if  (job != null)
               }
               catch (UnsupportedOperationException ex)
               {
                  String error = ex.toString();
                  error = error.substring(error.indexOf(":")+1);

                  // Raise dialog, job raised exception
                  Object[] options = {"OK"};
                  JFrame frame = null;
                  int opt = JOptionPane.showOptionDialog(frame,
                           error,
                           "Job",
                           JOptionPane.OK_OPTION,
                           JOptionPane.ERROR_MESSAGE,
                           null, options, options[0]);

               } // -- try catch (UnsupportedOperationException ex)

            }
            else // -- can not run job, raise dialog
            {
               // Raise dialog
               JFrame frame = null;
               JOptionPane.showMessageDialog(frame,
                       "Job can not run. \n" +
                       "Job : " + mJobName + "\n" +
                       "Queue State : " +  queue.getState()  + "\n" +
                       "Stop or pause the Queue.",
                       "Job - Run Failed",
                       JOptionPane.ERROR_MESSAGE);

            } // -- if (queue.isQueuePaused() || queue.isQueueIdle() || queue.isQueueEmpty())
            
         } // -- if ( queue != null &&

      } // -- if (jobQueue != null)

}//GEN-LAST:event_runButtonActionPerformed

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
            JobManagerUI theGUI = new JobManagerUI();

            // Display the GUI.
            theGUI.setVisible(true);

         }
      });
   }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JTextField baselinesField;
  private javax.swing.JLabel baselinesLabel;
  private javax.swing.JPanel bottomPanel;
  private javax.swing.JTextField channelsField;
  private javax.swing.JLabel channelsLabel;
  private javax.swing.JButton closeButton;
  private javax.swing.JTextField currentTimeField;
  private javax.swing.JLabel currentTimeLabel;
  private javax.swing.JToolBar diskToolBar;
  private javax.swing.JLabel jobDateLabel;
  private javax.swing.JProgressBar jobProgressBar;
  private javax.swing.JTextField jobStartDateField;
  private javax.swing.JTextField jobStopDateField;
  private javax.swing.JToolBar jobToolBar;
  private javax.swing.JButton modulesButton;
  private javax.swing.JTextField playRateField;
  private javax.swing.JLabel playRateLabel;
  private javax.swing.JButton runButton;
  private javax.swing.JTextField speedUpField;
  private javax.swing.JLabel speedUpLabel;
  private javax.swing.JTextField stateField;
  private javax.swing.JLabel stateLabel;
  private javax.swing.JPanel stationPanel;
  private javax.swing.JTextField stationsField;
  private javax.swing.JLabel stationsLabel;
  private javax.swing.JScrollPane stationsScrollPane;
  private javax.swing.JTable stationsTable;
  private javax.swing.JPanel statusPanel;
  private javax.swing.JButton stopButton;
  private javax.swing.JLabel stopTimeLabel;
  private javax.swing.JLabel timeRemainLabel;
  private javax.swing.JTextField timeRemainingField;
  private javax.swing.JButton validateButton;
  private javax.swing.JButton viewLogButton;
  // End of variables declaration//GEN-END:variables
}
