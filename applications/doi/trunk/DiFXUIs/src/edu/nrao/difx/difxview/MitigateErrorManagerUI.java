/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * ErrorManagerUI.java
 *
 * Created on Oct 6, 2009, 10:10:41 AM
 */
package edu.nrao.difx.difxview;

import edu.nrao.difx.difxcontroller.DiFXController;
import edu.nrao.difx.difxdatamodel.DOISystemConfig;
import edu.nrao.difx.difxdatamodel.DiFXDataModel;
import edu.nrao.difx.difxdatamodel.DiFXSystemConfig;
import edu.nrao.difx.difxdatamodel.Job;
import edu.nrao.difx.difxdatamodel.MessageListener;
import edu.nrao.difx.difxdatamodel.Queue;
import edu.nrao.difx.difxutilities.DiFXJobTableCellRenderer;
import edu.nrao.difx.xmllib.difxmessage.Body;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;
import edu.nrao.difx.xmllib.difxmessage.DoiJobCommand;
import edu.nrao.difx.xmllib.difxmessage.Header;
import edu.nrao.difx.xmllib.difxmessage.ObjectFactory;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import javax.swing.RowFilter;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

/**
 *
 * @author mguerra
 */
public class MitigateErrorManagerUI extends javax.swing.JFrame
{

   private static final long serialVersionUID = 4;
   private static MitigateErrorManagerUI mInstance = null;

   // Allow only one data model and controller instance
   private static DiFXDataModel  mDataModel  = null;
   private static DiFXController mController = null;

   private boolean mUpdate = true;

   // Listen for data model updates
   private MessageListener mListener = null;

   // Table model and custom table sorter
   TableRowSorter<TableModel> mSorter = null;

   /** Creates new form ErrorManagerUI */
   private MitigateErrorManagerUI()
   {
      initComponents();
      mDataModel   = null;
      mController  = null;
      mSorter = new TableRowSorter<TableModel>(errorTable.getModel());
      errorTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   private MitigateErrorManagerUI(DiFXDataModel theModel)
   {
      // initialize GUI controls and data model
      initComponents();

      mDataModel   = theModel;
      mController  = null;
      mSorter = new TableRowSorter<TableModel>(errorTable.getModel());
      errorTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

  public MitigateErrorManagerUI(DiFXDataModel theModel, DiFXController theController)
   {
      // initialize GUI controls and data model
      initComponents();

      mDataModel   = theModel;
      mController  = theController;
      mSorter = new TableRowSorter<TableModel>(errorTable.getModel());
      errorTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   public static MitigateErrorManagerUI instance(DiFXDataModel theModel, DiFXController theController)
   {
      if (mInstance == null)
      {
         mInstance = new MitigateErrorManagerUI(theModel, theController);
         mInstance.attachListenerCallback();
      }

      return mInstance;
   }

   public void attachListenerCallback()
   {
      // check dataModel and hand it an implementation of update()...
      if (mDataModel != null)
      {
         this.setTitle(this.getTitle() + " " + DOISystemConfig.DOIVersion);
         System.out.println("***************** Mitigate Error Manager attach listener.");

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
         System.out.println("***************** Mitigate Error Manager listener not attached .\n");
      }
   }

   public void detachListener()
   {
      // remove message listener
      if (mDataModel != null)
      {
         System.out.printf("***************** Mitigate Error Manager detach listener. \n");
         mDataModel.detachListener(mListener);
      }
      else
      {
         System.out.println("***************** Mitigate Error Manager listener not detached. \n");
      }

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
            if ( queue != null && queue.isEmpty() != true )
            {

               // Verify jobs actually exist
               ArrayList<Job> jobs = queue.getJobsToRun();
               if (jobs != null)
               {
                  // Size up the table
                  DefaultTableModel tableModel = (DefaultTableModel) errorTable.getModel();
                  int numRows = tableModel.getRowCount();
                  int numJobs = jobs.size();

                  while (numJobs > numRows)
                  {
                     tableModel.addRow(new Object[] {null, null, null, null, null, null});
                     numJobs--;
                  }

                  // fill in jobs table
                  int tableRow = 0;
                  Iterator it = jobs.iterator();
                  while (it.hasNext())
                  {
                     // get job data from model to the screen
                     Job job = (Job) it.next();
                     if  ( job != null && 
                           (job.isFailed() || job.isUnknown()) )
                     {
                        // insert job data into the table
                        tableModel.setValueAt(job.getProjectName(), tableRow, 0);         // project
                        tableModel.setValueAt(job.getObjName(),     tableRow, 1);         // job
                        tableModel.setValueAt(job.getStateString(), tableRow, 2);         // state
                        tableModel.setValueAt(String.format("%.2f", job.getCompletion()), // complete
                                                                    tableRow, 3);
                        Date stopDate = new Date(job.getCorrelationStopUTC());
                        SimpleDateFormat dateFormat = new SimpleDateFormat(DiFXSystemConfig.DATE_TIME_FORMAT);
                        tableModel.setValueAt(dateFormat.format(stopDate), tableRow, 4);  // date complete
                        tableModel.setValueAt(job.getProjectPath(), tableRow, 5);         // project path

                     } // -- if (job != null)

                     if  ( job != null &&
                           (job.isReady() || job.isComplete ()) )
                     {
                        // insert job data into the table
                        tableModel.setValueAt("", tableRow, 0);             // project
                        tableModel.setValueAt("", tableRow, 1);             // job
                        tableModel.setValueAt("", tableRow, 2);             // state
                        tableModel.setValueAt(String.format("%.2f", 0.0f),  // complete
                                                  tableRow, 3);
                        tableModel.setValueAt("", tableRow, 4);             // date complete
                        tableModel.setValueAt("", tableRow, 5);             // project path

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
         mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
         scrollPaneToBottom();

         // cleanup

         } // -- if (mUpdate == true)
      }
      else // -- lost the datamodel
      {
         System.out.printf("***************** Mitigate Error Manager data model not defined. \n");
      }
   }

   private void scrollPaneToBottom() {

   SwingUtilities.invokeLater(new Runnable() {
      @Override
      public void run()
      {
         errorScrollPane.getVerticalScrollBar().setValue(errorScrollPane.getVerticalScrollBar().getMaximum());
      }

      });

   }

   private void updateDatamodel(String projectName, String projectPath, String jobName, String option)
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
      jobCommand.setJobName(jobName);
      jobCommand.setProject(projectName);
      jobCommand.setFullPath(projectPath);
      jobCommand.setCommand(option);

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

   }

   /** This method is called from within the constructor to
    * initialize the form.
    * WARNING: Do NOT modify this code. The content of this method is
    * always regenerated by the Form Editor.
    */
   @SuppressWarnings("unchecked")
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    errorPanel = new javax.swing.JPanel();
    errorScrollPane = new javax.swing.JScrollPane();
    errorTable = new javax.swing.JTable();
    bottomButtonPanel = new javax.swing.JPanel();
    completeButton = new javax.swing.JButton();
    failButton = new javax.swing.JButton();
    readyButton = new javax.swing.JButton();
    unknownButton = new javax.swing.JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
    setTitle("Mitigate Error Manager");

    errorPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(""));

    errorTable.setModel(new javax.swing.table.DefaultTableModel(
      new Object [][] {

      },
      new String [] {
        "Project", "Job", "State", "Complete (%)", "Date", "Path"
      }
    ) {
      Class[] types = new Class [] {
        java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class
      };
      boolean[] canEdit = new boolean [] {
        true, false, false, false, false, false
      };

      public Class getColumnClass(int columnIndex) {
        return types [columnIndex];
      }

      public boolean isCellEditable(int rowIndex, int columnIndex) {
        return canEdit [columnIndex];
      }
    });
    errorScrollPane.setViewportView(errorTable);
    errorTable.getColumnModel().getColumn(2).setCellRenderer(new DiFXJobTableCellRenderer());

    javax.swing.GroupLayout errorPanelLayout = new javax.swing.GroupLayout(errorPanel);
    errorPanel.setLayout(errorPanelLayout);
    errorPanelLayout.setHorizontalGroup(
      errorPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGap(0, 442, Short.MAX_VALUE)
      .addGroup(errorPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addComponent(errorScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 442, Short.MAX_VALUE))
    );
    errorPanelLayout.setVerticalGroup(
      errorPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGap(0, 82, Short.MAX_VALUE)
      .addGroup(errorPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
        .addComponent(errorScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 82, Short.MAX_VALUE))
    );

    bottomButtonPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));

    completeButton.setText("Complete");
    completeButton.setMaximumSize(new java.awt.Dimension(39, 19));
    completeButton.setMinimumSize(new java.awt.Dimension(39, 19));
    completeButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        completeButtonActionPerformed(evt);
      }
    });

    failButton.setText("Fail");
    failButton.setMaximumSize(new java.awt.Dimension(39, 19));
    failButton.setMinimumSize(new java.awt.Dimension(39, 19));
    failButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        failButtonActionPerformed(evt);
      }
    });

    readyButton.setText("Ready");
    readyButton.setMaximumSize(new java.awt.Dimension(39, 19));
    readyButton.setMinimumSize(new java.awt.Dimension(39, 19));
    readyButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        readyButtonActionPerformed(evt);
      }
    });

    unknownButton.setText("Unknown");
    unknownButton.setMaximumSize(new java.awt.Dimension(39, 19));
    unknownButton.setMinimumSize(new java.awt.Dimension(39, 19));
    unknownButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        unknownButtonActionPerformed(evt);
      }
    });

    javax.swing.GroupLayout bottomButtonPanelLayout = new javax.swing.GroupLayout(bottomButtonPanel);
    bottomButtonPanel.setLayout(bottomButtonPanelLayout);
    bottomButtonPanelLayout.setHorizontalGroup(
      bottomButtonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(bottomButtonPanelLayout.createSequentialGroup()
        .addContainerGap()
        .addComponent(completeButton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(failButton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(readyButton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(unknownButton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(103, Short.MAX_VALUE))
    );
    bottomButtonPanelLayout.setVerticalGroup(
      bottomButtonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(bottomButtonPanelLayout.createSequentialGroup()
        .addGroup(bottomButtonPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
          .addComponent(completeButton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(failButton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(readyButton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
          .addComponent(unknownButton, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(bottomButtonPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addComponent(errorPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
    );
    layout.setVerticalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
        .addComponent(errorPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(bottomButtonPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
    );

    pack();
  }// </editor-fold>//GEN-END:initComponents

   private void completeButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_completeButtonActionPerformed
   {//GEN-HEADEREND:event_completeButtonActionPerformed
     

       // Create cache of selected job name, projects and paths
       int rows[] = errorTable.getSelectedRows();

       // convert rows from view into tablemodel perspective
       for (int i = 0; i < rows.length; i++)
       {
          rows[i] = errorTable.convertRowIndexToModel(rows[i]);
       }

       // Is a row selected?
       if (rows.length > 0)
       {
          String[] projects     = new String[rows.length];
          String[] projectPaths = new String[rows.length];
          String[] jobNames     = new String[rows.length];

          DefaultTableModel model = (DefaultTableModel) (errorTable.getModel());

          // Jobs are ordered in the array similar to table ordering
          int numJobs = rows.length;
          for (int index = 0; index < numJobs; index++)
          {
             String project = (model.getValueAt(rows[index], 0)).toString();
             String jobName = (model.getValueAt(rows[index], 1)).toString();
             String path    = (model.getValueAt(rows[index], 5)).toString();
             projects[index]     = project;
             projectPaths[index] = path;
             jobNames[index]     = jobName;
          }

          // march through the rows of jobs, and pretty up the table
          for (int index = 0; index < errorTable.getRowCount(); index++)
          {
             model.setValueAt("", index, 0);
             model.setValueAt("", index, 1);
             model.setValueAt("", index, 2);
             model.setValueAt("", index, 3);
             model.setValueAt("", index, 4);
             model.setValueAt("", index, 5);
          }

          // march through the rows of jobs, and delete from datamodel
          for (int jobIndex = 0; jobIndex < numJobs; jobIndex++)
          {
               Job job = mDataModel.getJob(jobNames[jobIndex], projectPaths[jobIndex]);
               if ( job != null )
               {
                  //job.setState(DiFXSystemStatus.JobStates.COMPLETE);
               }

               // Remove job from jobs queue in data model
               updateDatamodel(projects[jobIndex], projectPaths[jobIndex], jobNames[jobIndex], "ResetComplete");
          }

       } // if (rows.length > 0)

   }//GEN-LAST:event_completeButtonActionPerformed

   private void failButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_failButtonActionPerformed
   {//GEN-HEADEREND:event_failButtonActionPerformed

       // Create cache of selected job name, projects and paths
       int rows[] = errorTable.getSelectedRows();

       // convert rows from view into tablemodel perspective
       for (int i = 0; i < rows.length; i++)
       {
          rows[i] = errorTable.convertRowIndexToModel(rows[i]);
       }

       // Is a row selected?
       if (rows.length > 0)
       {
          String[] projects     = new String[rows.length];
          String[] projectPaths = new String[rows.length];
          String[] jobNames     = new String[rows.length];

          DefaultTableModel model = (DefaultTableModel) (errorTable.getModel());

          // Jobs are ordered in the array similar to table ordering
          int numJobs = rows.length;
          for (int index = 0; index < numJobs; index++)
          {
             String project = (model.getValueAt(rows[index], 0)).toString();
             String jobName = (model.getValueAt(rows[index], 1)).toString();
             String path    = (model.getValueAt(rows[index], 5)).toString();
             projects[index]     = project;
             projectPaths[index] = path;
             jobNames[index]     = jobName;
          }

          // march through the rows of jobs, and pretty up the table
          for (int index = 0; index < errorTable.getRowCount(); index++)
          {
             model.setValueAt("", index, 0);
             model.setValueAt("", index, 1);
             model.setValueAt("", index, 2);
             model.setValueAt("", index, 3);
             model.setValueAt("", index, 4);
             model.setValueAt("", index, 5);
          }

          // march through the rows of jobs, and delete from datamodel
          for (int jobIndex = 0; jobIndex < numJobs; jobIndex++)
          {
               Job job = mDataModel.getJob(jobNames[jobIndex], projectPaths[jobIndex]);
               if ( job != null )
               {
                  //job.setState(DiFXSystemStatus.JobStates.FAILED);
               }

               // Remove job from jobs queue in data model
               updateDatamodel(projects[jobIndex], projectPaths[jobIndex], jobNames[jobIndex], "ResetFail");
          }

       } // if (rows.length > 0)

   }//GEN-LAST:event_failButtonActionPerformed

   private void readyButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_readyButtonActionPerformed
   {//GEN-HEADEREND:event_readyButtonActionPerformed

       // Create cache of selected job name, projects and paths
       int rows[] = errorTable.getSelectedRows();

       // convert rows from view into tablemodel perspective
       for (int i = 0; i < rows.length; i++)
       {
          rows[i] = errorTable.convertRowIndexToModel(rows[i]);
       }

       // Is a row selected?
       if (rows.length > 0)
       {
          String[] projects     = new String[rows.length];
          String[] projectPaths = new String[rows.length];
          String[] jobNames     = new String[rows.length];

          DefaultTableModel model = (DefaultTableModel) (errorTable.getModel());

          // Jobs are ordered in the array similar to table ordering
          int numJobs = rows.length;
          for (int index = 0; index < numJobs; index++)
          {
             String project = (model.getValueAt(rows[index], 0)).toString();
             String jobName = (model.getValueAt(rows[index], 1)).toString();
             String path    = (model.getValueAt(rows[index], 5)).toString();
             projects[index]     = project;
             projectPaths[index] = path;
             jobNames[index]     = jobName;
          }

          // march through the rows of jobs, and pretty up the table
          for (int index = 0; index < errorTable.getRowCount(); index++)
          {
             model.setValueAt("", index, 0);
             model.setValueAt("", index, 1);
             model.setValueAt("", index, 2);
             model.setValueAt("", index, 3);
             model.setValueAt("", index, 4);
             model.setValueAt("", index, 5);
          }

          // march through the rows of jobs, and delete from datamodel
          for (int jobIndex = 0; jobIndex < numJobs; jobIndex++)
          {
               Job job = mDataModel.getJob(jobNames[jobIndex], projectPaths[jobIndex]);
               if ( job != null )
               {
                  //job.setState(DiFXSystemStatus.JobStates.READY);
               }

               // Remove job from jobs queue in data model
               updateDatamodel(projects[jobIndex], projectPaths[jobIndex], jobNames[jobIndex], "ResetReady");
          }

       } // if (rows.length > 0)

   }//GEN-LAST:event_readyButtonActionPerformed

   private void unknownButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_unknownButtonActionPerformed
   {//GEN-HEADEREND:event_unknownButtonActionPerformed

       // Create cache of selected job name, projects and paths
       int rows[] = errorTable.getSelectedRows();

       // convert rows from view into tablemodel perspective
       for (int i = 0; i < rows.length; i++)
       {
          rows[i] = errorTable.convertRowIndexToModel(rows[i]);
       }

       // Is a row selected?
       if (rows.length > 0)
       {
          String[] projects     = new String[rows.length];
          String[] projectPaths = new String[rows.length];
          String[] jobNames     = new String[rows.length];

          DefaultTableModel model = (DefaultTableModel) (errorTable.getModel());

          // Jobs are ordered in the array similar to table ordering
          int numJobs = rows.length;
          for (int index = 0; index < numJobs; index++)
          {
             String project = (model.getValueAt(rows[index], 0)).toString();
             String jobName = (model.getValueAt(rows[index], 1)).toString();
             String path    = (model.getValueAt(rows[index], 5)).toString();
             projects[index]     = project;
             projectPaths[index] = path;
             jobNames[index]     = jobName;
          }

          // march through the rows of jobs, and pretty up the table
          for (int index = 0; index < errorTable.getRowCount(); index++)
          {
             model.setValueAt("", index, 0);
             model.setValueAt("", index, 1);
             model.setValueAt("", index, 2);
             model.setValueAt("", index, 3);
             model.setValueAt("", index, 4);
             model.setValueAt("", index, 5);
          }

          // march through the rows of jobs, and delete from datamodel
          for (int jobIndex = 0; jobIndex < numJobs; jobIndex++)
          {
               Job job = mDataModel.getJob(jobNames[jobIndex], projectPaths[jobIndex]);
               if ( job != null )
               {
                  //job.setState(DiFXSystemStatus.JobStates.UNKNOWN);
               }

               // Remove job from jobs queue in data model
               updateDatamodel(projects[jobIndex], projectPaths[jobIndex], jobNames[jobIndex], "ResetUnknown");
          }

       } // if (rows.length > 0)

   }//GEN-LAST:event_unknownButtonActionPerformed

   /**
    * @param args the command line arguments
    */
   public static void main(String args[])
   {
      java.awt.EventQueue.invokeLater(new Runnable()
      {

         public void run()
         {
            new MitigateErrorManagerUI().setVisible(true);
         }
      });
   }

  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JPanel bottomButtonPanel;
  private javax.swing.JButton completeButton;
  private javax.swing.JPanel errorPanel;
  private javax.swing.JScrollPane errorScrollPane;
  private javax.swing.JTable errorTable;
  private javax.swing.JButton failButton;
  private javax.swing.JButton readyButton;
  private javax.swing.JButton unknownButton;
  // End of variables declaration//GEN-END:variables
}
