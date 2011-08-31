/*
 * ProjectManagerUI.java
 *
 * Created on May 18, 2008, 1:22 PM
 */
package edu.nrao.difx.difxview;

import javax.swing.JFileChooser;

import java.io.*;
import java.util.*;

import edu.nrao.difx.difxdatamodel.*;
import edu.nrao.difx.difxcontroller.*;

import edu.nrao.difx.difxutilities.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.RowFilter;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

/**
 *
 * @author  mguerra
 */
public class ProjectManagerUI extends javax.swing.JFrame
{

   private static final long serialVersionUID = 3;
   private static ProjectManagerUI mInstance = null;

    // Allow only one data model and controller instance
   static DiFXDataModel  mDataModel;
   static DiFXController mController;

   // Listen for data model updates
   MessageListener mListener;

   // local members to contain the selected project and job
   String mProjectName;
   String mProjectPath;
   String mJobName;

   // counters and flags
   int mJobCount;
   int mTotalDuration;
   boolean mUpdate;
   
   // Custom table sorter
   TableRowSorter<TableModel> mSorter;

   /** Creates new form ProjectManagerUI */
   private ProjectManagerUI()
   {
      // initialize GUI controls
      initComponents();
      
      mDataModel  = null;
      mController = null;
      mListener   = null;
      
      mProjectName = "";
      mProjectPath = "";
      mJobName     = "";
      
      mJobCount      = 0;
      mTotalDuration = 0;
      mUpdate        = true;

      mSorter = new TableRowSorter<TableModel>(jobsTable.getModel());
      jobsTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   /** Creates new form ProjectManagerUI */
   private ProjectManagerUI(DiFXDataModel theModel)
   {
      // initialize GUI controls and data model
      initComponents();

      mDataModel  = theModel;
      mController = null;
      mListener   = null;

      mProjectName = "";
      mProjectPath = "";
      mJobName     = "";

      mJobCount      = 0;
      mTotalDuration = 0;
      mUpdate        = true;

      mSorter = new TableRowSorter<TableModel>(jobsTable.getModel());
      jobsTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   /** Creates new form ProjectManagerUI */
   public ProjectManagerUI(DiFXDataModel theModel, DiFXController theController)
   {
      // initialize GUI controls and data model
      initComponents();
      
      mDataModel  = theModel;
      mController = theController;
      mListener   = null;

      mProjectName = "";
      mProjectPath = "";
      mJobName     = "";

      mJobCount      = 0;
      mTotalDuration = 0;
      mUpdate        = true;

      mSorter = new TableRowSorter<TableModel>(jobsTable.getModel());
      jobsTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   public static ProjectManagerUI instance(DiFXDataModel theModel, DiFXController theController)
   {
      if (mInstance == null)
      {
         mInstance = new ProjectManagerUI(theModel, theController);
         mInstance.attachListenerCallback();
      }

      return mInstance;
   }

   private void serviceDataModel(String projectName, String projectPath, List<String> jobList)
   {
      // Add project to the datamodel
      Project proj = new Project();
      if ( (mDataModel != null) && (proj != null) )
      {
         proj.setObjType("project");
         proj.setObjId(0);
         proj.setObjName(projectName);
         proj.setProjectPath(projectPath);

         // Directly service model with project
         mDataModel.serviceDataModel(proj);

         // Add all the project's jobs to the data model
         Iterator jobIt = jobList.iterator();
         while (jobIt.hasNext())
         {
            // Create new job and fill in the data
            String jobName    = (String) jobIt.next();
            String entireName = projectPath + "/" + jobName;

            Job job = new Job(mDataModel);
            job.setObjType("job");
            job.setObjId(0);
            job.setObjName(jobName);
            job.setJobPath(projectPath);    //-- jobs are contained in the project's folder
            job.setProjectName(projectName);
            job.setProjectPath(projectPath);

            // read job's data file
            job.readJobData();

            // Directly service model with job
            mDataModel.serviceDataModel(job);
         }
      }
      
      // clean up
      proj = null;
   }

   private void updateView()
   {
      // System.out.printf("***************** Project Manager update view. \n");

      if (mUpdate == true)
      {
         // get data from the model
         projectField.setText(mProjectName);
         projDirField.setText(mProjectPath);

         // update the jobs table
         updateTable();

         // update counts
         totalDurationField.setText(Integer.toString(mTotalDuration));
         jobCountField.setText(Integer.toString(mJobCount));
         
      } // -- if (mUpdate == true)

      //System.out.printf("***************** Project Manager update view complete. \n");
   }

   private void updateTable()
   {
      // -- Get the data from the model

      mTotalDuration = 0;
      int tableRow   = 0;

      // Get data from the data model and update the view
      if (mDataModel != null)
      {

         // Verify jobs actually exist
         List<Job> jobs = mDataModel.getJobs(mProjectName, mProjectPath);
         if (jobs != null)
         {
            // Size up the table
            DefaultTableModel tableModel = (DefaultTableModel) jobsTable.getModel();
            int numRows = tableModel.getRowCount();
            int numJobs = jobs.size();

            while (numJobs > numRows)
            {
               tableModel.addRow(new Object[] {null, null, null, null, null, null});
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
                  tableModel.setValueAt(job.getObjName(),            tableRow, 0);  // job
                  //tableModel.setValueAt(job.getStatus(),           tableRow, 1);  // status
                  tableModel.setValueAt(job.getStateString(),        tableRow, 1);  // state
                  tableModel.setValueAt(job.getExecuteTimeSeconds(), tableRow, 2);  // duration
                  tableModel.setValueAt(String.format("%.2f", job.getCompletion()),
                                                                     tableRow, 3);  // completion
                  tableModel.setValueAt(job.inputFileExists(),       tableRow, 4);  // .input
                  tableModel.setValueAt(job.calcFileExists(),        tableRow, 5);  // .calc

                  // increment the total time
                  mTotalDuration = mTotalDuration + job.getExecuteTimeSeconds();

               } // -- if (job != null)

               // inc row
               ++tableRow;

               // clean up
               job = null;
               
            } // -- while (it.hasNext())

            // clean up
            tableModel = null;

            // Filter out empty rows
            mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

         } // -- if (jobs != null)

         // clean up
         jobs = null;
      }
      else
      {
         System.out.printf("***************** Resource Manager data model not defined. \n");
      }

      mJobCount = tableRow;

      //System.out.printf("***************** Project Manager update jobs table complete. \n");

   }

   private void updateQueue(String projectName, String jobName, String option)
   {
      // Update queue with job project relationshop
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

   public void attachListenerCallback()
   {
      // Hand the thread manager the implementation of update()...
      if (mDataModel != null)
      {
         this.setTitle(this.getTitle() + " " + DOISystemConfig.DOIVersion);
         System.out.println("***************** Project Manager attach listener.");

         // create listener implementation of update()...
         mListener = new MessageListener()
         {
            @Override
            public void update()
            {
               // Get handle to GUI and UpdateGUI()
               updateView();
               // ServiceDataModel();
            }
         };

         // hand thread manager a listener
         mDataModel.attachListener(mListener);

      }
      else
      {
         System.out.println("***************** Project Manager listener not attached. \n");
      }
   }

   public void detachListener()
   {
      // remove message listener
      if (mDataModel != null)
      {
         System.out.printf("***************** Project Manager detach listener. \n");
         mDataModel.detachListener(mListener);
      }
      else
      {
         System.out.println("***************** Project Manager listener not detached. \n");
      }

   }
   
   /** This method is called from within the constructor to
    * initialize the form.
    * WARNING: Do NOT modify this code. The content of this method is
    * always regenerated by the Form Editor.
    */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jobsPopupMenu = new javax.swing.JPopupMenu();
        jobsQueueAllMenuItem = new javax.swing.JMenuItem();
        jSeparator1 = new javax.swing.JSeparator();
        jobsQueueAddMenuItem = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JSeparator();
        jobsQueueDeleteMenuItem = new javax.swing.JMenuItem();
        projectToolBar = new javax.swing.JToolBar();
        openProjectButton = new javax.swing.JButton();
        queueToolBar = new javax.swing.JToolBar();
        queueAllButton = new javax.swing.JButton();
        addButton = new javax.swing.JButton();
        deleteButton = new javax.swing.JButton();
        appsToolBar = new javax.swing.JToolBar();
        jobManagerButton = new javax.swing.JButton();
        vewLogButton = new javax.swing.JButton();
        projPanel = new javax.swing.JPanel();
        projectLabel = new javax.swing.JLabel();
        projectField = new javax.swing.JTextField();
        dirLabel = new javax.swing.JLabel();
        projDirField = new javax.swing.JTextField();
        durationLabel = new javax.swing.JLabel();
        durationField = new javax.swing.JTextField();
        secsLabel = new javax.swing.JLabel();
        totalDurationLabel = new javax.swing.JLabel();
        totalDurationField = new javax.swing.JTextField();
        secsLabel1 = new javax.swing.JLabel();
        jobsPanel = new javax.swing.JPanel();
        jobsScrollPane = new javax.swing.JScrollPane();
        jobsTable = new javax.swing.JTable();
        bottomPanel = new javax.swing.JPanel();
        closeButton = new javax.swing.JButton();
        jobCountLabel = new javax.swing.JLabel();
        jobCountField = new javax.swing.JTextField();

        jobsQueueAllMenuItem.setText("Item");
        jobsPopupMenu.add(jobsQueueAllMenuItem);
        jobsPopupMenu.add(jSeparator1);

        jobsQueueAddMenuItem.setText("Item");
        jobsPopupMenu.add(jobsQueueAddMenuItem);
        jobsPopupMenu.add(jSeparator2);

        jobsQueueDeleteMenuItem.setText("Item");
        jobsPopupMenu.add(jobsQueueDeleteMenuItem);

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Project Manager");

        projectToolBar.setBackground(java.awt.Color.lightGray);
        projectToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        projectToolBar.setRollover(true);

        openProjectButton.setText("Open Project");
        openProjectButton.setFocusable(false);
        openProjectButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        openProjectButton.setMaximumSize(new java.awt.Dimension(91, 30));
        openProjectButton.setMinimumSize(new java.awt.Dimension(91, 30));
        openProjectButton.setPreferredSize(new java.awt.Dimension(91, 30));
        openProjectButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        openProjectButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                openProjectButtonActionPerformed(evt);
            }
        });
        projectToolBar.add(openProjectButton);

        queueToolBar.setBackground(java.awt.Color.lightGray);
        queueToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        queueToolBar.setRollover(true);

        queueAllButton.setText("Queue All");
        queueAllButton.setFocusable(false);
        queueAllButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        queueAllButton.setMaximumSize(new java.awt.Dimension(73, 30));
        queueAllButton.setMinimumSize(new java.awt.Dimension(73, 30));
        queueAllButton.setPreferredSize(new java.awt.Dimension(73, 30));
        queueAllButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        queueAllButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                queueAllButtonActionPerformed(evt);
            }
        });
        queueToolBar.add(queueAllButton);

        addButton.setText("Add");
        addButton.setFocusable(false);
        addButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        addButton.setMaximumSize(new java.awt.Dimension(37, 30));
        addButton.setMinimumSize(new java.awt.Dimension(37, 30));
        addButton.setPreferredSize(new java.awt.Dimension(37, 30));
        addButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        addButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                addButtonActionPerformed(evt);
            }
        });
        queueToolBar.add(addButton);

        deleteButton.setText("Remove");
        deleteButton.setFocusable(false);
        deleteButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        deleteButton.setMaximumSize(new java.awt.Dimension(70, 30));
        deleteButton.setMinimumSize(new java.awt.Dimension(70, 30));
        deleteButton.setPreferredSize(new java.awt.Dimension(52, 30));
        deleteButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        deleteButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                deleteButtonActionPerformed(evt);
            }
        });
        queueToolBar.add(deleteButton);

        appsToolBar.setBackground(java.awt.Color.lightGray);
        appsToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        appsToolBar.setRollover(true);

        jobManagerButton.setText("Job Manager");
        jobManagerButton.setFocusable(false);
        jobManagerButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jobManagerButton.setMaximumSize(new java.awt.Dimension(90, 30));
        jobManagerButton.setMinimumSize(new java.awt.Dimension(90, 30));
        jobManagerButton.setPreferredSize(new java.awt.Dimension(90, 30));
        jobManagerButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jobManagerButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                jobManagerButtonActionPerformed(evt);
            }
        });
        appsToolBar.add(jobManagerButton);

        vewLogButton.setText("View Log");
        vewLogButton.setFocusable(false);
        vewLogButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        vewLogButton.setMaximumSize(new java.awt.Dimension(69, 30));
        vewLogButton.setMinimumSize(new java.awt.Dimension(69, 30));
        vewLogButton.setPreferredSize(new java.awt.Dimension(69, 30));
        vewLogButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        vewLogButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                vewLogButtonActionPerformed(evt);
            }
        });
        appsToolBar.add(vewLogButton);

        projPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 2), "Project"));

        projectLabel.setText("Name:");

        projectField.setBackground(new java.awt.Color(238, 238, 238));
        projectField.setBorder(null);

        dirLabel.setText("Directory:");

        projDirField.setBackground(new java.awt.Color(238, 238, 238));
        projDirField.setBorder(null);

        durationLabel.setText("Duration:");

        durationField.setBackground(new java.awt.Color(238, 238, 238));
        durationField.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
        durationField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        secsLabel.setFont(new java.awt.Font("Dialog", 1, 10));
        secsLabel.setText("secs");

        totalDurationLabel.setText("Total Duration:");

        totalDurationField.setBackground(new java.awt.Color(238, 238, 238));
        totalDurationField.setHorizontalAlignment(javax.swing.JTextField.RIGHT);
        totalDurationField.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));

        secsLabel1.setFont(new java.awt.Font("Dialog", 1, 10));
        secsLabel1.setText("secs");

        javax.swing.GroupLayout projPanelLayout = new javax.swing.GroupLayout(projPanel);
        projPanel.setLayout(projPanelLayout);
        projPanelLayout.setHorizontalGroup(
            projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(projPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addGroup(projPanelLayout.createSequentialGroup()
                        .addComponent(projectLabel)
                        .addGap(3, 3, 3)
                        .addComponent(projectField, javax.swing.GroupLayout.PREFERRED_SIZE, 83, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 305, Short.MAX_VALUE)
                        .addComponent(durationLabel))
                    .addComponent(totalDurationLabel, javax.swing.GroupLayout.Alignment.TRAILING)
                    .addGroup(projPanelLayout.createSequentialGroup()
                        .addComponent(dirLabel)
                        .addGap(3, 3, 3)
                        .addComponent(projDirField, javax.swing.GroupLayout.PREFERRED_SIZE, 236, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)))
                .addGroup(projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addGroup(projPanelLayout.createSequentialGroup()
                        .addGap(1, 1, 1)
                        .addComponent(durationField, javax.swing.GroupLayout.PREFERRED_SIZE, 92, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(projPanelLayout.createSequentialGroup()
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(totalDurationField)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(secsLabel1)
                    .addComponent(secsLabel))
                .addContainerGap())
        );
        projPanelLayout.setVerticalGroup(
            projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(projPanelLayout.createSequentialGroup()
                .addGroup(projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(durationLabel)
                    .addComponent(durationField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(secsLabel))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(totalDurationLabel)
                    .addComponent(totalDurationField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(secsLabel1)))
            .addGroup(projPanelLayout.createSequentialGroup()
                .addGroup(projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(projectLabel)
                    .addComponent(projectField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(projPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(dirLabel)
                    .addComponent(projDirField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
        );

        jobsPanel.setBorder(javax.swing.BorderFactory.createEtchedBorder());

        jobsTable.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        jobsTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null},
                {null, null, null, null, null, null}
            },
            new String [] {
                "Job", "State", "Duration", "Completion (%)", ".input", ".calc"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.String.class, java.lang.String.class, java.lang.Integer.class, java.lang.String.class, java.lang.Boolean.class, java.lang.Boolean.class
            };
            boolean[] canEdit = new boolean [] {
                false, false, false, false, false, false
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        jobsTable.setComponentPopupMenu(jobsPopupMenu);
        jobsScrollPane.setViewportView(jobsTable);
        jobsTable.getColumnModel().getColumn(1).setCellRenderer(new DiFXJobTableCellRenderer());

        javax.swing.GroupLayout jobsPanelLayout = new javax.swing.GroupLayout(jobsPanel);
        jobsPanel.setLayout(jobsPanelLayout);
        jobsPanelLayout.setHorizontalGroup(
            jobsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jobsScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 647, Short.MAX_VALUE)
        );
        jobsPanelLayout.setVerticalGroup(
            jobsPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jobsScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 260, Short.MAX_VALUE)
        );

        bottomPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));

        closeButton.setText("Close");
        closeButton.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED));
        closeButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                closeButtonActionPerformed(evt);
            }
        });

        jobCountLabel.setText("Job Count:");

        jobCountField.setBackground(new java.awt.Color(238, 238, 238));
        jobCountField.setEditable(false);
        jobCountField.setBorder(null);

        javax.swing.GroupLayout bottomPanelLayout = new javax.swing.GroupLayout(bottomPanel);
        bottomPanel.setLayout(bottomPanelLayout);
        bottomPanelLayout.setHorizontalGroup(
            bottomPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(bottomPanelLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jobCountLabel, javax.swing.GroupLayout.PREFERRED_SIZE, 75, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jobCountField, javax.swing.GroupLayout.PREFERRED_SIZE, 37, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 419, Short.MAX_VALUE)
                .addComponent(closeButton, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addContainerGap())
        );
        bottomPanelLayout.setVerticalGroup(
            bottomPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(bottomPanelLayout.createSequentialGroup()
                .addGroup(bottomPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(closeButton, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(bottomPanelLayout.createSequentialGroup()
                        .addContainerGap()
                        .addGroup(bottomPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                            .addComponent(jobCountLabel)
                            .addComponent(jobCountField, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))))
                .addContainerGap(13, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(projectToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(30, 30, 30)
                .addComponent(queueToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 191, Short.MAX_VALUE)
                .addComponent(appsToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addComponent(bottomPanel, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(projPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jobsPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(queueToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.TRAILING, false)
                        .addComponent(projectToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addComponent(appsToolBar, javax.swing.GroupLayout.Alignment.LEADING, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(projPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jobsPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(bottomPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
   private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed

       // remove listener
       detachListener();

       // close window
       mInstance = null;
       this.dispose();
        
}//GEN-LAST:event_closeButtonActionPerformed

    private void openProjectButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_openProjectButtonActionPerformed

       // create a file chooser
       final JFileChooser fc = new JFileChooser();

       fc.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
       fc.setCurrentDirectory(new File(DOISystemConfig.DiFXHome));
       fc.setApproveButtonText("Select Project");

       // in response to a button click:
       int returnVal = fc.showOpenDialog(this);

       // get project and jobs into the tree
       if (returnVal == JFileChooser.APPROVE_OPTION)
       {
          // -- Get the list of jobs into data model

          // filter only on .input file
          InputFileFilter filter = new InputFileFilter();
          File selectedFile      = fc.getSelectedFile();
          String[] jobs          = selectedFile.list(filter);

          // remove extension .input from each job name
          int i = 0;
          while (i < jobs.length)
          {
             jobs[i] = jobs[i].substring(0, jobs[i].indexOf(".input"));
             i++;
          }

          // create list of job names
          List<String> jobList;
          jobList = new ArrayList<String>();
          jobList.addAll(Arrays.asList(jobs));
          
          // add the project and all associated jobs to the data model
          mProjectName = selectedFile.getName();
          mProjectPath = selectedFile.getPath();
          serviceDataModel(mProjectName, mProjectPath, jobList);
          
          // clean up
          jobList.clear();
          jobs = null;
       }
       
}//GEN-LAST:event_openProjectButtonActionPerformed

    private void addButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_addButtonActionPerformed

       // Get the selected job(s)
       int rows[] = jobsTable.getSelectedRows();
       if ( (rows.length != 0) )
       {
          // no updates
          mUpdate = false;

          // save project path and selected job names
          String completeProjName = projDirField.getText();
          String jobNames[]       = new String[rows.length];

          // Determine if any of the selected jobs is missing files
          boolean okToContinue = true;
          int     index = 0;
          while (okToContinue && (index < rows.length))
          {
             boolean inputExists = ((Boolean)(jobsTable.getValueAt(rows[index], 4))).booleanValue();
             boolean calcExists  = ((Boolean)(jobsTable.getValueAt(rows[index], 5))).booleanValue();

             okToContinue = (inputExists && calcExists);
             index++;
          }

          // Insert into the queue only if all the jobs have their files
          if (okToContinue)
          {
             for (int i = 0; i < rows.length; i++)
             {
                // insert job into the jobs queue
                jobNames[i] = jobsTable.getValueAt(rows[i], 0).toString();
                updateQueue(completeProjName, jobNames[i], "Insert") ;
             }
          }
          else // -- if (okToContinue)
          {
             // Raise dialog
             JFrame frame = null;
             JOptionPane.showMessageDialog(frame,
                                           "Job missing files, jobs not added to queue.",
                                           "Job warning",
                                            JOptionPane.WARNING_MESSAGE);

          }

          // allow updates
          mUpdate = true;
       }
       else  // -- if ( (rows.length != 0) )
       {
          // Raise dialog
          JFrame frame = null;
          JOptionPane.showMessageDialog(frame,
                                        "Job not selected, select a job to add.",
                                        "Job warning",
                                         JOptionPane.WARNING_MESSAGE);
       }
       
    }//GEN-LAST:event_addButtonActionPerformed

    private void queueAllButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_queueAllButtonActionPerformed
    {//GEN-HEADEREND:event_queueAllButtonActionPerformed

       // Select all jobs
       jobsTable.selectAll();

       // Get the selected job(s)
       int rows[] = jobsTable.getSelectedRows();
       if ( (rows.length != 0) )
       {
          // no updates
          mUpdate = false;

          // save project path and selected job names
          String completeProjName = projDirField.getText();
          String jobNames[]       = new String[rows.length];

          // Determine if any of the selected jobs is missing files
          boolean okToContinue = true;
          int     index = 0;
          while (okToContinue && (index < rows.length))
          {
             boolean inputExists = ((Boolean)(jobsTable.getValueAt(rows[index], 4))).booleanValue();
             boolean calcExists  = ((Boolean)(jobsTable.getValueAt(rows[index], 5))).booleanValue();

             okToContinue = (inputExists && calcExists);
             index++;
          }

          // Insert into the queue only if all the jobs have thier files
          if (okToContinue)
          {
             for (int i = 0; i < rows.length; i++)
             {
                // insert job into the jobs queue
                jobNames[i] = jobsTable.getValueAt(rows[i], 0).toString();
                updateQueue(completeProjName, jobNames[i], "Insert") ;
             }
          }
          else // -- one of the jobs is missing a file
          {
             // Raise dialog
             JFrame frame = null;
             JOptionPane.showMessageDialog(frame,
                                           "Job missing files, jobs not added to queue.",
                                           "Job warning",
                                            JOptionPane.WARNING_MESSAGE);

          }

          // allow updates
          mUpdate = true;

       } // -- if ( (rows.length != 0) )

    }//GEN-LAST:event_queueAllButtonActionPerformed

    private void deleteButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_deleteButtonActionPerformed
    {//GEN-HEADEREND:event_deleteButtonActionPerformed

       // Get the selected job(s)
       int rows[] = jobsTable.getSelectedRows();
       if ( (rows.length != 0) )
       {
          // no updates
          mUpdate = false;

          // Raise dialog, verify deletion from queue
          Object[] options ={"Yes", "No"};
          JFrame frame = null;
          int opt = JOptionPane.showOptionDialog(frame,
                               "Selected jobs will be removed from Queue. \n" +
                               "Do you wish to continue and remove the job(s).",
                               "Job Queue message",
                               JOptionPane.YES_NO_OPTION,
                               JOptionPane.QUESTION_MESSAGE,
                               null, options, options[1]);

          // Yes, delete selected rows
          if (opt == 0)
          {
             // save project path name
             String completeProjName = projDirField.getText();

             // get name of the job
             String jobNames[] = new String[rows.length];

             for (int i = 0; i < rows.length; i++)
             {
                jobNames[i] = jobsTable.getValueAt(rows[i], 0).toString();

                // delete jobs from the jobs queue
                updateQueue(completeProjName, jobNames[i], "Delete") ;
             }
          }

          // allow updates
          mUpdate = true;
       }
       else  // -- if ( (rows.length != 0) )
       {
          // Raise dialog
          JFrame frame = null;
          JOptionPane.showMessageDialog(frame,
                                        "Job not selected, select a job to delete.",
                                        "Job warning",
                                         JOptionPane.WARNING_MESSAGE);
       }
    }//GEN-LAST:event_deleteButtonActionPerformed

    private void jobManagerButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_jobManagerButtonActionPerformed
    {//GEN-HEADEREND:event_jobManagerButtonActionPerformed

       // save project name
       mProjectName = projectField.getText();

       // get the job manager associated with the selected job
       int row = jobsTable.getSelectedRow();
       if (row != -1)
       {
          // get name of the job
          mJobName = jobsTable.getValueAt(row, 0).toString();

          // Display the GUI
          JobManagerUI theJM = new JobManagerUI(mDataModel, mController, mJobName);
          theJM.attachListenerCallback();
          theJM.setVisible(true);
       }
       else  // -- if (row != -1)
       {
          // Raise dialog
          JFrame frame = null;
          JOptionPane.showMessageDialog(frame,
                                        "Job not selected, select a job.",
                                        "Job warning",
                                         JOptionPane.WARNING_MESSAGE);
       }

       // clear
       mJobName = "";

}//GEN-LAST:event_jobManagerButtonActionPerformed

    private void vewLogButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_vewLogButtonActionPerformed
    {//GEN-HEADEREND:event_vewLogButtonActionPerformed

      int row = jobsTable.getSelectedRow();
      if (row != -1)
      {
         // Display the UI
         PrintDataManagerUI thePDM = new PrintDataManagerUI();
         thePDM.setVisible(true);

         // get name of the job
         String jobLogFile = (mProjectPath + "/" + (jobsTable.getValueAt(row, 0).toString())).trim() + ".difxlog";
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

      }

    }//GEN-LAST:event_vewLogButtonActionPerformed

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
            ProjectManagerUI theGUI = new ProjectManagerUI();

            // Display the GUI.
            theGUI.setVisible(true);

         }
      });
   }
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton addButton;
    private javax.swing.JToolBar appsToolBar;
    private javax.swing.JPanel bottomPanel;
    private javax.swing.JButton closeButton;
    private javax.swing.JButton deleteButton;
    private javax.swing.JLabel dirLabel;
    private javax.swing.JTextField durationField;
    private javax.swing.JLabel durationLabel;
    private javax.swing.JSeparator jSeparator1;
    private javax.swing.JSeparator jSeparator2;
    private javax.swing.JTextField jobCountField;
    private javax.swing.JLabel jobCountLabel;
    private javax.swing.JButton jobManagerButton;
    private javax.swing.JPanel jobsPanel;
    private javax.swing.JPopupMenu jobsPopupMenu;
    private javax.swing.JMenuItem jobsQueueAddMenuItem;
    private javax.swing.JMenuItem jobsQueueAllMenuItem;
    private javax.swing.JMenuItem jobsQueueDeleteMenuItem;
    private javax.swing.JScrollPane jobsScrollPane;
    private javax.swing.JTable jobsTable;
    private javax.swing.JButton openProjectButton;
    private javax.swing.JTextField projDirField;
    private javax.swing.JPanel projPanel;
    private javax.swing.JTextField projectField;
    private javax.swing.JLabel projectLabel;
    private javax.swing.JToolBar projectToolBar;
    private javax.swing.JButton queueAllButton;
    private javax.swing.JToolBar queueToolBar;
    private javax.swing.JLabel secsLabel;
    private javax.swing.JLabel secsLabel1;
    private javax.swing.JTextField totalDurationField;
    private javax.swing.JLabel totalDurationLabel;
    private javax.swing.JButton vewLogButton;
    // End of variables declaration//GEN-END:variables
}
