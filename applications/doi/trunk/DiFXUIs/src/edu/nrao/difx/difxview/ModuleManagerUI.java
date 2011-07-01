/*
 * ModuleManagerUI.java
 *
 * Created on January 14, 2009, 4:01 PM
 */
package edu.nrao.difx.difxview;

import edu.nrao.difx.difxcontroller.DiFXController;
import edu.nrao.difx.difxdatamodel.DOISystemConfig;

import edu.nrao.difx.difxdatamodel.DiFXDataModel;
import edu.nrao.difx.difxdatamodel.Mark5Unit;
import edu.nrao.difx.difxdatamodel.Mark5Unit.Mark5Commands;
import edu.nrao.difx.difxdatamodel.MessageListener;

import edu.nrao.difx.xmllib.difxmessage.DifxMessage;

import edu.nrao.difx.difxutilities.DiFXModuleTableCellRenderer;
import edu.nrao.difx.xmllib.difxmessage.Body;
import edu.nrao.difx.xmllib.difxmessage.DifxAlert;
import edu.nrao.difx.xmllib.difxmessage.Header;
import edu.nrao.difx.xmllib.difxmessage.ObjectFactory;

import java.util.Iterator;
import java.util.List;

import javax.swing.RowFilter;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

/**
 *
 * @author  mguerra
 */
public class ModuleManagerUI extends javax.swing.JFrame
{

   private static final long serialVersionUID = 6;
   private static ModuleManagerUI mInstance = null;

   // Allow only one data model and controller instance
   static DiFXDataModel  mDataModel;
   static DiFXController mController;

   // Listen for data model updates
   MessageListener mListener;

   // Custom table sorter
   TableRowSorter<TableModel> mSorter;

   /** Creates new form ModuleManagerUI */
   private ModuleManagerUI()
   {
      initComponents();

      mDataModel  = null;
      mController = null;
      mListener   = null;

      mSorter = new TableRowSorter<TableModel>(modulesTable.getModel());
      modulesTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   /** Creates new form ModuleManagerUI */
   private ModuleManagerUI(DiFXDataModel theModel)
   {
      // initialize GUI controls, data model, and controller
      initComponents();

      mDataModel  = theModel;
      mController = null;
      mListener   = null;

      mSorter = new TableRowSorter<TableModel>(modulesTable.getModel());
      modulesTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   /** Creates new form ModuleManagerUI */
   public ModuleManagerUI(DiFXDataModel theModel, DiFXController theController)
   {
      // initialize GUI controls, data model, and controller
      initComponents();

      mDataModel  = theModel;
      mController = theController;
      mListener   = null;
      
      mSorter = new TableRowSorter<TableModel>(modulesTable.getModel());
      modulesTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   public static ModuleManagerUI instance(DiFXDataModel theModel, DiFXController theController)
   {
      if (mInstance == null)
      {
         mInstance = new ModuleManagerUI(theModel, theController);
         mInstance.attachListenerCallback();
      }

      return mInstance;
   }

   private void serviceDataModel()
   {
      // Add code to service the data model
      // System.out.printf("***************** Module Manager service data model. \n");
   }

   private void updateView()
   {
      // System.out.printf("***************** Module Manager update view. \n");

      updateTable();

      //System.out.printf("***************** Module Manager update view complete. \n");
   }

   private void updateTable()
   {
      // fill in module table
      int tableRow = 0;

      // Get mark5Units
      List<Mark5Unit> mark5s = mDataModel.getMark5Units();

      // Verify jobs actually exist
      if (mark5s != null)
      {
         // Size up the table
         DefaultTableModel tableModel = (DefaultTableModel) modulesTable.getModel();
         int numRows = tableModel.getRowCount();
         int numJobs = mark5s.size();

         while (numJobs > numRows)
         {
            tableModel.addRow(new Object[] {null, null, null, null, null, null});
            numJobs--;
         }

         // fill in mark5s table
         Iterator it = mark5s.iterator();
         while (it.hasNext())
         {
            // get job data from model to the screen
            Mark5Unit mark5 = (Mark5Unit) it.next();
            if (mark5 != null)
            {
               // insert mark5 data into the table
               tableModel.setValueAt(mark5.getObjName(),    tableRow, 0);  // Unit name
               tableModel.setValueAt(mark5.getBankAVSN(),   tableRow, 1);  // BankA VSN
               tableModel.setValueAt(mark5.getBankBVSN(),   tableRow, 2);  // BankB VSN
               if (mark5.isStatusCurrent() == true)
               {
                  tableModel.setValueAt(mark5.getState(),    tableRow, 3); // state
               }
               else
               {
                  tableModel.setValueAt("Lost", tableRow, 3);              // exceed valid status

                  // generate mark5statusmessage - comment out generates to many alerts
                  // generateAlert( mark5.getObjName() + " connection is lost and not responding." );
               }
               tableModel.setValueAt(mark5.getActiveBank(), tableRow, 4);  // active bank
               tableModel.setValueAt(mark5.getCurrentJob(), tableRow, 5);  // running job id

            } // -- if (mark5 != null)

            // inc row
            ++tableRow;

            // clean up
            mark5 = null;

         } // -- while (it.hasNext())

         // clean up
         tableModel = null;
         
      } // -- if (mark5s != null)

      // cleanup
      mark5s = null;

      // Filter out empty rows
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

   }

   private void generateAlert(String message)
   {
      // Get data from the data model and update the view
      if (mDataModel.getQueue() != null)
      {
         // create alert message
         String alertMessage =  message;

         ObjectFactory factory = new ObjectFactory();

         // Create header
         Header header = factory.createHeader();
         header.setFrom("DOIView");
         header.setTo("DOIModel");
         header.setMpiProcessId("0");
         header.setIdentifier("doi");
         header.setType("DifxAlertMessage");

         // Create alert informational message
         DifxAlert alert = factory.createDifxAlert();
         alert.setAlertMessage(alertMessage);
         alert.setSeverity(2);

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

   public void attachListenerCallback()
   {
      // Hand the thread manager the implementation of update()...
      if (mDataModel != null)
      {
         this.setTitle(this.getTitle() + " " + DOISystemConfig.DOIVersion);
         System.out.println("***************** Module Manager attach listener.");

         // create listener implementation of update()...
         mListener = new MessageListener()
         {

            @Override
            public void update()
            {
               // System.out.printf("***************** Module Manager service data model and view. \n");
               // ServiceDataModel();
               updateView();
            // System.out.println("***************** Moduel Manager Update data model and view complete. \n");
            }
         };

         // hand data model a listener
         mDataModel.attachListener(mListener);
      } else
      {
         System.out.println("***************** Module Manager listener not attached. \n");
      }
   }

   public void detachListener()
   {
      // remove message listener
      if (mDataModel != null)
      {
         System.out.printf("***************** ModuleManager detach listener. \n");
         mDataModel.detachListener(mListener);
      }
      else
      {
         System.out.println("***************** Module Manager listener not detached. \n");
      }


   }

   /** This method is called from within the constructor to
    * initialize the form.
    * WARNING: Do NOT modify this code. The content of this method is
    * always regenerated by the Form Editor.
    */
  // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
  private void initComponents() {

    commandToolBar = new javax.swing.JToolBar();
    getLoadButton = new javax.swing.JButton();
    resetButton = new javax.swing.JButton();
    dirButton = new javax.swing.JButton();
    startButton = new javax.swing.JButton();
    clearButton = new javax.swing.JButton();
    stopButton = new javax.swing.JButton();
    rebootButton = new javax.swing.JButton();
    powerOffButton = new javax.swing.JButton();
    copyButton = new javax.swing.JButton();
    getVSNButton = new javax.swing.JButton();
    modulesPanel = new javax.swing.JPanel();
    modulesScrollPane = new javax.swing.JScrollPane();
    modulesTable = new javax.swing.JTable();
    bottomPanel = new javax.swing.JPanel();
    closeButton = new javax.swing.JButton();

    setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
    setTitle("Mark5 Units");
    setName("moduleFrame"); // NOI18N

    commandToolBar.setBackground(java.awt.Color.lightGray);
    commandToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
    commandToolBar.setRollover(true);

    getLoadButton.setText("GetLoad");
    getLoadButton.setFocusable(false);
    getLoadButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    getLoadButton.setMaximumSize(new java.awt.Dimension(80, 35));
    getLoadButton.setMinimumSize(new java.awt.Dimension(80, 35));
    getLoadButton.setPreferredSize(new java.awt.Dimension(60, 35));
    getLoadButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    getLoadButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        getLoadButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(getLoadButton);

    resetButton.setText("Reset");
    resetButton.setFocusable(false);
    resetButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    resetButton.setMaximumSize(new java.awt.Dimension(46, 35));
    resetButton.setMinimumSize(new java.awt.Dimension(46, 35));
    resetButton.setPreferredSize(new java.awt.Dimension(46, 35));
    resetButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    resetButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        resetButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(resetButton);

    dirButton.setText("GetDir");
    dirButton.setFocusable(false);
    dirButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    dirButton.setMaximumSize(new java.awt.Dimension(52, 35));
    dirButton.setMinimumSize(new java.awt.Dimension(52, 35));
    dirButton.setPreferredSize(new java.awt.Dimension(52, 35));
    dirButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    dirButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        dirButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(dirButton);

    startButton.setText("Start");
    startButton.setFocusable(false);
    startButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    startButton.setMaximumSize(new java.awt.Dimension(41, 35));
    startButton.setMinimumSize(new java.awt.Dimension(41, 35));
    startButton.setPreferredSize(new java.awt.Dimension(41, 35));
    startButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    startButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        startButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(startButton);

    clearButton.setText("Clear");
    clearButton.setFocusable(false);
    clearButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    clearButton.setMaximumSize(new java.awt.Dimension(44, 35));
    clearButton.setMinimumSize(new java.awt.Dimension(44, 35));
    clearButton.setPreferredSize(new java.awt.Dimension(44, 35));
    clearButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    clearButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        clearButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(clearButton);

    stopButton.setText("Stop");
    stopButton.setFocusable(false);
    stopButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    stopButton.setMaximumSize(new java.awt.Dimension(40, 35));
    stopButton.setMinimumSize(new java.awt.Dimension(40, 35));
    stopButton.setPreferredSize(new java.awt.Dimension(40, 35));
    stopButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    stopButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        stopButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(stopButton);

    rebootButton.setText("Reboot");
    rebootButton.setFocusable(false);
    rebootButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    rebootButton.setMaximumSize(new java.awt.Dimension(56, 35));
    rebootButton.setMinimumSize(new java.awt.Dimension(56, 35));
    rebootButton.setPreferredSize(new java.awt.Dimension(56, 35));
    rebootButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    rebootButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        rebootButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(rebootButton);

    powerOffButton.setText("Poweroff");
    powerOffButton.setFocusable(false);
    powerOffButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    powerOffButton.setMaximumSize(new java.awt.Dimension(68, 35));
    powerOffButton.setMinimumSize(new java.awt.Dimension(68, 35));
    powerOffButton.setPreferredSize(new java.awt.Dimension(68, 35));
    powerOffButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    powerOffButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        powerOffButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(powerOffButton);

    copyButton.setText("Copy");
    copyButton.setFocusable(false);
    copyButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    copyButton.setMaximumSize(new java.awt.Dimension(44, 35));
    copyButton.setMinimumSize(new java.awt.Dimension(44, 35));
    copyButton.setPreferredSize(new java.awt.Dimension(36, 35));
    copyButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    copyButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        copyButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(copyButton);

    getVSNButton.setText("GetVSN");
    getVSNButton.setFocusable(false);
    getVSNButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
    getVSNButton.setMaximumSize(new java.awt.Dimension(57, 35));
    getVSNButton.setMinimumSize(new java.awt.Dimension(57, 35));
    getVSNButton.setPreferredSize(new java.awt.Dimension(57, 35));
    getVSNButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
    getVSNButton.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent evt) {
        getVSNButtonActionPerformed(evt);
      }
    });
    commandToolBar.add(getVSNButton);

    modulesPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 2), "Units/Modules"));

    modulesTable.setAutoCreateRowSorter(true);
    modulesTable.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED));
    modulesTable.setModel(new javax.swing.table.DefaultTableModel(
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
        "Unit", "Bank-A", "Bank-B", "Status", "Active", "Job"
      }
    ) {
      Class[] types = new Class [] {
        java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class, java.lang.String.class
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
    modulesScrollPane.setViewportView(modulesTable);
    modulesTable.getColumnModel().getColumn(3).setCellRenderer(new DiFXModuleTableCellRenderer());

    javax.swing.GroupLayout modulesPanelLayout = new javax.swing.GroupLayout(modulesPanel);
    modulesPanel.setLayout(modulesPanelLayout);
    modulesPanelLayout.setHorizontalGroup(
      modulesPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(modulesScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 569, Short.MAX_VALUE)
    );
    modulesPanelLayout.setVerticalGroup(
      modulesPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(modulesScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 293, Short.MAX_VALUE)
    );

    bottomPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));

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
      .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, bottomPanelLayout.createSequentialGroup()
        .addContainerGap(485, Short.MAX_VALUE)
        .addComponent(closeButton, javax.swing.GroupLayout.PREFERRED_SIZE, 80, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap())
    );
    bottomPanelLayout.setVerticalGroup(
      bottomPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(bottomPanelLayout.createSequentialGroup()
        .addComponent(closeButton, javax.swing.GroupLayout.PREFERRED_SIZE, 31, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
    );

    javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
    getContentPane().setLayout(layout);
    layout.setHorizontalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addComponent(modulesPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addComponent(bottomPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
      .addGroup(layout.createSequentialGroup()
        .addComponent(commandToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, 534, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addContainerGap(47, Short.MAX_VALUE))
    );
    layout.setVerticalGroup(
      layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
      .addGroup(layout.createSequentialGroup()
        .addComponent(commandToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, 33, javax.swing.GroupLayout.PREFERRED_SIZE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
        .addComponent(modulesPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
        .addComponent(bottomPanel, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
    );

    pack();
  }// </editor-fold>//GEN-END:initComponents

   private void closeButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_closeButtonActionPerformed

      // stop listening for updates
      detachListener();

      // close window
      mInstance = null;
      this.dispose();

   }//GEN-LAST:event_closeButtonActionPerformed

   private void getVSNButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_getVSNButtonActionPerformed
   {//GEN-HEADEREND:event_getVSNButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Units are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName  = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5  = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.GETSVN);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }

         } // -- if (mark5 != null)

      } // -- for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_getVSNButtonActionPerformed

   private void getLoadButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_getLoadButtonActionPerformed
   {//GEN-HEADEREND:event_getLoadButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Units are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      // int row = modulesTable.getSelectedRow();
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName  = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5  = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.GETLOAD);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }

         } // -- if (mark5 != null)

      } // -- for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_getLoadButtonActionPerformed

   private void resetButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_resetButtonActionPerformed
   {//GEN-HEADEREND:event_resetButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Units are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName  = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5  = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.RESETMARK5);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }

         } // -- if (mark5 != null)

      } // -- for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_resetButtonActionPerformed

   private void startButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_startButtonActionPerformed
   {//GEN-HEADEREND:event_startButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Units are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5 = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.STARTMARK5A);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }

         } // -- if (mark5 != null)

      } // for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_startButtonActionPerformed

   private void stopButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_stopButtonActionPerformed
   {//GEN-HEADEREND:event_stopButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Units are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName  = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5  = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.STOPMARK5A);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }

         } // -- if (mark5 != null)

      } // -- for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_stopButtonActionPerformed

   private void clearButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_clearButtonActionPerformed
   {//GEN-HEADEREND:event_clearButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Units are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5 = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.CLEAR);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }

         } // -- if (mark5 != null)

      } // -- for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_clearButtonActionPerformed

   private void rebootButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_rebootButtonActionPerformed
   {//GEN-HEADEREND:event_rebootButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Jobs are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
//      int row = modulesTable.getSelectedRow();
//      if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5 = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.REBOOT);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }
            
         } // -- if (mark5 != null)

      } // -- for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_rebootButtonActionPerformed

   private void powerOffButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_powerOffButtonActionPerformed
   {//GEN-HEADEREND:event_powerOffButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Jobs are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5 = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.POWEROFF);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }

         } // -- if (mark5 != null)

      } // -- for (int i = 0; i < numRows; i++)
      
   }//GEN-LAST:event_powerOffButtonActionPerformed

   private void copyButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_copyButtonActionPerformed
   {//GEN-HEADEREND:event_copyButtonActionPerformed

      // -- Create DifxMessage to command the mark 5

      // Jobs are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5 = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage("BANK VSN SCANS");

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }

         } // -- if (mark5 != null)
         
      } // -- for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_copyButtonActionPerformed

   private void dirButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_dirButtonActionPerformed
   {//GEN-HEADEREND:event_dirButtonActionPerformed
      // -- Create DifxMessage to command the mark 5

      // Jobs are ordered similar to table ordering
      int rows[]  = modulesTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = modulesTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = modulesTable.getModel();
         String unitName  = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5  = mDataModel.getMark5Unit(unitName);

         // Create the command
         if (mark5 != null)
         {
            DifxMessage difxMsg = mark5.CreateDiFXCommandMessage(Mark5Commands.GETDIR);

            // send out the message
            if ((mController != null) && (difxMsg != null))
            {
               mController.writeToSocket(difxMsg);
            }
            
         } // -- if (mark5 != null)

      } // -- for (int i = 0; i < numRows; i++)

   }//GEN-LAST:event_dirButtonActionPerformed

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
            new ModuleManagerUI().setVisible(true);
         }
      });
   }
  // Variables declaration - do not modify//GEN-BEGIN:variables
  private javax.swing.JPanel bottomPanel;
  private javax.swing.JButton clearButton;
  private javax.swing.JButton closeButton;
  private javax.swing.JToolBar commandToolBar;
  private javax.swing.JButton copyButton;
  private javax.swing.JButton dirButton;
  private javax.swing.JButton getLoadButton;
  private javax.swing.JButton getVSNButton;
  private javax.swing.JPanel modulesPanel;
  private javax.swing.JScrollPane modulesScrollPane;
  private javax.swing.JTable modulesTable;
  private javax.swing.JButton powerOffButton;
  private javax.swing.JButton rebootButton;
  private javax.swing.JButton resetButton;
  private javax.swing.JButton startButton;
  private javax.swing.JButton stopButton;
  // End of variables declaration//GEN-END:variables
}
