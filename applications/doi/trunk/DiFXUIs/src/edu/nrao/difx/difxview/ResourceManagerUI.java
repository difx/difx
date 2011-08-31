/*
 * ResourceManagerUI.java
 *
 * Created on March 7, 2008, 1:47 PM
 */
package edu.nrao.difx.difxview;

import edu.nrao.difx.xmllib.difxmessage.*;
import edu.nrao.difx.difxdatamodel.*;
import edu.nrao.difx.difxcontroller.*;

import edu.nrao.difx.difxdatamodel.Mark5Unit.Mark5Commands;
import edu.nrao.difx.difxutilities.DiFXResourceTableCellRenderer;

import java.util.*;

import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.RowFilter;
import javax.swing.table.TableModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableRowSorter;

/**
 *
 * @author  mguerra
 */
public class ResourceManagerUI extends javax.swing.JFrame
{

   private static final long serialVersionUID = 5;
   private static ResourceManagerUI mInstance = null;
   
   // Allow only one data model and controller instance
   static DiFXDataModel  mDataModel;
   static DiFXController mController;

   // Listen for data model updates
   MessageListener mListener;

   // Custom table sorter
   TableRowSorter<TableModel> mSorter;

   /** Creates new form ResourceManagerUI */
   private ResourceManagerUI()
   {
      // initialize GUI controls
      initComponents();

      mDataModel  = null;
      mController = null;
      mListener   = null;

      mSorter = new TableRowSorter<TableModel>(resourceTable.getModel());
      resourceTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   /** Creates new form ResourceManagerUI */
   private ResourceManagerUI(DiFXDataModel theModel)
   {
      // initialize GUI controls and data model
      initComponents();

      mDataModel  = theModel;
      mController = null;
      mListener   = null;

      mSorter = new TableRowSorter<TableModel>(resourceTable.getModel());
      resourceTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   /** Creates new form ResourceManagerUI */
   public ResourceManagerUI(DiFXDataModel theModel, DiFXController theController)
   {
      // initialize GUI controls and data model
      initComponents();

      mDataModel  = theModel;
      mController = theController;
      mListener   = null;

      mSorter = new TableRowSorter<TableModel>(resourceTable.getModel());
      resourceTable.setRowSorter(mSorter);
      mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));
   }

   public static ResourceManagerUI instance(DiFXDataModel theModel, DiFXController theController)
   {
      if (mInstance == null)
      {
         mInstance = new ResourceManagerUI(theModel, theController);
         mInstance.attachListenerCallback();
      }

      return mInstance;
   }

   private void serviceDataModel()
   {
      // Add code to service the data model
      System.out.printf("***************** Resource Manager service data model. \n");

   }

   private void updateView()
   {
      // System.out.printf("***************** Resource Manager update view. \n");

      updateTable();

      //System.out.printf("***************** Resource Manager update view complete. \n");
   }

   private void updateTable()
   {

      // Get data from the data model and update the view
      if (mDataModel != null)
      {
         DefaultTableModel tableModel = (DefaultTableModel) resourceTable.getModel();

         // define here to use with mark5s below
         int numProcs = 0;

         // verify processors exist
         List<ProcessorNode> processorNodes = mDataModel.getProcessorNodes();
         if (processorNodes != null)
         {
            // Size up the table
            int numRows  = tableModel.getRowCount();
            numProcs     = processorNodes.size();
            while (numProcs > numRows)
            {
               tableModel.addRow(new Object[] {null, null, null, null, null, null, null, null, null, null, null});
               numProcs--;
            }

            // Fill table with the processor node data
            for (int i = 0; i < processorNodes.size(); i++)
            {
               tableModel.setValueAt(processorNodes.get(i).getObjName(),    i, 0);
               tableModel.setValueAt(processorNodes.get(i).getNumCPUs(),    i, 1);
               tableModel.setValueAt(processorNodes.get(i).getNumCores(),   i, 2);
               tableModel.setValueAt(processorNodes.get(i).getBogusGHz(),   i, 3);
               tableModel.setValueAt(processorNodes.get(i).getTypeString(), i, 4);
               tableModel.setValueAt(processorNodes.get(i).getEnabled(),    i, 5);
               if (processorNodes.get(i).isStatusCurrent() == true)
               {
                  tableModel.setValueAt(processorNodes.get(i).getState(),   i, 6);
               }
               else
               {
                  tableModel.setValueAt("Lost", i, 6);

                  // generate mark5statusmessage

               }
               tableModel.setValueAt(processorNodes.get(i).getCpuLoad(),    i, 7);
               tableModel.setValueAt(processorNodes.get(i).getMemLoad(),    i, 8);
               tableModel.setValueAt(processorNodes.get(i).getNetRxRate(),  i, 9);
               tableModel.setValueAt(processorNodes.get(i).getNetTxRate(),  i, 10);
            }
            
         } // -- if (processorNodes != null)

         // Fill the table with mark5s data
         List<Mark5Unit> mark5Units = mDataModel.getMark5Units();
         if (mark5Units != null)
         {
            // Size up the table
            int numRows   = tableModel.getRowCount();
            int numMark5s = ( mark5Units.size() + numProcs );
            while (numMark5s > numRows)
            {
               tableModel.addRow(new Object[] {null, null, null, null, null, null, null, null, null, null, null});
               numMark5s--;
            }

            // Fill in the table
            for (int i = 0; i < mark5Units.size(); i++)
            {
               int j = processorNodes.size() + i;
               tableModel.setValueAt(mark5Units.get(i).getObjName(),    j, 0);
               tableModel.setValueAt(mark5Units.get(i).getNumCPUs(),    j, 1);
               tableModel.setValueAt(mark5Units.get(i).getNumCores(),   j, 2);
               tableModel.setValueAt(mark5Units.get(i).getBogusGHz(),   j, 3);
               tableModel.setValueAt(mark5Units.get(i).getTypeString(), j, 4);
               tableModel.setValueAt(mark5Units.get(i).getEnabled(),    j, 5);
               if (mark5Units.get(i).isStatusCurrent() == true)
               {
                  tableModel.setValueAt(mark5Units.get(i).getState(), j, 6);
               }
               else
               {
                  tableModel.setValueAt("Lost", j, 6);

                  // generate mark5statusmessage
               }
               tableModel.setValueAt(mark5Units.get(i).getCpuLoad(),    j, 7);
               tableModel.setValueAt(mark5Units.get(i).getMemLoad(),    j, 8);
               tableModel.setValueAt(mark5Units.get(i).getNetRxRate(),  j, 9);
               tableModel.setValueAt(mark5Units.get(i).getNetTxRate(),  j, 10);
            }

         } // --  if (mark5Units != null)

         // clean up
         processorNodes = null;
         mark5Units     = null;
         tableModel     = null;
         
         // Filter out empty rows
         mSorter.setRowFilter(RowFilter.regexFilter("^[a-zA-Z]", 0));

      }
      else
      {
         System.out.printf("***************** Resource Manager data model not defined. \n");
      }

   }

   public void attachListenerCallback()
   {
      // check dataModel and hand it an implementation of update()...
      if (mDataModel != null)
      {
         this.setTitle(this.getTitle() + " " + DOISystemConfig.DOIVersion);
         System.out.println("***************** Resource Manager attach listener.");

         // create listener implementation of update()...
         mListener = new MessageListener()
         {
            @Override
            public void update()
            {
               // Get handle to GUI and UpdateGUI()
               //System.out.printf("***************** Resource Manager service data model and view. \n");
               //ServiceDataModel();
               updateView();
               //System.out.println("***************** Resource Manager service data model and view complete. \n");
            }
         };

         // hand DataModel a call back listener
         mDataModel.attachListener(mListener);
      }
      else
      {
         System.out.println("***************** Resource Manager listener not attached . \n");
      }
   }

   public void detachListener()
   {
      // remove message listener
      if (mDataModel != null)
      {
         System.out.printf("***************** Resource Manager detach listener. \n");
         mDataModel.detachListener(mListener);
      }
      else
      {
         System.out.println("***************** Resource Manager listener not detached. \n");
      }

   }

   /** This method is called from within the constructor to
    * initialize the form.
    * WARNING: Do NOT modify this code. The content of this method is
    * always regenerated by the Form Editor.
    */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        resourceToolBar = new javax.swing.JToolBar();
        enableButton = new javax.swing.JButton();
        disableButton = new javax.swing.JButton();
        addButton = new javax.swing.JButton();
        removeButton = new javax.swing.JButton();
        killButton = new javax.swing.JButton();
        appsToolBar = new javax.swing.JToolBar();
        rebootButton = new javax.swing.JButton();
        powerOffButton = new javax.swing.JButton();
        resetButton = new javax.swing.JButton();
        clusterPanel = new javax.swing.JPanel();
        clusterScrollPane = new javax.swing.JScrollPane();
        resourceTable = new javax.swing.JTable();
        bottomPanel = new javax.swing.JPanel();
        closeButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
        setTitle("Resource Manager");

        resourceToolBar.setBackground(java.awt.Color.lightGray);
        resourceToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        resourceToolBar.setRollover(true);

        enableButton.setText("Enable");
        enableButton.setFocusable(false);
        enableButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        enableButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        enableButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                enableButtonActionPerformed(evt);
            }
        });
        resourceToolBar.add(enableButton);

        disableButton.setText("Disable");
        disableButton.setFocusable(false);
        disableButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        disableButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        disableButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                disableButtonActionPerformed(evt);
            }
        });
        resourceToolBar.add(disableButton);

        addButton.setText("Add New");
        addButton.setToolTipText("currently not implemented");
        addButton.setEnabled(false);
        addButton.setFocusable(false);
        addButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        addButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        resourceToolBar.add(addButton);

        removeButton.setText("Remove Existing");
        removeButton.setToolTipText("currently not implemented");
        removeButton.setEnabled(false);
        removeButton.setFocusable(false);
        removeButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        removeButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        resourceToolBar.add(removeButton);

        killButton.setText("Kill Mpifxcorr");
        killButton.setFocusable(false);
        killButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        killButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        killButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                killButtonActionPerformed(evt);
            }
        });
        resourceToolBar.add(killButton);

        appsToolBar.setBackground(java.awt.Color.lightGray);
        appsToolBar.setBorder(new javax.swing.border.SoftBevelBorder(javax.swing.border.BevelBorder.RAISED));
        appsToolBar.setRollover(true);

        rebootButton.setText("Reboot");
        rebootButton.setFocusable(false);
        rebootButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        rebootButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        rebootButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                rebootButtonActionPerformed(evt);
            }
        });
        appsToolBar.add(rebootButton);

        powerOffButton.setText("PowerOff");
        powerOffButton.setFocusable(false);
        powerOffButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        powerOffButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        powerOffButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                powerOffButtonActionPerformed(evt);
            }
        });
        appsToolBar.add(powerOffButton);

        resetButton.setText("Reset");
        resetButton.setFocusable(false);
        resetButton.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        resetButton.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        resetButton.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                resetButtonActionPerformed(evt);
            }
        });
        appsToolBar.add(resetButton);

        clusterPanel.setBorder(javax.swing.BorderFactory.createTitledBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(0, 0, 0), 2), "Cluster Nodes"));

        resourceTable.setBorder(javax.swing.BorderFactory.createEtchedBorder());
        resourceTable.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null},
                {null, null, null, null, null, null, null, null, null, null, null}
            },
            new String [] {
                "Name", "CPUs", "Cores", "GHz", "Type", "Enabled", "State", "CPU Load", "Memory Load", "Rx Rate", "Tx Rate"
            }
        ) {
            Class[] types = new Class [] {
                java.lang.String.class, java.lang.Integer.class, java.lang.Integer.class, java.lang.Float.class, java.lang.String.class, java.lang.Boolean.class, java.lang.String.class, java.lang.Float.class, java.lang.Float.class, java.lang.Integer.class, java.lang.Integer.class
            };
            boolean[] canEdit = new boolean [] {
                false, false, false, false, false, false, false, false, false, false, false
            };

            public Class getColumnClass(int columnIndex) {
                return types [columnIndex];
            }

            public boolean isCellEditable(int rowIndex, int columnIndex) {
                return canEdit [columnIndex];
            }
        });
        resourceTable.setAutoCreateRowSorter(true);
        clusterScrollPane.setViewportView(resourceTable);
        resourceTable.getColumnModel().getColumn(6).setCellRenderer(new DiFXResourceTableCellRenderer());

        javax.swing.GroupLayout clusterPanelLayout = new javax.swing.GroupLayout(clusterPanel);
        clusterPanel.setLayout(clusterPanelLayout);
        clusterPanelLayout.setHorizontalGroup(
            clusterPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(clusterScrollPane, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 888, Short.MAX_VALUE)
        );
        clusterPanelLayout.setVerticalGroup(
            clusterPanelLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(clusterScrollPane, javax.swing.GroupLayout.DEFAULT_SIZE, 345, Short.MAX_VALUE)
        );

        bottomPanel.setBorder(javax.swing.BorderFactory.createBevelBorder(javax.swing.border.BevelBorder.RAISED));

        closeButton.setText("Close");
        closeButton.setBorder(javax.swing.BorderFactory.createEtchedBorder(javax.swing.border.EtchedBorder.RAISED));
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
                .addContainerGap(804, Short.MAX_VALUE)
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
            .addGroup(layout.createSequentialGroup()
                .addComponent(resourceToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 325, Short.MAX_VALUE)
                .addComponent(appsToolBar, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
            .addComponent(clusterPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(bottomPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING, false)
                    .addComponent(appsToolBar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                    .addComponent(resourceToolBar, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(clusterPanel, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
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

    private void enableButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_enableButtonActionPerformed

       // Get list of selected rows
       int rows[] = resourceTable.getSelectedRows();
       
       // Deselect the rows
       if (rows.length > 0)
       {          
          // Create the DOI message, fill in header
          ObjectFactory factory = new ObjectFactory();
          Header header = factory.createHeader();
          header.setFrom("DOIView");
          header.setTo("DOIModel");
          header.setMpiProcessId("0");
          header.setIdentifier("doi");
          header.setType("DOIMessage");

          // fill in the resource data
          DoiResourceConfig resource = factory.createDoiResourceConfig();          
          resource.setEnabled(true);
          TableModel tableModel = resourceTable.getModel();
          resource.setName(tableModel.getValueAt(rows[0], 0).toString());
          resource.setNumCPUs( Short.valueOf(tableModel.getValueAt(rows[0], 1).toString()) );
          resource.setNumCores( Short.valueOf(tableModel.getValueAt(rows[0], 2).toString()) );
          resource.setBogusGHz( Float.valueOf(tableModel.getValueAt(rows[0], 3).toString()) );

          String strType = tableModel.getValueAt(rows[0], 4).toString();
          if ( strType.equals("processor") )
          {
             resource.setType((short)(0));             
          }
          else if ( strType.equals("mark5") )
          {
             resource.setType((short)(1));                          
          }
          else if ( strType.equals("manager") )
          {
             resource.setType((short)(2));                                       
          }
          else
          {
             // -- Invalid type
             System.out.printf("******** Resource manager - invalid resource type \n");
          }
             
          // set resource data into the body
          Body body = factory.createBody();
          body.setDoiResourceConfig(resource);

          // update the data model with DifxMessage
          DifxMessage difxMsg = factory.createDifxMessage();
          difxMsg.setHeader(header);
          difxMsg.setBody(body);
          mDataModel.serviceDataModel(difxMsg);

          resourceTable.removeRowSelectionInterval(rows[0], rows.length);
       }
       
    }//GEN-LAST:event_enableButtonActionPerformed

    private void disableButtonActionPerformed(java.awt.event.ActionEvent evt) {//GEN-FIRST:event_disableButtonActionPerformed

       // Get list of selected rows
       int rows[] = resourceTable.getSelectedRows();
       
       // Create message to enable/disable rows
       for (int i = 0; i < rows.length; i++)
       {
          TableModel tableModel = resourceTable.getModel();
          if (tableModel.getValueAt(rows[i], 6).toString().equalsIgnoreCase("Idle"))
          {
             // Create the DOI message, fill in header
             ObjectFactory factory = new ObjectFactory();
             Header header = factory.createHeader();
             header.setFrom("DOIView");
             header.setTo("DOIModel");
             header.setMpiProcessId("0");
             header.setIdentifier("doi");
             header.setType("DOIMessage");

             // fill in the resource data
             DoiResourceConfig resourceCfg = factory.createDoiResourceConfig();
             resourceCfg.setEnabled(false);

             resourceCfg.setName(tableModel.getValueAt(rows[i], 0).toString());
             resourceCfg.setNumCPUs( Short.valueOf(tableModel.getValueAt(rows[i], 1).toString()) );
             resourceCfg.setNumCores( Short.valueOf(tableModel.getValueAt(rows[i], 2).toString()) );
             resourceCfg.setBogusGHz( Float.valueOf(tableModel.getValueAt(rows[i], 3).toString()) );

             String strType = tableModel.getValueAt(rows[i], 4).toString();
             if ( strType.equals("processor") )
             {
                resourceCfg.setType((short)(0));
             }
             else if ( strType.equals("mark5") )
             {
                resourceCfg.setType((short)(1));
             }
             else if ( strType.equals("manager") )
             {
                resourceCfg.setType((short)(2));
             }
             else
             {
                // -- Invalid type
                System.out.printf("******** Resource manager - invalid resource type \n");
             }

             // set resource config data into the body
             Body body = factory.createBody();
             body.setDoiResourceConfig(resourceCfg);

             // update the data model with DifxMessage
             DifxMessage difxMsg = factory.createDifxMessage();
             difxMsg.setHeader(header);
             difxMsg.setBody(body);
             mDataModel.serviceDataModel(difxMsg);

             // Deselect the rows
             resourceTable.removeRowSelectionInterval(rows[i], 1);
          }
       }

    }//GEN-LAST:event_disableButtonActionPerformed

    private void rebootButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_rebootButtonActionPerformed
    {//GEN-HEADEREND:event_rebootButtonActionPerformed
      // -- Create DifxMessage to command the mark 5 only, do not allow processor reboot

      // Jobs are ordered similar to table ordering
      int rows[]  = resourceTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = resourceTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = resourceTable.getModel();
         String unitName  = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5  = mDataModel.getMark5Unit(unitName);

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

       // -- Create DifxMessage to command the mark 5, do not allow processor reboot
      // -- Create DifxMessage to command the mark 5

      // Jobs are ordered similar to table ordering
      int rows[]  = resourceTable.getSelectedRows();
      int numRows = rows.length;

      // convert rows from view into tablemodel perspective
      for (int i = 0; i < numRows; i++)
      {
         rows[i] = resourceTable.convertRowIndexToModel(rows[i]);
      }

      // Get mark5 unit and command it
      //int row = modulesTable.getSelectedRow();
      //if (row > -1)
      for (int i = 0; i < numRows; i++)
      {
         TableModel model = resourceTable.getModel();
         String unitName  = (String) model.getValueAt(rows[i], 0);
         Mark5Unit mark5  = mDataModel.getMark5Unit(unitName);

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

    private void resetButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_resetButtonActionPerformed
    {//GEN-HEADEREND:event_resetButtonActionPerformed
      // -- Create DifxMessage to command the mark 5, do not allow processor reboot

      // Get mark5 unit name
      int row = resourceTable.getSelectedRow();
      if (row > -1)
      {
         TableModel model = resourceTable.getModel();
         String unitName  = (String) model.getValueAt(row, 0);
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
         }
      }

    }//GEN-LAST:event_resetButtonActionPerformed

    private void killButtonActionPerformed(java.awt.event.ActionEvent evt)//GEN-FIRST:event_killButtonActionPerformed
    {//GEN-HEADEREND:event_killButtonActionPerformed

       // stop updates and stop queue
       //mUpdate = false;

       killMpifxcorr();

       // Allow updates
       //mUpdate = true;


    }//GEN-LAST:event_killButtonActionPerformed

   private void killMpifxcorr()
   {
      // Stop the current running job
      if (mDataModel != null)
      {
         if (mDataModel.getQueue() != null)
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
                  (mDataModel.getQueue()).killMpifxcorr();
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

      } // if (mDatamodel != null)

   }

   /**
    * @param args the command line arguments
    */
   public static void main(String args[])
   {

      java.awt.EventQueue.invokeLater(new Runnable() {

         @Override
         public void run()
         {

            // Instantiate the GUI
            ResourceManagerUI theGUI = new ResourceManagerUI();

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
    private javax.swing.JPanel clusterPanel;
    private javax.swing.JScrollPane clusterScrollPane;
    private javax.swing.JButton disableButton;
    private javax.swing.JButton enableButton;
    private javax.swing.JButton killButton;
    private javax.swing.JButton powerOffButton;
    private javax.swing.JButton rebootButton;
    private javax.swing.JButton removeButton;
    private javax.swing.JButton resetButton;
    private javax.swing.JTable resourceTable;
    private javax.swing.JToolBar resourceToolBar;
    // End of variables declaration//GEN-END:variables
}
