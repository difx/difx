/***************************************************************************
 *   Copyright (C) 2016 by NRAO                                            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
package difx;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.StringTokenizer;
import java.util.Vector;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.table.AbstractTableModel;

public class EditNodePanel extends DiFXPanel implements TableModelListener {
	
	private static final String [] selectednodecolumnnames = {"Type", "Node", "Number of threads"};
	private static final String [] availablenodecolumnnames = {"Use?", "Name", "Availability", "Numbers of processors"};
	
	private JPanel northpanel, centrepanel, southpanel, temppanel;
	private JButton savethreadfilebutton, savemachinefilebutton, usepbsbutton, refreshloadbutton;
	private JTextField threadfilenamefield, machinefilenamefield, numnodesfield, walltimefield, procspernodefield;
	private JComboBox queuebox;
	private JLabel freenodeslabel;
	private JScrollPane availablescrollpane, selectedscrollpane;
	private JTable availablenodetable, selectednodetable;
	private boolean [] nodesselected;
	private String [][] selectednodetabledata;
	private String [][] availablenodelist;
	private Object [][] availablenodetabledata;
	private JFileChooser chooser;
	private FileFilter threadfilter, machinefilter;
	private boolean usepbs;

	public EditNodePanel(DiFXgui parent, CorrelationConfig corrconfig) {
		super(parent, corrconfig);
		
		usepbs = corrconfig.usePbs();
		
		//create the top panel, which has all the buttons and options
		northpanel = new JPanel(onecollayout);
		
		queuebox = new JComboBox(getqueues());
		queuebox.setEnabled(false);
		usepbsbutton = new JButton("Use PBS job submission");
		usepbsbutton.addActionListener(this);
		addInputField(northpanel, queuebox, "Target queue: ", usepbsbutton);
		threadfilenamefield = new JTextField(30);
		threadfilenamefield.addActionListener(this);
		savethreadfilebutton = new JButton("Save thread configuration");
		savethreadfilebutton.addActionListener(this);
		addInputField(northpanel, threadfilenamefield, "Thread filename: ", savethreadfilebutton);
		machinefilenamefield = new JTextField(30);
		machinefilenamefield.addActionListener(this);
		savemachinefilebutton = new JButton("Save machine configuration");
		savemachinefilebutton.addActionListener(this);
		addInputField(northpanel, machinefilenamefield, "Machine filename: ", savemachinefilebutton);
		numnodesfield = new JTextField(5);
		numnodesfield.setText(corrconfig.getNumNodes() + "");
		numnodesfield.addActionListener(this);
		addInputField(northpanel, numnodesfield, "Number of nodes: ");
                procspernodefield = new JTextField(5);
                procspernodefield.setText(corrconfig.getProcsPerNode() + "");
                procspernodefield.addActionListener(this);
                addInputField(northpanel, procspernodefield, "Procs per node: ");
                walltimefield = new JTextField(5);
                walltimefield.setText(corrconfig.getWalltime() + "");
                walltimefield.addActionListener(this);
                addInputField(northpanel, walltimefield, "Walltime (hours): ");
		
		//create the centre panel, which has the interactive selection
		centrepanel = new JPanel(new BorderLayout());
		
		selectednodetable = new JTable();
		selectednodetabledata = new String[0][0];
		selectedscrollpane = new JScrollPane(selectednodetable);
		centrepanel.add(selectedscrollpane);
		nodesselected = new boolean[0];
		
		//create the bottom panel, which shows all available nodes with loads if possible and checkboxes
		southpanel = new JPanel(new BorderLayout());
		
		temppanel = new JPanel(onecollayout);
		freenodeslabel = new JLabel("??");
		refreshloadbutton = new JButton("Refresh node loads");
		refreshloadbutton.addActionListener(this);
		addInputField(temppanel, freenodeslabel, "Free nodes (current cluster): ", refreshloadbutton);
		southpanel.add(temppanel, "North");
		availablenodetable = new JTable();
		availablescrollpane = new JScrollPane(availablenodetable);
		southpanel.add(availablescrollpane);
		refreshLoads();
		
		//add the panels to the mainpanel
		mainpanel.setLayout(new BorderLayout());
		mainpanel.add(northpanel, "North");
		mainpanel.add(centrepanel);
		mainpanel.add(southpanel, "South");
		
		//create filters and other miscellanea
    threadfilter = new FileFilter() {public boolean accept(File f)  { return f.getName().endsWith(".threads") || f.isDirectory(); } 
                                     public String getDescription() { return "Thread setup files"; }};
    machinefilter = new FileFilter() {public boolean accept(File f)  { return f.getName().startsWith("machines.") || f.isDirectory(); } 
                                      public String getDescription() { return "Machine files"; }};
    chooser = new JFileChooser();
	}

	@Override
	public void commitcorrchanges() {
		corrconfig.setNodeSettings(usepbs, (String)queuebox.getSelectedItem(), machinefilenamefield.getText(), 
				                       threadfilenamefield.getText(), Integer.parseInt(numnodesfield.getText()));
                corrconfig.setProcsPerNode(Integer.parseInt(procspernodefield.getText()));
                corrconfig.setWalltime(Integer.parseInt(walltimefield.getText()));
		try {
			savethreadfile(threadfilenamefield.getText());
			if(!usepbs)
				savemachinefile(machinefilenamefield.getText());
			updated = true;
		}
		catch (IOException e) {
			JOptionPane.showMessageDialog(this, "Error trying to save file!!! " + e.getMessage(), "File I/O Error!!!", JOptionPane.ERROR_MESSAGE);
		}
	}

	@Override
	public void refreshdisplay() {
		if(usepbs) {
			queuebox.setEnabled(true);
			usepbsbutton.setText("Use manual job submission");
			savemachinefilebutton.setEnabled(false);
			machinefilenamefield.setEditable(false);
		}
		else {
			queuebox.setEnabled(false);
			usepbsbutton.setText("Use PBS job submission");
			savemachinefilebutton.setEnabled(true);
			machinefilenamefield.setEditable(true);
		}
		threadfilenamefield.setText(corrconfig.getThreadFilename());
		refreshLoads();
	}
	
	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		Object source = e.getSource();
		
		if(source == usepbsbutton) {
			usepbs = !usepbs;
			refreshdisplay(true);
			updated = false;
		}
		else if (source == savethreadfilebutton) {
			popupThreadChooser();
		}
		else if (source == savemachinefilebutton) {
			popupMachineChooser();
		}
		else if (source == numnodesfield || source == procspernodefield) {
			changeNumNodes();
			updated = false;
		}
		else if (source == refreshloadbutton) {
			refreshLoads();
		}
                else if (source == walltimefield) {
                        updated = false;
                }
	}
	
  public void tableChanged(TableModelEvent e) {
  	int count = 0;
  	if(e.getSource() == selectednodetable.getModel())
  		updated = false;
  	else if(e.getSource() == availablenodetable.getModel()) {
  		if(e.getColumn() == 0) {
  			updated = false;
  			nodesselected[e.getFirstRow()] = !nodesselected[e.getFirstRow()];
  			if(nodesselected[e.getFirstRow()]) {
  				corrconfig.setNumNodes(corrconfig.getNumNodes() + 1);
  			}
  			else {
  				corrconfig.setNumNodes(corrconfig.getNumNodes() - 1);
  			}
  			numnodesfield.setText(corrconfig.getNumNodes() + "");
                        procspernodefield.setText(corrconfig.getProcsPerNode() + "");
                        int numprocs = corrconfig.getNumNodes()*corrconfig.getProcsPerNode();
  			String [] newnumthreads = new String[numprocs + 1];
  			for(int i=corrconfig.getNumDatastreams();i<selectednodetabledata.length;i++)
  				newnumthreads[i] = selectednodetabledata[i][2];
  			for(int i=0;i<availablenodelist.length;i++) {
  				if(nodesselected[i]) {
  					if(count <= corrconfig.getNumDatastreams()) {
  						newnumthreads[count++] = "";
  					}
  					else {
  						if (i > e.getFirstRow()) {
	  						if(nodesselected[e.getFirstRow()]) //this one was added
	  							newnumthreads[count] = selectednodetabledata[count-1][2];
	  						else
	  							newnumthreads[count] = selectednodetabledata[count+1][2];
	  						count++;
	  					}
	  					else if (i == e.getFirstRow()) {
	  						if(nodesselected[e.getFirstRow()]) { //this one was added
	  								newnumthreads[count] = "1";
	  							count++;
	  						}
	  					}
	  					else
	  						count++;
  					}
  				}
  			}
  			updateSelectedNodes(newnumthreads);
  		}
  	}
  }
  
  private void refreshdisplay(boolean reset) {
  	if(reset) {
  		for(int i=0;i<nodesselected.length;i++)
  			nodesselected[i] = false;
  		int currentnodes = Integer.parseInt(numnodesfield.getText());
  		numnodesfield.setText("0"); //clear the table
  		changeNumNodes();
  		numnodesfield.setText(""+currentnodes);
  		changeNumNodes();
  	}
  	
  	refreshdisplay();
  }
	
	private String [] getqueues() {
		String queuestring;
		String [] toreturn = null;
		StringTokenizer st;
		BufferedReader stdoutreader;
		Process getqueuenames;
		Runtime rt = Runtime.getRuntime();
		int count = 0;
		
		try {
			getqueuenames = rt.exec(DiFXgui.getGetQueuesCommand());
			stdoutreader = new BufferedReader(new InputStreamReader(getqueuenames.getInputStream()));
			queuestring = stdoutreader.readLine();
			st = new StringTokenizer(queuestring);
			toreturn = new String [st.countTokens()];
			while(st.hasMoreTokens())
				toreturn[count++] = st.nextToken();
			getqueuenames.waitFor();
		}
		catch(IOException e) {
			JOptionPane.showMessageDialog(this, "IOError executing " + DiFXgui.getGetQueuesCommand() + 
					                          ": " + e.getMessage(), "Error!!!", JOptionPane.ERROR_MESSAGE);
		}
                catch(InterruptedException e) {
                        JOptionPane.showMessageDialog(this, "InterruptedError executing " + DiFXgui.getGetQueuesCommand() +
                                                                  ": " + e.getMessage(), "Error!!!", JOptionPane.ERROR_MESSAGE);
                }
		
		return toreturn;
	}
	
	private void refreshLoads() {
		int count = 0;
		int numnodesfree = 0;
		boolean [] oldnodesselected = nodesselected;
		int numoldnodes = nodesselected.length;
		System.out.println("Number of old nodes is: " + numoldnodes);
		availablenodelist = getAvailableNodeList();
		
		nodesselected = new boolean[availablenodelist.length];
		availablenodetabledata = new Object[availablenodelist.length][4];
		for(int i=0;i<availablenodelist.length;i++) {
			if(i<numoldnodes && count < corrconfig.getNumNodes()*corrconfig.getProcsPerNode())
				nodesselected[i] = oldnodesselected[i];
			else
				nodesselected[i] = false;
			if(nodesselected[i])
				count++;
				
			availablenodetabledata[i][0] = new Boolean(nodesselected[i]);
			availablenodetabledata[i][1] = availablenodelist[i][0];
			if(availablenodelist[i][1].equals("free"))
				numnodesfree++;
			availablenodetabledata[i][2] = availablenodelist[i][1];
  		availablenodetabledata[i][3] = availablenodelist[i][2];
		}
		availablenodetable.setModel(new AvailableNodeTableModel(availablenodetabledata, availablenodecolumnnames));
		availablenodetable.getModel().addTableModelListener(this);
		freenodeslabel.setText("" + numnodesfree);
	}
	
	private void updateSelectedNodes(String [] newnumthreads) {
		int count = 0;
		selectednodetabledata = new String[corrconfig.getNumNodes()*corrconfig.getProcsPerNode()][3];
		
		for(int i=0;i<availablenodetabledata.length;i++) {
			if(nodesselected[i]) {
				if(count == 0) {
					selectednodetabledata[count][0] = "Manager";
				}
				else if (count <= corrconfig.getNumDatastreams()) {
					selectednodetabledata[count][0] = "Datastream";
				}
				else {
					selectednodetabledata[count][0] = "Core";
				}
				selectednodetabledata[count][1] = (usepbs)?"Assigned by PBS":availablenodelist[i][0];
				selectednodetabledata[count][2] = newnumthreads[count];
				count++;
			}
		}
		selectednodetable.setModel(new SelectedNodeTableModel(selectednodetabledata, selectednodecolumnnames));
		selectednodetable.getModel().addTableModelListener(this);
		
		//fix up the available node table
		count = 0;
		for(int i=0;i<availablenodelist.length;i++) {
			if(nodesselected[i]) {
				if(count >= corrconfig.getNumNodes()*corrconfig.getProcsPerNode()) {
					nodesselected[i] = false;
					availablenodetabledata[i][0] = new Boolean(false);
				}
				count++;
			}
		}
		availablenodetable.setModel(new AvailableNodeTableModel(availablenodetabledata, availablenodecolumnnames));
		availablenodetable.getModel().addTableModelListener(this);
	}
	
	private void changeNumNodes() {
		String [][] oldtabledata = selectednodetabledata;
		corrconfig.setNumNodes(Integer.parseInt(numnodesfield.getText()));
                corrconfig.setProcsPerNode(Integer.parseInt(procspernodefield.getText()));
                int numprocs = corrconfig.getNumNodes()*corrconfig.getProcsPerNode();
		selectednodetabledata = new String[numprocs][3];
		int minrows = (oldtabledata.length < numprocs)?oldtabledata.length:numprocs;
		int extrarows = numprocs - oldtabledata.length;
		int count = 0;
		
		for(int i=0;i<minrows;i++) {
			selectednodetabledata[i][0] = oldtabledata[i][0];
			selectednodetabledata[i][1] = oldtabledata[i][1];
			selectednodetabledata[i][2] = oldtabledata[i][2];
		}
		for(int i=0;i<availablenodelist.length;i++) {
			if(count >= extrarows)
				break;
			if(!nodesselected[i]) {
				nodesselected[i] = true;
				selectednodetabledata[count+minrows][0] = ((count+minrows == 0)?"Manager":((count+minrows <= corrconfig.getNumDatastreams())?"Datastream":"Core"));
				selectednodetabledata[count+minrows][1] = (usepbs)?"Assigned by PBS":availablenodelist[i][0];
				selectednodetabledata[count+minrows][2] = ((count+minrows <= corrconfig.getNumDatastreams())?"":"1");
				count++;
			}
		}
			
		selectednodetable.setModel(new SelectedNodeTableModel(selectednodetabledata, selectednodecolumnnames));
		selectednodetable.getModel().addTableModelListener(this);
		refreshLoads();
	}
	
	private String [][] getAvailableNodeList() {
		String [][] toreturn = null;
		Vector<String> lines = new Vector<String>();
		String line, command = "";
		StringTokenizer st;
		Runtime rt;
		Process getnodelist;
		BufferedReader stdoutreader, stderrreader;
		int count = 0;
		
		rt = Runtime.getRuntime();
		try {
			if(usepbs)
				command = DiFXgui.getGetPBSNodesCommand();
			else
				command = DiFXgui.getGetNodesCommand();
			getnodelist = rt.exec(command);
			stdoutreader = new BufferedReader(new InputStreamReader(getnodelist.getInputStream()));
			stderrreader = new BufferedReader(new InputStreamReader(getnodelist.getErrorStream()));
			
			while ( (line = stdoutreader.readLine()) != null) {
        lines.add(line);
        count++;
			}
			if((line = stderrreader.readLine()) != null)
			{
			  JOptionPane.showMessageDialog(this, "Error executing " + command + ": " + line, "Error!!!", JOptionPane.ERROR_MESSAGE);
			  return new String[0][0]; //abort, couldn't load the file
			}
			getnodelist.waitFor();
			toreturn = new String[count][3];
			for(int i=0;i<count;i++) {
				st = new StringTokenizer(lines.elementAt(i));
				toreturn[i][0] = st.nextToken();
				toreturn[i][1] = st.nextToken();
				toreturn[i][2] = st.nextToken();
			}
		}
		catch(IOException e) {
			JOptionPane.showMessageDialog(this, "IO error executing " + command + ": " + e.getMessage(), "Error!!!", JOptionPane.ERROR_MESSAGE);
		}
                catch(InterruptedException e) {
                        JOptionPane.showMessageDialog(this, "Interrupted error executing " + command + ": " + e.getMessage(), "Error!!!", JOptionPane.ERROR_MESSAGE);
                }
		
		return toreturn;
	}
	
	private void savethreadfile(String threadfilename) throws IOException {
		PrintWriter output;
                int numcores = corrconfig.getNumNodes()*corrconfig.getProcsPerNode() - (1 + corrconfig.getNumDatastreams());
	
		output = new PrintWriter(new FileWriter(threadfilename));
		output.println("NUMBER OF CORES:    " + numcores);
		for(int i=1+corrconfig.getNumDatastreams();i<corrconfig.getNumNodes()*corrconfig.getProcsPerNode();i++) {
                        if(selectednodetabledata[i][2] == "")
                            output.println("2");
                        else
                        {
                            try { 
                                Integer.parseInt(selectednodetabledata[i][2]);
                                output.println(selectednodetabledata[i][2]+"");
                            }
                            catch (NumberFormatException e) {
                                output.println("2");
                            }
                        }
		}
		output.close();
	}
	
	private void savemachinefile(String machinefilename) throws IOException {
		PrintWriter output;
		
		output = new PrintWriter(new FileWriter(machinefilename));
		for(int i=0;i<corrconfig.getNumNodes();i++) {
			output.println((String)selectednodetabledata[i][1]);
		}
		output.close();
	}
  
	private void popupThreadChooser() {
		int returnval;
		
		chooser.addChoosableFileFilter(threadfilter);
		chooser.setFileFilter(threadfilter);

		// open a jfiledialog and pick the file
		returnval = chooser.showOpenDialog(this);
		if(returnval == JFileChooser.APPROVE_OPTION) {
			threadfilenamefield.setText(chooser.getSelectedFile().getAbsolutePath());
			try {
				savethreadfile(chooser.getSelectedFile().getAbsolutePath());
				corrconfig.setThreadFileName(chooser.getSelectedFile().getAbsolutePath());
				updated = false;
			}
			catch (IOException e) {
				JOptionPane.showMessageDialog(this, "Error trying to save file " + chooser.getSelectedFile().getAbsolutePath()
						                         + "!!! " + e.getMessage(), "File I/O Error!!!", JOptionPane.ERROR_MESSAGE);
			}
		}

		chooser.removeChoosableFileFilter(threadfilter);
	}
	
	private void popupMachineChooser() {
		int returnval;
		
		chooser.addChoosableFileFilter(machinefilter);
		chooser.setFileFilter(machinefilter);

		// open a jfiledialog and pick the file
		returnval = chooser.showOpenDialog(this);
		if(returnval == JFileChooser.APPROVE_OPTION) {
			machinefilenamefield.setText(chooser.getSelectedFile().getAbsolutePath());
			try {
				savemachinefile(chooser.getSelectedFile().getAbsolutePath());
				corrconfig.setMachineFileName(chooser.getSelectedFile().getAbsolutePath());
				updated = false;
			}
			catch (IOException e) {
				JOptionPane.showMessageDialog(this, "Error trying to save file " + chooser.getSelectedFile().getAbsolutePath()
						                         + "!!! " + e.getMessage(), "File I/O Error!!!", JOptionPane.ERROR_MESSAGE);
			}
		}

		chooser.removeChoosableFileFilter(machinefilter);
	}
	
	private class AvailableNodeTableModel extends AbstractTableModel {

		int numrows, numcols;
		Object [][] tabledata;
		String [] columnnames;
		public AvailableNodeTableModel(Object [][] tabledata, String [] columnnames) {
			numrows = tabledata.length;
			numcols = columnnames.length;
			this.tabledata = tabledata;
			this.columnnames = columnnames;
		}
		
		public int getColumnCount() {
			return numcols;
		}

		public int getRowCount() {
			return numrows;
		}

		public Object getValueAt(int row, int col) {
			return tabledata[row][col];
		}
		
		public Class getColumnClass(int col) {
			switch(col) {
				case 0:
					return (new Boolean(true)).getClass();
				default:
					return (new String()).getClass();
			}
		}
		
		public boolean isCellEditable(int row, int col) {
			return col == 0 && !usepbs;
		}
		
		public void setValueAt(Object toset, int row, int col) {
			tabledata[row][col] = toset;
			fireTableCellUpdated(row, col);
		}
		
		public String getColumnName(int col) {
			return columnnames[col];
		}
	}
	
	private class SelectedNodeTableModel extends AbstractTableModel {

		int numrows, numcols;
		String [][] tabledata;
		String [] columnnames;
		public SelectedNodeTableModel(String [][] tabledata, String [] columnnames) {
			numrows = tabledata.length;
			numcols = columnnames.length;
			this.tabledata = tabledata;
			this.columnnames = columnnames;
		}
		
		public int getColumnCount() {
			return numcols;
		}

		public int getRowCount() {
			return numrows;
		}

		public Object getValueAt(int row, int col) {
			return tabledata[row][col];
		}
		
		public Class getColumnClass(int col) {
			return (new String()).getClass();
		}
		
		public boolean isCellEditable(int row, int col) {
			return !usepbs || (col == 2);
		}
		
		public void setValueAt(Object toset, int row, int col) {
			tabledata[row][col] = (String)toset;
			fireTableCellUpdated(row, col);
		}
		
		public String getColumnName(int col) {
			return columnnames[col];
		}
	}
}
