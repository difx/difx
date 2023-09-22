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
import java.awt.Component;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableModel;

public class EditDatastreamPanel extends DiFXPanel implements TableModelListener, ListSelectionListener {

	private static final String [] telescopetableheadings = {"Name", "Clock delay (us)", "Clock rate (us/s)"};
	private CalcStationFile csf;
	private JTable telescopetable;
	private JScrollPane tscrollpane;
	private JButton addcalctelescopebutton, addnewtelescopebutton, removeactivetelescopebutton,
	                addnewdatastreambutton;
	private SingleDatastreamPanel [] datastreampanels;
	private JPanel dsdisplaypanel, toppanel, buttonpanel, temppanel;
	private String [][] telescopetabledata;
	private int numdisplayeddatastreams;
	private boolean refreshing = false;
	
	public EditDatastreamPanel(DiFXgui parent, CorrelationConfig corrconfig) {
		super(parent, corrconfig);
		
		numdisplayeddatastreams = 0;
		mainpanel.setLayout(new BorderLayout());
		
		addcalctelescopebutton = new JButton("Add a telescope from the CALC database");
		addcalctelescopebutton.addActionListener(this);
		addnewtelescopebutton = new JButton("Add a new blank telescope");
		addnewtelescopebutton.addActionListener(this);
		removeactivetelescopebutton = new JButton("Remove the selected telescope");
		removeactivetelescopebutton.addActionListener(this);
		addnewdatastreambutton = new JButton("Add a new datastream for this telescope");
		addnewdatastreambutton.addActionListener(this);
	
		telescopetabledata = new String[0][3];
		telescopetable = new JTable(new DefaultTableModel(telescopetabledata, telescopetableheadings));
		telescopetable.getModel().addTableModelListener(this);
		telescopetable.getSelectionModel().addListSelectionListener(this);
		
		toppanel = new JPanel(new BorderLayout());
		buttonpanel = new JPanel(onecollayout);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(addcalctelescopebutton);
		buttonpanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(addnewtelescopebutton);
		buttonpanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(removeactivetelescopebutton);
		buttonpanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(addnewdatastreambutton);
		
		toppanel.add(buttonpanel, "North");
		tscrollpane = new JScrollPane(telescopetable);
		Dimension size = tscrollpane.getViewport().getPreferredSize();
        size.height = telescopetable.getRowCount()*telescopetable.getRowHeight();
        telescopetable.setPreferredScrollableViewportSize(size);
		toppanel.add(tscrollpane);
		toppanel.add(temppanel, "South");
		
		dsdisplaypanel = new JPanel();
		numdisplayeddatastreams = 0;
		datastreampanels = new SingleDatastreamPanel[0];
		
		mainpanel.add(toppanel, "North");
		mainpanel.add(dsdisplaypanel);
	}
	
	public void setNotUpdated() {
		updated = false;
	}
	
	public void copyDataPanel(SingleDatastreamPanel target) {
		//popup and allow user to select what to copy to - warn it will happen immediately
		String allselectedstring = "All datastreams from the currently selected telescopes";
		String allstring = "All datastreams";
		String [] options = new String[corrconfig.getDatastreamTableLength() + 2];
		boolean [] copyto = new boolean[corrconfig.getDatastreamTableLength()];
		int [] selectedrows;
		for(int i=0;i<corrconfig.getDatastreamTableLength();i++)
			options[i] = corrconfig.getDatastreamSummary().split("\n\n")[i];
		options[corrconfig.getDatastreamTableLength()] = allselectedstring;
		options[corrconfig.getDatastreamTableLength()+1] = allstring;
		String selection = (String) JOptionPane.showInputDialog(this, 
				"Which datastreams would you like to copy to (this operation cannot be undone)? ", "Select destinations", 
				JOptionPane.QUESTION_MESSAGE, null, options, options[corrconfig.getDatastreamTableLength()+1]);
		if(selection == null)
			return; // user cancelled
		if(selection.equals(options[corrconfig.getDatastreamTableLength()]))
			selectedrows = telescopetable.getSelectedRows();
		else
			selectedrows = new int[0];
		
		//tell corrconfig to copy to each of the targets
		for(int i=0;i<copyto.length;i++) {
			boolean copythis = selection.equals(options[i]) || 
			                   selection.equals(options[corrconfig.getDatastreamTableLength() + 1]);
			for(int j=0;j<selectedrows.length;j++) {
				if(corrconfig.getTelescopeIndex(i) == selectedrows[i])
					copythis = true;
			}
			if(copythis)
				corrconfig.copyDatastreamFreqSetup(target.getDatastreamIndex(), i);
		}
	}
	
	public void removeDataPanel(SingleDatastreamPanel target) {
		//popup asking user to confirm as operation cannot be undone
		int confirm = JOptionPane.showConfirmDialog(this, "This operation cannot be undone - are you sure you want to proceed? ", 
				"Warning!!!", JOptionPane.YES_NO_OPTION);
		
		if(confirm == JOptionPane.YES_OPTION) {
			//save present screen to corrconfig
			commitcorrchanges();
			
			//remove the selected one, and get corrconfig to refactor the datastream table
			corrconfig.removeDatastream(target.getDatastreamIndex());
		}
		refreshdisplay();
	}

	@Override
	public void commitcorrchanges() {
		//set the corrconfig telescope table
		String [][] telescopetabledata = new String[telescopetable.getModel().getRowCount()][3];
		for(int i=0;i<telescopetabledata.length;i++) {
			for(int j=0;j<3;j++) {
				telescopetabledata[i][j] = (String)telescopetable.getModel().getValueAt(i,j);
				System.out.println("read table data of " + telescopetabledata[i][j]);
			}
		}
		try {
			corrconfig.setTelescopeTableData(telescopetabledata);
		}
		catch(NumberFormatException e) {
			JOptionPane.showMessageDialog(this, "Problem setting telescope data: " + e.getMessage(),
					"Error!!!", JOptionPane.ERROR_MESSAGE);
		}
		
		//set the corrconfig data for the displayed datastream(s)
		for(int i=0;i<numdisplayeddatastreams;i++) {
			datastreampanels[i].commitcorrchanges();
			System.out.println("Main: finished datastream " + i);
		}
		
		updated = true;
	}

	@Override
	public void refreshdisplay() {
		int choice, selectedtelescope, telescopetablelength;
		String [][] telescopetabledata;
		
		if(!updated) {
			choice = JOptionPane.showConfirmDialog(this, "Save changes before changing displayed information?",
					  "Save changes?", JOptionPane.YES_NO_CANCEL_OPTION);
			if(choice == JOptionPane.CANCEL_OPTION) {
				return;
			}
			else {
				if(choice == JOptionPane.YES_OPTION)
					commitcorrchanges();
			}
		}
		
		refreshing = true;
		selectedtelescope = telescopetable.getSelectedRow();
		if(selectedtelescope < 0) selectedtelescope = 0;
		
		//get the telescope table information from the corrconfig and display it
		System.out.println("About to do the telescopetable");
		telescopetablelength = corrconfig.getTelescopeTableLength();
		telescopetabledata = new String[telescopetablelength][3];
		for(int i=0;i<telescopetablelength;i++) {
			telescopetabledata[i][0] = corrconfig.getTelescopeName(i);
			telescopetabledata[i][1] = corrconfig.getTelescopeClockDelay(i) + "";
			telescopetabledata[i][2] = corrconfig.getTelescopeClockRate(i) + "";
		}
		telescopetable.setModel(new DefaultTableModel(telescopetabledata, telescopetableheadings));
		Dimension size = tscrollpane.getViewport().getPreferredSize();
        size.height = telescopetable.getRowCount()*telescopetable.getRowHeight();
        telescopetable.setPreferredScrollableViewportSize(size);
		dsdisplaypanel.removeAll();
		
		System.out.println("Telescopetabledata.length is " + telescopetabledata.length);
		if(telescopetabledata.length > 0) {
			telescopetable.setRowSelectionInterval(selectedtelescope, selectedtelescope);
			
			//get the information for the datastreams belonging to the selected telescope and display it
			int [] telescopedsindices = corrconfig.getTelescopeDatastreamIndices(selectedtelescope);
			numdisplayeddatastreams = telescopedsindices.length;
			datastreampanels = new SingleDatastreamPanel[numdisplayeddatastreams];
			dsdisplaypanel.setLayout(new GridLayout(1, numdisplayeddatastreams));
			System.out.println("About to do the " + numdisplayeddatastreams + " datastreams for this telescope");
			for(int i=0;i<numdisplayeddatastreams;i++) {
				datastreampanels[i] = new SingleDatastreamPanel(this, corrconfig, telescopedsindices[i]);
				dsdisplaypanel.add(datastreampanels[i]);
				datastreampanels[i].refreshdisplay();
			}
		}
		//Runtime.getRuntime().gc();
		telescopetable.getModel().addTableModelListener(this);
		
		revalidate();
		repaint();
		refreshing = false;
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		System.out.println("Updated is " + updated);
		super.actionPerformed(e);
		
		int option;
		String telescopename = null;
		String [] calcnames;
		Object source = e.getSource();
		
		if(source == addcalctelescopebutton) {
			csf = new CalcStationFile(this);
			calcnames = csf.getCalcStationNames();
			if(calcnames != null)
				telescopename = (String) JOptionPane.showInputDialog(this, "Select the CALC telescope", 
					"Please select", JOptionPane.QUESTION_MESSAGE, 
					null, calcnames, calcnames[0]);
			if(telescopename != null) {
				addNewTelescope(telescopename);
				updated = false;
			}
		}
		else if(source == addnewtelescopebutton) {
			telescopename = (String) JOptionPane.showInputDialog(this, "Enter the telescope name",
					"Enter telescope", JOptionPane.QUESTION_MESSAGE);
			if(telescopename != null) {
				addNewTelescope(telescopename);
				updated = false;
			}
		}
		else if(source == removeactivetelescopebutton) {
			option = JOptionPane.showConfirmDialog(this, "This will immediately remove the telescope " +
					"from memory and cannot be undone - are you sure you want to remove this telescope " +
					"from the experiment table? ", "Confirm immediate telescope deletion", 
					JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
			if(option == JOptionPane.YES_OPTION) {
				corrconfig.removeTelescope((String)telescopetable.getModel().getValueAt(
						telescopetable.getSelectedRow(), 0));
				refreshdisplay();
			}
		}
		else if (source == addnewdatastreambutton) {
			corrconfig.addNewDatastream(telescopetable.getSelectedRow());
			refreshdisplay();
		}
	}

	public void tableChanged(TableModelEvent arg0) {
		updated = false;
	}
	
	public void valueChanged(ListSelectionEvent arg0) {
		if(refreshing)
			return;
		refreshdisplay();
	}
	
	private void addNewTelescope(String telescopename) {
		int numtelescopes = telescopetable.getModel().getRowCount();
		String [][] newtabledata = new String[numtelescopes+1][3];
		for(int i=0;i<numtelescopes;i++) {
			for(int j=0;j<3;j++)
				newtabledata[i][j] = (String)telescopetable.getModel().getValueAt(i,j);
		}
		newtabledata[numtelescopes][0] = telescopename;
		newtabledata[numtelescopes][1] = "0.0";
		newtabledata[numtelescopes][2] = "0.0";
		telescopetable.setModel(new DefaultTableModel(newtabledata, telescopetableheadings));
		telescopetable.getModel().addTableModelListener(this);
		telescopetable.getSelectionModel().addListSelectionListener(this);
	}
	
	private class CalcStationFile {
		String [] calcstationnames;
		Component parent;
		
		public CalcStationFile(Component parent) {
			this.parent = parent;
			loadCalcStations();
		}
		
		public String [] getCalcStationNames() {
			return calcstationnames;
		}
		
		private void loadCalcStations() {
			String calcdb = System.getenv("CALCDB");
			Vector<String> names = new Vector<String>();
			StringTokenizer st;
			String line, result;
			BufferedReader input;
			try{
				input = new BufferedReader(new FileReader(calcdb + "/stations.tab"));
				line = input.readLine();
				while(line != null) {
					if(line.charAt(0) != '#') {
						result = null;
						st = new StringTokenizer(line, "$");
						while(st.hasMoreTokens()) {
							result = st.nextToken();
						}
						if(result != null)
							names.add(result);
					}
					line = input.readLine();
				}
				input.close();
				calcstationnames = new String [names.size()];
				for(int i=0;i<names.size();i++)
					calcstationnames[i] = names.elementAt(i);
			}
			catch(IOException e) {
				JOptionPane.showMessageDialog(parent, "Problem obtaining CALC station names" + 
						e.getMessage(), "Error!!!", JOptionPane.ERROR_MESSAGE);
			}
		}
	}
}
