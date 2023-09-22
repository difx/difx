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
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;

public class EditConfigPanel extends DiFXPanel implements ItemListener, TableModelListener {
	
	private static final String [] streamtablecolumnnames = {"Index", "Telescope name", "Bands", "Selected"};

	private JButton selectdefaultdsbutton, selectdefaultblbutton;
	private JLabel numactiveconfigslabel;
	private JComboBox sourcebox;
	private JCheckBox configusedbox, pulsaronbox, postffringebox, quaddelaybox, writeautobox;
	private JTextField inttimefield, numchannelsfield, pulsarconfigfilefield, blockspersendfield, guardblocksfield;
	private JTable datastreamtable, baselinetable;
	private JPanel tablepanel, fieldpanel, temppanel,  toptemppanel;
	private int lastindex;
	boolean fromlistchange;
	
	public EditConfigPanel(DiFXgui parent, CorrelationConfig corrconfig) {
		super(parent, corrconfig);
		fromlistchange = false;
		
		mainpanel.setLayout(new BorderLayout());
		fieldpanel = new JPanel(onecollayout);
		numactiveconfigslabel = new JLabel("Number of configs enabled: " + corrconfig.getNumActiveConfigs());
		sourcebox = new JComboBox(corrconfig.getSourceNames());
		sourcebox.insertItemAt("DEFAULT", 0);
		sourcebox.setSelectedIndex(0);
		sourcebox.addItemListener(this);
		configusedbox = new JCheckBox("Config active? ", true);
		configusedbox.addActionListener(this);
		inttimefield = new JTextField(10);
		inttimefield.addActionListener(this);
		numchannelsfield = new JTextField(10);
		numchannelsfield.addActionListener(this);
		blockspersendfield = new JTextField(10);
		blockspersendfield.addActionListener(this);
		guardblocksfield = new JTextField(10);
		guardblocksfield.addActionListener(this);
		postffringebox = new JCheckBox("Use post-F fringe rotation? ", false);
		postffringebox.addActionListener(this);
		quaddelaybox = new JCheckBox("Use quadratic delay interpolation? ", true);
		quaddelaybox.addActionListener(this);
		writeautobox = new JCheckBox("Write autocorrelations? ", true);
		writeautobox.addActionListener(this);
		pulsaronbox = new JCheckBox("Pulsar binning used? ", false);
		pulsaronbox.addActionListener(this);
		pulsarconfigfilefield = new JTextField(10);
		pulsarconfigfilefield.addActionListener(this);
		pulsarconfigfilefield.setEditable(false);
		fieldpanel.add(numactiveconfigslabel);
		addInputField(fieldpanel, sourcebox, "Available config sources: ");
		fieldpanel.add(configusedbox);
		addInputField(fieldpanel, inttimefield, "Integration time (seconds): ");
		addInputField(fieldpanel, numchannelsfield, "Number of channels per subband: ");
		addInputField(fieldpanel, blockspersendfield, "Number of FFT blocks per message: ");
		addInputField(fieldpanel, guardblocksfield, "Additional guard blocks per message (rec. 1): ");
		fieldpanel.add(postffringebox);
		fieldpanel.add(quaddelaybox);
		fieldpanel.add(writeautobox);
		fieldpanel.add(pulsaronbox);
		addInputField(fieldpanel, pulsarconfigfilefield, "Pulsar configuration file name: ");
		
		tablepanel = new JPanel(new BorderLayout());
		selectdefaultdsbutton = new JButton("Select defaults");
		selectdefaultdsbutton.addActionListener(this);
		toptemppanel = new JPanel(twocollayout);
		toptemppanel.add(new JLabel("Available Datastreams"));
		toptemppanel.add(selectdefaultdsbutton);
		temppanel = new JPanel(new BorderLayout());
		temppanel.add(toptemppanel, "North");
		datastreamtable = new JTable(new StreamTableModel(streamtablecolumnnames));
		temppanel.add(datastreamtable);
		tablepanel.add(temppanel, "North");
		selectdefaultblbutton = new JButton("Select defaults");
		selectdefaultblbutton.addActionListener(this);
		toptemppanel = new JPanel(twocollayout);
		toptemppanel.add(new JLabel("Available Baselines"));
		toptemppanel.add(selectdefaultblbutton);
		temppanel = new JPanel(new BorderLayout());
		temppanel.add(toptemppanel, "North");
		baselinetable = new JTable(new StreamTableModel(streamtablecolumnnames));
		temppanel.add(baselinetable);
		tablepanel.add(temppanel);
		
		mainpanel.add(fieldpanel, "North");
		mainpanel.add(new JScrollPane(tablepanel));
		refreshdisplay();
	}

	@Override
	public void commitcorrchanges() {
		int currentindex, count;
		int [] datastreamindices = new int[corrconfig.getNumDatastreams()];
		int [] baselineindices = new int[corrconfig.getNumBaselines()];
		System.out.println("Committing some corr changes");
		
		if(fromlistchange)
			currentindex = lastindex;
		else
			currentindex = sourcebox.getSelectedIndex();
		try {
			if(configusedbox.isSelected()) {
				corrconfig.setConfigStatus(currentindex, true);
				corrconfig.setIntTime(currentindex, Double.parseDouble(inttimefield.getText()));
				corrconfig.setNumChannels(currentindex, Integer.parseInt(numchannelsfield.getText()));
				corrconfig.setBlocksPerSend(currentindex, Integer.parseInt(blockspersendfield.getText()));
				corrconfig.setGuardBlocks(currentindex, Integer.parseInt(guardblocksfield.getText()));
				corrconfig.setPostFFringe(currentindex, postffringebox.isSelected());
				corrconfig.setQuadDelayInterp(currentindex, quaddelaybox.isSelected());
				corrconfig.setWriteAutocorrs(currentindex, writeautobox.isSelected());
				corrconfig.setPulsarBinOn(currentindex, pulsaronbox.isSelected());
				if(pulsaronbox.isSelected())
					corrconfig.setPulsarFilename(currentindex, pulsarconfigfilefield.getText());
				//get the datastreams and baselines
				count = 0;
				for(int i=0;i<corrconfig.getDatastreamTableLength();i++) {
					if(((Boolean)datastreamtable.getModel().getValueAt(i, 3)).booleanValue())
						datastreamindices[count++] = i;
				}
				if(count != corrconfig.getNumDatastreams())
					JOptionPane.showMessageDialog(this, "Warning - " + count + " datastreams selected - there should be "
							+ corrconfig.getNumDatastreams(), "Warning!!!", JOptionPane.WARNING_MESSAGE);
				corrconfig.setConfigDatastreamIndices(currentindex, datastreamindices);
				count = 0;
				for(int i=0;i<corrconfig.getBaselineTableLength();i++) {
					if(((Boolean)baselinetable.getModel().getValueAt(i, 3)).booleanValue())
						baselineindices[count++] = i;
				}
				if(count != corrconfig.getNumBaselines())
					JOptionPane.showMessageDialog(this, "Warning - " + count + " baselines selected - there should be "
							+ corrconfig.getNumBaselines(), "Warning!!!", JOptionPane.WARNING_MESSAGE);
				corrconfig.setConfigBaselineIndices(currentindex, baselineindices);
			}
			else
				corrconfig.setConfigStatus(currentindex, false);
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(this, "Problem parsing configuration settings: " + e.getMessage(),
					"Error!!!", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		updated = true;
		refreshdisplay();
	}

	@Override
	public void refreshdisplay() {
		String lastsource = (String)sourcebox.getSelectedItem();
		String sourcename  = "";
		int [] datastreamindices;
		int [] baselineindices;
		Object [][] datastreamtabledata = new Object[corrconfig.getDatastreamTableLength()][4];
		Object [][] baselinetabledata = new Object[corrconfig.getBaselineTableLength()][4];
		
		sourcebox.removeItemListener(this);
		numactiveconfigslabel.setText("Number of configs enabled: " + corrconfig.getNumActiveConfigs());
		sourcebox.removeAllItems();
		for(int i=0;i<corrconfig.getNumConfigs();i++) {
			sourcename = new String(corrconfig.getConfigSourceName(i));
			System.out.println("Doing source " + sourcename);
			if(corrconfig.configActive(i))
				sourcename += "(active)";
			sourcebox.insertItemAt(sourcename, i);
		}
		if(lastsource != null) {
			for(int i=0;i<corrconfig.getNumConfigs();i++) {
				if(sourcebox.getItemAt(i).equals(lastsource))
					sourcebox.setSelectedIndex(i);
			}
		}
		if(sourcebox.getSelectedIndex() < 0 && corrconfig.getNumConfigs() > 0)
			sourcebox.setSelectedIndex(0);
		lastindex = sourcebox.getSelectedIndex();
		if(lastindex < 0)
			configusedbox.setSelected(false);
		else
			configusedbox.setSelected(corrconfig.configActive(lastindex));
		
		datastreamindices = corrconfig.getConfigDatastreamIndices(lastindex);
		for(int i=0;i<corrconfig.getDatastreamTableLength();i++) {
			boolean selected = false;
				if(datastreamindices != null) {
				for(int j=0;j<datastreamindices.length;j++) {
					if(i==datastreamindices[j]) selected = true;
				}
			}
			datastreamtabledata[i][0] = "" + i;
			datastreamtabledata[i][1] = corrconfig.getDatastreamName(i);
			datastreamtabledata[i][2] = corrconfig.getDatastreamBandSummary(i);
			datastreamtabledata[i][3] = new Boolean(selected);
		}
		datastreamtable.setModel(new StreamTableModel(datastreamtabledata, streamtablecolumnnames));
		datastreamtable.getModel().addTableModelListener(this);
		System.out.println("Finished making the datastream table of length " + corrconfig.getDatastreamTableLength());
		
		baselineindices = corrconfig.getConfigBaselineIndices(lastindex);
		for(int i=0;i<corrconfig.getBaselineTableLength();i++) {
			boolean selected = false;
			if(baselineindices != null) {
				for(int j=0;j<baselineindices.length;j++) {
					if(i==baselineindices[j]) selected = true;
				}
			}
			baselinetabledata[i][0] = "" + i;
			baselinetabledata[i][1] = corrconfig.getBaselineName(i);
			baselinetabledata[i][2] = corrconfig.getBaselineBandSummary(i);
			baselinetabledata[i][3] = new Boolean(selected);
		}
		baselinetable.setModel(new StreamTableModel(baselinetabledata, streamtablecolumnnames));
		baselinetable.getModel().addTableModelListener(this);
		System.out.println("Finished setting table models!!!");
		if(configusedbox.isSelected()) {
			inttimefield.setText(corrconfig.getIntTime(lastindex) + "");
			numchannelsfield.setText(corrconfig.getNumChannels(lastindex) + "");
			pulsaronbox.setSelected(corrconfig.isPulsarBinning(lastindex));
			pulsarconfigfilefield.setText(corrconfig.getPulsarConfigFilename(lastindex));
			blockspersendfield.setText(corrconfig.getBlocksPerSend(lastindex) + "");
			guardblocksfield.setText(corrconfig.getGuardBlocks(lastindex) + "");
		}
		else {
			inttimefield.setText("");
			numchannelsfield.setText("");
			pulsaronbox.setSelected(false);
			pulsarconfigfilefield.setText("");
			blockspersendfield.setText("");
			guardblocksfield.setText("");
			//datastreamtable.setModel(new StreamTableModel(streamtablecolumnnames));
			//baselinetable.setModel(new StreamTableModel(streamtablecolumnnames));
		}
		sourcebox.addItemListener(this);
	}

	public void itemStateChanged(ItemEvent e) {
		int choice;
		
		Object source = e.getSource();
		fromlistchange = true;
		
		if(source == sourcebox && lastindex != sourcebox.getSelectedIndex()) {
			if(!updated) {
				choice = JOptionPane.showConfirmDialog(this, "Save changes to this configuration before changing?",
						  "Save changes?", JOptionPane.YES_NO_CANCEL_OPTION);
				if(choice == JOptionPane.CANCEL_OPTION) {
					sourcebox.setSelectedIndex(lastindex);
				}
				else {
					if(choice == JOptionPane.YES_OPTION)
						commitcorrchanges();
					refreshdisplay();
				}
			}
			else
				refreshdisplay();
		}
		
		fromlistchange = false;
	}

	public void tableChanged(TableModelEvent arg0) {
		// TODO Auto-generated method stub
		updated = false;
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
		super.actionPerformed(e);
		String sourcename;
		Object source = e.getSource();
		
		if(source == postffringebox) {
			updated = false;
			quaddelaybox.setEnabled(!postffringebox.isSelected());
			if(postffringebox.isSelected())
				quaddelaybox.setSelected(false);
		}
		else if(source == pulsaronbox) {
			pulsarconfigfilefield.setEditable(pulsaronbox.isSelected());
			updated = false;
		}
		else if (source == numchannelsfield) {
			updated = false;
		}
		else if(source == configusedbox) {
			updated = false;
			sourcebox.removeItemListener(this);
			sourcename = ((String)sourcebox.getItemAt(lastindex));
			if(configusedbox.isSelected()) {
				sourcename += " (active)";
			}
			else {
				sourcename = sourcename.substring(0, sourcename.length() -9);
			}
			sourcebox.removeItemAt(lastindex);
			sourcebox.insertItemAt(sourcename, lastindex);
			sourcebox.setSelectedIndex(lastindex);
			sourcebox.addItemListener(this);
		}
		else if (source == selectdefaultdsbutton) {
			int count = 0;
			boolean [] telescopeused = new boolean[corrconfig.getTelescopeTableLength()];
			for(int i=0;i<corrconfig.getTelescopeTableLength();i++)
				telescopeused[i] = false;
			for(int i=0;i<corrconfig.getDatastreamTableLength();i++) {
				if(!telescopeused[corrconfig.getTelescopeIndex(i)] && count < corrconfig.getNumDatastreams()) {
					telescopeused[corrconfig.getTelescopeIndex(i)]  = true;
					count++;
					datastreamtable.setValueAt(new Boolean(true), i, 3);
				}
				else
					datastreamtable.setValueAt(new Boolean(false), i, 3);
			}
		}
		else if (source == selectdefaultblbutton) {
			int ds1, ds2, count = 0;
			boolean alreadyin;
			for(int i=0;i<corrconfig.getBaselineTableLength();i++) {
				baselinetable.setValueAt(new Boolean(false), i, 3);
				ds1 = corrconfig.getBaselineDS1index(i);
				ds2 = corrconfig.getBaselineDS2index(i);
				if(((Boolean)datastreamtable.getValueAt(ds1, 3)).booleanValue() && 
						((Boolean)datastreamtable.getValueAt(ds2, 3)).booleanValue() && count < corrconfig.getNumBaselines()) {
					alreadyin = false;
					for(int j=0;j<i;j++) {
						if(ds1 == corrconfig.getBaselineDS1index(j) && ds2 == corrconfig.getBaselineDS2index(j))
							alreadyin = true;
						//ds1 = corrconfig.getBaselineDS1index(j);
						//ds2 = corrconfig.getBaselineDS2index(j);
						//if(((Boolean)datastreamtable.getValueAt(ds1, 3)).booleanValue() && 
						//		((Boolean)datastreamtable.getValueAt(ds2, 3)).booleanValue())
						//	alreadyin = true;
					}
					if(!alreadyin) {
						baselinetable.setValueAt(new Boolean(true), i, 3);
						count++;
					}
				}
			}
		}
		else if(source == quaddelaybox || source == writeautobox ||
				   source == blockspersendfield || source == guardblocksfield) {
					updated = false;
				}
	}
	
	private class StreamTableModel extends AbstractTableModel {

		int numrows, numcols;
		Object [][] tabledata;
		String [] columnnames;
		public StreamTableModel(Object [][] tabledata, String [] columnnames) {
			numrows = tabledata.length;
			numcols = columnnames.length;
			this.tabledata = tabledata;
			this.columnnames = columnnames;
		}
		public StreamTableModel(String [] columnnames) {
			this(new Object[0][3], columnnames);
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
			if(col == 3)
				return (new Boolean(false)).getClass();
			else
				return (new String()).getClass();
		}
		
		public boolean isCellEditable(int row, int col) {
			return (col==3 && configusedbox.isSelected());
		}
		
		public void setValueAt(Object toset, int row, int col) {
			tabledata[row][col] = toset;
			fireTableCellUpdated(row, col);
		}
		
		public String getColumnName(int col) {
			return columnnames[col];
		}
	}
}
