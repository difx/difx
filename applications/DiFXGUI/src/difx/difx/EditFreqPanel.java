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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;

public class EditFreqPanel extends DiFXPanel implements TableModelListener {

	private static String [] freqtablecolumnnames = {"Band edge frequency (MHz)", "Bandwidth (MHz)", "Lower sideband"};

	private JButton addnewfreqbutton, removeactivefreqbutton;
	private JTable freqtable;
	private JScrollPane tablescrollpane;
	private Object [][] tabledata;
	private JPanel southpanel;
	private int numfreqs;
	
	public EditFreqPanel(DiFXgui parent, CorrelationConfig corrconfig) {
		super(parent, corrconfig);
		// TODO Auto-generated constructor stub
		addnewfreqbutton = new JButton("Add another frequency subband");
		addnewfreqbutton.addActionListener(this);
		removeactivefreqbutton = new JButton("Remove selected frequency");
		removeactivefreqbutton.addActionListener(this);
		numfreqs = 0;
		tabledata = new Object[0][3];
		freqtable = new JTable(new FreqTableModel(tabledata, freqtablecolumnnames));
		tablescrollpane = new JScrollPane(freqtable);
		
		southpanel = new JPanel(new FlowLayout());
		southpanel.add(addnewfreqbutton);
		southpanel.add(removeactivefreqbutton);
		
		mainpanel.setLayout(new BorderLayout());
		mainpanel.add(tablescrollpane);
		mainpanel.add(southpanel, "South");
	}

	@Override
	public void commitcorrchanges() {
		// TODO Auto-generated method stub
		corrconfig.setFreqTableLength(numfreqs);
		for(int i=0;i<numfreqs;i++) {
			corrconfig.setBandEdgeFreq(i, ((Double)tabledata[i][0]).doubleValue());
			corrconfig.setBandwidth(i, ((Double)tabledata[i][1]).doubleValue());
			corrconfig.setLowerSideband(i, ((Boolean)tabledata[i][2]).booleanValue());
		}
	}

	@Override
	public void refreshdisplay() {
		// TODO Auto-generated method stub
		numfreqs = corrconfig.getFreqTableLength();
		tabledata = new Object[numfreqs][3];
		for(int i=0;i<numfreqs;i++) {
			System.out.println("About to refresh frequency entry " + i);
			tabledata[i][0] = new Double(corrconfig.getBandEdgeFreq(i));
			tabledata[i][1] = new Double(corrconfig.getBandwidth(i));
			tabledata[i][2] = new Boolean(corrconfig.isLowerSideband(i));
		}
		freqtable.setModel(new FreqTableModel(tabledata, freqtablecolumnnames));
		freqtable.getModel().addTableModelListener(this);
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		
		String error;
		Object source = e.getSource();
		
		if(source == addnewfreqbutton) {
			corrconfig.addNewFreq();
			numfreqs++;
			refreshdisplay();
		}
		else if(source == removeactivefreqbutton) {
			error = corrconfig.removeFrequency(freqtable.getSelectedRow());
			if(error != null)
				JOptionPane.showMessageDialog(this, "Some Datastreams referred to the deleted frequency - they must now be edited!!!\n" + error,
						                      "Warning!!!", JOptionPane.WARNING_MESSAGE);
			refreshdisplay();
		}
	}
	
	public void tableChanged(TableModelEvent e) {
		if(e.getSource() == freqtable)
			updated = false;
	}

	private class FreqTableModel extends AbstractTableModel {

		int numrows, numcols;
		Object [][] tabledata;
		String [] columnnames;
		public FreqTableModel(Object [][] tabledata, String [] columnnames) {
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
			if(col == 2) 
				return (new Boolean(true)).getClass();
			else
				return (new String()).getClass();
		}
		
		public boolean isCellEditable(int row, int col) {
			return true;
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
