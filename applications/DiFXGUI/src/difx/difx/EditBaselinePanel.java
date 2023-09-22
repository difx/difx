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
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;

public class EditBaselinePanel extends DiFXPanel {

	private static final String[] tablecolumnnames = {"Telescope", "Band summary"};
	
	private JButton makeallbutton, removeallbutton, addbutton;
	private JTable datastream1table, datastream2table;
	private JScrollPane s1, s2;
	private JComboBox productbox;
	private JCheckBox [][][] productselections;
	private JPanel [][] tablecells;
	private JButton [] removebuttons;
	private JPanel toppanel, tablepanel, temppanel;
	private boolean refreshing;
	
	public EditBaselinePanel(DiFXgui parent, CorrelationConfig corrconfig) {
		super(parent, corrconfig);
		
		Dimension size;
		refreshing = false;

		makeallbutton = new JButton("Make all baseline pairs");
		makeallbutton.addActionListener(this);
		removeallbutton = new JButton("Remove all baselines");
		removeallbutton.addActionListener(this);
		datastream1table = new JTable(new UneditableTableModel());
		datastream2table = new JTable(new UneditableTableModel());
		productselections = new JCheckBox[0][0][4];
		tablecells = new JPanel[2][0];
		removebuttons = new JButton[0];
		s1 = new JScrollPane(datastream1table);
		size = s1.getViewport().getPreferredSize();
        size.height = datastream1table.getRowCount()*datastream1table.getRowHeight();
        datastream1table.setPreferredScrollableViewportSize(size);
        s2 = new JScrollPane(datastream2table);
		size = s2.getViewport().getPreferredSize();
        size.height = datastream2table.getRowCount()*datastream2table.getRowHeight();
        datastream2table.setPreferredScrollableViewportSize(size);
		productbox = new JComboBox(new String [] {"Parallel products only", 
				                                  "Include cross-pol products where possible"});
        addbutton = new JButton("Add selected telescope pair");
        addbutton.addActionListener(this);
        
		toppanel = new JPanel(new BorderLayout());
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(makeallbutton);
		temppanel.add(removeallbutton);
		toppanel.add(temppanel, "North");
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(productbox);
		temppanel.add(addbutton);
		toppanel.add(temppanel, "South");
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(datastream1table);
		temppanel.add(datastream2table);
		toppanel.add(temppanel);
		
		tablepanel = new JPanel(new GridLayout(1,1));
		tablepanel.add(new JLabel("Baseline Name"));
		
		mainpanel.setLayout(new BorderLayout());
		mainpanel.add(tablepanel, "South");
		mainpanel.add(toppanel);
	}

	@Override
	public void commitcorrchanges() {
		boolean [][] selections = new boolean[corrconfig.getFreqTableLength()][4];
		corrconfig.createNewBaselineTable(removebuttons.length);
		for(int i=0;i<removebuttons.length;i++) {
			for(int j=0;j<corrconfig.getFreqTableLength();j++) {
				for(int k=0;k<4;k++)
					selections[j][k] = productselections[i][j][k].isSelected();
			}
			corrconfig.setBaseline(i, Integer.parseInt(((JLabel)tablecells[i+2][1].getComponent(0)).getText()),
					               Integer.parseInt(((JLabel)tablecells[i+2][2].getComponent(0)).getText()),
					               selections);
		}
	}

	@Override
	public void refreshdisplay() {
		int row1, row2, blength, dlength, flength;
		Dimension size;
		refreshing = true;
		String [][] t1data = new String[corrconfig.getDatastreamTableLength()][2];
		String [][] t2data = new String[corrconfig.getDatastreamTableLength()][2];
		
		blength = corrconfig.getBaselineTableLength();
		dlength = corrconfig.getDatastreamTableLength();
		flength = corrconfig.getFreqTableLength();
		
		//fill in the datastream tables
		for(int i=0;i<dlength;i++) {
			t1data[i][0] = corrconfig.getDatastreamName(i);
			t1data[i][1] = corrconfig.getDatastreamBandSummary(i);
			t2data[i][0] = t1data[i][0];
			t2data[i][1] = t1data[i][1];
		}
		row1 = datastream1table.getSelectedRow();
		row2 = datastream1table.getSelectedRow();
		if(row1 < 0) row1 = 0;
		if(row2 < 0) row2 = 0;
		datastream1table.setModel(new UneditableTableModel(t1data, tablecolumnnames));
		datastream2table.setModel(new UneditableTableModel(t2data, tablecolumnnames));
		datastream1table.addRowSelectionInterval(row1, row1);
		datastream2table.addRowSelectionInterval(row2, row2);
		size = s1.getViewport().getPreferredSize();
    size.height = datastream1table.getRowCount()*datastream1table.getRowHeight();
    size.width = this.getWidth()/3;
    datastream1table.setPreferredScrollableViewportSize(size);
    datastream2table.setPreferredScrollableViewportSize(size);
    
    //fill in the baseline table
    tablepanel.removeAll();
    tablepanel.setLayout(new GridLayout(blength+2, flength+4));
    tablecells = new JPanel[blength+2][flength+4];
    removebuttons = new JButton[blength];
    productselections = new JCheckBox[blength][flength][4];
    addHeader();
    for(int i=2;i<blength+2;i++) {
    	tablecells[i][0] = new JPanel(new FlowLayout(FlowLayout.CENTER));
    	tablecells[i][1] = new JPanel(new FlowLayout(FlowLayout.CENTER));
    	tablecells[i][2] = new JPanel(new FlowLayout(FlowLayout.CENTER));
    	for(int j=0;j<flength;j++) {
    		tablecells[i][j+3] = new JPanel(new GridLayout(1,4));
    		for(int k=0;k<4;k++) {
    			productselections[i-2][j][k] = new JCheckBox();
    			productselections[i-2][j][k].addActionListener(this);
       	}
     	}
    	tablecells[i][flength+3] = new JPanel(new FlowLayout(FlowLayout.CENTER));
    }
    for(int i=0;i<blength;i++) {
    	tablecells[i+2][0].add(new JLabel(corrconfig.getBaselineName(i)));
    	tablecells[i+2][1].add(new JLabel(corrconfig.getBaselineDS1index(i)+""));
    	tablecells[i+2][2].add(new JLabel(corrconfig.getBaselineDS2index(i)+""));
    	for(int j=0;j<flength;j++) {
    		for(int k=0;k<4;k++) {
    			tablecells[i+2][j+3].add(productselections[i][j][k]);
    			if(corrconfig.productselectionpossible(i,j,k) && corrconfig.productselected(i,j,k))
    				productselections[i][j][k].setSelected(true);
    			else {
    				productselections[i][j][k].setSelected(false);
    				if(corrconfig.productselectionpossible(i,j,k))
    					productselections[i][j][k].setEnabled(true);
    				else
    					productselections[i][j][k].setEnabled(false);
        	}
    		}
    	}
    	removebuttons[i] = new JButton("Remove");
    	removebuttons[i].addActionListener(this);
    	tablecells[i+2][flength+3].add(removebuttons[i]);
    }
    for(int i=2;i<blength+2;i++) {
    	for(int j=0;j<flength+4;j++)
    		tablepanel.add(tablecells[i][j]);
    }
    refreshing = false;
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		if(refreshing)
			return; //don't deal with actionevents while refreshing
		Object source = e.getSource();
		
		for(int i=0;i<productselections.length;i++) {
			for(int j=0;j<productselections[i].length;j++) {
				for(int k=0;k<productselections[i][j].length;k++) {
					if(source == productselections[i][j][k])
						updated = false;
				}
			}
		}
		if(source == removeallbutton) {
			tablepanel.removeAll();
			tablepanel.setLayout(new GridLayout(2,corrconfig.getFreqTableLength()+4));
			productselections = new JCheckBox[0][0][4];
			tablecells = new JPanel[2][corrconfig.getFreqTableLength()+4];
			removebuttons = new JButton[0];
			addHeader();
			updated = false;
		}
		for(int i=0;i<tablecells.length-2;i++) {
			if(source == removebuttons[i]) {
				tablepanel.removeAll();
				int newtablelength = tablecells.length - 1;
				int tablewidth = tablecells[0].length;
				tablepanel.setLayout(new GridLayout(newtablelength, tablewidth));
				JPanel [][] oldcells = tablecells;
				JCheckBox [][][] oldselections = productselections;
				JButton [] oldremovebuttons = removebuttons;
				tablecells = new JPanel[newtablelength][tablewidth];
				productselections = new JCheckBox[oldselections.length-1][oldselections[0].length][4];
				removebuttons = new JButton[oldremovebuttons.length-1];
				addHeader();
				for(int j=0;j<oldremovebuttons.length;j++) {
					if(j<i) {
						copybaselineentry(j,j,oldcells,oldselections,oldremovebuttons);
					}
					else if(j>i) {
						copybaselineentry(j,j-1,oldcells,oldselections,oldremovebuttons);
					}
				}
				updated = false;
			}
		}
		if(source == addbutton) {
			int ds1index = datastream1table.getSelectedRow();
			int ds2index = datastream2table.getSelectedRow();
			if(ds1index > ds2index) {
				int temp = ds2index;
				ds2index = ds1index;
				ds1index = temp;
			}
			int currentlength = removebuttons.length;
			boolean crosspols = !((String)productbox.getSelectedItem()).startsWith("Parallel");
			addbaselinetablerow(1);
			addtotable(ds1index, ds2index, currentlength, crosspols);
			updated = false;
		}
		if(source == makeallbutton) {
			int count = 0;
			int currentlength = removebuttons.length;
			int numdatastreams = corrconfig.getDatastreamTableLength();
			int numnewbaselines = (numdatastreams*(numdatastreams-1))/2;
			boolean crosspols = !((String)productbox.getSelectedItem()).startsWith("Parallel");
			addbaselinetablerow(numnewbaselines);
			for(int i=0;i<numdatastreams-1;i++) {
				for(int j=i+1;j<numdatastreams;j++) {
					addtotable(i,j,currentlength + count++, crosspols);
				}
			}
			updated = false;
		}
		revalidate();
		repaint();
	}
	
	private void addHeader() {
		int flength = corrconfig.getFreqTableLength();
		
		for(int i=0;i<flength+4;i++)
        	tablecells[0][i] = new JPanel(new FlowLayout(FlowLayout.CENTER));
		tablecells[1][0] = new JPanel(new FlowLayout(FlowLayout.CENTER));
    	tablecells[1][1] = new JPanel(new FlowLayout(FlowLayout.CENTER));
    	tablecells[1][2] = new JPanel(new FlowLayout(FlowLayout.CENTER));
        for(int i=0;i<flength;i++)
        	tablecells[1][i+3] = new JPanel(new GridLayout(1,4));
        tablecells[1][flength+3] = new JPanel(new FlowLayout(FlowLayout.CENTER));
        		
        tablecells[0][0].add(new JLabel("Baseline Name"));
        tablecells[0][1].add(new JLabel("D/stream Index 1"));
        tablecells[0][2].add(new JLabel("D/stream Index 2"));
        for(int i=0;i<flength;i++) {
        	tablecells[0][i+3].add(new JLabel(corrconfig.getFreqList()[i]));
        	for(int j=0;j<4;j++)
        		tablecells[1][i+3].add(new JLabel(corrconfig.getPolPairs()[j]));
        }
        
        for(int i=0;i<2;i++) {
        	for(int j=0;j<flength+4;j++)
        		tablepanel.add(tablecells[i][j]);
        }
	}
	
	private void addtotable(int ds1, int ds2, int tableindex, boolean crosspols) {
		System.out.println("About to do table index " + tableindex);
		tablecells[tableindex+2][0].add(new JLabel(corrconfig.getDatastreamName(ds1) + "-" + corrconfig.getDatastreamName(ds2)));
		tablecells[tableindex+2][1].add(new JLabel(ds1 + ""));
		tablecells[tableindex+2][2].add(new JLabel(ds2 + ""));
		for(int i=0;i<corrconfig.getFreqTableLength();i++) {
			for(int j=0;j<4;j++) {
				productselections[tableindex][i][j].setSelected(corrconfig.productselectionpossible(ds1, ds2, i, j));
				if(!crosspols && j > 1)
					productselections[tableindex][i][j].setSelected(false);
				tablecells[tableindex+2][i+3].add(productselections[tableindex][i][j]);
			}
		}
		tablecells[tableindex+2][tablecells[tableindex+2].length-1].add(removebuttons[tableindex]);
	}
	
	private void addbaselinetablerow(int toadd) {
		int oldnumentries = removebuttons.length;
		JPanel [][] oldcells = tablecells;
		JCheckBox [][][] oldselections = productselections;
		JButton [] oldbuttons = removebuttons;
		tablepanel.removeAll();
		tablecells = new JPanel[oldcells.length+toadd][oldcells[0].length];
		productselections = new JCheckBox[oldselections.length + toadd][corrconfig.getFreqTableLength()][4];
		removebuttons = new JButton[oldbuttons.length + toadd];
		tablepanel.setLayout(new GridLayout(oldcells.length+toadd, oldcells[0].length));
		addHeader();
		for(int i=0;i<oldbuttons.length;i++)
			copybaselineentry(i,i,oldcells,oldselections,oldbuttons);
		for(int i=0;i<toadd;i++) {
			System.out.println("Creating table line " + (oldnumentries + i + 2));
			tablecells[oldnumentries+i+2][0] = new JPanel(new FlowLayout(FlowLayout.CENTER));
	    	tablecells[oldnumentries+i+2][1] = new JPanel(new FlowLayout(FlowLayout.CENTER));
	    	tablecells[oldnumentries+i+2][2] = new JPanel(new FlowLayout(FlowLayout.CENTER));
			for(int j=3;j<tablecells[oldnumentries+i+2].length-1;j++)
				tablecells[oldnumentries+i+2][j] = new JPanel(new GridLayout(1,4));
			tablecells[oldnumentries+i+2][tablecells[oldnumentries+i+2].length - 1] = 
				new JPanel(new FlowLayout(FlowLayout.CENTER));
			for(int j=0;j<tablecells[oldnumentries+i+2].length;j++)
				tablepanel.add(tablecells[oldnumentries+i+2][j]);
			for(int j=0;j<corrconfig.getFreqTableLength();j++) {
				for(int k=0;k<4;k++)
					productselections[oldnumentries + i][j][k] = new JCheckBox();
			}
			removebuttons[oldnumentries+i] = new JButton("Remove");
			removebuttons[oldnumentries+i].addActionListener(this);
		}
	}
	
	private void copybaselineentry(int from, int to, JPanel [][] oldcells, JCheckBox [][][] oldselections,
			JButton [] oldbuttons) {
		for(int i=0;i<oldcells[from].length;i++) {
			tablecells[to+2][i] = oldcells[from+2][i];
			tablepanel.add(tablecells[to+2][i]);
		}
		for(int i=0;i<oldselections[from].length;i++) {
			for(int j=0;j<oldselections[from][i].length;j++) {
				productselections[to][i][j] = oldselections[from][i][j];
			}
		}
		removebuttons[to] = oldbuttons[from];
	}
	
	private class UneditableTableModel extends DefaultTableModel {
		public UneditableTableModel() { super(); }
		public UneditableTableModel(Object [][] data, String [] colnames) { super(data, colnames); }
		public boolean isCellEditable(int row, int col) {
			return false;
		}
	}
}
