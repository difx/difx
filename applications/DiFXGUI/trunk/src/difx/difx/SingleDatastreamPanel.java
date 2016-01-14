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
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.io.FileFilter;
import java.util.Arrays;
import java.util.StringTokenizer;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

public class SingleDatastreamPanel extends JPanel implements ActionListener, ItemListener, KeyListener {

	private EditDatastreamPanel parent;
	private CorrelationConfig corrconfig;
	private JButton removebutton, copybutton, adddatabutton;
	private JTextField tsysfield, numbandsfield, fanoutfield, portfield, tcpwindowsizefield;
	private JComboBox recordformatbox, datasourcebox;
	private JComboBox [] bandfreqs;
	private JTextField [] bandpolarisations;
	private JTextArea datalist;
	private JTextField [] freqoffsetfields;
	private JPanel tablepanel, toppanel, bottompanel, temppanel, diskpanel, networkpanel, sourcepanel;
	private JFileChooser datachooser;
	private int datastreamindex, numbands;
	private boolean refreshing;
	
	public SingleDatastreamPanel(EditDatastreamPanel parent, CorrelationConfig corrconfig, int datastreamindex) {
		this.parent = parent;
		this.corrconfig = corrconfig;
		this.datastreamindex = datastreamindex;
		numbands = 0;
		refreshing = false;
		
		datachooser = new JFileChooser();
		datachooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		
		removebutton = new JButton("Remove this datastream");
		removebutton.addActionListener(this);
		copybutton = new JButton("Copy datastream band setup to others");
		copybutton.addActionListener(this);
		adddatabutton = new JButton("Add data files from a directory");
		adddatabutton.addActionListener(this);
		tsysfield = new JTextField(10);
		tsysfield.addActionListener(this);
		numbandsfield = new JTextField(10);
		numbandsfield.addActionListener(this);
		fanoutfield = new JTextField(10);
		fanoutfield.addActionListener(this);
		fanoutfield.setEditable(false);
		portfield = new JTextField(10);
		portfield.addActionListener(this);
		tcpwindowsizefield = new JTextField(10);
		tcpwindowsizefield.addActionListener(this);
		recordformatbox = new JComboBox(CorrelationConfig.getDataFormats());
		recordformatbox.addItemListener(this);
		datasourcebox = new JComboBox(new String [] {"Disk", "Network"});
		datasourcebox.setSelectedIndex(0);
		datasourcebox.addItemListener(this);
		freqoffsetfields = new JTextField[0];
		bandfreqs = new JComboBox[0];
		bandpolarisations = new JTextField[0];
		datalist = new JTextArea();
		datalist.setRows(10);
		datalist.setColumns(0);
		datalist.addKeyListener(this);
		
		setLayout(new BorderLayout());
		toppanel = new JPanel(new GridLayout(0,1));
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(removebutton);
		toppanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(copybutton);
		toppanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(new JLabel("Tsys (Jy): "));
		temppanel.add(tsysfield);
		toppanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(new JLabel("Recording format: "));
		temppanel.add(recordformatbox);
		toppanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(new JLabel("Fanout: "));
		temppanel.add(fanoutfield);
		toppanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(new JLabel("Num bands: "));
		temppanel.add(numbandsfield);
		toppanel.add(temppanel);
		
		tablepanel = new JPanel(new GridLayout(1,2));
		tablepanel.add(new JLabel("Frequency (MHz)"));
		tablepanel.add(new JLabel("Polarisation"));
		
		bottompanel = new JPanel(new BorderLayout());
		bottompanel.add(datasourcebox, "North");
		diskpanel = new JPanel(new BorderLayout());
		diskpanel.add(adddatabutton, "North");
		diskpanel.add(new JScrollPane(datalist));
		bottompanel.add(diskpanel);
		networkpanel = new JPanel(new GridLayout(2,1));
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(new JLabel("Network port: "));
		temppanel.add(portfield);
		networkpanel.add(temppanel);
		temppanel = new JPanel(new FlowLayout(FlowLayout.CENTER));
		temppanel.add(new JLabel("TCP window size (kb): "));
		temppanel.add(tcpwindowsizefield);
		networkpanel.add(temppanel);
		sourcepanel = diskpanel;
		
		add(toppanel, "North");
		add(tablepanel);
		add(bottompanel, "South");
	}
	
	public int getDatastreamIndex() {
		return datastreamindex;
	}
	
	public void commitcorrchanges() {
		try {
			System.out.println("Starting to save datastream " + datastreamindex + ", numbands is " + numbands);
			int [] freqindices = new int[numbands];
			char [] polarisations = new char[numbands];
			System.out.println("About to set numbands");
			corrconfig.setNumInputBands(datastreamindex, numbands);
			for(int i=0;i<numbands;i++) {
				freqindices[i] = bandfreqs[i].getSelectedIndex();
				polarisations[i] = bandpolarisations[i].getText().charAt(0);
			}
			System.out.println("After the freqs and polarisation");
			corrconfig.setFreqIndices(datastreamindex, freqindices);
			corrconfig.setPolarisations(datastreamindex, polarisations);
			corrconfig.setTsys(datastreamindex, Double.parseDouble(tsysfield.getText()));
			corrconfig.setDataFormatIndex(datastreamindex, recordformatbox.getSelectedIndex());
			for(int i=0;i<corrconfig.getDatastreamNumFreqs(datastreamindex);i++)
				corrconfig.setDatastreamFreqClockOffset(datastreamindex, i, 
						Double.parseDouble(freqoffsetfields[i].getText()));
			if(((String)recordformatbox.getSelectedItem()).startsWith("MK"))
				corrconfig.setFanout(datastreamindex, Integer.parseInt(fanoutfield.getText()));
			System.out.println(datasourcebox.getSelectedItem());
			if(datasourcebox.getSelectedIndex() == 0) {
				corrconfig.setNetworkDatastream(datastreamindex, false);
				System.out.println("About to set datafile names");
				corrconfig.setDataFilenames(datastreamindex, datalist.getText().trim().split("\n"));
				System.out.println("Finished setting datafile names");
			}
			else {
				System.out.println(portfield.getText());
				System.out.println(tcpwindowsizefield.getText());
				corrconfig.setNetworkDatastream(datastreamindex, true);
				corrconfig.setPort(datastreamindex, Integer.parseInt(portfield.getText()));
				corrconfig.setTCPWindowSize(datastreamindex, Integer.parseInt(tcpwindowsizefield.getText()));
			}
		}
		catch(NumberFormatException e) {
			JOptionPane.showMessageDialog(this, "Problem setting datastream data: " + e.getMessage(),
					"Error!!!", JOptionPane.ERROR_MESSAGE);
		}
		System.out.println("Finished this datastream");
	}
	
	public void refreshdisplay() {
		refreshing = true;
		tsysfield.setText(corrconfig.getTsys(datastreamindex) + "");
		recordformatbox.setSelectedIndex(corrconfig.getDataFormatIndex(datastreamindex));
		if(((String)recordformatbox.getSelectedItem()).startsWith("MK"))
			fanoutfield.setText(corrconfig.getFanout(datastreamindex) + "");
		numbands = corrconfig.getNumInputBands(datastreamindex);
		int nfreqs = corrconfig.getDatastreamNumFreqs(datastreamindex);
		numbandsfield.setText(numbands + "");
		bottompanel.remove(sourcepanel);
		if(corrconfig.isNetworkDatastream(datastreamindex)) {
			datasourcebox.setSelectedIndex(1);
			sourcepanel = networkpanel;
		}
		else {
			datasourcebox.setSelectedIndex(0);
			sourcepanel = diskpanel;
		}
		bottompanel.add(sourcepanel);
		portfield.setText(corrconfig.getPort(datastreamindex) + "");
		tcpwindowsizefield.setText(corrconfig.getTCPWindowSize(datastreamindex) + "");
		String [] datafilenames = corrconfig.getDataFileList(datastreamindex);
		datalist.setText("");
		if(datafilenames != null) {
			System.out.println("About to do the " + datafilenames.length + " datafiles");
			for(int i=0;i<datafilenames.length;i++)
				datalist.append(datafilenames[i] + "\n");
		}

		tablepanel.removeAll();
		tablepanel.setLayout(new GridLayout(numbands+nfreqs+2,2));
		tablepanel.add(new JLabel("Frequency (MHz)"));
		tablepanel.add(new JLabel("Polarisation"));
		bandfreqs = new JComboBox[numbands];
		bandpolarisations = new JTextField[numbands];
		System.out.println("About to do the " + numbands + " bands");
		for(int i=0;i<numbands;i++) {
			bandfreqs[i] = new JComboBox(corrconfig.getFreqList());
			bandfreqs[i].setSelectedIndex(corrconfig.getFreqIndex(datastreamindex, i));
			bandfreqs[i].addItemListener(this);
			bandpolarisations[i] = new JTextField(10);
			bandpolarisations[i].setText(corrconfig.getPolarisation(datastreamindex, i) + "");
			bandpolarisations[i].addKeyListener(this);
			tablepanel.add(bandfreqs[i]);
			tablepanel.add(bandpolarisations[i]);
		}
		tablepanel.add(new JLabel("Frequency (MHz)"));
		tablepanel.add(new JLabel("Offset (microseconds)"));
		
		freqoffsetfields = new JTextField[nfreqs];
		for(int i=0;i<nfreqs;i++) {
			freqoffsetfields[i] = new JTextField(10);
			freqoffsetfields[i].setText(corrconfig.getDatastreamFreqClockOffset(datastreamindex, i) + "");
			freqoffsetfields[i].addKeyListener(this);
			tablepanel.add(new JLabel(corrconfig.getFreqList()[corrconfig.getDatastreamFreqIndex(datastreamindex, i)]));
			tablepanel.add(freqoffsetfields[i]);
		}
		refreshing = false;
	}

	public void actionPerformed(ActionEvent e) {
		Object source = e.getSource();
		if(refreshing)
			return; //ignore actionevents while updating  the screen
		
		if(source == numbandsfield) {
			changeNumBands();
			parent.setNotUpdated();
			tablepanel.repaint();
		}
		else if(source == removebutton) {
			parent.removeDataPanel(this);
		}
		else if(source == copybutton) {
			parent.copyDataPanel(this);
		}
		else if(source == adddatabutton) {
			getDataFiles();
		}
	}

	public void itemStateChanged(ItemEvent e) {
		if(refreshing)
			return;
		parent.setNotUpdated();
		if(e.getSource() == recordformatbox) {
			if(((String)recordformatbox.getSelectedItem()).startsWith("MK"))
				fanoutfield.setEditable(true);
			else
				fanoutfield.setEditable(false);
		}
		if(e.getSource() == datasourcebox) {
			bottompanel.removeAll();
			if(((String)datasourcebox.getSelectedItem()).equals("Disk"))
				bottompanel.add(diskpanel);
			else
				bottompanel.add(networkpanel);
		}
	}
	
	public void keyPressed(KeyEvent e) {}

	public void keyReleased(KeyEvent e) {}

	public void keyTyped(KeyEvent e) {
		Object source = e.getSource();
		if(source == datalist)
			parent.setNotUpdated();
		else {
			for(int i=0;i<corrconfig.getDatastreamNumFreqs(datastreamindex);i++) {
				if(source == freqoffsetfields[i])
					parent.setNotUpdated();
			}
			for(int i=0;i<numbands;i++) {
				if(source == bandpolarisations[i])
					parent.setNotUpdated();
			}
		}
	}
	
	private void changeNumBands() {
		try {
			numbands = Integer.parseInt(numbandsfield.getText());
		}
		catch (NumberFormatException e) {
			JOptionPane.showMessageDialog(this, "Non-integer value for number of bands",
					"Parse error!!!", JOptionPane.ERROR_MESSAGE);
		}
		JComboBox [] oldbandfreqs = bandfreqs;
		JTextField [] oldpolarisations = bandpolarisations;
		bandfreqs = new JComboBox[numbands];
		bandpolarisations = new JTextField[numbands];
		
		tablepanel.removeAll();
		tablepanel.add(new JLabel("Frequency (MHz)"));
		tablepanel.add(new JLabel("Polarisation"));
		
		for(int i=0;i<numbands;i++) {
			if(i<oldbandfreqs.length) {
				bandfreqs[i] = oldbandfreqs[i];
				bandpolarisations[i] = oldpolarisations[i];
			}
			else {
				bandfreqs[i] = new JComboBox(corrconfig.getFreqList());
				bandpolarisations[i] = new JTextField(10);
			}
			tablepanel.add(bandfreqs[i]);
			tablepanel.add(bandpolarisations[i]);
		}
	}
	
	private void getDataFiles() {
		File [] directorylist;
		String filterstring;
		File directory;
		FileFilter datafilter;
		int dialogstatus;
		
		dialogstatus = datachooser.showOpenDialog(this);
		if(dialogstatus == JFileChooser.APPROVE_OPTION) {
			directory = datachooser.getSelectedFile();
		  filterstring = JOptionPane.showInputDialog(this, "Enter the (simple) filter for files to select: ");
		  if(filterstring != null) {
		  	datafilter = new SimpleFilter(filterstring);
		  	directorylist = directory.listFiles(datafilter);
		  	Arrays.sort(directorylist);
		   	for(int i=0;i<directorylist.length;i++) {
		   		datalist.append(directorylist[i].getAbsolutePath() + "\n");
		   	}
		   	parent.setNotUpdated();
		  }
		}
	}
	
	private class SimpleFilter implements FileFilter {
		String [] orderedtokens;
		public SimpleFilter(String filterstring) {
			int numtokens;
			
			StringTokenizer st = new StringTokenizer(filterstring, "*");
			numtokens = st.countTokens();
			orderedtokens = new String[numtokens];
			for(int i=0;i<numtokens;i++) {
				orderedtokens[i] = st.nextToken();
			}
		}
		
		public boolean accept(File f) {
			String filename = f.getName();
			int lastindex = 0;
			for(int i=0;i<orderedtokens.length;i++) {
				if((lastindex = filename.indexOf(orderedtokens[i], lastindex)) < 0)
					return false;
			}
			return true;
		}
	}
}
