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
import java.awt.GridLayout;
import java.awt.event.*;
import java.io.File;

import org.math.plot.*;


import javax.swing.*;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.filechooser.FileFilter;
import javax.swing.table.DefaultTableModel;

public class PulsarPanel extends DiFXPanel implements ItemListener,
		ComponentListener, TableModelListener {

	private static final String[] bintablecolumnnames = { "Phase End", "Weight" };

	private PulsarCube pcube;

	private JFileChooser binconfigchooser;
	private FileFilter binconfigfilter;

	private boolean cubeactive, profileactive, changinglists;

	private int selectedxindex, selectedyindex, numbins, numconfigsloaded;

	private int[] inactiveindices;

	private String[][] bintablevalues;

	private JPanel controlpanel, tablepanel, tnorthpanel;

	private Plot3DPanel cubepanel;

	private Plot2DPanel profilepanel;

	private JButton showcubebutton, showbinconfigbutton;

	private JComboBox[] plotcontrols;

	private JTable bintable;

	private JScrollPane binscrollpane;

	private JTextField numbinstextfield;

	private JButton changenumbinsbutton, plotnewbutton;

	private JComboBox correlationBinConfigBox;

	public PulsarPanel(DiFXgui parent, CorrelationConfig corrconfig) {
		super(parent, corrconfig);
		mainpanel.setLayout(new BorderLayout());
		cubeactive = false;
		profileactive = true;
		changinglists = false;
		numbins = 0;
		numconfigsloaded = 0;
		this.addComponentListener(this);

		cubepanel = new Plot3DPanel("SOUTH");
		profilepanel = new Plot2DPanel("SOUTH");
		mainpanel.add(profilepanel);
		inactiveindices = new int[2];
		pcube = new PulsarCube();
		showcubebutton = new JButton("Show Cube");
		showcubebutton.addActionListener(this);
		showbinconfigbutton = new JButton("Show Bin Configuration");
		showbinconfigbutton.addActionListener(this);
		plotcontrols = new JComboBox[4];
		for (int i = 0; i < 4; i++)
			plotcontrols[i] = new JComboBox();
		for (int i = 0; i < PulsarCube.AXIS_NAMES.length; i++) {
			plotcontrols[0].addItem(new String(PulsarCube.AXIS_NAMES[i]));
			plotcontrols[1].addItem(new String(PulsarCube.AXIS_NAMES[i]));
		}
		plotcontrols[0].setSelectedIndex(0);
		plotcontrols[1].setSelectedIndex(1);
		updateControls();
		plotcontrols[0].addItemListener(this);
		plotcontrols[1].addItemListener(this);

		controlpanel = new JPanel();
		for (int i = 0; i < PulsarCube.AXIS_NAMES.length; i++)
			controlpanel.add(plotcontrols[i]);
		controlpanel.add(showbinconfigbutton);
		controlpanel.add(showcubebutton);
		mainpanel.add(controlpanel, "North");

		tablepanel = new JPanel(new BorderLayout());
		tnorthpanel = new JPanel(new GridLayout(3, 1));
		bintablevalues = new String[0][0];
		bintable = new JTable(bintablevalues, bintablecolumnnames);
		bintable.getModel().addTableModelListener(this);
		binscrollpane = new JScrollPane(bintable);
		tablepanel.add(binscrollpane);
		numbinstextfield = new JTextField("Number of bins", 15);
		changenumbinsbutton = new JButton("Change number of bins");
		changenumbinsbutton.addActionListener(this);
		correlationBinConfigBox = new JComboBox();
		correlationBinConfigBox.addItemListener(this);
		correlationBinConfigBox.setEditable(false);
		tnorthpanel.add(numbinstextfield);
		tnorthpanel.add(changenumbinsbutton);
		tnorthpanel.add(correlationBinConfigBox);
		tablepanel.add(tnorthpanel, "North");
		plotnewbutton = new JButton("Plot new binconfig");
		plotnewbutton.addActionListener(this);
		tablepanel.add(plotnewbutton, "South");
		mainpanel.add(tablepanel, "East");
		
		//set up the file chooser
		binconfigchooser = new JFileChooser();
		binconfigfilter = new FileFilter() {public boolean accept(File f)  { return f.getName().endsWith(".binconfig") || f.isDirectory(); } 
		                                    public String getDescription() { return "Pulsar bin configurations"; }};
		binconfigchooser.addChoosableFileFilter(binconfigfilter);
		binconfigchooser.setFileFilter(binconfigfilter);
	}

	// handle pulsar menu selection events
	public void openBinConfigFile() {
		int returnval;

		// open a jfiledialog and pick the file
		returnval = binconfigchooser.showOpenDialog(this);
		if (returnval == JFileChooser.APPROVE_OPTION) {
			// get the pulsarcube to load it up
			pcube.loadBinConfigFile(binconfigchooser.getSelectedFile().getAbsolutePath());
		}
		displayTableInfo();
	}

	public void saveBinConfigFile(boolean asknewfilename) {
		int returnval = 0;

		// open a jfiledialog and pick the file
		if (asknewfilename)
			returnval = binconfigchooser.showSaveDialog(this);
		if (!asknewfilename || (returnval == JFileChooser.APPROVE_OPTION)) {
			double[] tempbinphases = new double[numbins];
			double[] tempbinweights = new double[numbins];
			for (int i = 0; i < numbins; i++) {
				tempbinphases[i] = Double.valueOf((String) bintable.getValueAt(i, 0));
				tempbinweights[i] = Double.valueOf((String) bintable.getValueAt(i, 1));
			}

			pcube.setBinConfigPhases(tempbinphases);
			pcube.setBinConfigWeights(tempbinweights);
			if (asknewfilename)
				pcube.saveBinConfigFile(binconfigchooser.getSelectedFile()
						.getAbsolutePath());
			else
				pcube.saveBinConfigFile();
		}
	}

	public void openPulsarCubeFile() {
		int returnval;

		// open a jfiledialog and pick the file
		binconfigchooser = new JFileChooser();
		returnval = binconfigchooser.showOpenDialog(this);
		if (returnval == JFileChooser.APPROVE_OPTION) {
			// get the pulsarcube to load it up
			pcube.loadPulsarCubeFile(binconfigchooser.getSelectedFile()
					.getAbsolutePath());
		}
	}

	@Override
	public void commitcorrchanges() {
		// TODO Auto-generated method stub
		saveBinConfigFile(false);
		updated = true;
	}

	@Override
	public void refreshdisplay() {
		if (corrconfig.getNumConfigsLoaded() != numconfigsloaded) {
			numconfigsloaded = corrconfig.getNumConfigsLoaded();
			correlationBinConfigBox.removeAll();
			// add the new config selections
			for (int i = 0; i < corrconfig.getNumConfigs(); i++) {
				if (corrconfig.isPulsarBinning(i)) {
					correlationBinConfigBox.addItem("P: "
							+ corrconfig.getConfigSourceName(i));
				} else {
					correlationBinConfigBox.addItem("N: "
							+ corrconfig.getConfigSourceName(i));
				}
			}
		}

		int configindex = correlationBinConfigBox.getSelectedIndex();
		if (configindex >= 0 && corrconfig.isPulsarBinning(configindex)) {
			// pop up the bin configuration for this correlator config
			pcube.loadBinConfigFile(corrconfig.getPulsarConfigFilename(configindex));
			displayTableInfo();
			showBinConfig();
		}
	}

	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);

		Object source = e.getSource();

		if (source == showbinconfigbutton)
			showBinConfig();
		else if (source == showcubebutton)
			showCube();
		else if (source == changenumbinsbutton)
			changeNumBins();
		else if (source == plotnewbutton)
			plotNewBinConfig();
	}

	public void itemStateChanged(ItemEvent e) {
		Object source = e.getSource();

		if (source == plotcontrols[0] || source == plotcontrols[1])
			updateControls();
		else if (source == correlationBinConfigBox)
			refreshdisplay();

	}

	public void componentHidden(ComponentEvent e) {
	}

	public void componentMoved(ComponentEvent e) {
	}

	public void componentShown(ComponentEvent e) {
	}

	public void componentResized(ComponentEvent e) {
		if (e.getSource() == this) {
			profilepanel.setPreferredSize(new Dimension((3 * mainpanel
					.getWidth()) / 4, mainpanel.getHeight()));
			cubepanel.setPreferredSize(new Dimension(
					(3 * mainpanel.getWidth()) / 4, mainpanel.getHeight()));
			// System.out.println("Have been resized - size is now " +
			// this.getWidth() + ", " + this.getHeight());
			// tablepanel.setSize(new Dimension(this.getWidth()/4,
			// this.getHeight()));
			tablepanel.setMaximumSize(new Dimension(mainpanel.getWidth() / 4,
					mainpanel.getHeight()));
			tablepanel.setPreferredSize(new Dimension(mainpanel.getWidth() / 4,
					mainpanel.getHeight()));
			// bintable.setMaximumSize(new Dimension(this.getWidth()/4,
			// (3*this.getHeight())/4));
			// bintable.setPreferredSize(new Dimension(this.getWidth()/4,
			// (3*this.getHeight())/4));
			doLayout();
			// tablepanel.doLayout();
		}
	}
	
	public void tableChanged(TableModelEvent e) {
		if(e.getSource() == bintable.getModel()) {
			updated = false;
		}
	}

	private void displayTableInfo() {
		System.out.println("Displaying table info");
		numbins = pcube.getNumConfigPBins();

		// get the details from the pcube and stick them in the table
		double[] binconfigphases = new double[numbins];
		double[] binconfigweights = new double[numbins];
		for (int i = 0; i < numbins; i++) {
			binconfigphases[i] = pcube.getBinConfigPhases()[i];
			binconfigweights[i] = pcube.getBinConfigWeights()[i];
		}

		bintablevalues = new String[binconfigphases.length][2];
		for (int i = 0; i < binconfigphases.length; i++) {
			bintablevalues[i][0] = (binconfigphases[i]) + "";
			bintablevalues[i][1] = (binconfigweights[i]) + "";
		}

		bintable.setModel(new DefaultTableModel(bintablevalues, bintablecolumnnames));
		bintable.getModel().addTableModelListener(this);
	}

	private void showCube() {
		cubepanel.removeAllPlots();

		int unselectedindex1, unselectedindex2;
		unselectedindex1 = plotcontrols[2].getSelectedIndex() - 1;
		unselectedindex2 = plotcontrols[3].getSelectedIndex() - 1;

		if (cubeactive) {
			mainpanel.remove(cubepanel);
			cubeactive = false;
		}
		if (profileactive) {
			mainpanel.remove(profilepanel);
			profileactive = false;
		}
		pcube.setXIndex(selectedxindex);
		pcube.setYIndex(selectedyindex);
		System.out.println("Inactiveindices[0] is " + inactiveindices[0]);
		System.out.println("unselectedindex1 is " + unselectedindex1);
		System.out.println("Inactiveindices[1] is " + inactiveindices[1]);
		System.out.println("unselectedindex2 is " + unselectedindex2);
		pcube.calculate3DPlotValues(inactiveindices[0], unselectedindex1,
				inactiveindices[1], unselectedindex2);

		System.out.println("Length of x axis is" + pcube.getXValues().length);
		for (int i = 0; i < pcube.getXValues().length; i++)
			System.out.println(pcube.getXValues()[i] + "");
		System.out.println("Length of y axis is" + pcube.getYValues().length);
		for (int i = 0; i < pcube.getYValues().length; i++)
			System.out.println(pcube.getYValues()[i] + "");
		System.out.println("Length of z axis is" + pcube.getZValues().length);
		System.out.println("Length of z[0] axis is"
				+ pcube.getZValues()[0].length);
		/*
		 * for(int i=0;i<pcube.getZValues().length;i++) { for(int j=0;j<pcube.getZValues()[0].length;j++) {
		 * System.out.print(" " + pcube.getZValues()[i][j]); }
		 * System.out.println(); }
		 */
		cubepanel.addGridPlot(PulsarCube.AXIS_NAMES[selectedxindex] + " vs "
				+ PulsarCube.AXIS_NAMES[selectedyindex], pcube.getXValues(),
				pcube.getYValues(), pcube.getZValues());
		cubeactive = true;
		mainpanel.add(cubepanel);
		repaint();
	}

	private void showBinConfig() {
		profilepanel.removeAllPlots();

		double[] binconfigphases = pcube.getBinConfigPhases();
		double[] binconfigweights = pcube.getBinConfigWeights();

		if (cubeactive) {
			mainpanel.remove(cubepanel);
			cubeactive = false;
		}
		if (profileactive) {
			mainpanel.remove(profilepanel);
			profileactive = false;
		}
		// System.out.println("Numbins is " + numbins);
		// for(int i=0;i<numbins;i++)
		// System.out.println("Phase " + i + ": " + binphasemidpoints[i] +
		// "(came from " + binconfigphases[i] + ") = " + binconfigweights[i]);
		plotbins(binconfigphases, binconfigweights);
		mainpanel.add(profilepanel);
		profileactive = true;
	}

	private void changeNumBins() {
		updated = false;
		int oldnumbins = numbins;
		numbins = Integer.valueOf(numbinstextfield.getText());

		String[][] tempbintablevalues = new String[numbins][2];
		for (int i = 0; i < numbins; i++) {
			if (i < oldnumbins) {
				tempbintablevalues[i][0] = bintable.getValueAt(i, 0) + "";
				tempbintablevalues[i][1] = bintable.getValueAt(i, 1) + "";
			} else {
				if (i > 0)
					tempbintablevalues[i][0] = tempbintablevalues[i - 1][0]
							+ "";
				else
					tempbintablevalues[i][0] = "0.0";
				tempbintablevalues[i][1] = "0.0";
			}
		}

		bintable.setModel(new DefaultTableModel(tempbintablevalues,
				bintablecolumnnames));

		numbinstextfield.setText("Number of bins");
	}

	private void plotNewBinConfig() {
		double[] tempphases = new double[numbins];
		double[] tempamps = new double[numbins];

		for (int i = 0; i < numbins; i++) {
			tempphases[i] = Double.valueOf((String) bintable.getValueAt(i, 0));
			tempamps[i] = Double.valueOf((String) bintable.getValueAt(i, 1));
		}

		plotbins(tempphases, tempamps);
	}

	private void plotbins(double[] phaseends, double[] weights) {
		double[] binphasemidpoints = new double[phaseends.length];
		double[] amplitudes;

		if ((1.0 - phaseends[numbins - 1]) > phaseends[0]) {
			amplitudes = new double[weights.length];
			binphasemidpoints[numbins - 1] = (1 + phaseends[0] + phaseends[numbins - 1]) / 2.0;
			;
			for (int i = 0; i < numbins - 1; i++)
				binphasemidpoints[i] = (phaseends[i + 1] + phaseends[i]) / 2;
			for (int i = 0; i < numbins; i++)
				amplitudes[i] = weights[(i + 1) % numbins];
		} else {
			amplitudes = weights;
			binphasemidpoints[0] = (phaseends[0] + phaseends[numbins - 1] - 1) / 2;
			for (int i = 1; i < numbins; i++)
				binphasemidpoints[i] = (phaseends[i] + phaseends[i - 1]) / 2;
		}

		profilepanel.addLinePlot("Bin configuration", binphasemidpoints,
				amplitudes);
		repaint();
	}

	private void updateControls() {
		int count;
		double[] list;
		boolean[] inactive = { true, true, true, true };

		// work out what's selected and validate it
		if (changinglists)
			return; // spuriously generated events, ignore
		changinglists = true;
		selectedxindex = plotcontrols[0].getSelectedIndex();
		selectedyindex = plotcontrols[1].getSelectedIndex();
		if (selectedxindex == selectedyindex) {
			JOptionPane
					.showMessageDialog(
							this,
							"You must select two different parameters to plot against each other!!",
							"Error", JOptionPane.ERROR_MESSAGE);
			if (selectedxindex == 0) {
				selectedyindex = 1;
				plotcontrols[1].setSelectedIndex(1);
			} else {
				selectedxindex = 0;
				plotcontrols[0].setSelectedIndex(selectedxindex - 1);
			}
		}
		if (selectedxindex > selectedyindex) {
			plotcontrols[0].setSelectedIndex(selectedyindex);
			plotcontrols[1].setSelectedIndex(selectedxindex);
			selectedxindex = plotcontrols[0].getSelectedIndex();
			selectedyindex = plotcontrols[1].getSelectedIndex();
		}

		// set up the other options
		plotcontrols[2].removeAll();
		plotcontrols[3].removeAll();
		pcube.setXIndex(selectedxindex);
		pcube.setYIndex(selectedyindex);
		inactive[selectedxindex] = false;
		inactive[selectedyindex] = false;
		count = 0;
		plotcontrols[2].removeAllItems();
		plotcontrols[3].removeAllItems();
		for (int i = 0; i < inactive.length; i++) {
			if (inactive[i]) {
				list = pcube.getAxisValues(i);
				if (list != null) {
					plotcontrols[2 + count].addItem("Average");
					for (int j = 0; j < list.length; j++)
						plotcontrols[2 + count].addItem("" + list[j]);
					inactiveindices[count] = i;
				} else
					System.out
							.println("Error - received a null list from pulsarcube!!!");
				count++;
			}
		}
		changinglists = false;
	}
}
