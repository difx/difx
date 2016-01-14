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


import java.awt.GraphicsConfiguration;
import java.awt.HeadlessException;
import java.awt.event.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileFilter;

public class DiFXgui extends JFrame implements ActionListener, ChangeListener {

	private JTabbedPane displayarea;
	private DiFXPanel lastselectedpanel;
	private PulsarPanel pulsarpanel;
	private CorrSummaryPanel corrsummarypanel;
	private EditCommonPanel editcommonpanel;
	private EditConfigPanel editconfigpanel;
	private EditDatastreamPanel editdatastreampanel;
	private EditBaselinePanel editbaselinepanel;
	private EditNodePanel editnodepanel;
	private EditFreqPanel editfreqpanel;
	private JMenuBar mainmenubar;
	private JMenu pulsarmenu, mainmenu, modelmenu, advancedmenu;
	private JMenuItem[] pulsarmenuitems;
	private JMenuItem[] mainmenuitems;
	private JMenuItem[] modelmenuitems;
	private JMenuItem[] advancedmenuitems;
	private JFileChooser vexchooser, corrconfigchooser;
	private FileFilter vexfilter, corrconfigfilter;
	private String corr_root, calc_root, queuegatewaymachine;
	private static String vex2configcommand, vex2modelcommand, getnodescommand, getpbsnodescommand, getqueuescommand, vex2modetimelistcommand;
	private CorrelationConfig corrconfig;

	public DiFXgui() throws HeadlessException {
		super("DiFX GUI");
		
		//create the correlation config object
		corrconfig = new CorrelationConfig(this);
		
		//read in the commands from the default setup file
		readsetup();
		
		//create the filechoosers
		vexchooser = new JFileChooser();
		vexfilter = new FileFilter() {public boolean accept(File f)  { return f.getName().endsWith(".skd") || f.getName().endsWith("vex") || f.isDirectory(); } 
                                  public String getDescription() { return "VEX configuration files"; }};
    vexchooser.addChoosableFileFilter(vexfilter);
    vexchooser.setFileFilter(vexfilter);
		corrconfigchooser = new JFileChooser();
		corrconfigfilter = new FileFilter() {public boolean accept(File f)  { return f.getName().endsWith(".input") || f.isDirectory(); } 
                                         public String getDescription() { return "Correlation configuration files"; }};
    corrconfigchooser.addChoosableFileFilter(corrconfigfilter);
    corrconfigchooser.setFileFilter(corrconfigfilter);

		// set up the tabbed panels to be used
		displayarea = new JTabbedPane();
		displayarea.addChangeListener(this);
		pulsarpanel = new PulsarPanel(this, corrconfig);
		corrsummarypanel = new CorrSummaryPanel(this, corrconfig);
		editcommonpanel = new EditCommonPanel(this, corrconfig);
		editconfigpanel = new EditConfigPanel(this, corrconfig);
		editdatastreampanel = new EditDatastreamPanel(this, corrconfig);
		editbaselinepanel = new EditBaselinePanel(this, corrconfig);
		editnodepanel = new EditNodePanel(this, corrconfig);
		editfreqpanel = new EditFreqPanel(this, corrconfig);
		lastselectedpanel = corrsummarypanel;
		displayarea.add("Correlation Summary", corrsummarypanel);
		displayarea.add("Common Settings", editcommonpanel);
		displayarea.add("Config Settings", editconfigpanel);
		displayarea.add("Datastream Settings", editdatastreampanel);
		displayarea.add("Baseline Settings", editbaselinepanel);
		displayarea.add("Frequency Settings", editfreqpanel);
		displayarea.add("Node Settings", editnodepanel);
		displayarea.add("Pulsar", pulsarpanel);
		displayarea.setSelectedComponent(corrsummarypanel);
		add(displayarea);

		// setup the menubar
		mainmenubar = new JMenuBar();
		pulsarmenu = new JMenu("Pulsar");
		mainmenu = new JMenu("Main");
		modelmenu = new JMenu("Model");
		advancedmenu = new JMenu("Advanced");

		//create the "main" menu
		String [] mainmenustrings = {"Open existing configuration file", 
				                     "Create new configuration file from vex file",
				                     "Create new blank configuration file",
				                     "Save configuration file", "Save configuration file as", "Exit"};
		mainmenuitems = new JMenuItem[mainmenustrings.length]; 
		for(int i=0;i<mainmenustrings.length;i++) {
			mainmenuitems[i] = new JMenuItem(mainmenustrings[i]);
			mainmenuitems[i].addActionListener(this);
			mainmenu.add(mainmenuitems[i]);
		}

		//create the pulsar menu
		String [] pulsarmenustrings = {"Open binconfig file", "Open pulsarcube file", "Save binconfig file"};
		pulsarmenuitems = new JMenuItem[pulsarmenustrings.length];
		for (int i = 0; i < pulsarmenustrings.length; i++) {
			pulsarmenuitems[i] = new JMenuItem(pulsarmenustrings[i]);
			pulsarmenuitems[i].addActionListener(this);
			pulsarmenu.add(pulsarmenuitems[i]);
		}
		
		//create the model menu
		String [] modelmenustrings = {"Generate model using CALC from a vex file", "Generate model using CALC manually",
				                          "Add a station to the CALC database", "Add a source to the CALC database"};
		modelmenuitems = new JMenuItem[modelmenustrings.length];
		for(int i=0;i<modelmenustrings.length;i++) {
			modelmenuitems[i] = new JMenuItem(modelmenustrings[i]);
			modelmenuitems[i].addActionListener(this);
			modelmenu.add(modelmenuitems[i]);
		}
		
		//create the advanced menu
		String [] advancedmenustrings = {"Trim datafile selection to match configs", "Trim datafile selection to match correlation timerange","Trim datafile selections to match a mode"};
		advancedmenuitems = new JMenuItem[advancedmenustrings.length];
		for(int i=0;i<advancedmenustrings.length;i++) {
			advancedmenuitems[i] = new JMenuItem(advancedmenustrings[i]);
			advancedmenuitems[i].addActionListener(this);
			advancedmenu.add(advancedmenuitems[i]);
		}
		
		//add all the menus to the menubar
		mainmenubar.add(mainmenu);
		mainmenubar.add(modelmenu);
		mainmenubar.add(pulsarmenu);
		mainmenubar.add(advancedmenu);
		setJMenuBar(mainmenubar);
	}
	
	public static String getGetNodesCommand() {
		return getnodescommand;
	}
	
	public static String getGetPBSNodesCommand() {
		return getpbsnodescommand;
	}
	
	public static String getGetQueuesCommand() {
		return getqueuescommand;
	}
	
	public void launchcorrelation() {
		Runtime rt;
		Process launchprocess = null;
		PrintWriter out;
		BufferedReader stdoutreader, stderrreader;
		String clustername, line;
		String [] launchcommand = null;
                int walltimehours = corrconfig.getWalltime();
                int procspernode = corrconfig.getProcsPerNode();
		String cwd = System.getProperty("user.dir") + "/";
		int returnval;

		//check if changes need to be committed to disk - if so, do it
		if(!corrconfig.isUpdated()) {
			int choice = JOptionPane.showConfirmDialog(this, "Save changes to current file before launch?",
					"Save changes", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
			if(choice == JOptionPane.CANCEL_OPTION)
				return;
			if(choice == JOptionPane.YES_OPTION)
				corrconfig.saveCorrConfigFile();
		}
		
		try {
			//do a consistency check
			corrconfig.consistencyCheck(true);
			
			rt = Runtime.getRuntime();
			
			//check if its a pbs job, and launch appropriately
			if(corrconfig.usePbs()) {
				//create the little script to be run, and run it
				out = new PrintWriter(new FileWriter("pbs.script", false));
				out.println("#PBS -S /bin/csh");
				out.println("#PBS -l nodes=" + corrconfig.getNumNodes() + ":ppn=" + procspernode + ",walltime=" + walltimehours +
						":00:00");
                                out.println("@ ppn=" + procspernode);
				out.println();
				out.println("set activedir=" + cwd);
				out.println("set machinefilename=$activedir/machines.list");
				out.println();
				out.println("set nmax=`wc -l $PBS_NODEFILE`");
				out.println("set nmax=$nmax[1]");
				out.println("rm -f $machinefilename");
                                out.println("@ at=1");
                                out.println("@ i=0");
                                out.println("while ($i < $ppn)");
                                out.println("        @ at= 1 + $i");
                                out.println("        while ($at <= $nmax)");
                                out.println("               set NODENAME=`sed -n -e \"$at p\" $PBS_NODEFILE`");
				out.println("               echo $NODENAME : mpifxcorr " + corrconfig.getCurrentFilename() + " >> $machinefilename");
                                out.println("               @ at = $at + $ppn");
                                out.println("        end");
                                out.println("        @ i++");
                                out.println("end");
				out.println("");
				out.println("mpiexec -config $machinefilename");
				out.close();
				
				out = new PrintWriter(new FileWriter("launchme", false));
				clustername = corrconfig.getClusterName();
				launchcommand = new String [] {"ssh", queuegatewaymachine, "-f", cwd + "/launchme"};
				
				/*if(clustername.startsWith("DEFAULT"))
					launchcommand = new String [] {"ssh", queuegatewaymachine, "-f", "\"qsub -j oe -o " + 
													corrconfig.getCurrentFilename() + ".log " + cwd + "/pbs.script\""};
				else
					launchcommand = new String [] {"ssh", queuegatewaymachine, "-f", "\"qsub -q " + clustername + 
							" -j oe -o " + corrconfig.getCurrentFilename() + ".log " + cwd + "/pbs.script\""};*/
				if(clustername.startsWith("DEFAULT"))
					out.println("qsub -j oe -o " + corrconfig.getCurrentFilename() + ".log " + cwd + "/pbs.script");
				else
					out.println("qsub -q " + clustername + " -j oe -o " + corrconfig.getCurrentFilename() + 
							".log " + cwd + "/pbs.script");
				out.close();
				rt.exec("chmod 775 launchme");
			}
			else {
				//go ahead and run
				launchcommand = new String [] {"mpirun", "-np", corrconfig.getNumNodes()+"", "-machinefile", 
						corrconfig.getMachineFilename(), "-nolocal", corr_root + "/mpifxcorr/src/mpifxcorr", 
						corrconfig.getCurrentFilename(), ">&!", "mpifxoutput.txt", "&"};
			}
			System.out.print("About to run: ");
                        for(int i=0;i<launchcommand.length;i++)
                            System.out.print(launchcommand[i] + " ");
			launchprocess = rt.exec(launchcommand);
                        if(corrconfig.usePbs()) {
			    stdoutreader = new BufferedReader(new InputStreamReader(launchprocess.getInputStream()));
			    stderrreader = new BufferedReader(new InputStreamReader(launchprocess.getErrorStream()));
			    while ( (line = stdoutreader.readLine()) != null)
            System.out.println(line);
			    if((line = stderrreader.readLine()) != null)
			    {
			      JOptionPane.showMessageDialog(this, "Error executing " + launchcommand + ": " + line, "Error!!!", JOptionPane.ERROR_MESSAGE);
			      return; //abort, couldn't load the file
			    }
			    returnval = launchprocess.waitFor();
                        }
		} catch (ConsistencyException e) {
			JOptionPane.showMessageDialog(this, "Inconsistency: " + e.getMessage(), "Configuration inconsistency",
					JOptionPane.ERROR_MESSAGE);
		} catch (IOException e) {
			JOptionPane.showMessageDialog(this, "Runtime error: " + e.getMessage(), "Launch problem!!!",
					JOptionPane.ERROR_MESSAGE);
		} catch (InterruptedException e) {
			JOptionPane.showMessageDialog(this, "Runtime error: " + e.getMessage(), "Launch problem!!!",
					JOptionPane.ERROR_MESSAGE);
		}
	}
	
	public void actionPerformed(ActionEvent e) {
		Object source = e.getSource();

		if (source == pulsarmenuitems[0])
			pulsarpanel.openBinConfigFile();
		else if (source == pulsarmenuitems[1])
			pulsarpanel.openPulsarCubeFile();
		else if (source == pulsarmenuitems[2])
			pulsarpanel.saveBinConfigFile(true);

		else if (source == mainmenuitems[0])
			loadCorrConfigFile();
		else if (source == mainmenuitems[1])
			createConfigFile(true);
		else if (source == mainmenuitems[2])
			createConfigFile(false);
		else if (source == mainmenuitems[3])
			corrconfig.saveCorrConfigFile();
		else if (source == mainmenuitems[4])
			saveCorrConfigFileAs();
		else if (source == mainmenuitems[5])
			closegui();
		
		else if (source == modelmenuitems[0])
			createModelFiles(true);
		else if (source == modelmenuitems[1])
			createModelFiles(false);
		else if (source == modelmenuitems[2])
			addCalcStation();
		else if (source == modelmenuitems[3])
			addCalcSource();
		
		else if (source == advancedmenuitems[0])
			trimDataFilesBySource();
		else if (source == advancedmenuitems[1])
		        trimDataFilesByTimerange();
		else if (source == advancedmenuitems[2])
		        trimDataFilesByMode();
	}

	public void stateChanged(ChangeEvent e) {
		String confirmmessage = "The last active panel had unsaved changes - would you like to commit them to the current correlation configuration?  Cancel to return to editing last active panel.";
		int commitchanges;
		boolean cancel = false;
		Object source = e.getSource();
		DiFXPanel activepanel;

		if (source == displayarea) {
			if(displayarea.getSelectedComponent() == lastselectedpanel)
				return; //take care of the event that results when we switch back after a cancel
			if(!lastselectedpanel.isUpdated()) {
				commitchanges = JOptionPane.showConfirmDialog(this, confirmmessage, "Save changes?", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
				switch(commitchanges) {
					case JOptionPane.YES_OPTION:
						lastselectedpanel.commitcorrchanges();
					case JOptionPane.NO_OPTION:
						break;
					case JOptionPane.CANCEL_OPTION:
						cancel = true;
						break;
				}
			}
			
			if(cancel) {
				displayarea.setSelectedComponent(lastselectedpanel);
			}
			else {
				activepanel = (DiFXPanel)displayarea.getSelectedComponent();
				activepanel.refreshdisplay();
				lastselectedpanel = activepanel;
			}
		}
	}

	private void loadCorrConfigFile() {
		int returnval;

		// open a jfiledialog and pick the file
		returnval = corrconfigchooser.showOpenDialog(this);
		if (returnval == JFileChooser.APPROVE_OPTION) {
			corrconfig.loadCorrConfigFile(corrconfigchooser.getSelectedFile().getAbsolutePath());
			((DiFXPanel) displayarea.getSelectedComponent()).refreshdisplay();
		}
	}

	private int saveCorrConfigFileAs() {
		int returnval;

		// open a jfiledialog and pick the file
		returnval = corrconfigchooser.showSaveDialog(this);
		if (returnval == JFileChooser.APPROVE_OPTION) {
			corrconfig.saveCorrConfigFile(corrconfigchooser.getSelectedFile().getAbsolutePath());
		}
		return returnval;
	}
	
	private void createConfigFile(boolean fromvex) {
		Runtime rt;
		Process vex2config;
		PrintWriter redirected;
		BufferedReader stdoutreader, stderrreader;
		String line, vexpath = "", corrconfigpath = "";
		int returnval;
		
		//get the filename(s)
		if(fromvex) {
			returnval = vexchooser.showOpenDialog(this);
			if (returnval == JFileChooser.APPROVE_OPTION) 
				vexpath = vexchooser.getSelectedFile().getAbsolutePath();
			else
				return; //bail out since they didn't choose a file
		}
		returnval = corrconfigchooser.showSaveDialog(this);
		if (returnval == JFileChooser.APPROVE_OPTION) 
			corrconfigpath = corrconfigchooser.getSelectedFile().getAbsolutePath();
		else
			return; //bail out since they didn't choose a file
		
		//if we are creating from vex, run vex2config and tell corrconfig to load from that
		if(fromvex) {
			rt = Runtime.getRuntime();
			try {
				redirected = new PrintWriter(new FileOutputStream(corrconfigpath));
				vex2config = rt.exec(vex2configcommand + " " + vexpath + " " + vexchooser.getCurrentDirectory());
				stdoutreader = new BufferedReader(new InputStreamReader(vex2config.getInputStream()));
				stderrreader = new BufferedReader(new InputStreamReader(vex2config.getErrorStream()));
				while ( (line = stdoutreader.readLine()) != null)
          redirected.println(line);
				if((line = stderrreader.readLine()) != null)
				{
				  JOptionPane.showMessageDialog(this, "Error executing " + vex2configcommand + ": " + line, "Error!!!", JOptionPane.ERROR_MESSAGE);
				  return; //abort, couldn't load the file
				}
        redirected.flush();
				returnval = vex2config.waitFor();
			}
			catch(Exception e) {
				JOptionPane.showMessageDialog(this, "Error executing " + vex2configcommand + ": " + e.getMessage(), "Error!!!", JOptionPane.ERROR_MESSAGE);
			}
			corrconfig.loadCorrConfigFile(corrconfigpath);
		}
		else {		
			//otherwise just tell corrconfig to reset
			corrconfig.reset();
			corrconfig.setConfigFilename(corrconfigpath);
		}
		((DiFXPanel) displayarea.getSelectedComponent()).refreshdisplay();
	}
	
	private void createModelFiles(boolean fromvex) {
		int returnval;
		String vexpath, line;
		Runtime rt;
		Process vex2model;
		BufferedReader stdoutreader, stderrreader;
		PrintWriter outputreturn;
                boolean skipextrapolate = false;
		
		if(!fromvex) {
			JOptionPane.showMessageDialog(this, "Manual creation of model files not yet supported - fake up a vex file",
					                          "Sorry!!!", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		returnval = vexchooser.showOpenDialog(this);
		if (returnval == JFileChooser.APPROVE_OPTION)  {
			vexpath = vexchooser.getSelectedFile().getAbsolutePath();
		
			rt = Runtime.getRuntime();
			try {
				System.out.println("About to execute " + vex2modelcommand + " " + vexpath + " " + vexchooser.getCurrentDirectory());
				vex2model = rt.exec(vex2modelcommand + " " + vexpath + " " + vexchooser.getCurrentDirectory() + "/");
				System.out.println("Have started execution");
				stderrreader = new BufferedReader(new InputStreamReader(vex2model.getErrorStream()));
				stdoutreader = new BufferedReader(new InputStreamReader(vex2model.getInputStream()));
				System.out.println("Have created inputstreamreaders");
				//check and see if a reference is required for ATCA
				line = stdoutreader.readLine();
				while(line != null) {
					System.out.println("Read: " + line);
					if(line.startsWith("Enter ATCA station reference")) {
						line = JOptionPane.showInputDialog(this, "Please enter the ATCA reference position (eg CATW104): ", "Additional information", JOptionPane.QUESTION_MESSAGE);
						outputreturn = new PrintWriter(new OutputStreamWriter(vex2model.getOutputStream()));
						outputreturn.println(line);
						outputreturn.close();
					}
                                        else if(!skipextrapolate && line.indexOf("EOP FILE NOT UP TO DATE - INTERPOLATING") >= 0)
                                        {
                                          returnval = JOptionPane.showConfirmDialog(this, "The EOP file is not up to date and CALC must extrapolate from old EOPs - do you want to continue?  (Suggested course of action: Select NO and run update_eops in your $CALCDB directory)", "Warning - out-of-date EOPs!!", JOptionPane.YES_NO_OPTION);
                                          if(returnval == JOptionPane.YES_OPTION)
                                            skipextrapolate = true;
                                          else
                                            return; //bail out since the EOPs aren't up to date
                                        }
					line = stdoutreader.readLine();
				}
				
				if((line = stderrreader.readLine()) != null)
				{
				  JOptionPane.showMessageDialog(this, "Error executing " + vex2modelcommand + ": " + line, "Error!!!", JOptionPane.ERROR_MESSAGE);
				  return; //abort, couldn't load the file
				}
				System.out.println("About to waitfor");
				returnval = vex2model.waitFor();
			}
			catch(Exception e) {
				JOptionPane.showMessageDialog(this, "Error executing " + vex2modelcommand + ": " + e.getMessage(), "Error!!!", JOptionPane.ERROR_MESSAGE);
			}
		}
	}
	
	private void addCalcStation() {
		JOptionPane.showMessageDialog(this, "Adding a CALC station is not supported yet - edit $CALCDB/stations.tab by hand",
        "Sorry!!!", JOptionPane.ERROR_MESSAGE);
	}
	
	private void addCalcSource() {
		JOptionPane.showMessageDialog(this, "Adding a CALC station is not supported yet - edit $CALCDB/source.tab by hand",
        "Sorry!!!", JOptionPane.ERROR_MESSAGE);
	}
	
	private void readsetup() {
		BufferedReader input;
                int walltimehours;
		
		corr_root = System.getenv("CORR_ROOT");
		calc_root = System.getenv("CALCDB");
		
		try {
			input = new BufferedReader(new FileReader(corr_root + "/DiFXGUI/COMMANDS.CONFIG"));
			vex2configcommand = corr_root + "/utilities/" + corrconfig.getinputline(input, "VEX2CONFIG CMD");
			vex2modelcommand = corr_root + "/utilities/" + corrconfig.getinputline(input, "VEX2MODEL CMD");
			getnodescommand = corr_root + "/utilities/" + corrconfig.getinputline(input, "GETNODELIST CMD");
			getpbsnodescommand = corr_root + "/utilities/" + corrconfig.getinputline(input, "GETPBSNODELIST CMD");
			getqueuescommand = corr_root + "/utilities/" + corrconfig.getinputline(input, "GETQUEUES CMD");
			walltimehours = Integer.parseInt(corrconfig.getinputline(input, "PBS WALLTIME HOURS"));
                        corrconfig.setWalltime(walltimehours);
                        corrconfig.setProcsPerNode(1);
			queuegatewaymachine = corrconfig.getinputline(input, "QUEUE GATEWAY");
			vex2modetimelistcommand = corr_root + "/utilities/vex2modetimelist.pl";
		}
		catch(IOException e) {
			JOptionPane.showMessageDialog(this, "Problem opening " + corr_root + "/DiFXgui/COMMANDS.CONFIG: " + 
					                          e.getMessage() + " DiFX is aborting", "FATAL ERROR!!!",
					                          JOptionPane.ERROR_MESSAGE);
			System.exit(1);
		}
	}
	
	private void trimDataFilesBySource() {
		int accept = JOptionPane.showConfirmDialog(this, "This function only works with data files of format ***_dayofyear_hhmmss.xxx - are you sure you want to continue?",
				"Warning!!!", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
		if(accept == JOptionPane.YES_OPTION) {
			corrconfig.trimDataFilesBySource();
		}
	}

	private void trimDataFilesByTimerange() {
		int accept = JOptionPane.showConfirmDialog(this, "This function only works with data files of format ***_dayofyear_hhmmss.xxx - are you sure you want to continue?",
				"Warning!!!", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
		if(accept == JOptionPane.YES_OPTION) {
			corrconfig.trimDataFilesByTimerange();
		}
	}

        private void trimDataFilesByMode() {
	    String vexpath, mode, line, vals = "";
	    String [] splitline;
	    Runtime rt;
	    Process vex2modetimelist;
	    BufferedReader stdoutreader, stderrreader;
	    int accept, numentries;
	    int [] modestarttimes;
	    int [] modestoptimes;

	    accept = JOptionPane.showConfirmDialog(this, "This function only works with data files of format ***_dayofyear_hhmmss.xxx - are you sure you want to continue?",
				"Warning!!!", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
	    if(accept == JOptionPane.YES_OPTION) {
		mode = JOptionPane.showInputDialog(this, "Enter the name of the mode you want to cut down to", "Mode Entry", JOptionPane.QUESTION_MESSAGE);
		accept = vexchooser.showOpenDialog(this);
		if (accept == JFileChooser.APPROVE_OPTION)  {
		    vexpath = vexchooser.getSelectedFile().getAbsolutePath();
		
		    rt = Runtime.getRuntime();
		    try {
			System.out.println("About to execute " + vex2modetimelistcommand + " " + vexpath + " " + mode);
			vex2modetimelist = rt.exec(vex2modetimelistcommand + " " + vexpath + " " + mode);
			System.out.println("Have started execution");
			stderrreader = new BufferedReader(new InputStreamReader(vex2modetimelist.getErrorStream()));
			stdoutreader = new BufferedReader(new InputStreamReader(vex2modetimelist.getInputStream()));
			System.out.println("Have created inputstreamreaders");
			line = stdoutreader.readLine();
			while(line != null) {
			    System.out.println("Read line " + line);
			    vals = vals + " " + line.trim();
			    System.out.println("Vals is now " + vals);
			    line = stdoutreader.readLine();
			}
			if((line = stderrreader.readLine()) != null){
			    System.out.println("Stderr not empty! Was " + line);
			    JOptionPane.showMessageDialog(this, "Error executing " + vex2modetimelistcommand + ": " + line, "Error!!!", JOptionPane.ERROR_MESSAGE);
			    return; //abort, couldn't load the file
			}
			System.out.println("About to waitfor");
			vex2modetimelist.waitFor();
			splitline = vals.trim().split(" ");
			numentries = splitline.length/2;
			if(numentries == 0) {
			    JOptionPane.showMessageDialog(this, "Didn't find any scans for mode " + mode, "Error!!!", JOptionPane.ERROR_MESSAGE);
			    return; //abort, couldn't load the file
			}
			modestarttimes = new int[numentries];
			modestoptimes = new int[numentries];
			for(int i=0;i<numentries;i++) {
			    System.out.println("Storing entry " + i + " - will be " + splitline[2*i] + ", " + splitline[2*i+1]);
			    modestarttimes[i] = Integer.parseInt(splitline[2*i]);
			    modestoptimes[i] = Integer.parseInt(splitline[2*i+1]);
			}
			System.out.println("About to actually trim the files");
			corrconfig.trimDataFilesByMode(numentries,modestarttimes,modestoptimes);
		    }
		    catch(Exception e) {
			JOptionPane.showMessageDialog(this, "Error executing " + vex2modetimelistcommand + ": " + e.getMessage(), "Error!!!", JOptionPane.ERROR_MESSAGE);
		    }
		}
	    }
	}
	
	private void closegui() {
		int response, saveresponse;
		
		if(!(((DiFXPanel)displayarea.getSelectedComponent()).isUpdated())) {
			response = JOptionPane.showConfirmDialog(this, "Commit changes to the correlation configuration and save to disk before exiting?", 
                    "Save before exit?", JOptionPane.YES_NO_CANCEL_OPTION, 
                    JOptionPane.QUESTION_MESSAGE);
			switch(response) {
			case JOptionPane.CANCEL_OPTION:
				break;
			case JOptionPane.YES_OPTION:
				((DiFXPanel)displayarea.getSelectedComponent()).commitcorrchanges();
				saveresponse = saveCorrConfigFileAs();
				if(saveresponse == JFileChooser.CANCEL_OPTION)
					break;
				//otherwise fall through to the NO response, which is to actually exit
			case JOptionPane.NO_OPTION:
				System.exit(0);
			}
			return;
		}
		else if(!corrconfig.isUpdated()) {
			response = JOptionPane.showConfirmDialog(this, "Save changes to the current correlation configuration before exiting?", 
					                                           "Save before exit?", JOptionPane.YES_NO_CANCEL_OPTION, 
					                                           JOptionPane.QUESTION_MESSAGE);
			switch (response) {
				case JOptionPane.CANCEL_OPTION:
					break;
				case JOptionPane.YES_OPTION:
					saveresponse = saveCorrConfigFileAs();
					if(saveresponse == JFileChooser.CANCEL_OPTION)
						break;
					//otherwise fall through to the NO response, which is to actually exit
				case JOptionPane.NO_OPTION:
					System.exit(0);
			}
		}
		else
			System.exit(0);
	}
}
