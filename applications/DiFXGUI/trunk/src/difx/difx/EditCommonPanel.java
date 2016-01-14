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
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.filechooser.FileFilter;

public class EditCommonPanel extends DiFXPanel implements ItemListener {

	private JTextField startyearfield, endyearfield, startdayfield, enddayfield, starthourfield, endhourfield;
	private JTextField startminutefield, endminutefield, startsecondfield, endsecondfield;
	private JComboBox startmonthbox, endmonthbox;
	private JTextField numdatastreamsfield, numbaselinesfield, numdatasegmentsfield, databufferfactorfield;
	private JComboBox outputformatbox, headeroverridebox;
	private JTextField delayfilenamefield, uvwfilenamefield, outputfilenamefield;
	private JButton selectdelayfilebutton, selectuvwfilebutton, selectoutputfilebutton;
	private JLabel startmjdlabel, startsecondslabel, endmjdlabel, endsecondslabel, experimentstartlabel, experimentendlabel;
	private JPanel holdingpanel, starttimepanel, endtimepanel;
	private JFileChooser chooser;
	private FileFilter rpffilter;
	private FileFilter delayfilter;
	private FileFilter uvwfilter;
	private DateTimeUpdater datetimeupdater;
	private int startmjd, startseconds, endmjd, endseconds, executeseconds;
	private final String [] monthnames = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
	
	public EditCommonPanel(DiFXgui parent, CorrelationConfig corrconfig) {
		super(parent, corrconfig);
		
		//create all the components and lay them out - do it in rows
		mainpanel.setLayout(new BorderLayout());
		
		//do the start and end time panels
		starttimepanel = new JPanel(onecollayout);
		experimentstartlabel = new JLabel("Experiment Start");
		starttimepanel.add(experimentstartlabel);
		startyearfield = new JTextField(10);
		addInputField(starttimepanel, startyearfield, "Start year: ");
		startmonthbox = new JComboBox(monthnames);
		addInputField(starttimepanel, startmonthbox, "Start month: ");
		startdayfield = new JTextField(10);
		addInputField(starttimepanel, startdayfield, "Start day: ");
		starthourfield = new JTextField(10);
		addInputField(starttimepanel, starthourfield, "Start hour: ");
		startminutefield = new JTextField(10);
		addInputField(starttimepanel, startminutefield, "Start minute: ");
		startsecondfield = new JTextField(10);
		addInputField(starttimepanel, startsecondfield, "Start second: ");
		startmjdlabel = new JLabel("Start MJD: ");
		starttimepanel.add(startmjdlabel);
		startsecondslabel = new JLabel("Start seconds: ");
		starttimepanel.add(startsecondslabel);
		
		endtimepanel = new JPanel(onecollayout);
		experimentendlabel = new JLabel("Experiment End");
		endtimepanel.add(experimentendlabel);
		endyearfield = new JTextField(10);
		addInputField(endtimepanel, endyearfield, "End year: ");
		endmonthbox = new JComboBox(monthnames);
		addInputField(endtimepanel, endmonthbox, "End month: ");
		enddayfield = new JTextField(10);
		addInputField(endtimepanel, enddayfield, "End day: ");
		endhourfield = new JTextField(10);
		addInputField(endtimepanel, endhourfield, "End hour: ");
		endminutefield = new JTextField(10);
		addInputField(endtimepanel, endminutefield, "End minute: ");
		endsecondfield = new JTextField(10);
		addInputField(endtimepanel, endsecondfield, "End second: ");
		endmjdlabel = new JLabel("End MJD: ");
		endtimepanel.add(endmjdlabel);
		endsecondslabel = new JLabel("End seconds: ");
		endtimepanel.add(endsecondslabel);
		
		holdingpanel = new JPanel(twocollayout);
		holdingpanel.add(starttimepanel);
		holdingpanel.add(endtimepanel);
		mainpanel.add(holdingpanel, "North");
		holdingpanel = new JPanel(onecollayout);
		mainpanel.add(holdingpanel);
		
		//do the next few fields
		numdatastreamsfield = new JTextField(10);
		numdatastreamsfield.addActionListener(this);
		addInputField(holdingpanel, numdatastreamsfield, "Number of datastreams: ");
		numbaselinesfield = new JTextField(10);
		numbaselinesfield.addActionListener(this);
		addInputField(holdingpanel, numbaselinesfield, "Number of baselines: ");
		numdatasegmentsfield = new JTextField(10);
		numdatasegmentsfield.addActionListener(this);
		addInputField(holdingpanel, numdatasegmentsfield, "Number of segments in data buffer: ");
		databufferfactorfield = new JTextField(10);
		databufferfactorfield.addActionListener(this);
		addInputField(holdingpanel, databufferfactorfield, "Length of data buffer (x max FFT send size): ");
		outputformatbox = new JComboBox(new String [] {"ASCII", "RPFITS"});
		outputformatbox.addItemListener(this);
		addInputField(holdingpanel, outputformatbox, "Output format: ");
		//headeroverridebox = new JComboBox(new Boolean [] {new Boolean(true), new Boolean(false)});
		//addInputField(mainpanel, headeroverridebox, "Allow parameter override by data headers: ");
		
		//do the filename fields
		outputfilenamefield = new JTextField(30);
		outputfilenamefield.addActionListener(this);
		selectoutputfilebutton = new JButton("Select output file");
		selectoutputfilebutton.addActionListener(this);
		addInputField(holdingpanel, outputfilenamefield, "Output filename: ", selectoutputfilebutton); 
		delayfilenamefield = new JTextField(30);
		delayfilenamefield.addActionListener(this);
		selectdelayfilebutton = new JButton("Select delay file");
		selectdelayfilebutton.addActionListener(this);
		addInputField(holdingpanel, delayfilenamefield, "Delay filename: ", selectdelayfilebutton); 
		uvwfilenamefield = new JTextField(30);
		uvwfilenamefield.addActionListener(this);
		selectuvwfilebutton = new JButton("Select uvw file");
		selectuvwfilebutton.addActionListener(this);
		addInputField(holdingpanel, uvwfilenamefield, "UVW filename: ", selectuvwfilebutton); 

		//create the filechooser and the file filters
		chooser = new JFileChooser();
		rpffilter = new FileFilter() {public boolean accept(File f)  { return f.getName().endsWith(".rpf") || f.isDirectory(); } 
                                  public String getDescription() { return "RPFITS files"; }};
    delayfilter = new FileFilter() {public boolean accept(File f)  { return f.getName().endsWith(".delay") || f.isDirectory(); } 
                                    public String getDescription() { return "Delay model files"; }};    
    uvwfilter = new FileFilter() {public boolean accept(File f)  { return f.getName().endsWith(".uvw") || f.isDirectory(); } 
                                    public String getDescription() { return "UVW model files"; }};
                                     
    //create the datetimeupdater and add the listeners to it
    datetimeupdater = new DateTimeUpdater(this);
    startyearfield.addActionListener(this);
    startmonthbox.addItemListener(this);
    startdayfield.addActionListener(this);
    starthourfield.addActionListener(this);
    startminutefield.addActionListener(this);
    startsecondfield.addActionListener(this);
    endyearfield.addActionListener(this);
    endmonthbox.addItemListener(this);
    enddayfield.addActionListener(this);
    endhourfield.addActionListener(this);
    endminutefield.addActionListener(this);
    endsecondfield.addActionListener(this);
	}

	@Override
	public void commitcorrchanges() {
		//copy what's on the screen to the corrconfig
		datetimeupdater.updateDateTime();
		
		corrconfig.setCommonSettings(delayfilenamefield.getText(), uvwfilenamefield.getText(),  
        startmjd, startseconds, executeseconds, Integer.parseInt(numdatastreamsfield.getText()), 
        Integer.parseInt(numbaselinesfield.getText()), Integer.parseInt(numdatasegmentsfield.getText()),
        Integer.parseInt(databufferfactorfield.getText()), false, (String)outputformatbox.getSelectedItem(), 
        outputfilenamefield.getText());
		updated = true;
	}

	@Override
	public void refreshdisplay() {
	  int [] ymd;
		
		//get everything from the corrconfig again
		delayfilenamefield.setText(corrconfig.getDelayFilename());
		uvwfilenamefield.setText(corrconfig.getUVWFilename());
		startmjd = corrconfig.getStartMJD();
		startseconds = corrconfig.getStartSeconds();
		executeseconds = corrconfig.getExecuteSeconds();
		numdatastreamsfield.setText(Integer.toString(corrconfig.getNumDatastreams()));
		numbaselinesfield.setText(Integer.toString(corrconfig.getNumBaselines()));
		numdatasegmentsfield.setText(corrconfig.getNumDataSegments() + "");
		databufferfactorfield.setText(corrconfig.getDataBufferFactor() + "");
		outputformatbox.setSelectedItem(corrconfig.getOutputFormat());
		outputfilenamefield.setText(corrconfig.getOutputFilename());
		
		//work out months and days, hours and minutes etc from the mjd and seconds and update those fields
		ymd = CorrelationConfig.mjd2ymd(startmjd);
		startyearfield.setText(ymd[0] + "");
		startmonthbox.setSelectedIndex(ymd[1] - 1);
		startdayfield.setText(ymd[2] + "");
		starthourfield.setText(startseconds/3600 + "");
		startminutefield.setText((startseconds%3600)/60 + "");
		startsecondfield.setText(startseconds%60 + "");
		
		endmjd = startmjd + (startseconds+executeseconds)/86400;
		endseconds = (startseconds+executeseconds)%86400;
		ymd = CorrelationConfig.mjd2ymd(endmjd);
		endyearfield.setText(ymd[0] + "");
		endmonthbox.setSelectedIndex(ymd[1] - 1);
		enddayfield.setText(ymd[2] + "");
		endhourfield.setText(endseconds/3600 + "");
		endminutefield.setText((endseconds%3600)/60 + "");
		endsecondfield.setText(endseconds%60 + "");
		
		datetimeupdater.updateDateTime();
		updated = true;
	}
	
	public void actionPerformed(ActionEvent e) {
		super.actionPerformed(e);
		Object source = e.getSource();
		
		if(source == selectoutputfilebutton)
			popupOutputChooser();
		else if(source == selectdelayfilebutton)
			popupDelayChooser();
		else if(source == selectuvwfilebutton)
			popupUVWChooser();
	}
	
	public void itemStateChanged(ItemEvent e) {
		updated = false;
	}
	
	private void popupOutputChooser() {
		int returnval;
		
		chooser.addChoosableFileFilter(rpffilter);
		chooser.setFileFilter(rpffilter);

		// open a jfiledialog and pick the file
		returnval = chooser.showSaveDialog(this);
		if(returnval == JFileChooser.APPROVE_OPTION)
			outputfilenamefield.setText(chooser.getSelectedFile().getAbsolutePath());
		updated = false;
		chooser.removeChoosableFileFilter(rpffilter);
	}
	
	private void popupDelayChooser() {
		int returnval;
		
		chooser.addChoosableFileFilter(delayfilter);
		chooser.setFileFilter(delayfilter);

		// open a jfiledialog and pick the file
		returnval = chooser.showOpenDialog(this);
		if(returnval == JFileChooser.APPROVE_OPTION)
			delayfilenamefield.setText(chooser.getSelectedFile().getAbsolutePath());
		updated = false;
		chooser.removeChoosableFileFilter(delayfilter);
	}
	
	private void popupUVWChooser() {
		int returnval;
		
		chooser.addChoosableFileFilter(uvwfilter);
		chooser.setFileFilter(uvwfilter);

		// open a jfiledialog and pick the file
		returnval = chooser.showOpenDialog(this);
		if(returnval == JFileChooser.APPROVE_OPTION)
			uvwfilenamefield.setText(chooser.getSelectedFile().getAbsolutePath());
		updated = false;
		chooser.removeChoosableFileFilter(uvwfilter);
	}

	class DateTimeUpdater implements ActionListener, ItemListener {
		private Component parent;
		
		public DateTimeUpdater(Component parent) {
			this.parent = parent;
		}
		
		public void actionPerformed (ActionEvent e) {
			updateDateTime();
		}
		public void itemStateChanged (ItemEvent e) {
			updateDateTime();
		}
		
		public void updateDateTime() {
			int startyear, startmonth, startday, starthour, startminute, startsecond;
			int endyear, endmonth, endday, endhour, endminute, endsecond;
			updated = false;
			
			try {
				//work out the mjds etc
				startyear = Integer.valueOf(startyearfield.getText());
				startmonth = startmonthbox.getSelectedIndex() + 1;
				startday = Integer.valueOf(startdayfield.getText());
				starthour = Integer.valueOf(starthourfield.getText());
				startminute = Integer.valueOf(startminutefield.getText());
				startsecond = Integer.valueOf(startsecondfield.getText());
				
				endyear = Integer.valueOf(endyearfield.getText());
				endmonth = endmonthbox.getSelectedIndex() + 1;
				endday = Integer.valueOf(enddayfield.getText());
				endhour = Integer.valueOf(endhourfield.getText());
				endminute = Integer.valueOf(endminutefield.getText());
				endsecond = Integer.valueOf(endsecondfield.getText());

				startmjd = CorrelationConfig.ymd2mjd(startyear, startmonth, startday);
				startseconds = 3600*starthour + 60*startminute + startsecond;
				endmjd = CorrelationConfig.ymd2mjd(endyear, endmonth, endday);
				endseconds = 3600*endhour + 60*endminute + endsecond;
				executeseconds = (endmjd-startmjd)*86400 + endseconds - startseconds;
				
				//set the label texts
				startmjdlabel.setText("Start MJD: " + startmjd);
				startsecondslabel.setText("Start seconds: " + startseconds);
				endmjdlabel.setText("End MJD: " + endmjd);
				endsecondslabel.setText("End seconds: " + endseconds);
			}
			catch (NumberFormatException e) {
				JOptionPane.showMessageDialog(parent, "Error parsing dates and times: " + e.getMessage(), "Numerical error!!!", JOptionPane.ERROR_MESSAGE);
			}
		}
	}
}
