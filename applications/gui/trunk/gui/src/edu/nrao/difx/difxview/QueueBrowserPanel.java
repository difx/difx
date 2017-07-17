/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
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
package edu.nrao.difx.difxview;

import edu.nrao.difx.difxcontroller.AttributedMessageListener;

import java.io.File;

import java.util.Iterator;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.util.ArrayDeque;
import java.util.HashMap;

import edu.nrao.difx.difxcontroller.DiFXMessageProcessor;

import edu.nrao.difx.xmllib.difxmessage.DifxMessage;

import edu.nrao.difx.difxdatabase.QueueDBConnection;
import edu.nrao.difx.difxutilities.DiFXCommand_ls;
import edu.nrao.difx.difxutilities.TabCompletedTextField;
import edu.nrao.difx.difxutilities.ChannelServerSocket;

import java.awt.*;

import java.lang.instrument.Instrumentation;

import java.awt.event.*;

import java.sql.ResultSet;
import java.util.ArrayList;
import javax.swing.*;
import mil.navy.usno.widgetlib.*;

public class QueueBrowserPanel extends TearOffPanel {

    public QueueBrowserPanel( SystemSettings systemSettings ) {
        _settings = systemSettings;
        _settings.queueBrowser( this );
        setLayout( null );
        _browserPane = new NodeBrowserScrollPane();
        this.add( _browserPane );
        _headerPane = new NodeBrowserScrollPane();
        _headerPane.noScrollbar( true );
        this.add( _headerPane );
        addKeyListener( new KeyEventListener() );
        _browserPane.setBackground( Color.WHITE );
        _mainLabel = new JLabel( "Queue Browser" );
        _mainLabel.setBounds( 5, 0, 150, 20 );
        _mainLabel.setFont( new Font( "Dialog", Font.BOLD, 14 ) );
        add( _mainLabel );
        _experimentButton = new JButton( "Experiments" );
        _experimentButton.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _experimentMenu.show( _experimentButton, 0, 25 );
            }
        });
        this.add( _experimentButton );
        _experimentMenu = new JPopupMenu( "Experiments:" );
        _newExperimentItem = new ZMenuItem( "Create New..." );
        _newExperimentItem.setToolTipText( "Create a new experiment." );
        _newExperimentItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                newExperiment();
            }
        });
        _experimentMenu.add( _newExperimentItem );
        _existingExperimentItem = new ZMenuItem( "Locate on Disk..." );
        _existingExperimentItem.setToolTipText( "Locate previously created experiments from disk on the DiFX cluster." );
        _existingExperimentItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                setDiskSearchRules();
            }
        });
        _experimentMenu.add( _existingExperimentItem );
        _fromDatabaseItem = new ZMenuItem( "Located in Database..." );
        _fromDatabaseItem.setToolTipText( "Locate experiments in the database." );
        _fromDatabaseItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //newExperiment();
            }
        });
        _experimentMenu.add( _fromDatabaseItem );
        _updateDatabaseItem = new ZMenuItem( "Update Now" );
        _updateDatabaseItem.setToolTipText( "Update the experiment list to reflect any changes in the database or disk." );
        _updateDatabaseItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _triggerDatabaseUpdate = true;
            }
        });
        _updateDatabaseItem.setEnabled( false );
        _experimentMenu.add( _updateDatabaseItem );
        //  The menu for the "select" button.
        _selectMenu = new JPopupMenu();
        ZMenuItem selectAllItem = new ZMenuItem( "Select All" );
        selectAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectAll();
            }
        });
        _selectMenu.add( selectAllItem );
        ZMenuItem unselectAllItem = new ZMenuItem( "Unselect All" );
        unselectAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                unselectAll();
            }
        });
        _selectMenu.add( unselectAllItem );
        ZMenuItem selectIncompleteItem = new ZMenuItem( "Select Incomplete" );
        selectIncompleteItem.setToolTipText( "Select all jobs for which the State is not \"Done\"." );
        selectIncompleteItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectIncomplete();
            }
        });
        _selectMenu.add( selectIncompleteItem );
        _selectMenu.add( new JSeparator() );
        ZMenuItem runSelectedItem = new ZMenuItem( "Schedule Selected" );
        runSelectedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                runSelected( false );
            }
        });
        _selectMenu.add( runSelectedItem );
        runSelectedItem.toolTip( "Schedule the selected jobs to run using either job-specific user settings\n"
                + "or the current automated run parameters.\n"
                + "Automated run parameters can be viewed and changed in the Settings Window.", null );
        ZMenuItem scheduleIncompleteItem = new ZMenuItem( "Schedule Incomplete" );
        scheduleIncompleteItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                scheduleIncomplete( false );
            }
        });
        _selectMenu.add( scheduleIncompleteItem );
        scheduleIncompleteItem.toolTip( "Schedule any jobs that do not have state \"Done\".", null );
        ZMenuItem scheduleAllItem = new ZMenuItem( "Schedule All" );
        scheduleAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                scheduleAll( false );
            }
        });
        _selectMenu.add( scheduleAllItem );
        scheduleAllItem.toolTip( "Schedule all jobs to run.", null );

        ZMenuItem configTestSelectedItem = new ZMenuItem( "Config Test Selected" );
        configTestSelectedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                runSelected( true );
            }
        });
        _selectMenu.add( configTestSelectedItem );
        configTestSelectedItem.toolTip( "Schedule a configuration test on the selected jobs.\n", null );
        ZMenuItem configTestIncompleteItem = new ZMenuItem( "Config Test Incomplete" );
        configTestIncompleteItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                scheduleIncomplete( true );
            }
        });
        _selectMenu.add( configTestIncompleteItem );
        configTestIncompleteItem.toolTip( "Schedule any jobs that do not have state \"Done\" for a configuration test.", null );
        ZMenuItem configTestAllItem = new ZMenuItem( "Config Test All" );
        configTestAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                scheduleAll( true );
            }
        });
        _selectMenu.add( configTestAllItem );
        configTestAllItem.toolTip( "Schedule all jobs for a configuration test.\n", null );
       
        ZMenuItem unscheduleSelectedItem = new ZMenuItem( "Remove Selected From Schedule" );
        unscheduleSelectedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                unscheduleSelected();
            }
        });
        unscheduleSelectedItem.setEnabled( true );
        _selectMenu.add( unscheduleSelectedItem );
        unscheduleSelectedItem.toolTip( "Remove the selected jobs from the current schedule.  This\n"
                + "is harmless if the job is not actually scheduled.", null );
        ZMenuItem deleteSelectedItem = new ZMenuItem( "Delete Selected" );
        deleteSelectedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                deleteSelected();
            }
        });
        _selectMenu.add( deleteSelectedItem );
        _selectButton = new JButton( "Select" );
        _selectButton.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _selectMenu.show( _selectButton, 0, 25 );
            }
        });
        this.add( _selectButton );
        //  The menu for the "show" button.
        _showMenu = new JPopupMenu();
        _showSelectedItem = new JCheckBoxMenuItem( "Selected" );
        _showSelectedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showItemChange();
            }
        });
        _showSelectedItem.setEnabled( false );
        _showSelectedItem.setSelected( _settings.queueBrowserSettings().showSelected );
        _showMenu.add( _showSelectedItem );
        _showUnselectedItem = new JCheckBoxMenuItem( "Unselected" );
        _showUnselectedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showItemChange();
            }
        });
        _showUnselectedItem.setEnabled( false );
        _showUnselectedItem.setSelected( _settings.queueBrowserSettings().showUnselected );
        _showMenu.add( _showUnselectedItem );
        _showIncompleteItem = new JCheckBoxMenuItem( "Scheduled" );
        _showIncompleteItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showItemChange();
            }
        });
        _showIncompleteItem.setEnabled( false );
        _showIncompleteItem.setSelected( _settings.queueBrowserSettings().showIncomplete );
        _showMenu.add( _showIncompleteItem );
        _showCompletedItem = new JCheckBoxMenuItem( "Completed" );
        _showCompletedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showItemChange();
            }
        });
        _showCompletedItem.setEnabled( false );
        _showCompletedItem.setSelected( _settings.queueBrowserSettings().showCompleted );
        _showMenu.add( _showCompletedItem );
        _showMenu.add( new JSeparator() );
        _showExperimentScheduledItem = new JCheckBoxMenuItem( "Experiment Schedule Progress" );
        _showExperimentScheduledItem.setEnabled( true );
        _showExperimentScheduledItem.setSelected( _settings.queueBrowserSettings().showExperimentScheduled );
        _showExperimentScheduledItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showExperimentScheduled();
            }
        });
        _showMenu.add( _showExperimentScheduledItem );
        _showPassScheduledItem = new JCheckBoxMenuItem( "Pass Schedule Progress" );
        _showPassScheduledItem.setEnabled( true );
        _showPassScheduledItem.setSelected( _settings.queueBrowserSettings().showPassScheduled );
        _showPassScheduledItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showPassScheduled();
            }
        });
        _showMenu.add( _showPassScheduledItem );
        _showMenu.add( new JSeparator() );
        ZMenuItem expandAllItem = new ZMenuItem( "Expand All" );
        expandAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                expandAll();
            }
        });
        _showMenu.add( expandAllItem );
        ZMenuItem collapseAllItem = new ZMenuItem( "Collapse All" );
        collapseAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                collapseAll();
            }
        });
        _showMenu.add( collapseAllItem );
        _showButton = new JButton( "Show..." );
        _showButton.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showMenu.show( _showButton, 0, 25 );
            }
        });
        this.add( _showButton );
        _guiServerConnectionLight = new ActivityMonitorLight() {
             public JToolTip createToolTip() {
                 ComplexToolTip tip = new ComplexToolTip();
                 tip.setComponent(this);
                 return tip;
             }
             public Point getToolTipLocation( MouseEvent e) {
                return new Point( 10, getHeight() );
             }
        };
        _difxVersionLabel = new JLabel( "" ) {
             public JToolTip createToolTip() {
                 ComplexToolTip tip = new ComplexToolTip();
                 tip.setComponent(this);
                 return tip;
             }
             public Point getToolTipLocation( MouseEvent e) {
                return new Point( 10, getHeight() );
             }
        };
        _difxVersionLabel.setHorizontalAlignment( JLabel.RIGHT );
        this.add( _difxVersionLabel );
        _guiServerConnectionLight.setBounds( 360, 32, 12, 12 );
        _guiServerConnectionLight.alertTime( 0 );
        _guiServerConnectionLight.warningTime( 0 );
        this.add( _guiServerConnectionLight );
        _guiServerConnectionLabel = new JLabel( "guiServer Connection" ) {
             public JToolTip createToolTip() {
                 ComplexToolTip tip = new ComplexToolTip();
                 tip.setComponent(this);
                 return tip;
             }
             public Point getToolTipLocation( MouseEvent e) {
                return new Point( 10, getHeight() );
             }
        };
        _guiServerConnectionLabel.setBounds( 380, 25, 200, 25 );
        this.add( _guiServerConnectionLabel );
        _workingSpinner = new Spinner();
        _workingSpinner.setVisible( false );
        this.add( _workingSpinner );
        _workingLabel = new JLabel( "" );
        _workingLabel.setVisible( false );
        _workingLabel.setHorizontalAlignment( JLabel.RIGHT );
        this.add( _workingLabel );
        
        //  Create a header line of all jobs.
        _header = new JobNodesHeader( _settings );
        _headerPane.addNode( _header );
        
        //  Start a thread that will respond to database update requests.
        _databaseUpdateThread = new DatabaseUpdateThread();
        _databaseUpdateThread.start();

        //  Add a listener for automatic database updates.  When these occur,
        //  we want to update the data for this browser.
        _settings.addDatabaseUpdateListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _triggerDatabaseUpdate = true;
            }
        });
        
        //  Create a queue for the scheduler and then start the scheduler itself.
        _scheduleQueue = new ArrayDeque<JobNode>();
        _scheduleThread = new ScheduleThread();
        _scheduleThread.start();

        //  MEMORY LEAK MONITOR
        //_memoryMonitor = new MemoryMonitor();
        //_memoryMonitor.start();
    }

    public MemoryMonitor _memoryMonitor;
    
    public class MemoryMonitor extends Thread {
        public boolean stop;
        public Instrumentation inst;
        public MemoryMonitor() {
        }
        public void run () {
            while ( !stop ) {
                Runtime rt = Runtime.getRuntime();
                System.out.println( ( (rt.totalMemory() - rt.freeMemory()) / 1024 / 1024 ) + " MB" );
                try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
            }
        }
    }
    
    /*
     * Simple class to display numbers of things on the title bar.
     */
    public class NumLabel extends Object {
        public NumLabel( String name, QueueBrowserPanel panel ) {
            _num = new JLabel( "0" );
            _num.setHorizontalAlignment( JLabel.RIGHT );
            panel.add( _num );
            _label = new JLabel( name );
            _label.setHorizontalAlignment( JLabel.LEFT );
            panel.add( _label );
        }
        public void setBounds( int x, int y, int w, int h ) {
            _num.setBounds( x, y, 40, h );
            _label.setBounds( x + 45, y, w - 45, h );
        }
        public void value( Integer newVal ) {
            _num.setText( newVal.toString() );
        }
        public int value() {
            return Integer.parseInt( _num.getText() );
        }
        protected JLabel _num;
        protected JLabel _label;
    }
    
    /*
     * This method allows me to control resize behavior.  Otherwise I have to
     * leave it up to the layouts, which is a disaster.
     */
    @Override
    public void setBounds(int x, int y, int width, int height) {
        _browserPane.setBounds( 0, 79, width, height - 79 );
        _headerPane.setBounds( 0, 60, width, 20 );
        super.setBounds( x, y, width, height );
        _experimentButton.setBounds( 5, 30, 110, 25 );
        _selectButton.setBounds( 120, 30, 110, 25 );
        _showButton.setBounds( 235, 30, 110, 25 );
        _difxVersionLabel.setBounds( width - 510, 30, 280, 25 );
        _guiServerConnectionLight.setBounds( width - 160, 37, 12, 12 );
        _guiServerConnectionLabel.setBounds( width - 140, 30, 130, 25 );
        _workingSpinner.setBounds( width - 210, 32, 21, 21 );
        _workingLabel.setBounds( width - 420, 30, 200, 25 );
    }
    
    public ActivityMonitorLight guiServerConnectionLight() {
        return _guiServerConnectionLight;
    }
    
    /*
     * Control the "status" of the guiServer connection light.  The status is
     * used in the tooltip - the text of the status is given one of a few choices
     * of color.
     */
    public void guiServerConnectionStatus( String status, String color ) {
        String newTooltip = 
                  "Indicates the status of the connection to <<italic>>guiServer<</italic>>\n"
                + "on the difx host.\n"
                + "<<fixed>>"
                + "<<bold>>  host:<</bold>>" + _settings.difxControlAddress() + "\n"
                + "<<bold>>  port:<</bold>>" + _settings.difxControlPort() + "\n"
                + "<<bold>>status:<</bold>> ";
        if ( color == null )
            newTooltip += status;
        else if ( color.equalsIgnoreCase( "green" ) )
            newTooltip += "<<green>>" + status + "<</color>>";
        else if ( color.equalsIgnoreCase( "red" ) )
            newTooltip += "<<red>>" + status + "<</color>>";
        else if ( color.equalsIgnoreCase( "yellow" ) )
            newTooltip += "<<yellow>>" + status + "<</color>>";
        else
            newTooltip += status;
        newTooltip += "\n<</sans>>See <<blue>><<underline>><<link=" + _settings.guiBrowsePath() + "/settings_content.html#DiFX_Control_Connection>>"
                + "DiFX Control Connection<</link>><</underline>><</color>> for more on how\n"
                + "to connect to <<italic>>guiServer<</italic>>";
        _guiServerConnectionLight.setToolTipText( newTooltip );
        _guiServerConnectionLabel.setToolTipText( newTooltip );
    }
    
    /*
     * Control the content of the DiFX Version label.  This produces a tooltip that
     * includes all of the possibilities.
     */
    public void setDifxVersionLabel() {
        String newTooltip = "DiFX Processing will be done using this DiFX version.\n";
        if ( _settings.difxVersion() != null ) {
            _difxVersionLabel.setText( "using DiFX version \"" + _settings.difxVersion() + "\"" );
            _difxVersionLabel.setForeground( Color.BLACK );
            newTooltip += "Available versions include:\n";
            ArrayList<String> list = _settings.availableDifxVersions();
            for ( Iterator<String> iter = list.iterator(); iter.hasNext(); )
                newTooltip += "<<bold>>" + iter.next() + "<</bold>>\n";
        }
        else {
            _difxVersionLabel.setText( "DiFX unavailable" );
            _difxVersionLabel.setForeground( Color.RED );
            newTooltip += "<<red>>No valid version of DiFX is currently available.<</red>>\n";
        }
        newTooltip += "\nSee <<blue>><<underline>><<link=" + _settings.guiBrowsePath() + "/settings_content.html>>"
                + "DiFX Control Connection<</link>><</underline>><</color>> for more information.";
        _difxVersionLabel.setToolTipText( newTooltip );
    }

    /*
     * Request callbacks from the DiFX Message Processor for message types we are
     * interested in.
     */
    public void difxMessageProcessor( DiFXMessageProcessor processor ) {
        processor.addDifxStatusMessageListener(new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processDifxStatusMessage( difxMsg );
            }
        } );
        processor.addDifxAlertMessageListener(new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processDifxAlertMessage( difxMsg );
            }
        } );
        processor.addDifxDiagnosticMessageListener(new AttributedMessageListener() {
            @Override
            public void update( DifxMessage difxMsg ) {
                processDifxDiagnosticMessage( difxMsg );
            }
        } );
    }
    
    protected class KeyEventListener extends KeyAdapter {    
        public void keyPressed( KeyEvent e ) {
            System.out.println( "push " + e.getKeyCode() );
        }    
        public void keyReleased( KeyEvent e ) {
            System.out.println( "release " + e.getKeyCode() );
        }
    }
    
    /*
     * Count the number of experiments, passes and jobs in the current list.  The
     * counts are put in labels on the title bar.
     */
    public void countListed() {
        int experimentCount = 0;
        int passCount = 0;
        int jobCount = 0;
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> iter = _browserPane.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)(iter.next());
                ++experimentCount;
                for ( Iterator<BrowserNode> pIter = thisExperiment.childrenIterator(); pIter.hasNext(); ) {
                    PassNode thisPass = (PassNode)(pIter.next());
                    ++passCount;
                    for ( Iterator<BrowserNode> jIter = thisPass.childrenIterator(); jIter.hasNext(); )
                        ++jobCount;
                }
            }
        }
        _numExperiments.value( experimentCount );
        _numPasses.value( passCount );
        _numJobs.value( jobCount );
    }

    /*
     * Add a new experiment to the browser.
     */
    public void addExperiment( ExperimentNode newExperiment ) {
        synchronized ( _browserPane ) {
            _browserPane.addNode( newExperiment );
        }
    }
    
    /*
     * Add a new job to the header.  This is so changes in header column widths
     * apply to the job.
     */
    public void addJob( JobNode newJob ) {
        _header.addJob( newJob );
        //  Increment the counters in the headers for the pass and experiment for this job.
        //  This may not keep it 100% accurate, but it should make it more accurate, and
        //  more visually pleasing, if that's worth anything.
        newJob.passNode().addJobs( 1 );
        newJob.passNode().displayNow();
        newJob.passNode().experimentNode().addJobs( 1 ); 
        newJob.passNode().experimentNode().displayNow();
        _browserPane.setBounds( _browserPane.getX(), _browserPane.getY(), _browserPane.getWidth(), _browserPane.getHeight() );
        _browserPane.updateUI();
    }
    
    /*
     * Allow the user to produce a new experiment by bringing up the Experiment
     * Editor.
     */
    protected void newExperiment() {
        //  If the user is currently using the data base, try to connect to it.
        //  Failure, or no attept to connect, will leave "db" as null, indicating
        //  we should try creating an experiment without using the data base.
        QueueDBConnection db = null;
        if ( _settings.useDatabase() ) {
            db = new QueueDBConnection( _settings );
            if ( !db.connected() )
                db = null;
        }

        //  Generate an ID number, which is also used to generate an initial name.
        //  We do this using the database if possible, or by looking at other
        //  experiments if not.
        Integer newExperimentId = 1;
        if ( db != null ) {
            //  Scan the database for the highest experiment ID in existence.  Then
            //  assume the data base software will assign an ID number for this experiment
            //  that is 1 higher than this number.
            ResultSet dbExperimentList = db.experimentList();
            try {
                //  Parse out the ID numbers.  We don't care about the names.
                while ( dbExperimentList.next() ) {
                    int newId = dbExperimentList.getInt( "id" );
                    if ( newId >= newExperimentId )
                        newExperimentId = newId + 1;
                }
            } catch ( Exception e ) {
                    java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, null, e );
            }
        }
        else {
            //  Look at all existing experiments that are NOT in the database and
            //  get their ID numbers.  Find the largest and increment by 1.
            synchronized ( _browserPane ) {
                BrowserNode experimentList = _browserPane.browserTopNode();
                for ( Iterator<BrowserNode> iter = experimentList.childrenIterator(); iter.hasNext(); ) {
                    ExperimentNode thisExperiment = (ExperimentNode)(iter.next());
                    if ( thisExperiment.id() != null && thisExperiment.id() >= newExperimentId )
                        newExperimentId = thisExperiment.id() + 1;
                }
            }
        }
        
        //  Open a window where the user can specify details of the new experiment.
        Point pt = _experimentButton.getLocationOnScreen();
        ExperimentEditor win =
                new ExperimentEditor( pt.x + 25, pt.y + 25, _settings );
        win.setTitle( "Create New Experiment" );
        win.number( 0 );
        win.name( "Experiment_" + newExperimentId.toString() );
        win.id( newExperimentId );
        if ( db != null )
            win.inDataBase( true );
        else
            win.inDataBase( false );
        String creationDate = (new SimpleDateFormat( "yyyy-mm-dd HH:mm:ss" )).format( new Date() );
        win.created( creationDate );
        win.status( "unknown" );
        win.directory( _settings.workingDirectory() + "/" + win.name() );
        win.vexFileName( win.name() + ".vex" );
        win.addVexFileName( "one" );
        win.addVexFileName( "two" );
        win.addVexFileName( "three" );
        win.keepDirectory( false );
        win.passName( "Production Pass" );
        win.createPass( _settings.defaultNames().createPassOnExperimentCreation );
        win.newExperimentMode( true );
        win.setVisible( true );
    }
    
    /*
     * Select all jobs the queue browser knows about.
     */
    protected void selectAll() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                thisExperiment.selectAllJobsAction();
            }
        }
    }
    
    /*
     * Select all jobs that don't have "Done" as their state.
     */
    protected void selectIncomplete() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                thisExperiment.selectIncomplete();
            }
        }
    }
    
    /*
     * Clear the selection of all jobs.
     */
    protected void unselectAll() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                thisExperiment.unselectAllJobsAction();
            }
        }
    }

    /*
     * Delete the selected jobs.  
     */
    protected void deleteSelected() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                thisExperiment.deleteSelectedAction();
            }
        }
    }
    
    /*
     * Select or deselect a group of jobs between the two given (including the two given).  The
     * given jobs are not necessarily in order (i.e. "end" can be before "start").
     */
    public void groupSelection( JobNode job1, JobNode job2, boolean select ) {
        if ( job1 == null || job2 == null )
            return;
        //  First locate the start and end job (we need to make sure we can find both!).
        JobNode start = null;
        JobNode end = null;
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator(); 
                    projectIter.hasNext() && end == null; ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                for ( Iterator<BrowserNode> passIter = thisExperiment.childrenIterator(); 
                        passIter.hasNext() && end == null; ) {
                    PassNode thisPass = (PassNode)passIter.next();
                    for ( Iterator<BrowserNode> jobIter = thisPass.childrenIterator(); 
                            jobIter.hasNext() && end == null; ) {
                        JobNode thisJob = (JobNode)jobIter.next();
                        //  Look first for the start...
                        if ( start == null ) {
                            if ( job1.inputFile().contentEquals( thisJob.inputFile() ) || 
                                    job2.inputFile().contentEquals( thisJob.inputFile() ) ) {
                                start = thisJob;
                            }
                        }
                        //  Then look for the end.  This might be the same job as the start!
                        if ( start != null ) {
                            if ( ( job1 != start && job1.inputFile().contentEquals( thisJob.inputFile() ) ) ||
                                    ( job2 != start && job2.inputFile().contentEquals( thisJob.inputFile() ) ) ) {
                                end = thisJob;
                            }
                        }
                    }
                }
            }
        }
        //  If both have been found, run through the list and select them and everything
        //  in between them.
        if ( start != null && end != null ) {
            boolean done = false;
            boolean started = false;
            synchronized ( _browserPane ) {
                for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator(); 
                        projectIter.hasNext() && !done; ) {
                    ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                    for ( Iterator<BrowserNode> passIter = thisExperiment.childrenIterator(); 
                            passIter.hasNext() && !done; ) {
                        PassNode thisPass = (PassNode)passIter.next();
                        for ( Iterator<BrowserNode> jobIter = thisPass.childrenIterator(); 
                                jobIter.hasNext() && !done; ) {
                            JobNode thisJob = (JobNode)jobIter.next();
                            //  Look first for the start...
                            if ( !started && start == thisJob ) {
                                started = true;
                            }
                            //  Select/deselect the job if we've started.
                            if ( started ) {
                                thisJob.selected( select );
                                //  Then look for the end.
                                if ( end == thisJob )
                                    done = true;
                            }
                        }
                    }
                }
            }
        }
    }
    
    protected void showItemChange() {};
    
    /*
     * Open all experiments and passes so all jobs are visible.
     */
    protected void expandAll() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                thisExperiment.open( true );
                if ( thisExperiment.children().size() > 0 ) {
                    for ( Iterator<BrowserNode> iter = thisExperiment.childrenIterator(); iter.hasNext(); ) {
                        PassNode thisPass = (PassNode)(iter.next());
                        thisPass.open( true );
                    }
                }
            }
        }
    }
    
    /*
     * Close all experiments so all you see is a list of experiments.  This doesn't
     * bother with the passes.
     */
    protected void collapseAll() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                thisExperiment.open( false );
            }
        }
    }
                
    /*
     * Schedule all selected jobs to be run.
     */
    protected void runSelected( boolean configOnly ) {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                if ( thisExperiment.children().size() > 0 ) {
                    for ( Iterator<BrowserNode> iter = thisExperiment.childrenIterator(); iter.hasNext(); ) {
                        PassNode thisPass = (PassNode)(iter.next());
                        for ( Iterator<BrowserNode> jobIter = thisPass.children().iterator(); jobIter.hasNext(); ) {
                            JobNode thisJob = (JobNode)jobIter.next();
                            if ( thisJob.selected() )
                                thisJob.autoStartJob( configOnly );
                        }
                    }
                }
            }
        }
    }
    
    /*
     * Schedule all jobs to be run whether they are selected or not.
     */
    protected void scheduleAll( boolean configOnly ) {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                if ( thisExperiment.children().size() > 0 ) {
                    for ( Iterator<BrowserNode> iter = thisExperiment.childrenIterator(); iter.hasNext(); ) {
                        PassNode thisPass = (PassNode)(iter.next());
                        for ( Iterator<BrowserNode> jobIter = thisPass.children().iterator(); jobIter.hasNext(); ) {
                            JobNode thisJob = (JobNode)jobIter.next();
                            thisJob.autoStartJob( configOnly );
                        }
                    }
                }
            }
        }
    }
    
    /*
     * Schedule all jobs to be run if they are not currently "Done".
     */
    protected void scheduleIncomplete( boolean configOnly ) {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                if ( thisExperiment.children().size() > 0 ) {
                    for ( Iterator<BrowserNode> iter = thisExperiment.childrenIterator(); iter.hasNext(); ) {
                        PassNode thisPass = (PassNode)(iter.next());
                        for ( Iterator<BrowserNode> jobIter = thisPass.children().iterator(); jobIter.hasNext(); ) {
                            JobNode thisJob = (JobNode)jobIter.next();
                            if ( !thisJob.state().getText().contentEquals( "Done" ) )
                                thisJob.autoStartJob( configOnly );
                        }
                    }
                }
            }
        }
    }
    
    /*
     * Set all experiments to show/not show scheduling information.
     */
    protected void showExperimentScheduled() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                thisExperiment.statsVisible( _showExperimentScheduledItem.isSelected() );
            }
        }
        _settings.queueBrowserSettings().showExperimentScheduled = _showExperimentScheduledItem.isSelected();
    }
    
    /*
     * Show (or not) the scheduling information for each pass.
     */
    protected void showPassScheduled() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                if ( thisExperiment.children().size() > 0 ) {
                    for ( Iterator<BrowserNode> iter = thisExperiment.childrenIterator(); iter.hasNext(); ) {
                        PassNode thisPass = (PassNode)(iter.next());
                        thisPass.statsVisible( _showPassScheduledItem.isSelected() );
                    }
                }
            }
        }
        _settings.queueBrowserSettings().showPassScheduled = _showPassScheduledItem.isSelected();
    }
    
    /*
     * Remove selected jobs from the schedule.
     */
    protected void unscheduleSelected() {
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                projectIter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)projectIter.next();
                if ( thisExperiment.children().size() > 0 ) {
                    for ( Iterator<BrowserNode> iter = thisExperiment.childrenIterator(); iter.hasNext(); ) {
                        PassNode thisPass = (PassNode)(iter.next());
                        for ( Iterator<BrowserNode> jobIter = thisPass.children().iterator(); jobIter.hasNext(); ) {
                            JobNode thisJob = (JobNode)jobIter.next();
                            if ( thisJob.selected() )
                                thisJob.unschedule();
                        }
                    }
                }
            }
        }
    }
    
    /*
     * This thread is used to update the queue from the data base.  It runs continuously
     * waiting for a request to update.  It will then trigger the update and wait for
     * the next request.  This needs to be in a thread because database updates are
     * slow.
     */
    public class DatabaseUpdateThread extends Thread {
        
        public void run() {
            //  Endless loop with a .1 second interval.  This is enough to make responses
            //  to user requests look "instantaneous".
            boolean keepGoing = true;
            while ( keepGoing ) {
                if ( _triggerDatabaseUpdate ) {
                    _workingLabel.setText( "updating from database" );
                    _workingLabel.setVisible( true );
                    _workingSpinner.setVisible( true );
                    _workingSpinner.ok();
                    //try { Thread.sleep( 10000 ); } catch ( Exception e ) { keepGoing = false; }
                    updateQueueFromDatabase();
                    _triggerDatabaseUpdate = false;
                    _workingLabel.setVisible( false );
                    _workingSpinner.setVisible( false );
                }                
                try { Thread.sleep( 100 ); } catch ( Exception e ) { keepGoing = false; }
            }
        }
        
    }
    
    /*
     * Update our list of experiments, passes, and nodes from the database.  This
     * pulls everything off the database and uses it to change our current list.  Things
     * which are in the current list that are NOT found in the database are eliminated.
     */
    void updateQueueFromDatabase() {
        
        //  Don't do this if the user isn't using the database.
        if ( !_settings.useDatabase() )
            return;
        
        //  Get a new connection to the database.  Bail out if this doesn't work.
        QueueDBConnection db = new QueueDBConnection( _settings );
        if ( !db.connected() )
            return;
        
        //  Get lists of all experiments, passes, and jobs in the database.
        ResultSet dbExperimentList = db.experimentList();
        ResultSet dbPassList = db.passList();
        ResultSet dbJobList = db.jobList();
        ResultSet dbPassTypeList = db.passTypeList();
        ResultSet dbJobStatusList = db.jobStatusList();
        ResultSet dbSlotList = db.slotList();
        ResultSet dbModule = db.moduleList();
        ResultSet dbExperimentAndModule = db.experimentAndModuleList();
        ResultSet dbExperimentStatus = db.experimentStatusList();
        
        //  We need to track the addition and deletion of items in the data base.
        //  To make this possible, we set a "found" flag to false in each item we
        //  already know about - if we don't "find" any one item again, we'll
        //  remove it.
        BrowserNode experimentList = _browserPane.browserTopNode();
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> iter = experimentList.childrenIterator(); iter.hasNext(); ) {
                QueueBrowserNode thisExperiment = (QueueBrowserNode)(iter.next());
                thisExperiment.found( false );
                //  Within each experiment, flag passes....
                for ( Iterator<BrowserNode> pIter = thisExperiment.childrenIterator(); pIter.hasNext(); ) {
                    QueueBrowserNode thisPass = (QueueBrowserNode)(pIter.next());
                    thisPass.found( false );
                    //  Within each pass, flag each job...
                    for ( Iterator<BrowserNode> jIter = thisPass.childrenIterator(); jIter.hasNext(); )
                        ((QueueBrowserNode)(jIter.next())).found( false );
                }
            }
        }
        
        //  Database operations generate exceptions here and there....
        try {
            //  Run through each experiment, pass and job in the data base and see if we know about
            //  it already in our list.  If we do, set the "found" flag.  If we don't,
            //  add it to the list.
            //
            //  ======== EXPERIMENTS =========
            //
            while ( dbExperimentList.next() ) {
                String name = dbExperimentList.getString( "code" );
                Integer id = dbExperimentList.getInt( "id" );
                Integer number = dbExperimentList.getInt( "number" );
                Integer status = dbExperimentList.getInt( "statusID" );
                String dateCreated = dbExperimentList.getString( "dateCreated" );
                String directory = dbExperimentList.getString( "directory" );
                //  Find a match in our experiment list.
                ExperimentNode thisExperiment = null;
                synchronized ( _browserPane ) {
                    experimentList = _browserPane.browserTopNode();
                    for ( Iterator<BrowserNode> iter = experimentList.childrenIterator(); iter.hasNext(); ) {
                        ExperimentNode testExperiment = (ExperimentNode)(iter.next());
                        //  We should be able to use the ID to match experiments, as it is
                        //  supposed to be unique.
                        if ( testExperiment.inDatabase() && testExperiment.idMatch( id ) )
                            thisExperiment = testExperiment;
                    }
                }
                //  Create a new experiment if we didn't find the named one.
                if ( thisExperiment == null ) {
                    thisExperiment = new ExperimentNode( name, _settings );
                    thisExperiment.id( id );
                    thisExperiment.inDatabase( true );
                    thisExperiment.creationDate( dateCreated );
                    thisExperiment.directory( directory );
                    thisExperiment.statsVisible( _showPassScheduledItem.isSelected() );
                    //thisExperiment.segment( segment );
                    _browserPane.addNode( thisExperiment );
                }
                //  Flag the experiment as "found" so we don't eliminate it.
                thisExperiment.found( true );
            }
            //
            //  ======== PASSES =========
            //
            while ( dbPassList.next() ) {
                String name = dbPassList.getString( "passName" );
                Integer id = dbPassList.getInt( "id" );
                Integer experimentId = dbPassList.getInt( "experimentID" );
                Integer passTypeID = dbPassList.getInt( "passTypeID" );
                String passType = null;
                dbPassTypeList.beforeFirst();
                while ( dbPassTypeList.next() )
                    if ( passTypeID == dbPassTypeList.getInt( "id" ) )
                        passType = dbPassTypeList.getString( "type" );
                PassNode thisPass = null;
                ExperimentNode thisExperiment = null;
                synchronized ( _browserPane ) {
                    experimentList = _browserPane.browserTopNode();
                    for ( Iterator<BrowserNode> iter = experimentList.childrenIterator(); iter.hasNext(); ) {
                        ExperimentNode testExperiment = (ExperimentNode)(iter.next());
                        //  Match the experiment ID.
                        if ( testExperiment.idMatch( experimentId ) ) {
                            thisExperiment = testExperiment;
                            //  Then find the pass in the experiment.
                            for ( Iterator<BrowserNode> pIter = testExperiment.childrenIterator(); pIter.hasNext(); ) {
                                PassNode testPass = (PassNode)(pIter.next());
                                //  Match the pass ID.
                                if ( id.intValue() == testPass.id().intValue() )
                                    thisPass = testPass;
                            }
                        }
                    }
                }
                //  If this pass wasn't encountered in the list of experiments, see
                //  if it is floating outside the experiment list or if it needs to
                //  be added somewhere.
                if ( thisPass == null ) {
                    //  Was the experiment for this pass identified at least?
                    if ( thisExperiment != null ) {
                        //  Okay, it must be a new pass in the experiment - add it.
                        thisPass = new PassNode( name, _settings );
                        thisPass.type( passType );
                        thisPass.id( id );
                        thisPass.inDatabase( true );
                        thisPass.experimentNode( thisExperiment );
                        synchronized ( _browserPane ) {
                            thisExperiment.addChild( thisPass );  
                        }
                        thisPass.found( true );
                        thisPass.statsVisible( _showPassScheduledItem.isSelected() );
                    }
                    else {
                        //  TODO:  accomodate passes outside of the experiment structure
                        //  Right.  This is a "floating" pass outside the experiment
                        //  list.  See if it already exists in our list of such
                        //  things.
                        
                        //  It wasn't found, so add it.
                    }
                }
            }
            //
            //  ======== JOBS =========
            //
            while ( dbJobList.next() ) {
                Integer id = dbJobList.getInt( "id" );
                Integer passId = dbJobList.getInt( "passID" );
                //  Locate this job within the proper pass.  Both are identified
                //  by ID's, so screwed up ID's will likely kill us.
                PassNode thisPass = null;
                JobNode thisJob = null;
                ExperimentNode thisExperiment = null;
                synchronized ( _browserPane ) {
                    experimentList = _browserPane.browserTopNode();
                    for ( Iterator<BrowserNode> iter = experimentList.childrenIterator(); iter.hasNext() && thisJob == null; ) {
                        ExperimentNode testExperiment = (ExperimentNode)(iter.next());
                        for ( Iterator<BrowserNode> pIter = testExperiment.childrenIterator(); pIter.hasNext() && thisJob == null && thisPass == null; ) {
                            PassNode testPass = (PassNode)(pIter.next());
                            if ( passId.equals( testPass.id() ) ) {
                                thisPass = testPass;
                                thisExperiment = testExperiment;
                                for ( Iterator<BrowserNode> jIter = thisPass.childrenIterator(); jIter.hasNext() && thisJob == null; ) {
                                    JobNode testJob = (JobNode)(jIter.next());
                                    if ( id.intValue() == testJob.id().intValue() ) {
                                        thisJob = testJob;
                                        thisJob.found( true );
                                    }
                                }
                            }
                        }
                    }
                }
                //  Add the job if we haven't found it.
                if ( thisJob == null ) {
                    //  Did we find the pass?
                    if ( thisPass != null ) {
                        //  Generate a job name, either from the input file if that
                        //  works or from the pass name and job number.
                        File tryFile = new File( dbJobList.getString( "inputFile" ) );
                        Integer jobNumber = dbJobList.getInt( "jobNumber" );
                        String jobName = null;
                        if ( tryFile != null && tryFile.getName() != null && tryFile.getName().lastIndexOf( "." ) > 0 ) {
                            jobName = tryFile.getName().substring( 0, tryFile.getName().lastIndexOf( "." ) );
                        }
                        if ( jobName == null )
                            jobName = thisPass.name() + "_" + jobNumber.toString();
                        thisJob = new JobNode( jobName, _settings );
                        thisJob.id( id );
                        thisJob.inDatabase( true );
                        thisJob.experiment( thisExperiment.name() );
                        thisJob.pass( thisPass.name() );
                        thisJob.passNode( thisPass );
                        synchronized ( _browserPane ) {
                            thisPass.addChild( thisJob );
                        }
                        _header.addJob( thisJob ); 
                    }
                    else {
                        //  Floating job - figure out what to do with this, if anything.
                    }
                }
                //  Fill in all information about the job if it was found (or newly created).
                //  This will update job settings with anything that might have changed
                //  in the database.
                if ( thisJob != null ) {
                    thisJob.found( true );
                    //  Setting the input file name triggers reading of it, which sets many of
                    //  the items we set below.  Hopefully these settings are the same...but if
                    //  not, should the stuff in the database dominate (as here), or should the
                    //  stuff in the input file be used?  Not sure.
                    thisJob.inputFile( dbJobList.getString( "inputFile" ), false );
                    thisJob.priority( dbJobList.getInt("priority") );
                    thisJob.queueTime( dbJobList.getString( "queueTime" ) );
                    thisJob.correlationStart( dbJobList.getString( "correlationStart" ) );
                    thisJob.correlationEnd( dbJobList.getString( "correlationEnd" ) );
                    thisJob.jobStart( dbJobList.getDouble( "jobStart" ) );
                    thisJob.jobDuration( dbJobList.getDouble( "jobDuration" ) ); 
                    thisJob.outputFile( dbJobList.getString( "outputFile" ) );
                    thisJob.outputSize( dbJobList.getInt( "outputSize" ) );
                    thisJob.difxVersion( dbJobList.getString( "difxVersion" ) );
                    thisJob.speedUpFactor( dbJobList.getDouble( "speedupFactor" ) );
                    thisJob.numAntennas( dbJobList.getInt( "numAntennas" ) );
                    thisJob.numForeignAntennas( dbJobList.getInt( "numForeign" ) );
                    thisJob.dutyCycle( dbJobList.getDouble( "dutyCycle" ) );
                    thisJob.status( "unknown" );
                    thisJob.active( false );
                    thisJob.statusId( dbJobList.getInt( "statusID" ) );
                    Integer statusIdInt = dbJobList.getInt( "statusID" );
                    dbJobStatusList.beforeFirst();
                    if ( dbJobStatusList.next() ) {
                        thisJob.status( dbJobStatusList.getString( "status" ) );
                        thisJob.active( dbJobStatusList.getBoolean( "active" ) );
                    }
                    //  If the job has a log file, parse it to find the run state.
                    //thisJob.parseLogFile();
                }
            }
        } catch ( Exception e ) {
            System.out.println( e );
            e.printStackTrace();
        }

        //  Eliminate any items we have failed to find in the data base,
        //  with the exception of those that aren't actually in the data base.
        synchronized ( _browserPane ) {
            experimentList = _browserPane.browserTopNode();
            for ( Iterator<BrowserNode> iter = experimentList.childrenIterator(); iter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)(iter.next());
                if ( !thisExperiment.found() && thisExperiment.inDatabase() )
                    _browserPane.browserTopNode().removeChild( thisExperiment );
                else {
                    //  Eliminate passes under each experiment...
                    for ( Iterator<BrowserNode> pIter = thisExperiment.childrenIterator(); pIter.hasNext(); ) {
                        PassNode thisPass = (PassNode)(pIter.next());
                        if ( !thisPass.found() && thisPass.inDatabase() )
                            thisExperiment.removeChild( thisPass );
                        else {
                            //  Eliminate jobs under each pass.
                            for ( Iterator<BrowserNode> jIter = thisPass.childrenIterator(); jIter.hasNext(); ) {
                                JobNode thisJob = (JobNode)(jIter.next());
                                if ( !thisJob.found() && thisJob.inDatabase() ) {
                                    thisPass.removeChild( thisJob );
                                }
                            }
                        }
                    }
                }
            }
        }
       
    }
    
    /*
     * Show a window that allows the user to define what experiments/jobs/whatever
     * are scanned for on disk.
     */
    public void setDiskSearchRules() {
        if ( _diskSearchRules == null ) {
            _diskSearchRules = new DiskSearchRules( MouseInfo.getPointerInfo().getLocation().x, 
                        MouseInfo.getPointerInfo().getLocation().y );
        }
        _diskSearchRules.setVisible( true );
    }
    
    /*
     * This class contains a window that allows the user to set a series of rules
     * for searching for experiments/jobs/etc on disk.
     */
    public class DiskSearchRules extends JFrame {

        public DiskSearchRules( int x, int y ) {
            _this = this;
            //_settings.setLookAndFeel();
            this.setLayout( null );
            this.setBounds( x, y, _settings.windowConfiguration().diskSearchRulesDisplayW,
                _settings.windowConfiguration().diskSearchRulesDisplayH );
            this.getContentPane().setLayout( null );
            this.setTitle( "Locate Experiments on Disk" );
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentResized( ComponentEvent e ) {
                    _settings.windowConfiguration().diskSearchRulesDisplayW = _this.getWidth();
                    _settings.windowConfiguration().diskSearchRulesDisplayH = _this.getHeight();
                    newSize();
                }
            });
            this.addComponentListener( new java.awt.event.ComponentAdapter() {
                public void componentShown( ComponentEvent e ) {
                    newSize();
                }
            });
            _menuBar = new JMenuBar();
            _this.add( _menuBar );
            JMenu helpMenu = new JMenu( "  Help  " );
            _menuBar.add( helpMenu );
            ZMenuItem settingsHelpItem = new ZMenuItem( "Disk Search Help" );
            settingsHelpItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _settings.launchGUIHelp( "Queue_Browser_content.html#LOCATE_ON_DISK" );
                }
            } );
            helpMenu.add( settingsHelpItem );
            ZMenuItem helpIndexItem = new ZMenuItem( "GUI Documentation" );
            helpIndexItem.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _settings.launchGUIHelp( "intro.html" );
                }
            } );
            helpMenu.add( helpIndexItem );

            _fileFilter = new TabCompletedTextField( _settings );
            _fileFilter.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    updateList();
                    _settings.jobLocationDefaults().fileFilter = _fileFilter.getText();
                }
            } );
            _this.add( _fileFilter );
            JLabel fileFilterLabel = new JLabel( "Locate .input File(s) Matching:" );
            fileFilterLabel.setBounds( 10, 30, 400, 25 );
            _this.add( fileFilterLabel );
            JLabel experimentNameLabel = new JLabel( "Experiment Name(s):" );
            experimentNameLabel.setBounds( 10, 80, 400, 25 );
            _this.add( experimentNameLabel );
            _experimentBasedOnPath = new JCheckBox( "Based on Path" );
            _experimentBasedOnPath.setToolTipText( "Use the subdirectory that contains the \"pass\" subdirectories (or jobs if no passes are used) as the experiment name." );
            _experimentBasedOnPath.setBounds( 10, 105, 200, 25 );
            _experimentBasedOnPath.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    checkExperimentBase( _experimentBasedOnPath );
                    synchronizedTranslateList();
                }
            } );
            _this.add( _experimentBasedOnPath );
            _experimentNamed = new JCheckBox( "" );
            _experimentNamed.setToolTipText( "Assign a specific name to the experiment (which will contain all jobs)." );
            _experimentNamed.setBounds( 220, 105, 25, 25 );
            _experimentNamed.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    checkExperimentBase( _experimentNamed );
                    synchronizedTranslateList();
                }
            } );
            _this.add( _experimentNamed );
            _experimentName = new SaneTextField();
            _experimentName.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    _settings.jobLocationDefaults().experimentName = _experimentName.getText();
                    synchronizedTranslateList();
                }
            } );
            _this.add( _experimentName );
            JLabel passNameLabel = new JLabel( "Pass Name(s):" );
            passNameLabel.setBounds( 10, 140, 400, 25 );
            _this.add( passNameLabel );
            _passBasedOnPath = new JCheckBox( "Based On Path" );
            _passBasedOnPath.setToolTipText( "Use the subdirectory name that contains any .input files as the pass name." );
            _passBasedOnPath.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    checkPassBase( _passBasedOnPath );
                    synchronizedTranslateList();
                }
            } );
            _passBasedOnPath.setBounds( 10, 165, 120, 25 );
            _this.add( _passBasedOnPath );
            _noPass = new JCheckBox( "None" );
            _noPass.setToolTipText( "Do not include a pass." );
            _noPass.setBounds( 130, 165, 70, 25 );
            _noPass.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    checkPassBase( _noPass );
                    synchronizedTranslateList();
                }
            } );
            _this.add( _noPass );
            _passNamed = new JCheckBox( "" );
            _passNamed.setToolTipText( "Assign a specific name to the pass (which will contain all jobs)." );
            _passNamed.setBounds( 220, 165, 25, 25 );
            _passNamed.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    checkPassBase( _passNamed );
                    synchronizedTranslateList();
                }
            } );
            _this.add( _passNamed );
            _passName = new SaneTextField();
            _passName.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    _settings.jobLocationDefaults().passName = _passName.getText();
                    synchronizedTranslateList();
                }
            } );
            _this.add( _passName );
            JLabel previewLabel = new JLabel( "Preview" );
            previewLabel.setBounds( 10, 200, 200, 25 );
            _this.add( previewLabel );
            _previewSpinner = new Spinner();
            _previewSpinner.setVisible( false );
            _this.add( _previewSpinner );
            _spinnerLabel = new JLabel( "updating" );
            _spinnerLabel.setHorizontalTextPosition( JLabel.RIGHT );
            _spinnerLabel.setVisible( false );
            _this.add( _spinnerLabel );
            _updateButton = new JButton( "Update Now" );
            _updateButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    updateList();
                }
            } );
            _this.add( _updateButton );
//            _expandAllButton = new JButton( "Expand All" );
//            _expandAllButton.addActionListener( new ActionListener() {
//                public void actionPerformed( ActionEvent evt ) {
//                    expandAll();
//                }
//            } );
//            _this.add( _expandAllButton ) ;
//            _collapseAllButton = new JButton( "Collapse All" );
//            _collapseAllButton.addActionListener( new ActionListener() {
//                public void actionPerformed( ActionEvent evt ) {
//                    collapseAll();
//                }
//            } );
//            _this.add( _collapseAllButton ) ;
            _selectIncompleteButton = new JButton( "Select Incomplete" );
            _selectIncompleteButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    selectIncompleteAction();
                }
            } );
            _this.add( _selectIncompleteButton ) ;
            _selectAllButton = new JButton( "Select All" );
            _selectAllButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    selectAll();
                }
            } );
            _this.add( _selectAllButton ) ;
            _deselectAllButton = new JButton( "Deselect All" );
            _deselectAllButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    deselectAll();
                }
            } );
            _this.add( _deselectAllButton ) ;
            _preview = new NodeBrowserScrollPane();
            _preview.setBackground( Color.WHITE );
            _this.add( _preview );
            _autoUpdate = new JCheckBox( "Auto Update" );
            _autoUpdate.setToolTipText( "Periodically re-run this search to find new experiments that are created in real time." );
            _autoUpdate.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    _settings.jobLocationDefaults().autoUpdate = _autoUpdate.isSelected();
                }
            } );
            _this.add( _autoUpdate );
            _applyButton = new JButton( "Apply" );
            _applyButton.setToolTipText( "Download the listed experiments and jobs to the Queue Browser." );
            _applyButton.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    apply();
                }
            } );
            _this.add( _applyButton );
            
            //  Set defaults for everything....
            _fileFilter.setText( _settings.jobLocationDefaults().fileFilter );
            _experimentBasedOnPath.setSelected( _settings.jobLocationDefaults().experimentBasedOnPath );
            _experimentNamed.setSelected( _settings.jobLocationDefaults().experimentNamed );
            _experimentName.setText( _settings.jobLocationDefaults().experimentName );
            _passBasedOnPath.setSelected( _settings.jobLocationDefaults().passBasedOnPath );
            _passNamed.setSelected( _settings.jobLocationDefaults().passNamed );
            _noPass.setSelected( _settings.jobLocationDefaults().noPass );
            _passName.setText( _settings.jobLocationDefaults().passName );
            _autoUpdate.setSelected( _settings.jobLocationDefaults().autoUpdate );
            
            //  De-highlight some stuff if necessary...
            if ( _experimentNamed.isSelected() )
                _experimentName.setEnabled( true );
            else
                _experimentName.setEnabled( false );
            if ( _passNamed.isSelected() )
                _passName.setEnabled( true );
            else
                _passName.setEnabled( false );

            _allObjectsBuilt = true;
            
            //  Update using current settings.
            updateList();
            
            _this.newSize();
            
        }
        
        /*
         * Change checkboxes for the experiment naming (and other things) based on
         * which one was pushed.
         */
        public void checkExperimentBase( JCheckBox pushed ) {
            if ( pushed == _experimentBasedOnPath ) {
                _experimentBasedOnPath.setSelected( true );
                _experimentNamed.setSelected( false );
                _experimentName.setEnabled( false );
            }
            else {
                _experimentBasedOnPath.setSelected( false );
                _experimentNamed.setSelected( true );
                _experimentName.setEnabled( true );
            }
            _settings.jobLocationDefaults().experimentBasedOnPath = _experimentBasedOnPath.isSelected();
            _settings.jobLocationDefaults().experimentNamed = _experimentNamed.isSelected();
        }
        
        /*
         * Change things based on the pass naming choice.
         */
        public void checkPassBase( JCheckBox pushed ) {
            if ( pushed == _passBasedOnPath ) {
                _passBasedOnPath.setSelected( true );
                _noPass.setSelected( false );
                _passNamed.setSelected( false );
                _passName.setEnabled( false );
            }
            else if ( pushed == _noPass ) {
                _passBasedOnPath.setSelected( false );
                _noPass.setSelected( true );
                _passNamed.setSelected( false );
                _passName.setEnabled( false );
            }
            else {
                _passBasedOnPath.setSelected( false );
                _noPass.setSelected( false );
                _passNamed.setSelected( true );
                _passName.setEnabled( true );
            }
            _settings.jobLocationDefaults().passBasedOnPath = _passBasedOnPath.isSelected();
            _settings.jobLocationDefaults().noPass = _noPass.isSelected();
            _settings.jobLocationDefaults().passNamed = _passNamed.isSelected();
        }
        
        @Override
        public void setBounds( int x, int y, int w, int h ) {
            super.setBounds( x, y, w, h );
            newSize();
        }
    
        public void newSize() {
            if ( _allObjectsBuilt ) {
                int w = _this.getContentPane().getSize().width;
                int h = _this.getContentPane().getSize().height;
                _menuBar.setBounds( 0, 0, w, 25 );
                _fileFilter.setBounds( 10, 50, w - 25, 25 );
                _experimentName.setBounds( 250, 105, w - 265, 25 );
                _passName.setBounds( 250, 165, w - 265, 25 );
                _previewSpinner.setBounds( w - 45, 200, 20, 20 );
                _spinnerLabel.setBounds( w - 150, 195, 100, 25 );
                _updateButton.setBounds( w - 135, 195, 120, 25 );
                _preview.setBounds( 10, 225, w - 25, h - 265 );
                _autoUpdate.setBounds( w - 300, h - 30, 160, 25 );
                _applyButton.setBounds( w - 130, h - 30, 115, 25 );
//                _expandAllButton.setBounds( 10, h - 30, 115, 25 );
//                _collapseAllButton.setBounds( 130, h - 30, 115, 25 );
                _selectIncompleteButton.setBounds( 10, h - 30, 150, 25 );
                _selectAllButton.setBounds( 165, h - 30, 150, 25 );
                _deselectAllButton.setBounds( 320, h - 30, 150, 25 );
            }
        }
        
        synchronized public void updateList() {
            //  If the connection to the guiServer doesn't exist, this can't work.
            if ( !_settings.guiServerConnection().connected() ) {
                Component comp = this;
                while ( comp.getParent() != null )
                    comp = comp.getParent();
                JOptionPane.showMessageDialog( (Frame)comp, "GuiServer is not connected.", "ls Error", JOptionPane.WARNING_MESSAGE );
                return;
            }
            //  Erase the "update now" button and give us a "busy" spinnner.  This
            //  will be undone when all is completed.
            _updateButton.setVisible( false );
            _previewSpinner.ok();
            _previewSpinner.setVisible( true );
            //  Give us a "wait" cursor.
            final Cursor cursor = this.getCursor();
            this.setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );
            if ( _newList == null )
                _newList = new ArrayList<String>();
            _newList.clear();
            DiFXCommand_ls ls = null;
            //  Slap *.input on the end of the file filter string if it hasn't been done by the user.
            String searchStr = _fileFilter.getText().trim();
            if ( !searchStr.endsWith( ".input" ) ) {
                searchStr += "*.input";
            }
            //  Get the status of all jobs (by reading .difxlog files).  The "true" flag
            //  indicates that all we want is a quick single-string status.
            getJobStatus( searchStr, true );
            ls = new DiFXCommand_ls( searchStr, _settings );
            //  Set the callback for when the list is complete.  
            ls.addEndListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    //  Found anything at all?
                    if ( _newList.size() > 0 ) {
                        //  We've got a list of files - turn them into experiments/passes/jobs
                        //  based on user rules.
                        translateList();
                    }
                    _this.setCursor( cursor );
                    _previewSpinner.setVisible( false );
                    _updateButton.setVisible( true );

                }
            });
            //  Set the callback for when a new item is added to the list.
            ls.addIncrementalListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _newList.add( e.getActionCommand().trim() );
                }
            });
            try {
                ls.send();
            } catch ( java.net.UnknownHostException e ) {
                //  BLAT handle this
            }
        }
        
        /*
         * Call the translateList() function with a synchronized wrapper - also change
         * the cursor, spinner, update button, etc.
         */
        synchronized public void synchronizedTranslateList() {
            _updateButton.setVisible( false );
            _previewSpinner.ok();
            _previewSpinner.setVisible( true );
            final Cursor cursor = this.getCursor();
            this.setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );
            translateList();
            _this.setCursor( cursor );
            _previewSpinner.setVisible( false );
            _updateButton.setVisible( true );
        }
        
        protected ArrayList<LocalJobNode> _missingSimpleStatus;
        
        /*
         * Do user-specified translation of path names (or whatever) to obtain
         * experiment and pass names for each found input file.  The items that
         * match specifications are displayed in the preview box.
         */
        public void translateList() {
            _preview.clear();
            if ( _missingSimpleStatus == null )
                _missingSimpleStatus = new ArrayList<LocalJobNode>();
            else
                _missingSimpleStatus.clear();
            Iterator<String> iter = _newList.iterator();
            while ( iter.hasNext() ) {
                String nextFile = iter.next();
                String experimentName = null;
                String passName = null;
                String jobName = null;
                String experimentPath = null;
                String passPath = null;
                String vexFile = null;
                String v2dFile = null;
                if ( _experimentNamed.isSelected() )
                    experimentName = _experimentName.getText();
                //  Extract the experiment name from the path if we don't have a defined
                //  experiment name.  Also extract the experiment path.  Might need to do some
                //  checks here to avoid running out of path (if input files are stored
                //  too high in the directory tree).
                if ( nextFile.lastIndexOf( "/" ) > 0 ) {
                    experimentPath = nextFile.substring( 0, nextFile.lastIndexOf( "/" ) );
                    if ( _noPass.isSelected() ) {
                        if ( !_experimentNamed.isSelected() )
                            experimentName = experimentPath.substring( experimentPath.lastIndexOf( "/" ) + 1 );
                    }
                    else {
                        experimentPath = experimentPath.substring( 0, experimentPath.lastIndexOf( "/" ) );
                        if ( !_experimentNamed.isSelected() )
                            experimentName = experimentPath.substring( experimentPath.lastIndexOf( "/" ) + 1 );
                    }
                    //  Then the pass name (if there is one).
                    if ( _passNamed.isSelected() )
                        passName = _passName.getText();
                    else if ( _passBasedOnPath.isSelected() ) {
                        String shortName = nextFile.substring( 0, nextFile.lastIndexOf( "/" ) );
                        passName = shortName.substring( shortName.lastIndexOf( "/" ) + 1 );
                    }
                    else
                        passName = "";
                    //  Get the name of the job from the .input file name.  I'm assuming
                    //  this is accurate...
                    String shortName = nextFile.substring( nextFile.lastIndexOf( "/" ) + 1 );
                    jobName = shortName.substring( 0, shortName.lastIndexOf( "." ) );
                    
                    //  Get the "simple" status of this job.  We hope the status update is done by
                    //  now???
                    String status = null;
                    boolean hasStatus = haveSimpleStatus( nextFile );
                    if ( hasStatus )
                        status = getSimpleStatus( nextFile );

                    //  Create a new job entry and add it to the proper location in the preview browser.
                    //  Create experiments and passes as necessary.
                    LocalJobNode newJob = new LocalJobNode( jobName, nextFile, status );
                    newJob.addSelectionButton( null, null );
                    newJob.selected( true );
                    newJob.xOffset( 20 );

                    //  Save a list of jobs that have missing status.  These will be fixed below.
                    if ( !hasStatus )
                        _missingSimpleStatus.add( newJob );

                    //  Search for the experiment name...
                    BrowserNode experimentList = _preview.browserTopNode();
                    boolean experimentFound = false;
                    BrowserNode thisExperiment = null;
                    for ( Iterator<BrowserNode> iter2 = experimentList.childrenIterator(); 
                          !experimentFound && iter2.hasNext(); ) {
                        thisExperiment = iter2.next();
                        //  Match the name of each experiment with our new experiment...
                        if ( thisExperiment.name().contentEquals( experimentName ) )
                            experimentFound = true;
                    }
                    //  Create a new experiment if we didn't find out current one...
                    if ( !experimentFound ) {
                        thisExperiment = new LocalBrowserNode( experimentName );
                        thisExperiment.addSelectionButton( null, null );
                        thisExperiment.selected( true );
                        thisExperiment.xOffset( 20 );
                        thisExperiment.addCountWhenClosed( true );
                        ((LocalBrowserNode)thisExperiment).path( experimentPath );
                        _preview.addNode( thisExperiment );
                    }
                    //  Then find the pass...if there is one.
                    if ( !_noPass.isSelected() ) {
                        boolean passFound = false;
                        BrowserNode thisPass = null;
                        synchronized ( _browserPane ) {
                            for ( Iterator<BrowserNode> iter3 = thisExperiment.childrenIterator();
                                  !passFound && iter3.hasNext(); ) {
                                thisPass = iter3.next();
                                if ( thisPass.name().contentEquals( passName ) )
                                    passFound = true;
                            }
                        }
                        if ( !passFound ) {
                            thisPass = new LocalBrowserNode( passName );
                            thisPass.addSelectionButton( null, null );
                            thisPass.selected( true );
                            thisPass.xOffset( 20 );
                            thisPass.addCountWhenClosed( true );
                            synchronized ( _browserPane ) {
                                thisExperiment.addChild( thisPass );
                            }
                        }
                        synchronized ( _browserPane ) {
                            thisPass.addChild( newJob );
                        }
                    }
                    else {
                        thisExperiment.addChild( newJob );
                    }
                }
            }
            _preview.setBounds( _preview.getX(), _preview.getY(), _preview.getWidth(), _preview.getHeight() );
            _preview.updateUI();
            _this.newSize();
            //  Launch a repeating thread to try to update the "simple status" of each
            //  job.  This runs forever...um, is this a good idea?
            Thread statusUpdateThread = new Thread() {
                public void run() {
                    while ( _missingSimpleStatus.size() > 0 ) {
                        int n = _missingSimpleStatus.size();
                        try {
                            for ( int i = 0; i < n; ++i ) {
                                LocalJobNode node = _missingSimpleStatus.get(i);
                                if ( haveSimpleStatus( node.inputFile() ) ) {
                                    node.setStatus( getSimpleStatus( node.inputFile() ) );
                                    _missingSimpleStatus.remove(i);
                                }
                            }
                        } catch ( java.lang.IndexOutOfBoundsException ex ) {}
                        try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
                    }
                }
            };
            statusUpdateThread.start();
        }
        
        public class LocalBrowserNode extends BrowserNode {
            LocalBrowserNode( String name ) {
                super( name );
            }
            public String path() { return _path; }
            public void path( String newVal ) { _path = newVal; }
            protected String _path;
        }
        
        public class LocalJobNode extends BrowserNode {
            LocalJobNode( String name, String inputFile, String status ) {
                super( name );
                _inputFile.setText( inputFile );
                setStatus( status );
            }
            public void setStatus( String status ) {
                if ( status != null ) {
                    _status.setText( status );
                    if ( status.contentEquals( "Done" ) )
                        _status.setBackground( Color.GREEN );
                    else {
                        _status.setText( "Run Failed" );
                        _status.setBackground( Color.RED );
                    }
                }
                else {
                    _status.setText( "not Started" );
                    _status.setBackground( Color.LIGHT_GRAY );
                }
            }
            @Override
            public void createAdditionalItems() {
                _inputFile = new JLabel( "" );
                this.add( _inputFile );
                _status = new ColumnTextArea( "" );
                this.add( _status );
            }
            @Override
            public void positionItems() {
                super.positionItems();
                _status.setBounds( 400, 1, 140, _ySize - 2 );
                _inputFile.setBounds( 550, 0, 1000, _ySize );
            }
            public String inputFile() { return _inputFile.getText(); }
            protected JLabel _inputFile;
            public String status() { return _status.getText(); }
            protected ColumnTextArea _status;
            public String vexFile() { return _vexFile; }
            public void vexFile( String newVal ) { _vexFile = newVal; }
            protected String _vexFile;
            public String v2dFile() { return _v2dFile; }
            public void v2dFile( String newVal ) { _v2dFile = newVal; }
            protected String _v2dFile;
        }
        
        /*
         * This is what happens when you hit the "apply" button - all selected objects still
         * in the preview list are added to the queue browser.
         */
        public void apply() {
            //  Do a count of the number of jobs we are retrieving for a progress bar.
            int totalCount = 0;
            for ( Iterator<BrowserNode> iter = _preview.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                BrowserNode thisExperiment = iter.next();
                if ( thisExperiment.selected() ) {
                    for ( Iterator<BrowserNode> iter2 = thisExperiment.childrenIterator(); iter2.hasNext(); ) {
                        BrowserNode thisPass = iter2.next();
                        if ( thisPass.selected() ) {
                            for ( Iterator<BrowserNode> iter3 = thisPass.childrenIterator(); iter3.hasNext(); ) {
                                BrowserNode thisJob = iter3.next();
                                if ( thisJob.selected() )
                                    totalCount += 1;
                            }
                        }
                    }
                }
            }
            final int _totalCount = totalCount;
            //  Then actually to the retrieval.  This is done in a thread to allow the
            //  user to do other things (and to permit cancellations).
            Thread runThread = new Thread() {
                public void run() {
                    _continueRetrieval = true;
                    int count = 0;
                    RetrievalMonitor monitor = new RetrievalMonitor();
                    monitor.progressBar.setMaximum( _totalCount );
                    //  Set up the file reading system to save and reuse any files that it is
                    //  asked to down load more than once.  This saves a lot of time in the
                    //  download process.
                    GetFileMonitor.allowReuse( true );
                    monitor.setVisible( true );
                    for ( Iterator<BrowserNode> iter = _preview.browserTopNode().childrenIterator(); iter.hasNext() && _continueRetrieval; ) {
                        BrowserNode thisExperiment = iter.next();
                        ExperimentNode currentExperiment = null;
                        if ( thisExperiment.selected() ) {
                            for ( Iterator<BrowserNode> iter2 = thisExperiment.childrenIterator(); iter2.hasNext() && _continueRetrieval; ) {
                                BrowserNode thisPass = iter2.next();
                                if ( thisPass.selected() ) {
                                    for ( Iterator<BrowserNode> iter3 = thisPass.childrenIterator(); iter3.hasNext() && _continueRetrieval; ) {
                                        BrowserNode thisJob = iter3.next();
                                        if ( thisJob.selected() ) {
                                            ++count;
                                            monitor.label.setText( "Retrieving \"" 
                                                    + thisExperiment.name() + "/" +
                                                    thisPass.name() + "/" + thisJob.name() + "\" (" +
                                                    count + "/" + _totalCount + ")" );
                                            monitor.progressBar.setValue( count );
                                            currentExperiment = addDefinedJob( thisExperiment.name(), thisPass.name(), thisJob.name(), 
                                                    ((LocalJobNode)thisJob).inputFile(), ((LocalBrowserNode)thisExperiment).path() );
                                        }
                                    }
                                }
                            }
                        }
                        //  Now that we have collected all of the jobs, wait until the .v2d file has been read
                        //  and use it to produce source names.
                        int safecounter = 0;  //  I'm putting 10 second limit on this...
                        while( ( currentExperiment.editor().v2dFileParser() == null || currentExperiment.editor().vexFileParser() == null )
                                && safecounter < 10 ) {
                            try { Thread.sleep( 1000 ); } catch( Exception e ) {}
                            ++safecounter;
                        }
                        if ( currentExperiment.editor().v2dFileParser() != null && currentExperiment.editor().vexFileParser() != null ) {
                            currentExperiment.editor().findScansInJobs();
                            for ( Iterator<BrowserNode> iter2 = currentExperiment.childrenIterator(); iter2.hasNext(); ) {
                                PassNode thisPass = (PassNode)iter2.next();
                                for ( Iterator<BrowserNode> iter3 = thisPass.childrenIterator(); iter3.hasNext(); ) {
                                    JobNode thisJob = (JobNode)iter3.next();
                                    thisJob.setSource( currentExperiment.editor() );
                                    thisJob.setScan( currentExperiment.editor() );
                                }
                            }
                        }
                    }
                    //  Shut off the reuse feature.
                    GetFileMonitor.allowReuse( false );
                    monitor.setVisible( false );
                }
            };
            runThread.start();
            _this.setVisible( false );
        }
        
        //  Monitor window for the retrieval process.
        public class RetrievalMonitor extends JFrame {
            public RetrievalMonitor() {
                super( "Loading Existing Jobs From Disk..." );
                setLayout( null );
                setBounds( _settings.queueBrowser().getX() + _settings.queueBrowser().getWidth() - 500,
                        _settings.queueBrowser().getY(), 500, 200 );
                ZButton cancelButton = new ZButton( "Cancel" );
                cancelButton.setBounds( 350, 125, 125, 25 );
                cancelButton.setToolTipText( "Stop the current job retrieval process." );
                cancelButton.addActionListener( new ActionListener() {
                    public void actionPerformed( ActionEvent e ) {
                        System.out.println( "CANCEL NOW!!!!!" );
                        _continueRetrieval = false;
                    }
                });
                this.add( cancelButton );
                label = new JLabel( "Initializing" );
                label.setBounds( 20, 20, 460, 25 );
                this.add( label );
                progressBar = new JProgressBar();
                progressBar.setBounds( 20, 50, 460, 25 );
                progressBar.setValue( 0 );
                progressBar.setMinimum( 0 );
                this.add( progressBar );
            }
            public JLabel label;
            public JProgressBar progressBar;
        }
        
        public void expandAll() {
            for ( Iterator<BrowserNode> iter = _preview.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                BrowserNode thisExperiment = iter.next();
                thisExperiment.open( true );
                for ( Iterator<BrowserNode> iter2 = thisExperiment.childrenIterator(); iter2.hasNext(); ) {
                    BrowserNode thisPass = iter2.next();
                    thisPass.open( true );
                }
            }
            _preview.listChange();
        }
        
        public void collapseAll() {
            for ( Iterator<BrowserNode> iter = _preview.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                BrowserNode thisExperiment = iter.next();
                thisExperiment.open( false );
                for ( Iterator<BrowserNode> iter2 = thisExperiment.childrenIterator(); iter2.hasNext(); ) {
                    BrowserNode thisPass = iter2.next();
                    thisPass.open( false );
                }
            }
            _preview.listChange();
        }
        
        public void selectIncompleteAction() {
            for ( Iterator<BrowserNode> iter = _preview.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                BrowserNode thisExperiment = iter.next();
                boolean experimentSelected = false;
                for ( Iterator<BrowserNode> iter2 = thisExperiment.childrenIterator(); iter2.hasNext(); ) {
                    BrowserNode thisPass = iter2.next();
                    boolean passSelected = false;
                    for ( Iterator<BrowserNode> iter3 = thisPass.childrenIterator(); iter3.hasNext(); ) {
                        LocalJobNode jobNode = (LocalJobNode)iter3.next();
                        if ( !jobNode.status().contentEquals( "Done" ) ) {
                            jobNode.selected( true );
                            passSelected = true;
                        }
                        else
                            jobNode.selected( false );
                    }
                    thisPass.selected( passSelected );
                    experimentSelected |= passSelected;
                }
                thisExperiment.selected( experimentSelected );
            }
            _preview.listChange();
        }
        
        public void selectAll() {
            for ( Iterator<BrowserNode> iter = _preview.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                BrowserNode thisExperiment = iter.next();
                thisExperiment.selected( true );
                for ( Iterator<BrowserNode> iter2 = thisExperiment.childrenIterator(); iter2.hasNext(); ) {
                    BrowserNode thisPass = iter2.next();
                    thisPass.selected( true );
                    for ( Iterator<BrowserNode> iter3 = thisPass.childrenIterator(); iter3.hasNext(); ) {
                        iter3.next().selected( true );
                    }
                }
            }
            _preview.listChange();
        }
        
        public void deselectAll() {
            for ( Iterator<BrowserNode> iter = _preview.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                BrowserNode thisExperiment = iter.next();
                thisExperiment.selected( false );
                for ( Iterator<BrowserNode> iter2 = thisExperiment.childrenIterator(); iter2.hasNext(); ) {
                    BrowserNode thisPass = iter2.next();
                    thisPass.selected( false );
                    for ( Iterator<BrowserNode> iter3 = thisPass.childrenIterator(); iter3.hasNext(); ) {
                        iter3.next().selected( false );
                    }
                }
            }
            _preview.listChange();
        }
        
        protected boolean _continueRetrieval;
        protected JMenuBar _menuBar;
        protected DiskSearchRules _this;
        protected boolean _allObjectsBuilt;
        TabCompletedTextField _fileFilter;
        protected JCheckBox _experimentBasedOnPath;
        protected JCheckBox _experimentNamed;
        protected SaneTextField _experimentName;
        protected JCheckBox _passBasedOnPath;
        protected JCheckBox _passNamed;
        protected JCheckBox _noPass;
        protected SaneTextField _passName;
        protected Spinner _previewSpinner;
        protected NodeBrowserScrollPane _preview;
        protected JCheckBox _autoUpdate;
        protected JButton _applyButton;
        protected JLabel _spinnerLabel;
        protected JButton _updateButton;
//        protected JButton _collapseAllButton;
//        protected JButton _expandAllButton;
        protected JButton _selectIncompleteButton;
        protected JButton _selectAllButton;
        protected JButton _deselectAllButton;
        ArrayList<String> _newList;
        
    }
    
    //--------------------------------------------------------------------------
    //  A function to create a list of files and their "status" values, as
    //  seen by the guiServer.  This function starts a thread to obtain this information.
    //--------------------------------------------------------------------------
    public void getJobStatus( String fileList, boolean shortStatus ) {
        int port = _settings.newDifxTransferPort( 0, 100, true, true );
        if ( port == -1 ) {
            return;
        }
        //  Create packet data to transmit all we need for the filelist generation
        //  on guiServer.  4 bytes for the "short status" flag...
        int nbytes = 4;
        //  4 bytes for the length of the file list, then enough to contain the list.
        nbytes += 4;
        nbytes += fileList.length();
        //  Space for the address.
        nbytes += 4;
        nbytes += _settings.guiServerConnection().myIPAddress().length();
        //  4 bytes for the port...
        nbytes += 4;
        //  Allocate a properly-sized buffer for the message.
        java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate( nbytes );
        //  The short status flag - determines whether we want a single-string status
        //  (such as "Done") or a complex one including all previous activities.
        if ( shortStatus )
            bb.putInt( 1 );
        else
            bb.putInt( 0 );
        //  Add the file list.
        bb.putInt( fileList.length() );
        bb.put( fileList.getBytes() );
        //  The communications IP address.
        bb.putInt( _settings.guiServerConnection().myIPAddress().length() );
        bb.put( _settings.guiServerConnection().myIPAddress().getBytes() );
        //  Finally, the communications port.
        bb.putInt( port );
        //  Convert to byte data.
        byte [] getJobStatusData = bb.array();
        //  Start a thread to monitor generation of the job status list.  The thread (below) is
        //  self-terminating.
        GetJobStatusMonitor monitor = new GetJobStatusMonitor( port );
        monitor.start();
        //  Wait a second for the monitor to start.  Not sure why it takes so long, but
        //  this seems to be a problem.
        try { Thread.sleep( 100 ); } catch ( Exception e ) {}
        //  Send the request to the guiServer - it will connect with the thread.
        _settings.guiServerConnection().sendPacket( _settings.guiServerConnection().GET_JOB_STATUS,
                getJobStatusData.length, getJobStatusData );
    }
    
    protected HashMap<String, String> _simpleStatusMap;
    
    //--------------------------------------------------------------------------
    //  This thread opens and monitors a TCP socket for data from the guiServer as it
    //  determines the "status" of a list of jobs.
    //--------------------------------------------------------------------------
    protected class GetJobStatusMonitor extends Thread {
        
        public GetJobStatusMonitor( int port ) {
            _port = port;
        }
        
        /*
         * These packet types are exchanged with the "GetJobStatusConnection" class in the
         * guiServer application on the DiFX host.
         */
        protected final int GET_JOB_STATUS_TASK_ENDED_GRACEFULLY               = 101;
        protected final int GET_JOB_STATUS_TASK_STARTED                        = 102;
        protected final int GET_JOB_STATUS_INPUT_FILE_NAME                     = 104;
        protected final int GET_JOB_STATUS_NO_DIFXLOG                          = 105;
        protected final int GET_JOB_STATUS_STATUS                              = 106;
        protected final int GET_JOB_STATUS_OPEN_ERROR                          = 107;
        protected final int GET_JOB_STATUS_NO_STATUS                           = 108;
        protected final int GET_JOB_STATUS_TASK_TERMINATED                     = 109;
        protected final int GET_JOB_STATUS_TIME_STRING                         = 110;
        protected final int GET_JOB_STATUS_FORK_FAILED                         = 111;
        
        @Override
        public void run() {
            //  Open a new server socket and await a connection.  The connection
            //  will timeout after a given number of seconds (nominally 10).
            try {
                ChannelServerSocket ssock = new ChannelServerSocket( _port, _settings );
                ssock.setSoTimeout( 10000 );  //  timeout is in millisec
                try {
                    ssock.accept();
                    //  Loop collecting diagnostic packets from the guiServer.  These
                    //  are identified by an initial integer, and then are followed
                    //  by a data length, then data.
                    boolean connected = true;
                    String _currentInputFile = null;
                    String _currentStatus = null;
                    while ( connected ) {
                        //  Read the packet type as an integer.  The packet types
                        //  are defined above (within this class).
                        int packetType = ssock.readInt();
                        //  Read the size of the incoming data (bytes).
                        int packetSize = ssock.readInt();
                        //  Read the data (as raw bytes)
                        byte [] data = null;
                        if ( packetSize > 0 ) {
                            data = new byte[packetSize];
                            ssock.readFully( data, 0, packetSize );
                        }
                        //  Interpret the packet type.
                        if ( packetType == GET_JOB_STATUS_TASK_TERMINATED ) {
                            saveSimpleStatus( _currentInputFile, _currentStatus );
                            connected = false;
                        }
                        else if ( packetType == GET_JOB_STATUS_TASK_ENDED_GRACEFULLY ) {
                            saveSimpleStatus( _currentInputFile, _currentStatus );
                            connected = false;
                        }
                        else if ( packetType == GET_JOB_STATUS_TASK_STARTED ) {
                            //  Zorch the current map of simple statuses...
                            if ( _simpleStatusMap == null )
                                _simpleStatusMap = new HashMap<String,String>();
                            else {
                                synchronized( _simpleStatusMap ) {
                                    _simpleStatusMap.clear();
                                }
                            }
                        }
                        else if ( packetType == GET_JOB_STATUS_INPUT_FILE_NAME ) {
                            //  If we have information on a previous .input file, dump it in the
                            //  hash map.
                            saveSimpleStatus( _currentInputFile, _currentStatus );
                            _currentInputFile = new String( data );
                            _currentStatus = null;
                        }
                        else if ( packetType == GET_JOB_STATUS_NO_DIFXLOG ) {
                        }
                        else if ( packetType == GET_JOB_STATUS_NO_STATUS ) {
                            //  A log file exists, but has no status.  This may indicate a
                            //  job that didn't start properly - probably an interesting piece of
                            //  information.
                            _currentStatus = "no status";
                        }
                        else if ( packetType == GET_JOB_STATUS_STATUS ) {
                            _currentStatus = new String( data ).substring( 24 ).trim();
                        }
                        else if ( packetType == GET_JOB_STATUS_OPEN_ERROR ) {
                            _settings.messageCenter().error( 0, "getJobStatus", "Error opening .difxlog file: " + new String( data ) );
                        }
                        else if ( packetType == GET_JOB_STATUS_TIME_STRING ) {
                            //  Data is a time stamp at the start of the job status request.
                            //  It can be compared to time stamps that tell when each status
                            //  value was updated.
                        }
                        else if ( packetType == GET_JOB_STATUS_FORK_FAILED ) {
                            //  The job status request won't work, but we ignore this message
                            //  (it will send a TERMINATED message momentarily).
                        }
                    }
                } catch ( java.net.SocketTimeoutException e ) {
                }
                ssock.close();
            } catch ( java.io.IOException e ) {
                e.printStackTrace();
            }
            _settings.releaseTransferPort( _port );
        }
        
        void saveSimpleStatus( String inputFile, String status ) {
            if ( inputFile != null ) {
                synchronized( _simpleStatusMap ) {
                    _simpleStatusMap.put( inputFile, status );
                }
            }
        }
        
        protected int _port;
        
    }
    
    //--------------------------------------------------------------------------
    //  Get the "simple" status for the named input file.
    //--------------------------------------------------------------------------
    public String getSimpleStatus( String inputFile ) {
        String ret = null;
        if ( _simpleStatusMap == null )
            return null;
        synchronized( _simpleStatusMap ) {
            ret = _simpleStatusMap.get( inputFile );
        }
        return ret;
    }
    
    //--------------------------------------------------------------------------
    //  Return whether or not we have a status associated with this input file.
    //--------------------------------------------------------------------------
    public boolean haveSimpleStatus( String inputFile ) {
        boolean ret;
        if ( _simpleStatusMap == null )
            return false;
        synchronized( _simpleStatusMap ) {
            ret = _simpleStatusMap.containsKey( inputFile );
        }
        return ret;
    }
    
    /*
     * Perform a search for experiments/jobs/etc on disk following the rules defined
     * in the DiskSearchRules class.
     */
    public void updateQueueFromDisk() {
    }
    
    public Iterator<BrowserNode> experimentsIterator() {
        return _browserPane.browserTopNode().children().iterator();
    }
    
    /*
     * Check the status of all known jobs in the database and update any information
     * that changes.  Most items related to a job do not change in the database, so
     * we don't need to check much.  This function will also figure out if a job has
     * been de-queued, i.e. removed from the database.
     */
    protected void checkQueueStatusFromDatabase() {
    }
    
    /*
     * Process a DiFX Status message.  These messages come from processors (usually the
     * head node, seemingly) when jobs are running.  
     */
    protected void processDifxStatusMessage( DifxMessage difxMsg ) {
        serviceUpdate( difxMsg );
    }
    
    //--------------------------------------------------------------------------
    //!  Diagnostic messages are job-related.
    //--------------------------------------------------------------------------
    protected void processDifxDiagnosticMessage( DifxMessage difxMsg ) {
        serviceUpdate( difxMsg );
    }
    
    /*
     * Process a DiFX Alert message.  Here we are only interested in the "alerts" that
     * appear to emerge from jobs.  For the moment I'm assuming any alert that does
     * not come from mk5daemon is a job-related message (this is possibly not true in
     * all cases).
     */
    protected void processDifxAlertMessage( DifxMessage difxMsg ) {
        if ( !difxMsg.getHeader().getIdentifier().trim().equals( "mk5daemon" ) )
            serviceUpdate( difxMsg );
    }
    
    /*
     * Parse a difx message relayed to us from the data model.  This (presumably)
     * contains some information about a job.  Some difx messages are caused by actions
     * not related to job processing - file copying, building directories, etc.  These
     * we ignore.
     */
    public void serviceUpdate( DifxMessage difxMsg ) {
        
        //  See if this message looks like it is for a job.  Only proceed if it
        //  does.
        if ( JobNode.testJobMessage( difxMsg ) ) {
        
            //  The identifier provides us with the job name.  Lacking anything else
            //  to go on, we use the job name to locate the job in our current list of
            //  jobs.
            JobNode thisJob = null;
            
            //  First, we loop through all the jobs we know about and see if one matching
            //  this name considers itself currently "running".  If so, it gets the message.
            //  We do this to *try* to get the correct job to absorb the message, as it
            //  is easy for multiple jobs to have the same names.  Until we have access
            //  to a unique identifier, this is the best we can do.  Note that jobs
            //  become "running" when the user starts them from the GUI.
            synchronized ( _browserPane ) {
                for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                        projectIter.hasNext() && thisJob == null; ) {
                    ExperimentNode testExperiment = (ExperimentNode)projectIter.next();
                    PassNode thisPass = null;
                    if ( testExperiment.children().size() > 0 ) {
                        for ( Iterator<BrowserNode> iter = testExperiment.childrenIterator(); iter.hasNext(); ) {
                            PassNode testPass = (PassNode)(iter.next());
                            //  Within each project, look at all jobs...
                            for ( Iterator<BrowserNode> jobIter = testPass.children().iterator(); 
                                jobIter.hasNext() && thisJob == null; ) {
                                JobNode testJob = (JobNode)jobIter.next();
                                if ( testJob.name().equals( difxMsg.getHeader().getIdentifier() ) && testJob.running() )
                                    thisJob = testJob;
                            }
                        }
                    }
                }
            }
            
            //  If the job hasn't been located using the "active" search above, try
            //  finding the job just using its name.
            if ( thisJob == null ) {
                synchronized ( _browserPane ) {
                    for ( Iterator<BrowserNode> projectIter = _browserPane.browserTopNode().children().iterator();
                            projectIter.hasNext() && thisJob == null; ) {
                        ExperimentNode testExperiment = (ExperimentNode)projectIter.next();
                        PassNode thisPass = null;
                        if ( testExperiment.children().size() > 0 ) {
                            for ( Iterator<BrowserNode> iter = testExperiment.childrenIterator(); iter.hasNext(); ) {
                                PassNode testPass = (PassNode)(iter.next());
                                //  Within each project, look at all jobs...
                                for ( Iterator<BrowserNode> jobIter = testPass.children().iterator(); 
                                    jobIter.hasNext() && thisJob == null; ) {
                                    JobNode testJob = (JobNode)jobIter.next();
                                    if ( testJob.name().equals( difxMsg.getHeader().getIdentifier() ) )
                                        thisJob = testJob;
                                }
                            }
                        }
                    }
                }
            }

            //  If we didn't find this job, create an entry for it in the "unaffiliated"
            //  project (which we might have to create if it doesn't exist!).
            if ( thisJob == null ) {
                if ( _unaffiliated == null ) {
                    _unaffiliated = new ExperimentNode( "Jobs Outside Queue", _settings );
                    _browserPane.addNode( _unaffiliated );
                    _unknown = new PassNode( "", _settings );
                    _unknown.experimentNode( _unaffiliated );
                    _unknown.setHeight( 0 );
                    _unknown.showNothing( true );
                    _unknown.statsVisible( false );
                    _unaffiliated.addChild( _unknown );
                }
                thisJob = new JobNode( difxMsg.getHeader().getIdentifier(), _settings );
                _unknown.addChild( thisJob );
                thisJob.passNode( _unknown );
                _header.addJob( thisJob );
            }
            
            boolean unknownJob = ( thisJob == null || thisJob.passNode() == null || thisJob.passNode() == _unknown );

            //  Send the message to the job node.
            thisJob.consumeMessage( difxMsg, unknownJob );
        
        }
        
    }  
    
    /*
     * Add the given job to the queue browser list (if its not already there).  The
     * job has an experiment and pass name along with the full path to an input file.
     * The input file can be parsed for complete job information.
     */
    public ExperimentNode addDefinedJob( String experiment, String pass, String job, String inputFile,
            String experimentPath ) {

        //  Locate the experiment in the current list...if it is there.
        ExperimentNode thisExperiment = null;
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> iter = _browserPane.browserTopNode().childrenIterator();
                    iter.hasNext() && thisExperiment == null; ) {
                ExperimentNode testExperiment = (ExperimentNode)iter.next();
                if ( testExperiment.name().contentEquals( experiment ) )
                    thisExperiment = testExperiment;
            }
        }
        //  Create a new experiment if it was not found.
        if ( thisExperiment == null ) {
            thisExperiment = new ExperimentNode( experiment, _settings );
            //  Some of this information we should probably be able to figure out.
            thisExperiment.number( 0 );
            thisExperiment.name( experiment );
            thisExperiment.id( 0 );                  //  dunno
            thisExperiment.inDatabase( false );      //  dunno
            thisExperiment.status( "unknown" );
            thisExperiment.directory( experimentPath );
            thisExperiment.statsVisible( _showPassScheduledItem.isSelected() );
            //  Use the creation date of the experiment path as the experiment's
            //  creation date.  This should be pretty accurate.
            thisExperiment.useCreationDate( experimentPath );
            thisExperiment.vexFile( "" );
            _browserPane.addNode( thisExperiment );
        }
        
        //  Locate the pass under this experiment, if there.
        PassNode thisPass = null;
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> iter = thisExperiment.childrenIterator(); 
                    iter.hasNext() && thisPass == null; ) {
                PassNode testPass = (PassNode)iter.next();
                if ( testPass.name().contentEquals( pass ) )
                    thisPass = testPass;
            }
        }
        //  Create the pass if we didn't find that.
        if ( thisPass == null ) {
            thisPass = new PassNode( pass, _settings );
//            thisPass.type( passType );
//            thisPass.id( id );
//            thisPass.inDatabase( true );
            thisPass.experimentNode( thisExperiment );
            thisPass.statsVisible( _showPassScheduledItem.isSelected() );
            thisExperiment.addChild( thisPass );      
            //  Get the full path to the pass using the .input file.
            thisPass.fullPath( inputFile.substring( 0, inputFile.lastIndexOf( '/' ) ) );
            //  Locate the "original" .v2d file name.  There can be a bunch of .v2d files due to
            //  user actions on individual jobs.  However such actions create .v2d files that have
            //  names based on the original, all of which are LONGER.  So hopefully the original
            //  file will be the one with the shortest name.  Kind of ugly.
            final ArrayList<String> v2dList = new ArrayList<String>();
            final PassNode searchPass = thisPass;
            DiFXCommand_ls ls = new DiFXCommand_ls( thisPass.fullPath() + "/*.v2d", _settings );
            ls.addEndListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    //  Found anything at all?
                    if ( v2dList.size() > 0 ) {
                        Integer len = null;
                        String saveStr = null;
                        for ( Iterator<String> iter = v2dList.iterator(); iter.hasNext(); ) {
                            String newStr = iter.next();
                            if ( len == null ) {
                                len = newStr.length();
                                saveStr = newStr;
                            }
                            else if ( len > newStr.length() ) {
                                len = newStr.length();
                                saveStr = newStr;
                            }
                        }
                        if ( saveStr != null )
                            searchPass.v2dFileName( saveStr.substring( saveStr.lastIndexOf( '/' ) + 1 ) );
                    }
                    else
                        _settings.messageCenter().error( 0, "Queue Browser", "no .v2d files found in " + searchPass.fullPath() );
                }
            });
            ls.addIncrementalListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    v2dList.add( e.getActionCommand().trim() );
                }
            });
            try {
                ls.send();
            } catch ( java.net.UnknownHostException e ) {
                //  BLAT handle this
            }            
        }
        
        //  Then locate the job, if there.
        JobNode thisJob = null;
        synchronized ( _browserPane ) {
            for ( Iterator<BrowserNode> iter = thisPass.childrenIterator();
                    iter.hasNext() && thisJob == null; ) {
                JobNode testJob = (JobNode)iter.next();
                if ( testJob.inputFile().contentEquals( inputFile ) )
                    thisJob = testJob;
            }
        }
        //  If the job wasn't found, add it.
        if ( thisJob == null ) {
            thisJob = new JobNode( job, _settings );
//            thisJob.id( id );
//            thisJob.inDatabase( true );
            thisJob.experiment( thisExperiment.name() );
            thisJob.pass( thisPass.name() );
            thisJob.passNode( thisPass );
            thisJob.inputFile( inputFile, false );
            //  Try to get the "simple" status of this job.
            String status = getSimpleStatus( inputFile );
            if ( status != null ) {
                if ( status.contentEquals( "Done" ) )
                    thisJob.setState( "Done", Color.GREEN );
                else
                    thisJob.setState( "Run Failed", Color.RED );
            }
            else
                thisJob.setState( "not Started", Color.LIGHT_GRAY );
            //thisJob.setState( "getting log", Color.yellow );
            //thisJob.parseLogFile();
//            //  Create a new log file for this job using what we expect its name to be.
//            thisJob.logFile( new ActivityLogFile( inputFile.substring( 0, inputFile.lastIndexOf( "/" ) ) + "/guiLogs"
//                    + inputFile.substring( inputFile.lastIndexOf( "/" ) ).replace( ".input", ".jobLog" ) ) );
//            //  If such a file exists already, force it to be downloaded the first time it
//            //  is used.
//            thisJob.logFile().downloadExisting( true );
            synchronized ( _browserPane ) {
                thisPass.addChild( thisJob );
            }
            _header.addJob( thisJob );
        }
        //  Increment the counters in the headers for the pass and experiment for this job.
        //  This may not keep it 100% accurate, but it should make it more accurate, and
        //  more visually pleasing, if that's worth anything.
        thisPass.addJobs( 1 );
        thisPass.displayNow();
        thisExperiment.addJobs( 1 ); 
        thisExperiment.displayNow();
        //  Adjust the setting of the experiment editor to match the most recently
        //  added job settings (based on that job's .v2d file).  This won't always
        //  be useful, but will at least sometimes be what the user wants.  We only
        //  do this if the experiment editor was just created (otherwise this might
        //  be done over and over again).
        if ( thisExperiment.editor() == null ) {
            ExperimentEditor editor = thisExperiment.generateEditor();
            String v2dFileBase = inputFile.substring( 0, inputFile.lastIndexOf( '/' ) + 1 );
            editor.findOldV2dFile( v2dFileBase );
        }
//        thisJob.setSource( thisExperiment.editor() );
        _browserPane.setBounds( _browserPane.getX(), _browserPane.getY(), _browserPane.getWidth(), _browserPane.getHeight() );
        _browserPane.updateUI();
        return thisExperiment;
    }   
    
    public JobNodesHeader header() { return _header; }
    
    protected NodeBrowserScrollPane _browserPane;
    protected NodeBrowserScrollPane _headerPane;
    protected JLabel _mainLabel;
    protected ExperimentNode _unaffiliated;
    protected PassNode _unknown;
    protected SystemSettings _settings;
    protected JobNodesHeader _header;
    protected JButton _experimentButton;
    protected JPopupMenu _experimentMenu;
    protected ZMenuItem _newExperimentItem;
    protected ZMenuItem _existingExperimentItem;
    protected ZMenuItem _fromDatabaseItem;
    protected ZMenuItem _updateDatabaseItem;
    protected JButton _existingButton;
    protected JButton _selectButton;
    protected JPopupMenu _selectMenu;
    protected JButton _showButton;
    protected JPopupMenu _showMenu;
    protected JCheckBoxMenuItem _showSelectedItem;
    protected JCheckBoxMenuItem _showUnselectedItem;
    protected JCheckBoxMenuItem _showCompletedItem;
    protected JCheckBoxMenuItem _showIncompleteItem;
    protected JCheckBoxMenuItem _showExperimentScheduledItem;
    protected JCheckBoxMenuItem _showPassScheduledItem;
    protected ActivityMonitorLight _guiServerConnectionLight;
    protected JLabel _guiServerConnectionLabel;
    protected JLabel _difxVersionLabel;
    protected NumLabel _numExperiments;
    protected NumLabel _numPasses;
    protected NumLabel _numJobs;
    protected boolean _triggerDatabaseUpdate;
    protected Spinner _workingSpinner;
    protected JLabel _workingLabel;
    protected DatabaseUpdateThread _databaseUpdateThread;
    protected DiskSearchRules _diskSearchRules;
    protected ArrayDeque<JobNode> _scheduleQueue;
    protected ScheduleThread _scheduleThread;
    
    protected Double _extraTime;
    protected Double _startBuffer;
    protected Double _timePerBaselinePerSecond;
    protected Integer _lastBaselines;
    protected Double _lastDuration;
    protected Integer _lastThreads;
    
    //--------------------------------------------------------------------------
    //!  As jobs run they provide us with statistics that can be used to improve
    //!  the estimates of how long jobs will take to run.
    //--------------------------------------------------------------------------
    public void jobRunStats( double startBuffer, double runTime, int baselines, double duration, int threads, double extraTime ) {
        //  The "extra time" is the number of extra seconds a job had in its estimate
        //  of how long a job needed to run when it actually ended.  This is used to
        //  try to account for the "rapid completion" exhibited by DiFX jobs (where
        //  the "fraction completed" jumps non-linearly in time to 100%).
        _extraTime = extraTime;  //  crude estimate now...
        _startBuffer = startBuffer;
        _lastBaselines = baselines;
        _lastDuration = duration;
        _lastThreads = threads;
        _timePerBaselinePerSecond = runTime / duration / (double)baselines * (double)threads;
        _settings.addCorrelationTimeSum( _timePerBaselinePerSecond );
        _settings.addCorrelationTimeN( 1.0 );
    }
    
    public double estimateExtraTime() {
        if ( _extraTime == null )
            return 0.0;
        else
            return _extraTime;
    }
    
    public double estimateStartBuffer() {
        if ( _startBuffer == null )
            return 0.0;
        else
            return _startBuffer;
    }
    
    public double estimateProcessTime( int baselines, double duration, int threads ) {
        if ( _settings.correlationTimeN() == null )
            return 0.0;
        else
            return _settings.correlationTimeSum() / _settings.correlationTimeN() * duration * (double)baselines / threads;
    }
    
    /*
     * Add a new job to the scheduling queue.  Return true if this was successful, false if not
     * (false usually means its already there).
     */
    public boolean addJobToSchedule( JobNode newJob ) {
        //  Make sure the job isn't already in the list of jobs to be run...
        boolean found = false;
        synchronized ( _scheduleQueue ) {
            for ( Iterator<JobNode> iter = _scheduleQueue.iterator(); iter.hasNext() && !found; ) {
                JobNode thisJob = iter.next();
                if ( thisJob.inputFile().contentEquals( newJob.inputFile() ) )
                    found = true;
            }
        }
        if ( found )
            return false;
        synchronized ( _scheduleQueue ) {
            _scheduleQueue.add( newJob );
        }
        return true;
    }
    
    /*
     * Remove a job from the scheduling queue.
     */
    public boolean removeJobFromSchedule( JobNode newJob ) {
        boolean found = false;
        synchronized ( _scheduleQueue ) {
            for ( Iterator<JobNode> iter = _scheduleQueue.iterator(); iter.hasNext() && !found; ) {
                JobNode thisJob = iter.next();
                if ( thisJob.inputFile().contentEquals( newJob.inputFile() ) ) {
                    iter.remove();
                    found = true;
                }
            }
        }
        return found;
    }
    
    boolean isJobInSchedule( String name ) {
        boolean found = false;
        synchronized ( _scheduleQueue ) {
            for ( Iterator<JobNode> iter = _scheduleQueue.iterator(); iter.hasNext() && !found; ) {
                JobNode thisJob = iter.next();
                if ( thisJob.name().contentEquals( name ) ) {
                    found = true;
                }
            }
        }
        return found;
    }
    
    class ScheduleThread extends Thread {
        public boolean keepGoing = true;
        @Override
        public void run() {
            while ( keepGoing ) {
                synchronized ( _scheduleQueue ) {
                    //  Check the state of jobs in the queue - first remove any jobs that are "done",
                    //  which could mean failed or finished cleanly - we are only concerned here with
                    //  whether we should consider scheduling them for something in the future (which
                    //  we should not for "done" jobs).
                    boolean runningJob = false;
                    boolean initializingJob = false;
                    int scheduleCount = 0;
                    int runningCount = 0;
                    for ( Iterator<JobNode> iter = _scheduleQueue.iterator(); iter.hasNext(); ) {
                        JobNode thisJob = iter.next();
                        //  If we are only supposed to run one job at a time, we need to know if any jobs
                        //  are running or in the process of allocating resources.
                        if ( thisJob.autostate() == JobNode.AUTOSTATE_INITIALIZING || thisJob.autostate() == JobNode.AUTOSTATE_RUNNING ) {
                            runningJob = true;
                            thisJob.incrementIdleTime();
                            if ( thisJob.autostate() == JobNode.AUTOSTATE_RUNNING )
                                ++runningCount;
                            else if ( thisJob.autostate() == JobNode.AUTOSTATE_INITIALIZING )
                                initializingJob = true;
                        }
                        else if ( thisJob.autostate() == JobNode.AUTOSTATE_SCHEDULED ) {
                            ++scheduleCount;
                            thisJob.initializeTimeRemaining();
                            thisJob.state().setText( "Scheduled (" + scheduleCount + ")" );
                            thisJob.state().updateUI();
                        }
                        else if ( thisJob.autostate() == JobNode.AUTOSTATE_DONE ) {
                            thisJob.freeMonitor( 60 );
                            iter.remove();
                        }
                        else if ( thisJob.autostate() == JobNode.AUTOSTATE_UNSCHEDULED ||
                                thisJob.autostate() == JobNode.AUTOSTATE_FAILED ||
                                thisJob.autostate() == JobNode.AUTOSTATE_RESOURCE_TIMEOUT ||
                                thisJob.autostate() == JobNode.AUTOSTATE_STOPPED ) {
                            iter.remove();
                            thisJob.flushFromActiveNodes();
                        }
                    }
                    //  If any jobs are running and we are only supposed to run jobs sequentially,
                    //  we are done for this cycle.  Also bail out if we are running as many jobs
                    //  as our maximum limit of simultaneous jobs.
                    boolean didSomething = false;
                    if ( ( !_settings.sequentialCheck() && runningCount < _settings.maxJobs() ) || !runningJob ) { 
                        //  Look at the remaining jobs in the queue and run them according to scheduling
                        //  rules.  We run only one at a time for each cycle of the scheduler.  First see
                        //  if any are ready to run.
                        for ( Iterator<JobNode> iter = _scheduleQueue.iterator(); iter.hasNext() && !didSomething; ) {
                            JobNode thisJob = iter.next();
                            if ( thisJob.autostate() == JobNode.AUTOSTATE_READY ) {
                                thisJob.autostartJobStart();
                                didSomething = true;
                            }
                        }
                        //  Next see if anything needs to be initialized (hardware resources allocated, etc.).
                        //  Only one job should be initializing at a time - this keeps the hardware allocation
                        //  logic from getting confused (jobs that are initialized look for free hardware, but
                        //  hardware does not become "reserved" - i.e. not free - until the job starts...so a
                        //  bunch of jobs could be simultaneously initialized with inaccurate hardware availability).
                        for ( Iterator<JobNode> iter = _scheduleQueue.iterator(); iter.hasNext() && !didSomething && !initializingJob; ) {
                            JobNode thisJob = iter.next();
                            if ( thisJob.autostate() == JobNode.AUTOSTATE_SCHEDULED ) {
                                thisJob.autostartCheckResources();
                                didSomething = true;
                            }
                        }
                    }
                    //  If there has been no activity for the maximum number of seconds allowed
                    //  to either check resources or run jobs, try unscheduling an activity in
                    //  an effort to kick the correlator back into action.
                    if ( !didSomething && ( _settings.useMaxSecondsForHardware() || _settings.useMaxSecondsForProcessing() ) ) {
                        for ( Iterator<JobNode> iter = _scheduleQueue.iterator(); iter.hasNext() && !didSomething; ) {
                            JobNode thisJob = iter.next();
                            if ( thisJob.autostate() == JobNode.AUTOSTATE_INITIALIZING && 
                                _settings.useMaxSecondsForHardware() &&
                                thisJob.idleTime() >= _settings.maxSecondsForHardware() ) {
                                thisJob.autoUnscheduleResourceAllocation();
                                didSomething = true;
                            }
                            else if ( thisJob.autostate() == JobNode.AUTOSTATE_RUNNING && 
                                _settings.useMaxSecondsForProcessing() &&
                                thisJob.idleTime() >= _settings.maxSecondsForProcessing() ) {
                                thisJob.autoUnscheduleProcessing();
                                didSomething = true;
                            }
                        }
                    }
                }
                //  Count the number of jobs, and the number scheduled, completed and failed
                //  for each pass and experiment.
                try {
                    for ( Iterator<BrowserNode> iter = _browserPane.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                        ExperimentNode thisExperiment = (ExperimentNode)(iter.next());
                        thisExperiment.clearCounters();
                        boolean experimentRunning = false;
                        double experimentTimeRemaining = 0.0;
                        for ( Iterator<BrowserNode> pIter = thisExperiment.childrenIterator(); pIter.hasNext(); ) {
                            PassNode thisPass = (PassNode)(pIter.next());
                            thisPass.clearCounters();
                            double passTimeRemaining = 0.0;
                            for ( Iterator<BrowserNode> jIter = thisPass.childrenIterator(); jIter.hasNext(); ) {
                                JobNode thisJob = (JobNode)(jIter.next());
                                thisPass.addJobs( 1 );
                                switch ( thisJob.autostate() ) {
                                    case JobNode.AUTOSTATE_DONE:
                                        thisPass.addCompleted( 1 );
                                        break;
                                    case JobNode.AUTOSTATE_FAILED:
                                    case JobNode.AUTOSTATE_RESOURCE_TIMEOUT:
                                    case JobNode.AUTOSTATE_STOPPED:
                                        thisPass.addFailed( 1 );
                                        break;
                                    case JobNode.AUTOSTATE_INITIALIZING:
                                    case JobNode.AUTOSTATE_RUNNING:
                                    case JobNode.AUTOSTATE_SCHEDULED:
                                    case JobNode.AUTOSTATE_READY:
                                        thisPass.addScheduled( 1 );
                                        //  If this job has a "calculated" time remaining, use it.
                                        if ( thisJob.calculatedTimeRemaining() != null )
                                            passTimeRemaining += thisJob.calculatedTimeRemaining();
                                        //  Otherwise, take a guess as it based on previous jobs.
                                        else {
                                            if ( _lastDuration != null && _lastBaselines != null )
                                                passTimeRemaining += estimateProcessTime( _lastBaselines, _lastDuration, _lastThreads )
                                                        + estimateStartBuffer();
//                                        timeRemaining += _settings.queueBrowser().estimateProcessTime( editorMonitor()._inputFile.baselineTable().num,
//                    _jobDuration ) + _settings.queueBrowser().estimateStartBuffer();
                                        }
                                        break;
                                }
                            }
                            thisPass.timeRemaining( passTimeRemaining );
                            experimentTimeRemaining += passTimeRemaining;
                            //  This stuff is used to isolate the correlation time for this pass.
                            if ( thisPass.numScheduled() > 0 ) {
                                if ( thisPass.correlating ) {
                                    thisPass.correlationTime( (double)System.currentTimeMillis() / 1000.0 - thisPass.correlationStart );
                                    experimentRunning = true;
                                }
                                else {
                                    thisPass.correlating = true;
                                    thisPass.correlationStart = (double)System.currentTimeMillis() / 1000.0;
                                    experimentRunning = true;
                                }
                            }
                            else {
                                thisPass.correlating = false;
                            }
                            thisPass.displayNow();
                            thisExperiment.timeRemaining( experimentTimeRemaining );
                            thisExperiment.addJobs( thisPass.numJobs() );
                            thisExperiment.addScheduled( thisPass.numScheduled() );
                            thisExperiment.addCompleted( thisPass.numCompleted() );
                            thisExperiment.addFailed( thisPass.numFailed() );
                            //  This stuff is used to isolate the correlation time for this experiment.
                            if ( experimentRunning && !thisExperiment.correlating ) {
                                thisExperiment.correlating = true;
                                thisExperiment.correlationStart = (double)System.currentTimeMillis() / 1000.0;
                            }
                            else if ( experimentRunning && thisExperiment.correlating ) {
                                thisExperiment.correlationTime( (double)System.currentTimeMillis() / 1000.0 - thisExperiment.correlationStart );
                            }
                            else
                                thisExperiment.correlating = false;
                        }
                        thisExperiment.displayNow();
                    }
                } catch ( java.util.ConcurrentModificationException e ) {
                }
                try { Thread.sleep( 1000 ); } catch ( Exception e ) {}
            }
        }
    }
    
}
