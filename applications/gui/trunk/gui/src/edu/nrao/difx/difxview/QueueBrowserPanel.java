/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxview;

import edu.nrao.difx.difxcontroller.AttributedMessageListener;
import mil.navy.usno.widgetlib.NodeBrowserScrollPane;
import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.TearOffPanel;
import mil.navy.usno.widgetlib.ActivityMonitorLight;

import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JSeparator;

import java.awt.Font;
import java.awt.Color;
import java.awt.Insets;
import java.awt.Point;

import java.io.File;

import java.util.Iterator;
import java.util.Date;
import java.text.SimpleDateFormat;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import edu.nrao.difx.difxcontroller.DiFXMessageProcessor;

import edu.nrao.difx.xmllib.difxmessage.DifxMessage;

import edu.nrao.difx.difxdatabase.QueueDBConnection;

import java.awt.event.KeyEvent;
import java.awt.event.KeyAdapter;

import java.sql.ResultSet;

public class QueueBrowserPanel extends TearOffPanel {

    public QueueBrowserPanel( SystemSettings systemSettings ) {
        _systemSettings = systemSettings;
        _systemSettings.queueBrowser( this );
        setLayout( null );
        _browserPane = new NodeBrowserScrollPane( 20 );
        this.add( _browserPane );
        _headerPane = new NodeBrowserScrollPane( 20 );
        _headerPane.noScrollbar( true );
        this.add( _headerPane );
        addKeyListener( new KeyEventListener() );
        _browserPane.setBackground( Color.WHITE );
        _mainLabel = new JLabel( "Queue Browser" );
        _mainLabel.setBounds( 5, 0, 150, 20 );
        _mainLabel.setFont( new Font( "Dialog", Font.BOLD, 14 ) );
        add( _mainLabel );
        _newButton = new JButton( "New" );
        _newButton.setToolTipText( "Create a new experiment." );
        _newButton.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                newExperiment();
            }
        });
        this.add( _newButton );
        //  The menu for the "select" button.
        _selectMenu = new JPopupMenu();
        JMenuItem selectAllItem = new JMenuItem( "Select All" );
        selectAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectAll();
            }
        });
        _selectMenu.add( selectAllItem );
        JMenuItem unselectAllItem = new JMenuItem( "Unselect All" );
        unselectAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                unselectAll();
            }
        });
        _selectMenu.add( unselectAllItem );
        _selectMenu.add( new JSeparator() );
        JMenuItem runSelectedItem = new JMenuItem( "Run Selected" );
        runSelectedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                runSelected();
            }
        });
        _selectMenu.add( runSelectedItem );
        runSelectedItem.setEnabled( false );
        JMenuItem deleteSelectedItem = new JMenuItem( "Delete Selected" );
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
        _showSelectedItem.setSelected( _systemSettings.queueBrowserSettings().showSelected );
        _showMenu.add( _showSelectedItem );
        _showUnselectedItem = new JCheckBoxMenuItem( "Unselected" );
        _showUnselectedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showItemChange();
            }
        });
        _showUnselectedItem.setSelected( _systemSettings.queueBrowserSettings().showUnselected );
        _showMenu.add( _showUnselectedItem );
        _showCompletedItem = new JCheckBoxMenuItem( "Completed" );
        _showCompletedItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showItemChange();
            }
        });
        _showCompletedItem.setSelected( _systemSettings.queueBrowserSettings().showCompleted );
        _showMenu.add( _showCompletedItem );
        _showIncompleteItem = new JCheckBoxMenuItem( "Incomplete" );
        _showIncompleteItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                showItemChange();
            }
        });
        _showIncompleteItem.setSelected( _systemSettings.queueBrowserSettings().showIncomplete );
        _showMenu.add( _showIncompleteItem );
        _showMenu.add( new JSeparator() );
        JMenuItem expandAllItem = new JMenuItem( "Expand All" );
        expandAllItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                expandAll();
            }
        });
        _showMenu.add( expandAllItem );
        JMenuItem collapseAllItem = new JMenuItem( "Collapse All" );
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
        _guiServerConnectionLight = new ActivityMonitorLight();
        _guiServerConnectionLight.setBounds( 360, 32, 12, 12 );
        _guiServerConnectionLight.alertTime( 0 );
        _guiServerConnectionLight.warningTime( 0 );
        this.add( _guiServerConnectionLight );
        _guiServerConnectionLabel = new JLabel( "guiServer Connection" );
        _guiServerConnectionLabel.setBounds( 380, 25, 200, 25 );
        this.add( _guiServerConnectionLabel );
        _updateButton = new JButton( "DB Update" );
        _updateButton.setToolTipText( "Update queue data from the DiFX database." );
        _updateButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _systemSettings.updateDatabaseNow( true );
            }
        });
        this.add( _updateButton );
        _autoButton = new JButton( "" );
        _autoButton.setToolTipText( "Auto update DiFX queue." );
        _autoButton.setMargin( new Insets( 2, 4, 2, 4 ) );
        this.add( _autoButton );
        _autoActiveLight = new ActivityMonitorLight();
        _autoActiveLight.setBounds( 4, 4, 16, 21 );
        _autoActiveLight.onColor( Color.GREEN );
        _autoButton.add( _autoActiveLight );
        _autoButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                autoButtonAction();
            }
        });
        
        //  Create a header line of all jobs.
        _header = new JobNodesHeader( _systemSettings );
        _headerPane.addNode( _header );

        //  Add a listener for automatic database updates.  When these occur,
        //  we want to update the data for this browser.
        _systemSettings.addDatabaseUpdateListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateQueueFromDatabase();
            }
        });
        
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
        _newButton.setBounds( 5, 30, 100, 25 );
        _selectButton.setBounds( 110, 30, 100, 25 );
        _showButton.setBounds( 220, 30, 100, 25 );
        _guiServerConnectionLight.setBounds( width - 300, 37, 12, 12 );
        _guiServerConnectionLabel.setBounds( width - 280, 30, 130, 25 );
        _updateButton.setBounds( width - 120, 30, 110, 25 );
        _autoButton.setBounds( width - 145, 30, 25, 25 );
    }
    
    public ActivityMonitorLight guiServerConnectionLight() {
        return _guiServerConnectionLight;
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
     * Add a new experiment to the browser.
     */
    public void addExperiment( ExperimentNode newExperiment ) {
        _browserPane.addNode( newExperiment );
    }
    
    /*
     * Add a new job to the header.  This is so changes in header column widths
     * apply to the job.
     */
    public void addJob( JobNode newJob ) {
        _header.addJob( newJob );
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
        if ( _systemSettings.useDatabase() ) {
            db = new QueueDBConnection( _systemSettings );
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
            BrowserNode experimentList = _browserPane.browserTopNode();
            for ( Iterator<BrowserNode> iter = experimentList.childrenIterator(); iter.hasNext(); ) {
                ExperimentNode thisExperiment = (ExperimentNode)(iter.next());
                if ( thisExperiment.id() != null && thisExperiment.id() >= newExperimentId )
                    newExperimentId = thisExperiment.id() + 1;
            }
        }
        
        //  Open a window where the user can specify details of the new experiment.
        Point pt = _newButton.getLocationOnScreen();
        ExperimentEditor win =
                new ExperimentEditor( pt.x + 25, pt.y + 25, _systemSettings );
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
        win.directory( _systemSettings.workingDirectory() + "/" + win.name() );
        win.vexFileName( win.name() + ".vex" );
        win.addVexFileName( "one" );
        win.addVexFileName( "two" );
        win.addVexFileName( "three" );
        win.keepDirectory( false );
        win.passName( "Production Pass" );
        win.createPass( _systemSettings.defaultNames().createPassOnExperimentCreation );
        win.newExperimentMode( true );
        win.setVisible( true );
    }
        
    protected void selectAll() {};
    protected void unselectAll() {};
    protected void runSelected() {};
    protected void deleteSelected() {};
    protected void showItemChange() {};
    protected void expandAll() {};
    protected void collapseAll() {};
                
    /*
     * The user has hit the "auto" button.  This activates the "auto" light and
     * causes auto updates to occur.  Or it turns them off.
     */
    public void autoButtonAction() {
        if ( _systemSettings.dbAutoUpdate() ) {
            _systemSettings.dbAutoUpdate( false );
        }
        else {
            _systemSettings.dbAutoUpdate( true );
        }
    }
    
    /*
     * Update our list of experiments, passes, and nodes from the database.  This
     * pulls everything off the database and uses it to change our current list.  Things
     * which are in the current list that are NOT found in the database are eliminated.
     */
    void updateQueueFromDatabase() {
        
        //  Don't do this if the user isn't using the database.
        if ( !_systemSettings.useDatabase() )
            return;
        
        //  Get a new connection to the database.  Bail out if this doesn't work.
        QueueDBConnection db = new QueueDBConnection( _systemSettings );
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
        
        //  Getting this far indicates a successful update from the queue.  
        //  Flash the indicator light!
        _autoActiveLight.on( true );

        //  We need to track the addition and deletion of items in the data base.
        //  To make this possible, we set a "found" flag to false in each item we
        //  already know about - if we don't "find" any one item again, we'll
        //  remove it.
        BrowserNode experimentList = _browserPane.browserTopNode();
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
                experimentList = _browserPane.browserTopNode();
                for ( Iterator<BrowserNode> iter = experimentList.childrenIterator(); iter.hasNext(); ) {
                    ExperimentNode testExperiment = (ExperimentNode)(iter.next());
                    //  We should be able to use the ID to match experiments, as it is
                    //  supposed to be unique.
                    if ( testExperiment.inDatabase() && testExperiment.idMatch( id ) )
                        thisExperiment = testExperiment;
                }
                //  Create a new experiment if we didn't find the named one.
                if ( thisExperiment == null ) {
                    thisExperiment = new ExperimentNode( name, _systemSettings );
                    thisExperiment.id( id );
                    thisExperiment.inDatabase( true );
                    thisExperiment.creationDate( dateCreated );
                    thisExperiment.directory( directory );
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
                //  If this pass wasn't encountered in the list of experiments, see
                //  if it is floating outside the experiment list or if it needs to
                //  be added somewhere.
                if ( thisPass == null ) {
                    //  Was the experiment for this pass identified at least?
                    if ( thisExperiment != null ) {
                        //  Okay, it must be a new pass in the experiment - add it.
                        thisPass = new PassNode( name, _systemSettings );
                        thisPass.type( passType );
                        thisPass.id( id );
                        thisPass.inDatabase( true );
                        thisPass.experimentNode( thisExperiment );
                        thisExperiment.addChild( thisPass );                        
                        thisPass.found( true );
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
                        thisJob = new JobNode( jobName, _systemSettings );
                        thisJob.id( id );
                        thisJob.inDatabase( true );
                        thisJob.experiment( thisExperiment.name() );
                        thisJob.pass( thisPass.name() );
                        thisJob.passNode( thisPass );
                        thisPass.addChild( thisJob );
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
                    thisJob.inputFile( dbJobList.getString( "inputFile" ) );
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
                }
            }
        } catch ( Exception e ) {
            System.out.println( e );
            e.printStackTrace();
        }

        //  Eliminate any items we have failed to find in the data base,
        //  with the exception of those that aren't actually in the data base.
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
            
            //  If the job hasn't been located using the "active" search above, try
            //  finding the job just using its name.
            if ( thisJob == null ) {
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

            //  If we didn't find this job, create an entry for it in the "unaffiliated"
            //  project (which we might have to create if it doesn't exist!).
            if ( thisJob == null ) {
                if ( _unaffiliated == null ) {
                    _unaffiliated = new ExperimentNode( "Jobs Outside Queue", _systemSettings );
                    _browserPane.addNode( _unaffiliated );
                    _unknown = new PassNode( "", _systemSettings );
                    _unknown.experimentNode( _unaffiliated );
                    _unknown.setHeight( 0 );
                    _unaffiliated.addChild( _unknown );
                }
                thisJob = new JobNode( difxMsg.getHeader().getIdentifier(), _systemSettings );
                _unknown.addChild( thisJob );
                thisJob.passNode( _unknown );
                _header.addJob( thisJob );
            }

            //  Send the message to the job node.
            thisJob.consumeMessage( difxMsg );
        
        }
        
    }  
    
    protected NodeBrowserScrollPane _browserPane;
    protected NodeBrowserScrollPane _headerPane;
    protected JLabel _mainLabel;
    protected JButton _updateButton;
    protected ExperimentNode _unaffiliated;
    protected PassNode _unknown;
    protected SystemSettings _systemSettings;
    protected int _timeoutCounter;
    protected JButton _autoButton;
    protected ActivityMonitorLight _autoActiveLight;
    protected JobNodesHeader _header;
    protected boolean _updateNow;
    protected JButton _newButton;
    protected JButton _selectButton;
    protected JPopupMenu _selectMenu;
    protected JButton _showButton;
    protected JPopupMenu _showMenu;
    protected JCheckBoxMenuItem _showSelectedItem;
    protected JCheckBoxMenuItem _showUnselectedItem;
    protected JCheckBoxMenuItem _showCompletedItem;
    protected JCheckBoxMenuItem _showIncompleteItem;
    protected ActivityMonitorLight _guiServerConnectionLight;
    protected JLabel _guiServerConnectionLabel;
    
}
