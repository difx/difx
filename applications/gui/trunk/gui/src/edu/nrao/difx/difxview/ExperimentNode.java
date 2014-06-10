package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.BrowserNode;

import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.JOptionPane;

import java.awt.Frame;
import java.awt.Component;
import java.awt.Point;

import java.util.Iterator;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import mil.navy.usno.widgetlib.ZMenuItem;

import edu.nrao.difx.difxutilities.DiFXCommand_ls;

import edu.nrao.difx.difxdatabase.QueueDBConnection;

import edu.nrao.difx.difxutilities.DiFXCommand_mv;
import edu.nrao.difx.difxutilities.DiFXCommand_rm;

public class ExperimentNode extends QueueBrowserContainerNode {
    
    public ExperimentNode( String name, SystemSettings settings ) {
        super( name, settings );
        name( name );
    }
    
    @Override
    public void createAdditionalItems() {
        //  Create a popup menu appropriate to a "project".
        _popup = new JPopupMenu();
        JMenuItem selectJobsItem = new JMenuItem( "Select All Jobs" );
        selectJobsItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectAllJobsAction();
            }
        });
        _popup.add( selectJobsItem );
        JMenuItem unselectJobsItem = new JMenuItem( "Unselect All Jobs" );
        unselectJobsItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                unselectAllJobsAction();
            }
        });
        _popup.add( unselectJobsItem );
        ZMenuItem selectIncompleteItem = new ZMenuItem( "Select Incomplete Jobs" );
        selectIncompleteItem.setToolTipText( "Select all jobs for which the State is not \"Done\"." );
        selectIncompleteItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectIncomplete();
            }
        });
        _popup.add( selectIncompleteItem );
        _popup.add( new JSeparator() );
        JMenuItem menuItem4 = new JMenuItem( "Edit Properties" );
        menuItem4.setToolTipText( "Show/Edit the properties of this Experiment, add a new Pass, and create new Jobs." );
        menuItem4.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                propertiesAction();
            }
        });
        _popup.add( menuItem4 );
        JMenuItem copyItem = new JMenuItem( "Copy" );
        copyItem.setToolTipText( "Make a copy of this Experiment, its properties, and its Pass/Job structure." );
        copyItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                copyAction();
            }
        });
        _popup.add( copyItem );
        copyItem.setEnabled( false );
        JMenuItem deleteSelectedItem = new JMenuItem( "Delete Selected Jobs" );
        deleteSelectedItem.setToolTipText( "Delete any selected jobs within this Experiment." );
        deleteSelectedItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                deleteSelectedAction();
            }
        });
        _popup.add( deleteSelectedItem );
        JMenuItem deleteExperimentItem = new JMenuItem( "Delete Experiment" );
        deleteExperimentItem.setToolTipText( "Remove this Experiment from the database.  The Experiment must be empty!" );
        deleteExperimentItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                deleteAction();
            }
        });
        _popup.add( deleteExperimentItem );

        _label.setText( _name );
    }
    
    /*
     * Test whether this node's ID and a given ID are equal.  This is messy due to
     * the possibility things can be null.
     */
    public boolean idMatch( Integer testId ) {
        if ( _id == null && testId == null )
            return true;
        if ( _id == null && testId != null )
            return false;
        if ( _id != null && testId == null )
            return false;
        if ( _id.intValue() != testId.intValue() )
            return false;
        return true;
    }
    
    /*
     * Test whether a name matches.
     */
    public boolean nameMatch( String testName ) {
        if ( name() == null && testName == null )
            return true;
        if ( name() == null && testName != null )
            return false;
        if ( name() != null && testName == null )
            return false;
        if ( name().contentEquals( testName ) )
            return true;
        return false;
    }
    
    public void selectAllJobsAction() {
        for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
            PassNode thisPass = (PassNode)(iter.next());
            thisPass.selectAllJobsAction();
        }
    }
    
    public void selectIncomplete() {
        for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
            PassNode thisPass = (PassNode)(iter.next());
            thisPass.selectIncomplete();
        }
    }
    
    public void unselectAllJobsAction() {
        for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
            PassNode thisPass = (PassNode)(iter.next());
            thisPass.unselectAllJobsAction();
        }
    }
    
    /*
     * Adjust the position of items to correspond to the current level.  This
     * method should be overridden by any inheriting classes that add new items.
     */
    @Override
    public void positionItems() {
        if ( _settings != null )
            _labelWidth = _settings.jobColumnSpecs().name.width + 74;
        super.positionItems();
    }
    
    /*
     * Delete this experiment and its contents.  If the experiment is not empty,
     * prompt the user to see if this is really what they want to do.
     */
    public void deleteAction() {
        if ( !this._children.isEmpty() ) {
            Object[] options = { "Delete All Passes", "Cancel" };
            int ans = JOptionPane.showOptionDialog( this.getParent(),
                    "The Experiment still contains passes which must be deleted first!\n" +
                    "Do you wish to delete them now?",
                    "Experiment Contains Passes",
                    JOptionPane.YES_NO_OPTION,
                    JOptionPane.WARNING_MESSAGE,
                    null,
                    options,
                    options[1] );
            if ( ans == 1 )
                return;
        }
        deleteThisExperiment();
        _settings.queueBrowser().updateUI();
    }

    /*
     * Delete this experiment and its entire contents.
     */
    public void deleteThisExperiment() {
        for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
            PassNode thisPass = (PassNode)(iter.next());
            thisPass.deleteThisPass();
            iter.remove();
        }
        clearChildren();
        //  Remove this experiment from the DiFX host.  This is simply a matter of
        //  deleting the entire directory associated with it.
        System.out.println( "remove \"" + this.directory() + "\"" );
        DiFXCommand_rm rm = new DiFXCommand_rm( this.directory(), "-rf", _settings );
        try { rm.send(); } catch ( Exception e ) {}
        //  Remove this experiment from the database.
        if ( this.inDatabase() ) {
            QueueDBConnection db = null;
            if ( _settings.useDatabase() ) {
                db = new QueueDBConnection( _settings );
                if ( db.connected() ) {
                    db.deleteExperiment( _id );
                }
            }
        }
        //  Remove this experiment from its parent - the queue browser.
        ((BrowserNode)(this.getParent())).removeChild( this );
    }
    
    /*
     * Delete selected jobs from this experiment.
     * QUESTION: should we delete a pass if it is emptied by this action?
     */
    public void deleteSelectedAction() {
        for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
            PassNode thisPass = (PassNode)(iter.next());
            thisPass.deleteSelectedAction();
        }
    }
    
    /*
     * Use the creation time of the given path on the host as the creation time of
     * this experiment.  
     */
    public void useCreationDate( String filePath ) {
        if ( filePath != null ) {
            DiFXCommand_ls ls = new DiFXCommand_ls( filePath, "-l -d --time-style=+\"%Y-%m-%d %H:%M:%S\"", _settings );
            //  Set the callback for when a new item is added to the list.
            ls.addIncrementalListener( new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    creationDate( e.getActionCommand().trim().substring( 28, 47 ) );
                }
            });
            try {
                ls.send();
            } catch ( java.net.UnknownHostException e ) {
                //  BLAT handle this
            }     
        }
    }
    
    /*
     * Set the editor window that is used to change items associated with this
     * experiment.
     */
    public void editor( ExperimentEditor newEditor ) {
        _editor = newEditor;
    }
    
    /*
     * Return a pointer to the editor.
     */
    public ExperimentEditor editor() {
        return _editor;
    }
    
    /*
     * Generate an experiment editor for this experiment.
     */
    public ExperimentEditor generateEditor() {
        if ( _editor == null ) {
            Point pt = this.getLocationOnScreen();
            _editor = new ExperimentEditor( pt.x + 25, pt.y + 25, _settings );
            _editor.number( this.number() );
            _editor.name( this.name() );
            _editor.id( this.id() );
            _editor.inDataBase( this.inDatabase() );
            _editor.created( creationDate() );
            _editor.status( this.status() );
            _editor.directory( this.directory() );
            _editor.vexFileName( this.vexFile() );
        }
        return _editor;
    }
    
    /*
     * Bring up a display/editor for the properties of this experiment.  The
     * editor may or may not already exist - if it does not, create it.
     */
    public void propertiesAction() {
        if ( _editor == null ) {
            Point pt = this.getLocationOnScreen();
            _editor = new ExperimentEditor( pt.x + 25, pt.y + 25, _settings );
            _editor.number( this.number() );
            _editor.name( this.name() );
            _editor.id( this.id() );
            _editor.inDataBase( this.inDatabase() );
            _editor.created( creationDate() );
            _editor.status( this.status() );
            _editor.directory( this.directory() );
            _editor.vexFileName( this.vexFile() );
        }
        _editor.setTitle( "Edit Experiment " + name() );
        _editor.newExperimentMode( false );
        _editor.setVisible( true );
    }
    
    public void copyAction() {
        //  TODO:  copy needs to be implemented
        System.out.println( "java sucks" );
    }

    public void name( String newVal ) {
        _name = newVal;
        _label.setText( _name );
    }
    @Override
    public String name() { return _name; }
    
    public void creationDate( String newVal ) { _creationDate = newVal; }
    public String creationDate() { return _creationDate; }
    
    public void status( String newVal ) { _status = newVal; }
    public String status() { return _status; }
    
    public void number( Integer newVal ) { _number = newVal; }
    public Integer number() { return _number; }
    
    public void directory( String newVal ) { _directory = newVal; }
    public String directory() { return _directory; }
    
    public void vexFile( String newVal ) { _vexFile = newVal; }
    public String vexFile() { return _vexFile; }
    
    protected String _name;
    protected String _creationDate;
    protected String _status;
    protected Integer _number;
    protected String _directory;
    protected String _vexFile;
    
    protected ExperimentEditor _editor;
    
}
