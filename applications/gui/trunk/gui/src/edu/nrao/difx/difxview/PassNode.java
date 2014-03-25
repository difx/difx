/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.BrowserNode;

import javax.swing.JPopupMenu;
import javax.swing.JMenuItem;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JTextField;
import javax.swing.JSeparator;
import javax.swing.JOptionPane;
import javax.swing.JLabel;

import java.util.Iterator;
import java.util.Map;
import java.util.ArrayList;
import java.awt.Color;
import java.awt.Container;
import java.awt.event.MouseEvent;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import edu.nrao.difx.difxdatabase.QueueDBConnection;
import java.awt.Font;

/**
 *
 * @author jspitzak
 */
public class PassNode extends QueueBrowserNode {
    
    public PassNode( String name, SystemSettings settings ) {
        super( name );
        _name = name;
        _settings = settings;
        this.labelWidth( 400 );
    }
    
    @Override
    public void createAdditionalItems() {
        //  This field is used to edit the name of the experiment when "rename"
        //  is picked from the popup menu.
        _nameEditor = new JTextField( "" );
        _nameEditor.setVisible( false );
        _nameEditor.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                nameEditorAction();
            }
        });
        this.add( _nameEditor );
        _stateLabel = new ColumnTextArea( "" );
        _stateLabel.setVisible( false );
        this.add( _stateLabel );
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
        _popup.add( new JSeparator() );
        JMenuItem menuItem4 = new JMenuItem( "Rename" );
        menuItem4.setToolTipText( "Rename this Pass." );
        menuItem4.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                renameAction();
            }
        });
        _popup.add( menuItem4 );
        _typeMenu = new JMenu( "Set Type" );
        _popup.add( _typeMenu );
        JMenuItem copyItem = new JMenuItem( "Copy" );
        copyItem.setToolTipText( "Make a copy of this Pass, its properties, and all contained Jobs." );
        copyItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                copyAction();
            }
        });
        _popup.add( copyItem );
        copyItem.setEnabled( false );
        JMenuItem menuItem2 = new JMenuItem( "Delete Selected Jobs" );
        menuItem2.setToolTipText( "Delete any selected jobs within this Pass." );
        menuItem2.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                deleteSelectedAction();
            }
        });
        _popup.add( menuItem2 );
        JMenuItem deletePassItem = new JMenuItem( "Delete Pass" );
        deletePassItem.setToolTipText( "Delete this Pass from the database.  The Pass should be empty." );
        deletePassItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                deleteAction();
            }
        });
        _popup.add( deletePassItem );
    }
    
    public void stateLabel( String text, Color color, boolean visible ) {
        _stateLabel.setText( text );
        _stateLabel.setBackground( color );
        _stateLabel.setVisible( visible );
    }
    
    /*
     * Dynamically build the type components before making the popup menu visible.
     */
    @Override
    public void mousePressed( MouseEvent e ) {
        if ( e.getButton() == MouseEvent.BUTTON3 && _popup != null ) {
            _typeMenu.removeAll();
            if ( _checkList == null )
                _checkList = new ArrayList<JCheckBoxMenuItem>();
            _checkList.clear();
            Map<Integer, String> passTypeList = _settings.passTypeList();
            if ( passTypeList != null ) {
                Iterator iter = passTypeList.entrySet().iterator();
                for ( ; iter.hasNext(); ) {
                    Map.Entry m = (Map.Entry)iter.next();
                    JCheckBoxMenuItem newStatus = new JCheckBoxMenuItem( (String)m.getValue() );
                    final int id = (Integer)m.getKey();
                    newStatus.addActionListener(new ActionListener() {
                        public void actionPerformed( ActionEvent e ) {
                            newTypeChoice( id );
                        }
                    });
                    _typeMenu.add( newStatus );
                    _checkList.add( newStatus );
                    if ( type().equalsIgnoreCase( newStatus.getText() ) )
                        newStatus.setSelected( true );
                }
            }
            JMenuItem addItem = new JMenuItem( "Add Type" );
            addItem.addActionListener(new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    System.out.println( "Add a new type" );
                }
            });
            _typeMenu.add( addItem );
            _popup.show( e.getComponent(), e.getX(), e.getY() );
        }
        else {
            Container foo = this.getParent();
            foo.dispatchEvent( e );
        }
    }
    
    /*
     * Function called when the user picks a new status from the popup menu
     * we built in the "mousePressed()" function.
     */
    public void newTypeChoice( int id ) {
        String thisType = _settings.passTypeString( id );
        //  Set the new type
        type( thisType );
        //  Then run through the check boxes and make sure the chosen one is checked
        //  (and the others aren't).
        for ( Iterator<JCheckBoxMenuItem> iter = _checkList.iterator(); iter.hasNext(); ) {
            JCheckBoxMenuItem item = iter.next();
            if ( item.getText().equalsIgnoreCase( thisType ) )
                item.setSelected( true );
            else
                item.setSelected( false );
        }
        changeTypeInDatabase();
    }
    
    /*
     * Adjust the position of items to correspond to the current level.
     */
    @Override
    public void positionItems() {
        super.positionItems();
        _nameEditor.setBounds( _level * _levelOffset, 0, _labelWidth, _ySize );
        _stateLabel.setBounds( _level * _levelOffset + _xOffset + _labelWidth, 0, 300, _ySize );
    }
    
    public void selectAllJobsAction() {
        for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
            JobNode thisJob = (JobNode)(iter.next());
            thisJob.selected( true );
        }
    }
    
    public void unselectAllJobsAction() {
        for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
            JobNode thisJob = (JobNode)(iter.next());
            thisJob.selected( false );
        }
    }
    
    /*
     * This function responds to a rename request from the popup menu.  It replaces
     * the "label" text field with an editable field containing the name.
     */
    public void renameAction() {
        _nameEditor.setText( _name );
        _nameEditor.setVisible( true );
        _label.setVisible( false );
    }
    
    /*
     * This is the callback for the editable name field triggered by a rename
     * request.  It replaces the label containing the name with whatever is in
     * the edited field.  The change must be sent to the database as well!
     */
    public void nameEditorAction() {
        _name = _nameEditor.getText();
        setLabelText();
        _label.setVisible( true );
        _nameEditor.setVisible( false );
        updateDatabase( "passName", name() );
    }

    public void copyAction() {
        //  TODO: copy needs to work!
    }
    
    /*
     * Attempt to delete this pass from the database.  A check is made to assure that
     * the pass is empty of all jobs - if not, the user is given the option of deleting
     * all jobs first.
     */
    public void deleteAction() {
        if ( !this._children.isEmpty() ) {
            Object[] options = { "Delete All Jobs", "Cancel" };
            int ans = JOptionPane.showOptionDialog( this,
                    "The Pass still contains jobs which must be deleted first!\n" +
                    "Do you wish to delete them now?",
                    "Pass Contains Jobs",
                    JOptionPane.YES_NO_OPTION,
                    JOptionPane.WARNING_MESSAGE,
                    null,
                    options,
                    options[1] );
            if ( ans == 1 )
                return;
        }
        deleteThisPass();
        deleteThisPassFromParent();
    }
    
    /*
     * Delete all of the children of this pass (if there are any) and then delete the
     * pass itself.
     */
    public void deleteThisPass() {
        for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
            JobNode thisJob = (JobNode)(iter.next());
            _settings.queueBrowser().removeJobFromSchedule( thisJob );
            thisJob.removeFromDatabase();
        }
        clearChildren();
        //  We can't remove the pass from the DiFX host because we don't necessarily
        //  know the name of the directory we are in.  If we did, we'd just remove the
        //  entire directory.  We could use the .v2d file name to get it, but the
        //  database doesn't currently store this information.  So we're leaving a 
        //  little litter on the DiFX host.  BLAT
        //  Remove the pass from the database (if we are using it).
        if ( this.inDatabase() ) {
            QueueDBConnection db = null;
            if ( _settings.useDatabase() ) {
                db = new QueueDBConnection( _settings );
                if ( db.connected() ) {
                    db.deletePass( _id );
                }
            }
        }
    }
    
    public void deleteThisPassFromParent() {
        //  Remove this pass from its parent experiment.
        ((BrowserNode)(this.getParent())).removeChild( this );
    }
    
    /*
     * Delete any selected jobs from this pass.
     */
    public void deleteSelectedAction() {
        try {
            for ( Iterator<BrowserNode> iter = childrenIterator(); iter.hasNext(); ) {
                JobNode thisJob = (JobNode)(iter.next());
                if ( thisJob.selected() ) {
                    _settings.queueBrowser().removeJobFromSchedule( thisJob );
                    thisJob.removeFromDatabase();
                    removeChild( thisJob );
                }
            }
        } catch ( java.util.ConcurrentModificationException e ) {}
    }
    
    public void type( String newVal ) {
        _type = newVal;
        setLabelText();
    }
    
    public String type() {
        return _type;
    }
    
    protected void setLabelText() {
        _label.setText( _name + " (" + _type + ")" );
    }
    
    public void name( String newVal ) {
        _name = newVal;
        setLabelText();
    }
    @Override
    public String name() { return _name; }
    
    protected void productionItemAction() {
        _productionItem.setSelected( true );
        _clockItem.setSelected( false );
        _testItem.setSelected( false );
        type( "production" );
        changeTypeInDatabase();
    }
    
    protected void clockItemAction() {
        _productionItem.setSelected( false );
        _clockItem.setSelected( true );
        _testItem.setSelected( false );
        type( "clock" );
        changeTypeInDatabase();
    }
    
    protected void testItemAction() {
        _productionItem.setSelected( false );
        _clockItem.setSelected( false );
        _testItem.setSelected( true );
        type( "test" );
        changeTypeInDatabase();
    }
    
    /*
     * Change the pass type in the database.  This is done using a pass type ID, 
     * which we have to look up in the database first.
     */
    public void changeTypeInDatabase() {
        if ( this.inDatabase() ) {
            QueueDBConnection db = null;
            if ( _settings.useDatabase() ) {
                db = new QueueDBConnection( _settings );
                if ( db.connected() ) {
                    Integer passId = _settings.passTypeID( _type );
                    updateDatabase( "passTypeID", passId.toString() );
                }
            }
        }
    }
    
    /*
     * This is a generic database update function for this object.  It will change
     * a specific field to a specific value - both are strings.
     */
    public void updateDatabase( String param, String setting ) {
        if ( this.inDatabase() ) {
            QueueDBConnection db = null;
            if ( _settings.useDatabase() ) {
                db = new QueueDBConnection( _settings );
                if ( db.connected() ) {
                    db.updatePass( _id, param, setting );
                }
            }
        }
    }

    public ExperimentNode experimentNode() {
        return _experimentNode;
    }
    public void experimentNode( ExperimentNode newNode ) {
        _experimentNode = newNode;
    }
    
    public void logFile( ActivityLogFile logFile ) { _logFile = logFile; }
    public ActivityLogFile logFile() { return _logFile; }
    
    public void fullPath( String newPath ) { _fullPath = newPath; }
    public String fullPath() { return _fullPath; }
    
    protected ExperimentNode _experimentNode;
    protected String _name;
    protected String _type;
    protected JCheckBoxMenuItem _productionItem;
    protected JCheckBoxMenuItem _clockItem;
    protected JCheckBoxMenuItem _testItem;
    protected JTextField _nameEditor;
    protected SystemSettings _settings;
    protected JMenu _typeMenu;
    protected ArrayList<JCheckBoxMenuItem> _checkList;
    protected ActivityLogFile _logFile;
    protected String _fullPath;
    protected ColumnTextArea _stateLabel;
    
}
