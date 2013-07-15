/*
 * This panel is contained in the ExperimentEditor, and is meant to hold all of
 * the information about a station.  It was originally developed as a class
 * entirely contained within the ExperimentEditor class, which makes sense (as that's
 * the only place it is used), but Netbeans continually crashes when confronted
 * with files that are above a few hundred lines.  Something to do with the Mac
 * implementation of Java.  Anyway, this is the only way to maintain sanity.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.SaneTextField;
import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.NumberBox;

import edu.nrao.difx.difxutilities.DiFXCommand_ls;

import javax.swing.JLabel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JTextField;
import javax.swing.JButton;

import java.awt.Color;
import java.awt.Cursor;

import java.util.Iterator;
import java.util.ArrayList;

import edu.nrao.difx.difxutilities.DiFXCommand;
import edu.nrao.difx.xmllib.difxmessage.DifxGetDirectory;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.event.PopupMenuListener;
import javax.swing.event.PopupMenuEvent;

import java.io.DataInputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;

import mil.navy.usno.widgetlib.NodeBrowserScrollPane;
import mil.navy.usno.widgetlib.IndexedPanel;

import javax.swing.event.EventListenerList;

public class StationPanel extends IndexedPanel {

    public StationPanel( VexFileParser.Station station, SystemSettings settings ) {
        super( station.name );
        _settings = settings;
        _this = this;
        _changeListeners = new EventListenerList();
        this.closedHeight( 20 );
        this.open( false );
        this.darkTitleBar( false );
        this.drawFrame( false );
        this.resizeOnTopBar( true );
        _useCheck = new JCheckBox( "" );
        _useCheck.setBounds( 100, 2, 18, 16 );
        _useCheck.setSelected( true );
        _useCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                dispatchChangeCallback();
            }
        } );
        this.add( _useCheck );
        _contentPane = new NodeBrowserScrollPane();
        _contentPane.setLevel( 2 );
        _contentPane.drawFrame( false );
        _contentPane.respondToResizeEvents( true );
        _contentPane.noTimer();
        this.addScrollPane( _contentPane );
        
        //  The data source panel lets the user specify where data for this station
        //  come from.
        _dataSourcePanel = new IndexedPanel( "Data Source: not set" );
        _dataSourcePanel.closedHeight( 20 );
        _dataSourcePanel.open( false );
        _dataSourcePanel.drawFrame( false );
        _dataSourcePanel.resizeOnTopBar( true );
        _contentPane.addNode( _dataSourcePanel );
        //  The data format applies to all data sources.
        String defaultdataFormat = _settings.dataFormat();
        _dataFormat = new JComboBox();
        _dataFormat.setBounds( 200, 30, 180, 25 );
        _dataFormat.setToolTipText( "Format of the data, regardless of source." );
        _dataFormat.setEditable( true );
        //  This little bit causes a typed-in item to be treated as a format.
        _dataFormat.getEditor().addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  If not already in the list of VSNs, add this name.
                if ( !_settings.inModuleFormatList( (String)_dataFormat.getEditor().getItem() ) ) {
                    if ( ((String)_dataFormat.getEditor().getItem()).length() > 0 )
                        _settings.addModuleFormat( (String)_dataFormat.getEditor().getItem() );
                }
                dispatchChangeCallback();
            }
        });
        _dataFormat.setBackground( Color.WHITE );
        _dataFormat.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                dispatchChangeCallback();
            }
        });
        //  Put current items in the popup menu and set the current selection to match the
        //  default.
        int index = 0;
        int selectionIndex = 0;
        for ( Iterator<String> iter2 = _settings.moduleFormatList().iterator(); iter2.hasNext(); ) {
            String thisItem = iter2.next();
            _dataFormat.addItem( thisItem );
            if ( thisItem.contentEquals( defaultdataFormat ) )
                selectionIndex = index;
            ++index;
        }
        _dataFormat.setSelectedIndex( selectionIndex );
        //  This causes the popup menu to be rebuilt each time the button is hit.
        //  Hopefully this is quick!
        _dataFormat.addPopupMenuListener( new PopupMenuListener() {
            public void popupMenuWillBecomeVisible( PopupMenuEvent e ) {
                //  Save the current item so we can make it the choice of the new, rebuilt
                //  popup.
                String currentItem = dataFormat();
                _dataFormat.removeAllItems();
                for ( Iterator<String> iter = _settings.moduleFormatList().iterator(); iter.hasNext(); )
                    _dataFormat.addItem( iter.next() );
                _dataFormat.setSelectedItem( currentItem );
            }
            public void popupMenuCanceled( PopupMenuEvent e ) {
                //System.out.println( "canceled" );
            }
            public void popupMenuWillBecomeInvisible( PopupMenuEvent e ) {
                //System.out.println( "make invisible" );
            }
        });
        _dataSourcePanel.add( _dataFormat );
        JLabel dataFormatLabel = new JLabel( "Data Format: " );
        dataFormatLabel.setBounds( 100, 30, 95, 25 );
        dataFormatLabel.setHorizontalAlignment( JLabel.RIGHT );
        _dataSourcePanel.add( dataFormatLabel );
        _vsnCheck = new JCheckBox( "" );
        _vsnCheck.setBounds( 200, 60, 25, 25 );
        _vsnCheck.setSelected( false );
        _vsnCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                setEnabledItems( _vsnCheck );
                dispatchChangeCallback();
            }
        } );
        _dataSourcePanel.add( _vsnCheck );
        JLabel vsnLabel = new JLabel( "Module: " );
        vsnLabel.setBounds( 100, 60, 95, 25 );
        vsnLabel.setHorizontalAlignment( JLabel.RIGHT );
        _dataSourcePanel.add( vsnLabel );
        _vsnList = new JComboBox();
        _vsnList.setBounds( 230, 60, 150, 25 );
        _vsnList.setToolTipText( "VSN of module containing data for this antenna." );
        _vsnList.setEditable( true );
        //  This little bit causes a typed-in item to be treated as a module name.
        _vsnList.getEditor().addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _dataSourcePanel.name( "Data Source: " + (String)_vsnList.getEditor().getItem() );
                //  If not already in the list of VSNs, add this name.
                if ( !_settings.dataSourceInList( (String)_vsnList.getEditor().getItem(), "VSN" ) ) {
                    if ( ((String)_vsnList.getEditor().getItem()).length() > 0 )
                        _settings.addDataSource( (String)_vsnList.getEditor().getItem(), "VSN", "hardware" );
                }

                //dispatchChangeCallback();
            }
        });
        _vsnList.setBackground( Color.WHITE );
        _vsnList.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _dataSourcePanel.name( "Data Source: " + (String)_vsnList.getSelectedItem() );
                getDirectory();
                dispatchChangeCallback();
            }
        });
        _dataSourcePanel.add( _vsnList );
        _vsnList.setEnabled( true );
        _dirListLocation = new SaneTextField();
        _dirListLocation.setToolTipText( "Location on the Mark5 of the file containing a directory listing for this module." );
//        _dirListLocation.addActionListener( new ActionListener() {
//            public void actionPerformed( ActionEvent e ) {
//                _dataSourcePanel.name( "Data Source: " + (String)_vsnList.getSelectedItem() );
//                _settings.defaultNames().dirListLocation = _dirListLocation.getText();
//            }
//        });
//        _dirListLocation.setText( _settings.defaultNames().dirListLocation );
        _dirListLocation.setEditable( false );
        _dirListLocation.setText( "" );
//        _dirListLocation.addActionListener( new ActionListener() {
//            public void actionPerformed( ActionEvent e ) {
//                dispatchChangeCallback();
//            }
//        });
        _dataSourcePanel.add( _dirListLocation );
        JLabel dirListLabel = new JLabel( "Directory:" );
        dirListLabel.setBounds( 390, 60, 85, 25 );
        dirListLabel.setHorizontalAlignment( JLabel.RIGHT );
        _dataSourcePanel.add( dirListLabel );
        Iterator<SystemSettings.DataSource> iter = _settings.listDataSources( "VSN" ).iterator();
        for ( ; iter.hasNext(); )
            _vsnList.addItem( iter.next().name );
        _fileCheck = new JCheckBox( "" );
        _fileCheck.setBounds( 200, 120, 25, 25 );
        _fileCheck.setSelected( false );
        _fileCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                setEnabledItems( _fileCheck );
                dispatchChangeCallback();
            }
        } );
        _dataSourcePanel.add( _fileCheck );
        JLabel fileLabel = new JLabel( "Files: " );
        fileLabel.setBounds( 100, 120, 95, 25 );
        fileLabel.setHorizontalAlignment( JLabel.RIGHT );
        _dataSourcePanel.add( fileLabel );
        _fileFilter = new SaneTextField();
        _fileFilter.setEnabled( false );
        _fileFilter.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                fileFilterCallback();
            }
        } );
        _fileFilter.addTabListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                fileFilterCallback();
            }
        } );
        _dataSourcePanel.add( _fileFilter ); 
        _fileList = new NodeBrowserScrollPane();
        _fileList.setBackground( Color.WHITE );
        _dataSourcePanel.add( _fileList );
        JLabel fileFilterLabel = new JLabel( "Filter:" );
        fileFilterLabel.setHorizontalAlignment( JLabel.RIGHT );
        fileFilterLabel.setBounds( 195, 120, 80, 25 );
        _dataSourcePanel.add( fileFilterLabel );
        _eVLBICheck = new JCheckBox( "" );
        _eVLBICheck.setBounds( 200, 90, 25, 25 );
        _eVLBICheck.setSelected( false );
        _eVLBICheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                setEnabledItems( _eVLBICheck );
                dispatchChangeCallback();
            }
        } );
        _dataSourcePanel.add( _eVLBICheck );
        JLabel eVLBILabel = new JLabel( "Network: " );
        eVLBILabel.setBounds( 100, 90, 95, 25 );
        eVLBILabel.setHorizontalAlignment( JLabel.RIGHT );
        _dataSourcePanel.add( eVLBILabel );
        //  Default setup.  This should come from the SystemSettings class so
        //  it is saved between runs.
        _eVLBIPort = new NumberBox();
        _eVLBIPort.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                setEnabledItems( _eVLBICheck );
                dispatchChangeCallback();
            }
        } );
        _eVLBIPort.setToolTipText( "The port number used for network transfer of this station's data." );
        _eVLBIPort.setBounds( 230, 90, 150, 25 );
        _eVLBIPort.precision( 0 );
        _eVLBIPort.intValue( 5000 );
        _eVLBIPort.minimum( 0 );
        _dataSourcePanel.add( _eVLBIPort );
        setEnabledItems( null );
        
        //  The antenna panel contains information about the antenna - mount, offsets,
        //  size, etc.  This is filled in by a function call.
        _antennaPanel = new IndexedPanel( "Antenna: " + station.antenna );
        _antennaPanel.closedHeight( 20 );
        _antennaPanel.openHeight( 20 );
        _antennaPanel.open( false );
        _antennaPanel.drawFrame( false );
        _antennaPanel.resizeOnTopBar( true );
        _contentPane.addNode( _antennaPanel );
        
        //  The site panel contains information about the physical site....location,
        //  etc.  The content of this panel is added by a function call.
        _sitePanel = new IndexedPanel( "Site: " + station.site );
        _sitePanel.closedHeight( 20 );
        _sitePanel.openHeight( 105 );
        _sitePanel.open( false );
        _sitePanel.drawFrame( false );
        _sitePanel.resizeOnTopBar( true );
        _contentPane.addNode( _sitePanel );
        
        //  The Settings panel contains a bunch of things that don't fit into the
        //  above panels.
        _settingsPanel = new IndexedPanel( "Settings" );
        _settingsPanel.closedHeight( 20 );
        _settingsPanel.openHeight( 120 );
        _settingsPanel.open( false );
        _settingsPanel.drawFrame( false );
        _settingsPanel.resizeOnTopBar( true );
        _contentPane.addNode( _settingsPanel );
        //  Get the default value of the tone selection from the settings...we have
        //  to do it here because some of the callbacks below will mess it up.
        String defaultToneSelection = _settings.toneSelection();
        _toneSelection = new JComboBox();
        _toneSelection.setBounds( 150, 30, 120, 25 );
        _toneSelection.setToolTipText( "Use an algorithm to choose tones for you." );
        _toneSelection.setEditable( true );
        //  This little bit causes a typed-in item to be treated as a format.
        _toneSelection.getEditor().addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                //  If not already in the list of tones, add this name.
                if ( !_settings.inToneSelectionList( (String)_toneSelection.getEditor().getItem() ) ) {
                    if ( ((String)_toneSelection.getEditor().getItem()).length() > 0 )
                        _settings.addToneSelection( (String)_toneSelection.getEditor().getItem() );
                }
                _settings.toneSelection( toneSelection() );
                dispatchChangeCallback();
            }
        });
        _toneSelection.setBackground( Color.WHITE );
        _toneSelection.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.toneSelection( toneSelection() );
                dispatchChangeCallback();
            }
        });
        //  Put current items in the popup menu and set the current selection to match the
        //  default.
        index = 0;
        selectionIndex = 0;
        for ( Iterator<String> iter2 = _settings.toneSelectionList().iterator(); iter2.hasNext(); ) {
            String thisItem = iter2.next().trim();
            _toneSelection.addItem( thisItem );
            if ( thisItem.contentEquals( defaultToneSelection ) )
                selectionIndex = index;
            ++index;
        }
        _toneSelection.setSelectedIndex( selectionIndex );
        //  This causes the popup menu to be rebuilt each time the button is hit.
        //  Hopefully this is quick!
        _toneSelection.addPopupMenuListener( new PopupMenuListener() {
            public void popupMenuWillBecomeVisible( PopupMenuEvent e ) {
                //  Save the current item so we can make it the choice of the new, rebuilt
                //  popup.
                String currentItem = toneSelection();
                _toneSelection.removeAllItems();
                for ( Iterator<String> iter = _settings.toneSelectionList().iterator(); iter.hasNext(); )
                    _toneSelection.addItem( iter.next() );
                _toneSelection.setSelectedItem( currentItem );
            }
            public void popupMenuCanceled( PopupMenuEvent e ) {
                //System.out.println( "canceled" );
            }
            public void popupMenuWillBecomeInvisible( PopupMenuEvent e ) {
                //System.out.println( "make invisible" );
            }
        });
        _settingsPanel.add( _toneSelection );
        JLabel toneSelectionLabel = new JLabel( "Tone Selection:" );
        toneSelectionLabel.setBounds( 10, 30, 135, 25 );
        toneSelectionLabel.setHorizontalAlignment( JLabel.RIGHT );
        _settingsPanel.add( toneSelectionLabel );
        _phaseCalInt = new NumberBox();
        _phaseCalInt.precision( 0 );
        _phaseCalInt.minimum( 0.0 );
        _phaseCalInt.intValue( _settings.phaseCalInt() );
        _phaseCalInt.setBounds( 150, 60, 80, 25 );
        _phaseCalInt.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _settings.phaseCalInt( _phaseCalInt.intValue() );
                dispatchChangeCallback();
            }
        });
        _settingsPanel.add( _phaseCalInt );
        JLabel phaseCalIntLabel = new JLabel( "Phase Cal Interval:" );
        phaseCalIntLabel.setBounds( 10, 60, 135, 25 );
        phaseCalIntLabel.setHorizontalAlignment( JLabel.RIGHT );
        _settingsPanel.add( phaseCalIntLabel );
        _deltaClock = new NumberBox();
        _deltaClock.setBounds( 150, 90, 120, 25 );
        _deltaClock.precision( 3 );
        _deltaClock.value( 0.0 );
        _deltaClock.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                dispatchChangeCallback();
            }
        } );
        _settingsPanel.add( _deltaClock );
        JLabel deltaClockLabel = new JLabel( "Delta Clock (\u03bcs):" );
        deltaClockLabel.setBounds( 10, 90, 135, 25 );
        deltaClockLabel.setHorizontalAlignment( JLabel.RIGHT );
        _settingsPanel.add( deltaClockLabel );
        
    }

    /*
     * This function is used to make things visible/enabled/etc as
     * fits each data source choice.  The selected data source check box is
     * used to determine which was selected.
     */
    protected void setEnabledItems( JCheckBox selector ) {
        //  Turn everything off first...
        _vsnCheck.setSelected( false );
        _fileCheck.setSelected( false );
        _eVLBICheck.setSelected( false );
        _vsnList.setEnabled( false );
        _dirListLocation.setEnabled( false );
        _fileFilter.setEnabled( false );
        _fileList.setVisible( false );
        _dataSourcePanel.staticHeight( 135 );
        //  Then turn appropriate stuff back on.
        if ( selector == _vsnCheck ) {
            _vsnCheck.setSelected( true );
            _vsnList.setEnabled( true );
            _dirListLocation.setEnabled( true );
            _dataSourcePanel.name( "Data Source: " + (String)_vsnList.getSelectedItem() );
        }
        else if ( selector == _fileCheck ) {
            _fileCheck.setSelected( true );
            //  Show the selection of the file list if there are any items in it.
            Iterator<BrowserNode> iter = _fileList.browserTopNode().childrenIterator();
            if ( iter.hasNext() )
                _dataSourcePanel.name( "Data Source: files " + _fileFilter.getText().trim() + "*" );
            else
                _dataSourcePanel.name( "Data Source: unspecified files" );
            _fileFilter.setEnabled( true );
            _fileList.setVisible( true );
            _dataSourcePanel.staticHeight( 265 );
        }
        else if ( selector == _eVLBICheck ) {
            _eVLBICheck.setSelected( true );
            _dataSourcePanel.name( "Data Source: network (port " + networkPort() + ")" );
        }
        else {
            _dataSourcePanel.name( "Data Source: not set" );
        }
        this.updateUI();
    }
    
    /*
     * Use site information to create and populate the site panel.
     */
    public void addSiteInformation( VexFileParser.Site site ) {
        JLabel siteName = new JLabel( site.name );
        siteName.setBounds( 150, 25, 100, 25 );
        _sitePanel.add( siteName );
        JLabel siteNameLabel = new JLabel( "Name:" );
        siteNameLabel.setBounds( 60, 25, 85, 25 );
        siteNameLabel.setHorizontalAlignment( JLabel.RIGHT );
        _sitePanel.add( siteNameLabel );
        JLabel siteId = new JLabel( site.id );
        siteId.setBounds( 150, 50, 100, 25 );
        _sitePanel.add( siteId );
        JLabel siteIdLabel = new JLabel( "ID:" );
        siteIdLabel.setBounds( 60, 50, 85, 25 );
        siteIdLabel.setHorizontalAlignment( JLabel.RIGHT );
        _sitePanel.add( siteIdLabel );
        JLabel siteType = new JLabel( site.type );
        siteType.setBounds( 150, 75, 100, 25 );
        _sitePanel.add( siteType );
        JLabel siteTypeLabel = new JLabel( "Type:" );
        siteTypeLabel.setBounds( 60, 75, 85, 25 );
        siteTypeLabel.setHorizontalAlignment( JLabel.RIGHT );
        _sitePanel.add( siteTypeLabel );
        //  Parse out the x, y, and z positions from the site position string.
        String posString = site.position.trim();
        int end = posString.indexOf( 'm' );
        _xpos = Double.parseDouble( posString.substring( 0, end ).trim() );
        posString = posString.substring( end + 3 ).trim();
        end = posString.indexOf( 'm' );
        _ypos = Double.parseDouble( posString.substring( 0, end ).trim() );
        posString = posString.substring( end + 3 ).trim();
        end = posString.indexOf( 'm' );
        _zpos = Double.parseDouble( posString.substring( 0, end ).trim() );
        _positionX = new NumberBox();
        _positionX.setBounds( 320, 25, 120, 25 );
        _sitePanel.add( _positionX );
        _positionX.value( _xpos );
        _positionX.precision( 3 );
        _positionX.setHorizontalAlignment( JTextField.RIGHT );
        _positionX.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                dispatchChangeCallback();
            }
        } );
        JLabel positionXLabel = new JLabel( "X Position (m):" );
        positionXLabel.setBounds( 220, 25, 95, 25 );
        positionXLabel.setHorizontalAlignment( JLabel.RIGHT );
        _sitePanel.add( positionXLabel );
        JButton resetX = new JButton( "Reset" );
        resetX.setToolTipText( "Reset the X position to its original value." );
        resetX.setBounds( 445, 25, 80, 24 );
        resetX.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                _positionX.value( _xpos );
                dispatchChangeCallback();
            }
        } );
        _sitePanel.add( resetX );
        _positionY = new NumberBox();
        _positionY.setBounds( 320, 50, 120, 25 );
        _sitePanel.add( _positionY );
        _positionY.value( _ypos );
        _positionY.precision( 3 );
        _positionY.setHorizontalAlignment( JTextField.RIGHT );
        _positionY.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                dispatchChangeCallback();
            }
        } );
        JLabel positionYLabel = new JLabel( "Y Position (m):" );
        positionYLabel.setBounds( 220, 50, 95, 25 );
        positionYLabel.setHorizontalAlignment( JLabel.RIGHT );
        _sitePanel.add( positionYLabel );
        JButton resetY = new JButton( "Reset" );
        resetY.setToolTipText( "Reset the Y position to its original value." );
        resetY.setBounds( 445, 50, 80, 24 );
        resetY.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                _positionY.value( _ypos );
                dispatchChangeCallback();
            }
        } );
        _sitePanel.add( resetY );
        _positionZ = new NumberBox();
        _positionZ.setBounds( 320, 75, 120, 25 );
        _sitePanel.add( _positionZ );
        _positionZ.value( _zpos );
        _positionZ.precision( 3 );
        _positionZ.setHorizontalAlignment( JTextField.RIGHT );
        _positionZ.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                dispatchChangeCallback();
            }
        } );
        JLabel positionZLabel = new JLabel( "Z Position (m):" );
        positionZLabel.setBounds( 220, 75, 95, 25 );
        positionZLabel.setHorizontalAlignment( JLabel.RIGHT );
        _sitePanel.add( positionZLabel );
        JButton resetZ = new JButton( "Reset" );
        resetZ.setToolTipText( "Reset the Z position to its original value." );
        resetZ.setBounds( 445, 75, 80, 24 );
        resetZ.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                _positionZ.value( _zpos );
                dispatchChangeCallback();
            }
        } );
        _sitePanel.add( resetZ );
    }

    /*
     * Use site information to create and populate the site panel.
     */
    public void addAntennaInformation( VexFileParser.Antenna antenna ) {
    }

    /*
     * Change things to match a new width.
     */
    public void newWidth( int w ) {
        _fileFilter.setBounds( 280, 120, w - 305, 25 );
        _fileList.setBounds( 230, 155, w - 255, 120 );
        _dirListLocation.setBounds( 480, 60, w - 505, 25 );
        _contentPane.setBounds( 0, 20, w - 2, _contentPane.dataHeight() );
    }

    /*
     * This function is called when the file filter is changed.  It gets a list
     * of files from the DiFX Host that match the current filter.
     */
    public void fileFilterCallback() {
        final ArrayList<String> newList = new ArrayList<String>();
        final Cursor cursor = this.getCursor();
        final IndexedPanel _this = this;
        this.setCursor( Cursor.getPredefinedCursor( Cursor.WAIT_CURSOR ) );
        DiFXCommand_ls ls = null;
        if ( _fileFilter.getText().endsWith( "*" ) )
            ls = new DiFXCommand_ls( _fileFilter.getText().trim(), _settings );
        else
            ls = new DiFXCommand_ls( _fileFilter.getText().trim() + "*", _settings );
        ls.addEndListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _this.setCursor( cursor );
                //  Found anything at all?
                if ( newList.size() > 0 ) {
                    Iterator<String> iter = newList.iterator();
                    String commonString = iter.next();
                    clearFileList();
                    addToFileList( commonString, _settings.difxControlAddress() );
                    for ( ; iter.hasNext(); ) {
                        String newStr = iter.next();
                        addToFileList( newStr, _settings.difxControlAddress() );
                        int i = 0;
                        while ( commonString.regionMatches( 0, newStr, 0, i ) )
                            ++i;
                        if ( i > 0 )
                            commonString = commonString.substring( 0, i - 1 );
                        else
                            commonString = "";
                    }
                    //  The common string becomes the new contents of the filter...
                    _fileFilter.setText( commonString );
                    _fileFilter.setCaretPosition( commonString.length() );
                    if ( newList.size() > 1 ) {
                        _dataSourcePanel.name( "Data Source: files " + _fileFilter.getText().trim() + "*" );
                    }
                    else
                        _dataSourcePanel.name( "Data Source: files " + _fileFilter.getText().trim() );
                }
                dispatchChangeCallback();
            }
        });
        ls.addIncrementalListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                newList.add( e.getActionCommand().trim() );
            }
        });
        try {
            ls.send();
        } catch ( java.net.UnknownHostException e ) {
            //  BLAT handle this
        }
    }

    protected class FileListItem extends IndexedPanel {

        FileListItem( String filename, String sourceNode, boolean use ) {
            super( filename );
            this.alwaysOpen( true );
            this.openHeight( 20 );
            this.noArrow( true );
            this.backgroundColor( Color.WHITE );
            this.drawFrame( false );
            _use = new JCheckBox( "" );
            _use.setBounds( 5, 0, 20, 20 );
            _use.setBackground( Color.WHITE );
            //  Any change in the "use" needs to trigger a change event.
            _use.addActionListener( new ActionListener() {
                public void actionPerformed( ActionEvent evt ) {
                    dispatchChangeCallback();
                }
            } );
            if ( use )
                _use.setSelected( true );
            this.add( _use );
            sourceNode( sourceNode );
        }

        public boolean use() { return _use.isSelected(); }
        public void use( boolean newVal ) { _use.setSelected( newVal ); }
        public String sourceNode() { return _sourceNode; }
        public void sourceNode( String newVal ) { _sourceNode = newVal; }

        protected JCheckBox _use;
        protected String _sourceNode;

    }

    /*
     * Add a new item to the list of files - presumably a new file name.
     */
    public void addToFileList( String newFile, String sourceNode ) {
        //  Make sure this doesn't terminate with a "/" character - so we
        //  know its a file.
        if ( newFile.charAt( newFile.length() - 1 ) != '/' )
            _fileList.addNode( new FileListItem( newFile, sourceNode, true ) );
        else
            _fileList.addNode( new FileListItem( newFile, sourceNode, false ) );
    }

    /*
     * Clear all items from the list of files.
     */
    public void clearFileList() {
        _fileList.browserTopNode().clearChildren();
    }

    /*
     * Return a list of all filenames listed in the file list.  Only those
     * items with the "use" check are listed.
     */
    public ArrayList<String> fileList() {
        ArrayList<String> newList = new ArrayList<String>();
        for ( Iterator<BrowserNode> iter = _fileList.browserTopNode().childrenIterator(); iter.hasNext(); ) {
            FileListItem newItem = (FileListItem)iter.next();
            if ( newItem.use() )
                newList.add( newItem.name() );
        }
        return newList;
    }

    public void useFile( String newFile ) {
        for ( Iterator<BrowserNode> iter = _fileList.browserTopNode().childrenIterator(); iter.hasNext(); ) {
            FileListItem newItem = (FileListItem)iter.next();
            if ( newItem.name().contentEquals( newFile ) ) {
                newItem.use( true );
                return;
            }
        }
        _fileList.addNode( new FileListItem( newFile, null, true ) );

    }

    /*
     * Return the machine name associated with the given file item.
     */
    public String machineForFile( String filename ) {
        try {
            for ( Iterator<BrowserNode> iter = _fileList.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                FileListItem newItem = (FileListItem)iter.next();
                if ( newItem.name().contentEquals( filename ) )
                    return newItem.sourceNode();
            }
        }
        catch ( java.util.ConcurrentModificationException e ) {}
        return null;
    }
    
    public void vsnSource( String name ) {
        for ( int i = 0; i < _vsnList.getItemCount(); ++i ) {
            if ( name.contentEquals( (String)_vsnList.getItemAt( i ) ) ) {
                _vsnList.setSelectedIndex( i );
                return;
            }
        }
        _vsnList.addItem( name );
        _vsnList.setSelectedIndex( _vsnList.getItemCount() - 1 );
    }

    public boolean use() { return _useCheck.isSelected(); }
    public void use( boolean newVal ) { _useCheck.setSelected( newVal ); }
    public boolean useVsn() { return _vsnCheck.isSelected(); }
    public void useVsn( boolean newVal ) { setEnabledItems( _vsnCheck ); }
    public boolean useFile() { return _fileCheck.isSelected(); }
    public void useFile( boolean newVal ) { setEnabledItems( _fileCheck ); }
    public boolean useEVLBI() { return _eVLBICheck.isSelected(); }
    public void useEVLBI( boolean newVal ) { setEnabledItems( _eVLBICheck ); }
    public String vsnSource() { return (String)_vsnList.getSelectedItem(); }
    public String dataFormat() { return (String)_dataFormat.getSelectedItem(); }
    public void dataFormat( String newVal ) {
        for ( int i = 0; i < _dataFormat.getItemCount(); ++i ) {
            if ( newVal.contentEquals( (String)_dataFormat.getItemAt( i ) ) ) {
                _dataFormat.setSelectedIndex( i );
                return;
            }
        }
    }
    public String toneSelection() { return (String)_toneSelection.getSelectedItem(); }
    public void toneSelection( String newVal ) {
        for ( int i = 0; i < _toneSelection.getItemCount(); ++i ) {
            if ( newVal.contentEquals( (String)_toneSelection.getItemAt( i ) ) ) {
                _toneSelection.setSelectedIndex( i );
                return;
            }
        }
    }
    public String dirListLocation() { return _dirListLocation.getText(); }
    public int phaseCalInt() { return _phaseCalInt.intValue(); }
    public void phaseCalInt( int newVal ) { _phaseCalInt.intValue( newVal ); }
    public int networkPort() { return _eVLBIPort.intValue(); }
    public void networkPort( int newVal ) { _eVLBIPort.intValue( newVal ); }
    
    public boolean positionChange() {
        if ( _positionX.value() != _xpos ||
             _positionY.value() != _ypos ||
             _positionZ.value() != _zpos )
            return true;
        else
            return false;
    }
    public boolean positionXChange() {
        if ( _positionX == null)
            return false;
        return ( _positionX.value() != _xpos );
    }
    public boolean positionYChange() {
        if ( _positionY == null)
            return false;
        return ( _positionY.value() != _ypos );
    }
    public boolean positionZChange() {
        if ( _positionZ == null)
            return false;
        return ( _positionZ.value() != _zpos );
    }
    public Double positionX() { return _positionX.value(); }
    public void positionX( double newVal ) { _positionX.value( newVal ); }
    public Double positionY() { return _positionY.value(); }
    public void positionY( double newVal ) { _positionY.value( newVal ); }
    public Double positionZ() { return _positionZ.value(); }
    public void positionZ( double newVal ) { _positionZ.value( newVal ); }
    
    public boolean deltaClockChange() {
        return ( _deltaClock.value() != 0.0 );
    }
    public Double deltaClock() { return _deltaClock.value(); }
    public void deltaClock( double newVal ) { _deltaClock.value( newVal ); }

    /*
     * Add a "listener" to callbacks when any changes to button states occur.
     */
    public void addChangeListener( ActionListener a ) {
        _changeListeners.add( ActionListener.class, a );
    }

    protected void dispatchChangeCallback() {
        Object[] listeners = _changeListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }

    /*
     * Get the full path to a directory for the current VSN.  This will be done in a thread.
     * This is a limited version of what the DirectoryDisplay does.
     */
    public void getDirectory() {
        if ( vsnHost( (String)_vsnList.getSelectedItem() ) == null ) {
            _dirListLocation.setText( "no Mark5 for VSN" );
            _dirListLocation.setForeground( Color.RED );
            return;
        }
            
        //  Construct a Get Directory command.
        DiFXCommand command = new DiFXCommand( _settings );
        command.header().setType( "DifxGetDirectory" );
        command.mpiProcessId( "-1" );
        command.identifier( "gui" );
        int monitorPort = 0;
        DifxGetDirectory cmd = command.factory().createDifxGetDirectory();
        cmd.setMark5( vsnHost( (String)_vsnList.getSelectedItem() ) );
        cmd.setVsn( (String)_vsnList.getSelectedItem() );
        cmd.setDifxVersion( _settings.difxVersion() );

        // If we are using the TCP connection, set the address and port for diagnostic
        // reporting.
        if ( _settings.sendCommandsViaTCP() ) {
            cmd.setAddress( _settings.guiServerConnection().myIPAddress() );
            monitorPort = _settings.newDifxTransferPort();
            cmd.setPort( monitorPort );
        }
        
        //  Set up a monitor thread to collect and interpret diagnostic messages from
        //  guiServer as it sets up the threads and machine files.
        GetDirectoryMonitor monitor = null;
        if ( _settings.sendCommandsViaTCP() ) {
            monitor = new GetDirectoryMonitor( monitorPort );
            monitor.start();
        }
        
        // -- Create the XML defined messages and process through the system
        command.body().setDifxGetDirectory( cmd );
        try {
            //command.sendPacket( _settings.guiServerConnection().COMMAND_PACKET );
            command.send();
        } catch ( java.net.UnknownHostException e ) {
            java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, null,
                    e.getMessage() );  //BLAT should be a pop-up
        }    
    }
    
    /*
     * Thread to receive information from a "get directory" request.  Here we are only
     * interested in the full path name and whether the request fails.  A more complete
     * use of the available information is done in the DirectoryDisplay class.
     */
    protected class GetDirectoryMonitor extends Thread {
        
        public GetDirectoryMonitor( int port ) {
            _dirListLocation.setText( "" );
            _dirListLocation.setForeground( Color.GREEN );
            _port = port;
        }
        
        //  Information packets - most we ignore.
        protected final int GETDIRECTORY_STARTED                = 301;
        protected final int GETDIRECTORY_COMPLETED              = 302;
        protected final int GETDIRECTORY_FULLPATH               = 303;
        protected final int GETDIRECTORY_FAILED                 = 304;
        protected final int MPIRUN_ERROR                        = 305;
        protected final int NO_ENVIRONMENT_VARIABLE             = 306;
        protected final int GETDIRECTORY_DATE                   = 307;
        protected final int FILE_NOT_FOUND                      = 308;
        protected final int GETDIRECTORY_FILESTART              = 309;
        protected final int GETDIRECTORY_FILEDATA               = 310;
        
        @Override
        public void run() {
            //  Open a new server socket and await a connection.  The connection
            //  will timeout after a given number of seconds (nominally 10).
            try {
                ServerSocket ssock = new ServerSocket( _port );
                ssock.setSoTimeout( 10000 );  //  timeout is in millisec
                try {
                    Socket sock = ssock.accept();
                    //  Turn the socket into a "data stream", which has useful
                    //  functions.
                    DataInputStream in = new DataInputStream( sock.getInputStream() );
                    
                    //  Loop collecting diagnostic packets from the guiServer.  These
                    //  are identified by an initial integer, and then are followed
                    //  by a data length, then data.
                    boolean connected = true;
                    while ( connected ) {
                        //  Read the packet type as an integer.  The packet types
                        //  are defined above (within this class).
                        int packetType = in.readInt();
                        //  Read the size of the incoming data (bytes).
                        int packetSize = in.readInt();
                        //  Read the data (as raw bytes)
                        byte [] data = null;
                        if ( packetSize > 0 ) {
                            data = new byte[packetSize];
                            in.readFully( data );
                        }
                        //  Interpret the packet type.
                        if ( packetType == GETDIRECTORY_STARTED ) {
                            connected = true;
                        }
                        else if ( packetType == GETDIRECTORY_COMPLETED ) {
                            connected = false;
                        }
                        else if ( packetType == GETDIRECTORY_FULLPATH ) {
                            _dirListLocation.setText( new String( data ) );
                            _dirListLocation.setForeground( Color.BLACK );
                        }
                        else if ( packetType == GETDIRECTORY_FAILED ) {
                        }
                        else if ( packetType == MPIRUN_ERROR ) {
                        }
                        else if ( packetType == NO_ENVIRONMENT_VARIABLE ) {
                            _dirListLocation.setText( "No MARK5_DIR_PATH environment variable on " + vsnHost( (String)_vsnList.getSelectedItem() ) );
                            _dirListLocation.setForeground( Color.RED );
                        }
                        else if ( packetType == GETDIRECTORY_DATE ) {
                        }
                        else if ( packetType == FILE_NOT_FOUND ) {
                            _dirListLocation.setText( "File not found on " + vsnHost( (String)_vsnList.getSelectedItem() ) );
                            _dirListLocation.setForeground( Color.RED );
                        }
                        else if ( packetType == GETDIRECTORY_FILESTART ) {
                        }
                        else if ( packetType == GETDIRECTORY_FILEDATA ) {
                        }
                    }
                    sock.close();
                    dispatchChangeCallback();
                } catch ( SocketTimeoutException e ) {
                }
                ssock.close();
            } catch ( java.io.IOException e ) {
                e.printStackTrace();
            }
            _settings.releaseTransferPort( _port );
        }
        
        protected int _port;
        
    }
    
    /*
     * From a VSN string, get the Mark5 that holds it, if available.
     */
    public String vsnHost( String VSN ) {
        for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor().mk5Modules().children().iterator();
                iter.hasNext(); ) {
            Mark5Node thisModule = (Mark5Node)(iter.next());
            if ( thisModule.bankAVSN().trim().contentEquals( VSN ) ||
                 thisModule.bankBVSN().trim().contentEquals( VSN ) ) {
                //  Found it!
                return thisModule.name();
            }
        }
        return null;
    }
    
    protected JCheckBox _useCheck;
    protected JCheckBox _vsnCheck;
    protected JCheckBox _fileCheck;
    protected JCheckBox _eVLBICheck;
    protected NumberBox _eVLBIPort;
    protected JComboBox _vsnList;
    protected JComboBox _dataFormat;
    protected JLabel _dataSource;
    protected EventListenerList _changeListeners;
    protected SaneTextField _fileFilter;
    protected NodeBrowserScrollPane _fileList;
    protected StationPanel _this;
    protected SaneTextField _dirListLocation;
    
    protected SystemSettings _settings;
    
    protected NodeBrowserScrollPane _contentPane;
    protected IndexedPanel _dataSourcePanel;
    protected IndexedPanel _antennaPanel;
    protected IndexedPanel _sitePanel;
    protected IndexedPanel _settingsPanel;
    
    protected NumberBox _positionX;
    protected NumberBox _positionY;
    protected NumberBox _positionZ;
    protected double _xpos;
    protected double _ypos;
    protected double _zpos;
    
    protected NumberBox _deltaClock;
    protected JComboBox _toneSelection;
    protected NumberBox _phaseCalInt;

}
