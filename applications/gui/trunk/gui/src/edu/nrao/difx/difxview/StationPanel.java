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
import mil.navy.usno.widgetlib.ZCheckBox;
import mil.navy.usno.widgetlib.ZButton;
import mil.navy.usno.widgetlib.PopupMonitor;

import edu.nrao.difx.difxutilities.DiFXCommand_ls;

import javax.swing.JOptionPane;
import javax.swing.JLabel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JTextField;
import javax.swing.JButton;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Point;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

import java.util.Iterator;
import java.util.ArrayList;
import java.util.GregorianCalendar;

import edu.nrao.difx.difxutilities.DiFXCommand;
import edu.nrao.difx.difxutilities.ChannelServerSocket;
import edu.nrao.difx.xmllib.difxmessage.DifxGetDirectory;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.event.PopupMenuListener;
import javax.swing.event.PopupMenuEvent;

import java.net.SocketTimeoutException;
import java.util.Calendar;
import java.util.Locale;

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
                //  Locate this station in the vex data scans and turn it off within
                //  them.
                if ( _vexData != null ) {
                    for ( Iterator<VexFileParser.Scan> iter = _vexData.scanList().iterator(); iter.hasNext(); ) {
                        VexFileParser.Scan scan = iter.next();
                        for ( Iterator<VexFileParser.ScanStation> iter2 = scan.station.iterator(); iter2.hasNext(); ) {
                            VexFileParser.ScanStation scanStation = iter2.next();
                            if ( scanStation.name.equalsIgnoreCase( _name ) )
                                scanStation.omitFlag = !_useCheck.isSelected();
                        }
                    }
                }
                dispatchChangeCallback();
            }
        } );
        this.add( _useCheck );
        _contentPane = new NodeBrowserScrollPane( false );
        _contentPane.setLevel( 2 );
        _contentPane.drawFrame( false );
        _contentPane.respondToResizeEvents( true );
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
        _dataFormat = new SaneTextField();
        _dataFormat.setText( "N/A" );
        _dataFormat.setToolTipText( "Format specified by the .vex file for this station." );
        _dataFormat.setEditable( false );
        _dataFormat.setBackground( this.getBackground() );
        _dataFormat.setBounds( 200, 30, 180, 25 );
        _dataSourcePanel.add( _dataFormat );
        _useSourceNode = new JCheckBox( "" );
        _useSourceNode.setBounds( 390, 30, 25, 25 );
        _useSourceNode.setSelected( false );
        _useSourceNode.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                if ( _useSourceNode.isSelected() )
                    _sourceNodeChoice.setEnabled( true );
                else
                    _sourceNodeChoice.setEnabled( false );
            }
        } );
        _useSourceNode.setToolTipText( "Check to use a specific node as the source for these data." );
        _dataSourcePanel.add( _useSourceNode );
        JLabel dataNodeLabel = new JLabel( "Node:" );
        dataNodeLabel.setBounds( 415, 30, 60, 25 );
        dataNodeLabel.setHorizontalAlignment( JLabel.RIGHT );
        _dataSourcePanel.add( dataNodeLabel );
        _sourceNodeChoice = new JComboBox();
        _sourceNodeChoice.addPopupMenuListener( new PopupMenuListener() {
            public void popupMenuWillBecomeVisible( PopupMenuEvent e ) {
                String currentItem = (String)_sourceNodeChoice.getSelectedItem();
                _sourceNodeChoice.removeAllItems();
                for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor()._clusterNodes.childrenIterator(); iter.hasNext(); )
                    _sourceNodeChoice.addItem( iter.next().name() );
                for ( Iterator<BrowserNode> iter = _settings.hardwareMonitor()._mk5Modules.childrenIterator(); iter.hasNext(); )
                    _sourceNodeChoice.addItem( iter.next().name() );
                _sourceNodeChoice.setSelectedItem( currentItem );
            }
            public void popupMenuCanceled( PopupMenuEvent e ) {}
            public void popupMenuWillBecomeInvisible( PopupMenuEvent e ) {}
        });
        _sourceNodeChoice.setEnabled( false );
        _dataSourcePanel.add( _sourceNodeChoice );
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
        _fileListCheck = new ZCheckBox( "File List" );
        _fileListCheck.setToolTipText( "Check here if the (single) named file contains a \"file list\"\n"
                + "of data files and their start and stop times.  This will considerably\n"
                + "speed up processing for large data sets with many jobs." );
        _fileListCheck.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                dispatchChangeCallback();
            }
        } );
        _dataSourcePanel.add( _fileListCheck );
        _generateFileList = new ZButton( "Generate FileList" );
        _generateFileList.setToolTipText( "Attempt to generate a file list from the data files listed.\n"
                + "Results will be put in the file name in the \"Filter\" field.\n"
                + "This process may or may not work, depending on the file\n"
                + "format." );
        _generateFileList.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                generateFileList();
            }
        } );
        _dataSourcePanel.add( _generateFileList );
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
        
        //  Set defaults for the data source if those are available in the setting menu.
        String antennaSource = _settings.antennaDefaultSource( station.name );
        if ( antennaSource != null ) {
            _vsnCheck.setSelected( false );
            _fileCheck.setSelected( false );
            _eVLBICheck.setSelected( false );
            if ( antennaSource.contentEquals( "Files" ) ) {
                _fileCheck.setSelected( true );
                setEnabledItems( _fileCheck );
                String antennaDataPath = _settings.antennaDefaultDataPath( station.name );
                if ( antennaDataPath != null ) {
                    _fileFilter.setText( antennaDataPath );
                    fileFilterCallback();
                }
            }
            else if ( antennaSource.contentEquals( "Network" ) ) {
                _eVLBICheck.setSelected( true );
                setEnabledItems( _eVLBICheck );
            }
            else if ( antennaSource.contentEquals( "Module" ) ) {
                _vsnCheck.setSelected( true );
                setEnabledItems( _eVLBICheck );
                String antennaDataPath = _settings.antennaDefaultDataPath( station.name );
                if ( antennaDataPath != null )
                    _vsnList.setSelectedItem( antennaDataPath );
            }
            else
                _vsnCheck.setSelected( true );
        }

        
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
        int index = 0;
        int selectionIndex = 0;
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
        _fileListCheck.setEnabled( false );
        _generateFileList.setEnabled( false );
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
            _fileListCheck.setEnabled( true );
            _generateFileList.setEnabled( true );
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
        _sourceNodeChoice.setBounds( 480, 30, w - 505, 25 );
        _fileFilter.setBounds( 280, 120, w - 560, 25 );
        _fileListCheck.setBounds( w - 105, 120, 100, 25 );
        _generateFileList.setBounds( w - 275, 120, 150, 25 );
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
        synchronized ( _fileList ) {
            //  Make sure this doesn't terminate with a "/" character - so we
            //  know its a file.
            if ( newFile.charAt( newFile.length() - 1 ) != '/' )
                _fileList.addNode( new FileListItem( newFile, sourceNode, true ) );
            else
                _fileList.addNode( new FileListItem( newFile, sourceNode, false ) );
        }
    }

    /*
     * Clear all items from the list of files.
     */
    public void clearFileList() {
        _fileList.browserTopNode().clearChildren();
    }

    public boolean useFileList() { return _fileListCheck.isSelected(); }
    
    public String fileListName() { return _fileFilter.getText(); }

    /*
     * Return a list of all filenames listed in the file list.  Only those
     * items with the "use" check are listed.
     */
    public ArrayList<String> fileList() {
        ArrayList<String> newList = null;
        synchronized ( _fileList ) {
            newList = new ArrayList<String>();
            for ( Iterator<BrowserNode> iter = _fileList.browserTopNode().childrenIterator(); iter.hasNext(); ) {
                FileListItem newItem = (FileListItem)iter.next();
                if ( newItem.use() )
                    newList.add( newItem.name() );
            }
        }
        return newList;
    }

    public void useFile( String newFile ) {
        synchronized ( _fileList ) {
            boolean found = false;
            for ( Iterator<BrowserNode> iter = _fileList.browserTopNode().childrenIterator(); iter.hasNext() && !found; ) {
                FileListItem newItem = (FileListItem)iter.next();
                if ( newItem.name().contentEquals( newFile ) ) {
                    newItem.use( true );
                    found = true;
                }
            }
            if ( !found )
                _fileList.addNode( new FileListItem( newFile, null, true ) );
        }
    }

    /*
     * Return the machine name associated with the given file item.
     */
    public String machineForFile( String filename ) {
        String ret = null;
        synchronized ( _fileList ) {
            for ( Iterator<BrowserNode> iter = _fileList.browserTopNode().childrenIterator(); iter.hasNext() && ret == null; ) {
                FileListItem newItem = (FileListItem)iter.next();
                if ( newItem.name().contentEquals( filename ) )
                    ret = newItem.sourceNode();
            }
        }
        return ret;
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
            monitorPort = _settings.newDifxTransferPort( 0, 100, true, true );
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
                ChannelServerSocket ssock = new ChannelServerSocket( _port, _settings );
                ssock.setSoTimeout( 10000 );  //  timeout is in millisec
                try {
                    ssock.accept();
                    //  Loop collecting diagnostic packets from the guiServer.  These
                    //  are identified by an initial integer, and then are followed
                    //  by a data length, then data.
                    boolean connected = true;
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
    
    //  Set the vex data for this station.
    public void vexData( VexFileParser vexData ) {
        _vexData = vexData;
        ArrayList<String> formats = _vexData.formats( _this.name() );
        if ( formats.size() > 0 ) {
            _dataFormat.setText( formats.get( 0 ) );
            if ( formats.size() > 1 ) {
                _dataFormat.setForeground( Color.RED );
                String str = "Multiple data formats for this station were found in the vex file:\n";
                for ( Iterator<String> iter = formats.iterator(); iter.hasNext(); )
                    str += "     " + iter.next() + "\n";
                str += "This is not a problem for DiFX processing, but generating a filelist\n"
                        + "may not be possible.";
                _dataFormat.setToolTipText( str );
            }
            else {
                _dataFormat.setForeground( Color.BLACK );
                _dataFormat.setToolTipText( "Data format for scans using this station." );
            }
        }
        else {
            _dataFormat.setText( "N/A" );
            _dataFormat.setForeground( Color.RED );
            _dataFormat.setToolTipText( "No data format was located in the vex file.\n"
                    + "This is likely indicative of a problem." );
        }
    }
    
    protected int _messageX;
    protected int _messageY;
    
    /*
     * Using the current filter value as a set of files, instruct guiServer to try to
     * generate a filelist file.  This process will often fail depending on the data,
     * so a pop up window will provide feedback and some control.
     */
    public void generateFileList() {
        //  Get the list of files we want to include in the file list.  This is every
        //  "checked" file in the file list window.
        _listOfFiles = fileList();
        //  Make sure at least one file is included in this list.  Otherwise pop up a
        //  warning.
        if ( _listOfFiles.size() < 1 ) {
            JOptionPane.showMessageDialog( _fileFilter, "No files specified as part of the file list!",
                    "Zero Files", JOptionPane.WARNING_MESSAGE );
            return;
        }
        //  Get a new port for this activity.  If this fails, produce an error.
        int port = _settings.newDifxTransferPort( 0, 100, true, true );
        if ( port == -1 ) {
            JOptionPane.showMessageDialog( _fileFilter, "Unable to allocate transfer port.",
                    "Port Error", JOptionPane.ERROR_MESSAGE );
            return;
        }
        //  Figure out what the name of the file list should be, then prompt the
        //  user to see if its okay.
        String fileListName = _fileFilter.getText().substring( 0, _fileFilter.getText().lastIndexOf( "/" ) );
        fileListName += "/FileList";
        fileListName = JOptionPane.showInputDialog( _fileFilter, "FileList will be written to:", fileListName );
        if ( fileListName == null )
            return;
        //  Create packet data to transmit all we need for the filelist generation
        //  on guiServer.  4 bytes for the port...
        int nbytes = 4;
        //  4 bytes for the number of files to be included.
        nbytes += 4;
        //  4 bytes for length of each file list name, then enough to contain each name.
        for ( Iterator<String> iter = _listOfFiles.iterator(); iter.hasNext(); ) {
            nbytes += 4;
            nbytes += iter.next().length();
        }
        //  Then space to send the "filter" value, which is the destination.
        nbytes += 4;
        nbytes += fileListName.length();
        //  Space for the current format.
        nbytes += 4;
        nbytes += _dataFormat.getText().length();
        //  Space for the reference MJD
        nbytes += 4;
        //  Space for the address.
        nbytes += 4;
        nbytes += _settings.guiServerConnection().myIPAddress().length();
        //  Allocate a properly-sized buffer for the message.
        java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate( nbytes );
        //  Add the number of files to the message.
        bb.putInt( _listOfFiles.size() );
        //  Then each file name, preceded by its character length.
        for ( Iterator<String> iter = _listOfFiles.iterator(); iter.hasNext(); ) {
            String name = iter.next();
            bb.putInt( name.length() );
            bb.put( name.getBytes() );
        }
        //  Then the filter value.
        bb.putInt( fileListName.length() );
        bb.put( fileListName.getBytes() );
        //  And the data format.
        bb.putInt( _dataFormat.getText().length() );
        bb.put( _dataFormat.getText().getBytes() );
        //  The reference MJD for these data.  This is used to resolve ambiguities
        //  in MKIV and Mark5B data.  Not always necessary, but including it doesn't
        //  hurt.  We use the date associated with the first scan in the .vex file,
        //  hopefully this is a sensible assumption.  The reference MJD doesn't have
        //  to be exact so this is a rough calculation of it.
        int refmjd = 365 * ( _vexData.scanList().get(0).start.get( GregorianCalendar.YEAR ) - 1970) +
                _vexData.scanList().get(0).start.get( GregorianCalendar.DAY_OF_YEAR ) + 40587;
        bb.putInt( refmjd ); 
        //  The communications IP address.
        bb.putInt( _settings.guiServerConnection().myIPAddress().length() );
        bb.put( _settings.guiServerConnection().myIPAddress().getBytes() );
        //  Finally, the communications port.
        bb.putInt( port ); 
        byte [] generateData = bb.array();
        //  Start a thread to monitor generate of the filelist.  The thread (below) is
        //  self-terminating.
        GenerateFileListMonitor monitor = new GenerateFileListMonitor( port );
        _messageX = _fileFilter.getX();
        _messageY = _fileFilter.getY();
        monitor.start();
        //  Wait a second for the monitor to start.  Not sure why it takes so long, but
        //  this seems to be a problem.
        try { Thread.sleep( 100 ); } catch ( Exception e ) {}
        //  Send the request to the guiServer - it will connect with the thread.
        _settings.guiServerConnection().sendPacket( _settings.guiServerConnection().GENERATE_FILELIST,
                generateData.length, generateData );
    }
    
    /*
     * This thread opens and monitors a TCP socket for diagnostic reports from the
     * guiServer as it generates a filelist.  The opposite side of this
     * communication link is in the file "guiServer/src/generateFileList.cpp" in
     * the DiFX source tree.
     */
    protected class GenerateFileListMonitor extends Thread {
        
        public GenerateFileListMonitor( int port ) {
            _port = port;
        }
        
        /*
         * These packet types are exchanged with the "FileListConnection" class in the
         * guiServer application on the DiFX host.
         */
        protected final int GENERATE_FILELIST_TASK_TERMINATED                     = 100;
        protected final int GENERATE_FILELIST_TASK_ENDED_GRACEFULLY               = 101;
        protected final int GENERATE_FILELIST_TASK_STARTED                        = 102;
        protected final int GENERATE_FILELIST_DESTINATION_EXISTS                  = 103;
        protected final int GENERATE_FILELIST_FINAL_NAME                          = 104;
        protected final int GENERATE_FILELIST_PATH_ACCESS_FAILURE                 = 105;
        protected final int GENERATE_FILELIST_OVERWRITE                           = 106;
        protected final int GENERATE_FILELIST_CANCEL                              = 107;
        protected final int GENERATE_FILELIST_OPEN_ERROR                          = 108;
        protected final int GENERATE_FILELIST_BAD_FORMAT                          = 109;
        protected final int GENERATE_FILELIST_PROCESSED_COUNT                     = 110;
        protected final int GENERATE_FILELIST_FILE_RESULT                         = 111;
        protected final int GENERATE_FILELIST_ERRORS_ENCOUNTERED                  = 112;
        
        @Override
        public void run() {
            Point pt = _fileList.getLocationOnScreen();
            PopupMonitor popupMonitor = new PopupMonitor( null, pt.x, pt.y, 600, 145, 100 );
            popupMonitor.showProgress( true );
            boolean errors = false;
            String fileListName = null;
            int processedCount = 0;
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
                        if ( packetType == GENERATE_FILELIST_TASK_TERMINATED ) {
                            connected = false;
                        }
                        else if ( packetType == GENERATE_FILELIST_TASK_ENDED_GRACEFULLY ) {
                            connected = false;
                        }
                        else if ( packetType == GENERATE_FILELIST_TASK_STARTED ) {
                        }
                        else if ( packetType == GENERATE_FILELIST_DESTINATION_EXISTS ) {
                            Object[] options = { "Overwrite", "Cancel" };
                            int ret = JOptionPane.showOptionDialog( _fileFilter, "Destination path \"" + new String( data ) + "\"\nexists!", 
                                "Destinations Exists", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0] );
                            if ( ret == JOptionPane.YES_OPTION )
                                ssock.writeInt( GENERATE_FILELIST_OVERWRITE );
                            else {
                                ssock.writeInt( GENERATE_FILELIST_CANCEL );
                            }
                        }
                        else if ( packetType == GENERATE_FILELIST_PATH_ACCESS_FAILURE ) {
                            JOptionPane.showMessageDialog( _fileFilter, "Access denied to path \"" + new String( data ) + "\"",
                                "Access Error", JOptionPane.ERROR_MESSAGE );
                            connected = false;
                        }
                        else if ( packetType == GENERATE_FILELIST_FINAL_NAME ) {
                            fileListName = new String( data );
                            _fileFilter.setText( fileListName );
                            processedCount = 0;
                            popupMonitor.start();
                            popupMonitor.progress( 0, _listOfFiles.size() );
                        }
                        else if ( packetType == GENERATE_FILELIST_OPEN_ERROR ) {
                            JOptionPane.showMessageDialog( _fileFilter, "Error opening destination file:\n" + new String( data ),
                                    "File Open Error", JOptionPane.ERROR_MESSAGE );
                        }
                        else if ( packetType == GENERATE_FILELIST_BAD_FORMAT ) {
                            JOptionPane.showMessageDialog( _fileFilter, "Data format \"" + new String( data ) + "\" is not recognized.\n"
                                    + "Filelist cannot be generated.",
                                "Format Error", JOptionPane.ERROR_MESSAGE );
                            connected = false;
                        }
                        else if ( packetType == GENERATE_FILELIST_PROCESSED_COUNT ) {
                            ByteBuffer b = ByteBuffer.wrap( data, 0, 4 );
                            b.order( ByteOrder.BIG_ENDIAN );
                            processedCount = b.getInt();
                            popupMonitor.progress( processedCount, _listOfFiles.size() );
                        }
                        else if ( packetType == GENERATE_FILELIST_FILE_RESULT ) {
                            popupMonitor.status( new String( data ) );
                        }
                        else if ( packetType == GENERATE_FILELIST_ERRORS_ENCOUNTERED ) {
                            popupMonitor.error( processedCount + " of " + _listOfFiles.size() + " files processed.",
                                    "There were errors generated (see message window for details)." );
                            errors = true;
                        }
                    }
                } catch ( SocketTimeoutException e ) {
                }
                ssock.close();
            } catch ( java.io.IOException e ) {
                e.printStackTrace();
            }
            _settings.releaseTransferPort( _port );
            if ( !errors ) {
                popupMonitor.success( "Complete!", "FileList \"" + _fileFilter.getText() + "\" Created.", 
                        processedCount + " of " + _listOfFiles.size() + " files processed." );
                _fileListCheck.setSelected( true );
                _dataSourcePanel.name( "Data Source: files " + _fileFilter.getText().trim() );
                dispatchChangeCallback();
            }
        }
        
        protected int _port;
        
    }
    
    protected JCheckBox _useCheck;
    protected JCheckBox _vsnCheck;
    protected JCheckBox _fileCheck;
    protected JCheckBox _eVLBICheck;
    protected NumberBox _eVLBIPort;
    protected JComboBox _vsnList;
    protected SaneTextField _dataFormat;
    protected JLabel _dataSource;
    protected EventListenerList _changeListeners;
    protected SaneTextField _fileFilter;
    protected ZCheckBox _fileListCheck;
    protected ZButton _generateFileList;
    protected NodeBrowserScrollPane _fileList;
    protected StationPanel _this;
    protected SaneTextField _dirListLocation;
    protected ArrayList<String> _listOfFiles;

    
    protected SystemSettings _settings;
    
    protected NodeBrowserScrollPane _contentPane;
    protected IndexedPanel _dataSourcePanel;
    protected IndexedPanel _antennaPanel;
    protected IndexedPanel _sitePanel;
    protected IndexedPanel _settingsPanel;
    
    protected JCheckBox _useSourceNode;
    protected JComboBox _sourceNodeChoice;
    
    protected NumberBox _positionX;
    protected NumberBox _positionY;
    protected NumberBox _positionZ;
    protected double _xpos;
    protected double _ypos;
    protected double _zpos;
    
    protected NumberBox _deltaClock;
    protected JComboBox _toneSelection;
    protected NumberBox _phaseCalInt;
    
    protected VexFileParser _vexData;

}
