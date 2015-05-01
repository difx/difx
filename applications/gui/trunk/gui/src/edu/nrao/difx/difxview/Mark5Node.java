/*
 * This is a browser node used to display Mark5 data.  It inherits the "ProcessorNode"
 * class, as it shares many of the data display traits with that class.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.ActivityMonitorLight;

import edu.nrao.difx.xmllib.difxmessage.DifxMessage;

import javax.swing.JSeparator;
import javax.swing.JMenuItem;
import javax.swing.JMenu;
import javax.swing.JButton;
import javax.swing.JPopupMenu;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.Color;
import java.awt.Insets;
import java.awt.MouseInfo;

import java.util.ArrayList;
import java.util.Iterator;

import edu.nrao.difx.difxutilities.SMARTMonitor;

public class Mark5Node extends ProcessorNode {
    
    public Mark5Node( String name, SystemSettings settings, SMARTMonitor smartMonitor ) {
        super( name, settings );
        _smartMonitor = smartMonitor;
        _smartDisplayList = new ArrayList<SMARTDisplay>();
        _directoryDisplayList = new ArrayList<DirectoryDisplay>();
        _isMark5 = true;
    }
    
    @Override
    public void createAdditionalItems() {
        super.createAdditionalItems();
        _stateChanged = new ActivityMonitorLight();
        this.add( _stateChanged );
        //  VSN banks are buttons the trigger a popup menu of options.
        _bankAVSN = new JButton();
        _bankAVSN.setMargin( new Insets( 0, 0, 2, 0 ) );
        _bankAVSN.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _bankAVSN != null && _bankAVSN.getText() != null &&
                _bankAVSN.getText().length() > 0 && !_bankAVSN.getText().trim().contentEquals( "none") ) {
                    VSNPopupMenu vsnMenu = new VSNPopupMenu( _bankAVSN.getText().trim() );
                    vsnMenu.show( _bankAVSN, 0, 0 );
                }
            }
        });
        this.add( _bankAVSN );
        _bankBVSN = new JButton();
        _bankBVSN.setMargin( new Insets( 0, 0, 2, 0 ) );
        _bankBVSN.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _bankBVSN != null && _bankBVSN.getText() != null &&
                _bankBVSN.getText().length() > 0 && !_bankBVSN.getText().trim().contentEquals( "none") ) {
                    VSNPopupMenu vsnMenu = new VSNPopupMenu( _bankBVSN.getText().trim() );
                    vsnMenu.show( _bankBVSN, 0, 0 );
                }
            }
        });
        this.add( _bankBVSN );
        _statusWord = new ColumnTextArea();
        _statusWord.justify( ColumnTextArea.RIGHT );
        this.add( _statusWord );
        _activeBank = new ColumnTextArea();
        _activeBank.justify( ColumnTextArea.RIGHT );
        this.add( _activeBank );
        _scanNumber = new ColumnTextArea();
        _scanNumber.justify( ColumnTextArea.RIGHT );
        this.add( _scanNumber );
        _scanName = new ColumnTextArea();
        _scanName.justify( ColumnTextArea.RIGHT );
        this.add( _scanName );
        _position = new ColumnTextArea();
        _position.justify( ColumnTextArea.RIGHT );
        this.add( _position );
        _playRate = new ColumnTextArea();
        _playRate.justify( ColumnTextArea.RIGHT );
        this.add( _playRate );
        _dataMJD = new ColumnTextArea();
        _dataMJD.justify( ColumnTextArea.RIGHT );
        this.add( _dataMJD );
        _currentJob = new ColumnTextArea();
        _currentJob.justify( ColumnTextArea.RIGHT );
        this.add( _currentJob );
    }
    
    @Override
    public void generatePopupMenu() {
        super.generatePopupMenu();
        _popup.add( new JSeparator() );
        JMenuItem startItem = new JMenuItem( "Start" );
        startItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                sendDiFXCommandMessage( "StartMark5A" );
            }
        });
        _popup.add( startItem );
        JMenuItem stopItem = new JMenuItem( "Stop" );
        stopItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                sendDiFXCommandMessage( "StopMark5A" );
            }
        });
        _popup.add( stopItem );
//        JMenuItem clearItem = new JMenuItem( "Clear" );
//        clearItem.addActionListener(new ActionListener() {
//            public void actionPerformed( ActionEvent e ) {
//                sendDiFXCommandMessage( "Clear" );
//            }
//        });
//        _popup.add( clearItem );
//        JMenuItem copyItem = new JMenuItem( "Copy" );
//        copyItem.addActionListener(new ActionListener() {
//            public void actionPerformed( ActionEvent e ) {
//                sendDiFXCommandMessage( "Copy" );
//            }
//        });
//        _popup.add( copyItem );
//        JMenuItem getVSNItem = new JMenuItem( "Get VSN" );
//        getVSNItem.addActionListener(new ActionListener() {
//            public void actionPerformed( ActionEvent e ) {
//                sendDiFXCommandMessage( "GetVSN" );
//            }
//        });
//        _popup.add( getVSNItem );
//        JMenuItem getLoadItem = new JMenuItem( "Get Load" );
//        getLoadItem.addActionListener(new ActionListener() {
//            public void actionPerformed( ActionEvent e ) {
//                sendDiFXCommandMessage( "GetLoad" );
//            }
//        });
//        _popup.add( getLoadItem );
        //  Add options for existing VSNs.
        if ( _bankAVSN != null && _bankAVSN.getText() != null &&
                _bankAVSN.getText().length() > 0 && !_bankAVSN.getText().trim().contentEquals( "none") ) {
            VSNMenu vsnMenu = new VSNMenu( _bankAVSN.getText().trim() );
            _popup.add( vsnMenu );
        }
        if ( _bankBVSN != null && _bankBVSN.getText() != null &&
                _bankBVSN.getText().length() > 0 && !_bankBVSN.getText().trim().contentEquals( "none") ) {
            VSNMenu vsnMenu = new VSNMenu( _bankBVSN.getText().trim() );
            _popup.add( vsnMenu );
        }
    }
    
    /*
     * This is a sub-menu showing things that can be run on a particular VSN.
     */
    protected class VSNMenu extends JMenu {
        
        protected String _vsn;

        public VSNMenu( String vsn ) {
            super( vsn );
            _vsn = vsn;
            JMenuItem smartItem = new JMenuItem( "S.M.A.R.T. Monitor" );
            smartItem.addActionListener(new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    showSMARTDisplay( _vsn );
                }
            });
            add( smartItem );
            JMenuItem viewDirItem = new JMenuItem( "Module Directory" );
            viewDirItem.addActionListener(new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    displayDirectory( _vsn );
                }
            });
            add( viewDirItem );
        }
        
    }
    
    /*
     * The popup menu shows the same items as the above sub-menu, but it is triggered
     * by clicking on one of the VSNs themselves (they are buttons).
     */
    protected class VSNPopupMenu extends JPopupMenu {

        protected String _vsn;
        
        public VSNPopupMenu( String vsn ) {
            super( vsn );
            _vsn = vsn;
            JMenuItem title = new JMenuItem( vsn + " Controls" );
            add( title );
            add( new JSeparator() );
            JMenuItem smartItem = new JMenuItem( "S.M.A.R.T. Monitor" );
            smartItem.addActionListener(new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    showSMARTDisplay( _vsn );
                }
            });
            add( smartItem );
            JMenuItem viewDirItem = new JMenuItem( "Module Directory" );
            viewDirItem.addActionListener(new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    displayDirectory( _vsn );
                }
            });
            add( viewDirItem );
        }
        
    }
    
    /*
     * Functions in commmon called by both menu types.
     */
    public void showSMARTDisplay( String vsn ) {
        //  Check the list of SMARTDisplays we have generated already to see if this one
        //  is available.    
        SMARTDisplay smartDisplay = null;
        for ( Iterator<SMARTDisplay> iter = _smartDisplayList.iterator(); iter.hasNext(); ) {
            SMARTDisplay thisDisplay = iter.next();
            if ( vsn.contentEquals( thisDisplay.vsn() ) && name().contentEquals( thisDisplay.host() ) )
                smartDisplay = thisDisplay;
        }
        //  If the display isn't available, create a new one.
        if ( smartDisplay == null ) {
            smartDisplay = new SMARTDisplay( MouseInfo.getPointerInfo().getLocation().x, 
                MouseInfo.getPointerInfo().getLocation().y, _smartMonitor, _settings, name(), vsn );
            _smartDisplayList.add( smartDisplay );
        }
        smartDisplay.setVisible( true );
    }
    
    /*
     * Display the directory for a VSN.  This window will also allow the user to
     * regenerate it.
     */
    public void displayDirectory( String vsn ) {
        //  Check the list of DirectoryDisplays we have generated already to see if this one
        //  is available.    
        DirectoryDisplay directoryDisplay = null;
        for ( Iterator<DirectoryDisplay> iter = _directoryDisplayList.iterator(); iter.hasNext(); ) {
            DirectoryDisplay thisDisplay = iter.next();
            if ( vsn.contentEquals( thisDisplay.vsn() ) )
                directoryDisplay = thisDisplay;
        }
        //  If the display isn't available, create a new one.
        if ( directoryDisplay == null ) {
            directoryDisplay = new DirectoryDisplay( MouseInfo.getPointerInfo().getLocation().x, 
                MouseInfo.getPointerInfo().getLocation().y, _settings, this, vsn );
            _directoryDisplayList.add( directoryDisplay );
        }
        directoryDisplay.setVisible( true );
    }
    
    @Override
    public void positionItems() {
        super.positionItems();
        if ( _showStateChanged ) {
            _stateChanged.setBounds( _xOff + ( _widthStateChanged - 10 ) / 2, 6, 10, 10 );
            _xOff += _widthStateChanged;
            _colorColumn = false;
        }
        if ( _showBankAVSN ) {
            _bankAVSN.setBounds( _xOff, 1, _widthBankAVSN, _ySize - 2);
            if ( _activeBank.getText().equalsIgnoreCase( "A" ) ) {
                _bankAVSN.setForeground( Color.BLACK );
                _bankAVSN.setBackground( Color.GREEN );
            }
            else {
                _bankAVSN.setForeground( Color.DARK_GRAY );
                _bankAVSN.setBackground( Color.LIGHT_GRAY );
            }
            _xOff += _widthBankAVSN;
            _colorColumn = false;
        }
        if ( _showBankBVSN ) {
            _bankBVSN.setBounds( _xOff, 1, _widthBankBVSN, _ySize - 2);
            if ( _activeBank.getText().equalsIgnoreCase( "B" ) ) {
                _bankBVSN.setForeground( Color.BLACK );
                _bankBVSN.setBackground( Color.GREEN );
            }
            else {
                _bankBVSN.setForeground( Color.DARK_GRAY );
                _bankBVSN.setBackground( Color.LIGHT_GRAY );
            }
            _xOff += _widthBankBVSN;
            _colorColumn = false;
        }
        if ( _showStatusWord )
            setTextArea( _statusWord, _widthStatusWord );
        if ( _showActiveBank )
            setTextArea( _activeBank, _widthActiveBank );
        if ( _showScanNumber )
            setTextArea( _scanNumber, _widthScanNumber );

        if ( _showScanName )
            setTextArea( _scanName, _widthScanName );
        if ( _showPosition )
            setTextArea( _position, _widthPosition );
        if ( _showPlayRate )
            setTextArea( _playRate, _widthPlayRate );
        if ( _showDataMJD )
            setTextArea( _dataMJD, _widthDataMJD );
        if ( _showCurrentJob )
            setTextArea( _currentJob, _widthCurrentJob );
    }
    
    public void showStateChanged( boolean newVal ) {
        _showStateChanged = newVal;
        _stateChanged.setVisible( newVal );            
    }
    public void showBankAVSN( boolean newVal ) {
        _showBankAVSN = newVal;
        _bankAVSN.setVisible( newVal );            
    }
    public void showBankBVSN( boolean newVal ) {
        _showBankBVSN = newVal;
        _bankBVSN.setVisible( newVal );            
    }
    public void showStatusWord( boolean newVal ) {
        _showStatusWord = newVal;
        _statusWord.setVisible( newVal );            
    }
    public void showActiveBank( boolean newVal ) {
        _showActiveBank = newVal;
        _activeBank.setVisible( newVal );            
    }
    public void showScanNumber( boolean newVal ) {
        _showScanNumber = newVal;
        _scanNumber.setVisible( newVal );            
    }
    public void showScanName( boolean newVal ) {
        _showScanName = newVal;
        _scanName.setVisible( newVal );            
    }
    public void showPosition( boolean newVal ) {
        _showPosition = newVal;
        _position.setVisible( newVal );            
    }
    public void showPlayRate( boolean newVal ) {
        _showPlayRate = newVal;
        _playRate.setVisible( newVal );            
    }
    public void showDataMJD( boolean newVal ) {
        _showDataMJD = newVal;
        _dataMJD.setVisible( newVal );            
    }
    public void showCurrentJob( boolean newVal ) {
        _showCurrentJob = newVal;
        _currentJob.setVisible( newVal );            
    }
    
    @Override
    public void statusMessage( DifxMessage difxMsg ) {
        super.statusMessage( difxMsg );
        _bankAVSN.setText( difxMsg.getBody().getMark5Status().getBankAVSN() );
        //  Add this module to our list of modules (if its not there already).
        if ( !_settings.dataSourceInList( difxMsg.getBody().getMark5Status().getBankAVSN(), "VSN" ) ) {
            if ( difxMsg.getBody().getMark5Status().getBankAVSN().length() > 0 && !difxMsg.getBody().getMark5Status().getBankAVSN().equalsIgnoreCase( "NONE" ) )
                _settings.addDataSource( difxMsg.getBody().getMark5Status().getBankAVSN(), "VSN", "hardware" );
        }
        _bankBVSN.setText( difxMsg.getBody().getMark5Status().getBankBVSN() );
        //  Add this module to our list of modules (if its not there already).
        if ( !_settings.dataSourceInList( difxMsg.getBody().getMark5Status().getBankBVSN(), "VSN" ) ) {
            if ( difxMsg.getBody().getMark5Status().getBankBVSN().length() > 0 && !difxMsg.getBody().getMark5Status().getBankBVSN().equalsIgnoreCase( "NONE" ) )
                _settings.addDataSource( difxMsg.getBody().getMark5Status().getBankBVSN(), "VSN", "hardware" );
        }
        _statusWord.setText( difxMsg.getBody().getMark5Status().getStatusWord() );
        _activeBank.setText( difxMsg.getBody().getMark5Status().getActiveBank() );
        _scanNumber.setText( String.format( "%10d", difxMsg.getBody().getMark5Status().getScanNumber() ) );
        _scanName.setText( difxMsg.getBody().getMark5Status().getScanName() );
        _position.setText( String.format( "%10d", difxMsg.getBody().getMark5Status().getPosition() ) );
        _playRate.setText( String.format( "%10.3f", difxMsg.getBody().getMark5Status().getPlayRate() ) );
        _dataMJD.setText( difxMsg.getBody().getMark5Status().getDataMJD().trim() );
        _currentJob.setText( difxMsg.getHeader().getIdentifier() );
        updateUI();
    }
    
    public void widthStateChanged( int newVal ) { _widthStateChanged = newVal; }
    public void widthBankAVSN( int newVal ) { _widthBankAVSN = newVal; }
    public void widthBankBVSN( int newVal ) { _widthBankBVSN = newVal; }
    public void widthStatusWord( int newVal ) { _widthStatusWord = newVal; }
    public void widthActiveBank( int newVal ) { _widthActiveBank = newVal; }
    public void widthScanNumber( int newVal ) { _widthScanNumber = newVal; }
    public void widthScanName( int newVal ) { _widthScanName = newVal; }
    public void widthPosition( int newVal ) { _widthPosition = newVal; }
    public void widthPlayRate( int newVal ) { _widthPlayRate = newVal; }
    public void widthDataMJD( int newVal ) { _widthDataMJD = newVal; }
    public void widthCurrentJob( int newVal ) { _widthCurrentJob = newVal; }
    
    public String bankAVSN() { return _bankAVSN.getText(); }
    public String bankBVSN() { return _bankBVSN.getText(); }
    public String activeBank() { return _activeBank.getText(); }
    public Integer scanNumber() {
        if ( _scanNumber == null )
            return null;
        if ( _scanNumber.getText() == null || _scanNumber.getText().length() <= 0 )
            return null;
        return Integer.parseInt( _scanNumber.getText().trim() );
    }
    public String scanName() {
        if ( _scanName == null )
            return null;
        if ( _scanName.getText() == null || _scanName.getText().length() <= 0 )
            return null;
        else
            return _scanName.getText();
    }
    
    protected ActivityMonitorLight _stateChanged;
    protected boolean _showStateChanged;
    protected JButton _bankAVSN;
    protected boolean _showBankAVSN;
    protected JButton _bankBVSN;
    protected boolean _showBankBVSN;
    protected ColumnTextArea _statusWord;
    protected boolean _showStatusWord;
    protected ColumnTextArea _activeBank;
    protected boolean _showActiveBank;
    protected ColumnTextArea _scanNumber;
    protected boolean _showScanNumber;
    protected ColumnTextArea _scanName;
    protected boolean _showScanName;
    protected ColumnTextArea _position;
    protected boolean _showPosition;
    protected ColumnTextArea _playRate;
    protected boolean _showPlayRate;
    protected ColumnTextArea _dataMJD;
    protected boolean _showDataMJD;
    protected ColumnTextArea _currentJob;
    protected boolean _showCurrentJob;

    protected int _widthStateChanged;
    protected int _widthBankAVSN;
    protected int _widthBankBVSN;
    protected int _widthStatusWord;
    protected int _widthActiveBank;
    protected int _widthScanNumber;
    protected int _widthScanName;
    protected int _widthPosition;
    protected int _widthPlayRate;
    protected int _widthDataMJD;
    protected int _widthCurrentJob;
    
    protected SMARTMonitor _smartMonitor;
    protected ArrayList<SMARTDisplay> _smartDisplayList;
    protected ArrayList<DirectoryDisplay> _directoryDisplayList;
    
}
