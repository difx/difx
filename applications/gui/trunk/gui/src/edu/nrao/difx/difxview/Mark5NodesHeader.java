/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.BrowserNode;
import javax.swing.JCheckBoxMenuItem;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.util.Iterator;
import java.awt.MouseInfo;
import javax.swing.JOptionPane;

/**
 *
 * @author jspitzak
 */
public class Mark5NodesHeader extends ProcessorNodesHeader {
    
    public Mark5NodesHeader( String name, SystemSettings settings ) {
        super( name, settings );
    }
    
    @Override
    public void createAdditionalItems() {
        super.createAdditionalItems();
        _stateChanged = new ColumnTextArea( "State Changed" );
        _stateChanged.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showStateChanged.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _stateChanged );
        _bankAVSN = new ColumnTextArea( "Bank A" );
        _bankAVSN.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showBankAVSN.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _bankAVSN );
        _bankBVSN = new ColumnTextArea( "Bank B" );
        _bankBVSN.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showBankBVSN.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _bankBVSN );
        _statusWord = new ColumnTextArea( "Status Word" );
        _statusWord.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showStatusWord.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _statusWord );
        _activeBank = new ColumnTextArea( "Active Bank" );
        _activeBank.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showActiveBank.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _activeBank );
        _scanNumber = new ColumnTextArea( "Scan #" );
        _scanNumber.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showScanNumber.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _scanNumber );
        _scanName = new ColumnTextArea( "Scan Name" );
        _scanName.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showScanName.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _scanName );
        _position = new ColumnTextArea( "Position" );
        _position.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showPosition.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _position );
        _playRate = new ColumnTextArea( "Play Rate" );
        _playRate.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showPlayRate.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _playRate );
        _dataMJD = new ColumnTextArea( "Data MJD" );
        _dataMJD.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showDataMJD.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _dataMJD );
        _currentJob = new ColumnTextArea( "Current Job" );
        _currentJob.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showCurrentJob.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _currentJob );
        _showStateChanged = new JCheckBoxMenuItem( "State Changed" );
        _showStateChanged.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showStateChanged );
        _showBankAVSN = new JCheckBoxMenuItem( "Bank A" );
        _showBankAVSN.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showBankAVSN );
        _showBankBVSN = new JCheckBoxMenuItem( "Bank B" );
        _showBankBVSN.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showBankBVSN );
        _showStatusWord = new JCheckBoxMenuItem( "Status Word" );
        _showStatusWord.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showStatusWord );
        _showActiveBank = new JCheckBoxMenuItem( "Active Bank" );
        _showActiveBank.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showActiveBank );
        _showScanNumber = new JCheckBoxMenuItem( "Scan Number" );
        _showScanNumber.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showScanNumber );
        _showScanName = new JCheckBoxMenuItem( "Scan Name" );
        _showScanName.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showScanName );
        _showPosition = new JCheckBoxMenuItem( "Position" );
        _showPosition.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showPosition );
        _showPlayRate = new JCheckBoxMenuItem( "Play Rate" );
        _showPlayRate.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showPlayRate );
        _showDataMJD = new JCheckBoxMenuItem( "Data MJD" );
        _showDataMJD.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showDataMJD );
        _showCurrentJob = new JCheckBoxMenuItem( "Current Job" );
        _showCurrentJob.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showCurrentJob );
    }
    
    @Override
    public void addInvisibleNode() {
        AddNode dialog = new AddNode( MouseInfo.getPointerInfo().getLocation().x,
                MouseInfo.getPointerInfo().getLocation().y, true );
        dialog.setVisible( true );
        if ( dialog.ok ) {
            if ( dialog.nodeName.getText().length() > 0 ) {
                if ( !checkAddNode( dialog.nodeName.getText() ) )
                    JOptionPane.showMessageDialog( this, "A node named \"" + dialog.nodeName.getText() + "\" already exists.",
                            "Node name exists", JOptionPane.WARNING_MESSAGE );
            }
            else
                JOptionPane.showMessageDialog( this, "No node name entered.",
                        "Node name required.", JOptionPane.WARNING_MESSAGE );
        }
    }

    /*
     * Check if an "invisible" node exists in the current list, then add it if it does not.
     * Return true if the node was added, false if not.
     */
    public boolean checkAddNode( String name ) {
        boolean found = false;
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext() && !found; ) {
            Mark5Node thisNode = (Mark5Node)(iter.next());
            if ( thisNode.name().contentEquals( name ) ) {
                found = true;
            }
        }
        if ( found )
            return false;
        else {
            Mark5Node newNode = new Mark5Node( name, _settings, null );
            newNode.currentState( "invisible" );
            this.addChild( newNode );
            return true;
        }
    }
    
    public void activateAll() {
        _showStateChanged.setState( true );
        _showBankAVSN.setState( true );
        _showBankBVSN.setState( true );
        _showStatusWord.setState( true );
        _showActiveBank.setState( true );
        _showScanNumber.setState( true );
        _showScanName.setState( true );
        _showPosition.setState( true );
        _showPlayRate.setState( true );
        _showDataMJD.setState( true );
        _showCurrentJob.setState( true );
        super.activateAll();
    }

    @Override
    public void positionItems() {
        super.positionItems();
        if ( _showStateChanged.getState() ) {
            setTextArea( _stateChanged, _settings.hardwareColumnSpecs().StateChanged.width );
            _positionStateChanged = _xOff;
        }
        else
            _positionStateChanged = -100;
        if ( _showBankAVSN.getState() ) {
            setTextArea( _bankAVSN, _settings.hardwareColumnSpecs().BankAVSN.width );
            _positionBankAVSN = _xOff;
        }
        else
            _positionBankAVSN = -100;
        if ( _showBankBVSN.getState() ) {
            setTextArea( _bankBVSN, _settings.hardwareColumnSpecs().BankBVSN.width );
            _positionBankBVSN = _xOff;
        }
        else
            _positionBankBVSN = -100;
        if ( _showStatusWord.getState() ) {
            setTextArea( _statusWord, _settings.hardwareColumnSpecs().StatusWord.width );
            _positionStatusWord = _xOff;
        }
        else
            _positionStatusWord = -100;
        if ( _showActiveBank.getState() ) {
            setTextArea( _activeBank, _settings.hardwareColumnSpecs().ActiveBank.width );
            _positionActiveBank = _xOff;
        }
        else
            _positionActiveBank = -100;
        if ( _showScanNumber.getState() ) {
            setTextArea( _scanNumber, _settings.hardwareColumnSpecs().ScanNumber.width );
            _positionScanNumber = _xOff;
        }
        else
            _positionScanNumber = -100;
        if ( _showScanName.getState() ) {
            setTextArea( _scanName, _settings.hardwareColumnSpecs().ScanName.width );
            _positionScanName = _xOff;
        }
        else
            _positionScanName = -100;
        if ( _showPosition.getState() ) {
            setTextArea( _position, _settings.hardwareColumnSpecs().Position.width );
            _positionPosition = _xOff;
        }
        else
            _positionPosition = -100;
        if ( _showPlayRate.getState() ) {
            setTextArea( _playRate, _settings.hardwareColumnSpecs().PlayRate.width );
            _positionPlayRate = _xOff;
        }
        else
            _positionPlayRate = -100;
        if ( _showDataMJD.getState() ) {
            setTextArea( _dataMJD, _settings.hardwareColumnSpecs().DataMJD.width );
            _positionDataMJD = _xOff;
        }
        else
            _positionDataMJD = -100;
        if ( _showCurrentJob.getState() ) {
            setTextArea( _currentJob, _settings.hardwareColumnSpecs().CurrentJob.width );
            _positionCurrentJob = _xOff;
        }
        else
            _positionCurrentJob = -100;
    }
    
    public void initializeDisplaySettings() {
        super.initializeDisplaySettings();
        _showStateChanged.setState( _settings.hardwareColumnSpecs().StateChanged.show );
        _showBankAVSN.setState( _settings.hardwareColumnSpecs().BankAVSN.show );
        _showBankBVSN.setState( _settings.hardwareColumnSpecs().BankBVSN.show );
        _showStatusWord.setState( _settings.hardwareColumnSpecs().StatusWord.show );
        _showActiveBank.setState( _settings.hardwareColumnSpecs().ActiveBank.show );
        _showScanNumber.setState( _settings.hardwareColumnSpecs().ScanNumber.show );
        _showScanName.setState( _settings.hardwareColumnSpecs().ScanName.show );
        _showPosition.setState( _settings.hardwareColumnSpecs().Position.show );
        _showPlayRate.setState( _settings.hardwareColumnSpecs().PlayRate.show );
        _showDataMJD.setState( _settings.hardwareColumnSpecs().DataMJD.show );
        _showCurrentJob.setState( _settings.hardwareColumnSpecs().CurrentJob.show );
    }
    
    @Override
    public void setChildColumnWidths() {
        super.setChildColumnWidths();
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            Mark5Node thisNode = (Mark5Node)(iter.next());
            //  Change the settings on these items to match our current specifications.
            thisNode.widthStateChanged( _settings.hardwareColumnSpecs().StateChanged.width );
            thisNode.widthBankAVSN( _settings.hardwareColumnSpecs().BankAVSN.width );
            thisNode.widthBankBVSN( _settings.hardwareColumnSpecs().BankBVSN.width );
            thisNode.widthStatusWord( _settings.hardwareColumnSpecs().StatusWord.width );
            thisNode.widthActiveBank( _settings.hardwareColumnSpecs().ActiveBank.width );
            thisNode.widthScanNumber( _settings.hardwareColumnSpecs().ScanNumber.width );
            thisNode.widthScanName( _settings.hardwareColumnSpecs().ScanName.width );
            thisNode.widthPosition( _settings.hardwareColumnSpecs().Position.width );
            thisNode.widthPlayRate( _settings.hardwareColumnSpecs().PlayRate.width );
            thisNode.widthDataMJD( _settings.hardwareColumnSpecs().DataMJD.width );
            thisNode.widthCurrentJob( _settings.hardwareColumnSpecs().CurrentJob.width );
            thisNode.updateUI();
        }
    }
    
    /*
     * Check mouse move events to see if it is being positioned over one of the
     * joints between column headers.  This should change the cursor.  We also
     * record which item we are over.
     */
    @Override
    public void mouseMoved( MouseEvent e ) {
        this.setCursor( _normalCursor );
        _adjustStateChanged = false;
        _adjustBankAVSN = false;
        _adjustBankBVSN = false;
        _adjustStatusWord = false;
        _adjustActiveBank = false;
        _adjustScanNumber = false;
        _adjustScanName = false;
        _adjustPosition = false;
        _adjustPlayRate = false;
        _adjustDataMJD = false;
        _adjustCurrentJob = false;
        if ( e.getX() > _positionStateChanged - 3 && e.getX() < _positionStateChanged + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustStateChanged = true;
        }
        else if ( e.getX() > _positionBankAVSN - 3 && e.getX() < _positionBankAVSN + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustBankAVSN = true;
        }
        else if ( e.getX() > _positionBankBVSN - 3 && e.getX() < _positionBankBVSN + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustBankBVSN = true;
        }
        else if ( e.getX() > _positionStatusWord - 3 && e.getX() < _positionStatusWord + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustStatusWord = true;
        }
        else if ( e.getX() > _positionActiveBank - 3 && e.getX() < _positionActiveBank + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustActiveBank = true;
        }
        else if ( e.getX() > _positionScanNumber - 3 && e.getX() < _positionScanNumber + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustScanNumber = true;
        }
        else if ( e.getX() > _positionScanName - 3 && e.getX() < _positionScanName + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustScanName = true;
        }
        else if ( e.getX() > _positionPosition - 3 && e.getX() < _positionPosition + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustPosition = true;
        }
        else if ( e.getX() > _positionPlayRate - 3 && e.getX() < _positionPlayRate + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustPlayRate = true;
        }
        else if ( e.getX() > _positionDataMJD - 3 && e.getX() < _positionDataMJD + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustDataMJD = true;
        }
        else if ( e.getX() > _positionCurrentJob - 3 && e.getX() < _positionCurrentJob + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustCurrentJob = true;
        }
        else {
            super.mouseMoved( e );
        }
    }
    
    /*
     * Mouse pressed events record the size of a column (if we are in a position
     * to adjust the column).
     */
    @Override
    public void mousePressed( MouseEvent e ) {
        if ( _adjustStateChanged ) {
            _startWidth = _settings.hardwareColumnSpecs().StateChanged.width;
            _startX = e.getX();
        }
        else if ( _adjustBankAVSN ) {
            _startWidth = _settings.hardwareColumnSpecs().BankAVSN.width;
            _startX = e.getX();
        }
        else if ( _adjustBankBVSN ) {
            _startWidth = _settings.hardwareColumnSpecs().BankBVSN.width;
            _startX = e.getX();
        }
        else if ( _adjustStatusWord ) {
            _startWidth = _settings.hardwareColumnSpecs().StatusWord.width;
            _startX = e.getX();
        }
        else if ( _adjustActiveBank ) {
            _startWidth = _settings.hardwareColumnSpecs().ActiveBank.width;
            _startX = e.getX();
        }
        else if ( _adjustScanNumber ) {
            _startWidth = _settings.hardwareColumnSpecs().ScanNumber.width;
            _startX = e.getX();
        }
        else if ( _adjustScanName ) {
            _startWidth = _settings.hardwareColumnSpecs().ScanName.width;
            _startX = e.getX();
        }
        else if ( _adjustPosition ) {
            _startWidth = _settings.hardwareColumnSpecs().Position.width;
            _startX = e.getX();
        }
        else if ( _adjustPlayRate ) {
            _startWidth = _settings.hardwareColumnSpecs().PlayRate.width;
            _startX = e.getX();
        }
        else if ( _adjustDataMJD ) {
            _startWidth = _settings.hardwareColumnSpecs().DataMJD.width;
            _startX = e.getX();
        }
        else if ( _adjustCurrentJob ) {
            _startWidth = _settings.hardwareColumnSpecs().CurrentJob.width;
            _startX = e.getX();
        }
        else
            super.mousePressed( e );
    }
    
    /*
     * Drag events might be used to change the width of columns.
     */
    @Override
    public void mouseDragged( MouseEvent e ) {
        if ( _adjustStateChanged ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().StateChanged.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustBankAVSN ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().BankAVSN.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustBankBVSN ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().BankBVSN.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustStatusWord ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().StatusWord.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustActiveBank ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().ActiveBank.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustScanNumber ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().ScanNumber.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustScanName ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().ScanName.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustPosition ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().Position.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustPlayRate ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().PlayRate.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustDataMJD ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().DataMJD.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustCurrentJob ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().CurrentJob.width = _startWidth + e.getX() - _startX;
        }
        super.mouseDragged( e );
    }
    
    @Override
    public void updateDisplayedData() {
        super.updateDisplayedData();
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            Mark5Node thisNode = (Mark5Node)(iter.next());
            thisNode.showStateChanged( _showStateChanged.getState() );
            thisNode.showBankAVSN( _showBankAVSN.getState() );
            thisNode.showBankBVSN( _showBankBVSN.getState() );
            thisNode.showStatusWord( _showStatusWord.getState() );
            thisNode.showActiveBank( _showActiveBank.getState() );
            thisNode.showScanNumber( _showScanNumber.getState() );
            thisNode.showScanName( _showScanName.getState() );
            thisNode.showPosition( _showPosition.getState() );
            thisNode.showPlayRate( _showPlayRate.getState() );
            thisNode.showDataMJD( _showDataMJD.getState() );
            thisNode.showCurrentJob( _showCurrentJob.getState() );
            thisNode.updateUI();
        }
        _stateChanged.setVisible( _showStateChanged.getState() );
        _bankAVSN.setVisible( _showBankAVSN.getState() );
        _bankBVSN.setVisible( _showBankBVSN.getState() );
        _statusWord.setVisible( _showStatusWord.getState() );
        _activeBank.setVisible( _showActiveBank.getState() );
        _scanNumber.setVisible( _showScanNumber.getState() );
        _scanName.setVisible( _showScanName.getState() );
        _position.setVisible( _showPosition.getState() );
        _playRate.setVisible( _showPlayRate.getState() );
        _dataMJD.setVisible( _showDataMJD.getState() );
        _currentJob.setVisible( _showCurrentJob.getState() );

        _settings.hardwareColumnSpecs().StateChanged.show = _showStateChanged.getState();
        _settings.hardwareColumnSpecs().BankAVSN.show = _showBankAVSN.getState();
        _settings.hardwareColumnSpecs().BankBVSN.show = _showBankBVSN.getState();
        _settings.hardwareColumnSpecs().StatusWord.show = _showStatusWord.getState();
        _settings.hardwareColumnSpecs().ActiveBank.show = _showActiveBank.getState();
        _settings.hardwareColumnSpecs().ScanNumber.show = _showScanNumber.getState();
        _settings.hardwareColumnSpecs().ScanName.show = _showScanName.getState();
        _settings.hardwareColumnSpecs().Position.show = _showPosition.getState();
        _settings.hardwareColumnSpecs().PlayRate.show = _showPlayRate.getState();
        _settings.hardwareColumnSpecs().DataMJD.show = _showDataMJD.getState();
        _settings.hardwareColumnSpecs().CurrentJob.show = _showCurrentJob.getState();
        
    }
    
    protected JCheckBoxMenuItem _showStateChanged;
    protected JCheckBoxMenuItem _showBankAVSN;
    protected JCheckBoxMenuItem _showBankBVSN;
    protected JCheckBoxMenuItem _showStatusWord;
    protected JCheckBoxMenuItem _showActiveBank;
    protected JCheckBoxMenuItem _showScanNumber;
    protected JCheckBoxMenuItem _showScanName;
    protected JCheckBoxMenuItem _showPosition;
    protected JCheckBoxMenuItem _showPlayRate;
    protected JCheckBoxMenuItem _showDataMJD;
    protected JCheckBoxMenuItem _showCurrentJob;

    protected ColumnTextArea _stateChanged;
    protected ColumnTextArea _bankAVSN;
    protected ColumnTextArea _bankBVSN;
    protected ColumnTextArea _statusWord;
    protected ColumnTextArea _activeBank;
    protected ColumnTextArea _scanNumber;
    protected ColumnTextArea _scanName;
    protected ColumnTextArea _position;
    protected ColumnTextArea _playRate;
    protected ColumnTextArea _dataMJD;
    protected ColumnTextArea _currentJob;

    protected int _positionStateChanged;
    protected int _positionBankAVSN;
    protected int _positionBankBVSN;
    protected int _positionStatusWord;
    protected int _positionActiveBank;
    protected int _positionScanNumber;
    protected int _positionScanName;
    protected int _positionPosition;
    protected int _positionPlayRate;
    protected int _positionDataMJD;
    protected int _positionCurrentJob;

    protected boolean _adjustStateChanged;
    protected boolean _adjustBankAVSN;
    protected boolean _adjustBankBVSN;
    protected boolean _adjustStatusWord;
    protected boolean _adjustActiveBank;
    protected boolean _adjustScanNumber;
    protected boolean _adjustScanName;
    protected boolean _adjustPosition;
    protected boolean _adjustPlayRate;
    protected boolean _adjustDataMJD;
    protected boolean _adjustCurrentJob;

}
