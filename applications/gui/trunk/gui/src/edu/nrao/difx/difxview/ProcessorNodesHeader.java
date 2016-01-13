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
/*
 * This class acts as a container for a list of cluster nodes (used in the
 * HardwareMonitorPanel).  It has a popup menu for activating or removing
 * different data displays and headers for each of them.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.BrowserNode;
import mil.navy.usno.widgetlib.ZMenuItem;
import mil.navy.usno.widgetlib.FormattedTextField;
import mil.navy.usno.widgetlib.NumberBox;
import javax.swing.JMenuItem;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JButton;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.Cursor;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.MouseInfo;

import java.util.Iterator;
import javax.swing.JOptionPane;

import javax.swing.event.EventListenerList;

public class ProcessorNodesHeader extends BrowserNode {
    
    public ProcessorNodesHeader( String name, SystemSettings settings ) {
        super( name );
        _normalCursor = this.getCursor();
        _columnAdjustCursor = new Cursor( Cursor.W_RESIZE_CURSOR );
        _settings = settings;
        initializeDisplaySettings();
        setChildColumnWidths();
        _popupButton.setVisible( true );
        _columnChangeListeners = new EventListenerList();
        //  The header is not drawn until a child is added (i.e. the header has
        //  something under it).  This is done in the addChild() function.
        this.showThis( false );
    }
    
    @Override
    public void createAdditionalItems() {
        _numCPUs = new ColumnTextArea( "CPUs" );
        _numCPUs.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showNumCPUs.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _numCPUs );
        _threadsUsed = new ColumnTextArea( "Threads" );
        _threadsUsed.setToolTipText( "The number of threads the GUI believes is in use,\n"
                + "and the total number available.  These numbers are not\n"
                + "reported by the processor itself - the GUI infers them." );
        _threadsUsed.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showThreadsUsed.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _threadsUsed );
        _numCores = new ColumnTextArea( "Cores" );
        _numCores.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showNumCores.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _numCores );
        _bogusGHz = new ColumnTextArea( "GHz" );
        _bogusGHz.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showBogusGHz.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _bogusGHz );
        _type = new ColumnTextArea( "Type Code" );
        _type.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showType.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _type );
        _typeString = new ColumnTextArea( "Type" );
        _typeString.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showTypeString.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _typeString );
        _state = new ColumnTextArea( "State" );
        _state.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showState.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _state );
        _enabled = new ColumnTextArea( "Enabled" );
        _enabled.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showEnabled.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _enabled );
        _cpuLoad = new ColumnTextArea( "CPU Usage" );
        _cpuLoad.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showCpuLoad.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _cpuLoad );
        _cpuLoadPlot = new ColumnTextArea( "CPU Usage" );
        _cpuLoadPlot.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showCpuLoadPlot.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _cpuLoadPlot );
        _usedMem = new ColumnTextArea( "Used Mem" );
        _usedMem.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showUsedMem.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _usedMem );
        _totalMem = new ColumnTextArea( "Total Mem");
        _totalMem.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showTotalMem.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _totalMem );
        _memLoad = new ColumnTextArea( "Mem Usage" );
        _memLoad.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showMemLoad.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _memLoad );
        _memLoadPlot = new ColumnTextArea( "Mem Usage" );
        _memLoadPlot.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showMemLoadPlot.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _memLoadPlot );
        _netRxRate = new ColumnTextArea( "Rx Rate" );
        _netRxRate.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showNetRxRate.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _netRxRate );
        _netTxRate = new ColumnTextArea( "Tx Rate" );
        _netTxRate.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showNetTxRate.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _netTxRate );

        _activeJob = new ColumnTextArea( "Active Job" );
        _activeJob.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showActiveJob.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _activeJob );
        _dataConsumed = new ColumnTextArea( "Consumed" );
        _dataConsumed.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showDataConsumed.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _dataConsumed );
        _inputDatarate = new ColumnTextArea( "Input Rate" );
        _inputDatarate.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showInputDatarate.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _inputDatarate );
        _memoryUsage = new ColumnTextArea( "Mem Usage" );
        _memoryUsage.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showMemoryUsage.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _memoryUsage );
        _numBufElements = new ColumnTextArea( "Buf Elements" );
        _numBufElements.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showNumBufElements.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _numBufElements );
        _startBufElement = new ColumnTextArea( "Start Element" );
        _startBufElement.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showStartBufElement.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _startBufElement );
        _activeBufElement = new ColumnTextArea( "Active Element" );
        _activeBufElement.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showActiveBufElement.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _activeBufElement );
        _threadID = new ColumnTextArea( "Thread ID" );
        _threadID.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showThreadID.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _threadID );
        _processMicrosec = new ColumnTextArea( "Process microsec" );
        _processMicrosec.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showProcessMicrosec.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _processMicrosec );
        _subintsLost = new ColumnTextArea( "Subints Lost" );
        _subintsLost.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showSubintsLost.setState( false );
                columnChangeActivity();
            }
        });
        this.add( _subintsLost );
        
        //  Create a popup menu that allows us to turn things on and off
        _popup = new JPopupMenu();
        JMenuItem selectAllItem = new JMenuItem( "Select All" );
        selectAllItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectAll( true );
            }
        });
        _popup.add( selectAllItem );
        JMenuItem unselectAllItem = new JMenuItem( "Unselect All" );
        unselectAllItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectAll( false );
            }
        });
        _popup.add( unselectAllItem );
        JMenu rebootMenu = new JMenu( "Reboot" );
        _popup.add( rebootMenu );
        JMenuItem rebootSelected = new JMenuItem( "Selected" );
        rebootSelected.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                rebootSelected();
            }
        });
        rebootMenu.add( rebootSelected );
        JMenuItem rebootAll = new JMenuItem( "All" );
        rebootAll.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                rebootAll();
            }
        });
        rebootMenu.add( rebootAll );
        JMenu resetMenu = new JMenu( "Reset" );
        _popup.add( resetMenu );
        JMenuItem resetSelected = new JMenuItem( "Selected" );
        resetSelected.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                resetSelected();
            }
        });
        resetMenu.add( resetSelected );
        JMenuItem resetAll = new JMenuItem( "All" );
        resetAll.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                resetAll();
            }
        });
        resetMenu.add( resetAll );
        JMenu powerOffMenu = new JMenu( "Power Off" );
        _popup.add( powerOffMenu );
        JMenuItem powerOffSelected = new JMenuItem( "Selected" );
        powerOffSelected.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                powerOffSelected();
            }
        });
        powerOffMenu.add( powerOffSelected );
        JMenuItem powerOffAll = new JMenuItem( "All" );
        powerOffAll.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                powerOffAll();
            }
        });
        powerOffMenu.add( powerOffAll );
        _popup.add( new JSeparator() );
        ZMenuItem addProcessorItem = new ZMenuItem( "Add \"Invisible\" Node" );
        addProcessorItem.toolTip( ""
                + "Add the name of a hardware node that is \"invisible\" to\n"
                + "the GUI because it is not running <<italic>>mk5daemon<</italic>>.  To make\n"
                + "use of node in DiFX processing, the name <<bold>>must<</bold>> be\n"
                + "recognizable by the headnode.", null );
        addProcessorItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                addInvisibleNode();
            }
        });
        _popup.add( addProcessorItem );
        ZMenuItem removeProcessorItem = new ZMenuItem( "Remove Selected \"Invisible\" Nodes" );
        removeProcessorItem.toolTip( ""
                + "Remove the selected \"invisible\" nodes.", null );
        removeProcessorItem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                removeInvisibleNodes();
            }
        });
        _popup.add( removeProcessorItem );
        _popup.add( new JSeparator() );
        JMenuItem menuItem;
        menuItem = new JMenuItem( _label.getText() + " Display Options:" );
        _popup.add( menuItem );
        _popup.add( new JSeparator() );
        _showIgnored = new JCheckBoxMenuItem( "Show \"Ignored\" Nodes" );
        _showIgnored.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showIgnored );
        JMenuItem allItem = new JMenuItem( "Show All Columns" );
        allItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                activateAll();
            }
        } );
        _popup.add( allItem );
        _broadcastMonitor = new JCheckBoxMenuItem( "Broadcast Monitor" );
        _broadcastMonitor.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _broadcastMonitor );
        _showNumCPUs = new JCheckBoxMenuItem( "CPUs" );
        _showNumCPUs.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showNumCPUs );
        _showThreadsUsed = new JCheckBoxMenuItem( "Threads" );
        _showThreadsUsed.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showThreadsUsed );
        _showNumCores = new JCheckBoxMenuItem( "Cores" );
        _showNumCores.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showNumCores );
        _showBogusGHz = new JCheckBoxMenuItem( "GHz" );
        _showBogusGHz.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showBogusGHz );
        _showType = new JCheckBoxMenuItem( "Type (Numeric)" );
        _showType.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showType );
        _showTypeString = new JCheckBoxMenuItem( "Type" );
        _showTypeString.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showTypeString );
        _showState = new JCheckBoxMenuItem( "State" );
        _showState.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showState );
        _showEnabled = new JCheckBoxMenuItem( "Enabled" );
        _showEnabled.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showEnabled );
        _showCpuLoad = new JCheckBoxMenuItem( "CPU Usage" );
        _showCpuLoad.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showCpuLoad );
        _showCpuLoadPlot = new JCheckBoxMenuItem( "CPU Usage Plot" );
        _showCpuLoadPlot.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showCpuLoadPlot );
        _showUsedMem = new JCheckBoxMenuItem( "Used Memory" );
        _showUsedMem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showUsedMem );
        _showTotalMem = new JCheckBoxMenuItem( "Total Memory" );
        _showTotalMem.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showTotalMem );
        _showMemLoad = new JCheckBoxMenuItem( "Memory Usage" );
        _showMemLoad.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showMemLoad );
        _showMemLoadPlot = new JCheckBoxMenuItem( "Memory Usage Plot" );
        _showMemLoadPlot.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showMemLoadPlot );
        _showNetRxRate = new JCheckBoxMenuItem( "Net Receive Rate" );
        _showNetRxRate.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showNetRxRate );
        _showNetTxRate = new JCheckBoxMenuItem( "Net Transmit Rate" );
        _showNetTxRate.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showNetTxRate );
        
        _showActiveJob = new JCheckBoxMenuItem( "Active Job" );
        _showActiveJob.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showActiveJob );
        _showDataConsumed = new JCheckBoxMenuItem( "Data Consumed" );
        _showDataConsumed.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showDataConsumed );
        _showInputDatarate = new JCheckBoxMenuItem( "Input Data Rate" );
        _showInputDatarate.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showInputDatarate );
        _showMemoryUsage = new JCheckBoxMenuItem( "Memory Usage (from diagnostics messages)" );
        _showMemoryUsage.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showMemoryUsage );
        _showNumBufElements = new JCheckBoxMenuItem( "Buffer Status: Num Elements" );
        _showNumBufElements.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showNumBufElements );
        _showStartBufElement = new JCheckBoxMenuItem( "Buffer Status: Start Element" );
        _showStartBufElement.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showStartBufElement );
        _showActiveBufElement = new JCheckBoxMenuItem( "Buffer Status: Active Element" );
        _showActiveBufElement.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showActiveBufElement );
        _showThreadID = new JCheckBoxMenuItem( "Processing Time: Thread ID" );
        _showThreadID.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showThreadID );
        _showProcessMicrosec = new JCheckBoxMenuItem( "Processing Time: microsec" );
        _showProcessMicrosec.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showProcessMicrosec );
        _showSubintsLost = new JCheckBoxMenuItem( "Number of Subints Lost" );
        _showSubintsLost.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                columnChangeActivity();
            }
        });
        _popup.add( _showSubintsLost );
    }
    
    /*
     * This function makes everything in the popup menu visible.
     */
    public void activateAll() {
        _broadcastMonitor.setState( true );
        _showNumCPUs.setState( true );
        _showThreadsUsed.setState( true );
        _showNumCores.setState( true );
        _showBogusGHz.setState( true );
        _showType.setState( true );
        _showTypeString.setState( true );
        _showState.setState( true );
        _showEnabled.setState( true );
        _showCpuLoad.setState( true );
        _showCpuLoadPlot.setState( true );
        _showUsedMem.setState( true );
        _showTotalMem.setState( true );
        _showMemLoad.setState( true );
        _showMemLoadPlot.setState( true );
        _showNetRxRate.setState( true );
        _showNetTxRate.setState( true );
        _showActiveJob.setState( true );
        _showDataConsumed.setState( true );
        _showInputDatarate.setState( true );
        _showMemoryUsage.setState( true );
        _showNumBufElements.setState( true );
        _showStartBufElement.setState( true );
        _showActiveBufElement.setState( true );
        _showThreadID.setState( true );
        _showProcessMicrosec.setState( true );
        _showSubintsLost.setState( true );
        columnChangeActivity();
    }

    @Override
    public void positionItems() {
        super.positionItems();
        _xOff = 220;
        if ( _broadcastMonitor.getState() )
            _xOff += 14;
        _popupButton.setBounds( _xOff + 2, 2, 16, _ySize - 4 );
        _xOff += 20;
        if ( _showNumCPUs.getState() ) {
            setTextArea( _numCPUs, _settings.hardwareColumnSpecs().NumCPUs.width );
            _positionNumCPUs = _xOff;
        }
        else
            _positionNumCPUs = -100;
        if ( _showThreadsUsed.getState() ) {
            setTextArea( _threadsUsed, _settings.hardwareColumnSpecs().ThreadsUsed.width );
            _positionThreadsUsed = _xOff;
        }
        else
            _positionThreadsUsed = -100;
        if ( _showNumCores.getState() ) {
            setTextArea( _numCores, _settings.hardwareColumnSpecs().NumCores.width );
            _positionNumCores = _xOff;
        }
        else
            _positionNumCores = -100;
        if ( _showBogusGHz.getState() ) {
            setTextArea( _bogusGHz, _settings.hardwareColumnSpecs().BogusGHz.width );
            _positionBogusGHz = _xOff;
        }
        else
            _positionBogusGHz = -100;
        if ( _showType.getState() ) {
            setTextArea( _type, _settings.hardwareColumnSpecs().Type.width );
            _positionType = _xOff;
        }
        else
            _positionType = -100;
        if ( _showTypeString.getState() ) {
            setTextArea( _typeString, _settings.hardwareColumnSpecs().TypeString.width );
            _positionTypeString = _xOff;
        }
        else
            _positionTypeString = -100;
        if ( _showState.getState() ) {
            setTextArea( _state, _settings.hardwareColumnSpecs().State.width );
            _positionState = _xOff;
        }
        else
            _positionState = -100;
        if ( _showEnabled.getState() ) {
            setTextArea( _enabled, _settings.hardwareColumnSpecs().Enabled.width );
            _positionEnabled = _xOff;
        }
        else
            _positionEnabled = -100;
        if ( _showCpuLoad.getState() ) {
            setTextArea( _cpuLoad, _settings.hardwareColumnSpecs().CpuLoad.width );
            _positionCpuLoad = _xOff;
        }
        else
            _positionCpuLoad = -100;
        if ( _showCpuLoadPlot.getState() ) {
            //  If the header "CPU Usage" is already displayed for the previous
            //  column, don't repeat it.
            if ( _showCpuLoad.getState() )
                _cpuLoadPlot.setText( "" );
            else
                _cpuLoadPlot.setText( "CPU Usage" );
            setTextArea( _cpuLoadPlot, _settings.hardwareColumnSpecs().CpuLoadPlot.width );
            _positionCpuLoadPlot = _xOff;
        }
        else
            _positionCpuLoadPlot = -100;
        if ( _showUsedMem.getState() ) {
            setTextArea( _usedMem, _settings.hardwareColumnSpecs().UsedMem.width );
            _positionUsedMem = _xOff;
        }
        else
            _positionUsedMem = -100;
        if ( _showTotalMem.getState() ) {
            setTextArea( _totalMem, _settings.hardwareColumnSpecs().TotalMem.width );
            _positionTotalMem = _xOff;
        }
        else
            _positionTotalMem = -100;
        if ( _showMemLoad.getState() ) {
            setTextArea( _memLoad, _settings.hardwareColumnSpecs().MemLoad.width );
            _positionMemLoad = _xOff;
        }
        else
            _positionMemLoad = -100;
        if ( _showMemLoadPlot.getState() ) {
            //  As with the CPU plot - don't repeat a column header if it is
            //  already there.
            if ( _showMemLoad.getState() )
                _memLoadPlot.setText( "" );
            else
                _memLoadPlot.setText( "Mem Usage" );
            setTextArea( _memLoadPlot, _settings.hardwareColumnSpecs().MemLoadPlot.width );
            _positionMemLoadPlot = _xOff;
        }
        else
            _positionMemLoadPlot = -100;
        if ( _showNetRxRate.getState() ) {
            setTextArea( _netRxRate, _settings.hardwareColumnSpecs().NetRxRate.width );
            _positionNetRxRate = _xOff;
        }
        else
            _positionNetRxRate = -100;
        if ( _showNetTxRate.getState() ) {
            setTextArea( _netTxRate, _settings.hardwareColumnSpecs().NetTxRate.width );
            _positionNetTxRate = _xOff;
        }
        else
            _positionNetTxRate = -100;        
        if ( _showActiveJob.getState() ) {
            setTextArea( _activeJob, _settings.hardwareColumnSpecs().ActiveJob.width );
            _positionActiveJob = _xOff;
        }
        else
            _positionActiveJob = -100;
        if ( _showDataConsumed.getState() ) {
            setTextArea( _dataConsumed, _settings.hardwareColumnSpecs().DataConsumed.width );
            _positionDataConsumed = _xOff;
        }
        else
            _positionDataConsumed = -100;
        if ( _showInputDatarate.getState() ) {
            setTextArea( _inputDatarate, _settings.hardwareColumnSpecs().InputDatarate.width );
            _positionInputDatarate = _xOff;
        }
        else
            _positionInputDatarate = -100;
        if ( _showMemoryUsage.getState() ) {
            setTextArea( _memoryUsage, _settings.hardwareColumnSpecs().MemoryUsage.width );
            _positionMemoryUsage = _xOff;
        }
        else
            _positionMemoryUsage = -100;
        if ( _showNumBufElements.getState() ) {
            setTextArea( _numBufElements, _settings.hardwareColumnSpecs().NumBufElements.width );
            _positionNumBufElements = _xOff;
        }
        else
            _positionNumBufElements = -100;
        if ( _showStartBufElement.getState() ) {
            setTextArea( _startBufElement, _settings.hardwareColumnSpecs().StartBufElement.width );
            _positionStartBufElement = _xOff;
        }
        else
            _positionStartBufElement = -100;
        if ( _showActiveBufElement.getState() ) {
            setTextArea( _activeBufElement, _settings.hardwareColumnSpecs().ActiveBufElement.width );
            _positionActiveBufElement = _xOff;
        }
        else
            _positionActiveBufElement = -100;
        if ( _showThreadID.getState() ) {
            setTextArea( _threadID, _settings.hardwareColumnSpecs().ThreadID.width );
            _positionThreadID = _xOff;
        }
        else
            _positionThreadID = -100;
        if ( _showProcessMicrosec.getState() ) {
            setTextArea( _processMicrosec, _settings.hardwareColumnSpecs().ProcessMicrosec.width );
            _positionProcessMicrosec = _xOff;
        }
        else
            _positionProcessMicrosec = -100;
        if ( _showSubintsLost.getState() ) {
            setTextArea( _subintsLost, _settings.hardwareColumnSpecs().SubintsLost.width );
            _positionSubintsLost = _xOff;
        }
        else
            _positionSubintsLost = -100;
    }
    
    public void setTextArea( Component area, int xSize ) {
        area.setBounds( _xOff + 1, 1, xSize - 2, _ySize - 2);
        _xOff += xSize;
    }

    @Override
    public void paintComponent( Graphics g ) {
        //  Use anti-aliasing on the text (looks much better)
        Graphics2D g2 = (Graphics2D)g;
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        super.paintComponent( g );
    }
    
    public void initializeDisplaySettings() {
        _showIgnored.setState( _settings.hardwareColumnSpecs().Ignored.show );
        _broadcastMonitor.setState( _settings.hardwareColumnSpecs().broadcastMonitor.show );
        _showNumCPUs.setState( _settings.hardwareColumnSpecs().NumCPUs.show );
        _showThreadsUsed.setState( _settings.hardwareColumnSpecs().ThreadsUsed.show );
        _showNumCores.setState( _settings.hardwareColumnSpecs().NumCores.show );
        _showBogusGHz.setState( _settings.hardwareColumnSpecs().BogusGHz.show );
        _showType.setState( _settings.hardwareColumnSpecs().Type.show );
        _showTypeString.setState( _settings.hardwareColumnSpecs().TypeString.show );
        _showState.setState( _settings.hardwareColumnSpecs().State.show );
        _showEnabled.setState( _settings.hardwareColumnSpecs().Enabled.show );
        _showCpuLoad.setState( _settings.hardwareColumnSpecs().CpuLoad.show );
        _showCpuLoadPlot.setState( _settings.hardwareColumnSpecs().CpuLoadPlot.show );
        _showUsedMem.setState( _settings.hardwareColumnSpecs().UsedMem.show );
        _showTotalMem.setState( _settings.hardwareColumnSpecs().TotalMem.show );
        _showMemLoad.setState( _settings.hardwareColumnSpecs().MemLoad.show );
        _showMemLoadPlot.setState( _settings.hardwareColumnSpecs().MemLoadPlot.show );
        _showNetRxRate.setState( _settings.hardwareColumnSpecs().NetRxRate.show );
        _showNetTxRate.setState( _settings.hardwareColumnSpecs().NetTxRate.show );
        _showActiveJob.setState( _settings.hardwareColumnSpecs().ActiveJob.show );
        _showDataConsumed.setState( _settings.hardwareColumnSpecs().DataConsumed.show );
        _showInputDatarate.setState( _settings.hardwareColumnSpecs().InputDatarate.show );
        _showMemoryUsage.setState( _settings.hardwareColumnSpecs().MemoryUsage.show );
        _showNumBufElements.setState( _settings.hardwareColumnSpecs().NumBufElements.show );
        _showStartBufElement.setState( _settings.hardwareColumnSpecs().StartBufElement.show );
        _showActiveBufElement.setState( _settings.hardwareColumnSpecs().ActiveBufElement.show );
        _showThreadID.setState( _settings.hardwareColumnSpecs().ThreadID.show );
        _showProcessMicrosec.setState( _settings.hardwareColumnSpecs().ProcessMicrosec.show );
        _showSubintsLost.setState( _settings.hardwareColumnSpecs().SubintsLost.show );
    }
    
    /*
     * This functions propogates current column widths to all children.
     */
    public void setChildColumnWidths() {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            ProcessorNode thisNode = (ProcessorNode)(iter.next());
            //  Change the settings on these items to match our current specifications.
            thisNode.widthNumCPUs( _settings.hardwareColumnSpecs().NumCPUs.width );
            thisNode.widthThreadsUsed( _settings.hardwareColumnSpecs().ThreadsUsed.width );
            thisNode.widthNumCores( _settings.hardwareColumnSpecs().NumCores.width );
            thisNode.widthBogusGHz( _settings.hardwareColumnSpecs().BogusGHz.width );
            thisNode.widthType( _settings.hardwareColumnSpecs().Type.width );
            thisNode.widthTypeString( _settings.hardwareColumnSpecs().TypeString.width );
            thisNode.widthState( _settings.hardwareColumnSpecs().State.width );
            thisNode.widthEnabled( _settings.hardwareColumnSpecs().Enabled.width );
            thisNode.widthCpuLoad( _settings.hardwareColumnSpecs().CpuLoad.width );
            thisNode.widthCpuLoadPlot( _settings.hardwareColumnSpecs().CpuLoadPlot.width );
            thisNode.widthUsedMem( _settings.hardwareColumnSpecs().UsedMem.width );
            thisNode.widthTotalMem( _settings.hardwareColumnSpecs().TotalMem.width );
            thisNode.widthMemLoad( _settings.hardwareColumnSpecs().MemLoad.width );
            thisNode.widthMemLoadPlot( _settings.hardwareColumnSpecs().MemLoadPlot.width );
            thisNode.widthNetRxRate( _settings.hardwareColumnSpecs().NetRxRate.width );
            thisNode.widthNetTxRate( _settings.hardwareColumnSpecs().NetTxRate.width );
            thisNode.widthActiveJob( _settings.hardwareColumnSpecs().ActiveJob.width );
            thisNode.widthDataConsumed( _settings.hardwareColumnSpecs().DataConsumed.width );
            thisNode.widthInputDatarate( _settings.hardwareColumnSpecs().InputDatarate.width );
            thisNode.widthMemoryUsage( _settings.hardwareColumnSpecs().MemoryUsage.width );
            thisNode.widthNumBufElements( _settings.hardwareColumnSpecs().NumBufElements.width );
            thisNode.widthStartBufElement( _settings.hardwareColumnSpecs().StartBufElement.width );
            thisNode.widthActiveBufElement( _settings.hardwareColumnSpecs().ActiveBufElement.width );
            thisNode.widthThreadID( _settings.hardwareColumnSpecs().ThreadID.width );
            thisNode.widthProcessMicrosec( _settings.hardwareColumnSpecs().ProcessMicrosec.width );
            thisNode.widthSubintsLost( _settings.hardwareColumnSpecs().SubintsLost.width );
            thisNode.updateUI();
        }
    }
    
    @Override
    public void addChild( BrowserNode newNode ) {
        super.addChild( newNode );
        sortByName();
        this.showThis( true );
        setChildColumnWidths();
        columnChangeActivity();
    }
    
    /*
     * Set the selection on all children.
     */
    public void selectAll( boolean selection ) {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            ((ProcessorNode)(iter.next())).selected( selection );
        }
    }
    
    /*
     * Perform various actions on all or selected children.
     */
    public void rebootAll() {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            ((ProcessorNode)(iter.next())).sendDiFXCommandMessage( "Reboot" );
            try { Thread.sleep( 500 ); } catch ( Exception e ) {}
        }
    }
    public void rebootSelected() {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            BrowserNode thisNode = iter.next();
            if ( thisNode.selected() ) {
                ((ProcessorNode)(thisNode)).sendDiFXCommandMessage( "Reboot" );
                try { Thread.sleep( 500 ); } catch ( Exception e ) {}
            }
        }
    }
    public void resetAll() {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            ((ProcessorNode)(iter.next())).sendDiFXCommandMessage( "ResetMark5" );
            try { Thread.sleep( 500 ); } catch ( Exception e ) {}
        }
    }
    public void resetSelected() {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            BrowserNode thisNode = iter.next();
            if ( thisNode.selected() ) {
                ((ProcessorNode)(thisNode)).sendDiFXCommandMessage( "ResetMark5" );
                try { Thread.sleep( 500 ); } catch ( Exception e ) {}
            }
        }
    }
    public void powerOffAll() {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            ((ProcessorNode)(iter.next())).sendDiFXCommandMessage( "Power Off" );
            try { Thread.sleep( 500 ); } catch ( Exception e ) {}
        }
    }
    public void powerOffSelected() {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            BrowserNode thisNode = iter.next();
            if ( thisNode.selected() ) {
                ((ProcessorNode)(thisNode)).sendDiFXCommandMessage( "Power Off" );
                try { Thread.sleep( 500 ); } catch ( Exception e ) {}
            }
        }
    }
    
    /*
     * Popup dialog to add a new processor.
     */
    public class AddNode extends JDialog {
        public AddNode( int x, int y, boolean isMark5 ) {
            super();
            setLayout( null );
            if ( isMark5 )
                setBounds( x, y, 500, 100 );
            else
                setBounds( x, y, 500, 140 );
            JLabel nodeLabel = new JLabel( "Node Name:" );
            nodeLabel.setHorizontalAlignment( JLabel.RIGHT );
            nodeLabel.setBounds( 10, 10, 100, 25 );
            this.add( nodeLabel );
            nodeName = new FormattedTextField();
            nodeName.employChangeBackground( false );
            nodeName.setBounds( 115, 10, 375, 25 );
            nodeName.toolTip( "This is the host name of the \"invisible\" node.\n"
                    + "This name must be how the headnode used in DiFX processing\n"
                    + "addresses the node for it to be useful.", null );
            this.add( nodeName );
            if ( !isMark5 ) {
                JLabel threadLabel = new JLabel( "Cores:" );
                threadLabel.setHorizontalAlignment( JLabel.RIGHT );
                threadLabel.setBounds( 10, 40, 100, 25 );
                this.add( threadLabel );
                threads = new NumberBox();
                threads.employChangeBackground( false );
                threads.minimum( 1 );
                threads.precision( 0 );
                threads.setBounds( 115, 40, 100, 25 );
                threads.intValue( 1 );
                threads.toolTip( "The default number of cores that will be used when running\n"
                        + "with this processor.", null );
                this.add( threads );
            }
            _this = this;
            int ypos = 40;
            if ( isMark5 )
                ypos = 40;
            JButton cancelButton = new JButton( "Cancel" );
            cancelButton.setBounds( 285, ypos, 100, 25 );
            cancelButton.addActionListener(new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    _this.setVisible( false );
                }
            });
            this.add( cancelButton );
            JButton okButton = new JButton( "OK" );
            okButton.setBounds( 390, ypos, 100, 25 );
            okButton.addActionListener(new ActionListener() {
                public void actionPerformed( ActionEvent e ) {
                    ok = true;
                    _this.setVisible( false );
                }
            });
            this.add( okButton );
            this.setResizable( false );
            this.setModal( true );
            if ( isMark5 )
                this.setTitle( "Add \"Invisible\" Mark 5 Unit" );
            else
                this.setTitle( "Add \"Invisible\" Processor" );
        }
        public FormattedTextField nodeName;
        public NumberBox threads;
        public boolean ok;
        public AddNode _this;
    }
    
    /*
     * Add an "invisible" processor name to the processor list.  Invisible processors
     * do not have mk5daemon running, and thus won't appear in the list on their own.
     * Once added, the new node will look like a processor and
     * be available when choosing nodes for DiFX processing, but its data will not be
     * updated.  The user is prompted for a name and default number of threads.
     */
    public void addInvisibleNode() {
        //  Create a dialog allowing the user to enter a name, threads, etc.
        AddNode dialog = new AddNode( MouseInfo.getPointerInfo().getLocation().x,
                MouseInfo.getPointerInfo().getLocation().y, false );
        dialog.setVisible( true );
        if ( dialog.ok ) {
            if ( dialog.nodeName.getText().length() > 0 ) {
                if ( !checkAddNode( dialog.nodeName.getText(), dialog.threads.intValue() ) )
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
    public boolean checkAddNode( String name, int cores ) {
        boolean found = false;
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext() && !found; ) {
            ProcessorNode thisNode = (ProcessorNode)(iter.next());
            if ( thisNode.name().contentEquals( name ) ) {
                found = true;
            }
        }
        if ( found )
            return false;
        else {
            ProcessorNode newNode = new ProcessorNode( name, _settings );
            newNode.currentState( "invisible" );
            newNode.numCores( cores );
            this.addChild( newNode );
            return true;
        }
    }
    
    /*
     * Remove any invisible processors from the list that have been selected.
     */
    public void removeInvisibleNodes() {
        boolean keepGoing = true;
        while ( keepGoing ) {
            keepGoing = false;
            BrowserNode deleteNode = null;
            for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext() && deleteNode == null; ) {
                ProcessorNode thisNode = (ProcessorNode)(iter.next());
                if ( thisNode.selected() && thisNode.currentState().contentEquals( "invisible" ) ) {
                    deleteNode = thisNode;
                    keepGoing = true;
                }
            }
            if ( deleteNode != null ) {
                this.removeChild( deleteNode );
            }
        }
    }
    
    /*
     * Change the intervals before network activity lights turn yellow (warning)
     * or red (error).  Intervals are in tenths of seconds.
     */
    public void inactivitySettings( int warning, int alert ) {
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            BrowserNode thisNode = iter.next();
            ((ProcessorNode)(thisNode)).inactivitySettings( warning, alert );
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
        _adjustNumCPUs = false;
        _adjustThreadsUsed = false;
        _adjustNumCores = false;
        _adjustBogusGHz = false;
        _adjustType = false;
        _adjustTypeString = false;
        _adjustState = false;
        _adjustEnabled = false;
        _adjustCpuLoad = false;
        _adjustCpuLoadPlot = false;
        _adjustUsedMem = false;
        _adjustTotalMem = false;
        _adjustMemLoad = false;
        _adjustMemLoadPlot = false;
        _adjustNetRxRate = false;
        _adjustNetTxRate = false;
        _adjustActiveJob = false;
        _adjustDataConsumed = false;
        _adjustInputDatarate = false;
        _adjustMemoryUsage = false;
        _adjustNumBufElements = false;
        _adjustStartBufElement = false;
         _adjustActiveBufElement = false;
        _adjustThreadID = false;
        _adjustProcessMicrosec = false;
        _adjustSubintsLost = false;
        if ( e.getX() > _positionNumCPUs - 3 && e.getX() < _positionNumCPUs + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustNumCPUs = true;
        }
        else if ( e.getX() > _positionThreadsUsed - 3 && e.getX() < _positionThreadsUsed + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustThreadsUsed = true;
        }
        else if ( e.getX() > _positionNumCores - 3 && e.getX() < _positionNumCores + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustNumCores = true;
        }
        else if ( e.getX() > _positionBogusGHz - 3 && e.getX() < _positionBogusGHz + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustBogusGHz = true;
        }
        else if ( e.getX() > _positionType - 3 && e.getX() < _positionType + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustType = true;
        }
        else if ( e.getX() > _positionTypeString - 3 && e.getX() < _positionTypeString + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustTypeString = true;
        }
        else if ( e.getX() > _positionState - 3 && e.getX() < _positionState + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustState = true;
        }
        else if ( e.getX() > _positionEnabled - 3 && e.getX() < _positionEnabled + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustEnabled = true;
        }
        else if ( e.getX() > _positionCpuLoad - 3 && e.getX() < _positionCpuLoad + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustCpuLoad = true;
        }
        else if ( e.getX() > _positionCpuLoadPlot - 3 && e.getX() < _positionCpuLoadPlot + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustCpuLoadPlot = true;
        }
        else if ( e.getX() > _positionUsedMem - 3 && e.getX() < _positionUsedMem + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustUsedMem = true;
        }
        else if ( e.getX() > _positionTotalMem - 3 && e.getX() < _positionTotalMem + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustTotalMem = true;
        }
        else if ( e.getX() > _positionMemLoad - 3 && e.getX() < _positionMemLoad + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustMemLoad = true;
        }
        else if ( e.getX() > _positionMemLoadPlot - 3 && e.getX() < _positionMemLoadPlot + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustMemLoadPlot = true;
        }
        else if ( e.getX() > _positionNetRxRate - 3 && e.getX() < _positionNetRxRate + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustNetRxRate = true;
        }
        else if ( e.getX() > _positionNetTxRate - 3 && e.getX() < _positionNetTxRate + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustNetTxRate = true;
        }        
        else if ( e.getX() > _positionActiveJob - 3 && e.getX() < _positionActiveJob + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustActiveJob = true;
        }
        else if ( e.getX() > _positionDataConsumed - 3 && e.getX() < _positionDataConsumed + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustDataConsumed = true;
        }
        else if ( e.getX() > _positionInputDatarate - 3 && e.getX() < _positionInputDatarate + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustInputDatarate = true;
        }
        else if ( e.getX() > _positionMemoryUsage - 3 && e.getX() < _positionMemoryUsage + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustMemoryUsage = true;
        }
        else if ( e.getX() > _positionNumBufElements - 3 && e.getX() < _positionNumBufElements + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustNumBufElements = true;
        }
        else if ( e.getX() > _positionStartBufElement - 3 && e.getX() < _positionStartBufElement + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustStartBufElement = true;
        }
        else if ( e.getX() > _positionActiveBufElement - 3 && e.getX() < _positionActiveBufElement + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustActiveBufElement = true;
        }
        else if ( e.getX() > _positionThreadID - 3 && e.getX() < _positionThreadID + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustThreadID = true;
        }
        else if ( e.getX() > _positionProcessMicrosec - 3 && e.getX() < _positionProcessMicrosec + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustProcessMicrosec = true;
        }
        else if ( e.getX() > _positionSubintsLost - 3 && e.getX() < _positionSubintsLost + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustSubintsLost = true;
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
        if ( _adjustNumCPUs ) {
            _startWidth = _settings.hardwareColumnSpecs().NumCPUs.width;
            _startX = e.getX();
        }
        else if ( _adjustThreadsUsed ) {
            _startWidth = _settings.hardwareColumnSpecs().ThreadsUsed.width;
            _startX = e.getX();
        }
        else if ( _adjustNumCores ) {
            _startWidth = _settings.hardwareColumnSpecs().NumCores.width;
            _startX = e.getX();
        }
        else if ( _adjustBogusGHz ) {
            _startWidth = _settings.hardwareColumnSpecs().BogusGHz.width;
            _startX = e.getX();
        }
        else if ( _adjustType ) {
            _startWidth = _settings.hardwareColumnSpecs().Type.width;
            _startX = e.getX();
        }
        else if ( _adjustTypeString ) {
            _startWidth = _settings.hardwareColumnSpecs().TypeString.width;
            _startX = e.getX();
        }
        else if ( _adjustState ) {
            _startWidth = _settings.hardwareColumnSpecs().State.width;
            _startX = e.getX();
        }
        else if ( _adjustEnabled ) {
            _startWidth = _settings.hardwareColumnSpecs().Enabled.width;
            _startX = e.getX();
        }
        else if ( _adjustCpuLoad ) {
            _startWidth = _settings.hardwareColumnSpecs().CpuLoad.width;
            _startX = e.getX();
        }
        else if ( _adjustCpuLoadPlot ) {
            _startWidth = _settings.hardwareColumnSpecs().CpuLoadPlot.width;
            _startX = e.getX();
        }
        else if ( _adjustUsedMem ) {
            _startWidth = _settings.hardwareColumnSpecs().UsedMem.width;
            _startX = e.getX();
        }
        else if ( _adjustTotalMem ) {
            _startWidth = _settings.hardwareColumnSpecs().TotalMem.width;
            _startX = e.getX();
        }
        else if ( _adjustMemLoad ) {
            _startWidth = _settings.hardwareColumnSpecs().MemLoad.width;
            _startX = e.getX();
        }
        else if ( _adjustMemLoadPlot ) {
            _startWidth = _settings.hardwareColumnSpecs().MemLoadPlot.width;
            _startX = e.getX();
        }
        else if ( _adjustNetRxRate ) {
            _startWidth = _settings.hardwareColumnSpecs().NetRxRate.width;
            _startX = e.getX();
        }
        else if ( _adjustNetTxRate ) {
            _startWidth = _settings.hardwareColumnSpecs().NetTxRate.width;
            _startX = e.getX();
        }
        else if ( _adjustActiveJob ) {
            _startWidth = _settings.hardwareColumnSpecs().ActiveJob.width;
            _startX = e.getX();
        }
        else if ( _adjustDataConsumed ) {
            _startWidth = _settings.hardwareColumnSpecs().DataConsumed.width;
            _startX = e.getX();
        }
        else if ( _adjustInputDatarate ) {
            _startWidth = _settings.hardwareColumnSpecs().InputDatarate.width;
            _startX = e.getX();
        }
        else if ( _adjustMemoryUsage ) {
            _startWidth = _settings.hardwareColumnSpecs().MemoryUsage.width;
            _startX = e.getX();
        }
        else if ( _adjustNumBufElements ) {
            _startWidth = _settings.hardwareColumnSpecs().NumBufElements.width;
            _startX = e.getX();
        }
        else if ( _adjustStartBufElement ) {
            _startWidth = _settings.hardwareColumnSpecs().StartBufElement.width;
            _startX = e.getX();
        }
        else if ( _adjustActiveBufElement ) {
            _startWidth = _settings.hardwareColumnSpecs().ActiveBufElement.width;
            _startX = e.getX();
        }
        else if ( _adjustThreadID ) {
            _startWidth = _settings.hardwareColumnSpecs().ThreadID.width;
            _startX = e.getX();
        }
        else if ( _adjustProcessMicrosec ) {
            _startWidth = _settings.hardwareColumnSpecs().ProcessMicrosec.width;
            _startX = e.getX();
        }
        else if ( _adjustSubintsLost ) {
            _startWidth = _settings.hardwareColumnSpecs().SubintsLost.width;
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
        if ( _adjustNumCPUs ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().NumCPUs.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustThreadsUsed ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().ThreadsUsed.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustNumCores ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().NumCores.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustBogusGHz ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().BogusGHz.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustType ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().Type.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustTypeString ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().TypeString.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustState ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().State.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustEnabled ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().Enabled.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustCpuLoad ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().CpuLoad.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustCpuLoadPlot ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().CpuLoadPlot.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustUsedMem ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().UsedMem.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustTotalMem ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().TotalMem.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustMemLoad ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().MemLoad.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustMemLoadPlot ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().MemLoadPlot.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustNetRxRate ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().NetRxRate.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustNetTxRate ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().NetTxRate.width = _startWidth + e.getX() - _startX;
        }

        else if ( _adjustActiveJob ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().ActiveJob.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustDataConsumed ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().DataConsumed.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustInputDatarate ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().InputDatarate.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustMemoryUsage ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().MemoryUsage.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustNumBufElements ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().NumBufElements.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustStartBufElement ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().StartBufElement.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustActiveBufElement ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().ActiveBufElement.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustThreadID ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().ThreadID.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustProcessMicrosec ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().ProcessMicrosec.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustSubintsLost ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.hardwareColumnSpecs().SubintsLost.width = _startWidth + e.getX() - _startX;
        }
        
        setChildColumnWidths();
        Object[] listeners = _columnChangeListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
    
    public void addColumnChangeListener( ActionListener a ) {
        _columnChangeListeners.add( ActionListener.class, a );
    }

    public void columnChangeActivity() {
        updateDisplayedData();
        Object[] listeners = _columnChangeListeners.getListenerList();
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( new ActionEvent( this, ActionEvent.ACTION_PERFORMED, "" ) );
        }
    }
   
    public void updateDisplayedData() {
        //  Run through the list of all "child" nodes, which are all of the listed
        //  cluster nodes.
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            ProcessorNode thisNode = (ProcessorNode)(iter.next());
            //  Change the settings on these items to match our current specifications.
            thisNode.showIgnored( _showIgnored.getState() );
            thisNode.showNetworkActivity( _broadcastMonitor.getState() );
            thisNode.showNumCPUs( _showNumCPUs.getState() );
            thisNode.showThreadsUsed( _showThreadsUsed.getState() );
            thisNode.showNumCores( _showNumCores.getState() );
            thisNode.showBogusGHz( _showBogusGHz.getState() );
            thisNode.showType( _showType.getState() );
            thisNode.showTypeString( _showTypeString.getState() );
            thisNode.showState( _showState.getState() );
            thisNode.showEnabled( _showEnabled.getState() );
            thisNode.showCpuLoad( _showCpuLoad.getState() );
            thisNode.showCpuLoadPlot( _showCpuLoadPlot.getState() );
            thisNode.showUsedMem( _showUsedMem.getState() );
            thisNode.showTotalMem( _showTotalMem.getState() );
            thisNode.showMemLoad( _showMemLoad.getState() );
            thisNode.showMemLoadPlot( _showMemLoadPlot.getState() );
            thisNode.showNetRxRate( _showNetRxRate.getState() );
            thisNode.showNetTxRate( _showNetTxRate.getState() );
            thisNode.showActiveJob( _showActiveJob.getState() );
            thisNode.showDataConsumed( _showDataConsumed.getState() );
            thisNode.showInputDatarate( _showInputDatarate.getState() );
            thisNode.showMemoryUsage( _showMemoryUsage.getState() );
            thisNode.showNumBufElements( _showNumBufElements.getState() );
            thisNode.showStartBufElement( _showStartBufElement.getState() );
            thisNode.showActiveBufElement( _showActiveBufElement.getState() );
            thisNode.showThreadID( _showThreadID.getState() );
            thisNode.showProcessMicrosec( _showProcessMicrosec.getState() );
            thisNode.showSubintsLost( _showSubintsLost.getState() );
            thisNode.updateUI();
        }
        //  Update the headers as well.
        _numCPUs.setVisible( _showNumCPUs.getState() );
        _threadsUsed.setVisible( _showThreadsUsed.getState() );
        _numCores.setVisible( _showNumCores.getState() );
        _bogusGHz.setVisible( _showBogusGHz.getState() );
        _type.setVisible( _showType.getState() );
        _typeString.setVisible( _showTypeString.getState() );
        _state.setVisible( _showState.getState() );
        _enabled.setVisible( _showEnabled.getState() );
        _cpuLoad.setVisible( _showCpuLoad.getState() );
        _cpuLoadPlot.setVisible( _showCpuLoadPlot.getState() );
        _usedMem.setVisible( _showUsedMem.getState() );
        _totalMem.setVisible( _showTotalMem.getState() );
        _memLoad.setVisible( _showMemLoad.getState() );
        _memLoadPlot.setVisible( _showMemLoadPlot.getState() );
        _netRxRate.setVisible( _showNetRxRate.getState() );
        _netTxRate.setVisible( _showNetTxRate.getState() );
        _activeJob.setVisible( _showActiveJob.getState() );
        _dataConsumed.setVisible( _showDataConsumed.getState() );
        _inputDatarate.setVisible( _showInputDatarate.getState() );
        _memoryUsage.setVisible( _showMemoryUsage.getState() );
        _numBufElements.setVisible( _showNumBufElements.getState() );
        _startBufElement.setVisible( _showStartBufElement.getState() );
        _activeBufElement.setVisible( _showActiveBufElement.getState() );
        _threadID.setVisible( _showThreadID.getState() );
        _processMicrosec.setVisible( _showProcessMicrosec.getState() );
        _subintsLost.setVisible( _showSubintsLost.getState() );
        this.updateUI();
        //  And the saved settings.
        _settings.hardwareColumnSpecs().Ignored.show = _showIgnored.getState();
        _settings.hardwareColumnSpecs().broadcastMonitor.show = _broadcastMonitor.getState();
        _settings.hardwareColumnSpecs().NumCPUs.show = _showNumCPUs.getState();
        _settings.hardwareColumnSpecs().ThreadsUsed.show = _showThreadsUsed.getState();
        _settings.hardwareColumnSpecs().NumCores.show = _showNumCores.getState();
        _settings.hardwareColumnSpecs().BogusGHz.show = _showBogusGHz.getState();
        _settings.hardwareColumnSpecs().Type.show = _showType.getState();
        _settings.hardwareColumnSpecs().TypeString.show = _showTypeString.getState();
        _settings.hardwareColumnSpecs().State.show = _showState.getState();
        _settings.hardwareColumnSpecs().Enabled.show = _showEnabled.getState();
        _settings.hardwareColumnSpecs().CpuLoad.show = _showCpuLoad.getState();
        _settings.hardwareColumnSpecs().CpuLoadPlot.show = _showCpuLoadPlot.getState();
        _settings.hardwareColumnSpecs().UsedMem.show = _showUsedMem.getState();
        _settings.hardwareColumnSpecs().TotalMem.show = _showTotalMem.getState();
        _settings.hardwareColumnSpecs().MemLoad.show = _showMemLoad.getState();
        _settings.hardwareColumnSpecs().MemLoadPlot.show = _showMemLoadPlot.getState();
        _settings.hardwareColumnSpecs().NetRxRate.show = _showNetRxRate.getState();
        _settings.hardwareColumnSpecs().NetTxRate.show = _showNetTxRate.getState();        
        _settings.hardwareColumnSpecs().ActiveJob.show = _showActiveJob.getState();
        _settings.hardwareColumnSpecs().DataConsumed.show = _showDataConsumed.getState();
        _settings.hardwareColumnSpecs().InputDatarate.show = _showInputDatarate.getState();
        _settings.hardwareColumnSpecs().MemoryUsage.show = _showMemoryUsage.getState();
        _settings.hardwareColumnSpecs().NumBufElements.show = _showNumBufElements.getState();
        _settings.hardwareColumnSpecs().StartBufElement.show = _showStartBufElement.getState();
        _settings.hardwareColumnSpecs().ActiveBufElement.show = _showActiveBufElement.getState();
        _settings.hardwareColumnSpecs().ThreadID.show = _showThreadID.getState();
        _settings.hardwareColumnSpecs().ProcessMicrosec.show = _showProcessMicrosec.getState();
        _settings.hardwareColumnSpecs().SubintsLost.show = _showSubintsLost.getState();
    }
    
    protected JCheckBoxMenuItem _showIgnored;
    protected JCheckBoxMenuItem _broadcastMonitor;
    protected JCheckBoxMenuItem _showNumCPUs;
    protected JCheckBoxMenuItem _showThreadsUsed;
    protected JCheckBoxMenuItem _showNumCores;
    protected JCheckBoxMenuItem _showBogusGHz;
    protected JCheckBoxMenuItem _showType;
    protected JCheckBoxMenuItem _showTypeString;
    protected JCheckBoxMenuItem _showState;
    protected JCheckBoxMenuItem _showEnabled;
    protected JCheckBoxMenuItem _showCpuLoad;
    protected JCheckBoxMenuItem _showCpuLoadPlot;
    protected JCheckBoxMenuItem _showUsedMem;
    protected JCheckBoxMenuItem _showTotalMem;
    protected JCheckBoxMenuItem _showMemLoad;
    protected JCheckBoxMenuItem _showMemLoadPlot;
    protected JCheckBoxMenuItem _showNetRxRate;
    protected JCheckBoxMenuItem _showNetTxRate;
    
    protected JCheckBoxMenuItem _showActiveJob;
    protected JCheckBoxMenuItem _showDataConsumed;
    protected JCheckBoxMenuItem _showInputDatarate;
    protected JCheckBoxMenuItem _showMemoryUsage;
    protected JCheckBoxMenuItem _showNumBufElements;
    protected JCheckBoxMenuItem _showStartBufElement;
    protected JCheckBoxMenuItem _showActiveBufElement;
    protected JCheckBoxMenuItem _showThreadID;
    protected JCheckBoxMenuItem _showProcessMicrosec;
    protected JCheckBoxMenuItem _showSubintsLost;

    ColumnTextArea _numCPUs;
    ColumnTextArea _threadsUsed;
    ColumnTextArea _numCores;
    ColumnTextArea _bogusGHz;
    ColumnTextArea _type;
    ColumnTextArea _typeString;
    ColumnTextArea _state;
    ColumnTextArea _enabled;
    ColumnTextArea _cpuLoad;
    ColumnTextArea _cpuLoadPlot;
    ColumnTextArea _usedMem;
    ColumnTextArea _totalMem;
    ColumnTextArea _memLoad;
    ColumnTextArea _memLoadPlot;
    ColumnTextArea _netRxRate;
    ColumnTextArea _netTxRate;

    ColumnTextArea _activeJob;
    ColumnTextArea _dataConsumed;
    ColumnTextArea _inputDatarate;
    ColumnTextArea _memoryUsage;
    ColumnTextArea _numBufElements;
    ColumnTextArea _startBufElement;
    ColumnTextArea _activeBufElement;
    ColumnTextArea _threadID;
    ColumnTextArea _processMicrosec;
    ColumnTextArea _subintsLost;

    int _positionNumCPUs;
    int _positionThreadsUsed;
    int _positionNumCores;
    int _positionBogusGHz;
    int _positionType;
    int _positionTypeString;
    int _positionState;
    int _positionEnabled;
    int _positionCpuLoad;
    int _positionCpuLoadPlot;
    int _positionUsedMem;
    int _positionTotalMem;
    int _positionMemLoad;
    int _positionMemLoadPlot;
    int _positionNetRxRate;
    int _positionNetTxRate;

    int _positionActiveJob;
    int _positionDataConsumed;
    int _positionInputDatarate;
    int _positionMemoryUsage;
    int _positionNumBufElements;
    int _positionStartBufElement;
    int _positionActiveBufElement;
    int _positionThreadID;
    int _positionProcessMicrosec;
    int _positionSubintsLost;

    boolean _adjustNumCPUs;
    boolean _adjustThreadsUsed;
    boolean _adjustNumCores;
    boolean _adjustBogusGHz;
    boolean _adjustType;
    boolean _adjustTypeString;
    boolean _adjustState;
    boolean _adjustEnabled;
    boolean _adjustCpuLoad;
    boolean _adjustCpuLoadPlot;
    boolean _adjustUsedMem;
    boolean _adjustTotalMem;
    boolean _adjustMemLoad;
    boolean _adjustMemLoadPlot;
    boolean _adjustNetRxRate;
    boolean _adjustNetTxRate;

    boolean _adjustActiveJob;
    boolean _adjustDataConsumed;
    boolean _adjustInputDatarate;
    boolean _adjustMemoryUsage;
    boolean _adjustNumBufElements;
    boolean _adjustStartBufElement;
    boolean _adjustActiveBufElement;
    boolean _adjustThreadID;
    boolean _adjustProcessMicrosec;
    boolean _adjustSubintsLost;

    protected int _xOff;
    
    protected Cursor _columnAdjustCursor;
    protected Cursor _normalCursor;
    
    protected int _startWidth;
    protected int _startX;
    
    protected SystemSettings _settings;
    protected EventListenerList _columnChangeListeners;
    
}
