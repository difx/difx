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
 * Provides a header line for jobs in the queue browser with titles for different
 * displayed items.  Items can be added, removed, and resized.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.BrowserNode;
import javax.swing.JMenuItem;
import javax.swing.JMenu;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JCheckBoxMenuItem;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.Cursor;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import java.util.Iterator;
import java.util.ArrayDeque;

public class JobNodesHeader extends BrowserNode {
    
    public JobNodesHeader( SystemSettings settings ) {
        super( "" );
        _settings = settings;
        _normalCursor = this.getCursor();
        _columnAdjustCursor = new Cursor( Cursor.W_RESIZE_CURSOR );
        initializeDisplaySettings();
        _popupButton.setVisible( true );
    }
    
    @Override
    public void createAdditionalItems() {
        
        //  Holds a list of jobs that can be changed to reflect changes in this
        //  header.
        _jobs = new ArrayDeque<JobNode>();
        
        //  Create a popup menu that allows us to turn things on and off
        _popup = new JPopupMenu();
        JMenuItem menuItem;
        menuItem = new JMenuItem( _label.getText() + " Column Options:" );
        _popup.add( menuItem );
        _popup.add( new JSeparator() );
        JMenuItem allItem = new JMenuItem( "Show All" );
        allItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                activateAll();
            }
        } );
        _popup.add( allItem );
        _showNetworkActivity = new JCheckBoxMenuItem( "Network Activity" );
        _showNetworkActivity.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showNetworkActivity );
        _showName = new JCheckBoxMenuItem( "Job Name" );
        _showName.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showName );
        _showState = new JCheckBoxMenuItem( "State" );
        _showState.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showState );
        _showProgressBar = new JCheckBoxMenuItem( "Progress" );
        _showProgressBar.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showProgressBar );
        _showWeights = new JCheckBoxMenuItem( "Weights" );
        _showWeights.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showWeights );
        JMenu weightMenu = new JMenu( "Show Weights As..." );
        _popup.add( weightMenu );
        _showWeightsAsNumbers = new JCheckBoxMenuItem( "Numbers" );
        _showWeightsAsNumbers.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                setWeightsAsPlots( false );
                updateDisplayedData();
            }
        });
        weightMenu.add( _showWeightsAsNumbers );
        _showWeightsAsPlots = new JCheckBoxMenuItem( "Plots" );
        _showWeightsAsPlots.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                setWeightsAsPlots( true );
                updateDisplayedData();
            }
        });
        weightMenu.add( _showWeightsAsPlots );
        _showExperiment = new JCheckBoxMenuItem( "Experiment" );
        _showExperiment.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showExperiment );
        _showPass = new JCheckBoxMenuItem( "Pass" );
        _showPass.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showPass );
        _showPriority = new JCheckBoxMenuItem( "Priority" );
        _showPriority.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showPriority );
        _showQueueTime = new JCheckBoxMenuItem( "Queue Time" );
        _showQueueTime.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showQueueTime );
        _showCorrelationStart = new JCheckBoxMenuItem( "Correlation Start" );
        _showCorrelationStart.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showCorrelationStart );
        _showCorrelationEnd = new JCheckBoxMenuItem( "Correlation End" );
        _showCorrelationEnd.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showCorrelationEnd );
        _showCorrelationTime = new JCheckBoxMenuItem( "Correlation Time" );
        _showCorrelationTime.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showCorrelationTime );
        _showJobStart = new JCheckBoxMenuItem( "Job Start" );
        _showJobStart.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showJobStart );
        _showJobDuration = new JCheckBoxMenuItem( "Job Duration");
        _showJobDuration.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showJobDuration );
        _showInputFile = new JCheckBoxMenuItem( "Input File" );
        _showInputFile.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showInputFile );
        _showOutputFile = new JCheckBoxMenuItem( "Output File" );
        _showOutputFile.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showOutputFile );
        _showOutputSize = new JCheckBoxMenuItem( "Output Size" );
        _showOutputSize.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showOutputSize );
        _showDifxVersion = new JCheckBoxMenuItem( "DiFX Version" );
        _showDifxVersion.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showDifxVersion );
        _showSpeedUpFactor = new JCheckBoxMenuItem( "Speed Up Factor" );
        _showSpeedUpFactor.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        //_popup.add( _showSpeedUpFactor );
        _showTimeRemaining = new JCheckBoxMenuItem( "Time Remaining" );
        _showTimeRemaining.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        //_popup.add( _showTimeRemaining );
        _showNumAntennas = new JCheckBoxMenuItem( "# Antennas" );
        _showNumAntennas.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showNumAntennas );
        _showNumForeignAntennas = new JCheckBoxMenuItem( "# Foreign Antennas" );
        _showNumForeignAntennas.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showNumForeignAntennas );
        _showDutyCycle = new JCheckBoxMenuItem( "Duty Cycle" );
        _showDutyCycle.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showDutyCycle );
        _showStatus = new JCheckBoxMenuItem( "Status" );
        _showStatus.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showStatus );
        _showActive = new JCheckBoxMenuItem( "Active" );
        _showActive.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showActive );
        _showStatusId = new JCheckBoxMenuItem( "Status ID" );
        _showStatusId.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                updateDisplayedData();
            }
        });
        _popup.add( _showStatusId );
        _popupButton.setVisible( true );

        //  Create column headers
        _nameArea = new ColumnTextArea( "Job Name" );
        _nameArea.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showName.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _nameArea );
        _progressBar = new ColumnTextArea( "Progress" );
        _progressBar.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showProgressBar.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _progressBar );
        _state = new ColumnTextArea( "State" );
        _state.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showState.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _state );
        _experiment = new ColumnTextArea( "Experiment" );
        _experiment.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showExperiment.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _experiment );
        _pass = new ColumnTextArea( "Pass" );
        _pass.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showPass.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _pass );
        _priority = new ColumnTextArea( "Priority" );
        _priority.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showPriority.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _priority );
        _queueTime = new ColumnTextArea( "Queue Time" );
        _queueTime.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showQueueTime.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _queueTime );
        _correlationStart = new ColumnTextArea( "Correlation Start" );
        _correlationStart.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showCorrelationStart.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _correlationStart );
        _correlationEnd = new ColumnTextArea( "Correlation End" );
        _correlationEnd.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showCorrelationEnd.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _correlationEnd );
        _correlationTime = new ColumnTextArea( "Correlation Time" );
        _correlationTime.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showCorrelationTime.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _correlationTime );
        _jobStart = new ColumnTextArea( "Job Start" );
        _jobStart.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showJobStart.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _jobStart );
        _jobDuration = new ColumnTextArea( "Job Duration");
        _jobDuration.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showJobDuration.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _jobDuration );
        _inputFile = new ColumnTextArea( "Input File" );
        _inputFile.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showInputFile.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _inputFile );
        _outputFile = new ColumnTextArea( "Output File" );
        _outputFile.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showOutputFile.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _outputFile );
        _outputSize = new ColumnTextArea( "Output Size" );
        _outputSize.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showOutputSize.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _outputSize );
        _difxVersion = new ColumnTextArea( "DiFX Version" );
        _difxVersion.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showDifxVersion.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _difxVersion );
        _speedUpFactor = new ColumnTextArea( "Speed Up Factor" );
        _speedUpFactor.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showSpeedUpFactor.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _speedUpFactor );
        _timeRemaining = new ColumnTextArea( "Time Remaining" );
        _timeRemaining.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showTimeRemaining.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _timeRemaining );
        _numAntennas = new ColumnTextArea( "# Antennas" );
        _numAntennas.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showNumAntennas.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _numAntennas );
        _numForeignAntennas = new ColumnTextArea( "# Foreign Antennas" );
        _numForeignAntennas.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showNumForeignAntennas.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _numForeignAntennas );
        _dutyCycle = new ColumnTextArea( "Duty Cycle" );
        _dutyCycle.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showDutyCycle.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _dutyCycle );
        _status = new ColumnTextArea( "Status" );
        _status.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showStatus.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _status );
        _active = new ColumnTextArea( "Active" );
        _active.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showActive.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _active );
        _statusId = new ColumnTextArea( "Status ID" );
        _statusId.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showStatusId.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _statusId );
        _weights = new ColumnTextArea( "Weights" );
        _weights.addKillButton(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _showWeights.setState( false );
                updateDisplayedData();
            }
        });
        this.add( _weights );
    }
    
    public void setWeightsAsPlots( boolean newVal ) {
        _showWeightsAsPlots.setState( newVal );
        _showWeightsAsNumbers.setState( !newVal );
    }
    
    /*
     * This function makes everything in the popup menu visible.
     */
    public void activateAll() {
        _showNetworkActivity.setState( true );
        _showName.setState( true );
        _showProgressBar.setState( true );
        _showState.setState( true );
        _showExperiment.setState( true );
        _showPass.setState( true );
        _showPriority.setState( true );
        _showQueueTime.setState( true );
        _showCorrelationStart.setState( true );
        _showCorrelationEnd.setState( true );
        _showCorrelationTime.setState( true );
        _showJobStart.setState( true );
        _showJobDuration.setState( true );
        _showInputFile.setState( true );
        _showOutputFile.setState( true );
        _showOutputSize.setState( true );
        _showDifxVersion.setState( true );
        //_showSpeedUpFactor.setState( true );
        //_showTimeRemaining.setState( true );
        _showNumAntennas.setState( true );
        _showNumForeignAntennas.setState( true );
        _showDutyCycle.setState( true );
        _showStatus.setState( true );
        _showActive.setState( true );
        _showStatusId.setState( true );
        _showWeights.setState( true );
        updateDisplayedData();
    }

    @Override
    public void positionItems() {
        super.positionItems();
        _xOff = 90;
        if ( _showNetworkActivity.getState() )
            _xOff += 14;
        _popupButton.setBounds( _xOff - 20, 0, 18, _ySize - 2 );
        if ( _showName.getState() ) {
            setTextArea( _nameArea, _settings.jobColumnSpecs().name.width );
            _positionName = _xOff;
        }
        else
            _positionName = -100;
        if ( _showState.getState() ) {
            setTextArea( _state, _settings.jobColumnSpecs().state.width );
            _positionState = _xOff;
        }
        else
            _positionState = -100;
        if ( _showProgressBar.getState() ) {
            setTextArea( _progressBar, _settings.jobColumnSpecs().progressBar.width );
            _positionProgressBar = _xOff;
        }
        else
            _positionProgressBar = -100;
        if ( _showWeights.getState() ) {
            setTextArea( _weights, _settings.jobColumnSpecs().weights.width );
            _positionWeights = _xOff;
        }
        else
            _positionWeights = -100;
        if ( _showExperiment.getState() ) {
            setTextArea( _experiment, _settings.jobColumnSpecs().experiment.width );
            _positionExperiment = _xOff;
        }
        else
            _positionExperiment = -100;
        if ( _showPass.getState() ) {
            setTextArea( _pass, _settings.jobColumnSpecs().pass.width );
            _positionPass = _xOff;
        }
        else
            _positionPass = -100;
        if ( _showPriority.getState() ) {
            setTextArea( _priority, _settings.jobColumnSpecs().priority.width );
            _positionPriority = _xOff;
        }
        else
            _positionPriority = -100;
        if ( _showQueueTime.getState() ) {
            setTextArea( _queueTime, _settings.jobColumnSpecs().queueTime.width );
            _positionQueueTime = _xOff;
        }
        else
            _positionQueueTime = -100;
        if ( _showCorrelationStart.getState() ) {
            setTextArea( _correlationStart, _settings.jobColumnSpecs().correlationStart.width );
            _positionCorrelationStart = _xOff;
        }
        else
            _positionCorrelationStart = -100;
        if ( _showCorrelationEnd.getState() ) {
            setTextArea( _correlationEnd, _settings.jobColumnSpecs().correlationEnd.width );
            _positionCorrelationEnd = _xOff;
        }
        else
            _positionCorrelationEnd = -100;
        if ( _showCorrelationTime.getState() ) {
            setTextArea( _correlationTime, _settings.jobColumnSpecs().correlationTime.width );
            _positionCorrelationTime = _xOff;
        }
        else
            _positionCorrelationTime = -100;
        if ( _showJobStart.getState() ) {
            setTextArea( _jobStart, _settings.jobColumnSpecs().jobStart.width );
            _positionJobStart = _xOff;
        }
        else
            _positionJobStart = -100;
        if ( _showJobDuration.getState() ) {
            setTextArea( _jobDuration, _settings.jobColumnSpecs().jobDuration.width );
            _positionJobDuration = _xOff;
        }
        else
            _positionJobDuration = -100;
        if ( _showInputFile.getState() ) {
            setTextArea( _inputFile, _settings.jobColumnSpecs().inputFile.width );
            _positionInputFile = _xOff;
        }
        else
            _positionInputFile = -100;
        if ( _showOutputFile.getState() ) {
            setTextArea( _outputFile, _settings.jobColumnSpecs().outputFile.width );
            _positionOutputFile = _xOff;
        }
        else
            _positionOutputFile = -100;
        if ( _showOutputSize.getState() ) {
            setTextArea( _outputSize, _settings.jobColumnSpecs().outputSize.width );
            _positionOutputSize = _xOff;
        }
        else
            _positionOutputSize = -100;
        if ( _showDifxVersion.getState() ) {
            setTextArea( _difxVersion, _settings.jobColumnSpecs().difxVersion.width );
            _positionDifxVersion = _xOff;
        }
        else
            _positionDifxVersion = -100;
        if ( _showSpeedUpFactor.getState() ) {
            setTextArea( _speedUpFactor, _settings.jobColumnSpecs().speedUpFactor.width );
            _positionSpeedUpFactor = _xOff;
        }
        else
            _positionSpeedUpFactor = -100;
        if ( _showTimeRemaining.getState() ) {
            setTextArea( _timeRemaining, _settings.jobColumnSpecs().timeRemaining.width );
            _positionTimeRemaining = _xOff;
        }
        else
            _positionTimeRemaining = -100;
        if ( _showNumAntennas.getState() ) {
            setTextArea( _numAntennas, _settings.jobColumnSpecs().numAntennas.width );
            _positionNumAntennas = _xOff;
        }
        else
            _positionNumAntennas = -100;
        if ( _showNumForeignAntennas.getState() ) {
            setTextArea( _numForeignAntennas, _settings.jobColumnSpecs().numForeignAntennas.width );
            _positionNumForeignAntennas = _xOff;
        }
        else
            _positionNumForeignAntennas = -100;
        if ( _showDutyCycle.getState() ) {
            setTextArea( _dutyCycle, _settings.jobColumnSpecs().dutyCycle.width );
            _positionDutyCycle = _xOff;
        }
        else
            _positionDutyCycle = -100;
        if ( _showStatus.getState() ) {
            setTextArea( _status, _settings.jobColumnSpecs().status.width );
            _positionStatus = _xOff;
        }
        else
            _positionStatus = -100;
        if ( _showActive.getState() ) {
            setTextArea( _active, _settings.jobColumnSpecs().active.width );
            _positionActive = _xOff;
        }
        else
            _positionActive = -100;
        if ( _showStatusId.getState() ) {
            setTextArea( _statusId, _settings.jobColumnSpecs().statusId.width );
            _positionStatusId = _xOff;
        }
        else
            _positionStatusId = -100;
        
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
        _showNetworkActivity.setState( _settings.jobColumnSpecs().networkActivity.show );
        _showName.setState( _settings.jobColumnSpecs().name.show );
        _showProgressBar.setState( _settings.jobColumnSpecs().progressBar.show );
        _showState.setState( _settings.jobColumnSpecs().state.show );
        _showExperiment.setState( _settings.jobColumnSpecs().experiment.show );
        _showPass.setState( _settings.jobColumnSpecs().pass.show );
        _showPriority.setState( _settings.jobColumnSpecs().priority.show );
        _showQueueTime.setState( _settings.jobColumnSpecs().queueTime.show );
        _showCorrelationStart.setState( _settings.jobColumnSpecs().correlationStart.show );
        _showCorrelationEnd.setState( _settings.jobColumnSpecs().correlationEnd.show );
        _showCorrelationTime.setState( _settings.jobColumnSpecs().correlationTime.show );
        _showJobStart.setState( _settings.jobColumnSpecs().jobStart.show );
        _showJobDuration.setState( _settings.jobColumnSpecs().jobDuration.show );
        _showInputFile.setState( _settings.jobColumnSpecs().inputFile.show );
        _showOutputFile.setState( _settings.jobColumnSpecs().outputFile.show );
        _showOutputSize.setState( _settings.jobColumnSpecs().outputSize.show );
        _showDifxVersion.setState( _settings.jobColumnSpecs().difxVersion.show );
        _showSpeedUpFactor.setState( false/*_settings.jobColumnSpecs().speedUpFactor.show*/ );
        _showTimeRemaining.setState( false/*_settings.jobColumnSpecs().timeRemaining.show*/ );
        _showNumAntennas.setState( _settings.jobColumnSpecs().numAntennas.show );
        _showNumForeignAntennas.setState( _settings.jobColumnSpecs().numForeignAntennas.show );
        _showDutyCycle.setState( _settings.jobColumnSpecs().dutyCycle.show );
        _showStatus.setState( _settings.jobColumnSpecs().status.show );
        _showActive.setState( _settings.jobColumnSpecs().active.show );
        _showStatusId.setState( _settings.jobColumnSpecs().statusId.show );
        _showWeights.setState( _settings.jobColumnSpecs().weights.show );
        _showWeightsAsPlots.setState( _settings.jobColumnSpecs().weightsAsPlots.show );
        _showWeightsAsNumbers.setState( _settings.jobColumnSpecs().weightsAsNumbers.show );
    }
    
    /*
     * This functions propogates current column widths to all jobs.
     */
    public void setJobColumnWidths() {
        synchronized( _jobs ) {
            for ( Iterator<JobNode> iter = _jobs.iterator(); iter.hasNext(); ) {
                JobNode thisJob = iter.next();
                //  Change the settings on these items to match our current specifications.
                thisJob.widthName( _settings.jobColumnSpecs().name.width );
                thisJob.widthProgressBar( _settings.jobColumnSpecs().progressBar.width );
                thisJob.widthState( _settings.jobColumnSpecs().state.width );
                thisJob.widthExperiment( _settings.jobColumnSpecs().experiment.width );
                thisJob.widthPass( _settings.jobColumnSpecs().pass.width );
                thisJob.widthPriority( _settings.jobColumnSpecs().priority.width );
                thisJob.widthQueueTime( _settings.jobColumnSpecs().queueTime.width );
                thisJob.widthCorrelationStart( _settings.jobColumnSpecs().correlationStart.width );
                thisJob.widthCorrelationEnd( _settings.jobColumnSpecs().correlationEnd.width );
                thisJob.widthCorrelationTime( _settings.jobColumnSpecs().correlationTime.width );
                thisJob.widthJobStart( _settings.jobColumnSpecs().jobStart.width );
                thisJob.widthJobDuration( _settings.jobColumnSpecs().jobDuration.width );
                thisJob.widthInputFile( _settings.jobColumnSpecs().inputFile.width );
                thisJob.widthOutputFile( _settings.jobColumnSpecs().outputFile.width );
                thisJob.widthOutputSize( _settings.jobColumnSpecs().outputSize.width );
                thisJob.widthDifxVersion( _settings.jobColumnSpecs().difxVersion.width );
                thisJob.widthSpeedUpFactor( _settings.jobColumnSpecs().speedUpFactor.width );
                thisJob.widthTimeRemaining( _settings.jobColumnSpecs().timeRemaining.width );
                thisJob.widthNumAntennas( _settings.jobColumnSpecs().numAntennas.width );
                thisJob.widthNumForeignAntennas( _settings.jobColumnSpecs().numForeignAntennas.width );
                thisJob.widthDutyCycle( _settings.jobColumnSpecs().dutyCycle.width );
                thisJob.widthStatus( _settings.jobColumnSpecs().status.width );
                thisJob.widthActive( _settings.jobColumnSpecs().active.width );
                thisJob.widthStatusId( _settings.jobColumnSpecs().statusId.width );
                thisJob.widthWeights( _settings.jobColumnSpecs().weights.width );
                thisJob.updateUI();
            }
        }
    }
    
    public void addJob( JobNode newNode ) {
        synchronized( _jobs ) {
            _jobs.add( newNode );
        }
        setJobColumnWidths();
        updateDisplayedData();
    }
    
    /*
     * Check mouse move events to see if it is being positioned over one of the
     * joints between column headers.  This should change the cursor.  We also
     * record which item we are over.
     */
    @Override
    public void mouseMoved( MouseEvent e ) {
        this.setCursor( _normalCursor );
        _adjustName = false;
        _adjustProgressBar = false;
        _adjustState = false;
        _adjustExperiment = false;
        _adjustPass = false;
        _adjustPriority = false;
        _adjustQueueTime = false;
        _adjustCorrelationStart = false;
        _adjustCorrelationEnd = false;
        _adjustCorrelationTime = false;
        _adjustJobStart = false;
        _adjustJobDuration = false;
        _adjustInputFile = false;
        _adjustOutputFile = false;
        _adjustOutputSize = false;
        _adjustDifxVersion = false;
        _adjustSpeedUpFactor = false;
        _adjustTimeRemaining = false;
        _adjustNumAntennas = false;
        _adjustNumForeignAntennas = false;
        _adjustDutyCycle = false;
        _adjustStatus = false;
        _adjustActive = false;
        _adjustStatusId = false;
        _adjustWeights = false;
        if ( e.getX() > _positionName - 3 && e.getX() < _positionName + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustName = true;
        }
        else if ( e.getX() > _positionProgressBar - 3 && e.getX() < _positionProgressBar + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustProgressBar = true;
        }
        else if ( e.getX() > _positionState - 3 && e.getX() < _positionState + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustState = true;
        }
        else if ( e.getX() > _positionExperiment - 3 && e.getX() < _positionExperiment + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustExperiment = true;
        }
        else if ( e.getX() > _positionPass - 3 && e.getX() < _positionPass + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustPass = true;
        }
        else if ( e.getX() > _positionPriority - 3 && e.getX() < _positionPriority + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustPriority = true;
        }
        else if ( e.getX() > _positionQueueTime - 3 && e.getX() < _positionQueueTime + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustQueueTime = true;
        }
        else if ( e.getX() > _positionCorrelationStart - 3 && e.getX() < _positionCorrelationStart + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustCorrelationStart = true;
        }
        else if ( e.getX() > _positionCorrelationEnd - 3 && e.getX() < _positionCorrelationEnd + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustCorrelationEnd = true;
        }
        else if ( e.getX() > _positionCorrelationTime - 3 && e.getX() < _positionCorrelationTime + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustCorrelationTime = true;
        }
        else if ( e.getX() > _positionJobStart - 3 && e.getX() < _positionJobStart + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustJobStart = true;
        }
        else if ( e.getX() > _positionJobDuration - 3 && e.getX() < _positionJobDuration + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustJobDuration = true;
        }
        else if ( e.getX() > _positionInputFile - 3 && e.getX() < _positionInputFile + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustInputFile = true;
        }
        else if ( e.getX() > _positionOutputFile - 3 && e.getX() < _positionOutputFile + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustOutputFile = true;
        }
        else if ( e.getX() > _positionOutputSize - 3 && e.getX() < _positionOutputSize + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustOutputSize = true;
        }
        else if ( e.getX() > _positionDifxVersion - 3 && e.getX() < _positionDifxVersion + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustDifxVersion = true;
        }
        else if ( e.getX() > _positionSpeedUpFactor - 3 && e.getX() < _positionSpeedUpFactor + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustSpeedUpFactor = true;
        }
        else if ( e.getX() > _positionTimeRemaining - 3 && e.getX() < _positionTimeRemaining + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustTimeRemaining = true;
        }
        else if ( e.getX() > _positionNumAntennas - 3 && e.getX() < _positionNumAntennas + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustNumAntennas = true;
        }
        else if ( e.getX() > _positionNumForeignAntennas - 3 && e.getX() < _positionNumForeignAntennas + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustNumForeignAntennas = true;
        }
        else if ( e.getX() > _positionDutyCycle - 3 && e.getX() < _positionDutyCycle + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustDutyCycle = true;
        }
        else if ( e.getX() > _positionStatus - 3 && e.getX() < _positionStatus + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustStatus = true;
        }
        else if ( e.getX() > _positionActive - 3 && e.getX() < _positionActive + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustActive = true;
        }
        else if ( e.getX() > _positionStatusId - 3 && e.getX() < _positionStatusId + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustStatusId = true;
        }
        else if ( e.getX() > _positionWeights - 3 && e.getX() < _positionWeights + 2 ) {
            setCursor( _columnAdjustCursor );
            _adjustWeights = true;
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
        if ( _adjustName ) {
            _startWidth = _settings.jobColumnSpecs().name.width;
            _startX = e.getX();
        }
        else if ( _adjustProgressBar ) {
            _startWidth = _settings.jobColumnSpecs().progressBar.width;
            _startX = e.getX();
        }
        else if ( _adjustState ) {
            _startWidth = _settings.jobColumnSpecs().state.width;
            _startX = e.getX();
        }
        else if ( _adjustExperiment ) {
            _startWidth = _settings.jobColumnSpecs().experiment.width;
            _startX = e.getX();
        }
        else if ( _adjustPass ) {
            _startWidth = _settings.jobColumnSpecs().pass.width;
            _startX = e.getX();
        }
        else if ( _adjustPriority ) {
            _startWidth = _settings.jobColumnSpecs().priority.width;
            _startX = e.getX();
        }
        else if ( _adjustQueueTime ) {
            _startWidth = _settings.jobColumnSpecs().queueTime.width;
            _startX = e.getX();
        }
        else if ( _adjustCorrelationStart ) {
            _startWidth = _settings.jobColumnSpecs().correlationStart.width;
            _startX = e.getX();
        }
        else if ( _adjustCorrelationEnd ) {
            _startWidth = _settings.jobColumnSpecs().correlationEnd.width;
            _startX = e.getX();
        }
        else if ( _adjustCorrelationTime ) {
            _startWidth = _settings.jobColumnSpecs().correlationTime.width;
            _startX = e.getX();
        }
        else if ( _adjustJobStart ) {
            _startWidth = _settings.jobColumnSpecs().jobStart.width;
            _startX = e.getX();
        }
        else if ( _adjustJobDuration ) {
            _startWidth = _settings.jobColumnSpecs().jobDuration.width;
            _startX = e.getX();
        }
        else if ( _adjustInputFile ) {
            _startWidth = _settings.jobColumnSpecs().inputFile.width;
            _startX = e.getX();
        }
        else if ( _adjustOutputFile ) {
            _startWidth = _settings.jobColumnSpecs().outputFile.width;
            _startX = e.getX();
        }
        else if ( _adjustOutputSize ) {
            _startWidth = _settings.jobColumnSpecs().outputSize.width;
            _startX = e.getX();
        }
        else if ( _adjustDifxVersion ) {
            _startWidth = _settings.jobColumnSpecs().difxVersion.width;
            _startX = e.getX();
        }
        else if ( _adjustSpeedUpFactor ) {
            _startWidth = _settings.jobColumnSpecs().speedUpFactor.width;
            _startX = e.getX();
        }
        else if ( _adjustTimeRemaining ) {
            _startWidth = _settings.jobColumnSpecs().timeRemaining.width;
            _startX = e.getX();
        }
        else if ( _adjustNumAntennas ) {
            _startWidth = _settings.jobColumnSpecs().numAntennas.width;
            _startX = e.getX();
        }
        else if ( _adjustNumForeignAntennas ) {
            _startWidth = _settings.jobColumnSpecs().numForeignAntennas.width;
            _startX = e.getX();
        }
        else if ( _adjustDutyCycle ) {
            _startWidth = _settings.jobColumnSpecs().dutyCycle.width;
            _startX = e.getX();
        }
        else if ( _adjustStatus ) {
            _startWidth = _settings.jobColumnSpecs().status.width;
            _startX = e.getX();
        }
        else if ( _adjustActive ) {
            _startWidth = _settings.jobColumnSpecs().active.width;
            _startX = e.getX();
        }
        else if ( _adjustStatusId ) {
            _startWidth = _settings.jobColumnSpecs().statusId.width;
            _startX = e.getX();
        }
        else if ( _adjustWeights ) {
            _startWidth = _settings.jobColumnSpecs().weights.width;
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
        if ( _adjustName ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().name.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustProgressBar ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().progressBar.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustState ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().state.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustExperiment ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().experiment.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustPass ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().pass.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustPriority ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().priority.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustQueueTime ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().queueTime.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustCorrelationStart ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().correlationStart.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustCorrelationEnd ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().correlationEnd.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustCorrelationTime ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().correlationTime.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustJobStart ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().jobStart.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustJobDuration ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().jobDuration.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustInputFile ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().inputFile.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustOutputFile ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().outputFile.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustOutputSize ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().outputSize.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustDifxVersion ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().difxVersion.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustSpeedUpFactor ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().speedUpFactor.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustTimeRemaining ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().timeRemaining.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustNumAntennas ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().numAntennas.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustNumForeignAntennas ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().numForeignAntennas.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustDutyCycle ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().dutyCycle.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustStatus ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().status.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustActive ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().active.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustStatusId ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().statusId.width = _startWidth + e.getX() - _startX;
        }
        else if ( _adjustWeights ) {
            if ( e.getX() - _startX + _startWidth > 5 )
                _settings.jobColumnSpecs().weights.width = _startWidth + e.getX() - _startX;
        }
        setJobColumnWidths();
    }
    
    public void updateDisplayedData() {
        //  Run through the list of all "child" nodes, which are all of the listed
        //  cluster nodes.
        synchronized( _jobs ) {
            for ( Iterator<JobNode> iter = _jobs.iterator(); iter.hasNext(); ) {
                JobNode thisJob = (JobNode)(iter.next());
                //  Change the settings on these items to match our current specifications.
                thisJob.showNetworkActivity( _showNetworkActivity.getState() );
                thisJob.showName( _showName.getState() );
                thisJob.showProgressBar( _showProgressBar.getState() );
                thisJob.showState( _showState.getState() );
                thisJob.showExperiment( _showExperiment.getState() );
                thisJob.showPass( _showPass.getState() );
                thisJob.showPriority( _showPriority.getState() );
                thisJob.showQueueTime( _showQueueTime.getState() );
                thisJob.showCorrelationStart( _showCorrelationStart.getState() );
                thisJob.showCorrelationEnd( _showCorrelationEnd.getState() );
                thisJob.showCorrelationTime( _showCorrelationTime.getState() );
                thisJob.showJobStart( _showJobStart.getState() );
                thisJob.showJobDuration( _showJobDuration.getState() );
                thisJob.showInputFile( _showInputFile.getState() );
                thisJob.showOutputFile( _showOutputFile.getState() );
                thisJob.showOutputSize( _showOutputSize.getState() );
                thisJob.showDifxVersion( _showDifxVersion.getState() );
                thisJob.showSpeedUpFactor( _showSpeedUpFactor.getState() );
                thisJob.showTimeRemaining( _showTimeRemaining.getState() );
                thisJob.showNumAntennas( _showNumAntennas.getState() );
                thisJob.showNumForeignAntennas( _showNumForeignAntennas.getState() );
                thisJob.showDutyCycle( _showDutyCycle.getState() );
                thisJob.showStatus( _showStatus.getState() );
                thisJob.showActive( _showActive.getState() );
                thisJob.showStatusId( _showStatusId.getState() );
                thisJob.showWeights( _showWeights.getState() );
                thisJob.showWeightsAsPlots( _showWeightsAsPlots.getState() );
                thisJob.updateUI();
            }
        }
        //  Update the headers as well.
        _nameArea.setVisible( _showName.getState() );
        _progressBar.setVisible( _showProgressBar.getState() );
        _state.setVisible( _showState.getState() );
        _experiment.setVisible( _showExperiment.getState() );
        _pass.setVisible( _showPass.getState() );
        _priority.setVisible( _showPriority.getState() );
        _queueTime.setVisible( _showQueueTime.getState() );
        _correlationStart.setVisible( _showCorrelationStart.getState() );
        _correlationEnd.setVisible( _showCorrelationEnd.getState() );
        _correlationTime.setVisible( _showCorrelationTime.getState() );
        _jobStart.setVisible( _showJobStart.getState() );
        _jobDuration.setVisible( _showJobDuration.getState() );
        _inputFile.setVisible( _showInputFile.getState() );
        _outputFile.setVisible( _showOutputFile.getState() );
        _outputSize.setVisible( _showOutputSize.getState() );
        _difxVersion.setVisible( _showDifxVersion.getState() );
        _speedUpFactor.setVisible( false/*_showSpeedUpFactor.getState()*/ );
        _timeRemaining.setVisible( false/*_showTimeRemaining.getState()*/ );
        _numAntennas.setVisible( _showNumAntennas.getState() );
        _numForeignAntennas.setVisible( _showNumForeignAntennas.getState() );
        _dutyCycle.setVisible( _showDutyCycle.getState() );
        _status.setVisible( _showStatus.getState() );
        _active.setVisible( _showActive.getState() );
        _statusId.setVisible( _showStatusId.getState() );
        _weights.setVisible( _showWeights.getState() );
        //  And the system settings.
        _settings.jobColumnSpecs().name.show = _showName.getState();
        _settings.jobColumnSpecs().progressBar.show = _showProgressBar.getState();
        _settings.jobColumnSpecs().state.show = _showState.getState();
        _settings.jobColumnSpecs().experiment.show = _showExperiment.getState();
        _settings.jobColumnSpecs().pass.show = _showPass.getState();
        _settings.jobColumnSpecs().priority.show = _showPriority.getState();
        _settings.jobColumnSpecs().queueTime.show = _showQueueTime.getState();
        _settings.jobColumnSpecs().correlationStart.show = _showCorrelationStart.getState();
        _settings.jobColumnSpecs().correlationEnd.show = _showCorrelationEnd.getState();
        _settings.jobColumnSpecs().correlationTime.show = _showCorrelationTime.getState();
        _settings.jobColumnSpecs().jobStart.show = _showJobStart.getState();
        _settings.jobColumnSpecs().jobDuration.show = _showJobDuration.getState();
        _settings.jobColumnSpecs().inputFile.show = _showInputFile.getState();
        _settings.jobColumnSpecs().outputFile.show = _showOutputFile.getState();
        _settings.jobColumnSpecs().outputSize.show = _showOutputSize.getState();
        _settings.jobColumnSpecs().difxVersion.show = _showDifxVersion.getState();
        _settings.jobColumnSpecs().speedUpFactor.show = false;//_showSpeedUpFactor.getState();
        _settings.jobColumnSpecs().timeRemaining.show = false;//_showTimeRemaining.getState();
        _settings.jobColumnSpecs().numAntennas.show = _showNumAntennas.getState();
        _settings.jobColumnSpecs().numForeignAntennas.show = _showNumForeignAntennas.getState();
        _settings.jobColumnSpecs().dutyCycle.show = _showDutyCycle.getState();
        _settings.jobColumnSpecs().status.show = _showStatus.getState();
        _settings.jobColumnSpecs().active.show = _showActive.getState();
        _settings.jobColumnSpecs().statusId.show = _showStatusId.getState();
        _settings.jobColumnSpecs().weights.show = _showWeights.getState();
        this.updateUI();
        
    }
    
    protected JCheckBoxMenuItem _showNetworkActivity;
    
    protected ColumnTextArea _nameArea;
    protected JCheckBoxMenuItem _showName;
    protected int _positionName;
    protected boolean _adjustName;
    
    protected ColumnTextArea _progressBar;
    protected JCheckBoxMenuItem _showProgressBar;
    protected int _positionProgressBar;
    protected boolean _adjustProgressBar;
    
    protected ColumnTextArea _state;
    protected JCheckBoxMenuItem _showState;
    protected int _positionState;
    protected boolean _adjustState;
    
    protected ColumnTextArea _experiment;
    protected JCheckBoxMenuItem _showExperiment;
    protected int _positionExperiment;
    protected boolean _adjustExperiment;
    
    protected ColumnTextArea _pass;
    protected JCheckBoxMenuItem _showPass;
    protected int _positionPass;
    protected boolean _adjustPass;
    
    protected ColumnTextArea _priority;
    protected JCheckBoxMenuItem _showPriority;
    protected int _positionPriority;
    protected boolean _adjustPriority;
    
    protected ColumnTextArea _queueTime;
    protected JCheckBoxMenuItem _showQueueTime;
    protected int _positionQueueTime;
    protected boolean _adjustQueueTime;
    
    protected ColumnTextArea _correlationStart;
    protected JCheckBoxMenuItem _showCorrelationStart;
    protected int _positionCorrelationStart;
    protected boolean _adjustCorrelationStart;
    
    protected ColumnTextArea _correlationEnd;
    protected JCheckBoxMenuItem _showCorrelationEnd;
    protected int _positionCorrelationEnd;
    protected boolean _adjustCorrelationEnd;
    
    protected ColumnTextArea _correlationTime;
    protected JCheckBoxMenuItem _showCorrelationTime;
    protected int _positionCorrelationTime;
    protected boolean _adjustCorrelationTime;
    public int positionCorrelationTime() { return _positionCorrelationTime; }
    
    protected ColumnTextArea _jobStart;
    protected JCheckBoxMenuItem _showJobStart;
    protected int _positionJobStart;
    protected boolean _adjustJobStart;
    
    protected ColumnTextArea _jobDuration;
    protected JCheckBoxMenuItem _showJobDuration;
    protected int _positionJobDuration;
    protected boolean _adjustJobDuration;
    
    protected ColumnTextArea _inputFile;
    protected JCheckBoxMenuItem _showInputFile;
    protected int _positionInputFile;
    protected boolean _adjustInputFile;
    
    protected ColumnTextArea _outputFile;
    protected JCheckBoxMenuItem _showOutputFile;
    protected int _positionOutputFile;
    protected boolean _adjustOutputFile;
    
    protected ColumnTextArea _outputSize;
    protected JCheckBoxMenuItem _showOutputSize;
    protected int _positionOutputSize;
    protected boolean _adjustOutputSize;
    
    protected ColumnTextArea _difxVersion;
    protected JCheckBoxMenuItem _showDifxVersion;
    protected int _positionDifxVersion;
    protected boolean _adjustDifxVersion;
    
    protected ColumnTextArea _speedUpFactor;
    protected JCheckBoxMenuItem _showSpeedUpFactor;
    protected int _positionSpeedUpFactor;
    protected boolean _adjustSpeedUpFactor;
    
    protected ColumnTextArea _timeRemaining;
    protected JCheckBoxMenuItem _showTimeRemaining;
    protected int _positionTimeRemaining;
    protected boolean _adjustTimeRemaining;
    public int positionTimeRemaining() { return _positionTimeRemaining; }
    
    protected ColumnTextArea _numAntennas;
    protected JCheckBoxMenuItem _showNumAntennas;
    protected int _positionNumAntennas;
    protected boolean _adjustNumAntennas;
    
    protected ColumnTextArea _numForeignAntennas;
    protected JCheckBoxMenuItem _showNumForeignAntennas;
    protected int _positionNumForeignAntennas;
    protected boolean _adjustNumForeignAntennas;
    
    protected ColumnTextArea _dutyCycle;
    protected JCheckBoxMenuItem _showDutyCycle;
    protected int _positionDutyCycle;
    protected boolean _adjustDutyCycle;
    
    protected ColumnTextArea _status;
    protected JCheckBoxMenuItem _showStatus;
    protected int _positionStatus;
    protected boolean _adjustStatus;
    
    protected ColumnTextArea _active;
    protected JCheckBoxMenuItem _showActive;
    protected int _positionActive;
    protected boolean _adjustActive;
    
    protected ColumnTextArea _statusId;
    protected JCheckBoxMenuItem _showStatusId;
    protected int _positionStatusId;
    protected boolean _adjustStatusId;
    
    protected ColumnTextArea _weights;
    protected JCheckBoxMenuItem _showWeights;
    protected int _positionWeights;
    protected boolean _adjustWeights;
    JCheckBoxMenuItem _showWeightsAsNumbers;
    JCheckBoxMenuItem _showWeightsAsPlots;
    
    SystemSettings _settings;
    
    protected int _xOff;
    
    protected Cursor _columnAdjustCursor;
    protected Cursor _normalCursor;
    
    ArrayDeque<JobNode> _jobs;
    
    protected int _startWidth;
    protected int _startX;
    
    
}
