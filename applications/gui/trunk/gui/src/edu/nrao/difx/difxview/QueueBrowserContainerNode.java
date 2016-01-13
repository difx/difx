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
import static edu.nrao.difx.difxview.JobNode.fromSeconds;
import javax.swing.JProgressBar;
import javax.swing.JLabel;
import java.awt.Color;
import java.awt.Font;
import mil.navy.usno.widgetlib.BrowserNode;

public class QueueBrowserContainerNode extends QueueBrowserNode {
    
    public QueueBrowserContainerNode( String name, SystemSettings settings ) {
        super( name );
        _settings = settings;
        _jobsLabel = new JLabel( "" );
        _jobsLabel.setVisible( false );
        _jobsLabel.setHorizontalAlignment( JLabel.CENTER );
        this.add( _jobsLabel );
        _progress = new JProgressBar();
        _progress.setMinimum( 0 );
        _progress.setBackground( Color.YELLOW );
        _progress.setForeground( Color.GREEN );
        _progress.setBorderPainted( false );
        _progress.setStringPainted( true );
        _progress.setVisible( false );
        this.add( _progress );
        _correlationTime = new ColumnTextArea();
        _correlationTime.setVisible( false );
        _correlationTime.setBackground( Color.WHITE );
        _correlationTime.setToolTipText( "Time the scheduler has been continually running jobs." );
        _correlationTime.justify( ColumnTextArea.RIGHT );
        this.add( _correlationTime );
        _timeRemaining = new ColumnTextArea();
        _timeRemaining.setVisible( false );
        _timeRemaining.setBackground( Color.WHITE );
        _timeRemaining.setToolTipText( "An estimate of the time required to complete all scheduled jobs." );
        _timeRemaining.justify( ColumnTextArea.RIGHT );
        this.add( _timeRemaining );
        _failedLabel = new ColumnTextArea();
        _failedLabel.setBackground( Color.RED );
        _failedLabel.setVisible( false );
        _failedLabel.justify( ColumnTextArea.CENTER );
        this.add( _failedLabel );
        _statsVisible = true;
        _setupComplete = true;
        resizeOnTopBar( true );
    }
    
    @Override
    public void positionItems() {
        super.positionItems();
        int xOff = _level * _levelOffset + _xOffset + _labelWidth;
        if ( _setupComplete ) {
            _jobsLabel.setBounds( xOff, 0 , _settings.jobColumnSpecs().state.width, 25 );
            xOff += _settings.jobColumnSpecs().state.width;
            _progress.setBounds( xOff, 0, _settings.jobColumnSpecs().progressBar.width, _ySize );
            xOff += _settings.jobColumnSpecs().progressBar.width;
            if ( _settings.jobColumnSpecs().correlationTime.show ) {
                if ( _settings.queueBrowser().header() != null ) {
                    _correlationTime.setBounds( _settings.queueBrowser().header().positionCorrelationTime() - _settings.jobColumnSpecs().correlationTime.width,
                            0, _settings.jobColumnSpecs().correlationTime.width, _ySize );
                    xOff = _settings.queueBrowser().header().positionCorrelationTime();
                    _correlationTime.setVisible( true );
                }
            }
            else
                _correlationTime.setVisible( false );
            if ( _settings.jobColumnSpecs().timeRemaining.show ) {
                if ( _settings.queueBrowser().header() != null ) {
                    _timeRemaining.setBounds( _settings.queueBrowser().header().positionTimeRemaining() - _settings.jobColumnSpecs().timeRemaining.width,
                            0, _settings.jobColumnSpecs().timeRemaining.width, _ySize );
                    xOff = _settings.queueBrowser().header().positionTimeRemaining();
                    _timeRemaining.setVisible( true );
                }
            }
            else
                _timeRemaining.setVisible( false );
            _failedLabel.setBounds( xOff, 0, 200, _ySize );
        }
    }
    
    public void clearCounters() {
        _numJobs = 0;
        _numScheduled = 0;
        _numCompleted = 0;
        _numFailed = 0;
    }
    
    public void addJobs( int n ) {
        _numJobs += n;
    }

    public void addScheduled( int n ) {
        _numScheduled += n;
    }

    public void addCompleted( int n ) {
        _numCompleted += n;
    }

    public void addFailed( int n ) {
        _numFailed += n;
    }
    
    public void setProgress() {
        if ( _progress != null ) {
            _progress.setMaximum( _numScheduled + _numCompleted + _numFailed );
            _progress.setValue( _numCompleted + _numFailed );
            if ( _numScheduled == 0 )
                _progress.setBackground( Color.GREEN );
            else
                _progress.setBackground( Color.YELLOW );
            _progress.setString( _numCompleted + " completed/" + _numScheduled + " scheduled" );
            
            this.updateUI();
        }
    }
    
    public void correlationTime( double newVal ) { 
        _correlationTime.setText( fromSeconds( newVal, 1 ) );
        _correlationTime.updateUI();
    }
    public String correlationTime() { return _correlationTime.getText(); }

    public void timeRemaining( double newVal ) { 
        _timeRemaining.setText( fromSeconds( newVal, 0 ) );
        _timeRemaining.updateUI();
    }
    public String timeRemaining() { return _timeRemaining.getText(); }
    public boolean correlating;
    public double correlationStart;

    public void checkVisibility() {
        if ( !_showNothing && _statsVisible && !_overrideVisible && _numJobs > 0 )
            _jobsLabel.setVisible( true );
        else
            _jobsLabel.setVisible( false );
        if ( !_showNothing && _statsVisible && !_overrideVisible && _numScheduled + _numCompleted + _numFailed > 0 )
            _progress.setVisible( true );
        else
            _progress.setVisible( false );
        if ( !_showNothing && _statsVisible && !_overrideVisible && _numFailed > 0 )
            _failedLabel.setVisible( true );
        else
            _failedLabel.setVisible( false );
        updateUI();
    }
    
    public void statsVisible( boolean newVal ) {
        _statsVisible = newVal;
        checkVisibility();
    }
    
    public int numJobs() { return _numJobs; }
    public int numScheduled() { return _numScheduled; }
    public int numCompleted() { return _numCompleted; }
    public int numFailed() { return _numFailed; }
    
    public void displayNow() {
        if ( _setupComplete ) {
            _jobsLabel.setText( _numJobs + " jobs" );
            _failedLabel.setText( _numFailed + " failed" );
        }
        setProgress();
        checkVisibility();        
    }
    
    public void overrideVisible( boolean newVal ) {
        _overrideVisible = newVal;
    }
    
    //  Yet another way to make things completely invisible...
    public void showNothing( boolean newVal ) {
        _showNothing = newVal;
    }

    protected int _numJobs;
    protected int _numScheduled;
    protected int _numCompleted;
    protected int _numFailed;
    
    protected boolean _setupComplete;
    
    protected boolean _showNothing;
    
    protected SystemSettings _settings;
    
    protected JProgressBar _progress;
    protected JLabel _jobsLabel;
    protected ColumnTextArea _failedLabel;
    protected ColumnTextArea _correlationTime;
    protected ColumnTextArea _timeRemaining;
    
    protected boolean _statsVisible;
    protected boolean _overrideVisible;
    
}
