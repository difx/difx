package edu.nrao.difx.difxview;
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
        _failedLabel = new ColumnTextArea();
        _failedLabel.setBackground( Color.RED );
        _failedLabel.setVisible( false );
        _failedLabel.justify( ColumnTextArea.CENTER );
        this.add( _failedLabel );
        _statsVisible = true;
        _setupComplete = true;
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
            _failedLabel.setBounds( xOff, 0, _settings.jobColumnSpecs().correlationTime.width, _ySize );
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
        if ( _setupComplete ) {
            _jobsLabel.setText( _numJobs + " jobs" );
        }
        checkVisibility();
    }

    public void addScheduled( int n ) {
        _numScheduled += n;
        setProgress();
        checkVisibility();
    }

    public void addCompleted( int n ) {
        _numCompleted += n;
        setProgress();
        checkVisibility();
    }

    public void addFailed( int n ) {
        _numFailed += n;
        setProgress();
        if ( _setupComplete ) {
            _failedLabel.setText( _numFailed + " failed" );
        }
        checkVisibility();
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
    
    public void checkVisibility() {
        if ( _statsVisible && _numJobs > 0 )
            _jobsLabel.setVisible( true );
        else
            _jobsLabel.setVisible( false );
        if ( _statsVisible && _numScheduled + _numCompleted + _numFailed > 0 )
            _progress.setVisible( true );
        else
            _progress.setVisible( false );
        if ( _statsVisible && _numFailed > 0 )
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

    protected int _numJobs;
    protected int _numScheduled;
    protected int _numCompleted;
    protected int _numFailed;
    
    protected boolean _setupComplete;
    
    protected SystemSettings _settings;
    
    protected JProgressBar _progress;
    protected JLabel _jobsLabel;
    protected ColumnTextArea _failedLabel;
    
    protected boolean _statsVisible;
    
}
