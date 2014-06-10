/*
 * This class provides a "monitor" popup - a modal dialog window that can be used
 * to monitor some process (a file transfer, or whatever).  The actual process takes
 * place during the (overridden) "run()" function.  The "success( true )" function should
 * be called when the process (whatever it is) completes successfully - at which
 * point the popup window will disappear.  The "errorCondition()" function should
 * be called if the process fails for some reason - the window will remain to
 * display an error message.  When running, there is a "cancel" button that turns
 * into a "dismiss" button when displaying errors.
 * 
 * The window is created with a "delay" value (in milliseconds) - the popup will not 
 * appear until after this time, and only if the process has not completed.  This
 * is used to keep the popup from flashing into and out of existence for fast
 * processes.
 * 
 * The window displays a little green spinning thing to show that it is working,
 * a primary status label (set using "statusLabel()"), an optional progress bar,
 * and an optional second status label (can't be displayed with the progress bar - they
 * occupy the same location).
 */
package mil.navy.usno.widgetlib;

import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JProgressBar;
import javax.swing.JPanel;

import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.lang.Thread;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Dimension;
import java.awt.Color;

import java.awt.Frame;

public class PopupMonitor extends JDialog implements WindowListener  {
    
    public PopupMonitor( Frame frame, int x, int y, int w, int h, int delay ) {
        super( frame, "", false );
        _theWindow = this;
        _delay = delay;
        _displayLock = new Object();
        this.setDefaultCloseOperation( JDialog.DO_NOTHING_ON_CLOSE );
        this.setLayout( null );
        this.setBounds( x, y, w, h );
        this.setResizable( false );
        _statusLabel = new JLabel( "" );
        _statusLabel.setBounds( 80, 20, w - 90, 25 );
        this.add( _statusLabel );
        _statusLabel2 = new JLabel( "" );
        _statusLabel2.setBounds( 80, 40, w - 90, 25 );
        _statusLabel2.setVisible( false );
        this.add( _statusLabel2 );
        _progress = new JProgressBar();
        _progress.setBounds( 80, 50, w - 200, 25 );
        _progress.setVisible( false );
        this.add( _progress );
        _cancelButton = new JButton( "Cancel" );
        _cancelButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                cancelOperation();
            }
        });
        _cancelButton.setBounds( w - 320, 80, 120, 25 );
        this.add( _cancelButton );
        addWindowListener( this );
        _this = this;
    }
    
    protected PopupMonitor _this;
    
    /*
     * Start things up.
     */
    public void start() {
        //  This is where we actually run the process.
        run();
        //  Wait for a delay in milliseconds before we actually display a pop-up
        //  window.  This will give the process a chance to finish quietly if it is
        //  very quick.
        int totalDelay = 0;
        while ( totalDelay < _delay && _success == false ) {
            try { Thread.sleep( 10 ); } catch ( Exception e ) {}
            totalDelay += 10;
        }
//        synchronized ( _displayLock ) {
            if ( _success == false && _cleanClose == false && !_noSpinnerStart ) {
                //  The spinner involves a thread, so we add it here (instead of when
                //  creating the popup).
                _spinner = new Spinner();
                _spinner.setBounds( 20, 20, 40, 40 );
                _this.add( _spinner );
                _this.setVisible( true );
            }
//        }
    }
    
    /*
     * Run process - to be overridden.
     */
    public void run() {
    }
    
    /*
     * Show the progress bar.
     */
    public void showProgress( boolean newVal ) {
        _progress.setVisible( newVal );
    }
    
    /*
     * Show the second status line.
     */
    public void showStatus2( boolean newVal ) {
        _statusLabel2.setVisible( newVal );
    }
    
    /*
     * Set the content of the status label.
     */
    public void status( String newVal ) {
        _statusLabel.setText( newVal );
    }
    
    /*
     * Set the content of the second status label.
     */
    public void status2( String newVal ) {
        _statusLabel2.setText( newVal );
    }
    
    /*
     * Called when things succeed.  This makes the window go away.
     */
    protected void successCondition() {
//        synchronized ( _displayLock ) {
            _cleanClose = true;
            _noSpinnerStart = true;
            _theWindow.setTitle( "DISMISSED - SUCCESS" );
            if ( _spinner != null ) _spinner.stop();
            _success = true;
            _theWindow.setVisible( false );
//        }
    }
    
    /*
     * Called when things fail.
     */
    protected void errorCondition() {
//        synchronized ( _displayLock ) {
            _noSpinnerStart = true;
            _cancelButton.setText( "Dismiss" );
            if ( _spinner != null ) _spinner.error();
            if ( _spinner != null ) _spinner.stop();
            _dismissActive = true;
//        }
    }
    
    /*
     * A "silent" failure.  This gets rid of any pop-up windows.
     */
    protected void silentError() {
//        synchronized ( _displayLock ) {
            _noSpinnerStart = true;
            showProgress( false );
            if ( _spinner != null ) _spinner.stop();
            _cleanClose = true;
            _theWindow.setVisible( false );
//        }
    }
    
    /*
     * Set the progress.
     */
    protected void progress( int value, int maximum ) {
        _progress.setMaximum( maximum );
        _progress.setValue( value );
    }
    
    /*
     * Combine a few things - set the first and second status lines (the second one
     * is optional), remove the progress bar, and set the error condition.
     */
    protected void error( String stat1, String stat2 ) {
        showProgress( false );
        if ( stat2 != null )
            showStatus2( true );
        status( stat1 );
        status2( stat2 );
        errorCondition();
    }
    
    /*
     * This tells us whether the "dismiss" state has been set - indicating an
     * error has occurred.
     */
    protected boolean dismissActive() {
        return _dismissActive;
    }
    
    /*
     * Called when the user hits the cancel button.  process
     * and sets the "success" flag to false.  Note that the file transfer might
     * possibly complete before success is made false, but we are assuming the user
     * cancelled deliberately.
     */
    protected void cancelOperation() {
//        synchronized ( _displayLock ) {
            _noSpinnerStart = true;
            _cleanClose = true;
            _theWindow.setTitle( "DISMISSED - CANCEL" );
            if ( _spinner != null ) _spinner.stop();
            _success = false;
            _theWindow.setVisible( false );
//        }
    }
    
    /*
     * Return whether the operation was completed successfully.
     */
    public boolean success() { return _success; }
    
    /*
     * Window event methods - we need each of these, even though we are only
     * interested in the "Closing" method.
     */
    @Override
    public void windowOpened(WindowEvent e) {
    }

    @Override
    public void windowClosed(WindowEvent e) {
    }

    @Override
    public void windowClosing(WindowEvent e) {
        if ( _cleanClose ) {}
        else if ( _dismissActive )
            cancelOperation();
        else {
            int ans = JOptionPane.showConfirmDialog( this,
                    "Do you really wish to cancel the current download?",
                    "Cancel Operation?",
                    JOptionPane.YES_NO_OPTION );
            if ( ans == JOptionPane.YES_OPTION )
                cancelOperation();
        }
    }

    @Override
    public void windowActivated(WindowEvent e) {
    }

    @Override
    public void windowDeactivated(WindowEvent e) {
    }

    @Override
    public void windowIconified(WindowEvent e) {
    }

    @Override
    public void windowDeiconified(WindowEvent e) {
    }

    protected boolean _success;
    protected JLabel _statusLabel;
    protected JLabel _statusLabel2;
    protected JButton _cancelButton;
    protected JProgressBar _progress;
    protected boolean _dismissActive;
    protected boolean _cleanClose;
    protected Spinner _spinner;
    protected PopupMonitor _theWindow;
    protected int _delay;
    protected boolean _noSpinnerStart;
    protected Object _displayLock;
    
}
