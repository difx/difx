/*
 * This class is used to obtain a file from the DiFX Host using the DiFXCommand_getFile
 * class.  It produces a modal JDialog that displays activity, progress, and any
 * errors encountered.  This window will appear after a short delay (a couple of
 * tenths of a second) so that if the activity is completed rapidly the user won't
 * be annoyed by a window blinking in and out of existence.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.PopupMonitor;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import edu.nrao.difx.difxutilities.DiFXCommand_getFile;

import java.awt.Frame;

/**
 *
 * @author jspitzak
 */
public class GetFileMonitor extends PopupMonitor {
    
    public GetFileMonitor( Frame frame, int x, int y, String filePath, SystemSettings settings ) {
        super( frame, x, y, 600, 145, 200 );
        _settings = settings;
        _filePath = filePath;
        //  Open a "get file" operation and set the various callbacks.
        _fileGet = new DiFXCommand_getFile( _filePath, _settings );
        if ( _fileGet.error() != null ) {
            _error = _fileGet.error();
            return;
        }
        _fileGet.addEndListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( !dismissActive() ) {
                    status( "Transfer completed" );
                    fileGetEnd();
                }
            }
        });
        _fileGet.addAcceptListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( !dismissActive() ) {
                    status( "Connection established" );
                    showProgress( true );
                }
            }
        });
        _fileGet.addIncrementalListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( !dismissActive() ) {
                    status( "File transfer in progress (" + 
                            _fileGet.inString().length() + "/" +
                            _fileGet.fileSize() + " bytes)" );
                    progress( _fileGet.inString().length(), _fileGet.fileSize() );
                }
            }
        });
        start();
    }
    
    /*     
     * Override the "run()" function - this is where the file is actually read.
     */
    public void run() {
        try {
            status( "Awaiting connection from " + _settings.difxControlAddress() );
            _fileGet.readString();
        } catch ( java.net.UnknownHostException e ) {
            error( "Connection failed: DiFX host \"" + _settings.difxControlAddress() + "\" is unknown.", null );
        }
    }
    
    /*
     * This function is called when transfer is terminated or completes.
     */
    protected void fileGetEnd() {
        //  Check the file size....this will tell us if anything went
        //  wrong, and to some degree what.
        int fileSize = _fileGet.fileSize();
        if ( _fileGet.inString() != null && fileSize == _fileGet.inString().length() ) {
            //  It worked!  Set the "success" value and get rid of the window.
            successCondition();
            return;
        }
        //  Otherwise, something went wrong.  We change the label to show the problem
        //  (to the extent that we understand it).  The progress bar is removed and
        //  the second label is made visible (in case we use it).
        if ( fileSize > 0 ) {
            //  Was it only partially read?
            if ( fileSize > _fileGet.inString().length() )
                error( "Connection terminated with "
                        + _fileGet.inString().length() + " of "
                        + fileSize + " bytes read.", null );
        }
        else if ( fileSize == 0 ) {
            error( "File \"" + _filePath + "\"", "has zero length." );
        }
        else if ( fileSize == -10 ) {
            error( "Socket connection timed out before DiFX host connected.", null );                                        }
        else if ( fileSize == -11 ) {
            error( "File transfer failed: ", _fileGet.error() );
        }
        else if ( fileSize == -1 ) {
            error( "Bad file name (probably the path was not complete.", null );
        }
        else if ( fileSize == -2 ) {
            error( "File \"" + _filePath + "\"", "does not exist on \"" + _settings.difxControlAddress() + "\"." );
        }
        else if ( fileSize == -3 ) {
            error( "Error - DiFX user \"" + _settings.difxControlUser() + "\"",
                    "does not have read permission for named file." );
        }
        else if ( fileSize == -4 ) {
            error( "DiFX user name " + _settings.difxControlUser(),
                    "is not valid on DiFX host \"" + _settings.difxControlAddress() + "\"." );   
        }
        else {
            error( "Unknown error encountered during file transfer.", null );
        }
    }
        
    /*
     * Get the file contents.
     */
    public String inString() { return _fileGet.inString(); }
    public String error() { return _error; }
    
    protected SystemSettings _settings;
    private DiFXCommand_getFile _fileGet;
    protected String _filePath;
    protected String _error;
    
}

