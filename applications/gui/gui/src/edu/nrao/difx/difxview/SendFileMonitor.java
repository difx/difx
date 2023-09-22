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

import edu.nrao.difx.difxutilities.DiFXCommand_sendFile;

import java.awt.Frame;

/**
 *
 * @author jspitzak
 */
public class SendFileMonitor extends PopupMonitor {
    
    public SendFileMonitor( Frame frame, int x, int y, String filePath, String content, SystemSettings settings ) {
        super( frame, x, y, 600, 145, 2000 );
        _settings = settings;
        _filePath = filePath;
        _content = content;
        //  Open a "get file" operation and set the various callbacks.
        _fileSend = new DiFXCommand_sendFile( _filePath, _settings );
        _fileSend.addEndListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( !dismissActive() ) {
                    status( "Transfer completed" );
                    showStatus2( false );
                    fileSendEnd();
                }
            }
        });
        _fileSend.addAcceptListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( !dismissActive() ) {
                    status( "Connection established" );
                    status2( "File transfer in progress." );
                    showStatus2( true );
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
            _fileSend.sendString( _content );
        } catch ( java.net.UnknownHostException e ) {
            error( "Connection failed: DiFX host \"" + _settings.difxControlAddress() + "\" is unknown.", null );
        }
    }
    
    /*
     * This function is called when transfer is terminated or completes.
     */
    protected void fileSendEnd() {
        //  Check the file size....this will tell us if anything went
        //  wrong, and to some degree what.
        int fileSize = _fileSend.fileSize();
        if ( fileSize == _content.length() ) {
            //  It worked!  Set the "success" value and get rid of the window.
            successCondition();
            return;
        }
        //  Otherwise, something went wrong.  We change the label to show the problem
        //  (to the extent that we understand it).  The progress bar is removed and
        //  the second label is made visible (in case we use it).
        if ( fileSize >= 0 ) {
            //  Was it only partially read?
            if ( fileSize < _content.length() )
                error( "Transmission error for \"" + _filePath + "\".",
                       "Only " + _fileSend.fileSize() + 
                       " of " + _content.length() + " characters were transmitted." );
        }
        else if ( fileSize == -10 ) {
            error( "Socket connection timed out before DiFX host connected.", 
                   "Upload of \"" + _filePath + "\" failed: " );
        }
        else if ( fileSize == -11 ) {
            error( "Upload of \"" + _filePath + "\" failed: ", _fileSend.error() );
        }
        else if ( fileSize == -1 ) {
            error( "Mangled path name \"" + _filePath + "\".", 
                   "Does the destination directory start with \"/\"?");
        }
        else if ( fileSize == -2 ) {
            error( "Destination directory for \"" + _filePath + "\"", "does not exist on \"" + _settings.difxControlAddress() + "\"." );
        }
        else if ( fileSize == -3 ) {
            error( "Error - DiFX user \"" + _settings.difxControlUser() + "\"",
                    "does not have write permission for named file." );
        }
        else if ( fileSize == -4 ) {
            error( "DiFX user name " + _settings.difxControlUser(),
                    "is not valid on DiFX host \"" + _settings.difxControlAddress() + "\"." );   
        }
        else {
            error( "Unknown error encountered during file transfer.", null );
        }
    }
        
    protected SystemSettings _settings;
    private DiFXCommand_sendFile _fileSend;
    protected String _filePath;
    protected String _content;
    
}

