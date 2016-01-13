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
//=============================================================================
//
//   ServerSideConnection::vex2difxRun Function (and associated functions)
//
//!  Called when an instruction to run vex2difx on a job is received.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <ExecuteSystem.h>
#include <sys/statvfs.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <pwd.h>
#include <fcntl.h>
#include <dirent.h>
#include <network/TCPClient.h>
#include <signal.h>
#include <GUIClient.h>
#include <set>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a request to run "vex2difx".  Work is done in a
//!  thread.
//-----------------------------------------------------------------------------
void ServerSideConnection::vex2difxRun( DifxMessageGeneric* G ) {
    Vex2DifxInfo* vex2DifxInfo = new Vex2DifxInfo;
    vex2DifxInfo->ssc = this;
    memcpy( &(vex2DifxInfo->v2dRun), &(G->body.vex2DifxRun), sizeof( DifxMessageVex2DifxRun ) );
    pthread_create( &(vex2DifxInfo->threadId), NULL, 
                    staticRunVex2Difx, (void*)vex2DifxInfo );      
}

//----------------------------------------------------------------------------
//!  The thread that runs the vex2difx command (as well as calcif2).  An
//!  additional thread is started to monitor and report the results.  These
//!  share a client connection with the GUI.
//----------------------------------------------------------------------------
void ServerSideConnection::runVex2Difx( Vex2DifxInfo* vex2DifxInfo ) {
	const DifxMessageVex2DifxRun *S = &(vex2DifxInfo->v2dRun);
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
    ExecuteSystem* executor;
    bool someError = false;
		    
	//  Generate a temporary garbage file that will give us a time stamp for when this
    //  function call was made - this is used to determine which files have been
    //  created by the activities that follow in this function.
    ulong modTime = fileTimeStamp( someError, S->passPath );
		
	GUIClient* guiClient;

	//  Open a client connection to the server that should be running for us on the
    //  host that requested this task (the GUI, presumably).  This connection is used
    //  to communicate which .input files have been created.
    guiClient = new GUIClient( vex2DifxInfo->ssc, S->address, S->port );
    guiClient->packetExchange();

	//  This is where we actually run vex2difx - this creates the .input and .calc files.
	//  It also creates some other things we don't care about (.flag files and a .joblist).
	snprintf( command, MAX_COMMAND_SIZE, "cd %s; %s vex2difx -f %s", 
			  S->passPath,
			  _difxSetupPath,
			  S->v2dFile );
	
	diagnostic( WARNING, "Executing: %s", command );
    executor = new ExecuteSystem( command );
    while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
        if ( ret == 1 )  { // stdout
            if ( strlen( message ) )
                diagnostic( INFORMATION, "vex2difx... %s", message );
        }
        else             // stderr
            diagnostic( ERROR, "vex2difx... %s", message );
    }
    if ( executor->noErrors() )
        diagnostic( WARNING, "vex2difx complete" );
    else
        diagnostic( ERROR, "vex2difx RETURNED ERRORS" );
    delete executor;
    
    //  List all of the .input files created.  These names are sent to the GUI (in full path
    //  form).
    struct dirent **inputList;
    int n = scandir( S->passPath, &inputList, inputFilter, alphasort );
    if ( n >= 0 ) {
        for ( int i = 0; i < n; ++i ) {
        	char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
        	snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", S->passPath, inputList[i]->d_name );
        	struct stat buf;
        	stat( fullPath, &buf );
        	if ( (ulong)buf.st_mtime >= modTime ) {
        	    //  Each file path is preceded by its string length.
        	    int sz = htonl( strlen( fullPath ) );
        	    guiClient->writer( &sz, sizeof( int ) );
        	    guiClient->writer( fullPath, strlen( fullPath ) );
        	}
        }
        free( inputList );
    }

    //  Run calcif2 on each .calc file that is new - i.e. that has a modification time matching
    //  or after the "modTime" found at the start of this function. 
    diagnostic( WARNING, "running calcif2 on all new .calc files" );
    struct dirent **calclist;
    n = scandir( S->passPath, &calclist, calcFilter, alphasort );
    if ( n >= 0 ) {
        for ( int i = 0; i < n; ++i ) {
        	char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
        	snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", S->passPath, calclist[i]->d_name );
        	struct stat buf;
        	stat( fullPath, &buf );
        	if ( (ulong)buf.st_mtime >= modTime ) {
	            snprintf( command, MAX_COMMAND_SIZE, "cd %s; %s calcif2 -f %s", 
			              S->passPath,
			              _difxSetupPath,
			              calclist[i]->d_name );	
                executor = new ExecuteSystem( command );
                while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                    if ( ret != 1 )  // stderr
                        diagnostic( ERROR, "calcif2... %s", message );
                }
                if ( !executor->noErrors() )
                    diagnostic( ERROR, "calcif2 RETURNED ERRORS" );
                delete executor;
                //  See if we can stat the .im file that should have been just created.
                int len = strlen( fullPath ) - 1;
                while ( fullPath[len] != '.' && len != 0 )
                    --len;
                fullPath[len+1] = 0;
                strncat( fullPath, "im", 2 );
                if ( !stat( fullPath, &buf ) ) {
                    //  If we find a file that is new and greater than zero length, send the
                    //  full path to the GUI.
                    if ( (ulong)buf.st_mtime >= modTime && buf.st_size > 0 ) {
                	    int sz = htonl( strlen( fullPath ) );
                	    guiClient->writer( &sz, sizeof( int ) );
                	    guiClient->writer( fullPath, strlen( fullPath ) );
                    }
                    else {
                        if ( buf.st_size <= 0 )
                            diagnostic( ERROR, "calcif2 product %s is zero length\n", fullPath );
                        else
                            diagnostic( ERROR, "calcif2 product %s exists but looks old\n", fullPath );
                    }
                }
                else
                    diagnostic( ERROR, "calcif2 failed to create %s\n", fullPath );
        	}
        }
        free( calclist );
    }
    diagnostic( WARNING, "calcif2 complete" );

    //  Sending a zero length tells the GUI that the list is finished.
    int zero = 0;
    guiClient->writer( &zero, sizeof( int ) );
    
    //  Sleep for a moment for all of that to clear, then shut down the connection.
    sleep( 2 );
    delete guiClient;

}

//-----------------------------------------------------------------------------
//!  This function is used to generate a time stamp that can be compared to
//!  the creation time for files.  It does this by creating a garbage file,
//!  looking at when it was created, then deleting it.  The time stamp will
//!  be created in a specified directory location, or in /tmp if no location
//!  is specified.
//-----------------------------------------------------------------------------
ulong ServerSideConnection::fileTimeStamp( bool& someError, const char* path ) {
    ExecuteSystem* executor;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
		    
    //  Generate a temporary garbage file that will give us a time stamp for when this
    //  function call was made - this is used below to determine which files have been
    //  created by the activities that follow in this function.  This used to be done with
    //  gettimeofday() but this was found to not necessarily work if the node running this
    //  function remote mounted the disk and did not coordinate its system time with the
    //  node that owned the disk.
    ulong modTime = 0;
    if ( path != NULL )
        snprintf( command, MAX_COMMAND_SIZE, "cd %s; /bin/touch garbage_timestamp_file_please_delete", path );
    else
        snprintf( command, MAX_COMMAND_SIZE, "/bin/touch /tmp/garbage_timestamp_file_please_delete" );
    executor = new ExecuteSystem( command );
    someError = false;
    while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
        //  An error returned by this is worrisome (probably means we don't have write permission,
        //  or the disk is full, or something like that).
        if ( ret != 1 )  {
            diagnostic( ERROR, "Error returned creating timing file: %s", message );
            someError = true;
        }
    }
    delete executor;
    //  Get the modification time for the garbage file.
    if ( !someError ) {
        struct dirent **namelist;
        int n = 0;
        if ( path != NULL )
            n = scandir( path, &namelist, 0, alphasort );
        else 
            n = scandir( "/tmp", &namelist, 0, alphasort );
        if ( n > 0 ) {
            while ( n-- ) {
                if ( !strcmp( namelist[n]->d_name, "garbage_timestamp_file_please_delete" ) ) {
            	    char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
            	    if ( path != NULL )
            	        snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", path, namelist[n]->d_name );
            	    else
            	        snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "/tmp/%s", namelist[n]->d_name );
            	    struct stat buf;
            	    stat( fullPath, &buf );
            	    modTime = buf.st_mtime;
            	}
                free( namelist[n] );
            }
            free( namelist );
        }
    }
    else {
        //  This probably won't save us!  
	    struct timeval tv;
	    gettimeofday( &tv, NULL );
	    modTime = tv.tv_sec;
	}
	//  Try to remove the garbage file we created.  Any errors probably indicate a wider problem.
	if ( path != NULL )
        snprintf( command, MAX_COMMAND_SIZE, "cd %s; /bin/rm garbage_timestamp_file_please_delete", path );
	else
        snprintf( command, MAX_COMMAND_SIZE, "/bin/rm /tmp/garbage_timestamp_file_please_delete" );
    executor = new ExecuteSystem( command );
    while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
        if ( ret != 1 )  {
            diagnostic( ERROR, "Error returned removing timing file: %s", message );
            someError = true;
        }
    }
    delete executor;
    
    return modTime;
    
}


