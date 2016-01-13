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
//   ServerSideConnection::getJobStatusThread Function (and associated functions)
//
//!  Called when an instruction to obtain the "status" of a list of jobs is
//!  received.  Status is determined by parsing .difxlog files.  Status detail
//!  is up to the user.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <ExecuteSystem.h>
#include <network/TCPClient.h>
#include <string>
#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#include <GUIClient.h>
#include <map>

using namespace guiServer;

//-----------------------------------------------------------------------------
//  These are packet types used for exchanging data with the GUI.
//-----------------------------------------------------------------------------
static const int GET_JOB_STATUS_TASK_ENDED_GRACEFULLY               = 101;
static const int GET_JOB_STATUS_TASK_STARTED                        = 102;
static const int GET_JOB_STATUS_INPUT_FILE_NAME                     = 104;
static const int GET_JOB_STATUS_NO_DIFXLOG                          = 105;
static const int GET_JOB_STATUS_STATUS                              = 106;
static const int GET_JOB_STATUS_OPEN_ERROR                          = 107;
static const int GET_JOB_STATUS_NO_STATUS                           = 108;
static const int GET_JOB_STATUS_TASK_TERMINATED                     = 109;
static const int GET_JOB_STATUS_TIME_STRING                         = 110;
static const int GET_JOB_STATUS_FORK_FAILED                         = 111;

//-----------------------------------------------------------------------------
//!  Thread function for running the operation.
//-----------------------------------------------------------------------------
void ServerSideConnection::getJobStatusThread( GetJobStatusInfo* getJobStatusInfo ) {

    char pCommand[MAX_COMMAND_SIZE];

    bool keepGoing = true;

	//  Open a client connection to the server that should be running for us on the
    //  host that requested this task.
    GUIClient* monitor = new GUIClient( getJobStatusInfo->ssc, getJobStatusInfo->address.c_str(), getJobStatusInfo->port );
    monitor->packetExchange();
    
    //  If the connection was made properly, send acknowledgement.  If not, we need to bail out now.
    if ( monitor->okay() )
        monitor->sendPacket( GET_JOB_STATUS_TASK_STARTED, NULL, 0 );
    else {
        diagnostic( ERROR, "(ServerSideConnection::getJobStatusThread) - client socket connection from guiServer failed - unable to provide job status" );
        delete monitor;
	    return;
    }
    
    //  Generate a time stamp for this activity.  This is simply sent to the client program
    //  where it can be compared to the time stamps applied to each log item.
    time_t t;
    char timebuf[26];
    time( &t );
    ctime_r( &t, timebuf );
    timebuf[strlen( timebuf ) - 1] = 0;
    monitor->sendPacket( GET_JOB_STATUS_TIME_STRING, timebuf, strlen( timebuf ) );

    //  To list all of the job files, run "ls".  Each line of response should be a valid job
    //  filename.    
	snprintf( pCommand, MAX_COMMAND_SIZE, "ls %s", getJobStatusInfo->files.c_str() );

	char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
	std::string inputFile;
	FILE* pp = popen( pCommand, "r" );
	if ( pp == NULL ) {
	    monitor->sendPacket( GET_JOB_STATUS_FORK_FAILED, NULL, 0 );
	    delete monitor;
	    keepGoing = false;
	}
	while ( keepGoing && fgets( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, pp ) != NULL ) {

	    //  If this is a .input file, convert it to a .difxlog file.
	    if ( strlen( fullPath ) > 0 && fullPath[strlen( fullPath ) - 1] == '\n' )
	        fullPath[strlen( fullPath ) - 1] = 0;  //  get rid of newline character
	    inputFile.assign( fullPath );
	    if ( inputFile.rfind( ".input" ) == inputFile.length() - 6 ) {
	        monitor->sendPacket( GET_JOB_STATUS_INPUT_FILE_NAME, inputFile.c_str(), strlen( inputFile.c_str() ) );
	        inputFile.replace( inputFile.rfind( ".input" ), 6, ".difxlog" );

            //  Try to open the .difxlog file.  Assume the file doesn't exist if we can't.
            FILE* fp = NULL;
            if ( keepGoing ) {
                fp = fopen( inputFile.c_str(), "r" );
                if ( fp == NULL ) {
                    monitor->sendPacket( GET_JOB_STATUS_NO_DIFXLOG, NULL, 0 );
                }
                else {
                    //  Parse the file.
                    char finalStatus[512];
                    char logLine[512];
                    finalStatus[0] = 0;
                    while ( fgets( logLine, 512, fp ) != NULL ) {
                        //  Make sure there is something to this line...a time stamp and source at least.
                        if ( strlen( logLine ) > 30 ) {
                            //  Each log entry has a time stamp (24 characters), an MPI process number (I believe
                            //  there is one/processor), and a source - the hostname involved.
                            char timeStamp[25];
                            strncpy( timeStamp, logLine, 24 );
                            timeStamp[24] = 0;
                            int mpiProcess;
                            sscanf( logLine + 25, "%d", &mpiProcess );
                            int pos = 29;
                            char source[100];
                            int i = 0;
                            while ( logLine[pos+i] != 0 && logLine[pos+i] != ' ' ) {
                                source[i] = logLine[pos+i];
                                ++i;
                            }
                            source[i] = 0;
                            pos += i;
                            //  Find what kind of log entry this is - it follows the date, time, and source.
                            while ( logLine[pos] == ' ' && logLine[pos] != 0 )
                                ++pos;
                            char entryType[100];
                            i = 0;
                            while ( logLine[pos+i] != ' ' && logLine[pos+i] != 0 ) {
                                entryType[i] = logLine[pos+i];
                                ++i;
                            }
                            entryType[i] = 0;
                            //  First we handle the "short" status case, where the only thing we are 
                            //  interested in is the final "STATUS".
                            if ( getJobStatusInfo->shortStatus ) {
                                if ( !strncmp( entryType, "STATUS", 6 ) ) {
                                    pos += 6;
                                    while ( logLine[pos] == ' ' )
                                        ++pos;
                                    strncpy( finalStatus, timeStamp, 511 );
                                    strncat( finalStatus, logLine + pos, 511 - strlen( finalStatus ) );
                                    finalStatus[99] = 0;
                                    if ( strlen( finalStatus ) > 0 && finalStatus[strlen( finalStatus ) - 1] == '\n' )
                                        finalStatus[strlen( finalStatus ) - 1] = 0;
                                }
                            }  
                        }         
                    }
                    fclose( fp );
                    if ( finalStatus[0] != 0 )
                        monitor->sendPacket( GET_JOB_STATUS_STATUS, finalStatus, strlen( finalStatus ) );
                    else
                        monitor->sendPacket( GET_JOB_STATUS_NO_STATUS, NULL, 0 );
                }
            }
    
	    }

    }
    pclose( pp );	    
    
    //  Report a healthy process end...or otherwise.
    if ( keepGoing )
        monitor->sendPacket( GET_JOB_STATUS_TASK_ENDED_GRACEFULLY, NULL, 0 );
        
    else
        monitor->sendPacket( GET_JOB_STATUS_TASK_TERMINATED, NULL, 0 );

    delete monitor;
        		
}


