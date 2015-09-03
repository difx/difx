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
#include <errno.h>
#include <GUIClient.h>
#include <map>

using namespace guiServer;

//-----------------------------------------------------------------------------
//  These are packet types used for exchanging data with the GUI.
//-----------------------------------------------------------------------------
static const int GET_JOB_STATUS_TASK_TERMINATED                     = 109;
static const int GET_JOB_STATUS_TASK_ENDED_GRACEFULLY               = 101;
static const int GET_JOB_STATUS_TASK_STARTED                        = 102;
static const int GET_JOB_STATUS_INPUT_FILE_NAME                     = 104;
static const int GET_JOB_STATUS_NO_DIFXLOG                          = 105;
static const int GET_JOB_STATUS_STATUS                              = 106;
static const int GET_JOB_STATUS_OPEN_ERROR                          = 107;
static const int GET_JOB_STATUS_NO_STATUS                           = 108;

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

    //  To list all of the job files, run "ls".  Each line of response should be a valid job
    //  filename.    
	snprintf( pCommand, MAX_COMMAND_SIZE, "ls %s", getJobStatusInfo->files.c_str() );

	char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
	std::string inputFile;
	FILE* pp = popen( pCommand, "r" );
	while ( fgets( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, pp ) != NULL ) {

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
                    char finalStatus[100];
                    char logLine[512];
                    finalStatus[0] = 0;
                    while ( fgets( logLine, 512, fp ) != NULL ) {
                        //  Make sure this line makes sense to us!
                        if ( strlen( logLine ) > 48 ) {
                            //  Find what kind of log entry this is - it follows the date, time, and source.
                            int pos = 48;
                            while ( logLine[pos] == ' ' )
                                ++pos;
                            //  First we handle the "short" status case, where the only thing we are 
                            //  interested in is the final "STATUS".
                            if ( getJobStatusInfo->shortStatus ) {
                                if ( !strncmp( logLine + pos, "STATUS", 6 ) ) {
                                    pos += 6;
                                    while ( logLine[pos] == ' ' )
                                        ++pos;
                                    strncpy( finalStatus, logLine + pos, 100 );
                                    finalStatus[99] = 0;
                                    if ( strlen( finalStatus ) > 0 && finalStatus[strlen( finalStatus ) - 1] == '\n' )
                                        finalStatus[strlen( finalStatus ) - 1] = 0;
                                }
                            }  
                        }         
                    }
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


