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

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a request to run "vex2difx".  Work is done in a
//!  thread.
//-----------------------------------------------------------------------------
void ServerSideConnection::vex2difxRun( DifxMessageGeneric* G ) {
    Vex2DifxInfo* vex2DifxInfo = new Vex2DifxInfo;
    vex2DifxInfo->ssc = this;
    memcpy( &(vex2DifxInfo->v2dRun), &(G->body.vex2DifxRun), sizeof( DifxMessageVex2DifxRun ) );
    pthread_attr_init( &(vex2DifxInfo->threadAttr) );
    pthread_create( &(vex2DifxInfo->threadId), &(vex2DifxInfo->threadAttr), 
                    staticRunVex2Difx, (void*)vex2DifxInfo );      
}

//----------------------------------------------------------------------------
//!  And this is the thread.
//----------------------------------------------------------------------------
void ServerSideConnection::runVex2Difx( Vex2DifxInfo* vex2DifxInfo ) {
	const DifxMessageVex2DifxRun *S = &(vex2DifxInfo->v2dRun);
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
    ExecuteSystem* executor;
		    
	    //  Generate a temporary garbage file that will give us a time stamp for when this
	    //  function call was made - this is used below to determine which files have been
	    //  created by the activities that follow in this function.  This used to be done with
	    //  gettimeofday() but this was found to not necessarily work if the node running this
	    //  function remote mounted the disk and did not coordinate its system time with the
	    //  node that owned the disk.
	    ulong modTime = 0;
	    snprintf( command, MAX_COMMAND_SIZE, "cd %s; /bin/touch garbage_timestamp_file_please_delete", S->passPath );
        executor = new ExecuteSystem( command );
        bool someError = false;
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
            int n = scandir( S->passPath, &namelist, 0, alphasort );
            if ( n > 0 ) {
                while ( n-- ) {
                    if ( !strcmp( namelist[n]->d_name, "garbage_timestamp_file_please_delete" ) ) {
                	    char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
                	    snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", S->passPath, namelist[n]->d_name );
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
	    snprintf( command, MAX_COMMAND_SIZE, "cd %s; /bin/rm garbage_timestamp_file_please_delete", S->passPath );
        executor = new ExecuteSystem( command );
        while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
            if ( ret != 1 )  {
                diagnostic( ERROR, "Error returned removing timing file: %s", message );
                someError = true;
            }
        }
        delete executor;
		
	GUIClient* monitor;

	//  Open a client connection to the server that should be running for us on the
    //  host that requested this task (the GUI, presumably).  This connection is used
    //  to communicate which .input files have been created.
    monitor = new GUIClient( vex2DifxInfo->ssc, S->address, S->port );
    monitor->packetExchange();
        
		//  This is where we actually run vex2difx - this creates the input files.
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

		//  Next thing to run - calcif2.
		snprintf( command, MAX_COMMAND_SIZE, "cd %s; %s calcif2 -f -a", 
				  S->passPath,
				  _difxSetupPath );
		
		diagnostic( WARNING, "Executing: %s", command );
        executor = new ExecuteSystem( command );
        while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
            if ( ret == 1 )  // stdout
                diagnostic( INFORMATION, "calcif2... %s", message );
            else             // stderr
                diagnostic( ERROR, "calcif2... %s", message );
        }
        if ( executor->noErrors() )
            diagnostic( WARNING, "calcif2 complete" );
        else
            diagnostic( ERROR, "calcif2 RETURNED ERRORS" );
        delete executor;
        
		
        //  Use the client connection to return a list of all new files.
        if ( monitor->okay() ) {
            //snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection looks good", S->address, S->port );
            //difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
		    //  Produce a list of the files in the target directory and send that back to the GUI.
		    //  Include only those files that have been modified/created since the beginning of this
	    	//  function call (measured in integer seconds).  This will include a bunch of things
	    	//  we aren't interested in, but *should* only include those .input files that are new
	    	//  (unless someone is simultaneously running vex2difx during the same second!).
            struct dirent **namelist;
            int n = scandir( S->passPath, &namelist, 0, alphasort );
            if ( n < 0 ) {
                snprintf( message, DIFX_MESSAGE_LENGTH, "%s", strerror( errno ) );
                difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            } else {
                while ( n-- ) {
                	char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
                	snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", S->passPath, namelist[n]->d_name );
                	struct stat buf;
                	stat( fullPath, &buf );
                	//  Compare the last modification time of each file in the pass directory
                	//  with the "gettimeofday" result at the top of this function.
                	if ( (ulong)buf.st_mtime >= modTime ) {
                	    //  Send the full path name of this file.
                	    //  Each separate file is preceded by its string length.
                	    int sz = htonl( strlen( fullPath ) );
                	    monitor->writer( &sz, sizeof( int ) );
                	    monitor->writer( fullPath, strlen( fullPath ) );
                	}
                    free( namelist[n] );
                }
                free( namelist );
                //  Sending a zero length tells the GUI that the list is finished.
                int zero = 0;
                monitor->writer( &zero, sizeof( int ) );
            }
		} 
		
		//  Error with the socket...
		else {
            snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection FAILED", S->address, S->port );
          	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
        }
        delete monitor;
        
		//difxMessageSendDifxAlert("vex2difx completed", DIFX_ALERT_LEVEL_INFO);
}

