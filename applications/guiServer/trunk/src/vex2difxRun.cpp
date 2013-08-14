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

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a user request to transfer a file either from the
//!  DiFX host to an external host (the GUI, presumably) or the reverse.  This
//!  is accomplished by opening a client TCP connection to the external host
//!  (which must have initiated a server already).
//-----------------------------------------------------------------------------
void ServerSideConnection::vex2difxRun( DifxMessageGeneric* G ) {
	const DifxMessageVex2DifxRun *S;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	pid_t childPid;
    ExecuteSystem* executor;
	
	S = &G->body.vex2DifxRun;

	childPid = fork();
	
	//  Forked process runs vex2difx...
    signal( SIGCHLD, SIG_IGN );
	if( childPid == 0 )
	{	    	
	    
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
		
		//  This is where we actually run vex2difx
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
        
		
    	//  Open a TCP socket connection to the server that should be running for us on the
        //  remote host.  This is used to transfer a list of all of the files we have created.
        int sockfd;
        struct sockaddr_in servaddr;
        sockfd = socket( AF_INET, SOCK_STREAM, 0 );
        memset( &servaddr, 0, sizeof( servaddr ) );
        servaddr.sin_family = AF_INET;
        servaddr.sin_port = htons( S->port );
        inet_pton( AF_INET, S->address, &servaddr.sin_addr );
        int ret = connect( sockfd, (const sockaddr*)&servaddr, sizeof( servaddr ) );
        
        //  Hopefully the connection worked....
        if ( ret == 0 ) {
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
                	    write( sockfd, &sz, sizeof( int ) );
                	    write( sockfd, fullPath, strlen( fullPath ) );
                	}
                    free( namelist[n] );
                }
                free( namelist );
                //  Sending a zero length tells the GUI that the list is finished.
                int zero = 0;
                write( sockfd, &zero, sizeof( int ) );
            }
		} 
		
		//  Error with the socket...
		else {
            snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection FAILED", S->address, S->port );
          	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
        }
        close( sockfd );
        
		//difxMessageSendDifxAlert("vex2difx completed", DIFX_ALERT_LEVEL_INFO);
    	exit(EXIT_SUCCESS);
		
	}
}

