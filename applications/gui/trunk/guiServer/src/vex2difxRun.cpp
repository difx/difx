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
	char difxPath[DIFX_MESSAGE_FILENAME_LENGTH];
	char roundup[DIFX_MESSAGE_LENGTH];
	char hostname[DIFX_MESSAGE_LENGTH];
	pid_t childPid;
    ExecuteSystem* executor;
	
	S = &G->body.vex2DifxRun;

	childPid = fork();
	
	//  Forked process runs vex2difx...
    signal( SIGCHLD, SIG_IGN );
	if( childPid == 0 )
	{	    	
	    //  Get the current time, used below to figure out which files in the directory
	    //  are new.
	    struct timeval tv;
	    gettimeofday( &tv, NULL );
		
		//  This is where we actually run vex2difx
		snprintf( command, MAX_COMMAND_SIZE, "source %s; cd %s; vex2difx -f %s", 
		          _difxSetupPath,
				  S->passPath,
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
            diagnostic( ERROR, "vex2difx FAILED" );
        delete executor;

		//  Next thing to run - calcif2.
		snprintf( command, MAX_COMMAND_SIZE, "source %s; cd %s; calcif2 -f -a", 
				  _difxSetupPath,
				  S->passPath );
		
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
            diagnostic( ERROR, "calcif2 FAILED" );
        delete executor;
        
		
    	//  Open a TCP socket connection to the server that should be running for us on the
        //  remote host.  This is used to transfer a list of all of the files we have created.
        int sockfd;
        struct sockaddr_in servaddr;
        sockfd = socket( AF_INET, SOCK_STREAM, 0 );
        bzero( &servaddr, sizeof( servaddr ) );
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
                	if ( buf.st_mtime >= tv.tv_sec ) {
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

