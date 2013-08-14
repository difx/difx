//=============================================================================
//
//   ServerSideConnection::getDirectory Function (and associated functions)
//
//!  Called when an instruction to either obtain an existing VSN directory
//!  or generate a new one.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <sys/statvfs.h>
#include <network/TCPClient.h>
#include <network/ActivePacketExchange.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <dirent.h>
#include <set>
#include <vector>
#include <fcntl.h>
#include <ExecuteSystem.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a user request for the contents of a VSN directory
//!  OR a request for to create that contents.  A forked process opens a two-way
//!  TCP connection to the source of the request.  Data are transfered over this
//!  connection and a limited command protocol is provided.
//-----------------------------------------------------------------------------
void ServerSideConnection::getDirectory( DifxMessageGeneric* G ) {
	const DifxMessageGetDirectory *S;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	pid_t childPid;
    char mark5[DIFX_MESSAGE_PARAM_LENGTH];
    char vsn[DIFX_MESSAGE_PARAM_LENGTH];
    char address[DIFX_MESSAGE_PARAM_LENGTH];
    char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
	char difxVersion[DIFX_MESSAGE_VERSION_LENGTH];
            char creationDate[DIFX_MESSAGE_FILENAME_LENGTH];
    int port;
    int generateNew;
    const char* mpiWrapper;
    network::ActivePacketExchange* packetExchange;
    
    static const int GETDIRECTORY_STARTED                = 301;
    static const int GETDIRECTORY_COMPLETED              = 302;
    static const int GETDIRECTORY_FULLPATH               = 303;
    static const int GETDIRECTORY_FAILED                 = 304;
    static const int MPIRUN_ERROR                        = 305;
    static const int NO_ENVIRONMENT_VARIABLE             = 306;
    static const int GETDIRECTORY_DATE                   = 307;
    static const int FILE_NOT_FOUND                      = 308;
    static const int GETDIRECTORY_FILESTART              = 309;
    static const int GETDIRECTORY_FILEDATA               = 310;
    static const int GENERATE_DIRECTORY_STARTED          = 311;
    static const int GENERATE_DIRECTORY_INFO             = 312;
    static const int GENERATE_DIRECTORY_COMPLETED        = 313;
    static const int GENERATE_DIRECTORY_ERRORS           = 314;
	
	//  Cast the message to a GetDirectory message, then make a copy of all
	//  parameters included.  We are entering a fork and we can't depend on the
	//  structure not being over-written.
	S = &G->body.getDirectory;
	strncpy( mark5, S->mark5, DIFX_MESSAGE_PARAM_LENGTH );
	strncpy( vsn, S->vsn, DIFX_MESSAGE_PARAM_LENGTH );
	strncpy( address, S->address, DIFX_MESSAGE_PARAM_LENGTH );
	strncpy( difxVersion, S->difxVersion, DIFX_MESSAGE_VERSION_LENGTH );
	port = S->port;
	generateNew = S->generateNew;
	
	//  Open a TCP socket connection to the server that should be running for us on the
    //  host that requested this directory information (the GUI, presumably).  This socket is used
    //  for diagnostic messages and to show progress on this specific job.  It can
    //  also be used for some rudimentary control of the job.
    network::TCPClient* guiSocket = new network::TCPClient( S->address, S->port );
    guiSocket->waitForConnect();
    //  Create a Job Monitor Connection out of this new socket if it connected properly.
    //  If it did not connect, we need to bail out now.
    if ( guiSocket->connected() ) {
        packetExchange = new network::ActivePacketExchange( guiSocket );
        packetExchange->sendPacket( GETDIRECTORY_STARTED, NULL, 0 );
    }
    else {
        diagnostic( ERROR, "client socket connection from guiServer to GUI failed - unable to start job" );
        return;
    }
    
    //  Option to run a different version of mpirun.
	if( S->mpiWrapper[0] )
	    mpiWrapper = S->mpiWrapper;
	else
		mpiWrapper = "mpirun";

	//  Fork a process to do everything from here.
	signal( SIGCHLD, SIG_IGN );
	childPid = fork();
	if( childPid == 0 ) {
	
	    bool noErrors = true;
  		
  		//  Find the directory path on the Mark5 where the VSN lives.
  		fullPath[0] = 0;
        snprintf( command, MAX_COMMAND_SIZE, 
            "%s -host %s /bin/echo $MARK5_DIR_PATH", mpiWrapper, mark5 );
        diagnostic( WARNING, "executing: %s\n", command );
        ExecuteSystem* executor = new ExecuteSystem( command );
        if ( executor->pid() > -1 ) {
            while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                if ( ret == 1 ) { // stdout
                    //  The only (non-error) return we expect from this command is the
                    //  value of the environment variable.
                    snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s", message );
                }
                else {            // stderr
                    packetExchange->sendPacket( MPIRUN_ERROR, message, strlen( message ) );
                    diagnostic( ERROR, "%s", message );
                }
            }
        }
        delete executor;

        //  Construct a complete path for the directory.  This requires that something
        //  was returned with the environment variable request above.
        if ( noErrors ) {
            if ( fullPath[0] != 0 ) {
                snprintf( fullPath + strlen( fullPath ), DIFX_MESSAGE_FILENAME_LENGTH - strlen( fullPath ), "/%s.dir", vsn );
                packetExchange->sendPacket( GETDIRECTORY_FULLPATH, fullPath, strlen( fullPath ) );
            }
            else {
                packetExchange->sendPacket( NO_ENVIRONMENT_VARIABLE, NULL, 0 );
                noErrors = false;
            }
        }

        //  Generate a new directory if that was requested.
        if ( generateNew ) {
            packetExchange->sendPacket( GENERATE_DIRECTORY_STARTED, NULL, 0 );
            snprintf( command, MAX_COMMAND_SIZE, 
                "%s -host %s %s mk5dir -n -f %s",
                mpiWrapper, mark5, _difxSetupPath, vsn );
            diagnostic( WARNING, "executing: %s\n", command );
            executor = new ExecuteSystem( command );
            bool thereWereErrors = false;
            if ( executor->pid() > -1 ) {
                while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                    if ( ret == 1 ) { // stdout
                        //  Each line represents file content.
                        packetExchange->sendPacket( GENERATE_DIRECTORY_INFO, message, strlen( message ) );
                        diagnostic( INFORMATION, "%s", message );
                    }
                    else {            // stderr
                        packetExchange->sendPacket( MPIRUN_ERROR, message, strlen( message ) );
                        diagnostic( ERROR, "%s", message );
                        thereWereErrors = true;
                    }
                }
            }
            if ( thereWereErrors )
                packetExchange->sendPacket( GENERATE_DIRECTORY_ERRORS, NULL, 0 );
            else
                packetExchange->sendPacket( GENERATE_DIRECTORY_COMPLETED, NULL, 0 );
            delete executor;
        }

        //  Get the creation date for the file, if it exists.
        if ( noErrors ) {
            creationDate[0] = 0;
            snprintf( command, MAX_COMMAND_SIZE, 
                "%s -host %s /bin/ls -l %s", mpiWrapper, mark5, fullPath );
            diagnostic( WARNING, "executing: %s\n", command );
            executor = new ExecuteSystem( command );
            if ( executor->pid() > -1 ) {
                while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                    if ( ret == 1 ) { // stdout
                        //  One line should be returned, containing the date the directory was
                        //  last updated.
                        snprintf( creationDate, DIFX_MESSAGE_FILENAME_LENGTH, "%s", message + 29 );
                        creationDate[12] = 0;
                    }
                    else {            // stderr
                        packetExchange->sendPacket( MPIRUN_ERROR, message, strlen( message ) );
                        diagnostic( ERROR, "%s", message );
                    }
                }
            }
            delete executor;
        }
        
        //  Send the creation date for this file if it exists.  If it doesn't exist,
        //  send that information.
        if ( noErrors ) {
            if ( creationDate[0] != 0 ) {
                packetExchange->sendPacket( GETDIRECTORY_DATE, creationDate, strlen( creationDate ) );
            }
            else {
                packetExchange->sendPacket( FILE_NOT_FOUND, NULL, 0 );
                noErrors = false;
            }
        }

        //  Get the file contents.
        if ( noErrors ) {
            packetExchange->sendPacket( GETDIRECTORY_FILESTART, NULL, 0 );
            snprintf( command, MAX_COMMAND_SIZE, 
                "%s -host %s /bin/cat %s", mpiWrapper, mark5, fullPath );
            diagnostic( WARNING, "executing: %s\n", command );
            executor = new ExecuteSystem( command );
            if ( executor->pid() > -1 ) {
                while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                    if ( ret == 1 ) { // stdout
                        //  Each line represents file content.
                        packetExchange->sendPacket( GETDIRECTORY_FILEDATA, message, strlen( message ) );
                    }
                    else {            // stderr
                        packetExchange->sendPacket( MPIRUN_ERROR, message, strlen( message ) );
                        diagnostic( ERROR, "%s", message );
                        noErrors = false;
                    }
                }
            }
            delete executor;
        }

  		//  Clean up litter and exit.
  		if ( noErrors )
	        packetExchange->sendPacket( GETDIRECTORY_COMPLETED, NULL, 0 );
	    else
	        packetExchange->sendPacket( GETDIRECTORY_FAILED, NULL, 0 );
	    sleep( 1 );
	    delete guiSocket;
		exit(EXIT_SUCCESS);
		
	}
	    
}

