//=============================================================================
//
//   ServerSideConnection::getDirectory Function (and associated functions)
//
//!  Called when an instruction to either obtain an existing VSN directory
//!  or generate a new one.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <GUIClient.h>
#include <unistd.h>
#include <ExecuteSystem.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a user request for the contents of a VSN directory
//!  OR a request for to create that contents.  A forked process opens a two-way
//!  TCP connection to the source of the request.  Data are transfered over this
//!  connection and a limited command protocol is provided.
//-----------------------------------------------------------------------------
void ServerSideConnection::getDirectory( DifxMessageGeneric* G ) {

    //  Cast the argument to a getDirectory message format.
	const DifxMessageGetDirectory *S = &G->body.getDirectory;
	//  Make a copy of all data in the message.
	GetDirectoryInfo* info = new GetDirectoryInfo;	
	snprintf( info->getDirectory.mpiWrapper, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->mpiWrapper );
	snprintf( info->getDirectory.difxVersion, DIFX_MESSAGE_VERSION_LENGTH, "%s", S->difxVersion );
	snprintf( info->getDirectory.mark5, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->mark5 );
	snprintf( info->getDirectory.vsn, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->vsn );
	snprintf( info->getDirectory.address, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->address );
	info->getDirectory.generateNew = S->generateNew;
	info->getDirectory.port = S->port;
	info->ssc = this;
    pthread_create( &(info->threadId), NULL, staticGetDirectoryThread, (void*)info );      
}	
	
//-----------------------------------------------------------------------------
//!  Thread to actually run the get directory operations.
//-----------------------------------------------------------------------------
void ServerSideConnection::getDirectoryThread( GetDirectoryInfo* info ) {

	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
    char creationDate[DIFX_MESSAGE_FILENAME_LENGTH];
    const char* mpiWrapper;
	const DifxMessageGetDirectory *S = &(info->getDirectory);
    
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
	
    //  Open a client connection to the server that should be running for us on the
    //  remote host.
    GUIClient* gc = new GUIClient( info->ssc, S->address, S->port );
    if ( gc->okay() ) {
        gc->packetExchange();
        gc->sendPacket( GETDIRECTORY_STARTED, NULL, 0 );
    }
    else {
        info->ssc->diagnostic( ERROR, "client socket connection from guiServer to GUI failed - unable to start job" );
        return;
    }
    
    //  Option to run a different version of mpirun.
	if( S->mpiWrapper[0] )
	    mpiWrapper = S->mpiWrapper;
	else
		mpiWrapper = "mpirun";

	
	    bool noErrors = true;
  		
  		//  Find the directory path on the Mark5 where the VSN lives.
  		char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
  		fullPath[0] = 0;
        snprintf( command, MAX_COMMAND_SIZE, 
            "%s -host %s /bin/echo $MARK5_DIR_PATH", mpiWrapper, S->mark5 );
        info->ssc->diagnostic( WARNING, "executing: %s\n", command );
        ExecuteSystem* executor = new ExecuteSystem( command );
        if ( executor->pid() > -1 ) {
            while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                if ( ret == 1 ) { // stdout
                    //  The only (non-error) return we expect from this command is the
                    //  value of the environment variable.
                    snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s", message );
                }
                else {            // stderr
                    gc->sendPacket( MPIRUN_ERROR, message, strlen( message ) );
                    info->ssc->diagnostic( ERROR, "%s", message );
                }
            }
        }
        delete executor;

        //  Construct a complete path for the directory.  This requires that something
        //  was returned with the environment variable request above.
        if ( noErrors ) {
            if ( fullPath[0] != 0 ) {
                snprintf( fullPath + strlen( fullPath ), DIFX_MESSAGE_FILENAME_LENGTH - strlen( fullPath ), "/%s.dir", S->vsn );
                gc->sendPacket( GETDIRECTORY_FULLPATH, fullPath, strlen( fullPath ) );
            }
            else {
                gc->sendPacket( NO_ENVIRONMENT_VARIABLE, NULL, 0 );
                noErrors = false;
            }
        }

        //  Generate a new directory if that was requested.
        if ( S->generateNew ) {
            gc->sendPacket( GENERATE_DIRECTORY_STARTED, NULL, 0 );
            snprintf( command, MAX_COMMAND_SIZE, 
                "%s -host %s %s mk5dir -n -f %s",
                mpiWrapper, S->mark5, info->ssc->difxSetupPath(), S->vsn );
            info->ssc->diagnostic( WARNING, "executing: %s\n", command );
            executor = new ExecuteSystem( command );
            bool thereWereErrors = false;
            if ( executor->pid() > -1 ) {
                while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                    if ( ret == 1 ) { // stdout
                        //  Each line represents file content.
                        gc->sendPacket( GENERATE_DIRECTORY_INFO, message, strlen( message ) );
                        info->ssc->diagnostic( INFORMATION, "%s", message );
                    }
                    else {            // stderr
                        gc->sendPacket( MPIRUN_ERROR, message, strlen( message ) );
                        info->ssc->diagnostic( ERROR, "%s", message );
                        thereWereErrors = true;
                    }
                }
            }
            if ( thereWereErrors )
                gc->sendPacket( GENERATE_DIRECTORY_ERRORS, NULL, 0 );
            else
                gc->sendPacket( GENERATE_DIRECTORY_COMPLETED, NULL, 0 );
            delete executor;
        }

        //  Get the creation date for the file, if it exists.
        if ( noErrors ) {
            creationDate[0] = 0;
            snprintf( command, MAX_COMMAND_SIZE, 
                "%s -host %s /bin/ls -l %s", mpiWrapper, S->mark5, fullPath );
            info->ssc->diagnostic( WARNING, "executing: %s\n", command );
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
                        gc->sendPacket( MPIRUN_ERROR, message, strlen( message ) );
                        info->ssc->diagnostic( ERROR, "%s", message );
                    }
                }
            }
            delete executor;
        }
        
        //  Send the creation date for this file if it exists.  If it doesn't exist,
        //  send that information.
        if ( noErrors ) {
            if ( creationDate[0] != 0 ) {
                gc->sendPacket( GETDIRECTORY_DATE, creationDate, strlen( creationDate ) );
            }
            else {
                gc->sendPacket( FILE_NOT_FOUND, NULL, 0 );
                noErrors = false;
            }
        }

        //  Get the file contents.
        if ( noErrors ) {
            gc->sendPacket( GETDIRECTORY_FILESTART, NULL, 0 );
            snprintf( command, MAX_COMMAND_SIZE, 
                "%s -host %s /bin/cat %s", mpiWrapper, S->mark5, fullPath );
            info->ssc->diagnostic( WARNING, "executing: %s\n", command );
            executor = new ExecuteSystem( command );
            if ( executor->pid() > -1 ) {
                while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                    if ( ret == 1 ) { // stdout
                        //  Each line represents file content.
                        gc->sendPacket( GETDIRECTORY_FILEDATA, message, strlen( message ) );
                    }
                    else {            // stderr
                        gc->sendPacket( MPIRUN_ERROR, message, strlen( message ) );
                        info->ssc->diagnostic( ERROR, "%s", message );
                        noErrors = false;
                    }
                }
            }
            delete executor;
        }

  		//  Clean up litter and exit.
  		if ( noErrors )
	        gc->sendPacket( GETDIRECTORY_COMPLETED, NULL, 0 );
	    else
	        gc->sendPacket( GETDIRECTORY_FAILED, NULL, 0 );
	    sleep( 1 );
	    delete gc;
			    
}

