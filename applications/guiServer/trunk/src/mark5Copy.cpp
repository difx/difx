//=============================================================================
//
//   ServerSideConnection::mark5Copy Function (and associated functions)
//
//!  Copies files from a Mark5 module to disk.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <GUIClient.h>
#include <unistd.h>
#include <ExecuteSystem.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called when a request for a Mark5 copy operation is received.  This
//!  sets up the operation and then runs it in a thread.
//-----------------------------------------------------------------------------
void ServerSideConnection::mark5Copy( DifxMessageGeneric* G ) {

    //  Cast the argument to a Mark5Copy message format (contained in difxmessage.h).
	const DifxMessageMark5Copy *S = &G->body.mark5Copy;
	//  Make a copy of all data in the message to the structure we use (defined in
	//  ServerSideConnection.h).
	Mark5CopyInfo* info = new Mark5CopyInfo;	
	snprintf( info->mark5Copy.mpiWrapper, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->mpiWrapper );
	snprintf( info->mark5Copy.difxVersion, DIFX_MESSAGE_VERSION_LENGTH, "%s", S->difxVersion );
	snprintf( info->mark5Copy.mark5, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->mark5 );
	snprintf( info->mark5Copy.vsn, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->vsn );
	snprintf( info->mark5Copy.scans, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->scans );
	snprintf( info->mark5Copy.destination, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->destination );
	snprintf( info->mark5Copy.address, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->address );
	info->mark5Copy.port = S->port;
	info->ssc = this;
    pthread_create( &(info->threadId), NULL, staticMark5CopyThread, (void*)info );      
}	
	
//-----------------------------------------------------------------------------
//!  Thread to actually run the Mark5 copy operation.  This thread contains
//!  a trivial bi-directional communication protocol that is used (mostly) for
//!  communicating progress to the GUI.
//-----------------------------------------------------------------------------
void ServerSideConnection::mark5CopyThread( Mark5CopyInfo* info ) {

	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
    char creationDate[DIFX_MESSAGE_FILENAME_LENGTH];
    const char* mpiWrapper;
	const DifxMessageMark5Copy *S = &(info->mark5Copy);
    
    static const int MARK5COPY_STARTED                   = 301;
    static const int MARK5COPY_COMPLETED                 = 302;
    static const int MARK5COPY_FAILED                    = 304;
    static const int MPIRUN_ERROR                        = 305;
    static const int NO_ENVIRONMENT_VARIABLE             = 306;
    static const int DIRECTORY_NOT_FOUND                 = 308;
    static const int MARK5COPY_INFO                      = 312;
    static const int MARK5COPY_ERRORS                    = 314;
	
    //  Open a client connection to the server that should be running for us on the
    //  remote host.
    GUIClient* gc = new GUIClient( info->ssc, S->address, S->port );
    if ( gc->okay() ) {
        gc->packetExchange();
        gc->sendPacket( MARK5COPY_STARTED, NULL, 0 );
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
	
    snprintf( command, MAX_COMMAND_SIZE, 
        "%s -host %s %s mk5cp -f %s %s %s",
        mpiWrapper, S->mark5, info->ssc->difxSetupPath(), S->vsn, S->scans, S->destination );
    info->ssc->diagnostic( WARNING, "executing: %s\n", command );
    ExecuteSystem* executor = new ExecuteSystem( command );
    bool thereWereErrors = false;
    if ( executor->pid() > -1 ) {
        while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
            if ( ret == 1 ) { // stdout
                //  Each line represents file content.
                gc->sendPacket( MARK5COPY_INFO, message, strlen( message ) );
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
        gc->sendPacket( MARK5COPY_ERRORS, NULL, 0 );
    delete executor;

	//  Clean up litter and exit.
    if ( noErrors )
        gc->sendPacket( MARK5COPY_COMPLETED, NULL, 0 );
    else
        gc->sendPacket( MARK5COPY_FAILED, NULL, 0 );
    sleep( 1 );
    delete gc;
			    
}

