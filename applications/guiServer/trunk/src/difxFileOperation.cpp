//=============================================================================
//
//   ServerSideConnection::difxFileOperation Function (and associated functions)
//
//!  Called with an instruction to perform "file" operations, such as creating
//!  directories, removing files, etc.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <sys/statvfs.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <pwd.h>
#include <fcntl.h>
#include <network/TCPClient.h>
#include <GUIClient.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a user request to perform a basic file operation
//!  (the GUI, presumably).  File operations include mkdir, rmdir, mv, rm,
//!  and ls.
//-----------------------------------------------------------------------------
void ServerSideConnection::difxFileOperation( DifxMessageGeneric* G ) {
	const DifxMessageFileOperation *S;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	const char *user;
	
	S = &G->body.fileOperation;
	
	user = getlogin();
	if ( !user ) {
    	user = getenv( "DIFX_USER_ID" );
	}
	
	//  Check the file operation against the list of "legal" operations.
	if ( !strcmp( S->operation, "mkdir" ) ) {
	    //  Make a new directory with the given path.  The "-p" option will make the entire path.  The
	    //  operation should be silent if all goes well - any output from popen will be something bad
	    //  (thus we generate an error message).
		snprintf( command, MAX_COMMAND_SIZE, "mkdir -p %s 2>&1", S->path );
  		FILE* fp = popen( command, "r" );
  		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL )
  		    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
  		pclose( fp );	    
  		snprintf( message, DIFX_MESSAGE_LENGTH, "%s performed!", command );
	}
	else if ( !strcmp( S->operation, "rmdir" ) ) {
	    //  The "rmdir" command doesn't remove content.
		snprintf( command, MAX_COMMAND_SIZE, "rm %s 2>&1", S->path );
  		FILE* fp = popen( command, "r" );
  		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL )
  		    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
  		pclose( fp );	    
    	snprintf( message, DIFX_MESSAGE_LENGTH, "%s performed", command );
    	printf( "%s\n", message );
	}
	else if ( !strcmp( S->operation, "rm" ) ) {
	    //  Remove the files matching the given path description.
		snprintf( command, MAX_COMMAND_SIZE, "rm %s %s 2>&1", S->arg, S->path );
  		FILE* fp = popen( command, "r" );
  		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL )
  		    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_INFO );
  		pclose( fp );	    
  		snprintf( message, DIFX_MESSAGE_LENGTH, "%s performed!", command );
    	printf( "%s\n", message );
	}
	else if ( !strcmp( S->operation, "mv" ) ) {
	    if ( S->arg[0] != '/' ) {
    		snprintf( message, DIFX_MESSAGE_LENGTH, "Destination of DifxFileOperation \"mv\" request (%s) must be a complete path", S->arg );
    		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
    		return;
	    }
		snprintf( command, MAX_COMMAND_SIZE, "mv %s %s", S->path, S->arg );
  		FILE* fp = popen( command, "r" );
  		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL )
  		    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_INFO );
  		pclose( fp );	    
  		snprintf( message, DIFX_MESSAGE_LENGTH, "%s performed!", command );
	}
	//  The "ls" operation actually returns data, so it must be provided with a TCP port and address.  It
	//  can take an undetermined amount of time so it is done in a thread.
	else if ( !strcmp( S->operation, "ls" ) ) {
	    //  Make a copy of information about the current file operation, including thread information
	    //  (so the thread variables are still viable after this function call returns).
	    DifxFileOperation* fileOperation = new DifxFileOperation;
	    snprintf( fileOperation->operation.path, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->path );
	    snprintf( fileOperation->operation.operation, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->operation );
	    snprintf( fileOperation->operation.dataNode, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->dataNode );
	    snprintf( fileOperation->operation.arg, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->arg );
	    snprintf( fileOperation->operation.address, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->address );
	    fileOperation->operation.port = S->port;
	    fileOperation->channelAllData = _channelAllData;
	    fileOperation->ssc = this;
        pthread_attr_init( &(fileOperation->threadAttr) );
        pthread_create( &(fileOperation->threadId), &(fileOperation->threadAttr), staticRunFileOperation, (void*)fileOperation );      	
	}
	else {
		snprintf( message, DIFX_MESSAGE_LENGTH, "Illegal DifxFileOperation request received - operation \"%s\" is not permitted", S->operation );
		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		return;
	}
	//difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
		
}

//-----------------------------------------------------------------------------
//!  Thread function for running file operations that require feedback to
//!  a remote host.  At the moment this is set up to run "ls" only, but
//!  could be expanded.
//-----------------------------------------------------------------------------
void ServerSideConnection::runFileOperation( DifxFileOperation* fileOperation ) {

	char message[DIFX_MESSAGE_LENGTH];
	char pCommand[MAX_COMMAND_SIZE];
	const DifxMessageFileOperation *S = &(fileOperation->operation);
	
	//  Open a client connection to the server that should be running for us on the
    //  remote host.  This is used to transfer the list of files.
    GUIClient* gc = new GUIClient( fileOperation->ssc, S->address, S->port );

    //  Make sure the connection worked....
    if ( gc->okay() ) {
        //snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection looks good", S->address, S->port );
        //difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );

		snprintf( pCommand, MAX_COMMAND_SIZE, "ls %s %s", S->arg, S->path );
		//snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", pCommand);
		//difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

  		FILE* fp = popen( pCommand, "r" );
  		char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
  		//  Each line of response from the ls should be a filename...
  		while ( fgets( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, fp ) != NULL ) {
  		    //  Send the full path.
        	//  Each separate file is preceded by its string length.
        	int sz = htonl( strlen( fullPath ) );
            gc->writer( &sz, sizeof( int ) );
            gc->writer( fullPath, strlen( fullPath ) );
  		    //difxMessageSendDifxAlert( fullPath, DIFX_ALERT_LEVEL_INFO );
  	    }
        pclose( fp );	    
  		//  Sending a zero length tells the remote host that the list is finished.
        int zero = 0;
        gc->writer( &zero, sizeof( int ) );
	} 

	//  Error with the connection...
	else {
        snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection FAILED", S->address, S->port );
      	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
    }
 
    delete gc;

}


