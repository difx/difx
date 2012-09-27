//=============================================================================
//
//   ServerSideConnection::stopDifx Function
//
//!  Called when an instruction to stop a DiFX job is received.  This function
//!  is a member of the ServerSideConnection class.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <ExecuteSystem.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a user request to stop a DiFX job.  Jobs are uniquely
//!  identified by input file, which also, conveniently, is part of the command
//!  to mpirun that STARTED the job.  To stop the specific job, we effectively do
//!  a "ps" searching for "mpi" and parse out the PIDs of any line that contains 
//!  the input file name.  This should kill everything associated with the job.
//!
//!  This is a "set and forget" command.  There is no dynamic feedback.  However
//!  messages are sent using the standard DiFX message system.
//-----------------------------------------------------------------------------
void ServerSideConnection::stopDifx( DifxMessageGeneric* G ) {
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	char machinesFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char inputFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char machineName[MAX_COMMAND_SIZE];
	const char *jobName;
	char filebase[DIFX_MESSAGE_FILENAME_LENGTH];
	const DifxMessageStop *S;
	const char *user;
	const char *mpiWrapper;
	const char *difxProgram;

	//  Cast the body of this message to a DifxMessageStop structure.		const DifxMessageStart *S;
    S = &G->body.stop;

    //  The user is used to further isolate processes that have been started by this
    //  user.
	user = getlogin();
	if ( !user ) {
    	user = getenv( "DIFX_USER_ID" );
	}

    //  Generate the job name.  We need this for multicast messages.
	strcpy( filebase, S->inputFilename );
	int l = strlen( filebase );
	for( int i = l-1; i > 0; i-- ) {
		if( filebase[i] == '.' ) {
			filebase[i] = 0;
			break;
		}
	}
	jobName = filebase;
	for( int i = 0; filebase[i]; ++i ) {
		if( filebase[i] == '/' ) {
			jobName = filebase + i + 1;
		}
	}

    //  We'll need this as well.
    strncpy( inputFilename, S->inputFilename, DIFX_MESSAGE_FILENAME_LENGTH );
	
    //  Get the machines file name.  This is the input file name with ".input" replaced
    //  by ".machines".
    strncpy( machinesFilename, S->inputFilename, DIFX_MESSAGE_FILENAME_LENGTH );
    machinesFilename[ DIFX_MESSAGE_FILENAME_LENGTH - 1 ] = 0;
    l = strlen( machinesFilename );
    for( int i = l-1; i > 0; i-- ) {
	    if( machinesFilename[i] == '.' ) {
		    machinesFilename[i] = 0;
		    break;
	    }
    }
    strncat( machinesFilename, ".machines", DIFX_MESSAGE_FILENAME_LENGTH );
    
    //  Option to run a different version of mpirun.
	if( S->mpiWrapper[0] )
	    mpiWrapper = S->mpiWrapper;
	else
		mpiWrapper = "mpirun";

    //  Option to run different DiFX commands.
	if( S->difxProgram[0] )
		difxProgram = S->difxProgram;
	else
	    difxProgram = "mpifxcorr";
	    
	//  Remove this job from the list of running jobs.  The startDifx() function will then
	//  know how to interpret the errors that it will shortly receive.
	removeRunningJob( inputFilename );
    difxMessageSendDifxStatus2( jobName, DIFX_STATE_TERMINATING, "" );
	    
    //  Each line in the machines file is a node that should be running part of
    //  this job.  Use mpirun on each one to locate PIDs and kill jobs.
    FILE* fp = fopen( machinesFilename, "r" );
    if ( fp != NULL ) {
    
        //  Read each line and parse out a machine name.
        int lineCount = 0;
        while ( fgets( machineName, MAX_COMMAND_SIZE, fp ) ) {
            ++lineCount;
            machineName[strcspn( machineName, " \n" )] = 0;
            if ( machineName[0] != 0 && lineCount == 1 ) {  //  Only using the first machine - the head node!
        
            //  Run "ps" to get all running mpi processes that contain the input file.
            snprintf( command, MAX_COMMAND_SIZE, 
                "source %s; %s -host %s /bin/ps -ef | /bin/grep %s | /bin/grep %s | /bin/grep -v /bin/grep",
                _difxSetupPath, mpiWrapper, machineName, difxProgram, S->inputFilename );
            diagnostic( WARNING, "executing: %s\n", command );
            ExecuteSystem* executor = new ExecuteSystem( command );
            if ( executor->pid() > -1 ) {
                while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                    if ( ret == 1 ) { // stdout
                        //  The "ps" command returns a line containing the user name that started each process
                        //  followed by the process ID.  There is a bunch of other stuff we don't care about.
                        //  First, limit ourselves to items that have our user name.
                        if ( !strncmp( message, user, strcspn( message, " " ) ) ) {
                            //  Parse the PID from these lines.  This will be the first integer.
                            int pid = -1;
                            sscanf( message + strcspn( message, " " ), "%d", &pid );
                            if ( pid > 0 ) {
                                diagnostic( WARNING, "killing PID %d on %s", pid, machineName );
                                snprintf( command, MAX_COMMAND_SIZE, 
                                    "source %s; %s -host %s /bin/kill -9 %d",
                                    _difxSetupPath, mpiWrapper, machineName, pid );
                                system( command );
                            }
                        }
                    }
                    else {            // stderr
                        //  We don't care about any errors, really.
                        diagnostic( ERROR, "%s", message );
                    }
                }
            }
            delete executor;
            }
    
        }
        fclose( fp );
        
    }

    difxMessageSendDifxStatus2( jobName, DIFX_STATE_TERMINATED, "" );

#ifdef NOT_NECESSARY
    //  For good measure try to kill things locally.  This may or may not work to kill
    //  off a running job completely (money on "not").  But it will at least stop the thing
    //  if mpirun isn't working for some reason.
    gethostname( machineName, MAX_COMMAND_SIZE );
    snprintf( command, MAX_COMMAND_SIZE, "/bin/ps -ef | /bin/grep %s | /bin/grep %s | /bin/grep -v /bin/grep", difxProgram, S->inputFilename );
    diagnostic( WARNING, "executing: %s\n", command );
    ExecuteSystem* executor = new ExecuteSystem( command );
    if ( executor->pid() > -1 ) {
        while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
            if ( ret == 1 ) { // stdout
                //  The "ps" command returns a line containing the user name that started each process
                //  followed by the process ID.  There is a bunch of other stuff we don't care about.
                //  First, limit ourselves to items that have our user name.
                if ( !strncmp( message, user, strcspn( message, " " ) ) ) {
                    //  Parse the PID from these lines.  This will be the first integer.
                    int pid = -1;
                    sscanf( message + strcspn( message, " " ), "%d", &pid );
                    if ( pid > 0 ) {
                        diagnostic( WARNING, "killing PID %d on %s", pid, machineName );
                        kill( pid, SIGKILL );
                    }
                }
            }
            else {            // stderr
                //  We don't care about any errors, really.
                diagnostic( ERROR, "%s", message );
            }
        }
    }
    delete executor;   
#endif 

}


