//=============================================================================
//
//   ServerSideConnection::startDifx Function (and associated functions)
//
//!  Called when an instruction to start a DiFX job is received.  This function
//!  is a member of the ServerSideConnection class.
//!
//!  Much of this code is swiped directly from the Mk5Daemon_startMpifxcorr()
//!  function of mk5daemon, with many thanks to Walter Brisken.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <sys/statvfs.h>
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
#include <configuration.h>
#include <GUIClient.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  These are packet types used for sending diagnostic and progress messages from
//!  running jobs to the GUI.  These get rather highly specific.  There are a
//!  few types that are used to obtain data from the GUI as well.
//-----------------------------------------------------------------------------
static const int RUN_DIFX_JOB_TERMINATED                     = 100;
static const int RUN_DIFX_JOB_ENDED_GRACEFULLY               = 101;
static const int RUN_DIFX_JOB_STARTED                        = 102;
static const int RUN_DIFX_PARAMETER_CHECK_IN_PROGRESS        = 103;
static const int RUN_DIFX_PARAMETER_CHECK_SUCCESS            = 104;
static const int RUN_DIFX_FAILURE_NO_HEADNODE                = 105;
static const int RUN_DIFX_FAILURE_NO_DATASOURCES             = 106;
static const int RUN_DIFX_FAILURE_NO_PROCESSORS              = 107;
static const int RUN_DIFX_FAILURE_NO_INPUTFILE_SPECIFIED     = 108;
static const int RUN_DIFX_FAILURE_INPUTFILE_NOT_FOUND        = 109;
static const int RUN_DIFX_FAILURE_INPUTFILE_NAME_TOO_LONG    = 110;
static const int RUN_DIFX_FAILURE_OUTPUT_EXISTS              = 111;
static const int RUN_DIFX_DELETING_PREVIOUS_OUTPUT           = 112;
static const int RUN_DIFX_STARTING_DIFX                      = 113;
static const int RUN_DIFX_DIFX_MESSAGE                       = 114;
static const int RUN_DIFX_DIFX_WARNING                       = 115;
static const int RUN_DIFX_DIFX_ERROR                         = 116;
static const int RUN_DIFX_DIFX_COMPLETE                      = 117;
static const int RUN_DIFX_DATA_FILE_SIZE                     = 118;
static const int RUN_DIFX_JOB_FAILED                         = 119;
static const int RUN_DIFX_JOB_ENDED_WITH_ERRORS              = 120;
static const int RUN_DIFX_DIFX_MONITOR_CONNECTION_ACTIVE     = 121;
static const int RUN_DIFX_DIFX_MONITOR_CONNECTION_BROKEN     = 122;
static const int RUN_DIFX_DIFX_MONITOR_CONNECTION_FAILED     = 123;

//-----------------------------------------------------------------------------
//!  Called in response to a user request to start a DiFX session.  This function
//!  does all of the necessary checking to assure (or at least increase the chances)
//!  that DiFX will run.  It also creates .thread and .machine files.  The actual
//!  running of DiFX is performed in a thread that this function, as its last act,
//!  spawns.
//-----------------------------------------------------------------------------
void ServerSideConnection::startDifx( DifxMessageGeneric* G ) {
	const int RestartOptionLength = 16;
	int l;
	char filebase[DIFX_MESSAGE_FILENAME_LENGTH];
	char filename[DIFX_MESSAGE_FILENAME_LENGTH];
	char workingDir[DIFX_MESSAGE_FILENAME_LENGTH];
	char machinesFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char restartOption[RestartOptionLength];
	char monitorOption[DIFX_MESSAGE_FILENAME_LENGTH];
	char inLine[MAX_COMMAND_SIZE];
	const char *jobName;
	const DifxMessageStart *S;
	bool outputExists = false;
	const char *mpiOptions;
	const char *mpiWrapper;
	GUIClient* jobMonitor;

	//  Cast the body of this message to a DifxMessageStart structure.
	S = &G->body.start;

	//  Open a client connection to the server that should be running for us on the
    //  host that requested this job start (the GUI, presumably).  This connection is used
    //  for diagnostic messages and to show progress on this specific job.  It can
    //  also be used for some rudimentary control of the job.
    jobMonitor = new GUIClient( this, S->address, S->port );
    jobMonitor->packetExchange();
    if ( jobMonitor->okay() )
        jobMonitor->sendPacket( RUN_DIFX_JOB_STARTED, NULL, 0 );
    else {
        diagnostic( ERROR, "client socket connection from guiServer to GUI failed - unable to start job" );
        delete jobMonitor;
        return;
    }
    
    //=========================================================================
    //  Checks below are run BEFORE we start the thread that actually runs
    //  the job.  Why?  Because we can only count on the viability of the
    //  DifxMessageGeneric structure in this function call.  The thread can't
    //  use it.
    //=========================================================================
    jobMonitor->sendPacket( RUN_DIFX_PARAMETER_CHECK_IN_PROGRESS, NULL, 0 );

	//  Make sure all needed parameters are included in the message.
	if ( S->headNode[0] == 0 ) {
    	diagnostic( ERROR, "DiFX start failed - no headnode specified." );
    	jobMonitor->sendPacket( RUN_DIFX_FAILURE_NO_HEADNODE, NULL, 0 );
        jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
        delete jobMonitor;
		return;
	}
	if ( S->nDatastream <= 0 ) {
    	diagnostic( ERROR, "DiFX start failed - no data sources specified." );
    	jobMonitor->sendPacket( RUN_DIFX_FAILURE_NO_DATASOURCES, NULL, 0 );
        jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
        delete jobMonitor;
		return;
	}
	if ( S->nProcess <= 0 ) {
    	diagnostic( ERROR, "DiFX start failed - no processing nodes  specified." );
    	jobMonitor->sendPacket( RUN_DIFX_FAILURE_NO_PROCESSORS, NULL, 0 );
        jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
        delete jobMonitor;
		return;
	}
	if ( S->inputFilename[0] == 0 ) {
    	diagnostic( ERROR, "DiFX start failed - no input file specified" );
    	jobMonitor->sendPacket( RUN_DIFX_FAILURE_NO_INPUTFILE_SPECIFIED, NULL, 0 );
        jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
        delete jobMonitor;
		return;
	}

	//  Check to make sure the input file exists
	if( access(S->inputFilename, R_OK) != 0 ) {
		diagnostic( ERROR, "DiFX start failed - input file %s not found.", S->inputFilename );
    	jobMonitor->sendPacket( RUN_DIFX_FAILURE_INPUTFILE_NOT_FOUND, NULL, 0 );
        jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
        delete jobMonitor;
		return;
	}

    //  Make sure the filename can fit in our allocated space for such things.
	if( strlen( S->inputFilename ) + 12 > DIFX_MESSAGE_FILENAME_LENGTH ) {
		diagnostic( ERROR, "Filename %s is too long.", S->inputFilename );
    	jobMonitor->sendPacket( RUN_DIFX_FAILURE_INPUTFILE_NAME_TOO_LONG, NULL, 0 );
        jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
        delete jobMonitor;
		return;
	}
	
    jobMonitor->sendPacket( RUN_DIFX_PARAMETER_CHECK_SUCCESS, NULL, 0 );

    //  Find the "working directory" (where the .input file resides and data will be put), the
    //  "filebase" (the path of the input file with ".input") and the "job name" (the name of
    //  the input file without .input or directory path).
	strcpy( workingDir, S->inputFilename );
    l = strlen( workingDir );
    for( int i = l-1; i > 0; i-- ) {
        if( workingDir[i] == '/') {
    		workingDir[i] = 0;
    		break;
    	}
    }	
	strcpy( filebase, S->inputFilename );
	l = strlen( filebase );
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

	//  Make sure the working directory exists and has enough free space
	struct statvfs fiData;
	int v = statvfs( workingDir, &fiData );
	if( v == 0 ) {
		long long freeSpace;
		freeSpace = fiData.f_bsize * fiData.f_bavail;
		if( freeSpace < 100000000 ) {
			diagnostic( WARNING, 
			    "Working directory %s has only %lld bytes free - mpifxcorr will likely crash!", 
				workingDir, freeSpace );
		}
		else if( fiData.f_ffree < 3 )
		{
			diagnostic( WARNING, "%s has no free inodes - mpifxcorr will likely crash!", 
				workingDir );
		}
	}
	else {
		diagnostic( ERROR, 
		    "statvfs failed when accessing directory %s : it seems not to exist!", 
			workingDir );
        delete jobMonitor;
		return;
	}
	
	//  See if the output file already exists.
	snprintf( filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s.difx", filebase );
	if( access( filename, F_OK ) == 0 )
	    outputExists = true;
    //  Should we be over writing it?  If not, we need to bail out.
	if( outputExists && !S->force ) {
		diagnostic( ERROR, "Output file %s exists.  Aborting correlation.", 
			filename );
        jobMonitor->sendPacket( RUN_DIFX_FAILURE_OUTPUT_EXISTS, NULL, 0 );
        jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
        delete jobMonitor;
		return;
	}

	//  Create a structure to hold all information about this job for the thread that will
	//  run it.
	DifxStartInfo* startInfo = new DifxStartInfo;
	startInfo->ssc = this;
	startInfo->jobMonitor = jobMonitor;
	strncpy( startInfo->filebase, filebase, MAX_COMMAND_SIZE );
	startInfo->filebase[strlen( filebase )] = 0;
	strncpy( startInfo->inputFile, S->inputFilename, DIFX_MESSAGE_FILENAME_LENGTH );
	
	//  Sanity check of the .machines file.  We use this to get the "np" value for mpirun
	//  as well.
	int procCount = 0;
    strncpy( machinesFilename, S->inputFilename, DIFX_MESSAGE_FILENAME_LENGTH );
    l = strlen( machinesFilename );
    for( int i = l-1; i > 0; i-- ) {
	    if( machinesFilename[i] == '.' ) {
		    machinesFilename[i] = 0;
		    break;
	    }
    }
    strncat( machinesFilename, ".machines", DIFX_MESSAGE_FILENAME_LENGTH );
    FILE* inp = fopen( machinesFilename, "r" );
    if ( inp != NULL ) {
        while ( fgets( inLine, MAX_COMMAND_SIZE, inp ) ) {
            if ( strlen( inLine ) > 0 )
                ++procCount;
        }
        fclose( inp );
    }
	
	snprintf( startInfo->jobName, MAX_COMMAND_SIZE, "%s", filebase );
	for ( int i = 0; filebase[i]; ++i ) {
	    if ( filebase[i] == '/' )
	        snprintf( startInfo->jobName, MAX_COMMAND_SIZE, "%s", filebase + i + 1 );
	}
	
	//  Build the "remove" command - this removes existing data if the force option is picked.
	if( S->force && outputExists ) {
		snprintf( startInfo->removeCommand, MAX_COMMAND_SIZE, "/bin/rm -rf %s.difx/", filebase );
		startInfo->force = 1;
	}
	else
	    startInfo->force = 0;
	    
	//  Include any options specified by the user - or use the defaults.
	if( S->mpiOptions[0] )
	    mpiOptions = S->mpiOptions;
 	else
 	    mpiOptions = "--mca mpi_yield_when_idle 1 --mca rmaps seq";

    //  Option to run a different version of mpirun.
	if( S->mpiWrapper[0] )
	    mpiWrapper = S->mpiWrapper;
	else
		mpiWrapper = "mpirun";

    //  Option to run different DiFX commands.
	if( S->difxProgram[0] )
		snprintf( startInfo->difxProgram, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->difxProgram );
	else
		snprintf( startInfo->difxProgram, DIFX_MESSAGE_FILENAME_LENGTH, "mpifxcorr" );
	    
	//  Build the "restart" instruction.  This becomes part of the start command.
	if ( S->restartSeconds > 0.0 )
	    snprintf( restartOption, RestartOptionLength, "-r %f", S->restartSeconds );
	else
	    restartOption[0] = 0;

	//  Include the option to run with the difx_monitor scheme.
	int monitorServerPort = 9999;  //  this should eventually be variable
	if ( S->function == DIFX_START_FUNCTION_RUN_MONITOR ) {
        startInfo->runMonitor = 1;
        //  Build the "monitor" instruction.  This becomes part of the start command.  The
        //  monitor is run on a "host" (which can be "localhost", i.e. the headnonde) with
        //  a port number.
        snprintf( monitorOption, DIFX_MESSAGE_FILENAME_LENGTH, "-Mlocalhost:%d", monitorServerPort );
	}
	else
	    monitorOption[0] = 0;
	    
	//  Build the actual start command.
	snprintf( startInfo->startCommand, MAX_COMMAND_SIZE, 
	          "%s -np %d --bynode --hostfile %s.machines %s %s %s %s %s %s", 
              mpiWrapper,
              procCount,
              filebase,
              mpiOptions,
              _difxSetupPath,
              startInfo->difxProgram,
              restartOption,
              S->inputFilename, monitorOption );
	
	//  Start a thread to run difx and report back stdout/stderr feedback.
    pthread_t threadId;
    pthread_create( &threadId, NULL, staticRunDifxThread, (void*)startInfo );
                   	
}

//-----------------------------------------------------------------------------
//!  Thread to actually run DiFX.  Running as a thread allows us to return the
//!  main thread to listen to other commands/messages.  This thread monitors
//!  all output from the running DiFX job and returns messages to the user
//!  interface.
//-----------------------------------------------------------------------------
void ServerSideConnection::runDifxThread( DifxStartInfo* startInfo ) {
	char message[DIFX_MESSAGE_LENGTH];
	
    //  Add this job to the "running" jobs list.
    addRunningJob( startInfo->inputFile );

    //  Delete data directories if "force" is in effect.
    if ( startInfo->force ) {
	    startInfo->jobMonitor->sendPacket( RUN_DIFX_DELETING_PREVIOUS_OUTPUT, NULL, 0 );
        system( startInfo->removeCommand );
    }
    
    //  Run the DiFX process!
    diagnostic( WARNING, "executing: %s\n", startInfo->startCommand );
    printf( "%s\n", startInfo->startCommand );
	startInfo->jobMonitor->sendPacket( RUN_DIFX_STARTING_DIFX, NULL, 0 );
	bool noErrors = true;  //  track whether any errors occured
	bool stillGoing = true;  //  track whether job still ran despite errors
    ExecuteSystem* executor = new ExecuteSystem( startInfo->startCommand );
    if ( executor->pid() > -1 ) {
        while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
            if ( ret == 1 ) { // stdout
                diagnostic( INFORMATION, "%s... %s", startInfo->difxProgram, message );
                startInfo->jobMonitor->sendPacket( RUN_DIFX_DIFX_MESSAGE, message, strlen( message ) );
                stillGoing = true;
            }
            else {            // stderr
                //  Only report errors if this is still a running job!
                if ( runningJobExists( startInfo->inputFile ) ) {
                    //  See if this message contains the string "warning" (multiple case situations)
                    //  indicating that....it is a warning.
                    if ( strcasestr( message, "WARNING" ) != NULL ) {
                        diagnostic( WARNING, "%s... %s", startInfo->difxProgram, message );
                        startInfo->jobMonitor->sendPacket( RUN_DIFX_DIFX_WARNING, message, strlen( message ) );
                    }
                    else {
                        diagnostic( ERROR, "%s... %s", startInfo->difxProgram, message );
	                    startInfo->jobMonitor->sendPacket( RUN_DIFX_DIFX_ERROR, message, strlen( message ) );
	                    noErrors = false;
	                    stillGoing = false;
                    }
                }
            }
        }
        if ( noErrors ) {
            if ( runningJobExists( startInfo->inputFile ) ) {
                diagnostic( WARNING, "%s complete", startInfo->difxProgram );
                difxMessageSendDifxStatus2( startInfo->jobName, DIFX_STATE_MPIDONE, "" );
        		startInfo->jobMonitor->sendPacket( RUN_DIFX_DIFX_COMPLETE, NULL, 0 );
                startInfo->jobMonitor->sendPacket( RUN_DIFX_JOB_ENDED_GRACEFULLY, NULL, 0 );
            } else {
                diagnostic( WARNING, "%s terminated by user", startInfo->difxProgram );
                startInfo->jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
            }
		}
        else if ( stillGoing ) {
            if ( runningJobExists( startInfo->inputFile ) ) {
                diagnostic( WARNING, "%s complete", startInfo->difxProgram );
                difxMessageSendDifxStatus2( startInfo->jobName, DIFX_STATE_MPIDONE, "" );
        		startInfo->jobMonitor->sendPacket( RUN_DIFX_DIFX_COMPLETE, NULL, 0 );
                startInfo->jobMonitor->sendPacket( RUN_DIFX_JOB_ENDED_WITH_ERRORS, NULL, 0 );
            } else {
                diagnostic( WARNING, "%s terminated by user", startInfo->difxProgram );
                startInfo->jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
            }
		}
        else {
            if ( runningJobExists( startInfo->inputFile ) ) {
                diagnostic( ERROR, "%s FAILED", startInfo->difxProgram );
                snprintf( message, DIFX_MESSAGE_LENGTH, "%s FAILED", startInfo->difxProgram );
                startInfo->jobMonitor->sendPacket( RUN_DIFX_DIFX_ERROR, message, strlen( message ) );
                startInfo->jobMonitor->sendPacket( RUN_DIFX_JOB_FAILED, NULL, 0 );
                difxMessageSendDifxStatus2( startInfo->jobName, DIFX_STATE_ABORTING, "" );
            } else {
                diagnostic( WARNING, "%s terminated by user", startInfo->difxProgram );
                startInfo->jobMonitor->sendPacket( RUN_DIFX_JOB_TERMINATED, NULL, 0 );
            }
        }
    }
    else {
        diagnostic( ERROR, "%s process not started for job %s; popen failed", startInfo->difxProgram, startInfo->jobName );
        snprintf( message, DIFX_MESSAGE_LENGTH, "%s process not started for job %s; popen failed", startInfo->difxProgram, startInfo->jobName );
	    startInfo->jobMonitor->sendPacket( RUN_DIFX_DIFX_ERROR, message, strlen( message ) );
        startInfo->jobMonitor->sendPacket( RUN_DIFX_JOB_FAILED, NULL, 0 );
        difxMessageSendDifxStatus2( startInfo->jobName, DIFX_STATE_ABORTING, "" );
    }
    delete executor;

    //  Shut down the monitor thread.
    startInfo->runMonitor = 0;
    
    //  Remove the job from the running job list.
    removeRunningJob( startInfo->inputFile );
    
    //  Torch the client connection.
    delete startInfo->jobMonitor;

}


