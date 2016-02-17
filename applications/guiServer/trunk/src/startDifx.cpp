/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
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
#include <sys/syscall.h>
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
static const int RUN_DIFX_FAILURE_INPUTFILE_BAD_CONFIG       = 124;
static const int RUN_DIFX_CONFIG_PASSED                      = 125;
static const int RUN_DIFX_CONFIG_COMMAND_NOT_FOUND           = 126;
static const int RUN_DIFX_RUN_CONFIG_CHECK                   = 127;
static const int RUN_DIFX_STOPPED_AFTER_CONFIG               = 128;

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
	
	//  Run a configuration check on the .input file.  This makes sure access to 
	//  data sources is good, etc.
	jobMonitor->sendPacket( RUN_DIFX_RUN_CONFIG_CHECK, NULL, 0 );
	char command[MAX_COMMAND_SIZE];
	char message[DIFX_MESSAGE_LENGTH];
	snprintf( command, MAX_COMMAND_SIZE, 
	          "%s checkmpifxcorr %s", 
              _difxSetupPath,
              S->inputFilename );

    ExecuteSystem* executor = new ExecuteSystem( command );
    bool errorDetected = false;
    if ( executor->pid() > -1 ) {
        while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
            if ( ret == 1 ) { // stdout
                if ( strlen( message ) > 0 ) {
                    diagnostic( INFORMATION, "checkmpicorr... %s", message );
                    //  If we've found errors, don't produce the "no errors" message, as that
                    //  looks a bit silly.
                    if ( !errorDetected || std::string( message ).find( "No errors with input" ) == std::string::npos )
                        jobMonitor->sendPacket( RUN_DIFX_DIFX_MESSAGE, message, strlen( message ) );
                }
            }
            else {            // stderr
                diagnostic( WARNING, "checkmpifxcorr... %s", message );
                jobMonitor->sendPacket( RUN_DIFX_DIFX_WARNING, message, strlen( message ) );
                if ( std::string( message ).find( "command not found" ) != std::string::npos )
                    jobMonitor->sendPacket( RUN_DIFX_CONFIG_COMMAND_NOT_FOUND, NULL, 0 );
                else
                    errorDetected = true;
            }
        }
    }
    delete executor;
    
    //  Let the user know how the configuration test fared.
    if ( errorDetected ) {
        diagnostic( ERROR, "Input file %s failed the configuration check.", S->inputFilename );
    	jobMonitor->sendPacket( RUN_DIFX_FAILURE_INPUTFILE_BAD_CONFIG, NULL, 0 );
    }
    else {
        jobMonitor->sendPacket( RUN_DIFX_CONFIG_PASSED, NULL, 0 );
    }

    //  And possibly bail out on this process here, dependent on what the user wants.
    if ( S->function == DIFX_START_FUNCTION_CONFIG_ONLY || 
        ( errorDetected &&  S->function == DIFX_START_FUNCTION_BAIL_ON_CONFIG_FAIL ) ) {
        if ( errorDetected ) {
            jobMonitor->sendPacket( RUN_DIFX_DIFX_ERROR, "Execution stopped.", strlen( "Execution stopped" ) );
            diagnostic( ERROR, "Execution of job %s stopped.", S->inputFilename );
        }
        jobMonitor->sendPacket( RUN_DIFX_STOPPED_AFTER_CONFIG, NULL, 0 );
        delete jobMonitor;
        return;
    }
       
	//  Make sure all required machine specifications are included in the message.
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
	
	//  Turn on logging - this create a .difxlog file out of all messages related
	//  to this job.
	turnLoggingOn( startInfo );

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
    
    //  Turn off logging.
    turnLoggingOff( startInfo );

    //  Shut down the monitor thread.
    startInfo->runMonitor = 0;
    
    //  Remove the job from the running job list.
    removeRunningJob( startInfo->inputFile );
    
    //  Torch the client connection.
    delete startInfo->jobMonitor;

}

//-----------------------------------------------------------------------------
//!  Turn logging on for the given job.  This adds this jobs "identifier" to
//!  a list that can be searched when DiFX processing messages are received.
//!  Messages that are for this job are then logged to the logfile we open.
//-----------------------------------------------------------------------------
void ServerSideConnection::turnLoggingOn( DifxStartInfo* startInfo ) {
	char logFile[DIFX_MESSAGE_FILENAME_LENGTH];
	
    //  Figure out the log file name and open it for (appended) writing.
    strncpy( logFile, startInfo->inputFile, DIFX_MESSAGE_FILENAME_LENGTH );
    int l = strlen( logFile );
    for( int i = l-1; i > 0; i-- ) {
	    if( logFile[i] == '.' ) {
		    logFile[i] = 0;
		    break;
	    }
    }
    strncat( logFile, ".difxlog", DIFX_MESSAGE_FILENAME_LENGTH );
	startInfo->logFile = fopen( logFile, "a" );
	if( !startInfo->logFile ) {
        diagnostic( ERROR, "unable to open \"%s\" for writing: %s", logFile, strerror( errno ) );
        return;
	}
	
	//  The "identifier" is the name of the job - use the filebase without any path
	//  information.
	l = strlen( startInfo->filebase );
	int i = 0;
    for( i = l-1; i > 0; i-- ) {
	    if( startInfo->filebase[i] == '/' ) {
		    break;
	    }
    }
    strncpy( startInfo->identifier, startInfo->filebase + i + 1, DIFX_MESSAGE_FILENAME_LENGTH );
	
	//  Add this job to the map of jobs that are logging, using the identifier as a key.
    pthread_mutex_lock( &_loggingJobsMutex );
	_loggingJobsList.insert( std::pair<char*, void*>( startInfo->identifier, startInfo ) );
    pthread_mutex_unlock( &_loggingJobsMutex );
    
}

//-----------------------------------------------------------------------------
//!  Turn logging off for the given job.  The job is located in the logging list
//!  using its identifier, and then removed.
//-----------------------------------------------------------------------------
void ServerSideConnection::turnLoggingOff( DifxStartInfo* startInfo ) {
    pthread_mutex_lock( &_loggingJobsMutex );
	for ( std::map<char*, void*>::iterator iter = _loggingJobsList.begin(); iter != _loggingJobsList.end(); ++iter ) {
	    //  Match the key...
	    if ( !strcmp( iter->first, startInfo->identifier ) ) {
	        if ( startInfo->logFile )
	            fclose( startInfo->logFile );
	        _loggingJobsList.erase( iter );
	    }
	}
    pthread_mutex_unlock( &_loggingJobsMutex );
}

//-----------------------------------------------------------------------------
//!  This function is called each time guiServer intercepts a UDP message.  The
//!  "identifier" of the message is used to search a list of jobs that we are
//!  interested in logging.
//!
//!  Code was mostly swiped from difxmessage/utils/difxlog.c.
//-----------------------------------------------------------------------------
void ServerSideConnection::logDifxMessage( DifxMessageGeneric* G, char* message ) {
	const int TimeLength = 64;
	const int TagLength = 64+DIFX_MESSAGE_PARAM_LENGTH;
	int logLevel = DIFX_ALERT_LEVEL_INFO;
	time_t t;
	char timestr[TimeLength];
	char tag[TagLength];

    //  Search through the list of jobs that should be logging to find this identifier.
    pthread_mutex_lock( &_loggingJobsMutex );
	for ( std::map<char*, void*>::iterator iter = _loggingJobsList.begin(); iter != _loggingJobsList.end(); ++iter ) {
	    //  Match the key...
	    if ( !strcmp( iter->first, G->identifier ) ) {
	        //  This message pertains to a job in the list!
		    DifxStartInfo* startInfo = (DifxStartInfo*)iter->second;
	        //  Time-tag the message and include it in the logfile.
	        if ( startInfo->logFile ) {
		        time( &t );
		        strncpy( timestr, ctime( &t ), TimeLength - 1 );
		        timestr[TimeLength-1] = 0;
			//  Get rid of newline character.
			timestr[strlen( timestr ) - 1] = 0;
			    int l = snprintf( tag, TagLength, "%s %3d %s", timestr, G->mpiId, G->from );
			    if( l >= TagLength )
				    fprintf( stderr, "Developer error: TagLength is too small for timestr='%s', mpiIf=%d, from='%s'\n", timestr, G->mpiId, G->from );
			    if( G->type == DIFX_MESSAGE_ALERT ) {
				    const DifxMessageAlert *A;
				    A = &G->body.alert;
				    if( A->severity <= logLevel ) {
					    fprintf( startInfo->logFile, "%s %s  %s\n", tag, difxMessageAlertString[A->severity], A->message );
					    fflush( startInfo->logFile );
				    }
			    }
			    else if(G->type == DIFX_MESSAGE_STATUS) {
				    const DifxMessageStatus *S;
				    S = &G->body.status;				
				    fprintf( startInfo->logFile, "%s  STATUS %s  %s\n", tag, DifxStateStrings[S->state], S->message );
				    if( S->maxDS >= 0 ) {
					    fprintf( startInfo->logFile, "%s  WEIGHTS", tag );
					    for( int i = 0; i <= S->maxDS; ++i ) {
						    fprintf( startInfo->logFile, " %4.2f", S->weight[i] );
					    }
					    fprintf( startInfo->logFile, "\n" );
				    }
			    }
			    else if( G->type == DIFX_MESSAGE_TRANSIENT ) {
				    const DifxMessageTransient *T;
				    T = &G->body.transient;
				    fprintf( startInfo->logFile, "Transient event received: jobId=%s priority=%f startMJD=%14.8f stopMJD=%14.8f destDir=\"%s\" comment=\"%s\"\n",
					    T->jobId, 
					    T->priority,
					    T->startMJD,
					    T->stopMJD,
					    T->destDir,
					    T->comment );
				    fflush( startInfo->logFile );
			    }
	        }
	    }
	}
    pthread_mutex_unlock( &_loggingJobsMutex );

}


