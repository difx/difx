//=============================================================================
//
//   ServerSideConnection::machinesDefinition Function (and associated functions)
//
//!  Called when an instruction to create machines and threads files from user
//!  specifications.
//!
//!  Much of this code is swiped directly from the Mk5Daemon_startMpifxcorr()
//!  function of mk5daemon, with many thanks to Walter Brisken.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <sys/statvfs.h>
#include <network/TCPClient.h>
#include <network/PacketExchange.h>
#include <list>
#include <string>
#include <signal.h>
#include <GUIClient.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//  These are packet types used for sending diagnostic and progress messages from
//  a machines definition task to the GUI.  These get rather highly specific.
//-----------------------------------------------------------------------------
static const int MACHINE_DEF_TASK_TERMINATED                     = 100;
static const int MACHINE_DEF_TASK_ENDED_GRACEFULLY               = 101;
static const int MACHINE_DEF_TASK_STARTED                        = 102;
static const int MACHINE_DEF_PARAMETER_CHECK_IN_PROGRESS         = 103;
static const int MACHINE_DEF_PARAMETER_CHECK_SUCCESS             = 104;
static const int MACHINE_DEF_FAILURE_NO_HEADNODE                 = 105;
static const int MACHINE_DEF_FAILURE_NO_DATASOURCES              = 106;
static const int MACHINE_DEF_FAILURE_NO_PROCESSORS               = 107;
static const int MACHINE_DEF_WARNING_NO_MACHINES_FILE_SPECIFIED  = 108;
static const int MACHINE_DEF_WARNING_NO_THREADS_FILE_SPECIFIED   = 109;
static const int MACHINE_DEF_THREADS_FILE_NAME                   = 110;
static const int MACHINE_DEF_MACHINES_FILE_NAME                  = 111;
static const int MACHINE_DEF_FAILURE_NO_FILES_SPECIFIED          = 112;
static const int MACHINE_DEF_FAILURE_OPEN_MACHINES_FILE          = 113;
static const int MACHINE_DEF_FAILURE_OPEN_THREADS_FILE           = 114;
static const int MACHINE_DEF_MACHINES_FILE_CREATED               = 115;
static const int MACHINE_DEF_THREADS_FILE_CREATED                = 116;
static const int MACHINE_DEF_FAILURE_FILE_REMOVAL                = 117;
static const int MACHINE_DEF_FAILURE_POPEN                       = 118;
static const int MACHINE_DEF_FAILURE_MPIRUN                      = 119;
static const int MACHINE_DEF_SUCCESS_MPIRUN                      = 120;
static const int MACHINE_DEF_LOW_THREAD_COUNT                    = 121;
static const int MACHINE_DEF_RUNNING_MPIRUN_TESTS                = 122;

//-----------------------------------------------------------------------------
//!  Called in response to a user request to specify machines/threads for
//!  mpirun to use in a DiFX processing session.  This creates .thread and 
//!  .machine files with names that match the associated job.  The bulk of the
//!  work is threaded.
//-----------------------------------------------------------------------------
void ServerSideConnection::machinesDefinition( DifxMessageGeneric* G ) {

    MachinesDefinitionInfo* machinesDefinitionInfo = new MachinesDefinitionInfo;
    machinesDefinitionInfo->ssc = this;
    memcpy( &(machinesDefinitionInfo->definition), &(G->body.machinesDefinition), sizeof( DifxMessageMachinesDefinition ) );
    pthread_attr_init( &(machinesDefinitionInfo->threadAttr) );
    pthread_create( &(machinesDefinitionInfo->threadId), &(machinesDefinitionInfo->threadAttr), 
                    staticRunMachinesDefinition, (void*)machinesDefinitionInfo );      
}

//-----------------------------------------------------------------------------
//!  Thread function for actually running the machines definition operations.
//-----------------------------------------------------------------------------
void ServerSideConnection::runMachinesDefinition( MachinesDefinitionInfo* machinesDefinitionInfo ) {

	int l;
	char machinesFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char threadsFilename[DIFX_MESSAGE_FILENAME_LENGTH];
	char workingDir[DIFX_MESSAGE_FILENAME_LENGTH];
	char testPath[DIFX_MESSAGE_FILENAME_LENGTH];
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	char nodename[MAX_COMMAND_SIZE];
	FILE *out;
	const DifxMessageMachinesDefinition *S = &(machinesDefinitionInfo->definition);
	const char *mpiWrapper;
	GUIClient* monitor;

	//  Open a client connection to the server that should be running for us on the
    //  host that requested this task (the GUI, presumably).  This connection is used
    //  for diagnostic messages and to show progress.
    monitor = new GUIClient( machinesDefinitionInfo->ssc, S->address, S->port );
    monitor->packetExchange();
    
    //  If the connection was made properly, send acknowledgement.  If not, we need to bail out now.
    if ( monitor->okay() )
        monitor->sendPacket( MACHINE_DEF_TASK_STARTED, NULL, 0 );
    else {
        diagnostic( ERROR, "client socket connection from guiServer to GUI failed - unable to create machines/threads files" );
        delete monitor;
	    return;
    }
    
    //=========================================================================
    //  Checks below are run BEFORE we start the thread that creates the machines
    //  and threads files.
    //=========================================================================
    monitor->sendPacket( MACHINE_DEF_PARAMETER_CHECK_IN_PROGRESS, NULL, 0 );

	//  Make sure all needed parameters are included in the message.
	if ( S->headNode[0] == 0 ) {
    	diagnostic( ERROR, "Machines specification failed - no headnode specified." );
    	monitor->sendPacket( MACHINE_DEF_FAILURE_NO_HEADNODE, NULL, 0 );
        monitor->sendPacket( MACHINE_DEF_TASK_TERMINATED, NULL, 0 );
        delete monitor;
	    return;
	}
	if ( S->nDatastream <= 0 ) {
    	diagnostic( ERROR, "Machines specification failed - no data sources specified." );
    	monitor->sendPacket( MACHINE_DEF_FAILURE_NO_DATASOURCES, NULL, 0 );
        monitor->sendPacket( MACHINE_DEF_TASK_TERMINATED, NULL, 0 );
        delete monitor;
	    return;
	}
	if ( S->nProcess <= 0 ) {
    	diagnostic( ERROR, "Machines specification failed - no processing nodes  specified." );
    	monitor->sendPacket( MACHINE_DEF_FAILURE_NO_PROCESSORS, NULL, 0 );
        monitor->sendPacket( MACHINE_DEF_TASK_TERMINATED, NULL, 0 );
        delete monitor;
	    return;
	}

	//  We need names for the threads and machines files.  If these are not provided, we
	//  will use the input file name.  If that is not provided, we can't do anything and have
	//  to bail out.
	//  Machines file first.
	if ( S->machinesFilename[0] == 0 ) {
    	diagnostic( WARNING, "No machines file specified - generating path from input file." );
    	monitor->sendPacket( MACHINE_DEF_WARNING_NO_MACHINES_FILE_SPECIFIED, NULL, 0 );
    	//  Check the input file name...it must be there!
	    if ( S->inputFilename[0] == 0 ) {
        	diagnostic( ERROR, "Machines specification failed - no input file or machines file specified" );
        	monitor->sendPacket( MACHINE_DEF_FAILURE_NO_FILES_SPECIFIED, NULL, 0 );
            monitor->sendPacket( MACHINE_DEF_TASK_TERMINATED, NULL, 0 );
            delete monitor;
	        return;
	    }
	    //  Create a .machines file name using the input file name.
	    strncpy( machinesFilename, S->inputFilename, DIFX_MESSAGE_FILENAME_LENGTH );
	    l = strlen( machinesFilename );
	    for( int i = l-1; i > 0; i-- ) {
		    if( machinesFilename[i] == '.' ) {
			    machinesFilename[i] = 0;
			    break;
		    }
	    }
	    strncat( machinesFilename, ".machines", DIFX_MESSAGE_FILENAME_LENGTH );
	}
	else
	    strncpy( machinesFilename, S->machinesFilename, DIFX_MESSAGE_FILENAME_LENGTH );
	monitor->sendPacket( MACHINE_DEF_MACHINES_FILE_NAME, machinesFilename, strlen( machinesFilename ) );

	//  Threads file.
	if ( S->threadsFilename[0] == 0 ) {
    	diagnostic( WARNING, "No threads file specified - generating path from input file." );
    	monitor->sendPacket( MACHINE_DEF_WARNING_NO_THREADS_FILE_SPECIFIED, NULL, 0 );
    	//  Check the input file name...it must be there!
	    if ( S->inputFilename[0] == 0 ) {
        	diagnostic( ERROR, "Machines specification failed - no input file or machines file specified" );
        	monitor->sendPacket( MACHINE_DEF_FAILURE_NO_FILES_SPECIFIED, NULL, 0 );
            monitor->sendPacket( MACHINE_DEF_TASK_TERMINATED, NULL, 0 );
            delete monitor;
	        return;
	    }
	    //  Create a .machines file name using the input file name.
	    strncpy( threadsFilename, S->inputFilename, DIFX_MESSAGE_FILENAME_LENGTH );
	    l = strlen( threadsFilename );
	    for( int i = l-1; i > 0; i-- ) {
		    if( threadsFilename[i] == '.' ) {
			    threadsFilename[i] = 0;
			    break;
		    }
	    }
	    strncat( threadsFilename, ".threads", DIFX_MESSAGE_FILENAME_LENGTH );
	}
	else
	    strncpy( threadsFilename, S->threadsFilename, DIFX_MESSAGE_FILENAME_LENGTH );
	monitor->sendPacket( MACHINE_DEF_THREADS_FILE_NAME, threadsFilename, strlen( threadsFilename ) );

    monitor->sendPacket( MACHINE_DEF_PARAMETER_CHECK_SUCCESS, NULL, 0 );

    //  Make (flexible) lists of each processor and data source we are using.
    std::list<std::string> dataNodes;
    std::list<std::string> processNodes;
    std::list<int> processThreads;
    for ( int i = 0; i < S->nDatastream; ++i ) {
        dataNodes.push_back( std::string( S->datastreamNode[i] ) );
    }
    for ( int i = 0; i < S->nProcess; ++i ) {
        processNodes.push_back( std::string( S->processNode[i] ) );
        //  We need to save the number of threads associated with each processor as well.
        processThreads.push_back( S->nThread[i] );
    }
        
        //  Find the "working directory" - where the .input file resides and .threads and .machines
        //  files will be put.  This is also where data are written when the job is eventually run.
	    strcpy( workingDir, S->inputFilename );
        l = strlen( workingDir );
        for( int i = l-1; i > 0; i-- ) {
            if( workingDir[i] == '/') {
        		workingDir[i] = 0;
        		break;
        	}
        }

        //  Option to run a different version of mpirun.
	    if( S->mpiWrapper[0] )
	        mpiWrapper = S->mpiWrapper;
	    else
		    mpiWrapper = "mpirun";

        //  Test mpirun on each processor and data source.  We do this by creating an empty
        //  file with the machines name in the "working" directory using mpirun on each machine.
        //  This should test both write permission for the host and ssh permission required by
        //  mpirun (which is more likely to be a problem).  Maybe these tests can get more 
        //  elaborate - test processing time, etc, if we get adventurous.
        monitor->sendPacket( MACHINE_DEF_RUNNING_MPIRUN_TESTS, NULL, 0 );
        std::list<std::string>::iterator i;
        std::list<int>::iterator j = processThreads.begin();
        for ( i = processNodes.begin(); i != processNodes.end(); ) {
            printf( "%s %d\n", i->c_str(), *j );
            snprintf( nodename, MAX_COMMAND_SIZE, "%s", i->c_str() );
            //  Create a test file name.
            snprintf( testPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/test_%s", workingDir, nodename );
            //  Execute an mpirun command to create a new file using each processor.
            snprintf( command, MAX_COMMAND_SIZE, "%s -host %s %s touch %s &", 
                mpiWrapper,
                nodename,
                _difxSetupPath,
                testPath );
            int ret = system( command );
            //  We give the remote system a generous one second to complete this task.
            sleep( 1 );
            //  Now, try to delete the test file.  If it exists, the delete should return
            //  without an error.  If there IS an error, our assumption is that the mpirun did not
            //  work the way we expected.
            ret = unlink( testPath );
            if ( ret == -1 ) {
    	        monitor->sendPacket( MACHINE_DEF_FAILURE_MPIRUN, nodename, 
    	            strlen( nodename ) );
    	        //  Eliminate this node if we are supposed to be doing so...
    	        if ( S->testProcessors ) {
    	            i = processNodes.erase( i );
    	            j = processThreads.erase( j );
    	        }
    	        else {
    	            ++i;
    	            ++j;
    	        }
            }
            else {
    	        monitor->sendPacket( MACHINE_DEF_SUCCESS_MPIRUN, nodename, 
    	            strlen( nodename ) );
    	        ++i;
    	        ++j;
            }
        }
        
        //  Enough checking - write the machines file.
        out = fopen( machinesFilename, "w" );
        if ( !out ) {
	        diagnostic( ERROR, "Cannot open machines file \"%s\" for writing", machinesFilename );
	        monitor->sendPacket( MACHINE_DEF_FAILURE_OPEN_MACHINES_FILE, strerror( errno ), 
	            strlen( strerror( errno ) ) );
            monitor->sendPacket( MACHINE_DEF_TASK_TERMINATED, NULL, 0 );
            delete monitor;
        }
        //  The "head" or "manager" node is always first.
        fprintf(out, "%s\n", S->headNode);
        //  Then the data source machines.
        for( int i = 0; i < S->nDatastream; ++i )
	        fprintf( out, "%s\n", S->datastreamNode[i] );
        //  Finally, the processing machines.  Only include those which include at least
        //  one processing thread - for the head node this must be an "extra" thread.
        j = processThreads.begin();
        for ( i = processNodes.begin(); i != processNodes.end(); ++i ) {
            if ( !strcmp( i->c_str(), S->headNode ) ) {
                if ( *j > 1 )
                    fprintf( out, "%s\n", i->c_str() );
            }
            else {
                if ( *j > 0 )
	                fprintf( out, "%s\n", i->c_str() );
	        }
            ++j;
        }
        fclose( out );
        monitor->sendPacket( MACHINE_DEF_MACHINES_FILE_CREATED, NULL, 0 );

        //  Write the threads file.
        out = fopen( threadsFilename, "w" );
        if( !out ) {
            diagnostic( ERROR, "Cannot open threads file \"%s\" for writing", threadsFilename );
	        snprintf( message, DIFX_MESSAGE_LENGTH, "%s - %s", threadsFilename, strerror( errno ) );
	        monitor->sendPacket( MACHINE_DEF_FAILURE_OPEN_THREADS_FILE, strerror( errno ), 
	            strlen( strerror( errno ) ) );
            monitor->sendPacket( MACHINE_DEF_TASK_TERMINATED, NULL, 0 );
            delete monitor;
        }
        fprintf(out, "NUMBER OF CORES:    %d\n", (int)processNodes.size() );
        //  Tally the total number of processing threads - make sure there is at
        //  least one!
        int threadCount = 0;
        j = processThreads.begin();
        for ( i = processNodes.begin(); i != processNodes.end(); ++i ) {
            //  The head node reserves one thread for its management duties.
            if ( !strcmp( i->c_str(), S->headNode ) ) {
                if ( *j > 1 ) {
                    fprintf( out, "%d\n", *j - 1 );
                    threadCount += *j - 1;
                }
            }
	        else {
                if ( *j > 0 ) {
                    fprintf( out, "%d\n", *j );
                    threadCount += *j;
                }
	        }
	        ++j;
        }
        fclose(out);
        
        //  A zero thread count is probably indicative of a problem.
        if ( threadCount < 1 )
            monitor->sendPacket( MACHINE_DEF_LOW_THREAD_COUNT, NULL, 0 );
            
        monitor->sendPacket( MACHINE_DEF_THREADS_FILE_CREATED, NULL, 0 );
	
        //  Healthy process end...
        monitor->sendPacket( MACHINE_DEF_TASK_ENDED_GRACEFULLY, NULL, 0 );
        delete monitor;
//        delete guiSocket;
		
}


