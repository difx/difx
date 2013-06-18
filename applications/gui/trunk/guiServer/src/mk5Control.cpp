//=============================================================================
//
//   ServerSideConnection::mk5Control Function (and associated functions)
//
//!  Called when a "mk5control" command is received from the GUI.  The command
//!  is executed and results are sent back to the GUI.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <sys/statvfs.h>
#include <network/TCPClient.h>
#include <Mk5ControlConnection.h>
#include <ExecuteSystem.h>
#include <list>
#include <string>
#include <signal.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Execute a mk5control command in a forked process.  A TCP connection allows
//!  two-way communication with the client that requested the mk5control command.
//-----------------------------------------------------------------------------
void ServerSideConnection::mk5Control( DifxMessageGeneric* G ) {
    int childPid;
    char command[MAX_COMMAND_SIZE];
    char targetNode[MAX_COMMAND_SIZE];
    const DifxMessageMk5Control *S;
    Mk5ControlConnection* monitor;

    //  Cast the body of this message to a DifxMessageStart structure.
    S = &G->body.mk5Control;

    printf( "This is an mk5 control operation....\n" );
    printf( "The command is \"%s\"\n", S->command );
    printf( "It is directed at %s\n", S->targetNode );

    //  Open a TCP socket connection to the server that should be running for us on the
    //  host that requested this task (the GUI, presumably).  This socket is used
    //  for diagnostic messages, data, and to show progress.
    network::TCPClient* guiSocket = new network::TCPClient( S->address, S->port );
    guiSocket->waitForConnect();
    //  Create a Mk5ControlConnection out of this new socket if it connected
    //  properly.  If it did not connect, we need to bail out now.
    if ( guiSocket->connected() ) {
        monitor = new Mk5ControlConnection( guiSocket );
        monitor->sendPacket( Mk5ControlConnection::TASK_STARTED, NULL, 0 );
    }
    else {
        diagnostic( ERROR, "client socket connection from guiServer to GUI failed - unable to execute mk5control command \"%s\"", S->command );
        delete guiSocket;
        return;
    }

    //  Fork a process to run the mk5control command and monitor the results.
    signal( SIGCHLD, SIG_IGN );
    childPid = fork();
    if( childPid == 0 ) {    
	char message[DIFX_MESSAGE_LENGTH];
        char mk5ControlCommand[ServerSideConnection::MAX_COMMAND_SIZE];
        snprintf( mk5ControlCommand, ServerSideConnection::MAX_COMMAND_SIZE, "%s; mk5control %s %s",
                _difxSetupPath, S->command, S->targetNode );
        printf( "COMMAND: \"%s\"\n", mk5ControlCommand );
        ExecuteSystem* executor = new ExecuteSystem( mk5ControlCommand );
        bool found = false;
        if ( executor->pid() > -1 ) {
            while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                if ( ret == 1 ) { // stdout
                    int portNum = -1;
                    int len = strlen( message );
                    if ( len >= 0 ) {
                        monitor->formatPacket( INFORMATION, "%s\n", message );
                    }
                }
                else { // error of some sort
                    int len = strlen( message );
                    if ( len >= 0 ) {
                        monitor->formatPacket( ERROR, "%s\n", message );
                    }
                }
            }
        }

            //        //  Make (flexible) lists of each processor and data source we are using.
//        std::list<std::string> dataNodes;
//        std::list<std::string> processNodes;
//        std::list<int> processThreads;
//        for ( int i = 0; i < S->nDatastream; ++i ) {
//            bool pass = true;
//            if ( pass ) {
//                dataNodes.push_back( std::string( S->datastreamNode[i] ) );
//            }
//        }
//        for ( int i = 0; i < S->nProcess; ++i ) {
//            bool pass = true;
//            if ( pass ) {
//                processNodes.push_back( std::string( S->processNode[i] ) );
//                //  We need to save the number of threads associated with each processor as well.
//                processThreads.push_back( S->nThread[i] );
//            }
//        }
//        
//        //  Find the "working directory" - where the .input file resides and .threads and .machines
//        //  files will be put.  This is also where data are written when the job is eventually run.
//	    strcpy( workingDir, S->inputFilename );
//        l = strlen( workingDir );
//        for( int i = l-1; i > 0; i-- ) {
//            if( workingDir[i] == '/') {
//        		workingDir[i] = 0;
//        		break;
//        	}
//        }
//
//        //  Option to run a different version of mpirun.
//	    if( S->mpiWrapper[0] )
//	        mpiWrapper = S->mpiWrapper;
//	    else
//		    mpiWrapper = "mpirun";
//
//        //  Test mpirun on each processor and data source.  We do this by creating an empty
//        //  file with the machines name in the "working" directory using mpirun on each machine.
//        //  This should test both write permission for the host and ssh permission required by
//        //  mpirun (which is more likely to be a problem).  Maybe these tests can get more 
//        //  elaborate - test processing time, etc, if we get adventurous.
//        monitor->sendPacket( MachinesMonitorConnection::RUNNING_MPIRUN_TESTS, NULL, 0 );
//        std::list<std::string>::iterator i;
//        std::list<int>::iterator j = processThreads.begin();
//        for ( i = processNodes.begin(); i != processNodes.end(); ) {
//            printf( "%s %d\n", i->c_str(), *j );
//            snprintf( nodename, MAX_COMMAND_SIZE, "%s", i->c_str() );
//            //  Create a test file name.
//            snprintf( testPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/test_%s", workingDir, nodename );
//            //  Execute an mpirun command to create a new file using each processor.
//            snprintf( command, MAX_COMMAND_SIZE, "source %s; %s -host %s touch %s &", 
//                _difxSetupPath,
//                mpiWrapper,
//                nodename,
//                testPath );
//            int ret = system( command );
//            //  We give the remote system a generous one second to complete this task.
//            sleep( 1 );
//            //  Now, try to delete the test file.  If it exists, the delete should return
//            //  without an error.  If there IS an error, our assumption is that the mpirun did not
//            //  work the way we expected.
//            ret = unlink( testPath );
//            if ( ret == -1 ) {
//    	        monitor->sendPacket( MachinesMonitorConnection::FAILURE_MPIRUN, nodename, 
//    	            strlen( nodename ) );
//    	        //  Eliminate this node if we are supposed to be doing so...
//    	        if ( S->testProcessors ) {
//    	            i = processNodes.erase( i );
//    	            j = processThreads.erase( j );
//    	        }
//    	        else {
//    	            ++i;
//    	            ++j;
//    	        }
//            }
//            else {
//    	        monitor->sendPacket( MachinesMonitorConnection::SUCCESS_MPIRUN, nodename, 
//    	            strlen( nodename ) );
//    	        ++i;
//    	        ++j;
//            }
//        }
//        
//        //  Enough checking - write the machines file.
//        out = fopen( machinesFilename, "w" );
//        if ( !out ) {
//	        diagnostic( ERROR, "Cannot open machines file \"%s\" for writing", machinesFilename );
//	        monitor->sendPacket( MachinesMonitorConnection::FAILURE_OPEN_MACHINES_FILE, strerror( errno ), 
//	            strlen( strerror( errno ) ) );
//            monitor->sendPacket( MachinesMonitorConnection::TASK_TERMINATED, NULL, 0 );
//            delete monitor;
//            delete guiSocket;
//	        exit( EXIT_SUCCESS );
//        }
//        //  The "head" or "manager" node is always first.
//        fprintf(out, "%s\n", S->headNode);
//        //  Then the data source machines.
//        for( int i = 0; i < S->nDatastream; ++i )
//	        fprintf( out, "%s\n", S->datastreamNode[i] );
//        //  Finally, the processing machines.  Only include those which include at least
//        //  one processing thread - for the head node this must be an "extra" thread.
//        j = processThreads.begin();
//        for ( i = processNodes.begin(); i != processNodes.end(); ++i ) {
//            if ( !strcmp( i->c_str(), S->headNode ) ) {
//                if ( *j > 1 )
//                    fprintf( out, "%s\n", i->c_str() );
//            }
//            else {
//                if ( *j > 0 )
//	                fprintf( out, "%s\n", i->c_str() );
//	        }
//            ++j;
//        }
//        fclose( out );
//        monitor->sendPacket( MachinesMonitorConnection::MACHINES_FILE_CREATED, NULL, 0 );
//
//        //  Write the threads file.
//        out = fopen( threadsFilename, "w" );
//        if( !out ) {
//            diagnostic( ERROR, "Cannot open threads file \"%s\" for writing", threadsFilename );
//	        snprintf( message, DIFX_MESSAGE_LENGTH, "%s - %s", threadsFilename, strerror( errno ) );
//	        monitor->sendPacket( MachinesMonitorConnection::FAILURE_OPEN_THREADS_FILE, strerror( errno ), 
//	            strlen( strerror( errno ) ) );
//            monitor->sendPacket( MachinesMonitorConnection::TASK_TERMINATED, NULL, 0 );
//            delete monitor;
//            delete guiSocket;
//	        exit( EXIT_SUCCESS );
//        }
//        fprintf(out, "NUMBER OF CORES:    %d\n", processNodes.size() );
//        //  Tally the total number of processing threads - make sure there is at
//        //  least one!
//        int threadCount = 0;
//        j = processThreads.begin();
//        for ( i = processNodes.begin(); i != processNodes.end(); ++i ) {
//            //  The head node reserves one thread for its management duties.
//            if ( !strcmp( i->c_str(), S->headNode ) ) {
//                if ( *j > 1 ) {
//                    fprintf( out, "%d\n", *j - 1 );
//                    threadCount += *j - 1;
//                }
//            }
//	        else {
//                if ( *j > 0 ) {
//                    fprintf( out, "%d\n", *j );
//                    threadCount += *j;
//                }
//	        }
//	        ++j;
//        }
//        fclose(out);
//        
//        //  A zero thread count is probably indicative of a problem.
//        if ( threadCount < 1 )
//            monitor->sendPacket( MachinesMonitorConnection::LOW_THREAD_COUNT, NULL, 0 );
//            
//        monitor->sendPacket( MachinesMonitorConnection::THREADS_FILE_CREATED, NULL, 0 );
	
        //  Healthy process end...
        monitor->sendPacket( Mk5ControlConnection::TASK_ENDED_GRACEFULLY, NULL, 0 );
        delete monitor;
        delete guiSocket;
        exit(EXIT_SUCCESS);		

    }
		
}


