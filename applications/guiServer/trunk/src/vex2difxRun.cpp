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
#include <string.h>
#include <pwd.h>
#include <fcntl.h>
#include <dirent.h>
#include <network/TCPClient.h>
#include <signal.h>
#include <GUIClient.h>
#include <set>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a request to run "vex2difx".  Work is done in a
//!  thread.
//-----------------------------------------------------------------------------
void ServerSideConnection::vex2difxRun( DifxMessageGeneric* G ) {
    Vex2DifxInfo* vex2DifxInfo = new Vex2DifxInfo;
    vex2DifxInfo->ssc = this;
    memcpy( &(vex2DifxInfo->v2dRun), &(G->body.vex2DifxRun), sizeof( DifxMessageVex2DifxRun ) );
//    pthread_attr_init( &(vex2DifxInfo->threadAttr) );
    pthread_create( &(vex2DifxInfo->threadId), NULL, //&(vex2DifxInfo->threadAttr), 
                    staticRunVex2Difx, (void*)vex2DifxInfo );      
}

//----------------------------------------------------------------------------
//!  The thread that runs the vex2difx command (as well as calcif2).  An
//!  additional thread is started to monitor and report the results.  These
//!  share a client connection with the GUI.
//----------------------------------------------------------------------------
void ServerSideConnection::runVex2Difx( Vex2DifxInfo* vex2DifxInfo ) {
	const DifxMessageVex2DifxRun *S = &(vex2DifxInfo->v2dRun);
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
    ExecuteSystem* executor;
    bool someError = false;
		    
	//  Generate a temporary garbage file that will give us a time stamp for when this
    //  function call was made - this is used to determine which files have been
    //  created by the activities that follow in this function.
    ulong modTime = fileTimeStamp( someError, S->passPath );
		
	GUIClient* guiClient;

	//  Open a client connection to the server that should be running for us on the
    //  host that requested this task (the GUI, presumably).  This connection is used
    //  to communicate which .input files have been created.
    guiClient = new GUIClient( vex2DifxInfo->ssc, S->address, S->port );
    guiClient->packetExchange();
    
    //  Start the monitoring thread.  This will report results of the commands below
    //  as they appear.
    Vex2DifxMonitorInfo* monitorInfo = new Vex2DifxMonitorInfo;
    monitorInfo->ssc = vex2DifxInfo->ssc;
    monitorInfo->S = S;
    monitorInfo->guiClient = guiClient;
    monitorInfo->modTime = modTime;
    pthread_mutex_t monitorMutex;
    pthread_mutex_init( &monitorMutex, NULL );
    pthread_cond_t done;
    monitorInfo->done = &done;
    pthread_cond_init( &done, NULL );
    pthread_create( &(monitorInfo->threadId), NULL, 
                    staticRunVex2DifxMonitor, (void*)monitorInfo ); 
        
	//  This is where we actually run vex2difx - this creates the input files.
	snprintf( command, MAX_COMMAND_SIZE, "cd %s; %s vex2difx -f %s", 
			  S->passPath,
			  _difxSetupPath,
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
        diagnostic( ERROR, "vex2difx RETURNED ERRORS" );
    delete executor;

	//  Next thing to run - calcif2.
	snprintf( command, MAX_COMMAND_SIZE, "cd %s; %s calcif2 -f -a", 
			  S->passPath,
			  _difxSetupPath );
	
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
        diagnostic( ERROR, "calcif2 RETURNED ERRORS" );
    delete executor;
    
    //  Wait here for the monitoring thread to die, then delete resources.
    pthread_mutex_lock( &monitorMutex );
    pthread_cond_wait( &done, &monitorMutex );
    pthread_mutex_unlock( &monitorMutex );
    pthread_cond_destroy( &done );
    pthread_mutex_destroy( &monitorMutex );
    delete guiClient;
    
	//difxMessageSendDifxAlert("vex2difx completed", DIFX_ALERT_LEVEL_INFO);
}

//-----------------------------------------------------------------------------
//!  This thread monitors the creation of products of the vex2difx and calcif2
//!  processes.  It is a continuously-looping but self-destoying thread, looking
//!  for changes but giving up after a certain amount of time and exiting.
//-----------------------------------------------------------------------------
void ServerSideConnection::runVex2DifxMonitor( Vex2DifxMonitorInfo* monitorInfo ) {
	char message[DIFX_MESSAGE_LENGTH];
	static const int INITIAL_WAIT = 180;
	static const int UPDATE_WAIT = 20;
	int waitTime = INITIAL_WAIT;
	std::set<std::string> knownList;
	bool gotItem = false;

    if ( monitorInfo->guiClient->okay() ) {
    
        //  This process repeats forever unless it hits some time limits during which there are
        //  no new files.  There is an INITIAL_WAIT and an UPDATE_WAIT, both measured in seconds.
        //  The INITIAL_WAIT is how long the thread will wait for the first file to be created
        //  before it gives up and bails out.  The UPDATE_WAIT is how long it will wait
        //  for each subsequent file to be created before it bails out.  There is a one second
        //  interval between each check for new files, which should be fast enough that the user
        //  will be satisfied with visible progress.
        while ( waitTime > 0 ) {
        
    	    //  Produce a list of the files in the target directory.  These are filtered using the
    	    //  function vex2DifxFilter(), which, annoyingly, is static and stored in ServerSideConnection.h.
    	    //  It limits us to .input and .im files, the previous telling us when a job has been created,
    	    //  the latter when calcif2 has been successfully run on it.
            struct dirent **namelist;
            int n = scandir( monitorInfo->S->passPath, &namelist, vex2DifxFilter, alphasort );
            int sentFiles = 0;
            if ( n < 0 ) {
                snprintf( message, DIFX_MESSAGE_LENGTH, "%s", strerror( errno ) );
                difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            } else {
                while ( n-- ) {
                	char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
                	snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", monitorInfo->S->passPath, namelist[n]->d_name );
                	struct stat buf;
                	stat( fullPath, &buf );
                	//  Compare the last modification time of each file in the pass directory
                	//  with the "gettimeofday" result at the top of this function.  This *should*
                	//  limit us to the files created since we started this process - and eliminate
                	//  files in this directory that were created earlier (unless someone is insidiously
                	//  running vex2difx during the same second).
                	if ( (ulong)buf.st_mtime >= monitorInfo->modTime ) {
                	    //  See if this is a "new" file name - not in the list of files we have
                	    //  already encountered in a previous loop.
                	    if ( knownList.find( std::string( fullPath ) ) == knownList.end() ) {
                    	    //  Send the full path name of this file.
                    	    //  Each separate file is preceded by its string length.
                    	    int sz = htonl( strlen( fullPath ) );
                    	    monitorInfo->guiClient->writer( &sz, sizeof( int ) );
                    	    monitorInfo->guiClient->writer( fullPath, strlen( fullPath ) );
                    	    //  Add this file to the "known" list so we don't repeat it.
                    	    knownList.insert( std::string( fullPath ) );
                    	    waitTime = UPDATE_WAIT;
                    	    sentFiles += 1;
                	    }
                	}
                    free( namelist[n] );
                    gotItem = true;
                }
                free( namelist );
            }
            //  This is a dummy file name that is used to indicate that we think there are no more
            //  files to send.
            if ( gotItem && sentFiles == 0 ) {
                int sz = htonl( strlen( "a.dummyfile" ) );
                monitorInfo->guiClient->writer( &sz, sizeof( int ) );
                monitorInfo->guiClient->writer( "a.dummyfile", strlen( "a.dummyfile" ) );
            }
            --waitTime;
            sleep( 1 );
        }
	} 
    //  Sending a zero length tells the GUI that the list is finished.
    int zero = 0;
    monitorInfo->guiClient->writer( &zero, sizeof( int ) );
	knownList.clear();
	//  Let the main thread know we are done, so it can clean up resources.
    pthread_cond_broadcast( monitorInfo->done );
}

//-----------------------------------------------------------------------------
//!  This function is used to generate a time stamp that can be compared to
//!  the creation time for files.  It does this by creating a garbage file,
//!  looking at when it was created, then deleting it.  The time stamp will
//!  be created in a specified directory location, or in /tmp if no location
//!  is specified.
//-----------------------------------------------------------------------------
ulong ServerSideConnection::fileTimeStamp( bool& someError, const char* path ) {
    ExecuteSystem* executor;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
		    
    //  Generate a temporary garbage file that will give us a time stamp for when this
    //  function call was made - this is used below to determine which files have been
    //  created by the activities that follow in this function.  This used to be done with
    //  gettimeofday() but this was found to not necessarily work if the node running this
    //  function remote mounted the disk and did not coordinate its system time with the
    //  node that owned the disk.
    ulong modTime = 0;
    if ( path != NULL )
        snprintf( command, MAX_COMMAND_SIZE, "cd %s; /bin/touch garbage_timestamp_file_please_delete", path );
    else
        snprintf( command, MAX_COMMAND_SIZE, "/bin/touch /tmp/garbage_timestamp_file_please_delete" );
    executor = new ExecuteSystem( command );
    someError = false;
    while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
        //  An error returned by this is worrisome (probably means we don't have write permission,
        //  or the disk is full, or something like that).
        if ( ret != 1 )  {
            diagnostic( ERROR, "Error returned creating timing file: %s", message );
            someError = true;
        }
    }
    delete executor;
    //  Get the modification time for the garbage file.
    if ( !someError ) {
        struct dirent **namelist;
        int n = 0;
        if ( path != NULL )
            n = scandir( path, &namelist, 0, alphasort );
        else 
            n = scandir( "/tmp", &namelist, 0, alphasort );
        if ( n > 0 ) {
            while ( n-- ) {
                if ( !strcmp( namelist[n]->d_name, "garbage_timestamp_file_please_delete" ) ) {
            	    char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
            	    if ( path != NULL )
            	        snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", path, namelist[n]->d_name );
            	    else
            	        snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "/tmp/%s", namelist[n]->d_name );
            	    struct stat buf;
            	    stat( fullPath, &buf );
            	    modTime = buf.st_mtime;
            	}
                free( namelist[n] );
            }
            free( namelist );
        }
    }
    else {
        //  This probably won't save us!  
	    struct timeval tv;
	    gettimeofday( &tv, NULL );
	    modTime = tv.tv_sec;
	}
	//  Try to remove the garbage file we created.  Any errors probably indicate a wider problem.
	if ( path != NULL )
        snprintf( command, MAX_COMMAND_SIZE, "cd %s; /bin/rm garbage_timestamp_file_please_delete", path );
	else
        snprintf( command, MAX_COMMAND_SIZE, "/bin/rm /tmp/garbage_timestamp_file_please_delete" );
    executor = new ExecuteSystem( command );
    while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
        if ( ret != 1 )  {
            diagnostic( ERROR, "Error returned removing timing file: %s", message );
            someError = true;
        }
    }
    delete executor;
    
    return modTime;
    
}


