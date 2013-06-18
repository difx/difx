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
	
        //  Healthy process end...
        monitor->sendPacket( Mk5ControlConnection::TASK_ENDED_GRACEFULLY, NULL, 0 );
        delete monitor;
        delete guiSocket;
        exit(EXIT_SUCCESS);		

    }
		
}


