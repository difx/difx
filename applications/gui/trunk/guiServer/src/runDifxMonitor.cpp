//=============================================================================
//
//   ServerSideConnection::difxMonitor Function (and associated functions)
//
//!  This is a server for GUI connections that want real-time monitoring of
//!  running DiFX jobs.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <network/TCPClient.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ExecuteSystem.h>
#include <configuration.h>
#include <network/TCPServer.h>
#include <network/TCPSocket.h>
#include <DifxMonitorExchange.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Thread to monitor a running DiFX job.  A connection is made to the
//!  "monitor_server" program and data products are requested.  These products
//!  are then provided to the GUI client.
//-----------------------------------------------------------------------------	
void ServerSideConnection::runDifxMonitor( DifxMonitorInfo* monitorInfo ) {
	char message[DIFX_MESSAGE_LENGTH];
    Configuration* config;
    bool allIsWell = true;
    DifxMonitorExchange* exchange = NULL;
    
    //  Start a server on the connection port and await a connection from the GUI.  
    printf( "using monitor port #%d\n", monitorInfo->connectionPort );
    network::TCPServer* server = new network::TCPServer( monitorInfo->connectionPort );
    network::TCPSocket* monitorClient = server->acceptClient();
    if ( monitorClient == NULL ) {
        allIsWell = false;
        diagnostic( ERROR, "acceptClient() failure in runDifxMonitor - real-time monitoring will not run" );
    }
    //  Once we have a connection we don't need the server anymore.
    delete server;

    //  We have a packet exchange mechanism to govern the connection to the GUI.
    if ( allIsWell )
        exchange = new DifxMonitorExchange( monitorClient, monitorInfo );

    while ( exchange->keepGoing() ) {

        //printf( "tick\n" );
        usleep( 1000000 );
        
    }
    
//    delete config;
    printf( "closing the monitor socket\n" );
//    monitorServerClient->closeConnection();
//    delete monitorServerClient;
    monitorClient->closeConnection();
    delete monitorClient;
    printf( "deleting packet exchange\n" );
    sleep( 1 );
    delete exchange;
    printf( "exiting monitor thread\n" );
    
}


