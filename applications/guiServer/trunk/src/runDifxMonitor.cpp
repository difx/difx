//=============================================================================
//
//   ServerSideConnection::difxMonitor Function (and associated functions)
//
//!  This is a server for GUI connections that want real-time monitoring of
//!  running DiFX jobs.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <GUIClient.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ExecuteSystem.h>
#include <configuration.h>
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
    printf( "using monitor port #%d - gui address is %s\n", monitorInfo->connectionPort, monitorInfo->addr.c_str() );
    GUIClient* client = new GUIClient( monitorInfo->ssc, monitorInfo->addr.c_str(), monitorInfo->connectionPort );

    //  We have a packet exchange mechanism to govern the connection to the GUI.
    if ( client->okay() ) {
        exchange = new DifxMonitorExchange( client, monitorInfo );

        //  One-second cycle to monitor the exchange with the GUI.  Once it is
        //  finished we will shut it down and delete it.
        while ( exchange->keepGoing() ) {

            //printf( "tick\n" );
            usleep( 1000000 );
            
        }
    
    }

    printf( "closing the monitor socket\n" );
    exchange->closeConnection();
    printf( "deleting packet exchange\n" );
    sleep( 1 );
    delete client;
    delete exchange;
    printf( "exiting monitor thread\n" );
    
}


