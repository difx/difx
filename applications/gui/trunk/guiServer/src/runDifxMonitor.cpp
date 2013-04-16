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

/*
    //  See if the "monitor_server" program is running (with the proper port).  If not,
    //  start it up.
    int monitorServerPort = 9999;
    char monCommand[ServerSideConnection::MAX_COMMAND_SIZE];
    snprintf( monCommand, ServerSideConnection::MAX_COMMAND_SIZE, 
        "/bin/ps -ef | /bin/grep -E \'monitor_server\\s|monitor_server$\' | /bin/grep -v grep" );
    ExecuteSystem* executor = new ExecuteSystem( monCommand );
    bool found = false;
    if ( executor->pid() > -1 ) {
        while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
            if ( ret == 1 ) { // stdout
                //  Any instance of monitor_server should show up as a result of this
                //  "ps" command.  Monitor_server is started with the port number as
                //  the last (and only) argument.
                int portNum = -1;
                int len = strlen( message );
                while ( len >= 0 ) {
                    if ( message[len] == ' ' ) {
                        portNum = atoi( message + len + 1 );
                        break;
                    }
                    --len;
                }
                if ( portNum == monitorServerPort ) {
                    exchange->formatPacket( DifxMonitorExchange::MESSAGE, "located monitor_server instance: %s\n", message );
                    found = true;
                }
            }
        }
    }
    //  Start the monitor_server if we have not found it.
    if ( !found ) {
        snprintf( monCommand, ServerSideConnection::MAX_COMMAND_SIZE,
             "source %s; monitor_server %d &> /dev/null &", monitorInfo->ssc->difxSetupPath(), monitorServerPort );
        exchange->formatPacket( DifxMonitorExchange::WARNING, "monitor_server needs to be started - executing: %s\n", monCommand );
        system( monCommand );
        sleep( 1 );
    }

    //  Make a new client connection to the monitor_server, which *should* now be running.
    //  At the moment the port is hard-wired to 52300 which might not be a perfect
    //  long term solution.  The host is the local host (so I'm using the loopback
    //  address).
    printf( "making client connection\n" );
    network::TCPClient* monitorServerClient = new network::TCPClient( "127.0.0.1", 52300 );
    monitorServerClient->waitForConnect();
    printf( "connection established!\n" );
    if ( monitorServerClient->connected() ) {
        int status;
        monitorServerClient->reader( (char*)(&status), sizeof( int ) );
        if ( status == 0 ) {
            exchange->formatPacket( DifxMonitorExchange::MESSAGE, "connection established with monitor_server" );
        }
        else {
            exchange->formatPacket( DifxMonitorExchange::ERROR, "monitor_server returned initial status failure (%d) - real time monitor will not run", status );
            allIsWell = false;
        }
    }
    else {
        exchange->formatPacket( DifxMonitorExchange::ERROR, "connection with monitor_server failed - real time monitor will not run" );
        allIsWell = false;
    }
        
    //  Generate a list of data products.  This code is basically swiped from Chris Phillips'
    //  difx_config program.
/*
    if ( allIsWell ) {
        config = new Configuration( startInfo->inputFile, 0 );
        if ( !config->consistencyOK() ) {
            snprintf( message, DIFX_MESSAGE_LENGTH, "Configuration had a problem parsing input file %s - real time monitor will not run",
                startInfo->inputFile );
            startInfo->jobMonitor->sendPacket( JobMonitorConnection::DIFX_MONITOR_CONNECTION_FAILED, NULL, 0 );
            startInfo->jobMonitor->sendPacket( JobMonitorConnection::DIFX_ERROR, message, strlen( message ) );
            allIsWell = false;
        }
        else {
            //  Send the possible data products and codes.  This I grabbed (mostly) from the monserver_productconfig()
            //  function in difx_monitor/monserver.cpp.  Without know entirely what I was doing.
            int currentscan = 0;  //  Is there ever more than one??
            int configindex = config->getScanConfigIndex( currentscan );
            char polpair[3];
            polpair[2] = 0;
            printf( "config index is %d\n", configindex );

            int binloop = 1;
            if ( config->pulsarBinOn( configindex ) && !config->scrunchOutputOn( configindex ) )
                binloop = config->getNumPulsarBins( configindex );
            printf( "binloop is %d\n", binloop );
            printf( "num baselines is %d\n", config->getNumBaselines() );

            for ( int i = 0; i < config->getNumBaselines(); i++ ) {
                int ds1index = config->getBDataStream1Index( configindex, i );
                int ds2index = config->getBDataStream2Index( configindex, i );
                printf( "telescopes...%s, %s\n", config->getTelescopeName( ds1index ).c_str(), config->getTelescopeName( ds2index ).c_str() );

                for( int j = 0; j < config->getBNumFreqs( configindex, i ); j++ ) {
                    int freqindex = config->getBFreqIndex( configindex, i, j );
                    int resultIndex = config->getCoreResultBaselineOffset( configindex, freqindex, i );
                    int freqchannels = config->getFNumChannels( freqindex ) / config->getFChannelsToAverage( freqindex );

                    for( int s = 0; s < config->getMaxPhaseCentres( configindex ); s++ ) {
	                    for( int b = 0; b < binloop; b++ ) {
	                        for( int k = 0; k < config->getBNumPolProducts( configindex, i, j ); k++ ) {
	                            config->getBPolPair( configindex, i, j, k, polpair );



	    products.push_back(DIFX_ProdConfig(ds1index,
					       ds2index,
					       config->getTelescopeName(ds1index), 
					       config->getTelescopeName(ds2index),
					       config->getFreqTableFreq(freqindex),
					       config->getFreqTableBandwidth(freqindex),
					       polpair,
					       s,
					       b,
					       resultindex,
					       freqchannels,
					       config->getFreqTableLowerSideband(freqindex)));
                    printf( "frequency....%.1f\n", config->getFreqTableFreq( freqindex ) );
                                printf( "result index is %d\n", resultIndex );
	                            resultIndex += freqchannels;
                            }
                        }
                    }
                }
            }
        }
            
    }
*/

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


