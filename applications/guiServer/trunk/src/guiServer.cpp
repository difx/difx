//=============================================================================
//
//   guiServer (main)
//
//!  Collect and monitor connections from clients (presumably the GUI), as
//!  well as monitor DiFX multicast traffic.  The ServerSideConnection class
//!  handles all of the specifics of each client connection (such as what to
//!  do with multicast messages).
//
//=============================================================================
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <vector>
#include <pthread.h>
#include <ExecuteSystem.h>
#include <network/TCPServer.h>
#include <ServerSideConnection.h>
#include <difxmessage.h>

//  Structure for holding information about client connections.
struct ClientConnection {
    char IP[16];
    guiServer::ServerSideConnection* client;
    network::TCPSocket* clientSocket;
};

//  This vector holds a list of client connections that are active.
std::vector<ClientConnection*> _clientConnections;

//  This is a mutex to lock the list of client connections (since two threads mess
//  around with it).
pthread_mutex_t _clientConnectionsMutex;

//-----------------------------------------------------------------------------
//!  Thread to monitor the list of client connections.
//-----------------------------------------------------------------------------
void* connectionMonitor( void* arg ) {
    while ( 1 ) {
        //  Examine the list of client connections to make sure all are still
        //  connected.
        pthread_mutex_lock( &_clientConnectionsMutex );
        for ( std::vector<ClientConnection*>::iterator it = _clientConnections.begin();
              it != _clientConnections.end(); ) {
            ClientConnection* thisConnection = (ClientConnection*)*it;
            //  Remove the connection from the list if it has been broken.
            if ( !thisConnection->clientSocket->connected() ) {
                printf( "%s disconnected\n", thisConnection->IP );
                it = _clientConnections.erase( it );
                delete thisConnection->clientSocket;
                delete thisConnection->client;
                delete thisConnection;
            }
            else
                ++it;
        }
        pthread_mutex_unlock( &_clientConnectionsMutex );
        sleep( 1 );
    }
    return NULL;
}

//-----------------------------------------------------------------------------
//!  main
//-----------------------------------------------------------------------------
int main( int argc, char **argv, char **envp ) {

    char difxBase[DIFX_MESSAGE_LENGTH];
    char command[guiServer::ServerSideConnection::MAX_COMMAND_SIZE];
    char message[DIFX_MESSAGE_LENGTH];
    const char* user;

    //  Configurables, with some sensible defaults.
    int serverPort = 50200;            //  Port for TCP connections
    int multicastPort = 50200;         //  Port for receiving DiFX multicasts
    char multicastGroup[16];           //  Group ID for DIFX multicasts.
    snprintf( multicastGroup, 16, "224.2.2.1" );
    
    //  Check values from environment variables.  The default port is used both
    //  for TCP connections and for the multicast network.  These don't need to
    //  stay the same!
    char* newPort = getenv( "DIFX_MESSAGE_PORT" );
    if ( newPort != NULL ) {
        serverPort = atoi( newPort );
        multicastPort = atoi( newPort );
    }
    //  Message group for multicasts.
    if ( getenv( "DIFX_MESSAGE_GROUP" ) != NULL )
        snprintf( multicastGroup, 16, "%s", getenv( "DIFX_MESSAGE_GROUP" ) );
        
    //  The DiFX "base" is where the setup files (used to set up DiFX processes)
    //  for different versions are located.  It must either be defined using an
    //  environment variable or on the command line, or guiServer quits.
    difxBase[0] = 0;
    if ( getenv( "DIFX_BASE" ) != NULL )
        strncpy( difxBase, getenv( "DIFX_BASE" ), DIFX_MESSAGE_LENGTH );
    
    //  Command line arguments -- error checking should perhaps be more thorough here.
    for ( int i = 1; i < argc; ++i ) {
        //  Command line args are null terminated so I can use strcmp...
        if ( !strcmp( argv[i], "-help" ) || !strcmp( argv[i], "-h" ) ) {
        }
        else if ( !strcmp( argv[i], "--multicastGroup" ) || !strcmp( argv[i], "-mg" ) ) {
            snprintf( multicastGroup, 16, "%s", argv[i+1] );
            ++i;
        }
        else if ( !strcmp( argv[i], "--multicastPort" ) || !strcmp( argv[i], "-mp" ) ) {
            multicastPort = atoi( argv[i+1] );
            ++i;
        }
        else if ( !strcmp( argv[i], "--base" ) || !strcmp( argv[i], "-b" ) ) {
            strncpy( difxBase, argv[i+1], DIFX_MESSAGE_LENGTH );
            ++i;
        }
        else {
            //  Try to interpret the final argument as the server port number
            bool allNum = true;
            char* newNum = argv[i];
            for ( int j = 0; j < strlen( newNum ); ++j ) {
                if ( !isdigit( newNum[j] ) )
                    allNum = false;
            }
            if ( allNum )
                serverPort = atoi( newNum );
        }
    }
    
    //  Bail out if we don't have a defined DiFX base.
    if ( difxBase[0] == 0 ) {
        fprintf( stderr, "No DiFX base was defined (either command line or environment variable).\n" );
        fprintf( stderr, "guiServer is terminating!\n" );
        exit( EXIT_FAILURE );
    }
    
    //  The TCP server for all connections.
    network::TCPServer* server = new network::TCPServer( serverPort );
    if ( !server->serverUp() )
        exit( EXIT_FAILURE );
    printf( "server at port %d\n", serverPort );
    printf( "DiFX base is %s\n", difxBase );
    
    //  Initialize the list of client connections.
    _clientConnections.clear();
    pthread_mutex_init( &_clientConnectionsMutex, NULL );
    
    //  Start the thread that monitors client connections.
    pthread_attr_t _monitorAttr;
    pthread_t _monitorId;
    pthread_attr_init( &_monitorAttr );
    pthread_create( &_monitorId, &_monitorAttr, connectionMonitor, NULL );               
    
    //  Enter an endless loop looking for client connections at the server port.
    while ( 1 ) {

        //  Wait for the next client to connect and spawn a new socket to cover it.
        printf( "guiServer: wait for new client connection\n" );
        network::TCPSocket* newClientSocket = server->acceptClient();
        printf( "guiServer: client connection from address %s\n", server->lastClientIP() );

        //  Open a packet exchange mechanism to deal with this connection as a server.
        guiServer::ServerSideConnection* newClient = new guiServer::ServerSideConnection( newClientSocket, server->lastClientIP(), difxBase, envp );
        
        //  Set the (default) multicast information for this client to match our defaults.
        newClient->multicast( multicastGroup, multicastPort );
        
        //  Save information about the new client.
        ClientConnection* newClientConnection = new ClientConnection;
        newClientConnection->clientSocket = newClientSocket;
        newClientConnection->client = newClient;
        sprintf( newClientConnection->IP, "%s", server->lastClientIP() );
        
        //  Put the information in our list of clients, which is mutex locked.
        pthread_mutex_lock( &_clientConnectionsMutex );
        _clientConnections.push_back( newClientConnection );
        pthread_mutex_unlock( &_clientConnectionsMutex );

    }
    
    delete server;
    
    printf( "guiServer: exited endless loop??\n" );

}
