//=============================================================================
//
//   testClient
//
//!  This is a simple application used to test the guiServer.
//
//=============================================================================
#include <stdio.h>
#include <stdlib.h>
#include <network/TCPClient.h>
#include <ClientSideConnection.h>
#include <network/UDPSocket.h>

main( int argc, char **argv ) {

    char* endptr;
    int relay = 0;

    if ( argc < 3 ) {
        printf( "Usage: testClient address port\n" );
        exit( EXIT_FAILURE );
    }
    
    //  TCP Socket
    network::TCPClient* client = new network::TCPClient( argv[1], strtol( argv[2], &endptr, 10 ) );
    printf( "client started\n" );

    client->waitForConnect();

    printf( "client got connection\n" );
    guiServer::ClientSideConnection* packetXO = new guiServer::ClientSideConnection( client );

    //  Accept commands for as long as the user wants.
    char inLine[1024];
    bool keepGoing = true;
    while ( keepGoing ) {
        fprintf( stdout, "> " );
        fflush( stdout );
        fgets( inLine, 1024, stdin );
        if ( !strcmp( inLine, "help\n" ) || !strcmp( inLine, "?\n" ) ) {
            //  Print some help...
            printf( "This is a simple interface to the guiServer.  Your command options are:\n" );
            printf( "\n" );
            printf( "help         - print this help menu\n" );
            printf( "?            - print this help menu\n" );
            printf( "relay        - toggle broadcast relay on/off\n" );
            printf( "\n" );
            printf( "\n" );
        }
        else if ( !strcmp( inLine, "relay\n" ) ) {
            relay = relay? 0: 1;
            int yaler = htonl( relay );
            packetXO->sendPacket( guiServer::PacketExchange::RELAY_PACKET, (char*)&yaler, sizeof( int ) );
        }
        else if ( !strcmp( inLine, "exit\n" ) ) {
            keepGoing = false;
        }
    }
    
    delete client;

}
