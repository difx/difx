#include <stdio.h>
#include <stdlib.h>
#include <network/TCPServer.h>
#include <network/PacketExchange.h>
#include <network/UDPSocket.h>

main( int argc, char **argv ) {

    char* endptr;

    if ( argc < 2 ) {
        printf( "Usage: serverTest port\n" );
        exit( EXIT_FAILURE );
    }

    network::TCPServer* server = new network::TCPServer( strtol( argv[1], &endptr, 10 ) );
    printf( "server at port %d\n", strtol( argv[1], &endptr, 10 ) );
    
    int count = 5;
    
    while ( count ) {

        network::TCPSocket* newClient = server->acceptClient();
        printf( "client connection successful\n" );
        printf( "woohoo - got client connection from address %s\n", server->lastClientIP() );

        network::PacketExchange* packetXO = new network::PacketExchange( newClient );

        char junk[400];

        //  Send some data.
        for ( int i = 1; i <= 20; ++i ) {
            packetXO->sendPacket( i, junk, i * 20 );
        }

        //  Wait for client to close.....closing the server before the client gums up sockets.
        while ( newClient->connected() ) {
            printf( "waiting for client to close\n" );
            sleep( 1 );
        }
        printf( "client closed\n" );
        delete newClient;
        --count;
    }
    
    delete server;

}
