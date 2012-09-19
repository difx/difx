#include <stdio.h>
#include <stdlib.h>
#include <network/TCPClient.h>
#include <network/PacketExchange.h>
#include <network/UDPSocket.h>

main( int argc, char **argv ) {

    char* endptr;

    if ( argc < 3 ) {
        printf( "Usage: clientTest address port\n" );
        exit( EXIT_FAILURE );
    }
    
    //  TCP Socket
    network::TCPClient* client = new network::TCPClient( argv[1], strtol( argv[2], &endptr, 10 ) );
    printf( "client started\n" );

    client->waitForConnect();

    printf( "client got connection\n" );
    network::PacketExchange* packetXO = new network::PacketExchange( client );

    //  Then see what we get.
    char* junk;
    int id, size;
    for ( int i = 0; i < 20; ++i ) {
        packetXO->getPacket( id, junk, size );
        printf( "got packet id = %d, size = %d\n", id, size );
    }
    
    delete client;

}
