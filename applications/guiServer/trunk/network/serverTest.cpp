/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
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
