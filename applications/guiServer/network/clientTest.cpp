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
