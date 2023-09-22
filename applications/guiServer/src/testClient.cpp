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
//=============================================================================
//
//   testClient
//
//!  This is a simple application used to test the guiServer.
//!  May or may not still work.
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
