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
//   udpTest
//
//!  Trivial "chat" utitily meant to test the UDPSocket class and show some
//!  of the uses of the PacketExchange class.  Start a number of these on
//!  different hosts on a network, all specifying the same port number (the
//!  one and only argument).
//!
//!  Type "list\n" to see list of host IPs who are running the program (own
//!  IP is omitted).  The list is updated so any host that leaves should
//!  disappear within a couple of seconds.
//!
//!  Type "exit\n" to leave.
//!
//!  Type "hello IP#\n" to "talk" to a specific IP.  Anything you type after
//!  will be sent to that IP.
//!
//!  Type "bye\n" to terminate a "talk" session with a particular IP.
//!
//!  That's it!  (It's just a demo afterall...)
//
//=============================================================================
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <vector>
#include <string>
#include <sys/time.h>
#include <network/UDPSocket.h>
#include <network/PacketExchange.h>

static const int HEARTBEAT_PACKET = 1;
static const int MESSAGE_PACKET = 2;

struct ListItem {
    std::string ip;
    unsigned long int timeStamp;
};
std::vector<ListItem> _ipList;

//-----------------------------------------------------------------------------
//!  Simple thread function providing a "heartbeat" broadcast once per second.
//-----------------------------------------------------------------------------
void* heartbeatThread( void* a ) {
    int port = *(int*)a;
    //  Open a UDP broadcast for the heartbeat on the given port, then use the socket
    //  in a new instance of the packet exchange protocol.
    network::UDPSocket* hbServer = new network::UDPSocket( network::UDPSocket::BROADCAST, NULL, port );
    network::PacketExchange* packetXO = new network::PacketExchange( hbServer );
    printf( "UDP broadcast set up....sending data\n" );
    
    //  Send a heartbeat (an empty packet) once per second.
    while( 1 ) {
        //printf( "broadcast heartbeat\n" );
        //packetXO->sendSync( "UDPTEST", 7 );
        packetXO->sendPacket( HEARTBEAT_PACKET, NULL, 0 );
        sleep( 1 );
    }

}

//-----------------------------------------------------------------------------
//!  This thread receives all UDP packets and figures out what to do with them.
//-----------------------------------------------------------------------------
void* receiveThread( void *a ) {
    int port = *(int*)a;
    //  Open a UDP socket for receiving and build a packet exchange protocol instance
    //  out of it.
    network::UDPSocket* client = new network::UDPSocket( network::UDPSocket::RECEIVE, NULL, port );
    network::PacketExchange* packetXO = new network::PacketExchange( client );
    printf( "recieve socket set up, waiting for data\n" );

    char newAddress[16];

    while ( 1 ) {
        int packetID;
        char* data;
        int nBytes;
        //packetXO->findSync( "UDPTEST", 7 );
        packetXO->getPacket( packetID, data, nBytes );
        data[nBytes] = 0;
        client->fromIPAddress( newAddress );
        switch( packetID ) {

        case HEARTBEAT_PACKET: {
                //  See if we have this address in our list already.  If not, add it.
                int n = _ipList.size();
                bool found = false;
                for ( int i = 0; i < n && !found; ++i ) {
                    if ( !strcmp( newAddress, _ipList[i].ip.c_str() ) ) {
                        struct timeval tv;
                        gettimeofday( &tv, NULL );
                        _ipList[i].timeStamp = tv.tv_sec;
                        found = true;
                    }
                }
                if ( !found ) {
                    ListItem newItem;
                    newItem.ip.assign( newAddress );
                    struct timeval tv;
                    gettimeofday( &tv, NULL );
                    newItem.timeStamp = tv.tv_sec;
                    _ipList.push_back( newItem );
                }
            }
            break;
            
        case MESSAGE_PACKET: {
            printf( "%s: %s", newAddress, data );
            }
            break;

        default:
            printf( "no idea what this is...packet ID is %d\n", packetID );
            break;

        }
        
        delete [] data;
        
        //  After each heartbeat check the time...every couple of seconds we
        //  want to 
    }
    
}
   

main( int argc, char **argv ) {

    if ( argc < 2 )  {
        printf( "usage: udpTest PORT#\n" );
        exit( EXIT_FAILURE );
    }
    
    int port = atoi( argv[1] );
    
    pthread_attr_t attr1, attr2;
    pthread_t threadId1, threadId2;
    pthread_attr_init( &attr1 );
    pthread_create( &threadId1, &attr1, &receiveThread, &port );
    pthread_attr_init( &attr2 );
    pthread_create( &threadId2, &attr2, &heartbeatThread, &port );
    
    network::UDPSocket* talk = NULL;
    network::PacketExchange* talkXO = NULL;
    
    char inLine[1024];
    bool keepGoing = true;
    while ( keepGoing ) {
        fgets( inLine, 1024, stdin );
        //  Before checking what the user wants, check all of our list items for
        //  timeliness - eliminate any addresses that have not updated in the last
        //  two seconds.
        struct timeval tv;
        gettimeofday( &tv, NULL );
        for ( std::vector<ListItem>::iterator it = _ipList.begin(); it != _ipList.end(); ) {
            if ( tv.tv_sec - it->timeStamp > 2 )
                it = _ipList.erase( it );
            else
                ++it;
        }
        if ( !strcasecmp( inLine, "exit\n" ) )
            keepGoing = false;
        else if ( !strcasecmp( inLine, "list\n" ) ) {
            for ( std::vector<ListItem>::iterator it = _ipList.begin(); it != _ipList.end(); ++it ) {
                printf( "%s   %ld\n", (it->ip).c_str(), it->timeStamp );
            }
        }
        else if ( !strcasecmp( inLine, "bye\n" ) ) {
            if ( talk != NULL ) {
                talkXO->sendPacket( MESSAGE_PACKET, inLine, strlen( inLine ) );
                delete talk;
                delete talkXO;
                talk = NULL;
            }
        }
        else if ( !strncasecmp( inLine, "hello", 5 ) ) {
            //  Open a new Unicast to the given address
            char newAddress[100];
            sprintf( newAddress, "%s", inLine + 6 );
            newAddress[ strlen( newAddress ) - 1 ] = 0;
            talk = new network::UDPSocket( network::UDPSocket::UNICAST, newAddress, port );
            talkXO = new network::PacketExchange( talk );
            talkXO->sendPacket( MESSAGE_PACKET, inLine, strlen( inLine ) );
        }
        else if ( talk != NULL ) {
            talkXO->sendPacket( MESSAGE_PACKET, inLine, strlen( inLine ) );
        }
        else
            printf( "?\n" );
    }
    
}
