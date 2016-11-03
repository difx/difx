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
#ifndef NETWORK_TCPCLIENT_H
#define NETWORK_TCPCLIENT_H
//==============================================================================
//
//   guiServer::TCPClient Class
//
//   Makes a TCP Client connection, based on IP address and port number.
//   This class inherits (via the TCPSocket class) the GenericSocket
//   class, from which we get virtual reader and writer functions.
//
//==============================================================================
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <network/TCPSocket.h>

namespace network {

    class TCPClient : public TCPSocket {

    public:
    
        //----------------------------------------------------------------------------
        //  Constructor is given the IP address and port of the server.  The
        //  file descriptor "_fd" is inherited.
        //----------------------------------------------------------------------------
        TCPClient( const char* ipAddress, int port ) {
            //  Create the socket.  This shouldn't fail unless there are too many
            //  sockets already open, which would be kind of strange.
            if ( ( _fd = socket( AF_INET, SOCK_STREAM, 0 ) ) == -1 ) {
                perror( "TCPClient() - socket() failed" );
                return;
            }

            //  Initialize server info.  There shouldn't be any trouble with this
            //  stuff.
            memset( &_connAddr, 0, sizeof( struct sockaddr_in ) );
            _connAddr.sin_family = AF_INET;
            _connAddr.sin_port = htons( port );

            if ( inet_pton( AF_INET, ipAddress, &_connAddr.sin_addr ) == -1 ) {
                perror( "TCPClient() - inet_pton() failed" );
                return;
            }

        }
        
        virtual ~TCPClient() {}

        //----------------------------------------------------------------------------
        //  Wait for the connection to be made.  This call will hang until a server
        //  connection is made.
        //----------------------------------------------------------------------------
        void waitForConnect( bool runMonitor = true ) {
            if ( connect( _fd, (struct sockaddr*)&_connAddr, sizeof( struct sockaddr_in ) ) == -1 ) {
                perror( "TCPClient::waitForConnect() - connect() failed" );
                return;
            }
            if ( runMonitor )
                startMonitor();
        }

    protected:

    };

}

#endif

