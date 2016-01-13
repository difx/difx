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
#ifndef GUISERVER_CLIENTSIDECONNECTION_H
#define GUISERVER_CLIENTSIDECONNECTION_H
//=============================================================================
//
//   guiServer::ClientSideConnection Class
//
//!  Handles a client connection to the GUI server.  The connection is
//!  bi-directional, so there is a receive thread that monitors incoming data
//!  and write functions.  The PacketExchange class is inherited.
//!
//!  This was originally designed to be used by the "testclient" application,
//!  and not really for prime time.
//
//=============================================================================
#include <pthread.h>
#include <network/UDPSocket.h>
#include <PacketExchange.h>

namespace guiServer {

    class ClientSideConnection : public PacketExchange {
    
    public:

        ClientSideConnection( network::GenericSocket* sock ) : PacketExchange( sock ) {
        }
        
        ~ClientSideConnection() {
        }
        
        //---------------------------------------------------------------------
        //!  Override the relay function.  When the client recieves a "relay"
        //!  it is assumed to be a relayed broadcast packet from a DiFX process.
        //!  All we do is print it out...
        //---------------------------------------------------------------------
        virtual void relay( char* data, const int nBytes ) {
            printf( "%s\n\n", data );
        }
        
    protected:
    

    };

}

#endif
