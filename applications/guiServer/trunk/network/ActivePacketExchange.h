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
#ifndef NETWORK_ACTIVEPACKETEXCHANGE_H
#define NETWORK_ACTIVEPACKETEXCHANGE_H
//=============================================================================
//
//    network::ActivePacketExchange Class
//
//!  This class inherits the PacketExchange Class mechanism and adds an active
//!  monitoring thread to receive packets.  Each received packet triggers a
//!  call to the "newPacket()" function which should be overridden by the
//!  inheriting class.
//
//=============================================================================
#include <pthread.h>
#include <network/PacketExchange.h>

namespace network {

    class ActivePacketExchange : public PacketExchange {
    
    public:
    
        ActivePacketExchange( GenericSocket* sock ) : PacketExchange( sock ) {
            _receiveActive = false;
            pthread_create( &_receiveId, NULL, staticReceiveThread, this );               
        }
        
        ~ActivePacketExchange() {
            _receiveActive = false;
        }

        //---------------------------------------------------------------------
        //!  Static start function for the receive thread.
        //---------------------------------------------------------------------
        static void* staticReceiveThread( void* a ) {
            ( (ActivePacketExchange*)a )->receiveThread();
            return NULL;
        }
        
        //---------------------------------------------------------------------
        //!  Thread to receive all incoming data on the socket.  Each received
        //!  packet spawns the do-nothing function "newPacket()" with type ID
        //!  byte size, and data content.  This function should be overridden by
        //!  inheriting classes to actually accomplish something.
        //---------------------------------------------------------------------
        void receiveThread() {
            _receiveActive = true;
            while ( _receiveActive ) {
                int packetId = 0;
                int nBytes;
                char* data;
                if ( getPacket( packetId, data, nBytes ) == -1 ) {
                    //  connection failure
                    _receiveActive = false;
                }
                else {
                    newPacket( packetId, data, nBytes );
                    //  Free the space allocated to this incoming message.  This
                    //  means the newPacket function must copy it if necessary.
//                    delete [] data;
                }
            }
        }
        
        virtual void newPacket( int packetId, char* data, const int nBytes ) {}
        
    protected:
    
        pthread_t _receiveId;
        bool _receiveActive;

    };

}

#endif
