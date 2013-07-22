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
            pthread_attr_init( &_receiveAttr );
            pthread_create( &_receiveId, &_receiveAttr, staticReceiveThread, this );               
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
                    delete [] data;
                }
            }
        }
        
        virtual void newPacket( int packetId, char* data, const int nBytes ) {}
        
    protected:
    
        pthread_attr_t _receiveAttr;
        pthread_t _receiveId;
        bool _receiveActive;

    };

}

#endif
