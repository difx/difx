#ifndef NETWORK_PACKETEXCHANGE_H
#define NETWORK_PACKETEXCHANGE_H
//==============================================================================
//
//   network::PacketExchange Class
//
//!  This class sets up a simple packet exchange mechanism using the
//!  GenericSocket class.  It provides some simple functions for sending
//!  packet data with assigned "IDs", which are simply integers that must be
//!  defined by the calling program.
//
//==============================================================================
#include <arpa/inet.h>
#include <string.h>
#include <pthread.h>
#include <network/GenericSocket.h>

namespace network {

    class PacketExchange
    {

    public:

        //----------------------------------------------------------------------------
        //!  The constructor is given a "generic" socket.  The socket itself can be
        //!  either TCP or UDP (or others, if I make something else).
        //----------------------------------------------------------------------------
        PacketExchange( GenericSocket* sock ) {
            _sock = sock;
            pthread_mutex_init( &_sendPacketMutex, NULL );
        }

        //----------------------------------------------------------------------------
        //!  This function sends a packet of data of given length, along with an
        //!  "ID" number.  The calling program/inheriting program defines the ID.
        //!  Data are not byte swapped (that is also the calling program's
        //!  responsibility).  This function returns the number of bytes sent,
        //!  or a -1 if there is a failure.  It is mutex locked to assure that
        //!  the entire packet is sent completely.
        //----------------------------------------------------------------------------
        virtual int sendPacket( const int packetId, char* data, const int nBytes ) {
            int swapped;

            //  Lock writing on the socket.  This makes certain this packet can't be 
            //  broken up by calls to this function from other threads.
            _sock->writeLock();

            //  Our trivial packet protocol is to send the packetId first (network byte
            //  ordered)...
            swapped = htonl( packetId );
            int ret = _sock->writer( (char*)&swapped, sizeof( int ) );

            //  ...then the size of the packet...
            if ( ret != -1 ) {
                swapped = htonl( nBytes );
                ret = _sock->writer( (char*)&swapped, sizeof( int ) );
            }

            //  ...then the data.
            if ( ret != -1 )
                ret = _sock->writer( data, nBytes );
                
            //  Unlock the socket.
            _sock->writeUnlock();
            
            return ret;

        }
        
        //----------------------------------------------------------------------------
        //!  This function reads packets send by the sendPacket function, following
        //!  the same protocol.  The number of bytes read is returned if all goes
        //!  well, a -1 is returned if something goes wrong.  The storage space for
        //!  the incoming data is allocated - it is the responsibility of the
        //!  calling program to delete it.
        //----------------------------------------------------------------------------
        int getPacket( int& packetId, char*& data, int& nBytes ) {
            int swapped;

            //  Get the id.
            if ( _sock->reader( (char*)&swapped, sizeof( int ) ) == -1 )
                return -1;
            packetId = ntohl( swapped );

            //  Then the number of bytes.
            if ( _sock->reader( (char*)&swapped, sizeof( int ) ) == -1 )
                return -1;
            nBytes = ntohl( swapped );

            //  Allocate the space for the data, then get it.
            data = new char[ nBytes + 1 ];
            int ret = _sock->reader( data, nBytes );
            if ( ret >= 0 )
                data[ret] = 0;
            return( ret );
        }

        //----------------------------------------------------------------------------
        //!  This function sends a "sychronizing" sequence of characters.  It is
        //!  outside the packet protocol.
        //----------------------------------------------------------------------------
        int sendSync( char* data, const int nBytes ) {
            return( _sock->writer( data, nBytes ) );
        }

        //----------------------------------------------------------------------------
        //!  This function tries to locate a synchronizing sequence in a data
        //!  stream.  It will not return until it finds it, or an error occurs.
        //----------------------------------------------------------------------------
        int findSync( const char* data, const int nBytes ) {

            char* trial = new char[ nBytes ];

            //  Start by reading the specified number of bytes.
            if ( _sock->reader( trial, nBytes ) == -1 )
                return -1;

            //  Compare until we get a match.
            while( strncmp( trial, data, nBytes ) ) {
                //  Shift all characters by 1
                for ( int i = 0; i < nBytes-1; ++i )
                    trial[i] = trial[i+1];
                //  Read a new character.
                if ( _sock->reader( &(trial[nBytes-1]), 1 ) == -1 )
                    return -1;
            }

            return 1;

        }


    protected:

        GenericSocket* _sock;
        pthread_mutex_t _sendPacketMutex;

    };

}

#endif

