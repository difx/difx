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
#include <stdarg.h>
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
            pthread_mutex_init( &_getPacketMutex, NULL );
        }
        
        virtual ~PacketExchange() {
            writeLock();
            readLock();
            writeUnlock();
            readUnlock();
//            pthread_mutex_destroy( &_sendPacketMutex );
        }

        //----------------------------------------------------------------------------
        //!  This function sends a packet of data of given length, along with an
        //!  "ID" number.  The calling program/inheriting program defines the ID.
        //!  Data are not byte swapped (that is also the calling program's
        //!  responsibility).  This function returns the number of bytes sent,
        //!  or a -1 if there is a failure.  It is mutex locked to assure that
        //!  the entire packet is sent completely.
        //----------------------------------------------------------------------------
        virtual int sendPacket( const int packetId, const char* data, const int nBytes ) {
            int swapped;

            //  Lock writing on the socket.  This makes certain this packet can't be 
            //  broken up by calls to this function from other threads.
            writeLock();

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
            writeUnlock();
            
            return ret;

        }
        
        //----------------------------------------------------------------------------
        //!  The "semaphore" packet is a data-free packet type - basically the packet
        //!  data is the packet ID itself.
        //----------------------------------------------------------------------------
        int semaphorePacket( const int packetId ) {
            return sendPacket( packetId, NULL, 0 );
        }
        
        //----------------------------------------------------------------------------
        //!  "Compose" a packet.  This allows the calling program to build a packet
        //!  on the fly, but it assumes the calling program knows what it is doing.
        //!  This function grabs the socket write lock, then sends a packet ID and
        //!  byte size, but no data.  The calling program is expected to send data using 
        //!  functions below - failure to send the correct number of bytes will screw
        //!  up the packet protocol - rather dangerous.  This function also DOES NOT 
        //!  release the write lock, which is also quite dangerous.  This needs to be
        //!  done using the "composeEnd()" function below.
        //----------------------------------------------------------------------------
        int composePacket( const int packetId, const int nBytes ) {
            int swapped;

            //  Lock writing on the socket.
            writeLock();

            //  packet ID
            swapped = htonl( packetId );
            int ret = _sock->writer( (char*)&swapped, sizeof( int ) );

            //  ...then the size of the packet...
            if ( ret != -1 ) {
                swapped = htonl( nBytes );
                ret = _sock->writer( (char*)&swapped, sizeof( int ) );
            }

            return ret;
        }
        
        //----------------------------------------------------------------------------
        //!  Send integer data as part of a composed packet.  The integer is either a
        //!  single number (by default) or an array.  Numbers are converted to network
        //!  byte order for transmission.
        //----------------------------------------------------------------------------
        int composeInt( const int* data, int n = 1 ) {
            int* swapped = new int[n];
            for ( int i = 0; i < n; ++i )
                swapped[i] = htonl( data[i] );
            int ret = _sock->writer( (char*)swapped, n * sizeof( int ) );
            delete [] swapped;
            return ret;
        }
        
        //----------------------------------------------------------------------------
        //!  Send float data as part of a composed packet.  The data are either a
        //!  single number (by default) or an array.  Numbers are converted to network
        //!  byte order for transmission.
        //----------------------------------------------------------------------------
        int composeFloat( const float* data, int n = 1 ) {
            int* swapped = new int[n];
            for ( int i = 0; i < n; ++i )
                swapped[i] = htonl( data[i] );
            int ret = _sock->writer( (char*)swapped, n * sizeof( float ) );
            delete [] swapped;
            return ret;
        }
        
        //----------------------------------------------------------------------------
        //!  Send double data as part of a composed packet.  The double is either a
        //!  single number (by default) or an array.  Numbers are converted to network
        //!  byte order for transmission.
        //----------------------------------------------------------------------------
        int composeDouble( const double* data, int n = 1 ) {
            double* swapped = new double[n];
            for ( int i = 0; i < n; ++i ) {
                swapped[i] = htond( data[i] );
            }
            int ret = _sock->writer( (char*)swapped, n * sizeof( double ) );
            delete [] swapped;
            return ret;
        }
        
        //----------------------------------------------------------------------------
        //!  Send character data as part of a composed packet.  No byte swapping here.
        //----------------------------------------------------------------------------
        int composeChar( char* data, int n = 1 ) {
            int ret = _sock->writer( data, n );
            return ret;
        }
        
        //----------------------------------------------------------------------------
        //!  Send double precision values using a character string conversion.  This
        //!  avoids some messy problems sending data to Java, but is, of course, rather
        //!  inefficient.  And loses precision.  Probably could make this function
        //!  more flexible and useful by expanding the precision option.
        //----------------------------------------------------------------------------
        void composeStringDouble( const double* data, int n = 1 ) {
            char buffer[15];
            for ( int i = 0; i < n; ++i ) {
                snprintf( buffer, 15, "%14.6e", data[i] );
                composeChar( buffer, 14 );
            }
        }
        
        //----------------------------------------------------------------------------
        //!  This function is called to terminate a "composed" packet.  It releases
        //!  the write lock.
        //----------------------------------------------------------------------------
        void composeEnd() {
            writeUnlock();
        }
        
        //----------------------------------------------------------------------------
        //!  Send a "formatted" packet as a string using printf formatting commands.
        //!  There is a (hopefully quite reasonable) limit to the length of these
        //!  packets.
        //----------------------------------------------------------------------------
        void formatPacket( const int packetId, const char *fmt, ... ) {
            static int MAX_PACKET_MESSAGE_LENGTH = 2048;
            //  Produce a new packet message using the formatting commands.  The message will
            //  be trimmed at the maximum message length.
            char message[MAX_PACKET_MESSAGE_LENGTH];
            va_list ap;
            va_start( ap, fmt );
            vsnprintf( message, MAX_PACKET_MESSAGE_LENGTH, fmt, ap );
            va_end( ap );
            sendPacket( packetId, message, strlen( message ) );
        }
        
        //----------------------------------------------------------------------------
        //!  Send integer data along with a packet type.  The integer is either a
        //!  single number (by default) or an array.  Numbers are converted to network
        //!  byte order for transmission.
        //----------------------------------------------------------------------------
        void intPacket( const int packetId, const int* data, int n = 1 ) {
            int* swapped = new int[n];
            for ( int i = 0; i < n; ++i )
                swapped[i] = htonl( data[i] );
            sendPacket( packetId, (char*)swapped, n * sizeof( int ) );
            delete [] swapped;
        }
        
        //----------------------------------------------------------------------------
        //!  Send double data along with a packet type.  The double is either a
        //!  single number (by default) or an array.  Numbers are converted to network
        //!  byte order for transmission.
        //----------------------------------------------------------------------------
        void doublePacket( const int packetId, const double* data, int n = 1 ) {
            double* swapped = new double[n];
            for ( int i = 0; i < n; ++i )
                swapped[i] = htond( data[i] );
            sendPacket( packetId, (char*)swapped, n * sizeof( double ) );
            delete [] swapped;
        }
        
        //----------------------------------------------------------------------------
        //!  Double precision byte swapper.
        //----------------------------------------------------------------------------
        double htond( double in ) {
            if ( 123 == htonl( 123 ) )
                return in;
            double out;
            int* ptrIn = (int*)&in;
            int* ptrOut = (int*)&out;
            ptrOut[0] = htonl( ptrIn[1] );
            ptrOut[1] = htonl( ptrIn[0] );
            return out;
        }
        
        //----------------------------------------------------------------------------
        //!  This function reads packets sent by the sendPacket function, following
        //!  the same protocol.  The number of bytes read is returned if all goes
        //!  well, a -1 is returned if something goes wrong.  The storage space for
        //!  the incoming data is allocated - it is the responsibility of the
        //!  calling program to delete it.
        //----------------------------------------------------------------------------
        int getPacket( int& packetId, char*& data, int& nBytes ) {
            readLock();
            int swapped;

            //  Get the id.
            if ( _sock->reader( (char*)&swapped, sizeof( int ) ) == -1 ) {
                readUnlock();
                return -1;
            }
            packetId = ntohl( swapped );

            //  Then the number of bytes.
            if ( _sock->reader( (char*)&swapped, sizeof( int ) ) == -1 ) {
                readUnlock();
                return -1;
            }
            nBytes = ntohl( swapped );

            //  Allocate the space for the data, then get it.
            data = new char[ nBytes + 1 ];
            int ret = _sock->reader( data, nBytes );
            if ( ret >= 0 )
                data[ret] = 0;
            readUnlock();
            return( ret );
        }

        //----------------------------------------------------------------------------
        //!  This function sends a "sychronizing" sequence of characters.  It is
        //!  outside the packet protocol.
        //----------------------------------------------------------------------------
//        int sendSync( char* data, const int nBytes ) {
//            return( _sock->writer( data, nBytes ) );
//        }

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
        
        void writeLock() { pthread_mutex_lock( &_sendPacketMutex ); }
        void writeUnlock() { pthread_mutex_unlock( &_sendPacketMutex ); }
        void readLock() { pthread_mutex_lock( &_getPacketMutex ); }
        void readUnlock() { pthread_mutex_unlock( &_getPacketMutex ); }


    protected:

        GenericSocket* _sock;
        pthread_mutex_t _sendPacketMutex;
        pthread_mutex_t _getPacketMutex;

    };

}

#endif

