#ifndef GUISERVER_GUICLIENT_H
#define GUISERVER_GUICLIENT_H
//=============================================================================
//
//!  This class is a wrapper for a TCP client connection to the GUI.  It
//!  accommodates a "channelled" connection to the GUI where all data are
//!  sent through the primary GUI/guiServer TCP connection instead of opening
//!  additional sockets.  This capability is useful when the GUI/guiServer
//!  connection is sent through a SHH tunnel (where each additional TCP
//!  connection would also require a tunnel, an annoying and maybe impossible
//!  thing to set up).
//!
//!  There are also functions to handle the PacketExchange class that is used
//!  by many guiServer threads to communicate.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <network/PacketExchange.h>
#include <sys/statvfs.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <fcntl.h>
#include <network/TCPClient.h>

namespace guiServer {

    class GUIClient {
    
    public:
    
        //---------------------------------------------------------------------
        //!  The constructor requires a ServerSideConnection instance.  This
        //!  is used to determine whether this connection is "channelled" or
        //!  conventional TCP.  The "channelled" option will apply to this
        //!  connection until it is closed - this should protect against
        //!  bizarre behavior if changes are made to the "channelled" state
        //!  during operation.
        //!
        //!  The connect() operation is performed here.  The "runMonitor"
        //!  variable, which by default is true, causes the TCPClient connection
        //!  to run a monitoring thread to receive data.  If data are not
        //!  expected this is a waste, so it should be set to false.
        //---------------------------------------------------------------------
        GUIClient( ServerSideConnection* ssc, const char* addr, int port, bool runMonitor = true ) {
            _ssc = ssc;
            _port = port;
            _channelData = ssc->channelData();
            _buff = NULL;
            _buffSize = 0;
            _packetExchange = NULL;
            //  Send the connection message if this is channelled.
            if ( _channelData ) {
                _swapPort = htonl( _port );
                _ssc->sendPacket( ServerSideConnection::CHANNEL_CONNECTION, (char*)&_swapPort, sizeof( int ), true );
                _ret = 0;
            }
            //  Conventional TCP setup if channeling is not on.
            else {
                _client = new network::TCPClient( addr, port );
                _client->waitForConnect( runMonitor );
            }
        }
        
        ~GUIClient() {
            closer();
        }

        //-----------------------------------------------------------------------------
        //!  Send channelled data associated with the given port in an analogue to the
        //!  "write" function.  If channeling is off, this is a normal write.
        //-----------------------------------------------------------------------------
        int writer( const void* data, int n ) {
            if ( _channelData ) {
                //  The data buffer expands as necessary to accommdate our largest message.
                //  It is saved between write operations for efficiency.
                int len = n + sizeof( int );
                if ( len > _buffSize ) {
                    _buff = realloc( _buff, len );
                    _buffSize = len;
                }
                memcpy( _buff, &_swapPort, len );
                memcpy( (char*)_buff + sizeof( int ), data, n );
                int ret = _ssc->sendPacket( ServerSideConnection::CHANNEL_DATA, (char*)_buff, len, true );
                return len;
            }
            else {
                //  Non-channelled is a conventional write.
                return _client->writer( (char*)data, n );
            }
        }
        
        //-----------------------------------------------------------------------------
        //!  Read channelled data associated with the given port in an analogue to the
        //!  "read" function.  If channeling is off this is a normal read.  Returns
        //!  a -1 on a select/read error.
        //-----------------------------------------------------------------------------
        int reader( void* data, int n ) {
            int rn = 0;
            if ( _channelData ) {
                while ( rn < n ) {
                    rn += _ssc->getChannelData( _port, (char*)data + rn, n );
                    if ( rn < n )
                        usleep( 10000 );
                }
            }
            else {
            	rn = _client->reader( (char*)data, n );
            }
            return rn;
        }
        
        //---------------------------------------------------------------------
        //!  Set this client connection to use a "PacketExchange".
        //---------------------------------------------------------------------
        void packetExchange() {
            if ( _channelData )
                pthread_mutex_init( &_sendPacketMutex, NULL );
            else
                _packetExchange = new network::PacketExchange( _client );
        }
        
        //---------------------------------------------------------------------
        //!  A wrapper for the PacketExchange::sendPacket function in TCP mode,
        //!  and a channelled version in channel mode.  This assumes a
        //!  packet exchange has been intitialized (which needs to be done 
        //!  using the packetExchange() method).
        //---------------------------------------------------------------------
        int sendPacket( const int packetId, const char* data, const int nBytes ) {
            if ( _channelData ) {
                writeLock();
                int swapped = htonl( packetId );
                int ret = writer( (char*)&swapped, sizeof( int ) );
                if ( ret != -1 ) {
                    swapped = htonl( nBytes );
                    ret = writer( (char*)&swapped, sizeof( int ) );
                }
                if ( ret != -1 && nBytes > 0 )
                    ret = writer( data, nBytes );
                writeUnlock();
                return ret;
            }
            else if ( _packetExchange != NULL )
                return _packetExchange->sendPacket( packetId, data, nBytes );
            else
                return -1;
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
            int ret = writer( (char*)&swapped, sizeof( int ) );

            //  ...then the size of the packet...
            if ( ret != -1 ) {
                swapped = htonl( nBytes );
                ret = writer( (char*)&swapped, sizeof( int ) );
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
            int ret = writer( (char*)swapped, n * sizeof( int ) );
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
            int ret = writer( (char*)swapped, n * sizeof( float ) );
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
            int ret = writer( (char*)swapped, n * sizeof( double ) );
            delete [] swapped;
            return ret;
        }
        
        //----------------------------------------------------------------------------
        //!  Send character data as part of a composed packet.  No byte swapping here.
        //----------------------------------------------------------------------------
        int composeChar( char* data, int n = 1 ) {
            int ret = writer( data, n );
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
        
        //---------------------------------------------------------------------
        //!  A wrapper for the PacketExchange::getPacket function that works in
        //!  both TCP and channel mode.  The packet exchange needs to be
        //!  initialized for this to work.
        //---------------------------------------------------------------------
        int getPacket( int& packetId, char*& data, int& nBytes ) {
            if ( _channelData ) {
                int swapped;
                //  Get the id.
                if ( reader( (char*)&swapped, sizeof( int ) ) == -1 )
                    return -1;
                packetId = ntohl( swapped );
                //  Then the number of bytes.
                if ( reader( (char*)&swapped, sizeof( int ) ) == -1 )
                    return -1;
                nBytes = ntohl( swapped );
                //  Allocate the space for the data, then get it.
                data = new char[ nBytes + 1 ];
                int ret = reader( data, nBytes );
                if ( ret >= 0 )
                    data[ret] = 0;
                return( ret );
            }
            else if ( _packetExchange != NULL )
                return _packetExchange->getPacket( packetId, data, nBytes );
            else
                return -1;
        }
        
        //---------------------------------------------------------------------
        //!  Return the health of the connection.  Not particularly sophisticated
        //!  right now.
        //---------------------------------------------------------------------
        bool okay() { 
            if ( _channelData )
                return _ret == 0;
            else
                return _client->connected();
        }

        void writeLock() { pthread_mutex_lock( &_sendPacketMutex ); }
        void writeUnlock() { pthread_mutex_unlock( &_sendPacketMutex ); }
        
    protected:
    
        void closer() {
            if ( _channelData ) {
                free( _buff );
                _ssc->purgeChannelData( _port );
            }
            else {
                if ( _packetExchange != NULL )
                    delete _packetExchange;
                _packetExchange = NULL;
                if ( _client != NULL )
                    delete _client;
                _client = NULL;
            }
        }

        int _port;
        ServerSideConnection* _ssc;
        bool _channelData;
        int _sockfd;
        int _ret;
        int _swapPort;
        void* _buff;
        int _buffSize;
        network::PacketExchange* _packetExchange;
        pthread_mutex_t _sendPacketMutex;
        network::TCPClient* _client;
    };

}

#endif
