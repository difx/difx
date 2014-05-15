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
                _ssc->sendPacket( ServerSideConnection::CHANNEL_DATA, (char*)_buff, len, true );
                usleep( 100000 );
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
                        usleep( 1000000 );
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
        
    protected:
    
        void closer() {
            if ( _channelData ) {
                free( _buff );
                _ssc->purgeChannelData( _port );
            }
            else {
                if ( _packetExchange != NULL )
                    delete _packetExchange;
                delete _client;
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
        void writeLock() { pthread_mutex_lock( &_sendPacketMutex ); }
        void writeUnlock() { pthread_mutex_unlock( &_sendPacketMutex ); }
        pthread_mutex_t _sendPacketMutex;
        network::TCPClient* _client;
    };

}

#endif
