#ifndef NETWORK_TCPSERVER_H
#define NETWORK_TCPSERVER_H
//==============================================================================
//
//   network::TCPServer Class
//
//   Sets up a TCP Server at a given port number.  This server will accept
//   client connections (see TCPClient) and return pointers to new
//   TCPSocket class instances when those connections are made.
//
//==============================================================================
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <network/TCPSocket.h>

namespace network {

    class TCPServer {

    public:

        //----------------------------------------------------------------------------
        //  The constructor is given a port number.  This is the port that is
        //  opened for client connections.
        //----------------------------------------------------------------------------
        TCPServer( int port ) {
        
            _serverUp = false;

            //  We need this value in one place and a pointer to it in another.
            _addrSize = sizeof( struct sockaddr_in );

            if ( ( _listenFd = socket( AF_INET, SOCK_STREAM, 0 ) ) == -1 ) {
                perror( "TCPServer() - socket() failed" );
                return;
            }

            int on = 1;
        	if ( setsockopt( _listenFd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof( on ) ) < 0 ) {
                perror( "TCPServer: trouble with setsockopt" );
                close( _listenFd );
                _listenFd = -1;
                return;
            }

            //  Initialize the server settings.
            memset( &_serverAddr, 0, sizeof( struct sockaddr ) );
            _serverAddr.sin_family = AF_INET;
            _serverAddr.sin_addr.s_addr = htonl( INADDR_ANY );
            _serverAddr.sin_port = htons( port );
 
            //  This is where the process tends to barf if a previous socket at
            //  the port was not properly closed.
            if ( bind( _listenFd, (struct sockaddr*)&_serverAddr, _addrSize ) == -1 ) {
                perror( "TCPServer() - bind() failed" );
                return;
            }

            //  This indicates "a willingness to accept incoming connections" (from
            //  the man pages).  The integer is the number of pending connection
            //  requests that can be queued.
            if ( listen( _listenFd, 1 ) == -1 ) {
                perror( "TCPServer() - listen() failed" );
                return;
            }
            
            _serverUp = true;

        }

        //----------------------------------------------------------------------------
        //  This should be all the destructor has to do.
        //----------------------------------------------------------------------------
        ~TCPServer() {
            if ( _listenFd > -1 )
                close( _listenFd );
        }

        //----------------------------------------------------------------------------
        //  Wait for a client connection.  If one occurs, return a pointer to a new
        //  TCPSocket instance (containing file descripter, reader/writer methods,
        //  and client address information).  This function can be called as many
        //  times as you like, in theory.
        //----------------------------------------------------------------------------
        TCPSocket* acceptClient() {
            int newFd;
            TCPSocket* newClient = new TCPSocket;

            //  Wait for the client connection.  This call will hang until it happens.
            if ( ( newFd = accept( _listenFd, (struct sockaddr*)&_connAddr, &_addrSize ) ) == -1 ) {
                perror( "TCPServer::acceptClient() - accept() failed" );
                delete newClient;
                return NULL;
            }

            //  Save the IP address of this connection.  This will only be good until the
            //  next call the "acceptClient()".
            inet_ntop( AF_INET, (char*)&(_connAddr.sin_addr), _clientIP, 15 );

            //  Set the file descriptor and return the new client.
            newClient->setFd( newFd );
            newClient->startMonitor();
            return( newClient );
        }

        //----------------------------------------------------------------------------
        //  IP address of the last client to connect.  This is only good between
        //  calls to "acceptClient()".
        //----------------------------------------------------------------------------
        const char* lastClientIP() { return _clientIP; }
        
        const bool serverUp() { return _serverUp; }

    protected:

        int _listenFd;
        struct sockaddr_in _serverAddr;
        struct sockaddr_in _connAddr;
        char _clientIP[16];
        socklen_t _addrSize;
        bool _serverUp;

    };

}

#endif
