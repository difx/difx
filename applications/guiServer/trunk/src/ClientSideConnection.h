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
