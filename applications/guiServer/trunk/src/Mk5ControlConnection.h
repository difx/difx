#ifndef GUISERVER_MK5CONTROLCONNECTION_H
#define GUISERVER_MK5CONTROLCONNECTION_H
//=============================================================================
//
//   guiServer::Mk5ControlConnection Class
//
//!  Handles a (client) TCP connection with the GUI to send it diagnostics
//!  and results while a "mk5control" command is being run.
//
//=============================================================================
#include <network/PacketExchange.h>

namespace guiServer {

    class Mk5ControlConnection : public network::PacketExchange {
    
    public:

        //-----------------------------------------------------------------------------
        //  These are packet types used for sending diagnostic and progress messages from
        //  a machines definition task to the GUI.  These get rather highly specific.
        //-----------------------------------------------------------------------------
        static const int TASK_TERMINATED                     = 100;
        static const int TASK_ENDED_GRACEFULLY               = 101;
        static const int TASK_STARTED                        = 102;
        static const int PARAMETER_CHECK_IN_PROGRESS         = 103;
        static const int PARAMETER_CHECK_SUCCESS             = 104;
        static const int FAILURE_BAD_TARGETNODE              = 105;
        static const int FAILURE_BAD_COMMAND                 = 106;
        static const int CANCEL_COMMAND                      = 107;
        static const int INFORMATION                         = 108;
        static const int WARNING                             = 109;
        static const int ERROR                               = 110;
    
        Mk5ControlConnection( network::GenericSocket* sock ) : network::PacketExchange( sock ) {
        }
        
        ~Mk5ControlConnection() {
        }
        
        //-----------------------------------------------------------------------------
        //!  Handle all of the packet types specific to this exchange.
        //-----------------------------------------------------------------------------
        virtual void newPacket( int packetId, char* data, const int nBytes ) {
            switch( packetId ) {
                case CANCEL_COMMAND:
                    printf( "cancel the mk5control command\n" );
                    break;
                default:
                    break;
            }
        }
        

    protected:
            
    };

}

#endif
