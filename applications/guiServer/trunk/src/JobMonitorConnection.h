#ifndef GUISERVER_JOBMONITORCONNECTION_H
#define GUISERVER_JOBMONITORCONNECTION_H
//=============================================================================
//
//   guiServer::JobMonitorConnection Class
//
//!  Handles a (client) TCP connection with the GUI to send it job monitoring
//!  diagnostics and to respond to some instructions.
//
//=============================================================================
#include <network/PacketExchange.h>

namespace guiServer {

    class JobMonitorConnection : public network::PacketExchange {
    
    public:

        //-----------------------------------------------------------------------------
        //  These are packet types used for sending diagnostic and progress messages from
        //  running jobs to the GUI.  These get rather highly specific.  There are a
        //!  few types that are used to obtain data from the GUI as well.
        //-----------------------------------------------------------------------------
        static const int JOB_TERMINATED                     = 100;
        static const int JOB_ENDED_GRACEFULLY               = 101;
        static const int JOB_STARTED                        = 102;
        static const int PARAMETER_CHECK_IN_PROGRESS        = 103;
        static const int PARAMETER_CHECK_SUCCESS            = 104;
        static const int FAILURE_NO_HEADNODE                = 105;
        static const int FAILURE_NO_DATASOURCES             = 106;
        static const int FAILURE_NO_PROCESSORS              = 107;
        static const int FAILURE_NO_INPUTFILE_SPECIFIED     = 108;
        static const int FAILURE_INPUTFILE_NOT_FOUND        = 109;
        static const int FAILURE_INPUTFILE_NAME_TOO_LONG    = 110;
        static const int FAILURE_OUTPUT_EXISTS              = 111;
        static const int DELETING_PREVIOUS_OUTPUT           = 112;
        static const int STARTING_DIFX                      = 113;
        static const int DIFX_MESSAGE                       = 114;
        static const int DIFX_WARNING                       = 115;
        static const int DIFX_ERROR                         = 116;
        static const int DIFX_COMPLETE                      = 117;
        static const int DATA_FILE_SIZE                     = 118;
        static const int JOB_FAILED                         = 119;
        static const int JOB_ENDED_WITH_ERRORS              = 120;
        static const int DIFX_MONITOR_CONNECTION_ACTIVE     = 121;
        static const int DIFX_MONITOR_CONNECTION_BROKEN     = 122;
        static const int DIFX_MONITOR_CONNECTION_FAILED     = 123;
    
        JobMonitorConnection( network::GenericSocket* sock ) : network::PacketExchange( sock ) {
        }
        
        ~JobMonitorConnection() {
        }
        

    protected:
            
    };

}

#endif
