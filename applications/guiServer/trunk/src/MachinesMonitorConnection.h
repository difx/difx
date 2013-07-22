#ifndef GUISERVER_MACHINESMONITORCONNECTION_H
#define GUISERVER_MACHINESMONITORCONNECTION_H
//=============================================================================
//
//   guiServer::MachinesMonitorConnection Class
//
//!  Handles a (client) TCP connection with the GUI to send it diagnostics
//!  while threads and machines files are created.
//
//=============================================================================
#include <network/PacketExchange.h>

namespace guiServer {

    class MachinesMonitorConnection : public network::PacketExchange {
    
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
        static const int FAILURE_NO_HEADNODE                 = 105;
        static const int FAILURE_NO_DATASOURCES              = 106;
        static const int FAILURE_NO_PROCESSORS               = 107;
        static const int WARNING_NO_MACHINES_FILE_SPECIFIED  = 108;
        static const int WARNING_NO_THREADS_FILE_SPECIFIED   = 109;
        static const int THREADS_FILE_NAME                   = 110;
        static const int MACHINES_FILE_NAME                  = 111;
        static const int FAILURE_NO_FILES_SPECIFIED          = 112;
        static const int FAILURE_OPEN_MACHINES_FILE          = 113;
        static const int FAILURE_OPEN_THREADS_FILE           = 114;
        static const int MACHINES_FILE_CREATED               = 115;
        static const int THREADS_FILE_CREATED                = 116;
        static const int FAILURE_FILE_REMOVAL                = 117;
        static const int FAILURE_POPEN                       = 118;
        static const int FAILURE_MPIRUN                      = 119;
        static const int SUCCESS_MPIRUN                      = 120;
        static const int LOW_THREAD_COUNT                    = 121;
        static const int RUNNING_MPIRUN_TESTS                = 122;
    
        MachinesMonitorConnection( network::GenericSocket* sock ) : network::PacketExchange( sock ) {
        }
        
        ~MachinesMonitorConnection() {
        }
        

    protected:
            
    };

}

#endif
