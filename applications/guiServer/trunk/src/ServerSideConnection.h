#ifndef GUISERVER_SERVERSIDECONNECTION_H
#define GUISERVER_SERVERSIDECONNECTION_H
//=============================================================================
//
//   guiServer::ServerSideConnection Class
//
//!  Handles a single connection to the GUI server.  The connection is
//!  bi-directional, so there is a receive thread that monitors incoming data
//!  and write functions.  The PacketExchange class is inherited.
//!
//!  Much of the DiFX interprocess communication occurs by multicasts.  This
//!  class can monitor that traffic and be set to "relay" it to the GUI client.
//!  It can also send commands and other communications to other DiFX processes
//!  using the multicast network.  
//
//=============================================================================
#include <stdlib.h>
#include <pthread.h>
#include <string>
#include <network/UDPSocket.h>
#include <network/ActivePacketExchange.h>
#include <difxmessage.h>
#include <JobMonitorConnection.h>
#include <list>
#include <string>
#include <glob.h>

namespace guiServer {

    class ServerSideConnection : public network::ActivePacketExchange {
    
        //---------------------------------------------------------------------
        //!  These are the packet types recognized and/or sent by this protocol.
        //---------------------------------------------------------------------
        static const int RELAY_PACKET                   = 1;
        static const int RELAY_COMMAND_PACKET           = 2;
        static const int COMMAND_PACKET                 = 3;
        static const int INFORMATION_PACKET             = 4;
        static const int WARNING_PACKET                 = 5;
        static const int ERROR_PACKET                   = 6;
        static const int MULTICAST_SETTINGS_PACKET      = 7;
        static const int GUISERVER_VERSION              = 8;
        static const int GUISERVER_DIFX_VERSION         = 9;
        static const int AVAILABLE_DIFX_VERSION         = 10;
        static const int DIFX_BASE                      = 11;
        static const int GUISERVER_ENVIRONMENT          = 12;
        static const int DIFX_SETUP_PATH                = 13;
        static const int START_DIFX_MONITOR             = 14;
        static const int DIFX_RUN_LABEL                 = 15;
        static const int GUISERVER_USER                 = 16;
        static const int MESSAGE_SELECTION_PACKET       = 17;

    public:

        static const int MAX_COMMAND_SIZE = 1024;
        
        ServerSideConnection( network::GenericSocket* sock, const char* clientIP, const char* difxBase, char** envp ) : network::ActivePacketExchange( sock ) {
            _commandSocket = NULL;
            _monitorSocket = NULL;
            _multicastGroup[0] = 0;
            _multicastPort = 0;
            _newMulticastSettings = true;            
            _difxAlertsOn = false;
            _relayDifxMulticasts = false;
            _diagnosticPacketsOn = true;
            _relayAllPackets = true;
            _relayAlertPackets = false;
            _relayCommandPackets = false;
            _relayFileOperationPackets = false;
            _relayFileTransferPackets = false;
            _relayGetDirectoryPackets = false;
            _relayInfoPackets = false;
            _relayLoadPackets = false;
            _relayMachinesDefinitionPackets = false;
            _relayMk5ControlPackets = false;
            _relaySmartPackets = false;
            _relayStatusPackets = false;
            _relayStopPackets = false;
            _relayVex2DifxRunPackets = false;
            _relayWeightPackets = false;
            _relayMark5StatusPackets = false;
            _relayUnknownPackets = false;
            snprintf( _clientIP, 16, "%s", clientIP );
            strncpy( _difxBase, difxBase, DIFX_MESSAGE_LENGTH );
            _envp = envp;
            pthread_mutex_init( &_runningJobsMutex, NULL );
        }
        
        ~ServerSideConnection() {
            if ( _monitorSocket != NULL ) {
                _monitorSocket->closeFd();
                delete _monitorSocket;
            }
            if ( _commandSocket != NULL ) {
                _commandSocket->closeFd();
                delete _commandSocket;
            }
            _monitorSocket = NULL;
            _commandSocket = NULL;
        }
        
        //---------------------------------------------------------------------
        //!  Start a thread to collect DiFX multicast messages using the current
        //!  multicast group and port settings.
        //---------------------------------------------------------------------
        void startMulticastMonitor() {
            //  Shut down the existing monitor if there is one.
            if ( _monitorSocket != NULL ) {
                _monitorSocket->closeFd();
                sleep( 1 );  //  this seems a bit long!
            }
            _monitorSocket = new network::UDPSocket( network::UDPSocket::RECEIVE, _multicastGroup, _multicastPort );
            _monitorSocket->ignoreOwn( false );
            //  Start the thread that reads the multicast messages.
            pthread_attr_init( &_monitorAttr );
            pthread_create( &_monitorId, &_monitorAttr, staticDifxMonitor, this );      
        }
        
        //---------------------------------------------------------------------
        //!  Static thread start function (for the multicast monitor).
        //---------------------------------------------------------------------
        static void* staticDifxMonitor( void* a ) {
            ( (ServerSideConnection*)a )->difxMonitor();
            return NULL;
        }
        
        static const int MAX_MESSAGE_LENGTH = 10 * 1024;
        
        //---------------------------------------------------------------------
        //!  This is the function that actually monitors DiFX multicasts - it
        //!  runs as a continuous thread until the socket it closed or returns
        //!  an error.
        //---------------------------------------------------------------------
        void difxMonitor() {
            //char hostName[512];
            char message[MAX_MESSAGE_LENGTH + 1];
            //char hostIP[512];
            while ( _monitorSocket != NULL && _monitorSocket->fd() != -1 ) {
                int ret = _monitorSocket->reader( message, MAX_MESSAGE_LENGTH );
                if ( ret == -1 ) {
                    fprintf( stderr, "closing monitor socket due to receive error\n" );
                    delete _monitorSocket;
                    _monitorSocket = NULL;
                }
                else if ( ret > 0 && ret <= ((int)(sizeof( DifxMessageGeneric ))) ) {
                    //  Decide what to do with this message.
                    DifxMessageGeneric G;
                    if ( !difxMessageParse( &G, message ) ) {
//                        _monitorSocket->fromIPAddress( hostIP );
                        //  Replace the "from" field in the message header with the full address
                        //  of the source of this message (as guiServer sees it).  We are assuming
                        //  that the first instances of "<from>" and "</from>" are the proper
                        //  location for this.
//                        char newMessage[MAX_MESSAGE_LENGTH + 1];
//                        strncpy( newMessage, message, MAX_MESSAGE_LENGTH + 1 );
//                        if ( !_monitorSocket->fromHostName( hostName, 512 ) ) {
//                            snprintf( G.from, DIFX_MESSAGE_MAX_INET_ADDRESS_LENGTH, "%s", hostName );
//                            message[MAX_MESSAGE_LENGTH] = 0;
//                            char* start = strcasestr( newMessage, "<from>" );
//                            char* end = strcasestr( message, "</from>" );
//                            if ( start != NULL && end != NULL )
//                                snprintf( start, MAX_MESSAGE_LENGTH, "<from>%s%s", hostName, end );
//                        }
//                        printf( "this message is from %s\n", hostIP );
//                        switch( G.type ) {
//                        case DIFX_MESSAGE_STATUS:
//                            break;
//                        case DIFX_MESSAGE_DIAGNOSTIC:
//                            break;
//                        case DIFX_MESSAGE_ALERT:
//                            break;
//                        default:
//                            break;
//                        }
                        if ( _relayDifxMulticasts ) {
                            //  Relay the packet if it is one of the types the GUI has selected.
                            bool relayThis = false;
                            if ( _relayAllPackets )
                                relayThis = true;
                            else {
                                if ( _relayAlertPackets && G.type == DIFX_MESSAGE_ALERT )
                                    relayThis = true;
                                else if ( _relayCommandPackets && G.type == DIFX_MESSAGE_COMMAND )
                                    relayThis = true;
                                else if ( _relayFileOperationPackets && G.type == DIFX_MESSAGE_FILEOPERATION )
                                    relayThis = true;
                                else if ( _relayFileTransferPackets && G.type == DIFX_MESSAGE_FILETRANSFER )
                                    relayThis = true;
                                else if ( _relayGetDirectoryPackets && G.type == DIFX_MESSAGE_GETDIRECTORY )
                                    relayThis = true;
                                else if ( _relayInfoPackets && G.type == DIFX_MESSAGE_INFO )
                                    relayThis = true;
                                else if ( _relayLoadPackets && G.type == DIFX_MESSAGE_LOAD )
                                    relayThis = true;
                                else if ( _relayMachinesDefinitionPackets && G.type == DIFX_MESSAGE_MACHINESDEFINITION )
                                    relayThis = true;
                                else if ( _relayMk5ControlPackets && G.type == DIFX_MESSAGE_MK5CONTROL )
                                    relayThis = true;
                                else if ( _relaySmartPackets && G.type == DIFX_MESSAGE_SMART )
                                    relayThis = true;
                                else if ( _relayStatusPackets && G.type == DIFX_MESSAGE_STATUS )
                                    relayThis = true;
                                else if ( _relayStopPackets && G.type == DIFX_MESSAGE_STOP )
                                    relayThis = true;
                                else if ( _relayVex2DifxRunPackets && G.type == DIFX_MESSAGE_VEX2DIFXRUN )
                                    relayThis = true;
//       does this message type still exist?
//                                else if ( _relayWeightPackets && G.type == DIFX_MESSAGE_UNKNOWN )
//                                    relayThis = true;
                                else if ( _relayMark5StatusPackets && G.type == DIFX_MESSAGE_MARK5STATUS )
                                    relayThis = true;
                                else if ( _relayUnknownPackets && G.type == DIFX_MESSAGE_UNKNOWN )
                                    relayThis = true;
//       unrecognized message types?
//	                            DIFX_MESSAGE_DATASTREAM,
//	                            DIFX_MESSAGE_PARAMETER,
//	                            DIFX_MESSAGE_START,
//	                            DIFX_MESSAGE_MARK5VERSION,
//	                            DIFX_MESSAGE_TRANSIENT,
//	                            DIFX_MESSAGE_DRIVE_STATS,
//	                            DIFX_MESSAGE_DIAGNOSTIC,
                            }
                            if ( relayThis )
                                sendPacket( RELAY_PACKET, message, strlen( message ) );
                        }
                    }
                }
                //sched_yield();
            }
        }

        //---------------------------------------------------------------------
        //!  Thread to receive all incoming data on the socket.  Each recognized
        //!  type spawns a do-nothing function that should be overridden by
        //!  inheriting classes to actually accomplish something.  Some of the
        //!  packet types (defined above) are used only for SENDING information,
        //!  and thus don't appear here.  If for some reason they ARE received,
        //!  the "newPacket()" function will be called with the packet type,
        //!  data, and size.  This can be overridden by inheriting functions.
        //---------------------------------------------------------------------
        void newPacket( int packetId, char* data, const int nBytes ) {
            printf( "ServerSideConnection: packet id is %d size is %d\n", packetId, nBytes );
            switch( packetId ) {
                case RELAY_PACKET:
                    relay( data, nBytes );
                    break;
                case RELAY_COMMAND_PACKET:
                    relayCommand( data, nBytes );
                    break;
                case COMMAND_PACKET:
                    command( data, nBytes );
                    break;
                case MULTICAST_SETTINGS_PACKET:
                    multicastSettings( data, nBytes );
                    break;
                case GUISERVER_VERSION:
                    guiServerVersionRequest();
                    break;
                case DIFX_SETUP_PATH:
                    difxSetupPath( data, nBytes );
                    break;
                case DIFX_RUN_LABEL:
                    difxRunLabel( data, nBytes );
                    break;
                case START_DIFX_MONITOR:
                    startDifxMonitor( data, nBytes );
                    break;
                case MESSAGE_SELECTION_PACKET:
                    messageSelection( data, nBytes );
                    break;
                default:
                    break;
            }
        }
        
        //---------------------------------------------------------------------
        //!  The GUI has requested all known version information for this
        //!  guiServer instance.  This includes the guiServer version itself,
        //!  the version of DiFX for which it was compiled (which difxio is
        //!  used) and all version of DiFX software that can be run by this
        //!  guiServer instance.
        //---------------------------------------------------------------------
        virtual void guiServerVersionRequest() {
            char newVersion[DIFX_MESSAGE_LENGTH];
            //  The guiServer version and the version of DiFX which was used
            //  to compile it come from compilation defines, which isn't pretty
            //  but it works.
#ifdef VERSION
            sendPacket( GUISERVER_VERSION, VERSION, strlen( VERSION ) );
#else
            sendPacket( GUISERVER_VERSION, "unknown", strlen( "unknown" ) );
#endif
            //  The "version" of guiServer is a little tricky to get right.  We first look
            //  to see if a "label" has been defined for this DiFX build (this is part of the
            //  difxbuild process, so if you aren't using it you probably don't have one).
            const char* difxLabel = getenv( "DIFX_LABEL" );
            if ( difxLabel ) {
                sendPacket( GUISERVER_DIFX_VERSION, difxLabel, strlen( difxLabel ) );
            }
            //  Failing that, use the defined "version", which is the version of the DiFX
            //  source.  If that doesn't exist, we don't know what version this is.
            else {
#ifdef DIFX_VERSION
                sendPacket( GUISERVER_DIFX_VERSION, DIFX_VERSION, strlen( DIFX_VERSION ) );
#else
                sendPacket( GUISERVER_DIFX_VERSION, "unknown", strlen( "unknown" ) );
#endif
            }
            
            //  Send the current DiFX "base" that we are using.  This determines
            //  where the setup files for different versions exist.
            sendPacket( DIFX_BASE, _difxBase, strlen( _difxBase ) );
            
            //  Send the user name that started this guiServer process.
            char* username = getlogin();
            if ( username != NULL )
                sendPacket( GUISERVER_USER, username, strlen( username ) );
                
            //  Find all available versions of DiFX software under the base.  Any of these
            //  can be used when running DiFX programs (vex2difx, runmpifxcorr, etc.).
            char searchStr[DIFX_MESSAGE_LENGTH];
//            snprintf( searchStr, DIFX_MESSAGE_LENGTH, "%s/bin/setup_difx.*", _difxBase );
            snprintf( searchStr, DIFX_MESSAGE_LENGTH, "%s/*/setup_difx.*", _difxBase );
            glob_t globbuf;
            if ( glob( searchStr, 0, NULL, &globbuf ) ) {
                diagnostic( ERROR, "On DiFX Host \"%s\" returns empty or path does not exist\n", searchStr );
            } else {
                //  Parse the "version" out of the name of each setup file we find.  This
                //  version is then transmitted back to the GUI so it knows it is an option.
                for ( int i = 0; i < globbuf.gl_pathc; ++i ) {
                    std::string nextPath( globbuf.gl_pathv[i] );
                    int nPos = nextPath.rfind( "setup_difx." );
                    if ( nPos < nextPath.length() - strlen( "setup_difx." ) )
                        strncpy( newVersion, globbuf.gl_pathv[i] + nPos + strlen( "setup_difx." ), DIFX_MESSAGE_LENGTH );
                    else
                        newVersion[0] = 0;
//                    strncpy( newVersion, globbuf.gl_pathv[i] + strlen( searchStr ) - 1, DIFX_MESSAGE_LENGTH );
                    if ( strlen( newVersion ) > 0 )
                        sendPacket( AVAILABLE_DIFX_VERSION, newVersion, strlen( newVersion ) );
                }
            }
            globfree( &globbuf );
            
            //  Send all of the environment variables for this guiServer.  Most of these are
            //  useless to the client, but there is little harm in sending them all.
            char** env;
            for ( env = _envp; *env != 0; ++env )
                sendPacket( GUISERVER_ENVIRONMENT, *env, strlen( *env ) );
                
        }
        
        //---------------------------------------------------------------------
        //!  Parse a command from the GUI to change which packets are relayed.
        //---------------------------------------------------------------------
        void messageSelection( char* data, const int nBytes ) {
            char* nextPtr = data;
            char* thisPtr = strsep( &nextPtr, "\n" );
            _relayAlertPackets = false;
            _relayCommandPackets = false;
            _relayFileOperationPackets = false;
            _relayFileTransferPackets = false;
            _relayGetDirectoryPackets = false;
            _relayInfoPackets = false;
            _relayLoadPackets = false;
            _relayMachinesDefinitionPackets = false;
            _relayMk5ControlPackets = false;
            _relaySmartPackets = false;
            _relayStatusPackets = false;
            _relayStopPackets = false;
            _relayVex2DifxRunPackets = false;
            _relayWeightPackets = false;
            _relayMark5StatusPackets = false;
            _relayUnknownPackets = false;
            while ( thisPtr != NULL ) {
                if ( strstr( thisPtr, "ALL_MESSAGES" ) != NULL ) {
                    _relayAllPackets = true;
                }
                else if ( strstr( thisPtr, "SELECTED_MESSAGES" ) != NULL ) {
                    _relayAllPackets = false;
                } 
                else if ( strstr( thisPtr, "ALERT_MESSAGES" ) != NULL ) {
                    _relayAlertPackets = true;
                } 
                else if ( strstr( thisPtr, "COMMAND_MESSAGES" ) != NULL ) {
                    _relayCommandPackets = true;
                } 
                else if ( strstr( thisPtr, "FILEOPERATION_MESSAGES" ) != NULL ) {
                    _relayFileOperationPackets = true;
                } 
                else if ( strstr( thisPtr, "FILETRANSFER_MESSAGES" ) != NULL ) {
                    _relayFileTransferPackets = true;
                } 
                else if ( strstr( thisPtr, "GETDIRECTORY_MESSAGES" ) != NULL ) {
                    _relayGetDirectoryPackets = true;
                } 
                else if ( strstr( thisPtr, "INFO_MESSAGES" ) != NULL ) {
                    _relayInfoPackets = true;
                } 
                else if ( strstr( thisPtr, "LOAD_MESSAGES" ) != NULL ) {
                    _relayLoadPackets = true;
                } 
                else if ( strstr( thisPtr, "MACHINESDEFINITION_MESSAGES" ) != NULL ) {
                    _relayMachinesDefinitionPackets = true;
                } 
                else if ( strstr( thisPtr, "MK5CONTROL_MESSAGES" ) != NULL ) {
                    _relayMk5ControlPackets = true;
                } 
                else if ( strstr( thisPtr, "SMART_MESSAGES" ) != NULL ) {
                    _relaySmartPackets = true;
                } 
                else if ( strstr( thisPtr, "STATUS_MESSAGES" ) != NULL ) {
                    _relayStatusPackets = true;
                } 
                else if ( strstr( thisPtr, "STOP_MESSAGES" ) != NULL ) {
                    _relayStopPackets = true;
                } 
                else if ( strstr( thisPtr, "VEX2DIFXRUN_MESSAGES" ) != NULL ) {
                    _relayVex2DifxRunPackets = true;
                } 
                else if ( strstr( thisPtr, "WEIGHT_MESSAGES" ) != NULL ) {
                    _relayWeightPackets = true;
                } 
                else if ( strstr( thisPtr, "MARK5STATUS_MESSAGES" ) != NULL ) {
                    _relayMark5StatusPackets = true;
                } 
                else if ( strstr( thisPtr, "UNKNOWN_MESSAGES" ) != NULL ) {
                    _relayUnknownPackets = true;
                } 
                thisPtr = strsep( &nextPtr, "\n" );
            }
        }

        //---------------------------------------------------------------------
        //!  Override the relay function.  The relay command starts with a
        //!  single integer of data telling us to turn on (1) or off (0) the
        //!  relaying of DiFX multicast messages to the client.
        //---------------------------------------------------------------------
        virtual void relay( char* data, const int nBytes ) {
            relayDifxMulticasts( ntohl( *(int*)data ) );
        }
        
        //---------------------------------------------------------------------
        //!  This is a relayed command from the client - these are converted to UDP
        //!  messages that the DiFX processes (mk5daemon, etc) can pick up.
        //---------------------------------------------------------------------
        virtual void relayCommand( char* data, const int nBytes ) {
            //  Close the old command socket if we have new settings for the multicast network.
            if ( _newMulticastSettings ) {
                if ( _commandSocket != NULL ) {
                    _commandSocket->closeFd();
                    _commandSocket = NULL;
                }
                _newMulticastSettings = false;
            }
            //  Create a new command socket if we don't have one.
            if ( _commandSocket == NULL ) {
                if ( _multicastPort == 0 ) {
                    fprintf( stderr, "Cannot start command socket - multicast port is not defined\n" );
                    return;
                }
                if ( _multicastGroup == NULL ) {
                    fprintf( stderr, "Cannot start command socket - multicast group is not defined\n" );
                    return;
                }
                _commandSocket = new network::UDPSocket( network::UDPSocket::MULTICAST, _multicastGroup, _multicastPort );
            }
            //  Send this message to the socket assuming the socket is good.
            if ( _commandSocket != NULL ) {
                _commandSocket->writer( data, nBytes );
            }
        }

        //---------------------------------------------------------------------
        //!  This is a command from the client that guiServer should try to
        //!  execute on its own.  It uses the same XML format as relayed
        //!  commands.
        //---------------------------------------------------------------------
        virtual void command( char* data, const int nBytes ) {
            //  Use the DiFX message parser on the XML this message presumably
            //  contains.
            if ( nBytes > sizeof( DifxMessageGeneric ) ) {
                diagnostic( ERROR, "Message length (%d) is longer than allowed DiFX maximum (%d) - ignored!\n", nBytes,
                    sizeof( DifxMessageGeneric ) );
                return;
            }
            char message[sizeof( DifxMessageGeneric ) + 1];
            strncpy( message, data, nBytes );
            message[ nBytes ] = 0;
            DifxMessageGeneric G;
            if ( !difxMessageParse( &G, message ) ) {
                switch( G.type ) {
                case DIFX_MESSAGE_START:
                    startDifx( &G );
                    break;
                case DIFX_MESSAGE_STOP:
                    stopDifx( &G );
                    break;
                case DIFX_MESSAGE_FILETRANSFER:
                    difxFileTransfer( &G );
                    break;
                case DIFX_MESSAGE_FILEOPERATION:
                    difxFileOperation( &G );
                    break;
                case DIFX_MESSAGE_VEX2DIFXRUN:
                    vex2difxRun( &G );
                    break;
                case DIFX_MESSAGE_MACHINESDEFINITION:
                    machinesDefinition( &G );
                    break;
                case DIFX_MESSAGE_GETDIRECTORY:
                    getDirectory( &G );
                    break;
                case DIFX_MESSAGE_MK5CONTROL:
                    mk5Control( &G );
                    break;
                case DIFX_MESSAGE_COMMAND:
                    relayCommand( data, nBytes );
                    break;
                default:
                    diagnostic( WARNING, "Received command message type %d - don't know what this is....\n", G.type );
                }
            }
            else {
                ServerSideConnection::diagnostic( ServerSideConnection::WARNING, 
                    "The guiServer DiFX parser received a command it could not parse." );
            }
        }
        
        //---------------------------------------------------------------------
        //!  This is a packet from the client containing the port number and
        //!  group IP of the DiFX multicasts.  It will trigger a change in the
        //!  multicast monitor as well as where relayed commands go.
        //---------------------------------------------------------------------
        virtual void multicastSettings( char* data, const int nBytes ) {
            //  The first line of the string contains the group IP.  Look for the
            //  terminating newline character.
            int i = 0;
            char group[16];
            while ( data[i] != '\n' ) {
                group[i] = data[i];
                ++i;
            }
            group[i] = 0;
            //  The next line contains the new port setting as a string.
            int port = atoi( data + i + 1 );
            multicast( group, port );
        }
        
        //---------------------------------------------------------------------
        //!  Sets the group IP and port number for DiFX multicasts.  This
        //!  changes the destination for any message this class sends on the
        //!  network and restarts the monitor.
        //---------------------------------------------------------------------
        void multicast( char* group, const int port ) {
            snprintf( _multicastGroup, 16, "%s", group );
            _multicastPort = port;
            _newMulticastSettings = true;
            startMulticastMonitor();
        }
        
        //---------------------------------------------------------------------
        //!  Get the DiFX setup path from the GUI.
        //---------------------------------------------------------------------
        void difxSetupPath( char* data, const int nBytes ) {
            if ( nBytes < DIFX_MESSAGE_FILENAME_LENGTH ) {
                strncpy( _difxSetupPath, data, nBytes );
                _difxSetupPath[nBytes] = 0;
            }
        }
        
        //---------------------------------------------------------------------
        //!  Get the DiFX "label" used for running stuff from the GUI.
        //---------------------------------------------------------------------
        void difxRunLabel( char* data, const int nBytes ) {
            if ( nBytes < DIFX_MESSAGE_FILENAME_LENGTH ) {
                strncpy( _difxRunLabel, data, nBytes );
                _difxRunLabel[nBytes] = 0;
            }
        }
        
        //---------------------------------------------------------------------
        //!  This is a request from the GUI to start a new DiFX job monitor
        //!  thread.  It comes with a port number.
        //---------------------------------------------------------------------
        void startDifxMonitor( char* data, const int nBytes ) {
            DifxMonitorInfo* monitorInfo = new DifxMonitorInfo;
            monitorInfo->connectionPort = ntohl( *(int*)data );
            monitorInfo->ssc = this;
            pthread_attr_t threadAttr;
            pthread_t threadId;
            pthread_attr_init( &threadAttr );
            pthread_create( &threadId, &threadAttr, staticRunDifxMonitor, (void*)(monitorInfo) );
        }
        
        //---------------------------------------------------------------------
        //!  Types of messages - used as the "severity" in calls to the diagnostic()
        //!  function.
        //---------------------------------------------------------------------
        static const int INFORMATION      = 0;
        static const int WARNING          = 1;
        static const int ERROR            = 2;
        
        //---------------------------------------------------------------------
        //!  Toggle "DiFX alert" messages - these are sent as multicast UDP
        //!  messages and should appear in the message window of the GUI.
        //---------------------------------------------------------------------
        void difxAlertsOn( const bool newVal ) { _difxAlertsOn = newVal; }
        
        //---------------------------------------------------------------------
        //!  Toggle diagnostic packet messages - these are sent as TCP packets
        //!  directly to the GUI.
        //---------------------------------------------------------------------
        void diagnosticPacketsOn( const bool newVal ) { _diagnosticPacketsOn = newVal; }
        
        //---------------------------------------------------------------------
        //!  Relay all collected DiFX multicast messages back to the GUI via
        //!  the TCP connection.
        //---------------------------------------------------------------------
        void relayDifxMulticasts( const bool newVal ) { _relayDifxMulticasts = newVal; }
        
        //---------------------------------------------------------------------
        //!  Structure used to pass information to the thread that monitors
        //!  the messages received from running DiFX job.  This has nothing to
        //!  do with real-time monitoring of DiFX output!  That is done by the
        //!  DifxMonitorInfo struct.  Apologies for the confusing names.
        //---------------------------------------------------------------------
        struct DifxStartInfo {
            ServerSideConnection* ssc;
            JobMonitorConnection* jobMonitor;
            int force;
            int sockFd;
            char removeCommand[MAX_COMMAND_SIZE];
            char startCommand[MAX_COMMAND_SIZE];
            char jobName[MAX_COMMAND_SIZE];
            char filebase[MAX_COMMAND_SIZE];
            char inputFile[DIFX_MESSAGE_FILENAME_LENGTH];
            char difxProgram[DIFX_MESSAGE_FILENAME_LENGTH];
            int runMonitor;
        };
        
        //-----------------------------------------------------------------------------
        //!  This is the structure used to pass information used to monitor the data
        //!  products of a DiFX job in real time.
        //-----------------------------------------------------------------------------
        struct DifxMonitorInfo {
            ServerSideConnection* ssc;
            int connectionPort;
        };

        //-----------------------------------------------------------------------------
        //!  Static function called to start the DiFX run thread.
        //-----------------------------------------------------------------------------	
        static void* staticRunDifxThread( void* a ) {
            DifxStartInfo* startInfo = (DifxStartInfo*)a;
            startInfo->ssc->runDifxThread( startInfo );
            return NULL;
        }
        
        //-----------------------------------------------------------------------------
        //!  Static function called to start the DiFX monitor thread.
        //-----------------------------------------------------------------------------	
        static void* staticRunDifxMonitor( void* a ) {
            DifxMonitorInfo* monitorInfo = (DifxMonitorInfo*)a;
            monitorInfo->ssc->runDifxMonitor( monitorInfo );
            delete monitorInfo;
            return NULL;
        }
        
        //-----------------------------------------------------------------------------
        //!  Structure used to start file operations.
        //-----------------------------------------------------------------------------
        struct DifxFileOperation {
            pthread_t threadId;
            pthread_attr_t threadAttr;
            ServerSideConnection* ssc;
            DifxMessageFileOperation operation;
        };
        
        //-----------------------------------------------------------------------------
        //!  Static function to start a file operation thread.
        //-----------------------------------------------------------------------------
        static void* staticRunFileOperation( void* a ) {
            DifxFileOperation* fileOperation = (DifxFileOperation*)a;
            fileOperation->ssc->runFileOperation( fileOperation );
            delete fileOperation;
            return NULL;
        }
        
        //---------------------------------------------------------------------
        //!  Add a job to the list of running jobs.  Jobs are identified by input
        //!  file name (full path).  These should be unique.
        //---------------------------------------------------------------------
        void addRunningJob( std::string inputFile ) {
            pthread_mutex_lock( &_runningJobsMutex );
            _runningJobs.push_back( inputFile );
            pthread_mutex_unlock( &_runningJobsMutex );
        }
        
        //---------------------------------------------------------------------
        //!  Remove a job from the list of running jobs.
        //---------------------------------------------------------------------
        void removeRunningJob( std::string inputFile ) {
            pthread_mutex_lock( &_runningJobsMutex );
            bool found = false;
            for ( std::list<std::string>::iterator i = _runningJobs.begin(); i != _runningJobs.end() && !found; ++i ) {
                if ( !( i->compare( inputFile ) ) ) {
                    found = true;
                    _runningJobs.erase( i );
                }
            }
            pthread_mutex_unlock( &_runningJobsMutex );
        }
        
        //---------------------------------------------------------------------
        //!  Return whether a job exists in the list.
        //---------------------------------------------------------------------
        bool runningJobExists( std::string inputFile ) {
            pthread_mutex_lock( &_runningJobsMutex );
            bool found = false;
            for ( std::list<std::string>::iterator i = _runningJobs.begin(); i != _runningJobs.end() && !found; ++i ) {
                if ( !( i->compare( inputFile ) ) )
                    found = true;
            }
            pthread_mutex_unlock( &_runningJobsMutex );
            return found;
        }

        //---------------------------------------------------------------------
        //!  Function prototypes - guts contained in [FUNCTON_NAME].cpp files
        //!  unless otherwise noted.
        //---------------------------------------------------------------------
        void startDifx( DifxMessageGeneric* G );
        void runDifxThread( DifxStartInfo* startInfo );  //  in startDifx.cpp
        void stopDifx( DifxMessageGeneric* G );
        void runDifxMonitor( DifxMonitorInfo* monitorInfo );
        void difxFileTransfer( DifxMessageGeneric* G );
        void difxFileOperation( DifxMessageGeneric* G );
        void runFileOperation( DifxFileOperation* fileOperation );
        void getDirectory( DifxMessageGeneric* G );
        void vex2difxRun( DifxMessageGeneric* G );
        void machinesDefinition( DifxMessageGeneric* G );
        void mk5Control( DifxMessageGeneric* G );
        void diagnostic( const int severity, const char *fmt, ... );
        int popenRWE( int *rwepipe, const char *exe, const char *const argv[] );
        int pcloseRWE( int pid, int *rwepipe );
        
        const char* difxSetupPath() { return _difxSetupPath; }
        const char* difxRunLabel() { return _difxRunLabel; }

    protected:
    
        network::UDPSocket* _monitorSocket;
        network::UDPSocket* _commandSocket;
        pthread_attr_t _monitorAttr;
        pthread_t _monitorId;
        pthread_attr_t _runAttr;
        pthread_t _runId;
        bool _relayDifxMulticasts;
        bool _difxAlertsOn;
        bool _diagnosticPacketsOn;
        bool _newMulticastSettings;
        char _multicastGroup[16];
        int _multicastPort;
        char _clientIP[16];
        char _difxBase[DIFX_MESSAGE_LENGTH];
        char _difxSetupPath[DIFX_MESSAGE_FILENAME_LENGTH];
        char _difxRunLabel[DIFX_MESSAGE_FILENAME_LENGTH];
        char** _envp;
        std::list<std::string> _runningJobs;
        pthread_mutex_t _runningJobsMutex;
        
        bool _relayAllPackets;
        bool _relayAlertPackets;
        bool _relayCommandPackets;
        bool _relayFileOperationPackets;
        bool _relayFileTransferPackets;
        bool _relayGetDirectoryPackets;
        bool _relayInfoPackets;
        bool _relayLoadPackets;
        bool _relayMachinesDefinitionPackets;
        bool _relayMk5ControlPackets;
        bool _relaySmartPackets;
        bool _relayStatusPackets;
        bool _relayStopPackets;
        bool _relayVex2DifxRunPackets;
        bool _relayWeightPackets;
        bool _relayMark5StatusPackets;
        bool _relayUnknownPackets;
};

}

#endif
