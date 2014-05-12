//=============================================================================
//
//   ServerSideConnection::diagnostic Function
//
//!  This function handles diagnostic messages produced by the gui Server when
//!  trying to execute DiFX operations.  The function takes a severity as a
//!  first argument and then accepts printf-style formmatting arguments.
//!  It is a member of the ServerSideConnection class.
//
//=============================================================================
#include <ServerSideConnection.h>

using namespace guiServer;

void
ServerSideConnection::diagnostic( const int severity, const char *fmt, ... ) {
    //  Produce a new packet message using the formatting commands.  The message will
    //  be trimmed at the maximum message length.
    char message[MAX_MESSAGE_LENGTH];
    va_list ap;
    va_start( ap, fmt );
    vsnprintf( message, MAX_MESSAGE_LENGTH, fmt, ap );
    va_end( ap );
    switch ( severity ) {
    case INFORMATION:
        if ( _difxAlertsOn )
            difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_INFO );
        if ( _diagnosticPacketsOn )
            sendPacket( INFORMATION_PACKET, message, strlen( message ), channelData() );
        break;
    case WARNING:
        if ( _difxAlertsOn )
            difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
        if ( _diagnosticPacketsOn )
            sendPacket( WARNING_PACKET, message, strlen( message ), channelData() );
        break;
    case ERROR:
        if ( _difxAlertsOn )
            difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
        if ( _diagnosticPacketsOn )
            sendPacket( ERROR_PACKET, message, strlen( message ), channelData() );
        break;
    }
}


