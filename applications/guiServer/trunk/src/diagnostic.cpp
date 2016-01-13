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
            sendPacket( INFORMATION_PACKET, message, strlen( message ) );
        break;
    case WARNING:
        if ( _difxAlertsOn )
            difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
        if ( _diagnosticPacketsOn )
            sendPacket( WARNING_PACKET, message, strlen( message ) );
        break;
    case ERROR:
        if ( _difxAlertsOn )
            difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
        if ( _diagnosticPacketsOn )
            sendPacket( ERROR_PACKET, message, strlen( message ) );
        break;
    }
}


