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
//   ServerSideConnection::mk5Control Function (and associated functions)
//
//!  Called when a "mk5control" command is received from the GUI.  The command
//!  is executed and results are sent back to the GUI.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <sys/statvfs.h>
#include <GUIClient.h>
#include <ExecuteSystem.h>
#include <list>
#include <string>
#include <signal.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Execute a mk5control command in a thread.  A TCP connection allows
//!  two-way communication with the client that requested the mk5control command.
//-----------------------------------------------------------------------------
void ServerSideConnection::mark5Control( DifxMessageGeneric* G ) {

    //  Cast the argument to a Mark5Control message format (contained in difxmessage.h).
	const DifxMessageMk5Control *S = &G->body.mk5Control;
	//  Make a copy of all data in the message to the structure we use (defined in
	//  ServerSideConnection.h).
	Mark5ControlInfo* info = new Mark5ControlInfo;	
	snprintf( info->mark5Control.command, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->command );
	snprintf( info->mark5Control.targetNode, DIFX_MESSAGE_HOSTNAME_LENGTH, "%s", S->targetNode );
	snprintf( info->mark5Control.address, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->address );
	info->mark5Control.port = S->port;
	info->ssc = this;
    pthread_create( &(info->threadId), NULL, staticMark5ControlThread, (void*)info );      
}	

//-----------------------------------------------------------------------------
//!  Thread to actually run the mark5Control operation.  This thread contains
//!  a trivial bi-directional communication protocol that is used (mostly) for
//!  communicating progress to the GUI.
//-----------------------------------------------------------------------------
void ServerSideConnection::mark5ControlThread( Mark5ControlInfo* info ) {


    const DifxMessageMk5Control *S = &(info->mark5Control);

//    static const int TASK_TERMINATED                     = 100;
    static const int TASK_ENDED_GRACEFULLY               = 101;
    static const int TASK_STARTED                        = 102;
//    static const int PARAMETER_CHECK_IN_PROGRESS         = 103;
//    static const int PARAMETER_CHECK_SUCCESS             = 104;
//    static const int FAILURE_BAD_TARGETNODE              = 105;
//    static const int FAILURE_BAD_COMMAND                 = 106;
//    static const int CANCEL_COMMAND                      = 107;
    static const int INFORMATION                         = 108;
//    static const int WARNING                             = 109;
    static const int ERROR                               = 110;

    //  Open a client connection to the server that should be running for us on the
    //  remote host.
    GUIClient* gc = new GUIClient( info->ssc, S->address, S->port );
    if ( gc->okay() ) {
        gc->packetExchange();
        gc->sendPacket( TASK_STARTED, NULL, 0 );
    }
    else {
        info->ssc->diagnostic( ERROR, "client socket connection from guiServer to GUI failed - unable to execute mk5control command \"%s\"", S->command );
        return;
    }

    //  Fork a process to run the mk5control command and monitor the results.
	char message[DIFX_MESSAGE_LENGTH];
    char mk5ControlCommand[ServerSideConnection::MAX_COMMAND_SIZE];
    snprintf( mk5ControlCommand, ServerSideConnection::MAX_COMMAND_SIZE, "%s mk5control %s %s",
            info->ssc->difxSetupPath(), S->command, S->targetNode );
    ExecuteSystem* executor = new ExecuteSystem( mk5ControlCommand );
    if ( executor->pid() > -1 ) {
        while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
            if ( ret == 1 ) { // stdout
                int len = strlen( message );
                if ( len >= 0 ) {
                    gc->formatPacket( INFORMATION, "%s\n", message );
                }
            }
            else { // error of some sort
                int len = strlen( message );
                if ( len >= 0 ) {
                    gc->formatPacket( ERROR, "%s\n", message );
                }
            }
        }
    }

    //  Healthy process end...
    gc->sendPacket( TASK_ENDED_GRACEFULLY, NULL, 0 );
    sleep( 1 );
    delete gc;
		
}


