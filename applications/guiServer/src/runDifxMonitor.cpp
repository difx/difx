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
//   ServerSideConnection::difxMonitor Function (and associated functions)
//
//!  This is a server for GUI connections that want real-time monitoring of
//!  running DiFX jobs.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <GUIClient.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ExecuteSystem.h>
#include <configuration.h>
#include <DifxMonitorExchange.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Thread to monitor a running DiFX job.  A connection is made to the
//!  "monitor_server" program and data products are requested.  These products
//!  are then provided to the GUI client.
//-----------------------------------------------------------------------------	
void ServerSideConnection::runDifxMonitor( DifxMonitorInfo* monitorInfo ) {
    DifxMonitorExchange* exchange = NULL;
    
    //  Start a server on the connection port and await a connection from the GUI.  
    printf( "using monitor port #%d - gui address is %s\n", monitorInfo->connectionPort, monitorInfo->addr.c_str() );
    GUIClient* client = new GUIClient( monitorInfo->ssc, monitorInfo->addr.c_str(), monitorInfo->connectionPort );

    //  We have a packet exchange mechanism to govern the connection to the GUI.
    if ( client->okay() ) {
        exchange = new DifxMonitorExchange( client, monitorInfo );

        //  One-second cycle to monitor the exchange with the GUI.  Once it is
        //  finished we will shut it down and delete it.
        while ( exchange->keepGoing() ) {

            //printf( "tick\n" );
            usleep( 1000000 );
            
        }
    
    }

    exchange->closeConnection();
    sleep( 2 );
    delete client;
    delete exchange;
    
}


