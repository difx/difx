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
#ifndef NETWORK_GENERICSOCKET_H
#define NETWORK_GENERICSOCKET_H
//==============================================================================
//
//   network::GenericSocket Class
//
//!  Contains virtual reader and writer functions shared by TCP and UDP sockets.
//
//==============================================================================
#include <pthread.h>

namespace network {

    class GenericSocket {

    public:
    
        //  Constructor sets up mutexes that can be used to lock the socket for
        //  one's exclusive read or write use.  Write is probably the more useful
        //  of these.
        GenericSocket() {
            pthread_mutex_init( &_readMutex, NULL );
            pthread_mutex_init( &_writeMutex, NULL );
        }
        
        virtual ~GenericSocket() {}

        //  Reader and writer functions both return the number of bytes
        //  successfully transfered, -1 on a failure, or 0 on a timeout
        //  (if applicable).  On failure or timeout, the _partialRead and
        //  _partialWrite variables are set to the  number of bytes sent 
        //  or received.
        virtual int reader( char* buff, int nBytes ) = 0;
        virtual int writer( const char* buff, int nBytes ) = 0;
        
        unsigned int partialRead() { return _partialRead; }
        unsigned int partialWrite() { return _partialWrite; }
        
        void readLock() {
            pthread_mutex_lock( &_readMutex );
        }
        void readUnlock() {
            pthread_mutex_unlock( &_readMutex );
        }
        void writeLock() {
            pthread_mutex_lock( &_readMutex );
        }
        void writeUnlock() {
            pthread_mutex_unlock( &_readMutex );
        }
        
    protected:
    
        unsigned int _partialRead;
        unsigned int _partialWrite;
        pthread_mutex_t _readMutex;
        pthread_mutex_t _writeMutex;

    };

}

#endif
