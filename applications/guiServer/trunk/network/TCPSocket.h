#ifndef NETWORK_TCPSOCKET_H
#define NETWORK_TCPSOCKET_H
//==============================================================================
//
//   network::TCPSocket Class
//
//   This class contains reader and writer functions for a TCP Socket, as well
//   as some socket health monitoring capability.  It can operate in two modes -
//   a straight-forward socket that reads a writes, and an actively monitored
//   socket that collects data as they become available, buffers them, and
//   generates callbacks when data become available or the socket is broken.
//
//==============================================================================
#include <strings.h>
#include <cstdio>
#include <unistd.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <pthread.h>
#include <string.h>
#include <network/GenericSocket.h>

namespace network {

    class TCPSocket : public GenericSocket {

    public:
    
        static const unsigned int READ_BLOCK_SIZE = 1024;
        static const unsigned int BUFFER_SIZE = 2 * 1024 * 1024;

        //----------------------------------------------------------------------------
        //  Note that the file descriptor should be set before read and write will
        //  work.
        //----------------------------------------------------------------------------
        TCPSocket() { 
            _fd = -1;
            _timeout = NULL;
            _monitored = false;
            _connected = false;
            _buffer = NULL;
            pthread_mutex_init( &_writeMutex, NULL );
        }

        //----------------------------------------------------------------------------
        //  Destructor closes the socket.  
        //----------------------------------------------------------------------------
        ~TCPSocket() {
            closeConnection();
            usleep( 1000 );
            if ( _buffer != NULL )
                delete[] _buffer;
            _buffer = NULL;
            if ( _fd > -1 )
                close( _fd );
        }
        
        //----------------------------------------------------------------------------
        //  Close the current connection.
        //----------------------------------------------------------------------------
        void closeConnection() {
            _connected = false;
        }

        //----------------------------------------------------------------------------
        //  File descriptor
        //----------------------------------------------------------------------------
        void setFd( int fd ) { _fd = fd; }
        int getFd() { return _fd; }
        
        //----------------------------------------------------------------------------
        //  Set the timeout for read and write operations.  There are three functions
        //  to do this...one specifying seconds and useconds, one only seconds, and
        //  one empty - indicating no timeout.
        //
        //  The "_timeoutReset" is used by the read monitor to assure that a timeout
        //  is actually applied to a read operation (this is done in the monitorThread)
        //  before the monitorReader returns believing the timeout occurred.
        //----------------------------------------------------------------------------
        void setTimeout( int seconds, int useconds ) {
            _timeout = &timeoutStruct;
            _timeout->tv_sec = seconds;
            _timeout->tv_usec = useconds;
            _timeoutReset = true;
        }
        void setTimeout( int seconds ) {
            _timeout = &timeoutStruct;
            _timeout->tv_sec = seconds;
            _timeout->tv_usec = 0;
            _timeoutReset = true;
        }

        void setTimeout() {
            _timeout = NULL;
            _timeoutReset = true;
        }


        //----------------------------------------------------------------------------
        //  Read a specified number of bytes into the given buffer.  The buffer
        //  must be previously allocated and large enough.  A timeout may be
        //  applied to the select function.  This function returns the number of
        //  bytes read, or a 0 if the select timed out, or a -1 if the read
        //  failed due to a broken socket.
        //----------------------------------------------------------------------------
        int reader( char* buff, int nBytes ) {
        
            //  Use the "monitored" read (from a buffer) if the socket is "connected".
            if ( _monitored )
                return monitorReader( buff, nBytes );
                
            fd_set rfds;
            int soFar = 0;
            int ret;
            struct timeval* localTimeout;
            struct timeval localTimeoutStruct;

            //  Continue to read until we've got our nBytes (or we get an error).
            while ( soFar < nBytes ) {

                //  Set the timeout to whatever has been specified.  We have to copy it because
                //  linux select() mangles the timeout values.
                if ( _timeout == NULL )
                    localTimeout = NULL;
                else {
                    localTimeout = &localTimeoutStruct;
                    localTimeout->tv_sec = _timeout->tv_sec;
                    localTimeout->tv_usec = _timeout->tv_usec;
                }
                //  Select
                FD_ZERO( &rfds );
                FD_SET( _fd, &rfds );
                ret = select( _fd + 1, &rfds, NULL, NULL, _timeout );
                if ( ret == -1 ) {  //  broken socket
                    _partialRead = soFar;
                    return -1;
                }
                if ( ret == 0 ) {  // timeout occurred
                    _partialRead = soFar;
                    return 0;
                }

                //  Read as much data as we can.  We will only get this far if the
                //  select returned on this socket.  This *might* occur when the
                //  socket is broken, in which case this read will return 0.
                ret = read( _fd, (void*)( buff + soFar ), nBytes - soFar );
                if ( ret == -1 || ret == 0 ) {  //  broken socket
                    _partialRead = soFar;
                    return -1;
                }

                soFar += ret;

            }

            return soFar;
        } 

        //----------------------------------------------------------------------------
        //  Writing is considerably easier than reading.  It is mutex locked to be
        //  thread safe.
        //----------------------------------------------------------------------------
        int writer( const char* buff, int nBytes ) {
            int soFar = 0;
            int ret;
            fd_set wfds;
            pthread_mutex_lock( &_writeMutex );
            while ( soFar < nBytes ) {

                FD_ZERO( &wfds );
                FD_SET( _fd, &wfds );
                ret = select( _fd + 1, NULL, &wfds, NULL, _timeout );
                if ( ret == -1 ) {  //  broken socket
                    return -1;
                }
                if ( ret == 0 ) {  // timeout occurred
                    return 0;
                }

                ret = write( _fd, (void*)( buff + soFar ), nBytes - soFar );
                if ( ret == -1 || ret == 0 ) {  //  broken socket
                    return -1;
                }

                soFar += ret;

            }

            pthread_mutex_unlock( &_writeMutex );
            return soFar;
        }
        
        //----------------------------------------------------------------------------
        //  Read the specified number of bytes out of the buffer (which is filled by
        //  the "monitored" thread).  The timeout applies to each attempt to read - a
        //  returned 0 indicates that we hit it.
        //----------------------------------------------------------------------------
        int monitorReader( char* buff, int nBytes ) {
            if ( !_connected )
                return -1;
            int soFar = 0;
            _readerHitTimeout = false;
            while ( soFar < nBytes && _connected && ( !_readerHitTimeout || _timeoutReset ) ) {
                //  The number of bytes we wish to read...
                int readN = nBytes - soFar;
                //  Are we limited by the write pointer?
                if ( _writePtrWrap == _readPtrWrap ) {
                    if ( _readPtr + readN > _writePtr )
                        readN = _writePtr - _readPtr;
                }
                //  Or the end of the buffer?
                else if ( _readPtr + readN > _bufferSize ) {
                    readN = _bufferSize - _readPtr;
                }
                //  Maybe there is nothing to read.
                if ( readN == 0 ) {
                    usleep( 10000 );
                }
                else {
                    //  Copy the data to the buffer...
                    memcpy( buff + soFar, _buffer + _readPtr, readN );
                    soFar += readN;
                    _readPtr += readN;
                    if ( _readPtr == _bufferSize ) {
                        _readPtr = 0;
                        _readPtrWrap += 1;
                    }
                }
            }
            if ( !_connected )
                return -1;
            else if ( _readerHitTimeout )
                return 0;
            else
                return soFar;
        }
        
        //----------------------------------------------------------------------------
        //  Determine whether the socket has been connected or not.  This should be
        //  set externally for the socket to "work" (i.e. for functions here to work).
        //----------------------------------------------------------------------------
        void connected( const bool newVal ) { _connected = newVal; }
        const bool connected() { return _connected; }

        //----------------------------------------------------------------------------
        //  The monitor runs a thread to do a permanent "select" on the socket, which
        //  will determine when/if it is broken.  It also reads data into a buffer,
        //  the size of which can be set here.
        //----------------------------------------------------------------------------
        void startMonitor( const unsigned int bufferSize = BUFFER_SIZE ) {
            _readBlockSize = READ_BLOCK_SIZE;
            _bufferSize = bufferSize;
            _buffer = new char[_bufferSize];
            _writePtr = 0;
            _readPtr = 0;
            _writePtrWrap = 0;
            _readPtrWrap = 0;
            _connected = true;
            _monitored = true;
            pthread_create( &_threadId, NULL, staticThreadStart, this );
        }
        
        //----------------------------------------------------------------------------
        //  This static function starts the thread for us.
        //----------------------------------------------------------------------------
        static void* staticThreadStart( void* a ) {
            ( (TCPSocket*)a )->monitorThread();
            return NULL;
        }
        
        //----------------------------------------------------------------------------
        //  This is the actual monitor thread.
        //----------------------------------------------------------------------------
        void monitorThread() {
            fd_set rfds;
            struct timeval* localTimeout;
            struct timeval localTimeoutStruct;
            long int timeoutTotal = 0;
            while ( _connected ) {
                localTimeout = &localTimeoutStruct;
                localTimeout->tv_sec = 0;
                localTimeout->tv_usec = 10000;
                //  Select
                FD_ZERO( &rfds );
                FD_SET( _fd, &rfds );
                int ret = select( _fd + 1, &rfds, NULL, NULL, localTimeout );
                if ( ret == -1 ) {  //  broken socket
                    _connected = false;
                    staticCallback( this );
                }
                else if ( ret == 0 ) {
                    //  Reset count if a new timeout has just been set.
                    if ( _timeoutReset ) {
                        timeoutTotal = 0;
                        _timeoutReset = false;
                    }
                    //  Timeout on the select.  Unless the user has specifically set a timeout,
                    //  we do nothing here.
                    if ( _timeout != NULL ) {
                        timeoutTotal += 10000;
                        if ( timeoutTotal >= _timeout->tv_sec * 1000000 + _timeout->tv_usec ) {
                            _readerHitTimeout = true;
                        }
                    }
                    else
                        timeoutTotal = 0;
                }
                else {  
                    //  Grab the read lock...
                    //pthread_mutex_lock( &_readMutex );
                    //  Actual data are available...maybe...figure how much we should
                    //  be reading.  The maximum read is the READ_BLOCK_SIZE, however
                    //  we don't want to write beyond the end of our buffer, nor overtake
                    //  the read pointer.
                    timeoutTotal = 0;
                    int readN = READ_BLOCK_SIZE;
                    //  See of the read pointer is limiting us....
                    if ( _readPtrWrap < _writePtrWrap ) {
                        if ( _writePtr + readN > _readPtr )
                            readN = _readPtr - _writePtr;
                    }
                    //  Otherwise we might be limited by the end of the buffer.
                    else if ( _writePtr + readN > _bufferSize ) {
                        readN = _bufferSize - _writePtr;
                    }
                    //  Maybe we can't read anything at all...
                    if ( readN == 0 ) {
                        //printf( "write pointer is limiting us...\n" );
                        usleep( 10000 );
                    }
                    else if ( readN < 0 ) {
                        printf( "MAYDAY, MAYDAY, MAYDAY!!!!!\n" );
                    }
                    else {
                        ret = read( _fd, (void*)(_buffer + _writePtr), readN );
                        if ( ret <= 0 ) {
                            //  Either a bad socket or an end of file.
                            _connected = false;
                        }
                        _writePtr += ret;
                        if ( _writePtr >= _bufferSize ) {
                            _writePtrWrap += 1;
                            _writePtr -= _bufferSize;
                        }
                    }
                }
            }
        }
        
        typedef void (Callback)( void* );
        Callback* _callback;
        void callback( Callback* c ) {
            _callback = c;
        }
        class staticArgs {
            void *a;
            void *arg;
        };
        static void* staticCallback( void *a ) {
            //  Extract the pointer to the class instance making this static call.
            TCPSocket* inst = (TCPSocket*)(a);
            inst->doCallback();
            return NULL;
        }
        
        void doCallback() {
            printf( "doing callback\n" );
        }

    protected:

        int _fd;
        bool _connected;
        pthread_t _threadId;
        struct timeval timeoutStruct;
        struct timeval* _timeout;
        struct sockaddr_in _connAddr;  //  used for address information
        unsigned int _readBlockSize;
        unsigned int _bufferSize;
        char* _buffer;
        unsigned int _readPtr;
        unsigned int _writePtr;
        unsigned int _readPtrWrap;
        unsigned int _writePtrWrap;
        bool _monitored;
        pthread_mutex_t _writeMutex;
        bool _readerHitTimeout;
        bool _timeoutReset;

    };

}

#endif
