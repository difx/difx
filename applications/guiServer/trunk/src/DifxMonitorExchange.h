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
#ifndef GUISERVER_DIFXMONITOREXCHANGE_H
#define GUISERVER_DIFXMONITOREXCHANGE_H
//=============================================================================
//
//   guiServer::DifxMonitorExchange Class
//
//!  Handles a server TCP connection with the GUI to send it monitoring data
//!  and diagnostics from a connection to monitor_server.  This class is
//!  designed to be used by the runDifxMonitor function.
//
//=============================================================================
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <ExecuteSystem.h>
#include <configuration.h>
#include <GUIClient.h>
#include <ServerSideConnection.h>
#include <vector>

namespace guiServer {

    class DifxMonitorExchange {
    
    public:

        //-----------------------------------------------------------------------------
        //!  Packet types used by this exchange.  
        //-----------------------------------------------------------------------------
        static const int MESSAGE                            = 100;
        static const int WARNING                            = 101;
        static const int ERROR                              = 102;
        static const int INPUT_FILE_PATH                    = 103;
        static const int CLOSE_CONNECTION                   = 104;
        static const int NUM_BASELINES                      = 105;
        static const int NUM_FREQUENCIES                    = 106;
        static const int BASELINE                           = 107;
        static const int FREQUENCY                          = 108;
        static const int NUM_SCANS                          = 109;
        static const int SCAN                               = 110;
        static const int TELESCOPE_1                        = 111;
        static const int TELESCOPE_2                        = 112;
        static const int BEGIN_CORRELATION_PRODUCTS         = 113;
        static const int NUM_PHASE_CENTERS                  = 114;
        static const int PHASE_CENTER                       = 115;
        static const int NUM_PULSAR_BINS                    = 116;
        static const int PULSAR_BIN                         = 117;
        static const int NUM_POL_PRODUCTS                   = 118;
        static const int POL_PRODUCT                        = 119;
        static const int NEW_PRODUCT                        = 120;
        static const int AUTOCORRELATION                    = 121;
        static const int PRODUCT_REQUEST                    = 122;
        static const int START_PRODUCT_REQUESTS             = 123;
        static const int END_PRODUCT_REQUESTS               = 124;
        static const int VISIBILITY_DATA                    = 125;
        static const int AMPLITUDE_DATA                     = 126;
        static const int PHASE_DATA                         = 127;
        static const int LAG_DATA                           = 128;
        static const int END_VISIBILITY_BLOCK               = 129;
        static const int JOB_NAME                           = 130;
        static const int OBS_CODE                           = 131;
        static const int SCAN_IDENTIFIER                    = 132;
        static const int SCAN_START_TIME                    = 133;
        static const int SCAN_END_TIME                      = 134;
        static const int SOURCE                             = 135;
        static const int SOURCE_RA                          = 136;
        static const int SOURCE_DEC                         = 137;
        static const int VISIBILITY_SCAN                    = 138;
        static const int FFT_SIZE                           = 139;
        static const int MEAN_AMPLITUDE_DATA                = 140;
        static const int MEAN_PHASE_DATA                    = 141;
        static const int MEAN_LAG_DATA                      = 142;
        static const int END_CORRELATION_PRODUCTS           = 143;
    
        DifxMonitorExchange( GUIClient* guiClient, ServerSideConnection::DifxMonitorInfo* monitorInfo ) {
            _keepGoing = true;
            _visConnectionOperating = false;
            _ssc = monitorInfo->ssc;
            _receiveActive = false;
            _guiClient = guiClient;
            _guiClient->packetExchange(); 
            pthread_create( &_receiveId, NULL, staticReceiveThread, this );
        }
        
        virtual ~DifxMonitorExchange() {
            _receiveActive = false;
        }
        
        //---------------------------------------------------------------------
        //!  Static start function for the receive thread.
        //---------------------------------------------------------------------
        static void* staticReceiveThread( void* a ) {
            ( (DifxMonitorExchange*)a )->receiveThread();
            return NULL;
        }
        
        //---------------------------------------------------------------------
        //!  Thread to receive all incoming data on the socket.  Each received
        //!  packet spawns the do-nothing function "newPacket()" with type ID
        //!  byte size, and data content.  This function should be overridden by
        //!  inheriting classes to actually accomplish something.
        //---------------------------------------------------------------------
        void receiveThread() {
            _receiveActive = true;
            while ( _receiveActive ) {
                int packetId = 0;
                int nBytes;
                char* data = NULL;
                if ( _guiClient->getPacket( packetId, data, nBytes ) == -1 ) {
                    //  connection failure
                    _receiveActive = false;
                }
                else {
                    newPacket( packetId, data, nBytes );
                    //  Free the space allocated to this incoming message.  This
                    //  means the newPacket function must copy it if necessary.
                    delete [] data;
                }
            }
        }
        
        //-----------------------------------------------------------------------------
        //!  Handle all of the packet types specific to this exchange.
        //-----------------------------------------------------------------------------
        virtual void newPacket( int packetId, char* data, const int nBytes ) {
            switch( packetId ) {
                case INPUT_FILE_PATH:
                    inputFilePath( data, nBytes );
                    break;
                case CLOSE_CONNECTION:
                    closeConnection();
                    break;
                case START_PRODUCT_REQUESTS:
                    startProductRequests( data, nBytes );
                    break;
                case PRODUCT_REQUEST:
                    productRequest( data, nBytes );
                    break;
                case END_PRODUCT_REQUESTS:
                    endProductRequests( data, nBytes );
                    break;
                case FFT_SIZE:
                    if ( nBytes == 4 )
                        _fftSize = ntohl( *(int*)data );
                    break;
                default:
                    break;
            }
        }
        
        //-----------------------------------------------------------------------------
        //!  Respond to a GUI instruction to close this connection.
        //-----------------------------------------------------------------------------
        void closeConnection() {
            _keepGoing = false;
            _receiveActive = false;
            sleep( 2 );
            if ( _monitorServerClient != NULL )
                delete _monitorServerClient;
            _monitorServerClient = NULL;
            _keepGoing = false;
        }
        
        //-----------------------------------------------------------------------------
        //!  Use a new input file path.  This triggers a connection to the monitor_server
        //!  program (which is started if necessary).  The input file is then used to
        //!  list data products that are available from monitor_server, information that
        //!  is sent to the GUI.
        //-----------------------------------------------------------------------------
        void inputFilePath( char* data, const int nBytes ) {
	        char message[DIFX_MESSAGE_LENGTH];

            //  See if the "monitor_server" program is running (with the proper port).  If not,
            //  start it up.
            int monitorServerPort = 9999;
            char monCommand[ServerSideConnection::MAX_COMMAND_SIZE];
            snprintf( monCommand, ServerSideConnection::MAX_COMMAND_SIZE, 
                "/bin/ps -ef | /bin/grep -E \'monitor_server\\s|monitor_server$\' | /bin/grep -v grep" );
            ExecuteSystem* executor = new ExecuteSystem( monCommand );
            bool found = false;
            if ( executor->pid() > -1 ) {
                while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                    if ( ret == 1 ) { // stdout
                        //  Any instance of monitor_server should show up as a result of this
                        //  "ps" command.  Monitor_server is started with the port number as
                        //  the last (and only) argument.
                        int portNum = -1;
                        int len = strlen( message );
                        while ( len >= 0 ) {
                            if ( message[len] == ' ' ) {
                                portNum = atoi( message + len + 1 );
                                break;
                            }
                            --len;
                        }
                        if ( portNum == monitorServerPort ) {
                            _guiClient->formatPacket( MESSAGE, "located monitor_server instance: %s\n", message );
                            found = true;
                        }
                    }
                }
            }
            //  Start the monitor_server if we have not found it.
            if ( !found ) {
                snprintf( monCommand, ServerSideConnection::MAX_COMMAND_SIZE,
                     "%s monitor_server %d &> /dev/null &", _ssc->difxSetupPath(), monitorServerPort );
                _guiClient->formatPacket( WARNING, "monitor_server needs to be started - executing: %s\n", monCommand );
                int ret = system( monCommand );
                if ( ret < 0 )
                    _guiClient->formatPacket( ERROR, "monitor_server had trouble starting - real time monitoring will not work" );
                //  Delay a bit to allow the monitor_server to start up.
                sleep( 1 );
            }

            //  Make a new client connection to the monitor_server, which *should* now be running.
            //  At the moment the port is hard-wired to 52300 which might not be a perfect
            //  long term solution.  The host is the local host (so I'm using the loopback
            //  address).
            _monitorServerClient = new network::TCPClient( "127.0.0.1", 52300 );
            _monitorServerClient->waitForConnect();
            if ( _monitorServerClient->connected() ) {
                int status;
                _monitorServerClient->reader( (char*)(&status), sizeof( int ) );
                if ( status == 0 ) {
                    _guiClient->formatPacket( MESSAGE, "connection established with monitor_server" );
                }
                else {
                    _guiClient->formatPacket( ERROR, "monitor_server returned initial status failure (%d) - real time monitor will not run", status );
                    _keepGoing = false;
                }
            }
            else {
                _guiClient->formatPacket( ERROR, "connection with monitor_server failed - real time monitor will not run" );
                _keepGoing = false;
            }

            //  Use the DiFX input file parser to find out what data products are available from this job.
            if ( _keepGoing ) {
                _config = new Configuration( data, 0 );
                if ( !_config->consistencyOK() ) {
                    _guiClient->formatPacket( ERROR, "Configuration had a problem parsing input file %s - real time monitor will not run",
                        data );
                    _keepGoing = false;
                }
                else {
                    //  Send all information we know about the data products.  This is all obtained from
                    //  the input file.  I grabbed this code (mostly) from the monserver_productconfig()
                    //  function in difx_monitor/monserver.cpp.  Without knowing entirely what I was doing.
                    
                    //  We save a detailed list of product information, indexed by the "product number",
                    //  which will be defined and incremented below.
                    _productInfo.clear();
                    
                    //  Indicate we are starting a new block of correlation products
                    _guiClient->sendPacket( BEGIN_CORRELATION_PRODUCTS, NULL, 0 );
                    
                    int nScans = _config->getModel()->getNumScans();
                    _guiClient->intPacket( NUM_SCANS, &nScans );
                    _guiClient->formatPacket( JOB_NAME, "%s", _config->getJobName().c_str() );
                    _guiClient->formatPacket( OBS_CODE, "%s", _config->getObsCode().c_str() );
                    
                    //  Send information on correlation products for all scans.
                    for ( int currentScan = 0; currentScan < nScans; ++currentScan ) {
                        _guiClient->intPacket( SCAN, &currentScan );
                        _guiClient->formatPacket( SCAN_IDENTIFIER, "%s", _config->getModel()->getScanIdentifier( currentScan ).c_str() );
                        _guiClient->formatPacket( SCAN_START_TIME, "%.7f", _config->getModel()->getScanStartMJD( currentScan ) );
                        _guiClient->formatPacket( SCAN_END_TIME, "%.7f", _config->getModel()->getScanEndMJD( currentScan ) );
                        _guiClient->formatPacket( SOURCE, "%s", _config->getModel()->getScanPointingCentreSource( currentScan )->name.c_str() );
                        _guiClient->formatPacket( SOURCE_RA, "%.15f", _config->getModel()->getScanPointingCentreSource( currentScan )->ra );
                        _guiClient->formatPacket( SOURCE_DEC, "%.15f", _config->getModel()->getScanPointingCentreSource( currentScan )->dec );
                        int configindex = _config->getScanConfigIndex( currentScan );
                        char polpair[3];
                        polpair[2] = 0;

                        int nBaselines = _config->getNumBaselines();
                        _guiClient->intPacket( NUM_BASELINES, &nBaselines );        
                        
                        int productN = 0;            

                        for ( int i = 0; i < nBaselines; i++ ) {
                            _guiClient->intPacket( BASELINE, &i );
                            int ds1index = _config->getBDataStream1Index( configindex, i );
                            int ds2index = _config->getBDataStream2Index( configindex, i );
                            _guiClient->formatPacket( TELESCOPE_1, "%s", _config->getTelescopeName( ds1index ).c_str() );
                            _guiClient->formatPacket( TELESCOPE_2, "%s", _config->getTelescopeName( ds2index ).c_str() );
                            
                            int nFrequencies = _config->getBNumFreqs( configindex, i );
                            _guiClient->intPacket( NUM_FREQUENCIES, &nFrequencies );

                            for( int j = 0; j < nFrequencies; j++ ) {
                                int freqindex = _config->getBFreqIndex( configindex, i, j );
                                double frequency = _config->getFreqTableFreq( freqindex );
                                _guiClient->doublePacket( FREQUENCY, &frequency );
                                int resultIndex = _config->getCoreResultBaselineOffset( configindex, freqindex, i );
                                int freqchannels = _config->getFNumChannels( freqindex ) / _config->getFChannelsToAverage( freqindex );
                                
                                int nPhaseCenters = _config->getMaxPhaseCentres( configindex );
                                _guiClient->intPacket( NUM_PHASE_CENTERS, &nPhaseCenters );

                                for( int s = 0; s < nPhaseCenters; s++ ) {
                                    _guiClient->intPacket( PHASE_CENTER, &s );
                                    
                                    int binloop = 1;
                                    if ( _config->pulsarBinOn( configindex ) && !_config->scrunchOutputOn( configindex ) )
                                        binloop = _config->getNumPulsarBins( configindex );
                                    _guiClient->intPacket( NUM_PULSAR_BINS, &binloop );
                                    
	                                for( int b = 0; b < binloop; b++ ) {
	                                    _guiClient->intPacket( PULSAR_BIN, &b );
	                                    
	                                    int nPolProducts = _config->getBNumPolProducts( configindex, i, j );
	                                    _guiClient->intPacket( NUM_POL_PRODUCTS, &nPolProducts );
	                                    
	                                    for( int k = 0; k < nPolProducts; k++ ) {
	                                        _guiClient->intPacket( POL_PRODUCT, &k );
	                                        
	                                        _config->getBPolPair( configindex, i, j, k, polpair );

                                            //  Send the number, resultIndex (offset) and frequency channels for this
                                            //  product.
                                            int productData[3];
                                            productData[0] = productN;
                                            productData[1] = resultIndex;
                                            productData[2] = freqchannels;
                                            _guiClient->intPacket( NEW_PRODUCT, productData, 3 );
                                            
                                            //  Add everything we know about this product to the product information list.
                                            _productInfo.push_back( ProductInfo( ds1index,
                                                                                 ds2index,
                                                                                 _config->getTelescopeName( ds1index ), 
					                                                             _config->getTelescopeName( ds2index ),
					                                                             _config->getFreqTableFreq( freqindex ),
					                                                             _config->getFreqTableBandwidth( freqindex ),
					                                                             polpair,
					                                                             s,
					                                                             b,
					                                                             resultIndex,
					                                                             freqchannels,
					                                                             _config->getFreqTableLowerSideband( freqindex ) ) );
                                            ++productN;
                                        }
                                    }
                                }
                            }
                        }

                        //  Auto correlations
                        int autocorrwidth = ( _config->getMaxProducts() > 2 )? 2: 1;

                        for( int i = 0; i < _config->getNumDataStreams(); i++ ) {
                            int offset = _config->getCoreResultAutocorrOffset( configindex, i );

                            for( int j = 0; j < autocorrwidth; j++ ) {
                                for( int k=0; k < _config->getDNumRecordedBands( configindex, i ); k++ ) {

                                    _guiClient->formatPacket( AUTOCORRELATION, "%s", _config->getDStationName( configindex, i ).c_str() );

                                    polpair[0] = _config->getDRecordedBandPol( configindex, i, k );
                                    if ( j==0 )
                                        polpair[1] = polpair[0];
                                    else
                                        polpair[1] = ( polpair[0] == 'R' )? 'L': 'R';

                                    int freqindex = _config->getDRecordedFreqIndex( configindex, i, k );
                                    double frequency = _config->getFreqTableFreq( freqindex );
                                    _guiClient->doublePacket( FREQUENCY, &frequency );
                                    int freqchannels = _config->getFNumChannels( freqindex ) / _config->getFChannelsToAverage( freqindex );

                                    int productData[3];
                                    productData[0] = productN;
                                    productData[1] = offset;
                                    productData[2] = freqchannels;
                                    _guiClient->intPacket( NEW_PRODUCT, productData, 3 );
                                    _productInfo.push_back( ProductInfo( i,
                                         i,
                                         _config->getDStationName( configindex, i ), 
                                         "",
                                         _config->getFreqTableFreq( freqindex ),
                                         _config->getFreqTableBandwidth( freqindex ),
                                         polpair,
                                         0,
                                         0,
                                         offset,
                                         freqchannels,
                                         _config->getFreqTableLowerSideband( freqindex ) ) );
                                    ++productN;
                                    offset += freqchannels;

                                }
                            }
                        }                    
                    }

                    //  This packet indicates that we are done sending the correlation products
                    //  information.
                    _guiClient->sendPacket( END_CORRELATION_PRODUCTS, NULL, 0 );
                }     
            }
            
        }
        
        //-----------------------------------------------------------------------------
        //!  This signifies the start of a new list of product requests.  We clear the
        //!  list we have.
        //-----------------------------------------------------------------------------
        void startProductRequests( char* data, const int nBytes ) {
            _productList.clear();
        }
        
        //-----------------------------------------------------------------------------
        //!  Respond to a GUI product request.
        //-----------------------------------------------------------------------------
        void productRequest( char* data, const int nBytes ) {
            int index = ntohl( *(int*)data );
            int offset = ntohl( *(int*)(data + 4) );
            int numFrequencies = ntohl( *(int*)(data + 8) );
            _productList.push_back( Product( index, offset, numFrequencies ) );
        }
        
        //-----------------------------------------------------------------------------
        //!  Respond to a GUI product request.
        //-----------------------------------------------------------------------------
        void endProductRequests( char* data, const int nBytes ) {
            if ( _productList.size() > 0 ) {
                //  See if we need to make a new connection to the monitor server.  This might happen if a request
                //  was already made, run through, and timed out.
                if ( _monitorServerClient == NULL ) {
                    _monitorServerClient = new network::TCPClient( "127.0.0.1", 52300 );
                    _monitorServerClient->waitForConnect();
                    if ( _monitorServerClient->connected() ) {
                        int status;
                        _monitorServerClient->reader( (char*)(&status), sizeof( int ) );
                        if ( status == 0 ) {
                            _guiClient->formatPacket( MESSAGE, "connection established with monitor_server" );
                        }
                        else {
                            _guiClient->formatPacket( ERROR, "monitor_server returned initial status failure (%d) - real time monitor will not run", status );
                            _keepGoing = false;
                        }
                    }
                    else {
                        _guiClient->formatPacket( ERROR, "connection with monitor_server failed - real time monitor will not run" );
                        _keepGoing = false;
                    }
                }
                if ( _keepGoing ) {
                    //  Write the number of products we are requesting...
                    int nProducts = _productList.size();
                    _monitorServerClient->writer( (char*)&nProducts, sizeof( int ) );
                    //  Send an individual request for each data product.
                    for ( std::vector<Product>::iterator i = _productList.begin(); i != _productList.end(); ++i ) {
                        printf( "requesting product - index is %d, offset is %d, number of frequencies is %d\n",
                            i->index, i->offset, i->numFrequencies );
                        _monitorServerClient->writer( (char*)&(i->offset), sizeof( int ) );
                        _monitorServerClient->writer( (char*)&(i->numFrequencies), sizeof( int ) );
                        _monitorServerClient->writer( (char*)&(i->index), sizeof( int ) );
                    }
                    int status;
                    _monitorServerClient->reader( (char*)(&status), sizeof( int ) );
                    if ( status == 0 ) {
                        _guiClient->formatPacket( MESSAGE, "data product requests sent to monitor_server" );
                        //  Start the thread that reads visibilities (assuming its not already running!).
                        if ( _visConnectionOperating )
                            _guiClient->formatPacket( WARNING, "a thread is already monitoring data products - a new one cannot be started!" );
                        else {
                            pthread_attr_init( &_monitorAttr );
                            pthread_create( &_monitorId, &_monitorAttr, staticVisibilityMonitor, this );      
                        }
                    }
                    else {
                        _guiClient->formatPacket( ERROR, "monitor_server returned failure status (%d) after data product requests", status );
                        _keepGoing = false;
                    }
                }
            }
        }
        
        //---------------------------------------------------------------------
        //!  Static thread start function for the visibility reader.
        //---------------------------------------------------------------------
        static void* staticVisibilityMonitor( void* a ) {
            ( (DifxMonitorExchange*)a )->visibilityMonitor();
            return NULL;
        }
        
        //---------------------------------------------------------------------
        //!  This is the visibility read thread, started using the static
        //!  function above.
        //---------------------------------------------------------------------
        void visibilityMonitor() {
            _visConnectionOperating = true;
            int timeStamp;
            int integrationTime;
            int nReturn;
            int buffSize = 0;
            int newBuffSize;
            char* buff = NULL;
            int procChannels = 0;
            double delay = 0.0;
            double snr = 0.0;
            int activeScan = -1;
            int meanCounter = 0;
            Ipp64f* amp = NULL;
            Ipp64f* phase = NULL;
            Ipp64f* lags = NULL;
            Ipp64f* delayLags = NULL;
            Ipp64fc* vis64 = NULL;
            Ipp64fc* fftVis64 = NULL;
            Ipp64fc* meanVis64 = NULL;
            Ipp64fc* meanfftVis64 = NULL;
            IppsFFTSpec_R_64f* fftspec = NULL;
	    u8 *fftworkbuf = NULL;
	    int wbufsize = 0;
            int threadTimeoutCount = 0;
            bool threadTimeoutActive = false;
            int threadTimeoutModelCount = 0;
            bool threadTimeoutInitialized = false;
            Ipp64fc complexZero;
            complexZero.re = 0.0;
            complexZero.im = 0.0;
            while( _keepGoing && _visConnectionOperating ) {
                //  Read the next block of visibility data.  We put a timeout on this to kill
                //  the thread when the user isn't doing anything.
                _monitorServerClient->setTimeout( 1 );
                int ret = 0;
                if ( _keepGoing )
                    ret = _monitorServerClient->reader( (char*)&timeStamp, sizeof( int ) );
                _monitorServerClient->setTimeout();
                //  The thread timeout scheme is "activated" by the first data to be received.
                //  It uses the "model" count (three times the time between the first two data blocks)
                //  to estimate how long it should wait between blocks of data before it gives up and
                //  terminates this thread.  This is far from fool-proof, but it should cut down on the
                //  number of these threads that remain running.
                if ( ret == 0 ) {
                    if ( threadTimeoutActive ) {
                        ++threadTimeoutCount;
                        if ( threadTimeoutCount > 3 * threadTimeoutModelCount ) {
                            if ( _keepGoing )
                                _guiClient->formatPacket( WARNING, "real-time monitor thread has timed out after %d seconds of inactivity", 2 * threadTimeoutModelCount );
                            _visConnectionOperating = false;
                        }
                    }
                    else if ( threadTimeoutInitialized )
                        ++threadTimeoutModelCount;
                }
                else if ( ret < 0 ) {
                    _visConnectionOperating = false;
                    //  Pause for a moment here to avoid a double free - if the connection was specifically
                    //  severed by a client "CLOSE_CONNECTION" message we need to give the closeConnection()
                    //  function time to work.
                    sleep( 1 );
                    if ( _keepGoing )
                        _guiClient->formatPacket( ERROR, "problem with socket connection to monitor_server: %d returned", ret );
                }
                else {
                    threadTimeoutCount = 0;
                    if ( !threadTimeoutInitialized ) {
                        threadTimeoutInitialized = true;
                        threadTimeoutModelCount = 0;
                    }
                    else
                        threadTimeoutActive = true;
                    //  Compare the time stamp to the range for each scan in this job so we can figure
                    //  out which scan we are looking at.  
                    int nScans = _config->getModel()->getNumScans();
                    for ( int currentScan = 0; currentScan < nScans; ++currentScan ) {
                        if ( timeStamp >= _config->getStartSeconds() + _config->getModel()->getScanStartSec( currentScan, _config->getStartMJD(), _config->getStartSeconds() ) &&
                             timeStamp < _config->getStartSeconds() + _config->getModel()->getScanEndSec( currentScan, _config->getStartMJD(), _config->getStartSeconds() ) ) {
                            //  Find the integration time (the size of each accumulation period) for this scan.
                            integrationTime = lrint( _config->getIntTime( _config->getScanConfigIndex( currentScan ) ) );
                            _guiClient->formatPacket( VISIBILITY_SCAN, "%s", _config->getModel()->getScanIdentifier( currentScan ).c_str() );
                            _guiClient->formatPacket( SOURCE, "%s", _config->getModel()->getScanPointingCentreSource( currentScan )->name.c_str() );
                            //  If this is a new scan, zero the "mean" counter.
                            if ( currentScan != activeScan ) {
                                activeScan = currentScan;
                                meanCounter = 0;
                            }
	                        ++meanCounter;
                        }
                    }
                    _monitorServerClient->reader( (char*)&nReturn, sizeof( int ) );
                    _monitorServerClient->reader( (char*)&newBuffSize, sizeof( int ) );
                    if ( buffSize < newBuffSize ) {
                        buffSize = newBuffSize;
                        if ( buff != NULL )
                            delete [] buff;
                        buff = new char[buffSize];
                    }
                    int ret = _monitorServerClient->reader( buff, buffSize );
                    if ( ret != buffSize ) {
                        _visConnectionOperating = false;
                        _guiClient->formatPacket( ERROR, "problem with visibility data from monitor_server: %d out of %d bytes returned", ret, buffSize );
                    }
                    else {
                        //  These are raw complex data - no effort to change byte order, etc.
                        //sendPacket( VISIBILITY_DATA, buff, buffSize );
                        //  Extract the number of channels, the product numbers, and the actual visibility data
                        //  from the just-received data buffer.  This is done for each product contained in the
                        //  data.
                        int buffOffset = 0;
                        int iProduct = 0;
                        Ipp32fc* vis32 = NULL;
                        
                        for ( int iVis = 0; iVis < nReturn; ++iVis ) {
                            int nChannels = *(int32_t*)(buff + buffOffset);
                            buffOffset += sizeof( int32_t );
                            iProduct = *(int32_t*)(buff + buffOffset);
                            buffOffset += sizeof( int32_t );
                            vis32 = (Ipp32fc*)(buff + buffOffset);
                            buffOffset += nChannels * sizeof( Ipp32fc );
                            //visBuff = (char*)(buff + buffOffset);
                            //buffOffset += nChannels * 2 * sizeof( int );
                            //  Use the FFT size specified by the user unless it is smaller than the data.
                            int useFFTSize = _fftSize;
                            if ( useFFTSize < nChannels * 2 )
                                useFFTSize = nChannels * 2;
                            //  Allocate vectors to hold and process the visibilities.  We only need to do this if
                            //  the size of our arrays (determined by nChannels) changes.
                            if ( procChannels != nChannels ) {
                                procChannels = nChannels;
                                if ( vis64 != NULL )
                                    ippsFree( vis64 );
                                if ( amp != NULL )
                                    ippsFree( amp );
                                if ( phase != NULL )
                                    ippsFree( phase );
                                if ( lags != NULL )
                                    ippsFree( lags );
                                if ( delayLags != NULL )
                                    ippsFree( delayLags );
                                if ( fftspec != NULL )
#ifdef IPP_9_API
                                    ippsFree( fftspec );
				    ippsFree( fftworkbuf );
#else
                                    ippsFFTFree_R_64f( fftspec );
#endif
                                vis64 = ippsMalloc_64fc( nChannels );
                                meanVis64 = ippsMalloc_64fc( nChannels );
                                //  For each product....
                                amp = ippsMalloc_64f( nChannels );
                                phase = ippsMalloc_64f( nChannels );
//                                lags = ippsMalloc_64f( nChannels * 2 );
//                                delayLags = ippsMalloc_64f( nChannels * 2 );
/**/                                lags = ippsMalloc_64f( useFFTSize );
/**/                                delayLags = ippsMalloc_64f( useFFTSize );
                                fftVis64 = ippsMalloc_64fc( useFFTSize / 2 );
                                meanfftVis64 = ippsMalloc_64fc( useFFTSize / 2 );
                                if ( amp == NULL || phase == NULL || lags == NULL ) {
                                    _guiClient->formatPacket( ERROR, "Failure to allocate memory for visibility processing arrays." );
                                    _visConnectionOperating = false;
                                    break;
                                }
                                int order = 0;
//                                while( ( ( nChannels * 2 ) >> order ) > 1 )
/**/                                while( ( ( useFFTSize ) >> order ) > 1 )
                                    order++;
#ifdef IPP_9_API
				{
				  int sizeFFTSpec, sizeFFTInitBuf;
				  u8 *fftInitBuf, *fftSpecBuf;

				  ippsFFTGetSize_R_64f(order, IPP_FFT_NODIV_BY_ANY, ippAlgHintNone, &sizeFFTSpec, &sizeFFTInitBuf, &wbufsize);
				  fftSpecBuf = ippsMalloc_8u(sizeFFTSpec);
				  fftInitBuf = ippsMalloc_8u(sizeFFTInitBuf);
				  fftworkbuf = ippsMalloc_8u(wbufsize);
				  ippsFFTInit_R_64f( &fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintNone, fftSpecBuf, fftInitBuf);
				}
#else
                                ippsFFTInitAlloc_R_64f( &fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast );
#endif
                            }
                            //  Put the visibility data into double-precision complex arrays.
	                        ippsSet_64fc( complexZero, fftVis64, useFFTSize / 2 );
	                        if ( meanCounter == 0 )
	                            ippsSet_64fc( complexZero, meanfftVis64, useFFTSize / 2 );
	                        for ( int i = 0; i < nChannels; i++ ) {
	                          vis64[i].re = vis32[i].re;
	                          vis64[i].im = vis32[i].im;
	                          fftVis64[i].re = vis32[i].re;
	                          fftVis64[i].im = vis32[i].im;
	                          //  Collect mean values.
	                          if ( meanCounter > 1 ) {
	                              meanVis64[i].re *= (double)(meanCounter - 1);
	                              meanVis64[i].im *= (double)(meanCounter - 1);
	                              meanfftVis64[i].re *= (double)(meanCounter - 1);
	                              meanfftVis64[i].im *= (double)(meanCounter - 1);
	                              meanVis64[i].re += vis32[i].re;
	                              meanVis64[i].im += vis32[i].im;
	                              meanfftVis64[i].re += vis32[i].re;
	                              meanfftVis64[i].im += vis32[i].im;
	                          }
	                          else {
	                              meanVis64[i].re = vis32[i].re;
	                              meanVis64[i].im = vis32[i].im;
	                              meanfftVis64[i].re = vis32[i].re;
	                              meanfftVis64[i].im = vis32[i].im;
	                          }
	                        }
	                        //  Adjust mean values.
	                        for ( int i = 0; i < nChannels; i++ ) {
                                meanVis64[i].re /= (double)meanCounter;
                                meanVis64[i].im /= (double)meanCounter;
                                meanfftVis64[i].re /= (double)meanCounter;
                                meanfftVis64[i].im /= (double)meanCounter;
                            }
	                        //  Compute amplitude, phase, lags...
                            ippsMagnitude_64fc( vis64, amp, nChannels );
                            ippsPhase_64fc( vis64, phase, nChannels );
                            ippsMulC_64f_I( 180.0/M_PI, phase, nChannels );
//                            ippsFFTInv_CCSToR_64f( (Ipp64f*)vis64, lags, fftspec, 0 );
/**/                            ippsFFTInv_CCSToR_64f( (Ipp64f*)fftVis64, lags, fftspec, 0 );
                            //  Generate a delay calculation...(swiped from vcal.cpp)
                            Ipp64f max = 0.0;
                            Ipp64f stddev = 0.0;
                            int imax = 0;
                            int i1 = 0;
                            int i2 = 0;
                            /*
                            ippsAbs_64f( lags, delayLags, nChannels * 2 );
                            ippsMaxIndx_64f( delayLags, nChannels * 2, &max, &imax );
                            i1 = ( imax - 10 + nChannels * 2 ) % ( nChannels * 2 );
                            i2 = ( imax + 10 ) % ( nChannels * 2 );
                            if ( i1 < i2 ) {
                                ippsMove_64f( &delayLags[i2], &delayLags[i1], nChannels * 2 - i2 );
                            } else {
                                ippsMove_64f( &delayLags[i2], delayLags, nChannels * 2 - 20 );    
                            }
                            ippsStdDev_64f( delayLags, nChannels * 2 - 20, &stddev );
                            ippsAbs_64f( lags, delayLags, nChannels * 2 );
                            */
                            ippsAbs_64f( lags, delayLags, useFFTSize );
                            ippsMaxIndx_64f( delayLags, useFFTSize, &max, &imax );
                            int windowSize = useFFTSize / 20;
                            i1 = ( imax - windowSize + useFFTSize ) % ( useFFTSize );
                            i2 = ( imax + windowSize ) % ( useFFTSize );
                            if ( i1 < i2 ) {
                                ippsMove_64f( &delayLags[i2], &delayLags[i1], useFFTSize - i2 );
                            } else {
                                ippsMove_64f( &delayLags[i2], delayLags, useFFTSize - 2 * windowSize );    
                            }
                            ippsStdDev_64f( delayLags, useFFTSize - 2 * windowSize, &stddev );
                            ippsAbs_64f( lags, delayLags, useFFTSize );
                            /**/
                            int maxChannel = imax;
//                            if ( imax > nChannels ) 
//                                imax -= nChannels * 2;
/**/                            if ( imax > useFFTSize / 2 ) 
/**/                                imax -= useFFTSize;
                            //  Convert a difference in FFT index to a time difference.  vcal.cpp says "really
                            //  should use sampling rate".  Probably true.
                            delay = (double)imax / ( 2.0 * _productInfo[iProduct].Bandwidth ) * ( (double)nChannels / (double)useFFTSize );
                            snr = (float)( max / stddev );
                            //  Send amplitude data to the client.  For sending these plot data we are using
                            //  "composed" packets, explained in the PacketExchange.  Double precision numbers
                            //  are sent as strings because Java and C++ don't appear to play nicely together.
                            //_guiClient->composePacket( AMPLITUDE_DATA, nChannels * sizeof( double ) + 4 * sizeof( int ) );
                            _guiClient->composePacket( AMPLITUDE_DATA, nChannels * 14 + 4 * sizeof( int ) );
                            _guiClient->composeInt( &iProduct );
                            _guiClient->composeInt( &nChannels );
                            _guiClient->composeInt( &timeStamp );
                            _guiClient->composeInt( &integrationTime );
                            _guiClient->composeStringDouble( amp, nChannels );
                            _guiClient->composeEnd();
                            //  Phase data.
                            //_guiClient->composePacket( PHASE_DATA, nChannels * sizeof( double ) + 4 * sizeof( int ) );
                            _guiClient->composePacket( PHASE_DATA, nChannels * 14 + 4 * sizeof( int ) );
                            _guiClient->composeInt( &iProduct );
                            _guiClient->composeInt( &nChannels );
                            _guiClient->composeInt( &timeStamp );
                            _guiClient->composeInt( &integrationTime );
                            _guiClient->composeStringDouble( phase, nChannels );
                            _guiClient->composeEnd();
                            //  Lag data require some rearrange.
//                            _guiClient->composePacket( LAG_DATA, 2 * ( nChannels + 1) * sizeof( double ) + 4 * sizeof( int ) );
                            //_guiClient->composePacket( LAG_DATA, ( 2 + useFFTSize ) * sizeof( double ) + 4 * sizeof( int ) );
                            _guiClient->composePacket( LAG_DATA, ( 2 + useFFTSize ) * 14 + 4 * sizeof( int ) );
                            _guiClient->composeInt( &iProduct );
//                            _guiClient->composeInt( &nChannels );
/**/                            int halfSize = useFFTSize / 2;
/**/                            _guiClient->composeInt( &halfSize );
                            _guiClient->composeInt( &timeStamp );
                            _guiClient->composeInt( &integrationTime );
                            _guiClient->composeInt( &maxChannel );
                            _guiClient->composeStringDouble( &delay );
                            _guiClient->composeStringDouble( &snr );
//                            _guiClient->composeStringDouble( delayLags + nChannels, nChannels );
//                            _guiClient->composeStringDouble( delayLags, nChannels );
/**/                            _guiClient->composeStringDouble( delayLags + useFFTSize / 2, useFFTSize / 2 );
/**/                            _guiClient->composeStringDouble( delayLags, useFFTSize / 2 );
                            _guiClient->composeEnd();
                            
                            //  Repeat analysis for the scan mean vectors.
	                        //  Compute amplitude, phase, lags...
                            ippsMagnitude_64fc( meanVis64, amp, nChannels );
                            ippsPhase_64fc( meanVis64, phase, nChannels );
                            ippsMulC_64f_I( 180.0/M_PI, phase, nChannels );
                            ippsFFTInv_CCSToR_64f( (Ipp64f*)meanfftVis64, lags, fftspec, 0 );
                            //  Generate a delay calculation...(swiped from vcal.cpp)
                            max = 0.0;
                            stddev = 0.0;
                            imax = 0;
                            i1 = 0;
                            i2 = 0;
                            ippsAbs_64f( lags, delayLags, useFFTSize );
                            ippsMaxIndx_64f( delayLags, useFFTSize, &max, &imax );
                            i1 = ( imax - windowSize + useFFTSize ) % ( useFFTSize );
                            i2 = ( imax + windowSize ) % ( useFFTSize );
                            if ( i1 < i2 ) {
                                ippsMove_64f( &delayLags[i2], &delayLags[i1], useFFTSize - i2 );
                            } else {
                                ippsMove_64f( &delayLags[i2], delayLags, useFFTSize - 2 * windowSize );    
                            }
                            ippsStdDev_64f( delayLags, useFFTSize - 2 * windowSize, &stddev );
                            ippsAbs_64f( lags, delayLags, useFFTSize );
                            maxChannel = imax;
                            if ( imax > useFFTSize / 2 ) 
                                imax -= useFFTSize;
                            //  Convert a difference in FFT index to a time difference.  vcal.cpp says "really
                            //  should use sampling rate".  Probably true.
                            delay = (double)imax / ( 2.0 * _productInfo[iProduct].Bandwidth ) * ( (double)nChannels / (double)useFFTSize );
                            snr = (float)( max / stddev );
                            
                            //  Send mean amplitude data to the client.
                            //_guiClient->composePacket( MEAN_AMPLITUDE_DATA, nChannels * sizeof( double ) + 3 * sizeof( int ) );
                            _guiClient->composePacket( MEAN_AMPLITUDE_DATA, nChannels * 14 + 3 * sizeof( int ) );
                            _guiClient->composeInt( &iProduct );
                            _guiClient->composeInt( &nChannels );
                            _guiClient->composeInt( &timeStamp );
                            _guiClient->composeInt( &integrationTime );
                            _guiClient->composeStringDouble( amp, nChannels );
                            _guiClient->composeEnd();
                            //  Phase data.
                            //_guiClient->composePacket( MEAN_PHASE_DATA, nChannels * sizeof( double ) + 3 * sizeof( int ) );
                            _guiClient->composePacket( MEAN_PHASE_DATA, nChannels * 14 + 3 * sizeof( int ) );
                            _guiClient->composeInt( &iProduct );
                            _guiClient->composeInt( &nChannels );
                            _guiClient->composeInt( &timeStamp );
                            _guiClient->composeInt( &integrationTime );
                            _guiClient->composeStringDouble( phase, nChannels );
                            _guiClient->composeEnd();
                            //  Lag data require some rearrange.
                            //_guiClient->composePacket( MEAN_LAG_DATA, ( 2 + useFFTSize ) * sizeof( double ) + 4 * sizeof( int ) );
                            _guiClient->composePacket( MEAN_LAG_DATA, ( 2 + useFFTSize ) * 14 + 4 * sizeof( int ) );
                            _guiClient->composeInt( &iProduct );
                            halfSize = useFFTSize / 2;
                            _guiClient->composeInt( &halfSize );
                            _guiClient->composeInt( &timeStamp );
                            _guiClient->composeInt( &integrationTime );
                            _guiClient->composeInt( &maxChannel );
                            _guiClient->composeStringDouble( &delay );
                            _guiClient->composeStringDouble( &snr );
                            _guiClient->composeStringDouble( delayLags + useFFTSize / 2, useFFTSize / 2 );
                            _guiClient->composeStringDouble( delayLags, useFFTSize / 2 );
                            _guiClient->composeEnd();

                            //  Indicate that we have sent all the data associated with this most
                            //  recent set of visibilities.
                            _guiClient->semaphorePacket( END_VISIBILITY_BLOCK );
                        }
                    }
                }
            }
            if ( vis64 != NULL )
                ippsFree( vis64 );
            if ( amp != NULL )
                ippsFree( amp );
            if ( phase != NULL )
                ippsFree( phase );
            if ( lags != NULL )
                ippsFree( lags );
            if ( delayLags != NULL )
                ippsFree( delayLags );
            if ( fftspec != NULL )
#ifdef IPP_9_API
                ippsFree( fftspec );
		ippsFree( fftworkbuf );
#else
                ippsFFTFree_R_64f( fftspec );
#endif
            if ( _monitorServerClient != NULL )
                delete _monitorServerClient;
            _monitorServerClient = NULL;
        }
        
        bool keepGoing() { return _keepGoing; }

    protected:
    
        bool _keepGoing;
        bool _visConnectionOperating;
        int _fftSize;
        pthread_attr_t _monitorAttr;
        pthread_t _monitorId;
        ServerSideConnection* _ssc;
        Configuration* _config;
        class Product {
        public:
            Product( int newIndex, int newOffset, int newNumFrequencies ) {
                index = newIndex;
                offset = newOffset;
                numFrequencies = newNumFrequencies;
            }
            int index;
            int offset;
            int numFrequencies;
        };
        network::TCPClient* _monitorServerClient;
        std::vector<Product> _productList;
        class ProductInfo {
        public:
            ProductInfo( int TelIndex1, int TelIndex2, std::string TelName1, 
				         std::string TelName2, double freq, double bandwidth, 
				         char polpair[3], int nbin, int nphasecentre, int offset,
				         int nchan, bool lsbval) {
                TelescopeIndex1 = TelIndex1;
                TelescopeIndex2 = TelIndex2;
                TelescopeName1 = TelName1;
                TelescopeName2 = TelName2;
                Bandwidth = bandwidth;
                Freq = freq;
                PolPair[0] = polpair[0];
                PolPair[1] = polpair[1];
                PolPair[2] = 0;
                Nbin = nbin;
                Nphasecentre = nphasecentre;
                Offset = offset;
                Nchan = nchan;
                lsb = lsbval;
            }
            int TelescopeIndex1;
            int TelescopeIndex2;
            std::string TelescopeName1;
            std::string TelescopeName2;
            double Bandwidth;
            double Freq;
            char PolPair[3];
            int Nbin;
            int Nphasecentre;
            int Offset;
            int Nchan;
            bool lsb;
        };
        std::vector<ProductInfo> _productInfo;
            
        pthread_t _receiveId;
        bool _receiveActive;
        
        GUIClient* _guiClient;


    };

}

#endif
