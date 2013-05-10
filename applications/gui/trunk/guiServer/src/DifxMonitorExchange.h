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
#include <network/ActivePacketExchange.h>
#include <ServerSideConnection.h>
#include <vector>

namespace guiServer {

    class DifxMonitorExchange : public network::ActivePacketExchange {
    
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
        static const int CORRELATION_PRODUCTS               = 113;
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
    
        DifxMonitorExchange( network::GenericSocket* sock, ServerSideConnection::DifxMonitorInfo* monitorInfo ) : 
            network::ActivePacketExchange( sock ) {
            _keepGoing = true;
            _visConnectionOperating = false;
            _monitorInfo = monitorInfo;
        }
        
        ~DifxMonitorExchange() {
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
                    closeConnection( data, nBytes );
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
                default:
                    break;
            }
        }
        
        //-----------------------------------------------------------------------------
        //!  Respond to a GUI instruction to close this connection.
        //-----------------------------------------------------------------------------
        void closeConnection( char* data, const int nBytes ) {
            printf( "received close connection request\n" );
            if ( _monitorServerClient != NULL )
                delete _monitorServerClient;
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
            printf( "input file path is \"%s\"\n", data );

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
                            formatPacket( MESSAGE, "located monitor_server instance: %s\n", message );
                            found = true;
                        }
                    }
                }
            }
            //  Start the monitor_server if we have not found it.
            if ( !found ) {
                snprintf( monCommand, ServerSideConnection::MAX_COMMAND_SIZE,
                     "source %s; monitor_server %d &> /dev/null &", _monitorInfo->ssc->difxSetupPath(), monitorServerPort );
                formatPacket( WARNING, "monitor_server needs to be started - executing: %s\n", monCommand );
                system( monCommand );
            }

            //  Make a new client connection to the monitor_server, which *should* now be running.
            //  At the moment the port is hard-wired to 52300 which might not be a perfect
            //  long term solution.  The host is the local host (so I'm using the loopback
            //  address).
            printf( "making client connection\n" );
            _monitorServerClient = new network::TCPClient( "127.0.0.1", 52300 );
            _monitorServerClient->waitForConnect();
            printf( "connection established!\n" );
            if ( _monitorServerClient->connected() ) {
                int status;
                _monitorServerClient->reader( (char*)(&status), sizeof( int ) );
                if ( status == 0 ) {
                    formatPacket( MESSAGE, "connection established with monitor_server" );
                }
                else {
                    formatPacket( ERROR, "monitor_server returned initial status failure (%d) - real time monitor will not run", status );
                    _keepGoing = false;
                }
            }
            else {
                formatPacket( ERROR, "connection with monitor_server failed - real time monitor will not run" );
                _keepGoing = false;
            }

            //  Use the DiFX input file parser to find out what data products are available from this job.
            if ( _keepGoing ) {
                _config = new Configuration( data, 0 );
                if ( !_config->consistencyOK() ) {
                    formatPacket( ERROR, "Configuration had a problem parsing input file %s - real time monitor will not run",
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
                    sendPacket( CORRELATION_PRODUCTS, NULL, 0 );
                    
                    int nScans = 1;  //  Is there ever more than one??
                    intPacket( NUM_SCANS, &nScans );
                    int currentScan = 0;
                    intPacket( SCAN, &currentScan );
                    int configindex = _config->getScanConfigIndex( currentScan );
                    char polpair[3];
                    polpair[2] = 0;
                    printf( "config index is %d\n", configindex );
                    printf( "start time is %d\n", _config->getStartSeconds() );

                    int nBaselines = _config->getNumBaselines();
                    intPacket( NUM_BASELINES, &nBaselines );        
                    
                    int productN = 0;            

                    for ( int i = 0; i < nBaselines; i++ ) {
                        intPacket( BASELINE, &i );
                        int ds1index = _config->getBDataStream1Index( configindex, i );
                        int ds2index = _config->getBDataStream2Index( configindex, i );
                        formatPacket( TELESCOPE_1, "%s", _config->getTelescopeName( ds1index ).c_str() );
                        formatPacket( TELESCOPE_2, "%s", _config->getTelescopeName( ds2index ).c_str() );
                        
                        int nFrequencies = _config->getBNumFreqs( configindex, i );
                        intPacket( NUM_FREQUENCIES, &nFrequencies );

                        for( int j = 0; j < nFrequencies; j++ ) {
                            int freqindex = _config->getBFreqIndex( configindex, i, j );
                            double frequency = _config->getFreqTableFreq( freqindex );
                            doublePacket( FREQUENCY, &frequency );
                            int resultIndex = _config->getCoreResultBaselineOffset( configindex, freqindex, i );
                            int freqchannels = _config->getFNumChannels( freqindex ) / _config->getFChannelsToAverage( freqindex );
                            
                            int nPhaseCenters = _config->getMaxPhaseCentres( configindex );
                            intPacket( NUM_PHASE_CENTERS, &nPhaseCenters );

                            for( int s = 0; s < nPhaseCenters; s++ ) {
                                intPacket( PHASE_CENTER, &s );
                                
                                int binloop = 1;
                                if ( _config->pulsarBinOn( configindex ) && !_config->scrunchOutputOn( configindex ) )
                                    binloop = _config->getNumPulsarBins( configindex );
                                intPacket( NUM_PULSAR_BINS, &binloop );
                                
	                            for( int b = 0; b < binloop; b++ ) {
	                                intPacket( PULSAR_BIN, &b );
	                                
	                                int nPolProducts = _config->getBNumPolProducts( configindex, i, j );
	                                intPacket( NUM_POL_PRODUCTS, &nPolProducts );
	                                
	                                for( int k = 0; k < nPolProducts; k++ ) {
	                                    intPacket( POL_PRODUCT, &k );
	                                    
	                                    _config->getBPolPair( configindex, i, j, k, polpair );

                                        //  Send the number, resultIndex (offset) and frequency channels for this
                                        //  product.
                                        int productData[3];
                                        productData[0] = productN;
                                        productData[1] = resultIndex;
                                        productData[2] = freqchannels;
                                        intPacket( NEW_PRODUCT, productData, 3 );
                                        
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

                                formatPacket( AUTOCORRELATION, "%s", _config->getDStationName( configindex, i ).c_str() );

                                polpair[0] = _config->getDRecordedBandPol( configindex, i, k );
                                if ( j==0 )
                                    polpair[1] = polpair[0];
                                else
                                    polpair[1] = ( polpair[0] == 'R' )? 'L': 'R';

                                int freqindex = _config->getDRecordedFreqIndex( configindex, i, k );
                                double frequency = _config->getFreqTableFreq( freqindex );
                                doublePacket( FREQUENCY, &frequency );
                                int freqchannels = _config->getFNumChannels( freqindex ) / _config->getFChannelsToAverage( freqindex );

                                int productData[3];
                                productData[0] = productN;
                                productData[1] = offset;
                                productData[2] = freqchannels;
                                intPacket( NEW_PRODUCT, productData, 3 );
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

                    sendPacket( CORRELATION_PRODUCTS, NULL, 0 );
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
            printf( "at end of requests...there are %d\n", _productList.size() );
            if ( _productList.size() > 0 ) {
                //  See if we need to make a new connection to the monitor server.  This might happen if a request
                //  was already made, run through, and timed out.
                if ( _monitorServerClient == NULL ) {
                    printf( "making client connection\n" );
                    _monitorServerClient = new network::TCPClient( "127.0.0.1", 52300 );
                    _monitorServerClient->waitForConnect();
                    printf( "connection established!\n" );
                    if ( _monitorServerClient->connected() ) {
                        int status;
                        _monitorServerClient->reader( (char*)(&status), sizeof( int ) );
                        if ( status == 0 ) {
                            formatPacket( MESSAGE, "connection established with monitor_server" );
                        }
                        else {
                            formatPacket( ERROR, "monitor_server returned initial status failure (%d) - real time monitor will not run", status );
                            _keepGoing = false;
                        }
                    }
                    else {
                        formatPacket( ERROR, "connection with monitor_server failed - real time monitor will not run" );
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
                        formatPacket( MESSAGE, "data product requests sent to monitor_server" );
                        //  Start the thread that reads visibilities (assuming its not already running!).
                        if ( _visConnectionOperating )
                            formatPacket( WARNING, "a thread is already monitoring data products - a new one cannot be started!" );
                        else {
                            pthread_attr_init( &_monitorAttr );
                            pthread_create( &_monitorId, &_monitorAttr, staticVisibilityMonitor, this );      
                        }
                    }
                    else {
                        formatPacket( ERROR, "monitor_server returned failure status (%d) after data product requests", status );
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
            int nReturn;
            int buffSize = 0;
            int newBuffSize;
            char* buff;
            int iVis = 0;
            int nProducts = _productList.size();
            int procChannels = 0;
            double delay = 0.0;
            double snr = 0.0;
            Ipp64f* amp = NULL;
            Ipp64f* phase = NULL;
            Ipp64f* lags = NULL;
            Ipp64f* delayLags = NULL;
            Ipp64fc* vis64 = NULL;
            IppsFFTSpec_R_64f* fftspec = NULL;
            int threadTimeoutCount = 0;
            bool threadTimeoutActive = false;
            int threadTimeoutModelCount = 0;
            bool threadTimeoutInitialized = false;
            while( _keepGoing && _visConnectionOperating ) {
                //  Read the next block of visibility data.  We put a timeout on this to kill
                //  the thread when the user isn't doing anything.
                _monitorServerClient->setTimeout( 1 );
                int ret = _monitorServerClient->reader( (char*)&timeStamp, sizeof( int ) );
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
                            formatPacket( WARNING, "real-time monitor thread has timed out after %d seconds of inactivity", 2 * threadTimeoutModelCount );
                            _visConnectionOperating = false;
                        }
                    }
                    else if ( threadTimeoutInitialized )
                        ++threadTimeoutModelCount;
                }
                else if ( ret < 0 ) {
                    _visConnectionOperating = false;
                    formatPacket( ERROR, "problem with socket connection to monitor_server: %d returned", ret );
                }
                else {
                    threadTimeoutCount = 0;
                    if ( !threadTimeoutInitialized ) {
                        threadTimeoutInitialized = true;
                        threadTimeoutModelCount = 0;
                    }
                    else
                        threadTimeoutActive = true;
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
                        formatPacket( ERROR, "problem with visibility data from monitor_server: %d out of %d bytes returned", ret, buffSize );
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
                            printf( "%d channels from product %d\n", nChannels, iProduct );
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
                                    ippsFFTFree_R_64f( fftspec );
                                vis64 = ippsMalloc_64fc( nChannels );
                                //  For each product....
                                amp = ippsMalloc_64f( nChannels );
                                phase = ippsMalloc_64f( nChannels );
                                lags = ippsMalloc_64f( nChannels * 2 );
                                delayLags = ippsMalloc_64f( nChannels * 2 );
                                if ( amp == NULL || phase == NULL || lags == NULL ) {
                                    formatPacket( ERROR, "Failure to allocate memory for visibility processing arrays." );
                                    _visConnectionOperating = false;
                                    break;
                                }
                                int order = 0;
                                while( ( ( nChannels * 2 ) >> order ) > 1 )
                                    order++;
                                ippsFFTInitAlloc_R_64f( &fftspec, order, IPP_FFT_NODIV_BY_ANY, ippAlgHintFast );
                            }
                            //  Put the visibility data into double-precision complex arrays.
	                        for ( int i = 0; i < nChannels; i++ ) {
	                          vis64[i].re = vis32[i].re;
	                          vis64[i].im = vis32[i].im;
	                        }
	                        //  Compute amplitude, phase, lags...
                            ippsMagnitude_64fc( vis64, amp, nChannels );
                            ippsPhase_64fc( vis64, phase, nChannels );
                            ippsMulC_64f_I( 180.0/M_PI, phase, nChannels );
                            ippsFFTInv_CCSToR_64f( (Ipp64f*)vis64, lags, fftspec, 0 );
                            //  Generate a delay calculation...(swiped from vcal.cpp)
                            Ipp64f max = 0.0;
                            Ipp64f stddev = 0.0;
                            int imax = 0;
                            int i1 = 0;
                            int i2 = 0;
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
                            int maxChannel = imax;
                            if ( imax > nChannels ) 
                                imax -= nChannels * 2;
                            //  Convert a difference in FFT index to a time difference.  vcal.cpp says "really
                            //  should use sampling rate".  Probably true.
                            delay = (double)imax / ( 2.0 * _productInfo[iProduct].Bandwidth );
                            snr = (float)( max / stddev );
                            //  Send amplitude data to the client.  For sending these plot data we are using
                            //  "composed" packets, explained in the PacketExchange.  Double precision numbers
                            //  are sent as strings because Java and C++ don't appear to play nicely together.
                            composePacket( AMPLITUDE_DATA, nChannels * sizeof( double ) + 3 * sizeof( int ) );
                            composeInt( &iProduct );
                            composeInt( &nChannels );
                            composeInt( &timeStamp );
                            composeStringDouble( amp, nChannels );
                            composeEnd();
                            //  Phase data.
                            composePacket( PHASE_DATA, nChannels * sizeof( double ) + 3 * sizeof( int ) );
                            composeInt( &iProduct );
                            composeInt( &nChannels );
                            composeInt( &timeStamp );
                            composeStringDouble( phase, nChannels );
                            composeEnd();
                            //  Lag data require some rearrange.
                            composePacket( LAG_DATA, 2 * ( nChannels + 1) * sizeof( double ) + 4 * sizeof( int ) );
                            composeInt( &iProduct );
                            composeInt( &nChannels );
                            composeInt( &timeStamp );
                            composeInt( &maxChannel );
                            composeStringDouble( &delay );
                            composeStringDouble( &snr );
                            composeStringDouble( lags + nChannels, nChannels );
                            composeStringDouble( lags, nChannels );
                            composeEnd();
                        }
                    }
                }
            }
            printf( "stopping visibility monitor\n" );
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
                ippsFFTFree_R_64f( fftspec );
            if ( _monitorServerClient != NULL )
                delete _monitorServerClient;
            _monitorServerClient = NULL;
        }
        
        bool keepGoing() { return _keepGoing; }

    protected:
    
        bool _keepGoing;
        bool _visConnectionOperating;
        pthread_attr_t _monitorAttr;
        pthread_t _monitorId;
        ServerSideConnection::DifxMonitorInfo* _monitorInfo;
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
            
    };

}

#endif
