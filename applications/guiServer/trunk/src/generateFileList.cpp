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
//   ServerSideConnection::generateFileList Function (and associated functions)
//
//!  Called when an instruction to generate a filelist from a list of DiFX
//!  data files.  Should work for Mark5B, MKIV, VDIF, and VLBA formats.  The
//!  code for MKIV and VLBA was shamelessly swiped from Helge Rottmann's
//!  "directory2filelist" program.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <ExecuteSystem.h>
//#include <sys/statvfs.h>
#include <network/TCPClient.h>
//#include <network/PacketExchange.h>
//#include <list>
#include <string>
//#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <GUIClient.h>
#include <map>

using namespace guiServer;

//-----------------------------------------------------------------------------
//  These are packet types used for exchanging data with the GUI.
//-----------------------------------------------------------------------------
static const int GENERATE_FILELIST_TASK_TERMINATED                     = 100;
static const int GENERATE_FILELIST_TASK_ENDED_GRACEFULLY               = 101;
static const int GENERATE_FILELIST_TASK_STARTED                        = 102;
static const int GENERATE_FILELIST_DESTINATION_EXISTS                  = 103;
static const int GENERATE_FILELIST_FINAL_NAME                          = 104;
static const int GENERATE_FILELIST_PATH_ACCESS_FAILURE                 = 105;
static const int GENERATE_FILELIST_OVERWRITE                           = 106;
static const int GENERATE_FILELIST_CANCEL                              = 107;
static const int GENERATE_FILELIST_OPEN_ERROR                          = 108;
static const int GENERATE_FILELIST_BAD_FORMAT                          = 109;
static const int GENERATE_FILELIST_PROCESSED_COUNT                     = 110;
static const int GENERATE_FILELIST_FILE_RESULT                         = 111;
static const int GENERATE_FILELIST_ERRORS_ENCOUNTERED                  = 112;

//-----------------------------------------------------------------------------
//!  Thread function for actually running the machines definition operations.
//-----------------------------------------------------------------------------
void ServerSideConnection::generateFileList( GenerateFileListInfo* generateFileListInfo ) {

//    printf( "source list is...\n" );
//    for ( int i = 0; i < generateFileListInfo->nFiles; ++i )
//        printf( "%s\n", generateFileListInfo->file[i].c_str() );
//    printf( "destination is \"%s\"\n", generateFileListInfo->destination.c_str() );
//    printf( "format is \"%s\"\n", generateFileListInfo->format.c_str() );
//    printf( "port is %d\n", generateFileListInfo->port );
    
    bool keepGoing = true;

	//  Open a client connection to the server that should be running for us on the
    //  host that requested this task (the GUI, presumably).
    GUIClient* monitor = new GUIClient( generateFileListInfo->ssc, generateFileListInfo->address.c_str(), generateFileListInfo->port );
    monitor->packetExchange();
    
    //  If the connection was made properly, send acknowledgement.  If not, we need to bail out now.
    if ( monitor->okay() )
        monitor->sendPacket( GENERATE_FILELIST_TASK_STARTED, NULL, 0 );
    else {
        diagnostic( ERROR, "client socket connection from guiServer to GUI failed - unable to generate filelist" );
        delete monitor;
	    return;
    }
    
    //  Check the format against the types we know about.
    static const int MKIV_FORMAT = 1;
    static const int VLBA_FORMAT = 2;
    static const int MK5B_FORMAT = 3;
    static const int VDIF_FORMAT = 4;
    int useFormat = 0;
    if ( !generateFileListInfo->format.compare( 0, 4, "MKIV" ) )
        useFormat = MKIV_FORMAT;
    else if ( !generateFileListInfo->format.compare( 0, 4, "VLBA" ) )
        useFormat = VLBA_FORMAT;
    else if ( !generateFileListInfo->format.compare( 0, 6, "MARK5B" ) )
        useFormat = MK5B_FORMAT;
    else if ( !generateFileListInfo->format.compare( 0, 4, "VDIF" ) )
        useFormat = VDIF_FORMAT;
    else {
        monitor->sendPacket( GENERATE_FILELIST_BAD_FORMAT, 
            generateFileListInfo->format.c_str(), strlen( generateFileListInfo->format.c_str() ) );
        keepGoing = false;
    }
    
    //  Figure out the destination of this filelist.  If it exists, see if the user wants
    //  to overwrite it.  If the user can't write to it, complain about that.
    char fileListName[2048];
    if ( keepGoing ) {
        sprintf( fileListName, "%s", generateFileListInfo->destination.c_str() );
        struct stat buf;
        int ret = stat( generateFileListInfo->destination.c_str(), &buf ) ;
        //  A bad return value here might mean we can't see this file path, or it might be
        //  "good" in that the file doesn't exist.
        if ( ret == -1 ) {
            if ( errno == EACCES ) {
                monitor->sendPacket( GENERATE_FILELIST_PATH_ACCESS_FAILURE, 
                    generateFileListInfo->destination.c_str(), strlen( generateFileListInfo->destination.c_str() ) );
                keepGoing = false;
            }
        }
        else {
            //  The file exists already.  Warn the user of this and see what they want to do.
            monitor->sendPacket( GENERATE_FILELIST_DESTINATION_EXISTS, 
                generateFileListInfo->destination.c_str(), strlen( generateFileListInfo->destination.c_str() ) );
            int answer;
            monitor->reader( &answer, sizeof( int ) );
            answer = ntohl( answer );
            if ( answer == GENERATE_FILELIST_CANCEL )
                keepGoing = false;
        }
    }
    
    //  Try to open the file.  Error out if we can't.
    FILE* fp = NULL;
    if ( keepGoing ) {
        fp = fopen( generateFileListInfo->destination.c_str(), "w" );
        if ( fp == NULL ) {
            monitor->sendPacket( GENERATE_FILELIST_OPEN_ERROR, strerror( errno ), strlen( strerror( errno ) ) );
            keepGoing = false;
        }
    }
    
    //  Send the final file name for the filelist to the GUI.
    if ( keepGoing )
        monitor->sendPacket( GENERATE_FILELIST_FINAL_NAME, fileListName, strlen( fileListName ) );
        
    //  Run the command appropriate to the format to generate start and stop time information.
    if ( keepGoing ) {
        int processedFiles = 0;
        bool errorsEncountered = false;
        char command[2048];
        char message[DIFX_MESSAGE_LENGTH];
        double startmjd = 0.0;
        double stopmjd = 0.0;
        ExecuteSystem* executor;
        std::map<double, std::string> resultsMap;
        for ( int i = 0; i < generateFileListInfo->nFiles; ++i ) {
            switch ( useFormat ) {
                case MKIV_FORMAT:
                case VLBA_FORMAT:
                {
                    int ret = verify( generateFileListInfo->file[i].c_str(), generateFileListInfo->format.c_str(), startmjd, stopmjd,
                        generateFileListInfo->refmjd );
                    if ( ret == 0 ) {
                        snprintf( message, DIFX_MESSAGE_LENGTH, "%s %f %f", generateFileListInfo->file[i].c_str(), startmjd, stopmjd );
                        monitor->sendPacket( GENERATE_FILELIST_FILE_RESULT, message, strlen( message ) );
                        processedFiles += 1;
                        std::string newString( message );
                        resultsMap.insert( std::pair<double, std::string>( startmjd, newString ) );
                    }
                }
                    break;
                case MK5B_FORMAT:
                case VDIF_FORMAT:
                {
                    //  Construct and execute a system command appropriate to the format.
                    if ( useFormat == MK5B_FORMAT )
                	    snprintf( command, MAX_COMMAND_SIZE, "%s m5bsum -s -r %d %s", 
			                _difxSetupPath, generateFileListInfo->refmjd, generateFileListInfo->file[i].c_str() );
			        else if ( useFormat == VDIF_FORMAT )
                	    snprintf( command, MAX_COMMAND_SIZE, "%s vsum -s -r %d %s", 
			                _difxSetupPath, generateFileListInfo->refmjd, generateFileListInfo->file[i].c_str() );
                    executor = new ExecuteSystem( command );
                    //  And watch the results for stdout and stderr messages.
                    while ( int ret = executor->nextOutput( message, DIFX_MESSAGE_LENGTH ) ) {
                        if ( ret == 1 ) {
                            //  Unfortunately, errors as well as proper data are sent to stdout.  A "good"
                            //  return contains the filename and start and stop times.  Other lines are
                            //  considered errors.
                            if ( strlen( message ) > 0 ) {
                                //  Make sure the file name is the first item!
                                if ( !strncmp( message, generateFileListInfo->file[i].c_str(), strlen( generateFileListInfo->file[i].c_str() ) ) ) {
                                    monitor->sendPacket( GENERATE_FILELIST_FILE_RESULT, message, strlen( message ) );
                                    processedFiles += 1;
                                    //  Grab the start value from the message.  This is used to sort the data in time
                                    //  order after all of it is collected.
                                    double startTime = atof( message + strlen( generateFileListInfo->file[i].c_str() ) );
                                    std::string newString( message );
                                    resultsMap.insert( std::pair<double, std::string>( startTime, newString ) );
                                }
                                else {
                                    diagnostic( ERROR, "filelist generation: %s", message );
                                    errorsEncountered = true;
                                }
                            }
                        }
                        else {
                            //  Error messages are generally not desired, but they may not kill us.
                            diagnostic( ERROR, "filelist generation: %s", message );
                            errorsEncountered = true;
                        }
                    }
                    delete executor;
			    }
                    break;
            }	
        }
        //  Send the number of files processed to the GUI.
        int foo = htonl( processedFiles );
        monitor->sendPacket( GENERATE_FILELIST_PROCESSED_COUNT, (char*)&foo, sizeof( int ) );
        //  Any errors in doing so?
        if ( errorsEncountered )
            monitor->sendPacket( GENERATE_FILELIST_ERRORS_ENCOUNTERED, NULL, 0 );
        //  Just putting things in the map sorts them based on key value (start time)....nature of
        //  maps and all...
        for ( std::map<double, std::string>::iterator iter = resultsMap.begin(); iter != resultsMap.end(); ++iter ) {
            fprintf( fp, "%s\n", iter->second.c_str() );
        }
        fclose( fp );
    }
    
    //  Report a healthy process end...or otherwise.
    if ( keepGoing )
        monitor->sendPacket( GENERATE_FILELIST_TASK_ENDED_GRACEFULLY, NULL, 0 );
        
    else
        monitor->sendPacket( GENERATE_FILELIST_TASK_TERMINATED, NULL, 0 );

    delete monitor;
        		
}

//-----------------------------------------------------------------------------
//!  Stuff used for MKIV and VLBA formats.  Some of these functions have been
//!  adjusted a little, but for the most part they come from the
//!  "directory2filelist" program.
//-----------------------------------------------------------------------------
int ServerSideConnection::verify( const char *filename, const char *formatname, double& startmjd, double& stopmjd, int refMJD ) {
	struct mark5_stream *ms;
	int i;
	int status = 0, corrupt = 0;
	int mjd, sec;
    double ns, eofmjd = 0.0;
	long validoffset = 0;
    long eofReadLength = (80000+40000);	
    
	startmjd = 0.0;
	stopmjd = 0.0;

	// open with seeking to first valid-looking frame pair
	ms = openmk5(filename, formatname, &validoffset);
	if(!ms)
	{
		fprintf(stderr, "problem opening %s\n", filename);
		return 0;
	}

	// resolve any day ambiguities
    mark5_stream_fix_mjd(ms, refMJD);

	//mark5_stream_print(ms);

	mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
	startmjd = mjd + (sec + ns/1e9) / 86400.0;
	stopmjd = startmjd;
	eofReadLength = ms->datawindowsize;

	FILE *fp = fopen(filename, "rb");

	fseek (fp,0L,SEEK_END);
        long length = ftell(fp);
        fclose (fp);


	long numFrames = length / ms->framebytes;
	double jumpNs = numFrames * ms->framens;

	double skipNs = ns + jumpNs;


	long endSec = skipNs / 1e9;
	/* double endNs = skipNs - endSec*1e9; */	/* FIXME: variable set but not used */

	status = mark5_stream_seek(ms, mjd, sec+endSec-1, ns);

	for(i = 0; ; i++)
	{
		if(status < 0)
		{
			break;
		}

		if(ms->nvalidatefail > 1024)
		{
			fprintf(stderr, "Warning: too many frame validation failures sequentially scanning file %s\n", filename);
			corrupt = 1;
			break;
		}

		if(i%1 == 0)
		{
			int mjd, sec;
			double ns;

			mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);

			if((mjd - (int)stopmjd) >= 2 || (mjd < (int)stopmjd))
			{
#ifdef DEBUG
				int gday = (int)stopmjd;
				double gsec = (stopmjd - (double)gday) * 24.0*3600.0;

				fprintf(stderr, "Jump in MJD day (%d/%.4fs (mjd=%.6f) -> %d/%.4fs), "
						"trying to resync\n", 
						gday, gsec, stopmjd, mjd, sec+ns*1e-9);
#endif

				status = mark5_stream_resync(ms);
				status = mark5_stream_next_frame(ms);
				corrupt = 1;

				continue;
			}
			else
			{
				stopmjd = mjd + (sec + ns/1e9) / 86400.0;
			}
		}

		status = mark5_stream_next_frame(ms);

	}

	delete_mark5_stream(ms);

	// open short before EOF, with seeking to first valid-looking frame pair
        // note: this may generate many "Shortening datawindowsize" warnings
	validoffset = length - eofReadLength;
	ms = openmk5(filename, formatname, &validoffset);
	if(!ms)
	{
		fprintf(stderr, "problem opening at tail of %s\n", filename);
		return -1;
	}

	// resolve any day ambiguities
        mark5_stream_fix_mjd(ms, refMJD);

	mark5_stream_get_frame_time(ms, &mjd, &sec, &ns);
	eofmjd = mjd + (sec + ns/1e9) / 86400.0;

	delete_mark5_stream(ms);

#ifdef DEBUG
	fprintf(stderr, "Timing: MJD start=%lf stop=%lf eof=%lf in %s\n", startmjd, stopmjd, eofmjd, filename);
#endif

	// choose most plausible scan data stop time in presence of frame or sync errors

	// both Stop and EOF MJD corrupt?
	if(!is_reasonable_timediff(startmjd, stopmjd) && !is_reasonable_timediff(startmjd, eofmjd))
	{
		stopmjd = startmjd;
		corrupt = 1;
	}
	// both Stop and EOF MJD look good?
	else if(is_reasonable_timediff(startmjd, stopmjd) && is_reasonable_timediff(startmjd, eofmjd))
	{
		stopmjd = fmax(stopmjd, eofmjd);
	}
	// either Stop or EOF MJD is corrupt
	else
	{
		if(is_reasonable_timediff(startmjd,eofmjd))
		{
			stopmjd = eofmjd;
		}
		corrupt = 1;
	}

	if(corrupt)
	{
		fprintf(stderr, "Warning: found corrupt data frames in file %s\n", filename);
	}

	return 0;
}

struct mark5_stream* ServerSideConnection::openmk5(const char *filename, const char *formatname, long *offset)
{
	struct mark5_stream *ms;
	long offset0 = *offset;
	char did_fail = 0;
	while (1) {
                ms = new_mark5_stream_absorb(
                        new_mark5_stream_file(filename, *offset),
                        new_mark5_format_generic_from_string(formatname) );

                if(!ms)
                {
			if (*offset < (offset0 + 32*43500L))
			{
                        	fprintf(stderr, "problem at initial decode of %s at offset %ld, trying new offset\n", filename, (int64_t)(*offset));
				*offset += 43500;
				did_fail = 1;
				continue;
			}
			else
			{
                        	fprintf(stderr, "problem opening %s\n", filename);
	                        break;
			}
                }
                if(0 == (ms->samprate % 1000) && ms->samprate>0)
                {
			if (did_fail)
			{
				fprintf(stderr, "decode %s at offset %ld succeeded\n\n", filename, (int64_t)(*offset));
			}
                        break;
                }
                fprintf(stderr, "File offset %ld: decoded suspect sample rate %d, trying new offset\n", *offset, ms->samprate);
                delete_mark5_stream(ms);
                (*offset) += 43500;
	}
	return ms;
}

int ServerSideConnection::is_reasonable_timediff(double startmjd, double stopmjd)
{
	int startday = (int)startmjd;
	int stopday = (int)stopmjd;
	return ( (startmjd <= stopmjd) && ((stopday-startday) <= 1) );
}


