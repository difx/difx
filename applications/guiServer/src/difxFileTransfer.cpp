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
//   ServerSideConnection::difxFileTransfer Function (and associated functions)
//
//!  Called when an instruction to transfer a file to/from DiFX is received.  
//!  This function is a member of the ServerSideConnection class.
//
//=============================================================================
#include <ServerSideConnection.h>
#include <sys/statvfs.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <pwd.h>
#include <fcntl.h>
#include <sys/file.h>
#include <network/TCPClient.h>
#include <signal.h>
#include <GUIClient.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a user request to transfer a file either from the
//!  DiFX host to an external host (the GUI, presumably) or the reverse.  This
//!  is accomplished by opening a client TCP connection to the external host
//!  (which must have initiated a server already).  The bulk of the work is
//!  threaded.
//-----------------------------------------------------------------------------
void ServerSideConnection::difxFileTransfer( DifxMessageGeneric* G ) {

    //  Cast the message to a File Transfer message.
	const DifxMessageFileTransfer *S = &G->body.fileTransfer;
	//  Make a copy of information about the current file operation, including thread information
	//  (so the thread variables are still viable after this function call returns).
	DifxFileTransfer* fileTransfer = new DifxFileTransfer;
	snprintf( fileTransfer->transfer.origin, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->origin );
	snprintf( fileTransfer->transfer.destination, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->destination );
	snprintf( fileTransfer->transfer.dataNode, DIFX_MESSAGE_HOSTNAME_LENGTH, "%s", S->dataNode );
	snprintf( fileTransfer->transfer.address, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->address );
	snprintf( fileTransfer->transfer.direction, DIFX_MESSAGE_PARAM_LENGTH, "%s", S->direction );
	fileTransfer->transfer.port = S->port;
	fileTransfer->channelAllData = _channelAllData;
	fileTransfer->ssc = this;
    pthread_create( &(fileTransfer->threadId), NULL, staticRunFileTransfer, (void*)fileTransfer );      
}	

//-----------------------------------------------------------------------------
//!  Thread function for actually running file transfer operations.
//-----------------------------------------------------------------------------
void ServerSideConnection::runFileTransfer( DifxFileTransfer* fileTransfer ) {

	const DifxMessageFileTransfer *S = &(fileTransfer->transfer);
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	const char *user;

	user = getlogin();
	if ( !user ) {
    	user = getenv( "DIFX_USER_ID" );
	}
	
	//user = "difxmgr";
	if ( !strcmp( S->direction, "to DiFX" ) ) {
	
    	//snprintf( message, DIFX_MESSAGE_LENGTH, "Request for transfer of file from %s on remote host to DiFX host - filesize is unknown", S->origin );
        //difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );

    	    //  Open a client connection to the server that should be running for us on the
    	    //  remote host.
    	    int filesize = 0;
            GUIClient* gc = new GUIClient( fileTransfer->ssc, S->address, S->port );
      	    
      	    //  Assuming the connection was successful, read the file contents.
      	    if ( gc->okay() ) {
            	//snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection looks good", S->address, S->port );
            	//difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
            	//  Get the number of bytes we expect.
            	int n = 0;
                int rtn = gc->reader( &n, sizeof( int ) );
            	if ( rtn < 0 ) {
            	    snprintf( message, DIFX_MESSAGE_LENGTH, "Select error (%s) %s port: %d - transfer FAILED", strerror( errno ), S->address, S->port );
            	    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            	}
            	filesize = ntohl( n );
            	//  Then read the data.  This is broken into 1024-byte blocks.  I'm not certain
            	//  why there is (or was) a need to do this, but it works!
            	int count = 0;
            	short blockSize = 1024;
            	char blockData[blockSize + 1];
	        const int tmpFileSize = 100;
            	char tmpFile[tmpFileSize];
            	snprintf( tmpFile, tmpFileSize, "/tmp/filetransfer_%d", S->port );
            	FILE *fp = fopen( tmpFile, "w" );
		// +++MSD 2018-12-12 Get the file descriptor and use flock to place a lock on tmpFile
		int fd = fileno(fp);
		flock(fd, LOCK_EX);
		printf("1. opened file %s",tmpFile);
            	rtn = 0;
            	while ( count < filesize && rtn != -1 ) {
            	    int readn = blockSize;
            	    if ( filesize - count < readn )
            	        readn = filesize - count;
                    rtn = gc->reader( blockData, readn );
                    if ( rtn != -1 ) {
            	            count += rtn;
            	            blockData[rtn] = 0;
            	            fprintf( fp, "%s", blockData );
                    }
            	    else {
            	        snprintf( message, DIFX_MESSAGE_LENGTH, "Select error (%s) %s port: %d - transfer FAILED", strerror( errno ), S->address, S->port );
            	        difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            	    }
            	}
		// +++MSD 2018-12-12 The lock should be released once the file pointer is closed 
            	fclose( fp );
            	       
      	    }
      	    else {
            	snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - transfer FAILED", S->address, S->port );
            	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            	filesize = -10;
      	    }
    
    	    //  Check the destination filename
        	if( S->destination[0] != '/' )  {
        		filesize = -1;
        	}
        	else if ( filesize >= 0 ) {
        	    //  Check the existence of the destination directory
        	    char path[DIFX_MESSAGE_FILENAME_LENGTH];
        	    snprintf( path, DIFX_MESSAGE_FILENAME_LENGTH, "%s", S->destination );
        	    int i = strlen( path );
        	    while ( i > 0 && path[i] != '/' ) --i;
        	    path[i] = 0;
    	        struct stat stt;
    	        int ret = stat( path, &stt );
    	        if ( ret == -1 ) {
    	            //  Either we aren't allowed to view this directory
    	            if ( errno == EACCES )
    	                filesize = -3;
    	            //  Or it doesn't exist at all
    	            else
    	                filesize = -2;
    	        }
    	        //  Make sure the destination is a directory
    	        else if ( !(stt.st_mode & S_IFDIR) ) {
    	            filesize = -5;
    	        }
    	        else {
    	            //  Check write permissions and uid for the difx user
    	            struct passwd *pwd = getpwnam( user );
    	            if ( pwd == NULL ) {
    	                snprintf( message, DIFX_MESSAGE_LENGTH, "DiFX username %s is not valid", user );
    	                difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
    	                filesize = -4;
    	            }
    	            else {
    	                //  Make sure the DiFX user has write permission in the destination directory (via owner, group, or world).
    	                if ( ( stt.st_uid == pwd->pw_uid && stt.st_mode & S_IRUSR ) || ( stt.st_gid == pwd->pw_gid && stt.st_mode & S_IRGRP ) ||
    	                     ( stt.st_mode & S_IROTH ) ) {
                      		//  Change permissions on the temporary file so the DiFX user can read it.
                    		//snprintf( command, MAX_COMMAND_SIZE, "chmod 644 /tmp/filetransfer_%d", S->port );
                            //Mk5Daemon_system( D, command, 1 );
                            //  Copy the new file to its specified location (as the DiFX user).
				// +++MSD 2018-12-12 Added lock to cp from tmp file to destination
                    		snprintf( command, MAX_COMMAND_SIZE, "flock  ~/.guiserver.lock -c \"cp /tmp/filetransfer_%d %s\"", 
                    				 S->port,
                    				 S->destination );
                      		int ret = system( command );
                        	if ( ret < 0 ) {
                        	    snprintf( message, DIFX_MESSAGE_LENGTH, "Failed to execute command \"%s\" - transfer FAILED", command );
                        	    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
                        	}
                  		}
     	                //  Otherwise, we can't read it.
    	                else
    	                    filesize = -3;
    	            }
    	        }
    	    }

            int n = htonl( filesize );            	
            gc->writer( &n, sizeof( int ) );

      		//  Then clean up our litter.
		//  +++MSD 2018-12-12 Added lock to rm command
    		snprintf( command, MAX_COMMAND_SIZE, "flock  ~/.guiserver.lock -c \"rm -f /tmp/filetransfer_%d\"", S->port );
            int ret = system( command );
        	if ( ret < 0 ) {
        	    snprintf( message, DIFX_MESSAGE_LENGTH, "Failed to execute command \"%s\" - transfer FAILED", command );
        	    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
        	}
            delete gc;
      		
	}
	    
	else if ( !strcmp( S->direction, "from DiFX" ) ) {
	    //  This is a request for a file transfer from the DiFX host (i.e. this host) to a remote host.
	    //  Before we transfer the data, we will transfer the size of the file (an integer).  We use
	    //  negative integer values to indicate different problems.
	    //   -1:  bad file name (incomplete path)
	    //   -2:  file not found
	    //   -3:  read permission denied for DiFX user
	    //   -4:  bad DiFX user name
	    //    0:  zero length file
	    int filesize = 0;
	    //  Make sure the file desired has a sensible name....
	    if ( S->origin[0] != '/' )
	        filesize = -1;   // bad file name
	    else {
	        //  Do a "stat" on the file to see if it exists, what permissions there are on it, and then
	        //  find its size.
	        struct stat stt;
	        int ret = stat( S->origin, &stt );
	        if ( ret == -1 ) {
	            //  stat errors are due to mangled files or permission problems
	            perror( "stat error" );
	            if ( errno == EACCES )
	                filesize = -3;
	            else
	                filesize = -2;
	        }
	        else {
	            //  Check read permissions for the difx user (for which we need the uid)
	            struct passwd *pwd = getpwnam( user );
	            if ( pwd == NULL ) {
	                snprintf( message, DIFX_MESSAGE_LENGTH, "DiFX username %s is not valid", user );
	                difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
	                filesize = -4;
	            }
	            else {
	                //  If the DiFX user has read permission on the file (via owner, group, or world),
	                //  get the file size.
	                if ( ( stt.st_uid == pwd->pw_uid && stt.st_mode & S_IRUSR ) || ( stt.st_gid == pwd->pw_gid && stt.st_mode & S_IRGRP ) ||
	                     ( stt.st_mode & S_IROTH ) )
	                    filesize = stt.st_size;
	                //  Otherwise, we can't read it.
	                else
	                    filesize = -3;
	            }
	        }	    
	    }
    	//snprintf( message, DIFX_MESSAGE_LENGTH, "Request for transfer of file from %s on DiFX host to remote host - filesize is %d", S->origin, filesize );
        //difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
    	
    	//  Use the DiFX user to copy the requested file to a temporary location (if there is anything to copy, that is...).
    	if ( filesize > 0 ) {
	        // +++MSD 2018-12-12 added a lock	
    		snprintf( command, MAX_COMMAND_SIZE, "flock  ~/.guiserver.lock -c \"rm -f /tmp/filetransfer_%d\"", S->port );
            int ret = system( command );
        	if ( ret < 0 ) {
        	    snprintf( message, DIFX_MESSAGE_LENGTH, "Failed to execute command \"%s\" - transfer FAILED", command );
        	    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
        	}
		// +++MSD 2018-12-12 added a lock
    		snprintf( command, MAX_COMMAND_SIZE, "flock  ~/.guiserver.lock -c \"cp %s /tmp/filetransfer_%d\"", 
    				 S->origin,
    				 S->port );
      		ret = system( command );
        	if ( ret < 0 ) {
        	    snprintf( message, DIFX_MESSAGE_LENGTH, "Failed to execute command \"%s\" - transfer FAILED", command );
        	    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
        	}
  		}
    	
	    //  Open a client connection to the server that should be running for us on the
	    //  remote host.
        GUIClient* gc = new GUIClient( this, S->address, S->port );
  	    
  	    //  Assuming the socket connection was successful, write the file contents to the server.
  	    if ( gc->okay() ) {
        	//snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection looks good", S->address, S->port );
        	//difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
        	//  Send the total size first.
        	int n = htonl( filesize );            	
        	gc->writer( &n, sizeof( int ) );
        	if ( filesize > 0 ) {
        	    //  Then break the file up into "blocks" for sending.
        	    short blockSize = 1024;
        	    char blockData[blockSize];
	            const int tmpFileSize = 100;
        	    char tmpFile[tmpFileSize];
        	    snprintf( tmpFile, tmpFileSize, "/tmp/filetransfer_%d", S->port );
        	    int fd = open( tmpFile, O_RDONLY );
		    // +++MSD 2018-12-12 added a lock
		    flock(fd, LOCK_EX);
 		    printf("2. opened %s for reading",tmpFile); 
	       	    while ( filesize > 0 ) {
        	        short readsize = read( fd, blockData, blockSize );
        	        gc->writer( blockData, readsize );
        	        filesize -= readsize;
        	    }
        	    close( fd );
        	}
  	    }
  	    else {
        	snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection FAILED", S->address, S->port );
        	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
  	    }

        //  Clean up our litter.
		//snprintf( command, MAX_COMMAND_SIZE, "rm -f /tmp/filetransfer_%d", S->port );
        int ret = system( command );
    	if ( ret < 0 ) {
    	    snprintf( message, DIFX_MESSAGE_LENGTH, "Failed to execute command \"%s\" - transfer FAILED", command );
    	    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
    	}
  		delete gc;
      		    	
	}
		
}

