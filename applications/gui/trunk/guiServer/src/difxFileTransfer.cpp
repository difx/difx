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
#include <pwd.h>
#include <fcntl.h>
#include <network/TCPClient.h>
#include <signal.h>

using namespace guiServer;

//-----------------------------------------------------------------------------
//!  Called in response to a user request to transfer a file either from the
//!  DiFX host to an external host (the GUI, presumably) or the reverse.  This
//!  is accomplished by opening a client TCP connection to the external host
//!  (which must have initiated a server already).
//-----------------------------------------------------------------------------
void ServerSideConnection::difxFileTransfer( DifxMessageGeneric* G ) {
	const DifxMessageFileTransfer *S;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	char hostname[DIFX_MESSAGE_LENGTH];
	const char *user;
	pid_t childPid;
	
	//  Cast the message to a File Transfer message.
	S = &G->body.fileTransfer;

	user = getlogin();
	if ( !user ) {
    	user = getenv( "DIFX_USER_ID" );
	}

	if ( !strcmp( S->direction, "to DiFX" ) ) {
	
    	//snprintf( message, DIFX_MESSAGE_LENGTH, "Request for transfer of file from %s on remote host to DiFX host - filesize is unknown", S->origin );
        //difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
    	//  Fork a process to do the file transfer via a TCP client.
    	signal( SIGCHLD, SIG_IGN );
    	childPid = fork();
    	if( childPid == 0 ) {
    	    //  Open a TCP socket connection to the server that should be running for us on the
    	    //  remote host.
    	    int sockfd;
    	    struct sockaddr_in servaddr;
    	    sockfd = socket( AF_INET, SOCK_STREAM, 0 );
    	    bzero( &servaddr, sizeof( servaddr ) );
    	    servaddr.sin_family = AF_INET;
    	    servaddr.sin_port = htons( S->port );
    	    inet_pton( AF_INET, S->address, &servaddr.sin_addr );
      	    int filesize = connect( sockfd, (const sockaddr*)&servaddr, sizeof( servaddr ) );
      	    
      	    //  Assuming the socket connection was successful, write the file contents to the socket.
      	    if ( filesize == 0 ) {
            	//snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection looks good", S->address, S->port );
            	//printf( "%s\n", message );
            	//difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
            	//  Get the number of bytes we expect.
            	int n = 0;
            	fd_set rfds;
                struct timeval tv;
            	int rn = 0;
            	int trySec = 5;
            	while ( trySec > 0 && rn < (int)(sizeof( int )) ) {
            	    FD_ZERO(&rfds);
                    FD_SET( sockfd, &rfds );
                    tv.tv_sec = 1;
                    tv.tv_usec = 0;
                    int rtn = select( sockfd + 1, &rfds, NULL, NULL, &tv );
                    if ( rtn >= 0 ) {
            	        int readRtn = read( sockfd, (unsigned char*)(&n) + rn, sizeof( int ) - rn );
            	        if ( readRtn > 0 )
            	            rn += readRtn;
            	    }
            	    else if ( rtn < 0 ) {
            	        snprintf( message, DIFX_MESSAGE_LENGTH, "Select error (%s) %s port: %d - transfer FAILED", strerror( errno ), S->address, S->port );
            	        difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            	    }
            	    --trySec;
            	}
            	filesize = ntohl( n );
            	//  Then read the data.
            	int ret = 0;
            	int count = 0;
            	short blockSize = 1024;
            	char blockData[blockSize + 1];
		        const int tmpFileSize = 100;
            	char tmpFile[tmpFileSize];
            	snprintf( tmpFile, tmpFileSize, "/tmp/filetransfer_%d", S->port );
            	FILE *fp = fopen( tmpFile, "w" );
            	int rtn = 0;
            	while ( count < filesize && rtn != -1 ) {
            	    int readn = blockSize;
            	    if ( filesize - count < readn )
            	        readn = filesize - count;
            	    FD_ZERO(&rfds);
                    FD_SET( sockfd, &rfds );
                    tv.tv_sec = 1;
                    tv.tv_usec = 0;
                    rtn = select( sockfd + 1, &rfds, NULL, NULL, &tv );
                    if ( rtn != -1 ) {
            	        ret = read( sockfd, blockData, readn );
            	        if ( ret > 0 ) {
            	            count += ret;
            	            blockData[ret] = 0;
            	            fprintf( fp, "%s", blockData );
            	        }
                    }
            	    else {
            	        snprintf( message, DIFX_MESSAGE_LENGTH, "Select error (%s) %s port: %d - transfer FAILED", strerror( errno ), S->address, S->port );
            	        difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            	    }
            	}
            	fclose( fp );
            	       
      	    }
      	    else {
            	snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - transfer FAILED", S->address, S->port );
            	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
      	    }
    
    	    //  Check the destination filename
        	if( S->destination[0] != '/' )  {
        		filesize = -1;
        	}
        	else {
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
                    		snprintf( command, MAX_COMMAND_SIZE, "cp /tmp/filetransfer_%d %s", 
                    				 S->port,
                    				 S->destination );
                      		system( command );
                  		}
     	                //  Otherwise, we can't read it.
    	                else
    	                    filesize = -3;
    	            }
    	        }
    	    }

            //printf( "sending %d as confirmation\n", filesize );
            int n = htonl( filesize );            	
            write( sockfd, &n, sizeof( int ) );

      		//  Then clean up our litter.
    		snprintf( command, MAX_COMMAND_SIZE, "rm -f /tmp/filetransfer_%d", S->port );
            system( command );
            close( sockfd );
      		
    		exit(EXIT_SUCCESS);
    	}
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
    		snprintf( command, MAX_COMMAND_SIZE, "rm -f /tmp/filetransfer_%d", S->port );
            system( command );
    		snprintf( command, MAX_COMMAND_SIZE, "cp %s /tmp/filetransfer_%d", 
    				 S->origin,
    				 S->port );
      		system( command );
  		}
    	
    	//  Fork a process to do the file transfer via a TCP client.
    	signal( SIGCHLD, SIG_IGN );
    	childPid = fork();
    	if( childPid == 0 ) {
    	    //  Open a TCP socket connection to the server that should be running for us on the
    	    //  remote host.
    	    int sockfd;
    	    struct sockaddr_in servaddr;
    	    sockfd = socket( AF_INET, SOCK_STREAM, 0 );
    	    bzero( &servaddr, sizeof( servaddr ) );
    	    servaddr.sin_family = AF_INET;
    	    servaddr.sin_port = htons( S->port );
    	    inet_pton( AF_INET, S->address, &servaddr.sin_addr );
      	    int ret = connect( sockfd, (const sockaddr*)&servaddr, sizeof( servaddr ) );
      	    
      	    //  Assuming the socket connection was successful, write the file contents to the socket.
      	    if ( ret == 0 ) {
            	//snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection looks good", S->address, S->port );
            	//difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
            	//  Send the total size first.
            	int n = htonl( filesize );            	
            	write( sockfd, &n, sizeof( int ) );
            	if ( filesize > 0 ) {
            	    //  Then break the file up into "blocks" for sending.
            	    short blockSize = 1024;
            	    char blockData[blockSize];
		            const int tmpFileSize = 100;
            	    char tmpFile[tmpFileSize];
            	    snprintf( tmpFile, tmpFileSize, "/tmp/filetransfer_%d", S->port );
            	    int fd = open( tmpFile, O_RDONLY );
            	    while ( filesize > 0 ) {
            	        short readsize = read( fd, blockData, blockSize );
            	        short ns = htons( readsize );
            	        //write( sockfd, &ns, sizeof( short ) );
            	        write( sockfd, blockData, readsize );
            	        filesize -= readsize;
            	    }
            	    close( fd );
            	}
      	    }
      	    else {
            	snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection FAILED", S->address, S->port );
            	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
      	    }
    
            //  Clean up our litter and bail out of this forked process
    		snprintf( command, MAX_COMMAND_SIZE, "rm -f /tmp/filetransfer_%d", S->port );
            system( command );
      		close( sockfd );
      		
    		exit( EXIT_SUCCESS );
    	}
    	
	}
		
}

