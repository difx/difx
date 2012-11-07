/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken & John Spitzak              *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id$
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/stat.h>
#include <sys/time.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>
#include <arpa/inet.h>
#include <pwd.h>
#include <sys/statvfs.h>
#include "mk5daemon.h"

const char defaultMpiWrapper[] = "mpirun";
const char defaultMpiOptions[] = "--mca mpi_yield_when_idle 1 --mca rmaps seq";
const char defaultDifxProgram[] = "mpifxcorr";

typedef struct
{
	char hostname[DIFX_MESSAGE_PARAM_LENGTH];
	int n;
} Uses;

static int addUse(Uses *U, const char *hostname)
{
	int i;

	for(i = 0; U[i].n; ++i)
	{
		if(strcmp(hostname, U[i].hostname) == 0)
		{
			++U[i].n;
			
			return i;
		}
	}

	strcpy(U[i].hostname, hostname);
	U[i].n = 1;

	return i;
}

static int getUse(const Uses *U, const char *hostname)
{
	for(int i = 0; U[i].n; ++i)
	{
		if(strcmp(hostname, U[i].hostname) == 0)
		{
			return U[i].n;
		}
	}

	return 0;
}

static int checkDiskFree(const char *path, long long minFree)
{
	struct statvfs fiData;
	char message[DIFX_MESSAGE_LENGTH] = "";
	int v;
	
	v = statvfs(path, &fiData);

	if(v == 0)
	{
		long long freeSpace;

		freeSpace = fiData.f_bsize * fiData.f_bavail;
		if(freeSpace < minFree)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, 
				"%s has less than %lld bytes free.  mpifxcorr will likely crash!", 
				path, minFree);
		}
		else if(fiData.f_ffree < 3)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, 
				"%s has no free inodes.  mpifxcorr will likely crash!", 
				path);
		}
	}
	else
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"statvfs failed when accessing directory %s : it seems not to exist!", 
			path);
	}

	if(message[0])
	{
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		return -1;
	}

	return 0;
}

static int isUsed(const Mk5Daemon *D, const DifxMessageStart *S)
{
	if(strcmp(S->headNode, D->hostName) == 0)
	{
		return 1;
	}
	for(int i = 0; i < S->nDatastream; ++i)
	{
		if(strcmp(S->datastreamNode[i], D->hostName) == 0)
		{
			return 1;
		}
	}
	for(int i = 0; i < S->nProcess; ++i)
	{
		if(strcmp(S->processNode[i], D->hostName) == 0)
		{
			return 1;
		}
	}

	return 0;
}

void Mk5Daemon_startMpifxcorr(Mk5Daemon *D, const DifxMessageGeneric *G, int noSu)
{
	const int RestartOptionLength = 16;
	int l, n;
	int childPid;
	char filebase[DIFX_MESSAGE_FILENAME_LENGTH];
	char filename[DIFX_MESSAGE_FILENAME_LENGTH];
	char workingDir[DIFX_MESSAGE_FILENAME_LENGTH];
	char destdir[DIFX_MESSAGE_FILENAME_LENGTH];
	char message[DIFX_MESSAGE_LENGTH];
  	char restartOption[RestartOptionLength];
	char command[MAX_COMMAND_SIZE];
	char chmodCommand[MAX_COMMAND_SIZE];
	FILE *out;
	Uses *uses;
	const char *jobName;
	const DifxMessageStart *S;
	int outputExists = 0;
	const char *mpiOptions;
	const char *mpiWrapper;
	const char *difxProgram;
	int returnValue;
	char altDifxProgram[64];
	const char *user;

	if(!G)
	{
		difxMessageSendDifxAlert(
			"Developer error: Mk5Daemon_startMpifxcorr received null DifxMessageGeneric",
			DIFX_ALERT_LEVEL_ERROR);
		
		return;
	}

	S = &G->body.start;
	
	if(S->headNode[0] == 0 || S->nDatastream <  0 || S->nProcess <= 0 || S->inputFilename[0] != '/')
	{
		difxMessageSendDifxAlert("Malformed DifxStart message received", DIFX_ALERT_LEVEL_ERROR);
		Logger_logData(D->log, "Mk5Daemon_startMpifxcorr: degenerate request\n");
		
		return;
	}

	if(G->nTo != 1)
	{
		difxMessageSendDifxAlert("Malformed DifxStart message received (empty to field)", DIFX_ALERT_LEVEL_ERROR);
                Logger_logData(D->log, "Mk5Daemon_startMpifxcorr: degenerate request (empty to field) \n");

		return;
	}

	/* Check to make sure /tmp has some free space */
	if(isUsed(D, S))
	{
		returnValue = checkDiskFree("/tmp", 24000);
	}
	else
	{
		returnValue = 0;
	}

	/* All recipients of this message should skip the next directory read just in case */
	D->skipGetModule = 1;

	if(strcmp(G->to[0], D->hostName) != 0)
	{
		return;
	}

	if(returnValue < 0)
	{
		difxMessageSendDifxAlert("Since /tmp is full, mpifxcorr will not be started.", DIFX_ALERT_LEVEL_ERROR);
		
		return;
	}

	/* Check to make sure the input file exists */
	if(access(S->inputFilename, R_OK) != 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Input file %s not found; cannot correlate it!", S->inputFilename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		return;
	}

	snprintf(message, DIFX_MESSAGE_LENGTH, "DiFX version %s to be started", S->difxVersion);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

	/* Check to make sure the destination directory has some free space */
	strcpy(destdir, S->inputFilename);
	n = 0;
	for(l = 0; destdir[l]; ++l)
	{
		if(destdir[l] == '/')
		{
			n = l;
		}
	}
	destdir[n+1] = 0;
	returnValue = checkDiskFree(destdir, 100000000);
	if(returnValue < 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "The output directory %s is full, mpifxcorr will not be started.", destdir);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		return;
	}


	if(!D->isHeadNode)
	{
		difxMessageSendDifxAlert("Attempt to start job from non head node", DIFX_ALERT_LEVEL_ERROR);
		Logger_logData(D->log, "Mk5Daemon_startMpifxcorr: I am not a head node\n");

		return;
	}

	if(strlen(S->inputFilename) + 12 > DIFX_MESSAGE_FILENAME_LENGTH)
	{
		difxMessageSendDifxAlert("Attempt to start job with filename that is too long", DIFX_ALERT_LEVEL_ERROR);

		return;
	}
	
	/* generate filebase */
	strcpy(filebase, S->inputFilename);
	l = strlen(filebase);
	for(int i = l-1; i > 0; i--)
	{
		if(filebase[i] == '.')
		{
			filebase[i] = 0;
			break;
		}
	}
	jobName = filebase;
	for(int i = 0; filebase[i]; ++i)
	{
		if(filebase[i] == '/')
		{
			jobName = filebase + i + 1;
		}
	}
	
	user = D->userID;

	//  Record the current permissions on the filebase (the working directory), and then
	//  change them to allow root to write there.  This is required if the working directory
	//  is on a disk mounted by the headnode, where root wouldn't necessarily be able to write.
	//  It is done only for "USNO" start requests because that's where it proved necessary,
	//  but might be useful generally?
	if(S->function == DIFX_START_FUNCTION_USNO)
	{
		strcpy(workingDir, S->inputFilename);
		l = strlen( workingDir );
		for(int i = l-1; i > 0; --i)
		{
			if(workingDir[i] == '/')
			{
				workingDir[i] = 0;
				break;
			}
		}
		
		struct stat statBuf;
		stat(workingDir, &statBuf);
		//  Build a command string for changing the directory BACK to this mode.  Maybe there
		//  is a better way to do this...
		snprintf(chmodCommand, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod ", user, S->headNode);

		int newperm = 0;
		if(statBuf.st_mode & S_IRUSR)
		{
			newperm += 4;
		}
		if(statBuf.st_mode & S_IWUSR)
		{
			newperm += 2;
		}
		if(statBuf.st_mode & S_IXUSR)
		{
			newperm += 1;
		}
		snprintf(chmodCommand + strlen(chmodCommand), MAX_COMMAND_SIZE - strlen(chmodCommand), "%d", newperm);
		newperm = 0;
		if(statBuf.st_mode & S_IRGRP)
		{
			newperm += 4;
		}
		if(statBuf.st_mode & S_IWGRP)
		{
			newperm += 2;
		}
		if(statBuf.st_mode & S_IXGRP)
		{
			newperm += 1;
		}
		snprintf(chmodCommand + strlen(chmodCommand), MAX_COMMAND_SIZE - strlen(chmodCommand), "%d", newperm);
		newperm = 0;
		if(statBuf.st_mode & S_IROTH)
		{
			newperm += 4;
		}
		if(statBuf.st_mode & S_IWOTH)
		{
			newperm += 2;
		}
		if(statBuf.st_mode & S_IXOTH)
		{
			newperm += 1;
		}
		snprintf(chmodCommand + strlen(chmodCommand), MAX_COMMAND_SIZE - strlen(chmodCommand), "%d %s'", newperm, workingDir);
		//  Change the permissions as the difx user.
		snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod 777 %s'", user, S->headNode, workingDir);
		Mk5Daemon_system(D, command, 1);
	}

	if(access(S->inputFilename, F_OK) != 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Input file %s does not exist.  Aborting correlation.", S->inputFilename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);

		snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_startMpifxcorr: input file %s does not exist\n", S->inputFilename);
		Logger_logData(D->log, message);

		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			Mk5Daemon_system(D, chmodCommand, 1);
		}

		return;
	}

	snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s.difx", filebase);
	if(access(filename, F_OK) == 0)
	{
		outputExists = 1;
	}
	
	if(outputExists && !S->force)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Output file %s exists.  Aborting correlation.", filename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_startMpifxcorr: output file %s exists\n", filename);
		Logger_logData(D->log, message);

		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			Mk5Daemon_system(D, chmodCommand, 1);
		}

		return;
	}

	/* lock state.  Make sure to unlock if early return happens! */
	pthread_mutex_lock(&D->processLock);

	/* unless no core processes are identified in the start message, write the machines, threads files */
	if(S->nProcess > 0)
	{
		/* determine usage of each node */
		uses = (Uses *)calloc(1 + S->nProcess + S->nDatastream, sizeof(Uses));
		addUse(uses, S->headNode);
		for(int i = 0; i < S->nProcess; ++i)
		{
			addUse(uses, S->processNode[i]);
		}
		for(int i = 0; i < S->nDatastream; ++i)
		{
			addUse(uses, S->datastreamNode[i]);
		}


		/* write machines file */
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s.machines", filebase);
		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			out = fopen("/tmp/machinefile", "w");
		}
		else
		{
			out = fopen(filename, "w");
		}
		if(!out)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open %s for write", filename);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);

			snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_startMpifxcorr: cannot open %s for write\n", filename);
			Logger_logData(D->log, message);

			pthread_mutex_unlock(&D->processLock);
			free(uses);
			
			if(S->function == DIFX_START_FUNCTION_USNO)
			{
				Mk5Daemon_system(D, chmodCommand, 1);
			}

			return;
		}

		fprintf(out, "%s\n", S->headNode);
		for(int i = 0; i < S->nDatastream; ++i)
		{
			fprintf(out, "%s\n", S->datastreamNode[i]);
		}
		for(int i = 0; i < S->nProcess; ++i)
		{
			fprintf(out, "%s\n", S->processNode[i]);
		}

		fclose(out);
		/* change ownership and permissions to match the input file */
		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'cp /tmp/machinefile %s'", user, S->headNode, filename);
		}
		else
		{
			snprintf(command, MAX_COMMAND_SIZE, "chown --reference=%s %s", S->inputFilename, filename);
		}
		Mk5Daemon_system(D, command, 1);

		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod --reference=%s %s'", user, S->headNode, S->inputFilename, filename);
		}
		else
		{
			snprintf(command, MAX_COMMAND_SIZE, "chmod --reference=%s %s", S->inputFilename, filename);
		}
		Mk5Daemon_system(D, command, 1);

		/* write threads file */
		snprintf(filename, DIFX_MESSAGE_FILENAME_LENGTH, "%s.threads", filebase);
		
		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			out = fopen("/tmp/threadfile", "w");
		}
		else
		{
			out = fopen(filename, "w");
		}
		if(!out)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open %s for write", filename);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
			
			snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_startMpifxcorr: cannot open %s for write\n", filename);

			Logger_logData(D->log, message);
			pthread_mutex_unlock(&D->processLock);
			free(uses);

			if(S->function == DIFX_START_FUNCTION_USNO)
			{
				Mk5Daemon_system(D, chmodCommand, 1);
			}

			return;
		}

		fprintf(out, "NUMBER OF CORES:    %d\n", S->nProcess);

		for(int i = 0; i < S->nProcess; ++i)
		{
			n = S->nThread[i] - getUse(uses, S->processNode[i]) + 1;
			if(n <= 0)
			{
				n = 1;
			}
			fprintf(out, "%d\n", n);
		}

		fclose(out);
		/* change ownership and permissions to match the input file */
		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'cp /tmp/threadfile %s'", user, S->headNode, filename );
		}
		else
		{
			snprintf(command, MAX_COMMAND_SIZE, "chown --reference=%s %s", S->inputFilename, filename);
		}
		Mk5Daemon_system(D, command, 1);

		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod --reference=%s %s'", user, S->headNode, S->inputFilename, filename );
		}
		else
		{
			snprintf(command, MAX_COMMAND_SIZE, "chmod --reference=%s %s", S->inputFilename, filename);
		}
		Mk5Daemon_system(D, command, 1);


		/* Don't need usage info anymore */
		free(uses);
	}

	pthread_mutex_unlock(&D->processLock);

	if(S->mpiOptions[0])
	{
		mpiOptions = S->mpiOptions;
	}
	else
	{
		mpiOptions = defaultMpiOptions;
	}

	if(S->mpiWrapper[0])
	{
		mpiWrapper = S->mpiWrapper;
	}
	else
	{
		mpiWrapper = defaultMpiWrapper;
	}

	if(S->difxProgram[0])
	{
		difxProgram = S->difxProgram;

		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"Using specified Difx Program: %s", difxProgram);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
	}
	else if(strcmp(S->difxVersion, "unknown") != 0)
	{
		snprintf(altDifxProgram, 63, "runmpifxcorr.%s", S->difxVersion);
		difxProgram = altDifxProgram;
		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"Using Difx Program wrapper: %s", difxProgram);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
	}
	else
	{
		difxProgram = defaultDifxProgram;

		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"Warning: using default Difx Program: %s", difxProgram);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
	}

	childPid = fork();

	/* here is where the spawning of mpifxcorr happens... */
	if(childPid == 0)
	{

		/* This is the child process.  Don't use Mk5Daemon_system or Logger_logData or anything
		 * else that writes to an already open file for fear of all sorts of weirdness */

		FILE *difxPipe = 0;

		difxMessageInit(-1, jobName);

		if(S->force && outputExists)
		{
			snprintf(command, MAX_COMMAND_SIZE, "/bin/rm -rf %s.difx/", filebase);

			snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", command);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
		
			system(command);
		}

		if(S->restartSeconds > 0.0)
		{
			snprintf(restartOption, RestartOptionLength, "-r %f", S->restartSeconds);
		}
		else
		{
			restartOption[0] = 0;
		}

		difxMessageSendDifxAlert("mk5daemon spawning mpifxcorr process", DIFX_ALERT_LEVEL_INFO);

        //  This is where the USNO mpirun process deviates.  This arrangement with "ssh" was necessary to avoid
        //  RSA key requests.
    	if ( S->function == DIFX_START_FUNCTION_USNO )  {
		    snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'source %s/setup.bash; %s -np %d --bynode --hostfile %s.machines %s %s %s %s 2>&1'", 
				    user,
				    S->headNode,
				    workingDir,
				    mpiWrapper,
			        1 + S->nDatastream + S->nProcess,
			        filebase,
			        mpiOptions,
			        difxProgram,
			        restartOption,
			        S->inputFilename );
	    }
	    
	    //  Normal (non-USNO) operation using "su".
	    else {
		
		if (noSu)
    		     snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s \"%s -np %d --bynode --hostfile %s.machines %s %s %s %s\" 2>&1", 
                            user,
                            S->headNode,
                            mpiWrapper,
                            1 + S->nDatastream + S->nProcess,
                            filebase,
                            mpiOptions,
                            difxProgram,
                            restartOption,
                            S->inputFilename);
		else
    		     snprintf(command, MAX_COMMAND_SIZE, "su - %s -c 'ssh -x %s \"%s -np %d --bynode --hostfile %s.machines %s %s %s %s\"' 2>&1", 
			    user,
			    S->headNode,
			    mpiWrapper,
			    1 + S->nDatastream + S->nProcess,
			    filebase,
			    mpiOptions,
			    difxProgram,
			    restartOption,
			    S->inputFilename);
	    }

		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", command);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

		snprintf(message, DIFX_MESSAGE_LENGTH, "Spawning %d processes", 1 + S->nDatastream + S->nProcess);
		difxMessageSendDifxStatus2(jobName, DIFX_STATE_SPAWNING, message);

		difxPipe = popen(command, "r");
		if(!difxPipe)
		{
			snprintf(message, DIFX_MESSAGE_LENGTH, "mpifxcorr process not started for job %s; popen returned NULL", jobName);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);

			exit(EXIT_FAILURE);
		}
		else
		{
			char line[DIFX_MESSAGE_LENGTH];

			for(;;)
			{
				const char *rv;

				rv = fgets(line, DIFX_MESSAGE_LENGTH, difxPipe);
				if(!rv)	/* eof, probably */
				{
					break;
				}

				for(int i = 0; line[i]; ++i)
				{
					if(line[i] == '\n')
					{
						line[i] = ' ';
					}
				}

                //  USNO requires different interpretation of messages (otherwise every diagnostic is interpreted as
                //  an error!).  Not sure why this is...
            	if ( S->function == DIFX_START_FUNCTION_USNO ) {
        		    if ( strstr( message, "ERROR" ) != NULL ) {
    					snprintf(message, DIFX_MESSAGE_LENGTH, "MPI: %s", line);
    					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
        		    }
        		    else if ( strstr( message, "WARNING" ) != NULL ) {
    					snprintf(message, DIFX_MESSAGE_LENGTH, "MPI: %s", line);
    					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_WARNING);
        		    }
        		    else {
    					snprintf(message, DIFX_MESSAGE_LENGTH, "MPI: %s", line);
    					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
        		    }
            	}
            	else {
    				if(line[0] == '[')	/* likely an mpi error */
    				{
    					snprintf(message, DIFX_MESSAGE_LENGTH, "MPI Error: %s", line);
    					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
    				}
				}
			}
			returnValue = pclose(difxPipe);

			difxMessageSendDifxStatus2(jobName, DIFX_STATE_MPIDONE, "");

			snprintf(message, DIFX_MESSAGE_LENGTH, "mpifxcorr process done; return value = %d", returnValue);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

			/* change ownership to match input file */
            if ( S->function == DIFX_START_FUNCTION_USNO )
        		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chown --recursive --reference=%s %s.difx'", user, S->headNode, S->inputFilename, filebase );
            else
                snprintf(command, MAX_COMMAND_SIZE, "chown --recursive --reference=%s %s.difx", S->inputFilename, filebase);
			returnValue = system(command);

            if ( S->function == DIFX_START_FUNCTION_USNO )
        		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod g+w %s.difx'", user, S->headNode, filebase );
            else
    			snprintf(command, MAX_COMMAND_SIZE, "chmod g+w %s.difx", filebase);
			returnValue = system(command);

            if ( S->function == DIFX_START_FUNCTION_USNO )
        		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod --reference=%s %s.difx/*'", user, S->headNode, S->inputFilename, filebase );
            else
    			snprintf(command, MAX_COMMAND_SIZE, "chmod --reference=%s %s.difx/*", S->inputFilename, filebase);
	    returnValue = system(command);

            if ( S->function == DIFX_START_FUNCTION_USNO )
                Mk5Daemon_system( D, chmodCommand, 1 );
            
			exit(EXIT_SUCCESS);
		}
	}

	/* if we got here, we are the parent process */
	/* now spawn the difxlog process. */
	if(fork() == 0)
	{
		if(S->function == DIFX_START_FUNCTION_USNO) 
		{
			snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'difxlog %s %s.difxlog 4 %d &> /dev/null'",
			     user, S->headNode, jobName, filebase, childPid);
		}
		else
		{
			if(noSu)
			{
				snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s \"difxlog %s %s.difxlog 4 %d &> /dev/null\"",
					user, S->headNode, jobName, filebase, childPid);
			}
			else
			{
				snprintf(command, MAX_COMMAND_SIZE, "su - %s -c 'ssh -x %s \"difxlog %s %s.difxlog 4 %d &> /dev/null\"'",
					user, S->headNode, jobName, filebase, childPid);
			}
		}
		Mk5Daemon_system(D, command, 1);	/* this will return when difxlog terminates. */

		/* change ownership to match input file */
		if(S->function != DIFX_START_FUNCTION_USNO)
		{
			snprintf(command, MAX_COMMAND_SIZE, "chown --reference=%s %s.difxlog", S->inputFilename, filebase);
			Mk5Daemon_system(D, command, 1);
		}

		if(S->function == DIFX_START_FUNCTION_USNO)
		{
			snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod --reference=%s %s.difxlog'", user, S->headNode, S->inputFilename, filebase);
		}
		else
		{
			snprintf(command, MAX_COMMAND_SIZE, "chmod --reference=%s %s.difxlog", S->inputFilename, filebase);
		}
		Mk5Daemon_system(D, command, 1);

		exit(EXIT_SUCCESS);
	}
}

//-----------------------------------------------------------------------------
//!  Stop a running mpifxcorr.
//-----------------------------------------------------------------------------	
void Mk5Daemon_stopMpifxcorr_USNO( Mk5Daemon *D, const DifxMessageGeneric *G ) {
	const DifxMessageStop *S;
	char message[DIFX_MESSAGE_LENGTH];
	
	if( !G ) {
		difxMessageSendDifxAlert(
								 "Developer error: Mk5Daemon_stopMpifxcorr received null DifxMessageGeneric",
								 DIFX_ALERT_LEVEL_ERROR);
		return;
	}
	
	S = &G->body.stop;
	
	if( S->inputFilename[0] != '/' ) {
		difxMessageSendDifxAlert( "Malformed DifxStop message received", DIFX_ALERT_LEVEL_ERROR );
		Logger_logData( D->log, "Mk5Daemon_stopMpifxcorr: degenerate request\n" );
		return;
	}
	
	snprintf( message, DIFX_MESSAGE_LENGTH, "Stop request associated with %s - stop does not work at this time", S->inputFilename );
	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		
}

//-----------------------------------------------------------------------------
//!  Transfer the contents of a specified file from/to a client using a
//!  TCP connection.  This is responding to a request, presumably from the GUI.
//-----------------------------------------------------------------------------	
void Mk5Daemon_fileTransfer( Mk5Daemon *D, const DifxMessageGeneric *G ) {
	const DifxMessageFileTransfer *S;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	char hostname[DIFX_MESSAGE_LENGTH];
	const char *user;
	pid_t childPid;
	
	if( !G ) {
		difxMessageSendDifxAlert(
								 "Developer error: Mk5Daemon_fileTransfer() received null DifxMessageGeneric",
								 DIFX_ALERT_LEVEL_ERROR);
		return;
	}
	
	S = &G->body.fileTransfer;
	
	//  Make sure this message is intended for this host
	gethostname( hostname, DIFX_MESSAGE_LENGTH );
	if ( strcmp( S->dataNode, hostname ) ) {
		return;
	}
	
	//  Check the sanity of the transfer message.
	if( S->address[0] == 0 || S->port <= 0 ||
	    ( strcmp( S->direction, "to DiFX" ) && strcmp( S->direction, "from DiFX" ) ) ) {
		difxMessageSendDifxAlert( "Malformed DifxFileTransfer message received", DIFX_ALERT_LEVEL_ERROR );
		Logger_logData( D->log, "Mk5Daemon_FileTransfer: degenerate request\n" );
		return;
	}
	
	user = D->userID;

	if ( !strcmp( S->direction, "to DiFX" ) ) {
	
    	//snprintf( message, DIFX_MESSAGE_LENGTH, "Request for transfer of file from %s on remote host to DiFX host - filesize is unknown", S->origin );
        //difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
    	//  Fork a process to do the file transfer via a TCP client.
    	childPid = fork();
    	if( childPid == 0 ) {
    	    //  Open a TCP socket connection to the server that should be running for us on the
    	    //  remote host.
    	    int sockfd;
    	    struct sockaddr_in servaddr;
    	    sockfd = socket( AF_INET, SOCK_STREAM, 0 );
    	    memset( &servaddr, 0, sizeof( servaddr ) );
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
                    		snprintf( command, MAX_COMMAND_SIZE, "chmod 644 /tmp/filetransfer_%d", S->port );
                            Mk5Daemon_system( D, command, 1 );
      		
                            //  Copy the new file to its specified location (as the DiFX user).
                    		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'cp /tmp/filetransfer_%d %s'", 
                    				 user,
                    				 S->dataNode,
                    				 S->port,
                    				 S->destination );
                      		Mk5Daemon_system( D, command, 1 );
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
            Mk5Daemon_system( D, command, 1 );
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
            Mk5Daemon_system( D, command, 1 );
    		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'cp %s /tmp/filetransfer_%d'", 
    				 user,
    				 S->dataNode,
    				 S->origin,
    				 S->port );
      		Mk5Daemon_system( D, command, 1 );
  		}
    	
    	//  Fork a process to do the file transfer via a TCP client.
    	childPid = fork();
    	if( childPid == 0 ) {
    	    //  Open a TCP socket connection to the server that should be running for us on the
    	    //  remote host.
    	    int sockfd;
    	    struct sockaddr_in servaddr;
    	    sockfd = socket( AF_INET, SOCK_STREAM, 0 );
    	    memset( &servaddr, 0, sizeof( servaddr ) );
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
            Mk5Daemon_system( D, command, 1 );
    		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'cp %s /tmp/filetransfer_%d'", 
    				 user,
    				 S->dataNode,
    				 S->origin,
    				 S->port );
      		Mk5Daemon_system( D, command, 1 );
      		close( sockfd );
      		
    		exit(EXIT_SUCCESS);
    	}
    	
	}
		
}

//-----------------------------------------------------------------------------
//!  Perform a file or directory operation on a specified path.  A limited
//!  number of operations are considered legal, and all are done as the
//!  difx user (not root) for obvious security reasons.
//-----------------------------------------------------------------------------	
void Mk5Daemon_fileOperation( Mk5Daemon *D, const DifxMessageGeneric *G ) {
	const DifxMessageFileOperation *S;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	char hostname[DIFX_MESSAGE_LENGTH];
	const char *user;
	pid_t childPid;
	
	if( !G ) {
		difxMessageSendDifxAlert(
								 "Developer error: Mk5Daemon_fileOperation() received null DifxMessageGeneric",
								 DIFX_ALERT_LEVEL_ERROR);
		return;
	}
	
	S = &G->body.fileOperation;
	
	//  Make sure this message is intended for this host
	gethostname( hostname, DIFX_MESSAGE_LENGTH );
	if ( strcmp( S->dataNode, hostname ) ) {
		return;
	}
	
	if( S->path[0] != '/' ) {
		difxMessageSendDifxAlert( "Malformed DifxFileOperation message received", DIFX_ALERT_LEVEL_ERROR );
		Logger_logData( D->log, "Mk5Daemon_FileOperation: degenerate request\n" );
		return;
	}
	
	user = D->userID;

	//  Check the file operation against the list of "legal" operations.
	if ( !strcmp( S->operation, "mkdir" ) ) {
	    //  Make a new directory with the given path.  The "-p" option will make the entire path.  The
	    //  operation should be silent if all goes well - any output from popen will be something bad
	    //  (thus we generate an error message).
		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'mkdir -p %s'", 
				 user,
				 S->dataNode,
				 S->path );
  		FILE* fp = Mk5Daemon_popen( D, command, 1 );
  		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL )
  		    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_INFO );
  		pclose( fp );	    
  		snprintf( message, DIFX_MESSAGE_LENGTH, "%s performed!", command );
	}
	else if ( !strcmp( S->operation, "rmdir" ) ) {
    	snprintf( message, DIFX_MESSAGE_LENGTH, "rmdir %s", S->path );
	}
	else if ( !strcmp( S->operation, "rm" ) ) {
	    //  Remove the files matching the given path description.
		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'rm %s %s'", 
				 user,
				 S->dataNode,
				 S->arg,
				 S->path );
  		FILE* fp = Mk5Daemon_popen( D, command, 1 );
  		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL )
  		    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_INFO );
  		pclose( fp );	    
  		snprintf( message, DIFX_MESSAGE_LENGTH, "%s performed!", command );
	}
	else if ( !strcmp( S->operation, "mv" ) ) {
	    if ( S->arg[0] != '/' ) {
    		snprintf( message, DIFX_MESSAGE_LENGTH, "Destination of DifxFileOperation \"mv\" request (%s) must be a complete path", S->arg );
    		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
    		strcat( message, "\n" );
    		Logger_logData( D->log, message );
    		return;
	    }
		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'mv %s %s'", 
				 user,
				 S->dataNode,
				 S->path,
				 S->arg );
  		FILE* fp = Mk5Daemon_popen( D, command, 1 );
  		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL )
  		    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_INFO );
  		pclose( fp );	    
  		snprintf( message, DIFX_MESSAGE_LENGTH, "%s performed!", command );
	}
	//  The "ls" operation actually returns data, so it must be provided with a TCP port and address.
	else if ( !strcmp( S->operation, "ls" ) ) {
	    //  Fork a process to do this.
    	childPid = fork();
    	if(childPid == 0)
    	{
        	//  Open a TCP socket connection to the server that should be running for us on the
            //  remote host.  This is used to transfer the list of files.
            int sockfd;
            struct sockaddr_in servaddr;
            sockfd = socket( AF_INET, SOCK_STREAM, 0 );
            memset( &servaddr, 0, sizeof( servaddr ) );
            servaddr.sin_family = AF_INET;
            servaddr.sin_port = htons( S->port );
            inet_pton( AF_INET, S->address, &servaddr.sin_addr );
            int ret = connect( sockfd, (const sockaddr*)&servaddr, sizeof( servaddr ) );
        
            //  Make sure the connection worked....
            if ( ret == 0 ) {
                //snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection looks good", S->address, S->port );
                //difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );

        		snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'ls %s %s'", 
        				 user,
        				 S->dataNode,
        				 S->arg,
        				 S->path );
        		//snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", command);
        		//difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

          		FILE* fp = Mk5Daemon_popen( D, command, 1 );
          		char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
          		//  Each line of response from the ls should be a filename...
          		while ( fgets( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, fp ) != NULL ) {
          		    //  Send the full path.
                	//  Each separate file is preceded by its string length.
                	int sz = htonl( strlen( fullPath ) );
                	write( sockfd, &sz, sizeof( int ) );
                	write( sockfd, fullPath, strlen( fullPath ) );
          		    //difxMessageSendDifxAlert( fullPath, DIFX_ALERT_LEVEL_INFO );
          	    }
                pclose( fp );	    
          		//  Sending a zero length tells the GUI that the list is finished.
                int zero = 0;
                write( sockfd, &zero, sizeof( int ) );

    		} 
		
    		//  Error with the socket...
    		else {
                snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection FAILED", S->address, S->port );
              	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            }
            close( sockfd );
        
        	exit(EXIT_SUCCESS);		
    	}
    	
	}
	else {
		snprintf( message, DIFX_MESSAGE_LENGTH, "Illegal DifxFileOperation request received - operation \"%s\" is not permitted", S->operation );
		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		strcat( message, "\n" );
		Logger_logData( D->log, message );
		return;
	}
	//difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
		
}

//-----------------------------------------------------------------------------
//!  Run vex2difx using specified data.  This is a command from the GUI.
//-----------------------------------------------------------------------------	
void Mk5Daemon_vex2DifxRun( Mk5Daemon *D, const DifxMessageGeneric *G ) {
	const DifxMessageVex2DifxRun *S;
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
	char difxPath[DIFX_MESSAGE_FILENAME_LENGTH];
	char roundup[DIFX_MESSAGE_LENGTH];
	char hostname[DIFX_MESSAGE_LENGTH];
	pid_t childPid;
	
	if( !G ) {
		difxMessageSendDifxAlert(
								 "Developer error: Mk5Daemon_fileTransfer() received null DifxMessageGeneric",
								 DIFX_ALERT_LEVEL_ERROR);
		return;
	}
	
	S = &G->body.vex2DifxRun;
	
	//  Make sure this message is intended for this host
	gethostname( hostname, DIFX_MESSAGE_LENGTH );
	if ( strcmp( S->headNode, hostname ) ) {
		return;
	}
	
	snprintf( message, DIFX_MESSAGE_LENGTH, "vex2difx command....%s, %s, %s, %s, %s",
             S->user,
             S->headNode,
             S->difxVersion,
             S->passPath,
             S->v2dFile );
	//difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );

	childPid = fork();
	
	//  Use a specified path to the DiFX software if the user has chosen one.  Otherwise, use
	//  the default path.
//	if ( strlen( S->difxVersion ) > 0 )
//	    strncpy( difxPath, S->difxVersion, DIFX_MESSAGE_FILENAME_LENGTH );
//	else
	    strncpy( difxPath, getenv( "DIFX_PREFIX" ), DIFX_MESSAGE_FILENAME_LENGTH );
	
	//  Forked process runs vex2difx...
	if(childPid == 0)
	{
	    //  Copy the .bash file for the difx user to the pass working directory.
		snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'cp %s/setup/setup.bash %s'", 
				 S->user,
				 S->headNode,
				 difxPath,
				 S->passPath );
		Mk5Daemon_system( D, command, 1 );
	    
	
	    //  Get the current time, used below to figure out which files in the directory
	    //  are new.
	    struct timeval tv;
	    gettimeofday( &tv, NULL );
		
		//  This is where we actually run vex2difx
		snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'source %s/setup/setup.bash; cd %s; vex2difx -f %s 2>&1'", 
				 S->user,
				 S->headNode,
				 difxPath,
				 S->passPath,
				 S->v2dFile );
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", command);
		//difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
		//printf( "%s\n", command );

        roundup[0] = 0;		
		FILE* fp = Mk5Daemon_popen( D, command, 1 );
		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL ) {
		    //  Try to create a clean "# jobs" message for transmission back to the GUI.
		    if ( strstr( message, "created" ) != NULL ) {
		        strncat( roundup, message, strcspn( message, "(" ) );
		        sprintf( roundup + strlen( roundup ), "(s) created in %s", S->passPath );
		        difxMessageSendDifxAlert( roundup, DIFX_ALERT_LEVEL_INFO );
		    }
		    else if ( !strncmp( message, "WARNING", 7 ) || !strncmp( message, "Warning", 7 ) ) {
		        difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
		    }
		    else if ( !strncmp( message, "ERROR", 5 ) || !strncmp( message, "Error", 5 ) ) {
		        difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		    }
		}
		pclose( fp );
		
		//  Next thing to run - calcif2.
		snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'source %s/setup/setup.bash; cd %s; calcif2 -f -a'", 
				 S->user,
				 S->headNode,
				 difxPath,
				 S->passPath );
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", command);
		//difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

        roundup[0] = 0;		
		fp = Mk5Daemon_popen( D, command, 1 );
		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL ) {
		    if ( !strncmp( message, "WARNING", 7 ) || !strncmp( message, "Warning", 7 ) ) {
		        difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
		    }
		    else if ( !strncmp( message, "ERROR", 5 ) || !strncmp( message, "Error", 5 ) ) {
		        difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		    }
		}
		pclose( fp );
		
    	//  Open a TCP socket connection to the server that should be running for us on the
        //  remote host.  This is used to transfer a list of all of the files we have created.
        int sockfd;
        struct sockaddr_in servaddr;
        sockfd = socket( AF_INET, SOCK_STREAM, 0 );
        memset( &servaddr, 0, sizeof( servaddr ) );
        servaddr.sin_family = AF_INET;
        servaddr.sin_port = htons( S->port );
        inet_pton( AF_INET, S->address, &servaddr.sin_addr );
        int ret = connect( sockfd, (const sockaddr*)&servaddr, sizeof( servaddr ) );
        
        //  Hopefully the connection worked....
        if ( ret == 0 ) {
            //snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection looks good", S->address, S->port );
            //difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
		    //  Produce a list of the files in the target directory and send that back to the GUI.
		    //  Include only those files that have been modified/created since the beginning of this
	    	//  function call (measured in integer seconds).  This will include a bunch of things
	    	//  we aren't interested in, but *should* only include those .input files that are new
	    	//  (unless someone is simultaneously running vex2difx during the same second!).
            struct dirent **namelist;
            int n = scandir( S->passPath, &namelist, 0, alphasort );
            if ( n < 0 ) {
                snprintf( message, DIFX_MESSAGE_LENGTH, "%s", strerror( errno ) );
                difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
            } else {
                while ( n-- ) {
                	char fullPath[DIFX_MESSAGE_FILENAME_LENGTH];
                	snprintf( fullPath, DIFX_MESSAGE_FILENAME_LENGTH, "%s/%s", S->passPath, namelist[n]->d_name );
                	struct stat buf;
                	stat( fullPath, &buf );
                	//  Compare the last modification time of each file in the pass directory
                	//  with the "gettimeofday" result at the top of this function.
                	if ( buf.st_mtime >= tv.tv_sec ) {
                	    //  Send the full path name of this file.
                	    //  Each separate file is preceded by its string length.
                	    int sz = htonl( strlen( fullPath ) );
                	    write( sockfd, &sz, sizeof( int ) );
                	    write( sockfd, fullPath, strlen( fullPath ) );
                	}
                    free( namelist[n] );
                }
                free( namelist );
                //  Sending a zero length tells the GUI that the list is finished.
                int zero = 0;
                write( sockfd, &zero, sizeof( int ) );
            }
		} 
		
		//  Error with the socket...
		else {
            snprintf( message, DIFX_MESSAGE_LENGTH, "Client address: %s   port: %d - connection FAILED", S->address, S->port );
          	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
        }
        close( sockfd );
        
		//difxMessageSendDifxAlert("vex2difx completed", DIFX_ALERT_LEVEL_INFO);
    	exit(EXIT_SUCCESS);
		
	}
}

	



