/***************************************************************************
 *   Copyright (C) 2008-2011 by Walter Brisken                             *
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
#include <unistd.h>
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

void Mk5Daemon_startMpifxcorr(Mk5Daemon *D, const DifxMessageGeneric *G)
{
	const int RestartOptionLength = 16;
	int l, n;
	int childPid;
	char filebase[DIFX_MESSAGE_FILENAME_LENGTH];
	char filename[DIFX_MESSAGE_FILENAME_LENGTH];
	char destdir[DIFX_MESSAGE_FILENAME_LENGTH];
	char message[DIFX_MESSAGE_LENGTH];
	char restartOption[RestartOptionLength];
	char command[MAX_COMMAND_SIZE];
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
	
	/*  Use the start function specified.  USNO is only option that does anything right now.  */
	switch ( S->function ) {
	case DIFX_START_FUNCTION_USNO:
		Mk5Daemon_startMpifxcorr_USNO( D,  G );
	    return;
	    break;
    case DIFX_START_FUNCTION_UNKNOWN:
	case DIFX_START_FUNCTION_DEFAULT:
	case DIFX_START_FUNCTION_NRAO:
	default:
	    break;
	}

	if(S->headNode[0] == 0 || S->nDatastream <= 0 || S->nProcess <= 0 || S->inputFilename[0] != '/')
	{
		difxMessageSendDifxAlert("Malformed DifxStart message received", DIFX_ALERT_LEVEL_ERROR);
		Logger_logData(D->log, "Mk5Daemon_startMpifxcorr: degenerate request\n");
		
		return;
	}

	if(G->nTo != 1)
	{
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
		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"Input file %s not found; cannot correlate it!", S->inputFilename);
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

	if(access(S->inputFilename, F_OK) != 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Input file %s does not exist.  Aborting correlation.", S->inputFilename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);

		snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_startMpifxcorr: input file %s does not exist\n", S->inputFilename);
		Logger_logData(D->log, message);

		return;
	}

	sprintf(filename, "%s.difx", filebase);
	if(access(filename, F_OK) == 0)
	{
		outputExists = 1;
	}
	
	if(outputExists && !S->force)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, 
			"Output file %s exists.  Aborting correlation.", 
			filename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"Mk5Daemon_startMpifxcorr: output file %s exists\n", 
			filename);
		Logger_logData(D->log, message);

		return;
	}

	/* lock state.  Make sure to unlock if early return happens! */
	pthread_mutex_lock(&D->processLock);

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
	sprintf(filename, "%s.machines", filebase);
	out = fopen(filename, "w");
	if(!out)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH,
			"Cannot open %s for write", filename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);

		snprintf(message, DIFX_MESSAGE_LENGTH,
			"Mk5Daemon_startMpifxcorr: cannot open %s for write\n", 
			filename);
		Logger_logData(D->log, message);

		pthread_mutex_unlock(&D->processLock);
		free(uses);
		
		return;
	}

#if 0
	fprintf(out, "%s slots=1 max-slots=%d\n", S->headNode, getUse(uses, S->headNode));
	for(int i = 0; i < S->nDatastream; ++i)
	{
		n = getUse(uses, S->datastreamNode[i]);
		fprintf(out, "%s slots=1 max-slots=%d\n", S->datastreamNode[i], n);
	}
	for(int i = 0; i < S->nProcess; ++i)
	{
		n = getUse(uses, S->processNode[i]);
		fprintf(out, "%s slots=1 max-slots=%d\n", S->processNode[i], n);
	}
#endif
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
	snprintf(command, MAX_COMMAND_SIZE, "chown --reference=%s %s", S->inputFilename, filename);
	Mk5Daemon_system(D, command, 1);
	
	snprintf(command, MAX_COMMAND_SIZE, "chmod --reference=%s %s", S->inputFilename, filename);
	Mk5Daemon_system(D, command, 1);


	/* write threads file */
	sprintf(filename, "%s.threads", filebase);
	
	out = fopen(filename, "w");
	if(!out)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open %s for write", filename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Mk5Daemon_startMpifxcorr: cannot open %s for write\n", filename);

		Logger_logData(D->log, message);
		pthread_mutex_unlock(&D->processLock);
		free(uses);

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
	snprintf(command, MAX_COMMAND_SIZE, "chown --reference=%s %s", S->inputFilename, filename);
	Mk5Daemon_system(D, command, 1);

	snprintf(command, MAX_COMMAND_SIZE, "chmod --reference=%s %s", S->inputFilename, filename);
	Mk5Daemon_system(D, command, 1);


	/* Don't need usage info anymore */
	free(uses);

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

	user = getenv("DIFX_USER_ID");
	if(!user)
	{
		user = difxUser;
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

				if(line[0] == '[')	/* likely an mpi error */
				{
					snprintf(message, DIFX_MESSAGE_LENGTH, "MPI Error: %s", line);
					difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
				}
			}
			returnValue = pclose(difxPipe);

			difxMessageSendDifxStatus2(jobName, DIFX_STATE_MPIDONE, "");

			snprintf(message, DIFX_MESSAGE_LENGTH, "mpifxcorr process done; return value = %d", returnValue);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);

			/* change ownership to match input file */
			snprintf(command, MAX_COMMAND_SIZE, "chown --recursive --reference=%s %s.difx", S->inputFilename, filebase);
			returnValue = system(command);

			snprintf(command, MAX_COMMAND_SIZE, "chmod g+w %s.difx", filebase);
			returnValue = system(command);

			snprintf(command, MAX_COMMAND_SIZE, "chmod --reference=%s %s.difx/*", S->inputFilename, filebase);
			returnValue = system(command);

			exit(EXIT_SUCCESS);
		}
	}

	/* if we got here, we are the parent process */
	/* now spawn the difxlog process. */
	if(fork() == 0)
	{
		snprintf(command, MAX_COMMAND_SIZE,
			"su - %s -c 'ssh -x %s \"difxlog %s %s.difxlog 4 %d &> /dev/null\"'",
			user, S->headNode, jobName, filebase, childPid);
		Mk5Daemon_system(D, command, 1);

		/* change ownership to match input file */
		snprintf(command, MAX_COMMAND_SIZE,
			"chown --reference=%s %s.difxlog", 
			S->inputFilename, filebase);
		Mk5Daemon_system(D, command, 1);

		snprintf(command, MAX_COMMAND_SIZE, 
			"chmod --reference=%s %s.difxlog", 
			S->inputFilename, filebase);
		Mk5Daemon_system(D, command, 1);

		exit(EXIT_SUCCESS);
	}
}

void Mk5Daemon_startMpifxcorr_USNO(Mk5Daemon *D, const DifxMessageGeneric *G)
{
	int l, n;
	int childPid;
	char filebase[DIFX_MESSAGE_FILENAME_LENGTH];
	char filename[DIFX_MESSAGE_FILENAME_LENGTH];
	char destdir[DIFX_MESSAGE_FILENAME_LENGTH];
	char message[DIFX_MESSAGE_LENGTH];
	char command[MAX_COMMAND_SIZE];
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
	
	if( S->nDatastream <= 0 || S->nProcess <= 0 || S->inputFilename[0] != '/' ) {
		difxMessageSendDifxAlert( "Malformed DifxStart (USNO) message received", DIFX_ALERT_LEVEL_ERROR );
		Logger_logData( D->log, "Mk5Daemon_startMpifxcorr: degenerate request\n" );
		
		return;
	}
	
	if(G->nTo != 1)
	{
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
		difxMessageSendDifxAlert("Since /tmp is full, mpifxcorr will not be started.",
								 DIFX_ALERT_LEVEL_ERROR);
		
		return;
	}
	
	/* Check to make sure the input file exists */
	if(access(S->inputFilename, R_OK) != 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, 
				 "Input file %s not found; cannot correlate it!",
				 S->inputFilename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		return;
	}
	
	snprintf(message, DIFX_MESSAGE_LENGTH, "DiFX version %s to be started", S->difxVersion);
	difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
	
	/* Check to make sure the destination directory has some free space */
	strcpy(destdir, S->inputFilename);
	n = 0;
	for(l = 0; destdir[l]; l++)
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
		snprintf(message, DIFX_MESSAGE_LENGTH, 
				 "The output directory %s is full, mpifxcorr will not be started.", 
				 destdir);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		return;
	}
	
	
//	if(!D->isHeadNode)
//	{
//		difxMessageSendDifxAlert("Attempt to start job from non head node", DIFX_ALERT_LEVEL_ERROR);
//		Logger_logData(D->log, "Mk5Daemon_startMpifxcorr: I am not a head node\n");
//		
//		return;
//	}
	
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
	for(int i = 0; filebase[i]; i++)
	{
		if(filebase[i] == '/')
		{
			jobName = filebase + i + 1;
		}
	}
	
	if(access(S->inputFilename, F_OK) != 0)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, 
				 "Input file %s does not exist.  Aborting correlation.",
				 S->inputFilename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		snprintf(message, DIFX_MESSAGE_LENGTH,
				 "Mk5Daemon_startMpifxcorr: input file %s does not exist\n", 
				 S->inputFilename);
		Logger_logData(D->log, message);
		
		return;
	}
	
	sprintf(filename, "%s.difx", filebase);
	if(access(filename, F_OK) == 0)
	{
		outputExists = 1;
	}
	
	if(outputExists && !S->force)
	{
		snprintf(message, DIFX_MESSAGE_LENGTH, 
				 "Output file %s exists.  Aborting correlation.", 
				 filename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		
		snprintf(message, DIFX_MESSAGE_LENGTH,
				 "Mk5Daemon_startMpifxcorr: output file %s exists\n", 
				 filename);
		Logger_logData(D->log, message);
		
		return;
	}
	
	/* lock state.  Make sure to unlock if early return happens! */
	pthread_mutex_lock(&D->processLock);
	
	/* determine usage of each node */
	uses = (Uses *)calloc(1 + S->nProcess + S->nDatastream, sizeof(Uses));
	addUse(uses, S->headNode);
	for(int i = 0; i < S->nProcess; i++)
	{
		addUse(uses, S->processNode[i]);
	}
	for(int i = 0; i < S->nDatastream; i++)
	{
		addUse(uses, S->datastreamNode[i]);
	}
	
	user = getenv( "DIFX_USER_ID" );
	if(!user)
	{
		user = difxUser;
	}
	
#if 0
	//  Write the "machines" file, first to a temporary location.
	out = fopen( "/tmp/machines", "w" );
	if( !out ) {
		snprintf( message, DIFX_MESSAGE_LENGTH, "Cannot open %s for write", 
				 filename );
		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		
		snprintf( message, DIFX_MESSAGE_LENGTH, 
				 "Mk5Daemon_startMpifxcorr: cannot open %s for write\n", 
				 filename );
		
		Logger_logData( D->log, message );
		pthread_mutex_unlock( &D->processLock );
		free( uses );
		
		return;
	}
	
	//  The first line of the machines file contains the head node and the number of
	//  threads it uses.
	int headNodeIndex = -1;
	for( int i = 0; i < S->nProcess; ++i ) {
		if ( !strcmp( S->processNode[i], S->headNode ) ) {
			fprintf( out, "%s slots=1 max-slots=%d\n", S->processNode[i], S->nThread[i] );
			headNodeIndex = i;
		}
	}
	
	//  A little error checking here - the head node needs to have its number of threads
	//  listed.
	if ( headNodeIndex == -1 ) {
		snprintf( message, DIFX_MESSAGE_LENGTH, "Head node %s is not in processors list - execution terminating", 
				 S->headNode );
		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		
		snprintf( message, DIFX_MESSAGE_LENGTH, 
				 "Head node %s is not in processors list - execution terminating", 
				 S->headNode );
		
		fclose( out );
		Logger_logData( D->log, message );
		pthread_mutex_unlock( &D->processLock );
		free( uses );
		
		return;
	}
	
	//  Next comes the list of data sources.
	for( int i = 0; i < S->nDatastream; ++i )
		fprintf( out, "%s slots=1 max-slots=1\n", S->datastreamNode[i] );
	
	//  Then a line for each head node thread NOT used for control (i.e. all but one).
	for ( int i = 1; i < S->nThread[headNodeIndex]; ++i )
		fprintf( out, "%s slots=1 max-slots=%d\n", S->processNode[headNodeIndex], S->nThread[headNodeIndex] );
	
	//  Then similar lines for each thread of all other non-head processing nodes.
	for( int i = 0; i < S->nProcess; ++i ) {
		if ( i != headNodeIndex ) {
			for ( int j = 0; j < S->nThread[i]; ++j )
				fprintf( out, "%s slots=1 max-slots=%d\n", S->processNode[i], S->nThread[i] );
		}
	}
	
	fclose(out);
	
	//  As the user (not root), copy the temporary file to the proper location.
	sprintf( filename, "%s.machines", filebase );
	snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'cp -f /tmp/machines %s'", 
			 user, S->headNode, filename );
	Mk5Daemon_system( D, command, 0 );
	
	//  Change permissions to match the input file
	snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod --reference=%s %s'", 
			 user, S->headNode, S->inputFilename, filename );
	Mk5Daemon_system(D, command, 1);
	
	//  Write the "threads" file, temporary first.
	out = fopen( "/tmp/threads", "w" );
	if( !out ) {
		snprintf( message, DIFX_MESSAGE_LENGTH, "Cannot open %s for write", 
				  filename );
		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		
		snprintf( message, DIFX_MESSAGE_LENGTH, 
				  "Mk5Daemon_startMpifxcorr: cannot open %s for write\n", 
				  filename );
		
		Logger_logData( D->log, message );
		pthread_mutex_unlock( &D->processLock );
		free( uses );
		
		return;
	}
	
	//  The first line of the threads file contains the number of processors
	//  used.
	fprintf( out, "NUMBER OF CORES:    %d\n", S->nProcess );
	
	//  The next line is the number of threads for the head node, less one (the one
	//  used to control things).
	for( int i = 0; i < S->nProcess; ++i ) {
		if ( !strcmp( S->processNode[i], S->headNode ) ) {
			if ( S->nThread[i] <= 1 )  //  shouldn't expect this...
				fprintf( out, "1\n" );
			else
				fprintf( out, "%d\n", S->nThread[i] - 1 );
		}
	}
	//  Then all of the non-head nodes.  These use all threads.
	for( int i = 0; i < S->nProcess; ++i ) {
		if ( strcmp( S->processNode[i], S->headNode ) ) {
			fprintf( out, "%d\n", S->nThread[i] );
		}
	}
	
	fclose(out);
	
	//  As the user (not root), copy the temporary file to the proper location.
	sprintf( filename, "%s.threads", filebase );
	snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'cp -f /tmp/threads %s'", 
			 user, S->headNode, filename );
	Mk5Daemon_system( D, command, 0 );
	
	//  Change permissions to match the input file
	snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod --reference=%s %s'", 
			 user, S->headNode, S->inputFilename, filename );
	Mk5Daemon_system(D, command, 1);
#endif

	//  Write a temporary data file to serve as input for "genmachines".  This file contains
	//  lines describing the number of threads and type of each machine used:
	//    MACHINE_NAME  THREADS   0 or 1 (0 for processing node, 1 for data source)
	sprintf( filename, "/tmp/difx_machines" );
	out = fopen( filename, "w" );
	//  Can't imagine why this would happen...
	if ( !out ) {
		snprintf(message, DIFX_MESSAGE_LENGTH, "Cannot open %s for write - unable to run genmachines", filename);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
		snprintf(message, DIFX_MESSAGE_LENGTH,
				 "Mk5Daemon_startMpifxcorr: cannot open %s for write\n", 
				 filename);
		Logger_logData(D->log, message);		
		pthread_mutex_unlock(&D->processLock);
		free(uses);		
		return;
	}
	//  Make the first line the head node.
	for( int i = 0; i < S->nProcess; ++i ) {
		if ( !strcmp( S->processNode[i], S->headNode ) )
            fprintf( out, "%s %d 0\n", S->processNode[i], S->nThread[i] );
    }
	//  Create the lines for each additional processor
	for( int i = 0; i < S->nProcess; ++i ) {
		if ( strcmp( S->processNode[i], S->headNode ) )
            fprintf( out, "%s %d 0\n", S->processNode[i], S->nThread[i] );
    }
	//  Data stream nodes have only one thread
	for( int i = 0; i < S->nDatastream; ++i )
		fprintf( out, "%s 1 1\n", S->datastreamNode[i] );
	fclose( out );
	
	//  Run genmachines on the temporary file.
	snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'genmachines -m %s %s'", 
			 user, S->headNode, filename, S->inputFilename );
	Mk5Daemon_system( D, command, 1 );

	//  Change permissions to match the input file
	sprintf( filename, "%s.machines", filebase );
	snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod --reference=%s %s'", 
			 user, S->headNode, S->inputFilename, filename );
	Mk5Daemon_system(D, command, 1);
	sprintf( filename, "%s.threads", filebase );
	snprintf( command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'chmod --reference=%s %s'", 
			 user, S->headNode, S->inputFilename, filename );
	Mk5Daemon_system(D, command, 1);

	/* Don't need usage info anymore */
	free(uses);
	
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
		if(S->force && outputExists)
		{
			snprintf(command, MAX_COMMAND_SIZE, 
					 "sh -x %s@%s '/bin/rm -rf %s.difx/'", user, S->headNode, filebase );
			
			snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", command);
			difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
			
			Mk5Daemon_system(D, command, 1);
		}
		
		difxMessageSendDifxAlert("mk5daemon spawning mpifxcorr process", DIFX_ALERT_LEVEL_INFO);
		
		char jobPath[512];
		char inputFile[512];
		int i = strlen( S->inputFilename );
		while ( S->inputFilename[i] != '.' )
			--i;
		strncpy( inputFile, S->inputFilename, i );
		inputFile[i] = 0;
		while ( S->inputFilename[i] != '/' )
			--i;
		strncpy( jobPath, S->inputFilename, i );
		jobPath[i] = 0;
		snprintf(command, MAX_COMMAND_SIZE, "ssh -x %s@%s 'source /usr/local/swc/difx/setup/setup.bash; cd %s; startdifx -n -f %s'", 
				 user,
				 S->headNode,
				 jobPath,
				 inputFile );
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Executing: %s", command);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_INFO);
		
		snprintf(message, DIFX_MESSAGE_LENGTH, "Spawning %d processes", 
				 1 + S->nDatastream + S->nProcess);
		difxMessageSendDifxStatus2(jobName, DIFX_STATE_SPAWNING, 
								   message);
		
#warning FIXME: make use of this return value
//		FILE* fp = Mk5Daemon_popen( D, command, 1 );
//		while ( fgets( message, DIFX_MESSAGE_LENGTH, fp ) != NULL )
//		    difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_INFO );
//		pclose( fp );
		
		difxMessageSendDifxStatus2(jobName, DIFX_STATE_MPIDONE, "");
		difxMessageSendDifxAlert("mpifxcorr process done", DIFX_ALERT_LEVEL_INFO);
		
		/* change ownership to match input file */
		snprintf(command, MAX_COMMAND_SIZE, 
				 "ssh -x %s@%s 'chown --recursive --reference=%s %s.difx'", 
				 user, S->headNode, S->inputFilename, filebase);
		Mk5Daemon_system(D, command, 1);
		
		snprintf(command, MAX_COMMAND_SIZE,
				 "ssh -x %s@%s 'chmod g+w %s.difx'", user, S->headNode, filebase);
		Mk5Daemon_system(D, command, 1);
		
		snprintf(command, MAX_COMMAND_SIZE, 
				 "ssh -x %s@%s 'chmod --reference=%s %s.difx/*'", 
				 user, S->headNode, S->inputFilename, filebase);
		Mk5Daemon_system(D, command, 1);
		
		exit(EXIT_SUCCESS);
	}
	
	/* if we got here, we are the parent process */
	/* now spawn the difxlog process. */
	if(fork() == 0)
	{
		snprintf(command, MAX_COMMAND_SIZE,
				 "ssh -x %s@%s 'difxlog %s %s.difxlog 4 %d &> /dev/null'",
				 user, S->headNode, jobName, filebase, childPid);
		Mk5Daemon_system(D, command, 1);
		
		/* change ownership to match input file */
		snprintf(command, MAX_COMMAND_SIZE,
				 "ssh -x %s@%s 'chown --reference=%s %s.difxlog'", 
				 user, S->headNode, S->inputFilename, filebase);
		Mk5Daemon_system(D, command, 1);
		
		snprintf(command, MAX_COMMAND_SIZE, 
				 "ssh -x %s@%s 'chmod --reference=%s %s.difxlog'", 
				 user, S->headNode, S->inputFilename, filebase);
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
	
	sprintf( message, "Stop request associated with %s - stop does not work at this time", S->inputFilename );
	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		
}

//-----------------------------------------------------------------------------
//!  Transfer the contents of a specified file from/to a client using a
//!  TCP connection.  This is responding to a request, presumably from the GUI.
//-----------------------------------------------------------------------------	
void Mk5Daemon_fileTransfer( Mk5Daemon *D, const DifxMessageGeneric *G ) {
	const DifxMessageFileTransfer *S;
	char message[DIFX_MESSAGE_LENGTH];
	
	if( !G ) {
		difxMessageSendDifxAlert(
								 "Developer error: Mk5Daemon_fileTransfer() received null DifxMessageGeneric",
								 DIFX_ALERT_LEVEL_ERROR);
		return;
	}
	
	S = &G->body.fileTransfer;
	
	if( S->origin[0] != '/' || S->destination[0] != '/' || S->address[0] == 0 || S->port <= 0 ||
	    ( strcmp( S->direction, "from client" ) && strcmp( S->direction, "to client" ) ) ) {
		difxMessageSendDifxAlert( "Malformed DifxFileTransfer message received", DIFX_ALERT_LEVEL_ERROR );
		Logger_logData( D->log, "Mk5Daemon_FileTransfer: degenerate request\n" );
		return;
	}
	
	if ( !strcmp( S->direction, "from client" ) )
	    sprintf( message, "Request for trasfer of file %s from GUI client to %s on DiFX host", S->origin, S->destination );
	else if ( !strcmp( S->direction, "to client" ) )
	    sprintf( message, "Request for trasfer of file %s from DiFX host to %s on GUI client", S->origin, S->destination );
	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
	sprintf( message, "Client address: %s   port: %d", S->address, S->port );
	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
		
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
	const char *user;
	
	if( !G ) {
		difxMessageSendDifxAlert(
								 "Developer error: Mk5Daemon_fileOperation() received null DifxMessageGeneric",
								 DIFX_ALERT_LEVEL_ERROR);
		return;
	}
	
	S = &G->body.fileOperation;
	
	if( S->path[0] != '/' ) {
		difxMessageSendDifxAlert( "Malformed DifxFileOperation message received", DIFX_ALERT_LEVEL_ERROR );
		Logger_logData( D->log, "Mk5Daemon_FileOperation: degenerate request\n" );
		return;
	}
	
	user = getenv( "DIFX_USER_ID" );
	if(!user)
	{
		user = difxUser;
	}

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
  		sprintf( message, "%s performed!", command );
	}
	else if ( !strcmp( S->operation, "rmdir" ) ) {
    	sprintf( message, "rmdir %s", S->path );
	}
	else if ( !strcmp( S->operation, "rm" ) ) {
    	sprintf( message, "rm %s", S->path );
	}
	else if ( !strcmp( S->operation, "mv" ) ) {
	    if ( S->arg[0] != '/' ) {
    		sprintf( message, "Destination of DifxFileOperation \"mv\" request (%s) must be a complete path", S->arg );
    		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
    		strcat( message, "\n" );
    		Logger_logData( D->log, message );
    		return;
	    }
    	sprintf( message, "mv %s %s", S->path, S->arg );
	}
	else {
		sprintf( message, "Illegal DifxFileOperation request received - operation \"%s\" is not permitted", S->operation );
		difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_ERROR );
		strcat( message, "\n" );
		Logger_logData( D->log, message );
		return;
	}
	difxMessageSendDifxAlert( message, DIFX_ALERT_LEVEL_WARNING );
		
}




