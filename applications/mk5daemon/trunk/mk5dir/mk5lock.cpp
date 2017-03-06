/***************************************************************************
 *   Copyright (C) 2010-2011 by Walter Brisken                             *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <mark5ipc.h>

static void usage(const char *cmd)
{
	printf("mk5lock\n\n");
	printf("A utility to manage the IPC semaphore locking access to Mark5\n\n");
	printf("Usage: %s [options]\n\n", cmd);
	printf("Options can include:\n\n");
	printf("  -l    Lock the unit\n\n");
	printf("  -u    Unlock the unit\n\n");
	printf("  -5    Lock the unit for 5 seconds\n\n");
	printf("  -lf   Lock the unit for fuse\n\n");
	printf("  -uf   Unlock the unit from fuse\n\n");
	printf("  -p    Print the status of the locks\n\n");
	printf("  -h    Print this useful help information\n\n");
}

void printLockInfo()
{
	printf("standard lock value %d last update pid %d\n", getMark5LockValue(), getMark5LockPID());
	printf("    fuse lock value %d last update pid %d\n", getFuseLockValue(), getFuseLockPID());
}

int main(int argc, char **argv)
{
	const char *action;
	int v;

	if(argc < 2)
	{
		usage(argv[0]);

		return EXIT_FAILURE;
	}
	action = argv[1];

	if(strcmp(action, "-l") == 0)
	{
		v = lockMark5(MARK5_LOCK_DONT_WAIT);
		if(v < 0)
		{
			printf("Sorry, unit is locked by process ID %d\n", getMark5LockPID());
		}
	}
	else if(strcmp(action, "-u") == 0)
	{
		unlockMark5();
	}
	else if(strcmp(action, "-lf") == 0)
	{
		v = lockFuse();
		if(v < 0)
		{
			printf("Sorry, unit is locked by process ID %d\n", getFuseLockPID());
		}
	}
	else if(strcmp(action, "-uf") == 0)
	{
		unlockFuse();
	}
	else if(strcmp(action, "-5") == 0)
	{
		v = lockMark5(MARK5_LOCK_DONT_WAIT);
		if(v < 0)
		{
			printf("Sorry, unit is locked\n");
		}

		sleep(5);
		
		unlockMark5();
	}
	else if(strcmp(action, "-p") == 0)
	{
		printLockInfo();
	}
	else
	{
		usage(argv[0]);

		if(strcmp(action, "-h") == 0)
		{
			return EXIT_SUCCESS;
		}
		else
		{
			return EXIT_FAILURE;
		}
	}

	return EXIT_SUCCESS;
}
