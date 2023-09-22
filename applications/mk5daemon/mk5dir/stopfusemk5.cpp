/***************************************************************************
 *   Copyright (C) 2017 by Mark Wainright                                  *
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
// $Id: mk5cp.cpp 7420 2016-08-05 16:58:20Z WalterBrisken $
// $HeadURL: https://svn.atnf.csiro.au/difx/applications/mk5daemon/trunk/mk5dir/mk5cp.cpp $
// $LastChangedRevision: 7420 $
// $Author: WalterBrisken $
// $LastChangedDate: 2016-08-05 10:58:20 -0600 (Fri, 05 Aug 2016) $
//
//============================================================================

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <signal.h>
#include <unistd.h>
#include <ctype.h>
#include <time.h>
#include <sys/time.h>
#include <difxmessage.h>
#include <mark5ipc.h>

const char program[] = "stopfusemk5";
const char author[]  = "Mark Wainright";
const char version[] = "0.01";
const char verdate[] = "20170203";

int verbose = 0;

int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	fprintf(stderr, "A program to stop a fuse mount on a Mark5 module and return control to mk5daemon\n");
	fprintf(stderr, "\nUsage : %s [<options>] stop\n\n", pgm);
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h             Print this help message\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v             Be more verbose\n\n");
	fprintf(stderr, "  --quiet\n");
	fprintf(stderr, "  -q             Be less verbose\n\n");

	return 0;
}

int main(int argc, char **argv)
{
	int v;
	char hostname[25];
	char confirm[5] = "";

	difxMessageInit(-1, "stopfusemk5");

	if(argc < 2)
	{
		return usage(argv[0]);
	}

	for(int a = 1; a < argc; ++a)
	{
		if(strcmp(argv[a], "-h") == 0 ||
		   strcmp(argv[a], "--help") == 0)
		{
			return usage(argv[0]);
		}
		else if(strcmp(argv[a], "-v") == 0 ||
		        strcmp(argv[a], "--verbose") == 0)
		{
			++verbose;
		}
		else if(strcmp(argv[a], "-q") == 0 ||
		        strcmp(argv[a], "--quiet") == 0)
		{
			--verbose;
		}
		else if(confirm[0] == 0)
		{
			strncpy(confirm, argv[a], 4);
			confirm[4] = 0;
		}
		else
		{
			return usage(argv[0]);
		}
	}

	// confirm the stop argument
	if(strcmp(confirm, "stop") != 0)
	{
		return usage(argv[0]);
	}

	// unmount fuse from /mnt/diskpack
	system("fusermount -u /mnt/diskpack");

	// tell mk5daemon to start VSIS
	gethostname(hostname, 25);
	difxMessageSendVsis("start", hostname);

	// enable access for other mk5 applications
	unlockFuse();

	return 0;
}
