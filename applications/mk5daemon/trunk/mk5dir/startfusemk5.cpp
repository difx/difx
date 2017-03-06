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
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <difxmessage.h>
#include <mark5ipc.h>

const char program[] = "startfusemk5";
const char author[]  = "Mark Wainright";
const char version[] = "0.01";
const char verdate[] = "20170203";

int verbose = 0;

int usage(const char *pgm)
{
	fprintf(stderr, "\n%s ver. %s   %s %s\n\n", program, version, author, verdate);
	fprintf(stderr, "A program to start a fuse mount on a Mark5 module at mount point /mnt/diskpack/\n");
	fprintf(stderr, "\nUsage : %s [<options>] <bank>\n\n", pgm);
	fprintf(stderr, "options can include:\n");
	fprintf(stderr, "  --help\n");
	fprintf(stderr, "  -h             Print this help message\n\n");
	fprintf(stderr, "  --verbose\n");
	fprintf(stderr, "  -v             Be more verbose\n\n");
	fprintf(stderr, "  --quiet\n");
	fprintf(stderr, "  -q             Be less verbose\n\n");
	fprintf(stderr, "  --novsis\n");
	fprintf(stderr, "  -n             Do not send vsis stop message to mk5daemon (mk5control/mk5daemon use only)\n\n");
	fprintf(stderr, "<bank> is either A or B\n\n");

	return 0;
}

int main(int argc, char **argv)
{
	char bank = 0;
	char vsis = 1;
	int v;
	char hostname[25];

	difxMessageInit(-1, "startfusemk5");

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
		else if(strcmp(argv[a], "-n") == 0 ||
		        strcmp(argv[a], "--novsis") == 0)
		{
			vsis = 0;
		}
		else if(bank == 0)
		{
			bank = argv[a][0];
		}
		else
		{
			return usage(argv[0]);
		}
	}

	// validate bank identifier
        if(bank == 0 || !strchr("AaBb", bank))
	{
		fprintf(stderr, "\n!!! Bank %c is not valid.\n", bank);
		return usage(argv[0]);
	}

	// lock out other mk5 applications
	v = lockFuse();

	if(v < 0)
	{
		fprintf(stderr, "Another process has a lock on this Mark5 unit.\n");
		fprintf(stderr, "standard lock value %d last update PID %d\n", getMark5LockValue(), getMark5LockPID());
		fprintf(stderr, "    fuse lock value %d last update PID %d\n", getFuseLockValue(), getFuseLockPID());
	}
	else
	{
		// tell mk5daemon to stop VSIS
		if(vsis)
		{
			gethostname(hostname, 25);
			difxMessageSendVsis("stop", hostname);
		}

		// start fuse
		int pid, status, result;
		int i = 0;

		pid = fork();

		if(pid < 0)
		{
			fprintf(stderr,"startfusemk5: fork fail - %d\n", pid);
			return -1;
		}
		else if(pid == 0)
		{
			// child
			
			// setup output redirection
			int fd = open("/tmp/fuse.out", O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
			dup2(fd, 1); // redirect stdout
			dup2(fd, 2); // redirect stderr
			close(fd);   // can close fd, redirects are in place

			// execute fuseMk5
			if(bank == 'A' || bank == 'a')
			{
				execlp("fuseMk5", "fuseMk5", "/mnt/diskpack", (char *)NULL);
			}
			else
			{
				execlp("fuseMk5", "fuseMk5", "-b", "1", "/mnt/diskpack", (char *)NULL);
			}
		}
		else
		{
			// parent
			for(;;)
			{
				// check status of fuseMk5 child
				result = waitpid(pid, &status, WNOHANG);
				if(result < 0)
				{
					fprintf(stderr, "startfusemk5: waitpid: error result %d\n", result);
					break;
				}
				else if(result > 0)
				{
					fprintf(stderr,"startfusemk5: fuseMk5 pid %d exited, result %d, normal %d\n",
						pid, result, WIFEXITED(status));
					break;
				}

				// build and send a mk5 status message indicating continuing fuseMk5 use
				DifxMessageMk5Status dm;
				memset(&dm, 0, sizeof(DifxMessageMk5Status));
				dm.state = MARK5_STATE_FUSEMK5;
				if(bank == 'A' || bank == 'a')
				{
					strncpy(dm.vsnA, "  In Use", 8);
				}
				else
				{
					strncpy(dm.vsnB, "  In Use", 8);
				}
				dm.dataMJD = i;

				difxMessageSendMark5Status(&dm);
				
				i += 5;
				sleep(5);
			}

		}
	}

	return 0;
}
