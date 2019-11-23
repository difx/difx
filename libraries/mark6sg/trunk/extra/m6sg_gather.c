/***************************************************************************
 *   Copyright (C) 2014-2019 by Jan Wagner                                 *
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
//
// m6sg_gather [ --list | <scanname> <destination | - > ]
//
// Example program for the mark6sg library. Has functionality identical to the
// Mark6 utility 'gather'. Similar to the example program mk6copy except for
// arguments and printout details.
//
// Copies a single VLBI scan into a file by gathering together all the related
// files from the several disks of a scatter-gather mode VLBI recording.
//
//============================================================================

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>
#include <limits.h> // PATH_MAX since POSIX 2008

#include "mark6sg.h"

#define M_READ_BUF_SIZE (32*1024*1024)

void usage(void)
{
    printf("\n"
           "Mark6 SG library utility  m6sg_gather v1.0.1   Jan Wagner 20141121\n"
           "\n"
           "Copies a single VLBI scan into a file by gathering together all the\n"
           "piecewise files from several disks associated with scatter-gather\n"
           "mode VLBI recording in Mark6 software.\n"
           "\n"
           "Usage: m6sg_gather  [ --list | <scanname> <destination | - > ]\n"
           "\n"
           "   --list         show a list of available scan names on the disks\n"
           "\n"
           "   <scanname>     is the name of the scan on the disks, without paths\n"
           "   <destination>  is the output directory or full output file name and path\n"
           "\n"
           "The scanname will be used as the name of the output file if the specified\n"
           "destination is a directory. The destination '-' outputs to stdout for piping.\n"
           "\n"
    );
}


int main(int argc, char** argv)
{
    char** uniquenamelist;
    char*  scanname;
    char*  destination;

    ssize_t nrd, nwr;
    size_t  ntotal, ncopied;
    int     nscans, i;
    void*   buf;

    int    fdin;
    FILE*  fdout;
    struct stat st;
    struct timeval tstart;
    struct timeval tstop;
    double dT;

    int show_progress = 1;

    mark6_sg_verbositylevel(0);


    /* Command line arguments */
    if (getenv("MARK6_ROOT") != NULL)
    {
        mark6_sg_set_rootpattern(getenv("MARK6_ROOT"));
    }
    if (argc != 2 && argc != 3)
    {
        usage();
        exit(-1);
    }
    if (argc == 2 && strcmp(argv[1], "--list") != 0)
    {
        usage();
        exit(-1);
    }
    scanname    = argv[1];
    destination = argv[2];


    /* Mode I: List all scans behind Mark6 scatter-gather disk array mountpoints */
    if (argc == 2)
    {
        nscans = mark6_sg_list_all_scans(&uniquenamelist);
        if (nscans <= 0)
        {
            fprintf(stderr, "No scans. Are disks of the JBOD array / 'diskpack' mounted?\n");
        }
        else
        {
            fprintf(stderr, "There are %d scans in total. The scan names are:\n", nscans);
            for (i=0; i<nscans; i++)
            {
                fprintf(stdout, "\t%s\n", uniquenamelist[i]);
            }
        }
        return 0;
    }


    /* Mode II: Copy specified scan into a file */
    buf = malloc(M_READ_BUF_SIZE);

    // Input "file"
    fprintf(stderr, "Opening scatter-gather files of scan %s...\n", argv[1]);
    fdin = mark6_sg_open(argv[1], O_RDONLY);
    if (fdin < 0)
    {
        fprintf(stderr, "error: open failed (%s)\n", strerror(errno));
        exit(-1);
    }

    // Length of input "file"
    mark6_sg_fstat(fdin, &st);
    ncopied = 0;
    ntotal  = st.st_size;
    if (ntotal <= 0)
    {
        fprintf(stderr, "Scan seems to have no data. Stopping.\n");
        return 0;
    }

    // Output file
    if (strcmp(destination, "-") != 0)
    {
        stat(destination, &st);
        if (S_ISDIR(st.st_mode))
        {
            char tmp[PATH_MAX];
            snprintf(tmp, PATH_MAX-1, "%s/%s", destination, scanname);
            destination = strdup(tmp);
        }
        fdout = fopen(destination, "w");
        if (fdout == NULL)
        {
            fprintf(stderr, "error: could not open output file %s (%s)\n", destination, strerror(errno));
            exit(-1);
        }
    }
    else
    {
        fdout = stdout;
    }

    // Copy
    fprintf(stderr, "Copying to output file %s...\n", destination);
    gettimeofday(&tstart, NULL);
    if (show_progress)
    {
        setbuf(stdout, NULL);
    }
    while (ncopied < ntotal)
    {
        size_t nremain = ntotal - ncopied;
        size_t nwanted = (nremain < M_READ_BUF_SIZE) ? nremain : M_READ_BUF_SIZE;

        nrd = mark6_sg_read(fdin, buf, nwanted);
        if (nrd <= 0)
        {
            fprintf(stderr, "\n unexpected EOF\n");
            break;
        }

        nwr = fwrite(buf, nrd, 1, fdout);
	if (nwr != nrd)
	{
            fprintf(stderr, "Write error (disk full?)\n");
            fprintf(stderr, "  %zd bytes written\n", ncopied + nwr);
            fprintf(stderr, "  %zd bytes remained to be written\n", nremain - nwr);
            break;
	}
        nremain -= nrd;
        ncopied += nrd;

        if(show_progress)
        {
            fprintf(stderr, " %ld/%ld MByte                 \r", ncopied/(1024*1024), ntotal/(1024*1024));
        }
    }
    gettimeofday(&tstop, NULL);

    dT = (tstop.tv_sec - tstart.tv_sec) + 1e-6*(tstop.tv_usec - tstart.tv_usec);
    fprintf(stderr, "Elapsed time %.2f seconds (excluding file open time); rate %.0f MB/s; %ld bytes copied.\n",
           dT, ((double)ncopied)/(1024.0*1024.0*dT), ncopied
    );

    fclose(fdout);
    mark6_sg_close(fdin);

    return 0;
}
