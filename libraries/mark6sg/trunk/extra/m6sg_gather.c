/***************************************************************************
 *   Copyright (C) 2014 by Jan Wagner                                      *
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

// A program with functionality identical to the Mark6 utility 'gather'.
//
// Copies a single VLBI scan into a file by gathering together all the related
// files on the several disks of a scatter-gather mode VLBI recording.

#include <errno.h>
#include <fcntl.h>
#include <linux/limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "mark6sg.h"

#define M_READ_BUF_SIZE (32*1024*1024)

void usage(void)
{
    printf("\n"
           "Mark6 SG library utility  m6sg_gather v1.0.0   Jan Wagner 20141121\n"
           "\n"
           "Copies a single VLBI scan into a file by gathering together all the\n"
           "piecewise files from several disks associated with scatter-gather\n"
           "mode VLBI recording in Mark6 software.\n"
           "\n"
           "Usage: m6sg_gather [--list | <scanname> <destination>]\n"
           "\n"
           "   --list         show a list of available scan names on the disks\n"
           "\n"
           "   <scanname>     is the name of the scan on the disks, without paths\n"
           "   <destination>  is the output directory or full output file name and path\n"
           "\n"
           "The scanname will be used as the name of the output file if the specified\n"
           "destination is a directory.\n"
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

    int show_progress = 1;

    mark6_sg_verbositylevel(0);


    /* Command line arguments */
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
            printf("No scans. Are disks of the JBOD array / 'diskpack' mounted?\n");
        }
        else
        {
            printf("There are %d scans in total. The scan names are:\n", nscans);
            for (i=0; i<nscans; i++)
            {
                printf("\t%s\n", uniquenamelist[i]);
            }
        }
        return 0;
    }


    /* Mode II: Copy specified scan into a file */
    buf = malloc(M_READ_BUF_SIZE);

    // Input "file"
    printf("Opening scatter-gather files of scan %s...\n", argv[1]);
    fdin = mark6_sg_open(argv[1], O_RDONLY);
    if (fdin < 0)
    {
        printf("error: open failed (%s)\n", strerror(errno));
        exit(-1);
    }

    // Length of input "file"
    mark6_sg_fstat(fdin, &st);
    ncopied = 0;
    ntotal  = st.st_size;
    if (ntotal <= 0)
    {
        printf("Scan seems to have no data. Stopping.\n");
        return 0;
    }

    // Output file
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
        printf("error: could not open output file %s (%s)\n", destination, strerror(errno));
        exit(-1);
    }

    // Copy
    printf("Copying to output file %s...\n", destination);
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
            printf("\n unexpected EOF\n");
            break;
        }

        nwr = fwrite(buf, nrd, 1, fdout);
        nremain -= nrd;
        ncopied += nrd;

        if(show_progress)
        {
            printf(" %ld/%ld MByte                 \r", ncopied/(1024*1024), ntotal/(1024*1024));
        }
    }
    printf("Copied %ld of %ld bytes.\n", ncopied, ntotal);

    fclose(fdout);
    mark6_sg_close(fdin);

    return 0;
}
