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

// A short program to test / try out some of the library functions.

#include <errno.h>
#include <glob.h>
#include <linux/limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>

#include "mark6sg.h"

#define M_BUFSIZE (128*1024*1024)
#define OUT_FILE  "m6sg_test1_out.tmp"

int main(int argc, char** argv)
{
    int    i, nscans, fd;
    char** uniquenamelist;
    char*  fname;

    FILE*   fo;
    void*   buf;
    ssize_t nrd;
    struct  stat st;

    /* Change verbosity */
    mark6_sg_verbositylevel(0); // 0=none, 1=little, 2=debug, 3=more debug

    /* List of all uniquely named files behind Mark6 scatter-gather mountpoints */
    nscans = mark6_sg_list_all_scans(&uniquenamelist);
    if (nscans <= 0)
    {
        printf("No scans. Are disks of the JBOD array / 'diskpack' mounted?\n");
        return -1;
    }

    for (i=0; i<nscans; i++)
    {
        printf("Scan %3d/%d : %s\n", i, nscans, uniquenamelist[i]);
    }

    /* Choose a scan */
    fname = uniquenamelist[0]; // first scan
    //fname = strdup("wrtest_kjcc_test_2014y314d10h59m09s.vdif"); // some known other scan

    /* Extract some data from the scan */
    buf = malloc(M_BUFSIZE);

    printf("Opening a scan...\n");
    fd  = mark6_sg_open(fname, O_RDONLY);
    if (fd < 0)
    {
        printf("Error opening scan: %s\n", strerror(errno));
        exit(-1);
    }

    printf("Getting scan infos...\n");
    (int)mark6_sg_fstat(fd, &st);
    printf("Stat: file size %ld, block size %ld.  mark6_sg_packetsize(): %d bytes\n",
            st.st_size, st.st_blksize, mark6_sg_packetsize(fd)
    );

    printf("Doing lseek() to skip 14000 packets...\n");
    (off_t)mark6_sg_lseek(fd, ((off_t)mark6_sg_packetsize(fd))*14000, SEEK_SET);

    printf("Trying to read...\n");
    nrd = mark6_sg_read(fd, buf, M_BUFSIZE);
    if (nrd > 0)
    {
        printf("Writing %ld bytes of extracted data to %s\n", nrd, OUT_FILE);
        fo  = fopen(OUT_FILE, "w");
        fwrite(buf, nrd, 1, fo);
        fclose(fo);
    }
    else
    {
        printf("Scan %s could not be read or contained no data\n", fname);
    }
    (int)mark6_sg_close(fd);
    free(buf);

    /* Clean-up for mark6_sg_list_all_scans() */
    for (i=0; i<nscans; i++)
    {
        free(uniquenamelist[i]);
    }
    free(uniquenamelist);

    return 0;
}
