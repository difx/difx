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
    FILE* fo;
    int i, nscans, nfiles, fd;
    char** filepathlist;
    char** filenamelist;
    char** uniquenamelist;
    char*  fname;

    struct stat st;
    int packetsize;

    m6sg_blockmeta_t* blks;
    void* buf;

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
    // fname = strdup("wrtest_kjcc_test_2014y314d10h59m09s.vdif"); // some known other scan

    /* Extract some data from the scan */
    buf = malloc(M_BUFSIZE);

    printf("Opening a scan, seeking a bit, and reading ~128MB of data...\n");
    fd  = mark6_sg_open(fname, O_RDONLY);
    packetsize = mark6_sg_packetsize(fd);
    (off_t)mark6_sg_lseek(fd, ((off_t)packetsize)*14000, SEEK_SET);
    (size_t)mark6_sg_read(fd, buf, M_BUFSIZE);

    (int)mark6_sg_fstat(fd, &st);
    printf("File size %ld, block size %ld (=packet size)\n", st.st_size, st.st_blksize);
    (int)mark6_sg_close(fd);

    printf("Writing extracted data to %s\n", OUT_FILE);
    fo  = fopen(OUT_FILE, "w");
    fwrite(buf, M_BUFSIZE, 1, fo);
    fclose(fo);
    free(buf);

    /* Clean-up for mark6_sg_list_all_scans() */
    for (i=0; i<nscans; i++)
    {
        free(uniquenamelist[i]);
    }
    free(uniquenamelist);

    return 0;
}
