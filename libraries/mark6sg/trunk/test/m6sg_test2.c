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

// A short program to test / try out some of the library write functions.

#include "mark6sg.h"

#include <errno.h>
#include <glob.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/time.h>

#define M_BUFSIZE (128*1024*1024)
#define OUT_FILE  "m6sg_test1_out.tmp"
#define SCAN_NAME "newTestScan_01.vdif"

// $ xxd --include singleframe.vdif
unsigned char singleframe_vdif[8224] = {
  0xb8, 0xa4, 0x84, 0x00, 0x00, 0x00, 0x00, 0x22, 0x04, 0x04, 0x00, 0x00,
  0x63, 0x4a, 0x00, 0x04, 0x07, 0x00, 0x00, 0x02, 0x0c, 0x00, 0x00, 0x00,
  0xd1, 0x7e, 0x3c, 0xd5, 0x08, 0x00, 0x00, 0x00, 0x17, 0x41, 0x96, 0x5b,
  0xb0, 0xaa, 0x96, 0xaf, 0x99, 0x05, 0x59, 0xf6, 0x6a, 0xfa, 0xaa, 0x85,
  0x2a, 0xe5, 0x6a, 0xa6, 0xaa, 0x96, 0x91, 0xbd, 0xa5, 0x5b, 0x69, 0x85,
  0x6b, 0x55, 0x92, 0x5a, 0x99, 0x6a, 0xbe, 0x5a, 0xa8, 0x16, 0xe9, 0x06,
  0xea, 0x5a, 0x90, 0x79, 0x05, 0x40, 0x5e, 0xda, 0x65, 0x25, 0xe9, 0x1b,
  0xd8, 0x57, 0xfa, 0x6b, 0x6e, 0x69, 0xe6, 0x55, 0x5a, 0xa9, 0x9a, 0x57,
  0xe5, 0x9d, 0x96, 0x11, 0xe4, 0xbb, 0x86, 0x2f, 0x94, 0xba, 0x40, 0x7d,
  0x00
};

volatile int ctrl_c = 0;

void intHandler(int dummy) {
    printf("Got Ctrl-C, exiting soon\n");
    ctrl_c = 1;
}

int main(int argc, char** argv)
{
    int    i, j, nfragments, fd;
    char **fragmentpaths;
    char **fragmentnames;
    struct timeval tv_start, tv_stop;
    double dT;
    ssize_t nwritten = 0;

    signal(SIGINT, intHandler);

    /* Change verbosity */
    mark6_sg_verbositylevel(0); // 0=none, 1=little, 2=debug, 3=more debug

    /* Set root to a single module */
    //mark6_sg_set_rootpattern("/mnt/disks/1/[0-7]/data/"); // actual module
    mark6_sg_set_rootpattern("/mnt/disks/[1-2]/[0-7]/dummy/"); // actual module, but dummy loc
    //mark6_sg_set_rootpattern("./[0-7]/data/"); // test location, assumed to have been created manually

    /* Generate list of scatter-gather files to create for a new scan */
    nfragments = mark6_sg_filelist_for_new(SCAN_NAME, &fragmentpaths, &fragmentnames);
    if (nfragments <= 0)
    {
        printf("Couldn't generate fragment filenames for future scan %s. Is the Mark6 root path correct?\n", SCAN_NAME);
        return -1;
    }

    for (i=0; i<nfragments; i++)
    {
        printf("Scan %s fragment %d/%d at %s\n", SCAN_NAME, i+1, nfragments, fragmentpaths[i]);
    }

    /* Clean-up for mark6_sg_filelist_for_new() */
    for (i=0; i<nfragments; i++)
    {
        free(fragmentpaths[i]);
        free(fragmentnames[i]);
    }
    free(fragmentpaths);
    free(fragmentnames);

    /* Hint at VDIF size before creat() */
    mark6_sg_packetsize_hint(sizeof(singleframe_vdif));

    /* Create new scan ; internally this invokes mark6_sg_filelist_for_new() tested above */
    fd = mark6_sg_creat(SCAN_NAME, S_IWUSR);

    /* Write some data */
    gettimeofday(&tv_start, NULL);
    for (j = 0; j < 640*nfragments && !ctrl_c; j++)
    {
        for (i = 0; i < 5000/*WRITER_FRAMES_PER_BLOCK*/; i++)
        {
            nwritten += mark6_sg_write(fd, singleframe_vdif, sizeof(singleframe_vdif));
        }
    }
    printf("Expected nr of blocks written is %zu\n", (size_t)8*nfragments);

    /* Cleanup (blocks until internal write() threads finished) */
    mark6_sg_close(fd);

    /* Speed report */
    gettimeofday(&tv_stop, NULL);
    dT = tv_stop.tv_sec - tv_start.tv_sec + 1e-6*(tv_stop.tv_usec - tv_start.tv_usec);
    printf("Test write finished in %.3f seconds after %zd bytes, %.3f Gbit/s\n", dT, nwritten, 8.0e-9*nwritten/dT);

    return 0;
}
