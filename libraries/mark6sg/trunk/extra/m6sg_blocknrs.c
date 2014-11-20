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

//
// This quick-check utility does not use the library.
// It prints the Mark6 Scatter-Gather block numbers in a given list of files.
//
// Example:
//   $  m6sg_blocknrs.c /mnt/disks/1/[0-7]/testrecording.vdif
//

#include <stdio.h>
#include <glob.h>
#include <stdlib.h>
#include <string.h>
#include <linux/limits.h>

#define MARK6_SG_MAXFILES 32

// Direct copy from d-plane-1.12 header file dplane.h
struct file_header_tag              // file header - one per file
    {
    unsigned int sync_word;         // see SYNC_WORD - nominally 0xfeed6666
    int version;                    // defines format of file
    int block_size;                 // length of blocks including header (bytes)
    int packet_format;              // format of data packets, enumerated below
    int packet_size;                // length of packets (bytes)
    };

struct wb_header_tag                // write block header - version 2
    {
    int blocknum;                   // block number, starting at 0
    int wb_size;                    // same as block_size, or occasionally shorter
    };

struct wb_header_tag_v1             // write block header - version 1
    {
    int blocknum;                   // block number, starting at 0
    };

int main(int argc, char** argv)
{
    FILE* f[MARK6_SG_MAXFILES];
    int i, nfiles, done=0;

    struct file_header_tag fhdr[MARK6_SG_MAXFILES];
    struct wb_header_tag   bhdr[MARK6_SG_MAXFILES];

    nfiles = argc - 1;

    if (nfiles <= 0)
    {
        printf("No files specified.\n");
        return 0;
    }

    for (i=0; i<nfiles; i++)
    {
        f[i] = fopen(argv[i+1], "r");
        fread(&fhdr[i], sizeof(struct file_header_tag), 1, f[i]);
        printf("File %d block size %d\n", i, fhdr[i].block_size);
    }

    while (!done)
    {
        for (i=0; i<nfiles; i++)
        {
            fread(&bhdr[i], sizeof(struct wb_header_tag), 1, f[i]);
            if (feof(f[i]))
            {
                printf("File %d EOF\n", i);
                done = 1;
                continue;
            }
            fseek(f[i], bhdr[i].wb_size - sizeof(struct wb_header_tag), SEEK_CUR);
            printf("File %d block size %d has nr %d\n", i, bhdr[i].wb_size, bhdr[i].blocknum);
        }
    }

    return 0;
}
