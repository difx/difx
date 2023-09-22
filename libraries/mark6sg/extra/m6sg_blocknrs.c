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
// m6sg_blocknrs /mnt/disks/1/[0-7]/testrecording.vdif
//
// This quick-check utility does not use the library.
// It prints the Mark6 Scatter-Gather block numbers in a given list of files.
//
//============================================================================

#include <glob.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

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

void usage(void)
{
    printf("\nUsage: m6sg_blocknrs <fragment files>\n\n");
    printf("Prints the Mark6 Scatter-Gather block numbers in a given list of files.\n\n");
    printf("Example: m6sg_blocknrs /mnt/disks/1/[0-7]/testrecording.vdif\n");
}

int main(int argc, char** argv)
{
    FILE* f[MARK6_SG_MAXFILES];
    int i, nfiles, done=0;

    struct file_header_tag fhdr[MARK6_SG_MAXFILES];
    struct wb_header_tag   bhdr[MARK6_SG_MAXFILES];
    off_t offset[MARK6_SG_MAXFILES];
    off_t file_size[MARK6_SG_MAXFILES];
    off_t blocknum[MARK6_SG_MAXFILES];

    nfiles = argc - 1;

    if (nfiles <= 0)
    {
        usage();
        return 0;
    }

    for (i=0; i<nfiles; i++)
    {
        size_t nrd;
        struct stat st;

        offset[i] = 0;
        blocknum[i] = -1;

        f[i] = fopen(argv[i+1], "r");
        if (!f[i])
        {
            fprintf(stderr, "Error: Could not open %s.\n", argv[i+1]);
            return 0;
        }

        nrd = fread(&fhdr[i], sizeof(struct file_header_tag), 1, f[i]);
        if (nrd != 1)
        {
            fprintf(stderr, "Error: could not read block size in %s.\n", argv[i+1]);
            return 0;
        }
        if (fhdr[i].sync_word != 0xfeed6666)
        {
            fprintf(stderr, "Warning: file header of file %d has unexpected sync word %08x\n", i, fhdr[i].sync_word);
        }

        nrd = stat(argv[i+1], &st);
        if (nrd != 0)
        {
           file_size[i] = (off_t)-1;
           fprintf(stderr, "Warning: could not stat() file %d for file size\n", i);
        }
        else
        {
           file_size[i] = st.st_size;
        }

        fprintf(stderr, "File header of file %d: version %d, block size %d, packet_format %d, packet_size %d\n",
            i, fhdr[i].version, fhdr[i].block_size, fhdr[i].packet_format, fhdr[i].packet_size
        );
    }

    printf("# <fileNr> <offset> <wb_size> <blocknr>\n");

    while (!done)
    {
        for (i=0; i<nfiles; i++)
        {
            size_t nrd;

            memset(&bhdr[i], 0, sizeof(struct wb_header_tag));
            nrd = fread(&bhdr[i], sizeof(struct wb_header_tag), 1, f[i]);
            if (feof(f[i]))
            {
                fprintf(stderr, "File %d EOF\n", i);
                done = 1;
                continue;
            }
            else if (nrd != 1)
            {
                fprintf(stderr, "Error: file %d read error, but not EOF\n", i);
                done = 1;
                continue;
            }

            printf("%d %ld %u %u\n", i, offset[i], bhdr[i].wb_size, bhdr[i].blocknum);

            if (bhdr[i].wb_size != fhdr[i].block_size && offset[i] < (file_size[i] - fhdr[i].block_size))
            {
                fprintf(stderr,
                    "ERROR: file %d at offset %ld: mismatch in current wb_size %u vs. file block_size %u : blk %u, previous blk %u\n",
                    i, offset[i], bhdr[i].wb_size, fhdr[i].block_size, bhdr[i].blocknum, blocknum[i]
                );

                do
                {
                   memset(&bhdr[i], 0, sizeof(struct wb_header_tag));
                   nrd = fread(&bhdr[i], sizeof(struct wb_header_tag), 1, f[i]);
                } while (nrd > 0 && bhdr[i].wb_size != fhdr[i].block_size);

                fprintf(stderr,
                    "  resync: candidate header with wb_size %u blk %d found off by +%ld from de-sync position %ld\n",
                    bhdr[i].wb_size, bhdr[i].blocknum, ftello(f[i]) - offset[i] - sizeof(struct wb_header_tag), offset[i]
                );
            }

            fseek(f[i], bhdr[i].wb_size - sizeof(struct wb_header_tag), SEEK_CUR);
            offset[i] = ftello(f[i]);
            blocknum[i] = bhdr[i].blocknum;
        }
    }

    return 0;
}
