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
#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <linux/limits.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "mark6_sg_utils.h"

// Global library debug level
int m_m6sg_dbglevel = 0;

// Internal structs
//
// There is apparently no documentation on the Mark6 non-RAID recording format(?).
// From Mark6 source code one can see that the several files associated with a
// single scan all look like this:
//
//  [file header]
//  [block a header] [block a data (~10MB)]
//  [block b header] [block b data (~10MB)]
//  [block c header] [block c data (~10MB)]
//  ...
//
// Block numbers are increasing. They will have gaps (e.g, a=0, b=16, c=35, ...).
// The 'missing' blocks (1,2,3..,15,17,18,..,34,...) of one file are found in one of
// the other files. Blocks can have different data sizes.
//
// Because 10GbE recording in Mark6 software is not done Round-Robin across files,
// the order of blocks between files is somewhat random.
//
// The below structures are adopted from the source code of the
// Mark6 program 'd-plane' version 1.12:

struct file_header_tag              // file header - one per file
{
    unsigned int sync_word;         // MARK6_SG_SYNC_WORD
    int version;                    // defines format of file
    int block_size;                 // length of blocks including header (bytes)
    int packet_format;              // format of data packets, enumerated below
    int packet_size;                // length of packets (bytes)
};

typedef struct wb_header_tag_v2     // write block header - version 2
{
    int blocknum;                   // block number, starting at 0
    int wb_size;                    // same as block_size, or occasionally shorter
} wb_header_tag_v2_t;

typedef struct wb_header_tag_v1     // write block header - version 1
{
    int blocknum;                   // block number, starting at 0
} wb_header_tag_v1_t;


//////////////////////////////////////////////////////////////////////////////////////
////// Library Functions /////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

/*
 * Change the verbosity level.
 */
int mark6_sg_verbositylevel(int level)
{
    m_m6sg_dbglevel = (level >= 0) ? level : m_m6sg_dbglevel;
    return level;
}

/**
 * Helper callback used internally in calls to glob()
 */
static int globerr(const char *path, int eerrno)
{
    fprintf(stderr, "Mark6 SG file list assembly: %s: %s\n", path, strerror(eerrno));
    return 0; // 0 to allow glob() to continue despite errors on single files
}

/**
 * List all "scans" on the Mark6.
 * Internally this first assembles a list of all Mark6 scatter-gather
 * file recordings (e.g. all files in"/mnt/disks/[1-4]/[0-7]/"). It then
 * builds a list of unique files based on the file name without file path.
 */
int mark6_sg_list_all_scans(char*** uniquenamelist)
{
    char** filepathlist;
    char** filenamelist;
    int nfiles, nuniques, i;

    nfiles   = mark6_sg_filelist_from_name("*", &filepathlist, &filenamelist);
    nuniques = mark6_sg_filelist_uniques(nfiles, (const char**)filenamelist, uniquenamelist);

    for (i=0; i<nfiles; i++)
    {
        free(filepathlist[i]);
        free(filenamelist[i]);
    }
    if (NULL != filepathlist) { free(filepathlist); }
    if (NULL != filenamelist) { free(filenamelist); }

    return nuniques;
}


/**
 * Assemble a list of files associated with a given scan name.
 * The scan name is allowed to contain wildcards. This allows to build
 * a list of scans, and their associated files.
 *
 * The scan name is essentially a base file name. Files with the same name
 * are searched for under the Mark6 scatter-gather mount points.
 * A list of these files (with and without prepended paths) is returned.
 */
int mark6_sg_filelist_from_name(const char* scanname, char*** filepathlist, char*** filenamelist)
{
    char*  searchpattern;
    char** fplist;
    char** fnlist;
    int    rc, nfiles, i;
    glob_t g;

    *filepathlist = NULL;
    *filenamelist = NULL;

    searchpattern = (char*)malloc(PATH_MAX);
    if (NULL == searchpattern) { return -1; }

    snprintf(searchpattern, PATH_MAX-1,  "%s%s", MARK6_SG_ROOT_PATTERN, scanname);
    rc = glob(searchpattern, GLOB_MARK, globerr, &g);
    free(searchpattern);

    if (rc != 0)
    {
        fprintf(stderr, "Mark6 SG file list assembly: '%s': %s\n", searchpattern,
           (rc == GLOB_ABORTED ? "filesystem problem"  :
            rc == GLOB_NOMATCH ? "no match of pattern" :
            rc == GLOB_NOSPACE ? "no dynamic memory"   :
            "unknown problem")
        );
        return 0;
    }

    nfiles = g.gl_pathc;
    fplist = (char**)malloc(nfiles*sizeof(char*));
    fnlist = (char**)malloc(nfiles*sizeof(char*));
    if (NULL == fplist || NULL == fnlist)
    {
        fprintf(stderr, "Mark6 SG file list assembly: error in alloc of output file list space.\n");
        globfree(&g);
        return 0;
    }

    for (i=0; i<nfiles; i++)
    {
        char* s   = strrchr(g.gl_pathv[i], '/');
        fplist[i] = strdup(g.gl_pathv[i]);
        if (NULL != s)
        {
            fnlist[i] = strdup(s+1);
        }
        else
        {
            fnlist[i] = strdup(g.gl_pathv[i]);
        }
        if (m_m6sg_dbglevel > 1) { printf(" %2d : %s : %s\n", i, fplist[i], fnlist[i]); }
    }

    globfree(&g);
    *filepathlist = fplist;
    *filenamelist = fnlist;

    return nfiles;
}

/**
 * Find all unique file names (ignoring the path) in a list of files.
 * Mainly used internally by mark6_sg_list_all_scans().
 */
int mark6_sg_filelist_uniques(int nfiles, const char** filenamelist, char*** uniquenamelist)
{
    int    nunique = 0;
    char** fqlist  = NULL;
    int    i, j;

    *uniquenamelist = NULL;

    if (nfiles <= 0 || filenamelist == NULL || uniquenamelist == NULL)
    {
        fprintf(stderr, "mark6_sg_filelist_uniques: bad call args\n");
        return 0;
    }

    // TODO: possibly poor implementation performance-wise, could improve this later?
    for (i=0; i<nfiles; i++)
    {
        int already_added = 0;
        for (j=0; j<nunique && !already_added; j++)
        {
            already_added = (strcasecmp(filenamelist[i], fqlist[j]) == 0);
        }
        if (!already_added)
        {
            char** newlist = (char**)realloc(fqlist, (nunique+1)*sizeof(char*));
            if (NULL == newlist)
            {
                fprintf(stderr, "mark6_sg_filelist_uniques: alloc error while growing the unique files list\n");
                break;
            }
            fqlist = newlist;
            fqlist[nunique] = strdup(filenamelist[i]);
            if (m_m6sg_dbglevel > 1) { printf(" sg file in list #%2d is unique -> scan #%2d : %s\n", i, nunique, filenamelist[i]); }
            nunique++;
        }
    }

    *uniquenamelist = fqlist;

    return nunique;
}

/**
 * Helper for qsort() to allow sorting a scatter-gather block list.
 */
int m6sg_block_comparefunc(const void* a, const void* b)
{
   m6sg_blockmeta_t* A = (m6sg_blockmeta_t*)a;
   m6sg_blockmeta_t* B = (m6sg_blockmeta_t*)b;
   return (A->blocknum - B->blocknum);
}

/**
 * Make an ordered list of the scatter-gather blocks in a list of files.
 * The files should be the files associated with a single scan.
 * The output list can be used to look up "linear" byte offsets in a
 * virtual "de-scattered" scan, sans all the metadata of the individual files.
 */
int mark6_sg_blocklist(int nfiles, const char** filenamelist, m6sg_blockmeta_t** blocklist)
{
    FILE*  f[MARK6_SG_MAXFILES];
    size_t nrd;
    int    nopened = 0;

    struct file_header_tag  fhdr[MARK6_SG_MAXFILES];
    struct wb_header_tag_v2 bhdr[MARK6_SG_MAXFILES];
    size_t bhdr_size = sizeof(wb_header_tag_v2_t);

    m6sg_blockmeta_t* blks = NULL;
    m6sg_blockmeta_t* prevblk = NULL;
    int nblocks = 0, nallocated = 0, nmissing = 0;
    int i, j;

    if (nfiles > MARK6_SG_MAXFILES)
    {
        fprintf(stderr, "mark6_sg_blocklist: nfiles %d > Mark6 limit of %d, will check only the first %d\n", nfiles, MARK6_SG_MAXFILES, MARK6_SG_MAXFILES);
        nfiles = MARK6_SG_MAXFILES;
    }

    // Get the file format descriptor of each file
    memset(&fhdr, 0, sizeof(fhdr));
    for (i=0; i<nfiles; i++)
    {
        f[i] = fopen(filenamelist[i], "r");
        if (NULL == f[i])
        {
            fprintf(stderr, "mark6_sg_blocklist: file %2d: could not open %s: %s\n", i, filenamelist[i], strerror(errno));
            continue;
        }

        fread(&fhdr[i], sizeof(struct file_header_tag), 1, f[i]);
        switch (fhdr[i].version)
        {
            case 1:
                bhdr_size = sizeof(wb_header_tag_v1_t);
                break;
            case 2:
                bhdr_size = sizeof(wb_header_tag_v2_t);
                break;
            default:
                fprintf(stderr, "mark6_sg_blocklist: file %2d: %s: unsupported Scatter Gather format version %d!", i, filenamelist[i], fhdr[i].version);
                fclose(f[i]);
                continue;
        }

        nopened++;
        if (m_m6sg_dbglevel > 0) { printf("mark6_sg_blocklist: file %2d block size %d bytes, packet size %d bytes\n", i, fhdr[i].block_size, fhdr[i].packet_size); }
    }

    // Get all block descriptors from all files
    memset(&bhdr, 0, sizeof(bhdr));
    if (m_m6sg_dbglevel > 0) { printf("Extracting block list from %d files...\n", nfiles); }
    while (nopened>0)
    {
        for (i=0; i<nfiles; i++)
        {
            if (f[i] == NULL) { continue; }

            // Get block header
            nrd = fread(&bhdr[i], bhdr_size, 1, f[i]);
            if ((nrd != 1) || feof(f[i]))
            {
                fclose(f[i]);
                f[i] = NULL;
                nopened--;
                continue;
            }
            if (m_m6sg_dbglevel > 2) { printf("File %2d block size %d has nr %d\n", i, bhdr[i].wb_size, bhdr[i].blocknum); }

            // Append block infos to our block list
            nblocks++;
            if (nblocks > nallocated)
            {
                nallocated = 2*nblocks;
                void* p = realloc(blks, nallocated*sizeof(m6sg_blockmeta_t));
                if (NULL == p)
                {
                    fprintf(stderr, "mark6_sg_blocklist: failed to grow in-memory block list to %d blocks!\n", nallocated);
                    for (j=0; j<nfiles; j++) { fclose(f[j]); }
                    nopened = 0;
                    nblocks--;
                    break;
                }
                blks = (m6sg_blockmeta_t*)p;
            }
            blks[nblocks-1].file_id     = i;
            blks[nblocks-1].packetsize  = fhdr[i].packet_size;
            blks[nblocks-1].blocknum    = bhdr[i].blocknum;
            blks[nblocks-1].file_offset = ftell(f[i]); // points to VLBI data, block header was already read/skipped
            blks[nblocks-1].datalen     = (fhdr[i].version == 1) ? (fhdr[i].block_size - bhdr_size) :
                                          (fhdr[i].version == 2) ? (bhdr[i].wb_size - bhdr_size)    : (fhdr[i].block_size - bhdr_size) ;

            // Seek to next block header
            fseek(f[i], blks[nblocks-1].datalen, SEEK_CUR);
        }
    }

    if (nblocks == 0)
    {
        fprintf(stderr, "mark6_sg_blocklist: no data found in files.\n");
        *blocklist = NULL;
        return nblocks;
    }

    // Sort the block list
    if (m_m6sg_dbglevel > 0) { printf("Got %d blocks. Sorting...\n", nblocks); }
    qsort((void*)blks, nblocks, sizeof(m6sg_blockmeta_t), m6sg_block_comparefunc);

    // Go through the block list and calculate "virtual" byte offsets
    prevblk = &blks[0];
    blks[0].virtual_offset = 0;
    for (i=1; i<nblocks; i++)
    {
        blks[i].virtual_offset = prevblk->virtual_offset + prevblk->datalen;

        if (blks[i].blocknum != (prevblk->blocknum + 1))
        {
            int gaplen = ((blks[i].blocknum-1) - (prevblk->blocknum+1));
            // NOTE:  if Mark6 used a variable block size it is impossible to tell here how much data were lost
            // NOTE2: choosing here to hop over missing data to have gapless virtual offsets, but in principle, could allow gaps via
            //    blks[i].virtual_offset +=  ((off_t)gaplen)*prevblk.datalen;
            nmissing += gaplen;
            if (m_m6sg_dbglevel > 0) { printf("Missing blocks %d < #blk < %d.\n", prevblk->blocknum+1, blks[i].blocknum-1); }
        }

        prevblk = &blks[i];
    }

    if (m_m6sg_dbglevel > 2)
    {
        for (i=0; i<nblocks; i++)
        {
            printf("Blocklist %2d --> file %2d, blocknum %2d, virt offset %ld\n", i, blks[i].file_id, blks[i].blocknum, blks[i].virtual_offset);
        }
    }

    if (nmissing > 0)
    {
        fprintf(stderr, "Scan is missing %d blocks of data, possibly due to a missing scatter-gather file.\n", nmissing);
    }

    *blocklist = blks;
    return nblocks;
}
