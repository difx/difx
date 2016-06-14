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
#include "mark6_sg_utils.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <linux/limits.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <jsmn.h> // Jasmine/JSMN C parser library for JSON


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

// File header - one per file
struct file_header_tag
{
    uint32_t sync_word;         // MARK6_SG_SYNC_WORD
    int32_t  version;           // defines format of file
    int32_t  block_size;        // length of blocks including header (bytes)
    int32_t  packet_format;     // format of data packets, enumerated below
    int32_t  packet_size;       // length of packets (bytes)
};

// Write block header - version 2
typedef struct wb_header_tag_v2
{
    int32_t blocknum;           // block number, starting at 0
    int32_t wb_size;            // same as block_size, or occasionally shorter
} wb_header_tag_v2_t;

// Write block header - version 1
typedef struct wb_header_tag_v1
{
    int32_t blocknum;           // block number, starting at 0
} wb_header_tag_v1_t;

// Base directory
static char* mark6_sg_root_pattern = MARK6_SG_ROOT_PATTERN;
static int mark6_sg_root_pattern_changed = 0;

//////////////////////////////////////////////////////////////////////////////////////
////// Local Functions   /////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////

typedef struct blklist_thread_ctx_tt {
    const char*       filename;
    int               fileidx;
    m6sg_blockmeta_t* blks;
    size_t            nallocated;
    size_t            nblocks;
} blklist_thread_ctx_t;


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
 * Thread worker that reads all block headers from one file.
 */
static void* thread_extract_file_blocklist(void* v_args)
{
    struct file_header_tag  fhdr;
    struct wb_header_tag_v2 bhdr;
    size_t bhdr_size;
    struct stat st;

    blklist_thread_ctx_t* ctx = (blklist_thread_ctx_t*)v_args;
    ctx->nblocks    = 0;
    ctx->nallocated = 0;
    ctx->blks       = NULL;

    FILE* f = fopen(ctx->filename, "r");
    if (NULL == f)
    {
        fprintf(stderr, "thread_extract_file_blocklist: file %2d: could not open %s: %s\n", ctx->fileidx, ctx->filename, strerror(errno));
        return NULL;
    }

    // Make good initial guess for #blocks in file
    stat(ctx->filename, &st);
    ctx->nallocated = st.st_size/9998760L;
    ctx->blks = malloc(ctx->nallocated * sizeof(m6sg_blockmeta_t));

    // Read main header
    memset(&fhdr, 0, sizeof(fhdr));
    fread(&fhdr, sizeof(fhdr), 1, f);
    switch (fhdr.version)
    {
        case 1:
            bhdr_size = sizeof(wb_header_tag_v1_t);
            break;
        case 2:
            bhdr_size = sizeof(wb_header_tag_v2_t);
            break;
        default:
            fprintf(stderr, "thread_extract_file_blocklist: file %2d: %s: unsupported Scatter Gather format version %d!", ctx->fileidx, ctx->filename, fhdr.version);
            fclose(f);
            return NULL;
    }

    if (m_m6sg_dbglevel > 1) { printf("thread_extract_file_blocklist: file %2d block size %d bytes, packet size %d bytes\n", ctx->fileidx, fhdr.block_size, fhdr.packet_size); }

    // Read all block headers
    memset(&bhdr, 0, sizeof(bhdr));
    while (1)
    {
        size_t nrd = fread(&bhdr, bhdr_size, 1, f);
        if ((nrd != 1) || feof(f))
        {
            fclose(f);
            break;
        }
        if (m_m6sg_dbglevel > 1) { printf("File %2d block size %d has nr %d\n", ctx->fileidx, bhdr.wb_size, bhdr.blocknum); }

        // Append block infos to block list
        if (ctx->nblocks >= ctx->nallocated)
        {
            ctx->nallocated = 2*(ctx->nblocks + 1);
            void* p = realloc(ctx->blks, ctx->nallocated*sizeof(m6sg_blockmeta_t));
            if (NULL == p)
            {
                fprintf(stderr, "thread_extract_file_blocklist: failed to grow in-memory block list to %zu blocks!\n", ctx->nallocated);
                fclose(f);
                return NULL;
            }
            ctx->blks = (m6sg_blockmeta_t*)p;
        }
        ctx->blks[ctx->nblocks].file_id     = ctx->fileidx;
        ctx->blks[ctx->nblocks].packetsize  = fhdr.packet_size;
        ctx->blks[ctx->nblocks].blocknum    = bhdr.blocknum;
        ctx->blks[ctx->nblocks].file_offset = ftell(f); // points to VLBI data, block header was already read/skipped
        switch (fhdr.version)
        {
            case 2:
                ctx->blks[ctx->nblocks].datalen = bhdr.wb_size - bhdr_size;
                break;
            case 1:
            default:
                ctx->blks[ctx->nblocks].datalen = fhdr.block_size - bhdr_size;
                break;
        }

        if ((ctx->blks[ctx->nblocks].file_offset + ctx->blks[ctx->nblocks].datalen) > st.st_size)
        {
            if (m_m6sg_dbglevel) { printf("File %2d ended with incomplete block!\n", ctx->fileidx); }
            // TODO: what is the best strategy? currenlty rounding down towards a packet size -multiple
            ctx->blks[ctx->nblocks].datalen = (st.st_size - ftell(f)) / ctx->blks[ctx->nblocks].packetsize;
            ctx->blks[ctx->nblocks].datalen *= ctx->blks[ctx->nblocks].packetsize;
            if (ctx->blks[ctx->nblocks].datalen > 0)
            {
                ctx->nblocks++;
            }
            break;
        }

        // Seek to next block header
        fseek(f, ctx->blks[ctx->nblocks].datalen, SEEK_CUR);
        ctx->nblocks++;
    }

    if (ctx->nblocks == 0)
    {
        fprintf(stderr, "thread_extract_file_blocklist: no data found in file %2d.\n", ctx->fileidx);
    }
    else
    {
        // Pre-sort
        if (m_m6sg_dbglevel > 0) { printf("File %2d had %zu blocks. Pre-sorting...\n", ctx->fileidx, ctx->nblocks); }
        qsort((void*)ctx->blks, ctx->nblocks, sizeof(m6sg_blockmeta_t), m6sg_block_comparefunc);
    }

    return NULL;
}


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
 * file recordings (e.g. all files in "/mnt/disks/[1-4]/[0-7]/"). It then
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
    int    rc, nfiles, nadded = 0, i;
    glob_t g;

    *filepathlist = NULL;
    *filenamelist = NULL;

    searchpattern = (char*)malloc(PATH_MAX);
    if (NULL == searchpattern) { return -1; }

    snprintf(searchpattern, PATH_MAX-1,  "%s%s", mark6_sg_root_pattern, scanname);
    rc = glob(searchpattern, GLOB_MARK, globerr, &g);

    if (rc != 0)
    {
        fprintf(stderr, "Mark6 SG file list assembly: '%s': %s\n", searchpattern,
           (rc == GLOB_ABORTED ? "filesystem problem"  :
            rc == GLOB_NOMATCH ? "no match of pattern" :
            rc == GLOB_NOSPACE ? "no dynamic memory"   :
            "unknown problem")
        );
        free(searchpattern);
        return 0;
    }
    free(searchpattern);

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
        char* s;
        struct stat st;
        stat(g.gl_pathv[i], &st);
        if (!S_ISREG(st.st_mode))
        {
            if (m_m6sg_dbglevel > 1) { printf(" %2d : %s : directory skipped\n", i, g.gl_pathv[i]); }
            continue;
        }

        s = strrchr(g.gl_pathv[i], '/');
        if (NULL != s)
        {
            fnlist[nadded] = strdup(s+1);
        }
        else
        {
            fnlist[nadded] = strdup(g.gl_pathv[i]);
        }
        fplist[nadded] = strdup(g.gl_pathv[i]);
        if (m_m6sg_dbglevel > 1) { printf(" %2d : %s : %s\n", i, fplist[nadded], fnlist[nadded]); }
        nadded++;
    }

    globfree(&g);
    *filepathlist = realloc(fplist, nadded*sizeof(char*));
    *filenamelist = realloc(fnlist, nadded*sizeof(char*));

    return nadded;
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
 * Make an ordered list of the scatter-gather blocks in a list of files.
 * The files should be the files associated with a single scan.
 * The output list can be used to look up "linear" byte offsets in a
 * virtual "de-scattered" scan, sans all the metadata of the individual files.
 */
size_t mark6_sg_blocklist(int nfiles, const char** filenamelist, m6sg_blockmeta_t** blocklist)
{
    blklist_thread_ctx_t** ctxs;
    pthread_t* tids;

    struct timeval tv_start;

    m6sg_blockmeta_t* blks = NULL;
    m6sg_blockmeta_t* prevblk = NULL;
    size_t nblocks = 0, ncopied = 0, nmissing = 0;
    size_t i;

    gettimeofday(&tv_start, NULL);

    // Create contexts for threads that read out block infos from each file
    tids = (pthread_t*)malloc(nfiles*sizeof(pthread_t));
    ctxs = (blklist_thread_ctx_t**)malloc(nfiles*sizeof(blklist_thread_ctx_t*));
    for (i=0; i<nfiles; i++)
    {
        ctxs[i] = (blklist_thread_ctx_t*)malloc(sizeof(blklist_thread_ctx_t));
        ctxs[i]->fileidx = i;
        ctxs[i]->filename = filenamelist[i];
    }

    // Start parallel read of block infos
    for (i=0; i<nfiles; i++)
    {
        if(pthread_create(&tids[i], NULL, thread_extract_file_blocklist, ctxs[i]))
        {
            fprintf(stderr, "Error creating thread %zu/%d\n", i+1, nfiles);
            return 0;
        }
    }

    // Wait for all threads to finish
    nblocks = 0;
    for (i=0; i<nfiles; i++)
    {
        pthread_join(tids[i], NULL);
        nblocks += ctxs[i]->nblocks;
    }
    blks = malloc(nblocks*sizeof(m6sg_blockmeta_t));

    // Concatenate the thread sub-results into 'blks'
    ncopied = 0;
    for (i=0; i<nfiles; i++)
    {
        memcpy(&blks[ncopied], ctxs[i]->blks, ctxs[i]->nblocks*sizeof(m6sg_blockmeta_t));
        ncopied += ctxs[i]->nblocks;
        free(ctxs[i]->blks);
        free(ctxs[i]);
    }
    free(ctxs);

    if (nblocks == 0)
    {
        fprintf(stderr, "mark6_sg_blocklist: no data found in files.\n");
        *blocklist = NULL;
        return nblocks;
    }

    // Sort the block list
    if (m_m6sg_dbglevel > 0) { printf("Got %zu blocks. Sorting...\n", nblocks); }
    qsort((void*)blks, nblocks, sizeof(m6sg_blockmeta_t), m6sg_block_comparefunc); // if random
    // mergesort((void*)blks, nblocks, sizeof(m6sg_blockmeta_t), m6sg_block_comparefunc); // if partly pre-sorted; libbsd required

    // Go through the block list and calculate "virtual" byte offsets
    prevblk = &blks[0];
    blks[0].virtual_offset = 0;
    for (i=1; i<nblocks; i++)
    {
        assert(prevblk->datalen > 0);
        blks[i].virtual_offset = prevblk->virtual_offset + prevblk->datalen;

        if (blks[i].blocknum != (prevblk->blocknum + 1))
        {
            int gaplen = ((blks[i].blocknum-1) - (prevblk->blocknum+1));
            // NOTE:  if Mark6 used a variable block size it is impossible to tell here how much data were lost
            // NOTE2: choosing here to hop over missing data to have gapless virtual offsets, but in principle, could allow gaps via
            //    blks[i].virtual_offset +=  ((off_t)gaplen)*prevblk.datalen;
            nmissing += gaplen;
            if (m_m6sg_dbglevel > 0) { printf("Missing blocks %d to %d.\n", prevblk->blocknum+1, blks[i].blocknum-1); }
        }

        prevblk = &blks[i];
    }

    if (m_m6sg_dbglevel > 0)
    {
        struct timeval tv_stop;
        gettimeofday(&tv_stop, NULL);
        printf("Building a block list took %.2f seconds.\n", (tv_stop.tv_sec-tv_start.tv_sec) + 1e-6*(tv_stop.tv_usec-tv_start.tv_usec));
    }

    if (m_m6sg_dbglevel > 2)
    {
        for (i=0; i<nblocks; i++)
        {
            printf("Blocklist %2zu --> file %2d, blocknum %2d, virt offset %ld\n", i, blks[i].file_id, blks[i].blocknum, blks[i].virtual_offset);
        }
    }

    if (nmissing > 0)
    {
        fprintf(stderr, "Scan is missing %zu blocks of data, possibly due to a missing scatter-gather file.\n", nmissing);
    }

    *blocklist = blks;
    return nblocks;
}


/**
 * Read the context of all Mark6 SG metadata files, i.e., the 'slist' (JSON) and 'group' (flat text)
 * files found under e.g. /mnt/disks/.meta/[1-4]/[0-7]/. Duplicate metadata entries are removed.
 * Produces a linked list of scanlist metadata entries. Allocates the required memory.
 * Returns the number of entries, or -1 on error.
 */
int mark6_sg_collect_metadata(m6sg_slistmeta_t** list)
{
    int   disk;
    int   nscans = 0;
    char  tmppath[1024];
    char  group_meta_str[512];
    m6sg_slistmeta_t  slistentry;
    m6sg_slistmeta_t* ptr_list;
    m6sg_slistmeta_t* tmp_entry;

    jsmn_parser p;     // Jasmin (JSMN) JSON C parser library http://zserge.com/jsmn.html
    void*       json;
    size_t      json_strlen;
    jsmntok_t*  tok;
    size_t      tokcount;
    size_t      tokidx;
    size_t      ii;
    int         rc;

    FILE* f;
    struct stat st;

    if (list == NULL)
    {
        return -1;
    }

    ptr_list = NULL;
    *list    = NULL;

    for (disk=0; disk<MARK6_SG_MAXFILES; disk++)  // note: max disks = max num. of scatter-gather files per scan
    {
        int group = (disk/8) + 1;
        int gdisk = disk % 8;

        // Read the 'group' metadata first.
        // In the current Mark6 cplane v1.12 version it is a single text line without a terminating newline,
        // and has a format like "2:KVN00500/32000/4/8:KVN00600/32000/4/8".
        memset(group_meta_str, 0, sizeof(group_meta_str));
        memset(tmppath, 0, sizeof(tmppath));
        sprintf(tmppath, "/mnt/disks/.meta/%d/%d/group", group, gdisk);
        f = fopen(tmppath, "r");
        if (f != NULL)
        {
             fread(group_meta_str, sizeof(group_meta_str)-1, 1, f);
             fclose(f);
        }
        else if (m_m6sg_dbglevel > 0) { printf("mark6_sg_collect_metadata: %s not found\n", tmppath); }

        // Now read the 'slist' JSON metadata.
        memset(tmppath, 0, sizeof(tmppath));
        sprintf(tmppath, "/mnt/disks/.meta/%d/%d/slist", group, gdisk);
        f = fopen(tmppath, "r");
        if (f == NULL)
        {
            if (m_m6sg_dbglevel > 0) { printf("mark6_sg_collect_metadata: %s not found\n", tmppath); }
            continue;
        }
        fstat(fileno(f), &st);
        json_strlen = st.st_size;

        json = malloc(json_strlen + 16);
        fread(json, json_strlen, 1, f);
        fclose(f);

        // Parse JSON using the he Jasmin (JSMN) JSON C parser library
        jsmn_init(&p);
        tokcount = 8; // an initial guess

        // The JSMN JSON parser does not allocate memory itself. Instead it tells
        // if a parse attempt will fit into a user-provided buffer or not. Need
        // to grow the buffer and "try again" until parser output fits:
        tok = malloc(sizeof(*tok) * tokcount);
        rc  = jsmn_parse(&p, json, json_strlen, tok, tokcount);
        while (rc == JSMN_ERROR_NOMEM)
        {
            tokcount = tokcount * 2;
            tok = realloc(tok, sizeof(*tok) * tokcount);
            rc  = jsmn_parse(&p, json, json_strlen, tok, tokcount);
        }

        // The contents of 'slist' in Mark6 cplane v1.12 is:
        //    "{ 1:{scandata1}, 2:{scandata2}, ..., n:{scandatan} }"
        // where {scandata} entries look like, for example,
        //    "{'status': 'recorded', 'num_str': 1, 'start_tm': 1412834280.3427939,
        //     'create_time': '2014y282d05h58m01s', 'sn': 'wrtest_ys_scan_1009_05',
        //     'dur': 10, 'spc': 0, 'size': '7.276'}"
        if (rc < 1 || tok[0].type != JSMN_OBJECT)
        {
            // "{ 1:{scandata1}, ... }" missing
            if (m_m6sg_dbglevel > 0) { printf("mark6_sg_collect_metadata: JSON data in %s looks bad (no top-level object)\n", tmppath); }
            continue;
	}

	tokcount = tok[0].size;
        tokidx   = 1;
        if (m_m6sg_dbglevel > 0) { printf("mark6_sg_collect_metadata: JSON data in %s has %ld scans\n", tmppath, tokcount); }

        for (ii=0; ii<tokcount; ii++)
        {
            int nsubentries, jj, exists;

            if (tok[tokidx].type != JSMN_PRIMITIVE)
            {
                // the "n:" of "n:{scandatan}" is missing
                if (m_m6sg_dbglevel > 0) { printf("mark6_sg_collect_metadata: JSON data in %s looks bad (a sub-obj ID number is missing; token %ld)\n", tmppath, tokidx); }
                break;
            }
            tokidx++;

            if (tok[tokidx].type != JSMN_OBJECT)
            {
                // the "{scandatan}" of "n:{scandatan}" is missing
                if (m_m6sg_dbglevel > 0) { printf("mark6_sg_collect_metadata: JSON data in %s looks bad (a sub-obj ID number is missing; token %ld)\n", tmppath, tokidx); }
                break;
            }

            // Prepare storage of newest scan list entry metadata
            memset(&slistentry, 0, sizeof(slistentry));
            slistentry.group = strdup(group_meta_str);
            slistentry.next  = NULL;

//FIXME: JSMN parser fails for entries that contain a space, for example "'create_time': 'Sat Sep 13 09:55:17 2014'"

            // Decode entries inside "{scandata}"
            nsubentries = tok[tokidx].size;
            tokidx++;
            for (jj=0; jj<nsubentries; jj++, tokidx+=2)
            {
                char* key      = (char*)json + tok[tokidx+0].start;
                char* arg      = (char*)json + tok[tokidx+1].start;
                int key_strlen = tok[tokidx+0].end - tok[tokidx+0].start;
                int arg_strlen = tok[tokidx+1].end - tok[tokidx+1].start;

                if (m_m6sg_dbglevel > 1)
                {
                    printf("JSON file %s : field %d/%d ", tmppath, jj+1, nsubentries);
                    printf("key-arg pair : tok[%ld].size=%d l=%d %.*s : ", tokidx, tok[tokidx].size, key_strlen, key_strlen, key);
                    printf("tok[%ld].size=%d l=%d %.*s) \n", tokidx+1, tok[tokidx+1].size, arg_strlen, arg_strlen, arg);
                }

                if (strncmp(key, "sn", key_strlen) == 0)
                {
                    // Format is " 'sn': 'wrtest_ys_scan_1009_05' "
                    slistentry.scanname = strndup(arg, arg_strlen);
                    // TODO: add .vdif suffix?
                }
                else if (strncmp(key, "size", key_strlen) == 0)
                {
                    // Format is " 'size': '7.276' "
                    arg[arg_strlen - 1] = '\0';
                    slistentry.size = atof(arg) * 1024.0*1024.0*1024.0;
                }
                else if (strncmp(key, "start_tm", key_strlen) == 0)
                {
                    // Format is " 'start_tm': 1412834280.3427939 " and arg has no single hyphens
                    slistentry.starttime = atof(arg);
                }
            }

            // Check and ignore if entry is already found in list
            ptr_list = *list;
            exists   = 0;
            while ((ptr_list != NULL) && !exists)
            {
                exists   = (slistentry.starttime == ptr_list->starttime)
                             && (slistentry.size == ptr_list->size)
                             && (ptr_list->scanname != NULL && strcmp(slistentry.scanname, ptr_list->scanname) == 0);
                ptr_list = (m6sg_slistmeta_t*) ptr_list->next;
            }
            if (exists)
            {
                if (slistentry.scanname != NULL) { free(slistentry.scanname); }
                if (m_m6sg_dbglevel > 1) { printf("mark6_sg_collect_metadata: skipping duplicate entry\n"); }
                continue;
            }

            // Add to the list
            tmp_entry = malloc(sizeof(slistentry));
            memcpy(tmp_entry, &slistentry, sizeof(slistentry));
            if (*list == NULL)
            {
                // First entry in the list
                *list = tmp_entry;
            }
            else
            {
                // Some later entry in the list
                ptr_list = *list;
                while (ptr_list->next != NULL)
                {
                    ptr_list = (m6sg_slistmeta_t*) ptr_list->next;
                }
                ptr_list->next = tmp_entry;
            }
            nscans++;

            // Freeing up list->scanname and list->next is up to the callee later.

        }//(while tokcount)

        free(json);
    }

    if (m_m6sg_dbglevel > 0) { printf("mark6_sg_collect_metadata: %d unique scans added\n", nscans); }
    return nscans;
}

/**
 * Change the base search pattern for SG files used by the library,
 * for example "/mnt/disks/[1-4]/[0-7]/data/".
 */
char* mark6_sg_set_rootpattern(const char* new_sg_root_pattern)
{
    if (NULL != new_sg_root_pattern)
    {
        if (0 != mark6_sg_root_pattern_changed)
        {
            free(mark6_sg_root_pattern);
        }
        mark6_sg_root_pattern = strdup(new_sg_root_pattern);
        mark6_sg_root_pattern_changed = 1;
        return mark6_sg_root_pattern;
    }
    return NULL;
}
