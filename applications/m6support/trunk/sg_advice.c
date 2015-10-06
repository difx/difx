/*
 * $Id$
 * 
 * Code to boost performance on reads to sg files
 *
 * This function is called at times where bulk reads need to be optimized
 * to get the full bandwidth out of the filesystem.  There are several
 * types of optimization available:
 *
 *   madvise(2) which gives the kernel guidance
 *   posix_madvise(2) which does something similar
 *   readahead(2) (linux only) which populates the page cache (but blocks)
 *   posix_fadvise(2) which does something similar
 *   touching disk blocks (in a separate thread)
 *   setting affinity to the disk controller(s)
 *
 *   page size is portably available vi sysconf.
 */

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/mman.h>
#include "sg_access.h"

int sg_advice_disable = 0;

/*
 * Some controls on what this module does
 */
#define SG_ADVICE_DISABLE           0
#define SG_ADVICE_MADV_SEQUENTIAL   1
#define SG_ADVICE_MADV_WILLNEED     2
#define SG_ADVICE_POSIX_FADVISE     3
#define SG_ADVICE_SPAWN_READAHEAD   4
#define SG_ADVICE_SPAWN_READTHREAD  5
#define SG_ADVICE_MAXIMUM           SG_ADVICE_SPAWN_READTHREAD

#define SG_ADVICE_DEFAULT           SG_ADVICE_MADV_WILLNEED
#define SG_ADVICE_BLOCKS            20000000L

static int  advice_type = SG_ADVICE_DISABLE;
static long pagesize = 0L;
static unsigned long addrmask = 0L;
static long blockage = SG_ADVICE_BLOCKS;

/*
 * Initialization code.
 * For the methods that require a helper thread, we make preparations
 * to support them.
 */
static void sg_advice_init(int verbose)
{
    char    *hint = getenv("SG_ACCESS_ADVICE");
    char    *blks = getenv("SG_ACCESS_BLOCKS");

    pagesize = sysconf(_SC_PAGESIZE);
    addrmask = ~(pagesize - 1);
    advice_type = hint ? atoi(hint) : SG_ADVICE_DEFAULT;
    blockage = blks ? atol(blks) : SG_ADVICE_BLOCKS;

    switch (advice_type) {
    case SG_ADVICE_DISABLE:
        sg_advice_disable = 1;
        break;
    case SG_ADVICE_MADV_SEQUENTIAL:
    case SG_ADVICE_MADV_WILLNEED:
    case SG_ADVICE_POSIX_FADVISE:
    default:
        /* no action here */
        break;
    case SG_ADVICE_SPAWN_READAHEAD:
    case SG_ADVICE_SPAWN_READTHREAD:
        /* initialize for threads */
        break;
    }
    if (hint || blks) fprintf(stderr,
        "Page size is %luB Blockage is %luB Mase is %lx\n"
        "Advice is %d and Disable is %d hint %s type %d\n",
        pagesize, blockage, addrmask,
        advice_type, sg_advice_disable, hint, advice_type);
}

/*
 * If threads are in use, terminate the appropriate helper
 */
void sg_advice_term(int mmfd)
{
    if (advice_type != SG_ADVICE_SPAWN_READAHEAD &&
        advice_type != SG_ADVICE_SPAWN_READTHREAD) return;

    /* FIXME: code to terminate mmfd */
}

/*
 * A method to get the kernel to preload pages we expect
 * to access in the near future in order to boost performance
 * by avoiding page faults
 */
void sg_advice(SGInfo *sgi, void *pkt, int dir)
{
    void *addr;
    size_t len = blockage;

    if (pagesize == 0L) sg_advice_init(sgi->verbose);   /* once */
    if (sg_advice_disable || pagesize == 0L) return;

    /* madvice code; trim request to mmap()ed range */
    if (dir > 0) addr = pkt;
    else addr = pkt - len;
    if (addr < sgi->smi.start) addr = sgi->smi.start;
    if (addr + len >= sgi->smi.eomem) addr = sgi->smi.eomem - len;
    /* expressing an interest in (page-aligned) addr to addr + length */
    addr = (void*)((unsigned long)addr & addrmask);

    /*
     * Take an appropriate action based on advice type
     * Errors are treated as fatal to the method.
     */ 
    switch (advice_type) {
    case SG_ADVICE_MADV_SEQUENTIAL:
        if (madvise(addr, len, MADV_SEQUENTIAL)) perror("madvise:seq");
        else return;
        break;
    case SG_ADVICE_MADV_WILLNEED:
        if (madvise(addr, len, MADV_WILLNEED)) perror("madvise:need");
        else return;
        break;
    case SG_ADVICE_POSIX_FADVISE:
        if (posix_fadvise(sgi->smi.mmfd, addr - sgi->smi.start, len,
            POSIX_FADV_WILLNEED)) perror("posix_fadvise: need");
        else return;
        break;
    case SG_ADVICE_SPAWN_READAHEAD:
        /* FIXME: talk to the right hand */
        break;
    case SG_ADVICE_SPAWN_READTHREAD:
        /* FIXME: talk to the left hand */
        break;
    default:
        break;
    }

    /* if we got here something bad happed and we should stop trying */
    sg_advice_disable = 1;
}

/*
 * eof
 */
