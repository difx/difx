/*
 * (c) Massachusetts Institute of Technology, 2013..2023
 * (c) Geoffrey B. Crew, 2013..2023
 *
 * $Id: fix_the_file.h 5677 2023-03-04 19:06:02Z gbc $
 *
 * Code to pull good packets from a bad file.
 */

#include "sg_access.h"

#ifndef ROLLOVER
#define ROLLOVER    0
#endif /* ROLLOVER using UA */

typedef struct fixer_info {
    int         verb;               /* how chatty to be */
    int         prate;              /* packet rate in pkts/s */
    long        extra;              /* extra packets */
    off_t       start, clone;       /* offsets in the files */
    char        *ifile, *ofile;     /* names of the files */
    SGMMInfo    smi, smo;           /* mmap info for files */
    VDIFsigu    vsign;              /* VDIF packet signature */
    size_t      pktsize;            /* full packet size (bytes) */
    uint64_t    *fill;              /* the fill packet */
    long        good;               /* count of good packets */
    long        nfil;               /* count of fill packets */
    long        patches;            /* number of good runs */
} Fixer;

extern int fix_the_file(Fixer *fxp);

/*
 * eof
 */
