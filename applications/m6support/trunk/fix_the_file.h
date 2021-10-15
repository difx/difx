/*
 * $Id: fix_the_file.h 4778 2018-11-19 16:04:50Z gbc $
 *
 * Code to pull good packets from a bad file.
 */

#include "sg_access.h"

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
