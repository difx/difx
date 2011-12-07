/*
 * $Id$
 *
 * Hook which allow nuking of sdata pcal.  Other sdata surgery could follow.
 */

#define read_sdata orig_read_sdata
#include "read_sdata.c"
#undef  read_sdata

#include <stdlib.h>

int read_sdata(struct fileset *fset, struct mk4_sdata *sdata)
{
    int rv = orig_read_sdata(fset, sdata), nf;
    char *ep = getenv("HOPS_FEARFIT_NOPCAL");
    /* act like it just isn't there */
    if (ep) for (nf = 0; nf < MAXSTATIONS; nf++)
        sdata[nf].n308 = sdata[nf].n309 = 0;
    return(rv);
}

/*
 * eof
 */
