/************************************************************************/
/*                                                                      */
/* Reallocates the segment data area when needed                        */
/*                                                                      */
/*      Inputs:         fxp             pointer to fringe file already  */
/*                                      filled in                       */
/*                                                                      */
/*      Output:         error code      0 for success, 1 for error      */
/*                                                                      */
/* Created January 22 2018 JPB                                          */
/*                                                                      */
/************************************************************************/

#include "fringex.h"

int realloc_segs(struct fxparam *fxp, int nsegs)
{
    int ret_val = NO_ALLOC_ERROR;
    if (nsegs > fxp->nsegs_allocd)
    {
        //we need to reallocate the space available for the segments
        fxp->rsum = realloc(fxp->rsum, nsegs*sizeof(double) );
        fxp->isum = realloc(fxp->isum, nsegs*sizeof(double) );
        fxp->segsec = realloc(fxp->segsec, nsegs*sizeof(double) );
        fxp->segcount = realloc(fxp->segcount, nsegs*sizeof(double) );
        fxp->seglen = realloc(fxp->seglen, nsegs*sizeof(double) );
        fxp->nsegs_allocd = nsegs;

        if(fxp->rsum == NULL ||  fxp->isum == NULL || fxp->segsec == NULL || fxp->segcount == NULL || fxp->seglen == NULL )
        {
            ret_val = ALLOC_ERROR;
            free(fxp->rsum);
            free(fxp->isum);
            free(fxp->segsec);
            free(fxp->segcount);
            free(fxp->seglen);
            fxp->nsegs_allocd = 0;
        }
        
    }
    return ret_val;
}
