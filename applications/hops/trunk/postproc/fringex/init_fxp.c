/************************************************************************/
/*                                                                      */
/* Initiallizes the segment data area to null pointers                  */
/*                                                                      */
/*      Inputs:         fxp             pointer to fringe file already  */
/*                                      filled in                       */
/*                                                                      */
/*      Output:         none                                            */
/*                                                                      */
/* Created January 22 2018 JPB                                          */
/*                                                                      */
/************************************************************************/

#include "fringex.h"

void init_fxp(struct fxparam *fxp)
{
    //this function should only be called once, to intialize the pointers
    //which store the segment data, all other memory management should go 
    //through realloc_segs()
    fxp->fringe = NULL;
    fxp->sdata[0] = NULL;
    fxp->sdata[1] = NULL;
    fxp->nsegs_allocd = 0;
    fxp->rsum = NULL;
    fxp->isum = NULL;
    fxp->segsec = NULL;
    fxp->segcount = NULL;
    fxp->seglen = NULL;
}
