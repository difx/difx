/************************************************************************/
/*                                                                      */
/* This routine fills in the dsumm structure.  It is simply an index    */
/* of the sources and baselines/triangles in memory, for convenience    */
/* of operations downstream.                                            */
/*                                                                      */
/*      Inputs:         data            one big seg_data struct array   */
/*                      nseg            Number of data elements         */
/*                                                                      */
/*      Output:         dsumm           array of structs containing     */
/*                                      indexing information            */
/*                      return value    Number of dsumm elements used   */
/*                                                                      */
/* Created May 10 1995 by CJL                                           */
/*                                                                      */
/************************************************************************/
#include <string.h>
#include "average.h"

int
index_data (seg_data *data,
            int nseg,
            summary dsumm[])
    {
    int i, nsource, nid, index;
    char id[5], oldid[5], source[32], oldsrc[32];
    fringesum *fdatum;
    trianglesum *tdatum;
    extern int datatype;
                                        /* Loop over all data in memory */
    nsource = nid = 0;
    for (i=0; i<nseg; i++)
        {
        index = data[i].order;
        fdatum = &(data[index].u.fdata);
        tdatum = &(data[index].u.tdata);
                                        /* Extract relevant strings */
        if (datatype == 2)
            {
            strcpy (id, fdatum->baseline);
            strcpy (source, fdatum->source);
            }
        else if (datatype == 3)
            {
            strcpy (id, tdatum->triangle);
            strcpy (source, tdatum->source);
            }
                                        /* Initialize for first pass */
        if (i == 0)
            {
            dsumm[nsource].start_index = i;
            dsumm[nsource].tribase[nid].start_index = i;
            strcpy (oldid, id);
            strcpy (oldsrc, source);
            }
                                        /* Compare with previous to see if */
                                        /* this is a new baseline/triangle, */
                                        /* or a new source */
        if (strcmp (source, oldsrc) != 0)
            {
                                        /* Close out previous source */
            dsumm[nsource].end_index = i-1;
            dsumm[nsource].tribase[nid].end_index = i-1;
            dsumm[nsource].nid = nid + 1;
                                        /* Start new source */
            msg("Source %d used %d ids.", -2, nsource, nid+1);
                nsource++;
            if (nsource >= MAXSRC) break;
                nid = 0;
                dsumm[nsource].start_index = i;
                dsumm[nsource].tribase[nid].start_index = i;
            }
                                        /* Or a new baseline/triangle? */
        else if (strcmp (id, oldid) != 0)
            {
            dsumm[nsource].tribase[nid].end_index = i-1;
            nid++;
            dsumm[nsource].tribase[nid].start_index = i;
            }
                                        /* update comparison strings */
        strcpy (oldid, id);
        strcpy (oldsrc, source);
        }
                                        /* Tidy up the last source/tribase */
    dsumm[nsource].end_index = nseg - 1;
    dsumm[nsource].tribase[nid].end_index = nseg - 1;
    dsumm[nsource].nid = nid + 1;

    return (nsource + 1);
    }
