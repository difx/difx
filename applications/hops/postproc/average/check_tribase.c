/************************************************************************/
/*                                                                      */
/* This routine examines the data for one triangle/baseline             */
/* It makes sure the data records are consistent with each other, and   */
/* if not, tags the triangle/baseline as invalid (to be skipped over    */
/* later in the program).  The main purpose of the routine is to figure */
/* out the data segment length, which can be non-trivial.               */
/*                                                                      */
/*      Inputs:         data            one big seg_data struct array   */
/*                      idsumm          summary struct for one tribase  */
/*                                                                      */
/*      Output:         idsumm          flagged either valid or invalid */
/*                                                                      */
/* Created May 10 1995 by CJL                                           */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "average.h"
#include "mk4_util.h"

#define MAXHIST 10
#define NLEN 27

void
check_tribase (seg_data *data,
               tbsumm *idsumm)
    {
    int i, j, index, start, end, tdiff, nhist, biggest, next_biggest;
    int seglen, time, old_time, duration, old_duration;
    char origin, id[5], source[32];
    float ratio;
    fringesum *fdatum;
    trianglesum *tdatum;
// allowed_seglen[] appears to be an ancient restriction
//  static int allowed_seglen[NLEN] = {1,2,3,4,5,6,10,12,15,20,30,40,60,90,
//      120,150,180,240,300,360,480,600,720,900,1200,1500,1800};
    extern int datatype;
    struct { int tdiff; int count; } hist[MAXHIST];

                                        /* Init */
    for (i=0; i<MAXHIST; i++) hist[i].tdiff = hist[i].count = 0;
    nhist = 0;
                                        /* Convenience variables */
    start = idsumm->start_index;
    end = idsumm->end_index;
                                        /* Grab some useful quantities */
    index = data[start].order;
    if (datatype == 2)
        {
        origin = data[index].u.fdata.datatype[0];
        strcpy (source, data[index].u.fdata.source);
        strcpy (id, data[index].u.fdata.baseline);
        }
    else if (datatype == 3)
        {
        origin = data[index].u.tdata.datatype[0];
        strcpy (source, data[index].u.tdata.source);
        strcpy (id, data[index].u.tdata.triangle);
        }
                                        /* Impose some rules */
                                        /* Loop over all data */
    for (i=start; i<=end; i++)
        {
        index = data[i].order;
        fdatum = &(data[index].u.fdata);
        tdatum = &(data[index].u.tdata);
                                        /* Extract relevant quantities */
        if (datatype == 2)
            {
            duration = fdatum->duration;
            time = fdatum->time_tag;
            }
        else if (datatype == 3)
            {
            duration = tdatum->duration;
            time = tdatum->time_tag;
            }
                                        /* Initialize for first pass */
        if (i == start)
            {
            old_duration = duration;
            old_time = time;
            }
                                        /* All durations should be the same */
        if (duration != old_duration)
            {
            msg ("Mixed nominal scan durations for '%s:%s', skipping",
                                3, source, id);
            idsumm->valid = FALSE;
            return;
            }
                                        /* Make a histogram of scantime diffs */
        if (i == start) continue;
        tdiff = time - old_time;
        for (j=0; j<nhist; j++)
            if (tdiff == hist[j].tdiff) break;
        if (j < nhist) hist[j].count += 1;
        else if (nhist < MAXHIST)
            {
            hist[j].tdiff = tdiff;
            hist[j].count = 1;
            nhist++;
            }
                                        /* Set old_time for next loop */
        old_time = time;
        }
                                        /* Examine the histogram for a good */
                                        /* peak, and make sure it agrees with */
                                        /* nominal scan duration */
    biggest = next_biggest = 0;
                                        /* Must be a single datum */
    if (nhist == 0) seglen = duration;
                                        /* Multiple data */
    else
        {
        for (i=0; i<nhist; i++)
            {
            if (hist[i].count > biggest)
                {
                biggest = hist[i].count;
                seglen = hist[i].tdiff;
                }
            else if (hist[i].count > next_biggest)
            next_biggest = hist[i].count;
            }
        }
    ratio = 0.0;
    if (next_biggest > 0) ratio = (float)biggest/(float)next_biggest;
    if ((ratio < 2.0) && (next_biggest > 0))
        {
        msg ("Time tags for '%s:%s' data irregularly spaced, skipping", 
                        3, source, id);
        idsumm->valid = FALSE;
        return;
        }
                                        /* No-op for afile format < 3 */
    if (duration == 0) duration = seglen;
    // unclear what this is enforcing.  However -q mode in fringex
    // turns into a 'C' and if we are using -q and want to averge
    // this test will prevent us as fringex is currently coded, better is:
    // if ((seglen != duration) && (origin != 'O' && origin != 'C'))
    // however that doesn't seem to work properly either.
    if ((seglen != duration) && (origin != 'O'))
        {
        msg ("Time tag spacing %d different from nominal segment length %d", 
                        3, seglen, duration);
        msg ("Skipping '%s:%s'", 3, source, id);
        idsumm->valid = FALSE;
        return;
        }
                                        /* Finally, check that it's a sensible */
                                        /* segment length */
/*    for (i=0; i<NLEN; i++) 
        if (seglen == allowed_seglen[i]) break;
    if (i == NLEN)
        {
        msg ("Input segment length %d non-standard for '%s:%s', skipping", 
                        3, seglen, source, id);
        idsumm->valid = FALSE;
        return;
        } */
                                        /* If we get this far, must be OK */
    idsumm->seglen = seglen;
    idsumm->valid = TRUE;
    return;
    }
