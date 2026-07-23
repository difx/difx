/************************************************************************/
/*                                                                      */
/* This routine is responsible for figuring out the interaction between */
/* input segment length, requested averaging time, and "even" output    */
/* data time boundaries.  Depending on how poorly these things can be   */
/* reconciled, the routine may issue a warning message, or tag entire   */
/* baselines/triangles invalid.                                         */
/*                                                                      */
/*      Inputs:         data            Need this for input data times  */
/*                      dsumm           summary for one source          */
/*                      int_time        requested integration time      */
/*                                                                      */
/*      Output:         dsumm           start times filled in           */
/*                                                                      */
/* Created 10 April 1995 by CJL                                         */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "average.h"
#include "mk4_util.h"

void
check_times (seg_data *data,
             summary *dsumm,
             int int_time)
    {
    int i, j, time, start, end, index, boundary, first, earliest, latest;
    int offset, noff, seglen, rem, nseg;
    char source[31], id[5];
    tbsumm *tbs;
    extern int datatype;

    if (! dsumm->valid) return;
                                        /* Convenience */
    start = dsumm->start_index;
    end = dsumm->end_index;
                                        /* Extract source name */
    index = data[start].order;
    if (datatype == 2) strcpy (source, data[index].u.fdata.source);
    else if (datatype == 3) strcpy (source, data[index].u.tdata.source);
                                        /* Find time range for entire */
                                        /* source */
    for (i=start; i<=end; i++)
        {
        index = data[i].order;
        if (datatype == 2) 
            time = data[index].u.fdata.time_tag;
        else if (datatype == 3) 
            time = data[index].u.tdata.time_tag;
                                        /* Init for first time through */
        if (i == start) earliest = latest = time;

        if (time < earliest) earliest = time;
        if (time > latest) latest = time;
        }
                                        /* Flag bogus timerange */
    if ((latest - earliest) > 1209600)
        {
        msg ("Input times for source '%s' span more than 2 weeks, skipping", 
                        3, source);
        dsumm->valid = FALSE;
        return;
        }
                                        /* What should our boundaries be? */
                                        /* No attempt to optimize, just get */
                                        /* them on nice round numbers */
    boundary = 3600;
    if (int_time <= 1800) boundary = 1800;
    if (int_time <= 600) boundary = 600;
    if (int_time <= 300) boundary = 300;
    if (int_time <= 60) boundary = 60;
                                        /* Start of first integration period */
                                        /* (reference time for entire source) */
                                        /* All integrations for all tribases */
                                        /* should line up for this source */
    first = (earliest/boundary) * boundary;
                                        /* Insert start times for each tribase */
                                        /* Loop over tribases */
    for (i=0; i<dsumm->nid; i++)
        {
                                        /* Indices for this tribase */
        tbs = dsumm->tribase + i;
        start = tbs->start_index;
        end = tbs->end_index;
                                        /* Get the tribase id string */
        index = data[start].order;
        if (datatype == 2) strcpy (id, data[index].u.fdata.baseline);
        else if (datatype == 3) strcpy (id, data[index].u.tdata.triangle);
                                        /* Init and loop over data */
        earliest = latest;
        for (j=start; j<=end; j++)
            {
            index = data[j].order;
            if (datatype == 2) 
                time = data[index].u.fdata.time_tag;
            else if (datatype == 3) 
                time = data[index].u.tdata.time_tag;
            if (time < earliest) earliest = time;
            }
        offset = earliest - first;
        if (int_time != 0) 
            {
            noff = offset/int_time;
            tbs->start_time = first + (noff * int_time);
            }
        else tbs->start_time = first;
                                        /* Now look for seglen/int_time */
                                        /* mismatches */
        if (int_time != 0)
            {
            seglen = tbs->seglen;
            rem = int_time % seglen;
            nseg = int_time / seglen;
            if (int_time < seglen)
                {
                msg ("Integration time %d shorter than segment length %d for", 3,
                        int_time, seglen);
                msg ("'%s:%s', skipping", 3, source, id);
                tbs->valid = FALSE;
                continue;
                }
            if (rem > 0)
                {
                msg ("Warning: integration time is not a multiple of the", 2);
                msg ("segment length for '%s:%s'.  Up to %d percent of the data",
                        2, source, id, 100/nseg);
                msg ("which spans a boundary may be ignored", 2);
                }
            else if (nseg < 4)
                {
                msg ("Warning: only %d segments per integration time", 2, nseg);
                msg ("for '%s:%s'.  This could result in significant data loss",
                        2, source, id, 100/nseg);
                msg ("if the boundaries don't line up", 2);
                }
            }
        }

    }
