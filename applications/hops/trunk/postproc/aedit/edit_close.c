/************************************************************************/
/*                                                                      */
/* This routine loops through all baseline data in memory and flags     */
/* those which do not belong to a triangle record in memory.  In short, */
/* it makes triangles and baselines consistent after triangle editing   */
/* has been performed.                                                  */
/*                                                                      */
/*      Inputs:         data                                            */
/*                      mode            bits defined in flags.h         */
/*                                                                      */
/*      Output:         data            suitably edited                 */
/*                      tedit, fedit    number of pts flagged here      */
/*                      return value    0=OK, 1=BAD                     */
/*                                                                      */
/* Created 1 September 1994 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <string.h>
#include "aedata.h"
#include "flags.h"
#include "summary.h"

#define TRUE 1
#define FALSE 0

int
edit_close (data, mode, fedit, tedit)
esum *data;
int mode, *tedit, *fedit;
    {
    extern int fscan, fflag, tscan, tflag;
    extern int fsortstat[10], tsortstat[10];
    fringearray *fdatum;
    trianglearray *tdatum;
    int i, j, bno, nbase, tindex, findex, time, tlen, earliest, latest;
    int extent, epoch, fepoch, fmin, fmax, tmin, tmax, funsort, tunsort;
    int ftime;
    char freq_code, root_id[7], source[32], baseline[3], revbase[3];
    double ref_freq;
                                        /* Init and sanity check */
    *tedit = *fedit = 0;

    if ((fscan - fflag) == 0)
        {
        msg ("No baseline data to edit!", 2);
        return (1);
        }
    if ((tscan - tflag) == 0)
        {
        msg ("No triangle data present!", 2);
        return (1);
        }
                                        /* Sort the data records by timetag */
    summ_data (data, VERSION);
    sorter (data->fdata, "timetag", 2);
    sorter (data->tdata, "timetag", 3);
                                        /* Now we are going to cheat.  Each */
                                        /* baseline may participate in multiple */
                                        /* triangles, so we need to keep a count, */
                                        /* not just a flag.  When editing both */
                                        /* triangles and baselines, this count may */
                                        /* change during this routine, so we only */
                                        /* flag a baseline when the count goes to */
                                        /* zero.  A convenient misuse of the keyval */
                                        /* variable does the job ... */
    for (i=0; i<fscan; i++) data->fdata[i].keyval = 0;
                                        /* Loop over all triangles, tagging */
                                        /* baselines that belong to them as we go */
    earliest = latest = 0;
    for (i=0; i<tscan; i++)
        {
        tindex = data->tdata[i].order;
        tdatum = data->tdata + tindex;
        if (tdatum->flag != 0) continue;
                                        /* Explicitly discard index information */
        for (j=0; j<3; j++) tdatum->index[j] = -1;
                                        /* Get time tag of triangle */
        time = tdatum->data.time_tag;
        tlen = (tdatum->data.length[0] + tdatum->data.length[1] 
                                + tdatum->data.length[2]) / 3;
                                        /* Define a range of baseline indices that */
                                        /* might contain relevant records for this */
                                        /* triangle time tag */
        for (j=earliest; j<fscan; j++)
            {
            findex = data->fdata[j].order;
            ftime = data->fdata[findex].data.time_tag;
            if (ftime > (time - tlen/2)) break;
            }
        earliest = j;
        for (j=earliest; j<fscan; j++)
            {
            findex = data->fdata[j].order;
            ftime = data->fdata[findex].data.time_tag;
            if (ftime > (time + tlen/2)) break;
            }
        latest = j;
                                        /* Record various things that must match up */
        freq_code = tdatum->data.freq_code;
        strcpy (source, tdatum->data.source);
        epoch = 60*tdatum->data.epoch[0] + tdatum->data.epoch[1];
        ref_freq = tdatum->data.ref_freq;
                                        /* Loop over baselines of the triangle */
        nbase = 0;
        for (bno=0; bno<3; bno++)
            {
            strcpy (root_id, tdatum->data.root_id[bno]);
            extent = tdatum->data.extent_no[bno];
            baseline[0] = tdatum->data.triangle[bno];
            baseline[1] = tdatum->data.triangle[(bno+1) % 3];
            baseline[2] = '\0';
            revbase[0] = baseline[1];
            revbase[1] = baseline[0];
            revbase[2] = baseline[2];
                                        /* Search relevant range of baseline indices */
                                        /* for matches, and tag them */
            for (j=earliest; j<=latest; j++)
                {
                findex = data->fdata[j].order;
                fdatum = data->fdata + findex;
                                        /* Do easy part first to save time */
                if (fdatum->flag != 0) continue;
                if ((strcmp (baseline, fdatum->data.baseline) != 0)
                        && (strcmp (revbase, fdatum->data.baseline) != 0)) continue;
                if (strcmp (root_id, fdatum->data.root_id) != 0) continue;
                if (strcmp (source, fdatum->data.source) != 0) continue;
                if (extent != fdatum->data.extent_no) continue;
                if (freq_code != fdatum->data.freq_code) continue;
                if (ref_freq != fdatum->data.ref_freq) continue;
                fepoch = 60*fdatum->data.epoch[0] + fdatum->data.epoch[1];
                if (fepoch != epoch) continue;
                                        /* OK, now we have to check time */
                ftime = fdatum->data.time_tag;
                fmin = ftime - fdatum->data.length/10;
                fmax = ftime + fdatum->data.length/10;
                tmin = time - tlen/10;
                tmax = time + tlen/10;
                                        /* If times match to required accuracy */
                                        /* (may not be exact because of data */
                                        /* averaging), update tdata indices and */
                                        /* tag data point with nonzero keyval */
                if ((fmin <= tmax) || (tmin <= fmax)) 
                    {
                    tdatum->index[bno] = findex;
                    fdatum->keyval++;
                    nbase++;
                    }
                }
            }
                                        /* Flag the triangle if constituent */
                                        /* baselines are absent */
        if ((mode & MODE_TRI) && (nbase < 3))
            {
            tdatum->flag |= NO_BASELINE;
            *tedit += 1;
                                        /* Remove tags due to this (now edited) */
                                        /* triangle */
            for (j=0; j<3; j++)
                {
                if ((findex = tdatum->index[j]) < 0) continue;
                if (data->fdata[findex].keyval > 0) data->fdata[findex].keyval--;
                }
            }
        }
                                        /* Flag baselines if they are not */
                                        /* tagged as participating in a triangle */
    if (mode & MODE_BASE)
        for (i=0; i<fscan; i++)
            {
            fdatum = data->fdata + i;
            if (fdatum->keyval == 0)
                {
                if (fdatum->flag == 0) *fedit += 1;
                fdatum->flag |= NO_TRIANGLE;
                }
            }
                                        /* Restore original sort order */
    for (i=0; i<fscan; i++) data->fdata[data->fdata[i].lastorder].order = i;
    fsortstat[0]--;
    for (i=0; i<tscan; i++) data->tdata[data->tdata[i].lastorder].order = i;
    tsortstat[0]--;

    return (0);
    }
