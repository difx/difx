/********************************************************************************/
/*                                                                              */
/* This routine figures out the range of times present in the data, and fills   */
/* in the relevant entries in the param structure.  Some elementary error       */
/* checking is performed, and "bogus" points are ignored                        */
/*                                                                              */
/* Original version  CJL April 8 1992                                           */
/* Rewritten to use integer number of 1/4 msec periods for times, so we can     */
/* avoid floating-point precision-related problems  CJL 25 March 1993           */
/* Rewritten using AP numbers for Mk4  CJL 1st April 1998                       */
/*                                                                              */
/********************************************************************************/
#include <stdio.h>
#include <math.h>

#include "mk4_data.h"
#include "vex.h"
#include "param_struct.h"

int
time_range (struct scan_struct* ovex, struct station_struct* stn1, struct station_struct* stn2, struct mk4_corel* cdata, struct type_param* param)
    {
    char st1, st2;
    double scan_start_ref, start_ref, startoff, sstart, sstop;
    int i, ap_since_year, ind, ap, apno, start_offset, stop_offset, max_offset;
    int earliest_start, latest_start, earliest_stop, latest_stop;
    int apoff;
    extern int reftime_offset;
                                        /* Find baseline start/stop relative */
                                        /* to nominal scan start time */
    param->start_offset = imax ((int)stn1->start_offset, (int)stn2->start_offset);
    param->stop_offset = imin ((int)stn1->stop_offset, (int)stn2->stop_offset);
                                        /* Nominal start time in secs since */
                                        /* beginning of year, based on */
                                        /* scheduled start/stop times in root */
                                        /* start_ref is earliest start time for any */
                                        /* station in this scan */
    scan_start_ref = 86400.0 *(ovex->start_time.day - 1)
                    + 3600.0 * ovex->start_time.hour
                      + 60.0 * ovex->start_time.minute
                      +        ovex->start_time.second;
    latest_start = latest_stop = 0;
    earliest_start = earliest_stop = 10000000;
    for (i=0; i<ovex->nst; i++)
        {
        if (ovex->st[i].start_offset < earliest_start)
            earliest_start = ovex->st[i].start_offset;
        if (ovex->st[i].start_offset > latest_start)
            latest_start = ovex->st[i].start_offset;
        if (ovex->st[i].stop_offset < earliest_stop)
            earliest_stop = ovex->st[i].stop_offset;
        if (ovex->st[i].stop_offset > latest_stop)
            latest_stop = ovex->st[i].stop_offset;
        }
                                        /* Times of baseline with shortest time span */
    sstart = scan_start_ref + latest_start;
    sstop = scan_start_ref + earliest_stop;
                                        /* Nominal times for this baseline */
    param->start_nom = scan_start_ref + param->start_offset;
    param->stop_nom = scan_start_ref + param->stop_offset;
                                        /* Loop over all data to find extrema */
    param->minap = 10000;
    param->maxap = 0;
    for (ind=0; ind<cdata->index_space; ind++)
        {
        if (cdata->index[ind].t101 == NULL)
            continue;
        for (ap=0; ap<cdata->index[ind].ap_space; ap++)
            {
            if (cdata->index[ind].t120[ap] == NULL)
                continue;
            apno = cdata->index[ind].t120[ap]->ap;
            if (apno > param->maxap) param->maxap = apno;
            if (apno < param->minap) param->minap = apno;
            }
        }

    /*this check is here to catch situations where, not valid t101 or t120 data is found*/
    if(param->minap == 10000)
        {
            msg ("Inconsistent type 101 and type 120 data, could not determine number of APs.", 3);
            return (-1);
        }

    param->num_ap = param->maxap - param->minap + 1;
                                        /* These are the actual start and stop */
                                        /* times of the data stream, from start */
                                        /* of the 1st ap to end of last ap */
                                        /* start_ref refers to time of start */
                                        /* of 1st AP */
    start_ref = 86400.0 *(cdata->t100->start.day - 1)
               + 3600.0 * cdata->t100->start.hour
               + 60.0   * cdata->t100->start.minute
               +          cdata->t100->start.second;
                                        /* Time extrema determined by data found, */
                                        /* but within scheduled time range */
    param->start = start_ref + param->minap * param->acc_period;
                                        /* adjust start to be no earlier than the start
                                         * of the AP containing the scheduled start time */
    if (param->start + param->acc_period <= param->start_nom)
        {
                                        /* Adjust to new "first ap" */
        startoff = param->start_nom - param->start;
        apoff = rint (startoff / param->acc_period);
        param->minap += apoff;
        param->num_ap -= apoff;
        param->start = param->start_nom;
        }
    param->stop = start_ref + (param->maxap + 1) * param->acc_period;
                                        // include last partial AP, but no more
                                        // rjc/kad   2010.7.27
    // if (param->stop > param->stop_nom)
    //     param->stop = param->stop_nom;
    if (param->stop > param->stop_nom + param->acc_period)
        param->stop = param->stop_nom + param->acc_period
                    - fmod (param->stop_nom, param->acc_period);
                                        /* To line up all baselines and bands */
                                        /* we pick midpoint of baseline with shortest */
                                        /* timerange, keeping closures accurate */
    param->reftime = (sstart + sstop) / 2.0;
                                        /* Override with time embedded in ovex */
    if (ovex->ffit_reftime.year > 0)
        param->reftime = 86400.0 *(ovex->ffit_reftime.day - 1)
                        + 3600.0 * ovex->ffit_reftime.hour
                        +   60.0 * ovex->ffit_reftime.minute
                        +          ovex->ffit_reftime.second;

                                        /* Geodetic mode, all reftimes a fixed amount */
                                        /* after global scan start time, for closure */
    if (reftime_offset > 0)
        param->reftime = scan_start_ref + reftime_offset;

    param->frt_offset = param->start - param->reftime;

    msg ("adjusted start %.2f stop %.2f ref. epoch %.2f", 0,
          param->start, param->stop, param->reftime);
                                        /* Should never happen */
    if (param->num_ap > MAXAP)
        {
        msg ("Too many accumulation periods in this scan", 3);
        return (-1);
        }
    if (param->num_ap < 0)
        {
        msg ("Too few accumulation periods in this scan, range %d..%d",
             3, param->minap, param->maxap);
        return (-1);
        }

    return (0);
    }
