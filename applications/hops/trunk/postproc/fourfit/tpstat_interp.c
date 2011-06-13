/************************************************************************/
/*                                                                      */
/* Given memory structures containing the type-3 file information for   */
/* both stations, and the corel time/frequency array, this routine      */
/* matches up the channels and APs with the type-304 entries in the     */
/* type-3 files, and inserts interpolated tape error rates into each    */
/* cell of the time/frequency array.                                    */
/*                                                                      */
/*      Inputs:         sd1, sd2        sdata pointers for the 2 stns   */
/*                      param           Needed for start time etc.      */
/*                                                                      */
/*      Output:         corel           tape numbers filled in          */
/*                                                                      */
/* Created 27 October 1999 by CJL                                       */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "ovex.h"               /* For track number magic value */
#include "mk4_sizes.h"

#define USB_LCP 1
#define USB_RCP 2
#define LSB_LCP 3
#define LSB_RCP 4
#define NONE    5

int
tpstat_interp (/* sd1, sd2, param, corel) */
struct mk4_sdata *sd1,
struct mk4_sdata *sd2,
struct type_param *param,
struct freq_corel *corel)
    {
    int i, j, k, ap, minap, maxap, f, lcptrk, rcptrk;
    double start, start_ap, stop_ap, minwt, maxwt, weight, nlcp, nrcp, totap;
    double bytes_per_frame, tot_errate_lcp[MAX_CHAN_PP][16], tot_errate_rcp[MAX_CHAN_PP][16], value;
    struct freq_corel *fc;
    struct mk4_sdata *sd;
    struct type_304 *t304;
    struct interp_sdata *isd;
                                        /* How many total APs will be done? */
    totap = param->maxap - param->minap;
    if (totap == 0.0) return (-1);
                                        /* Scaling factor (speedup = 1) */
    bytes_per_frame = param->bocf_period / 32000000.0 / param->samp_period 
                                                  / 8 * param->bits_sample;
                                        /* Do it one station at a time */
    for (i=0; i<2; i++)
        {
        if (i == 0)
            msg ("Reference station:", 1);
        else
            msg ("Remote station:", 1);
        sd = (i == 0) ? sd1 : sd2;
                                        /* Loop over type 304 records */
        msg ("station %c, # of 304 records = %d", -1, sd->t300->id, sd->n304);
        for (f=0; f<MAX_CHAN_PP; f++) 
            for (k=0; k<16; k++) 
                tot_errate_lcp[f][k] = tot_errate_rcp[f][k] = 0.0;
        for (j=0; j<sd->n304; j++)
            {
            t304 = sd->t304[j];
                                        /* Figure out AP indices */
                                        /* Get actual start and stop of */
                                        /* this tape stat period in APs */
            start = 86400.0 *(t304->time.day - 1)
                   + 3600.0 * t304->time.hour
                   +   60.0 * t304->time.minute
                   +          t304->time.second;
            start_ap = (start - param->start) / param->acc_period;
            stop_ap = start_ap + t304->duration / param->acc_period;
                                        /* Get ap index extrema, with weights */
                                        /* for 1st and last one */
            minap = (int)start_ap;
            maxap = (int)stop_ap;
            if ((minap < 0) || (maxap > param->maxap))
                {
                msg ("Out of range time stamp/duration in type 304 record", 1);
                msg ("Min, max AP indices for 304 record = %d %d", 1, minap, maxap);
                msg ("Start time of 304 record = %03d-%02d%02d%g", 1,
                        t304->time.day, t304->time.hour, t304->time.minute,
                        t304->time.second);
                continue;
                }
            minwt = (double)(minap + 1) - start_ap;
            maxwt = stop_ap - (double)maxap;
                                        /* Loop over all freqs */
            for (f=0; f<MAX_CHAN_PP; f++)
                {
                fc = corel + f;
                if (fc->frequency < 0.0) continue;
                                        /* How many track contribute (for weighting) */
                                        /* This includes both sidebands */
                nlcp = nrcp = 0.0;
                for (k=0; k<16; k++)
                    {
                    lcptrk = fc->trk_lcp[i][k];
                    rcptrk = fc->trk_rcp[i][k];
                                        /* In case vex library used for init */
                    if (lcptrk == I_UNDEFINED) lcptrk = -1;
                    if (rcptrk == I_UNDEFINED) rcptrk = -1;
                    if (lcptrk > 0) nlcp += 1.0;
                    if (rcptrk > 0) nrcp += 1.0;
/*                     if (lcptrk >= 0) nlcp += 1.0; */
/*                     if (rcptrk >= 0) nrcp += 1.0; */
                    }
                if ((nlcp == 0.0) && (nrcp == 0.0)) continue;
                msg ("freq %d, nlcp,nrcp %f %f", -2, f, nlcp, nrcp);
                                        /* Loop over possible contributing trks */
                for (k=0; k<16; k++)
                    {
                    lcptrk = fc->trk_lcp[i][k];
                    rcptrk = fc->trk_rcp[i][k];
                                        /* In case vex library used for init */
                    if (lcptrk == I_UNDEFINED) lcptrk = -1;
                    if (rcptrk == I_UNDEFINED) rcptrk = -1;
                                        /* Skip if both absent */
                    if ((lcptrk < 0) && (rcptrk < 0)) continue;
                                        /* Loop over affected APs */
                    for (ap=minap; ap<=maxap; ap++)
                        {
                        if (i == 0) isd = &(fc->data[ap].ref_sdata);
                        else isd = &(fc->data[ap].rem_sdata);
                                        /* Accumulated weight/AP = 1.0 per freq */
                                        /* Yields mean tape error rate for freq */
                                        /* by polarization, with sidebands avgd */
                        weight = 1.0;
                        if (ap == minap) weight -= minwt;
                        if (ap == maxap) weight -= maxwt;
                        if (lcptrk > 0) 
                            {
                            value = weight * t304->trackstats[lcptrk].error_count 
                                            / nlcp / bytes_per_frame;
                            isd->errate_lcp += value;
                            tot_errate_lcp[f][k] += value;
                            }
                        if (rcptrk > 0) 
                            {
                            value = weight * t304->trackstats[rcptrk].error_count 
                                            / nrcp / bytes_per_frame;
                            isd->errate_rcp += value;
                            tot_errate_rcp[f][k] += value;
                            }
                        }
                    }
                }
            }
        for (f=0; f<MAX_CHAN_PP; f++)
            {
            fc = corel + f;
            for (k=0; k<16; k++)
                {
                fc->mean_lcp_trk_err[i][k] = tot_errate_lcp[f][k] / totap;
                fc->mean_rcp_trk_err[i][k] = tot_errate_rcp[f][k] / totap;
                if (fc->trk_lcp[i][k] >= 0)
                    msg ("track %2d parity error rate %.1e", -3,
                          fc->trk_lcp[i][k], fc->mean_lcp_trk_err[i][k]);
                if (fc->trk_rcp[i][k] >= 0)
                    msg ("track %2d parity error rate %.1e", -3,
                          fc->trk_rcp[i][k], fc->mean_rcp_trk_err[i][k]);
                }
            }
        }       /* End loop over stations */

    return (0);
    }
