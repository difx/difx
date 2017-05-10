/************************************************************************/
/*                                                                      */
/*  Fills in a type_206 record                                          */
/*                                                                      */
/*      Inputs:         via externs                                     */
/*                                                                      */
/*      Output:         t206        Filled in type_206 record           */
/*                                                                      */
/* Created 1 September 1999 by CJL                                      */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include "mk4_data.h"
#include "param_struct.h"
#include "pass_struct.h"
#include "vex.h"
#include "filter.h"

int
fill_206 (
struct scan_struct *root,
struct type_pass *pass,
struct type_param *param,
struct type_status *status,
struct type_206 *t206)
    {
    int int_start, max, min, fr, num_ap, samp_per_ap;
    extern struct type_filter filter;

    clear_206 (t206);

    t206->start.year = root->start_time.year;
    t206->start.second = fmod ((double)param->start,  60.0);
    int_start = param->start;               /* In seconds */
    int_start /= 60;                        /* Now in minutes */
    t206->start.minute = int_start % 60;
    int_start /= 60;                        /* Now in hours */
    t206->start.hour = int_start % 24;
    t206->start.day = int_start / 24 + 1;   // add 1 to make doy

    t206->first_ap = pass->ap_off;
    t206->last_ap = pass->ap_off + pass->num_ap;
                                            /* Take account of fractional APs */
    if (pass->channels > 0)
        t206->intg_time = status->total_ap_frac * param->acc_period / pass->channels;

    min = max = status->ap_num[0][0] + status->ap_num[1][0];
    samp_per_ap = param->acc_period / param->samp_period;
    for (fr = 0; fr < pass->nfreq; fr++)
        {
        t206->accepted[fr].usb = status->ap_num[0][fr];
        t206->accepted[fr].lsb = status->ap_num[1][fr];
        num_ap = status->ap_num[0][fr] + status->ap_num[1][fr];
        if (num_ap > max) max = num_ap;
        if (num_ap < min) min = num_ap;
                                            /* Number of samples by freq/sband */
        t206->weights[fr].usb = status->ap_frac[0][fr] * samp_per_ap;
        t206->weights[fr].lsb = status->ap_frac[1][fr] * samp_per_ap;
        }
    if (max > 0) t206->accept_ratio = (float)(100 * min) / (float)max;
    t206->discard = (float)(100 * filter.ndiscard) / 
                        (float)(status->total_ap + filter.ndiscard);

    t206->ratesize = status->drsp_size;
    t206->mbdsize = status->grid_points;
    t206->sbdsize = param->nlags * 4;

    return (0);
    }
