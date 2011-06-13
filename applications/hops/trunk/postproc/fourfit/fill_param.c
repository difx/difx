/************************************************************************/
/*                                                                      */
/* This routine fills in the param structure as much as possible.  The  */
/* idea is to collect all the information useful to the actual fringe   */
/* fit in one place, the param structure, instead of leaving it         */
/* scattered around in the root and corel files.                        */
/*                                                                      */
/*      Inputs:         ovex, ivex      parsed ovex and ivex data       */
/*                      stn1, stn2      station info from root file     */
/*                      cdata           corel file structure            */
/*                                                                      */
/*      Output:         param           mostly filled in                */
/*                      return value    0=OK, else bad                  */
/*                                                                      */
/* Created 9 April 1998 by CJL                                          */
/* added determination of corr_type   rjc  2010.3.16                    */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "mk4_data.h"
#include "vex.h"
#include "param_struct.h"

int
fill_param (struct scan_struct *ovex,
            struct ivex_struct *ivex,
            struct station_struct *stn1,
            struct station_struct *stn2,
            struct mk4_corel *cdata,
            struct type_param *param)
    {
    double fact1, fact2;
    unsigned long ap_in_sysclks;
                                        /* Record the baseline */
    strncpy (param->baseline, cdata->t100->baseline, 2);
                                        /* Set the correlation type, lags */
/*     param->corr_type = cdata->t100->mode; */
    if (strcmp (ovex->correlator, "difx") == 0)
        param->corr_type = DIFX;
    else
        param->corr_type = MK4HDW;

    param->nlags = cdata->t100->nlags;
                                        /* Bits per sample */
    if (stn1->bits_sample != stn2->bits_sample)
        {
        msg ("Mismatching bits/sample: %d vs. %d", 2,
             stn1->bits_sample, stn2->bits_sample);
        return (-1);
        }
    param->bits_sample = stn1->bits_sample;
    if (param->bits_sample != 1 &&  param->bits_sample != 2)
        {
        msg ("Invalid bits/sample: %d", 2, param->bits_sample);
        return (-1);
        }
                                        /* Sample period comes from inverse sample rate */
    param->samp_period = 1.0  / stn1->samplerate;
    msg ("samplerates %lf %lf\n", 0, stn1->samplerate, stn2->samplerate);
    if (stn1->samplerate != stn2->samplerate)
        {
        msg ("Mismatching sample rates between stations %g %g",
        2, stn1->samplerate, stn2->samplerate);
        return (-1);
        }
                                        /* 1 over sigma noise */
                                        /* This depends on 1 or 2 bit */
                                        /* samples, and the Mk4 */
                                        /* correlator arithmetic. */
                                        /* Initial guesses based on Thompson, */
                                        /* Moran and Swenson p.304, VLBI */
                                        /* geodetic memo 008, and Mark 4 memo */
                                        /* number 148. */
    if (param->nlags == 16)
        fact1 = 0.985;
    else if (param->nlags > 16)
        fact1 = 1.0;

    if (stn1 -> bits_sample == 1) 
        fact2 = 0.573;
    else if (stn1 -> bits_sample == 2)
        fact2 = 0.790;
    param->inv_sigma = fact1 * fact2 * sqrt(param->acc_period / param->samp_period);
                                        /* bocf period */
    param->bocf_period = ivex->bocf_period;
                                        /* calculate ap length in playback time sysclks */
    ap_in_sysclks = rint ((double)param->acc_period * 32e6 / param->speedup);

    if ((ap_in_sysclks % param->bocf_period) != 0)
        {
        msg ("Error, AP seems not to be integral number of frames (%d/%d)",
                    2, ap_in_sysclks, param->bocf_period);
        return (-1);
        }
    param->bocfs_per_ap = ap_in_sysclks / param->bocf_period;

                                        /* Skip pulsar parameters for now */

    return(0);
    }
