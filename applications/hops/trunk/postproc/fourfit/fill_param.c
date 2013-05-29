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
#include "control.h"

int
fill_param (struct scan_struct *ovex,
            struct ivex_struct *ivex,
            struct station_struct *stn1,
            struct station_struct *stn2,
            struct mk4_corel *cdata,
            struct type_param *param)
    {
    double fact1, fact2, fact3;
    unsigned long ap_in_sysclks;
    extern struct c_block *cb_head;
    struct c_block *cb_ptr;
                                        /* Record the baseline */
    strncpy (param->baseline, cdata->t100->baseline, 2);

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
    msg ("samplerates %lf %lf\n", 0, stn1->samplerate, stn2->samplerate);
    if (stn1->samplerate != stn2->samplerate)
        msg ("Sample rate mismatch for stations %g %g, assuming zoom mode",
        1, stn1->samplerate, stn2->samplerate);
    if (stn1->samplerate < stn2->samplerate)
        param->samp_period = 1.0  / stn1->samplerate;
    else
        param->samp_period = 1.0  / stn2->samplerate;
   
                                        /* Set the correlation type, lags */
    param->nlags = cdata->t100->nlags;
    if (param->nlags == 8)
        fact1 = 0.96;
    else if (param->nlags == 16)
        fact1 = 0.985;
    else 
        fact1 = 1.0;
                                    // Nyquist-sampling losses
    if (stn1 -> bits_sample == 1) 
        fact2 = 0.637;
    else if (stn1 -> bits_sample == 2)
        fact2 = 0.881;
                                    // correlator-dependent snr loss factors
    if (strcmp (ovex->correlator, "difx") == 0)
        {
        param->corr_type = DIFX;
        fact3 = 0.970;              // bandpass (0.970)
        }
    else
        {
        param->corr_type = MK4HDW;
        fact3 = 0.8995;             // bandpass (0.970) * rotator loss(0.960) 
                                    // * discrete delay (0.966)
        }
    param->inv_sigma = fact1 * fact2 * fact3 * 
                       sqrt(param->acc_period / param->samp_period);
                                        /* bocf period */
    param->bocf_period = ivex->bocf_period;
                                        /* calculate ap length in playback time sysclks */
    ap_in_sysclks = rint ((double)param->acc_period * 32e6 / param->speedup);
                                        // bocf_period only used in hardware correlator
    if ((ap_in_sysclks % param->bocf_period) != 0 && param->corr_type == MK4HDW)
        {
        msg ("Error, AP seems not to be integral number of frames (%d/%d)",
                    2, ap_in_sysclks, param->bocf_period);
        return (-1);
        }
    param->bocfs_per_ap = ap_in_sysclks / param->bocf_period;

                                        /* Skip pulsar parameters for now */

                                        // bit of a kludge to get non-null fmatch_bw_pct
                                        // if one exists (regardless of baseline)
    param->fmatch_bw_pct = 25.0;        // default value
    for (cb_ptr=cb_head; cb_ptr!=NULL; cb_ptr=cb_ptr->cb_chain)
        if (cb_ptr->fmatch_bw_pct != NULLFLOAT)
            param->fmatch_bw_pct = cb_ptr->fmatch_bw_pct;
    return(0);
    }
