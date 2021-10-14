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
/* added code for 1bit x 2bit         rjc  2015.9.11                    */
/*                                                                      */
/************************************************************************/
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "mk4_data.h"
#include "vex.h"
#include "param_struct.h"
#include "control.h"
#include "ff_misc_if.h"

int
fill_param (struct scan_struct *ovex,
            struct ivex_struct *ivex,
            struct station_struct *stn1,
            struct station_struct *stn2,
            struct mk4_corel *cdata,
            struct type_param *param,
            struct c_block *cb_head)
    {
    int i;
    double fact1, fact2, fact3;
    unsigned long ap_in_sysclks;
    struct c_block *cb_ptr;
                                        /* Record the baseline */
    strncpy (param->baseline, cdata->t100->baseline, 2);

                                        /* Bits per sample */
    param->bits_sample[0] = stn1->bits_sample;
    param->bits_sample[1] = stn2->bits_sample;
    for (i=0; i<2; i++)                 // validate quantization parameters
        if (param->bits_sample[i] != 1 &&  param->bits_sample[i] != 2)
            {
            msg ("Invalid bits/sample: %d", 2, param->bits_sample[i]);
            return (-1);
            }
                                        /* Sample period comes from inverse sample rate */
    // one presumes this is correct insofar as only the narrow band correlates
    msg ("samplerates %lf %lf\n", 0, stn1->samplerate, stn2->samplerate);
    if (stn1->samplerate < stn2->samplerate)
        param->samp_period = 1.0  / stn1->samplerate;
    else
        param->samp_period = 1.0  / stn2->samplerate;
   
                                        /* Set the correlation type, lags */
                                    // correlator-dependent snr loss factors
    if (strcmp (ovex->correlator, "difx") == 0)
        {
        param->corr_type = DIFX;
        param->nlags = 2 * cdata->t100->nlags;
        fact3 = 0.970;              // bandpass (0.970)
        }
    else if (strcmp (ovex->correlator, "sfxc") == 0)
        {
        param->corr_type = SFXC;
        param->nlags = 2 * cdata->t100->nlags;
        fact3 = 0.970;              // BTFOOM
        }
    else
        {
        param->corr_type = MK4HDW;
        param->nlags = cdata->t100->nlags;
        fact3 = 0.8995;             // bandpass (0.970) * rotator loss(0.960) 
                                    // * discrete delay (0.966)
        }
    if (param->nlags == 8)
        fact1 = 0.96;
    else if (param->nlags == 16)
        fact1 = 0.985;
    else 
        fact1 = 1.0;

    // see extended discussion in adjust_snr.  difx2mark4 partitions
    // these sampling losses by station (i.e. a sqrt of the actual
    // factor which combine for the correct factor), however, difx2mark4
    // has higher precision values than these original ones.
    // The 1-bit x 2-bit case is what it is due to what is done in
    // difx2mark4.  Someone should actually work out the true loss.
    //
    // The corrections are only staged in the next few lines.
                                    // Nyquist-sampling losses
    switch (param->bits_sample[0] + param->bits_sample[1])
        {
        case 2:                     // 1-bit x 1-bit
            fact2 = 0.637;
            //fact2 = (param->corr_type==DIFX) ? 0.6366239755 : 0.637;
            break;
        case 3:                     // 1-bit x 2-bits
            fact2 = 0.749;        // approx. by harmonic mean of 1x1 & 2x2
            //fact2 = (param->corr_type==DIFX) ? 0.7495558345 : 0.749;
            break;
        case 4:                     // 2-bits x 2-bits
            fact2 = 0.881;
            //fact2 = (param->corr_type==DIFX) ? 0.8825208768 : 0.881;
            break;
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
